;_____________________________IOISIOI____________________
; NAME:
;       s_Surface_Method_AnalyseFRAP
;
; PURPOSE:
;       Analysis of X-Ray Spectra
;
; AUTHOR:
;     Dr. Steffen Härtel (10|2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;     s_Surface_Method_AnalyseFRAP, image
;
;  KEYWORD PARAMETERS:
;     None
;
;  ORIGINAL SOFTWARE WRITTEN BY :
;_____________________________IOISIOI____________________
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;      Image Processing, Widgets.
;
; CALLING SEQUENCE:
;
;      s_Surface_Method_AnalyseFRAP, image
;
; INPUTS:
;
;      image:     A 2D array of image data.
;
; KEYWORD PARAMETERS:
;
;       BOTTOM:   The lowest color index of the colors to be used (see
;                 NCOLORS). The default is 0.
;
;       COLORINDEX: The color index for the rubberband box. This index will
;                 be loaded with a green color. Whatever color is there will
;                 be restored when the ZIMAGE program exits. The default is
;                 NCOLORS + BOTTOM.
;
;       NCOLORS:  This is the number of colors to use when displaying the
;                 image. The default is !D.N_colors-2.
;
;       GROUP_LEADER: This keyword is used to assign a group leader to this
;                 program. This program will be destroyed when the group
;                 leader is destroyed. Use this keyword if you are calling
;                 ZIMAGE from another widget program.
;
;       NOINTERPOLATION: Setting this keyword causes nearest neighbor resampling of
;                 of the zoomed image instead of the default bilinear interpolation
;                 of resampled pixels.
;
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;
;       The COLORINDEX color is turned to green while the rubberband box
;       is being drawn. The color is restored after the box is drawn.
;
; RESTRICTIONS:
;
;       Uses XCOLORS from the Coyote Library:
;          http://www.dfanning.com/programs/xcolors.pro
;
; PROCEDURE:
;
;       Clicking the left mouse button allows you to drag a rubberband box
;       over the portion of the window you want to zoom into.
;
;       Clicking the right mouse button calls up hidden controls that allow
;       you to load different color tables and set the zoom factor.
;
;       The rubberband box is drawn with pixmaps and the "device copy"
;       technique.
;
;       This is an excellent example of how you can take advantage of the
;       widget program *as* the loop do to something (i.e., draw the box)
;       that in a regular IDL program would have to be done in a loop. Motion
;       events are only turned on for the draw widget when the box has to be
;       drawn.
;
; EXAMPLE:
;
;        To display an image you can zoom into, type:
;
;        ZIMAGE, image
;
; MODIFICATION HISTORY:
;
;        Written by: David Fanning, 15 August 96.
;        Fixed a !D.N_colors problem. 17 June 98.
;        Made modifications so program works in 24-bit environment. 28 July 98. DWF.
;        Fixed a problem with the pop-up controls under certain circumstances.
;          13 Oct 98. DWF.
;        Added 24-bit color response. 13 Oct 98. DWF.
;        Added ability for each window to have its own color changing tool. 9 Oct 99. DWF.
;        Small changes, error checking. 24 April 2000. DWF.
;        Modified draw widget error handling to be consistent with current programming
;         practices. 26 April 2001. DWF.
;       - Dr. Steffen Härtel (10|2001). See Top.
;
;-
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 1999-2001 Fanning Software Consulting
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. if you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; for more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################

pro s_Surface_Method_AnalyseFRAP_control, event

  ; Event handler to select surface style.
  widget_control, event.top, get_uvalue=info, /no_copy
  ; Event handler to select Button uValue.
  widget_control, event.id, Get_Value=ButtonValue, /no_copy
  
  case ButtonValue of
    'Measure Mode': begin
      
      info.Modecontrol = 1 - info.Modecontrol            ; Switch Flag (1/0 ON/OFF) for Mode control
      widget_control, info.measureID, sensitive = 0      ; to signify it is in use
      widget_control, info.zoomID, sensitive = 1         ; to signify you can use it in the future
      widget_control, info.zoomfactorID, sensitive = 0   ; can't use it until you activate zoomID 
      info.nDrawZoom = 0
      info.nDraw = 1
      ; make the text buttons sensitive for input
      widget_control, info.field1, sensitive = 1
      widget_control, info.field2, sensitive = 1
      widget_control, info.field3, sensitive = 1
      widget_control, info.field4, sensitive = 1
      widget_control, info.field5, sensitive = 1
      widget_control, info.field6, sensitive = 1
      ; operations available after taking into consideration user inputs
      widget_control, info.drawID, sensitive = 1        ; draw concentric circles
      widget_control, info.calcID, sensitive = 1        ; calculate circles intensities
      widget_control, info.calcLineID, sensitive = 1    ; calculate line intensities
      widget_control, info.drawMaskID, sensitive = 1    ; use segmentation when calculating intensities
      info.useMask = 0                                  ; set internal info for reference
      info.nCalcCircle = 1
      info.nCalcLine = 0
      widget_control, info.resetID, sensitive = 1
      widget_control, info.saveID, sensitive = 0        ; the save and plot buttons don't get activated until after a calculation
      widget_control, info.plotID, sensitive = 0      
      
      if NOT(ptr_valid(info.pDrawObj[4])) then begin
        for i = 0,5 do begin
          if (obj_valid(*info.pDrawObj[i])) then obj_destroy, *info.pDrawObj[i]
          if (ptr_valid(info.pDrawObj[i])) then ptr_free, info.pDrawObj[i]
        endfor
        info.pDrawObj[1] = ptr_new(obj_new('C_sCircle'), /no_copy)
        info.pDrawObj[2] = ptr_new(obj_new('C_sCross'), /no_copy)
        info.pDrawObj[3] = ptr_new(obj_new('C_sAngle'), /no_copy)
        info.pDrawObj[4] = ptr_new(obj_new('C_sFrapCircle'), /no_copy)
        info.pDrawObj[5] = ptr_new(obj_new('C_sFrapLine'), /no_copy)
      endif
      
    endcase
    'Zoom Mode': begin

      info.Modecontrol = 1 - info.Modecontrol            ; Switch Flag (1/0 ON/OFF) for Mode control
      widget_control, info.measureID, sensitive = 1
      widget_control, info.zoomID, sensitive = 0
      widget_control, info.zoomfactorID, sensitive = 1
      info.nDrawZoom = 1
      info.nDraw = 0
      
      ; make the text buttons sensitive for input
      widget_control, info.field1, sensitive = 0
      widget_control, info.field2, sensitive = 0
      widget_control, info.field3, sensitive = 0
      widget_control, info.field4, sensitive = 0
      widget_control, info.field5, sensitive = 0
      widget_control, info.field6, sensitive = 0
      ; operations available after taking into consideration user inputs
      widget_control, info.drawID, sensitive = 0        ; draw concentric circles
      widget_control, info.calcID, sensitive = 0       ; calculate circles intensities
      widget_control, info.calcLineID, sensitive = 0    ; calculate line intensities
      widget_control, info.drawMaskID, sensitive = 0    ; use segmentation when calculating intensities
      info.useMask = 0                                  ; set internal info for reference
      info.nCalcCircle = 0
      info.nCalcLine = 0
      widget_control, info.resetID, sensitive = 0
      widget_control, info.saveID, sensitive = 0        ; the save and plot buttons don't get activated until after a calculation
      widget_control, info.plotID, sensitive = 0      

      ; create square for zoom box
      if NOT(ptr_valid(info.pDrawObj[0])) then info.pDrawObj[0] = ptr_new(obj_new('C_sSquare'), /no_copy)
    endcase
 
  end
  
  ;Put the info structure back.
  widget_control, event.top, set_uvalue=info, /no_copy
  
end
;-s_Surface_Method_AnalyseFRAP_control, event-----------------------------------------------------------------shaertel-2001-



pro s_Surface_Method_AnalyseFRAP_colors, event

  widget_control, event.top, get_uvalue=info, /no_copy
  ; What kind of event is this?
  
  thisEvent = Tag_Names(event, /Structure)
  case thisEvent of
    'WIDGET_BUTTON': begin
      TVLCT, info.r, info.g, info.b, info.bottom
      XColors, Group=event.top, NColors = info.ncolors,$
        Bottom=info.bottom, NotifyID=[event.id, event.top],$
        Title='ZImage Colors (' + StrTrim(info.drawIndex,2) + ')'
      widget_control, info.controlID, Map=0
      info.map = 0
    endcase
    'REDRAW_IMAGE': begin
      Device, Get_Visual_Depth = thisDepth
      if thisDepth GT 8 then begin
        WSet, info.drawIndex
        TVLCT, info.r, info.g, info.b, info.bottom
        tv, BytScl((info.image), Top = info.ncolors-1) + info.bottom
        WSet, info.pixIndex
        Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.drawIndex]
        
        if (info.Modecontrol) then begin
          WSet, info.drawIndex
          (*info.pDrawObj[1])->Draw
          (*info.pDrawObj[2])->Draw
          (*info.pDrawObj[3])->Draw
          (*info.pDrawObj[4])->Draw
          (*info.pDrawObj[5])->Draw  
        endif
        
        if (info.Modecontrol ne 1) then begin
          ; Draw the zoom box.
          WSet, info.drawIndex
          TVLCT, info.r, info.g, info.b, info.bottom
          Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]
          TVLCT, 0B, 255B, 0B, info.colorIndex
          PlotS, [info.xs, info.xs, info.xd, info.xd, info.xs], [info.ys, info.yd, info.yd, info.ys, info.ys], /Device, Color = info.colorIndex
          (*info.pDrawObj[0])->Draw 
          
          if widget_Info(info.zoomDrawID, /Valid_ID) then begin
            zoomFactor = info.zoomFactor
            zoomXSize = (abs(info.xd - info.xs) + 1) * zoomFactor
            zoomYSize = (abs(info.yd - info.ys) + 1) * zoomFactor
            zoomedImage = Congrid((info.image)[ info.xs< info.xd:info.xd>info.xs, info.ys<info.yd :info.ys>info.yd], zoomXSize, zoomYSize, Interp = info.interp)
            ; Zoomed window exists. Make it correct size and load image.
            WSet, info.zoomWindowID
            if ptr_valid(info.subimage) then *info.subimage = BytScl(zoomedImage, Top = info.ncolors-1,$
              max = max(info.image), min = min(info.image)) + info.bottom else  $
              info.subimage = ptr_new(BytScl(zoomedImage, Top = info.ncolors-1,$
              max = max(info.image), min = min(info.image)) + info.bottom)
            WSet, info.zoomWindowID
            tvscl, *info.subimage
          endif
        endif
      endif
    endcase
    'XCOLORS_LOAD':begin
    
    ; Extract the new color table vectors from XCOLORS.
    info.r = event.r[info.bottom:info.bottom+info.ncolors-1]
    info.g = event.g[info.bottom:info.bottom+info.ncolors-1]
    info.b = event.b[info.bottom:info.bottom+info.ncolors-1]
    
    Device, Get_Visual_Depth=thisDepth
    if thisDepth GT 8 then begin
    
      ; Redisplay the image.
      WSet, info.drawIndex
      tv, BytScl(info.image, Top=info.ncolors-1) + info.bottom
      WSet, info.pixIndex
      Device, Copy=[0, 0, info.xsize, info.ysize, 0, 0, info.drawIndex]
      
      ; Is a zoom window open? if so, redisplay it as well.
      if Widget_Info(info.zoomDrawID, /Valid_ID) then begin
        WSet, info.zoomWindowID
        if ptr_valid(info.subimage) then tv, *info.subimage
      endif
      
    endif
  endcase
endcase
widget_control, event.top, set_uvalue=info, /no_copy
end ;*******************************************************************



pro s_Surface_Method_AnalyseFRAP_QUITTER, event
  s_Surface_Method_AnalyseFRAP_cleanup, event.top
  if widget_info(event.top, /valid_id) then widget_control, event.top, /destroy
end ;*******************************************************************



pro s_Surface_Method_AnalyseFRAP_cleanup, tlb

  widget_control, tlb, get_uvalue = state, /no_copy
  
  if n_elements(state) NE 0 then begin
    for i = 0, (size(state.pDrawObj, /dim))[0]-1 do begin
      if (ptr_valid((state.pDrawObj)[i])) then begin
        if (obj_valid(*(state.pDrawObj)[i])) then obj_destroy, *(state.pDrawObj)[i]
        ptr_free, (state.pDrawObj)[i]
      endif
    endfor
    
    for i = 0, n_tags(state)-1 do begin
      case size(state.(i), /tname) of
        'POINTER': ptr_free, state.(i)
        'OBJREF': obj_destroy, state.(i)
        else:
      endcase
    endfor
  endif
  
  if widget_info(state.groupLeader, /valid_id) then begin
    widget_control, state.groupLeader, get_uvalue = stateParent, /no_copy
    if (n_elements(stateParent) ne 0) then begin
      ; MOR - added OnOff to the wXRayButton variable
      if (s_ToggleButtonOnOffState(stateParent.wFRAPButtonOnOff) eq 1) then void = s_ToggleButtonOnOffState(stateParent.wFRAPButtonOnOff)
      widget_control, state.groupLeader, set_uvalue = stateParent, /no_copy
    endif
  endif
  
  widget_control, tlb, set_uvalue = state, /no_copy
  if widget_info(tlb, /valid_id) then widget_control, tlb, /destroy
  
  ; delete the plot results window
  wdelete, 1 

  
  
end ; **s_Surface_Method_AnalyseFRAP_cleanup **********************************************************



pro s_Surface_Method_AnalyseFRAP_FACTOR, event
  ; The purpose of this event handler is to set the zoom factor.
  widget_control, event.top, get_uvalue=info, /no_copy
  widget_control, event.id, get_uvalue=factor
  info.zoomfactor = factor(event.index)
  widget_control, info.controlID, Map=0
  info.map = 0
  widget_control, event.top, set_uvalue=info, /no_copy
end ; **s_Surface_Method_AnalyseFRAP_FACTOR **********************************************************



pro s_Surface_Method_AnalyseFRAP_BUTTON_DOWN, event

  ; This event handler ONLY responds to button down events from the
  ; draw widget. if it gets a DOWN event, it does three things: (1) sets
  ; the static and dynamic corners of the zoom box, (2) changes the
  ; event handler for the draw widget to ZOOMBOX_DRAWBOX and turns on MOTION
  ; events, and (3) takes over color index 1 of the color table for the
  ; zoom box drawing color.
  ; Put the info structure back into its storage location.

  widget_control, event.top, set_uvalue=info, /no_copy
end ; of s_Surface_Method_AnalyseFRAP_BUTTON_DOWN *****************************************************

pro s_Surface_Method_AnalyseFRAP_DRAWMASK, event

  widget_control, event.id, GET_UVALUE = eventval
  widget_control, event.top, GET_UVALUE = info
   
  case eventval of
    "USEMASK": begin 
      info.useMask = 1
      widget_control, info.drawMaskID, sensitive = 0 ; 10Mar2011
      print, 'create window with the intensity only at the segmentation or something here'
     end
    
  endcase
  
  
  ; Put the info structure back into its storage location.
  widget_control, event.top, set_uvalue=info, /no_copy
  
end


pro s_Surface_Method_AnalyseFRAP_DRAW, event

  widget_control, event.id, GET_UVALUE = eventval
  widget_control, event.top, GET_UVALUE = info
  
  widget_control, info.calcID, sensitive = 1
  widget_control, info.calcLineID, sensitive = 1
  widget_control, info.drawMaskID, sensitive = 1 ; 10Mar2011
  info.useMask = 0
  
  case eventval of
    "DRAWFRAP": begin
    
      ;************** TODO: if you change radius, change in the text box widget *******
      ; check to see that the dimensions given by the user are within the image
      temp = info.xc
      info.xc = fix(info.xc)
      if( (info.xc gt (info.xsize-1 - info.rad)) or (info.yc gt (info.ysize-1 - info.rad)) ) then begin
        r = info.rad
        print, 'changing radius'
        print, 'current radius is ', r
        ;                    rx = r - abs((info.xs - (info.xsize-1 - r)))
        ;                    ry = r - abs((info.ys - (info.ysize-1 - r)))
        rx = r - abs((info.xsize-1) - (info.xc + r))
        ry = r - abs((info.ysize-1) - (info.yc + r) )
        if( info.xc gt (info.xsize-1 - info.rad) ) then rMin = rx
        if( info.yc gt (info.ysize-1 - info.rad) )  then rMin = ry
        if( info.xc gt (info.xsize-1 - info.rad) and (info.yc gt (info.ysize-1 - info.rad) ) ) then rMin = min(rx,ry)
        info.rad = rMin
        print,'new radius is', rMin
        widget_control, info.field3, set_value = info.rad
      endif else begin
        ;                    r = info.rad
        rMin = info.rad
      endelse
      
      if( (info.rad gt info.xc) or (info.rad gt info.yc)) then begin
        rx = info.rad - abs(info.rad - info.xc)
        ry = info.rad - abs(info.rad - info.yc)
        if( info.rad gt float(info.xc) ) then rMin = rx
        if( info.rad gt float(info.yc) ) then rMin = ry
        if( (info.rad gt float(info.xc)) and (info.rad gt float(info.yc))) then rMin = min(rx,ry)
        info.rad = rMin
        widget_control, info.field3, set_value = info.rad
      endif else begin
        rMin = info.rad
      endelse
      
      ; Turn draw Button events ON.
      widget_control, event.id, Draw_Button_Events=1
      ; Load a green color in color index 1 to draw the zoom box with.
      TVLCT, 255B, 0B, 0B, info.colorIndex
      WSet, info.drawIndex
      TVLCT, info.r, info.g, info.b
      Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]
      ; create cross hairs
      x1 = info.xc
      x2 = x1+1
      y1 = info.yc
      y2 = y1+1
      (*info.pDrawObj[2])->setProperty,x1=x1, y1=y1, x2=x2, y2=y2,  color = info.colorIndex
      (*info.pDrawObj[2])->Draw
      
      ; FRAP concentric circles
      (*info.pDrawObj[4])->setProperty, x = info.xc, y = info.yc,  color = info.colorIndex, r = rMin, dr = info.interval
      (*info.pDrawObj[4])->Draw
      
      ; FRAP concentric circles and two lines
      (*info.pDrawObj[5])->setProperty, x = info.xc, y = info.yc,  color = info.colorIndex, r = rMin, dr = info.interval, angle = info.angle
      (*info.pDrawObj[5])->Draw  
    
  endcase
  
  end ; end switch statement
  
  
  ; Put the info structure back into its storage location.
  widget_control, event.top, set_uvalue=info, /no_copy
  
end

pro s_Surface_Method_AnalyseFRAP_SAVE, event

  widget_control, event.id, GET_UVALUE = eventval
  widget_control, event.top, GET_UVALUE = info
  
  ; checks if the user wants calculations for [0,r] or [-r,r]
  idLine = widget_info(info.calclineid, /button_set)
  idConcentric = widget_info(info.calcid, /button_set)
  
  if(idLine or info.nCalcLine) then title = '_line'
  if(idConcentric or info.nCalcCircle) then title = '_circle'
  
  case eventval of
    "SAVEFRAP": begin
;      if(info.meanFrapPlot[*,info.tPos] ne -1) then begin
     
;      plot, DR, meanFrapPlot, color = 3, thick = 2, xtitle = 'radius [pix]', ytitle = 'mean intensity'
      ; Parameter_Save
      name = strCompress('meanIntensity_xc' + string(info.xc) + '_yc' + string(info.yc) + '_r' + string(info.rad) + '_int' + string(info.interval) + '_ang' +string(info.angle)+ '_mk' + string(info.useMask)+ title + '.dat', /rem)
      filename = strCompress(info.path + name, /rem)
      get_lun, U
      
;      ; check if file exists
;      if(file_test(filename) eq 0) then begin
;        openW, U, filename
;        printF, U, 'rad' + string(9b) + strCompress('t' + string(info.tPos))
;        close,U
;      endif 
      
      ; write to fle
        openW, U, filename
        ; get dimensions of meanFrapPlot
        dims = size((*info.meanFrapPlot), /dim)
;        temp = make_array(info.totalTNum+1, info.interval+1, value = -1.)
        temp = make_array(dims[1]+1,dims[0], value = -1.)
        temp[0,*] = *(info).DR
       ; print, (*info.meanFrapPlot)
        ; writing to file incorrectly...maybe need to tranpose data
        temp[1:info.totalTNum,*] = transpose((*info.meanFrapPlot))
        ;print, temp[1:info.totalTNum,*]
        cols = info.totalTNum+1
        printF,U, temp, format = strcompress('(',/rem) + strcompress(string(cols),/rem) + strcompress('F10.3',/rem) +strcompress(')' ,/rem)
        close, U 
        free_lun, U
        
         widget_control, info.plotID, sensitive = 1
        
        endcase
    
    'PLOTFRAPRESULTS': begin
    
      idLine = widget_info(info.calclineid, /button_set)
      idConcentric = widget_info(info.calcid, /button_set)
    
        ; determine filename
        name = strCompress('meanIntensity_xc' + string(info.xc) + '_yc' + string(info.yc) + '_r' + string(info.rad) + '_int' + string(info.interval) + '_ang' +string(info.angle)+'_mk' + string(info.useMask) + title + '.dat', /rem)
        filename = strCompress(info.path + name, /rem)
;        tempInput = read_binary(filename, data_dims = [info.totalTNum+1, info.interval+1])
;        plot, tempInput
        
        ; open file for reading info.
        openR, 1, filename, error = err
        
        ; check if there was an error in opening the file for reading, err = 0 means something happened
        if (err ne 0) then printf, -2, !error_state.msg 
        
        if (err eq 0 ) then begin
        
          if(idConcentric or info.nCalcCircle) then begin
            tempInput = make_array(info.totalTNum+1, info.interval+1, value = -1.)
          endif else begin
            tempInput = make_array(info.totalTNum+1, 2*info.interval+1, value = -1.)
          endelse
        
          while ~eof(1) do begin
            readf, 1, tempInput
          endwhile
          
          close, /all
          
          ; plot window for results
          window, 1, title = 'Intensity vs. Time'
          table = read_tiff(s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_HistoLUT.tif')
          rgbArray = table
          rgbStep = 1
          
          if(idConcentric) then begin 
            j = -40.551 * alog(info.interval+1) + 250.32
            step = 1920. / ((j+1)*info.totalTNum+1) ;Gradient+
           
           ncol = !d.n_colors/(info.interval+1)
           colors = ncol*indgen(info.interval+1)+ ncol
                             
           plot, tempInput[1:info.totalTNum,0], xtitle = 'time [number of frames] ',ytitle = 'mean intensity'
;          plot, tempInput[1:info.totalTNum,0], color = rgbArray[*,rgbStep]
 
            for k = 1,info.interval do begin
          
              oplot, tempInput[1:info.totalTNum,k], color = colors[k]
;            oplot, tempInput[1:info.totalTNum,k], color= rgbArray[*,rgbStep]
              rgbStep += ceil(step) ; Gradient
            endfor
          
          endif 
          
          if(idLine) then begin
            j = -40.551 * alog(info.interval+1) + 250.32
            step = 1920. / ((j+1)*info.totalTNum+1) ;Gradient+
           
            ncol = !d.n_colors/(2*info.interval+1)
            colors = ncol*indgen(2*info.interval+1)+ ncol
                             
            plot, tempInput[1:info.totalTNum,0], xtitle = 'time [number of frames] ',ytitle = 'mean intensity'

            for k = 1,2*info.interval do begin
              oplot, tempInput[1:info.totalTNum,k], color = colors[k]
              rgbStep += ceil(step) ; Gradient
            endfor
          
          endif
          
        endif
      
      endcase
      
      end
    

  widget_control, event.top, set_uvalue=info, /no_copy
   
end

pro s_Surface_Method_AnalyseFRAP_CALC, event

  widget_control, event.id, GET_UVALUE = eventval
  widget_control, event.top, GET_UVALUE = info
  

  ; change the dimensions of the pointer for the intensity data if calcs on [-r,r]
 
  if(n_elements(eventval) ne 0) then begin
    if(eventval eq 'CALCFRAPLINE') then begin
      if(ptr_valid(info.meanFrapPlot)) then begin
          ptr_free, info.meanFrapPlot
          info.meanFrapPlot = ptr_new(fltArr(2*info.interval+1,info.totalTNum),/no_copy)
          widget_control, info.calcID, sensitive = 0
          info.nCalcCircle = 0
          info.nCalcLine = 1
      endif
    endif
  endif
    if(n_elements(eventval) ne 0) then begin
    if(eventval eq 'CALCFRAP') then begin
      if(ptr_valid(info.meanFrapPlot)) then begin
          ptr_free, info.meanFrapPlot
          info.meanFrapPlot = ptr_new(fltArr(info.interval+1,info.totalTNum),/no_copy)
          widget_control,info.calcLineID, sensitive = 0
          info.nCalcCircle = 1
          info.nCalcLine = 0          
      endif
    endif
  endif
  
      ; checks if the user wants calculations for [0,r] or [-r,r]
  idLine = widget_info(info.calclineid, /button_set)
  idConcentric = widget_info(info.calcid, /button_set)
  
      
  if ( n_elements(eventval) eq 0 ) then begin
    possibleEventTypes = [ 'PRESS', 'RELEASE', 'MOTION', 'SCROLL' ]
    eventval = possibleEventTypes[event.type]
     ; need to make the line and concentric circle buttons different or turn off
     if((idConcentric and idLine) and (eventval eq 'CALCFRAP')) then widget_control,info.calcLineID, sensitive = 0, idLine = 0
     if((idConcentric and idLine) and (eventval eq 'CALCFRAPLINE')) then widget_control, info.calcID, sensitive = 0, idConcentric = 0
     
     ; this event happens during the update call
     if(eventval eq 'PRESS' and idConcentric) then begin 
        eventval = "CALCFRAP" 
        idLine = 0
        info.nCalcCircle = 1
        info.nCalcLine = 0
     endif
     if(eventval eq 'PRESS' and idLine) then begin
        eventval = "CALCFRAPLINE" 
        idConcentric = 0
        info.nCalcLine = 1
        info.nCalcCircle = 0
     endif  
     
     ; this event happens during the update call but no calc button has been selected
     if(eventval eq 'PRESS' and (idConcentric eq 0) and (idLine eq 0)) then  begin
        widget_control, event.top, set_uvalue=info, /no_copy 
        return
     endif 
     
       
  end

   
  case eventval of
    "CALCFRAP" or 'PRESS': begin
    
      pImageData = ptr_new(info.image)
      pMaskData = ptr_new(info.mask) ; 10Mar2011
      pXYDistMatrix = ptr_new()
      pRadMatrix = ptr_new()
      
      (*info.pDrawObj[4])->getConcentricIntensities,  pImageData = pImageData,$
        pXYDistMatrix = pXYDistMatrix,$
        pRadMatrix = pRadMatrix,$
        xyRealSize = [info.realXSize, info.realYSize],$
        count = b, rad = a, xc = xc, yc = yc, dr = numC, $
        useMask = info.useMask, pMaskData = pMaskData
        
      info.count = b
      info.xc = xc
      info.yc = yc
      info.interval  = numC
      ;print, info.realXSize
      if (ptr_valid(pImageData)) then ptr_free, pImageData
      
      if NOT(ptr_valid(pRadMatrix)) then begin
        widget_control, event.top, set_uvalue=info, /no_copy
        Return
      endif
      
      wTitle = strCompress('FRAP Analysis')
      ; if the Zoom Window exists, make it the proper size and load
      ; the zoomed image into it. if it does not exists, create it.
      if Widget_Info(info.zoomDrawID, /ValID) then begin
        WSet, info.zoomWindowID
        ; Zoomed window exists. Make it correct size and load image.
        widget_control, info.zoomDrawID, XSize=300, YSize=200, TLB_Set_Title = wTitle
        
      endif else begin
      
        ; Get offset positions for the non-existing zoom window.
        widget_control, event.top, TLB_Get_Size=sizes, TLB_Get_Offset=offsets
        xpos = sizes[0] + offsets[0] + 20
        ypos = offsets[1] + 40
        
        ; Zoom window does not exist. Create it.
        zoomtlb = widget_base(Title = wTitle, Group=event.top, XOffset=xpos, YOffset=ypos)
        zoomdraw = Widget_Draw(zoomtlb, XSize=300, YSize=200)
        widget_control, zoomtlb, /Realize
        widget_control, zoomdraw, Get_Value=windowID
        info.zoomDrawID = zoomdraw
        info.zoomWindowID = windowID
      endelse
      
      WSet, info.zoomWindowID
      WShow, info.zoomWindowID
      
      ;                    endelse
      
      dimplot = Size(*pRadMatrix)
      meanPlot = fltArr( dimplot[2] )
      meanX = fltArr( dimplot[2] )
      wherePos = where((*pRadMatrix)[*,0] ne -1)
      if (wherePos[0] ne -1) then begin
        meanPlot[0] = mean( (*pRadMatrix)[wherePos, 0] )
        meanX[0] = mean( (*pXYDistMatrix)[wherePos, 0] )
      endif
      ; mean over all angles for each radius
      for i = 0, dimplot(2)-1 do begin
        wherePos = where((*pRadMatrix)[*,i] ne -1)
        if (wherePos[0] ne -1) then begin
          meanPlot[i] = mean( (*pRadMatrix)[wherePos, i] )
          meanX[i] = mean( (*pXYDistMatrix)[wherePos, i] )
        endif
      endfor
      
      tvlct, r, g, b, /get
      old_color = [r,g,b]
      r = [0, 1, 1, 0, 0, 1]   ;Specify the red component,..
      g = [0, 1, 0, 1, 0, 1]     ;green component, and..
      b = [0, 1, 0, 0, 1, 0]     ;blue component of each color.
      tvlct, 255*r, 255*g, 255*b   ; COLOR 0/BLACK 1/WHITE 2/RED 3/GREEN 4/BLUE 5/YELLOW
      
      ;                   oplot, meanX, meanPlot, color = 2, thick = 2
      
      ; generate a mean plot for the angles for each concentric circle
      ddr = 0
      ddr = 1.*(info.rad*(info.realXSize/info.xsize))/info.interval
      DR = ddr # make_array(info.interval+1, /index)
      meanFrapPlot = fltArr( info.interval + 1)
      meanFrapX = fltArr( info.interval + 1)

      
      for j = 0, info.interval do begin
        if (j eq 0) then  begin
          whereConRad = where(meanX eq DR[0], dum)
          if (whereConRad[0] ne -1) then begin
            meanFrapPlot[0] = mean(meanPlot[whereConRad])
          endif
        endif else begin
        
          whereConRad = where(meanX ge DR[j-1] and meanX lt DR[j], dum)
          if (whereConRad[0] ne -1) then begin
            meanFrapPlot[j] = mean(meanPlot[whereConRad])
          endif
        endelse
        
      endfor
      
      ; color the numC approximations in green
      plot, DR, meanFrapPlot, color = 3, thick = 2, xtitle = 'radius [pix]', ytitle = 'mean intensity', title = strcompress('mean intensity at tPos: ' + string(info.tPos * info.realDt, format = '(f0.2)') + ' [s]')
      
      ; save to info. struct for access later 
      *(info).DR = DR
      (*info.meanFrapPlot)[*,info.tPos]= transpose(meanFrapPlot)
      
          end
    
    "CALCFRAPLINE" or 'RELEASE' : begin 
    
      pImageData = ptr_new(info.image)
      pXYDistMatrix = ptr_new()
      pRadMatrix = ptr_new()
      pMaskData = ptr_new(info.mask) ; 10Mar2011

   
            ; MOR - 03Mar2011 frap line
      (*info.pDrawObj[5])->getConcentricIntensities,  pImageData = pImageData,$
        pXYDistMatrix = pXYDistMatrix,$
        pRadMatrix = pRadMatrix,$
        xyRealSize = [info.realXSize, info.realYSize],$
        count = b, rad = a, xc = xc, yc = yc, dr = numC, ang = ang, pMaskData = pMaskData, useMask = info.useMask
        
      info.count = b
      info.xc = xc
      info.yc = yc
      info.interval  = numC

      if (ptr_valid(pImageData)) then ptr_free, pImageData
      if (ptr_valid(pMaskData)) then ptr_free, pMaskData ; 10March2011
      
      if NOT(ptr_valid(pRadMatrix)) then begin
        widget_control, event.top, set_uvalue=info, /no_copy
        Return
      endif
      
      wTitle = strCompress('FRAP Line Analysis')
      ; if the Zoom Window exists, make it the proper size and load
      ; the zoomed image into it. if it does not exists, create it.
      if Widget_Info(info.zoomDrawID, /ValID) then begin
        WSet, info.zoomWindowID
        ; Zoomed window exists. Make it correct size and load image.
        widget_control, info.zoomDrawID, XSize=300, YSize=200, TLB_Set_Title = wTitle
        
      endif else begin
      
        ; Get offset positions for the non-existing zoom window.
        widget_control, event.top, TLB_Get_Size=sizes, TLB_Get_Offset=offsets
        xpos = sizes[0] + offsets[0] + 20
        ypos = offsets[1] + 40
        
        ; Zoom window does not exist. Create it.
        zoomtlb = widget_base(Title = wTitle, Group=event.top, XOffset=xpos, YOffset=ypos)
        zoomdraw = Widget_Draw(zoomtlb, XSize=300, YSize=200)
        widget_control, zoomtlb, /Realize
        widget_control, zoomdraw, Get_Value=windowID
        info.zoomDrawID = zoomdraw
        info.zoomWindowID = windowID
      endelse
      
      WSet, info.zoomWindowID
      WShow, info.zoomWindowID
      
      
      dimplot = Size(*pRadMatrix)
      meanPlot = fltArr( 2*dimplot(2) )
      meanX = fltArr( 2*dimplot(2) )
       ; get the ranges that represent the negative radius values in wherePosNegR
       ; as well as the ranges that represent the positive radius values in wherePosPosR
       ; start for r = 0
      wherePos = where((*pRadMatrix)[*,0] ne -1)
      idx = info.rad*2 + 1
      midIdx = floor(idx/2)
      ; handle the negative angles 
      if(fix(info.angle - 90) lt 0) then begin 
          sAng = fix((360 + (info.angle - 90)) mod 360)
      endif else begin
          sAng = fix((info.angle - 90) mod 360) 
      endelse
      eAng = fix((info.angle + 89) mod 360)
      if (wherePos[0] ne -1) then begin
        ; partition into quadrants for displaying (-r, r) values
        ; the 2nd and 3rd quadrant will be for -r and 1st and 4th for +r
         minAng = min([sAng, eAng], max = maxAng)
         wherePosNegR = where(wherePos ge minAng and  wherePos le maxAng, complement = wherePosPosR)
         if((wherePosNegR[0] eq -1) or (wherePosPosR[0] eq -1)) then break
        meanPlot[midIdx] = mean( (*pRadMatrix)[wherePosPosR, 0] )
        meanX[midIdx] = mean( (*pXYDistMatrix)[wherePosPosR, 0] )
      endif
      
      ; mean over all angles for each radius
      for i = 0, dimplot(2)-1 do begin
        wherePos = where((*pRadMatrix)[*,i] ne -1)
        if (wherePos[0] ne -1) then begin
        ; partition into quadrants for displaying (-r, r) values
        ; the 2nd and 3rd quadrant will be for -r and 1st and 4th for +r
          minAng = min([sAng, eAng], max = maxAng)
          wherePosNegR = where(wherePos ge minAng and  wherePos le maxAng, complement = wherePosPosR)
          ; add error warning
          if((wherePosNegR[0] eq -1) or (wherePosPosR[0] eq -1)) then break
          meanPlot[(midIdx-1)-i] = mean( (*pRadMatrix)[wherePosNegR, i] )
          meanX[(midIdx-1)-i] = - mean( (*pXYDistMatrix)[wherePosNegR, i] ) ; make this negative
          meanPlot[i+midIdx] = mean( (*pRadMatrix)[wherePosPosR, i] )
          meanX[i+midIdx] = mean( (*pXYDistMatrix)[wherePosPosR, i] )          
          
        endif
      endfor
      
      tvlct, r, g, b, /get
      old_color = [r,g,b]
      r = [0, 1, 1, 0, 0, 1]   ;Specify the red component,..
      g = [0, 1, 0, 1, 0, 1]     ;green component, and..
      b = [0, 1, 0, 0, 1, 0]     ;blue component of each color.
      tvlct, 255*r, 255*g, 255*b   ; COLOR 0/BLACK 1/WHITE 2/RED 3/GREEN 4/BLUE 5/YELLOW
      
      ; generate a mean plot for the angles for each concentric circle
      ddr = 0
      ddr = 1.*(info.rad*(info.realXSize/info.xsize))/info.interval
      DR = make_array(2*info.interval+1)
      tempDR = ddr # make_array(info.interval+1, /index)      
      DR[0:(info.interval)-1] = -1.*reverse(tempDR[1:info.interval]) ; do not copy r = 0
      DR[(info.interval):2*(info.interval)]  = tempDR 
      meanFrapPlot = fltArr( 2*info.interval + 1)
      meanFrapX = fltArr( 2*info.interval + 1)

      jj = 0;
      for j = 0, 2*info.interval-1 do begin
        if (j eq fix(info.interval)) then  begin
          whereConRad = where(meanX eq DR[fix(info.interval)], dum)
          if (whereConRad[0] ne -1) then begin
            meanFrapPlot[fix(info.interval)] = mean(meanPlot[whereConRad])
          endif
        endif
        
       if(j lt fix(info.interval)) then jj = j
       if(j eq fix(info.interval)) then jj = j+1
       if(j gt fix(info.interval)) then jj = j+1 
       
       whereConRad = where(meanX ge DR[j] and meanX lt DR[j+1], dum)
          if (whereConRad[0] ne -1) then begin
              meanFrapPlot[jj] = mean(meanPlot[whereConRad])
          endif    
      endfor
      
      ; color the numC approximations in green
      plot, DR, meanFrapPlot, color = 1, thick = 2, xtitle = 'radius [pix]', ytitle = 'mean intensity', title = strcompress('mean intensity at tPos: ' + string(info.tPos * info.realDt, format = '(f0.2)') + ' [s]')
      
      *(info).DR = DR
     (*info.meanFrapPlot)[*,info.tPos]= transpose(meanFrapPlot)        
        
        end        
  endcase
  
  widget_control, info.saveID, sensitive = 1
  
  ; Turn draw Button events ON.
  ; widget_control, event.id, Draw_Button_Events = 1
  ; Returns control to Big Window
  ;  WSet, info.drawIndex
  widget_control, event.top, set_uvalue=info, /no_copy
  
  
end

pro s_Surface_Method_AnalyseFRAP_EVENT,event

  widget_control, event.id, GET_UVALUE = eventval
  
  case eventval of
    "XCOORD": begin
      widget_control, event.top, get_uvalue = info, /no_copy
      Widget_Control, info.field1, Get_Value=theValue
      info.xc = *theValue
      
    end
    "YCOORD": begin
      widget_control, event.top, get_uvalue = info, /no_copy
      widget_control, info.field2, Get_Value=theValue
      info.yc = *theValue
    ;        temp = *(event).value
    ;        info.ys = fix(temp)
    end
    "RAD": begin
      widget_control, event.top, get_uvalue = info, /no_copy
      widget_control, info.field3, Get_Value=theValue
      info.rad = *theValue
    ;        temp = *(event).value
    ;        info.rad = fix(temp)
    end
    "INTER": begin
      widget_control, event.top, get_uvalue = info, /no_copy
      widget_control, info.field4, Get_Value=theValue
      info.interval = *theValue
      
      ; check if the user wants to plot the line intensity
      lineButton = widget_info(info.calcLineID, /button_set)
      
      if (lineButton) then begin
            info.nCalcLine = 1
            info.nCalcCircle = 0
            info.DR = ptr_new(2*info.interval+1)
        
            if(ptr_valid(info.meanFrapPlot)) then begin
              ptr_free, info.meanFrapPlot
              info.meanFrapPlot = ptr_new(fltArr(2*info.interval+1,info.totalTNum),/no_copy)
            endif
      
      endif else begin
        
        info.DR = ptr_new(info.interval+1)
        info.nCalcLine = 0
        info.nCalcCircle = 1        
        
        if(ptr_valid(info.meanFrapPlot)) then begin
          ptr_free, info.meanFrapPlot
          info.meanFrapPlot = ptr_new(fltArr(info.interval+1,info.totalTNum),/no_copy)
        endif
      
      endelse
      
    end
    "ANGLE": begin
      widget_control, event.top, get_uvalue = info, /no_copy
      widget_control, info.field5, Get_Value=theValue
      info.angle = *theValue

    end
     "CLUS": begin
      widget_control, event.top, get_uvalue = info, /no_copy
      widget_control, info.field6, Get_Value=theValue
      info.angle = *theValue

    end
    'RESET': begin
    widget_control, event.top, get_uvalue = info, /no_copy
    widget_control, info.calcID, sensitive = 1
    widget_control, info.calcLineID, sensitive = 1
    widget_control, info.drawMaskID, sensitive = 1 ; 10Mar2011
    info.useMask = 0
    info.nCalcCircle = 1
    info.nCalcLine = 0
    widget_control, info.saveID, sensitive = 0
    widget_control, info.plotID, sensitive = 0
    
    end
    
    
  endcase
  
  ; set the new values into the info structure for calling
  ;later when drawing/calculating intensities
  widget_control, event.top, set_uvalue=info, /no_copy
  
end

pro s_Surface_Method_AnalyseFRAP_UPDATE, wTopBase, child_window_id, newImages = newImages, tPos = tPos, newMask = newMask
  ;  print, 'time selected: ', tPos
  widget_control, wTopBase, get_uValue = state, /no_copy
  ;   print, 'previously recorded time: ', state.tPos
  
  ; need to reassign time to the state.tPos struct
  state.tPos = tPos
  state.image = newImages
  state.mask = newMask ; 10Mar2011
  
  widget_control, wTopBase, set_uValue = state, /no_copy
  ; redraws the image
  s_Surface_Method_AnalyseFRAP_colors, {redraw_image, top : wTopBase}
  
 ; recalculates the concentric intensities
  
    s_Surface_Method_AnalyseFRAP_CALC, {WIDGET_DRAW, $
      ID: child_window_id, $
      TOP: wTopBase, $
      HANDLER:0, $
      TYPE:0, X:0,Y:0, $
      PRESS:1B, RELEASE:0B, CLICKS:0L, MODIFIERS:0L, CH:0, KEY:0L}
 
   
  widget_control, wTopBase, set_uValue = state, /no_copy
  
end


pro s_Surface_Method_AnalyseFRAP_DRAW_EVENTS, event

  ; This event handler continuously draws and erases the zoom box until it
  ; receives an UP event from the draw widget. then it turns draw widget
  ; motion events OFF.

  ; Get the info structure out of the top-level base.
  widget_control, event.top, get_uvalue = info, /no_copy
  
  ; What type of an event is this?
  possibleEventTypes = [ 'PRESS', 'RELEASE', 'MOTION', 'SCROLL' ]
  thisEvent = possibleEventTypes(event.type)
  
  case thisEvent of
    'PRESS': begin
      ; Is this the left or right button?
      ; if RIGHT, then map or unmap controls.
      buttonPressed = What_Button_Pressed(event)
      
      if buttonPressed eq 'RIGHT' then begin
        info.map = 1 - info.map
        widget_control, info.controlID, Map = info.map
        widget_control, event.top, set_uvalue=info, /no_copy
        ; MOR - 10Nov2010 - BEGIN
        widget_control, event.top, get_uvalue = info, /no_copy
        ; Turn draw Button events ON.
        widget_control, event.id, Draw_Button_Events=1
      
        if(info.nDraw) then begin
          ; create cross hairs
          x1 = info.xc
          x2 = x1+1
          y1 = info.yc
          y2 = y1+1
          WSet, info.drawIndex
          TVLCT, info.r, info.g, info.b
          Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]
          ; cross hairs
          (*info.pDrawObj[2])->setProperty,x1=x1, y1=y1, x2=x2, y2=y2,  color = info.colorIndex
          (*info.pDrawObj[2])->Draw
          ; frap circles
          (*info.pDrawObj[4])->setProperty, x = info.xc, y = info.yc, r = info.rad, color = info.colorIndex, dr = info.interval
          (*info.pDrawObj[4])->Draw
          ; frap line
          (*info.pDrawObj[5])->setProperty, x = info.xc, y = info.yc, r = info.rad, color = info.colorIndex, dr = info.interval, angle = info.angle
          (*info.pDrawObj[5])->Draw          
        endif
                widget_control, event.top, set_uvalue=info, /no_copy
        ; MOR - 10Nov2010 - END
        
        return
        endif ; end checking if right button release
        
        if(info.zoomDrawID) then begin
          ; if new object is drawn, set the static corners of the box to current cursor location.
          info.xs = event.x
          info.ys = event.y   
          
        widget_control, event.top, set_uvalue=info, /no_copy

        return
            
        endif
        
    endcase
    'RELEASE': begin
      ; Is this the left or right button? if RIGHT, then do nothing.
      buttonReleased = What_Button_Released(event)
      if buttonReleased eq 'RIGHT' then begin
        widget_control, event.top, set_uvalue=info, /no_copy
        ; MOR - 10Nov2010 - BEGIN
        widget_control, event.top, get_uvalue = info, /no_copy
        widget_control, event.top, set_uvalue=info, /no_copy
       
        return
        
        endif
        
        if ((info.Modecontrol eq 0) and info.nDrawZoom)  then begin
        
          widget_control, event.top, set_uvalue=info, /no_copy
          widget_control, event.top, get_uvalue = info, /no_copy

                    ; if this is an UP event, you need to erase the zoombox, restore
                    ; the user's color table, turn motion events OFF, and
                    ; draw the "zoomed" plot in both the draw widget and the pixmap.
                    ; Erase the zoombox one final time by copying the plot from the pixmap.
                 WSet, info.drawIndex
                 TVLCT, info.r, info.g, info.b
                 Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

                 ; Load a green color in color index 1 to draw the zoom box with.
                 TVLCT, 0B, 255B, 0B, info.colorIndex

                    ; Draw the "zoomed" image. Start by getting the LAST zoom
                    ; box outline. These are indices into image array.
                 event.x = 0 > event.x < (info.xsize - 1)
                 event.y = 0 > event.y < (info.ysize - 1)
                 x = [info.xs, event.x]
                 y = [info.ys, event.y]

                    ; Make sure the user didn't just click in the window.
                 if info.xs eq event.x OR info.ys eq event.y then begin
                    widget_control, event.top, set_uvalue=info, /no_copy
                    return
                 endif

                    ; Make sure the x and y values are ordered as [min, max].
                 if info.xs gt event.x then x = [event.x, info.xs]
                 if info.ys gt event.y then y = [event.y, info.ys]
                 
                 ; draw on image
                 info.xd = ( event.x > 0) < (info.xsize-1)
                 info.yd = ( event.y > 0) < (info.ysize-1)
                 (*info.pDrawObj(0))->setProperty, x1= info.xs, y1 = info.ys, x2= info.xd, y2 = info.yd, color = info.colorIndex
                 (*info.pDrawObj(0))->Draw

                   zoomFactor = info.zoomFactor
                   wTitle = strCompress('Zoomed Image Dimensions ('+ string(x(1)-x(0)) +' [x], '+ string(y(1)-y(0)) + ' [y] )')


                       ; Set the zoom factor and determine the new X and Y sizes of the Zoom Window.
                    zoomXSize = (x(1) - x(0) + 1) * zoomFactor
                    zoomYSize = (y(1) - y(0) + 1) * zoomFactor

                       ; Subset the image, and apply the zoom factor to it.
                    imageSubset = info.image(x(0):x(1), y(0):y(1))
                    zoomedImage = Congrid(imageSubset, zoomXSize, zoomYSize, Interp=info.interp)

                       ; if the Zoom Window exists, make it the proper size and load
                       ; the zoomed image into it. if it does not exists, create it.
                    if widget_info(info.zoomDrawID, /Valid_ID) then begin

                          ; Zoomed window exists. Make it correct size and load image.
                       widget_control, info.zoomDrawID, XSize=zoomXSize, YSize=zoomYSize, TLB_Set_Title = wTitle
                       WSet, info.zoomWindowID
                       if ptr_valid(info.subimage) then *info.subimage = BytScl(zoomedImage, Top=info.ncolors-1,$
                                                             max=max(info.image), min=min(info.image)) + info.bottom else  $
                                                          info.subimage = ptr_new(BytScl(zoomedImage, Top=info.ncolors-1,$
                                                             max=max(info.image), min=min(info.image)) + info.bottom)
                       tv, *info.subimage
                     endif else begin

                          ; Get offset positions for the non-existing zoom window.
                       widget_control, event.top, TLB_Get_Size=sizes, TLB_Get_Offset=offsets
                       xpos = sizes[0] + offsets[0] + 20
                       ypos = offsets[1] + 40

                          ; Zoom window does not exist. Create it.
                       zoomtlb = widget_base(Title = wTitle, Group=event.top, XOffset=xpos, YOffset=ypos)
                       zoomdraw = Widget_Draw(zoomtlb, XSize=zoomXSize, YSize=zoomYSize)
                       widget_control, zoomtlb, /Realize
                       widget_control, zoomdraw, Get_Value=windowID
                       info.zoomDrawID = zoomdraw
                       info.zoomWindowID = windowID
                       WSet, windowID
                       if ptr_valid(info.subimage) then  *info.subimage = BytScl(zoomedImage, Top=info.ncolors-1,$
                                                 max=max(info.image), min=min(info.image)) + info.bottom else  $
                                                 info.subimage = ptr_new(BytScl(zoomedImage, Top=info.ncolors-1,$
                                                 max=max(info.image), min=min(info.image)) + info.bottom)
                       tv, *info.subimage
                     endelse ; end checking for zoom draw id valid
                     
                     widget_control, event.top, set_uvalue=info, /no_copy
                     return
                 endif ; end checking for zoom draw id 
                 
                   ; Turn motion events off.
                 widget_control, event.id, Draw_Motion_Events=0

;                    ; if the controls were mapped, unmap them. -- this takes away the menu display
;                 if info.map eq 1 then begin
;                    widget_control, info.controlID, Map=0
;                    info.map = 0
;                 endif                             
        
        widget_control, event.top, set_uvalue=info, /no_copy
        ; MOR - 10Nov2010 - END
       
      ;  RETURN
      ;endif ; end if statement about right button
        ; Returns control to Big Window
      WSet, info.drawIndex
    endcase
    'MOTION': begin
    
                  if info.nDrawZoom then begin
                    ; Erase old draw objects
                 WSet, info.drawIndex
                 TVLCT, info.r, info.g, info.b, info.bottom
                 Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

                    ; Load a green color in color index 1 to draw the zoom box with.
                 TVLCT, 255B, 0B, 0B, info.colorIndex

                 ; Update the dynamic corner of the zoom box to the current cursor location.
                    info.xd = ( event.x > 0) < (info.xsize-1)
                    info.yd = ( event.y > 0) < (info.ysize-1)
                 (*info.pDrawObj(0))->setProperty, x1= info.xs, y1 = info.ys, x2= info.xd, y2 = info.yd, color = info.colorIndex
                 (*info.pDrawObj(0))->Draw
              endif
    
   
      endcase
  endcase
  
  ; Put the info structure back into its storage location.
  widget_control, event.top, set_uvalue=info, /no_copy
end ; of s_Surface_Method_AnalyseFRAP_DRAW_EVENTS ******************************************************************



pro s_Surface_Method_AnalyseFRAP, image, ColorIndex = colorIndex, Bottom=bottom, path = path,$
    Group_Leader = group_leader, stack_tlb = stack_tlb, pPreState = pPreState, NColors = ncolors, NoInterpolation=interp,  xSize = xSize, ySize = ySize,$
    realXSize = realXSize, realYSize = realYSize, application_tlb = application_tlb, mask = mask
    
  if (n_elements(group_leader) eq 0) then group_leader = 0
  if (n_elements(stack_tlb) eq 0) then stack_tlb = group_leader
  
  ; On an error condition, return to the main level of IDL.
  On_Error, 1
  
  ;Check the validity of the group identifier.
  ngroup = n_elements(group_leader)
  if (ngroup ne 0) then begin
    if ((widget_info(group_leader, /valid_id)) ne 1) then begin
      print,'Error: the group identifier is not valid.  |--> Returning to the main application.'
      return
    endif
  endif else group_leader = 0l
  
  
  
  if (n_elements(path) eq 0) then path = 'C:/RSI'
  
  if n_params() eq 0 then begin
    filename = Filepath(SubDirectory=['examples','data'], 'worldelv.dat')
    OpenR, lun, filename, /Get_Lun
    image = bytArr(360,360)
    ReadU, lun, image
    Free_Lun, lun
  endif
  
  ; Make sure a window has been opened in this IDL session for accurate color number determination.
  Window, /Pixmap, XSize=10, YSize=10
  WDelete, !D.Window
  
  ; Check for keywords. Set defaults to size of image if necessary.
  szImage = size(image, /dim)
  szScreen = get_screen_size()
  if (n_elements(szImage) eq 2) then begin
    ;    if ((szImage[0] gt szScreen[0]) or ((szImage[1] gt szScreen[1]))) then begin
    ;       r_x = 1. * szImage[0]/szScreen[0]
    ;       r_y = 1. * szImage[1]/szScreen[1]
    ;       if (r_x gt r_y) then r_xy = r_x else r_xy = r_y
    ;       image = congrid(image, round(szImage[0]/r_xy), round(szImage[1]/r_xy))
    ;    endif
  
    xSize = (size(image, /dim))[0]
    ySize = (size(image, /dim))[1]
    if (n_elements(realXSize) eq 0) then begin
      realXSize = szImage[0]
      realYSize = szImage[1]
    endif
    if (realXSize eq -1) then begin
      realXSize = szImage[0]
      realYSize = szImage[1]
    endif
    
    if n_elements(ncolors) eq 0 then ncolors = (!D.N_colors - 2) < 254
    if n_elements(bottom) eq 0 then bottom = 0B
    if n_elements(colorIndex) eq 0 then colorIndex = (ncolors + bottom) < 255
    interp = 1 - keyWord_set(interp)
    
    ; set default values for the frap circles and lines measurements
    x0 = fix(xsize/2)
    y0 = fix(ysize/2)
    r0 = fix(100)
    inter0 = fix(10)
    ang0 = fix(0)
    clus0 = fix(0)
    
  endif else begin
    message, 'Image parameter must be 2D.'
    return
  endelse
  
  ; Works with 2D images.
  Device, Decomposed=0
  
  ; Create a top-level base for this program. No resizing of this base.
  tlb = widget_base(TLB_Frame_Attr=1)
  application_tlb = tlb ; Return parameter.

  
  ; Create two bases. One for controls and the other for the
  ; draw widget. Leave the control base unmapped for now.
  
  controlID = widget_base(tlb, Map=0, column=1)
  
  modeID = widget_button(controlID, Value='Operation Mode', /Menu)
  measureID = widget_button(modeID, Value='Measure Mode', uValue = 'MEASUREMODE', Event_Pro='s_Surface_Method_AnalyseFRAP_control', Sensitive = 0)
  zoomID = widget_button(modeID, Value='Zoom Mode', uValue = 'ZOOMMODE', Event_Pro='s_Surface_Method_AnalyseFRAP_control')
  factorString = ['2x', '3x', '5x', '8x']
  factors = [2, 3, 5, 8]
  zoomfactorID = Widget_DropList(controlID, Value = factorString, Event_Pro='s_Surface_Method_AnalyseFRAP_FACTOR', uValue=factors, Title='Zoom Factor', Sensitive = 0)
  
  colors = widget_button(controlID, Value='Load Colors', Event_Pro='s_Surface_Method_AnalyseXRays_colors')
  
  ; display coordinates, radius and number of concentric circles on which to calculate average intensities
  row1 = widget_base(controlID, /row, /frame)
  row2 = widget_base(controlID, /row, /frame)
  row3 = widget_base(controlID, /row, /frame)
  row4 = widget_base(controlID, /row, /frame)
  row5 = widget_base(controlID, /row, /frame)
  row6 = widget_base(controlID, /row, /frame)
  
  field1 = COYOTE_Field(row1, Title='X-Coordinate [pixels]:', LabelSize=100, Value=x0, $
    /IntegerValue, /CR_Only, UValue='XCOORD', Event_Pro = 's_Surface_Method_AnalyseFRAP_EVENT')
  field2 = COYOTE_Field(row2, Title='Y-Coordinate [pixels]:', LabelSize=100, Value=y0, $
    /IntegerValue, /CR_Only, UValue='YCOORD', Event_Pro = 's_Surface_Method_AnalyseFRAP_EVENT')
  field3 = COYOTE_Field(row3, Title='Radius [pixels]:', LabelSize=100, Value=r0, $
    /IntegerValue, /CR_Only, UValue='RAD',  Event_Pro = 's_Surface_Method_AnalyseFRAP_EVENT')
  field4 = COYOTE_Field(row4, Title='Number of Intervals:', LabelSize=100, Value=inter0, $
    /IntegerValue, /CR_Only, UValue='INTER', Event_Pro = 's_Surface_Method_AnalyseFRAP_EVENT')
  field5 = COYOTE_Field(row5, Title='Angle:', LabelSize=100, Value=ang0, $
    /IntegerValue, /CR_Only, UValue='ANGLE', Event_Pro = 's_Surface_Method_AnalyseFRAP_EVENT')   
  field6 = COYOTE_Field(row6, Title='Cluster:', LabelSize=100, Value=clus0, $
    /IntegerValue, /CR_Only, UValue='CLUS', Event_Pro = 's_Surface_Method_AnalyseFRAP_EVENT')    
    
  drawID = widget_button(controlID, Value='Draw FRAP Circles',  Event_Pro='s_Surface_Method_AnalyseFRAP_DRAW', UValue = 'DRAWFRAP')
  drawMaskID = widget_button(controlID, Value='Use Mask',  Event_Pro='s_Surface_Method_AnalyseFRAP_DRAWMASK', UValue = 'USEMASK')
  calcID = widget_button(controlID, Value='Calculate Intensities',  Event_Pro='s_Surface_Method_AnalyseFRAP_CALC',UValue = 'CALCFRAP')
  calcLineID = widget_button(controlID, Value='Calculate Line Intensities',  Event_Pro='s_Surface_Method_AnalyseFRAP_CALC',UValue = 'CALCFRAPLINE')
  saveID = widget_button(controlID, Value='Save to File',  Event_Pro='s_Surface_Method_AnalyseFRAP_SAVE',UValue = 'SAVEFRAP', sensitive = 0)
  plotID = widget_button(controlID, Value='Plot FRAP Results',  Event_Pro='s_Surface_Method_AnalyseFRAP_SAVE',UValue = 'PLOTFRAPRESULTS', sensitive = 0)
  resetID = widget_button(controlID, Value='Reset Fields',  Event_Pro='s_Surface_Method_AnalyseFRAP_EVENT',UValue = 'RESET', sensitive = 1)
  exitID = widget_button(controlID, Value='Exit',  Event_Pro='s_Surface_Method_AnalyseFRAP_QUITTER')
  
  drawbase = widget_base(tlb, map=1)
  wDrawWindow = widget_draw(drawbase, XSize = xSize, YSize = ySize,$
    Button_Events=1, Event_Pro='s_Surface_Method_AnalyseFRAP_DRAW_EVENTS')
  
  ; do not use mask as default
  useMask = 0
  nCalcCircle = 1
  nCalcLine = 0
    
  ; Realize the program.
  widget_control, tlb, /Realize
  
  ; Get the window index number of the draw widget.
  ; Make the draw widget the current graphics window
  ; and display the image in it.
  widget_control, wDrawWindow, Get_Value = drawIndex
  WSet, drawIndex
  tv, BytScl(image, Top=ncolors-1, max=max(image), min=min(image)) + bottom
  
  ; Set the title of the window.
  widget_control, tlb, TLB_Set_Title = strCompress('Image Dimensions: ('+ string(xsize) + ' / ' + string(realXSize) + ' [Pix/Real in x]  ||  ' +  $
    string(ysize) + ' / ' + string(realYSize) + ' [Pix/Real in y] )')
    
  ; Create a pixmap window the same size as the draw widget window.
  ; Store its window index number in a local variable. Display the
  ; image you just put in the draw widget in the pixmap window.
    
  Window, /Free, XSize=xsize, YSize=ysize, /Pixmap
  pixIndex = !D.Window
  tv, BytScl(image, Top=ncolors-1, max=max(image), min=min(image)) + bottom
  
  ; Get color vectors for this application.
  TVLCT, r, g, b, /Get
  r = r(bottom:bottom+ncolors-1)
  g = g(bottom:bottom+ncolors-1)
  b = b(bottom:bottom+ncolors-1)
  
  ; get info. about current time and total number of time slices - BEGIN
  widget_control, group_leader, get_uValue = state, /no_copy
  (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
  pathname = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]
  totalTNum = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Total Number of Times'))[0]]
  realDt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Time Interval [s]'))[0]]
  ; try to get segmentation

   ;tPos = [tPos, tPos]
   ;chPos = [chPos, chPos]
   ;zPos = [zPos, zPos]
   clusPos = clus0
   if (n_elements(clusPos) eq 0) then mask = (*state.pImageStackInfoObject)->applyImageSegmentation(tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clus0) 
  
  
  widget_control, state.wListTime, get_uValue = uValueList, /no_copy
  tPos = uValueList.active
  totalTNum = n_elements(*uValueList.value)
  widget_control, state.wListTime, set_uValue = uValueList, /no_copy
  
  ; get info. about current time and total number of time slices - END
  
  ; Create an info structure to hold information required by the program.
  info = {Modecontrol: 1,$  ; A flag to activate/desactivate the cut option (1/ON, 0/OFF)
    groupLeader: group_leader,$ ;
    path: pathname,$ ; mor- used for saving results
    wDrawWindow : wDrawWindow ,$
    image:image,$               ; The original image.
    mask:mask,$                 ; mask of image
    count:0,$
    rad:fix(r0),$
    pXRadData:ptr_new(),$    ; Pointer on X-Ray-Data
    pXRadData2:ptr_new(),$   ; Pointer on X-Ray-Data
    fNewSquare: 0              ,$ ; Flag for New Square (1/ON, 0/OFF)
    pDrawObj:ptrArr(6),$       ; Pointerarray on Draw Objects. (0/Square, 1/Circle, 2/Cross, 3/Angle, 4/FRAP)
    row1:row1,$
    field1:field1,$
    field2:field2,$
    field3:field3,$
    field4:field4,$
    field5:field5,$
    field6:field6,$
    interval:fix(inter0),$
    angle:fix(ang0), $
    clusPos:fix(clus0), $
    nfield1:1,$
    nfield2:1,$
    nfield3:1,$
    nfield4:1,$
    drawID:drawID,$
    nDraw:1,$
    nDrawZoom:1,$
    calcID:calcID,$
    calcLineID:calcLineID,$
    drawMaskID:drawMaskID,$
    useMask:useMask,$
    nCalcCircle:nCalcCircle,$
    nCalcLine:nCalcLine,$
    nCalc:0,$
    subimage:ptr_new(),$        ; The scaled and resized subimage.
    xsize:xsize,$               ; The x size of the image window.
    ysize:ysize,$               ; The y size of the image window.
    realXSize : realXSize,$ ; The total real x size of the image window.
    realYSize : realYSize,$ ; The total real y size of the image window.
    realDt:realDt, $ ; time interval 
    pmeanX:ptr_new() ,$
    pmeanPlot:ptr_new(),$
    ReducedSizeXleft: -1,$   ; Left x coordinate of the Surface chosen for further Analysis
    ReducedSizeXright: -1,$     ; Right x coordinate of the Surface chosen for further Analysis
    ReducedSizeYupper: -1,$     ; Upper y coordinate of the Surface chosen for further Analysis
    ReducedSizeYlower: -1,$     ; Lower y coordinate of the Surface chosen for further Analysis
    drawIndex:drawIndex,$       ; The draw window index number.
    pixIndex:pixIndex,$         ; The pixmap window index number.
    ncolors:ncolors,$           ; The number of colors for the image.
    bottom:bottom,$             ; The bottom color index.
    colorIndex:colorIndex,$     ; The drawing color index.
    xs:0.,$                      ; X static corner of the zoom box OR X static centre of circle
    ys:0.,$                      ; Y static corner of the zoom box OR X static centre of circle
    xc:fix(x0),$                      ;  X static centre of circle
    yc:fix(y0),$                      ; Y static centre of circle
    xd:0.,$                      ; X dynamic corner of the zoom box OR X dynamic perimeter line  of circle
    yd:0.,$                      ; Y dynamic corner of the zoom box OR X dynamic  perimeter line of circle
    saveID: saveID, $            ; Save Data Button ID
    plotID:plotID, $             ; Plot intensity results vs. time
    measureID:measureID,$        ; Measure - Button ID.
    ;frapID:frapID,$            ; FRAP parameter ID
    zoomID:zoomID,$             ; Zoom - Button ID.
    zoomfactorID:zoomfactorID,$ ; ZoomFaktor - Button ID.
    zoomDrawID:-1L,$            ; Zoomed image draw widget ID.
    zoomWindowID:-1,$           ; Zoomed image window index number.
    r:r,$                       ; The red color vector.
    g:g,$                       ; The green color vector.
    b:b,$                       ; The blue color vector.
    r_old:0,$                   ; The user's red color value.
    g_old:0,$                   ; The user's green color value.
    b_old:0,$                   ; The user's blue color value.
    zoomfactor:2,$              ; The initial zoom factor.
    interp:interp,$             ; A flag to select nearest neighbor or bilinear resampling.
    map:0,$                     ; A flag to tell if the controls are mapped.
    id_RayScan: '',$
    controlID:controlID,$       ; The identifier of the control base to map.
    tPos:tPos,$
    totalTNum:totalTNum, $
    meanFrapPlot: ptr_new(FltArr(inter0+1,totalTNum),/no_copy),$
    DR : ptr_new(fix(inter0)+1,/no_copy),$ 
    resetID:resetID}
    
  if NOT(ptr_valid(info.pDrawObj[1])) then  begin
    info.pDrawObj[1] = ptr_new(obj_new('C_sCircle'), /no_copy)
    info.pDrawObj[2] = ptr_new(obj_new('C_sCross'), /no_copy)
    info.pDrawObj[3] = ptr_new(obj_new('C_sAngle'), /no_copy)
    info.pDrawObj[4] = ptr_new(obj_new('C_sFrapCircle'), /no_copy)
    info.pDrawObj[5] = ptr_new(obj_new('C_sFrapLine'), /no_copy)
  endif
  
  ; Store the info structure in the user value of the top-level base.
  widget_control, tlb, set_uvalue=info, /no_copy
  
  ; Register this program and set up the event loop.
  XManager, 's_Surface_Method_AnalyseFRAP', tlb, cleanUp='s_Surface_Method_AnalyseFRAP_cleanup', group_leader = group_leader, /No_Block
  
  
end ; of s_Surface_Method_AnalyseFRAP ****************************************************************************
