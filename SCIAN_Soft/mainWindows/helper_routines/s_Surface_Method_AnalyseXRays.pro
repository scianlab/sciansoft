;_____________________________IOISIOI____________________
; NAME:
;       s_Surface_Method_AnalyseXRays
;
; PURPOSE:
;       Analysis of X-Ray Spectra
;
; AUTHOR:
;     Dr. Steffen Härtel (10|2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;     s_Surface_Method_AnalyseXRays, image
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
;      s_Surface_Method_AnalyseXRays, image
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

pro s_Surface_Method_AnalyseXRays_control, event

     ; Event handler to select surface style.
widget_control, event.top, get_uvalue=info, /no_copy
     ; Event handler to select Button uValue.
widget_control, event.id, Get_Value=ButtonValue, /no_copy
case ButtonValue of
    'X-Ray Mode': begin
         info.fNewSquare = 0
         info.fNewCircle = 1
         info.fNewAngle = 0
         info.fAdjustCircle = 0
         info.fAdjustAngle = 0

         info.Modecontrol = 1 - info.Modecontrol            ; Switch Flag (1/0 ON/OFF) for Mode control
         widget_control, info.cutID, sensitive = 0
         widget_control, info.zoomID, sensitive = 1
         widget_control, info.zoomfactorID, sensitive = 0
         widget_control, info.xrayID, sensitive = 1

         if NOT(ptr_valid(info.pDrawObj[1])) then begin
          for i = 0,3 do begin
              if (obj_valid(*info.pDrawObj[i])) then obj_destroy, *info.pDrawObj[i]
              if (ptr_valid(info.pDrawObj[i])) then ptr_free, info.pDrawObj[i]
          endfor
          info.pDrawObj[1] = ptr_new(obj_new('C_sCircle'), /no_copy)
          info.pDrawObj[2] = ptr_new(obj_new('C_sCross'), /no_copy)
          info.pDrawObj[3] = ptr_new(obj_new('C_sAngle'), /no_copy)
         endif

      endcase
    'Zoom Mode': begin
         info.fNewCircle = 0
         info.fNewAngle = 0
         info.fAdjustCircle = 0
         info.fAdjustAngle = 0
         info.fNewSquare = 1
         info.Modecontrol = 1 - info.Modecontrol            ; Switch Flag (1/0 ON/OFF) for Mode control
         widget_control, info.cutID, sensitive = 1
         widget_control, info.zoomID, sensitive = 0
         widget_control, info.zoomfactorID, sensitive = 1
         widget_control, info.xrayID, sensitive = 0

         if NOT(ptr_valid(info.pDrawObj(0))) then info.pDrawObj[0] = ptr_new(obj_new('C_sSquare'), /no_copy)
      endcase
    'New Circle': begin
         info.fNewCircle = 1
         info.fNewAngle = 0
         info.fAdjustCircle = 0
         info.fAdjustAngle = 0

         widget_control, info.newcircleID, sensitive =0
         widget_control, info.newangleID, sensitive = 1
         widget_control, info.adjustcircleID, sensitive = 1
         widget_control, info.adjustangleID, sensitive = 1

         if (ptr_valid(info.pDrawObj[1])) then begin
          if (obj_valid(*info.pDrawObj[1])) then obj_destroy, *info.pDrawObj[1]
          if (obj_valid(*info.pDrawObj[2])) then obj_destroy, *info.pDrawObj[2]
          if (ptr_valid(info.pDrawObj[1])) then ptr_free, info.pDrawObj[1]
          if (ptr_valid(info.pDrawObj[2])) then ptr_free, info.pDrawObj[2]
          info.pDrawObj[1] = ptr_new(obj_new('C_sCircle'), /no_copy)
          info.pDrawObj[2] = ptr_new(obj_new('C_sCross'), /no_copy)
         endif
      endcase
    'New Angle': begin
         info.fNewCircle = 0
         info.fNewAngle = 1
         info.fAdjustCircle = 0
         info.fAdjustAngle = 0

         widget_control, info.newcircleID, sensitive =1
         widget_control, info.newangleID, sensitive = 0
         widget_control, info.adjustcircleID, sensitive = 1
         widget_control, info.adjustangleID, sensitive = 1

         if (ptr_valid(info.pDrawObj[3])) then begin
          if (obj_valid(*info.pDrawObj[3])) then obj_destroy, *info.pDrawObj[3]
          ptr_free, info.pDrawObj[3]
          info.pDrawObj[3] = ptr_new(obj_new('C_sAngle'), /no_copy)
         endif

      endcase
    'Adjust Circle': begin
         if NOT(ptr_valid(info.pDrawObj[1])) then  begin
         print, 'define circle first'
         endif else begin

          info.fNewCircle = 0
          info.fNewAngle = 0
          info.fAdjustCircle = 1
          info.fAdjustAngle = 0

          widget_control, info.newcircleID, sensitive =1
          widget_control, info.newangleID, sensitive = 1
          widget_control, info.adjustcircleID, sensitive = 0
          widget_control, info.adjustangleID, sensitive = 1

          if (ptr_valid(info.pDrawObj[3])) then begin
              if (obj_valid(*info.pDrawObj[3])) then obj_destroy, *info.pDrawObj[3]
              ptr_free, info.pDrawObj[3]
              info.pDrawObj[3] = ptr_new(obj_new('C_sAngle'), /no_copy)
          endif

          (*info.pDrawObj[1])->Draw
          (*info.pDrawObj[1])->getProperty, x= info.xs, y = info.ys, r = r
          info.xd = info.xs + r
          info.yd = 0
         endelse
      endcase
    'Save Data': begin
          ; Parameter_Save
          file = pickfile(/write, path=info.path , filter='*.dat', file = 'file Name', /noconf)
          if (file eq '') OR (file eq (info.path+'\file Name')) then begin
              a = s_apop_shout('Not saved !')
          endif else begin
              info.path = strMid(file,0,(rStrPos(file,'\'))+1)

                 if (ptr_valid(info.pXRadData)) then begin
                   for i = 0, info.count-1 do begin
                    openW,2 , strCompress(file+strcompress(string(i)+'.dat', /rem))
                    printF, 2,  byte((*info.pXRadData)(i,*))
                    close, 2
                   endfor
                   openW,2 , strCompress(file + strcompress('_X_Axis.dat', /rem))
                   printF, 2,  transpose(fltarr((size(*info.pmeanX, /dim))[0] ) + *info.pmeanX)
                   close, 2
                   openW,2 , strCompress(file + strcompress('_Mean.dat', /rem))
                   printF, 2,  transpose(fltarr((size(*info.pmeanPlot, /dim))[0] ) + *info.pmeanPlot)
                   close, 2

                   a = s_apop_shout(info.path)
                 endif else    begin
                   a = s_apop_shout(' Nicht gespeichert')
                 endelse
              ;info.pXRadData
          endelse
      endcase
endcase

    ;Put the info structure back.
widget_control, event.top, set_uvalue=info, /no_copy
end
;-s_Surface_Method_AnalyseXRays_control, event-----------------------------------------------------------------shaertel-2001-



pro s_Surface_Method_AnalyseXRays_colors, event

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
          endif

          if (info.Modecontrol ne 1) then begin
            ; Draw the zoom box.
             WSet, info.drawIndex
             TVLCT, info.r, info.g, info.b, info.bottom
             Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]
             TVLCT, 0B, 255B, 0B, info.colorIndex
             PlotS, [info.xs, info.xs, info.xd, info.xd, info.xs], [info.ys, info.yd, info.yd, info.ys, info.ys], /Device, Color = info.colorIndex

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



pro s_Surface_Method_AnalyseXRays_QUITTER, event
    s_Surface_Method_AnalyseXRays_cleanup, event.top
    if widget_info(event.top, /valid_id) then widget_control, event.top, /destroy
end ;*******************************************************************


pro s_Surface_Method_AnalyseXRays_cleanup, tlb

    widget_control, tlb, get_uvalue = state, /no_copy

    if n_elements(state) NE 0 then begin
       for i = 0, (size(state.pDrawObj, /dim))[0]-1 do begin
         if (ptr_valid((state.pDrawObj)[i])) then begin
          if (obj_valid(*(state.pDrawObj)[i])) then obj_destroy, *(state.pDrawObj)[i]
          ptr_free, (state.pDrawObj)[i]
         endif
       endfor

       for i = 0, n_tags(state)-1 do begin
           ; XXX FIX CHECK THE USE FOR .(i)
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
          if (s_ToggleButtonOnOffState(stateParent.wXRayButtonOnOff) eq 1) then void = s_ToggleButtonOnOffState(stateParent.wXRayButtonOnOff)
          widget_control, state.groupLeader, set_uvalue = stateParent, /no_copy
         endif
    endif

    widget_control, tlb, set_uvalue = state, /no_copy
    if widget_info(tlb, /valid_id) then widget_control, tlb, /destroy

end ; **s_Surface_Method_AnalyseXRays_cleanup **********************************************************



pro s_Surface_Method_AnalyseXRays_FACTOR, event
       ; The purpose of this event handler is to set the zoom factor.
    widget_control, event.top, get_uvalue=info, /no_copy
    widget_control, event.id, get_uvalue=factor
    info.zoomfactor = factor(event.index)
    widget_control, info.controlID, Map=0
    info.map = 0
    widget_control, event.top, set_uvalue=info, /no_copy
end ; **s_Surface_Method_AnalyseXRays_FACTOR **********************************************************



pro s_Surface_Method_AnalyseXRays_BUTTON_DOWN, event

   ; This event handler ONLY responds to button down events from the
   ; draw widget. if it gets a DOWN event, it does three things: (1) sets
   ; the static and dynamic corners of the zoom box, (2) changes the
   ; event handler for the draw widget to ZOOMBOX_DRAWBOX and turns on MOTION
   ; events, and (3) takes over color index 1 of the color table for the
   ; zoom box drawing color.
   ; Put the info structure back into its storage location.

    widget_control, event.top, set_uvalue=info, /no_copy
end ; of s_Surface_Method_AnalyseXRays_BUTTON_DOWN *****************************************************


pro s_Surface_Method_AnalyseXRays_DRAW_EVENTS, event

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
                    return
                 endif

                     ; if new object is drawn, set the static corners of the box to current cursor location.
                 if info.fNewSquare or info.fNewCircle then begin
                   info.xs = event.x
                   info.ys = event.y
                 endif

                    ; if new angle object is drawn, set the  corners of the angle to current cursor location.
                 if (info.fNewAngle) then begin
                    if (ptr_valid(info.pDrawObj[3])) then begin
                    if (obj_valid(*info.pDrawObj[3])) then obj_destroy, *info.pDrawObj[3]
                    ptr_free, info.pDrawObj[3]
                    info.pDrawObj[3] = ptr_new(obj_new('C_sAngle'), /no_copy)
                 endif

                    ; Load a green color in color index 1 to draw the zoom box with.
                  TVLCT, 255B, 0B, 0B, info.colorIndex

                  (*info.pDrawObj[1])->getProperty, x= xc, y = yc, r = rc
                  info.xs = event.x
                  info.ys = event.y
                  (*info.pDrawObj[3])->setProperty, xcenter = xc, ycenter = yc,  color = info.colorIndex, xalpha1 = info.xs,  yalpha1 = info.ys
                  (*info.pDrawObj[3])->Draw
                  endif

                       ; if circle is to be choose radius or center adjustment.
                 if ( info.fAdjustCircle ) then begin
                       ; Update the static center of Circle/Cross or Angle-Object
                    (*info.pDrawObj[1])->getProperty, x= x, y = y, r = r

                   if ( ( SQRT( ABS(x- event.x)^2 +  ABS(y- event.y)^2 ) ) LT (r-2) ) then begin
                     info.fAdjustCircleRadCen = 1

                    if (event.x GT x) then info.xs += 1
                    if (event.x LT x) then info.xs -= 1
                    if (event.y GT y) then info.ys += 1
                    if (event.y LT y) then info.ys -= 1

                       info.xd = info.xd - ( x- info.xs)
                       info.yd = info.yd - ( y- info.ys)

                      WSet, info.drawIndex
                      TVLCT, info.r, info.g, info.b
                    Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]
                    (*info.pDrawObj[1])->setProperty, x= info.xs, y = info.ys
                    (*info.pDrawObj[1])->Draw
                    (*info.pDrawObj[2])->setProperty, x1= info.xs, y1 = info.ys, x2= info.xd, y2 = info.yd
                    (*info.pDrawObj[2])->Draw

                   endif else begin
                    info.fAdjustCircleRadCen = 0
                   endelse
                 endif

                  ; Turn draw MOTION events ON.
                 widget_control, event.id, Draw_Motion_Events=1

                 ; Take over the color index for the zoom box drawing color. Store the
                 ; current (r,g,b) values for color index so you can restore the
                 ; current colors after the zoom box is drawn.
                 TVLct, r, g, b, /Get
                 info.r_old = r[info.colorIndex]
                 info.g_old = g[info.colorIndex]
                 info.b_old = b[info.colorIndex]
             endcase
   'RELEASE': begin
                  ; Is this the left or right button? if RIGHT, then do nothing.
                 buttonReleased = What_Button_Released(event)
                 if buttonReleased eq 'RIGHT' then begin
                       widget_control, event.top, set_uvalue=info, /no_copy
                       RETURN
                 endif

                 if (info.Modecontrol eq 1) AND (info.fNewAngle OR info.fAdjustAngle) then begin      ; Decide if the image has to be cut or zoomed

                   pImageData = ptr_new(info.image)
                   pXYDistMatrix = ptr_new()
                   pRadMatrix = ptr_new()

                   (*info.pDrawObj[3])->getRadialIntensities,  pImageData = pImageData,$
                                                         pXYDistMatrix = pXYDistMatrix,$
                                                         pRadMatrix = pRadMatrix,$
                                                         xyRealSize = [info.realXSize, info.realYSize],$
                                                         rad = a, count = b

                   if (ptr_valid(pImageData)) then ptr_free, pImageData

                   if NOT(ptr_valid(pRadMatrix)) then begin
                       widget_control, event.top, set_uvalue=info, /no_copy
                    Return
                   endif

                   info.rad = a
                   info.count = b

                   wTitle = strCompress('X-Ray Analysis')

                       ; if the Zoom Window exists, make it the proper size and load
                       ; the zoomed image into it. if it does not exists, create it.
                    if Widget_Info(info.zoomDrawID, /ValID) then begin

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

                   wherePos = where((*pRadMatrix)[0,*] ne -1)
                   if (wherePos[0] ne -1) then  plot, (*pXYDistMatrix)[0, wherePos]  , (*pRadMatrix)[0, wherePos]
                   for i = 0, dimplot[1]-1 do begin
                      wherePos = where((*pRadMatrix)[i,*] ne -1)
                      if (wherePos[0] ne -1) then oplot, (*pXYDistMatrix)[i, wherePos] , (*pRadMatrix)[i,wherePos]
                   endfor

                   meanPlot = fltArr( dimplot[2] )
                   meanX = fltArr( dimplot[2] )
                   wherePos = where((*pRadMatrix)[*,0] ne -1)
                   if (wherePos[0] ne -1) then begin
                      meanPlot[0] = mean( (*pRadMatrix)[wherePos, 0] )
                      meanX[0] = mean( (*pXYDistMatrix)[wherePos, 0] )
                   endif
                   for i = 0, dimplot[2]-1 do begin
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


;                   minNewX = min(meanX, max = maxNewX) 
;                   xRange = [minNewX - .05*(maxNewX-minNewX), maxNewX + .05*(maxNewX-minNewX)]
;                   minPlot = min(meanPlot, max = maxPlot) 
;                   yRange = [minPlot - .05*(maxPlot-minPlot), maxPlot + .05*(maxPlot-minPlot)]
; 
                   oSys = _IDLitSys_GetSystem()
                   ; MOR - 20Oct2010 - do not use any iplots since they are locked open until you close the program - BEGIN COMMENT OUT
;                   if obj_valid(oSys->GetByIdentifier(info.id_RayScan)) then begin
;                      iplot, meanX, meanPlot, overplot = info.id_RayScan, xRange = xRange, yRange = yRange
;                   endif else begin
;                      iplot, meanX, meanPlot, identifier = id_RayScan, xTitle = 'distance', yTitle = 'mean intensity', xRange = xRange, yRange = yRange
;                      info.id_RayScan = id_RayScan
;                   endelse
                  ; MOR - 20Oct2010 - do not use any iplots since they are locked open until you close the program - END COMMENT OUT
            
                   oplot, meanX, meanPlot, color = 2, thick = 2

                   if widget_info(info.groupLeader, /valid) then widget_control, info.groupLeader, /sensitive

                   if (ptr_valid(info.pmeanX)) then ptr_free, info.pmeanX
                   if (ptr_valid(info.pmeanPlot)) then ptr_free, info.pmeanPlot
                   if (ptr_valid(info.pXRadData)) then ptr_free, info.pXRadData
                   info.pmeanX = ptr_new(meanX, /no_copy)
                   info.pmeanPlot = ptr_new(meanPlot, /no_copy)
                   info.pXRadData = ptr_new(*pRadMatrix, /no_copy)
                   if (ptr_valid(pRadMatrix)) then ptr_free, pRadMatrix
                   if (ptr_valid(pXYDistMatrix)) then ptr_free, pXYDistMatrix
                 endif
                 if (info.Modecontrol eq 0)  then begin

                    ; if this is an UP event, you need to erase the zoombox, restore
                    ; the user's color table, turn motion events OFF, and
                    ; draw the "zoomed" plot in both the draw widget and the pixmap.
                    ; Erase the zoombox one final time by copying the plot from the pixmap.
                 WSet, info.drawIndex
                 TVLCT, info.r, info.g, info.b
                 Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

                    ; Restore the user's color table.
                 TVLct, info.r_old, info.g_old, info.b_old, info.colorIndex

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

                   zoomFactor = info.zoomFactor
                   wTitle = strCompress('Zoomed Image Dimensions ('+ string(x[1]-x[0]) +' [x], '+ string(y[1]-y[0]) + ' [y] )')


                       ; Set the zoom factor and determine the new X and Y sizes of the Zoom Window.
                    zoomXSize = (x[1] - x[0] + 1) * zoomFactor
                    zoomYSize = (y[1] - y[0] + 1) * zoomFactor

                       ; Subset the image, and apply the zoom factor to it.
                    imageSubset = info.image(x[0]:x[1], y[0]:y[1])
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
                     endelse

                 endif

                   ; Turn motion events off.
                 widget_control, event.id, Draw_Motion_Events=0

                    ; if the controls were mapped, unmap them.
                 if info.map eq 1 then begin
                    widget_control, info.controlID, Map=0
                    info.map = 0
                 endif

                    ; Returns control to Big Window
                 WSet, info.drawIndex
               endcase
   'MOTION': begin

              if info.fNewSquare then begin
                    ; Erase old draw objects
                 WSet, info.drawIndex
                 TVLCT, info.r, info.g, info.b, info.bottom
                 Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

                    ; Load a green color in color index 1 to draw the zoom box with.
                 TVLCT, 255B, 0B, 0B, info.colorIndex

                 ; Update the dynamic corner of the zoom box to the current cursor location.
                    info.xd = ( event.x > 0) < (info.xsize-1)
                    info.yd = ( event.y > 0) < (info.ysize-1)
                 (*info.pDrawObj[0])->setProperty, x1= info.xs, y1 = info.ys, x2= info.xd, y2 = info.yd, color = info.colorIndex
                 (*info.pDrawObj[0])->Draw
              endif

             if info.fNewCircle then begin
                    ; Erase old draw objects
                 WSet, info.drawIndex
                 TVLCT, info.r, info.g, info.b, info.bottom
                 Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

                    ; Load a green color in color index 1 to draw the zoom box with.
                 TVLCT, 255B, 0B, 0B, info.colorIndex

                   ; Update the dynamic radius of the circle  to the current cursor location.
                 info.xd = event.x
                 info.yd = event.y

                 (*info.pDrawObj[1])->setProperty, x= info.xs, y = info.ys, r = ( SQRT( ABS(info.xs-info.xd)^2 +  ABS(info.ys-info.yd)^2 ) ), color = info.colorIndex
                 (*info.pDrawObj[1])->Draw
                 (*info.pDrawObj[2])->setProperty, x1= info.xs, y1 = info.ys, x2= info.xd, y2 = info.yd, color = info.colorIndex
                 (*info.pDrawObj[2])->Draw
              endif

              if ( info.fAdjustCircle ) then begin
                    ; Erase old draw objects
                 WSet, info.drawIndex
                 TVLCT, info.r, info.g, info.b, info.bottom
                 Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]
;               tv, BytScl(info.image, Top=info.ncolors-1, max=max(info.image), min=min(info.image)) + info.bottom

                     ; Update the static center of Circle/Cross or Angle-Object
                 (*info.pDrawObj[1])->getProperty, x= x, y = y, r = r

                 if ( info.fAdjustCircleRadCen ) then begin
                    info.xs = event.x
                    info.ys = event.y
                    info.xd = info.xd - ( x- info.xs)
                    info.yd = info.yd -  ( y- info.ys)

                   (*info.pDrawObj[1])->setProperty, x= info.xs, y = info.ys
                   (*info.pDrawObj[1])->Draw
                   (*info.pDrawObj[2])->setProperty, x1= info.xs, y1 = info.ys, x2= info.xd, y2 = info.yd
                   (*info.pDrawObj[2])->Draw
                 endif else begin
                    info.xd = event.x
                    info.yd = event.y

                   (*info.pDrawObj[1])->setProperty, r = ( SQRT( ABS(info.xs-info.xd)^2 +  ABS(info.ys-info.yd)^2 ) )
                   (*info.pDrawObj[1])->Draw
                   (*info.pDrawObj[2])->setProperty, x2= info.xd, y2 = info.yd
                   (*info.pDrawObj[2])->Draw
                 endelse
              endif

                  ; if new angle object is drawn, set the  corners of the angle to current cursor location.
              if (info.fNewAngle) then begin
                    ; Erase old draw objects
                 WSet, info.drawIndex
                 TVLCT, info.r, info.g, info.b, info.bottom
                 Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

                 info.xs = event.x
                 info.ys = event.y

                 (*info.pDrawObj[3])->setProperty,  xalpha2 = info.xs,  yalpha2 = info.ys
                 (*info.pDrawObj[3])->Draw
                 (*info.pDrawObj[1])->Draw
                 (*info.pDrawObj[2])->Draw
                 a = 0
                 (*info.pDrawObj[3])->getProperty,  r = a
                 print, 'Radius =', Round(a)
              endif

              if ( info.fAdjustAngle ) then (*info.pDrawObj[3])->Draw

        endcase
endcase

   ; Put the info structure back into its storage location.
widget_control, event.top, set_uvalue=info, /no_copy
end ; of s_Surface_Method_AnalyseXRays_DRAW_EVENTS ******************************************************************



pro s_Surface_Method_AnalyseXRays, image, ColorIndex = colorIndex, Bottom=bottom, path = path,$
   Group_Leader = group_leader, NColors = ncolors, NoInterpolation=interp,  xSize = xSize, ySize = ySize,$
   realXSize = realXSize, realYSize = realYSize, application_tlb = application_tlb

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
cutID = widget_button(modeID, Value='X-Ray Mode', uValue = 'CUTMODE', Event_Pro='s_Surface_Method_AnalyseXRays_control', Sensitive = 0)
zoomID = widget_button(modeID, Value='Zoom Mode', uValue = 'ZOOMMODE', Event_Pro='s_Surface_Method_AnalyseXRays_control')

xrayID = widget_button(controlID, Value='X-Ray Params', /Menu, Sensitive = 1)
newcircleID = widget_button(xrayID, Value='New Circle', uValue = 'NEWCIRCLE', Event_Pro='s_Surface_Method_AnalyseXRays_control', Sensitive = 0)
adjustcircleID = widget_button(xrayID, Value='Adjust Circle', uValue = 'ADJUSTCIRCLE', Event_Pro='s_Surface_Method_AnalyseXRays_control', Sensitive = 1)
newangleID = widget_button(xrayID, Value='New Angle', uValue = 'NEWANGLE', Event_Pro='s_Surface_Method_AnalyseXRays_control', Sensitive = 1)
adjustangleID = widget_button(xrayID, Value='Save Data', uValue = 'ADJUSTANGLE', Event_Pro='s_Surface_Method_AnalyseXRays_control', Sensitive = 1)

factorString = ['2x', '3x', '5x', '8x']
factors = [2, 3, 5, 8]
zoomfactorID = Widget_DropList(controlID, Value = factorString, Event_Pro='s_Surface_Method_AnalyseXRays_FACTOR', uValue=factors, Title='Zoom Factor', Sensitive = 0)

colors = widget_button(controlID, Value='Load Colors', Event_Pro='s_Surface_Method_AnalyseXRays_colors')

exitID = widget_button(controlID, Value='Exit',  Event_Pro='s_Surface_Method_AnalyseXRays_QUITTER')

drawbase = widget_base(tlb, map=1)
wDrawWindow = widget_draw(drawbase, XSize = xSize, YSize = ySize,$
                             Button_Events=1, Event_Pro='s_Surface_Method_AnalyseXRays_DRAW_EVENTS')

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
r = r[bottom:bottom+ncolors-1]
g = g[bottom:bottom+ncolors-1]
b = b[bottom:bottom+ncolors-1]

   ; Create an info structure to hold information required by the program.
info = {Modecontrol: 1,$  ; A flag to activate/desactivate the cut option (1/ON, 0/OFF)
    groupLeader: group_leader,$ ;
    path: path,$
    wDrawWindow : wDrawWindow ,$
   image:image,$               ; The original image.
   count:0,$
   rad:0,$
   pXRadData:ptr_new(),$    ; Pointer on X-Ray-Data
   pXRadData2:ptr_new(),$   ; Pointer on X-Ray-Data
   fNewSquare: 0              ,$ ; Flag for New Square (1/ON, 0/OFF)
   pDrawObj:ptrArr(4),$       ; Pointerarray on Draw Objects. (0/Square, 1/Circle, 2/Cross, 3/Angle)
   xrayID:xrayID,    $        ;         X-Ray-ID
   newcircleID:newcircleID,$       ; New-Circle-ID
   fNewCircle: 1              ,$ ; Flag for New Circle (1/ON, 0/OFF)
   newangleID:newangleID ,$     ; New-Angle-ID
   fNewAngle: 0               ,$; Flag for New Angle (1/ON, 0/OFF)
   adjustcircleID:adjustcircleID,$ ; Adjust-CircleID
   fAdjustCircle: 0           ,$; Flag for Adjust Circle (1/ON, 0/OFF)
   fAdjustCircleRadCen: 0             ,$; Flag to Adjust Circle(0/Radius, 1/Center)
   adjustangleID:adjustangleID,$    ; Adjust-AngleID
   fAdjustAngle: 0            ,$  ; Flag for Adjust Angle (1/ON, 0/OFF)
   subimage:ptr_new(),$        ; The scaled and resized subimage.
   xsize:xsize,$               ; The x size of the image window.
   ysize:ysize,$               ; The y size of the image window.
   realXSize : realXSize,$ ; The total real x size of the image window.
   realYSize : realYSize,$ ; The total real y size of the image window.
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
   xd:0.,$                      ; X dynamic corner of the zoom box OR X dynamic perimeter line  of circle
   yd:0.,$                      ; Y dynamic corner of the zoom box OR X dynamic  perimeter line of circle
   cutID:cutID,$      ; Cut - Button ID.
   zoomID:zoomID,$        ; Zoom - Button ID.
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
   controlID:controlID}         ; The identifier of the control base to map.

if NOT(ptr_valid(info.pDrawObj[1])) then  begin
    info.pDrawObj[1] = ptr_new(obj_new('C_sCircle'), /no_copy)
    info.pDrawObj[2] = ptr_new(obj_new('C_sCross'), /no_copy)
    info.pDrawObj[3] = ptr_new(obj_new('C_sAngle'), /no_copy)
endif

   ; Store the info structure in the user value of the top-level base.
widget_control, tlb, set_uvalue=info, /no_copy

   ; Register this program and set up the event loop.
XManager, 's_Surface_Method_AnalyseXRays', tlb, cleanUp='s_Surface_Method_AnalyseXRays_cleanup', group_leader = group_leader, /No_Block
end ; of s_Surface_Method_AnalyseXRays ****************************************************************************
