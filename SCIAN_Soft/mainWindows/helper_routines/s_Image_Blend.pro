pro s_channel_blend_Output, ev
   widget_control, ev.top, get_uValue = state, /no_copy
    state.thisWindow->getProperty, Image_Data = snapshot
    widget_control, ev.id, get_uValue = whichFileType
    case whichFileType of
        'PNG' :  begin
           filename = dialog_pickfile(/Write, File = 'idl.png')
           if filename ne '' then Write_PNG, filename, snapshot
       end
        'JPEG' :  begin
          filename = dialog_pickfile(/Write, File = 'idl.jpg')
           if filename ne '' then Write_JPEG, filename, snapshot, True = 1
       end
    endcase
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_channel_blend_colors, ev
   widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uNum, /no_copy
      if tag_names(ev, /structure_name) eq 'XCOLORS_LOAD' then begin
      iName = strCompress('Image_Channel' + string(uNum), /rem)
        oImage = (state.thisView->get(position = 0))->getByName(iName)
      oImage->getProperty, Data = thisData, /no_copy
      thisData[0,*, *] = ev.r[state.images[uNum,*,*]]
      thisData[1,*, *] = ev.g[state.images[uNum,*,*]]
      thisData[2,*, *] = ev.b[state.images[uNum,*,*]]
      thisData[3, *, *] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
      oImage->setProperty, Data = thisData, /no_copy
      state.thisWindow->draw, state.thisView
     endif else begin
      XColors, NotifyID = [ev.id, ev.top], group_leader = ev.top, title = strCompress('Color_Table_Ch_' + string(uNum)), xOffset = 100, yOffset = 100;, nColors = state.ncolors, bottom = state.bottom
     endelse
    widget_control, ev.id, set_uValue = uNum, /no_copy
  widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_channel_blend_CleanUp, id
  widget_control, id, get_uValue = state
  if n_elements(state) ne 0 then obj_destroy, state.thisContainer
end


pro s_channel_blend_slider, ev
   widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uNum, /no_copy
     iName = strCompress('Image_Channel' + string(uNum), /rem)
     oImage = (state.thisView->get(position = 0))->getByName(iName)
     oImage->getProperty, Data = thisData, /no_copy
     state.blendValues[uNum] = ev.value
     thisData[3, *, *] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
     oImage->setProperty, Data = thisData, /no_copy
     state.thisWindow->draw, state.thisView
    widget_control, ev.id, set_uValue = uNum, /no_copy
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_channel_blend_expose, ev
   widget_control, ev.top, get_uValue = state, /no_copy
    state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_channel_blend_Event, ev
  widget_control, ev.top, get_uValue = state, /no_copy
    state.thisWindow->setProperty, dimension = [ev.x, ev.y*state.drawScale+15]
    state.thisWindow->draw, state.thisView
  widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_channel_blend, images
  if (n_elements(images) eq 0) then begin
   filename = filePath(SubDir = ['examples', 'data'], 'worldelv.dat')
   openR, lun, filename, /Get_LUN
   backgroundImage = bytArr(360,360)
   readU, lun, backgroundImage
   free_lun, lun
   filename = filePath(SubDir = ['examples', 'data'], 'ctscan.dat')
   openR, lun, filename, /Get_LUN
   foregroundImage = bytArr(256,256)
   readU, lun, foregroundImage
   free_lun, lun
   images = bytArr(2, 360, 360)
   images[0,*,*] = backgroundImage
   images[1,*,*] = congrid(foregroundImage, 360,360)
  endif
  dim_images = size(images, /dim)

  alpha_image = bytArr(4, dim_images[1],dim_images[2])
  opacMasks = make_array(dim_images[0], dim_images[1],dim_images[2], /byte, value = 1b)
  blendValues = make_array(dim_images[0], /byte, value = 128b)
  whereOpacNotZero = where(images eq 0)
  if (whereOpacNotZero[0] ne -1) then opacMasks(whereOpacNotZero) = 0b
  colorPalette = [0, 3, 8, 1]
  thisContainer = obj_new('IDL_Container')
  thisModel = obj_new('IDLgrModel')
  thisContainer->add, thisModel
  for i = 0, dim_images[0]-1 do begin
   if (i eq 0) then opacMasks[0, *, *] = 1b
   loadCT, colorPalette[i]
   tvLCT, r, g, b, /get
   alpha_image[0, *, *] = r[images[i,*,*]]
   alpha_image[1, *, *] = g[images[i,*,*]]
   alpha_image[2, *, *] = b[images[i,*,*]]
   alpha_image[3,*, *] = opacMasks[i,*, *] * blendValues[i]
   iName = strCompress('Image_Channel' + string(i), /rem)
   oImage = obj_new('IDLgrImage', alpha_image, dimensions = [dim_images[1],dim_images[2]], interleave = 0, blend_func = [3,4], name = iName)
   thisModel->add, oImage
   thisContainer->add, oImage
  endfor

      ; Create a view.
  viewRect = [0, 0, dim_images[1], dim_images[2]]
  thisView = obj_new('IDLgrView', Viewplane_Rect = viewRect)
  thisView->add, thisModel
  thisContainer->add, thisView

      ; Create the widgets for this program.
  tlb = widget_Base(title = '|-> s_Channel_Blend', MBar = menuBase, TLB_size_events = 1, column = 1)
  drawID = widget_Draw(tlb, xSize = dim_images[1], ySize = dim_images[2], graphics_level = 2, expose_events = 1, retain = 0, event_pro = 's_channel_blend_expose')

  for i = 0, dim_images[0]-1 do begin
   iTitle = strCompress('Change_Image_Colors_Ch ' + string(i), /rem)
   b = widget_button(tlb, value = iTitle, uValue = i, event_pro = 's_channel_blend_colors')
   iTitle = strCompress('Opacity_Control_Ch ' + string(i), /rem)
   sliderID = widget_slider(tlb, min = 0, max = 255, value = 128, title = iTitle, uValue = i, event_pro = 's_channel_blend_slider')
  endfor

     ; Get geometry information for resizing.
   tlbGeo = widget_info(tlb, /geometry)
   drawGeo = widget_info(drawID, /geometry)
   drawScale = float(drawGeo.scr_ySize) / tlbGeo.ySize

      ; realize the widgets and get the window object.
   widget_control, tlb, /realize
   widget_control, drawID, get_value = thisWindow
   thisWindow->draw, thisView
   thisContainer->add, thisWindow

   state = {thisContainer : thisContainer,$         ; The container object.
           thisWindow : thisWindow,$             ; The window object.
           images : images,$             ; The foreground image object.
           opacMasks : opacMasks,$               ; Masks for screening blending values.
           blendValues : blendValues,$
           drawScale : drawScale,$               ; The draw widget scaling factor.
           drawID : drawID,$                   ; The draw widget identifier.
           sliderID : sliderID,$                ; The slider widget identifier.
           thisView : thisView}                 ; The object view.

  widget_control, tlb, set_uValue = state, /no_copy
  XManager, 's_channel_blend', tlb, Cleanup = 's_channel_blend_Cleanup', group_leader = group, /No_Block
end