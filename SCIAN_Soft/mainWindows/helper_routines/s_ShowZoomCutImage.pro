;_____________________________IOISIOI____________________
; NAME:
;       s_Image_ShowZoomCut_Window
;
; PURPOSE:
;       Cut & Zoom Image
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       s_Image_ShowZoomCut_Window, imageData
;
;  KEYWORD PARAMETERS:
;        None
;_____________________________IOISIOI____________________


pro s_Image_SZC_updateCreateHistSegWin, wBaseID = wBaseID
    widget_control, wBaseID, get_uValue = state, /no_copy
       child_HistSegWin_tlb = state.child_HistSegWin_tlb
       subImage = *state.subImage
       stack_tlb = state.stack_tlb
       zoomWid_tlb = state.zoomDraw_tlb
    widget_control, wBaseID, set_uValue = state, /no_copy

    if not(widget_info(child_HistSegWin_tlb, /valid)) then begin
       widget_control, wBaseID, tlb_get_offset = offSets
       s_HistPlot_Window, subImage, groupLeader = wBaseID, stack_tlb = stack_tlb, basePosition = [offSets[0] + 35, offSets[1] + 35], application_tlb = application_tlb, zoomWid_tlb = zoomWid_tlb
       widget_control, wBaseID, get_uValue = state, /no_copy
         state.child_HistSegWin_tlb = application_tlb
       widget_control, wBaseID, set_uValue = state, /no_copy
    endif else s_HistPlot_Window_Update, histData = subImage, child_HistSegWin_tlb
end


function s_Image_SZC_getSubImage, wBaseID = wBaseID
    widget_control, wBaseID, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       cut_x = [state.xs, state.xd]
       cut_y = [state.ys, state.yd]
    widget_control, wBaseID, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                              clusPos = clusPos, selClusName = selClusName, segPos = segPos
    fApplySeg = 0b
    fApplySavedClusterMask = 0b
    fApplyPaintedMask = 0b
    fApplySavedROI2D = 0b
    fApplySavedROI3D = 0b
    widget_control, stack_tlb, get_uValue = stateStack, /no_copy
       if widget_info(stateStack.child_SegmentationWindow_tlb, /valid) then begin
          widget_control, stateStack.child_SegmentationWindow_tlb, get_uValue = stateSegWin, /no_copy
             fApplySeg = stateSegWin.fApplySeg
             fApplySavedClusterMask = stateSegWin.fApplySavedClusterMask
             fApplyPaintedMask = stateSegWin.fApplyPaintedMask
             fApplySavedROI2D = stateSegWin.fApplySavedROI2D
             fApplySavedROI3D = stateSegWin.fApplySavedROI3D
          widget_control, stateStack.child_SegmentationWindow_tlb, set_uValue = stateSegWin, /no_copy
       endif
    widget_control, stack_tlb, set_uValue = stateStack, /no_copy

    widget_control, wBaseID, get_uValue = state, /no_copy
       if not(fApplyPaintedMask) then state.fUpdatePaintedMaskPath = 1b
    widget_control, wBaseID, set_uValue = state, /no_copy

    if (fApplySeg or fApplySavedClusterMask or fApplyPaintedMask or fApplySavedROI2D or fApplySavedROI3D) then begin
       widget_control, wBaseID, get_uValue = state, /no_copy
          subImage = *state.image
       widget_control, wBaseID, set_uValue = state, /no_copy
       widget_control, stack_tlb, get_uValue = stateStack, /no_copy
          imageStackInfoObject = *stateStack.pImageStackInfoObject
       widget_control, stack_tlb, set_uValue = stateStack, /no_copy

         ; Apply new Segmentation
       if fApplySeg then begin
            subImage = imageStackInfoObject->applyImageSegmentation(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                                     clusPos = clusPos, segPos = segPos, cut_x = cut_x, cut_y = cut_y)
       endif
         ; Get saved Cluster Masks
       if fApplySavedClusterMask then begin
          widget_control, stack_tlb, get_uValue = stateStack, /no_copy
             oImage = (*stateStack.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
          widget_control, stack_tlb, set_uValue = stateStack, /no_copy

          oImage->get, pParamStruct = pParamStruct
          segXBox0 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]]
          segXBox1 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]]
          segYBox0 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]]
          segYBox1 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]]

          clusterMask = imageStackInfoObject->getSelectedClusterMask(tPos = tPos , chPos = chPos , zPos = zPos , clusPos = clusPos)
          if (clusterMask[0] eq -1) and (n_elements(clusterMask) eq 1) then begin
               clusterMask = subImage * 0
          endif
          dimCM = size(clusterMask ,/dim)
          if ((dimCM[0] ne (segXBox1-segXBox0+1)) or (dimCM[1] ne (segYBox1-segYBox0+1))) then clusterMask = congrid(clusterMask, (segXBox1-segXBox0+1), (segYBox1-segYBox0+1))

          subImage[segXBox0:segXBox1, segYBox0:segYBox1] = clusterMask
          subImage = subImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
       endif

         ; Get Painted Masks
       if fApplyPaintedMask then begin
          widget_control, wBaseID, get_uValue = state, /no_copy
             fUpdatePaintedMaskPath = state.fUpdatePaintedMaskPath
             paintedMaskPath = state.paintedMaskPath
          widget_control, wBaseID, set_uValue = state, /no_copy
          widget_control, stack_tlb, get_uValue = stateStack, /no_copy
             oImage = (*stateStack.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
          widget_control, stack_tlb, set_uValue = stateStack, /no_copy

          oImage->get, pParamStruct = pParamStruct
          segXBox0 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]]
          segXBox1 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]]
          segYBox0 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]]
          segYBox1 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]]

;Yoya_OLD
;          if fUpdatePaintedMaskPath then begin
;             file = dialog_pickfile(/read, path = paintedMaskPath[0], get_path = path, filter = '*.tif')
;             posLastBatch = strPos(path,'\', /reverse_s)
;             path = strMid(path,0, posLastBatch)
;             posNextBatch = strPos(path,'\', /reverse_s)
;             paintedMaskPath = [strMid(path, 0, posNextBatch+1), strMid(path, posNextBatch+1, posLastBatch-posNextBatch)]
;             fUpdatePaintedMaskPath = 0b
;          endif
;          pos_c = strPos(paintedMaskPath[1],'_c', /reverse_s)
;          pos_fill = strPos(paintedMaskPath[1],'_fill', /reverse_s)
;          nCell = strMid(paintedMaskPath[1],pos_c + 2, pos_fill - pos_c - 2)
;          nCellNum = s_getRightNumberFromString(nCell)
;          baseName = strMid(paintedMaskPath[1],0, pos_c + 2) + strCompress(string(nCellNum), /rem) + '_fill'
;          strZ = strCompress(string(zPos+1), /rem)
;          while (strLen(strZ) lt 2) do strZ = strCompress('0' + strZ, /rem)
;          tryName = paintedMaskPath[0] + baseName + '\' + baseName + '_z' + strZ + '.tif'
;          subImage[*] = 0
;          while query_tiff(tryName) do begin
;             thisImage = (read_tiff(tryName))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
;;             thisImage = (congrid((read_tiff(tryName)), 512,512))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
;             maxI = max(thisImage, min = minI)
;
;             if ((maxI eq minI) and (maxI eq 255)) then thisImage[*] = 0
;             if (maxI ne minI) then begin
;               whereCell = where(thisImage ne thisImage[0], count, complement = whereNoCell)
;               if (count gt 0) then subImage[whereCell] = nCellNum
;             endif
;
;             nCell += 1
;             nCellNum = s_getRightNumberFromString(nCell)
;             baseName = strMid(paintedMaskPath[1],0, pos_c + 2) + strCompress(string(nCellNum), /rem) + '_fill'
;             tryName = paintedMaskPath[0] + baseName + '\' + baseName + '_z' + strZ + '.tif'
;          endwhile
;          widget_control, wBaseID, get_uValue = state, /no_copy
;             state.fUpdatePaintedMaskPath = fUpdatePaintedMaskPath
;             state.paintedMaskPath = paintedMaskPath
;          widget_control, wBaseID, set_uValue = state, /no_copy
;       endif

          if fUpdatePaintedMaskPath then begin
             file = dialog_pickfile(/read, path = paintedMaskPath[0], get_path = path, filter = '*.tif')
             posLastBatch = strPos(path,'\', /reverse_s)
             path = strMid(path,0, posLastBatch)
             posNextBatch = strPos(path,'\', /reverse_s)
             paintedMaskPath = [strMid(path, 0, posNextBatch+1), strMid(path, posNextBatch+1, posLastBatch-posNextBatch)]
             fUpdatePaintedMaskPath = 0b
          endif
          pos_c = strPos(paintedMaskPath[1],'_c', /reverse_s)
          if (pos_c eq -1) then print, '_c is not present in mask-name !'  $
            else nCellNum = s_getRightNumberFromString(s_getLeftNumberFromStringAsString(strMid(paintedMaskPath[1],pos_c + 2)))
          baseName = strMid(paintedMaskPath[1],0, pos_c + 2) + strCompress(string(nCellNum), /rem)
          strZ = strCompress(string(zPos), /rem)
          strLen = strLen(strZ)

          for i  = strLen, 10 do begin
             if query_tiff(paintedMaskPath[0] + baseName + '\' + baseName + '_z' + strZ + '.tif') then strZNum = i
             strZ = '0' + strZ
          endfor
          strZ = strCompress(string(zPos), /rem)
          while (strLen(strZ) lt strZNum) do strZ = strCompress('0' + strZ, /rem)
          tryName = paintedMaskPath[0] + baseName + '\' + baseName + '_z' + strZ + '.tif'

          subImage[*] = 0
          while query_tiff(tryName) do begin
             print, 'load painted mask in:', tryName
             thisImage = (read_tiff(tryName))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
;             thisImage = (congrid((read_tiff(tryName)), 512,512))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]

             whereCell = where(thisImage ne thisImage[0], count, complement = whereNoCell)
             if (count gt 0) then subImage[whereCell] = nCellNum

             nCellNum += 1
             baseName = strMid(paintedMaskPath[1],0, pos_c + 2) + strCompress(string(nCellNum), /rem)
             tryName = paintedMaskPath[0] + baseName + '\' + baseName + '_z' + strZ + '.tif'
          endwhile
          widget_control, wBaseID, get_uValue = state, /no_copy
             state.fUpdatePaintedMaskPath = fUpdatePaintedMaskPath
             state.paintedMaskPath = paintedMaskPath
          widget_control, wBaseID, set_uValue = state, /no_copy
       endif

       if fApplySavedROI2D then begin
         oCurrROI2DGroup = 1
         s_ISM_getProjectInfo, stack_tlb = stack_tlb, oCurrROI2DGroup = oCurrROI2DGroup
         if not(obj_valid(oCurrROI2DGroup)) then oCurrROI2DGroup = s_ISegM_getROI2DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos)

         if obj_valid(oCurrROI2DGroup) then begin
            clusterMask = oCurrROI2DGroup->getGroupMask()
            dimCM = size(clusterMask, /dim)
            dimSI = size(subImage, /dim)
            if ((dimCM[0] ne dimSI[0]) or (dimCM[1] ne dimSI[1])) then subImage = congrid(clusterMask, dimSI) else subImage = clusterMask
         endif
         subImage = subImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
       endif

       if fApplySavedROI3D then begin
         oCurrROI3DGroup = 1
         s_ISM_getProjectInfo, stack_tlb = stack_tlb, oCurrROI3DGroup = oCurrROI3DGroup
         if not(obj_valid(oCurrROI3DGroup)) then oCurrROI3DGroup = s_ISegM_getROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos)

         if obj_valid(oCurrROI3DGroup) then begin
            clusterMask = oCurrROI3DGroup->getGroupMask(zPos = zPos)
            dimCM = size(clusterMask, /dim)
            dimSI = size(subImage, /dim)
            if ((dimCM[0] ne dimSI[0]) or (dimCM[1] ne dimSI[1])) then subImage = congrid(clusterMask, dimSI) else subImage = clusterMask
         endif
         subImage = subImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
;         subImage = subImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] gt 0
       endif

       widget_control, wBaseID, get_uValue = state, /no_copy
          if state.fCut then begin
             dimZM = size(subImage, /dim)
             if (size(subImage, /n_dim) lt 2) then begin
                subImage = bytArr(3,3)
                dimZM = size(subImage, /dim)
             endif
             state.zoomXSize = dimZM[0]
             state.zoomYSize = dimZM[1]
          endif else begin
               subImage = congrid(subImage, state.zoomXSize, state.zoomYSize, /minus, interp = state.fInterpolateZoom, /center)
          endelse
       widget_control, wBaseID, set_uValue = state, /no_copy

    endif else begin
       widget_control, wBaseID, get_uValue = state, /no_copy
         dimI = size(*state.image, /dim)
         if ((dimI[0] le state.xd) or (dimI[1] le state.yd)) then begin
           state.xd = dimI[0]-1
           state.yd = dimI[1]-1
         endif
         subImage = congrid((*state.image)[ state.xs:state.xd, state.ys:state.yd], state.zoomXSize, state.zoomYSize, /minus, interp = state.fInterpolateZoom, /center)
       widget_control, wBaseID, set_uValue = state, /no_copy
    endelse
    return, subImage
end


pro s_Image_SZC_updateZoomWindow, wBaseID = wBaseID
    widget_control, wBaseID, get_uValue = state, /no_copy
          ; if the Zoom window exists, make it the proper size and load the zoomed image into it. if it does not exists, create it.
       if widget_info(state.child_zoomWindow_tlb, /valid) then begin
          widget_control, state.child_zoomWindow_tlb, tlb_Set_Title = state.wTitle
          widget_control, state.zoomDraw_tlb, draw_XSize = state.zoomXSize, draw_YSize = state.zoomYSize
          wSet, state.zoomWindowID
          wShow, state.zoomWindowID
          if state.fTvScaleZoomWindow then begin
               ; Trouble when all image'pixels have the sames values... ever are draw in black... this is visible when use Hist or trheshold filter ...
               ; temporal correction FASL... i need verify... keep for now
               ; molesta para guardar imagenes, hablamos, steffen
               ; (*state.subImage)[0,0] = 0 
               tvscl, *state.subImage 
          endif else begin
               tv, uInt(*state.subImage * state.imageBitMapScale)
          endelse
       endif else begin
             ; Get Offset positions for the non-existing zoom window.
          widget_control, wBaseID, tlb_Get_Size = sizes, tlb_Get_Offset = offSets
          xpos = offSets[0] + state.xs + 20
          ypos = offSets[1] + sizes[1] - state.ys - 15

             ; Zoom window does not exist. Create it.
          wZoomBase = widget_base(Title = state.wTitle, XOffset = xpos, YOffset = ypos, group_leader = wBaseID, tlb_Frame_Attr = 1)
          wZoomDraw = widget_draw(wZoomBase, xSize = state.zoomXSize, ySize = state.zoomYSize, button_events = 1, event_pro = 's_Image_Zoom_Window_event')
          widget_control, wZoomBase, /realize
          widget_control, wZoomDraw, get_value = windowID
          state.zoomDraw_tlb = wZoomDraw
          state.child_zoomWindow_tlb = wZoomBase
          state.zoomWindowID = windowID
          wSet, windowID
          if state.fTvScaleZoomWindow then tvscl, *state.subImage else tv, uInt(*state.subImage * state.imageBitMapScale)

             ; if the controls were mapped, unmap them.
          if state.fMap then begin
             widget_control, state.wBaseID, map = 0
             state.fMap = 0
          endif

          widget_control, wBaseID, set_uValue = state, /no_copy
          widget_control, wZoomDraw, set_uValue = wBaseID
          XManager, 's_Image_Zoom_Window', wZoomBase, Group_Leader = wBaseID, /no_block
          if widget_info(wBaseID, /valid) then widget_control, wBaseID, get_uValue = state, /no_copy
       endelse
       mom = moment(*state.subImage)
       print, 'ROI-Statistics [mean(+-sd)] = ', mom[0], ' (+- ', sqrt(mom[1]), ')'
       print, 'ROI-Statistics [position in pixel]: ', strCompress(string(state.xs) + '-' + string(state.xd) , /rem) + ' [x]' + ' | ' + strCompress(string(state.ys) + '-' + string(state.yd) , /rem) + ' [y]'
       print, 'ROI-Statistics [area in pixel]: ', strCompress(string(state.xd - state.xs) , /rem) + ' [x]'  + '  * ' + strCompress(string(state.yd - state.ys) , /rem) + ' [y] = ' + $
                                                  strCompress(string(1.*(state.yd - state.ys) * (state.xd - state.xs)) , /rem)
       print, 'ROI-Statistics [pixel in ROI]: ', total(*state.subImage gt 0)

       if (n_elements(state) ne 0) then begin
         if (state.fSaveGreyResult or state.fSaveRGBResult or state.fSaveGreyResultByte) then begin
            stack_tlb = state.stack_tlb
            widget_control, wBaseID, set_uValue = state, /no_copy
            s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,clusPos = clusPos
            widget_control, wBaseID, get_uValue = state, /no_copy
            widget_control, state.stack_tlb, get_uValue = stateStack, /no_copy
            (*stateStack.pImageStackInfoObject)->get, pParamStruct = pParamStruct
            path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]
            numberIndexDigits = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]]
            widget_control, state.stack_tlb, set_uValue = stateStack, /no_copy

            strImageNumber = strCompress(string(tPos), /rem)
            numberIndexDigits = numberIndexDigits > strLen(strImageNumber)
            strTimePos = strCompress(string(tPos), /rem)
            while (strLen(strTimePos) lt 3) do strTimePos = strCompress('0'+strTimePos, /rem)
            while (strLen(strImageNumber) lt numberIndexDigits) do strImageNumber = strCompress('0'+strImageNumber, /rem)

;            if state.fSaveGreyResult then write_tiff, strCompress(path + 'GreyZoomImage_' +strCompress('clus' + string(clusPos)+'_', /rem) $
;                                               + strCompress('ch' + string(chPos)+'_', /rem) $
;                                               + strCompress('z' + string(zPos)+'_', /rem) $
;                                               + strCompress('t' + strTimePos, /rem) $
;                                               + '.tif', /rem), tvrd()
            if state.fSaveGreyResult then write_tiff, strCompress(path + 'GreyZoomImageFloat_' +strCompress('clus' + string(clusPos)+'_', /rem) $
                                               + strCompress('ch' + string(chPos)+'_', /rem) $
                                               + strCompress('z' + string(zPos)+'_', /rem) $
                                               + strCompress('t' + strTimePos, /rem) $
                                               + '.tif', /rem), *state.subImage, /float

            if state.fSaveGreyResultByte then write_tiff, strCompress(path + 'GreyZoomImageByte_' +strCompress('clus' + string(clusPos)+'_', /rem) $
                                               + strCompress('ch' + string(chPos)+'_', /rem) $
                                               + strCompress('z' + string(zPos)+'_', /rem) $
                                               + strCompress('t' + strTimePos, /rem) $
                                               + '.tif', /rem), *state.subImage
                                               
            if state.fSaveRGBResult then  write_tiff, strCompress(path + 'RGBZoomImage_' +strCompress('clus' + string(clusPos)+'_', /rem) $
                                               + strCompress('ch' + string(chPos)+'_', /rem) $
                                               + strCompress('z' + string(zPos)+'_', /rem) $
                                               + strCompress('t' + strTimePos, /rem) $
                                               + '.tif', /rem), tvrd(true = 1)
         endif

         if state.fUpdateXRay then begin
          widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
             ; Update X-Ray Window
;          s_Surface_Method_AnalyseXRays, *state.subImage
          if widget_info(stackState.child_XRayAnalysis_Window_tlb, /valid) then begin
             widget_control, stackState.child_XRayAnalysis_Window_tlb, get_uValue = childState, /no_copy
             XImage = *state.image
             dimSI = size(*state.subImage, /dim)
             if ((dimSI[0] eq (state.xd-state.xs+1)) and (dimSI[1] eq (state.yd-state.ys+1))) then $
                XImage[state.xs:state.xd, state.ys:state.yd] = *state.subImage else $
                XImage[state.xs:state.xd, state.ys:state.yd] = congrid(*state.subImage,(state.xd-state.xs+1),(state.yd-state.ys+1))
             childState.image = XImage
             modeControl = childState.modeControl
             child_window_id = childState.wDrawWindow
             widget_control, stackState.child_XRayAnalysis_Window_tlb, set_uValue = childState, /no_copy
             s_Surface_Method_AnalyseXRays_colors, {redraw_image, top :stackState.child_XRayAnalysis_Window_tlb}
             if modeControl then s_Surface_Method_AnalyseXRays_DRAW_EVENTS, {WIDGET_DRAW,$
                                                                             ID :child_window_id,$
                                                                             TOP :stackState.child_XRayAnalysis_Window_tlb,$
                                                                             HANDLER :0,$
                                                                             TYPE :1,$
                                                                             X  :0, Y :0,$
                                                                             PRESS  :0,$
                                                                             RELEASE:1,$
                                                                             CLICKS:1,$
                                                                             MODIFIERS :0,$
                                                                             CH :0,$
                                                                             KEY :0L}
            endif
            widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
        endif
        widget_control, wBaseID, set_uValue = state, /no_copy
     endif
end


pro s_Image_Zoom_Window_event, ev

    if ((ev.press eq 1) or (ev.press eq 4)) then begin
       widget_control, ev.id, get_uValue = stateParent, /no_copy
       widget_control, stateParent, get_uValue = state, /no_copy
       wSet, state.zoomWindowID
       if state.fCut then zoomFactor = 1 else zoomFactor = state.zoomFactor
       state.zoomXSize = (state.xd - state.xs + 1) * zoomFactor
       state.zoomYSize = (state.yd - state.ys + 1) * zoomFactor

       if (ev.press eq 1) then begin
          if state.fTvScaleZoomWindow then $
            tvscl, congrid((*state.image)[ state.xs:state.xd, state.ys:state.yd], state.zoomXSize, state.zoomYSize, /minus, Interp = state.fInterpolateZoom, /center) else $
            tv, congrid((uInt(*state.image * state.imageBitMapScale))[state.xs:state.xd, state.ys:state.yd], state.zoomXSize, state.zoomYSize, /minus, Interp = state.fInterpolateZoom, /center)

         if state.fKeepFilterMatrix then begin
          stack_tlb = state.stack_tlb
          fShowStandPC = state.fShowStandPC
          fShowCCRI = state.fShowCCRI
          fShowJMSI = state.fShowJMSI
          fShowCC = state.fShowCC
          fShowDeltaR = state.fShowDeltaR
          widget_control, stateParent, set_uValue = state, /no_copy
          s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos , chPos = chPos, zPos = zPos, clusPos = clusPos, segPos = segPos
          widget_control, stack_tlb, get_uValue = stateStack, /no_copy
             selectedSegObject = (*stateStack.pImageStackInfoObject)->getSelectedSegObj(tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, segPos = segPos)
          widget_control, stack_tlb, set_uValue = stateStack, /no_copy

          if obj_isa(selectedSegObject,'C_SIMAGEFILTER_IMAGECOLOCALIZATION') then begin
                 ; if segmented image does not exist, then create it ... !
              selectedSegObject->getMatrixValues, ch0_vect = ch0_vect, ch1_vect = ch1_vect, evX = ev.X, evY = ev.Y, alpha = alpha, kernelActVect = kernelActVect, kernelPasVect = kernelPasVect
              if (n_elements(ch0_vect) eq 0) then begin
                 widget_control, stateParent, get_uValue = state, /no_copy
                    cut_x = [state.xs, state.xd]
                    cut_y = [state.ys, state.yd]
                 widget_control, stateParent, set_uValue = state, /no_copy
                 widget_control, stack_tlb, get_uValue = stateStack, /no_copy
                    imageStackInfoObject = *stateStack.pImageStackInfoObject
                 widget_control, stack_tlb, set_uValue = stateStack, /no_copy
                 subImage = imageStackInfoObject->applyImageSegmentation(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                                         clusPos = clusPos, segPos = segPos, cut_x = cut_x, cut_y = cut_y)
                 widget_control, stateParent, get_uValue = state, /no_copy
                    *state.subImage = subImage
                 widget_control, stateParent, set_uValue = state, /no_copy
              endif

              PCParam = correlate(ch0_vect, ch1_vect, /double)
              linFitParam = linFit(ch0_vect, ch1_vect, /double, chisq = chisq, sigma = sigma)
              maxVec = max(ch0_vect, min =minVec)
              xFit = make_array(maxVec - minVec + 3, /float, /index) + (minVec - 1)
              if (finite(PCParam) eq 0) then yFit = xFit * floor(mean(ch1_vect)) else yFit = float(linFitParam[0] + xFit * linFitParam[1])
              print, '___________________________________'
              print, 'Scatter Histogram:'
              print, 'Results of Linear Regression:Y = A + B * X'
              print, 'A:', linFitParam[0], '         B:', linFitParam[1]
              print, 'Pearsson Coefficient:', PCParam
              print, '___________________________________'

                 ; Normal Scatter Histogram
              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> I-Scatter Histogram'
              if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> I-Scatter Histogram" does not  exist. Operation not performed.') ) then begin
                 style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type :3, symbol_size :0.1},$
                                    visualization_properties = {color :'Light Gray'},$
                                    xAxis_properties = {axisTitle :'Ch 0', exact:1, compute_range :0},$
                                    yAxis_properties = {axisTitle :'Ch 1', exact:1, compute_range :0},$
                                    legend_properties = {hide :1})
                 live_plot, ch1_vect, independent = ch0_vect,/scatter, draw_dimension = [300,300], /no_select,style = style,$
                                    title = 's_ImageColoc |-> I-Scatter Histogram'
                 live_oPlot, yFit, independent = xFit, subType = 'LinePlot', /no_select,$
                                    window_in = 's_ImageColoc |-> I-Scatter Histogram'
              endif else begin
                 live_control, yFit, /update, window_in = 's_ImageColoc |-> I-Scatter Histogram'
                 live_control, xFit, /update, window_in = 's_ImageColoc |-> I-Scatter Histogram'
                 live_control, ch1_vect, /update, window_in = 's_ImageColoc |-> I-Scatter Histogram'
                 live_control, ch0_vect, /update, window_in = 's_ImageColoc |-> I-Scatter Histogram'
              endelse

              live_info, 'CH0_VECT Axis', properties = variable, window_in = 's_ImageColoc |-> I-Scatter Histogram'
              maxVec = max(ch0_vect, min = minVec)
              variable.minRange = minVec - .05 * (maxVec-minVec)
              variable.maxRange = maxVec + .05 * (maxVec-minVec)
              variable.axisTitle = 'Ch 0 (PC = ' + strCompress(string(PCParam)) + ')'
              live_control, 'CH0_VECT Axis', properties = variable, window_in = 's_ImageColoc |-> I-Scatter Histogram'
              live_info, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> I-Scatter Histogram'
              maxVec = max(ch1_vect, min = minVec)
              variable.minRange = minVec - .05 * (maxVec-minVec)
              variable.maxRange = maxVec + .05 * (maxVec-minVec)
              live_control, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> I-Scatter Histogram'

                ; PC-Histogram
              ch0_vectPC = ch0_vect * kernelActVect / max(kernelActVect)
              ch1_vectPC = ch1_vect * kernelPasVect / max(kernelPasVect)

;              data = make_array(2, n_elements(ch0_vectPC), /float)
;              data [0,*] = ch1_vectPC
;              data [1,*] = ch0_vectPC
;              openW,2, s_getPathForSystem()+'ChannelPCData.dat'
;              printF, 2, ['Channel_1', 'Channel_2']
;              printF, 2, data
;              close, 2
;
;              widget_control, stateParent, get_uValue = state, /no_copy
;                 subImage = *state.subImage
;              widget_control, stateParent, set_uValue = state, /no_copy
;              subImage[0,0] = 1
;              subImage = subImage * (subImage gt 0)
;              live_image, subImage

              PCParam = correlate(ch0_vectPC, ch1_vectPC, /double)
              linFitParam = linFit(ch0_vectPC, ch1_vectPC, /double, chisq = chisq, sigma = sigma)
              maxVec = max(ch0_vectPC, min =minVec)
              xFit = make_array(maxVec - minVec + 3, /float, /index) + (minVec - 1)
              if (finite(PCParam) eq 0) then yFit = xFit * floor(mean(ch1_vectPC)) else yFit = float(linFitParam[0] + xFit * linFitParam[1])
              print, '___________________________________'
              print, 'PC Histogram:'
              print, 'Results of Linear Regression:Y = A + B * X'
              print, 'A:', linFitParam[0], '         B:', linFitParam[1]
              print, 'Pearsson Coefficient:', PCParam
              print, '___________________________________'

                 ; Normal PC Histogram
              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> PC Histogram'
              if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> PC Histogram" does not exist.  Operation not performed.') ) then begin
                 style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type :3, symbol_size :0.1},$
                                    visualization_properties = {color :'Light Gray'},$
                                    xAxis_properties = {axisTitle :'Ch 0', exact:1, compute_range :0},$
                                    yAxis_properties = {axisTitle :'Ch 1', exact:1, compute_range :0},$
                                    legend_properties = {hide :1})
                 live_plot, ch1_vectPC, independent = ch0_vectPC,/scatter, draw_dimension = [300,300], /no_select,style = style,$
                                    title = 's_ImageColoc |-> PC Histogram'
                 live_oPlot, yFit, independent = xFit, subType = 'LinePlot', /no_select,$
                                    window_in = 's_ImageColoc |-> PC Histogram'
              endif else begin
                 live_control, yFit, /update, window_in = 's_ImageColoc |-> PC Histogram'
                 live_control, xFit, /update, window_in = 's_ImageColoc |-> PC Histogram'
                 live_control, ch1_vectPC, /update, window_in = 's_ImageColoc |-> PC Histogram'
                 live_control, ch0_vectPC, /update, window_in = 's_ImageColoc |-> PC Histogram'
              endelse

              live_info, 'CH0_VECTPC Axis', properties = variable, window_in = 's_ImageColoc |-> PC Histogram'
              maxVec = max(ch0_vectPC, min = minVec)
              variable.minRange = minVec - .05*(maxVec-minVec)
              variable.maxRange = maxVec + .05*(maxVec-minVec)
              variable.axisTitle = 'Ch 0 (PC = ' + strCompress(string(PCParam)) + ')'
              live_control, 'CH0_VECTPC Axis', properties = variable, window_in = 's_ImageColoc |-> PC Histogram'
              live_info, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> PC Histogram'
              maxVec = max(ch1_vectPC, min = minVec)
              variable.minRange = minVec - .05*(maxVec-minVec)
              variable.maxRange = maxVec + .05*(maxVec-minVec)
              live_control, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> PC Histogram'
          endif

          if (fShowStandPC and obj_isa(selectedSegObject,'C_SIMAGEFILTER_IMAGECOLOCALIZATION')) then begin
              varX = (moment(ch0_vect^2))[0] - ((moment(ch0_vect))[0])^2 + alpha
              varY = (moment(ch1_vect^2))[0] - ((moment(ch1_vect))[0])^2 + alpha
              ch0_vectStand = (ch0_vect - (moment(ch0_vect))[0]) / (sqrt(varX) > sqrt(1./n_elements(ch0_vect)))
              ch1_vectStand = (ch1_vect - (moment(ch1_vect))[0]) / (sqrt(varY) > sqrt(1./n_elements(ch0_vect)))
              PCParamStand = correlate(ch0_vectStand, ch1_vectStand, /double)
              linFitParamStand = linFit(ch0_vectStand, ch1_vectStand, /double, chisq = chisq, sigma = sigma)
              xFitStand = make_array(max(ch0_vectStand) - min(ch0_vectStand) + 3, /float, /index) + (min(ch0_vectStand) - 1)
              if (finite(PCParamStand) eq 0) then yFitStand = xFitStand*floor(mean(ch0_vectStand)) else yFitStand = float(linFitParamStand[0] + xFitStand * linFitParamStand[1])

              print, '___________________________________'
              print, 'Standardized PC Histogram'
              print, 'Results of Linear Regression:Y = A + B * X'
              print, 'A:', linFitParamStand[0], '         B:', linFitParamStand[1]
              print, 'Standardized Pearsson Coefficient:', PCParamStand
              print, '___________________________________'

              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> PC-Standard Histogram'
              if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> PC-Standard Histogram" does not  exist. Operation not performed.') ) then begin
                 style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type :3, symbol_size :0.1},$
                                     visualization_properties = {color :'Light Red'},$
                                     xAxis_properties = {axisTitle :'Ch 0', exact:1, compute_range :0},$
                                     yAxis_properties = {axisTitle :'Ch 1', exact:1, compute_range :0},$
                                     legend_properties = {hide :1})
                 live_plot, ch1_vectStand, independent = ch0_vectStand,$
                                                         /scatter, draw_dimension = [300,300], /no_select,$
                                                         style = style,$
                                                         title = 's_ImageColoc |-> PC-Standard Histogram'
                 live_oPlot, yFitStand, independent = xFitStand,$
                                                      subType = 'LinePlot', /no_select,$
                                                      window_in = 's_ImageColoc |-> PC-Standard Histogram'
              endif else begin
                 live_control, xFitStand, /update, window_in = 's_ImageColoc |-> PC-Standard Histogram'
                 live_control, yFitStand, /update, window_in = 's_ImageColoc |-> PC-Standard Histogram'
                 live_control, ch1_vectStand, /update, window_in = 's_ImageColoc |-> PC-Standard Histogram'
                 live_control, ch0_vectStand, /update, window_in = 's_ImageColoc |-> PC-Standard Histogram'
              endelse

              live_info, 'CH0_VECTSTAND Axis', properties = variable, window_in = 's_ImageColoc |-> PC-Standard Histogram'
              maxVec = max(ch0_vectStand, min = minVec)
              variable.minRange = minVec - .05 * (maxVec-minVec)
              variable.maxRange = maxVec + .05 * (maxVec-minVec)
              variable.axisTitle = 'Standardized PC = ' + strCompress(string(PCParamStand))
              live_control, 'CH0_VECTSTAND Axis', properties = variable, window_in = 's_ImageColoc |-> PC-Standard Histogram'
              live_info, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> PC-Standard Histogram'
              maxVec = max(ch1_vectStand, min = minVec)
              variable.minRange = minVec - .05 * (maxVec-minVec)
              variable.maxRange = maxVec + .05 * (maxVec-minVec)
              live_control, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> PC-Standard Histogram'
          endif

          if (fShowCCRI and obj_isa(selectedSegObject,'C_SIMAGEFILTER_IMAGECOLOCALIZATION')) then begin
                 ; varX = mean(x²) - mean(x)² + alpha
              varX = total(ch0_vect^2 * kernelActVect) - (total(ch0_vect * kernelActVect) )^2 + alpha
              varY = total(ch1_vect^2 * kernelPasVect) - (total(ch1_vect * kernelPasVect) )^2 + alpha

              CCRI = ( total(ch0_vect * ch1_vect * ((kernelActVect+kernelPasVect)/2.)) - total(ch0_vect * kernelActVect) * total(ch1_vect * kernelPasVect) ) / $
                     (((sqrt(varX)*sqrt(varY)) > (1./ n_elements(kernelActVect))))

              ch0_vectCCRI = ch0_vect * kernelActVect
              ch1_vectCCRI = ch1_vect * kernelPasVect

              linFitParamCCRI = linFit(ch0_vectCCRI, ch1_vectCCRI, /double, chisq = chisq, sigma = sigma)
              xFitCCRI = make_array(max(ch0_vectCCRI) - min(ch0_vectCCRI) + 3, /float, /index) + (min(ch0_vectCCRI) - 1)
              if (finite(CCRI) eq 0) then yFitCCRI = xFitCCRI * floor(mean(ch0_vectCCRI)) else yFitCCRI = float(linFitParamCCRI[0] + xFitCCRI * linFitParamCCRI[1])

              print, '___________________________________'
              print, 'Results of CCRI'
              print, 'X/Y:', ev.X, ' / ', ev.Y
              print, 'CCRI:', CCRI

;              data = make_array(2, n_elements(ch0_vectCCRI), /float)
;              data [0,*] = ch0_vectCCRI
;              data [1,*] = ch1_vectCCRI
;              openW,2, s_getPathForSystem()+'ChannelCCRIData.dat'
;              printF, 2, ['ch0_vectCCRI', 'ch1_vectCCRI']
;              printF, 2, data
;              close, 2
;
;              widget_control, stateParent, get_uValue = state, /no_copy
;                 subImage = *state.subImage
;              widget_control, stateParent, set_uValue = state, /no_copy
;              subImage[0,0] = 1
;              subImage = subImage * (subImage gt 0)
;              live_image, subImage


              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> CCRI Histogram'
              if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> CCRI Histogram" does not exist.  Operation not performed.') ) then begin
                  style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type :3, symbol_size :0.1},$
                                      visualization_properties = {color :'Light Green'},$
                                      xAxis_properties = {axisTitle :'Ch 0', exact:1, compute_range :0},$
                                      yAxis_properties = {axisTitle :'Ch 1', exact:1, compute_range :0},$
                                      legend_properties = {hide :1})
                  live_plot, ch1_vectCCRI, independent = ch0_vectCCRI,$
                                      /scatter, draw_dimension = [300,300], /no_select,$
                                      style = style, title = 's_ImageColoc |-> CCRI Histogram'
                  live_oPlot, yFitCCRI, independent = xFitCCRI,$
                                      subType = 'LinePlot', /no_select,$
                                      window_in = 's_ImageColoc |-> CCRI Histogram'
              endif else begin
                 live_control, xFitCCRI, /update, window_in = 's_ImageColoc |-> CCRI Histogram'
                 live_control, yFitCCRI, /update, window_in = 's_ImageColoc |-> CCRI Histogram'
                 live_control, ch1_vectCCRI, /update, window_in = 's_ImageColoc |-> CCRI Histogram'
                 live_control, ch0_vectCCRI, /update, window_in = 's_ImageColoc |-> CCRI Histogram'
              endelse

              live_info, 'CH0_VECTCCRI Axis', properties = variable, window_in = 's_ImageColoc |-> CCRI Histogram'
              maxVec = max(ch0_vectCCRI, min = minVec)
              variable.minRange = minVec - .05 * (maxVec-minVec)
              variable.maxRange = maxVec + .05 * (maxVec-minVec)
              variable.axisTitle = 'CCRI = ' + strCompress(string(CCRI))
              live_control, 'CH0_VECTCCRI Axis', properties = variable, window_in = 's_ImageColoc |-> CCRI Histogram'
              live_info, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> CCRI Histogram'
              maxVec = max(ch1_vectCCRI, min = minVec)
              variable.minRange = minVec - .05 * (maxVec-minVec)
              variable.maxRange = maxVec + .05 * (maxVec-minVec)
              live_control, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> CCRI Histogram'
          endif

          if (fShowJMSI and obj_isa(selectedSegObject,'C_SIMAGEFILTER_IMAGECOLOCALIZATION')) then begin
              nKernelVect = n_elements(kernelActVect)
              kernelCentre = floor(nKernelVect/2.)
                 ; varX = mean(x²) - mean(x)² + alpha
              varX = total(ch0_vect^2 * kernelActVect) - (total(ch0_vect * kernelActVect))^2 + alpha
              varY = total(ch1_vect^2 * kernelPasVect) - (total(ch1_vect * kernelPasVect))^2 + alpha
                 ; xs = (x - mean(x)) / sqrt(varX)
              xs = (ch0_vect[kernelCentre] - total(ch0_vect * kernelActVect)) / (sqrt(varX) > (1./nKernelVect))
              ys = (ch1_vect[kernelCentre] - total(ch1_vect * kernelPasVect)) / (sqrt(varY) > (1./nKernelVect))

              ch0_vectJMSI = (ch0_vect - total(ch0_vect * kernelActVect)) / (sqrt(varX) > (1./nKernelVect))
              ch1_vectJMSI = (ch1_vect - total(ch1_vect * kernelPasVect)) / (sqrt(varY) > (1./nKernelVect))

              JMSI = xs * ys
              xFitJMSI = ch0_vectJMSI * 0 + xs
              yFitJMSI = ch1_vectJMSI * 0 + ys

              print, '___________________________________'
              print, 'Results of JMSI'
              print, 'xs/ys:', xs, ' / ', ys
              print, 'varX/varY:', varX, ' / ', varY
              print, 'JMSI:', JMSI

;              data = make_array(2, n_elements(ch0_vectJMSI), /float)
;              data [0,*] = ch0_vectJMSI
;              data [1,*] = ch1_vectJMSI
;              openW,2, s_getPathForSystem()+'ChannelJMSIData.dat'
;              printF, 2, ['ch0_vectJMSI', 'ch1_vectJMSI']
;              printF, 2, data
;              close, 2
;
;              widget_control, stateParent, get_uValue = state, /no_copy
;                 subImage = *state.subImage
;              widget_control, stateParent, set_uValue = state, /no_copy
;              subImage[0,0] = 1
;              subImage = subImage * (subImage gt 0)
;              live_image, subImage

              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> JMSI Histogram'
              if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> JMSI Histogram" does not exist.  Operation not performed.') ) then begin
                     style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type :3, symbol_size :0.1},$
                                       visualization_properties = {color :'Light Blue'},$
                                       xAxis_properties = {axisTitle :'Ch 0', exact:1, compute_range :0},$
                                       yAxis_properties = {axisTitle :'Ch 1', exact:1, compute_range :0},$
                                       legend_properties = {hide :1})
                     live_plot, ch1_vectJMSI, independent = ch0_vectJMSI,$
                                       /scatter, draw_dimension = [300,300], /no_select,$
                                       style = style, title = 's_ImageColoc |-> JMSI Histogram'
                     live_oPlot, yFitJMSI, independent = xFitJMSI, /no_select,$
                                       window_in = 's_ImageColoc |-> JMSI Histogram'
              endif else begin
                  live_control, xFitJMSI, /update, window_in = 's_ImageColoc |-> JMSI Histogram'
                  live_control, yFitJMSI, /update, window_in = 's_ImageColoc |-> JMSI Histogram'
                  live_control, ch1_vectJMSI, /update, window_in = 's_ImageColoc |-> JMSI Histogram'
                  live_control, ch0_vectJMSI, /update, window_in = 's_ImageColoc |-> JMSI Histogram'
              endelse

              live_info, 'CH0_VECTJMSI Axis', properties = variable, window_in = 's_ImageColoc |-> JMSI Histogram'
              maxVec = max(ch0_vectJMSI, min = minVec)
              variable.minRange = minVec - .05*(maxVec-minVec)
              variable.maxRange = maxVec + .05*(maxVec-minVec)
              variable.axisTitle = 'JMSI = ' + strCompress(string(JMSI))
              live_control, 'CH0_VECTJMSI Axis', properties = variable, window_in = 's_ImageColoc |-> JMSI Histogram'
              live_info, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> JMSI Histogram'
              maxVec = max(ch1_vectJMSI, min = minVec)
              variable.minRange = minVec - .05*(maxVec-minVec)
              variable.maxRange = maxVec + .05*(maxVec-minVec)
              live_control, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> JMSI Histogram'
          endif

          if (fShowCC) and obj_isa(selectedSegObject,'C_SIMAGEFILTER_IMAGECOLOCALIZATION') then begin

              ch0_vectCC = ch0_vect
              ch1_vectCC = ch1_vect
              maxCh1 = max(ch0_vectCC, min = minCh1)
              maxCh2 = max(ch1_vectCC, min = minCh2)

                 ; consider only hist2D with dim ge 2
              if ((minCh1 lt (maxCh1-1)) and (minCh2 lt (maxCh2-1))) then begin
                 hist2D = hist_2d(ch0_vectCC, ch1_vectCC,$
                                  bin1 = 1, bin2 = 1, min1 = minCh1, min2 = minCh2, max1 = maxCh1, max2 = maxCh2)
                 stopC = (minCh1 < minCh2) + 1

                   ; start with at least 4 data points & do not consider 0-values.
                 c = (maxCh1 < maxCh2) - 2
                 if (c ge 1) then begin
                    repeat c = c - 1 $
                       until ( (n_elements(where(hist2D[(c-minCh1)>0:*, (c-minCh2)>0:*] ne 0)) ge 4) or (c lt stopC) )

                    c = c > stopC
                    maxR = 0.
                    repeat begin
                       n = long(total(hist2D[(c-minCh1)>0:*, (c-minCh2)>0:*]))
                       w = where(hist2D[(c-minCh1)>0:*, (c-minCh2)>0:*] ne 0)
                       dimH = size(hist2D[(c-minCh1)>0:*, (c-minCh2)>0:*], /dim)
                       xVec = [-1.]
                       yVec = [-1.]
                       for i = 0, n_elements(w)-1 do begin
                         xVec = [xVec, make_array((hist2D[(c-minCh1)>0:*, (c-minCh2)>0:*])[w[i]], value = (c > minCh1) + (w[i] mod dimH[0]))]
                         yVec = [yVec, make_array((hist2D[(c-minCh1)>0:*, (c-minCh2)>0:*])[w[i]], value = (c > minCh2) + floor(w[i] / dimH[0]))]
                       endfor
                       xVec = xVec[1:*]
                       yVec = yVec[1:*]
                       totX = total(xVec)
                       totY = total(yVec)
                       r = (n * total(xVec*yVec) - totX * totY) / sqrt( (n * total(xVec*xVec) - totX^2 ) * (n * total(yVec*yVec) - totY^2) )
                       maxR = maxR > r

                       xCutx = [c, maxCh1]
                       xCuty = [c,c]
                       yCuty = [c, maxCh2]
                       yCutx = [c,c]

                       live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> CC Histogram'
                       if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> CC Histogram" does not exist.  Operation not performed.') ) then begin
                         style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type:3, symbol_size:0.1},$
                                                visualization_properties = {color:'Light Blue'},$
                                                xAxis_properties = {axisTitle:'Ch 0', exact:1, compute_range:0},$
                                                yAxis_properties = {axisTitle:'Ch 1', exact:1, compute_range:0},$
                                                legend_properties = {hide :1})
                         live_plot, ch1_vectCC, independent = ch0_vectCC,$
                                                /scatter, draw_dimension = [300,300], /no_select,$
                                                style = style, title = 's_ImageColoc |-> CC Histogram'
                         live_oPlot, yVec, independent = xVec, /no_select, window_in = 's_ImageColoc |-> CC Histogram'
                         live_oPlot, xCuty, independent = xCutx, subType = 'LinePlot', /no_select, window_in = 's_ImageColoc |-> CC Histogram'
                         live_oPlot, yCuty, independent = yCutx, subType = 'LinePlot', /no_select, window_in = 's_ImageColoc |-> CC Histogram'
                         live_info, 'YCUTY Plot Line', properties = variable, window_in = 's_ImageColoc |-> CC Histogram'
                         variable.color = 'Red'
                         live_control, 'YCUTY Plot Line', properties = variable, window_in = 's_ImageColoc |-> CC Histogram'
                         live_info, 'XCUTY Plot Line', properties = variable, window_in = 's_ImageColoc |-> CC Histogram'
                         variable.color = 'Red'
                         live_control, 'XCUTY Plot Line', properties = variable, window_in = 's_ImageColoc |-> CC Histogram'
                       endif else begin
                         live_control, ch1_vectCC, /update, window_in = 's_ImageColoc |-> CC Histogram'
                         live_control, ch0_vectCC, /update, window_in = 's_ImageColoc |-> CC Histogram'
                         live_control, yVec, /update, window_in = 's_ImageColoc |-> CC Histogram'
                         live_control, xVec, /update, window_in = 's_ImageColoc |-> CC Histogram'
                         live_control, xCutx, /update, window_in = 's_ImageColoc |-> CC Histogram'
                         live_control, xCuty, /update, window_in = 's_ImageColoc |-> CC Histogram'
                         live_control, yCutx, /update, window_in = 's_ImageColoc |-> CC Histogram'
                         live_control, yCuty, /update, window_in = 's_ImageColoc |-> CC Histogram'
                       endelse
                       live_info, 'CH0_VECTCC Axis', properties = variable, window_in = 's_ImageColoc |-> CC Histogram'
                       maxVec = max(ch0_vectCC > ch1_vectCC)
                       minVec = min(ch0_vectCC < ch1_vectCC)
                       print, 'MinMax', minVec, maxVec
                       variable.minRange = minVec - .05*(maxVec-minVec)
                       variable.maxRange = maxVec + .05*(maxVec-minVec)
                       variable.axisTitle = 'Rmax = ' + strCompress(string(maxR))
                       live_control, 'CH0_VECTCC Axis', properties = variable, window_in = 's_ImageColoc |-> CC Histogram'
                       live_info, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> CC Histogram'
                       variable.minRange = minVec - .05*(maxVec-minVec)
                       variable.maxRange = maxVec + .05*(maxVec-minVec)
                       live_control, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> CC Histogram'

                       c = c - 1
                    endRep until ( (r lt 0) or (c lt stopC) )

                    if (c gt (minCh1>minCh2)) then begin
                       mx = total(xVec) / total(ch0_vectCC)
                       my = total(yVec) / total(ch1_vectCC)
                    endif
                 endif
              endif

              if (n_elements(my) eq 0) then my = 0
              if (n_elements(mx) eq 0) then mx = 0
              if (n_elements(maxR) eq 0) then maxR = 0

              print, '___________________________________'
              print, 'Results of Costes'
              print, 'Manders Coefficient Channel 0:', mx
              print, 'Manders Coefficient Channel 1:', my
              print, 'maxR:', maxR

          endif

          if (fShowDeltaR) and (obj_isa(selectedSegObject,'C_SIMAGEFILTER_IMAGECOLOCALIZATION') or obj_isa(selectedSegObject,'C_SIMAGEFILTER_IMAGECOLOCALIZATIONDELTAR')) then begin
              ch0_vectDeltaR = ch0_vect
              ch1_vectDeltaR = ch1_vect

              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> DeltaR Histogram'
              if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> DeltaR Histogram" does not exist.  Operation not performed.') ) then begin
                     style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type :3, symbol_size :0.1},$
                                        visualization_properties = {color :'Light Red'},$
                                        xAxis_properties = {axisTitle :'Ch 0', exact:1, compute_range :0},$
                                        yAxis_properties = {axisTitle :'Ch 1', exact:1, compute_range :0},$
                                        legend_properties = {hide :1})
                     live_plot, ch1_vectDeltaR, independent = ch0_vectDeltaR,$
                                                          draw_dimension = [300,300], /no_select,$
                                                          style = style, title = 's_ImageColoc |-> DeltaR Histogram'
;                  live_oPlot, yFitDeltaR, independent = xFitDELTAR, /no_select,$
;                                                       window_in = 's_ImageColoc |-> DeltaR Histogram'
              endif else begin
;                  live_control, xFitDeltaR, /update, window_in = 's_ImageColoc |-> DeltaR Histogram'
;                  live_control, yFitDeltaR, /update, window_in = 's_ImageColoc |-> DeltaR Histogram'
                  live_control, ch1_vectDeltaR, /update, window_in = 's_ImageColoc |-> DeltaR Histogram'
                  live_control, ch0_vectDeltaR, /update, window_in = 's_ImageColoc |-> DeltaR Histogram'
              endelse

              live_info, 'CH0_VECTDELTAR Axis', properties = variable, window_in = 's_ImageColoc |-> DeltaR Histogram'
              maxVec = max(ch0_vectDeltaR, min = minVec)
              variable.minRange = minVec - .05*(maxVec-minVec)
              variable.maxRange = maxVec + .05*(maxVec-minVec)
              variable.axisTitle = 'DELTAR = ' + strCompress(string(DELTAR))
              live_control, 'CH0_VECTDELTAR Axis', properties = variable, window_in = 's_ImageColoc |-> DeltaR Histogram'
              live_info, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> DeltaR Histogram'
              maxVec = max(ch1_vectDeltaR, min = minVec)
              variable.minRange = minVec - .05*(maxVec-minVec)
              variable.maxRange = maxVec + .05*(maxVec-minVec)
              live_control, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> DeltaR Histogram'
          endif
          if (fShowDeltaR eq 0) then begin
              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> DeltaR Histogram'
              if  (error eq '') then live_destroy, window_in = 's_ImageColoc |-> DeltaR Histogram'
          endif
          if (fShowJMSI eq 0) then begin
              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> JMSI Histogram'
              if  (error eq '') then live_destroy, window_in = 's_ImageColoc |-> JMSI Histogram'
          endif
          if (fShowCC eq 0) then begin
              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> CC Histogram'
              if  (error eq '') then live_destroy, window_in = 's_ImageColoc |-> CC Histogram'
          endif
          if (fShowStandPC eq 0) then begin
              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> PC-Standard Histogram'
              if  (error eq '') then live_destroy, window_in = 's_ImageColoc |-> PC-Standard Histogram'
          endif
          if (fShowCCRI eq 0) then begin
              live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> CCRI Histogram'
              if  (error eq '') then live_destroy, window_in = 's_ImageColoc |-> CCRI Histogram'
          endif

          widget_control, stateParent, get_uValue = state, /no_copy
         endif else begin
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> I-Scatter Histogram'
          if (error eq '') then live_destroy, window_in = 's_ImageColoc |-> I-Scatter Histogram'
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> PC Histogram'
          if (error eq '') then live_destroy, window_in = 's_ImageColoc |-> PC Histogram'
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> PC-Standard Histogram'
          if (error eq '') then live_destroy, window_in = 's_ImageColoc |-> PC-Standard Histogram'
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> CCRI Histogram'
          if (error eq '') then live_destroy, window_in = 's_ImageColoc |-> CCRI Histogram'
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> JMSI Histogram'
          if (error eq '') then live_destroy, window_in = 's_ImageColoc |-> JMSI Histogram'
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> CC Histogram'
          if (error eq '') then live_destroy, window_in = 's_ImageColoc |-> CC Histogram'
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> DeltaR Histogram'
          if (error eq '') then live_destroy, window_in = 's_ImageColoc |-> DeltaR Histogram'
         endelse
       endif

       if (ev.press eq 4) then if state.fTvScaleZoomWindow then tvscl, (*state.subImage ne 0) else tv, (*state.subImage ne 0)
       widget_control, stateParent, set_uValue = state, /no_copy
       widget_control, ev.id, set_uValue = stateParent, /no_copy
    endif
       ; button released
    if ((ev.release eq 1) or (ev.release eq 4)) then begin
       widget_control, ev.id, get_uValue = stateParent, /no_copy
       widget_control, stateParent, get_uValue = state, /no_copy
         wSet, state.zoomWindowID
         if (ev.release) then if (state.fTvScaleZoomWindow) then tvscl, *state.subImage else tv, uInt(*state.subImage * state.imageBitMapScale)
         if (ev.release eq 4) then begin
          if (state.fCut) then zoomFactor = 1 else zoomFactor = state.zoomFactor
          state.zoomXSize = (state.xd - state.xs + 1) * zoomFactor
          state.zoomYSize = (state.yd - state.ys + 1) * zoomFactor
          if state.fTvScaleZoomWindow then $
             tvscl, congrid((*state.image)[ state.xs:state.xd , state.ys:state.yd], state.zoomXSize, state.zoomYSize, /minus, Interp = state.fInterpolateZoom, /center) else $
             tv, congrid((uInt(*state.image * state.imageBitMapScale))[ state.xs:state.xd , state.ys:state.yd], state.zoomXSize, state.zoomYSize, /minus, Interp = state.fInterpolateZoom, /center)
         endif
       widget_control, stateParent, set_uValue = state, /no_copy
       widget_control, ev.id, set_uValue = stateParent, /no_copy
    endif
end


pro s_Image_SZC_optControl, ev
    widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue, /no_copy
    case uValue of
       'CUTMODE':begin
          state.fCut = 1 - state.fCut           ; Switch Flag (1/0 ON/OFF) for Cut Control
          widget_control, state.wCutButtom, sensitive = 0
          widget_control, state.wZoomButtom, sensitive = 1
          widget_control, state.wZoomFactor, sensitive = 0
          widget_control, state.wInterpolateZoomOnOff, sensitive = 0
        endcase
       'ZOOMMODE':begin
          state.fCut = 1 - state.fCut           ; Switch Flag (1/0 ON/OFF) for Cut Control
          widget_control, state.wCutButtom, sensitive = 1
          widget_control, state.wZoomButtom, sensitive = 0
          widget_control, state.wZoomFactor, sensitive = 1
          widget_control, state.wInterpolateZoomOnOff, sensitive = 1
        endcase
       'INTERPOLATEZOOMONOFF':state.fInterpolateZoom = s_ToggleButtonOnOffState(state.wInterpolateZoomOnOff)
       'TVSCALEZOOMBOXONOFF':state.fTvScaleZoomWindow = s_ToggleButtonOnOffState(state.wTvScaleZoomWindowOnOff)
       'SAVERGBRESULTONOFF':state.fSaveRGBResult = s_ToggleButtonOnOffState(state.wSaveRGBResultOnOff)
       'SAVEGREYRESULTONOFF':state.fSaveGreyResult = s_ToggleButtonOnOffState(state.wSaveGreyResultOnOff)
       'SAVEGREYRESULTBYTEONOFF':state.fSaveGreyResultByte = s_ToggleButtonOnOffState(state.wSaveGreyResultByteOnOff)
       'SELECTSEGMENTATIONBOXONOFF':begin            ; Switch Flag (1/0 ON/OFF) for Segmentation Control
          state.fSelectSegBox = s_ToggleButtonOnOffState(state.wSelectSegBoxOnOff)
          if state.fSelectSegBox then if state.fShowSegBox then state.fShowSegBox = s_ToggleButtonOnOffState(state.wShowSegBoxOnOff)
        endcase
       'KEEPFILTERMATRICESONOFF':begin
            state.fKeepFilterMatrix = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
            if not(state.fKeepFilterMatrix) then begin
               if state.fShowStandPC then state.fShowStandPC = s_ToggleButtonOnOffState(state.wShowStandPCOnOff)
               if state.fShowCCRI then state.fShowCCRI = s_ToggleButtonOnOffState(state.wShowCCRIOnOff)
               if state.fShowJMSI then state.fShowJMSI = s_ToggleButtonOnOffState(state.wShowJMSIOnOff)
               if state.fShowCC then state.fShowCC = s_ToggleButtonOnOffState(state.wShowCCOnOff)
               if state.fShowDeltaR then state.fShowDeltaR = s_ToggleButtonOnOffState(state.wShowDeltaROnOff)
            endif
        endcase
       'SHOWPCSTANDONOFF':begin
            state.fShowStandPC = s_ToggleButtonOnOffState(state.wShowStandPCOnOff)
            if state.fShowStandPC then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWCCRIWINONOFF':begin
            state.fShowCCRI = s_ToggleButtonOnOffState(state.wShowCCRIOnOff)
            if state.fShowCCRI then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWJMSIWINONOFF':begin
            state.fShowJMSI = s_ToggleButtonOnOffState(state.wShowJMSIOnOff)
            if state.fShowJMSI then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWCCWINONOFF':begin
            state.fShowCC = s_ToggleButtonOnOffState(state.wShowCCOnOff)
            if state.fShowCC then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWDELTARWINONOFF':begin
            state.fShowDeltaR = s_ToggleButtonOnOffState(state.wShowDeltaROnOff)
            if state.fShowDeltaR then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWXRATEWINONOFF':state.fUpdateXRay = s_ToggleButtonOnOffState(state.wUpdateXRayOnOff)
       'SHOWSELECTEDBOXONOFF':begin
          state.fShowSegBox = s_ToggleButtonOnOffState(state.wShowSegBoxOnOff)
          if state.fShowSegBox then if state.fKeepFilterMatrix then state.fSelectSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SELECTENTIREWINDOW':begin
          state.xs = 0
          state.xd = state.xSize -1
          state.ys = 0
          state.yd = state.ySize -1
        endcase
       'HISTSEGONOFF':begin      ; Switch Flag (1/0 ON/OFF) for the View-Histogram-window
          state.fHistSegWin = s_ToggleButtonOnOffState(state.wHistSegWinOnOff)
          if not(state.fHistSegWin) then begin
              child_HistSegWin_tlb = state.child_HistSegWin_tlb
              widget_control, ev.id, set_uValue = uValue, /no_copy
              widget_control, ev.top, set_uValue = state, /no_copy
                 if widget_info(child_HistSegWin_tlb, /valid) then s_HistPlot_Window_cleanUp, child_HistSegWin_tlb
              widget_control, ev.id, get_uValue = uValue, /no_copy
              widget_control, ev.top, get_uValue = state, /no_copy
          endif
        endcase
    endcase

    widget_control, ev.id, set_uValue = uValue, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy
    zImage_colors, {redraw_image, top :ev.top}
end


pro zImage_colors, ev
    widget_control, ev.top, get_uValue = state, /no_copy
    case (tag_names(ev, /structure)) of
        'WIDGET_BUTTON':begin
               tvLCT, state.r, state.g, state.b, state.bottom
               XColors, group = ev.top, nColors = state.nColors, bottom = state.bottom, notifyID = [ev.id, ev.top], title = 'ZImage Colors (' + StrTrim(state.drawIndex,2) + ')'
               widget_control, state.wBaseID, map = 0
               state.fMap = 0
            endcase
        'REDRAW_IMAGE':begin
              device, get_visual_depth = thisDepth
              if (thisDepth gt 8) then begin

                wSet, state.drawIndex
                tvLCT, state.r, state.g, state.b, state.bottom
                tv, uInt(*state.image * state.imageBitMapScale)
                wSet, state.pixIndex
                device, copy = [0, 0, state.xSize, state.ySize, 0, 0, state.drawIndex]

                   ; Draw into the zoom box.
                wSet, state.drawIndex
                device, copy = [0, 0, state.xSize, state.ySize, 0, 0, state.pixIndex]
                tvLCT, 0B, 255B, 0B, state.colorIndex
                plots, [state.xs, state.xs, state.xd, state.xd, state.xs], [state.ys, state.yd, state.yd, state.ys, state.ys], /device, color = state.colorIndex
                tvLCT, state.r, state.g, state.b, state.bottom

                if widget_info(state.child_zoomWindow_tlb, /valid) then begin
                   if state.fCut then zoomFactor = 1 else zoomFactor = state.zoomFactor
                   state.zoomXSize = (state.xd - state.xs + 1) * zoomFactor
                   state.zoomYSize = (state.yd - state.ys + 1) * zoomFactor
                   state.wTitle = strCompress(string(state.xs) + '-' + string(state.xd) , /rem) + ' [x]' + ' | ' + strCompress(string(state.ys) + '-' + string(state.yd) , /rem) + ' [y]'

                   fHistSegWin = state.fHistSegWin
                   widget_control, ev.top, set_uValue = state, /no_copy
                     subImage = s_Image_SZC_getSubImage(wBaseID = ev.top)
                     widget_control, ev.top, get_uValue = state, /no_copy
                     if ptr_valid(state.subImage) then begin
                        *state.subImage = subImage 
                     endif else begin
                        state.subImage = ptr_new(subImage, /no_copy)
                     endelse
                     widget_control, ev.top, set_uValue = state, /no_copy
                       ; if the Zoom window exists, make it the proper size and load the zoomed image into it. if it does not exists, create it.
                     s_Image_SZC_updateZoomWindow, wBaseID = ev.top
                      ; actualize Segmentation-Histogram
                     if fHistSegWin then s_Image_SZC_updateCreateHistSegWin, wBaseID = ev.top
                   widget_control, ev.top, get_uValue = state, /no_copy
                endif
             endif
         endcase
        'XCOLORS_LOAD':begin
               state.r = ev.r[state.bottom:state.bottom+state.nColors-1]
               state.g = ev.g[state.bottom:state.bottom+state.nColors-1]
               state.b = ev.b[state.bottom:state.bottom+state.nColors-1]
               device, get_visual_depth = thisDepth
               if (thisDepth gt 8) then begin
                   wSet, state.drawIndex
                   tv, uInt(*state.image * state.imageBitMapScale)
                   wSet, state.pixIndex
                   device, copy = [0, 0, state.xSize, state.ySize, 0, 0, state.drawIndex]
                   if widget_info(state.child_zoomWindow_tlb, /valid) then begin
                      wSet, state.zoomWindowID
                      if state.fTvScaleZoomWindow then tvscl, *state.subImage else tv, uInt(*state.subImage * state.imageBitMapScale)
                   endif
              endif
        endcase
    endcase
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Image_SZC_cleanUp, wBase
   widget_control, wBase, get_uValue = state, /no_copy
   if (n_elements(state) ne 0) then begin
      wDelete, state.pixIndex
      if widget_info(state.groupLeader, /valid) then begin
         widget_control, state.groupLeader, get_uValue = stateParent, /no_copy
         if (n_elements(stateParent) ne 0) then begin
             if (s_ToggleButtonOnOffState(stateParent.wViewWindowButton) eq 1) then void = s_ToggleButtonOnOffState(stateParent.wViewWindowButton)
             widget_control, state.groupLeader, set_uValue = stateParent, /no_copy
         endif
      endif
      for i = 0,n_tags(state)-1 do begin
         case size(state.(i), /tname) of
            'POINTER':ptr_free, state.(i)
            'OBJREF':obj_destroy, state.(i)
         else:
         endcase
      endfor
   endif
end


pro s_Image_SZC_zoomFactor, ev
      ; The purpose of this ev handler is to set the zoom factor.
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.id, get_uValue = factor, /no_copy
         state.zoomFactor = factor[ev.index]
         widget_control, state.wBaseID, map = 0
         state.fMap = 0
      widget_control, ev.id, set_uValue = factor, /no_copy
   widget_control, ev.top, set_uValue = state, /no_copy
   if widget_info(ev.top, /valid) then zImage_colors, {redraw_image, top :ev.top}
end


pro s_Image_SZC_drawEvents, ev
    x = [ 0,1024]
    y = [ 0,1024]
       ; This ev handler continuously draws and erases the zoom box until it receives an UP ev from the draw widget. then it turns draw widget motion events OFF.
    widget_control, ev.top, get_uValue = state, /no_copy
    case ((['PRESS', 'RELEASE', 'MOTION', 'SCROLL' ])[ev.type]) of
       'PRESS':begin
           state.click = (['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT'])[ev.press]
           case state.click of
             'LEFT':begin
                   ; Set the static corners of the box to current cursor location.
                 state.xs = (( ev.x > 0) < (state.xSize-1) > 0)
                 state.ys = (( ev.y > 0) < (state.ySize-1) > 0)
                 state.xd = state.xs
                 state.yd = state.ys
              endcase
             'MIDDLE':begin
                 state.xo = ev.x
                 state.yo = ev.y
              endcase
             'RIGHT':begin
                 state.fMap = 1 - state.fMap
                 widget_control, state.wBaseID, map = state.fMap
                 widget_control, ev.top, set_uValue = state, /no_copy
                 return
              endcase
              else:
           endcase

               ; Turn draw MOTION events ON.
            widget_control, ev.id, Draw_Motion_Events = 1

               ; Take over the color index for the zoom box drawing color. Store the
               ; current (r,g,b) values for color index so you can restore the current colors after the zoom box is drawn.
            tvLCT, r, g, b, /get
            state.r_old = r[state.colorIndex]
            state.g_old = g[state.colorIndex]
            state.b_old = b[state.colorIndex]
          endcase
       'RELEASE':begin
             ; Turn motion events off.
           widget_control, ev.id, Draw_Motion_Events = 0

           case state.click of
             'LEFT':begin
                    ; Draw the 'zoomed' image. Start by getting the LAST zoom box outline. These are indices into image array.
                 ev.x = 0 > ev.x < (state.xSize - 1)
                 ev.y = 0 > ev.y < (state.ySize - 1)
                     ; Make sure the user didn't just click in the window.
                 if ((state.xs eq ev.x) or (state.ys eq ev.y)) then begin
                   state.xs = (state.xysd_old[0] < state.xysd_old[1]-1) > 0   ; Left x coordinate of the Surface chosen for further Analysis
                   state.xd = state.xysd_old[1] > 0                          ; Right x coordinate of the Surface chosen for further Analysis
                   state.ys = (state.xysd_old[2] < state.xysd_old[3]-1) > 0   ; Upper y coordinate of the Surface chosen for further Analysis
                   state.yd = state.xysd_old[3] > 0                          ; Lower y coordinate of the Surface chosen for further Analysis
                   widget_control, ev.top, set_uValue = state, /no_copy
                   return
                endif
                      ; Make sure the x and y values are ordered as [min, max].
                   x = [ (state.xs < ev.x), (state.xs > ev.x) < (state.xSize - 1)]
                   y = [ (state.ys < ev.y), (state.ys > ev.y) < (state.ySize - 1) ]
                   x[0] = (x[0] < (x[1]-1)) > 0
                   y[0] = (y[0] < (y[1]-1)) > 0
                   state.xs = x[0]     ; Left x coordinate of the Surface chosen for further Analysis
                   state.xd = x[1]     ; Right x coordinate of the Surface chosen for further Analysis
                   state.ys = y[0]     ; Upper y coordinate of the Surface chosen for further Analysis
                   state.yd = y[1]     ; Lower y coordinate of the Surface chosen for further Analysis
                   state.xysd_old = [[x], [y]] ; remember zoom box coordinates.
              endcase
             'MIDDLE':begin
                   x = [ state.xs, state.xd]
                   y = [ state.ys, state.yd]
              endcase
             'RIGHT':begin
                  widget_control, ev.top, set_uValue = state, /no_copy
                  return
              endcase
              else:
           endcase

           wSet, state.drawIndex
           tvLCT, state.r, state.g, state.b;        tvLCT, state.r_old, state.g_old, state.b_old, state.colorIndex

          if state.fCut then begin ; Decide if the image has to be cut or zoomed
             zoomFactor = 1.
             state.reducedSizeXleft = x[0]   ; Left x coordinate of the Surface chosen for further Analysis
             state.reducedSizeXright = x[1]  ; Right x coordinate of the Surface chosen for further Analysis
             state.reducedSizeYupper = y[0]  ; Upper y coordinate of the Surface chosen for further Analysis
             state.reducedSizeYlower = y[1]  ; Lower y coordinate of the Surface chosen for further Analysis
          endif else zoomFactor = state.zoomFactor

               ; Set the zoom factor and determine the new X and Y sizes of the Zoom window.
          state.zoomXSize = (x[1] - x[0] + 1) * zoomFactor
          state.zoomYSize = (y[1] - y[0] + 1) * zoomFactor
          state.wTitle = strCompress( string(x[1]-x[0]+1) +' [x], '+ string(y[1]-y[0]+1) + ' [y] )')

          widget_control, ev.top, set_uValue = state, /no_copy
              subImage = s_Image_SZC_getSubImage(wBaseID = ev.top)
              widget_control, ev.top, get_uValue = state, /no_copy
                 if ptr_valid(state.subImage) then *state.subImage = subImage else state.subImage = ptr_new(subImage, /no_copy)
              widget_control, ev.top, set_uValue = state, /no_copy
                 ; if the Zoom window exists, make it the proper size and load the zoomed image into it. if it does not exists, create it.
              s_Image_SZC_updateZoomWindow, wBaseID = ev.top
              if not(widget_info(ev.top, /valid)) then return
          widget_control, ev.top, get_uValue = state, /no_copy

              ; show or actualize histogram
          if state.fHistSegWin then begin
             if (n_elements(state) gt 0) then begin
                widget_control, ev.top, set_uValue = state, /no_copy
                  s_Image_SZC_updateCreateHistSegWin, wBaseID = ev.top
                widget_control, ev.top, get_uValue = state, /no_copy
             endif
          endif

              ; if the controls were mapped, unmap them.
          if state.fMap then begin
             widget_control, state.wBaseID, map = 0
             state.fMap = 0
          endif
       endcase
       'MOTION':begin
          ; Most of the action in this ev handler occurs here while we are waiting for an UP ev to occur. As long as we don't get it, keep erasing the old zoom box and drawing a new one.
          ; Erase the old zoom box.
          wSet, state.drawIndex
          tvLCT, state.r, state.g, state.b, state.bottom
          device, copy = [0, 0, state.xSize, state.ySize, 0, 0, state.pixIndex]

           case state.click of
             'LEFT':begin
                   ; update the dynamic corner of the zoom box to the current cursor location.
                 state.xd = ev.x
                 state.yd = ev.y
              endcase
             'MIDDLE':begin
                 state.xd -= (state.xo - ev.x)
                 state.yd -= (state.yo - ev.y)
                 state.xs -= (state.xo - ev.x)
                 state.ys -= (state.yo - ev.y)
                 state.xo = ev.x
                 state.yo = ev.y
              endcase
              else:
           endcase

           state.xd >= 0
           state.yd >= 0
           state.xs >= 0
           state.ys >= 0
           state.xd <= (state.xSize-1)
           state.yd <= (state.xSize-1)
           state.xs <= (state.xSize-1)
           state.ys <= (state.xSize-1)

          ; Load a green color in color index 1 to draw the zoom box with. Draw the zoom box. Load old color talble
          tvLCT, 0B, 255B, 0B, state.colorIndex
          plots, [state.xs, state.xs, state.xd, state.xd, state.xs], [state.ys, state.yd, state.yd, state.ys, state.ys], /device, color = state.colorIndex
          tvLCT, state.r, state.g, state.b, state.bottom
       endcase
    endcase
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Image_ShowZoomCut_Window, image, imageBitRate = imageBitRate, ColorIndex = colorIndex, Bottom = bottom,$
       groupLeader = groupLeader, nColors = nColors, pSurfaceInfo = pSurfaceInfo,$
       basePosition = baseSize, application_tlb = application_tlb

    on_error, 1
       ;Check the validity of the group identifier.
    if (n_elements(groupLeader) ne 0) then begin
       if not(widget_info(groupLeader, /valid)) then begin
          print,'Error:the group identifier is not valid.  |--> Returning to the main application.'
          return
       endif
    endif else groupLeader = 0l
    stack_tlb = groupLeader

       ; Was an image passed into the procedure? if not, find one in the Examples directory.
    if (n_params() eq 0) then begin
       filename = filePath(SubDirectory = ['examples','data'], 'worldelv.dat')
       openR, lun, filename, /get_lun
       image = bytArr(360,360)
       readU, lun, image
       free_lun, lun
    endif

       ; Make sure a window has been opened in this IDL session for accurate color number determination.
    window, /pixmap, xSize = 10, ySize = 10
    wDelete, !D.window

       ; Check for keywords. Set defaults to size of image if necessary.
    szI = size(image)
    if (szI[0] ne 2) then message, 'Image parameter must be 2D.'
    xSize = szI[1]
    ySize = szI[2]
    if n_elements(nColors) eq 0 then nColors = (!D.N_Colors - 2) < 254
    if n_elements(bottom) eq 0 then bottom = 0b
    if n_elements(colorIndex) eq 0 then colorIndex = (nColors + bottom) < 255

       ; Works with 2D images.
    device, decomposed = 0

       ; Create a top-level base for this program. No resizing of this base. No closing of this base.
    wTopBase = widget_base(tlb_frame_attr = 9)
    application_tlb = wTopBase

       ; Create two bases. One for controls and the other for the draw widget. Leave the control base unmapped for now.
    wBaseID = widget_base(wTopBase, map = 0, column = 1)

    drawBase = widget_base(wTopBase)
    draw = widget_draw(drawBase, xSize = xSize, ySize = ySize, button_events = 1, retain = 2, event_pro = 's_Image_SZC_drawEvents')

    modeID = widget_button(wBaseID, value = 'Options', event_pro = 's_Image_SZC_optControl', /menu)
       wCutButtom = widget_button(modeID, value = 'Cut Mode', uValue = 'CUTMODE', Sensitive = 0)
       wZoomButtom = widget_button(modeID, value = 'Zoom Mode', uValue = 'ZOOMMODE')
       wInterpolateZoomOnOff = widget_button(modeID, value = 'Interpolate Zoom (on )', uValue = 'INTERPOLATEZOOMONOFF')
       if (groupLeader ne 0) then wSelectSegBoxOnOff = widget_button(modeID, value = 'Select Segmentation Box (off)', uValue = 'SELECTSEGMENTATIONBOXONOFF', /sep) else wSelectSegBoxOnOff = 0
       if (groupLeader ne 0) then wShowSegBoxOnOff = widget_button(modeID, value = 'Show Selected Box (off)', uValue = 'SHOWSELECTEDBOXONOFF') else wShowSegBoxOnOff = 0
       wTvScaleZoomWindowOnOff = widget_button(modeID, value = 'Tv Scale Zoom Box (on )', uValue = 'TVSCALEZOOMBOXONOFF', event_pro = 's_Image_SZC_optControl', /sep)
       colors = widget_button(modeID, value = 'Change Color Table (LUT)', event_pro = 'ZIMAGE_COLORS')
       wKeepFilterMatrixOnOff = widget_button(modeID, value = 'Keep Filter Matrices (off)', uValue = 'KEEPFILTERMATRICESONOFF', /sep)
       wShowStandPCOnOff = widget_button(modeID, value = 'Show Standardized Pearson Window (off)', uValue = 'SHOWPCSTANDONOFF')
       wShowCCRIOnOff = widget_button(modeID, value = 'Show CCRI Window (off)', uValue = 'SHOWCCRIWINONOFF')
       wShowJMSIOnOff = widget_button(modeID, value = 'Show JMSI Window (off)', uValue = 'SHOWJMSIWINONOFF')
       wShowCCOnOff = widget_button(modeID, value = 'Show CC Window (off)', uValue = 'SHOWCCWINONOFF')
       wShowDeltaROnOff = widget_button(modeID, value = 'Show DeltaR Window (off)', uValue = 'SHOWDELTARWINONOFF')
       wUpdateXRayOnOff = widget_button(modeID, value = 'Update X-Ray Window (off)', uValue = 'SHOWXRATEWINONOFF', /sep)
       wSaveRGBResultOnOff = widget_button(modeID, value = 'Save RGB-Result (off)', uValue = 'SAVERGBRESULTONOFF', /sep)
       wSaveGreyResultOnOff = widget_button(modeID, value = 'Save Grey-Result as Float (off)', uValue = 'SAVEGREYRESULTONOFF')
       wSaveGreyResultByteOnOff = widget_button(modeID, value = 'Save Grey-Result as Byte (off)', uValue = 'SAVEGREYRESULTBYTEONOFF')

    wZoomFactor = widget_dropList(wBaseID, value = ['.1x','.25x', '.5x', '.75x', '1x','2x', '3x', '4x','5x', '6x', '7x','8x'],$
                                  event_pro = 's_Image_SZC_zoomFactor', uValue = [.1,.25, .5, .75, 1, 2, 3, 4, 5, 6, 7, 8], title = 'Zoom Factor', sensitive = 0)
    widget_control, wZoomFactor, set_droplist_select = 2

    modeID = widget_button(wBaseID, value = 'Select Entire Window', event_pro = 's_Image_SZC_optControl', uValue = 'SELECTENTIREWINDOW')
    modeID = widget_button(wBaseID, value = 'Plot Windows', event_pro = 's_Image_SZC_optControl', /menu)
    wHistSegWinOnOff = widget_button(modeID, value = 'Segmentation Histogram (off)', uValue = 'HISTSEGONOFF')

       ; Get the window index number of the draw widget.
       ; Make the draw widget the current graphics window and display the image in it.
    widget_control, wTopBase, /realize
    widget_control, draw, get_value = drawIndex
    wSet, drawIndex
    tv, image

    widget_control, wTopBase, tlb_set_title = strCompress('s_2DView |-> Image Dimensions ('+ string(xSize) +' [x], '+ string(ySize) + ' [y] )')

      ; Create a pixmap window the same size as the draw widget window.
      ; Store its window index number in a local variable. Display the
      ; image you just put in the draw widget in the pixmap window.
    window, /free, XSize = xSize, YSize = ySize, /pixmap
    pixIndex = !D.window
    tv, image
    tvLCT, r, g, b, /get

    widget_control, stack_tlb, get_uValue = state, /no_copy
       (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = state, /no_copy
    paintedMaskPath = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]], 'X']

       ; Create an state structure to hold information required by the program.
    state = {wBaseID:wBaseID,$
       groupLeader:groupLeader,$
       stack_tlb:stack_tlb,$
       image:ptr_new(image, /no_copy),$    ; The original image.
       imageBitMapScale:255./(long(2)^imageBitRate),$
       subImage:ptr_new(),$      ; The scaled and resized subImage.
       fUpdatePaintedMaskPath:1b,$
       paintedMaskPath:paintedMaskPath,$
       xSize:xSize,$              ; The x size of the image window.
       ySize:ySize,$              ; The y size of the image window.
       ReducedSizeXleft:-1,$     ; Left x coordinate of the Surface chosen for further Analysis
       ReducedSizeXright:-1,$   ; Right x coordinate of the Surface chosen for further Analysis
       ReducedSizeYupper:-1,$   ; Upper y coordinate of the Surface chosen for further Analysis
       ReducedSizeYlower:-1,$   ; Lower y coordinate of the Surface chosen for further Analysis
       pPassSInfo:ptr_new(),$   ; PointerArray on pSurfaceInfo
       drawIndex:drawIndex,$       ; The draw window index number.
       pixIndex:pixIndex,$         ; The pixmap window index number.
       nColors:nColors,$           ; The number of colors for the image.
       bottom:bottom,$             ; The bottom color index.
       colorIndex:colorIndex,$     ; The drawing color index.
       click:'NONE',$
       xo:0,$                      ; X drag coordinates of the zoom box.
       yo:0,$                      ; Y drag coordinates of the zoom box.
       xs:0,$                      ; X static corner of the zoom box.
       ys:0,$                      ; Y static corner of the zoom box.
       xd:0,$                      ; X dynamic corner of the zoom box.
       yd:0,$                      ; Y dynamic corner of the zoom box.
       xysd_old:intArr(4),$ ; old zoom box positions.
       wCutButtom:wCutButtom,$       ; Cut - Button ID.
         fCut:1,$   ; A flag to activate/desactivate the cut option (1/ON, 2/OFF)
       wZoomButtom:wZoomButtom,$         ; Zoom - Button ID.
       wZoomFactor:wZoomFactor,$    ; ZoomFaktor - Button ID.
       wSaveRGBResultOnOff:wSaveRGBResultOnOff,$
         fSaveRGBResult:0b,$
       wSaveGreyResultOnOff:wSaveGreyResultOnOff,$
         fSaveGreyResult:0b,$
       wSaveGreyResultByteOnOff:wSaveGreyResultByteOnOff,$
         fSaveGreyResultByte:0b,$
       wInterpolateZoomOnOff:wInterpolateZoomOnOff,$
         fInterpolateZoom:1b,$
       wTvScaleZoomWindowOnOff:wTvScaleZoomWindowOnOff,$    ; TvScaleZoomWindow on/off button
         fTvScaleZoomWindow:1b,$    ; Flag for TvScaleZoomWindow on/off (1/0) control
       wSelectSegBoxOnOff:wSelectSegBoxOnOff,$  ; Show Segment Procedure on/off button
         fSelectSegBox:0b,$
       wShowSegBoxOnOff:wShowSegBoxOnOff ,$
         fShowSegBox:0b,$
       wKeepFilterMatrixOnOff:wKeepFilterMatrixOnOff,$
         fKeepFilterMatrix:0b,$
       wShowStandPCOnOff:wShowStandPCOnOff,$
         fShowStandPC:0b,$
       wShowCCRIOnOff:wShowCCRIOnOff,$
         fShowCCRI:0b,$
       wShowJMSIOnOff:wShowJMSIOnOff,$
         fShowJMSI:0b,$
       wShowCCOnOff:wShowCCOnOff,$
         fShowCC:0b,$
       wUpdateXRayOnOff:wUpdateXRayOnOff,$
         fUpdateXRay:0b,$
       wShowDeltaROnOff:wShowDeltaROnOff,$
         fShowDeltaR:0b,$
       wHistSegWinOnOff:wHistSegWinOnOff,$
         fHistSegWin:0b,$
         child_HistSegWin_tlb:-1L,$            ; Histogram window widget ID.
       r:r,$                       ; The red color vector.
       g:g,$                       ; The green color vector.
       b:b,$                       ; The blue color vector.
       r_old:0,$                   ; The user's red color value.
       g_old:0,$                   ; The user's green color value.
       b_old:0,$                   ; The user's blue color value.
       child_zoomWindow_tlb:-1L,$ ; Zoomed imageData draw widget ID.
       zoomDraw_tlb:-1L,$          ; Zoomed imageData draw widget ID.
       zoomWindowID:-1,$           ; Zoomed imageData window index number.
       zoomFactor:.5,$            ; The initial zoom factor.
       zoomXSize:0,$
       zoomYSize:0,$
       fMap:0b,$                   ; A flag to tell if the controls are mapped.
       wTitle:'|',$
       selXYZDataROIParamName:['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-'],$
       currROIGroupFileName:s_getPathForSystem()}

    if ptr_valid(pSurfaceInfo) then state.pPassSInfo = ptr_new(*pSurfaceInfo)

       ; Store the state structure in the user value of the top-level base. & Register this program and set up the event loop.
    widget_control, wTopBase, set_uValue = state, /no_copy
    XManager, 's_Image_ShowZoomCut_Window', wTopBase, Cleanup = 's_Image_SZC_cleanUp', group_leader = groupLeader, /no_block
    zImage_colors, {redraw_image, top :wTopBase}
end ; of ZIMAGE ****************************************************************************
