;_____________________________IOISIOI____________________
; NAME:
;       s_Image_RankOrderCut_Window
;
; PURPOSE:
;       Cut & Zoom Image
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       s_Image_RankOrderCut_Window, imageData
;
;  KEYWORD PARAMETERS:
;        None
;_____________________________IOISIOI____________________


pro s_Image_ROC_updateCreateHistSegWin, wBaseID = wBaseID
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


function s_Image_ROC_getSubImage, wBaseID = wBaseID
    widget_control, wBaseID, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       cut_x = [state.xs, state.xd]
       cut_y = [state.ys, state.yd]
    widget_control, wBaseID, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                              clusPos = clusPos, selClusName = selClusName, segPos = segPos
    fApplySeg = 0b
    fApplySavedClusterMask = 0b
    fApplySavedROI2D = 0b
    fApplySavedROI3D = 0b
    widget_control, stack_tlb, get_uValue = stateStack, /no_copy
       if widget_info(stateStack.child_SegmentationWindow_tlb, /valid) then begin
          widget_control, stateStack.child_SegmentationWindow_tlb, get_uValue = stateSegWin, /no_copy
          fApplySeg = stateSegWin.fApplySeg
             fApplySavedClusterMask = stateSegWin.fApplySavedClusterMask
             fApplySavedROI2D = stateSegWin.fApplySavedROI2D
             fApplySavedROI3D = stateSegWin.fApplySavedROI3D
          widget_control, stateStack.child_SegmentationWindow_tlb, set_uValue = stateSegWin, /no_copy
       endif
    widget_control, stack_tlb, set_uValue = stateStack, /no_copy

    if (fApplySeg or fApplySavedClusterMask or fApplySavedROI2D or fApplySavedROI3D) then begin
       widget_control, stack_tlb, get_uValue = stateStack, /no_copy
          imageStackInfoObject = *stateStack.pImageStackInfoObject
       widget_control, stack_tlb, set_uValue = stateStack, /no_copy

         ; Apply new Segmentation
       if fApplySeg then subImage = imageStackInfoObject->applyImageSegmentation(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                                                   clusPos = clusPos, segPos = segPos, cut_x = cut_x, cut_y = cut_y)
         ; Get saved Cluster Masks
       if fApplySavedClusterMask then begin
          widget_control, wBaseID, get_uValue = state, /no_copy
             subImage = *state.image
          widget_control, wBaseID, set_uValue = state, /no_copy
       widget_control, stack_tlb, get_uValue = stateStack, /no_copy
          oImage = (*stateStack.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
       widget_control, stack_tlb, set_uValue = stateStack, /no_copy

         oImage->get, pParamStruct = pParamStruct
         segXBox0 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]]
         segXBox1 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]]
         segYBox0 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]]
         segYBox1 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]]

         clusterMask = imageStackInfoObject->getSelectedClusterMask(tPos = tPos , chPos = chPos , zPos = zPos , clusPos = clusPos)
         if (clusterMask[0] eq -1) and (n_elements(clusterMask) eq 1) then clusterMask = subImage * 0
         dimCM = size(clusterMask ,/dim)
         if ((dimCM[0] ne (segXBox1-segXBox0+1)) or (dimCM[1] ne (segYBox1-segYBox0+1))) then clusterMask = congrid(clusterMask, (segXBox1-segXBox0+1), (segYBox1-segYBox0+1))

         subImage[segXBox0:segXBox1, segYBox0 : segYBox1] = clusterMask
         subImage = subImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
       endif

         ; get ROI-Objects
       if (fApplySavedROI2D or fApplySavedROI3D) then begin
         widget_control, wBaseID, get_uValue = state, /no_copy
            state.selXYZDataROIParamName = ['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-']
            subImage = *state.image
         widget_control, wBaseID, set_uValue = state, /no_copy
         widget_control, stack_tlb, get_uValue = stateStack, /no_copy
          if widget_info(stateStack.child_SegmentationWindow_tlb, /valid) then begin
             widget_control, stateStack.child_SegmentationWindow_tlb, get_uValue = stateSegWin, /no_copy
             if widget_info(stateSegWin.child_ROIObjectWindow_tlb, /valid) then child_ROIObjectWindow_tlb = stateSegWin.child_ROIObjectWindow_tlb else child_ROIObjectWindow_tlb = -1L
             widget_control, stateStack.child_SegmentationWindow_tlb, set_uValue = stateSegWin, /no_copy
          endif
         widget_control, stack_tlb, set_uValue = stateStack, /no_copy

         if widget_info(child_ROIObjectWindow_tlb, /valid) then begin
          widget_control, wBaseID, get_uValue = state, /no_copy
              state.selXYZDataROIParamName = s_ROIOM_getSelectedXYZObjectROIParamName(wTopBase = child_ROIObjectWindow_tlb)
          widget_control, wBaseID, set_uValue = state, /no_copy
          widget_control, child_ROIObjectWindow_tlb, get_uValue = ROIMStack, /no_copy
              oROIGroup = *ROIMStack.poCurrROI2DGroup
          widget_control, child_ROIObjectWindow_tlb, set_uValue = ROIMStack, /no_copy
         endif else oROIGroup = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos)

         widget_control, wBaseID, get_uValue = state, /no_copy
         if obj_valid(oROIGroup) then begin
            pParamStruct = oROIGroup->getpParamStruct()
            segXBox0 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ShowBox in Original Picture [x0]'))[0]]
            segXBox1 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ShowBox in Original Picture [x1]'))[0]]
            segYBox0 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ShowBox in Original Picture [y0]'))[0]]
            segYBox1 = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ShowBox in Original Picture [y1]'))[0]]

              clusterMask = -1
              case 1 of
                 fApplySavedROI2D: begin
                    if (state.selXYZDataROIParamName[0] ne '-NO SELECTION-') then $
                        clusterMask = oROIGroup->getGroupMaskInParamValOrder(selROIParamName = state.selXYZDataROIParamName[0])
                    if (clusterMask[0] eq -1) then if (state.selXYZDataROIParamName[1] ne '-NO SELECTION-') then $
                        clusterMask = oROIGroup->getGroupMaskInParamValOrder(selROIParamName = state.selXYZDataROIParamName[1])
                    if (clusterMask[0] eq -1) then if (state.selXYZDataROIParamName[2] ne '-NO SELECTION-') then $
                        clusterMask = oROIGroup->getGroupMaskInParamValOrder(selROIParamName = state.selXYZDataROIParamName[2])
                   endcase
                 fApplySavedROI3D: begin
                    if (widget_info(child_ROIObjectWindow_tlb, /valid)) then begin
                        widget_control, child_ROIObjectWindow_tlb, get_uValue = ROIMStack, /no_copy
                           if (obj_valid(*ROIMStack.poCurrROI3DGroup)) then clusterMask = (*ROIMStack.poCurrROI3DGroup)->getGroupMask(zPos = zPos)
                        widget_control, child_ROIObjectWindow_tlb, set_uValue = ROIMStack, /no_copy
                    endif
                    if (clusterMask[0] eq -1) then begin
                        oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos)
                        if (obj_valid(oROI3DGroup)) then clusterMask = oROI3DGroup->getGroupMask(zPos = zPos)
                    endif
                   endcase
              endcase

              if ((clusterMask[0] eq -1) or (max(clusterMask) eq 0)) then begin
                 clusterMask = oROIGroup->getGroupMask()
                 whereNonZero = where(clusterMask ne 0)
                 if (whereNonZero[0] ne -1) then begin
                   maxCM = max(clusterMask[whereNonZero], min = minCM)
                   if (maxCM gt minCM) then clusterMask[whereNonZero] = (clusterMask[whereNonZero]-minCM) * (205./(maxCM-minCM)) + 50
                 endif
              endif

              dimCM = size(clusterMask ,/dim)
              if ((dimCM[0] ne (segXBox1-segXBox0+1)) or (dimCM[1] ne (segYBox1-segYBox0+1))) then clusterMask = congrid(clusterMask, (segXBox1-segXBox0+1), (segYBox1-segYBox0+1))

              subImage[segXBox0:segXBox1, segYBox0:segYBox1] = clusterMask
              subImage = subImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
          endif else subImage = subImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
         widget_control, wBaseID, set_uValue = state, /no_copy
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
         endif else subImage = congrid(subImage, state.zoomXSize, state.zoomYSize, /minus, interp = state.fInterpolateZoom, /center)
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


pro s_Image_ROC_updateZoomWindow, wBaseID = wBaseID
    widget_control, wBaseID, get_uValue = state, /no_copy
          ; if the Zoom window exists, make it the proper size and load the zoomed image into it. if it does not exists, create it.
       if widget_info(state.child_zoomWindow_tlb, /valid) then begin
          widget_control, state.child_zoomWindow_tlb, tlb_Set_Title = state.wTitle
          widget_control, state.zoomDraw_tlb, draw_XSize = state.zoomXSize, draw_YSize = state.zoomYSize
          wSet, state.zoomWindowID
          wShow, state.zoomWindowID
          if state.fTvScaleZoomWindow then tvscl, *state.subImage else tv, uInt(*state.subImage * state.imageBitMapScale)
       endif else begin
             ; Get Offset positions for the non-existing zoom window.
          widget_control, wBaseID, tlb_Get_Size = sizes, tlb_Get_Offset = offSets
          xpos = offSets[0] + state.xs + 20
          ypos = offSets[1] + sizes[1] - state.ys - 15

             ; Zoom window does not exist. Create it.
          wZoomBase = widget_base(Title = state.wTitle, XOffset = xpos, YOffset = ypos, group_leader = wBaseID, tlb_Frame_Attr = 1)
          wZoomDraw = widget_draw(wZoomBase, XSize = state.zoomXSize, YSize = state.zoomYSize, button_events = 1, event_pro = 's_Image_Zoom_Window_event')
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
       print, '   |->mean(intensity) = ', mom[0], ' |->sd(intensity) = ', sqrt(mom[1])

       if (n_elements(state) ne 0) then begin
         if (state.fSaveGreyResult or state.fSaveRGBResult) then begin
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
            while (strLen(strImageNumber) lt numberIndexDigits) do strImageNumber = strCompress('0'+strImageNumber, /rem)

            if state.fSaveGreyResult then write_tiff, strCompress(path + 'GreyZoomImage_' +strCompress('clus' + string(clusPos)+'_', /rem) $
                                               + strCompress('ch' + string(chPos)+'_', /rem) $
                                               + strCompress('z' + string(zPos)+'_', /rem) $
                                               + strCompress('t' + string(tPos), /rem) $
                                               + '.tif'), tvrd()
            if state.fSaveRGBResult then  write_tiff, strCompress(path + 'RGBZoomImage_' +strCompress('clus' + string(clusPos)+'_', /rem) $
                                               + strCompress('ch' + string(chPos)+'_', /rem) $
                                               + strCompress('z' + string(zPos)+'_', /rem) $
                                               + strCompress('t' + string(tPos), /rem) $
                                               + '.tif'), tvrd(true = 1)
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
             s_Surface_Method_AnalyseXRays_colors, {redraw_image, top : stackState.child_XRayAnalysis_Window_tlb}
             if modeControl then s_Surface_Method_AnalyseXRays_DRAW_EVENTS, {WIDGET_DRAW,$
                                                                             ID : child_window_id,$
                                                                             TOP : stackState.child_XRayAnalysis_Window_tlb,$
                                                                             HANDLER : 0,$
                                                                             TYPE : 1,$
                                                                             X  : 0, Y : 0,$
                                                                             PRESS  :0,$
                                                                             RELEASE: 1,$
                                                                             CLICKS: 1,$
                                                                             MODIFIERS :0,$
                                                                             CH : 0,$
                                                                             KEY : 0L}
            endif
            widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
        endif
        widget_control, wBaseID, set_uValue = state, /no_copy
     endif
end


pro s_Image_ROC_optControl, ev
    widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue, /no_copy
    case uValue of
       'CUTMODE': begin
          state.fCut = 1 - state.fCut           ; Switch Flag (1/0 ON/OFF) for Cut Control
          widget_control, state.wCutButtom, sensitive = 0
          widget_control, state.wZoomButtom, sensitive = 1
          widget_control, state.wZoomFactor, sensitive = 0
          widget_control, state.wInterpolateZoomOnOff, sensitive = 0
        endcase
       'ZOOMMODE': begin
          state.fCut = 1 - state.fCut           ; Switch Flag (1/0 ON/OFF) for Cut Control
          widget_control, state.wCutButtom, sensitive = 1
          widget_control, state.wZoomButtom, sensitive = 0
          widget_control, state.wZoomFactor, sensitive = 1
          widget_control, state.wInterpolateZoomOnOff, sensitive = 1
        endcase
       'INTERPOLATEZOOMONOFF': state.fInterpolateZoom = s_ToggleButtonOnOffState(state.wInterpolateZoomOnOff)
       'TVSCALEZOOMBOXONOFF': state.fTvScaleZoomWindow = s_ToggleButtonOnOffState(state.wTvScaleZoomWindowOnOff)
       'SAVERGBRESULTONOFF': state.fSaveRGBResult = s_ToggleButtonOnOffState(state.wSaveRGBResultOnOff)
       'SAVEGREYRESULTONOFF': state.fSaveGreyResult = s_ToggleButtonOnOffState(state.wSaveGreyResultOnOff)
       'SELECTSEGMENTATIONBOXONOFF': begin            ; Switch Flag (1/0 ON/OFF) for Segmentation Control
          state.fSelectSegBox = s_ToggleButtonOnOffState(state.wSelectSegBoxOnOff)
          if state.fSelectSegBox then if state.fShowSegBox then state.fShowSegBox = s_ToggleButtonOnOffState(state.wShowSegBoxOnOff)
        endcase
       'KEEPFILTERMATRICESONOFF': begin
            state.fKeepFilterMatrix = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
            if not(state.fKeepFilterMatrix) then begin
               if state.fShowStandPC then state.fShowStandPC = s_ToggleButtonOnOffState(state.wShowStandPCOnOff)
               if state.fShowCCRI then state.fShowCCRI = s_ToggleButtonOnOffState(state.wShowCCRIOnOff)
               if state.fShowJMSI then state.fShowJMSI = s_ToggleButtonOnOffState(state.wShowJMSIOnOff)
               if state.fShowCC then state.fShowCC = s_ToggleButtonOnOffState(state.wShowCCOnOff)
               if state.fShowDeltaR then state.fShowDeltaR = s_ToggleButtonOnOffState(state.wShowDeltaROnOff)
            endif
        endcase
       'SHOWPCSTANDONOFF': begin
            state.fShowStandPC = s_ToggleButtonOnOffState(state.wShowStandPCOnOff)
            if state.fShowStandPC then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWCCRIWINONOFF': begin
            state.fShowCCRI = s_ToggleButtonOnOffState(state.wShowCCRIOnOff)
            if state.fShowCCRI then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWJMSIWINONOFF': begin
            state.fShowJMSI = s_ToggleButtonOnOffState(state.wShowJMSIOnOff)
            if state.fShowJMSI then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWCCWINONOFF': begin
            state.fShowCC = s_ToggleButtonOnOffState(state.wShowCCOnOff)
            if state.fShowCC then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWDELTARWINONOFF': begin
            state.fShowDeltaR = s_ToggleButtonOnOffState(state.wShowDeltaROnOff)
            if state.fShowDeltaR then if (state.fKeepFilterMatrix ne 1) then state.fShowSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SHOWXRATEWINONOFF': state.fUpdateXRay = s_ToggleButtonOnOffState(state.wUpdateXRayOnOff)
       'SHOWSELECTEDBOXONOFF': begin
          state.fShowSegBox = s_ToggleButtonOnOffState(state.wShowSegBoxOnOff)
          if state.fShowSegBox then if state.fKeepFilterMatrix then state.fSelectSegBox = s_ToggleButtonOnOffState(state.wKeepFilterMatrixOnOff)
        endcase
       'SELECTENTIREWINDOW': begin
          state.xs = 0
          state.xd = state.xSize -1
          state.ys = 0
          state.yd = state.ySize -1
        endcase
       'HISTSEGONOFF': begin      ; Switch Flag (1/0 ON/OFF) for the View-Histogram-window
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
    s_Image_ROC_colors, {redraw_image, top : ev.top}
end


pro s_Image_ROC_colors, ev
    widget_control, ev.top, get_uValue = state, /no_copy
    case (tag_names(ev, /structure)) of
        'WIDGET_BUTTON': begin
               tvLCT, state.r, state.g, state.b, state.bottom
               XColors, group = ev.top, nColors = state.nColors, bottom = state.bottom, notifyID = [ev.id, ev.top], title = 'ZImage Colors (' + StrTrim(state.drawIndex,2) + ')'
               widget_control, state.wBaseID, map = 0
               state.fMap = 0
            endcase
        'REDRAW_IMAGE': begin
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
                     subImage = s_Image_ROC_getSubImage(wBaseID = ev.top)
                     widget_control, ev.top, get_uValue = state, /no_copy
                     if ptr_valid(state.subImage) then *state.subImage = subImage else state.subImage = ptr_new(subImage, /no_copy)
                     widget_control, ev.top, set_uValue = state, /no_copy
                       ; if the Zoom window exists, make it the proper size and load the zoomed image into it. if it does not exists, create it.
                     s_Image_ROC_updateZoomWindow, wBaseID = ev.top
                      ; actualize Segmentation-Histogram
                     if fHistSegWin then s_Image_ROC_updateCreateHistSegWin, wBaseID = ev.top
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
                      if (state.fTvScaleZoomWindow) then tvscl, *state.subImage else tv, uInt(*state.subImage * state.imageBitMapScale)
                   endif
              endif
        endcase
    endcase
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Image_ROC_cleanUp, wBase
   widget_control, wBase, get_uValue = state, /no_copy
   if (n_elements(state) ne 0) then begin
      if widget_info(state.groupLeader, /valid) then begin
         widget_control, state.groupLeader, get_uValue = stateParent, /no_copy
         if (n_elements(stateParent) ne 0) then begin
            if (s_ToggleButtonOnOffState(stateParent.wViewWindowButton) eq 1) then void = s_ToggleButtonOnOffState(stateParent.wViewWindowButton)
            widget_control, state.groupLeader, set_uValue = stateParent, /no_copy
         endif
      endif
      for i = 0,n_tags(state)-1 do begin
         case size(state.(i), /tname) of
            'POINTER': ptr_free, state.(i)
            'OBJREF': obj_destroy, state.(i)
         else:
         endcase
      endfor
   endif
end


pro s_Image_ROC_drawEvents, ev
       ; This ev handler continuously draws and erases the zoom box until it receives an UP ev from the draw widget. then it turns draw widget motion events OFF.
    widget_control, ev.top, get_uValue = state, /no_copy
    case ((['PRESS', 'RELEASE', 'MOTION', 'SCROLL' ])[ev.type]) of
       'PRESS': begin
            if ((['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT'])[ev.press] eq 'RIGHT') then begin
                state.fMap = 1 - state.fMap
                widget_control, state.wBaseID, map = state.fMap
                widget_control, ev.top, set_uValue = state, /no_copy
                return
            endif

             ; Set the static corners of the box to current cursor location.
            state.xs = (( ev.x > 0) < (state.xSize-1) > 0)
            state.ys = (( ev.y > 0) < (state.ySize-1) > 0)
            state.xd = state.xs
            state.yd = state.ys

             ; Turn draw MOTION events ON.
            widget_control, ev.id, Draw_Motion_Events = 1

            ; Take over the color index for the zoom box drawing color. Store the
            ; current (r,g,b) values for color index so you can restore the current colors after the zoom box is drawn.
            tvLCT, r, g, b, /get
            state.r_old = r[state.colorIndex]
            state.g_old = g[state.colorIndex]
            state.b_old = b[state.colorIndex]
          endcase
       'RELEASE': begin
            ; Turn motion events off.
          widget_control, ev.id, Draw_Motion_Events = 0
          if ((['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT'])[ev.release] eq 'RIGHT') then begin
             widget_control, ev.top, set_uValue = state, /no_copy
             return
          endif

          wSet, state.drawIndex
          tvLCT, state.r, state.g, state.b;        tvLCT, state.r_old, state.g_old, state.b_old, state.colorIndex

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

               ; Set the zoom factor and determine the new X and Y sizes of the Zoom window.
          state.zoomXSize = (x[1] - x[0] + 1) * zoomFactor
          state.zoomYSize = (y[1] - y[0] + 1) * zoomFactor
          state.wTitle = strCompress( string(x[1]-x[0]+1) +' [x], '+ string(y[1]-y[0]+1) + ' [y] )')

          widget_control, ev.top, set_uValue = state, /no_copy
              subImage = s_Image_ROC_getSubImage(wBaseID = ev.top)
              widget_control, ev.top, get_uValue = state, /no_copy
                 if ptr_valid(state.subImage) then *state.subImage = subImage else state.subImage = ptr_new(subImage, /no_copy)
              widget_control, ev.top, set_uValue = state, /no_copy
                 ; if the Zoom window exists, make it the proper size and load the zoomed image into it. if it does not exists, create it.
              s_Image_ROC_updateZoomWindow, wBaseID = ev.top
              if not(widget_info(ev.top, /valid)) then return
          widget_control, ev.top, get_uValue = state, /no_copy

              ; show or actualize histogram
          if state.fHistSegWin then begin
             if (n_elements(state) gt 0) then begin
                widget_control, ev.top, set_uValue = state, /no_copy
                  s_Image_ROC_updateCreateHistSegWin, wBaseID = ev.top
                widget_control, ev.top, get_uValue = state, /no_copy
             endif
          endif

              ; if the controls were mapped, unmap them.
          if state.fMap then begin
             widget_control, state.wBaseID, map = 0
             state.fMap = 0
          endif
       endcase
       'MOTION': begin
          ; Most of the action in this ev handler occurs here while we are waiting for an UP ev to occur. As long as we don't get it, keep erasing the old zoom box and drawing a new one.
          ; Erase the old zoom box.
          wSet, state.drawIndex
          tvLCT, state.r, state.g, state.b, state.bottom
          device, copy = [0, 0, state.xSize, state.ySize, 0, 0, state.pixIndex]

          ; Update the dynamic corner of the zoom box to the current cursor location.
          state.xd = ( ev.x > 0) < (state.xSize-1)
          state.yd = ( ev.y > 0) < (state.ySize-1)

          ; Load a green color in color index 1 to draw the zoom box with. Draw the zoom box. Load old color talble
          tvLCT, 0B, 255B, 0B, state.colorIndex
          plots, [state.xs, state.xs, state.xd, state.xd, state.xs], [state.ys, state.yd, state.yd, state.ys, state.ys], /device, color = state.colorIndex
          tvLCT, state.r, state.g, state.b, state.bottom
       endcase
    endcase
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Image_RankOrderCut_Window, image, basePos = basePos, Bottom = bottom,$
                                 groupLeader = groupLeader, pColorState = pColorState, application_tlb = application_tlb, paramStr = paramStr

    on_error, 1
       ;Check the validity of the group identifier.
    if (n_elements(groupLeader) ne 0) then begin
       if not(widget_info(groupLeader, /valid)) then begin
          print,'Error: the group identifier is not valid.  |--> Returning to the main application.'
          return
       endif
    endif else groupLeader = 0l

       ; Make sure a window has been opened in this IDL session for accurate color number determination.
    window, /pixmap, xSize = 10, ySize = 10
    wDelete, !D.window

       ; Set color parameters
    if not(ptr_valid(pColorState)) then begin
       loadCT, 3
       tvLCT, r, g, b, /get
       colorState = {colTbl: 3,$
                     rgbValues: make_array(3,intScale, /byte) + [[r],[g],[b]],$
                     stretchBotTop: [0,intScale-1],$
                     gamma: 1.,$
                     fOpac: 0b,$
                     opacValues: 128b,$
                     opacValues: make_array(256, /byte, value = opacValues),$
                     opacStretchBotTop: [0,intScale-1],$
                     opacGamma: 1.}
       pColorState = ptr_new(colorState, /no_copy)
    endif

       ; Set image & rank bar parameters
    intScale = 256
    dimI = size(image, /dim)
    if (n_elements(minI) eq 0) then minI = min(image, max = maxI)
    sortI = sort(image)
    outI = congrid(image[sortI], intScale)
    outIs = fltArr(10, intScale)
    for i = 0,9 do outIs[i,*] = outI

    outI = fltArr(4,10,intScale)
    outI[0,*,*] = ((*pColorState.rgbValues)[0])[outIs[*,*]]
    outI[1,*,*] = ((*pColorState.rgbValues)[1])[outIs[*,*]]
    outI[2,*,*] = ((*pColorState.rgbValues)[2])[outIs[*,*]]
    outI[3,*,*] = 255

    oImage = obj_new('IDLgrImage', outI, dimensions = [dimI[0],dimI[1]], interleave = 0, blend_func = [3,4], name = paramStr)
    thisModel = obj_new('IDLgrModel')
    thisModel->add, oImage

    viewRect = [0, 0, dimI[0], dimI[1]]
    thisView = obj_new('IDLgrView', Viewplane_Rect = viewRect)
    thisView->add, thisModel

    if n_elements(basePos) eq 0 then basePos = [100,100]
    if n_elements(paramStr) eq 0 then paramStr = 'Image'

       ; Create a top-level base for this program. No resizing of this base.
    device, decomposed = 0
    wTopBase = widget_base(title = 's_Rank |->' + paramStr,$
                           xpad = 2, ypad = 2,$
                           xOffSet = basePos[0],$
                           yOffSet = basePos[1],$
                           tlb_frame_attr = 1)

    application_tlb = wTopBase

       ; Create two bases. One for controls and the other for the draw widget. Leave the control base unmapped for now.
    winPos = [0,100]
    wBaseID = widget_base(wTopBase, map = 0, column = 1)

    drawSize = [50,255]
    drawBase = widget_base(wTopBase)
    draw = widget_draw(drawBase, xSize = drawSize[0], ySize = drawSize[1], button_events = 1, retain = 2, event_pro = 's_Image_ROC_drawEvents')

    modeID = widget_button(wBaseID, value = 'Options', event_pro = 's_Image_ROC_optControl', /menu)
       wCutButtom = widget_button(modeID, value = 'Cut Mode', uValue = 'CUTMODE', Sensitive = 0)
       wZoomButtom = widget_button(modeID, value = 'Zoom Mode', uValue = 'ZOOMMODE')
       wInterpolateZoomOnOff = widget_button(modeID, value = 'Interpolate Zoom (on )', uValue = 'INTERPOLATEZOOMONOFF')
       if (groupLeader ne 0) then wSelectSegBoxOnOff = widget_button(modeID, value = 'Select Segmentation Box (off)', uValue = 'SELECTSEGMENTATIONBOXONOFF', /sep) else wSelectSegBoxOnOff = 0
       if (groupLeader ne 0) then wShowSegBoxOnOff = widget_button(modeID, value = 'Show Selected Box (off)', uValue = 'SHOWSELECTEDBOXONOFF') else wShowSegBoxOnOff = 0
       wTvScaleZoomWindowOnOff = widget_button(modeID, value = 'Tv Scale Zoom Box (on )', uValue = 'TVSCALEZOOMBOXONOFF', event_pro = 's_Image_ROC_optControl', /sep)
       colors = widget_button(modeID, value = 'Change Color Table (LUT)', event_pro = 's_Image_ROC_colors')
       wKeepFilterMatrixOnOff = widget_button(modeID, value = 'Keep Filter Matrices (off)', uValue = 'KEEPFILTERMATRICESONOFF', /sep)
       wShowStandPCOnOff = widget_button(modeID, value = 'Show Standardized Pearson Window (off)', uValue = 'SHOWPCSTANDONOFF')
       wShowCCRIOnOff = widget_button(modeID, value = 'Show CCRI Window (off)', uValue = 'SHOWCCRIWINONOFF')
       wShowJMSIOnOff = widget_button(modeID, value = 'Show JMSI Window (off)', uValue = 'SHOWJMSIWINONOFF')
       wShowCCOnOff = widget_button(modeID, value = 'Show CC Window (off)', uValue = 'SHOWCCWINONOFF')
       wShowDeltaROnOff = widget_button(modeID, value = 'Show DeltaR Window (off)', uValue = 'SHOWDELTARWINONOFF')
       wUpdateXRayOnOff = widget_button(modeID, value = 'Update X-Ray Window (off)', uValue = 'SHOWXRATEWINONOFF', /sep)
       wSaveRGBResultOnOff = widget_button(modeID, value = 'Save RGB-Result Automatically (off)', uValue = 'SAVERGBRESULTONOFF', /sep)
       wSaveGreyResultOnOff = widget_button(modeID, value = 'Save Result Automatically (off)', uValue = 'SAVEGREYRESULTONOFF')
    widget_control, wZoomFactor, set_droplist_select = 2

    modeID = widget_button(wBaseID, value = 'Select Entire Window', event_pro = 's_Image_ROC_optControl', uValue = 'SELECTENTIREWINDOW')
    modeID = widget_button(wBaseID, value = 'Plot Windows', event_pro = 's_Image_ROC_optControl', /menu)
    wHistSegWinOnOff = widget_button(modeID, value = 'Segmentation Histogram (off)', uValue = 'HISTSEGONOFF')

       ; Get the window index number of the draw widget. Make the draw widget the current graphics window and display the image in it.
    widget_control, wTopBase, /realize
    widget_control, draw, get_value = drawIndex
    wSet, drawIndex
    tv, congrid(outI,drawSize[0], drawSize[1])

    widget_control, wTopBase, tlb_set_title = strCompress('s_2DView |-> Image Dimensions ('+ string(xSize) +' [x], '+ string(ySize) + ' [y] )')

      ; Create a pixmap window the same size as the draw widget window.
      ; Store its window index number in a local variable. Display the
      ; image you just put in the draw widget in the pixmap window.
    window, /free, XSize = xSize, YSize = ySize, /pixmap
    pixIndex = !D.window
    tv, image
    tvLCT, r, g, b, /get

    widget_control, wDraw, get_value = thisWindow
    thisWindow->draw, thisView

       ; Create an state structure to hold information required by the program.
    state = {wBaseID: wBaseID,$
       groupLeader: groupLeader,$
       image: ptr_new(image),$   ; The original image.
       dimI:dimI,$               ; Dimensions of the image.
       pColorState:pColorState,$ ; Pointer in Colour State Parameters.
       subImage:ptr_new(),$     ; The scaled and resized subImage.
       drawIndex:drawIndex,$     ; The draw window index number.
       pixIndex:pixIndex,$       ; The pixmap window index number.
       nColors:nColors,$         ; The number of colors for the image.
       bottom:bottom,$           ; The bottom color index.
       colorIndex:colorIndex,$   ; The drawing color index.
       xs:0,$                    ; X static corner of the zoom box.
       ys:0,$                    ; Y static corner of the zoom box.
       xd:0,$                    ; X dynamic corner of the zoom box.
       yd:0,$                    ; Y dynamic corner of the zoom box.
       xysd_old: intArr(4),$ ; old zoom box positions.
       wCutButtom:wCutButtom,$      ; Cut - Button ID.
         fCut: 1,$   ; A flag to activate/desactivate the cut option (1/ON, 2/OFF)
       wZoomButtom:wZoomButtom,$    ; Zoom - Button ID.
       wZoomFactor:wZoomFactor,$    ; ZoomFaktor - Button ID.
       wSaveRGBResultOnOff:wSaveRGBResultOnOff,$
         fSaveRGBResult: 0b,$
       wSaveGreyResultOnOff:wSaveGreyResultOnOff,$
         fSaveGreyResult: 0b,$
       wInterpolateZoomOnOff:wInterpolateZoomOnOff,$
         fInterpolateZoom: 1b,$
       wTvScaleZoomWindowOnOff:wTvScaleZoomWindowOnOff,$    ; TvScaleZoomWindow on/off button
         fTvScaleZoomWindow: 1b,$    ; Flag for TvScaleZoomWindow on/off (1/0) control
       wSelectSegBoxOnOff:wSelectSegBoxOnOff,$  ; Show Segment Procedure on/off button
         fSelectSegBox: 0b,$
       wShowSegBoxOnOff: wShowSegBoxOnOff ,$
         fShowSegBox: 0b,$
       wKeepFilterMatrixOnOff: wKeepFilterMatrixOnOff,$
         fKeepFilterMatrix: 0b,$
       wShowStandPCOnOff: wShowStandPCOnOff,$
         fShowStandPC: 0b,$
       wShowCCRIOnOff: wShowCCRIOnOff,$
         fShowCCRI: 0b,$
       wShowJMSIOnOff: wShowJMSIOnOff,$
         fShowJMSI: 0b,$
       wShowCCOnOff: wShowCCOnOff,$
         fShowCC: 0b,$
       wUpdateXRayOnOff: wUpdateXRayOnOff,$
         fUpdateXRay: 0b,$
       wShowDeltaROnOff: wShowDeltaROnOff,$
         fShowDeltaR: 0b,$
       wHistSegWinOnOff: wHistSegWinOnOff,$
         fHistSegWin: 0b,$
         child_HistSegWin_tlb:-1L,$; Histogram window widget ID.
       r:r,$                       ; The red color vector.
       g:g,$                       ; The green color vector.
       b:b,$                       ; The blue color vector.
       r_old:0,$                   ; The user's red color value.
       g_old:0,$                   ; The user's green color value.
       b_old:0,$                   ; The user's blue color value.
       child_zoomWindow_tlb: -1L,$ ; Zoomed imageData draw widget ID.
       zoomDraw_tlb:-1L,$          ; Zoomed imageData draw widget ID.
       zoomWindowID:-1,$           ; Zoomed imageData window index number.
       zoomFactor: .5,$            ; The initial zoom factor.
       zoomXSize:0,$
       zoomYSize:0,$
       fMap:0b,$                     ; A flag to tell if the controls are mapped.
       wTitle:'|',$
       selXYZDataROIParamName: ['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-'],$
       currROIGroupFileName: s_getPathForSystem()}

       ; Store the state structure in the user value of the top-level base. & Register this program and set up the event loop.
    widget_control, wTopBase, set_uValue = state, /no_copy
    XManager, 's_Image_RankOrderCut_Window', wTopBase, Cleanup = 's_Image_ROC_cleanUp', group_leader = groupLeader, /no_block
    s_Image_ROC_colors, {redraw_image, top : wTopBase}
end ; of ZIMAGE ****************************************************************************
