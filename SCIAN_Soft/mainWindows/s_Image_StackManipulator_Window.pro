;_____________________________IOISIOI____________________
; NAME:
;      s_Image_StackManipulator_Window_Event
;
; PURPOSE:
;       - Sets or Destroys Image Parameter Structure
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________
pro s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateSeg_tlb = stateSeg_tlb, stateObj_tlb = stateObj_tlb, stateTrack_tlb = stateTrack_tlb,$
                          tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, segPos = segPos,$
                          totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum, totalClusNum = totalClusNum,$
                          selClusName = selClusName,$
                          ROIObjGroupPos = ROIObjGroupPos, selROIGroupObj = selROIGroupObj,$
                          ROIObjParamPos = ROIObjParamPos, selROIObjParam = selROIObjParam,$
                          ROIObjSubParamPos = ROIObjSubParamPos, selROIObjSubParam = selROIObjSubParam,$
                          oCurrROI2DGroup = oCurrROI2DGroup, oCurrROI3DGroup = oCurrROI3DGroup,$
                          ROIObjPos = ROIObjPos, selROIObject = selROIObject,$
                          ROIObjModelPos = ROIObjModelPos, selROIObjModel = selROIObjModel,$
                          ROIObjModelParamPos = ROIObjModelParamPos, selROIObjModelParam = selROIObjModelParam,$
                          selTrackObj = selTrackObj, selGroupParam = selGroupParam, selGroupSubParam = selGroupSubParam

   widget_control, stack_tlb, get_uValue = stateStack, /no_copy
        ;get selected image position
      widget_control, stateStack.wListTime, get_uValue = uValueList, /no_copy
        tPos = uValueList.active
        totalTNum = n_elements(*uValueList.value)
      widget_control, stateStack.wListTime, set_uValue = uValueList, /no_copy
        ;get selected channel position
      widget_control, stateStack.wListChannel, get_uValue = uValueList, /no_copy
        chPos = uValueList.active
        totalChNum = n_elements(*uValueList.value)
      widget_control, stateStack.wListChannel, set_uValue = uValueList, /no_copy
        ;get selected ZSlice position
      widget_control, stateStack.wListZSlice, get_uValue = uValueList, /no_copy
        zPos = uValueList.active
        totalZNum = n_elements(*uValueList.value)
      widget_control, stateStack.wListZSlice, set_uValue = uValueList, /no_copy

        ;get selected Segmentation Cluster & selected Segmentation Method in Cluster
      if widget_info(stateStack.child_SegmentationWindow_tlb, /valid) then begin
         stateSeg_tlb = stateStack.child_SegmentationWindow_tlb
         widget_control, stateStack.child_SegmentationWindow_tlb, get_uValue = stateSegWindow, /no_copy
             ;get selected Segmentation Cluster
         widget_control, stateSegWindow.wListSegCluster, get_uValue = uValueList, /no_copy
             clusPos = uValueList.active < (n_elements(*uValueList.value)-1)
             totalClusNum = n_elements(*uValueList.value)
             selClusName = (*uValueList.value)[clusPos]
         widget_control, stateSegWindow.wListSegCluster, set_uValue = uValueList, /no_copy
             ;get selected Segmentation Method
         widget_control, stateSegWindow.wListSelectSegObj, get_uValue = uValueList, /no_copy
             segPos = uValueList.active < (n_elements(*uValueList.value)-1)
         widget_control, stateSegWindow.wListSelectSegObj, set_uValue = uValueList, /no_copy

              ;get selected ROIGroupObj & and ROIObjectParameter
         if widget_info(stateSegWindow.child_ROIObjectWindow_tlb, /valid) then begin
            stateObj_tlb = stateSegWindow.child_ROIObjectWindow_tlb
            widget_control, stateSegWindow.child_ROIObjectWindow_tlb, get_uValue = stateObjWindow, /no_copy
            if (n_elements(stateObjWindow) gt 0) then begin
                  ;get current ROIGroupObjs
               if (n_elements(oCurrROI2DGroup) gt 0) then oCurrROI2DGroup = *stateObjWindow.poCurrROI2DGroup
               if (n_elements(oCurrROI3DGroup) gt 0) then oCurrROI3DGroup = *stateObjWindow.poCurrROI3DGroup
                 ;get selected ROIGroupObj
               widget_control, stateObjWindow.wListROIGroup, get_uValue = uValueList, /no_copy
                  ROIObjGroupPos = uValueList.active < (n_elements(*uValueList.value)-1)
                  selROIGroupObj = (*uValueList.value)[ROIObjGroupPos]
               widget_control, stateObjWindow.wListROIGroup, set_uValue = uValueList, /no_copy
                  ;get selected ROIObjParam
               widget_control, stateObjWindow.wListSelectROIGroupParams, get_uValue = uValueList, /no_copy
                  ROIObjParamPos = uValueList.active < (n_elements(*uValueList.value)-1)
                  selROIObjParam = (*uValueList.value)[ROIObjParamPos]
               widget_control, stateObjWindow.wListSelectROIGroupParams, set_uValue = uValueList, /no_copy
                  ;get selected ROIObjSubParam
               widget_control, stateObjWindow.wListSelectROIGroupSubParams, get_uValue = uValueList, /no_copy
                  ROIObjSubParamPos = uValueList.active < (n_elements(*uValueList.value)-1)
                  selROIObjSubParam = (*uValueList.value)[ROIObjSubParamPos]
               widget_control, stateObjWindow.wListSelectROIGroupSubParams, set_uValue = uValueList, /no_copy

                  ;get selected ROIObject & and ROIObjectModel
               if widget_info(stateObjWindow.child_ROITrackWindow_tlb, /valid) then begin
                  stateTrack_tlb = stateObjWindow.child_ROITrackWindow_tlb
                  widget_control, stateObjWindow.child_ROITrackWindow_tlb, get_uValue = stateTrackWindow, /no_copy
                  if (n_elements(stateTrackWindow) gt 0) then begin
                         ;get selected ROIObject
                     widget_control, stateTrackWindow.wListTrackObj, get_uValue = uValueList, /no_copy
                        trackObjPos = (uValueList.active < (n_elements(*uValueList.value)-1)) > 0
                        selTrackObj = (*uValueList.value)[trackObjPos]
                     widget_control, stateTrackWindow.wListTrackObj, set_uValue = uValueList, /no_copy
                         ;get selected ROIObjectTrack
                     widget_control, stateTrackWindow.wListTrackGroupParams, get_uValue = uValueList, /no_copy
                        trackGroupParamPos = (uValueList.active < (n_elements(*uValueList.value)-1)) > 0
                        selGroupParam = (*uValueList.value)[trackGroupParamPos]
                     widget_control, stateTrackWindow.wListTrackGroupParams, set_uValue = uValueList, /no_copy
                          ;get selected ROIObjectTrackParameter
                     widget_control, stateTrackWindow.wListTrackSubParams, get_uValue = uValueList, /no_copy
                        trackSubParamPos = (uValueList.active < (n_elements(*uValueList.value)-1)) > 0
                        selSubParam = (*uValueList.value)[trackSubParamPos]
                     widget_control, stateTrackWindow.wListTrackSubParams, set_uValue = uValueList, /no_copy
                     widget_control, stateObjWindow.child_ROITrackWindow_tlb, set_uValue = stateTrackWindow, /no_copy
                  endif
               endif
               widget_control, stateSegWindow.child_ROIObjectWindow_tlb, set_uValue = stateObjWindow, /no_copy
            endif
         endif
         widget_control, stateStack.child_SegmentationWindow_tlb, set_uValue = stateSegWindow, /no_copy

       endif else begin ; stateSegWindow closed
             clusPos = 0
             totalClusNum = 1
             selClusName = 'Clus0'
       endelse
    widget_control, stack_tlb, set_uValue = stateStack, /no_copy
end


pro s_ISM_setSelectedImageClusterSegPosition, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    widget_control, stack_tlb, get_uValue = stateStack, /no_copy
       widget_control, stateStack.wListTime, get_uValue = uValueList, /no_copy
         uValueList.active = tPos
       widget_control, stateStack.wListTime, set_uValue = uValueList, /no_copy
       widget_control, stateStack.wListChannel, get_uValue = uValueList, /no_copy
         uValueList.active = chPos
       widget_control, stateStack.wListChannel, set_uValue = uValueList, /no_copy
       widget_control, stateStack.wListZSlice, get_uValue = uValueList, /no_copy
          uValueList.active = zPos
       widget_control, stateStack.wListZSlice, set_uValue = uValueList, /no_copy

       if (widget_info(stateStack.child_SegmentationWindow_tlb, /valid) and (n_elements(clusPos) ne 0)) then begin
         widget_control, stateStack.child_SegmentationWindow_tlb, get_uValue = stateSegWindow, /no_copy
          widget_control, stateSegWindow.wListSegCluster, get_uValue = uValueList, /no_copy
              uValueList.active = clusPos < (n_elements(*uValueList.value)-1)
          widget_control, stateSegWindow.wListSegCluster, set_uValue = uValueList, /no_copy
         widget_control, stateStack.child_SegmentationWindow_tlb, set_uValue = stateSegWindow, /no_copy
       endif
    widget_control, stack_tlb, set_uValue = stateStack, /no_copy
end


pro s_ISM_Window_Button_Event, ev
    s_ISM_getProjectInfo, stack_tlb = ev.top, tPos = tPos, chPos = chPos, zPos = zPos,$
                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum
    widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue
    fUpdade = 1b
    slash = path_sep()
    case uValue of
       'DELETECHANNELINALLTIMESETS':begin
                        state.fListSelect = 1
                        dummy = 0
                        if (totalTNum ge 0) then begin
                           for i = totalTNum-1, 0, -1 do $
                             dummy = (*state.pImageStackInfoObject)->removeSelectedObject(tPos = i, chPos = chPos, zPos = zPos, moveActive = state.fListSelect)
                           if (dummy eq 1) then begin
                             widget_control, state.wListChannel, get_uValue = uValueFileParameter, /no_copy
                              if (uValueFileParameter.active eq (totalChNum-1) ) then listIndex = (uValueFileParameter.active - 1) > 0 else listIndex = uValueFileParameter.active
                              uValueFileParameter.active = listIndex
                             widget_control, state.wListChannel, set_list_select = uValueFileParameter.active, set_uValue = uValueFileParameter, /no_copy
                             (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] - 1) > 0
                             listTop = state.wTopBase
                             listID = state.wListChannel
                             widget_control, ev.top, set_uValue = state, /no_copy
                               s_ISM_List_Event, {WIDGET_LIST, ID: listID, TOP: listTop, HANDLER: 0b, INDEX: listIndex, CLICKS: 1}
                             widget_control, ev.top, get_uValue = state, /no_copy
                           endif
                        endif
         endcase
       'DELETE':begin
                        case state.fListSelect of
                           0:begin
                               wListActive = state.wListTime
                               listID = state.wListTime
                             endcase
                           1:begin
                               wListActive = state.wListChannel
                               listID = state.wListChannel
                             endcase
                           2:begin
                               wListActive = state.wListZSlice
                               listID = state.wListZSlice
                             endcase
                        endcase
                        widget_control, wListActive, get_uValue = uValueFileParameter, /no_copy
                           if (uValueFileParameter.active ge 0) then begin
                              dummy = (*state.pImageStackInfoObject)->removeSelectedObject(tPos = tPos, chPos = chPos, zPos = zPos, moveActive = state.fListSelect)
                              if (dummy eq 1) then begin
                                 listIndex = (uValueFileParameter.active - 1) > 0
                                 uValueFileParameter.active = listIndex
                              endif
                           endif else dummy = 0b
                        widget_control, wListActive, set_list_select = uValueFileParameter.active, set_uValue = uValueFileParameter, /no_copy

                        if (dummy eq 1) then begin
                           fUpdade = 0b
                           listTop = state.wTopBase
                           widget_control, ev.top, set_uValue = state, /no_copy
                              s_ISM_List_Event, {WIDGET_LIST, ID: listID, TOP: listTop, HANDLER: 0b, INDEX: listIndex, CLICKS: 1}
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
         endcase
       'MOVEUP':begin
                        fUpdade = 0b
                        case state.fListSelect of
                           0:wListActive = state.wListTime
                           1:wListActive = state.wListChannel
                           2:wListActive = state.wListZSlice
                        endcase
                        widget_control, wListActive, get_uValue = uValueFileParameter, /no_copy
                           if (uValueFileParameter.active gt 0) then begin
                             dummy = (*state.pImageStackInfoObject)->moveSelectedObject( tPos = tPos, chPos = chPos, zPos = zPos,$
                                                                 moveTo = uValueFileParameter.active-1, moveActive = state.fListSelect)
                             if (dummy eq 1) then uValueFileParameter.active = uValueFileParameter.active - 1
                           endif
                        widget_control, wListActive, set_list_select = uValueFileParameter.active, set_uValue = uValueFileParameter, /no_copy
         endcase
       'MOVEDOWN':begin
                        fUpdade = 0b
                        case state.fListSelect of
                           0:wListActive = state.wListTime
                           1:wListActive = state.wListChannel
                           2:wListActive = state.wListZSlice
                        endcase
                        widget_control, wListActive, get_uValue = uValueFileParameter, /no_copy
                           dummy = (*state.pImageStackInfoObject)->moveSelectedObject( tPos = tPos, chPos = chPos, zPos = zPos,$
                                                                                                moveTo = uValueFileParameter.active+1, moveActive = state.fListSelect)
                           if (dummy eq 1) then uValueFileParameter.active = uValueFileParameter.active + 1
                        widget_control, wListActive, set_list_select = uValueFileParameter.active, set_uValue = uValueFileParameter, /no_copy
         endcase
       'IMPORTIMAGESASHUYGENSOLYMPUSSELALLDIR':begin
                      (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct

                      res = dialog_message('The selected folder must NOT contain saved masks or any other TIF file, only the original Huygens/Olympus sequence. Continue?', /CANCEL)
                      if (strCmp(res, 'CANCEL', /FOLD_CASE) eq 1) then break

                      imgFolder = dialog_pickfile(title = 'Select the folder containing the Huygens/Olympus images to load',$
                                                  path  = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]],$
                                                  get_path = path, /DIRECTORY, /READ)
                      if (imgFolder[0] ne '') then begin
                        file = file_search(imgFolder[0] + '*.tif', count = nFile)
                        if (nFile gt 0) then begin
                           *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]]     = path
                           *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Mask Path'))[0]]      = path
                           *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Image Path'))[0]]     = path
                           maxXPixel    = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                           maxYPixel    = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                           imageBitRate = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                           maxTime      = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Total Number of Times'))[0]] - 1
                           maxChannel   = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] -1
                           maxzSlice    = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] - 1
                           dummy        = query_tiff(file[0], tiffInfo)
                           if (dummy ne 0) then $
                           case tiffInfo.channels of
                           1: begin
                              zRectificacion = 0
                              for i = 0, nFile-1 do begin
                                strImageName = strMid(file[i], (strPos(file[i], path_sep(), /reverse_s))+1, (strPos(file[i], '.', /reverse_s)-strPos(file[i], path_sep(), /reverse_s)-1))
                                s_getTimeChannelzSliceInfoFromString, strImageName, timeStr = timeStr, channelStr = channelStr, zSliceStr = zSliceStr, sInfo = sInfo
                                if(i eq 0) then zRectificacion = -s_getRightNumberFromString(zSliceStr)
                                if (timeStr ne -1) then timePos = s_getRightNumberFromString(timeStr) + sInfo.timeAdd else timePos = tPos
                                if (channelStr ne -1) then channelPos = s_getRightNumberFromString(channelStr) + sInfo.channelAdd else channelPos = chPos
                                ;if (zSliceStr ne -1) then zSlicePos = s_getRightNumberFromString(zSliceStr) + sInfo.zSliceAdd + zRectificacion else zSlicePos = zPos
                                if (zSliceStr ne -1) then zSlicePos = s_getRightNumberFromString(zSliceStr) + zRectificacion else zSlicePos = zPos

                                image = read_tiff(file[i])
                                szI = size(image, /dim)
                                if (size(image, /type) ne 1) then imageBitRate = 12

                                oImage = obj_new('C_sImageObject')
                                oImage->get, pParamStruct = pParamStruct
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strImageName
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * szI[0]
                                if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * szI[1]
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = timePos
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = channelPos
                                *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zSlicePos

                                (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = timePos, chPos = channelPos, zPos = zSlicePos
                                pParamStruct = -1

                                maxXPixel >= szI[0]
                                maxYPixel >= szI[1]
                                maxTime >= timePos
                                maxChannel >= channelPos
                                maxzSlice >= zSlicePos
                              endfor
                           endcase
                           else:
                           endcase
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = maxTime + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = maxChannel + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = maxzSlice + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = maxXPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = maxYPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * maxXPixel
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * maxYPixel
                           pParamStruct = -1
                        endif
                      endif
       endcase
       'IMPORTIMAGESASHUYGENSOLYMPUSSELALL':begin
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                        if (file[0] ne '') then begin
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                           maxXPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                           maxYPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                           imageBitRate = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                           maxTime = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] - 1
                           maxChannel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] -1
                           maxzSlice = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] - 1
                           nFile = n_elements(file)
                           dummy = query_tiff(file[0], tiffInfo)
                           if (dummy ne 0) then $
                           case tiffInfo.channels of
                             1:begin
                               zRectificacion = 0
                               for i = 0, nFile-1 do begin
                                    strImageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                                    s_getTimeChannelzSliceInfoFromString, strImageName, timeStr = timeStr, channelStr = channelStr, zSliceStr = zSliceStr, sInfo = sInfo
                                    if(i eq 0) then zRectificacion = -s_getRightNumberFromString(zSliceStr)
                                    if (timeStr ne -1) then timePos = s_getRightNumberFromString(timeStr) + sInfo.timeAdd else timePos = tPos
                                    if (channelStr ne -1) then channelPos = s_getRightNumberFromString(channelStr) + sInfo.channelAdd else channelPos = chPos
                                    ;if (zSliceStr ne -1) then zSlicePos = s_getRightNumberFromString(zSliceStr) + sInfo.zSliceAdd + zRectificacion else zSlicePos = zPos
                                    if (zSliceStr ne -1) then zSlicePos = s_getRightNumberFromString(zSliceStr) + zRectificacion else zSlicePos = zPos

                                    image = read_tiff(file[i])
                                    szI = size(image, /dim)
                                    if (size(image, /type) ne 1) then imageBitRate = 12

                                    oImage = obj_new('C_sImageObject')
                                    oImage->get, pParamStruct = pParamStruct
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strImageName
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                    if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * szI[0]
                                    if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * szI[1]
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = timePos
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = channelPos
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zSlicePos

                                    (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = timePos, chPos = channelPos, zPos = zSlicePos
                                    pParamStruct = -1

                                 maxXPixel >= szI[0]
                                 maxYPixel >= szI[1]
                                 maxTime >= timePos
                                 maxChannel >= channelPos
                                 maxzSlice >= zSlicePos
                             endfor
                            endcase
                            else:
                           endcase
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = maxTime + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = maxChannel + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = maxzSlice + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = maxXPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = maxYPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * maxXPixel
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * maxYPixel
                           pParamStruct = -1
                        endif
        endcase
       'IMPORTIMAGESASHUYGENSOLYMPUSSELFIRST':begin
       ; MOdified for compatibility with "All" method with all Formats...
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        File = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif')
                        if (File[0] ne '') then begin
                           ; Obtain others file names
                          
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                           maxXPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                           maxYPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                           imageBitRate = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                           maxTime = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] - 1
                           maxChannel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] -1
                           maxzSlice = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] - 1
                           ;nFile = numFilesUsed ; We need obtain the numbers of elements
                           dummy = query_tiff(file[0], tiffInfo)
                           if (dummy ne 0) then $
                           case tiffInfo.channels of
                             1:begin
                               thereAreFile = 1b
                                 zSliceInit = -1
                                 numFilesUsed = 1
                                 
                                 largoVariable = 3
                                 add = 2
                                 result = strPos(File[0], '_Z', /reverse_search)
                                 if (result eq -1) then result = strPos(File[0], '_z', /reverse_search)  else largoVariable = 4 ; if _Z need 4 elements to zslice
                                 if (result eq -1) then begin
                                    result = strPos(File[0], 'Z', /reverse_search)
                                    add = 1
                                 endif
                                 if (result eq -1) then begin
                                   result = strPos(File[0], '_co', /reverse_search)
                                   add = 3
                                 endif
                                 if (result ne -1) then zSliceInit = s_getRightNumberFromString(strMid(File[0], result+add,largoVariable))
                                 if(zSliceInit eq -1) then break
                               
                               levelAction = 1 ; 0: modify zslice, 1: modify channel, 2: modify time ... value 0 is not required... because is change in the next section- 
                               zRectificacion = 0
                               channelRectificacion = 0
                               tempNoUtil = 0
                               while thereAreFile do begin
                                    baseNameFile = strMid(File[0],0,strlen(File[0])-(4+largoVariable)) 
                                    strImageName = strMid(File[0],(strPos(File[0], slash, /reverse_s))+1, (strPos(File[0],'.', /reverse_s)-strPos(File[0], slash, /reverse_s)-1) )
                                    s_getTimeChannelzSliceInfoFromString, strImageName, timeStr = timeStr, channelStr = channelStr, zSliceStr = zSliceStr, sInfo = sInfo
                                    if(tempNoUtil eq 0) then begin
                                       zRectificacion = -s_getRightNumberFromString(zSliceStr)
                                       tempNoUtil = 1
                                    endif
                                    channelRectificacion = sInfo.channelAdd   
                                    if (timeStr ne -1) then timePos = s_getRightNumberFromString(timeStr) + sInfo.timeAdd else timePos = tPos
                                    if (channelStr ne -1) then channelPos = s_getRightNumberFromString(channelStr) + sInfo.channelAdd else channelPos = chPos
                                    ;if (zSliceStr ne -1) then zSlicePos = s_getRightNumberFromString(zSliceStr) + sInfo.zSliceAdd + zRectificacion else zSlicePos = zPos
                                    if (zSliceStr ne -1) then zSlicePos = s_getRightNumberFromString(zSliceStr) + zRectificacion else zSlicePos = zPos

                                    image = read_tiff(file[0])
                                    szI = size(image, /dim)
                                    if (size(image, /type) ne 1) then imageBitRate = 12

                                    oImage = obj_new('C_sImageObject')
                                    oImage->get, pParamStruct = pParamStruct
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strImageName
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                    if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * szI[0]
                                    if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * szI[1]
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = timePos
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = channelPos
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zSlicePos

                                    (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = timePos, chPos = channelPos, zPos = zSlicePos
                                    pParamStruct = -1

                                 maxXPixel >= szI[0]
                                 maxYPixel >= szI[1]
                                 maxTime >= timePos
                                 maxChannel >= channelPos
                                 maxzSlice >= zSlicePos
                                 
                                 ; verify if exists more files
                                 zSliceInit++ 
                                 zSliceString = strCompress(STRING(zSliceInit), /rem)
                                 if(strlen(zSliceString) eq 1) then zSliceString = '00' + zSliceString
                                 if(strlen(zSliceString) eq 2) then zSliceString = '0' + zSliceString
                                 if(largoVariable eq 4) then zSliceString = '0' + zSliceString
                                 File[0] = baseNameFile + zSliceString + ".tif"      
                                 if(FILE_TEST(File[0]) eq 0b) then begin
                                    dummy = s_getNextStringForInputFile(File[0],levelAction,-zRectificacion,-channelRectificacion)
                                    if(dummy eq "-1") then begin
                                       levelAction++
                                       dummy = s_getNextStringForInputFile(File[0],levelAction,-zRectificacion,-channelRectificacion)
                                       levelAction--
                                       if(dummy eq "-1") then thereAreFile = 0b else File[0] = dummy
                                    endif else File[0] = dummy
                                    zSliceInit = s_getRightNumberFromString(strMid(File[0], result+add,largoVariable)) ; reset last value
                                 endif                          
                             endwhile
                            endcase
                            else:
                           endcase
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = maxTime + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = maxChannel + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = maxzSlice + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = maxXPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = maxYPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * maxXPixel
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * maxYPixel
                           pParamStruct = -1
                        endif
        endcase
       'LEICAMULTITIFFTOHUYGENS':begin
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                        numberTimeDigits = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]] > strLen(strcompress(string(totalTNum-1), /rem))
                        if (file[0] ne '') then begin
                           if (n_elements(file) gt 1) then file = [file[1:n_elements(file)-1], file[0]]
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                           maxXPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                           maxYPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                           imageBitRate = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                           maxTime = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] - 1
                           maxChannel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] -1
                           maxzSlice = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] - 1
                           nFile = n_elements(file)
                           ok = query_tiff(file[0], tiffInfo)
                           if ok then $
                           case tiffInfo.channels of
                             1:begin
                               timepos = tPos
                               channelPos = chPos
                               for i = 0, nFile-1 do begin
                                 ok = query_tiff(file[i], tiffInfo)
                                 zSlicePos = 0b
                                 for zPos = 0, tiffInfo.NUM_IMAGES-1 do begin
                                    strImageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, 2)

                                    image = read_tiff(file[i], image_index = zSlicePos)
                                    szI = size(image, /dim)
                                    if (size(image, /type) ne 1) then begin
                                      imageBitRate = 12
                                      image = 4096 - temporary(image)
                                    endif else image = 255 - temporary(image)

                                    oImage = obj_new('C_sImageObject')
                                    oImage->get, pParamStruct = pParamStruct
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strImageName
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                    if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * szI[0]
                                    if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * szI[1]
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = timePos
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = channelPos
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zSlicePos

                                    (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = timePos, chPos = channelPos, zPos = zSlicePos

                                    maxXPixel >= szI[0]
                                    maxYPixel >= szI[1]
                                    maxTime >= timePos
                                    maxChannel >= channelPos
                                    maxzSlice >= zSlicePos

                                      ; write images as Huygens Series
                                    strImageTime = strCompress(string(timePos), /rem)
                                    while (strLen(strImageTime) lt numberTimeDigits) do strImageTime = strCompress('0' + strImageTime, /rem)
                                    strImageTime = strCompress('_t' + strImageTime, /rem)

                                    strImageChannel = strCompress(string(channelPos), /rem)
                                    while (strLen(strImageChannel) lt 2) do strImageChannel = strCompress('0' + strImageChannel, /rem)
                                    strImageChannel = strCompress('_ch' + strImageChannel, /rem)

                                    strImageZStack = strCompress(string(zSlicePos), /rem)
                                    while (strLen(strImageZStack) lt 3) do strImageZStack = strCompress('0' + strImageZStack, /rem)
                                    strImageZStack = strCompress('_z' + strImageZStack, /rem)

                                    newIName = strImageName + strImageTime + strImageChannel + strImageZStack
                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = newIName

                                    case imageBitRate of
                                       8: write_tiff, path + newIName + '.tif', image, 1
                                       12: write_tiff, path + newIName + '.tif', image, 1, /long
                                       else: write_tiff, path + newIName + '.tif', image, 1
                                    endcase
                                    zSlicePos += 1
                                 endfor
                                 timePos += 1
                              endfor
                            endcase
                            else:
                           endcase
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = maxTime + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = maxChannel + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = maxzSlice + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = maxXPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = maxYPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * maxXPixel
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * maxYPixel
                           pParamStruct = -1
                        endif
         endcase
       'RGBBMPTOCHTIMESERIES':begin
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.bmp', /multiple_files)
                        
                        numberTimeDigits = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]] > strLen(strcompress(string(totalTNum-1), /rem))
                        if (file[0] ne '') then begin
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                           xSizePixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                           ySizePixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                           ;xSizeReal = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
                           ;ySizeReal = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
                           imageBitRate = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                           maxTime = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] - 1
                           maxChannel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] - 1
                           maxzSlice = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] - 1

                           nFile = n_elements(file)
                           ok = query_bmp(file[0], bmpInfo)
                           if ok then begin
                              case bmpInfo.channels of
                                3:begin
                                  channelPos = chPos
                                  maxChannel = 5
                                  zSlicePos = 0b
                                  maxTime += nFile
                                  for i = 0, nFile-1 do begin
                                     ok = query_bmp(file[i], bmpInfo)
                                     strImageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, strlen(file[i])-1)
                                     strImageName = strMid(strImageName, 0, strlen(strImageName)-4)
                                     image = read_bmp(file[i], 0 ,0 ,0 ,bmpHeader)
                                                                          
                                     if (size(image, /type) eq 3) then imageBitRate = 12 else imageBitRate = 8
                                     xSizePixel >= bmpHeader.biwidth
                                     ySizePixel >=  bmpHeader.biheight
                                     maxzSlice >= zSlicePos
   
                                     strImageTime = strCompress(string(tPos), /rem)
                                     while (strLen(strImageTime) lt numberTimeDigits) do strImageTime = strCompress('0' + strImageTime, /rem)
                                     strImageZPos = strCompress(string(zSlicePos), /rem)
                                     while (strLen(strImageZPos) lt 3) do strImageZPos = strCompress('0' + strImageZPos, /rem)
                                     newName =  strImageName + '_t' + strImageTime
                                        ; IMPORTANT bmp files are [b,g,r] instead of [r,g,b] !!!
                                     bmpOrder = [2,1,0]
                                     for chPos = 0,4 do begin  
                                        strImageChannel = strCompress(string(chPos), /rem)
                                        while (strLen(strImageChannel) lt 2) do strImageChannel = strCompress('0' + strImageChannel, /rem)
                                        completImageName = path + newName + '_ch' + strImageChannel + '_z' + strImageZPos + '.tif'
                                        case chPos of
                                           4: write_tiff, completImageName, reform(image[0,*,*] < image[1,*,*] < image[2,*,*]), 1
                                           3: write_tiff, completImageName, reform( round((1.*image[0,*,*] + image[1,*,*] + image[2,*,*]) / 3.) ), 1
                                           else:write_tiff, completImageName, reform(image[bmpOrder[chPos],*,*]), 1
                                        endcase
                                        ok = query_tiff(completImageName, tiffHeader)
                                        if ok then begin 
                                           state.cutValues = [0, xSizePixel - 1, 0, ySizePixel - 1]
                                           widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) + ' ] | y[ ' $
                                                           + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) + ' ]'
                                           oImage = obj_new('C_sImageObject')
                                           oImage->get, pParamStruct = pParamStruct
                                           onlyName = strMid(completImageName, strPos(completImageName, slash, /reverse_s)+1)
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strMid(onlyName, 0, strLen(onlyName)-4)
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = xSizePixel
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = ySizePixel
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = xSizePixel - 1
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = ySizePixel - 1
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1.* xSizePixel
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1.* ySizePixel
;                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1.*xSizeReal
;                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1.*ySizeReal
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = tPos
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = chPos
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zPos
                                           (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = tPos, chPos = chPos, zPos = zPos
                                        endif else return
                                     endfor
                                     tPos += 1
                                 endfor
                              endcase
                              else:
                              endcase
                           endif

                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = maxTime
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = maxChannel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = maxzSlice + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = xSizePixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = ySizePixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1.*xSizePixel
                               ;*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = xSizePixel
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1.*ySizePixel
;                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = ySizePixel
                          if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Interval [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Interval [real]'))[0]] = 1
                           pParamStruct = -1
                        endif
        endcase
       'ADDIMAGESASASHUYGENSTOTIMESERIES':begin
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                        if (file[0] ne '') then begin
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                           maxXPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                           maxYPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                           imageBitRate = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                           maxTime = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] - 1
                           plusTime = maxTime+1
                           maxChannel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] - 1
                           maxzSlice = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] - 1
                           nFile = n_elements(file)
                           dummy = query_tiff(file[0], tiffInfo)
                           stop
                           case tiffInfo.channels of
                             1:begin
                                  for i = 0, nFile-1 do begin
                                     strImageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                                     s_getTimeChannelzSliceInfoFromString, strImageName, timeStr = timeStr, channelStr = channelStr, zSliceStr = zSliceStr, sInfo = sInfo
                                     if (timeStr ne -1) then timePos = s_getRightNumberFromString(timeStr) + plusTime + sInfo.timeAdd else timePos = tPos+plusTime
                                     if (channelStr ne -1) then channelPos = s_getRightNumberFromString(channelStr) + sInfo.channelAdd else channelPos = chPos
                                     if (zSliceStr ne -1) then zSlicePos = s_getRightNumberFromString(zSliceStr) + sInfo.zSliceAdd else zSlicePos = zPos

                                     image = read_tiff(file[i])
                                     szI = size(image, /dim)
                                     if (size(image, /type) ne 1) then  imageBitRate = 12

                                     oImage = obj_new('C_sImageObject')
                                     oImage->get, pParamStruct = pParamStruct
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strImageName
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                     if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * szI[0]
                                     if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * szI[1]
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = timePos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = channelPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zSlicePos

                                     (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = timePos, chPos = channelPos, zPos = zSlicePos
                                     pParamStruct = -1

                                     maxXPixel >= szI[0]
                                     maxYPixel >= szI[1]
                                     maxChannel >= channelPos
                                     maxzSlice >= zSlicePos
                                 endfor
                             endcase
                             else:
                           endcase
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = maxTime + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = maxChannel + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = maxzSlice + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = maxXPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = maxYPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                                 *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * maxXPixel
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                                 *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * maxYPixel
                           pParamStruct = -1
                          endif
         endcase
       'ADDIMAGESASASHUYGENSTOSELECTEDTIME':begin
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                        if (file[0] ne '') then begin
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                           maxXPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                           maxYPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                           imageBitRate = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                           maxTime = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] - 1
                           plusTime = maxTime+1
                           maxChannel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] - 1
                           maxzSlice = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] - 1
                           nFile = n_elements(file)
                           dummy = query_tiff(file[0], tiffInfo)
                           case tiffInfo.channels of
                             1:begin
                                     for i = 0, nFile-1 do begin
                                          strImageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                                          s_getTimeChannelzSliceInfoFromString, strImageName, timeStr = timeStr, channelStr = channelStr, zSliceStr = zSliceStr, sInfo = sInfo
                                          timePos = tPos
                                          if (channelStr ne -1) then channelPos = s_getRightNumberFromString(channelStr) + sInfo.channelAdd else channelPos = chPos
                                          if (zSliceStr ne -1) then zSlicePos = s_getRightNumberFromString(zSliceStr) + sInfo.zSliceAdd else zSlicePos = zPos

                                          image = read_tiff(file[i])
                                          szI = size(image, /dim)
                                          if (size(image, /type) ne 1) then imageBitRate = 12

                                          oImage = obj_new('C_sImageObject')
                                          oImage->get, pParamStruct = pParamStruct
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strImageName
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                          if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * szI[0]
                                          if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * szI[1]
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = timePos
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = channelPos
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zSlicePos

                                          (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = timePos, chPos = channelPos, zPos = zSlicePos
                                          pParamStruct = -1

                                          maxXPixel >= szI[0]
                                          maxYPixel >= szI[1]
                                          maxTime >= timePos
                                          maxChannel >= channelPos
                                          maxzSlice >= zSlicePos
                                     endfor
                             endcase
                             else:
                           endcase
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = maxTime + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = maxChannel + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = maxzSlice + 1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = maxXPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = maxYPixel
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] = 1. * maxXPixel
                           if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] eq -1) then $
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] = 1. * maxYPixel
                           pParamStruct = -1
                          endif
         endcase
       'EXPORTIMAGESASHUYGENS':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    numberTimeDigits = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]] > strLen(strcompress(string(totalTNum-1), /rem))
                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]] = numberTimeDigits
                    format = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Format'))[0]]
                    bitType = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                    readImagePathName = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]
                    newImageFileName = dialog_pickfile(title = 'Select Image Path and Name', /write, get_path = writeImagePathName, filter = format, path = readImagePathName)

                    if (newImageFileName ne '') then begin
                          ;define old and new Image Path Name and new Image Name Base
                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = writeImagePathName
                        if ( (strMid(newImageFileName, strLen(newImageFileName)-4, 4)) ne '.tif') then newImageFileName = newImageFileName + '.tif'
                        newImageName = strMid(newImageFileName,(strPos(newImageFileName, slash, /reverse_s))+1, (strPos(newImageFileName,'.', /reverse_s)-strPos(newImageFileName, slash, /reverse_s)-1) )

                        xSizePix = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                        ySizePix = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                        xSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
                        ySize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]

                        state.cutValues[[0,2]] = 0
                        state.cutValues[1] = xSizePix-1
                        state.cutValues[3] = ySizePix-1

                           ; set new image sizes
                        for k = 0, totalTNum-1  do $
                           for j = 0, totalChNum-1  do begin
                              widget_control, ev.top, set_uValue = state, /no_copy
                                 s_ISM_setSelectedImageClusterSegPosition, stack_tlb = ev.top, tPos = k, chPos = j, zPos = 0

                                  widget_control, ev.top, get_uValue = state, /no_copy
                                    ev_struct = {top: state.wTopBase,$
                                                 id: state.wListzSlice,$
                                                 handler: state.wTopBase,$
                                                 index: 0,$
                                                 clicks: 1}
                                  widget_control, ev.top, set_uValue = state, /no_copy
                                  s_ISM_List_Event, ev_struct

                                 s_ISM_getProjectInfo, stack_tlb = ev.top, totalZNum = totalZNum
                              widget_control, ev.top, get_uValue = state, /no_copy
                              for i = 0, totalZNum-1  do begin
                              oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                              if obj_valid(oImage) then begin
                                  oImage->get, pParamStruct = pParamStruct
                                  *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0])] = state.cutValues[1] - state.cutValues[0] + 1
                                  *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0])] = state.cutValues[3] - state.cutValues[2] + 1
                                  *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0])] = 1. * xSize * (state.cutValues[1] - state.cutValues[0] + 1) / xSizePix
                                  *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0])] = 1. * ySize * (state.cutValues[3] - state.cutValues[2] + 1) / ySizePix
                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = state.cutValues[1] - state.cutValues[0]
                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = state.cutValues[3] - state.cutValues[2]
                              endif
                           endfor
                        endfor
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0])] = state.cutValues[1] - state.cutValues[0] + 1
                        *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0])] = state.cutValues[3] - state.cutValues[2] + 1
                        *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0])] = 1. * xSize * (state.cutValues[1] - state.cutValues[0] + 1) / xSizePix
                        *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0])] = 1. * ySize * (state.cutValues[3] - state.cutValues[2] + 1) / ySizePix

                        for k = 0, totalTNum-1 do $
                           for j = 0, totalChNum-1 do begin
                              widget_control, ev.top, set_uValue = state, /no_copy
                              s_ISM_setSelectedImageClusterSegPosition, stack_tlb = ev.top, tPos = k, chPos = j, zPos = 0

                              widget_control, ev.top, get_uValue = state, /no_copy
                                ev_struct = {top: state.wTopBase,$
                                             id: state.wListzSlice,$
                                             handler: state.wTopBase,$
                                             index: 0,$
                                             clicks: 1}
                              widget_control, ev.top, set_uValue = state, /no_copy
                              s_ISM_List_Event, ev_struct

                              s_ISM_getProjectInfo, stack_tlb = ev.top, totalZNum = totalZNum
                              widget_control, ev.top, get_uValue = state, /no_copy
                                 for i = 0, totalZNum-1 do begin
                                    oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                                    if obj_valid(oImage) then begin
                                       oImage->get, pParamStruct = pParamStruct
                                       oldImageName = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]

                                       if ((state.cutValues[0] eq state.cutValues[1]) or (state.cutValues[2] eq state.cutValues[3])) then begin
                                            state.cutValues[[0,2]] = 0
                                            state.cutValues[1] = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]-1
                                            state.cutValues[3] = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]-1
                                       endif

                                       fOK = query_tiff(readImagePathName+oldImageName+format, info)
                                       if fOK then image = read_tiff(readImagePathName+oldImageName+format) else image = bytArr(state.cutValues[1]+1, state.cutValues[3]+1)
                                       szI = size(image, /dim)

                                       if (state.cutValues[1] gt (szI[0] - 1)) or ((state.cutValues[3] gt (szI[1] - 1))) then begin
                                          state.cutValues[[1,3]] = state.cutValues[[1,3]] < (szI - 1)
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                       endif

                                       image = image[state.cutValues[0] : state.cutValues[1], state.cutValues[2] : state.cutValues[3] ]

                                       strImageTime = strCompress(string(k), /rem)
                                       while (strLen(strImageTime) lt numberTimeDigits) do strImageTime = strCompress('0' + strImageTime, /rem)
                                       strImageTime = strCompress('_t' + strImageTime, /rem)

                                       strImageChannel = strCompress(string( j), /rem)
                                       while (strLen(strImageChannel) lt 2) do strImageChannel = strCompress('0' + strImageChannel, /rem)
                                       strImageChannel = strCompress('_ch' + strImageChannel, /rem)

                                       strImageZStack = strCompress(string(i), /rem)
                                       while (strLen(strImageZStack) lt 3) do strImageZStack = strCompress('0' + strImageZStack, /rem)
                                       strImageZStack = strCompress('_z' + strImageZStack, /rem)

                                       newIName = newImageName + strImageTime + strImageChannel + strImageZStack
                                       *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = newIName

                                       case bitType of
                                          8: write_tiff, writeImagePathName + newIName + '.tif', image, 1
                                          12: write_tiff, writeImagePathName + newIName + '.tif', image, 1, /long
                                          else: write_tiff, writeImagePathName + newIName + '.tif', image, 1
                                       endcase
                                    endif
                                 endfor
                        endfor

                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        state.cutValues = [0, -1 + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                                           0, -1 + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]]
                        widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) + ' ]  ||  y[ ' $
                        + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) + ' ]'
                        pParamStruct = -1
                    endif
         endcase
       'CONVERTIMAGESTOGREYCHANNEL':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                    if (file[0] ne '') then begin
                      *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                      image = read_tiff(file[0])
                      dimI = size(image, /dim)
                      imageName = strMid(file[0],(strPos(file[0], slash, /reverse_s))+1, (strPos(file[0],'.', /reverse_s)-strPos(file[0], slash, /reverse_s)-1) )
                      greyImage = bytArr(dimI[1], dimI[2]) + image[0,*,*] * 0.2125 + image[1,*,*] * 0.7154 + image[2,*,*] * 0.0721
                      write_tiff, path + 'grey_' + imageName + '.tif', greyImage, 1
                      for i = 1, n_elements(file)-1 do begin
                         image = read_tiff(file[i])
                         imageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                         dimI = size(image, /dim)
                         greyImage = bytArr(dimI[1], dimI[2]) + image[0,*,*] * 0.2125 + image[1,*,*] * 0.7154 + image[2,*,*] * 0.0721
                         write_tiff, path + 'grey_' + imageName + '.tif', greyImage, 1
                      endfor
                   endif
         endcase
       'CONVERTIMAGESTORGBCHANNEL':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                    if (file[0] ne '') then begin
                       *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                       image = read_tiff(file[0])
                       dimI = size(image, /dim)
                       imageName = strMid(file[0],(strPos(file[0], slash, /reverse_s))+1, (strPos(file[0],'.', /reverse_s)-strPos(file[0], slash, /reverse_s)-1) )
                       write_tiff, path + 'r_' + imageName + '.tif', reform(image[0,*,*]), 1
                       write_tiff, path + 'g_' + imageName + '.tif', reform(image[1,*,*]), 1
                       write_tiff, path + 'b_' + imageName + '.tif', reform(image[2,*,*]), 1
                       for i = 1, n_elements(file)-1 do begin
                        image = read_tiff(file[i])
                        imageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                        dimI = size(image, /dim)
                        write_tiff, path + 'r_' + imageName + '.tif', reform(image[0,*,*]), 1
                        write_tiff, path + 'g_' + imageName + '.tif', reform(image[1,*,*]), 1
                        write_tiff, path + 'b_' + imageName + '.tif', reform(image[2,*,*]), 1
                       endfor
                    endif
         endcase
       'CONVERTIMAGESTOHSVCHANNELS':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                    if (file[0] ne '') then begin
                       *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                       image = read_tiff(file[0])
                       dimI = size(image, /dim)
                       imageName = strMid(file[0],(strPos(file[0], slash, /reverse_s))+1, (strPos(file[0],'.', /reverse_s)-strPos(file[0], slash, /reverse_s)-1) )
                       color_convert, image, image_hsv, /rgb_hsv
                       write_tiff, path + 'h_' + imageName + '.tif', reform(image_hsv[0,*,*]), /float, 1
                       write_tiff, path + 's_' + imageName + '.tif', reform(image_hsv[1,*,*]), /float, 1
                       write_tiff, path + 'v_' + imageName + '.tif', reform(image_hsv[2,*,*]), /float, 1
                       for i = 1, n_elements(file)-1 do begin
                          image = read_tiff(file[i])
                          imageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                          dimI = size(image, /dim)
                          color_convert, image, image_hsv, /rgb_hsv
                          write_tiff, path + 'h_' + imageName + '.tif', reform(image_hsv[0,*,*]), /float, 1
                          write_tiff, path + 's_' + imageName + '.tif', reform(image_hsv[1,*,*]), /float, 1
                          write_tiff, path + 'v_' + imageName + '.tif', reform(image_hsv[2,*,*]), /float, 1
                       endfor
                    endif
         endcase
       'CONVERTIMAGESTOHSVCHANNELS8BITSCALE':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                    if (file[0] ne '') then begin
                       *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                       image = read_tiff(file[0])
                       dimI = size(image, /dim)
                       imageName = strMid(file[0],(strPos(file[0], slash, /reverse_s))+1, (strPos(file[0],'.', /reverse_s)-strPos(file[0], slash, /reverse_s)-1) )
                       color_convert, image, image_hsv, /rgb_hsv
                       write_tiff, path + 'h_' + imageName + '.tif', byte(reform(image_hsv[0,*,*]) * (255./360)), 1
                       write_tiff, path + 's_' + imageName + '.tif', byte(reform(image_hsv[1,*,*]) * 255), 1
                       write_tiff, path + 'v_' + imageName + '.tif', byte(reform(image_hsv[2,*,*]) * 255), 1
                       for i = 1, n_elements(file)-1 do begin
                          image = read_tiff(file[i])
                          imageName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                          dimI = size(image, /dim)
                          color_convert, image, image_hsv, /rgb_hsv
                          write_tiff, path + 'h_' + imageName + '.tif', byte(reform(image_hsv[0,*,*]) * (255./360)), 1
                          write_tiff, path + 's_' + imageName + '.tif', byte(reform(image_hsv[1,*,*]) * 255), 1
                          write_tiff, path + 'v_' + imageName + '.tif', byte(reform(image_hsv[2,*,*]) * 255), 1
                       endfor
                    endif
         endcase
       'ADDZTOFILLBYYOYA':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                    if (file[0] ne '') then begin
                       if query_tiff(file[0], tiffInfo) then begin
                          *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                          for i = 0, n_elements(file)-1 do begin
                             if query_tiff(file[i], tiffInfo) then begin
                                image = read_tiff(file[i])
                                whereCell = where(image eq 0)
                                image[*] = 0
                                if (whereCell[0] ne -1) then image[whereCell] = 255
                                newName = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                                newName = strMid(newName, 0, strPos(newName,'fill', /reverse_s)+4) + '_z' + strMid(newName, strPos(newName,'fill', /reverse_s)+5,2)
                                write_tiff, path + newName + '.tif', image, 1
                             endif
                          endfor
                       endif
                    endif
         endcase
       'FILLCHANNEL':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                          file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                          if (file[0] ne '') then begin
                             nFile = n_elements(file)
                             zSlice_number = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = floor (1. * nFile / zSlice_number)

                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                             numberlength = strLen(strCompress(string(s_getRightNumberFromString(*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]])), /rem) )
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]] = numberlength > *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]]
                             if (nFile gt 1) then file[[0,nFile-1]] = file[ [nFile-1, 0] ]

                        dummy = query_tiff(file[0], tiffInfo)
                        case tiffInfo.channels of
                           1:begin
                                  for i = 0, nFile-1 do begin
                                     image = read_tiff(file[i])
                                     szI = size(image, /dim)
                                     if (i eq 0) then begin
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                        state.cutValues = [ 0, szI[0]-1, 0, szI[1]-1]
                                        widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) + ' ] | y[ ' $
                                                                                                  + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) + ' ]'
                                     endif
                                     oImage = obj_new('C_sImageObject')
                                     oImage->get, pParamStruct = pParamStruct
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = $
                                        strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1

                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = floor(1.*i/zSlice_number)
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = chPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = i mod zSlice_number

                                     (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = floor(1.*i/zSlice_number), chPos = chPos, zPos = i mod zSlice_number
                                    pParamStruct = -1
                                  endfor
                             endcase
                           3:begin
                                  strImageName = strMid(file[0],(strPos(file[0], slash, /reverse_s))+1, (strPos(file[0],'.', /reverse_s)-strPos(file[0], slash, /reverse_s)-1) )
                                  numberlength = s_countRightNumberFromString(strImageName)
                                  strImageName = strMid(strImageName, 0, strLen(strImageName)-numberlength)
                                  for i = 0, nFile-1 do begin
                                     image = read_tiff(file[i])
                                     szI = size(image, /dim)

                                     if (i eq 0) then begin
                                        numberTimeDigits = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]]
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[1]
                                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[2]
                                        state.cutValues = [ 0, szI[1]-1, 0, szI[2]-1]
                                        widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) + ' ] | y[ ' $
                                              + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) + ' ]'
                                     endif

                                     strImageTime = strCompress(string(floor(1.*i/zSlice_number)), /rem)
                                     numberTimeDigits = numberTimeDigits > strLen(strImageTime)
                                     while (strLen(strImageTime) lt numberTimeDigits) do strImageTime = strCompress('0' + strImageTime, /rem)

                                     strImageZStack = strCompress(string( i mod zSlice_number ), /rem)
                                     while (strLen(strImageZStack) lt 3) do strImageZStack = strCompress('0' + strImageZStack, /rem)

                                     for j = 0, 2 do begin
                                        strImageChannel = strCompress(string( j), /rem)
                                        while (strLen(strImageChannel) lt 2) do strImageChannel = strCompress('0' + strImageChannel, /rem)

                                           ; save rgb-Images in Huygens-code
                                        strName = strCompress(strImageName+'_t'+strImageTime+'_z'+strImageZStack+'_ch'+strImageChannel)
                                        write_tiff, strCompress(path + strCompress(strName+'.tif', /rem)), (bytArr(szI[1], szI[2]) + image[j,*,*]), 1

                                         oImage = obj_new('C_sImageObject')
                                         oImage->get, pParamStruct = pParamStruct
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strName
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[1]
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[2]
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[1]-1
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[2]-1

                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = floor(1.*i/zSlice_number)
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = j
                                         *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = i mod zSlice_number

                                         (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = floor(1.*i/zSlice_number), chPos = j, zPos = i mod zSlice_number
                                          pParamStruct = -1
                                     endfor
                                  endfor
                               endcase
                           else:
                        endcase
                     endif
                     pParamStruct = -1
         endcase
       'LOADIMAGESASTIMESERIES':begin
                        fUpdade = 0b
                        state.fListSelect = 0b
                        widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.id, set_uValue = 'LOADIMAGESERIES'
                             s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'LOADIMAGESASTIMESERIES'
                        widget_control, ev.top, get_uValue = state, /no_copy
         endcase
       'LOADIMAGESASCHANNELSERIES':begin
                        fUpdade = 0b
                        state.fListSelect = 1b
                        widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.id, set_uValue = 'LOADIMAGESERIES'
                             s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'LOADIMAGESASCHANNELSERIES'
                        widget_control, ev.top, get_uValue = state, /no_copy
         endcase
       'LOADIMAGESASZSLICESERIES':begin
                        fUpdade = 0b
                        state.fListSelect = 2b
                        widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.id, set_uValue = 'LOADIMAGESERIES'
                             s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'LOADIMAGESASZSLICESERIES'
                        widget_control, ev.top, get_uValue = state, /no_copy
         endcase
        ; for windows openfile buffer issue...
       'LOADIMAGESASTIMESERIES_FROMFOLDER':begin
                        fUpdade = 0b
                        state.fListSelect = 0b
                        widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.id, set_uValue = 'LOADIMAGESERIES_FROMFOLDER'
                             s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'LOADIMAGESASTIMESERIES_FROMFOLDER'
                        widget_control, ev.top, get_uValue = state, /no_copy
         endcase
       'LOADIMAGESASCHANNELSERIES_FROMFOLDER':begin
                        fUpdade = 0b
                        state.fListSelect = 1b
                        widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.id, set_uValue = 'LOADIMAGESERIES_FROMFOLDER'
                             s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'LOADIMAGESASCHANNELSERIES_FROMFOLDER'
                        widget_control, ev.top, get_uValue = state, /no_copy
         endcase
       'LOADIMAGESASZSLICESERIES_FROMFOLDER':begin
                        fUpdade = 0b
                        state.fListSelect = 2b
                        widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.id, set_uValue = 'LOADIMAGESERIES_FROMFOLDER'
                             s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'LOADIMAGESASZSLICESERIES_FROMFOLDER'
                        widget_control, ev.top, get_uValue = state, /no_copy
         endcase

         
       'LOADIMAGESERIES':begin
                          (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                          file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
                          if (file[0] ne '') then begin

;                             file = shift(file, -1)
                             nFile = n_elements(file)
                             case state.fListSelect of
                              0: *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = $
                                                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] > nFile
                              1: *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = $
                                                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] > nFile
                              2: *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = $
                                                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] > nFile
                             endcase

                             imageBitRate = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                             numberlength = strLen(strCompress(string(s_getRightNumberFromString(*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]])), /rem) )
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]] = numberlength > *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]]

                             for i = 0, nFile-1 do begin
                               image = read_tiff(file[i])
                               szI = size(image, /dim)
                               if (size(image, /type) ne 1) then imageBitRate = 12
                               if (i eq 0) then begin
                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                  state.cutValues = [ 0, szI[0]-1, 0, szI[1]-1]
                                  widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) + ' ] | y[ ' $
                                                  + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) + ' ]'
                               endif

                              oImage = obj_new('C_sImageObject')
                              oImage->get, pParamStruct = pParamStruct
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = $
                                 strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1

                              case state.fListSelect of
                                0:begin
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = i
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = chPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zPos
                                     (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = i, chPos = chPos, zPos = zPos
                                 endcase
                                1:begin
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = tPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = i
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zPos
                                     (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = tPos, chPos = i, zPos = zPos
                                 endcase
                                2:begin
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = tPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = chPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = i
                                     (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = tPos, chPos = chPos, zPos = i
                                 endcase
                              endcase
                           endfor
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                         endif
                         pParamStruct = -1
         endcase
       'LOADIMAGESERIES_FROMFOLDER':begin
                          (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            selectedFolder = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path,  /DIRECTORY)
                            
                            if (selectedFolder[0] eq '') then break
                            file = FILE_SEARCH(selectedFolder,'*',/TEST_REGULAR) 

                          if (file[0] ne '') then begin

;                             file = shift(file, -1)
                             nFile = n_elements(file)
                             case state.fListSelect of
                              0: *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] = $
                                                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]] > nFile
                              1: *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] = $
                                                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]] > nFile
                              2: *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] = $
                                                                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] > nFile
                             endcase

                             imageBitRate = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]]
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                             numberlength = strLen(strCompress(string(s_getRightNumberFromString(*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]])), /rem) )
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]] = numberlength > *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Number of Time Digits'))[0]]

                             for i = 0, nFile-1 do begin
                               image = read_tiff(file[i])
                               szI = size(image, /dim)
                               if (size(image, /type) ne 1) then imageBitRate = 12
                               if (i eq 0) then begin
                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                                  state.cutValues = [ 0, szI[0]-1, 0, szI[1]-1]
                                  widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) + ' ] | y[ ' $
                                                  + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) + ' ]'
                               endif

                              oImage = obj_new('C_sImageObject')
                              oImage->get, pParamStruct = pParamStruct
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = $
                                 strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1

                              case state.fListSelect of
                                0:begin
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = i
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = chPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zPos
                                     (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = i, chPos = chPos, zPos = zPos
                                 endcase
                                1:begin
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = tPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = i
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = zPos
                                     (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = tPos, chPos = i, zPos = zPos
                                 endcase
                                2:begin
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Number'))[0]] = tPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Number'))[0]] = chPos
                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = i
                                     (*state.pImageStackInfoObject)->addSelectedImageObj, oImage, tPos = tPos, chPos = chPos, zPos = i
                                 endcase
                              endcase
                           endfor
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]] = imageBitRate
                         endif
                         pParamStruct = -1
         endcase
       'INSERTIMAGESBEFORE':begin
                    state.fInsertImagesBeforeAfter = 0
                    widget_control, ev.top, set_uValue = state, /no_copy
                        widget_control, ev.id, set_uValue = 'INSERTIMAGES'
                           s_ISM_Window_Button_Event, ev
                        widget_control, ev.id, set_uValue = 'INSERTIMAGESBEFORE'
                    widget_control, ev.top, get_uValue = state, /no_copy
         endcase
       'INSERTIMAGESAFTER':begin
                    state.fInsertImagesBeforeAfter = 1
                    widget_control, ev.top, set_uValue = state, /no_copy
                        widget_control, ev.id, set_uValue = 'INSERTIMAGES'
                           s_ISM_Window_Button_Event, ev
                        widget_control, ev.id, set_uValue = 'INSERTIMAGESAFTER'
                    widget_control, ev.top, get_uValue = state, /no_copy
         endcase
       'INSERTIMAGES':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        dummy = (where( *(*pParamStruct).pNames eq 'Image Path'))[0]
                          file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[dummy], get_path = path, filter = '*.tif', /multiple_files)
                          if (file[0] ne '') then begin

                        widget_control, state.wListTime, get_uValue = uValueFileParameter, /no_copy
                           index = uValueFileParameter.active
                        widget_control, state.wListTime, set_uValue = uValueFileParameter, /no_copy

                             nFile = n_elements(file)
                             *(*pParamStruct).pValues[dummy] = path
                        dummy = (where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]
                             *(*pParamStruct).pValues[dummy] = nFile + temporary(*(*pParamStruct).pValues[dummy])

                             if (nFile gt 1) then file[[0,nFile-1]] = file[ [nFile-1, 0] ]
                             for i = 0, nFile-1 do begin
                               image = read_tiff(file[i])
                               szI = size(image, /dimensions)

                           oImage = obj_new('C_sImageObject')
                           oImage->get, pParamStruct = pParamStruct
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = $
                                  strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Slice Number'))[0]] = s_getRightNumberFromString(*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]])
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = szI[0]
                               *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = szI[1]
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = 0
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = szI[0]-1
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = 0
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = szI[1]-1

                           (*state.pImageStackInfoObject)->IDL_Container::add, oImage, position = (i + index + state.fInsertImagesBeforeAfter) < (*state.pImageStackInfoObject)->IDL_Container::count()
                            pParamStruct = -1
                             endfor

                         endif
                    pParamStruct = -1
         endcase
       'SETSEGIMAGEASSTACKIMAGE':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        maskPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]]
                        imagePath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]
                          file = dialog_pickfile( /read, path = imagePath, get_path = path, filter = '*.tif', /multiple_files)
                          if (file[0] ne '') then begin
                              nFile = n_elements(file)

                             if (nFile gt 1) then file[[0,nFile-1]] = file[ [nFile-1, 0] ]
                             for i = 0, nFile-1 do begin
                                 image = read_tiff(file[i])
                                 filename = strMid(file[i],(strPos(file[i], slash, /reverse_s))+1, (strPos(file[i],'.tif', /reverse_s)-strPos(file[i], slash, /reverse_s)-1) )
                                 if (strPos(filename,'SegNum') eq -1) then begin
                                   widget_control, ev.top, set_uValue = state, /no_copy
                                   return
                                 endif
                                 filename = strMid(filename, (strPos(filename,'SegNum'))+1, strLen(filename)- (strPos(filename, 'SegNum')))
                                 if (strPos(filename,'_') eq -1) then begin
                                   widget_control, ev.top, set_uValue = state, /no_copy
                                   return
                                 endif
                                 filename = strMid(filename, (strPos(filename,'_'))+1, strLen(filename)- (strPos(filename, '_')))
                                 write_tiff, path + filename + '.tif', image, 1
                              endfor

                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] = path
                          endif
         endcase
       'OPENSTACK':begin
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]], get_path = path, filter = '*.sav')
                    if (file[0] ne '') then begin
                       *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]] = path
                       *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strMid(file,(strPos(file, slash, /reverse_s))+1, (strPos(file,'.', /reverse_s)-strPos(file, slash, /reverse_s)-1) )
                       if ptr_valid(state.pImageStackInfoObject) then if obj_valid(*state.pImageStackInfoObject) then begin
                          obj_destroy, *state.pImageStackInfoObject
                          ptr_free, state.pImageStackInfoObject
                       endif
                       restore, file, /relaxed
                       state.pImageStackInfoObject = ptr_new(dummy, /no_copy)

                       (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                       state.cutValues = [0, -1 + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                                          0, -1 + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
                                          0, -1 + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]  ]

                       widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) +  $
                                                                              ' ]  || y[ ' + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) +  $
                                                                              ' ]  || z[ ' + strCompress(state.cutValues[4], /rem) + ' - '+ strCompress(state.cutValues[5], /rem) + ' ]'
                    endif
                    widget_control, state.wListTime, get_uValue = uValueFileParameter, /no_copy
                        uValueFileParameter.active = 0
                    widget_control, state.wListTime, set_uValue = uValueFileParameter, /no_copy
                          if widget_info(state.child_ViewWindow_tlb, /valid) then begin
                             (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                             whereBits = (where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]
                             if (whereBits ne -1) then imageBitRate = *(*pParamStruct).pValues[whereBits] else imageBitRate = 8
                             widget_control, state.child_ViewWindow_tlb, /destroy
                             widget_control, ev.top, tlb_get_size = BaseSize
                             image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
                             widget_control, ev.top, set_uValue = state
                                 s_Image_ShowZoomCut_Window, image, imageBitRate = imageBitRate, groupLeader = ev.top, basePosition = BaseSize, application_tlb = application_tlb
                             widget_control, ev.top, get_uValue = state
                             state.child_ViewWindow_tlb = application_tlb
                             widget_control, ev.top, /show
                        endif
         endcase
       'SAVESTACK':begin
                    fUpdade = 0b
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
                    filename = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
                    dummy = *state.pImageStackInfoObject
                    save, dummy, filename = path + filename + '.sav'
                    help, dummy
                    print, 'path + filename + '
                    dummy = -1
         endcase
       'SAVESTACKAS':begin
                    fUpdade = 0b
                    (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                    file = dialog_pickfile( /write, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]], get_path = path, filter = '*.sav')
                    if (file ne '') then begin
                       *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]] = path
                       if ( (strMid(file, strLen(file)-4, 4)) ne '.sav') then file = file + '.sav'
                       *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strMid(file,(strPos(file, slash, /reverse_s))+1, (strPos(file,'.', /reverse_s)-strPos(file, slash, /reverse_s)-1) )
                       dummy = *state.pImageStackInfoObject
                       save, dummy, filename = file
                       print, 'saved at path + filename + '
                       dummy = -1
                    endif
         endcase
         ; MOR - 09Nov2010 - added frap window -  BEGIN
         'MEANINTENSITYONOFF':begin
                     state.fMEAN = s_ToggleButtonOnOffState(state.wMEANButtonOnOff)
                        if state.fMEAN then begin
                           image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
                           dimI = size(image, /dim)
                           ;mask = (*state.pImageStackInfoObject)->applyImageSegmentation(tPos = tPos, chPos = chPos, zPos = zPos) ; 25April2011 - MOR added stack info.
                           widget_control, ev.top, set_uValue = state, /no_copy
                           s_Surface_Method_AnalyseMEAN,image, ColorIndex = colorIndex, Bottom = bottom, path = path, $
                              Group_Leader = ev.top, NColors = ncolors, NoInterpolation = interp, xSize = dimI[0], ySize = dimI[1],$
                              realXSize = realXSize, realYSize = realYSize, application_tlb = application_tlb, stack_tlb = stack_tlb
                           widget_control, ev.top, get_uValue = state
                           state.child_MEAN_tlb = application_tlb
                           widget_control, ev.top, /show
                        endif else begin
                           if widget_info(state.child_MEAN_tlb, /valid_id) then begin
                              widget_control, state.child_MEAN_tlb, /destroy
                              state.child_MEAN_tlb = -1l
                           endif
                     endelse
        endcase
         ; MOR - 09Nov2010 - added frap window -  BEGIN
         'FRAPWINDOWONOFF':begin
                     state.fFRAP = s_ToggleButtonOnOffState(state.wFRAPButtonOnOff)
                        if state.fFRAP then begin
                           image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
                           dimI = size(image, /dim)
                           ;mask = (*state.pImageStackInfoObject)->applyImageSegmentation(tPos = tPos, chPos = chPos, zPos = zPos) ; 25April2011 - MOR added stack info.
                           widget_control, ev.top, set_uValue = state, /no_copy
                           s_Surface_Method_AnalyseFRAP,image, ColorIndex = colorIndex, Bottom = bottom, path = path, $
                              Group_Leader = ev.top, NColors = ncolors, NoInterpolation = interp, xSize = dimI[0], ySize = dimI[1],$
                              realXSize = realXSize, realYSize = realYSize, application_tlb = application_tlb, stack_tlb = stack_tlb
                           widget_control, ev.top, get_uValue = state
                           state.child_FRAP_tlb = application_tlb
                           widget_control, ev.top, /show
                        endif else begin
                           if widget_info(state.child_FRAP_tlb, /valid_id) then begin
                              widget_control, state.child_FRAP_tlb, /destroy
                              state.child_FRAP_tlb = -1l
                           endif
                     endelse
        endcase
        ; MOR - 09Nov2010 - added frap window -  END          
       'XRAYWINDOWONOFF':begin
                    image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
                    if (n_elements(image) gt 9) then begin
                        if (s_ToggleButtonOnOffState(state.wXRayButtonOnOff)) then begin
                           dimI = size(image, /dim)
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           realXSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
                           realYSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
                           s_Surface_Method_AnalyseXRays, image, ColorIndex = colorIndex, path = pathname,$
                                                Bottom = bottom, Group_Leader = ev.top, NColors = ncolors, NoInterpolation = interp,$
                                                xSize = dimI[0], ySize = dimI[1], realXSize = realXSize, realYSize = realYSize,$
                                                application_tlb = application_tlb
                           state.child_XRayAnalysis_Window_tlb = application_tlb
                           widget_control, ev.top, /show
                        endif else begin
                           if widget_info(state.child_XRayAnalysis_Window_tlb, /valid_id) then begin
                             widget_control, state.child_XRayAnalysis_Window_tlb, /destroy
                             state.child_XRayAnalysis_Window_tlb = -1l
                           endif
                        endelse
                     endif
        endcase
       'COLOCALIZATIONWINDOWONOFF':begin
                     state.fColocalization = s_ToggleButtonOnOffState(state.wColocalizationOnOff)
                        if state.fColocalization then begin
                           image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = 0, zPos = zPos)
                           dimI = size(image, /dim)
                           images = make_array(2<totalChNum, dimI[0], dimI[1], type = size(image, /type))
                           for i = 0, (2<totalChNum)-1 do $
                              images[i,*,*] = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = i, zPos = zPos)
                           widget_control, ev.top, set_uValue = state
                           s_Coloc_Window, images, application_tlb = application_tlb, groupLeader = ev.top
                           widget_control, ev.top, set_uValue = state
                           state.child_Colocalization_tlb = application_tlb
                           widget_control, ev.top, /show
                        endif else begin
                           if widget_info(state.child_Colocalization_tlb, /valid_id) then begin
                              widget_control, state.child_Colocalization_tlb, /destroy
                              state.child_Colocalization_tlb = -1l
                           endif
                     endelse
        endcase
       'SETSTACKINFORMATION':begin
                        fUpdade = 0b
                        paramTableuValue = {groupLeader: state.wTopBase,$
                                               paramNames: (*state.pImageStackInfoObject)->getParameterNameList(),$
                                               paramAsStruct: (*state.pImageStackInfoObject)->getParamAsStruct(),$
                                               name: 'Stack',$
                                               ev:ev,$
                                               wTopBase: -1}
                         s_ObjParamTableWidget, paramTableuValue = paramTableuValue
                         state.child_StackParameterTableWidget_tlb = paramTableuValue.wTopBase
         endcase
       'SETIMAGEINFORMATION':begin
                        fUpdade = 0b
                        paramTableuValue = {groupLeader: state.wTopBase,$
                                               paramNames: (*state.pImageStackInfoObject)->getSelectedImageParamNameList(tPos = tPos, chPos = chPos, zPos = zPos),$
                                               paramAsStruct: (*state.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = tPos, chPos = chPos, zPos = zPos),$
                                               name: 'Image', wTopBase: -1}
                        s_ObjParamTableWidget, paramTableuValue = paramTableuValue
                        state.child_ParamTableWidget_tlb = paramTableuValue.wTopBase
         endcase
       'CALCULATEALL':begin
                        fUpdade = 0b
                        widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.id, set_uValue = 'CALCULATETIMES'
                           s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'CALCULATEXYRESOLUTION'
                           s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'CALCULATEZSTACKDISTANCE'
                           s_ISM_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'CALCULATEALL'
                        widget_control, ev.top, get_uValue = state, /no_copy
         endcase
       'CALCULATETIMES':begin
                           (*state.pImageStackInfoObject)->get, pParamStruct = pStackParamStruct
                           totalTime = *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'Total Time [s]'))[0]]
                           timeInterval = *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'Time Interval [s]'))[0]]
                           case 1 of
                             (timeInterval ne -1):begin
                                              for k = 0, totalTNum-1  do $
                                              for j = 0, totalChNum-1  do $
                                                 for i = 0, totalZNum-1  do begin
                                        oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                                        if obj_valid(oImage) then begin
                                            oImage->get, pParamStruct = pParamStruct
                                            *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]] = 1. * k * timeInterval
                                        endif
                                          endfor
                                          *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'Total Time [s]'))[0]] = 1. * k * timeInterval
                              endcase
                             (totalTime ne -1):begin
                                              for k = 0, totalTNum-1  do $
                                              for j = 0, totalChNum-1  do $
                                                 for i = 0, totalZNum-1  do begin
                                        oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                                        if obj_valid(oImage) then begin
                                            oImage->get, pParamStruct = pParamStruct
                                            *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]] = 1. * k * totalTime / totalTNum
                                        endif
                                          endfor
                                          *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'Time Interval [s]'))[0]] = 1. * totalTime / totalTNum
                              endcase
                             ((totalTime eq -1) and (timeInterval eq -1)):begin
                                              for k = 0, totalTNum-1  do $
                                              for j = 0, totalChNum-1  do $
                                                 for i = 0, totalZNum-1  do begin
                                        oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                                        if obj_valid(oImage) then begin
                                           oImage->get, pParamStruct = pParamStruct
                                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]] = 1. * k
                                        endif
                                          endfor
                                          *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'Total Time [s]'))[0]] = 1. * (k-1)
                                          *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'Time Interval [s]'))[0]] = 1.
                              endcase
                           endcase
         endcase
       'CALCULATEXYRESOLUTION':begin
                           (*state.pImageStackInfoObject)->get, pParamStruct = pStackParamStruct
                           xSizePix = *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'x-Size [pixel]'))[0]]
                           ySizePix = *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'y-Size [pixel]'))[0]]
                           xSize = *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'x-Size [real]'))[0]]
                           ySize = *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'y-Size [real]'))[0]]
                           if (xSize eq -1) then xSize = 1.
                           if (ySize eq -1) then ySize = 1.
                           *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'x-Size [real]'))[0]] = xSize
                           *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'y-Size [real]'))[0]] = ySize
                           if (state.cutValues[0] eq state.cutValues[1]) then state.cutValues[0:1] = [0, xSizePix-1]
                           if (state.cutValues[2] eq state.cutValues[3]) then state.cutValues[2:3] = [0, ySizePix-1]

                               for k = 0, totalTNum-1  do $
                                    for j = 0, totalChNum-1  do $
                                    for i = 0, totalZNum-1  do begin
                             oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                             if obj_valid(oImage) then begin
                              oImage->get, pParamStruct = pParamStruct
                              *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0])] = state.cutValues[1] - state.cutValues[0] + 1
                              *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0])] = state.cutValues[3] - state.cutValues[2] + 1
                              *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0])] = 1. * xSize * (state.cutValues[1] - state.cutValues[0] + 1) / xSizePix
                              *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0])] = 1. * ySize * (state.cutValues[3] - state.cutValues[2] + 1) / ySizePix
                             endif
                            endfor
         endcase
       'CALCULATEZSTACKDISTANCE':begin
                           (*state.pImageStackInfoObject)->get, pParamStruct = pStackParamStruct
                           zInterval = *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'z-Interval [real]'))[0]]
                           if (zInterval eq -1) then zInterval = 1

                           for k = 0, totalTNum-1  do for j = 0, totalChNum-1 do for i = 0, totalZNum-1 do begin
                              oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                              if obj_valid(oImage) then begin
                                 oImage->get, pParamStruct = pParamStruct
                                 *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'z-Position [real]'))[0])] = 1. * i * zInterval
                              endif
                           endfor
                           *(*pStackParamStruct).pValues[(where( *(*pStackParamStruct).pNames eq 'z-Interval [real]'))[0]] = 1. * zInterval
         endcase
       'PASSTIMES':begin
                           for k = 0, totalTNum-1  do begin
                              oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = 0, zPos = 0)

                              if obj_valid(oImage) then begin
                                 oImage->get, pParamStruct = pParamStruct
                                 timeToPass = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0])]
                              endif else timeToPass = 0.

                              for j = 0, totalChNum-1 do for i = 0, totalZNum-1  do begin
                                 oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                                 if obj_valid(oImage) then begin
                                     oImage->get, pParamStruct = pParamStruct
                                     *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0])] = timeToPass
                                 endif
                              endfor
                           endfor
         endcase
       'PASSXYSIZE':begin
                           (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           xSizeToPass = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0])]
                           ySizeToPass = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0])]
                           xPixSizeToPass = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0])]
                           yPixSizeToPass = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0])]
                           oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                           if obj_valid(oImage) then begin
                              oImage->get, pParamStruct = pParamStruct
                              xSegmentBox0 = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0])]
                              xSegmentBox1 = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0])]
                              ySegmentBox0 = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0])]
                              ySegmentBox1 = *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0])]
                           endif
                           for k = 0, totalTNum-1  do begin
                              for j = 0, totalChNum-1 do for i = 0, totalZNum-1  do begin
                                 oImage = (*state.pImageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                                 if obj_valid(oImage) then begin
                                    oImage->get, pParamStruct = pParamStruct
                                    *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0])] = xSizeToPass
                                    *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0])] = ySizeToPass
                                    *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0])] = xPixSizeToPass
                                    *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0])] = yPixSizeToPass
                                    *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0])] = xSegmentBox0
                                    *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0])] = xSegmentBox1
                                    *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0])] = ySegmentBox0
                                    *(*pParamStruct).pValues[((where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0])] = ySegmentBox1
                                 endif
                              endfor
                           endfor
         endcase
       'SEGMENTATIONWINDOWONOFF':begin
                        fUpdade = 0b
                        image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
                        if (n_elements(image) gt 9) then begin
                               if (s_ToggleButtonOnOffState(state.wSegmentationOnOff)) then begin
                                widget_control, ev.top, tlb_get_size = baseSize, tlb_get_Offset = baseOffset
                                widget_control, ev.top, set_uValue = state, /no_copy
                                    s_Image_SegmentationManipulator_Window, groupLeader = ev.top, application_tlb = application_tlb, basePosition = (baseSize+baseOffset)
                                widget_control, ev.top, get_uValue = state, /no_copy
                                   state.child_SegmentationWindow_tlb = application_tlb
                                   widget_control, ev.top, /show
                               endif else begin
                                   if widget_info(state.child_SegmentationWindow_tlb, /valid_id) then begin
                                    widget_control, state.child_SegmentationWindow_tlb, /Destroy
                                    state.child_SegmentationWindow_tlb = -1l
                                endif
                               endelse
                           endif
         endcase
       'SETCUTVALUESFROMZOOMBOX':begin
                        fUpdade = 0b
                        if widget_info(state.child_ViewWindow_tlb, /valid_id) then begin
                           image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
                           if (n_elements(image) gt 9) then begin
                             widget_control, state.child_ViewWindow_tlb, get_uValue = stateChild, /no_copy
                             if ( ((stateChild.xd - stateChild.xs) gt 1) and ((stateChild.yd - stateChild.ys) gt 1) ) then begin
                              state.cutValues = [ stateChild.xs, stateChild.xd, stateChild.ys, stateChild.yd]
                             endif else   dummy = s_apop_shout('Select Reasonable Box-Area')
                             widget_control, state.child_ViewWindow_tlb, set_uValue = stateChild, /no_copy
                           endif else  dummy = s_apop_shout('Must contain images')
                        endif else dummy = s_apop_shout('View window must be visible and contain selected Box-Area')
                        widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) + ' ]  ||  y[ ' $
                                  + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) + ' ]'
         endcase
       'SETCUTVALUESFROMTOTALFRAME':begin
                        fUpdade = 0b
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        state.cutValues = [0, -1 + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                                           0, -1 + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]  ]
                        widget_control, state.wCutValueShowButton, set_value = 'x[ ' + strCompress(state.cutValues[0], /rem) + ' - '+ strCompress(state.cutValues[1], /rem) + ' ]  ||  y[ ' $
                                                                                + strCompress(state.cutValues[2], /rem) + ' - '+ strCompress(state.cutValues[3], /rem) + ' ]'
         endcase
       'VIEWWINDOWONOFF':begin
                        fUpdade = 0b
                        (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        whereBits = (where( *(*pParamStruct).pNames eq 'Image Bit-Rate'))[0]
                        if (whereBits ne -1) then imageBitRate = *(*pParamStruct).pValues[whereBits] else imageBitRate = 8
                        image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
                        if (n_elements(image) gt 9) then begin
                           dummy = s_ToggleButtonOnOffState(state.wViewWindowButtonOnOff)
                           if dummy then begin
                              widget_control, ev.top, set_uValue = state, /no_copy
                                 s_Image_ShowZoomCut_Window, image, imageBitRate = imageBitRate, groupLeader = ev.top, basePosition = baseSize, application_tlb = application_tlb
                              widget_control, ev.top, get_uValue = state, /no_copy
                              state.child_ViewWindow_tlb = application_tlb
                              widget_control, ev.top, /show
                           endif else begin
                              if widget_info(state.child_ViewWindow_tlb, /valid_id) then begin
                                 widget_control, state.child_ViewWindow_tlb, /destroy
                                 state.child_ViewWindow_tlb = -1l
                              endif
                           endelse
                        endif
         endcase
         'METAHISTCOLLAGE':begin
            s_CreateRecursiveHistogtamAndCollage
         endcase
         else:
    endcase

  widget_control, ev.top, set_uValue = state, /no_copy
  if fUpdade then s_ISM_UpdateWidgets, ev.top
end

pro s_ISM_List_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue, /no_copy
    uValue.active = ev.index
    update = 0
    case uValue.name of
       'TimeSelected':begin
                    oldStackImageActive = uValue.active
                    state.fListSelect = 0b
                    if ((*uValue.value)[ev.index] ne '-NO FILES-') then update = ev.Clicks
        endcase
       'ChannelSelected':begin
                    state.fListSelect = 1b
                    update = ev.Clicks
        endcase
       'ZSliceSelected':begin
                    state.fListSelect = 2b
                    update = ev.Clicks
        endcase
    endcase
       ; Update Widgets-List
    widget_control, ev.id, set_uValue = uValue, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy
      s_ISM_getProjectInfo, stack_tlb = ev.top, tPos = tPos, chPos = chPos, zPos = zPos, totalChNum = totalChNum, totalTNum = totalTNum ; MOR - 21Oct2010- for passing to xray window
      widget_control, ev.top, get_uValue = state, /no_copy
       if widget_info(state.child_FRAP_tlb, /valid) then begin       
           clusPos=ev.clus1
           skelclus=ev.clus2
        end
       widget_control, ev.top, set_uValue = state, /no_copy
     widget_control, ev.top, get_uValue = state, /no_copy

    case update of
    2:begin
       paramTableuValue = {groupLeader: state.wTopBase,$
                               paramNames: (*state.pImageStackInfoObject)->getSelectedImageParamNameList(tPos = tPos, chPos = chPos, zPos = zPos),$
                               paramAsStruct: (*state.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = tPos, chPos = chPos, zPos = zPos),$
                               name: 'Image',$
                               wTopBase: -1}
        s_ObjParamTableWidget, paramTableuValue = paramTableuValue
        state.child_ParamTableWidget_tlb = paramTableuValue.wTopBase
    endcase
    1:begin
         ; Update Segment Box Image Parameters or Show Selected Segmentation Box
         if widget_info(state.child_ViewWindow_tlb, /valid) then begin
            widget_control, state.child_ViewWindow_tlb, get_uValue = viewState, /no_copy
            if viewState.fSelectSegBox then begin
                pParamStruct = (*state.pImageStackInfoObject)->getpSelectedImageParams(tPos = tPos, chPos = chPos, zPos = zPos)
                *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]] = viewState.xs
                *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]] = viewState.xd
                *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]] = viewState.ys
                *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]] = viewState.yd
                pParamStruct = -1
            endif
            if viewState.fShowSegBox then begin
                pParamStruct = (*state.pImageStackInfoObject)->getpSelectedImageParams(tPos = tPos, chPos = chPos, zPos = zPos)
                viewState.xs = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]]
                viewState.xd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]]
                viewState.ys = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]]
                viewState.yd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]]
                pParamStruct = -1
            endif
            widget_control, state.child_ViewWindow_tlb, set_uValue = viewState, /no_copy
         endif

            ; Update Segmentation Window
          if widget_info(state.child_SegmentationWindow_tlb, /valid) then begin
             dummy = state.child_SegmentationWindow_tlb
             widget_control, ev.top, set_uValue = state, /no_copy
               s_ISegM_UpdateWidgets, dummy, oldStackImageActive = oldStackImageActive
             widget_control, ev.top, get_uValue = state, /no_copy
          endif
            ; Update View Window
          if widget_info(state.child_ViewWindow_tlb, /valid) then begin
             widget_control, state.child_ViewWindow_tlb, get_uValue = child_state, /no_copy
                *(child_state.image) = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
             widget_control, state.child_ViewWindow_tlb, set_uValue = child_state, /no_copy
             dummy = state.child_ViewWindow_tlb
             widget_control, ev.top, set_uValue = state, /no_copy
                zimage_colors, {redraw_image, top: dummy}
             widget_control, ev.top, get_uValue = state, /no_copy
          endif
            ; Update Stack Parameter Table Widget
          if widget_info(state.child_StackParameterTableWidget_tlb, /valid) then begin
             paramTableUValue = {groupLeader: state.wTopBase,$
                                    paramNames:-1,$
                                    paramAsStruct: (*state.pImageStackInfoObject)->getParamAsStruct(),$
                                    name: 'Stack',$
                                    wTopBase: state.child_StackParameterTableWidget_tlb}
             s_OPTW_Update, paramTableUValue = paramTableUValue
          endif
            ; Update Parameter Table Widget
          if widget_info(state.child_ParamTableWidget_tlb, /valid) then begin
             selectedImageParamAsStruct = (*state.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = tPos, chPos = chPos, zPos = zPos)
             if (n_tags(selectedImageParamAsStruct) gt 0) then begin
                paramTableUValue = {groupLeader: state.wTopBase,$
                            paramNames:-1,$
                            paramAsStruct: selectedImageParamAsStruct,$
                            name: 'Image',$
                            wTopBase: state.child_ParamTableWidget_tlb}
                s_OPTW_Update, paramTableUValue = paramTableUValue
             endif
          endif
            ; Update X-Ray Window
          if widget_info(state.child_XRayAnalysis_Window_tlb, /valid) then begin
             widget_control, state.child_XRayAnalysis_Window_tlb, get_uValue = child_state, /no_copy
             child_state.image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
             modeControl = child_state.modeControl
             child_window_id = child_state.wDrawWindow
             widget_control, state.child_XRayAnalysis_Window_tlb, set_uValue = child_state, /no_copy
             s_Surface_Method_AnalyseXRays_colors, {redraw_image, top : state.child_XRayAnalysis_Window_tlb}
             if modeControl then s_Surface_Method_AnalyseXRays_DRAW_EVENTS, {WIDGET_DRAW,$
                                                                             ID:child_window_id,$
                                                                             TOP:state.child_XRayAnalysis_Window_tlb,$
                                                                             HANDLER:0,$
                                                                             TYPE:1,$
                                                                             X:0, Y:0,$
                                                                             PRESS:0,$
                                                                             RELEASE:1,$
                                                                             CLICKS:1,$
                                                                             MODIFIERS:0,$
                                                                             CH:0,$
                                                                             KEY:0L}
          endif
              ; Update FRAP Window -- MOR 10Nov2010 - BEGIN
          if widget_info(state.child_FRAP_tlb, /valid) then begin
          
            widget_control, state.child_FRAP_tlb, get_uValue = child_state, /no_copy
            image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = 0, zPos = zPos)
            newMask = (*state.pImageStackInfoObject)->getSelectedClusterMask(tPos = tPos, chPos = 0, zPos = zPos, clusPos = clusPos) 
            skelMask = (*state.pImageStackInfoObject)->getSelectedClusterMask(tPos = tPos, chPos = 0, zPos = zPos, clusPos = skelclus)
            child_FRAP_tlb = state.child_FRAP_tlb
            child_window_id = child_state.wDrawWindow
            widget_control, state.child_FRAP_tlb, set_uValue = child_state, /no_copy
            s_Surface_Method_AnalyseFRAP_update, child_FRAP_tlb, newImages = image, tPos = tPos, child_window_id, newMask = newMask, skelMask = skelMask, clusPos = clusPos, skelclus = skelclus ; 30oct2012 - add cluster position
          endif
          ; Update FRAP Window -- MOR 10Nov2010 - END
          if widget_info(state.child_MEAN_tlb, /valid) then begin
          
            widget_control, state.child_MEAN_tlb, get_uValue = child_state, /no_copy
            image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = 0, zPos = zPos)
            newMask = (*state.pImageStackInfoObject)->getSelectedClusterMask(tPos = tPos, chPos = 0, zPos = zPos, clusPos = clusPos) 
            child_MEAN_tlb = state.child_MEAN_tlb
            child_window_id = child_state.wDrawWindow
            widget_control, state.child_MEAN_tlb, set_uValue = child_state, /no_copy
            s_Surface_Method_AnalyseMEAN_update, child_MEAN_tlb, newImages = image, tPos = tPos, child_window_id, newMask = newMask, clusPos = clusPos ; 25April2011 - add cluster position
          endif
            ; update Colocalization Window
          if widget_info(state.child_Colocalization_tlb, /valid) then begin
             image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = 0, zPos = zPos)
             dimI = size(image, /dim)
             dimICol = s_Coloc_getDimI(state.child_Colocalization_tlb)
             newImages = make_array(2<totalChNum, dimICol[1], dimICol[2], type = size(image, /type))
             for i = 0, (2<totalChNum)-1 do begin
                image = (*state.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = i, zPos = zPos)
                dimI = size(image, /dim)
                if (dimI[0] eq dimICol[1]) and (dimI[1] eq dimICol[2]) then newImages[i,*,*] = image else newImages[i,*,*] = congrid(image, dimICol[1], dimICol[2])
             endfor
             child_Colocalization_tlb = state.child_Colocalization_tlb
             widget_control, ev.top, set_uValue = state, /no_copy
                s_Coloc_update, child_Colocalization_tlb, newImages = newImages
             widget_control, ev.top, get_uValue = state, /no_copy
          endif
    endcase
    else:
    endcase
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_ISM_UpdateWidgets, wTopBase
    s_ISM_getProjectInfo, stack_tlb = wTopBase, tPos = tPos, chPos = chPos,$
                                              zPos = zPos, totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum

    widget_control, wTopBase, get_uValue = state, /no_copy

       (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       widget_control, state.wTopBase, tlb_set_title = 's_Stack |-> ' + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]] + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
       pParamStruct = -1

       (*state.pImageStackInfoObject)->getTimeChannelZSliceNameLists, timeList = timeList, channelList = channelList,$
                                            zSliceList = zSliceList, tPos = tPos, chPos = chPos,$
                                            zPos = zPos, moveActive = state.fListSelect

       widget_control, state.wListTime, get_uValue = uValueFileParameter, /no_copy
         if ptr_valid(uValueFileParameter.value) then ptr_free, uValueFileParameter.value
         uValueFileParameter.value = ptr_new(timeList, /no_copy)
         uValueFileParameter.active = tPos
       widget_control, state.wListTime, set_list_select = uValueFileParameter.active, set_value = *uValueFileParameter.value, set_uValue = uValueFileParameter, /no_copy

       widget_control, state.wListChannel, get_uValue = uValueFileParameter, /no_copy
         if ptr_valid(uValueFileParameter.value) then ptr_free, uValueFileParameter.value
         uValueFileParameter.value = ptr_new(channelList, /no_copy)
         uValueFileParameter.active = chPos
       widget_control, state.wListChannel, set_list_select = uValueFileParameter.active, set_value = *uValueFileParameter.value, set_uValue = uValueFileParameter, /no_copy

       widget_control, state.wListZSlice, get_uValue = uValueFileParameter, /no_copy
         if ptr_valid(uValueFileParameter.value) then ptr_free, uValueFileParameter.value
         uValueFileParameter.value = ptr_new(ZSliceList, /no_copy)
         uValueFileParameter.active = zPos
       widget_control, state.wListZSlice, set_list_select = uValueFileParameter.active, set_value = *uValueFileParameter.value, set_uValue = uValueFileParameter, /no_copy

       ; Update StackParameter Table Widget
    if widget_info(state.child_StackParameterTableWidget_tlb, /valid) then begin
       paramTableUValue = {groupLeader: state.wTopBase,$
                    paramNames:-1,$
                    paramAsStruct: (*state.pImageStackInfoObject)->getParamAsStruct(),$
                    name:'Stack',$
                    wTopBase: state.child_StackParameterTableWidget_tlb}
       s_OPTW_Update, paramTableUValue = paramTableUValue
    endif
       ; Update Parameter Table Widget
    if widget_info(state.child_ParamTableWidget_tlb, /valid) then begin
       selectedImageParamAsStruct = (*state.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = tPos, chPos = chPos, zPos = zPos)
       if (n_tags(selectedImageParamAsStruct) gt 0) then begin
         paramTableUValue = {groupLeader: state.wTopBase,$
                    paramNames:-1,$
                    paramAsStruct: selectedImageParamAsStruct,$
                    name:'Image',$
                    wTopBase: state.child_ParamTableWidget_tlb}
         s_OPTW_Update, paramTableUValue = paramTableUValue
       endif
    endif
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ISM_Resize_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy

    ; Don't resize listbase on UNIX
    if (!VERSION.OS_FAMILY eq 'Windows') then begin
       widget_control, state.wListBase, scr_xsize = ev.x-10, scr_ysize = ev.y-40
    endif

    evx = ev.x-13
    evy = floor((ev.y-72)/3.)
    widget_control, state.wListTime, scr_xsize = evx, scr_ysize = evy
    widget_control, state.wListChannel, scr_xsize = evx, scr_ysize = evy
    widget_control, state.wListZSlice, scr_xsize = evx, scr_ysize = evy
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_ISM_cleanUp, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
    if (n_elements(state) le 0) then return
    if widget_info(state.groupLeader, /valid_id) then begin
       widget_control, state.groupLeader, get_uValue = stateParent, /no_copy
       if (n_elements(stateParent) ne 0) then begin
         if (s_ToggleButtonOnOffState(stateParent.wImageStackManipulatorButton)) then void = s_ToggleButtonOnOffState(stateParent.wImageStackManipulatorButton)
         widget_control, state.groupLeader, set_uValue = stateParent, /no_copy
       endif
    endif else begin
       if ptr_valid(state.pImageStackInfoObject) then begin
         if obj_valid(*state.pImageStackInfoObject) then obj_destroy, *state.pImageStackInfoObject
       endif else state.pImageStackInfoObject = -1
       for i = 0,n_tags(state)-1 do begin
           case size(state.(i), /tname) of
               'POINTER':ptr_free, state.(i)
               'OBJREF':obj_destroy, state.(i)
           else:
           endcase
       endfor
    endelse
    if widget_info(state.groupLeader, /valid_id) then widget_control, state.groupLeader, /map
    file_delete, s_getPathForSystem()+'obj.tmp', s_getPathForSystem()+'stack_tlb.tmp', /allow
end


pro CleanList, wListBase
    widget_control, wListBase, get_uValue = uValue, /no_copy
    if (n_elements(uValue) gt 0) then if ptr_valid(uValue.value) then ptr_free, uValue.value
end


pro s_Image_StackManipulator_Window, groupLeader = groupLeader, basePosition = basePosition,$
                                     application_tlb = application_tlb, pImageStackInfoObject = pImageStackInfoObject

    ;Check the validity of the group identifier.
    ngroup = n_elements(groupLeader)
    if (ngroup ne 0) then begin
       if ((widget_info(groupLeader, /valid_id)) ne 1) then begin
            print,'Error: the group identifier is not valid.  |--> Returning to the main application.'
            return
        endif
    endif else groupLeader = 0l

    if (n_elements(basePosition) eq 0) then basePosition = [0,0]
    if (n_elements(pImageStackInfoObject) eq 0) then pImageStackInfoObject = ptr_new(obj_new('C_sImageStackObject'), /no_copy)

    wTopBase = widget_base(title = 's_Stack |->',$
                           xpad = 2,$
                           ypad = 2,$
                           xOffset = basePosition[0],$
                           MBar = menuBase,$
                           Group_Leader = groupLeader,$
                           tlb_size_events = 1,$
                           /column)

    application_tlb = wTopBase ; Return parameter.

        ; Create FILE menu buttons for printing and exiting.
    wMenuButton = widget_button(menuBase, value = 'Options', event_pro = 's_ISM_Window_Button_Event')
       void = widget_button(wMenuButton, value = 'Open Stack', ACCELERATOR = "Ctrl+O", uValue = 'OPENSTACK')
       void = widget_button(wMenuButton, value = 'Save Stack', ACCELERATOR = "Ctrl+S", uValue = 'SAVESTACK')
       void = widget_button(wMenuButton, value = 'Save Stack as ...', uValue = 'SAVESTACKAS')
       wSubMenuButton = widget_button(wMenuButton, value = 'Import Images ...', /menu, /sep)
         void = widget_button(wSubMenuButton, value = '... as Huygens/Olympus Series from folder', uValue = 'IMPORTIMAGESASHUYGENSOLYMPUSSELALLDIR')
         void = widget_button(wSubMenuButton, value = '... as Huygens/Olympus Series (select all)', uValue = 'IMPORTIMAGESASHUYGENSOLYMPUSSELALL')
         void = widget_button(wSubMenuButton, value = '... as Huygens/Olympus Series (select first)', uValue = 'IMPORTIMAGESASHUYGENSOLYMPUSSELFIRST')
         void = widget_button(wSubMenuButton, value = '... add Huygens Series to Time Series', uValue = 'ADDIMAGESASASHUYGENSTOTIMESERIES')
         void = widget_button(wSubMenuButton, value = '... add Huygens Series to Selected Time', uValue = 'ADDIMAGESASASHUYGENSTOSELECTEDTIME')
         void = widget_button(wSubMenuButton, value = '... LEICA-MultiTiff to Huygens Series', uValue = 'LEICAMULTITIFFTOHUYGENS')
         void = widget_button(wSubMenuButton, value = '... RGB.bmp Series to ch0/ch1/ch2 Time Series', uValue = 'RGBBMPTOCHTIMESERIES')
       wSubMenuButton = widget_button(wMenuButton, value = 'Export Images ...', /menu)
         void = widget_button(wSubMenuButton, value = '... as Huygens Series', uValue = 'EXPORTIMAGESASHUYGENS')
       wSubMenuButton = widget_button(wMenuButton, value = 'Convert Images ...', /menu)
         void = widget_button(wSubMenuButton, value = '... RGB to Grey-Channel', uValue = 'CONVERTIMAGESTOGREYCHANNEL')
         void = widget_button(wSubMenuButton, value = '... RGB to RGB-Channel', uValue = 'CONVERTIMAGESTORGBCHANNEL')
         void = widget_button(wSubMenuButton, value = '... RGB to HSV-Channels', uValue = 'CONVERTIMAGESTOHSVCHANNELS')
         void = widget_button(wSubMenuButton, value = '... RGB to HSV-Channels (8bit-scale)', uValue = 'CONVERTIMAGESTOHSVCHANNELS8BITSCALE')
         void = widget_button(wSubMenuButton, value = '... add z to fill by Yoya', uValue = 'ADDZTOFILLBYYOYA')
       wSubMenuButton = widget_button(wMenuButton, value = 'Load Images ...', /menu, /sep)
         void = widget_button(wSubMenuButton, value = '... as Time Series', uValue = 'LOADIMAGESASTIMESERIES')
         void = widget_button(wSubMenuButton, value = '... as Channel Series', uValue = 'LOADIMAGESASCHANNELSERIES')
         void = widget_button(wSubMenuButton, value = '... as z-Slice Series', uValue = 'LOADIMAGESASZSLICESERIES')
         void = widget_button(wSubMenuButton, value = '... fill Channel', uValue = 'FILLCHANNEL')
         void = widget_button(wSubMenuButton, value = '... as Time Series _FROM FOLDER', uValue = 'LOADIMAGESASTIMESERIES_FROMFOLDER')
         void = widget_button(wSubMenuButton, value = '... as Channel Series _FROM FOLDER', uValue = 'LOADIMAGESASCHANNELSERIES_FROMFOLDER')
         void = widget_button(wSubMenuButton, value = '... as z-Slice Series _FROM FOLDER', uValue = 'LOADIMAGESASZSLICESERIES_FROMFOLDER')
       wSubMenuButton = widget_button(wMenuButton, value = 'Insert Images', /menu)
         void = widget_button(wSubMenuButton, value = 'Insert Images before', uValue = 'INSERTIMAGESBEFORE')
         void = widget_button(wSubMenuButton, value = 'Insert Images after', uValue = 'INSERTIMAGESAFTER')
       wSubMenuButton = widget_button(wMenuButton, value = 'Set Image as', /menu)
         void = widget_button(wSubMenuButton, value = 'Set Seg-Image as Stack-Image', uValue = 'SETSEGIMAGEASSTACKIMAGE')
       wSubMenuButton = widget_button(wMenuButton, value = 'Delete Channel ...', /menu, /sep)
         void = widget_button(wSubMenuButton, value = '... in all Time Sets', uValue = 'DELETECHANNELINALLTIMESETS')
       wSubMenuButton = widget_button(wMenuButton, value = 'Delete z-Stack  ...', /menu)
         void = widget_button(wSubMenuButton, value = '... in all Time Sets', uValue = 'DELETEZSTACKINALLTIMESETS')
       void = widget_button(wMenuButton, value = 'Set Stack Information', uValue = 'SETSTACKINFORMATION', /sep)
       void = widget_button(wMenuButton, value = 'Set  Image Information', uValue = 'SETIMAGEINFORMATION')
       wSubMenuButton = widget_button(wMenuButton, value = 'Calculate Image Parameters ...', /menu)
         void = widget_button(wSubMenuButton, value = '... Times', uValue = 'CALCULATETIMES')
         void = widget_button(wSubMenuButton, value = '... XY Resolution', uValue = 'CALCULATEXYRESOLUTION')
         void = widget_button(wSubMenuButton, value = '... z-Stack Distance', uValue = 'CALCULATEZSTACKDISTANCE')
         void = widget_button(wSubMenuButton, value = '... All', uValue = 'CALCULATEALL', /sep)
       wSubMenuButton = widget_button(wMenuButton, value = 'Pass Image Parameters ...', /menu)
         void = widget_button(wSubMenuButton, value = '... Times', uValue = 'PASSTIMES')
         void = widget_button(wSubMenuButton, value = '... xy-size', uValue = 'PASSXYSIZE')
       wDefineCutSectionButton = widget_button(wMenuButton, value = 'Define Image Cut', /menu, /sep)
         wCutValueShowButton = widget_button(wDefineCutSectionButton, value = 'x[ 0 - 0 ]   ||  y[ 0 - 0 ]  ||  z[ 0 - 0 ]', sensitive = 0)
         void = widget_button(wDefineCutSectionButton, value = 'Set Cut Values from Zoom Box', uValue = 'SETCUTVALUESFROMZOOMBOX', /sep)
         void = widget_button(wDefineCutSectionButton, value = 'Set Cut Values from Total Frame', uValue = 'SETCUTVALUESFROMTOTALFRAME')

    wMenuButton = widget_button(menuBase, value = 'Analyse', event_pro = 's_ISM_Window_Button_Event')
       wSegmentationOnOff = widget_button(wMenuButton, value = 'Segmentation Window (off)', uValue = 'SEGMENTATIONWINDOWONOFF')
       wColocalizationOnOff = widget_button(wMenuButton, value = 'Colocalization Window (off)', uValue = 'COLOCALIZATIONWINDOWONOFF', /sep)
       wXRayButtonOnOff = widget_button(wMenuButton, value = 'X-Ray Window (off)', uValue = 'XRAYWINDOWONOFF', /sep)
       ; MOR - 09Nov2010 - add window for doing frap analysis - BEGIN
       wFRAPButtonOnOff = widget_button(wMenuButton, value = 'FRAP Circle Window (off)', uValue = 'FRAPWINDOWONOFF', /sep)
       ; MOR - 09Nov2010 - add window for doing frap analysis - END
       wMEANButtonOnOff = widget_button(wMenuButton, value = 'Mean Intensity Window (off)', uValue = 'MEANINTENSITYONOFF')

    wPluginsButton = widget_button(menuBase, value = 'Plugins', event_pro = 's_ISM_Window_Button_Event')
       RefMenuTemp = widget_button(wPluginsButton, value = '||-> MultiImages Anlaysis ...', /menu , /sep)
           void = widget_button(RefMenuTemp, value = 'MetaHist And Collage', uVal = 'METAHISTCOLLAGE', /sep)
       
   wListBase = widget_base(wTopBase, /column, /align_center)
       void = widget_button(wListBase, value = '|-> Save Stack <-| ', uValue = 'SAVESTACK', event_pro = 's_ISM_Window_Button_Event')
       wListTime = widget_list(wListBase, xSize = 30, ySize = 3, value = ['-NO FILES-' ],$
                        uValue = {name:'TimeSelected', value : ptr_new(['-NO FILES-'], /no_copy), active:0},$
                        event_pro = 's_ISM_List_Event', kill_notify = 'CleanList')
       wListChannel = widget_list(wListBase, xSize = 30, ySize = 3, value = ['Channel_000'],$
                        uValue = {name:'ChannelSelected', value : ptr_new(['Channel_000'], /no_copy), active:0},$
                        event_pro = 's_ISM_List_Event', kill_notify = 'CleanList')
       wListZSlice = widget_list(wListBase, xSize = 30, ySize = 3, value = ['ZSlice_000'],$
                        uValue = {name:'ZSliceSelected', value : ptr_new(['ZSlice_000'], /no_copy), active:0},$
                        event_pro = 's_ISM_List_Event', kill_notify = 'CleanList')

    wButtonBase = widget_base(wTopBase, event_pro = 's_ISM_Window_Button_Event', /row)
        wDeleteButton = widget_button(wButtonBase, value = 'Delete', uValue = 'DELETE')
        wUpButton = widget_button(wButtonBase, value = 'Up', uValue = 'MOVEUP')
        wDownButton = widget_button(wButtonBase, value = 'Down', uValue = 'MOVEDOWN')
        wViewWindowButtonOnOff = widget_button(wButtonBase, value = 'View (off)', uValue = 'VIEWWINDOWONOFF', ACCELERATOR = "Alt+V")

       ; Realize the widgets.
    widget_control, wTopBase, /realize;

       ; Create an state structure to hold needed program information.
    state = {groupLeader:groupLeader,$        ; The Group Leader Base widget ID.
             wTopBase:wTopBase,$                 ; This Base widget ID.
             wStackBase:wTopBase,$
             wListBase:wListBase,$
             wListZSlice:wListZSlice,$
             wListChannel:wListChannel,$
             wListTime:wListTime,$
               fListSelect:0b,$
             wCutValueShowButton:wCutValueShowButton,$
             cutValues:[0,0,0,0,0,0],$
             wViewWindowButtonOnOff:wViewWindowButtonOnOff,$
               child_ViewWindow_tlb: -1l,$
             wSegmentationOnOff:wSegmentationOnOff,$
             wColocalizationOnOff:wColocalizationOnOff,$
               fColocalization:0b,$
               child_Colocalization_tlb: -1l,$
             wXRayButtonOnOff:wXRayButtonOnOff,$
             wFRAPButtonOnOff:wFRAPButtonOnOff,$ ; MOR - 09Nov2010 added frap window - BEGIN
               fFRAP:0b,$
               child_FRAP_tlb: -1l,$             ; MOR - 09Nov2010 added frap window - END
             wMEANButtonOnOff:wMEANButtonOnOff,$ ; MOR - 09Nov2010 added frap window - BEGIN
               fMEAN:0b,$
               child_MEAN_tlb: -1l,$ 
             fInsertImagesBeforeAfter:0b,$
             child_SegmentationWindow_tlb:-1l,$
             child_XRayAnalysis_Window_tlb:-1l,$
             child_ParamTableWidget_tlb:-1l,$
             child_StackParameterTableWidget_tlb:-1l,$
             pImageStackInfoObject:pImageStackInfoObject}

       ; Store the state structure in the uValue of the wTopBase. Update Correct_And_Segment widgets, & Call XManager.
    widget_control, wTopBase, set_uValue = state, /no_copy
    stack_tlb = wTopBase
    save, stack_tlb, filename = s_getPathForSystem() + 'stack_tlb.tmp'
    loadct, 3;s_modifyColourTable
    s_ISM_UpdateWidgets, wTopBase
    XManager, 's_ISM_Resize', wTopBase, CleanUp = 's_ISM_cleanUp', /no_block, Group_Leader = groupLeader

    falseEvent = create_struct('top',wTopBase,'x',200,'y',700)
    s_ISM_Resize_Event, falseEvent
end

