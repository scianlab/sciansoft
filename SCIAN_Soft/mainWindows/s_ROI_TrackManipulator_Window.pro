;_____________________________IOISIOI____________________
; NAME:
;      s_ROI_TrackManipulator_Window and Events
;  
; PURPOSE:
;       - Manager of Morpho-Topological Parameters of 2D/3D ROI-Objects
;
; AUTHOR:
;     Dr. Steffen Härtel (2009)
;     e_mail: shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________
function s_ROITM_getROIParamNameList
    return, ['C_sROIParam_TrackParameter',$
             'C_sROIParam_TrackParameter II']
end


function s_ROITM_getROIGraphicNameList
    return, ['Track Path',$
             'Track VCL',$
             'Track Motil',$
             'Track Motil SCA',$
             'Track Motil Total',$
             'Track Motil Raton',$;             'Track Motil_2010',$; Susana para Ale Panamá TypeA + TypeB  rotulados PR
             'Track RoundCells',$
             'Track VSL',$
             'Track VAP',$
             'Track ALH',$
             'Track LIN',$
             'Track WOB',$
             'Track STR',$
             'Track Aceleration',$
             'Track Model II',$
             'Track Status',$;] ; MOR - 22 Mar 2011
             'Track Interactive',$
             'Track Combo. Velocities',$
             'Track Motil OMS',$
             'Track Motil Total OMS',$
             'Track Motil OMS Ajustada'] ; MOR - 22 Mar 2011
end

function s_ROITM_getROIObjModelParamList, track_tlb = track_tlb
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos
    if obj_valid(poCurrROI3DGroup) then objectModelList = (poCurrROI3DGroup)->getObjectModelParamList(ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos)
    return, objectModelList
end

function s_ROITM_getROIObjModelList, track_tlb = track_tlb
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then objectModelList = (*poCurrROI3DGroup)->getObjectModelList(ROIObjPos = ROIObjPos)
    return, objectModelList
end

function s_ROITM_getROIObjectNumberList, track_tlb = track_tlb
    widget_control, track_tlb, get_uValue = state, /no_copy
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    if obj_valid(*poCurrROI3DGroup) then return, (*poCurrROI3DGroup)->getObjectNumberVector() else return, -1
end

function s_ROITM_getoROIObjectModelContainer, track_tlb = track_tlb
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then return, (*poCurrROI3DGroup)->getoROIObjectModelContainer(ROIObjPos = ROIObjPos) else return, -1
end

function s_ROITM_getoROIObjectModel, track_tlb = track_tlb
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos
    if obj_valid(poCurrROI3DGroup) then return, (poCurrROI3DGroup)->getoROIObjectModel(ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos) else return, -1
end

pro s_ROITM_addObjectToGroup, track_tlb = track_tlb, ROIObjPos = ROIObjPos
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, track_tlb, set_uValue = state, /no_copy
    if obj_valid(poCurrROI3DGroup) then (poCurrROI3DGroup)->addObjectToGroup, ROIObjPos = ROIObjPos
end

pro s_ROITM_deleteObjectInGroup, track_tlb = track_tlb, ROIObjPos = ROIObjPos
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    if (n_elements(ROIObjPos) eq 0) then s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(poCurrROI3DGroup) then (poCurrROI3DGroup)->deleteObjectInGroup, ROIObjPos = ROIObjPos
end

pro s_ROITM_addModelToObject, track_tlb = track_tlb, selROIObjModel = selROIObjModel, position = position
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then (*poCurrROI3DGroup)->addModelToObject, ROIObjPos = ROIObjPos, selROIObjModel = selROIObjModel, position = position
end

pro s_ROITM_deleteModelInObject, track_tlb = track_tlb, ROIObjModelPos = ROIObjModelPos
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    if (n_elements(ROIObjModelPos) eq 0) then s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos else $
       s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then (*poCurrROI3DGroup)->deleteModelInObject, ROIObjModelPos = ROIObjModelPos, ROIObjPos = ROIObjPos
end

pro s_ROITM_setObjParamsFromModels, track_tlb = track_tlb, all = all
    widget_control, track_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, track_tlb, set_uValue = state, /no_copy
    if (n_elements(all) eq 0) then s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then (*poCurrROI3DGroup)->setObjParamsFromModels, ROIObjPos = ROIObjPos
end


function s_ROITM_getSelectedXYZMixedROIParamName, wTopBase = wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       widget_control, state.wXAxisMixedButton, get_value = dataName
         selXYZMixedROIParamName = strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )
       widget_control, state.wYAxisMixedButton, get_value = dataName
         selXYZMixedROIParamName = [selXYZMixedROIParamName, strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )]
       widget_control, state.wZAxisMixedButton, get_value = dataName
         selXYZMixedROIParamName = [selXYZMixedROIParamName, strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )]
    widget_control, wTopBase, set_uValue = state, /no_copy
    return, selXYZMixedROIParamName
end


pro s_ROITM_UpdateXYMixedParamPlotWin, wTopBase = wTopBase

    selXYZMixedROIParamName = s_ROITM_getSelectedXYZMixedROIParamName(wTopBase = wTopBase)
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       fXYMixedDataPlotControl = state.fXYMixedDataPlotControl
       child_MixedDataPlot_tlb = state.child_MixedDataPlot_tlb
       state.selXYZMixedROIParamName = selXYZMixedROIParamName
       fRefreshMixedData = state.fRefreshMixedData
       state.fRefreshMixedData = 0b
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos

    if fXYMixedDataPlotControl then begin
       if widget_info(child_MixedDataPlot_tlb, /valid) then begin
         if fRefreshMixedData then s_GroupParamPlot_Window_Update, group_tlb = child_MixedDataPlot_tlb, selectedXYParamNames = selXYZMixedROIParamName, tPos = tPos,$
                                                                                    chPos = chPos, zPos = zPos, clusPos = clusPos
       endif else begin
           widget_control, wTopBase, tlb_Get_Offset = Offsets, tlb_Get_Size = tblSize
         s_GroupParamPlot_Window, stack_tlb = stack_tlb,$
                                            ROIOM_tlb = wTopBase,$
                                            tPos = tPos,$
                                            chPos = chPos,$
                                            zPos = zPos,$
                                            clusPos = clusPos,$
                                            selectedXYParamNames = selXYZMixedROIParamName,$
                                            basePosition = [Offsets[0] + 15, tblSize[1]],$
                                            application_tlb = child_MixedDataPlot_tlb

         widget_control, wTopBase, get_uValue = state, /no_copy
          if widget_info(child_MixedDataPlot_tlb, /valid) then state.child_MixedDataPlot_tlb = child_MixedDataPlot_tlb
         widget_control, wTopBase, set_uValue = state, /no_copy
       endelse
    endif
end


pro s_ROITM_UpdateXYZPlots, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, totalTNum = totalTNum, selROIGroupObj = selROIGroupObj
    widget_control, wTopBase, get_uValue = state, /no_copy
       oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
       if widget_info(state.child_MixedDataPlot_tlb, /valid) then state.fRefreshMixedData = 1b
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROI_TrackManipulator_Window_Button_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
                                              clusPos = clusPos, segPos = segPos, selROIGroupObj = selROIGroupObj, ROIObjParamPos = ROIObjParamPos,$
                                              selROIObjSubParam = selROIObjSubParam, ROIObjSubParamPos = ROIObjSubParamPos
    widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue, /no_copy
    fUpdate = 1b
    case uValue of
       'SAVETRACKGROUP':begin
                        poDummy = state.poCurrTrackGroup
                        widget_control, ev.top, set_uValue = state, /no_copy
                           *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
                        widget_control, ev.top, get_uValue = state, /no_copy
                        oGroup = *state.poCurrTrackGroup
                        save, oGroup, filename = state.currTrackGroupFileName
          endcase
       'SAVEPARAMSAS':begin
                        file = dialog_pickfile( /write, path = state.currTrackGroupFileName, filter = '*.sav')
                        if (file ne '') then begin
                           if ( (strMid(file, strlen(file)-4, 4)) ne '.sav') then file = file + '.sav'
                           oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
                           save, oParamContainer, filename = file
                        endif
                        fUpdate = 0b
          endcase
       'OPENPARAMS':begin
                        file = dialog_pickfile( /read, path = state.currTrackGroupFileName, filter = '*.sav')
                        if (file ne '') then begin
                           oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
                           for i = oParamContainer->count()-1, 0, -1  do begin
                              oParamContainer->remove, position = i
                              obj_destroy, (oParamContainer->get(position = i))
                           endfor
                           restore, file, restored_objects = oNewParamContainer, /relaxed
                           oNewParamContainer = oNewParamContainer[0]
                           for i = 0, oNewParamContainer->count()-1 do oParamContainer->add, (oNewParamContainer->get(position = i))
                        endif
        endcase
       'OPENROISTACKTRACK':begin
                        fileName = dialog_pickfile( /read, path = state.currTrackGroupFileName, filter = '*.sav')
                        openr, 1, fileName, error = err
                        close, /all
                        if (err eq 0) then begin
                           currentStackTrackFileName = fileName
                           restore, fileName = fileName, restored_objects = oStackTrackContainer, /relaxed
                           for i = 0, (n_elements(oStackTrackContainer)-1) do if obj_isa(oStackTrackContainer[i], 'C_sROIParam_StackTrackContainer') then begin
                             oROIParams = (*state.poStackTrackContainer)->getAndRemoveROIParamObjs()
                             if obj_valid(oROIParams[0]) then for j = 0, n_elements(oROIParams)-1 do obj_destroy, oROIParams[j]
                             oROIParams = (oStackTrackContainer[i])->getAndRemoveROIParamObjs()
                             if obj_valid(oROIParams[0]) then for j = 0, n_elements(oROIParams)-1 do (*state.poStackTrackContainer)->addoTrackParam, oROIParams[j]
                           endif
                           state.fRefreshMixedData = 1b
                           if (*state.poStackTrackContainer->getfAllParamControl() gt 0) then widget_control, state.wSaveStackTrackParamsAs, sensitive = 1 else widget_control, state.wSaveStackTrackParamsAs, sensitive = 0
                        endif
        endcase
       'APPLYTOALLZSLICESANDCHANNELSANDTIMES':begin
                        zSliceNumber = totalZNum-1
                        if state.fStartFromSelectedTime then timeStart = tPos else timeStart = 0
                             for k = timeStart, totalTNum-1 do begin
                               for j = 0, totalChNum-1  do begin
                                    for i = 0, zSliceNumber  do begin
                                       widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                                       oImage = (*stackState.pimageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = j, zPos = i)
                                       if obj_valid(oImage) then begin

                                           widget_control, stackState.wListTime, get_uValue = uvalueFileParameter, /no_copy
                                              uvalueFileParameter.active = k
                                           widget_control, stackState.wListTime, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy

                                           widget_control, stackState.wListChannel, get_uValue = uvalueFileParameter, /no_copy
                                              uvalueFileParameter.active = j
                                           widget_control, stackState.wListChannel, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy

                                           widget_control, stackState.wListZSlice, get_uValue = uvalueFileParameter, /no_copy
                                              uvalueFileParameter.active = i
                                           widget_control, stackState.wListZSlice, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy

                                           ev_struct = {top:stackState.wTopBase,$
                                                        id:stackState.wListZSlice,$
                                                        handler:stackState.wTopBase,$
                                                        index:i,clicks:1}

                                           widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                                              widget_control, ev.top, set_uValue = state, /no_copy
                                                s_ISM_List_Event, ev_struct
                                              widget_control, ev.top, get_uValue = state, /no_copy
                                           widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                                       endif
                                       widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                                endfor
                            endfor
                           endfor
          endcase
       'APPLYTOALLZSLICESANDCHANNELS':begin
                            zSliceNumber = totalZNum-1
                               for j = 0, totalChNum-1  do begin
                                for i = 0, zSliceNumber  do begin
                                widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                                    oImage = (*stackState.pimageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = j, zPos = i)
                                       if obj_valid(oImage) then begin

                                 widget_control, stackState.wListChannel, get_uValue = uvalueFileParameter, /no_copy
                                     uvalueFileParameter.active = j
                                 widget_control, stackState.wListChannel, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy

                                 widget_control, stackState.wListZSlice, get_uValue = uvalueFileParameter, /no_copy
                                     uvalueFileParameter.active = i
                                 widget_control, stackState.wListZSlice, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy

                                 ev_struct = {top:stackState.wTopBase,$
                                                  id:stackState.wListZSlice,$
                                                  handler:stackState.wTopBase,$
                                                  index:i,$
                                                  clicks:1 }

                                 widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                                     widget_control, ev.top, set_uValue = state, /no_copy
                                        s_ISM_List_Event, ev_struct
                                     widget_control, ev.top, get_uValue = state, /no_copy
                                 widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                                endif
                                widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                               endfor
                            endfor
          endcase
       'APPLYTOALLZSLICESANDTIMES':begin
                        zSliceNumber = totalZNum-1
                        if (state.fStartFromSelectedTime eq 1) then timeStart = tPos else timeStart = 0
                               for k = timeStart, totalTNum-1  do begin
                                for i = 0, zSliceNumber do begin
                             widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                                 oImage = (*stackState.pimageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = chPos, zPos = i)
                                    if obj_valid(oImage) then begin

                              widget_control, stackState.wListTime, get_uValue = uvalueFileParameter, /no_copy
                                  uvalueFileParameter.active = k
                              widget_control, stackState.wListTime, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy

                              widget_control, stackState.wListZSlice, get_uValue = uvalueFileParameter, /no_copy
                                  uvalueFileParameter.active = i
                              widget_control, stackState.wListZSlice, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy

                              ev_struct = {top:stackState.wTopBase,$
                                               id:stackState.wListZSlice,$
                                               handler:stackState.wTopBase,$
                                               index:i,$
                                               clicks:1 }

                              widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                                  widget_control, ev.top, set_uValue = state, /no_copy
                                     s_ISM_List_Event, ev_struct
                                  widget_control, ev.top, get_uValue = state, /no_copy
                              widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                             endif
                             widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                            endfor
                           endfor
          endcase
       'APPLYTOALLZSLICES':begin
                        zSliceNumber = totalZNum-1
                           for i = 0, zSliceNumber do begin
                           widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                                 oImage = (*stackState.pimageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = i)
                               if obj_valid(oImage) then begin

                             widget_control, stackState.wListZSlice, get_uValue = uvalueFileParameter, /no_copy
                              uvalueFileParameter.active = i
                             widget_control, stackState.wListZSlice, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy

                             ev_struct = {top:stackState.wTopBase,$
                                            id:stackState.wListZSlice,$
                                            handler:stackState.wTopBase,$
                                            index:i,$
                                            clicks:1 }

                             widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                              widget_control, ev.top, set_uValue = state, /no_copy
                                  s_ISM_List_Event, ev_struct
                              widget_control, ev.top, get_uValue = state, /no_copy
                             widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           endif
                           widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        endfor
          endcase
       'APPLYTOALLTIMES':begin
                        zSliceNumber = 0
                        if (state.fStartFromSelectedTime eq 1) then timeStart = tPos else timeStart = 0
                               for k = timeStart, totalTNum-1 do begin
                           widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                                 oImage = (*stackState.pimageStackInfoObject)->getSelectedImageObject(tPos = k, chPos = chPos, zPos = 0)
                               if obj_valid(oImage) then begin
                             widget_control, stackState.wListTime, get_uValue = uvalueFileParameter, /no_copy
                              uvalueFileParameter.active = k
                             widget_control, stackState.wListTime, set_list_select = uvalueFileParameter.active, set_value = *uvalueFileParameter.value, set_uValue = uvalueFileParameter, /no_copy
                             ev_struct = {top:stackState.wTopBase,$
                                            id:stackState.wListZSlice,$
                                            handler:stackState.wTopBase,$
                                            index:0,$
                                            clicks:1 }
                             widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                              widget_control, ev.top, set_uValue = state, /no_copy
                                  s_ISM_List_Event, ev_struct
                              widget_control, ev.top, get_uValue = state, /no_copy
                             widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           endif
                           widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                         endfor
          endcase
       'XYMIXEDDATAPLOTONOFF':begin        ; Switch Flag (1/0 ON/OFF) for the View-Mixed-Data-Plot
          state.fXYMixedDataPlotControl = s_ToggleButtonOnOffState(state.wXYMixedDataPlotOnOff)
          if (state.fXYMixedDataPlotControl eq 0) then begin
              child_MixedDataPlot_tlb = state.child_MixedDataPlot_tlb
              widget_control, ev.id, set_uValue = uValue, /no_copy
              widget_control, ev.top, set_uValue = state, /no_copy
                 if widget_info(child_MixedDataPlot_tlb, /valid) then s_GroupParamPlot_Window_cleanUp, child_MixedDataPlot_tlb
              widget_control, ev.id, get_uValue = uValue, /no_copy
              widget_control, ev.top, get_uValue = state, /no_copy
              state.child_MixedDataPlot_tlb = -1
          endif
      endcase
       'CALCULATESELECTEDPARAMETER':begin
          if (strMid(selROIObjSubParam, 0, 5) eq '(on )') then begin
              poDummy = state.poCurrTrackGroup
              widget_control, ev.top, set_uValue = state, /no_copy
                 (*poDummy)->calculateGroupParameters, selROIParamName = strMid(selROIObjSubParam, 6, strlen(selROIObjSubParam)-6), stack_tlb = stack_tlb
              widget_control, ev.top, get_uValue = state, /no_copy

              if state.fParamFilter then begin
                 widget_control, ev.top, set_uValue = state, /no_copy
                   widget_control, ev.id, set_uValue = 'APPLYSELECTEDPARAMETERFILTER'
                   s_ROI_TrackManipulator_Window_Button_Event, ev
                   widget_control, ev.id, set_uValue = 'CALCULATESELECTEDPARAMETER'
                 widget_control, ev.top, get_uValue = state, /no_copy
              endif
          endif
       endcase
       'CALCULATEALLPARAMETERS':begin
                     poDummy = state.poCurrTrackGroup
                     widget_control, ev.top, set_uValue = state, /no_copy
                      (*poDummy)->calculateGroupParameters, stack_tlb = stack_tlb
                     widget_control, ev.top, get_uValue = state, /no_copy

                        if state.fParamFilter then begin
                            widget_control, ev.top, set_uValue = state, /no_copy
                               s_ROITM_UpdateWidgets, ev.top
                               widget_control, ev.id, set_uValue = 'APPLYSELECTEDPARAMETERFILTER'
                               s_ROI_TrackManipulator_Window_Button_Event, ev
                               widget_control, ev.id, set_uValue = 'CALCULATEALLPARAMETERS'
                            widget_control, ev.top, get_uValue = state, /no_copy
                        endif

                        widget_control, state.wListTrackGroupParams, get_uValue = uValueList, /no_copy
                           uValueList.active = ROIObjParamPos
                        widget_control, state.wListTrackGroupParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy
                        widget_control, state.wListTrackSubParams, get_uValue = uValueList, /no_copy
                           uValueList.active = ROIObjSubParamPos
                        widget_control, state.wListTrackSubParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy
                        if ( (where(tag_names(ev) eq 'SILENT'))[0] ne -1) then fUpdate = 0b
          endcase
       'APPLYSELECTEDPARAMETERFILTER':begin
                        if (strMid(selROIObjSubParam, 0, 5) eq '(on )') then begin

                             ;get selected oParamContainer and ROIParam
                           oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
                           oROIParam = oParamContainer->get(position = ROIObjParamPos)

                           (*state.poCurrTrackGroup)->selectObjectsInThresholdIntervals, selROIParamName = strMid(selROIObjSubParam, 6, strlen(selROIObjSubParam)-6)

                           widget_control, ev.top, set_uValue = state, /no_copy
                           objectNumberVector = *(*(oROIParam->getpParamStruct())).pROINumberVect
                           for i = 0, (oParamContainer->count()-1) do begin
                               oROIParam = oParamContainer->get(position = i)
                               if ( ((*(oROIParam->getpParamStruct())).type eq 'Single ROI-Parameter-Method') or $
                                    ((*(oROIParam->getpParamStruct())).type eq '3D ROI-Parameter-Method') ) then $
                                  oROIParam->updateParamValueStruct, objectNumberVector = objectNumberVector, /all
                           endfor
                          widget_control, ev.top, get_uValue = state, /no_copy
                        endif
                        if ((where(tag_names(ev) eq 'SILENT'))[0] ne -1) then fUpdate = 0b
          endcase
       'APPLYALLPARAMETERFILTERS':begin
                        oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
                        for i = 0, (oParamContainer->count())-1 do begin
                           widget_control, state.wListTrackGroupParams, get_uValue = uValueList, /no_copy
                             uValueList.active = i
                           widget_control, state.wListTrackGroupParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy

                           oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
                           oROIParam = oParamContainer->get(position = i)

                           whereROIParamActive = where(*(*(oROIParam->getpParamStruct())).pActive)
                           if (whereROIParamActive[0] ne -1) then begin
                             for j = 0, n_elements(whereROIParamActive)-1 do begin

                              widget_control, state.wListTrackSubParams, get_uValue = uValueList, /no_copy
                                 uValueList.active = whereROIParamActive[j]
                              widget_control, state.wListTrackSubParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy

                              widget_control, ev.top, set_uValue = state, /no_copy
                                 widget_control, ev.id, set_uValue = 'APPLYSELECTEDPARAMETERFILTER'
                                    s_ROI_TrackManipulator_Window_Button_Event, ev
                                 widget_control, ev.id, set_uValue = 'APPLYALLPARAMETERFILTERS'
                             widget_control, ev.top, get_uValue = state, /no_copy

                             endfor
                           endif
                        endfor

                        widget_control, state.wListTrackGroupParams, get_uValue = uValueList, /no_copy
                           uValueList.active = ROIObjParamPos
                        widget_control, state.wListTrackGroupParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy
                        widget_control, state.wListTrackSubParams, get_uValue = uValueList, /no_copy
                           uValueList.active = ROIObjSubParamPos
                        widget_control, state.wListTrackSubParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy
                        if ((where(tag_names(ev) eq 'SILENT'))[0] ne -1) then fUpdate = 0b
          endcase
       'DELETE':begin
                         case state.fListSelect of
                           3:begin
                                 widget_control, state.wListSelectTrackModels, get_uValue = uListValue, /no_copy
                                    if ((*uListValue.value)[0] ne '-NO SELECTION-') then begin
                                       case uListValue.active of
                                          0:if (n_elements(*uListValue.value) eq 1) then *uListValue.value = '-NO SELECTION-' else *uListValue.value = (*uListValue.value)[1:*]
                                          n_elements(*uListValue.value)-1:*uListValue.value = (*uListValue.value)[0:n_elements(*uListValue.value)-2]
                                          else:*uListValue.value = [(*uListValue.value)[0:uListValue.active-1], (*uListValue.value)[uListValue.active+1:*]]
                                       endcase
                                       uListValue.active = (uListValue.active-1) > 0
                                    endif
                                 widget_control, state.wListSelectTrackModels, set_list_select = uListValue.active, set_value = *uListValue.value, set_uValue = uListValue, /no_copy
                           endcase
                           else:begin
                              oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
                              count = oParamContainer->count()-1
                              if ((ROIObjParamPos ge 0) and (ROIObjParamPos le count)) then begin
                                 obj_destroy, oParamContainer->get(position = (ROIObjParamPos > 0))
                                 oParamContainer->remove, position = (ROIObjParamPos > 0)

                                 widget_control, state.wListTrackGroupParams, get_uValue = uvalueFileParameter, /no_copy
                                 if (ROIObjParamPos eq count) then uvalueFileParameter.active = (ROIObjParamPos - 1)>0 else $
                                   uvalueFileParameter.active = (ROIObjParamPos)<count
                                 widget_control, state.wListTrackGroupParams, set_uValue = uvalueFileParameter, /no_copy
                              endif
                           endcase
                         endcase
         endcase
       'KEEPVSONOFF':begin
                        state.fKeepVisualParams = s_ToggleButtonOnOffState(state.wKeepVisualParamsOnOff)
                        fUpdate = 0b
         endcase
       'KEEPSELECTEDROIPARAMFILTERSONOFF':begin
                        state.fKeepSelectParamFilters = s_ToggleButtonOnOffState(state.wKeepSelectROIParamsFiltersOnOff)
                        fUpdate = 0b
         endcase
       'KEEPSELECTEDROIPARAMSONOFF':begin
                        state.fKeepSelectParams = s_ToggleButtonOnOffState(state.wKeepSelectROIParamsOnOff)
                        if (state.fComplParams) then state.fComplParams = s_ToggleButtonOnOffState(state.wComplementROIParamsOnOff)
                        if ((state.fKeepSelectParams) and not(state.fRestoreAndCalculate) ) then state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        if (not(state.fKeepSelectParams) and (state.fRestoreAndCalculate) ) then state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        if ((state.fKeepSelectParams) and not(state.fSaveResults) ) then state.fSaveResults = s_ToggleButtonOnOffState(state.wSaveResultsOnOff)
                        if (not(state.fKeepSelectParams) and (state.fSaveResults) ) then state.fSaveResults = s_ToggleButtonOnOffState(state.wSaveResultsOnOff)
                        fUpdate = 0b
         endcase
       'COMPLEMENTROIPARAMSONOFF':begin
                        state.fComplParams = s_ToggleButtonOnOffState(state.wComplementROIParamsOnOff)
                        if (state.fKeepSelectParams) then state.fKeepSelectParams = s_ToggleButtonOnOffState(state.wKeepSelectROIParamsOnOff)
                        if ((state.fComplParams) and not(state.fRestoreAndCalculate) ) then state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        if (not(state.fComplParams) and (state.fRestoreAndCalculate) ) then state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        if ((state.fComplParams) and not(state.fSaveResults) ) then state.fSaveResults = s_ToggleButtonOnOffState(state.wSaveResultsOnOff)
                        if (not(state.fComplParams) and (state.fSaveResults) ) then state.fSaveResults = s_ToggleButtonOnOffState(state.wSaveResultsOnOff)
                        fUpdate = 0b
         endcase
       'CALCULATEPARAMETERSONOFF':begin
                        state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        fUpdate = 0b
         endcase
       'PARAMETERFILTERONOFF':begin
                        state.fParamFilter = s_ToggleButtonOnOffState(state.wParamFilterOnOff)
                        fUpdate = 0b
         endcase
       'SaveResultsONOFF':begin
                        state.fSaveResults = s_ToggleButtonOnOffState(state.wSaveResultsOnOff)
                        fUpdate = 0b
         endcase
       'SHOWASWIDGETONOFF':begin
                        state.fParamWidget = s_ToggleButtonOnOffState(state.wParamWidgetOnOff)
                        fUpdate = 0b
         endcase
       'SHOWASDATATABLEONOFF':begin
                         state.fParamTable = s_ToggleButtonOnOffState(state.wParamTableOnOff)
                        fUpdate = 0b
         endcase
       'SETXMIXEDAXIS':begin
                        state.fXYZMixedSensitive = 0
                        widget_control, state.wXYZAxisButton, set_value = 'X-Axis |-> '
         endcase
       'SETYMIXEDAXIS':begin
                        state.fXYZMixedSensitive = 1
                        widget_control, state.wXYZAxisButton, set_value = 'Y-Axis |-> '
         endcase
       'SETZMIXEDAXIS':begin
                        state.fXYZMixedSensitive = 2
                        widget_control, state.wXYZAxisButton, set_value = 'z-Axis |-> '
         endcase
       'ROIMODELWINDOWONOFF':begin
                        state.fROIModelWindow = s_ToggleButtonOnOffState(state.wROIModelWindowOnOff)
                             if state.fROIModelWindow then begin
                                widget_control, ev.top, tlb_get_size = baseSize, tlb_get_Offset = baseOffset
                                widget_control, ev.top, set_uValue = state, /no_copy
                                s_ROI_ModelManipulator_Window, groupLeader = ev.top, basePosition = (baseSize+baseOffset)
                                widget_control, ev.top, get_uValue = state, /no_copy
                             endif else begin
                                if widget_info(state.child_ROIModelWindow_tlb, /valid_id) then begin
                                widget_control, state.child_ROIModelWindow_tlb, /destroy
                                state.child_ROIModelWindow_tlb = -1l
                               endif
                             endelse
                        fUpdate = 0b
         endcase
       'STARTFROMSELECTEDTIMEONOFF':begin
                        state.fStartFromSelectedTime = s_ToggleButtonOnOffState(state.wStartFromSelectedTimeOnOff)
                        fUpdate = 0b
         endcase
       else:
       endcase

    widget_control, ev.id, set_uValue = uValue, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy
    if fUpdate then s_ROITM_UpdateWidgets, ev.top
end


pro s_ROITM_AddParam_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    widget_control, ev.id, get_value = selROIParameter
    oROIParam = obj_new(selROIParameter)
    pParamStruct = oROIParam->getpParamStruct()
    widget_control, ev.top, get_uValue = state, /no_copy
       case (*pParamStruct).type of
       'Track-Parameter-Method':begin
          if obj_valid(*state.poCurrTrackGroup) then begin
             (*state.poCurrTrackGroup)->addParamObj, oROIParam
          endif
       endcase
       endcase
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ROITM_UpdateWidgets, ev.top
end

pro s_ROITM_UpdateGraphicModel, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      stack_tlb = state.stack_tlb
   widget_control, wTopBase, set_uValue = state, /no_copy
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos

   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      image = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
      ; MOR - 05Jul2010
      ; get real pixel info to convert the track object coordinates back to pixel coords for plotting results
      (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       xReal =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
       yReal = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
       
   widget_control, stack_tlb, set_uValue = stackState, /no_copy

   widget_control, wTopBase, get_uValue = state, /no_copy
      widget_control, state.wListSelectTrackModels, get_uValue = uValue, /no_copy
        initModel = (*uValue.value)[uValue.active]
        id = (*uValue.id)[n_elements(*uValue.id)-1]
      widget_control, state.wListSelectTrackModels, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy
      case initModel of
      'Track Path':begin
       
         iimage, image, overplot = id, name = strCompress('imagen-'+string(tPos), /rem), /view_next
         
      endcase
      
      else:
      endcase
      widget_control, state.wListSelectTrackModels, get_uValue = uValue, /no_copy
        *uValue.id = [*uValue.id, id]
      widget_control, state.wListSelectTrackModels, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROITM_InitGraphicModel, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      tempState = state
      stack_tlb = state.stack_tlb
   widget_control, wTopBase, set_uValue = state, /no_copy

           widget_control,tempState.groupLeader , get_uValue = state, /no_copy
             roiState = state
           widget_control,tempState.groupLeader , set_uValue = state, /no_copy
           
   widget_control, wTopBase, get_uValue = state, /no_copy
      stack_tlb = state.stack_tlb
   widget_control, wTopBase, set_uValue = state, /no_copy
   ;s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalZNum = totalZNum
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalZNum = totalZNum,totalTNum = totalTNum,clusPos = clusPos,selROIGroupObj = selROIGroupObj ; Susana por clusPos

   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      image = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
            ; MOR - 05Jul2010
      ; get real pixel info to convert the track object coordinates back to pixel coords for plotting results
      (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
       yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
       xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
       yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
       xReal = xRe/xPixel
       yReal = yRe/yPixel
       ; get volume for concentration stats - 14
       zRe = 10. ; 10 um in z-direction
       conFactor = 1./(((xRe * yRe * zRe) mod 10E6) /1E6 ); want the units to be 10E6/mL, so need factor in 10E6 to divide count in entire frame
       
        ;MOR - save directory with stack info for saving images with results
        tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
        ; MOR - 8Sept 2010 - get border mask saved in cluster 10
       ; border =(*stackState.pImageStackInfoObject)->getSelectedClusterMask(tPos = 0, chPos = chPos, zPos = zPos, clusPos = 10) 
        
        
   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   widget_control, wTopBase, get_uValue = state, /no_copy
   
  
      widget_control, state.wListSelectTrackModels, get_uValue = uValue, /no_copy
        initModel = (*uValue.value)[uValue.active]
      widget_control, state.wListSelectTrackModels, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy

      
     scale_factor = 1
     
     case initModel of
     'Track Path':begin
          pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
          tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
          tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
          tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
           
          paramStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
          
          image_size = size(image,/dim)
          
          if paramStruct.ok then begin
          
             image_x = image_size[0]
             image_y = image_size[1]
             yr = [0, max(image_y)]
             xr = [0, max(image_x)]
             k = 1
             y = .45
             xs = [k+45, y*2]
             ys = [k+45, y*2]
  
             oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
             oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
             oXTitle = obj_new('IDLgrText', 'X Position en um', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys)
             oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
             oYTitle = obj_new('IDLgrText', 'Y Position en um', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys)
             oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
             oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotColor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
             oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotColor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)
             
             oModel = obj_new('IDLgrModel')
             oModel->remove, /all
             oModel->add, oImage
             oModel->add, oPlot
             oModel->add, oXAxis
             oModel->add, oYAxis
             oModel->add, oYAxisUp
             oModel->add, oXAxisUp
          
          endif
                 
          c = [[0,1,0],[0,0,1],[1,0.5,0],[1,0,0],[1,1,0],[0.666667,0,1]] * 255
          dimC = size(c,/dim)
          totObject = (*state.poCurrTrackGroup)->getObjNum()
          totColors = totObject / dimC[1]
          if (totColors*dimC[1] lt totObject) then totColors++
          f = transpose(sindgen(totColors)+1)
          h = f##c[*,0] / totColors
          for i = 1, dimC[1]-1 do begin
            j = f##C[*,i] / totColors
            h = [[h],[j]]  
          endfor
          dif = abs((size(h,/dim))[1] - totObject)
          h = h[*,dif:(size(h,/dim))[1]-1]
          for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
            paramStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
            if paramStruct.ok then begin
            ; MOR - 05Jul2010
            ; convert xyz track values back to pixels to correspond to the image
               oPlot = obj_new('IDLgrPlot', dataX = (*paramStruct.pXYZValues)[*,0] /xReal , dataY = (*paramStruct.pXYZValues)[*,1]/yReal, color = H[*,i], thick = 2, XCOORD_CONV = xs, YCOORD_CONV = ys)
               oModel->add, oPlot
            endif
          endfor
          
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         
         oBuffer = obj_new('IDLgrBuffer', dimensions = [1024, 768])
         oBuffer->draw, oView 
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         window, 0, xsize = 1024, ysize = 768, title =  'Track_Path.tiff'
         tvscl, outImage_1, /true  
         
         write_tiff, s_getPathForSystem() + 'Track_Path.tiff', outImage_1
     endcase
     'Track VCL':begin
     
         ;veloLimits = [0,1., 1.,5., 5.,25., 25.,10000.]
         veloLimits = [0,1., 1.,5., 5.,30., 30.,10000.]
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
              
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]

         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [um]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [um]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp


         ; --- Susana Creando vector para VSL --- INICIO
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         umbralVCL = 50 ;10
         umbralVSL = 2.5 ;um 4    2  1.799 4.5
         umbralVSL_TipoC=4.0 ;máxima VSL que podria presentar un TipoC en 1 segundo
         umbralVCL_TipoC=veloLimits[5]
         ; --- Susana Creando vector para VSL --- FIN
;         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum()) 
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
         ; MOR - comment out, no longer evaluating based on VSL, this is only VCL - BEGIN
;           ; Susana evaluando VSL -- INICIO
;           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
;           vslTotal = sqrt(vslStruct.VSL[*,0]^2 + vslStruct.VSL[*,1]^2 + vslStruct.VSL[*,2]^2)
;           ; Susana evaluando VSL -- FIN
         ; MOR - comment out, no longer evaluating based on VSL, this is only VCL - END
          
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
           ;if vclStruct.ok then vtotal = sqrt(vclStruct.XYZVelocity[*,0]^2 + vclStruct.XYZVelocity[*,1]^2 + vclStruct.XYZVelocity[*,2]^2)
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT
              VCLvect[i] = mean(vtotal)
           
           ; MOR - comment out, no longer evaluating based on VSL, this is only VCL - BEGIN
           
           ;aqui falta evaluar si VCLvect[i] < umbralVCL   &&  vslTotal[i] > umbralVSL
           ;los umbrales VCL y VSL  deben ser definidos según datos experimentales
           ;umbralVSL = máxima migración permitida para un espermio que no se mueve
           ;umbralVCL = máxima o minima velocidad curvilinea para un espermio que no se mueve


; MOR - comment out, no longer evaluating based on VSL, this is only VCL - END

              case 1 of
              VCLvect[i] lt veloLimits[1]: begin
                 col = [0,0,255] ;inmoviles azules "Tipo D"
                 thick = 1 ;4
                 vclVectTipo[i] = 'D' ;Susana
              end
              (VCLvect[i] ge veloLimits[2]) and (VCLvect[i] lt veloLimits[3]): begin
                 col = [0,255,0] ; No progresivos verdes "Tipo C"
                 thick = 1 ;orginal =2
                 vclVectTipo[i] = 'C' ;Susana
              end
              (VCLvect[i] ge veloLimits[4]) and (VCLvect[i] lt veloLimits[5]): begin
                 col = [255,255,0] ;Progresivos Lentros amarillos "Tipo B"
                 thick = 1 ;orginal =2
                 vclVectTipo[i] = 'B' ;Susana
              end
              VCLvect[i] ge veloLimits[6]: begin
                 col = [255,0,0] ;Progresivos Lentros amarillos "Tipo A"
                 thick = 1 ;orginal =2
                 vclVectTipo[i] = 'A' ;Susana
              end
              endcase
              ; MOR - comment out, no longer evaluating based on VSL, this is only VCL - BEGIN
  
           
        
           

           ; MOR - comment out, no longer evaluating based on VSL, this is only VCL - END
           
           if n_elements(*vclStruct.ptValues) gt 1 then begin 
           ; MOR - 05Jul2010
           ; convert xyz tracks to pixels for plotting against image
             oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thick, xcoord_conv = xs, ycoord_conv = ys)
             oModel->add, oPlot
           endif
         endfor
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         ;outImage_1=rotate(outImage_1,7)   ;Susana tratando de rotar verticalmente   ROTATE es solo para 2D, esto es 3D :(
         ;outImage_1=rotate(outImage_1[1,*,*],7)   ;Susana tratando de rotar verticalmente   ROTATE es solo para 2D, esto es 3D :(
         
         
         ; MOR - comment out old path for saving, try path where images are saved instead 
         ;write_tiff, s_getPathForSystem() + 'VCL_trajectory.tiff', outImage_1
         write_tiff, tempFileName + 'VCL_trajectory.tiff', outImage_1
         ; MOR - destroy unneeded objects
         obj_destroy, [oBuffer, oView]
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;path = s_getPathForSystem() + 'VCL_Histo.tiff'
         path = tempFileName + 'VCL_Histo.tiff'
         vect = VCLvect
         name = "curvilinear velocity VCL [um s-1]"
         linesFlag = 1
         
         CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimits = veloLimits, linesFlag = linesFlag, tempFileName = tempFileName
;         
;         ;Susana INICIO   pruebo clasificacion solo con VSL------------------PARTE 2----------------------------
;         vect_elements = n_elements(vect)
;         dummy = where(vclVectTipoVSL eq 'A', count) ;Susana
;         string_n1 ='VSL   TipoA = '
;         string_n3 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
;         ;string_n3 =string_n1+string_n2
;         ;print,string_n3
;         
;         dummy = where(vclVectTipoVSL eq 'B', count) ;Susana
;         string_n1 ='VSL   TipoB = '
;         string_n4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
;         ;string_n4 =string_n1+string_n2
;         ;print,string_n4
;         
;         dummy = where(vclVectTipoVSL eq 'C', count) ;Susana
;         string_n1 ='VSL   TipoC = '
;         string_n5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
;         ;string_n5 =string_n1+string_n2
;         ;print,string_n5
;         
;         dummy = where(vclVectTipoVSL eq 'D', count) ;Susana
;         string_n1 ='VSL   TipoD = '
;         string_n6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
;         ;string_n6 =string_n1+string_n2
;         ;print,string_n6
;         
;         string_n7 = 'TipoA VSL >= ' 
;         string_n8 = string(veloLimits[6], format = '(F6.2)')
;         ;Susana FIN   pruebo clasificacion solo con VSL------------------PARTE 2----------------------------
         
         vect_elements = n_elements(vect)
         ;dummy = where(vect ge veloLimits[6], count)
         dummy = where(vclVectTipo eq 'A', count) ;Susana
         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_0 = 'Type A:'
         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimits[4] and vect lt veloLimits[5], count)   ;Susana
         dummy = where(vclVectTipo eq 'B', count) ;Susana
         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_1 = 'Type B:' 
         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimits[2] and vect lt veloLimits[3], count)   ;Susana
         dummy = where(vclVectTipo eq 'C', count) ;Susana
         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_2 = 'Type C:'
         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimits[0] and vect lt veloLimits[1], count)   ;Susana
         dummy = where(vclVectTipo eq 'D', count) ;Susana
         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_3 = 'Type D:'
         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         ; MOR - comment out all previously defined save paths
         ;fileName = s_getPathForSystem() + 'VCL_Text.tiff'   
         fileName = tempFileName + 'VCL_Text.tiff'
         
         CEDAI_textPlot, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11
         
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_BackgroundImageForVCL.tif'
         ; MOR - use background written in english
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Spermiogram_VCL_Background.bmp'
         ; MOR - comment out all previously defined paths for saving results - BEGIN
;         imageLeft = s_getPathForSystem() + 'VCL_trajectory.tiff'
;         imageRight = s_getPathForSystem() + 'VCL_Histo.tiff'
;         textRight = s_getPathForSystem() + 'VCL_Text.tiff'
;         fileName = s_getPathForSystem() + 'VCL.tiff' 
         ; MOR - comment out all previously defined paths for saving results - END
         
         ; MOR - save results in the same directory as images - BEGIN
         imageLeft =tempFileName + 'VCL_trajectory.tiff'
         imageRight =  tempFileName + 'VCL_Histo.tiff'
         textRight = tempFileName + 'VCL_Text.tiff'
         fileName = tempFileName + 'VCL.tiff' 
         ; MOR - save results in the same directory as images - END
         
         CEDAI_combineImages, background, imageLeft, imageRight, TextRight, filename
         
;         ;Susana INICIO  guardo clasificacion VSL ---
;         ;fileName2 = s_getPathForSystem() + 'VSL_Text.tiff'
;         fileName2 =  tempFileName + 'VSL_Text.tiff' 
;          
;         CEDAI_textPlotVSL, fileName2, string_0, string_1, string_2, string_3, string_n3, string_n4, string_n5, string_n6, string_8, string_9, string_10, string_11,string_n7,string_n8  
;         ;Susana FIN  guardo clasificacion VSL ---
         
      endcase
      'Track Motil':begin
         ;print, 'correctly entered track motility after adding the graphic model'
         veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010 EN USO CEDAI ESTANDARIZACIÓN
         ;;veloLimitsVCL = [0,1., 1.,5., 5.,40., 40.,10000.];Solo para muestra M6 M 10 ambas del 28Enero2010
         ;veloLimitsVCL = [0,10., 10.,15., 15.,35., 35.,10000.] ;Rangos CASA Microptics
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
         
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
         ; do not include in analysis if started after the xx frame in time
         ; number of frames
         minTimeF = 5;  will be value in [frames]
         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]
         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
         minNumFramesTracked = floor((tEnd-tStart)*0.50);  value in [frames]
         ; how close to the border the object is 
         closeBorder = 9; [pixels] -- this value is the same as max_Dist in C_sROIParam_ObjTrackObject.pro    
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_z'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         
         ; write to file the track objects overallType of classification
         name = 'overallTypeTOs_'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U1
         openW, U1, file
         printF, U1, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'TypeNC'
         ; threshold area [in pixels] that the track covers
         areaThreshold = 3; this is 2x2 pixels large, Default = 2
         ; MOR - determine cutoffs for not including TOs in analysis - END
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]
         
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - INICIO
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - FIN
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange

         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
        
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
                                
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         
         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])
         name = 'CoordenadasTOs_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         get_lun, unit
         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
         ;;;;;;; MOR - temporarily printint the color code -----
         printF, Unit, 'zsliceA'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type'+STRING(9B)+'xBox_pix'+STRING(9B)+'yBox_pix'+STRING(9B)+'xBox_X_yBox_pix';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         vslMaxim = 0
         vslMaximPos = 0
         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
         ; --- Susana Creando vector para VSL --- INICIO
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         umbralVCL = 50 ;10
         ; MOR - 9Feb2011 - limits were found using pixels April 2010
         veloLimitsVSL = [2.5,4.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC ESTANDARIZACION
         ; --- Susana Creando vector para VSL --- FIN
         
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
         vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad A,B,C y D
         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         ; MOR - add vector for storing vsl values for the histogram - BEGIN
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum());
         ; MOR - add vector for storing vsl values for the histogram - END
         
         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
         
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
         
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - BEGIN
         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - END 
         
         ; MOR - 12Feb2011 - get concentration in time
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
         
         xBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBoxyBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         xBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBoxyBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana

         ; Susana - Optical Flow 1/2 -  BEGIN - 
           OFvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;OpticalFlow
           ;Sizevect = make_array((*state.poCurrTrackGroup)->getObjNum());Size
           DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Distancia
           DESPvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Desplazamiento
           DESP_DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); DESP/DIST
          
           s_OFParameter = 'Object Speed2 [pixelsPframe]'
           ;s_OFParameter = 'Object Vy [pixelsPframe]'
           strIDOF = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zpos) + '_'  + 'Clus' + string(clusPos) + '_', /rem)
            case selROIGroupObj of
             'oROI2DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Track Objects from Masks')
             'oROI3DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
             endcase
           ; sort objects in trackIDOF
           if (trackIDOF[0] ne -1) then begin
              whtrackIDOF = where(trackIDOF ne -1, count)
              if (count gt 0) then trackIDOF = trackIDOF[whtrackIDOF]
              sorttrackIDOF = fix(trackIDOF[uniq(trackIDOF, sort(trackIDOF))])
           endif

           case selROIGroupObj of
               'oROI2DGroup':trackParamsOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_OFParameter)
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase
           
           case selROIGroupObj of
               'oROI2DGroup':trackParamsTIME = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Object Time [s]')
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase           
           if (count gt 0) then begin
               trackParamsOF = trackParamsOF[whtrackIDOF]
               trackParamsTIME = trackParamsTIME[whtrackIDOF]
           endif
           
;           ; Size 1/2 - BEGIN - 
;           ;Size no me sirve, ya que es tamaño de cabecita brillante, no area de trayectoria
;           s_SizeParameter = 'Object Size [x²]';  'Object Size [x²]'  'Object Size [Pixel²]'   'Object Size [%]'
;           case selROIGroupObj of
;               'oROI2DGroup':trackParamsSize = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_SizeParameter)
;               ;'oROI3DGroup':trackParamsSize = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
;           endcase
;           if (count gt 0) then begin
;               trackParamsSize = trackParamsSize[whtrackIDOF]
;           endif
;           ; Size 1/2 - END -


         ; Susana - Optical Flow 1/2 -  END - 
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana
         
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
           vslTotal = vslVect[i] ;MOR changing calculation
           
           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
           
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
           
;                      ; Susana - Optical Flow 2/2 -  BEGIN - 
;            if (sorttrackIDOF[0] ne -1) then begin
;                whParamOF = where(trackIDOF eq sorttrackIDOF[i])
;                if (whParamOF[0] eq -1) then begin
;                    trackParamsOF_i = [min(trackParamsOF),max(trackParamsOF)]
;                    ;trackParamsSize_i = [min(trackParamsSize),max(trackParamsSize)]; Susana - Size 2/2       ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;                    trackParamsTIME_i = [min(trackParamsTIME),max(trackParamsTIME)]
;                endif else begin 
;                    trackParamsOF_i = trackParamsOF[whParamOF]
;                    ;trackParamsSize_i = trackParamsSize[whParamOF]; Susana - Size 2/2      ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;                    trackParamsTIME_i = trackParamsTIME[whParamOF]
;                endelse
;            endif
;            OFvect[i] = mean(trackParamsOF_i)
;            ;Sizevect[i] = trackParamsSize_i    ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;            trackParamsTIME_i_delta = 0
;            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
;              trackParamsTIME_i_delta = trackParamsTIME_i[1:N_ELEMENTS(trackParamsTIME_i)-1] - trackParamsTIME_i[0:N_ELEMENTS(trackParamsTIME_i)-2]
;            endif
;            ;endif else begin 
;            ;endelse
;            
;           ; Susana - Optical Flow 2/2 -  END - 
           
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
           ; check at what time/frame the object started being tracked
           firstAppeared = (*vclStruct.pTValues)[0]
           
           ; check how many frames the object was tracked
           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0]);
           
           ; check that all elements of the TO are not 'too close' to the border
           ;2D case --- @todo - need to figure out the 3D case
           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
           countBorder = 0
           if(firstAppeared gt 0) then begin
               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
               xO = (*vclStruct.pXYZValues)[0,0]/xReal
               yO = (*vclStruct.pXYZValues)[0,1]/yReal
               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
           endif
           ; create a bounding box of the track path -- convert trajectory to pixels
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
            
            ;xBox = 10000
            xBox = 0 ; Susana
            ;yBox = 10000
            yBox = 0 ;Susana
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
               ; find bounding center of mass values in each dimension
               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
               ; represent in whole pixels rather than subpixel positions
               xMin = floor(xMin)
               xMax = ceil(xMax)
               yMin = floor(yMin)
               yMax = ceil(yMax)
               ; check in each dimension what the number of pixels are
               xBox = xMax - xMin 
               yBox = yMax - yMin
            endif
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
           
           xBox_vect[i] = xBox ; Susana - evaluando xBox e yBox
           yBox_vect[i] = yBox ; Susana - evaluando xBox e yBox
           yBoxyBox_vect[i] = xBox_vect[i]*yBox_vect[i]
           
           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
           ;ALH , creo que no lo informo aùn no
           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)

           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
           
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
               if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
                  VCLvect[i] = 0
                  vslVect[i] = 0  
                  VAPvect[i] = 0
                  vslTotal = 0
               endif else begin
                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
               endelse
             
;lineas siguintes comentadas por susana 4 Junio 2013 - INICIO
;;           ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- INICIO
;            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
;             ;desp=sqrt( ( (vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) )^2 ); en micrómetros, usando VSL
;             ;DESPvect[i] = sqrt( ((((((*vclStruct.pXYZValues)[*,0])[(n_elements((*vclStruct.pXYZValues)[*,0]))-1])-(((*vclStruct.pXYZValues)[*,0])[0])))^2) + ((((((*vclStruct.pXYZValues)[*,1])[(n_elements((*vclStruct.pXYZValues)[*,1]))-1])-(((*vclStruct.pXYZValues)[*,1])[0])))^2) )
;             DESPvect[i] = ((vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) ); en micrómetros, usando VSL    vslTotal*total(vclStruct.dT)
;             
;             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta))/xReal   ;en pixeles
;             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta)); en micrómetros
;             vDist = (vclStruct.XYZDisplacement); en micrómetros  ;vtotal = vclStruct.XYZDisplacement/vclStruct.dT
;             DISTvect[i] = total(vDist); en pixeles
;             ;desp_dist = desp/dist ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
;             DESP_DISTvect[i] = DESPvect[i]/DISTvect[i] ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
;           endif
;;          ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- FIN

;             ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - INICIO
;             ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - INICIO
;             if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 17)) then begin ;en uso 
;             ;if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 20)) then begin ; PChenlo
;                  ;No motiles
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
;               endif
;           ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - FIN
;           ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - FIN
;lineas siguinte comentadas por susana 4 Junio 2013 - FIN
             
             
             ;SUSANA - LIN - INICIO
             ;LIN
             ; MOR @todo - need to update the functions calling LIN, WOB, STR in C_sTrackGroupObject function in order to have
             ; the correct access to these calculations 
             
             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
             if (VCLvect[i] ne 0) then begin
               ;LINvect[i] = vslVect[i]/VCLvect[i]
               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
               ;WOBvect[i] = VAPvect[i]/VCLvect[i]
               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               LINvect[i] = 0
               WOBvect[i] = 0
             endelse
             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
             ; MOR STR calculation - BEGIN
             if (VAPvect[i] ne 0) then begin
               ;STRvect[i] = vslVect[i]/VAPvect[i]
               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               STRvect[i] = 0
            endelse
             ; MOR STR calculation - END
             
             ; MOR - create a vector to store the reason why something was classified as 'NC' - BEGIN
;             overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
             ; legend 
             ; classified for analysis - 0
             ; less than 2 data points - 1
             ; appeared late in time - 2
             ; tracked less than 50% of total time sample - 3
             ; too near the border at time of first track, if it is after the 1st frame - 4
             ; MOR - create a vector to store the reason why something was classified as 'NC' - END
           ;@TODO Susana and/or Victor: spermatozoa should be categorized as   for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
;            print,'D VCL <'+STRING(veloLimitsVCL[1])+' y/o '+' VSL <='+STRING(veloLimitsVSL[0])
;            print,'C VCL >='+STRING(veloLimitsVCL[2])+' y <'+STRING(veloLimitsVCL[3])+'   y/o   VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'C VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'B VCL >='+STRING(veloLimitsVCL[4])+' y <'+STRING(veloLimitsVCL[5])
;            print,'A VCL >='+STRING(veloLimitsVCL[6])
;            print,'-- velocidades --'
             case 1 of
             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
            ; MOR - 6Sept2010 - previous check for 'NC' classification
            ;((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) ): begin
               ; MOR - store reason for 'NC' - BEGIN
               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
                     overallType[i] = 1
                     col = [0,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1
                     break 
               endif
               
               if ((countBorder gt 0)) then begin
                     overallType[i] = 4
                     col = [0,255,120]
                     thickk = 2
                     ;vclVectTipo[i] = 'NCpp' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     cuentaTOsNC += 1
                     break
               endif
               
               if( (firstAppeared ge minTime)) then begin 
                     overallType[i] = 2
                     col = [0,120,255]
                     thickk = 2;
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               if ((timesTracked le minNumFramesTracked)) then begin 
                     overallType[i] = 3
                     col = [120,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif
               ; MOR - store reason for 'NC' - END
             end
;            ( (OFvect[i] le 1.5)  and (OFvect[i] ge 0.12) ): begin
;              ; con flujo laminar ? podria usar todos desde cero hasta 1.5 de speed2
;              col = [0,120,120]
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'D' ;Susana
;              end
             ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - INICIO
            (vslTotal le veloLimitsVSL[0]): begin
              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
              ;col = [255,255,255]
              thickk = 4
              vclVectTipo[i] = 'D' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado
              end
            ((vslTotal gt veloLimitsVSL[0]) and (vslTotal le veloLimitsVSL[1])): begin
              ;para ¿TipoB?  que deben ser TipoC
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              ;col = [255,255,255]
              thickk = 8;default=4
              vclVectTipo[i] = 'C' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado 
              end
              ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - FIN
            (VCLvect[i] lt veloLimitsVCL[1]): begin
              col = [0,0,255] ;inmoviles azules "Tipo D"
              ;thickk = 5;4 ;default
              thickk = 2;4 ;default
              vclVectTipo[i] = 'D' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[2]) and (VCLvect[i] lt veloLimitsVCL[3]): begin
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'C' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[4]) and (VCLvect[i] lt veloLimitsVCL[5]): begin  ;CEDAI
              col = [255,255,0] ;Progresivos lentos amarillos "Tipo B"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'B' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[6]): begin  ;cedai
            col = [255,0,0] ;Progresivos rapidos rojos "Tipo A"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'A' ;Susana
              end
           endcase
           
           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'D') ) then begin  
            ;and (STRvect[i] lt 50) 
            cuentaCirc += 1
            thickk = 2
           endif
           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
           
           if (vslTotal gt vslMaxim) then begin 
            vslMaxim = vslTotal
            vslMaximPos = i
           endif
           
           
           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
             np = (*state.poCurrTrackGroup)->getObjNum()
             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
             miX = (*VAPStruct.pXYZValues)[*,0]
             miY = (*VAPStruct.pXYZValues)[*,1]
             miZ = (*VAPStruct.pXYZValues)[*,2]
             miT = (*VAPStruct.pTValues)
             np = n_elements(miX)
             for j = 0, (np-1) do begin
;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
              if(vclVectTipo[i] ne 'NC') then begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL)+STRING(9B)+strcompress(xBox,/REMOVE_ALL)+STRING(9B)+strcompress(yBox,/REMOVE_ALL)+STRING(9B)+strcompress(xBox*yBox,/REMOVE_ALL);+STRING(10B) ;para IGOR   con xBox e yBox
               printF, Unit,temp
              endif
             endfor
;            ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - BEGIN
;             if(vclVectTipo[i] ne 'NC') then begin ;guardo coordenadas de los clasificados
;             for j = 0, (np-1) do begin
;                temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem);+STRING(10B) ;para IGOR
;               printF, Unit,temp
;              endfor
;             endif
;             ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - END           
           
           ; MOR - get entire time vector
           timeTrack[i] = n_elements(*VAPStruct.ptValues)
           
           
                         ; Susana - Write OpticalFlowParameter in image - BEGIN 
              ;;v_OFmean = mean(trackParamsOF_i)
              ;;s_TagOF = strcompress("OF"+"_"+string(v_OFmean, format = '(F6.2)'),/rem)
              ;s_TagOF = strcompress("OF"+"_"+string(OFvect[i], format = '(F6.2)'),/rem)
              
                s_TagOF = strcompress("LIN"+"_"+string(LINvect[i], format = '(F6.2)'),/rem)
              
              ;;s_TagOF = strcompress("VCL"+"_"+string(VCLvect[i], format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VSL"+"_"+string(vslTotal, format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VAP"+"_"+string(VAPvect[i], format = '(F6.2)'),/rem)
;              if(i ne 76) then begin
;                col = [0, 0, 0]
;              endif
;              if(i eq 6) then begin
;                print,vslTotal
;                print,VCLvect[i]
;                print,VAPvect[i]
;                print,LINvect[i]
;                print,STRvect[i]
;                print,(vslTotal/VCLvect[i])*100.
;                print,(vslTotal/VAPvect[i])*100.
;              endif
               
              ;;;if((vclVectTipo[i] ne 'A') and (vclVectTipo[i] ne 'D')) then begin
              if((LINvect[i] gt 110.) and ((vslTotal/VCLvect[i]) lt 1)) then begin
              ;;if(OFvect[i] le 1.5) then begin  ;if((OFvect[i] le 1.5)  and (OFvect[i] ge 0.12)) then begin
                ;;oPlot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                Plot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [15, 15], locations = [((*vclStruct.pXYZValues)[0,0]+5)/xReal,((*vclStruct.pXYZValues)[0,1]+1)/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
                
                oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [16, 16], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
              endif
              ; Susana - Write OpticalFlowParameter in image - END 
            
           
           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
           if  (vclVectTipo[i] ne 'NC') then begin
           ; MOR - 05Jul2010
           ; convert track xyz to pixels for plotting against image
              if(vclVectTipo[i] eq 'D') then begin
                ;;oSym = obj_new('IDLgrSymbol', data = 3, size = 2.5, thick = thickk);original data = 3=>Dot
                oSym = obj_new('IDLgrSymbol', data = 3, size = 5.5, thick = thickk); Dot de mayor tamaño
              endif else begin
                ;oSym = obj_new('IDLgrSymbol', data = 2, thick = thickk);data = 2=>asterisco   thick=>line thickness
                oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);data = 0 =>no symbol   thick=>line thickness
              endelse
             
              oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oModel->add, oPlot
              
              ; Susana - Incluyo LIN temporalmente  INICIO
;              if((vclVectTipo[i] eq 'B')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;              if((vclVectTipo[i] eq 'C')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;             ; Susana - Incluyo LIN temporalmente  INICIO
              

              
              
             ; Susana - add TO number - INICIO
             ;;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [15, 15], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [10, 10], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [0,1.0,0],UPDIR = [-1.0, -1.0, 0.0])  ; Susana - With ROI number
;             v_TOsSelectos = [35,293,74,77,70,90,96,248,247,253,228,237,222,185,228,237,142,116,146,139,215,222,285,282,259,74,259,168,5,30,2,86,229,166,167,180,210,275]
;             v_whereTOsSelectos = where(v_TOsSelectos eq i, countTOsSelectos)
;             if (countTOsSelectos gt 0) then begin
;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [0,1.0,0],UPDIR = [1.0, 1.0, 0.0])  ; Susana - With ROI number  vertical paralelo a Y
;             oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [11, 11], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [1.0,0.0,0],UPDIR = [-1.0, -1.0, 0.0])  ; Susana - With ROI number  horizontal con flip vertical para verlos bien en trayectories
;             ;UPDIR=[0.0, 1.0, 0.0] parallel to the Y axis
 ;            ;BASELINE=[1.0,0,0] ;(i.e., parallel to the x-axis). 
;             oModel->add, oPlot
 ;            endif
              ; Susana - add TO number - FIN
           endif
           
           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
           ;name = 'CoordsTOs_'
           name = 'CoordsTOs_'+nom
           file = strCompress(tempFileName + name + '.dat', /remove)
           
           get_lun, U
            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            printF, U, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
           
           ; MOR - write to file the track objects overallType of classification - BEGIN    
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           printF, U1,temp 
           ; MOR - write to file the track objects overallType of classification - END
           
         endfor
         print, countTONC_pocosPuntos ;Susana
         print, countTONC_firstAppeared
         print, countTONC_timesTracked
         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         ; MOR - close up file with overalltype classification - BEGIN
         close, U1
         FREE_LUN, U1
         ; MOR - close up file with overalltype classification - END         
         

         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ; MOR - 9Feb 2011 - change so the last directory name prints on Motil.tiff file
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         name = 'VelocityData_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceC'+STRING(9B)+'TOVeloc'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(9B)+'xBoxVeloc_pix'+STRING(9B)+'yBoxVeloc_pix'+STRING(9B)+'xBox_X_yBoxVeloc_pix'+STRING(9B)+'desp'+STRING(9B)+'dist'+STRING(9B)+'desp_dist'+STRING(9B)+s_OFParameter+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR  ; Susana  - con xBox e yBox sin SIZE
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]*yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESP_DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(OFvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR   Susana- con xBox e yBox sin Size
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         
         
         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = '_z'+strcompress(string(zPos),/REMOVE_ALL)+'_' ; susana incluye zPos a nombre imagen
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
         name = 'Motil_MovVars_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceB'+STRING(9B)+'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'TypeMVars'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
         
           
         ;Susana INICIO --  Guardando #TrackObjects Reasignados
         name = 'NumTrackObjectsVSL_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit,transpose(vslReasign)
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN --  Guardando #TrackObjects Reasignados
                 
         
         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
         np = (*state.poCurrTrackGroup)->getObjNum()
         meanVAP_A = 0; promedio tipos A
         meanVAP_B = 0; promedio tipos B
         meanVAP_C = 0; promedio tipos C
         meanVAP_D = 0; promedio tipos D
         meanVAP_NC = 0; promedio tipos NC
         
         meanVCL_A = 0; promedio tipos A
         meanVCL_B = 0; promedio tipos B
         meanVCL_C = 0; promedio tipos C
         meanVCL_D = 0; promedio tipos D
         meanVCL_NC = 0; promedio tipos NC
         
         meanVSL_A = 0; promedio tipos A
         meanVSL_B = 0; promedio tipos B
         meanVSL_C = 0; promedio tipos C
         meanVSL_D = 0; promedio tipos D
         meanVSL_NC = 0; promedio tipos NC
         
         meanLIN_A = 0; promedio LIN tipos A
         meanLIN_B = 0; promedio LIN tipos B
         meanLIN_C = 0; promedio LIN tipos C
         meanLIN_D = 0; promedio LIN tipos D
         meanLIN_NC = 0; promedio LIN tipos NC
         
         meanSTR_A = 0; promedio STR tipos A
         meanSTR_B = 0; promedio STR tipos B
         meanSTR_C = 0; promedio STR tipos C
         meanSTR_D = 0; promedio STR tipos D
         meanSTR_NC = 0; promedio STR tipos NC
         
         meanWOB_A = 0; promedio WOB tipos A
         meanWOB_B = 0; promedio WOB tipos B
         meanWOB_C = 0; promedio WOB tipos C
         meanWOB_D = 0; promedio WOB tipos D
         meanWOB_NC = 0; promedio WOB tipos NC
         
         meanVAP_Acnt = 0; counter para tipos A
         meanVAP_Bcnt = 0; counter para tipos B
         meanVAP_Ccnt = 0; counter para tipos C
         meanVAP_Dcnt = 0; counter para tipos D
         meanVAP_NCcnt = 0; counter para tipos NC
         ;desde aqui descomenté -Susana
         for h = 0, (np-1) do begin
          case 1 of
            (vclVectTipo[h] eq 'A'): begin
              meanVAP_A += VAPvect[h]
              meanVCL_A += VCLvect[h]
              meanVSL_A += vslVect[h]
              meanLIN_A +=  LINvect[h]
              meanSTR_A +=  STRvect[h]
              meanWOB_A +=  WOBvect[h]
              ;meanALH_A += ALHvect[h]
              meanVAP_Acnt += 1
              end
            (vclVectTipo[h] eq 'B'): begin
              meanVAP_B += VAPvect[h]
              meanVCL_B += VCLvect[h]
              meanVSL_B += vslVect[h]
              meanLIN_B += LINvect[h]
              meanSTR_B +=  STRvect[h]
              meanWOB_B +=  WOBvect[h]
              ;meanALH_B += ALHvect[h]
              meanVAP_Bcnt += 1
              end
            (vclVectTipo[h] eq 'C'): begin
              meanVAP_C += VAPvect[h]
              meanVCL_C += VCLvect[h]
              meanVSL_C += vslVect[h]
              meanLIN_C += LINvect[h]
              meanSTR_C +=  STRvect[h]
              meanWOB_C +=  WOBvect[h]
              ;meanALH_C += ALHvect[h]
              meanVAP_Ccnt += 1
              end
            (vclVectTipo[h] eq 'D'): begin
              meanVAP_D += VAPvect[h]
              meanVCL_D += VCLvect[h]
              meanVSL_D += vslVect[h]
              meanLIN_D += LINvect[h]
              meanSTR_D +=  STRvect[h]
              meanWOB_D +=  WOBvect[h]
              ;meanALH_D += ALHvect[h]
              meanVAP_Dcnt += 1
              end
            (vclVectTipo[h] eq 'NC'): begin
              meanVAP_NC += VAPvect[h]
              meanVCL_NC += VCLvect[h]
              meanVSL_NC += vslVect[h]
              meanLIN_NC += LINvect[h]
              meanSTR_NC +=  STRvect[h]
              meanWOB_NC +=  WOBvect[h]
              ;meanALH_NC += ALHvect[h]
              meanVAP_NCcnt += 1
              end
           endcase
         endfor
         ;hasta aqui descomenté--Susana
         meanVAP_A /= meanVAP_Acnt
         meanVAP_B /= meanVAP_Bcnt
         meanVAP_C /= meanVAP_Ccnt
         meanVAP_D /= meanVAP_Dcnt
         meanVAP_NC /= meanVAP_NCcnt
         
         meanVCL_A /= meanVAP_Acnt
         meanVCL_B /= meanVAP_Bcnt
         meanVCL_C /= meanVAP_Ccnt
         meanVCL_D /= meanVAP_Dcnt
         meanVCL_NC /= meanVAP_NCcnt
         
         meanVSL_A /= meanVAP_Acnt
         meanVSL_B /= meanVAP_Bcnt
         meanVSL_C /= meanVAP_Ccnt
         meanVSL_D /= meanVAP_Dcnt
         meanVSL_NC /= meanVAP_NCcnt
         
         meanLIN_A /= meanVAP_Acnt
         meanLIN_B /= meanVAP_Bcnt
         meanLIN_C /= meanVAP_Ccnt
         meanLIN_D /= meanVAP_Dcnt
         meanLIN_NC /= meanVAP_NCcnt
         
         meanSTR_A /= meanVAP_Acnt
         meanSTR_B /= meanVAP_Bcnt
         meanSTR_C /= meanVAP_Ccnt
         meanSTR_D /= meanVAP_Dcnt
         meanSTR_NC /= meanVAP_NCcnt
         
         meanWOB_A /= meanVAP_Acnt
         meanWOB_B /= meanVAP_Bcnt
         meanWOB_C /= meanVAP_Ccnt
         meanWOB_D /= meanVAP_Dcnt
         meanWOB_NC /= meanVAP_NCcnt
         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
         
         
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         
         ; MOR - comment out old path for saving, try path where images are saved instead 
         write_tiff, tempFileName + 'Motil_trajectory'+nom+'.tiff', outImage_1
         
         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - INICIO
         for v=0, totalTNum-1 do begin
            widget_control, stack_tlb, get_uValue = stackState, /no_copy
              imagev = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = v, chPos = chPos, zPos = zPos)
            widget_control, stack_tlb, set_uValue = stackState, /no_copy
            
            ;oImagev = obj_new('IDLgrImage',imagev, xcoord_conv = xs, ycoord_conv = ys)
            oImage->setProperty, data = imagev
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_v
         
              write_tiff, tempFileName + '\Video\Motil_trajectory_'+string(v,format='("",I3.3)')+'.tiff', outImage_v
         endfor
         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - FIN
         
         ; MOR - destroy objects created for plotting to free up memory
         obj_destroy, [oBuffer, oView]
         
         ; MOR - add creation of VSL histogram - BEGIN
         path = tempFileName + 'VSL_Histo'+nom+'.tiff'
         
         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 ;Aqui reemplazo ne por eq, es decir envio al histograma todos los trackobjects menos los reasignados
         if(whClassified[0] ne -1) then begin; Susana1
          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
;         vect = vslVect ; MOR - commented out previous code
          name = "rectilinear velocity VSL [um s-1]"
          ;name = "rectilinear velocity VSL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          
          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
          CEDAI_histogramPlot_VSL, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName
          ; MOR - add creation of VSL histogram - END
          ;CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVSL_OK = 1
         endif; Susana1
         ;endif else begin; Susana1
            ;
         ;endelse; Susana1
         
         
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;path = s_getPathForSystem() + 'VCL_Histo.tiff'
         ;path = tempFileName + 'VCL_Histo.tiff'
         path = tempFileName + 'VCL_Histo'+nom+'.tiff'
         
         v_histogVCL_OK = 0 ; Susana - controlar si existe histograma VCL de clasificados no reasignados
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
         if(whClassified[0] ne -1) then begin; Susana2
          ; Susana - A B C o D, no reasignado
          vect = VCLvect[whClassified]
          name = "curvilinear velocity VCL [um s-1]"
          ;name = "curvilinear velocity VCL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          linesFlag = 1
          CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVCL_OK = 1
         endif; Susana2
         ;endif else begin; Susana2
            ;nada
            ;El problema de no haber alternativa es que no crea imágenes como VCL_Histo.tiff
         ;endelse; Susana2
         
         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
         
         ;Susana INICIO    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom2 = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
         nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
         ;name = 'DatosPChenlo_2011'
         name = 'Datos_'+nom2
         
         ;Susana - reemplazo tempFileName INICIO
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         ;Susana - reemplazo tempFileName FIN
         
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         get_lun, unitPCh
         OPENR, unitPCh, file, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitPCh, file
           ;printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         
         ;openW, unitPCh, file
         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
         ;recuentos
         aux = where(vclVectTipo eq 'A', tipoA)
         aux = where(vclVectTipo eq 'B', tipoB)
         aux = where(vclVectTipo eq 'C', tipoC)
         aux = where(vclVectTipo eq 'D', tipoD)
         aux = where(vclVectTipo eq 'NC', tipoNC)
         ;porcentajes
         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
         ;print,tipoA+tipoB+tipoC+tipoD  corresponde a vect_elements
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         ;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana FIN    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         
         
         ;dummy = where(vect ge veloLimitsVCL[6], count)
         dummy = where(vclVectTipo eq 'A', count) ;Susana
         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_0 = 'Type A:'
         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[4] and vect lt veloLimitsVCL[5], count)   ;Susana
         dummy = where(vclVectTipo eq 'B', count) ;Susana
         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_1 = 'Type B:' 
         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[2] and vect lt veloLimitsVCL[3], count)   ;Susana
         dummy = where(vclVectTipo eq 'C', count) ;Susana
         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_2 = 'Type C:'
         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[0] and vect lt veloLimitsVCL[1], count)   ;Susana
         dummy = where(vclVectTipo eq 'D', count) ;Susana
         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_3 = 'Type D:'
         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Motil_Text'+nom+'.tiff'
         
         
         ;SUSANA - Contabilizando trackobjects NC (No Clasificados) 
         dummy = where(vclVectTipo eq 'NC', count) ;Susana
         ;string_21 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_21 = strcompress( string(fix(count)), /remove_all) + ' sperms'
         ;string_23 = 'NC       :'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - INICIO
         string_23 =strcompress('#Traj.Circ.:' + string(fix(cuentaCirc)), /remove_all) + '(LIN<50) '+ '#NC:'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - FIN
         
         ;filename2 = nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])  ;Susana incluye nombre de muestra en informe para evitar error al ver informe, borrar 
         ;string_27 = string(100.*count / vect_elements, format = '(F6.2)') + ' %' ;NC no estan incluidos en el total de espermatozoides
         CEDAI_textPlot, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11,string_21,string_23, nom
         
         ; MOR - 12Feb 2011- average number of objects found per time
         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
         totalNumObjs.avg = stats[0]*conFactor
         totalNumObjs.std = sqrt(stats[1]*conFactor)
         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
         totalNumObjs.se = sqrt(totalNumObjs.avg*conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         
         string_0 = 'Concentration Statistics Summary'
;         string_1 = strcompress('Frames ') 
         string_2 = strcompress('Mean: ') 
;         string_3 = strcompress('SDev ') 
         string_4 = strcompress('Standard Error: ') 
;         string_5 = strcompress('Median ') 
;         string_6 = strcompress('Min ') 
;         string_7 = strcompress('Max ') 
         
         string_8 = strcompress(string(totalNumObjs.number, format = '(D6.2)'), /rem) 
         string_9 = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem) 
         string_10 = strcompress(string(totalNumObjs.std, format = '(I6.2)'), /rem) 
         string_11 = strcompress(string(totalNumObjs.se, format = '(D6.2)'), /rem) 
         string_12 = strcompress(string(totalNumObjs.med, format = '(I6.2)'), /rem) 
         string_13 = strcompress(string(totalNumObjs.mini, format = '(I6.2)'), /rem) 
         string_14 = strcompress(string(totalNumObjs.maxi, format = '(I6.2)'), /rem) 
         
         string_15 = strcompress('[10E6/mL]', /rem) 
       
         ; MOR - comment out all previously defined save paths
;         fileName = tempFileName + 'Concentration_Text.tiff'     
         
;         CEDAI_textPlot_Count, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7,string_8, string_9, string_10, string_11, string_12, string_13, string_14
         fileName = tempFileName + 'Concentration_Text_Short'+nom+'.tiff'
         
         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
         
         ; MOR - 12Feb2011 write total count data to .dat file 
         dum = max(timeTrack, maxI)
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
         
         name = 'Concentration'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit, 'zslice'+STRING(9B)+'Time' +string(9B)+ 'Count'+STRING(13B)
         indU = uniq(*numObjs.pTValues)       
         for i = 0, n_elements(indU)-1 do begin
            temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            printF, Unit,temp
         endfor
         close, Unit
         FREE_LUN, Unit
         
         ; MOR - save results in the same directory as images - BEGIN
        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_Short.bmp'
;         imageLeft =tempFileName + 'Motil_trajectory.tiff'
;         imageRight =  tempFileName + 'VCL_Histo.tiff'
;         imageRight2 =  tempFileName + 'VSL_Histo.tiff'
;         textRight = tempFileName + 'Motil_Text.tiff'
;         fileName = tempFileName + 'Motil_Short.tiff' 
         imageLeft =tempFileName + 'Motil_trajectory'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_Histo'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo'+nom+'.tiff'
         textRight = tempFileName + 'Motil_Text'+nom+'.tiff'
         fileName = tempFileName + 'Motil_Short'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Concentration_Text_Short'+nom+'.tiff' 
         
         
         CEDAI_combineImages_Motil_Short, background, imageLeft, TextRight, fileName, textLeft
         
        ; MOR - save results in the same directory as images - BEGIN
        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility.bmp'
         
;         imageLeft =tempFileName + 'Motil_trajectory.tiff'
         imageLeft =tempFileName + 'Motil_trajectory'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_Histo'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo'+nom+'.tiff'
         textRight = tempFileName + 'Motil_Text'+nom+'.tiff'
         fileName = tempFileName + 'Motil_new'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana 
         endif; Susana 
         
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         
         
         CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft
         ;CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft, v_histogVSL_OK    ; Susana evalúa si existe histograma
         
         ;SUSANA - Incluyo VAP promedio por grupos LO ESTOY INTENTANDO, AUN NO CREO EL HISTOGRAMA PENDIENTES TAMBIEN HISTOGRAMAS STR Y WOB
         ;SUSANA - add creation of ALH histogram - BEGIN
         veloLimitsVCL = [0,50., 50.,200., 200.,400., 400.,10000.] ; no tengo rangos para ALH :( 
         path = tempFileName + 'histo_ALH'+nom+'.tiff'
         
         vect = ALHvect
         name = "ALH Amplitude of Lateral Head displacement [um]"+nom
         
         linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
         ;CEDAI_histogramPlot_ALH, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
         ;Falta generar un graficador de histograma para ALH o llamar internamente el GraphicModel ALH
         ;SUSANA - add creation of VSL histogram - END
         
         
         fileName = tempFileName + 'Motil_MovVariables_Text'+nom+'.tiff' ; MovementsVariables
         
         string_0 = 'Type A:'
         ;string_4 = string(meanVAP_A, format = '(F6.2)') + ' um/s'
         string_4 = string(meanVAP_A, format = '(F6.1)')
         string_8 = strcompress(string(fix(meanLIN_A), format = '(F6.1)'), /remove_all)
         string_8_1 = strcompress(string(fix(meanSTR_A), format = '(F6.1)'), /remove_all)
         string_8_2 = strcompress(string(fix(meanWOB_A), format = '(F6.1)'), /remove_all)
         string_8_3 = strcompress(string(fix(meanVCL_A), format = '(F6.1)'), /remove_all)
         string_8_4 = strcompress(string(fix(meanVSL_A), format = '(F6.1)'), /remove_all)
         
         string_1 = 'Type B:' 
         ;string_5 = string(meanVAP_B, format = '(F6.2)') + ' um/s'
         string_5 = string(meanVAP_B, format = '(F6.1)')
         string_9 = strcompress(string(fix(meanLIN_B), format = '(F6.1)'), /remove_all)
         string_9_1 = strcompress(string(fix(meanSTR_B), format = '(F6.1)'), /remove_all)
         string_9_2 = strcompress(string(fix(meanWOB_B), format = '(F6.1)'), /remove_all)
         string_9_3 = strcompress(string(fix(meanVCL_B), format = '(F6.1)'), /remove_all)
         string_9_4 = strcompress(string(fix(meanVSL_B), format = '(F6.1)'), /remove_all)
         
         string_2 = 'Type C:'
         ;string_6 = string(meanVAP_C, format = '(F6.2)') + ' um/s'
         string_6 = string(meanVAP_C, format = '(F6.1)')
         string_10 = strcompress(string(fix(meanLIN_C), format = '(F6.1)'), /remove_all)
         string_10_1 = strcompress(string(fix(meanSTR_C), format = '(F6.1)'), /remove_all)
         string_10_2 = strcompress(string(fix(meanWOB_C), format = '(F6.1)'), /remove_all)
         string_10_3 = strcompress(string(fix(meanVCL_C), format = '(F6.1)'), /remove_all)
         string_10_4 = strcompress(string(fix(meanVSL_C), format = '(F6.1)'), /remove_all)
         
         string_3 = 'Type D:'  ; Type D  Talvez no deba incluirlo
         ;string_7 = string(meanVAP_D, format = '(F6.2)') + ' um/s'
         string_7 = string(meanVAP_D, format = '(F6.1)')
         string_11 = strcompress(string(fix(meanLIN_D), format = '(F6.1)'), /remove_all)
         string_11_1 = strcompress( string(fix(meanSTR_D), format = '(F6.1)'), /remove_all)
         string_11_2 = strcompress( string(fix(meanWOB_D), format = '(F6.1)'), /remove_all)
         string_11_3 = strcompress(string(fix(meanVCL_D), format = '(F6.1)'), /remove_all)
         string_11_4 = strcompress(string(fix(meanVSL_D), format = '(F6.1)'), /remove_all)
         
         string_21 = strcompress('VAP')
         string_23 = strcompress('VSL')
         string_27 = strcompress('VCL')
         string_28 = strcompress('LIN')
         string_29 = strcompress('STR')
         string_30 = strcompress('WOB')
         string_31 = ' um/s'
         string_32 = ' %'
         CEDAI_textPlotMotil, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         
         ; MOR - save results in the same directory as images - BEGIN
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         
         imageLeft =tempFileName + 'Motil_trajectory'+nom+'.tiff' ;Talvez esta cambie después ??
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_MovVariables.bmp'
         
         imageRight =  tempFileName + 'VCL_Histo'+nom+'.tiff' ;'histo_ALH.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo'+nom+'.tiff'
         textRight = tempFileName + 'Motil_MovVariables_Text'+nom+'.tiff'
         fileName = tempFileName + 'Motil_MovVariables'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png';Susana 
         endif; Susana
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, TextRight, filename,1
         
      endcase
      
      'Track Motil SCA':begin; IMPORTANTE: LOS RANGOS DE VELOCIDADES PARA CLASIFICACION SON INGRESADOS DIRECTAMENTE EN UM/S NO EN PIXELES COMO EN TRACK MOTIL, TRACK MOTIL TOTAL, TRACK MOTIL OMS Y TRACK MOTIL OMS TOTAL
         ;print, 'correctly entered track motility after adding the graphic model'
         ;veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010 EN USO CEDAI CALIBRACIÓN
         veloLimitsVCL = [0,10., 10.,15., 15.,1000., 35.,10000.] ;Rangos PChenlo en micrómetros
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
         
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
         ; do not include in analysis if started after the xx frame in time
         ; number of frames
         minTimeF = 5;  will be value in [frames]
         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]
         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
         minNumFramesTracked = floor((tEnd-tStart)*0.50);  value in [frames]
         ; how close to the border the object is 
         closeBorder = 9; [pixels] -- this value is the same as max_Dist in C_sROIParam_ObjTrackObject.pro    
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_z'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         
         ; write to file the track objects overallType of classification
         name = 'overallTypeTOs_'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U1
         openW, U1, file
         printF, U1, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'TypeNC'
         ; threshold area [in pixels] that the track covers
         areaThreshold = 3; this is 2x2 pixels large, Default = 2
         ; MOR - determine cutoffs for not including TOs in analysis - END
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]
         
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - INICIO
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - FIN
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange

         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
        
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
                                
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         
         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])
         name = 'CoordenadasTOs_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         get_lun, unit
         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
         ;;;;;;; MOR - temporarily printint the color code -----
         printF, Unit, 'zsliceA'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type'+STRING(9B)+'xBox_pix'+STRING(9B)+'yBox_pix'+STRING(9B)+'xBox_X_yBox_pix';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         vslMaxim = 0
         vslMaximPos = 0
         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
         ; --- Susana Creando vector para VSL --- INICIO
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         umbralVCL = 50 ;10
         ; MOR - 9Feb2011 - limits were found using pixels April 2010
         veloLimitsVSL = [2.5,4.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC
         ;veloLimitsVSL = [24.5,72.8]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC - Ratón
         ; --- Susana Creando vector para VSL --- FIN
         
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
         vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad A,B,C y D
         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         ; MOR - add vector for storing vsl values for the histogram - BEGIN
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum());
         ; MOR - add vector for storing vsl values for the histogram - END
         
         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
         
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
         
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - BEGIN
         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - END 
         
         ; MOR - 12Feb2011 - get concentration in time
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
         
         xBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBoxyBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         xBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBoxyBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana

         ; Susana - Optical Flow 1/2 -  BEGIN - 
           OFvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;OpticalFlow
           ;Sizevect = make_array((*state.poCurrTrackGroup)->getObjNum());Size
           DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Distancia
           DESPvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Desplazamiento
           DESP_DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); DESP/DIST
          
           s_OFParameter = 'Object Speed2 [pixelsPframe]'
           ;s_OFParameter = 'Object Vy [pixelsPframe]'
           strIDOF = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zpos) + '_'  + 'Clus' + string(clusPos) + '_', /rem)
            case selROIGroupObj of
             'oROI2DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Track Objects from Masks')
             'oROI3DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
             endcase
           ; sort objects in trackIDOF
           if (trackIDOF[0] ne -1) then begin
              whtrackIDOF = where(trackIDOF ne -1, count)
              if (count gt 0) then trackIDOF = trackIDOF[whtrackIDOF]
              sorttrackIDOF = fix(trackIDOF[uniq(trackIDOF, sort(trackIDOF))])
           endif

           case selROIGroupObj of
               'oROI2DGroup':trackParamsOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_OFParameter)
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase
           
           case selROIGroupObj of
               'oROI2DGroup':trackParamsTIME = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Object Time [s]')
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase           
           if (count gt 0) then begin
               trackParamsOF = trackParamsOF[whtrackIDOF]
               trackParamsTIME = trackParamsTIME[whtrackIDOF]
           endif
           
;           ; Size 1/2 - BEGIN - 
;           ;Size no me sirve, ya que es tamaño de cabecita brillante, no area de trayectoria
;           s_SizeParameter = 'Object Size [x²]';  'Object Size [x²]'  'Object Size [Pixel²]'   'Object Size [%]'
;           case selROIGroupObj of
;               'oROI2DGroup':trackParamsSize = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_SizeParameter)
;               ;'oROI3DGroup':trackParamsSize = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
;           endcase
;           if (count gt 0) then begin
;               trackParamsSize = trackParamsSize[whtrackIDOF]
;           endif
;           ; Size 1/2 - END -


         ; Susana - Optical Flow 1/2 -  END - 
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana
         
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
           vslTotal = vslVect[i] ;MOR changing calculation
           
           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
           
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
           vclVectTipo[i] = 'NC'; temporalmente aqui, borrar
           col = [0,0,0]; temporalmente aqui, borrar
                      ; Susana - Optical Flow 2/2 -  BEGIN - 
            if (sorttrackIDOF[0] ne -1) then begin
                whParamOF = where(trackIDOF eq sorttrackIDOF[i])
                if (whParamOF[0] eq -1) then begin
                    trackParamsOF_i = [min(trackParamsOF),max(trackParamsOF)]
                    ;trackParamsSize_i = [min(trackParamsSize),max(trackParamsSize)]; Susana - Size 2/2       ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
                    trackParamsTIME_i = [min(trackParamsTIME),max(trackParamsTIME)]
                endif else begin 
                    trackParamsOF_i = trackParamsOF[whParamOF]
                    ;trackParamsSize_i = trackParamsSize[whParamOF]; Susana - Size 2/2      ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
                    trackParamsTIME_i = trackParamsTIME[whParamOF]
                endelse
            endif
            OFvect[i] = mean(trackParamsOF_i)
            ;Sizevect[i] = trackParamsSize_i    ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
            trackParamsTIME_i_delta = 0
            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
              trackParamsTIME_i_delta = trackParamsTIME_i[1:N_ELEMENTS(trackParamsTIME_i)-1] - trackParamsTIME_i[0:N_ELEMENTS(trackParamsTIME_i)-2]
            endif
            ;endif else begin 
            ;endelse
            
           ; Susana - Optical Flow 2/2 -  END - 
           
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
           ; check at what time/frame the object started being tracked
           firstAppeared = (*vclStruct.pTValues)[0]
           
           ; check how many frames the object was tracked
           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0]);
           
           ; check that all elements of the TO are not 'too close' to the border
           ;2D case --- @todo - need to figure out the 3D case
           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
           countBorder = 0
           if(firstAppeared gt 0) then begin
               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
               xO = (*vclStruct.pXYZValues)[0,0]/xReal
               yO = (*vclStruct.pXYZValues)[0,1]/yReal
               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
           endif
           ; create a bounding box of the track path -- convert trajectory to pixels
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
            
            ;xBox = 10000
            xBox = 0 ; Susana
            ;yBox = 10000
            yBox = 0 ;Susana
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
               ; find bounding center of mass values in each dimension
               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
               ; represent in whole pixels rather than subpixel positions
               xMin = floor(xMin)
               xMax = ceil(xMax)
               yMin = floor(yMin)
               yMax = ceil(yMax)
               ; check in each dimension what the number of pixels are
               xBox = xMax - xMin 
               yBox = yMax - yMin
            endif
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
           
           xBox_vect[i] = xBox ; Susana - evaluando xBox e yBox
           yBox_vect[i] = yBox ; Susana - evaluando xBox e yBox
           yBoxyBox_vect[i] = xBox_vect[i]*yBox_vect[i]
           
           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
           ;ALH , creo que no lo informo aùn no
           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)

           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
           
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
               if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
                  ;VCLvect[i] = 0
                  ;vslVect[i] = 0  
                  ;VAPvect[i] = 0
                  ;vslTotal = 0
               endif else begin
                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
               endelse
             
;           ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- INICIO
            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
             ;desp=sqrt( ( (vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) )^2 ); en micrómetros, usando VSL
             ;DESPvect[i] = sqrt( ((((((*vclStruct.pXYZValues)[*,0])[(n_elements((*vclStruct.pXYZValues)[*,0]))-1])-(((*vclStruct.pXYZValues)[*,0])[0])))^2) + ((((((*vclStruct.pXYZValues)[*,1])[(n_elements((*vclStruct.pXYZValues)[*,1]))-1])-(((*vclStruct.pXYZValues)[*,1])[0])))^2) )
             DESPvect[i] = ((vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) ); en micrómetros, usando VSL    vslTotal*total(vclStruct.dT)
             
             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta))/xReal   ;en pixeles
             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta)); en micrómetros
             vDist = (vclStruct.XYZDisplacement); en micrómetros  ;vtotal = vclStruct.XYZDisplacement/vclStruct.dT
             DISTvect[i] = total(vDist); en pixeles
             ;desp_dist = desp/dist ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
             DESP_DISTvect[i] = DESPvect[i]/DISTvect[i] ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
           endif
;          ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- FIN

             ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - INICIO
             ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - INICIO
             ;if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 17)) then begin ;en uso 
             ;if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 3)) then begin
             if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 20)) then begin ; PChenlo
                  ;No motiles
                  ;VCLvect[i] = 0
                  ;vslVect[i] = 0  
                  ;VAPvect[i] = 0
                  ;vslTotal = 0
               endif
           ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - FIN
           ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - FIN
             
             
             ;SUSANA - LIN - INICIO
             ;LIN
             ; MOR @todo - need to update the functions calling LIN, WOB, STR in C_sTrackGroupObject function in order to have
             ; the correct access to these calculations 
             
             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
             if (VCLvect[i] ne 0) then begin
               ;LINvect[i] = vslVect[i]/VCLvect[i]
               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
               ;WOBvect[i] = VAPvect[i]/VCLvect[i]
               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               LINvect[i] = 0
               WOBvect[i] = 0
             endelse
             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
             ; MOR STR calculation - BEGIN
             if (VAPvect[i] ne 0) then begin
               ;STRvect[i] = vslVect[i]/VAPvect[i]
               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               STRvect[i] = 0
            endelse
             ; MOR STR calculation - END
             
             ; MOR - create a vector to store the reason why something was classified as 'NC' - BEGIN
;             overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
             ; legend 
             ; classified for analysis - 0
             ; less than 2 data points - 1
             ; appeared late in time - 2
             ; tracked less than 50% of total time sample - 3
             ; too near the border at time of first track, if it is after the 1st frame - 4
             ; MOR - create a vector to store the reason why something was classified as 'NC' - END
           ;@TODO Susana and/or Victor: spermatozoa should be categorized as   for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
;            print,'D VCL <'+STRING(veloLimitsVCL[1])+' y/o '+' VSL <='+STRING(veloLimitsVSL[0])
;            print,'C VCL >='+STRING(veloLimitsVCL[2])+' y <'+STRING(veloLimitsVCL[3])+'   y/o   VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'C VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'B VCL >='+STRING(veloLimitsVCL[4])+' y <'+STRING(veloLimitsVCL[5])
;            print,'A VCL >='+STRING(veloLimitsVCL[6])
;            print,'-- velocidades --'
             case 1 of
             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
            ; MOR - 6Sept2010 - previous check for 'NC' classification
            ;((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) ): begin
               ; MOR - store reason for 'NC' - BEGIN
               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
                     overallType[i] = 1
                     col = [0,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1
                     break 
               endif
               
               if ((countBorder gt 0)) then begin
                     overallType[i] = 4
                     col = [0,255,120]
                     thickk = 2
                     ;vclVectTipo[i] = 'NCpp' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     cuentaTOsNC += 1
                     break
               endif
               
               if( (firstAppeared ge minTime)) then begin 
                     overallType[i] = 2
                     col = [0,120,255]
                     thickk = 2;
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               if ((timesTracked le minNumFramesTracked)) then begin 
                     overallType[i] = 3
                     col = [120,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               ; MOR - store reason for 'NC' - END
        
             end
;            ( (OFvect[i] le 1.5)  and (OFvect[i] ge 0.12) ): begin
;              ; con flujo laminar ? podria usar todos desde cero hasta 1.5 de speed2
;              col = [0,120,120]
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'D' ;Susana
;              end
             ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - INICIO
            (vslTotal le veloLimitsVSL[0]): begin
              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
              ;col = [255,255,255]
              thickk = 4
              vclVectTipo[i] = 'D' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado
              end
            ((vslTotal gt veloLimitsVSL[0]) and (vslTotal le veloLimitsVSL[1])): begin
              ;para ¿TipoB?  que deben ser TipoC
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              ;col = [255,255,255]
              thickk = 8;default=4
              vclVectTipo[i] = 'C' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado 
              end
              ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - FIN
            (VCLvect[i] lt veloLimitsVCL[1]): begin
              col = [0,0,255] ;inmoviles azules "Tipo D"
              ;thickk = 5;4 ;default
              thickk = 2;4 ;default
              vclVectTipo[i] = 'D' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[2]) and (VCLvect[i] lt veloLimitsVCL[3]): begin
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'C' ;Susana
              end
            ;(VCLvect[i] ge veloLimitsVCL[6]): begin  ;cedai
            ((VCLvect[i] ge veloLimitsVCL[6]) and (LINvect[i] ge 50) and (STRvect[i] ge 80)): begin  ;pchenlo
            col = [255,0,0] ;Progresivos rapidos rojos "Tipo A"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'A' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[4]): begin  ;Solo PChenlo
              col = [255,255,0] ;Progresivos lentos amarillos "Tipo B"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'B' ;Susana
              end
           endcase
           
           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'D') ) then begin  
            ;and (STRvect[i] lt 50) 
            cuentaCirc += 1
            thickk = 2
           endif
           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
           
           if (vslTotal gt vslMaxim) then begin 
            vslMaxim = vslTotal
            vslMaximPos = i
           endif
           
           
           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
             np = (*state.poCurrTrackGroup)->getObjNum()
             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
             miX = (*VAPStruct.pXYZValues)[*,0]
             miY = (*VAPStruct.pXYZValues)[*,1]
             miZ = (*VAPStruct.pXYZValues)[*,2]
             miT = (*VAPStruct.pTValues)
             np = n_elements(miX)
             for j = 0, (np-1) do begin
;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
              if(vclVectTipo[i] ne 'NC') then begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL)+STRING(9B)+strcompress(xBox,/REMOVE_ALL)+STRING(9B)+strcompress(yBox,/REMOVE_ALL)+STRING(9B)+strcompress(xBox*yBox,/REMOVE_ALL);+STRING(10B) ;para IGOR   con xBox e yBox
               printF, Unit,temp
              endif
             endfor
;            ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - BEGIN
;             if(vclVectTipo[i] ne 'NC') then begin ;guardo coordenadas de los clasificados
;             for j = 0, (np-1) do begin
;                temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem);+STRING(10B) ;para IGOR
;               printF, Unit,temp
;              endfor
;             endif
;             ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - END           
           
           ; MOR - get entire time vector
           timeTrack[i] = n_elements(*VAPStruct.ptValues)
           
           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
           if  (vclVectTipo[i] ne 'NC') then begin
           ; MOR - 05Jul2010
           ; convert track xyz to pixels for plotting against image
              if(vclVectTipo[i] eq 'D') then begin
                ;;oSym = obj_new('IDLgrSymbol', data = 3, size = 2.5, thick = thickk);original data = 3=>Dot
                oSym = obj_new('IDLgrSymbol', data = 3, size = 5.5, thick = thickk); Dot de mayor tamaño
              endif else begin
                ;oSym = obj_new('IDLgrSymbol', data = 2, thick = thickk);data = 2=>asterisco   thick=>line thickness
                oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);data = 0 =>no symbol   thick=>line thickness
              endelse
             
              ;if(vclVectTipo[i] eq 'C') then begin
              oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oModel->add, oPlot
              ;endif
              ; Susana - Incluyo LIN temporalmente  INICIO
;              if((vclVectTipo[i] eq 'B')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;              if((vclVectTipo[i] eq 'C')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;             ; Susana - Incluyo LIN temporalmente  INICIO
              
              ; Susana - Write OpticalFlowParameter in image - BEGIN 
              ;;v_OFmean = mean(trackParamsOF_i)
              ;;s_TagOF = strcompress("OF"+"_"+string(v_OFmean, format = '(F6.2)'),/rem)
              ;s_TagOF = strcompress("OF"+"_"+string(OFvect[i], format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VCL"+"_"+string(VCLvect[i], format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VSL"+"_"+string(vslTotal, format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VAP"+"_"+string(VAPvect[i], format = '(F6.2)'),/rem)
              
              ;;;if((vclVectTipo[i] ne 'A') and (vclVectTipo[i] ne 'D')) then begin
              ;;if(OFvect[i] le 1.5) then begin  ;if((OFvect[i] le 1.5)  and (OFvect[i] ge 0.12)) then begin
                ;oPlot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oPlot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [15, 15], locations = [((*vclStruct.pXYZValues)[0,0]+5)/xReal,((*vclStruct.pXYZValues)[0,1]+1)/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
              ;;endif
              ; Susana - Write OpticalFlowParameter in image - END 
              
              
             ; Susana - add TO number - INICIO
             ;;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [15, 15], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
             ;oModel->add, oPlot
             ; Susana - add TO number - FIN
           endif
           
           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
           ;name = 'CoordsTOs_'
           name = 'CoordsTOs_'+nom
           file = strCompress(tempFileName + name + '.dat', /remove)
           
           get_lun, U
            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            printF, U, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
           
           ; MOR - write to file the track objects overallType of classification - BEGIN    
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           printF, U1,temp 
           ; MOR - write to file the track objects overallType of classification - END
           
         endfor
         print, countTONC_pocosPuntos ;Susana
         print, countTONC_firstAppeared
         print, countTONC_timesTracked
         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         ; MOR - close up file with overalltype classification - BEGIN
         close, U1
         FREE_LUN, U1
         ; MOR - close up file with overalltype classification - END         
         

         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ; MOR - 9Feb 2011 - change so the last directory name prints on Motil.tiff file
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         name = 'VelocityData_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceC'+STRING(9B)+'TOVeloc'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(9B)+'xBoxVeloc_pix'+STRING(9B)+'yBoxVeloc_pix'+STRING(9B)+'xBox_X_yBoxVeloc_pix'+STRING(9B)+'desp'+STRING(9B)+'dist'+STRING(9B)+'desp_dist'+STRING(9B)+s_OFParameter+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR  ; Susana  - con xBox e yBox sin SIZE
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]*yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESP_DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(OFvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR   Susana- con xBox e yBox sin Size
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         
         
         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
         name = 'Motil_MovVars_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceB'+STRING(9B)+'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'TypeMVars'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
         
           
         ;Susana INICIO --  Guardando #TrackObjects Reasignados
         name = 'NumTrackObjectsVSL_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit,transpose(vslReasign)
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN --  Guardando #TrackObjects Reasignados
                 
         
         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
         np = (*state.poCurrTrackGroup)->getObjNum()
         meanVAP_A = 0; promedio tipos A
         meanVAP_B = 0; promedio tipos B
         meanVAP_C = 0; promedio tipos C
         meanVAP_D = 0; promedio tipos D
         meanVAP_NC = 0; promedio tipos NC
         
         meanVCL_A = 0; promedio tipos A
         meanVCL_B = 0; promedio tipos B
         meanVCL_C = 0; promedio tipos C
         meanVCL_D = 0; promedio tipos D
         meanVCL_NC = 0; promedio tipos NC
         
         meanVSL_A = 0; promedio tipos A
         meanVSL_B = 0; promedio tipos B
         meanVSL_C = 0; promedio tipos C
         meanVSL_D = 0; promedio tipos D
         meanVSL_NC = 0; promedio tipos NC
         
         meanLIN_A = 0; promedio LIN tipos A
         meanLIN_B = 0; promedio LIN tipos B
         meanLIN_C = 0; promedio LIN tipos C
         meanLIN_D = 0; promedio LIN tipos D
         meanLIN_NC = 0; promedio LIN tipos NC
         
         meanSTR_A = 0; promedio STR tipos A
         meanSTR_B = 0; promedio STR tipos B
         meanSTR_C = 0; promedio STR tipos C
         meanSTR_D = 0; promedio STR tipos D
         meanSTR_NC = 0; promedio STR tipos NC
         
         meanWOB_A = 0; promedio WOB tipos A
         meanWOB_B = 0; promedio WOB tipos B
         meanWOB_C = 0; promedio WOB tipos C
         meanWOB_D = 0; promedio WOB tipos D
         meanWOB_NC = 0; promedio WOB tipos NC
         
         meanVAP_Acnt = 0; counter para tipos A
         meanVAP_Bcnt = 0; counter para tipos B
         meanVAP_Ccnt = 0; counter para tipos C
         meanVAP_Dcnt = 0; counter para tipos D
         meanVAP_NCcnt = 0; counter para tipos NC
         ;desde aqui descomenté -Susana
         for h = 0, (np-1) do begin
          case 1 of
            (vclVectTipo[h] eq 'A'): begin
              meanVAP_A += VAPvect[h]
              meanVCL_A += VCLvect[h]
              meanVSL_A += vslVect[h]
              meanLIN_A +=  LINvect[h]
              meanSTR_A +=  STRvect[h]
              meanWOB_A +=  WOBvect[h]
              ;meanALH_A += ALHvect[h]
              meanVAP_Acnt += 1
              end
            (vclVectTipo[h] eq 'B'): begin
              meanVAP_B += VAPvect[h]
              meanVCL_B += VCLvect[h]
              meanVSL_B += vslVect[h]
              meanLIN_B += LINvect[h]
              meanSTR_B +=  STRvect[h]
              meanWOB_B +=  WOBvect[h]
              ;meanALH_B += ALHvect[h]
              meanVAP_Bcnt += 1
              end
            (vclVectTipo[h] eq 'C'): begin
              meanVAP_C += VAPvect[h]
              meanVCL_C += VCLvect[h]
              meanVSL_C += vslVect[h]
              meanLIN_C += LINvect[h]
              meanSTR_C +=  STRvect[h]
              meanWOB_C +=  WOBvect[h]
              ;meanALH_C += ALHvect[h]
              meanVAP_Ccnt += 1
              end
            (vclVectTipo[h] eq 'D'): begin
              meanVAP_D += VAPvect[h]
              meanVCL_D += VCLvect[h]
              meanVSL_D += vslVect[h]
              meanLIN_D += LINvect[h]
              meanSTR_D +=  STRvect[h]
              meanWOB_D +=  WOBvect[h]
              ;meanALH_D += ALHvect[h]
              meanVAP_Dcnt += 1
              end
            (vclVectTipo[h] eq 'NC'): begin
              meanVAP_NC += VAPvect[h]
              meanVCL_NC += VCLvect[h]
              meanVSL_NC += vslVect[h]
              meanLIN_NC += LINvect[h]
              meanSTR_NC +=  STRvect[h]
              meanWOB_NC +=  WOBvect[h]
              ;meanALH_NC += ALHvect[h]
              meanVAP_NCcnt += 1
              end
           endcase
         endfor
         ;hasta aqui descomenté--Susana
         meanVAP_A /= meanVAP_Acnt
         meanVAP_B /= meanVAP_Bcnt
         meanVAP_C /= meanVAP_Ccnt
         meanVAP_D /= meanVAP_Dcnt
         meanVAP_NC /= meanVAP_NCcnt
         
         meanVCL_A /= meanVAP_Acnt
         meanVCL_B /= meanVAP_Bcnt
         meanVCL_C /= meanVAP_Ccnt
         meanVCL_D /= meanVAP_Dcnt
         meanVCL_NC /= meanVAP_NCcnt
         
         meanVSL_A /= meanVAP_Acnt
         meanVSL_B /= meanVAP_Bcnt
         meanVSL_C /= meanVAP_Ccnt
         meanVSL_D /= meanVAP_Dcnt
         meanVSL_NC /= meanVAP_NCcnt
         
         meanLIN_A /= meanVAP_Acnt
         meanLIN_B /= meanVAP_Bcnt
         meanLIN_C /= meanVAP_Ccnt
         meanLIN_D /= meanVAP_Dcnt
         meanLIN_NC /= meanVAP_NCcnt
         
         meanSTR_A /= meanVAP_Acnt
         meanSTR_B /= meanVAP_Bcnt
         meanSTR_C /= meanVAP_Ccnt
         meanSTR_D /= meanVAP_Dcnt
         meanSTR_NC /= meanVAP_NCcnt
         
         meanWOB_A /= meanVAP_Acnt
         meanWOB_B /= meanVAP_Bcnt
         meanWOB_C /= meanVAP_Ccnt
         meanWOB_D /= meanVAP_Dcnt
         meanWOB_NC /= meanVAP_NCcnt
         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
         
         
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         
         ; MOR - comment out old path for saving, try path where images are saved instead 
         write_tiff, tempFileName + 'Motil_trajectory'+nom+'.tiff', outImage_1
         
         ; MOR - destroy objects created for plotting to free up memory
         obj_destroy, [oBuffer, oView]
         
         ; MOR - add creation of VSL histogram - BEGIN
         path = tempFileName + 'VSL_Histo'+nom+'.tiff'
         
         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 ;Aqui reemplazo ne por eq, es decir envio al histograma todos los trackobjects menos los reasignados
         if(whClassified[0] ne -1) then begin; Susana1
          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
;         vect = vslVect ; MOR - commented out previous code
          name = "rectilinear velocity VSL [um s-1]"
          ;name = "rectilinear velocity VSL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          
          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
          CEDAI_histogramPlot_VSL, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName
          ; MOR - add creation of VSL histogram - END
          ;CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVSL_OK = 1
         endif; Susana1
         ;endif else begin; Susana1
            ;
         ;endelse; Susana1
         
         
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;path = s_getPathForSystem() + 'VCL_Histo.tiff'
         ;path = tempFileName + 'VCL_Histo.tiff'
         path = tempFileName + 'VCL_Histo'+nom+'.tiff'
         
         v_histogVCL_OK = 0 ; Susana - controlar si existe histograma VCL de clasificados no reasignados
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
         if(whClassified[0] ne -1) then begin; Susana2
          ; Susana - A B C o D, no reasignado
          vect = VCLvect[whClassified]
          name = "curvilinear velocity VCL [um s-1]"
          ;name = "curvilinear velocity VCL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          linesFlag = 1
          CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVCL_OK = 1
         endif; Susana2
         ;endif else begin; Susana2
            ;nada
            ;El problema de no haber alternativa es que no crea imágenes como VCL_Histo.tiff
         ;endelse; Susana2
         
         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
         
         ;Susana INICIO    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom2 = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
         nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
         ;name = 'DatosPChenlo_2011'
         name = 'Datos_'+nom2
         
         ;Susana - reemplazo tempFileName INICIO
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         ;Susana - reemplazo tempFileName FIN
         
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         get_lun, unitPCh
         OPENR, unitPCh, file, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitPCh, file
           ;printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         
         ;openW, unitPCh, file
         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
         ;recuentos
         aux = where(vclVectTipo eq 'A', tipoA)
         aux = where(vclVectTipo eq 'B', tipoB)
         aux = where(vclVectTipo eq 'C', tipoC)
         aux = where(vclVectTipo eq 'D', tipoD)
         aux = where(vclVectTipo eq 'NC', tipoNC)
         ;porcentajes
         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
         ;print,tipoA+tipoB+tipoC+tipoD  corresponde a vect_elements
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         ;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana FIN    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         
         
         ;dummy = where(vect ge veloLimitsVCL[6], count)
         dummy = where(vclVectTipo eq 'A', count) ;Susana
         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_0 = 'Type A:'
         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[4] and vect lt veloLimitsVCL[5], count)   ;Susana
         dummy = where(vclVectTipo eq 'B', count) ;Susana
         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_1 = 'Type B:' 
         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[2] and vect lt veloLimitsVCL[3], count)   ;Susana
         dummy = where(vclVectTipo eq 'C', count) ;Susana
         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_2 = 'Type C:'
         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[0] and vect lt veloLimitsVCL[1], count)   ;Susana
         dummy = where(vclVectTipo eq 'D', count) ;Susana
         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_3 = 'Type D:'
         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Motil_Text'+nom+'.tiff'
         
         
         ;SUSANA - Contabilizando trackobjects NC (No Clasificados) 
         dummy = where(vclVectTipo eq 'NC', count) ;Susana
         ;string_21 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_21 = strcompress( string(fix(count)), /remove_all) + ' sperms'
         ;string_23 = 'NC       :'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - INICIO
         string_23 =strcompress('#Traj.Circ.:' + string(fix(cuentaCirc)), /remove_all) + '(LIN<50) '+ '#NC:'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - FIN
         
         ;filename2 = nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])  ;Susana incluye nombre de muestra en informe para evitar error al ver informe, borrar 
         ;string_27 = string(100.*count / vect_elements, format = '(F6.2)') + ' %' ;NC no estan incluidos en el total de espermatozoides
         CEDAI_textPlot, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11,string_21,string_23, nom
         
         ; MOR - 12Feb 2011- average number of objects found per time
         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
         totalNumObjs.avg = stats[0]*conFactor
         totalNumObjs.std = sqrt(stats[1]*conFactor)
         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
         totalNumObjs.se = sqrt(totalNumObjs.avg*conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         
         string_0 = 'Concentration Statistics Summary'
;         string_1 = strcompress('Frames ') 
         string_2 = strcompress('Mean: ') 
;         string_3 = strcompress('SDev ') 
         string_4 = strcompress('Standard Error: ') 
;         string_5 = strcompress('Median ') 
;         string_6 = strcompress('Min ') 
;         string_7 = strcompress('Max ') 
         
         string_8 = strcompress(string(totalNumObjs.number, format = '(D6.2)'), /rem) 
         string_9 = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem) 
         string_10 = strcompress(string(totalNumObjs.std, format = '(I6.2)'), /rem) 
         string_11 = strcompress(string(totalNumObjs.se, format = '(D6.2)'), /rem) 
         string_12 = strcompress(string(totalNumObjs.med, format = '(I6.2)'), /rem) 
         string_13 = strcompress(string(totalNumObjs.mini, format = '(I6.2)'), /rem) 
         string_14 = strcompress(string(totalNumObjs.maxi, format = '(I6.2)'), /rem) 
         
         string_15 = strcompress('[10E6/mL]', /rem) 
       
         ; MOR - comment out all previously defined save paths
;         fileName = tempFileName + 'Concentration_Text.tiff'     
         
;         CEDAI_textPlot_Count, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7,string_8, string_9, string_10, string_11, string_12, string_13, string_14
         fileName = tempFileName + 'Concentration_Text_Short'+nom+'.tiff'
         
         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
         
         ; MOR - 12Feb2011 write total count data to .dat file 
         dum = max(timeTrack, maxI)
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
         
         name = 'Concentration'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit, 'zslice'+STRING(9B)+'Time' +string(9B)+ 'Count'+STRING(13B)
         indU = uniq(*numObjs.pTValues)       
         for i = 0, n_elements(indU)-1 do begin
            temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            printF, Unit,temp
         endfor
         close, Unit
         FREE_LUN, Unit
         
         ; MOR - save results in the same directory as images - BEGIN
        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_Short.bmp'
;         imageLeft =tempFileName + 'Motil_trajectory.tiff'
;         imageRight =  tempFileName + 'VCL_Histo.tiff'
;         imageRight2 =  tempFileName + 'VSL_Histo.tiff'
;         textRight = tempFileName + 'Motil_Text.tiff'
;         fileName = tempFileName + 'Motil_Short.tiff' 
         imageLeft =tempFileName + 'Motil_trajectory'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_Histo'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo'+nom+'.tiff'
         textRight = tempFileName + 'Motil_Text'+nom+'.tiff'
         fileName = tempFileName + 'Motil_Short'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Concentration_Text_Short'+nom+'.tiff' 
         
         
         CEDAI_combineImages_Motil_Short, background, imageLeft, TextRight, fileName, textLeft
         
        ; MOR - save results in the same directory as images - BEGIN
        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility.bmp'
         
;         imageLeft =tempFileName + 'Motil_trajectory.tiff'
         imageLeft =tempFileName + 'Motil_trajectory'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_Histo'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo'+nom+'.tiff'
         textRight = tempFileName + 'Motil_Text'+nom+'.tiff'
         fileName = tempFileName + 'Motil_new'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana 
         endif; Susana 
         
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         
         
         CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft
         ;CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft, v_histogVSL_OK    ; Susana evalúa si existe histograma
         
         ;SUSANA - Incluyo VAP promedio por grupos LO ESTOY INTENTANDO, AUN NO CREO EL HISTOGRAMA PENDIENTES TAMBIEN HISTOGRAMAS STR Y WOB
         ;SUSANA - add creation of ALH histogram - BEGIN
         veloLimitsVCL = [0,50., 50.,200., 200.,400., 400.,10000.] ; no tengo rangos para ALH :( 
         path = tempFileName + 'histo_ALH'+nom+'.tiff'
         
         vect = ALHvect
         name = "ALH Amplitude of Lateral Head displacement [um]"+nom
         
         linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
         ;CEDAI_histogramPlot_ALH, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
         ;Falta generar un graficador de histograma para ALH o llamar internamente el GraphicModel ALH
         ;SUSANA - add creation of VSL histogram - END
         
         
         fileName = tempFileName + 'Motil_MovVariables_Text'+nom+'.tiff' ; MovementsVariables
         
         string_0 = 'Type A:'
         ;string_4 = string(meanVAP_A, format = '(F6.2)') + ' um/s'
         string_4 = string(meanVAP_A, format = '(F6.1)')
         string_8 = strcompress(string(fix(meanLIN_A), format = '(F6.1)'), /remove_all)
         string_8_1 = strcompress(string(fix(meanSTR_A), format = '(F6.1)'), /remove_all)
         string_8_2 = strcompress(string(fix(meanWOB_A), format = '(F6.1)'), /remove_all)
         string_8_3 = strcompress(string(fix(meanVCL_A), format = '(F6.1)'), /remove_all)
         string_8_4 = strcompress(string(fix(meanVSL_A), format = '(F6.1)'), /remove_all)
         
         string_1 = 'Type B:' 
         ;string_5 = string(meanVAP_B, format = '(F6.2)') + ' um/s'
         string_5 = string(meanVAP_B, format = '(F6.1)')
         string_9 = strcompress(string(fix(meanLIN_B), format = '(F6.1)'), /remove_all)
         string_9_1 = strcompress(string(fix(meanSTR_B), format = '(F6.1)'), /remove_all)
         string_9_2 = strcompress(string(fix(meanWOB_B), format = '(F6.1)'), /remove_all)
         string_9_3 = strcompress(string(fix(meanVCL_B), format = '(F6.1)'), /remove_all)
         string_9_4 = strcompress(string(fix(meanVSL_B), format = '(F6.1)'), /remove_all)
         
         string_2 = 'Type C:'
         ;string_6 = string(meanVAP_C, format = '(F6.2)') + ' um/s'
         string_6 = string(meanVAP_C, format = '(F6.1)')
         string_10 = strcompress(string(fix(meanLIN_C), format = '(F6.1)'), /remove_all)
         string_10_1 = strcompress(string(fix(meanSTR_C), format = '(F6.1)'), /remove_all)
         string_10_2 = strcompress(string(fix(meanWOB_C), format = '(F6.1)'), /remove_all)
         string_10_3 = strcompress(string(fix(meanVCL_C), format = '(F6.1)'), /remove_all)
         string_10_4 = strcompress(string(fix(meanVSL_C), format = '(F6.1)'), /remove_all)
         
         string_3 = 'Type D:'  ; Type D  Talvez no deba incluirlo
         ;string_7 = string(meanVAP_D, format = '(F6.2)') + ' um/s'
         string_7 = string(meanVAP_D, format = '(F6.1)')
         string_11 = strcompress(string(fix(meanLIN_D), format = '(F6.1)'), /remove_all)
         string_11_1 = strcompress( string(fix(meanSTR_D), format = '(F6.1)'), /remove_all)
         string_11_2 = strcompress( string(fix(meanWOB_D), format = '(F6.1)'), /remove_all)
         string_11_3 = strcompress(string(fix(meanVCL_D), format = '(F6.1)'), /remove_all)
         string_11_4 = strcompress(string(fix(meanVSL_D), format = '(F6.1)'), /remove_all)
         
         string_21 = strcompress('VAP')
         string_23 = strcompress('VSL')
         string_27 = strcompress('VCL')
         string_28 = strcompress('LIN')
         string_29 = strcompress('STR')
         string_30 = strcompress('WOB')
         string_31 = ' um/s'
         string_32 = ' %'
         CEDAI_textPlotMotil, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         
         ; MOR - save results in the same directory as images - BEGIN
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         
         imageLeft =tempFileName + 'Motil_trajectory'+nom+'.tiff' ;Talvez esta cambie después ??
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_MovVariables.bmp'
         
         imageRight =  tempFileName + 'VCL_Histo'+nom+'.tiff' ;'histo_ALH.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo'+nom+'.tiff'
         textRight = tempFileName + 'Motil_MovVariables_Text'+nom+'.tiff'
         fileName = tempFileName + 'Motil_MovVariables'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png';Susana 
         endif; Susana
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, TextRight, filename,1
         
      endcase

;      'Track Motil SCA':begin
;         ;print, 'correctly entered track motility after adding the graphic model'
;         ;veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010 EN USO CEDAI CALIBRACIÓN
;         ;veloLimitsVCL = [0,10., 10.,15., 15.,35., 35.,10000.] ;Rangos CASA Microptics default
;         veloLimitsVCL = [0,10., 10.,15., 15.,19., 19.,10000.] ;Rangos CASA Microptics - modificado Susana
;         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
;         
;         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
;         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
;         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
;         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
;         
;         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
;         ; do not include in analysis if started after the xx frame in time
;         ; number of frames
;         minTimeF = 5;  will be value in [frames]
;         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]  Susana - frame en que aparece TO
;         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
;         minNumFramesTracked = floor((tEnd-tStart)*0.50);  value in [frames]
;         ; how close to the border the object is 
;         closeBorder = 9; [pixels] -- this value is the same as max_Dist in C_sROIParam_ObjTrackObject.pro    
;         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
;         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
;         
;         ; write to file the track objects overallType of classification
;         ;name = 'overallTypeTOs_'
;         name = 'overallTypeTOsSCA_'+nom
;         
;         file = strCompress(tempFileName + name + '.dat', /remove)  
;         get_lun, U1
;         openW, U1, file
;         ;printF, U1, 'TO'+STRING(9B)+'TypeNC'
;         printF, U1, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'TypeNC'
;         ; threshold area [in pixels] that the track covers
;         areaThreshold = 3; this is 2x2 pixels large, Default = 2
;         ; MOR - determine cutoffs for not including TOs in analysis - END
;         
;         image_size = size(image,/dim)
;         image_x = image_size[0]
;         image_y = image_size[1]
;         yr = [0, max(image_y)]
;         xr = [0, max(image_x)]
;         k = 1
;         y = .45
;         xs = [k+45,y*2]
;         ys = [k+45,y*2]
;         
;         oFont = obj_new('IDLgrFont','helvetica',size=14)
;         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
;         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
;         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
;         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
;         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
;         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
;         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
;         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)
;
;         ; MOR - modified LB code for generating scale bar below the trajectory image
;         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
;;         sb = 50 ;Susana - sb in pixels
;;         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
;;         pixsSB  = floor(1.0*sb*xReal)
;;         boxS = bytarr(3, pixsSB, 5)
;;         boxS[0,*,*] = 0
;;         boxS[1, *,*] = 0
;;         boxS[2,*,*] = 0
;         ;Susana - sb in micrometers - INICIO
;         sb = 50 ; sb in micrometers 
;         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
;         pixsSB  = floor(1.0*(sb/xReal))
;         boxS = bytarr(3, pixsSB, 5)
;         boxS[0,*,*] = 0
;         boxS[1, *,*] = 0
;         boxS[2,*,*] = 0
;         ;Susana - sb in micrometers - FIN
;         
;         position = [0.1, 0.1, 0.95, 0.95]
;         oPlot->GetProperty, XRange=xrange
;         oPlot->GetProperty, YRange=yrange
;
;         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
;         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
;        
;         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
;         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
;         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
;         
;         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
;                                
;         oModel = obj_new('IDLgrModel')
;         oModel->remove, /all
;         oModel->add, oImage
;         oModel->add, oPlot
;         oModel->add, oXAxis
;         oModel->add, oYAxis
;         oModel->add, oYAxisUp
;         oModel->add, oXAxisUp
;         oModel->add, oImage_box
;         oModel->add, oText_box
;         ;oModel->add, oFont_box
;         
;         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
;         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
;         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])
;         name = 'CoordenadasTOsSCA_'+nom ;incluyo nombre ultima carpeta en nombre archivo
;         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
;         get_lun, unit
;         openW, Unit, file
;;         np = (*state.poCurrTrackGroup)->getObjNum()
;         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
;         ;;;;;;; MOR - temporarily printint the color code -----
;         ;printF, Unit, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
;         printF, Unit, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type'+STRING(9B)+'xBox_pix'+STRING(9B)+'yBox_pix'+STRING(9B)+'xBox_X_yBox_pix';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
;         
;         
;         
;         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
;         
;         vslMaxim = 0
;         vslMaximPos = 0
;         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
;         ; --- Susana Creando vector para VSL --- INICIO
;         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
;         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
;         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
;         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
;         umbralVCL = 50 ;10
;         ; MOR - 9Feb2011 - limits were found using pixels April 2010
;         veloLimitsVSL = [2.5,4.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC
;         ;veloLimitsVSL = [24.5,72.8]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC - Ratón
;         ; --- Susana Creando vector para VSL --- FIN
;         
;         STRlim = 80 ; original SCA
;         ;STRlim = 79 ; susana
;         LINlim = 50 ; original SCA
;         ;LINlim = 42 ; susana 38
;         
;         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
;         np = (*state.poCurrTrackGroup)->getObjNum()
;         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
;         vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
;         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad A,B,C y D
;         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
;         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
;         
;         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
;         ; MOR - add vector for storing vsl values for the histogram - BEGIN
;         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum());
;         ; MOR - add vector for storing vsl values for the histogram - END
;         
;         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
;         
;         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
;         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
;         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
;         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
;         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
;         
;         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
;         ; the track and use this for classification of type of sperm - BEGIN
;         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
;         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
;         ; the track and use this for classification of type of sperm - END 
;         
;         ; MOR - 12Feb2011 - get concentration in time
;         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
;         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
;         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
;         
;         xBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
;         yBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
;         xBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
;         yBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
;         
;         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
;           ; Susana evaluando VSL -- INICIO
;           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
;           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
;           vslTotal = vslVect[i] ;MOR changing calculation
;           
;           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
;           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
;           ; Susana evaluando VSL -- FIN
;           
;           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
;           
;           ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- INICIO
;           y0=(*vclStruct.pXYZValues)[0,1]/yReal; en pixeles
;           yf=(*vclStruct.pXYZValues)[n_elements((*vclStruct.pXYZValues)[*,1])-1,1]/xReal; en pixeles
;           x0=(*vclStruct.pXYZValues)[0,0]/xReal; en pixeles
;           xf=(*vclStruct.pXYZValues)[n_elements((*vclStruct.pXYZValues)[*,0])-1,0]/xReal; en pixeles
;           desp=sqrt(((x0-xf)^2)+((y0-yf)^2)); en pixeles
;           
;           ;dist=total(vclStruct.XYZDisplacement); en micrómetros
;           dist=total(vclStruct.XYZDisplacement)/xReal; en pixeles
;           ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
;           ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- FIN
;           
;           
;           ;creo que vclStruc contiene las coordenadas,   talvez sea mas sencilo usar estas para calular area en lugar de recuperar las ya calculadas
;;           ; Susana - Evaluate Object size - BEGIN
;;           ;  'Object Size [x²]'  'Object Size [Pixel²]'   'Object Size [%]'
;;           clusPos = 2
;;           strIDCore = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPos) + '_', /rem); Susana - aqui permitir realizar informe para zpos chpos actual
;;           sizeParamsCorePixel = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Size [Pixel²]')
;;            if (n_elements(trackParams) ne 0) then begin
;;                  whNotMinusOne = where(trackParams ne -1, count)
;;            endif
;;            
;;            
;;            aqui evaluando size area caja trayectoria
;;           ; Susana - Evaluate Object size - END
;           
;           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
;           ; check at what time/frame the object started being tracked
;           firstAppeared = (*vclStruct.pTValues)[0]
;           
;           ; check how many frames the object was tracked
;           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0]);
;           
;           ; check that all elements of the TO are not 'too close' to the border
;           ;2D case --- @todo - need to figure out the 3D case
;           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
;           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
;           countBorder = 0
;           if(firstAppeared gt 0) then begin
;               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
;               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
;               xO = (*vclStruct.pXYZValues)[0,0]/xReal
;               yO = (*vclStruct.pXYZValues)[0,1]/yReal
;               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
;           endif
;;           if(i eq 4) then begin
;;             print,'SIP'
;;           endif 
;           ; create a bounding box of the track path -- convert trajectory to pixels
;            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
;            ;areaThreshold = 2; this is 2x2 pixels large
;            ;xBox = 10000
;            xBox = 0 ; Susana
;            ;yBox = 10000
;            yBox = 0 ;Susana
;            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
;           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
;               ; find bounding center of mass values in each dimension
;               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
;               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
;               ; represent in whole pixels rather than subpixel positions
;               xMin = floor(xMin)
;               xMax = ceil(xMax)
;               yMin = floor(yMin)
;               yMax = ceil(yMax)
;               ; check in each dimension what the number of pixels are
;               xBox = xMax - xMin 
;               yBox = yMax - yMin
;            endif
;           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
;           
;           xBox_vect[i] = xBox ; Susana - evaluando xBox e yBox
;           yBox_vect[i] = yBox ; Susana - evaluando xBox e yBox
;           
;           
;           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
;           ;ALH , creo que no lo informo aùn no
;           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
;           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
;           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)
;
;           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
;           
;           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
;           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
;               if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
;               endif else begin
;                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
;               endelse
;             
;             
;             ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - INICIO
;             ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - INICIO
;;             ;if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 17)) then begin ;en uso 
;;             if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 3)) then begin
;;                  ;No motiles
;;                  VCLvect[i] = 0
;;                  vslVect[i] = 0  
;;                  VAPvect[i] = 0
;;                  vslTotal = 0
;;               endif
;           ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - FIN
;           ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - FIN
;             
;             
;             ;SUSANA - LIN - INICIO
;             ;LIN
;             ; MOR @todo - need to update the functions calling LIN, WOB, STR in C_sTrackGroupObject function in order to have
;             ; the correct access to these calculations 
;             
;             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
;             if (VCLvect[i] ne 0) then begin
;               ;LINvect[i] = vslVect[i]/VCLvect[i]
;               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
;               ;WOBvect[i] = VAPvect[i]/VCLvect[i]
;               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
;             endif else begin
;               LINvect[i] = 0
;               WOBvect[i] = 0
;             endelse
;             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
;             ; MOR STR calculation - BEGIN
;             if (VAPvect[i] ne 0) then begin
;               ;STRvect[i] = vslVect[i]/VAPvect[i]
;               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
;             endif else begin
;               STRvect[i] = 0
;            endelse
;             ; MOR STR calculation - END
;             
;             ; MOR - create a vector to store the reason why something was classified as 'NC' - BEGIN
;;             overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
;             ; legend 
;             ; classified for analysis - 0
;             ; less than 2 data points - 1
;             ; appeared late in time - 2
;             ; tracked less than 50% of total time sample - 3
;             ; too near the border at time of first track, if it is after the 1st frame - 4
;             ; MOR - create a vector to store the reason why something was classified as 'NC' - END
;           ;if(i eq 94) then begin
;           ;    print,'ver minTime'
;           ;endif
;           ;@TODO Susana and/or Victor: spermatozoa should be categorized as   for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
;;            print,'D VCL <'+STRING(veloLimitsVCL[1])+' y/o '+' VSL <='+STRING(veloLimitsVSL[0])
;;            print,'C VCL >='+STRING(veloLimitsVCL[2])+' y <'+STRING(veloLimitsVCL[3])+'   y/o   VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;;;            print,'C VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;;            print,'B VCL >='+STRING(veloLimitsVCL[4])+' y <'+STRING(veloLimitsVCL[5])
;;            print,'A VCL >='+STRING(veloLimitsVCL[6])
;;            print,'-- velocidades --'
;            
;             case 1 of
;             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
;            ; MOR - 6Sept2010 - previous check for 'NC' classification
;               ; MOR - store reason for 'NC' - BEGIN
;               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
;                     overallType[i] = 1
;                     col = [0,255,255]
;                     thickk = 2 ;default
;                     vclVectTipo[i] = 'NC' 
;                     cuentaTOsNC += 1                     
;                     break 
;               endif
;               
;               if ((countBorder gt 0)) then begin
;                     overallType[i] = 4
;                     col = [0,255,120]
;                     thickk = 2
;                     ;vclVectTipo[i] = 'NCpp' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
;                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
;                     cuentaTOsNC += 1                      
;                     break
;               endif
;               
;               if( (firstAppeared ge minTime)) then begin 
;                     overallType[i] = 2
;                     col = [0,120,255]
;                     thickk = 2;
;                     ;vclVectTipo[i] = 'NCfa' 
;                     vclVectTipo[i] = 'NC'
;                     cuentaTOsNC += 1
;                     break
;               endif
;
;               if ((timesTracked le minNumFramesTracked)) then begin 
;                     overallType[i] = 3
;                     col = [120,255,255]
;                     thickk = 2
;                     ;vclVectTipo[i] = 'NCtt' 
;                     vclVectTipo[i] = 'NC'
;                     cuentaTOsNC += 1
;                     break
;               endif
;
;               ; MOR - store reason for 'NC' - END
;               
;             end
;;             ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - INICIO
;;            (vslTotal le veloLimitsVSL[0]): begin
;;              ;col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
;;              col = [255,255,255]
;;              thickk = 4;4
;;              vclVectTipo[i] = 'D' ;Susana
;;              vslReasign[i]=i ; Solo #TrackObjectReasignado
;;              end
;;            ((vslTotal gt veloLimitsVSL[0]) and (vslTotal le veloLimitsVSL[1])): begin
;;              ;para ¿TipoB?  que deben ser TipoC
;;              col = [0,255,0] ; No progresivos verdes "Tipo C"
;;              ;col = [255,255,255]
;;              ;thickk = 4 ;default
;;              thickk = 8
;;              vclVectTipo[i] = 'C' ;Susana
;;              vslReasign[i]=i ; Solo #TrackObjectReasignado 
;;              end
;;              ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - FIN
;
;            ;((VCLvect[i] ge veloLimitsVCL[6]) and (LINvect[i] ge 50) and (STRvect[i] ge 80)): begin
;            ((VCLvect[i] ge veloLimitsVCL[6]) and (LINvect[i] ge LINlim) and (STRvect[i] ge STRlim)): begin
;              col = [255,0,0] ;Progresivos rapidos rojos "Tipo A"
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'A' ;Susana
;              end
;            ((VCLvect[i] ge veloLimitsVCL[6]) and (LINvect[i] lt LINlim) and (STRvect[i] lt STRlim)): begin
;              col = [255,255,0] ;Progresivos lentos amarillos "Tipo B" FAST
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'B' ;Susana
;              end
;            ((VCLvect[i] ge veloLimitsVCL[4]) and (VCLvect[i] lt veloLimitsVCL[5]) and (STRvect[i] ge STRlim)): begin
;              ;col = [255,245,0] ;Progresivos lentos amarillos "Tipo B" MEDIUM
;              col = [255,255,0] ;Progresivos lentos amarillos "Tipo B" MEDIUM
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'B' ;Susana
;              end
;            ((VCLvect[i] ge veloLimitsVCL[4]) and (VCLvect[i] lt veloLimitsVCL[5])  and (STRvect[i] lt STRlim)): begin
;              col = [0,255,0] ; No progresivos verdes "Tipo C" MEDIUM
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'C' ;Susana
;              print,'TipeC 1 -----'
;              print,'VCL >='+strcompress(string(fix(veloLimitsVCL[4]), format = '(F6.1)'), /remove_all)+'  Y   < '+strcompress(string(fix(veloLimitsVCL[5]), format = '(F6.1)'), /remove_all)+'  Y   STR < '+strcompress(string(fix(STRlim), format = '(F6.1)'), /remove_all)
;              end
;            ((VCLvect[i] ge veloLimitsVCL[2]) and (VCLvect[i] lt veloLimitsVCL[3])): begin
;              col = [0,255,0] ; No progresivos verdes "Tipo C"
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'C' ;Susana
;              print,'TipeC 2 -----'
;              print,'VCL >='+strcompress(string(fix(veloLimitsVCL[2]), format = '(F6.1)'), /remove_all)+'  Y   < '+strcompress(string(fix(veloLimitsVCL[3]), format = '(F6.1)'), /remove_all)
;              end
;            (VCLvect[i] lt veloLimitsVCL[1]): begin
;              col = [0,0,255] ;inmoviles azules "Tipo D"
;              ;thickk = 5;4 ;default
;              thickk = 2;4 ;default
;              vclVectTipo[i] = 'D' ;Susana
;              end
;            ((VCLvect[i] ge veloLimitsVCL[6]) and (LINvect[i] ge LINlim) or (STRvect[i] ge STRlim)): begin
;              ;Si entra aqui  .. PROBLEMAS ... VCL alta con bajas LIN y STR ¿que será?
;              ;col = [255,255,255] ; BLANCO
;              ;col = [255,200,0] ; AMARLLO
;              col = [255,255,0] ; AMARLLO
;              overallType[i] = 3
;              thickk = 1
;              ;vclVectTipo[i] = 'D' ; blanco
;              vclVectTipo[i] = 'B'  ; amarillo
;              cuentaTOsNC += 1
;;              print,'--- NC ---  no cumple criterio clasificacion'
;;              print,'VCL '+strcompress(string(fix(VCLvect[i]), format = '(F6.1)'), /remove_all)
;;              print,'VAP '+strcompress(string(fix(VAPvect[i]), format = '(F6.1)'), /remove_all)
;;              print,'STR '+strcompress(string(fix(STRvect[i]), format = '(F6.1)'), /remove_all)
;;              print,'LIN '+strcompress(string(fix(LINvect[i]), format = '(F6.1)'), /remove_all)
;             end
;           endcase
;           
;           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
;           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'D') ) then begin  
;            ;and (STRvect[i] lt 50) 
;            cuentaCirc += 1
;            thickk = 2
;           endif
;           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
;           
;           if (vslTotal gt vslMaxim) then begin 
;            vslMaxim = vslTotal
;            vslMaximPos = i
;           endif
;           
;           
;           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
;             np = (*state.poCurrTrackGroup)->getObjNum()
;             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
;             miX = (*VAPStruct.pXYZValues)[*,0]
;             miY = (*VAPStruct.pXYZValues)[*,1]
;             miZ = (*VAPStruct.pXYZValues)[*,2]
;             miT = (*VAPStruct.pTValues)
;             np = n_elements(miX)
;             for j = 0, (np-1) do begin
;;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
;              ;if(vclVectTipo[i] ne 'NC') then begin
;               ;temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
;               ;temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
;               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL)+STRING(9B)+strcompress(xBox,/REMOVE_ALL)+STRING(9B)+strcompress(yBox,/REMOVE_ALL)+STRING(9B)+strcompress(xBox*yBox,/REMOVE_ALL);+STRING(10B) ;para IGOR   con xBox e yBox
;               printF, Unit,temp
;              ;endif
;             endfor
;;            ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - BEGIN
;;             if(vclVectTipo[i] ne 'NC') then begin ;guardo coordenadas de los clasificados
;;             for j = 0, (np-1) do begin
;;               ;temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
;;                temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem);+STRING(10B) ;para IGOR
;;               printF, Unit,temp
;;              endfor
;;             endif
;;             ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - END           
;           
;           ; MOR - get entire time vector
;           timeTrack[i] = n_elements(*VAPStruct.ptValues)
;           
;           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
;           if  (vclVectTipo[i] ne 'NC') then begin
;           ; MOR - 05Jul2010
;           ; convert track xyz to pixels for plotting against image
;              if(vclVectTipo[i] eq 'D') then begin
;                ;;oSym = obj_new('IDLgrSymbol', data = 3, size = 2.5, thick = thickk);original data = 3=>Dot
;                oSym = obj_new('IDLgrSymbol', data = 3, size = 5.5, thick = thickk); Dot de mayor tamaño
;              endif else begin
;                ;oSym = obj_new('IDLgrSymbol', data = 2, thick = thickk);data = 2=>asterisco   thick=>line thickness
;                oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);data = 0 =>no symbol   thick=>line thickness
;              endelse
;             
;              oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
;              oModel->add, oPlot
;              
;              ; Susana - Incluyo LIN temporalmente  INICIO
;;              if((vclVectTipo[i] eq 'B')) then begin
;;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;                oModel->add, oPlot
;;              endif
;;              if((vclVectTipo[i] eq 'C')) then begin
;;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;                oModel->add, oPlot
;;              endif
;;             ; Susana - Incluyo LIN temporalmente  INICIO
;             
;;             ; Susana - add TO number - INICIO
;;             oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [15, 15], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             oModel->add, oPlot
;;             ; Susana - add TO number - FIN
;           endif
;           
;           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
;           ;name = 'CoordsTOs_'
;           name = 'CoordsTOsSCA_'+nom
;           file = strCompress(tempFileName + name + '.dat', /remove)
;           
;           get_lun, U
;            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
;         if(i eq 0) then begin
;            openW, U, file
;            ;printF, U, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
;            printF, U, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
;         endif else begin
;            openW, U, file, /APPEND
;         endelse
;         ; print coordinates for every track object for every time
;         lrr = n_elements(*VAPStruct.ptValues) 
;             for j = 0, (lrr-1) do begin
;               ;;temp = strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
;               ;temp = strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
;               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
;               printF, U,temp
;             endfor   
;         close, U
;         FREE_LUN, U
;         
;         ; MOR write raw trajectory to file - END
;           
;           ; MOR - write to file the track objects overallType of classification - BEGIN    
;           ;temp = strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
;           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
;           printF, U1,temp 
;           ; MOR - write to file the track objects overallType of classification - END
;           
;         endfor
;         
;         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
;         close, Unit
;         FREE_LUN, Unit
;         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
;         
;         ; MOR - close up file with overalltype classification - BEGIN
;         close, U1
;         FREE_LUN, U1
;         ; MOR - close up file with overalltype classification - END         
;         
;
;         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
;         ;bslachs  total de backslashs
;         ;lbSlashs arreglo con largos de cada string separado
;         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
;         ; MOR - 9Feb 2011 - change so the last directory name prints on Motil.tiff file
;         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
;         name = 'VelocityDataSCA_'+nom ;incluyo nombre ultima carpeta en nombre archivo
;         
;         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
;         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
;         ;;printF, Unit, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR
;         ;printF, Unit, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR
;         printF, Unit, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(9B)+'xBoxVeloc_pix'+STRING(9B)+'yBoxVeloc_pix'+STRING(9B)+'xBox_X_yBoxVeloc_pix'+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR  ; Susana  - con xBox e yBox
;         for i = 0, (np-1) do begin
;          if(vclVectTipo[i] ne 'NC') then begin
;           ;;temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR
;           ;temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR
;           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]*yBox_vect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR   Susana- con xBox e yBox
;           printF, Unit,temp
;          endif
;         endfor
;         close, Unit
;         FREE_LUN, Unit
;         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
;         
;         
;         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
;         ;bslachs  total de backslashs
;         ;lbSlashs arreglo con largos de cada string separado
;         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
;         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
;         name = 'Motil_MovVarsSCA_'+nom ;incluyo nombre ultima carpeta en nombre archivo
;         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
;         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
;         ;printF, Unit, 'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'Type'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
;         printF, Unit, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'Type'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
;         
;         
;         for i = 0, (np-1) do begin
;          if(vclVectTipo[i] ne 'NC') then begin
;           ;;temp = strcompress(string(i),/REMOVE_ALL)+' '+strcompress(string(LINvect[i]),/REMOVE_ALL)+' '+strcompress(string(STRvect[i]),/REMOVE_ALL)+' '+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(13B)
;           ;temp = strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
;           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
;           printF, Unit,temp
;          endif
;         endfor
;         close, Unit
;         FREE_LUN, Unit
;         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
;         
;           
;         ;Susana INICIO --  Guardando #TrackObjects Reasignados
;         name = 'NumTrackObjectsVSLSCA_'+nom ;incluyo nombre ultima carpeta en nombre archivo
;         file = strCompress(tempFileName + name + '.dat', /remove)
;         get_lun, unit
;         openW, Unit, file
;         printF, Unit,transpose(vslReasign)
;         close, Unit
;         FREE_LUN, Unit
;         ;Susana FIN --  Guardando #TrackObjects Reasignados
;                 
;         
;         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
;         np = (*state.poCurrTrackGroup)->getObjNum()
;         meanVAP_A = 0; promedio tipos A
;         meanVAP_B = 0; promedio tipos B
;         meanVAP_C = 0; promedio tipos C
;         meanVAP_D = 0; promedio tipos D
;         meanVAP_NC = 0; promedio tipos NC
;         
;         meanVCL_A = 0; promedio tipos A
;         meanVCL_B = 0; promedio tipos B
;         meanVCL_C = 0; promedio tipos C
;         meanVCL_D = 0; promedio tipos D
;         meanVCL_NC = 0; promedio tipos NC
;         
;         meanVSL_A = 0; promedio tipos A
;         meanVSL_B = 0; promedio tipos B
;         meanVSL_C = 0; promedio tipos C
;         meanVSL_D = 0; promedio tipos D
;         meanVSL_NC = 0; promedio tipos NC
;         
;         meanLIN_A = 0; promedio LIN tipos A
;         meanLIN_B = 0; promedio LIN tipos B
;         meanLIN_C = 0; promedio LIN tipos C
;         meanLIN_D = 0; promedio LIN tipos D
;         meanLIN_NC = 0; promedio LIN tipos NC
;         
;         meanSTR_A = 0; promedio STR tipos A
;         meanSTR_B = 0; promedio STR tipos B
;         meanSTR_C = 0; promedio STR tipos C
;         meanSTR_D = 0; promedio STR tipos D
;         meanSTR_NC = 0; promedio STR tipos NC
;         
;         meanWOB_A = 0; promedio WOB tipos A
;         meanWOB_B = 0; promedio WOB tipos B
;         meanWOB_C = 0; promedio WOB tipos C
;         meanWOB_D = 0; promedio WOB tipos D
;         meanWOB_NC = 0; promedio WOB tipos NC
;         
;         meanVAP_Acnt = 0; counter para tipos A
;         meanVAP_Bcnt = 0; counter para tipos B
;         meanVAP_Ccnt = 0; counter para tipos C
;         meanVAP_Dcnt = 0; counter para tipos D
;         meanVAP_NCcnt = 0; counter para tipos NC
;         ;desde aqui descomenté -Susana
;         for h = 0, (np-1) do begin
;          case 1 of
;            (vclVectTipo[h] eq 'A'): begin
;              meanVAP_A += VAPvect[h]
;              meanVCL_A += VCLvect[h]
;              meanVSL_A += vslVect[h]
;              meanLIN_A +=  LINvect[h]
;              meanSTR_A +=  STRvect[h]
;              meanWOB_A +=  WOBvect[h]
;              ;meanALH_A += ALHvect[h]
;              meanVAP_Acnt += 1
;              end
;            (vclVectTipo[h] eq 'B'): begin
;              meanVAP_B += VAPvect[h]
;              meanVCL_B += VCLvect[h]
;              meanVSL_B += vslVect[h]
;              meanLIN_B += LINvect[h]
;              meanSTR_B +=  STRvect[h]
;              meanWOB_B +=  WOBvect[h]
;              ;meanALH_B += ALHvect[h]
;              meanVAP_Bcnt += 1
;              end
;            (vclVectTipo[h] eq 'C'): begin
;              meanVAP_C += VAPvect[h]
;              meanVCL_C += VCLvect[h]
;              meanVSL_C += vslVect[h]
;              meanLIN_C += LINvect[h]
;              meanSTR_C +=  STRvect[h]
;              meanWOB_C +=  WOBvect[h]
;              ;meanALH_C += ALHvect[h]
;              meanVAP_Ccnt += 1
;              end
;            (vclVectTipo[h] eq 'D'): begin
;              meanVAP_D += VAPvect[h]
;              meanVCL_D += VCLvect[h]
;              meanVSL_D += vslVect[h]
;              meanLIN_D += LINvect[h]
;              meanSTR_D +=  STRvect[h]
;              meanWOB_D +=  WOBvect[h]
;              ;meanALH_D += ALHvect[h]
;              meanVAP_Dcnt += 1
;              end
;            ;(vclVectTipo[h] eq 'NC') or (vclVectTipo[h] eq 'NCpp') or (vclVectTipo[h] eq 'NCfa') or (vclVectTipo[h] eq 'NCtt'): begin
;            (vclVectTipo[h] eq 'NC'): begin
;              meanVAP_NC += VAPvect[h]
;              meanVCL_NC += VCLvect[h]
;              meanVSL_NC += vslVect[h]
;              meanLIN_NC += LINvect[h]
;              meanSTR_NC +=  STRvect[h]
;              meanWOB_NC +=  WOBvect[h]
;              ;meanALH_NC += ALHvect[h]
;              meanVAP_NCcnt += 1
;              end
;           endcase
;         endfor
;         ;hasta aqui descomenté--Susana
;         meanVAP_A /= meanVAP_Acnt
;         meanVAP_B /= meanVAP_Bcnt
;         meanVAP_C /= meanVAP_Ccnt
;         meanVAP_D /= meanVAP_Dcnt
;         meanVAP_NC /= meanVAP_NCcnt
;         
;         meanVCL_A /= meanVAP_Acnt
;         meanVCL_B /= meanVAP_Bcnt
;         meanVCL_C /= meanVAP_Ccnt
;         meanVCL_D /= meanVAP_Dcnt
;         meanVCL_NC /= meanVAP_NCcnt
;         
;         meanVSL_A /= meanVAP_Acnt
;         meanVSL_B /= meanVAP_Bcnt
;         meanVSL_C /= meanVAP_Ccnt
;         meanVSL_D /= meanVAP_Dcnt
;         meanVSL_NC /= meanVAP_NCcnt
;         
;         meanLIN_A /= meanVAP_Acnt
;         meanLIN_B /= meanVAP_Bcnt
;         meanLIN_C /= meanVAP_Ccnt
;         meanLIN_D /= meanVAP_Dcnt
;         meanLIN_NC /= meanVAP_NCcnt
;         
;         meanSTR_A /= meanVAP_Acnt
;         meanSTR_B /= meanVAP_Bcnt
;         meanSTR_C /= meanVAP_Ccnt
;         meanSTR_D /= meanVAP_Dcnt
;         meanSTR_NC /= meanVAP_NCcnt
;         
;         meanWOB_A /= meanVAP_Acnt
;         meanWOB_B /= meanVAP_Bcnt
;         meanWOB_C /= meanVAP_Ccnt
;         meanWOB_D /= meanVAP_Dcnt
;         meanWOB_NC /= meanVAP_NCcnt
;         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
;         
;         
;         ;print, vclVectTipo
;         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
;         oView->add, oModel
;         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
;         oBuffer->draw, oView      
;         oOImage = oBuffer->read()
;         oOImage->getProperty, data = outImage_1
;         
;         ; MOR - comment out old path for saving, try path where images are saved instead 
;         ;write_tiff, s_getPathForSystem() + 'VCL_trajectory.tiff', outImage_1
;         ;write_tiff, tempFileName + 'VCL_trajectory.tiff', outImage_1
;         write_tiff, tempFileName + 'Motil_trajectorySCA'+nom+'.tiff', outImage_1
;         ;aqui borraesta linea ? Susana. nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
;         
;         ; MOR - destroy objects created for plotting to free up memory
;         obj_destroy, [oBuffer, oView]
;         
;         ; MOR - add creation of VSL histogram - BEGIN
;         ;path = tempFileName + 'VSL_Histo.tiff'
;         path = tempFileName + 'VSL_HistoSCA'+nom+'.tiff'
;         
;         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
;         whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
;         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 ;Aqui reemplazo ne por eq, es decir envio al histograma todos los trackobjects menos los reasignados
;         if(whClassified[0] ne -1) then begin; Susana1
;          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
;;         vect = vslVect ; MOR - commented out previous code
;          name = "rectilinear velocity VSL SCA [um s-1]"
;          ;name = "rectilinear velocity VSL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
;          
;          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
;          CEDAI_histogramPlot_VSL, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName
;          ; MOR - add creation of VSL histogram - END
;          ;CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
;          v_histogVSL_OK = 1
;         endif; Susana1
;         ;endif else begin; Susana1
;            ;
;         ;endelse; Susana1
;         
;         
;         
;         ; MOR - comment out old path for saving histogram, change to image path instead
;         ;path = s_getPathForSystem() + 'VCL_Histo.tiff'
;         ;path = tempFileName + 'VCL_Histo.tiff'
;         path = tempFileName + 'VCL_HistoSCA'+nom+'.tiff'
;         
;         v_histogVCL_OK = 0 ; Susana - controlar si existe histograma VCL de clasificados no reasignados
;         whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
;         if(whClassified[0] ne -1) then begin; Susana2
;          ; Susana - A B C o D, no reasignado
;          vect = VCLvect[whClassified]
;          name = "curvilinear velocity VCL SCA [um s-1]"
;          ;name = "curvilinear velocity VCL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
;          linesFlag = 1
;          CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
;          v_histogVCL_OK = 1
;         endif; Susana2
;         ;endif else begin; Susana2
;            ;nada
;            ;El problema de no haber alternativa es que no crea imágenes como VCL_Histo.tiff
;         ;endelse; Susana2
;         
;         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
;         
;         ;Susana INICIO    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
;         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
;         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
;         ;nom2 = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
;         nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
;         ;name = 'DatosPChenlo_2011'
;         name = 'DatosSCA_'+nom2
;         
;         ;Susana - reemplazo tempFileName INICIO
;         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
;         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
;         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
;         ;Susana - reemplazo tempFileName FIN
;         
;         ;Susana - si existe hago append, y si no creo archivo - INICIO
;         get_lun, unitPCh
;         OPENR, unitPCh, file, ERROR = err  
;          IF (err NE 0) THEN BEGIN
;           openW, unitPCh, file
;           ;printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
;           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
;          ENDIF ELSE BEGIN
;           close, unitPCh
;           FREE_LUN, unitPCh
;           openW, unitPCh, file,/APPEND
;          ENDELSE
;         ;Susana - si existe hago append, y si no creo archivo - INICIO
;         
;         ;openW, unitPCh, file
;         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
;         ;recuentos
;         aux = where(vclVectTipo eq 'A', tipoA)
;         aux = where(vclVectTipo eq 'B', tipoB)
;         aux = where(vclVectTipo eq 'C', tipoC)
;         aux = where(vclVectTipo eq 'D', tipoD)
;         aux = where(vclVectTipo eq 'NC', tipoNC)
;         ;porcentajes
;         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
;         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
;         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
;         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
;         ;print,tipoA+tipoB+tipoC+tipoD  corresponde a vect_elements
;         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
;         ;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
;         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
;         printF, unitPCh, temp
;         close, unitPCh
;         FREE_LUN, unitPCh
;         ;Susana FIN    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
;         
;         
;         ;dummy = where(vect ge veloLimitsVCL[6], count)
;         dummy = where(vclVectTipo eq 'A', count) ;Susana
;         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
;         string_0 = 'Type A:'
;         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
;         
;         ;dummy = where(vect ge veloLimitsVCL[4] and vect lt veloLimitsVCL[5], count)   ;Susana
;         dummy = where(vclVectTipo eq 'B', count) ;Susana
;         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
;         string_1 = 'Type B:' 
;         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
;         
;         ;dummy = where(vect ge veloLimitsVCL[2] and vect lt veloLimitsVCL[3], count)   ;Susana
;         dummy = where(vclVectTipo eq 'C', count) ;Susana
;         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
;         string_2 = 'Type C:'
;         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
;         
;         ;dummy = where(vect ge veloLimitsVCL[0] and vect lt veloLimitsVCL[1], count)   ;Susana
;         dummy = where(vclVectTipo eq 'D', count) ;Susana
;         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
;         string_3 = 'Type D:'
;         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
;         ; MOR - comment out all previously defined save paths
;         ;fileName = tempFileName + 'Motil_Text.tiff'
;         fileName = tempFileName + 'Motil_TextSCA'+nom+'.tiff'
;         
;         
;         ;SUSANA - Contabilizando trackobjects NC (No Clasificados) 
;         dummy = where(vclVectTipo eq 'NC', count) ;Susana
;         ;string_21 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
;         string_21 = strcompress( string(fix(count)), /remove_all) + ' sperms'
;         ;string_23 = 'NC       :'
;         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - INICIO
;         string_23 =strcompress('#Traj.Circ.:' + string(fix(cuentaCirc)), /remove_all) + '(LIN<50) '+ '#NC:'
;         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - FIN
;         
;         ;filename2 = nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])  ;Susana incluye nombre de muestra en informe para evitar error al ver informe, borrar 
;         ;string_27 = string(100.*count / vect_elements, format = '(F6.2)') + ' %' ;NC no estan incluidos en el total de espermatozoides
;         CEDAI_textPlot, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11,string_21,string_23, nom
;         
;         ; MOR - 12Feb 2011- average number of objects found per time
;         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
;         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
;         totalNumObjs.avg = stats[0]*conFactor
;         totalNumObjs.std = sqrt(stats[1]*conFactor)
;         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
;         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
;         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
;         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
;         totalNumObjs.se = sqrt(totalNumObjs.avg*conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
;         
;         string_0 = 'Concentration Statistics Summary'
;;         string_1 = strcompress('Frames ') 
;         string_2 = strcompress('Mean: ') 
;;         string_3 = strcompress('SDev ') 
;         string_4 = strcompress('Standard Error: ') 
;;         string_5 = strcompress('Median ') 
;;         string_6 = strcompress('Min ') 
;;         string_7 = strcompress('Max ') 
;         
;         string_8 = strcompress(string(totalNumObjs.number, format = '(D6.2)'), /rem) 
;         string_9 = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem) 
;         string_10 = strcompress(string(totalNumObjs.std, format = '(I6.2)'), /rem) 
;         string_11 = strcompress(string(totalNumObjs.se, format = '(D6.2)'), /rem) 
;         string_12 = strcompress(string(totalNumObjs.med, format = '(I6.2)'), /rem) 
;         string_13 = strcompress(string(totalNumObjs.mini, format = '(I6.2)'), /rem) 
;         string_14 = strcompress(string(totalNumObjs.maxi, format = '(I6.2)'), /rem) 
;         
;         string_15 = strcompress('[10E6/mL]', /rem) 
;       
;         ; MOR - comment out all previously defined save paths
;;         fileName = tempFileName + 'Concentration_Text.tiff'     
;         
;;         CEDAI_textPlot_Count, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7,string_8, string_9, string_10, string_11, string_12, string_13, string_14
;         ;fileName = tempFileName + 'Concentration_Text_Short.tiff'  
;         fileName = tempFileName + 'Concentration_Text_ShortSCA'+nom+'.tiff'
;         
;         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
;         
;         ; MOR - 12Feb2011 write total count data to .dat file 
;         dum = max(timeTrack, maxI)
;         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
;         
;         ;name = 'Concentration'
;         name = 'ConcentrationSCA'+nom
;         file = strCompress(tempFileName + name + '.dat', /remove)
;         get_lun, unit
;         openW, Unit, file
;         ;printF, Unit, 'Time' +string(9B)+ 'Count'+STRING(13B)
;         printF, Unit, 'zslice'+STRING(9B)+'Time' +string(9B)+ 'Count'+STRING(13B)
;         indU = uniq(*numObjs.pTValues)       
;         for i = 0, n_elements(indU)-1 do begin
;         ;strcompress(string((*VAPStruct.ptValues)[j]),/rem)
;            ;temp = strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
;            temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
;            printF, Unit,temp
;         endfor
;         close, Unit
;         FREE_LUN, Unit
;         
;         ; MOR - save results in the same directory as images - BEGIN
;        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
;;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_Short.bmp'
;;         imageLeft =tempFileName + 'Motil_trajectory.tiff'
;;         imageRight =  tempFileName + 'VCL_Histo.tiff'
;;         imageRight2 =  tempFileName + 'VSL_Histo.tiff'
;;         textRight = tempFileName + 'Motil_Text.tiff'
;;         fileName = tempFileName + 'Motil_Short.tiff' 
;         imageLeft =tempFileName + 'Motil_trajectorySCA'+nom+'.tiff'
;         imageRight =  tempFileName + 'VCL_HistoSCA'+nom+'.tiff'
;         imageRight2 =  tempFileName + 'VSL_HistoSCA'+nom+'.tiff'
;         textRight = tempFileName + 'Motil_TextSCA'+nom+'.tiff'
;         fileName = tempFileName + 'Motil_ShortSCA'+nom+'.tiff' 
;         ; MOR - save results in the same directory as images - END
;;         ; textLeft = tempFileName + 'Concentration_Text.tiff' 
;         ;textLeft = tempFileName + 'Concentration_Text_Short.tiff' 
;         textLeft = tempFileName + 'Concentration_Text_ShortSCA'+nom+'.tiff' 
;         
;         
;;;         CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft
;         CEDAI_combineImages_Motil_Short, background, imageLeft, TextRight, fileName, textLeft
;         
;        ; MOR - save results in the same directory as images - BEGIN
;        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility.bmp'
;         
;;         imageLeft =tempFileName + 'Motil_trajectory.tiff'
;;         imageRight =  tempFileName + 'VCL_Histo.tiff'
;;         imageRight2 =  tempFileName + 'VSL_Histo.tiff'
;;         textRight = tempFileName + 'Motil_Text.tiff'
;;         fileName = tempFileName + 'Motil_new.tiff' 
;         imageLeft =tempFileName + 'Motil_trajectorySCA'+nom+'.tiff'
;         imageRight =  tempFileName + 'VCL_HistoSCA'+nom+'.tiff'
;         imageRight2 =  tempFileName + 'VSL_HistoSCA'+nom+'.tiff'
;         textRight = tempFileName + 'Motil_TextSCA'+nom+'.tiff'
;         fileName = tempFileName + 'Motil_newSCA'+nom+'.tiff' 
;         ; MOR - save results in the same directory as images - END
;         
;         if(v_histogVSL_OK eq 0) then begin; Susana 
;         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
;            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
;            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana 
;         endif; Susana 
;         
;         if(v_histogVCL_OK eq 0) then begin; Susana 
;         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
;            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
;         endif; Susana 
;         
;         
;         CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft
;         ;CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft, v_histogVSL_OK    ; Susana evalúa si existe histograma
;         
;         ;SUSANA - Incluyo VAP promedio por grupos LO ESTOY INTENTANDO, AUN NO CREO EL HISTOGRAMA PENDIENTES TAMBIEN HISTOGRAMAS STR Y WOB
;         ;SUSANA - add creation of ALH histogram - BEGIN
;         ;veloLimitsVCL = [0,1., 1.,5., 5.,40., 40.,10000.]
;         veloLimitsVCL = [0,50., 50.,200., 200.,400., 400.,10000.] ; no tengo rangos para ALH :( 
;         ;path = tempFileName + 'histo_ALH.tiff'
;         path = tempFileName + 'histo_ALH'+nom+'.tiff'
;         
;         vect = ALHvect
;         ;name = "ALH Amplitude of Lateral Head displacement [um]"
;         name = "ALH Amplitude of Lateral Head displacement SCA [um]"+nom
;         
;         linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
;         ;CEDAI_histogramPlot_ALH, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
;         ;Falta generar un graficador de histograma para ALH o llamar internamente el GraphicModel ALH
;         ;SUSANA - add creation of VSL histogram - END
;         
;         
;         ;fileName = tempFileName + 'Motil_MovVariables_Text.tiff' ; MovementsVariables
;         fileName = tempFileName + 'Motil_MovVariables_TextSCA'+nom+'.tiff' ; MovementsVariables
;         
;         string_0 = 'Type A:'
;         ;string_4 = string(meanVAP_A, format = '(F6.2)') + ' um/s'
;         string_4 = string(meanVAP_A, format = '(F6.1)')
;         string_8 = strcompress(string(fix(meanLIN_A), format = '(F6.1)'), /remove_all)
;         string_8_1 = strcompress(string(fix(meanSTR_A), format = '(F6.1)'), /remove_all)
;         string_8_2 = strcompress(string(fix(meanWOB_A), format = '(F6.1)'), /remove_all)
;         string_8_3 = strcompress(string(fix(meanVCL_A), format = '(F6.1)'), /remove_all)
;         string_8_4 = strcompress(string(fix(meanVSL_A), format = '(F6.1)'), /remove_all)
;         
;         string_1 = 'Type B:' 
;         ;string_5 = string(meanVAP_B, format = '(F6.2)') + ' um/s'
;         string_5 = string(meanVAP_B, format = '(F6.1)')
;         string_9 = strcompress(string(fix(meanLIN_B), format = '(F6.1)'), /remove_all)
;         string_9_1 = strcompress(string(fix(meanSTR_B), format = '(F6.1)'), /remove_all)
;         string_9_2 = strcompress(string(fix(meanWOB_B), format = '(F6.1)'), /remove_all)
;         string_9_3 = strcompress(string(fix(meanVCL_B), format = '(F6.1)'), /remove_all)
;         string_9_4 = strcompress(string(fix(meanVSL_B), format = '(F6.1)'), /remove_all)
;         
;         string_2 = 'Type C:'
;         ;string_6 = string(meanVAP_C, format = '(F6.2)') + ' um/s'
;         string_6 = string(meanVAP_C, format = '(F6.1)')
;         string_10 = strcompress(string(fix(meanLIN_C), format = '(F6.1)'), /remove_all)
;         string_10_1 = strcompress(string(fix(meanSTR_C), format = '(F6.1)'), /remove_all)
;         string_10_2 = strcompress(string(fix(meanWOB_C), format = '(F6.1)'), /remove_all)
;         string_10_3 = strcompress(string(fix(meanVCL_C), format = '(F6.1)'), /remove_all)
;         string_10_4 = strcompress(string(fix(meanVSL_C), format = '(F6.1)'), /remove_all)
;         
;         string_3 = 'Type D:'  ; Type D  Talvez no deba incluirlo
;         ;string_7 = string(meanVAP_D, format = '(F6.2)') + ' um/s'
;         string_7 = string(meanVAP_D, format = '(F6.1)')
;         string_11 = strcompress(string(fix(meanLIN_D), format = '(F6.1)'), /remove_all)
;         string_11_1 = strcompress( string(fix(meanSTR_D), format = '(F6.1)'), /remove_all)
;         string_11_2 = strcompress( string(fix(meanWOB_D), format = '(F6.1)'), /remove_all)
;         string_11_3 = strcompress(string(fix(meanVCL_D), format = '(F6.1)'), /remove_all)
;         string_11_4 = strcompress(string(fix(meanVSL_D), format = '(F6.1)'), /remove_all)
;         
;         string_21 = strcompress('VAP')
;         string_23 = strcompress('VSL')
;         string_27 = strcompress('VCL')
;         string_28 = strcompress('LIN')
;         string_29 = strcompress('STR')
;         string_30 = strcompress('WOB')
;         string_31 = ' um/s'
;         string_32 = ' %'
;         CEDAI_textPlotMotil, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
;         
;         ; MOR - save results in the same directory as images - BEGIN
;         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
;         
;         ;imageLeft =tempFileName + 'Motil_trajectory.tiff' ;Talvez esta cambie después ??
;         imageLeft =tempFileName + 'Motil_trajectorySCA'+nom+'.tiff' ;Talvez esta cambie después ??
;;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_MovVariables.bmp'
;         
;;         imageRight =  tempFileName + 'VCL_Histo.tiff' ;'histo_ALH.tiff'
;;         imageRight2 =  tempFileName + 'VSL_Histo.tiff'
;;         textRight = tempFileName + 'Motil_MovVariables_Text.tiff'
;;         fileName = tempFileName + 'Motil_MovVariables.tiff' 
;         imageRight =  tempFileName + 'VCL_HistoSCA'+nom+'.tiff' ;'histo_ALH.tiff'
;         imageRight2 =  tempFileName + 'VSL_HistoSCA'+nom+'.tiff'
;         textRight = tempFileName + 'Motil_MovVariables_TextSCA'+nom+'.tiff'
;         fileName = tempFileName + 'Motil_MovVariablesSCA'+nom+'.tiff' 
;         ; MOR - save results in the same directory as images - END
;         
;         if(v_histogVSL_OK eq 0) then begin; Susana 
;         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
;            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
;            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png';Susana 
;         endif; Susana
;         if(v_histogVCL_OK eq 0) then begin; Susana 
;         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
;            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
;         endif; Susana 
;         CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, TextRight, filename,1
;         
;      endcase
      
      'Track Motil Total':begin
          ;Add track objects of other Z slices
          widget_control, wTopBase, set_uValue = state, /no_copy
          
          s_ROITM_Update, wTopBase, FMULTIPLE = 1b
          
          widget_control, wTopBase, get_uValue = state, /no_copy
          ;print, 'correctly entered track motility after adding the graphic model'
         
         veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010 ESTANDARIZACION
         ;veloLimitsVCL = [0,1., 1.,5., 5.,40., 40.,10000.];Solo para muestra M6 M 10 ambas del 28Enero2010
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
         
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
         ; do not include in analysis if started after the xx frame in time
         ; number of frames
         minTimeF = 5;  will be value in [frames]
         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]  Susana - frame en que aparece TO
         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
         minNumFramesTracked = floor((tEnd-tStart)*0.50);  value in [frames]
         ; how close to the border the object is 
         closeBorder = 9; [pixels] -- this value is the same as max_Dist in C_sROIParam_ObjTrackObject.pro    
        
         ; write to file the track objects overallType of classification
         name = 'overallTypeTOs_Total'
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U1
         openW, U1, file
         printF, U1, 'TO'+STRING(9B)+'TypeNC'
         ; threshold area [in pixels] that the track covers
         areaThreshold = 2; this is 2x2 pixels large
         ; MOR - determine cutoffs for not including TOs in analysis - END
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]
         
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
;         sb = 50 ;Susana - sb in pixels
;         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
;         pixsSB  = floor(1.0*sb*xReal)
;         boxS = bytarr(3, pixsSB, 5)
;         boxS[0,*,*] = 0
;         boxS[1, *,*] = 0
;         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - INICIO
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - FIN
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange

         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
        
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
                                
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         
         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])
         name = 'CoordenadasTOs_Total'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         get_lun, unit
         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
         ;;;;;;; MOR - temporarily printint the color code -----
         printF, Unit, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         ;printF, Unit, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         vslMaxim = 0
         vslMaximPos = 0
         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
         ; --- Susana Creando vector para VSL --- INICIO
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         umbralVCL = 50 ;10
         ; MOR - 9Feb2011 - limits were found using pixels April 2010
         veloLimitsVSL = [2.5,4.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC
         ; --- Susana Creando vector para VSL --- FIN
         
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
        vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad A,B,C y D
         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         ; MOR - add vector for storing vsl values for the histogram - BEGIN
         vslVect =  make_array((*state.poCurrTrackGroup)->getObjNum());
         ; MOR - add vector for storing vsl values for the histogram - END
         
         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
         
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
         
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - BEGIN
         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - END 
         
         ; MOR - 12Feb2011 - get concentration in time
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
                 
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           ; Susana evaluando VSL -- INICIO
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
           vslTotal = vslVect[i] ;MOR changing calculation
           
           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
           ; Susana evaluando VSL -- FIN
           
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
                      
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
           ; check at what time/frame the object started being tracked
           firstAppeared = (*vclStruct.pTValues)[0]
           
           ; check how many frames the object was tracked
           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0])
           
           ; check that all elements of the TO are not 'too close' to the border
           ;2D case --- @todo - need to figure out the 3D case
           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
           countBorder = 0
           if(firstAppeared gt 0) then begin
               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
               xO = (*vclStruct.pXYZValues)[0,0]/xReal
               yO = (*vclStruct.pXYZValues)[0,1]/yReal
               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
           endif
           
           ; create a bounding box of the track path -- convert trajectory to pixels
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
            ;areaThreshold = 2; this is 2x2 pixels large
            xBox = 10000
            yBox = 10000
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
               ; find bounding center of mass values in each dimension
               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
               ; represent in whole pixels rather than subpixel positions
               xMin = floor(xMin)
               xMax = ceil(xMax)
               yMin = floor(yMin)
               yMax = ceil(yMax)
               ; check in each dimension what the number of pixels are
               xBox = xMax - xMin 
               yBox = yMax - yMin
               
            endif
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
                      
           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
           ;ALH , creo que no lo informo aùn no
           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)

           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
           
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
               if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
                  VCLvect[i] = 0
                  vslVect[i] = 0  
                  VAPvect[i] = 0
                  vslTotal = 0
               endif else begin
                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
               endelse
             
             ;SUSANA - LIN - INICIO
             ;LIN
             ; MOR @todo - need to update the functions calling LIN, WOB, STR in C_sTrackGroupObject function in order to have
             ; the correct access to these calculations 
             
             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
             if (VCLvect[i] ne 0) then begin
               ;LINvect[i] = vslVect[i]/VCLvect[i]
               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
               ;WOBvect[i] = VAPvect[i]/VCLvect[i]
               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               LINvect[i] = 0
               WOBvect[i] = 0
             endelse
             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
             ; MOR STR calculation - BEGIN
             if (VAPvect[i] ne 0) then begin
               ;STRvect[i] = vslVect[i]/VAPvect[i]
               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               STRvect[i] = 0
            endelse
             ; MOR STR calculation - END
             
             ; MOR - create a vector to store the reason why something was classified as 'NC' - BEGIN
;             overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
             ; legend 
             ; classified for analysis - 0
             ; less than 2 data points - 1
             ; appeared late in time - 2
             ; tracked less than 50% of total time sample - 3
             ; too near the border at time of first track, if it is after the 1st frame - 4
             ; MOR - create a vector to store the reason why something was classified as 'NC' - END
           ;if(i eq 94) then begin
           ;    print,'ver minTime'
           ;endif
             case 1 of
             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
;            ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) : begin
;            ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime)): begin
;            ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (timesTracked le minNumFramesTracked)): begin
;            ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or countBorder gt 0) : begin
               
            ; MOR - 6Sept2010 - previous check for 'NC' classification
            
            ;((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) ): begin
               ; MOR - store reason for 'NC' - BEGIN
               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
                     overallType[i] = 1
                     col = [0,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1                     
                     break 
               endif
               
               if ((countBorder gt 0)) then begin
                     overallType[i] = 4
                     col = [0,255,120]
                     thickk = 2
                     ;vclVectTipo[i] = 'NCpp' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     cuentaTOsNC += 1                      
                     break
               endif
               
               if( (firstAppeared ge minTime)) then begin 
                     overallType[i] = 2
                     col = [0,120,255]
                     thickk = 2;
                     ;vclVectTipo[i] = 'NCfa' 
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               if ((timesTracked le minNumFramesTracked)) then begin 
                     overallType[i] = 3
                     col = [120,255,255]
                     thickk = 2;2
                     ;vclVectTipo[i] = 'NCtt' 
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               ; MOR - store reason for 'NC' - END
        
             end
            (vslTotal le veloLimitsVSL[0]): begin
              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
              thickk = 6;4
              vclVectTipo[i] = 'D' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado
              end
            ((vslTotal gt veloLimitsVSL[0]) and (vslTotal le veloLimitsVSL[1])): begin
              ;para ¿TipoB?  que deben ser TipoC
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              thickk = 4
              vclVectTipo[i] = 'C' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado 
              end
            VCLvect[i] lt veloLimitsVCL[1]: begin
              col = [0,0,255] ;inmoviles azules "Tipo D"
              thickk = 5;4
              vclVectTipo[i] = 'D' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[2]) and (VCLvect[i] lt veloLimitsVCL[3]): begin
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              thickk = 1 ;default 3
              vclVectTipo[i] = 'C' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[4]) and (VCLvect[i] lt veloLimitsVCL[5]): begin
              col = [255,255,0] ;Progresivos lentos amarillos "Tipo B"
              thickk = 1 ;default 3
              vclVectTipo[i] = 'B' ;Susana
              end
            VCLvect[i] ge veloLimitsVCL[6]: begin
              col = [255,0,0] ;Progresivos rapidos rojos "Tipo A"
              thickk = 1 ;default 3
              vclVectTipo[i] = 'A' ;Susana
              end
           endcase
           
           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'D') ) then begin  
            ;and (STRvect[i] lt 50) 
            cuentaCirc += 1
            thickk = 2
           endif
           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
           
           if (vslTotal gt vslMaxim) then begin 
            vslMaxim = vslTotal
            vslMaximPos = i
           endif
           
           
           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
             np = (*state.poCurrTrackGroup)->getObjNum()
             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
             miX = (*VAPStruct.pXYZValues)[*,0]
             miY = (*VAPStruct.pXYZValues)[*,1]
             miZ = (*VAPStruct.pXYZValues)[*,2]
             miT = (*VAPStruct.pTValues)
             np = n_elements(miX)
             for j = 0, (np-1) do begin
;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
              ;if(vclVectTipo[i] ne 'NC') then begin
               temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
               printF, Unit,temp
              ;endif
             endfor
;            ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - BEGIN
;             if(vclVectTipo[i] ne 'NC') then begin ;guardo coordenadas de los clasificados
;             for j = 0, (np-1) do begin
;               ;temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
;                temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem);+STRING(10B) ;para IGOR
;               
;               printF, Unit,temp
;              endfor
;             endif
;             ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - END           
                     
           
           ; MOR - get entire time vector
           timeTrack[i] = n_elements(*VAPStruct.ptValues)
           
           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
           if  (vclVectTipo[i] ne 'NC') then begin
           ; MOR - 05Jul2010
           ; convert track xyz to pixels for plotting against image
              if(vclVectTipo[i] eq 'D') then begin
                ;oSym = obj_new('IDLgrSymbol', data = 3, size = 2.5, thick = thickk);original data = 3=>Dot
                oSym = obj_new('IDLgrSymbol', data = 3, size = 5.5, thick = thickk); Dot de mayor tamaño
              endif else begin
                oSym = obj_new('IDLgrSymbol', data = 2, thick = thickk);data = 2=>asterisco   thick=>line thickness
              endelse
             
              oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oModel->add, oPlot
              
              ; Susana - Incluyo LIN temporalmente  INICIO
;              if((vclVectTipo[i] eq 'B')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;              if((vclVectTipo[i] eq 'C')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;             ; Susana - Incluyo LIN temporalmente  INICIO
             
             ; Susana - add TO number - INICIO
             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' TO_'+string(i, format = '(F6.0)')),color = [0,255,255], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;             oPlot = obj_new('IDLgrText', STRINGS = strcompress('  . '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [15, 15], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;             oModel->add, oPlot
             ; Susana - add TO number - FIN
           endif
           
           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
           name = 'CoordsTOs_Total'
           file = strCompress(tempFileName + name + '.dat', /remove)  
           get_lun, U
         
            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            printF, U, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               ;temp = strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               temp = strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
           
           ; MOR - write to file the track objects overallType of classification - BEGIN    
           temp = strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           printF, U1,temp 
           ; MOR - write to file the track objects overallType of classification - END
           
         endfor
         
         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         ; MOR - close up file with overalltype classification - BEGIN
         close, U1
         FREE_LUN, U1
         ; MOR - close up file with overalltype classification - END         
         

         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ; MOR - 9Feb 2011 - change so the last directory name prints on Motil.tiff file
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
         name = 'VelocityData_Total'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;printF, Unit, 'TO'+' '+'VCL'+' '+'VSL'+' '+'VAP'+STRING(13B) ;13B  carriage return     10B Linefeed
         printF, Unit, 'TO'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           ;temp = strcompress(string(i),/REMOVE_ALL)+' '+strcompress(string(VCLvect[i]),/REMOVE_ALL)+' '+strcompress(string(vslVect[i]),/REMOVE_ALL)+' '+strcompress(string(VAPvect[i]),/REMOVE_ALL)+STRING(13B)
           temp = strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         
         
         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
         name = 'Motil_MovVars_Total'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'Type'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           ;temp = strcompress(string(i),/REMOVE_ALL)+' '+strcompress(string(LINvect[i]),/REMOVE_ALL)+' '+strcompress(string(STRvect[i]),/REMOVE_ALL)+' '+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(13B)
           temp = strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
         
           
         ;Susana INICIO --  Guardando #TrackObjects Reasignados
         name = 'NumTrackObjectsVSL_Total'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit,transpose(vslReasign)
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN --  Guardando #TrackObjects Reasignados
                 
         
         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
         np = (*state.poCurrTrackGroup)->getObjNum()
         meanVAP_A = 0; promedio tipos A
         meanVAP_B = 0; promedio tipos B
         meanVAP_C = 0; promedio tipos C
         meanVAP_D = 0; promedio tipos D
         meanVAP_NC = 0; promedio tipos NC
         
         meanVCL_A = 0; promedio tipos A
         meanVCL_B = 0; promedio tipos B
         meanVCL_C = 0; promedio tipos C
         meanVCL_D = 0; promedio tipos D
         meanVCL_NC = 0; promedio tipos NC
         
         meanVSL_A = 0; promedio tipos A
         meanVSL_B = 0; promedio tipos B
         meanVSL_C = 0; promedio tipos C
         meanVSL_D = 0; promedio tipos D
         meanVSL_NC = 0; promedio tipos NC
         
         meanLIN_A = 0; promedio LIN tipos A
         meanLIN_B = 0; promedio LIN tipos B
         meanLIN_C = 0; promedio LIN tipos C
         meanLIN_D = 0; promedio LIN tipos D
         meanLIN_NC = 0; promedio LIN tipos NC
         
         meanSTR_A = 0; promedio STR tipos A
         meanSTR_B = 0; promedio STR tipos B
         meanSTR_C = 0; promedio STR tipos C
         meanSTR_D = 0; promedio STR tipos D
         meanSTR_NC = 0; promedio STR tipos NC
         
         meanWOB_A = 0; promedio WOB tipos A
         meanWOB_B = 0; promedio WOB tipos B
         meanWOB_C = 0; promedio WOB tipos C
         meanWOB_D = 0; promedio WOB tipos D
         meanWOB_NC = 0; promedio WOB tipos NC
         
         meanVAP_Acnt = 0; counter para tipos A
         meanVAP_Bcnt = 0; counter para tipos B
         meanVAP_Ccnt = 0; counter para tipos C
         meanVAP_Dcnt = 0; counter para tipos D
         meanVAP_NCcnt = 0; counter para tipos NC
         ;desde aqui descomenté -Susana
         for h = 0, (np-1) do begin
          case 1 of
            (vclVectTipo[h] eq 'A'): begin
              meanVAP_A += VAPvect[h]
              meanVCL_A += VCLvect[h]
              meanVSL_A += vslVect[h]
              meanLIN_A +=  LINvect[h]
              meanSTR_A +=  STRvect[h]
              meanWOB_A +=  WOBvect[h]
              ;meanALH_A += ALHvect[h]
              meanVAP_Acnt += 1
              end
            (vclVectTipo[h] eq 'B'): begin
              meanVAP_B += VAPvect[h]
              meanVCL_B += VCLvect[h]
              meanVSL_B += vslVect[h]
              meanLIN_B += LINvect[h]
              meanSTR_B +=  STRvect[h]
              meanWOB_B +=  WOBvect[h]
              ;meanALH_B += ALHvect[h]
              meanVAP_Bcnt += 1
              end
            (vclVectTipo[h] eq 'C'): begin
              meanVAP_C += VAPvect[h]
              meanVCL_C += VCLvect[h]
              meanVSL_C += vslVect[h]
              meanLIN_C += LINvect[h]
              meanSTR_C +=  STRvect[h]
              meanWOB_C +=  WOBvect[h]
              ;meanALH_C += ALHvect[h]
              meanVAP_Ccnt += 1
              end
            (vclVectTipo[h] eq 'D'): begin
              meanVAP_D += VAPvect[h]
              meanVCL_D += VCLvect[h]
              meanVSL_D += vslVect[h]
              meanLIN_D += LINvect[h]
              meanSTR_D +=  STRvect[h]
              meanWOB_D +=  WOBvect[h]
              ;meanALH_D += ALHvect[h]
              meanVAP_Dcnt += 1
              end
            ;(vclVectTipo[h] eq 'NC') or (vclVectTipo[h] eq 'NCpp') or (vclVectTipo[h] eq 'NCfa') or (vclVectTipo[h] eq 'NCtt'): begin
            (vclVectTipo[h] eq 'NC'): begin
              meanVAP_NC += VAPvect[h]
              meanVCL_NC += VCLvect[h]
              meanVSL_NC += vslVect[h]
              meanLIN_NC += LINvect[h]
              meanSTR_NC +=  STRvect[h]
              meanWOB_NC +=  WOBvect[h]
              ;meanALH_NC += ALHvect[h]
              meanVAP_NCcnt += 1
              end
           endcase
         endfor
         ;hasta aqui descomenté--Susana
         meanVAP_A /= meanVAP_Acnt
         meanVAP_B /= meanVAP_Bcnt
         meanVAP_C /= meanVAP_Ccnt
         meanVAP_D /= meanVAP_Dcnt
         meanVAP_NC /= meanVAP_NCcnt
         
         meanVCL_A /= meanVAP_Acnt
         meanVCL_B /= meanVAP_Bcnt
         meanVCL_C /= meanVAP_Ccnt
         meanVCL_D /= meanVAP_Dcnt
         meanVCL_NC /= meanVAP_NCcnt
         
         meanVSL_A /= meanVAP_Acnt
         meanVSL_B /= meanVAP_Bcnt
         meanVSL_C /= meanVAP_Ccnt
         meanVSL_D /= meanVAP_Dcnt
         meanVSL_NC /= meanVAP_NCcnt
         
         meanLIN_A /= meanVAP_Acnt
         meanLIN_B /= meanVAP_Bcnt
         meanLIN_C /= meanVAP_Ccnt
         meanLIN_D /= meanVAP_Dcnt
         meanLIN_NC /= meanVAP_NCcnt
         
         meanSTR_A /= meanVAP_Acnt
         meanSTR_B /= meanVAP_Bcnt
         meanSTR_C /= meanVAP_Ccnt
         meanSTR_D /= meanVAP_Dcnt
         meanSTR_NC /= meanVAP_NCcnt
         
         meanWOB_A /= meanVAP_Acnt
         meanWOB_B /= meanVAP_Bcnt
         meanWOB_C /= meanVAP_Ccnt
         meanWOB_D /= meanVAP_Dcnt
         meanWOB_NC /= meanVAP_NCcnt
         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
         
         
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         
         ; MOR - comment out old path for saving, try path where images are saved instead 
         write_tiff, tempFileName + 'Motil_trajectory.tiff', outImage_1
         ; MOR - destroy objects created for plotting to free up memory
         obj_destroy, [oBuffer, oView]
         
         ; MOR - add creation of VSL histogram - BEGIN
         path = tempFileName + 'VSL_Histo.tiff'
         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
         if(whClassified[0] ne -1) then begin; Susana1
          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
          name = "rectilinear velocity VSL [um s-1] Total"
          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
          CEDAI_histogramPlot_VSL, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName
          ; MOR - add creation of VSL histogram - END
          ;CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVSL_OK = 1
         endif; Susana1
         ;endif else begin; Susana1
            ;
         ;endelse; Susana1
         
         
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;path = s_getPathForSystem() + 'VCL_Histo.tiff'
         path = tempFileName + 'VCL_Histo.tiff'
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
         if(whClassified[0] ne -1) then begin; Susana2
          vect = VCLvect[whClassified]
          
          name = "curvilinear velocity VCL [um s-1] Total"
          linesFlag = 1
          CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
         endif; Susana2
         ;endif else begin; Susana2
            ;nada
            ;El problema de no haber alternativa es que no crea imágenes como VCL_Histo.tiff
         ;endelse; Susana2
         
         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
         
         ;Susana INICIO    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
         ;name = 'DatosPChenlo_2011_'+nom ;incluyo nombre ultima carpeta en nombre archivo  comento porque no incluiré el nombre de esta muestra en el nombre de archivo
         name = 'Datos_MotilTotal'
         
         ;Susana - reemplazo tempFileName INICIO
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         ;Susana - reemplazo tempFileName FIN
         
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         get_lun, unitPCh
         OPENR, unitPCh, file, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitPCh, file
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         
         ;openW, unitPCh, file
         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
         ;recuentos
         aux = where(vclVectTipo eq 'A', tipoA)
         aux = where(vclVectTipo eq 'B', tipoB)
         aux = where(vclVectTipo eq 'C', tipoC)
         aux = where(vclVectTipo eq 'D', tipoD)
         aux = where(vclVectTipo eq 'NC', tipoNC)
         ;porcentajes
         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
         ;print,tipoA+tipoB+tipoC+tipoD  corresponde a vect_elements
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana FIN    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         
         
         
         
         ;dummy = where(vect ge veloLimitsVCL[6], count)
         dummy = where(vclVectTipo eq 'A', count) ;Susana
         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_0 = 'Type A:'
         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[4] and vect lt veloLimitsVCL[5], count)   ;Susana
         dummy = where(vclVectTipo eq 'B', count) ;Susana
         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_1 = 'Type B:' 
         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[2] and vect lt veloLimitsVCL[3], count)   ;Susana
         dummy = where(vclVectTipo eq 'C', count) ;Susana
         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_2 = 'Type C:'
         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[0] and vect lt veloLimitsVCL[1], count)   ;Susana
         dummy = where(vclVectTipo eq 'D', count) ;Susana
         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_3 = 'Type D:'
         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Motil_TextTotal.tiff'
         
         ;SUSANA - Contabilizando trackobjects NC (No Clasificados) 
         dummy = where(vclVectTipo eq 'NC', count) ;Susana
         ;string_21 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_21 = strcompress( string(fix(count)), /remove_all) + ' sperms'
         ;string_23 = 'NC       :'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - INICIO
         string_23 =strcompress('#Traj.Circ.:' + string(fix(cuentaCirc)), /remove_all) + '(LIN<50) '+ '#NC:'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - FIN
         
         ;filename2 = nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])  ;Susana incluye nombre de muestra en informe para evitar error al ver informe, borrar 
         ;string_27 = string(100.*count / vect_elements, format = '(F6.2)') + ' %' ;NC no estan incluidos en el total de espermatozoides
          
         
         CEDAI_textPlot, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11,string_21,string_23, nom
         
         ; MOR - 12Feb 2011- average number of objects found per time
         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
         totalNumObjs.avg = stats[0]*conFactor
         totalNumObjs.std = sqrt(stats[1]*conFactor)
         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
;         totalNumObjs.se = sqrt(totalNumObjs.avg*conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         totalNumObjs.se = sqrt(totalNumObjs.avg) 
         
         string_0 = 'Concentration Statistics Summary'
;         string_1 = strcompress('Frames ') 
         string_2 = strcompress('Mean: ') 
;         string_3 = strcompress('SDev ') 
         string_4 = strcompress('Standard Error: ') 
;         string_5 = strcompress('Median ') 
;         string_6 = strcompress('Min ') 
;         string_7 = strcompress('Max ') 
         
         string_8 = strcompress(string(totalNumObjs.number, format = '(D6.2)'), /rem) 
         string_9 = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem) 
         string_10 = strcompress(string(totalNumObjs.std, format = '(I6.2)'), /rem) 
         string_11 = strcompress(string(totalNumObjs.se, format = '(D6.2)'), /rem) 
         string_12 = strcompress(string(totalNumObjs.med, format = '(I6.2)'), /rem) 
         string_13 = strcompress(string(totalNumObjs.mini, format = '(I6.2)'), /rem) 
         string_14 = strcompress(string(totalNumObjs.maxi, format = '(I6.2)'), /rem) 
         
         string_15 = strcompress('[10E6/mL]', /rem) 
       
         ; MOR - comment out all previously defined save paths
;         fileName = tempFileName + 'Concentration_Text.tiff'     
         
;         CEDAI_textPlot_Count, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7,string_8, string_9, string_10, string_11, string_12, string_13, string_14
         fileName = tempFileName + 'Concentration_Text_ShortTotal.tiff'  
         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
         
         ; MOR - 12Feb2011 write total count data to .dat file 
         dum = max(timeTrack, maxI)
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
         
         name = 'ConcentrationTotal'
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit, 'Time' +string(9B)+ 'Count'+STRING(13B)
         indU = uniq(*numObjs.pTValues)       
         for i = 0, n_elements(indU)-1 do begin
         ;strcompress(string((*VAPStruct.ptValues)[j]),/rem)
            temp = strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            printF, Unit,temp
         endfor
         close, Unit
         FREE_LUN, Unit
         
         ; MOR - save results in the same directory as images - BEGIN
        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_Short.bmp'
         imageLeft =tempFileName + 'Motil_trajectoryTotal.tiff'
         imageRight =  tempFileName + 'VCL_HistoTotal.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoTotal.tiff'
         textRight = tempFileName + 'Motil_TextTotal.tiff'
         fileName = tempFileName + 'Motil_ShortTotal.tiff' 
         ; MOR - save results in the same directory as images - END
;         textLeft = tempFileName + 'Concentration_Text.tiff' 
         textLeft = tempFileName + 'Concentration_Text_ShortTotal.tiff' 
         
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - INICIO -------------------
         imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motil_trajectoryTotal_Blanca.tiff'
         imageRight = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoTotal_Blanca.tiff'
         imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoTotal_Blanca.tiff'
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - FIN -------------------
         
         CEDAI_combineImages_Motil_Short, background, imageLeft, TextRight, fileName, textLeft
         
        ; MOR - save results in the same directory as images - BEGIN
        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility.bmp'
         imageLeft =tempFileName + 'Motil_trajectoryTotal.tiff'
         imageRight =  tempFileName + 'VCL_HistoTotal.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoTotal.tiff'
         textRight = tempFileName + 'Motil_TextTotal.tiff'
         fileName = tempFileName + 'Motil_newTotal.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana
         endif; Susana 
         
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - INICIO -------------------
         imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motil_trajectoryTotal_Blanca.tiff'
         imageRight = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoTotal_Blanca.tiff'
         imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoTotal_Blanca.tiff'
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - FIN -------------------
         
         CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft
         ;CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft, v_histogVSL_OK    ; Susana evalúa si existe histograma
         
         ;SUSANA - Incluyo VAP promedio por grupos LO ESTOY INTENTANDO, AUN NO CREO EL HISTOGRAMA PENDIENTES TAMBIEN HISTOGRAMAS STR Y WOB
         ;SUSANA - add creation of ALH histogram - BEGIN
         ;veloLimitsVCL = [0,1., 1.,5., 5.,40., 40.,10000.]
         veloLimitsVCL = [0,50., 50.,200., 200.,400., 400.,10000.] ; no tengo rangos para ALH :( 
         path = tempFileName + 'histo_ALHTotal.tiff'
         vect = ALHvect
         name = "ALH Amplitude of Lateral Head displacement [um] Total"
         linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
         ;CEDAI_histogramPlot_ALH, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
         ;Falta generar un graficador de histograma para ALH o llamar internamente el GraphicModel ALH
         ;SUSANA - add creation of VSL histogram - END
             
         
         fileName = tempFileName + 'Motil_MovVariables_TextTotal.tiff' ; MovementsVariables
         string_0 = 'Type A:'
         ;string_4 = string(meanVAP_A, format = '(F6.2)') + ' um/s'
         string_4 = string(meanVAP_A, format = '(F6.1)')
         string_8 = strcompress(string(fix(meanLIN_A), format = '(F6.1)'), /remove_all)
         string_8_1 = strcompress(string(fix(meanSTR_A), format = '(F6.1)'), /remove_all)
         string_8_2 = strcompress(string(fix(meanWOB_A), format = '(F6.1)'), /remove_all)
         string_8_3 = strcompress(string(fix(meanVCL_A), format = '(F6.1)'), /remove_all)
         string_8_4 = strcompress(string(fix(meanVSL_A), format = '(F6.1)'), /remove_all)
         
         string_1 = 'Type B:' 
         ;string_5 = string(meanVAP_B, format = '(F6.2)') + ' um/s'
         string_5 = string(meanVAP_B, format = '(F6.1)')
         string_9 = strcompress(string(fix(meanLIN_B), format = '(F6.1)'), /remove_all)
         string_9_1 = strcompress(string(fix(meanSTR_B), format = '(F6.1)'), /remove_all)
         string_9_2 = strcompress(string(fix(meanWOB_B), format = '(F6.1)'), /remove_all)
         string_9_3 = strcompress(string(fix(meanVCL_B), format = '(F6.1)'), /remove_all)
         string_9_4 = strcompress(string(fix(meanVSL_B), format = '(F6.1)'), /remove_all)
         
         string_2 = 'Type C:'
         ;string_6 = string(meanVAP_C, format = '(F6.2)') + ' um/s'
         string_6 = string(meanVAP_C, format = '(F6.1)')
         string_10 = strcompress(string(fix(meanLIN_C), format = '(F6.1)'), /remove_all)
         string_10_1 = strcompress(string(fix(meanSTR_C), format = '(F6.1)'), /remove_all)
         string_10_2 = strcompress(string(fix(meanWOB_C), format = '(F6.1)'), /remove_all)
         string_10_3 = strcompress(string(fix(meanVCL_C), format = '(F6.1)'), /remove_all)
         string_10_4 = strcompress(string(fix(meanVSL_C), format = '(F6.1)'), /remove_all)
         
         string_3 = 'Type D:'  ; Type D  Talvez no deba incluirlo
         ;string_7 = string(meanVAP_D, format = '(F6.2)') + ' um/s'
         string_7 = string(meanVAP_D, format = '(F6.1)')
         string_11 = strcompress(string(fix(meanLIN_D), format = '(F6.1)'), /remove_all)
         string_11_1 = strcompress( string(fix(meanSTR_D), format = '(F6.1)'), /remove_all)
         string_11_2 = strcompress( string(fix(meanWOB_D), format = '(F6.1)'), /remove_all)
         string_11_3 = strcompress(string(fix(meanVCL_D), format = '(F6.1)'), /remove_all)
         string_11_4 = strcompress(string(fix(meanVSL_D), format = '(F6.1)'), /remove_all)
         
         string_21 = strcompress('VAP')
         string_23 = strcompress('VSL')
         string_27 = strcompress('VCL')
         string_28 = strcompress('LIN')
         string_29 = strcompress('STR')
         string_30 = strcompress('WOB')
         string_31 = ' um/s'
         string_32 = ' %'
         CEDAI_textPlotMotil, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         
         ; MOR - save results in the same directory as images - BEGIN
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         imageLeft =tempFileName + 'Motil_trajectory.tiff' ;Talvez esta cambie después ??
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_MovVariables.bmp'
         imageRight =  tempFileName + 'VCL_Histo.tiff' ;'histo_ALH.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo.tiff'
         textRight = tempFileName + 'Motil_MovVariables_TextTotal.tiff'
         fileName = tempFileName + 'Motil_MovVariablesTotal.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana
         endif; Susana 
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - INICIO -------------------
         imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motil_trajectoryTotal_Blanca.tiff'
         imageRight = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoTotal_Blanca.tiff'
         imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoTotal_Blanca.tiff'
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - FIN -------------------
         CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, TextRight, filename,1
         
      endcase

      'Track Motil Raton':begin ;Track Motil Raton; IMPORTANTE: LOS RANGOS DE VELOCIDADES PARA CLASIFICACION SON INGRESADOS DIRECTAMENTE EN UM/S NO EN PIXELES COMO EN TRACK MOTIL, TRACK MOTIL TOTAL, TRACK MOTIL OMS Y TRACK MOTIL OMS TOTAL
;         veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010 EN USO CEDAI
         veloLimitsVCL = [0,10., 10.,50., 50.,10000., 50.,10000.] ; Susana - ingreso valores directamente en micrómetros por segundo
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
         
         ;Susana - trato de incluir zpos en nombre de informe --INICIO zPos
         
         
         
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
         ; do not include in analysis if started after the xx frame in time
         ; number of frames
         minTimeF = 5;  will be value in [frames]
         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]  Susana - frame en que aparece TO
         
         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
         minNumFramesTracked = floor((tEnd-tStart)*0.50);  value in [frames]
         ; how close to the border the object is 
         closeBorder = 9; [pixels] -- this value is the same as max_Dist in Slow_sROIParam_ObjTrackObject.pro    
         ;Susana - umbrales hiperactivados - INICIO
         maxLINhiperact = 40  ;Susana - LIN máxima de hiperactivados
         minALHhiperact = 4.0  ;Susana - ALH minima de hiperactivados
         minVCLhiperact = 100  ;Susana - VCL minima de hiperactivados
         ;Susana - umbrales hiperactivados - FIN
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_z'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         
         ; write to file the track objects overallType of classification
         ;name = 'overallTypeTOs_Raton_'
         name = 'overallTypeTOs_Raton'+nom
         
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U1
         openW, U1, file
         ;printF, U1, 'TO'+STRING(9B)+'TypeNC'
         printF, U1, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'TypeNC'
         ; threshold area [in pixels] that the track covers
         areaThreshold = 2; this is 2x2 pixels large
         ; MOR - determine cutoffs for not including TOs in analysis - END
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]
         
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - INICIO
         sb = 50 ; scale bar (sb) in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - FIN
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange

         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
        
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
                                
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         
         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         ;;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])  ¿lo comento aqui?
         ;name = 'CoordenadasTOs_Raton_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         name = 'CoordenadasTOs_Raton_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         get_lun, unit
         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
         ;;;;;;; MOR - temporarily printint the color code -----
         ;printF, Unit, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         printF, Unit, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         ;;printF, Unit, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'HY'
         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         vslMaxim = 0
         vslMaximPos = 0
         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
         ; --- Susana Creando vector para VSL --- INICIO
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         ;umbralVCL = 50 ;10
         ;umbralSTR = 50 ;en %  >umbralSTR  es Progressive   <umbralSTR es Motile
         ; MOR - 9Feb2011 - limits were found using pixels April 2010
         ;veloLimitsVSL = [2.5,4.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC
         veloLimitsVSL = [13.0,30.0] ;máxima VSL en micrómetros/s que podria presentar un Slow en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC - Ratón
         ; --- Susana Creando vector para VSL --- FIN
         
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
         vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad Progressive,Hyperact,Slow y Static
         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         ; MOR - add vector for storing vsl values for the histogram - BEGIN
         vslVect =  make_array((*state.poCurrTrackGroup)->getObjNum());
         ; MOR - add vector for storing vsl values for the histogram - END
         
         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
         
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
         
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - BEGIN
         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - END 
         
         ; MOR - 12Feb2011 - get concentration in time
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
                 
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           ; Susana evaluando VSL -- INICIO
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
           vslTotal = vslVect[i] ;MOR changing calculation
           
           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
           ; Susana evaluando VSL -- FIN
           
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
                      
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
           ; check at what time/frame the object started being tracked
           firstAppeared = (*vclStruct.pTValues)[0]
           
           ; check how many frames the object was tracked
           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0]);
           
           ; check that all elements of the TO are not 'too close' to the border
           ;2D case --- @todo - need to figure out the 3D case
           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
           countBorder = 0
           if(firstAppeared gt 0) then begin
               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
               xO = (*vclStruct.pXYZValues)[0,0]/xReal
               yO = (*vclStruct.pXYZValues)[0,1]/yReal
               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
           endif
           
           ; create a bounding box of the track path -- convert trajectory to pixels
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
            ;areaThreshold = 2; this is 2x2 pixels large
            xBox = 10000
            yBox = 10000
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
               ; find bounding center of mass values in each dimension
               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
               ; represent in whole pixels rather than subpixel positions
               xMin = floor(xMin)
               xMax = ceil(xMax)
               yMin = floor(yMin)
               yMax = ceil(yMax)
               ; check in each dimension what the number of pixels are
               xBox = xMax - xMin 
               yBox = yMax - yMin
               
            endif
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
                      
           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
           ;ALH , creo que no lo informo aùn no
           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)

           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
           
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
               if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
                  VCLvect[i] = 0
                  vslVect[i] = 0  
                  VAPvect[i] = 0
                  vslTotal = 0
               endif else begin
                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
               endelse
             
             ;SUSANA - LIN - INICIO
             ;LIN
             ; MOR @todo - need to update the functions calling LIN, WOB, STR in Slow_sTrackGroupObject function in order to have
             ; the correct access to these calculations 
             
             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
             if (VCLvect[i] ne 0) then begin
               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               LINvect[i] = 0
               WOBvect[i] = 0
             endelse
             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
             ; MOR STR calculation - BEGIN
             if (VAPvect[i] ne 0) then begin
               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               STRvect[i] = 0
            endelse
             ; MOR STR calculation - END
             
             ; MOR - create a vector to store the reason why something was classified as 'NC' - BEGIN
;             overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
             ; legend 
             ; classified for analysis - 0
             ; less than 2 data points - 1
             ; appeared late in time - 2
             ; tracked less than 50% of total time sample - 3
             ; too near the border at time of first track, if it is after the 1st frame - 4
             ; MOR - create a vector to store the reason why something was classified as 'NC' - END
           ;if(i eq 94) then begin
           ;    print,'ver minTime'
           ;endif
;           ;@TODO Susana and/or Victor: spermatozoa should be categorized as   for PR and HY => Progresssively Motil (PR)     Slow => Non-Progressively Motil (NP)   Static => Immotile (IM)
;            print,'Static VCL_Raton < '+STRING(veloLimitsVCL[1])+' um/s y/o '+' VSL <= '+STRING(veloLimitsVSL[0])+' um/s'
;            print,'Slow VCL_Raton >= '+STRING(veloLimitsVCL[2])+' um/s y VCL_Raton < '+STRING(veloLimitsVCL[3])+' um/s  y/o   VSL >'+STRING(veloLimitsVSL[0])+' um/s y <= '+STRING(veloLimitsVSL[1])+' um/s'
;;            print,'Slow VSL_Raton > '+STRING(veloLimitsVSL[0])+' um/s y <= '+STRING(veloLimitsVSL[1])
;;            print,'HY VCL_Raton >= '+STRING(veloLimitsVCL[4])+'um/s y < '+STRING(veloLimitsVCL[5])
;            print,'HY VCL_Raton >= '+STRING(veloLimitsVCL[6])+' um/s y STR <= '+STRING(umbralSTR)+'%'
;;            print,'Progressive (PR) VCL_Raton >='+STRING(veloLimitsVCL[6])
;            print,'PR VCL_Raton >= '+STRING(veloLimitsVCL[6])+' um/s y STR > '+STRING(umbralSTR)+'%'
;            print,'-- velocidades --'
             case 1 of
             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
            ; MOR - 6Sept2010 - previous check for 'NC' classification
               ; MOR - store reason for 'NC' - BEGIN
               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
                     overallType[i] = 1
                     col = [0,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1                     
                     break 
               endif
               
               if ((countBorder gt 0)) then begin
                     overallType[i] = 4
                     col = [0,255,120]
                     thickk = 2
                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     cuentaTOsNC += 1                      
                     break
               endif
               
               if( (firstAppeared ge minTime)) then begin 
                     overallType[i] = 2
                     col = [0,120,255]
                     thickk = 2;
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1
                     break
               endif

               if ((timesTracked le minNumFramesTracked)) then begin 
                     overallType[i] = 3
                     col = [120,255,255]
                     thickk = 2;2
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif
               ; MOR - store reason for 'NC' - END
        
             end
            (vslTotal le veloLimitsVSL[0]): begin
              col = [0,0,255] ;inmoviles azules "Tipo Static" ;Susana
              thickk = 3;6;4
              vclVectTipo[i] = 'Static' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado
              end
            ((vslTotal gt veloLimitsVSL[0]) and (vslTotal le veloLimitsVSL[1])): begin
              ;para ¿TipoB?  que deben ser TipoC
              col = [0,255,0] ; No progresivos verdes "Tipo Slow"
              thickk = 3;4
              vclVectTipo[i] = 'Slow' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado 
              end
            VCLvect[i] lt veloLimitsVCL[1]: begin
              col = [0,0,255] ;inmoviles azules "Tipo Static"
              thickk = 3;5
              vclVectTipo[i] = 'Static' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[2]) and (VCLvect[i] lt veloLimitsVCL[3]): begin
              col = [0,255,0] ; No progresivos verdes "Tipo Slow"
              thickk = 3
              vclVectTipo[i] = 'Slow' ;Susana
              end
            ;;(VCLvect[i] ge veloLimitsVCL[4]) and (VCLvect[i] lt veloLimitsVCL[5]): begin   original
            ;;(VCLvect[i] ge veloLimitsVCL[6]) and (STRvect[i] le umbralSTR): begin  ; usando STR<=50%
            ;(VCLvect[i] ge 100) and (LINvect[i] le 65) and (ALHvect[i] ge 7.5): begin  ; usando STR<=50%    humanos
            (VCLvect[i] ge minVCLhiperact) and (LINvect[i] le maxLINhiperact) and (ALHvect[i] ge minALHhiperact): begin  ; usando STR<=50%
              col = [255,255,0] ;Progresivos lentos amarillos "Tipo Hyperact"
              thickk = 3
              vclVectTipo[i] = 'HY' ;Susana
;              vclVectTipo[i] = 'HY' ;Susana Hiperactivados
;              print,'Hiperactivado'
;              print,i
;              print,'VCL ge 100   and   LIN le 36   and   ALH ge 4.0'
;              print,'-----------------'
              end
            VCLvect[i] ge veloLimitsVCL[6]: begin
            ;(VCLvect[i] ge veloLimitsVCL[6]) and (STRvect[i] gt umbralSTR): begin  ; usando STR>50%
              col = [255,0,0] ;Progresivos rapidos rojos "Tipo PR"
              thickk = 3
              vclVectTipo[i] = 'PR' ;Susana
              end
           endcase
           
;           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
;           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'Static') ) then begin  
;            ;and (STRvect[i] lt 50) 
;            cuentaCirc += 1
;            thickk = 2
;           endif
;           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
           
           if (vslTotal gt vslMaxim) then begin 
            vslMaxim = vslTotal
            vslMaximPos = i
           endif
           
           
           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
             np = (*state.poCurrTrackGroup)->getObjNum()
             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
             miX = (*VAPStruct.pXYZValues)[*,0]
             miY = (*VAPStruct.pXYZValues)[*,1]
             miZ = (*VAPStruct.pXYZValues)[*,2]
             miT = (*VAPStruct.pTValues)
             np = n_elements(miX)
             for j = 0, (np-1) do begin
;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
              ;if(vclVectTipo[i] ne 'NC') then begin
               ;temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
               printF, Unit,temp
              ;endif
             endfor
;            ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - BEGIN
;             if(vclVectTipo[i] ne 'NC') then begin ;guardo coordenadas de los clasificados
;             for j = 0, (np-1) do begin
;                temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem);+STRING(10B) ;para IGOR
;               printF, Unit,temp
;              endfor
;             endif
;             ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - END           
              
           ; MOR - get entire time vector
           timeTrack[i] = n_elements(*VAPStruct.ptValues)
           
           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
           if  (vclVectTipo[i] ne 'NC') then begin
           ; MOR - 05Jul2010
           ; convert track xyz to pixels for plotting against image
              if(vclVectTipo[i] eq 'Static') then begin
                ;oSym = obj_new('IDLgrSymbol', data = 3, size = 2.5, thick = thickk);original data = 3=>Dot
                oSym = obj_new('IDLgrSymbol', data = 3, size = 5.5, thick = thickk); Dot de mayor tamaño
              endif else begin
                oSym = obj_new('IDLgrSymbol', data = 2, thick = thickk);data = 2=>asterisco   thick=>line thickness
              endelse
             
              oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oModel->add, oPlot
              
              ; Susana - Incluyo LIN temporalmente  INICIO
;              if((vclVectTipo[i] eq 'B')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;              if((vclVectTipo[i] eq 'C')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;             ; Susana - Incluyo LIN temporalmente  INICIO
             
             ; Susana - add TO number - INICIO
             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' TO_'+string(i, format = '(F6.0)')),color = [0,255,255], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
             oPlot = obj_new('IDLgrText', STRINGS = strcompress(' .   '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [10, 10], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
             oModel->add, oPlot
             ; Susana - add TO number - FIN
           endif
           
           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
           ;name = 'CoordsTOs_Raton_'
           name = 'CoordsTOs_Raton'+nom
           
           file = strCompress(tempFileName + name + '.dat', /remove)  
           get_lun, U
         
            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            ;printF, U, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'HY'
            printF, U, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'HY'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               ;temp = strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
           
           ; MOR - write to file the track objects overallType of classification - BEGIN    
           ;temp = strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           printF, U1,temp 
           ; MOR - write to file the track objects overallType of classification - END
           
         endfor
         
         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         ; MOR - close up file with overalltype classification - BEGIN
         close, U1
         FREE_LUN, U1
         ; MOR - close up file with overalltype classification - END         
         

         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ; MOR - 9Feb 2011 - change so the last directory name prints on Motil.tiff file
         ;;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         ;name = 'VelocityData_Raton_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         name = 'VelocityData_Raton'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;printF, Unit, 'TO'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR
         printF, Unit, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         
         
         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         name = 'Motil_MovVars_Raton_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;printF, Unit, 'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'Type'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         printF, Unit, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'Type'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           ;temp = strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
         
           
         ;Susana INICIO --  Guardando #TrackObjects Reasignados
         ;name = 'NumTrackObjectsVSL_Raton_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         name = 'NumTrackObjectsVSL_Raton'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit,transpose(vslReasign)
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN --  Guardando #TrackObjects Reasignados
                 
         
         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
         np = (*state.poCurrTrackGroup)->getObjNum()
         meanVAP_Progressive = 0; promedio tipos Progressive
         meanVAP_Hyperact = 0; promedio tipos Hyperact
         meanVAP_Slow = 0; promedio tipos Slow
         meanVAP_Static = 0; promedio tipos Static
         meanVAP_NC = 0; promedio tipos NC
         
         meanVCL_Progressive = 0; promedio tipos Progressive
         meanVCL_Hyperact = 0; promedio tipos Hyperact
         meanVCL_Slow = 0; promedio tipos Slow
         meanVCL_Static = 0; promedio tipos Static
         meanVCL_NC = 0; promedio tipos NC
         
         meanVSL_Progressive = 0; promedio tipos Progressive
         meanVSL_Hyperact = 0; promedio tipos Hyperact
         meanVSL_Slow = 0; promedio tipos Slow
         meanVSL_Static = 0; promedio tipos Static
         meanVSL_NC = 0; promedio tipos NC
         
         meanLIN_Progressive = 0; promedio LIN tipos Progressive
         meanLIN_Hyperact = 0; promedio LIN tipos Hyperact
         meanLIN_Slow = 0; promedio LIN tipos Slow
         meanLIN_Static = 0; promedio LIN tipos Static
         meanLIN_NC = 0; promedio LIN tipos NC
         
         meanSTR_Progressive = 0; promedio STR tipos Progressive
         meanSTR_Hyperact = 0; promedio STR tipos Hyperact
         meanSTR_Slow = 0; promedio STR tipos Slow
         meanSTR_Static = 0; promedio STR tipos Static
         meanSTR_NC = 0; promedio STR tipos NC
         
         meanWOB_Progressive = 0; promedio WOB tipos Progressive
         meanWOB_Hyperact = 0; promedio WOB tipos Hyperact
         meanWOB_Slow = 0; promedio WOB tipos Slow
         meanWOB_Static = 0; promedio WOB tipos Static
         meanWOB_NC = 0; promedio WOB tipos NC
         
         meanVAP_Acnt = 0; counter para tipos Progressive
         meanVAP_Bcnt = 0; counter para tipos Hyperact
         meanVAP_Ccnt = 0; counter para tipos Slow
         meanVAP_Dcnt = 0; counter para tipos Static
         meanVAP_NCcnt = 0; counter para tipos NC
         ;desde aqui descomenté -Susana
         for h = 0, (np-1) do begin
          case 1 of
            (vclVectTipo[h] eq 'PR'): begin
              meanVAP_Progressive += VAPvect[h]
              meanVCL_Progressive += VCLvect[h]
              meanVSL_Progressive += vslVect[h]
              meanLIN_Progressive +=  LINvect[h]
              meanSTR_Progressive +=  STRvect[h]
              meanWOB_Progressive +=  WOBvect[h]
              ;meanALH_Progressive += ALHvect[h]
              meanVAP_Acnt += 1
              end
            (vclVectTipo[h] eq 'HY'): begin
              meanVAP_Hyperact += VAPvect[h]
              meanVCL_Hyperact += VCLvect[h]
              meanVSL_Hyperact += vslVect[h]
              meanLIN_Hyperact += LINvect[h]
              meanSTR_Hyperact +=  STRvect[h]
              meanWOB_Hyperact +=  WOBvect[h]
              ;meanALH_Hyperact += ALHvect[h]
              meanVAP_Bcnt += 1
              end
            (vclVectTipo[h] eq 'Slow'): begin
              meanVAP_Slow += VAPvect[h]
              meanVCL_Slow += VCLvect[h]
              meanVSL_Slow += vslVect[h]
              meanLIN_Slow += LINvect[h]
              meanSTR_Slow +=  STRvect[h]
              meanWOB_Slow +=  WOBvect[h]
              ;meanALH_Slow += ALHvect[h]
              meanVAP_Ccnt += 1
              end
            (vclVectTipo[h] eq 'Static'): begin
              meanVAP_Static += VAPvect[h]
              meanVCL_Static += VCLvect[h]
              meanVSL_Static += vslVect[h]
              meanLIN_Static += LINvect[h]
              meanSTR_Static +=  STRvect[h]
              meanWOB_Static +=  WOBvect[h]
              ;meanALH_Static += ALHvect[h]
              meanVAP_Dcnt += 1
              end
            ;(vclVectTipo[h] eq 'NC') or (vclVectTipo[h] eq 'NCpp') or (vclVectTipo[h] eq 'NCfa') or (vclVectTipo[h] eq 'NCtt'): begin
            (vclVectTipo[h] eq 'NC'): begin
              meanVAP_NC += VAPvect[h]
              meanVCL_NC += VCLvect[h]
              meanVSL_NC += vslVect[h]
              meanLIN_NC += LINvect[h]
              meanSTR_NC +=  STRvect[h]
              meanWOB_NC +=  WOBvect[h]
              ;meanALH_NC += ALHvect[h]
              meanVAP_NCcnt += 1
              end
           endcase
         endfor
         ;hasta aqui descomenté--Susana
         meanVAP_Progressive /= meanVAP_Acnt
         meanVAP_Hyperact /= meanVAP_Bcnt
         meanVAP_Slow /= meanVAP_Ccnt
         meanVAP_Static /= meanVAP_Dcnt
         meanVAP_NC /= meanVAP_NCcnt
         
         meanVCL_Progressive /= meanVAP_Acnt
         meanVCL_Hyperact /= meanVAP_Bcnt
         meanVCL_Slow /= meanVAP_Ccnt
         meanVCL_Static /= meanVAP_Dcnt
         meanVCL_NC /= meanVAP_NCcnt
         
         meanVSL_Progressive /= meanVAP_Acnt
         meanVSL_Hyperact /= meanVAP_Bcnt
         meanVSL_Slow /= meanVAP_Ccnt
         meanVSL_Static /= meanVAP_Dcnt
         meanVSL_NC /= meanVAP_NCcnt
         
         meanLIN_Progressive /= meanVAP_Acnt
         meanLIN_Hyperact /= meanVAP_Bcnt
         meanLIN_Slow /= meanVAP_Ccnt
         meanLIN_Static /= meanVAP_Dcnt
         meanLIN_NC /= meanVAP_NCcnt
         
         meanSTR_Progressive /= meanVAP_Acnt
         meanSTR_Hyperact /= meanVAP_Bcnt
         meanSTR_Slow /= meanVAP_Ccnt
         meanSTR_Static /= meanVAP_Dcnt
         meanSTR_NC /= meanVAP_NCcnt
         
         meanWOB_Progressive /= meanVAP_Acnt
         meanWOB_Hyperact /= meanVAP_Bcnt
         meanWOB_Slow /= meanVAP_Ccnt
         meanWOB_Static /= meanVAP_Dcnt
         meanWOB_NC /= meanVAP_NCcnt
         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
         
         
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         
         ; MOR - comment out old path for saving, try path where images are saved instead 
         ;;write_tiff, s_getPathForSystem() + 'VCL_trajectory.tiff', outImage_1
         ;;write_tiff, tempFileName + 'VCL_trajectory.tiff', outImage_1
         ;write_tiff, tempFileName + 'Motil_trajectory_Raton.tiff', outImage_1
         write_tiff, tempFileName + 'Motil_trajectory_Raton'+nom+'.tiff', outImage_1
         
         ; MOR - destroy objects created for plotting to free up memory
         obj_destroy, [oBuffer, oView]
         
         ; MOR - add creation of VSL histogram - BEGIN
         ;path = tempFileName + 'VSL_Histo_Raton.tiff'
         path = tempFileName + 'VSL_Histo_Raton'+nom+'.tiff'
         
         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 ;Aqui reemplazo ne por eq, es decir envio al histograma todos los trackobjects menos los reasignados
         if(whClassified[0] ne -1) then begin; Susana1
          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
;         vect = vslVect ; MOR - commented out previous code
          ;;name = "rectilinear velocity VSL_Raton [um s-1]"
          ;name = "rectilinear velocity VSL_Raton [um s-1]"+nom
          name = "rectilinear velocity VSL_Raton [um s-1]"+'_z'+strcompress(STRING(zPos),/rem)
          
          
          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
          CEDAI_histogramPlot_VSL, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName
          ; MOR - add creation of VSL histogram - END
          ;CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVSL_OK = 1
         endif; Susana1
         ;endif else begin; Susana1
            ;
         ;endelse; Susana1
         
         
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;;path = s_getPathForSystem() + 'VCL_Histo.tiff'
         ;path = tempFileName + 'VCL_Histo_Raton.tiff'
         ;path = tempFileName + 'VCL_Histo_Raton_zslice'+strcompress(STRING(zPos),/rem)+'.tiff'
         path =  tempFileName + 'VCL_Histo_Raton'+nom+'.tiff'
        
         v_histogVCL_OK = 0 ; Susana - controlar si existe histograma VCL de clasificados no reasignados
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
         ; Susana - A B C o D, no reasignado
         if(whClassified[0] ne -1) then begin; Susana2
          vect = VCLvect[whClassified]
          ;name = "curvilinear velocity VCL_Raton [um s-1]"+nom
          name = "curvilinear velocity VCL_Raton [um s-1]"+'_z'+strcompress(STRING(zPos),/rem)
          
          linesFlag = 1
          CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVCL_OK = 1
         endif; Susana2
         ;endif else begin; Susana2
            ;nada
            ;El problema de no haber alternativa es que no crea imágenes como VCL_Histo.tiff
         ;endelse; Susana2
         
         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
         
         ;Susana INICIO    Guardando Recuentos y Porcentajes Progressive Hyperact Slow y Static   de  todos datos PChenlo--------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna Progressive 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom2 = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
         ;;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
         ;;name = 'DatosPChenlo_2011_'+nom ;incluyo nombre ultima carpeta en nombre archivo  comento porque no incluiré el nombre de esta muestra en el nombre de archivo
         ;name = 'Datos_Raton_2011'
         name = 'Datos_Raton_2011_'+nom2
         
         ;Susana - reemplazo tempFileName INICIO
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         fileMovVars = strCompress(nomFolder + name + '_MVars.dat', /remove)   ;Susana, guardo movVariables
         ;Susana - reemplazo tempFileName FIN
         
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         get_lun, unitPCh
         OPENR, unitPCh, file, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitPCh, file
           ;printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#PR'+STRING(9B)+'#HY'+STRING(9B)+'#Slow'+STRING(9B)+'#Static'+STRING(9B)+'#NC'+STRING(9B)+'%PR'+STRING(9B)+'%HY'+STRING(9B)+'%Slow'+STRING(9B)+'%Static';
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#PR'+STRING(9B)+'#HY'+STRING(9B)+'#Slow'+STRING(9B)+'#Static'+STRING(9B)+'#NC'+STRING(9B)+'%PR'+STRING(9B)+'%HY'+STRING(9B)+'%Slow'+STRING(9B)+'%Static';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
           
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         
         ;openW, unitPCh, file
         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#PR'+STRING(9B)+'#HY'+STRING(9B)+'#Slow'+STRING(9B)+'#Static'+STRING(9B)+'#NC'+STRING(9B)+'%PR'+STRING(9B)+'%HY'+STRING(9B)+'%Slow'+STRING(9B)+'%Static';
         ;recuentos
         aux = where(vclVectTipo eq 'PR', tipoA)
         aux = where(vclVectTipo eq 'HY', tipoB)
         aux = where(vclVectTipo eq 'Slow', tipoC)
         aux = where(vclVectTipo eq 'Static', tipoD)
         aux = where(vclVectTipo eq 'NC', tipoNC)
         ;porcentajes
         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
         ;print,tipoA+tipoB+tipoC+tipoD  corresponde a vect_elements
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         ;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana FIN    Guardando Recuentos y Porcentajes Progressive Hyperact Slow y Static   de  todos datos PChenlo--------------
         
         ;dummy = where(vect ge veloLimitsVCL[6], count)
         dummy = where(vclVectTipo eq 'PR', count) ;Susana
         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_0 = 'PR:'
         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[4] and vect lt veloLimitsVCL[5], count)   ;Susana
         dummy = where(vclVectTipo eq 'HY', count) ;Susana
         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_1 = 'HY:' 
         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[2] and vect lt veloLimitsVCL[3], count)   ;Susana
         dummy = where(vclVectTipo eq 'Slow', count) ;Susana
         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_2 = 'Slow:'
         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[0] and vect lt veloLimitsVCL[1], count)   ;Susana
         dummy = where(vclVectTipo eq 'Static', count) ;Susana
         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_3 = 'Static:'
         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         ; MOR - comment out all previously defined save paths
         ;fileName = tempFileName + 'Motil_Text_Raton.tiff'
         fileName = tempFileName + 'Motil_Text_Raton'+nom+'.tiff'
         
         
         ;SUSANA - Contabilizando trackobjects NC (No Clasificados) 
         dummy = where(vclVectTipo eq 'NC', count) ;Susana
         ;string_21 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_21 = strcompress( string(fix(count)), /remove_all) + ' sperms'
         ;string_23 = 'NC       :'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - INICIO
         string_23 =strcompress('#Traj.Circ.:' + string(fix(cuentaCirc)), /remove_all) + '(LIN<50) '+ '#NC:'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - FIN
         
         ;filename2 = nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])  ;Susana incluye nombre de muestra en informe para evitar error al ver informe, borrar 
         ;string_27 = string(100.*count / vect_elements, format = '(F6.2)') + ' %' ;NC no estan incluidos en el total de espermatozoides
         CEDAI_textPlot, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11,string_21,string_23, nom
         
         ; MOR - 12Feb 2011- average number of objects found per time
         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
         totalNumObjs.avg = stats[0]*conFactor
         totalNumObjs.std = sqrt(stats[1]*conFactor)
         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
;         totalNumObjs.se = sqrt(totalNumObjs.avg*conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         totalNumObjs.se = sqrt(totalNumObjs.avg) 
         
         string_0 = 'Concentration Statistics Summary_Raton';_zslice'+strcompress(STRING(zPos))
;         string_1 = strcompress('Frames ') 
         string_2 = strcompress('Mean: ') 
;         string_3 = strcompress('SDev ') 
         string_4 = strcompress('Standard Error: ') 
;         string_5 = strcompress('Median ') 
;         string_6 = strcompress('Min ') 
;         string_7 = strcompress('Max ') 
         
         string_8 = strcompress(string(totalNumObjs.number, format = '(D6.2)'), /rem) 
         string_9 = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem) 
         string_10 = strcompress(string(totalNumObjs.std, format = '(I6.2)'), /rem) 
         string_11 = strcompress(string(totalNumObjs.se, format = '(D6.2)'), /rem) 
         string_12 = strcompress(string(totalNumObjs.med, format = '(I6.2)'), /rem) 
         string_13 = strcompress(string(totalNumObjs.mini, format = '(I6.2)'), /rem) 
         string_14 = strcompress(string(totalNumObjs.maxi, format = '(I6.2)'), /rem) 
         
         string_15 = strcompress('[10E6/mL]', /rem) 
       
         ; MOR - comment out all previously defined save paths
;         fileName = tempFileName + 'Concentration_Text.tiff'     
         
;         CEDAI_textPlot_Count, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7,string_8, string_9, string_10, string_11, string_12, string_13, string_14
         ;fileName = tempFileName + 'Concentration_Text_Short_Raton.tiff'  
         fileName = tempFileName + 'Concentration_Text_Short_Raton'+nom+'.tiff'
         
         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
         
         ; MOR - 12Feb2011 write total count data to .dat file 
         dum = max(timeTrack, maxI)
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
         
        ;name = 'Concentration_Raton'
        name = 'Concentration_Raton'+nom
         
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         ;printF, Unit, 'Time' +string(9B)+ 'Count'+STRING(13B)
         printF, Unit, 'zslice'+STRING(9B)+'Time' +string(9B)+ 'Count'+STRING(13B)
         indU = uniq(*numObjs.pTValues)       
         for i = 0, n_elements(indU)-1 do begin
         ;strcompress(string((*VAPStruct.ptValues)[j]),/rem)
            ;temp = strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            printF, Unit,temp
         endfor
         close, Unit
         FREE_LUN, Unit
         
         ; MOR - save results in the same directory as images - BEGIN
        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_Short.bmp'
         
;         imageLeft =tempFileName + 'Motil_trajectory_Raton.tiff'
;         imageRight =  tempFileName + 'VCL_Histo_Raton.tiff'
;         imageRight2 =  tempFileName + 'VSL_Histo_Raton.tiff'
;         textRight = tempFileName + 'Motil_Text_Raton.tiff'
;         fileName = tempFileName + 'Motil_Short_Raton.tiff' 
         imageLeft =tempFileName + 'Motil_trajectory_Raton'+nom+'.tiff'
         ;imageRight =  tempFileName + 'VCL_Histo_Raton_zslice'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_Histo_Raton'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo_Raton'+nom+'.tiff'
         textRight = tempFileName + 'Motil_Text_Raton'+nom+'.tiff'
         fileName = tempFileName + 'Motil_Short_Raton'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
;        ; textLeft = tempFileName + 'Concentration_Text.tiff' 
         ;textLeft = tempFileName + 'Concentration_Text_Short_Raton.tiff' 
         textLeft = tempFileName + 'Concentration_Text_Short_Raton'+nom+'.tiff'
         
         
;;         CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft
         CEDAI_combineImages_Motil_Short, background, imageLeft, TextRight, fileName, textLeft
         
        ; MOR - save results in the same directory as images - BEGIN
        ; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility.bmp'
         
;         imageLeft =tempFileName + 'Motil_trajectory_Raton.tiff'
;         imageRight =  tempFileName + 'VCL_Histo_Raton.tiff'
;         imageRight2 =  tempFileName + 'VSL_Histo_Raton.tiff'
;         textRight = tempFileName + 'Motil_Text_Raton.tiff'
;         fileName = tempFileName + 'Motil_new_Raton.tiff' 
         imageLeft =tempFileName + 'Motil_trajectory_Raton'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_Histo_Raton'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo_Raton'+nom+'.tiff'
         textRight = tempFileName + 'Motil_Text_Raton'+nom+'.tiff'
         fileName = tempFileName + 'Motil_new_Raton'+nom+'.tiff' 

         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana 
         endif; Susana 
         
         
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         
         
         CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft
         ;CEDAI_combineImages_motil, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft, v_histogVSL_OK    ; Susana evalúa si existe histograma
         
         ;SUSANA - Incluyo VAP promedio por grupos LO ESTOY INTENTANDO, AUN NO CREO EL HISTOGRAMA PENDIENTES TAMBIEN HISTOGRAMAS STR Y WOB
         ;SUSANA - add creation of ALH histogram - BEGIN
         ;veloLimitsVCL = [0,1., 1.,5., 5.,40., 40.,10000.]
         veloLimitsVCL = [0,50., 50.,200., 200.,400., 400.,10000.] ; no tengo rangos para ALH :( 
         ;path = tempFileName + 'histo_ALH_Raton.tiff'
         path = tempFileName + 'histo_ALH_Raton'+nom+'.tiff'
         
         vect = ALHvect
         ;;name = "ALH Amplitude of Lateral Head displacement_Raton [um]"
         ;name = "ALH Amplitude of Lateral Head displacement_Raton [um]"+nom
         name = "ALH Amplitude of Lateral Head displacement_Raton [um]"+'_z'+strcompress(STRING(zPos),/rem)
         
         
         linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
         ;CEDAI_histogramPlot_ALH, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
         ;Falta generar un graficador de histograma para ALH o llamar internamente el GraphicModel ALH
         ;SUSANA - add creation of VSL histogram - END
         
         
         ;fileName = tempFileName + 'Motil_MovVariables_Text_Raton.tiff' ; MovementsVariables
         fileName = tempFileName + 'Motil_MovVariables_Text_Raton'+nom+'.tiff' ; MovementsVariables
         
         string_0 = 'PR:'
         ;string_4 = string(meanVAP_Progressive, format = '(F6.2)') + ' um/s'
         string_4 = string(meanVAP_Progressive, format = '(F6.1)')
         string_8 = strcompress(string(fix(meanLIN_Progressive), format = '(F6.1)'), /remove_all)
         string_8_1 = strcompress(string(fix(meanSTR_Progressive), format = '(F6.1)'), /remove_all)
         string_8_2 = strcompress(string(fix(meanWOB_Progressive), format = '(F6.1)'), /remove_all)
         string_8_3 = strcompress(string(fix(meanVCL_Progressive), format = '(F6.1)'), /remove_all)
         string_8_4 = strcompress(string(fix(meanVSL_Progressive), format = '(F6.1)'), /remove_all)
         
         string_1 = 'HY:' 
         ;string_5 = string(meanVAP_Hyperact, format = '(F6.2)') + ' um/s'
         string_5 = string(meanVAP_Hyperact, format = '(F6.1)')
         string_9 = strcompress(string(fix(meanLIN_Hyperact), format = '(F6.1)'), /remove_all)
         string_9_1 = strcompress(string(fix(meanSTR_Hyperact), format = '(F6.1)'), /remove_all)
         string_9_2 = strcompress(string(fix(meanWOB_Hyperact), format = '(F6.1)'), /remove_all)
         string_9_3 = strcompress(string(fix(meanVCL_Hyperact), format = '(F6.1)'), /remove_all)
         string_9_4 = strcompress(string(fix(meanVSL_Hyperact), format = '(F6.1)'), /remove_all)
         
         string_2 = 'Slow:'
         ;string_6 = string(meanVAP_Slow, format = '(F6.2)') + ' um/s'
         string_6 = string(meanVAP_Slow, format = '(F6.1)')
         string_10 = strcompress(string(fix(meanLIN_Slow), format = '(F6.1)'), /remove_all)
         string_10_1 = strcompress(string(fix(meanSTR_Slow), format = '(F6.1)'), /remove_all)
         string_10_2 = strcompress(string(fix(meanWOB_Slow), format = '(F6.1)'), /remove_all)
         string_10_3 = strcompress(string(fix(meanVCL_Slow), format = '(F6.1)'), /remove_all)
         string_10_4 = strcompress(string(fix(meanVSL_Slow), format = '(F6.1)'), /remove_all)
         
         string_3 = 'Static:'  ; Type Static  Talvez no deba incluirlo
         ;string_7 = string(meanVAP_Static, format = '(F6.2)') + ' um/s'
         string_7 = string(meanVAP_Static, format = '(F6.1)')
         string_11 = strcompress(string(fix(meanLIN_Static), format = '(F6.1)'), /remove_all)
         string_11_1 = strcompress( string(fix(meanSTR_Static), format = '(F6.1)'), /remove_all)
         string_11_2 = strcompress( string(fix(meanWOB_Static), format = '(F6.1)'), /remove_all)
         string_11_3 = strcompress(string(fix(meanVCL_Static), format = '(F6.1)'), /remove_all)
         string_11_4 = strcompress(string(fix(meanVSL_Static), format = '(F6.1)'), /remove_all)
         
         string_21 = strcompress('VAP')
         string_23 = strcompress('VSL')
         string_27 = strcompress('VCL')
         string_28 = strcompress('LIN')
         string_29 = strcompress('STR')
         string_30 = strcompress('WOB')
         string_31 = ' um/s'
         string_32 = ' %'
         CEDAI_textPlotMotil, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         
         
         ;Susana - incluyo archivo de variables del movimiento - INICIO
         get_lun, unitMVars
         OPENR, unitMVars, fileMovVars, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitMVars, fileMovVars
           printF, unitMVars,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'meanVAP_Progressive'+STRING(9B)+'meanLIN_Progressive'+STRING(9B)+'meanSTR_Progressive'+STRING(9B)+'meanWOB_Progressive'+STRING(9B)+'meanVCL_Progressive'+STRING(9B)+'meanVSL_Progressive'+STRING(9B)+'meanVAP_Hyperact'+STRING(9B)+'meanLIN_Hyperact'+STRING(9B)+'meanSTR_Hyperact'+STRING(9B)+'meanWOB_Hyperact'+STRING(9B)+'meanVCL_Hyperact'+STRING(9B)+'meanVCL_Progressive'+STRING(9B)+'meanVSL_Hyperact'+STRING(9B)+'meanVAP_Slow'+STRING(9B)+'meanLIN_Slow'+STRING(9B)+'meanSTR_Slow'+STRING(9B)+'meanWOB_Slow'+STRING(9B)+'meanVCL_Slow'+STRING(9B)+'meanVSL_Slow'+STRING(9B)+'meanVAP_Static'+STRING(9B)+'meanLIN_Static'+STRING(9B)+'meanSTR_Static'+STRING(9B)+'meanWOB_Static'+STRING(9B)+'meanVCL_Static'+STRING(9B)+'meanVSL_Static' ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
           ;Susana - incluyo archivo de variables del movimiento - FIN
          ENDIF ELSE BEGIN
           close, unitMVars
           FREE_LUN, unitMVars
           openW, unitMVars, fileMovVars,/APPEND
          ENDELSE
         ;printF, unitMVars,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B),'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'Type'+STRING(9B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanVAP_Progressive),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanLIN_Progressive),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanSTR_Progressive),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanWOB_Progressive),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanVCL_Progressive),/REMOVE_ALL)+STRING(9B)+strcompress(meanVSL_Progressive,/REMOVE_ALL)+STRING(9B)+strcompress(meanVAP_Hyperact,/REMOVE_ALL)+STRING(9B)+strcompress(meanLIN_Hyperact,/REMOVE_ALL)+STRING(9B)+strcompress(meanSTR_Hyperact,/REMOVE_ALL)+STRING(9B)+strcompress(string(meanWOB_Hyperact),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanVCL_Hyperact),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanVCL_Progressive),/REMOVE_ALL)+STRING(9B)+strcompress(meanVSL_Hyperact,/REMOVE_ALL)+STRING(9B)+strcompress(meanVAP_Slow,/REMOVE_ALL)+STRING(9B)+strcompress(meanLIN_Slow,/REMOVE_ALL)+STRING(9B)+strcompress(meanSTR_Slow,/REMOVE_ALL)+STRING(9B)+strcompress(string(meanWOB_Slow),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanVCL_Slow),/REMOVE_ALL)+STRING(9B)+strcompress(string(meanVSL_Slow),/REMOVE_ALL)+STRING(9B)+strcompress(meanVAP_Static,/REMOVE_ALL)+STRING(9B)+strcompress(meanLIN_Static,/REMOVE_ALL)+STRING(9B)+strcompress(meanSTR_Static,/REMOVE_ALL)+STRING(9B)+strcompress(meanWOB_Static,/REMOVE_ALL)+STRING(9B)+strcompress(meanVCL_Static,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL)+STRING(9B)+strcompress(meanVSL_Static,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitMVars, temp
         close, unitMVars
         FREE_LUN, unitMVars
         ;Susana - incluyo archivo de variables del movimiento - FIN
         
         
         ; MOR - save results in the same directory as images - BEGIN
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         
         ;imageLeft =tempFileName + 'Motil_trajectory_Raton.tiff' ;Talvez esta cambie después ??
         imageLeft =tempFileName + 'Motil_trajectory_Raton'+nom+'.tiff' ;Talvez esta cambie después ??
;         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_MovVariables.bmp'

;         imageRight =  tempFileName + 'VCL_Histo_Raton.tiff' ;'histo_ALH.tiff'
;         imageRight2 =  tempFileName + 'VSL_Histo_Raton.tiff'
;         textRight = tempFileName + 'Motil_MovVariables_Text_Raton.tiff'
;         fileName = tempFileName + 'Motil_MovVariables_Raton.tiff' 
         imageRight =  tempFileName + 'VCL_Histo_Raton'+nom+'.tiff' ;'histo_ALH.tiff'
         imageRight2 =  tempFileName + 'VSL_Histo_Raton'+nom+'.tiff'
         textRight = tempFileName + 'Motil_MovVariables_Text_Raton'+nom+'.tiff'
         fileName = tempFileName + 'Motil_MovVariables_Raton'+nom+'.tiff' 

         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana 
         endif; Susana 
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, TextRight, filename,1
      endcase ;Track Motil Raton - FIN
      
      'Track RoundCells':begin ; basadio en Track Motil
         ;print, 'correctly entered track motility after adding the graphic model'
         veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010 EN USO CEDAI
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
         s_informe = 'RoundCells'
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
         ; do not include in analysis if started after the xx frame in time
         ; number of frames
         minTimeF = 10 ;minTimeF = 5;  will be value in [frames]
         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]  Susana - frame en que aparece TO
         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
         minNumFramesTracked = floor((tEnd-tStart)*0.90);  value in [frames]
         ; how close to the border the object is 
         closeBorder = 9; [pixels] -- this value is the same as max_Dist in C_sROIParam_ObjTrackObject.pro    
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_z'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         
         ; write to file the track objects overallType of classification
         ;name = 'overallTypeTOs_'
         name = 'overallTypeTOs_'+nom+s_informe
         
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U1
         openW, U1, file
         ;printF, U1, 'TO'+STRING(9B)+'TypeNC'
         printF, U1, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'TypeNC'
         ; threshold area [in pixels] that the track covers
         areaThreshold = 2; this is 2x2 pixels large
         ; MOR - determine cutoffs for not including TOs in analysis - END
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]
         
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
;         sb = 50 ;Susana - sb in pixels
;         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
;         pixsSB  = floor(1.0*sb*xReal)
;         boxS = bytarr(3, pixsSB, 5)
;         boxS[0,*,*] = 0
;         boxS[1, *,*] = 0
;         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - INICIO
         sb = 50 ; sb in micrometers 50
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - FIN
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange

         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
        
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
                                
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         
         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])
         name = 'CoordenadasTOs_'+nom+s_informe ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         get_lun, unit
         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
         ;;;;;;; MOR - temporarily printint the color code -----
         ;printF, Unit, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         printF, Unit, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         ;;printF, Unit, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         vslMaxim = 0
         vslMaximPos = 0
         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
         ; --- Susana Creando vector para VSL --- INICIO
         ; Susana - aqui falta proteger si no hay track objects, es decir, si (*state.poCurrTrackGroup)->getObjNum() == 0
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs  ; agregar if((*state.poCurrTrackGroup)->getObjNum() gt 0)  para protección cuando no existan RoundCells
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         umbralVCL = 50 ;10
         ; MOR - 9Feb2011 - limits were found using pixels April 2010
         veloLimitsVSL = [2.5,4.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC
         ;veloLimitsVSL = [24.5,72.8]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC - Ratón
         ; --- Susana Creando vector para VSL --- FIN
         
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
         vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad A,B,C y D
         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         ; MOR - add vector for storing vsl values for the histogram - BEGIN
         vslVect =  make_array((*state.poCurrTrackGroup)->getObjNum());
         ; MOR - add vector for storing vsl values for the histogram - END
         
         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
         
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
         
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - BEGIN
         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - END 
         
         ; MOR - 12Feb2011 - get concentration in time
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
                 
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           ; Susana evaluando VSL -- INICIO
           col = [0,0,255] ;inmoviles azules "Tipo D"
           thickk = 4;4
           vclVectTipo[i] = 'D' ;Susana
              
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
           vslTotal = vslVect[i] ;MOR changing calculation
           
           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
           ; Susana evaluando VSL -- FIN
           
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
                      
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
           ; check at what time/frame the object started being tracked
           firstAppeared = (*vclStruct.pTValues)[0]
           
           ; check how many frames the object was tracked
           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0]);
           
           ; check that all elements of the TO are not 'too close' to the border
           ;2D case --- @todo - need to figure out the 3D case
           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
           countBorder = 0
           if(firstAppeared gt 0) then begin
               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
               xO = (*vclStruct.pXYZValues)[0,0]/xReal
               yO = (*vclStruct.pXYZValues)[0,1]/yReal
               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
           endif
           
           ; create a bounding box of the track path -- convert trajectory to pixels
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
            ;areaThreshold = 2; this is 2x2 pixels large
            xBox = 10000
            yBox = 10000
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
               ; find bounding center of mass values in each dimension
               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
               ; represent in whole pixels rather than subpixel positions
               xMin = floor(xMin)
               xMax = ceil(xMax)
               yMin = floor(yMin)
               yMax = ceil(yMax)
               ; check in each dimension what the number of pixels are
               xBox = xMax - xMin 
               yBox = yMax - yMin
               
            endif
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
           
           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
           ;ALH , creo que no lo informo aùn no
           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)

           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
           
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
               ;if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
;               if(n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
;               endif else begin
                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
;               endelse
             
             ;SUSANA - LIN - INICIO
             ;LIN
             ; MOR @todo - need to update the functions calling LIN, WOB, STR in C_sTrackGroupObject function in order to have
             ; the correct access to these calculations 
             
             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
             if (VCLvect[i] ne 0) then begin
               ;LINvect[i] = vslVect[i]/VCLvect[i]
               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
               ;WOBvect[i] = VAPvect[i]/VCLvect[i]
               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               LINvect[i] = 0
               WOBvect[i] = 0
             endelse
             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
             ; MOR STR calculation - BEGIN
             if (VAPvect[i] ne 0) then begin
               ;STRvect[i] = vslVect[i]/VAPvect[i]
               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               STRvect[i] = 0
            endelse
             ; MOR STR calculation - END
             case 1 of
             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
            ; MOR - 6Sept2010 - previous check for 'NC' classification
            ;((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) ): begin
               ; MOR - store reason for 'NC' - BEGIN
               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
                     print,'TO='+string(i)+' '+string(VCLvect[i])+'    '+string(vslTotal)+'    '+string(VAPvect[i])
                     overallType[i] = 1
                     col = [0,255,255]
                     thickk = 10
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1                     
                     break 
               endif
               
               if ((countBorder gt 0)) then begin
                     print,'TO='+string(i)+' countBorder'
                     overallType[i] = 4
                     col = [0,255,0]
                     thickk = 10
                     ;vclVectTipo[i] = 'NCpp' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     cuentaTOsNC += 1                      
                     break
               endif
               
               if( (firstAppeared ge minTime)) then begin 
                     print,'TO='+string(i)+' firstAppeared >= '+string(minTime)
                     overallType[i] = 2
                     col = [0,0,255]
                     thickk = 10;
                     ;vclVectTipo[i] = 'NCfa' 
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               if ((timesTracked le minNumFramesTracked)) then begin 
                     print,'TO='+string(i)+' timesTracked <= '+string(minNumFramesTracked)
                     overallType[i] = 3
                     col = [255,255,0];col = [120,255,255];[255,255,0] ;Progresivos lentos amarillos "Tipo B"
                     thickk = 10;2
                     ;vclVectTipo[i] = 'NCtt' 
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               ; MOR - store reason for 'NC' - END
             end
            (vslTotal le 20): begin
              col = [0,0,255] ;Celulas redondas se mueven poco por efecto del medio
              thickk = 3
              vclVectTipo[i] = 'D' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado
              end
            VCLvect[i] le 20: begin
              col = [0,255,0]
              thickk = 10;4
              vclVectTipo[i] = 'C' ;Susana
              end
            VCLvect[i] gt 20: begin
              col = [0,255,255]
              thickk = 10;4
              vclVectTipo[i] = 'A' ;Susana
              end
           endcase
           
;           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
;           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'D') ) then begin  
;            ;and (STRvect[i] lt 50) 
;            cuentaCirc += 1
;            thickk = 20
;           endif
;           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
           
           if (vslTotal gt vslMaxim) then begin 
            vslMaxim = vslTotal
            vslMaximPos = i
           endif
           
           
           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
             np = (*state.poCurrTrackGroup)->getObjNum()
             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
             miX = (*VAPStruct.pXYZValues)[*,0]
             miY = (*VAPStruct.pXYZValues)[*,1]
             miZ = (*VAPStruct.pXYZValues)[*,2]
             miT = (*VAPStruct.pTValues)
             np = n_elements(miX)
             for j = 0, (np-1) do begin
;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
              ;if(vclVectTipo[i] ne 'NC') then begin
               ;temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(10B) ;para IGOR
               printF, Unit,temp
              ;endif
             endfor
           ; MOR - get entire time vector
           timeTrack[i] = n_elements(*VAPStruct.ptValues)
           
           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
           if  (vclVectTipo[i] ne 'NC') then begin
           ; MOR - 05Jul2010
           ; convert track xyz to pixels for plotting against image
              if(vclVectTipo[i] eq 'D') then begin
                ;oSym = obj_new('IDLgrSymbol', data = 1, size = 5.5, thick = thickk,ALPHA_CHANNEL = 0.7); Dot de mayor tamaño
                oSym = obj_new('IDLgrSymbol', data = 1, size = 1, thick = 1);data3 = Period (Dot)
              endif else begin
                oSym = obj_new('IDLgrSymbol', data = 1, size = 5.5, thick = thickk,ALPHA_CHANNEL = 0.7); Dot de mayor tamaño  arreglo para verlas grandes
              endelse
              
              radius = 10 ; circle radius
              points = (2 * !PI /99.0 * FINDGEN(100)) ; used for draw circles
              xParamsROI_circle = (*vclStruct.pXYZValues)[0,0] + radius * cos(points)
              yParamsROI_circle = (*vclStruct.pXYZValues)[0,1] + radius * sin(points)
              
              ;oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oModel->add, oPlot
             
             ; Susana - add TO number - INICIO
;             ;;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' TO_'+string(i, format = '(F6.0)')),color = [0,255,255], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' .   '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [15, 15], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;             oPlot = obj_new('IDLgrText', STRINGS = strcompress(' ..     '+string(i, format = '(I6)'),/rem),color = [255,0,0], CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;             oModel->add, oPlot
             ; Susana - add TO number - FIN
           endif
           
           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
           name = 'CoordsTOs_'+nom+s_informe
           file = strCompress(tempFileName + name + '.dat', /remove)
           
           get_lun, U
            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            ;printF, U, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
            printF, U, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
           
           ; MOR - write to file the track objects overallType of classification - BEGIN    
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           printF, U1,temp 
           ; MOR - write to file the track objects overallType of classification - END
         endfor
         
         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         ; MOR - close up file with overalltype classification - BEGIN
         close, U1
         FREE_LUN, U1
         ; MOR - close up file with overalltype classification - END         
         

         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         name = 'VelocityData_'+nom+s_informe ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;printF, Unit, 'TO'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR
         printF, Unit, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           ;;temp = strcompress(string(i),/REMOVE_ALL)+' '+strcompress(string(VCLvect[i]),/REMOVE_ALL)+' '+strcompress(string(vslVect[i]),/REMOVE_ALL)+' '+strcompress(string(VAPvect[i]),/REMOVE_ALL)+STRING(13B)
           ;temp = strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         
         
         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
         name = 'RoundCells_MovVars_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;printF, Unit, 'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'Type'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         printF, Unit, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'Type'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
         
           
         ;Susana INICIO --  Guardando #TrackObjects Reasignados
         name = 'NumTrackObjectsVSL_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit,transpose(vslReasign)
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN --  Guardando #TrackObjects Reasignados
                 
         
;         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
;         np = (*state.poCurrTrackGroup)->getObjNum()
;         meanVAP_A = 0; promedio tipos A
;         meanVAP_B = 0; promedio tipos B
;         meanVAP_C = 0; promedio tipos C
;         meanVAP_D = 0; promedio tipos D
;         meanVAP_NC = 0; promedio tipos NC
;         
;         meanVCL_A = 0; promedio tipos A
;         meanVCL_B = 0; promedio tipos B
;         meanVCL_C = 0; promedio tipos C
;         meanVCL_D = 0; promedio tipos D
;         meanVCL_NC = 0; promedio tipos NC
;         
;         meanVSL_A = 0; promedio tipos A
;         meanVSL_B = 0; promedio tipos B
;         meanVSL_C = 0; promedio tipos C
;         meanVSL_D = 0; promedio tipos D
;         meanVSL_NC = 0; promedio tipos NC
;         
;         meanLIN_A = 0; promedio LIN tipos A
;         meanLIN_B = 0; promedio LIN tipos B
;         meanLIN_C = 0; promedio LIN tipos C
;         meanLIN_D = 0; promedio LIN tipos D
;         meanLIN_NC = 0; promedio LIN tipos NC
;         
;         meanSTR_A = 0; promedio STR tipos A
;         meanSTR_B = 0; promedio STR tipos B
;         meanSTR_C = 0; promedio STR tipos C
;         meanSTR_D = 0; promedio STR tipos D
;         meanSTR_NC = 0; promedio STR tipos NC
;         
;         meanWOB_A = 0; promedio WOB tipos A
;         meanWOB_B = 0; promedio WOB tipos B
;         meanWOB_C = 0; promedio WOB tipos C
;         meanWOB_D = 0; promedio WOB tipos D
;         meanWOB_NC = 0; promedio WOB tipos NC
;         
;         meanVAP_Acnt = 0; counter para tipos A
;         meanVAP_Bcnt = 0; counter para tipos B
;         meanVAP_Ccnt = 0; counter para tipos C
;         meanVAP_Dcnt = 0; counter para tipos D
;         meanVAP_NCcnt = 0; counter para tipos NC
;         ;desde aqui descomenté -Susana
;         for h = 0, (np-1) do begin
;          case 1 of
;            (vclVectTipo[h] eq 'A'): begin
;              meanVAP_A += VAPvect[h]
;              meanVCL_A += VCLvect[h]
;              meanVSL_A += vslVect[h]
;              meanLIN_A +=  LINvect[h]
;              meanSTR_A +=  STRvect[h]
;              meanWOB_A +=  WOBvect[h]
;              ;meanALH_A += ALHvect[h]
;              meanVAP_Acnt += 1
;              end
;            (vclVectTipo[h] eq 'B'): begin
;              meanVAP_B += VAPvect[h]
;              meanVCL_B += VCLvect[h]
;              meanVSL_B += vslVect[h]
;              meanLIN_B += LINvect[h]
;              meanSTR_B +=  STRvect[h]
;              meanWOB_B +=  WOBvect[h]
;              ;meanALH_B += ALHvect[h]
;              meanVAP_Bcnt += 1
;              end
;            (vclVectTipo[h] eq 'C'): begin
;              meanVAP_C += VAPvect[h]
;              meanVCL_C += VCLvect[h]
;              meanVSL_C += vslVect[h]
;              meanLIN_C += LINvect[h]
;              meanSTR_C +=  STRvect[h]
;              meanWOB_C +=  WOBvect[h]
;              ;meanALH_C += ALHvect[h]
;              meanVAP_Ccnt += 1
;              end
;            (vclVectTipo[h] eq 'D'): begin
;              meanVAP_D += VAPvect[h]
;              meanVCL_D += VCLvect[h]
;              meanVSL_D += vslVect[h]
;              meanLIN_D += LINvect[h]
;              meanSTR_D +=  STRvect[h]
;              meanWOB_D +=  WOBvect[h]
;              ;meanALH_D += ALHvect[h]
;              meanVAP_Dcnt += 1
;              end
;            ;(vclVectTipo[h] eq 'NC') or (vclVectTipo[h] eq 'NCpp') or (vclVectTipo[h] eq 'NCfa') or (vclVectTipo[h] eq 'NCtt'): begin
;            (vclVectTipo[h] eq 'NC'): begin
;              meanVAP_NC += VAPvect[h]
;              meanVCL_NC += VCLvect[h]
;              meanVSL_NC += vslVect[h]
;              meanLIN_NC += LINvect[h]
;              meanSTR_NC +=  STRvect[h]
;              meanWOB_NC +=  WOBvect[h]
;              ;meanALH_NC += ALHvect[h]
;              meanVAP_NCcnt += 1
;              end
;           endcase
;         endfor
;         ;hasta aqui descomenté--Susana
;         meanVAP_A /= meanVAP_Acnt
;         meanVAP_B /= meanVAP_Bcnt
;         meanVAP_C /= meanVAP_Ccnt
;         meanVAP_D /= meanVAP_Dcnt
;         meanVAP_NC /= meanVAP_NCcnt
;         
;         meanVCL_A /= meanVAP_Acnt
;         meanVCL_B /= meanVAP_Bcnt
;         meanVCL_C /= meanVAP_Ccnt
;         meanVCL_D /= meanVAP_Dcnt
;         meanVCL_NC /= meanVAP_NCcnt
;         
;         meanVSL_A /= meanVAP_Acnt
;         meanVSL_B /= meanVAP_Bcnt
;         meanVSL_C /= meanVAP_Ccnt
;         meanVSL_D /= meanVAP_Dcnt
;         meanVSL_NC /= meanVAP_NCcnt
;         
;         meanLIN_A /= meanVAP_Acnt
;         meanLIN_B /= meanVAP_Bcnt
;         meanLIN_C /= meanVAP_Ccnt
;         meanLIN_D /= meanVAP_Dcnt
;         meanLIN_NC /= meanVAP_NCcnt
;         
;         meanSTR_A /= meanVAP_Acnt
;         meanSTR_B /= meanVAP_Bcnt
;         meanSTR_C /= meanVAP_Ccnt
;         meanSTR_D /= meanVAP_Dcnt
;         meanSTR_NC /= meanVAP_NCcnt
;         
;         meanWOB_A /= meanVAP_Acnt
;         meanWOB_B /= meanVAP_Bcnt
;         meanWOB_C /= meanVAP_Ccnt
;         meanWOB_D /= meanVAP_Dcnt
;         meanWOB_NC /= meanVAP_NCcnt
;         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
         
         
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         ; MOR - comment out old path for saving, try path where images are saved instead 
         write_tiff, tempFileName + 'RoundCells_trajectory'+nom+'.tiff', outImage_1
         ;aqui borraesta linea ? Susana. nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         
         ; MOR - destroy objects created for plotting to free up memory
         obj_destroy, [oBuffer, oView]
         
         ; MOR - add creation of VSL histogram - BEGIN
         path = tempFileName + 'VSL_Histo'+nom+s_informe+'.tiff'
         
         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
         if(whClassified[0] ne -1) then begin; Susana1
          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
;         vect = vslVect ; MOR - commented out previous code
          name = "rectilinear velocity VSL [um s-1]"+s_informe
          ;name = "rectilinear velocity VSL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          
          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
          CEDAI_histogramPlot_VSL, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName
          ; MOR - add creation of VSL histogram - END
          ;CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVSL_OK = 1
         endif; Susana1
         ;endif else begin; Susana1
            ;
         ;endelse; Susana1
         
         
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;path = s_getPathForSystem() + 'VCL_Histo.tiff'
         ;path = tempFileName + 'VCL_Histo.tiff'
         path = tempFileName + 'VCL_Histo'+nom+s_informe+'.tiff'
         
         v_histogVCL_OK = 0 ; Susana - controlar si existe histograma VCL de clasificados no reasignados
         whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
         if(whClassified[0] ne -1) then begin; Susana2
          ; Susana - A B C o D, no reasignado
          vect = VCLvect[whClassified]
          name = "curvilinear velocity VCL [um s-1]"+s_informe
          ;name = "curvilinear velocity VCL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          linesFlag = 1
          CEDAI_histogramPlot, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
          v_histogVCL_OK = 1
         endif; Susana2
         ;endif else begin; Susana2
            ;nada
            ;El problema de no haber alternativa es que no crea imágenes como VCL_Histo.tiff
         ;endelse; Susana2
         
         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
         
         ;Susana INICIO    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom2 = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
         nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
         ;name = 'DatosPChenlo_2011'
         name = 'DatosRoundCells_'+nom2+s_informe
         
         ;Susana - reemplazo tempFileName INICIO
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         ;Susana - reemplazo tempFileName FIN
         
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         get_lun, unitPCh
         OPENR, unitPCh, file, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitPCh, file
           ;printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         
         ;openW, unitPCh, file
         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
         ;recuentos
         aux = where(vclVectTipo eq 'A', tipoA)
         aux = where(vclVectTipo eq 'B', tipoB)
         aux = where(vclVectTipo eq 'C', tipoC)
         aux = where(vclVectTipo eq 'D', tipoD)
         aux = where(vclVectTipo eq 'NC', tipoNC)
         ;porcentajes
         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
         print,tipoA+tipoB+tipoC+tipoD  ;corresponde a vect_elements
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana FIN    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         
         
         ; MOR - 12Feb 2011- average number of objects found per time
         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
         totalNumObjs.avg = stats[0]*conFactor; recuento promedio, promedia recuento espermios en todos los tiempos
         totalNumObjs.std = sqrt(stats[1]*conFactor)
         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
         totalNumObjs.se = sqrt(totalNumObjs.avg*conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         
         ; corrijo concentración con recuento real
         dummy = where(vclVectTipo eq 'D', count) ;Susana
         ;totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         totalNumObjs.avg = count*conFactor
         totalNumObjs.se = sqrt(totalNumObjs.avg*conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         
         dummy = where(vclVectTipo eq 'D', count) ;Susana
         
;         string_9 = ''
;         string_1 = ''
;         string_5 = ''
;         string_10 = ''
;         string_3 = ''
;         string_6 = ''
;         string_11 = ''
;         string_3 = ''
;         string_7 = ''
         
         ;SUSANA- incluyo concentración -  1 cabeza/campo corresponde a 0.3143 millones/ml para campo 650.138umX483.436um en Makler 10prof - INICIO
         fileName = tempFileName + 'RoundCells_Text'+nom+'.tiff'
         ;string_3AA = strcompress('Conc.: ')
         string_3AA = strcompress('Round Cells:')
         string_7AA = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem)
         string_11AA = strcompress('10!U6!N/mL', /rem)
         
         ;CEDAI_textPlotOMS, filename, string_0, string_2, string_3, string_3AA, string_4, string_6, string_7, string_7AA, string_8, string_10, string_11, string_11AA,string_21,string_23, nom; ordenamiento de A+B
         CEDAI_textPlotOMS_Conc, fileName, string_3AA, string_7AA, string_11AA, nom; ordenamiento de A+B  - concentration
         
         
         fileName = tempFileName + 'Concentration_Text_Short'+nom+s_informe+'.tiff'
         
         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
         dummy = where(vclVectTipo eq 'NC', count) ;Susana
         
         ; MOR - 12Feb2011 write total count data to .dat file 
         dum = max(timeTrack, maxI)
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
         
         ;name = 'Concentration'
         name = 'Concentration'+nom+s_informe
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit, 'zslice'+STRING(9B)+'Time' +string(9B)+ 'Count'+STRING(13B)
         indU = uniq(*numObjs.pTValues)       
         for i = 0, n_elements(indU)-1 do begin
            temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            printF, Unit,temp
         endfor
         close, Unit
         FREE_LUN, Unit
         
         ; MOR - save results in the same directory as images - BEGIN
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_RoundCells_Short.bmp'
         ;aqui falta construir imagen _trajectory.tiff *****************************************************
         imageLeft =tempFileName + 'RoundCells_trajectory'+nom+'.tiff'
         ;imageRight =  tempFileName + 'VCL_Histo'+nom+s_informe+'.tiff'
         ;imageRight2 =  tempFileName + 'VSL_Histo'+nom+s_informe+'.tiff'
         textRight = tempFileName + 'RoundCells_Text'+nom+'.tiff'
         fileName = tempFileName + 'RoundCells_Short'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         ;textLeft = tempFileName + 'Concentration_Text_Short'+nom+s_informe+'.tiff' 
         ;textLeft = ''
         
         CEDAI_combineImages_RoundCells_Short, background, imageLeft, TextRight, fileName
      endcase   ; fin Track RoundCells
      
      
      
     'Track VSL':begin
        pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
        
        tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
        tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
        tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
        
        vslVect = make_array((*state.poCurrTrackGroup)->getObjNum())
        
        image_size = size(image, /dimensions)
       
        vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
        if vslStruct.ok then iimage, image, ident = id, /no_save,$;user_interface = 'none' $
        image_dimensions = [image_size[0]*scale_factor, image_size[1]*scale_factor],xtitle =  "X Position in um",ytitle =  "Y Position in um"
        
        for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
           ;vslTotal = sqrt(VSLStruct.VSL[*,0]^2 + VSLStruct.VSL[*,1]^2 + VSLStruct.VSL[*,2]^2)
           vslVect[i] = VSLStruct.VSL
           if vslStruct.ok then iplot,vslStruct.DPos[*,0]*scale_factor,vslStruct.DPos[*,1]*scale_factor,/no_saveprompt,overplot = id,Title = "VSL",thick = 2,linestyle = 0,xtitle =  "X Position en um",ytitle =  "Y Position en um"
           iplot, (*vslStruct.pXYZValues)[*,0]*scale_factor,(*vslStruct.pXYZValues)[*,1]*scale_factor,/no_saveprompt,overplot = id,color = [0,0,255],thick = 2,linestyle = 0,xtitle =  "X Position en um",ytitle =  "Y Position en um" 
        endfor
        
      export_image, s_getPathForSystem()+ 'sperms_VSL.tiff'
      make_hist,vslVect," Velocidad Rectilinea VSL en ums^-1"
      export_image, s_getPathForSystem()+ 'Histo_VSL.tiff'
     endcase
     'Track VAP':begin
        pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
        
        tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
        tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
        tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
        
        VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum()) 
        
        IMAGE_SIZE = SIZE(image, /DIMENSIONS)
        
        
;        VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
;        if VAPStruct.ok then iimage, image, ident = id, /no_save,$; user_interface = 'none' $
;        ;IMAGE_DIMENSIONS = [IMAGE_SIZE[0]*SCALE_FACTOR, IMAGE_SIZE[1]*SCALE_FACTOR],xtitle =  "X Position en um",ytitle =  "Y Position en um"
;        IMAGE_DIMENSIONS = [IMAGE_SIZE[0]*SCALE_FACTOR, IMAGE_SIZE[1]*SCALE_FACTOR],xtitle =  "X Position (um)",ytitle =  "Y Position (um)"
        
        for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
          ;for i = 50,51 do begin 
          VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
          
          if VAPStruct.ok then col = [255,0,255]
          ;VAPtotal = sqrt(VAPStruct.VAP[*,0]^2+VAPStruct.VAP[*,1]^2+VAPStruct.VAP[*,2]^2)
          ;VAPvect[i] = mean(VAPtotal) 
          VAPvect[i] = mean(VAPStruct.VAP)
          
          ;if n_elements(*VAPStruct.ptValues) gt 1 then iplot, (*VAPStruct.pXYZValues)[*,0]*SCALE_FACTOR,(*VAPStruct.pXYZValues)[*,1]*SCALE_FACTOR,$
          ;/no_saveprompt,overplot = id,Title = "VAP",color = col,thick = 2,linestyle = 0,xtitle =  "X Position en um",ytitle =  "Y Position en um"
          
          ; MOR - comment out to test write to file code - BEGIN
          
;          if n_elements(*VAPStruct.ptValues) gt 1 then iplot, (*VAPStruct.pXYZValues)[*,0]*SCALE_FACTOR,(*VAPStruct.pXYZValues)[*,1]*SCALE_FACTOR,$
;          /no_saveprompt,overplot = id,Title = "VAP",color = col,thick = 2,linestyle = 0,xtitle =  "X Position (um)",ytitle =  "Y Position (um)"
;          
;          ;iplot, (VAPStruct.Spath)[*,0]*SCALE_FACTOR,(VAPStruct.Spath)[*,1]*SCALE_FACTOR,/no_saveprompt,overplot = id,Title = "VAP",color = [255,255,0],thick = 2,linestyle = 0,xtitle =  "X Position en um",ytitle =  "Y Position en um"
;          iplot, (VAPStruct.Spath)[*,0]*SCALE_FACTOR,(VAPStruct.Spath)[*,1]*SCALE_FACTOR,/no_saveprompt,overplot = id,Title = "VAP",color = [255,255,0],thick = 2,linestyle = 0,xtitle =  "X Position (um)",ytitle =  "Y Position (um)"
;          
;          
         ; MOR - comment out to test write to file code - BEGIN
           
         ; MOR 17Aug 2010 - write smoothed trajectory to file - BEGIN
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         name = 'SmoothedCoordsTOs_'
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U
         
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            printF, U, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'VAP'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print smoothed coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               temp = strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string((VAPStruct.Spath)[j,0]),/rem)+STRING(9B)+strcompress(string((VAPStruct.Spath)[j,1]),/rem)+STRING(9B)+strcompress(string((VAPStruct.Spath)[j,2]),/rem)+STRING(9B)+strcompress(string(VAPvect[i]),/rem) ;para IGOR
               printF, U,temp
                        
             endfor
                               
         close, U
         FREE_LUN, U
         
         ; MOR write smoothed trajectory to file - END
         
         ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
         name = 'CoordsTOs_'
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U
         
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            printF, U, 'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print smoothed coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               temp = strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem) ;para IGOR
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
         
          
        endfor
        
        ;SUSANA - Cambio a ingles y ;SUSANA - Guardo en misma carpeta de datos, copio codigo MIA
        ;export_image, s_getPathForSystem()+ 'sperms_VAP.tiff'        
        ;export_image, tempFileName+ 'sperms_VAP.tiff'
        
        ; SUSANA - Ingles & guardo histograma en misma carpeta de datos
        ;make_hist,vapvect," Velocidad del Camino Promedio VAP en ums^-1"
        make_hist,VAPvect,"Average Path Velocity (ums^-1)",tempFileName
        ;export_image, s_getPathForSystem()+ 'histo_VAP.tiff'
        ;export_image, tempFileName+ 'histo_VAP.tiff'
       endcase
       
       'Track ALH':begin
        pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()

        tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
        tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
        tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         
         ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
         
         ;if ALHStruct.ok then iimage, image, ident = id, /no_save
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
         ALHStruct  = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
         if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)
         endfor  
          
          ;OUTPUT  
          ;SUSANA - Cambio a ingles y ;SUSANA - Guardo en misma carpeta de datos, copio codigo MIA
          ;make_hist,ALHvect,"ALH Amplitud del desplazamiento en um"
          make_hist,ALHvect,"ALH Amplitude of Lateral Head displacement (um)",tempFileName ;SUSANA - INCLUYO tempFileName para almacenar histograma en misma carpeta de los datos
          ; MOR - save results in the same directory as images - BEGIN
          ;export_image, s_getPathForSystem()+ 'histo_ALH.tiff'
          export_image, tempFileName+ 'histo_ALH.tiff';SUSANA - 
          ; MOR - save results in the same directory as images - END
       endcase
       'Track LIN':begin
           pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
                      
           tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
           tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
           tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
           
           LINvect = make_array((*state.poCurrTrackGroup)->getObjNum())        
           for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           
           LINStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
           lrr = n_elements(*linstruct.PTVALUES)
              if lrr gt 1 then begin
                 dt = shift(*linstruct.PTVALUES,-1)-(*linstruct.PTVALUES)
                 dt = dt[0:lrr-2]
                 dt = rebin(dt,lrr-1,3)
                 curl_dist = linstruct.vcl*dt
                 rect_dist = linstruct.vsl*((*linstruct.PTVALUES)[lrr-1]-(*linstruct.PTVALUES)[0])
                 LIN = sqrt(linstruct.vsl[*,0]^2+linstruct.vsl[*,1]^2+linstruct.vsl[*,2]^2)/mean(sqrt(linstruct.vcl[*,0]^2+linstruct.vcl[*,1]^2+linstruct.vcl[*,2]^2))
                 LIN_path = sqrt(rect_dist[*,0]^2+rect_dist[*,1]^2+rect_dist[*,2]^2)/total(sqrt(curl_dist[*,0]^2+curl_dist[*,1]^2+curl_dist[*,2]^2))
                 LINvect[i] = LIN
              endif else LINvect[i] = 'Nan'
           endfor
        
          ;make_hist,LINvect,"LIN Indice de Linealidad"
          make_hist,LINvect,"LIN Indice de Linealidad",tempFileName; SUSANA - guardo histograma en misma carpeta de datos
          export_image, s_getPathForSystem()+ 'histo_LIN.tiff'
      endcase
      'Track WOB':begin
           pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
                  
           tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
           tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
           tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
           
            
           WOBStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
           WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum())             
           for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
            WOBStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
            WOB = mean(sqrt(WOBstruct.vap[*,0]^2+WOBstruct.vap[*,1]^2+WOBstruct.vap[*,2]^2))/mean(sqrt(WOBstruct.vcl[*,0]^2+WOBstruct.vcl[*,1]^2+WOBstruct.vcl[*,2]^2))
            WOBvect[i] = WOB
           endfor  
   
           make_hist,WOBvect," WOB Indice de Tambaleo de la Cabeza"
           export_image, s_getPathForSystem()+ 'histo_WOB.tiff'
          endcase
        'Track STR':begin
          pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
                  
          tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
          tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
          tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
            
          STRStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
          STRvect = make_array((*state.poCurrTrackGroup)->getObjNum())            
          for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
            STRStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
            STR = mean(sqrt(STRstruct.vsl[*,0]^2+STRstruct.vsl[*,1]^2+STRstruct.vsl[*,2]^2))/mean(sqrt(STRstruct.vap[*,0]^2+STRstruct.vap[*,1]^2+STRstruct.vap[*,2]^2))
            STRvect[i] = STR
          endfor  
          make_hist,STRvect," STR Indice de Rectitud de la Trayectoria"
          export_image, s_getPathForSystem()+ 'histo_STR.tiff'
       endcase
        'Track Aceleration':begin
           pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
               
          tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
          tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
          tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
           
          IMAGE_SIZE = SIZE(image, /DIMENSIONS)
           
          iimage, image, ident = id, /no_save,$; user_interface = 'none' $
          IMAGE_DIMENSIONS = [IMAGE_SIZE[0]*SCALE_FACTOR, IMAGE_SIZE[1]*SCALE_FACTOR],xtitle =  "X Position en um",ytitle =  "Y Position en um"
          Acelvect = make_array((*state.poCurrTrackGroup)->getObjNum())
                         
          for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
             AcelStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
             acelvect[i] = mean(sqrt(AcelStruct.XYZaceleration[*,0]^2 + AcelStruct.XYZaceleration[*,1]^2 + AcelStruct.XYZaceleration[*,2]^2))
             if (acelvect[i]eq 0)then col = [0,1,1]*255 $; cyan velocidad constante
             else if (acelvect[i]lt 4.5) then col = [1,1,0]*255 $;amarillo aceleracion mediana 
             else col = [1,0.5,0]*255;naranjo aceleracion grande
             if n_elements(*AcelStruct.ptValues) gt 1 then iplot, (*(AcelStruct.pXYZValues))[*,0]*SCALE_FACTOR,(*(AcelStruct.pXYZValues))[*,1]*SCALE_FACTOR,$
             /no_saveprompt,overplot = id,title = "Aceleration",color = col,thick = 2,linestyle = 0,xtitle =  "X Position en um",ytitle =  "Y Position en um"
          endfor 
          export_image, s_getPathForSystem()+ 'sperms_aceleration.tiff'
          make_hist,acelvect," Aceleracion ums^-2"
          export_image, s_getPathForSystem()+ 'histo_ACEL.tiff'
      endcase
      ; MOR - 22Mar2011 - BEGIN
      'Track Status':begin
          ; code for status
          ;'normal': result = 0
          ;'appearance': result = 1
          ;'lost': result = 2
          ;'fusion': result = 3
          ;'fission': result = 4
        cl = make_array([5, 3], /byte)
        cl[0,*] = [0, 255, 255] ; normal - cyan
        cl[1,*] = [0, 0, 255]   ; new - blue
        cl[2,*] = [255, 0, 0] ; lost - red
        cl[3,*] = [255, 0, 128] ; fusion - magenta
        cl[4,*] = [255, 128, 64] ; fission - orange
        status = [0 ,1 ,2 ,3, 4]
          
      
          pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
          tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
          tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
          tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
                    
          totObject = (*state.poCurrTrackGroup)->getObjNum()
          paramStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
          
         image_size = size(image,/dim)
          
          if paramStruct.ok then begin
          
             image_x = image_size[0]
             image_y = image_size[1]
             yr = [0, max(image_y)]
             xr = [0, max(image_x)]
             k = 1
             y = .45
             xs = [k+45, y*2]
             ys = [k+45, y*2]
  
             oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
             oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
             oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys)
             oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
             oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys)
             oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
             oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotColor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
             oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotColor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)
             
             oModel = obj_new('IDLgrModel')
             oModel->remove, /all
             oModel->add, oImage
             oModel->add, oPlot
             oModel->add, oXAxis
             oModel->add, oYAxis
             oModel->add, oYAxisUp
             oModel->add, oXAxisUp
          
          endif
                          
          for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
            paramStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = i)
            if paramStruct.ok then begin
              framesTracked = n_elements(*paramStruct.pTValues)
              color = make_array([framesTracked, 3], /byte)
              for j = 0, n_elements(status) - 1 do begin
                   wh = where(status[j] eq (*paramStruct.pStatusCode), count)
                   if(count gt 0) then begin
                      temp = make_array([n_elements(wh),3],/byte, value = 1)
                      color[wh,0] = temp[wh,0] * cl[j,0]
                      color[wh,1] = temp[wh,1] * cl[j,1]
                      color[wh,2] = temp[wh,2] * cl[j,2]
                   endif
              endfor
            ; MOR - 05Jul2010
            ; convert xyz track values back to pixels to correspond to the image
            oSym = obj_new('IDLgrSymbol', data = 2, thick = 2, size = [2, 2])
                for k = 0, framesTracked-1 do begin
                  oPlot = obj_new('IDLgrPlot', datax = [(*paramStruct.pXYZValues)[k,0] /xReal,(*paramStruct.pXYZValues)[k,0]/xReal] ,datay= [(*paramStruct.pXYZValues)[k,1]/yReal, (*paramStruct.pXYZValues)[k,1]/yReal], color = transpose(color[k,*]), thick = 2, XCOORD_CONV = xs, YCOORD_CONV = ys, symbol = oSym)
                  oModel->add, oPlot
                endfor
            endif
          endfor
          
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         
         oBuffer = obj_new('IDLgrBuffer', dimensions = [1024, 768])
         oBuffer->draw, oView 
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         window, 0, xsize = 1024, ysize = 768, title =  'Track_Status.tiff'
         tvscl, outImage_1, /true  
         
         write_tiff, tempFileName + 'Track_Status.tiff', outImage_1
         
         ; destroy objects created
         obj_destroy, oOImage

      endcase ; MOR - 22Mar2011 - END    
      'Track Interactive': begin
          ; get the TO that is activated and the subparameter that is activated
          ; subModel
          ; T0Model
          ; activate some buttons to get a window up for plotting
          
;          title = 'Track Objects -> '
;          image_size = size(image,/dim)
;          if n_elements(ncolors) eq 0 then ncolors = (!D.N_colors - 2) < 254
;          if n_elements(bottom) eq 0 then bottom = 0B
;          if n_elements(colorIndex) eq 0 then colorIndex = (ncolors + bottom) < 255   
;          ; Works with 2D images.
;          Device, Decomposed=0       
;          
;          tlb = widget_base(Column = 1, Title = title, TLB_Frame_Attr = 1,mbar=menubase)
;         ; slider = widget_slider(tlb, xsize=image_size[0],event_pro='slider_event')
;          drawId = widget_draw( tlb, xsize=image_size[0], ysize=image_size[1] )
;          widget_control, tlb, /realize
;          
;          ;;- Display image
;          widget_control, drawId, Get_Value = drawIndex
;          WSet, drawIndex
;          tv, BytScl(image, Top=ncolors-1, max=max(image), min=min(image)) + bottom
;          
;          ; Get color vectors for this application.
;          TVLCT, r, g, b, /Get
;          r = r(bottom:bottom+ncolors-1)
;          g = g(bottom:bottom+ncolors-1)
;          b = b(bottom:bottom+ncolors-1)
;          
;          ; Store the info structure in the user value of the top-level base.
;          widget_control, tlb, set_uvalue=info, /no_copy
;                    
          ;if(widget_info(wTopBase, /valid_id) ) then zoombox, image, group_leader = wTopBase, application_tlb = wTrackPlot
          
      
      endcase
      'Track Combo. Velocities': begin
      
      
      endcase
      
      'Track Motil OMS':begin
         ;for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
         ;print, 'correctly entered track motility after adding the graphic model'
         ;veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010 EN USO CEDAI CALIBRACIÓN previo URUGUAY
         veloLimitsVCL = [0,4., 4.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010  URUGUAY
         ;veloLimitsVCL = [0,3.33, 3.33,8.0, 8.0,25., 25.,10000.] ; MOR - 9Feb2011 - limits were found using pixels April 2010  URUGUAY    ;correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
         
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
         ; do not include in analysis if started after the xx frame in time
         ; number of frames
         minTimeF = 5;  will be value in [frames]
         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]
         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
         minNumFramesTracked = floor((tEnd-tStart)*0.50);  value in [frames]
         ; how close to the border the object is 
         closeBorder = 9; [pixels] -- this value is the same as max_Dist in C_sROIParam_ObjTrackObject.pro    
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_z'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         
         ; write to file the track objects overallType of classification
         name = 'overallTypeTOsOMS_'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U1
         openW, U1, file
         printF, U1, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'TypeNC'
         ; threshold area [in pixels] that the track covers
         areaThreshold = 3; this is 2x2 pixels large, Default = 2
         ; MOR - determine cutoffs for not including TOs in analysis - END
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]
         
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - INICIO
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - FIN
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange

         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
        
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
                                
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         
         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])
         name = 'CoordenadasTOsOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         get_lun, unit
         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
         ;;;;;;; MOR - temporarily printint the color code -----
         printF, Unit, 'zsliceA'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type'+STRING(9B)+'xBox_pix'+STRING(9B)+'yBox_pix'+STRING(9B)+'xBox_X_yBox_pix';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         vslMaxim = 0
         vslMaximPos = 0
         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
         ; --- Susana Creando vector para VSL --- INICIO
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         umbralVCL = 50 ;10
         ; MOR - 9Feb2011 - limits were found using pixels April 2010
         ;;veloLimitsVSL = [2.5,4.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC EN USO previo reajuste tabla por Uruguay
         veloLimitsVSL = [4.0,4.0,6.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC ; URUGUAY  EN USOOOOOO
         ;veloLimitsVSL = [0.0,14.0,14.0];máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC ; URUGUAY  correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         ; --- Susana Creando vector para VSL --- FIN
         
         ; --- Susana - Para Barras de Rangos en Histogramas VSL y VCL - INICIO
         veloLimVSLb = [0.0,4.0,6.0]*xReal
         veloLimVCLb = veloLimitsVCL
         ;veloLimVSLb = [0.0,14.0,14.0];correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         ;veloLimVCLb = [0,3.33, 3.33,8., 8.,25., 25.,10000.];correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         ; --- Susana - Para Barras de Rangos en Histogramas VSL y VCL - FIN
         
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
         vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad A,B,C y D
         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         ; MOR - add vector for storing vsl values for the histogram - BEGIN
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum());
         ; MOR - add vector for storing vsl values for the histogram - END
         
         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
         
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
         
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - BEGIN
         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - END 
         
         ; MOR - 12Feb2011 - get concentration in time
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
         
         xBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBoxyBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         xBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBoxyBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana

         ; Susana - Optical Flow 1/2 -  BEGIN - 
           OFvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;OpticalFlow
           ;Sizevect = make_array((*state.poCurrTrackGroup)->getObjNum());Size
           DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Distancia
           DESPvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Desplazamiento
           DESP_DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); DESP/DIST
          
           s_OFParameter = 'Object Speed2 [pixelsPframe]'
           ;s_OFParameter = 'Object Vy [pixelsPframe]'
           strIDOF = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zpos) + '_'  + 'Clus' + string(clusPos) + '_', /rem)
            case selROIGroupObj of
             'oROI2DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Track Objects from Masks')
             'oROI3DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
             endcase
           ; sort objects in trackIDOF
           if (trackIDOF[0] ne -1) then begin
              whtrackIDOF = where(trackIDOF ne -1, count)
              if (count gt 0) then trackIDOF = trackIDOF[whtrackIDOF]
              sorttrackIDOF = fix(trackIDOF[uniq(trackIDOF, sort(trackIDOF))])
           endif

           case selROIGroupObj of
               'oROI2DGroup':trackParamsOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_OFParameter)
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase
           
           case selROIGroupObj of
               'oROI2DGroup':trackParamsTIME = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Object Time [s]')
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase           
           if (count gt 0) then begin
               trackParamsOF = trackParamsOF[whtrackIDOF]
               trackParamsTIME = trackParamsTIME[whtrackIDOF]
           endif
           
;           ; Size 1/2 - BEGIN - 
;           ;Size no me sirve, ya que es tamaño de cabecita brillante, no area de trayectoria
;           s_SizeParameter = 'Object Size [x²]';  'Object Size [x²]'  'Object Size [Pixel²]'   'Object Size [%]'
;           case selROIGroupObj of
;               'oROI2DGroup':trackParamsSize = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_SizeParameter)
;               ;'oROI3DGroup':trackParamsSize = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
;           endcase
;           if (count gt 0) then begin
;               trackParamsSize = trackParamsSize[whtrackIDOF]
;           endif
;           ; Size 1/2 - END -


         ; Susana - Optical Flow 1/2 -  END - 
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana
         
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
           vslTotal = vslVect[i] ;MOR changing calculation
           
           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
           
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
           
;                      ; Susana - Optical Flow 2/2 -  BEGIN - 
;            if (sorttrackIDOF[0] ne -1) then begin
;                whParamOF = where(trackIDOF eq sorttrackIDOF[i])
;                if (whParamOF[0] eq -1) then begin
;                    trackParamsOF_i = [min(trackParamsOF),max(trackParamsOF)]
;                    ;trackParamsSize_i = [min(trackParamsSize),max(trackParamsSize)]; Susana - Size 2/2       ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;                    trackParamsTIME_i = [min(trackParamsTIME),max(trackParamsTIME)]
;                endif else begin 
;                    trackParamsOF_i = trackParamsOF[whParamOF]
;                    ;trackParamsSize_i = trackParamsSize[whParamOF]; Susana - Size 2/2      ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;                    trackParamsTIME_i = trackParamsTIME[whParamOF]
;                endelse
;            endif
;            OFvect[i] = mean(trackParamsOF_i)
;            ;Sizevect[i] = trackParamsSize_i    ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;            trackParamsTIME_i_delta = 0
;            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
;              trackParamsTIME_i_delta = trackParamsTIME_i[1:N_ELEMENTS(trackParamsTIME_i)-1] - trackParamsTIME_i[0:N_ELEMENTS(trackParamsTIME_i)-2]
;            endif
;            ;endif else begin 
;            ;endelse
;            
;           ; Susana - Optical Flow 2/2 -  END - 
           
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
           ; check at what time/frame the object started being tracked
           firstAppeared = (*vclStruct.pTValues)[0]
           
           ; check how many frames the object was tracked
           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0]);
           
           ; check that all elements of the TO are not 'too close' to the border
           ;2D case --- @todo - need to figure out the 3D case
           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
           countBorder = 0
           if(firstAppeared gt 0) then begin
               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
               xO = (*vclStruct.pXYZValues)[0,0]/xReal
               yO = (*vclStruct.pXYZValues)[0,1]/yReal
               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
           endif
           ; create a bounding box of the track path -- convert trajectory to pixels
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
            
            ;xBox = 10000
            xBox = 0 ; Susana
            ;yBox = 10000
            yBox = 0 ;Susana
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
               ; find bounding center of mass values in each dimension
               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
               ; represent in whole pixels rather than subpixel positions
               xMin = floor(xMin)
               xMax = ceil(xMax)
               yMin = floor(yMin)
               yMax = ceil(yMax)
               ; check in each dimension what the number of pixels are
               xBox = xMax - xMin 
               yBox = yMax - yMin
            endif
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
           
           xBox_vect[i] = xBox ; Susana - evaluando xBox e yBox
           yBox_vect[i] = yBox ; Susana - evaluando xBox e yBox
           yBoxyBox_vect[i] = xBox_vect[i]*yBox_vect[i]
           
           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
           ;ALH , creo que no lo informo aùn no
           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)

           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
           
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
               if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
               endif else begin
                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
               endelse
             
;           ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- INICIO
            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
             ;desp=sqrt( ( (vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) )^2 ); en micrómetros, usando VSL
             ;DESPvect[i] = sqrt( ((((((*vclStruct.pXYZValues)[*,0])[(n_elements((*vclStruct.pXYZValues)[*,0]))-1])-(((*vclStruct.pXYZValues)[*,0])[0])))^2) + ((((((*vclStruct.pXYZValues)[*,1])[(n_elements((*vclStruct.pXYZValues)[*,1]))-1])-(((*vclStruct.pXYZValues)[*,1])[0])))^2) )
             DESPvect[i] = ((vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) ); en micrómetros, usando VSL    vslTotal*total(vclStruct.dT)
             
             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta))/xReal   ;en pixeles
             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta)); en micrómetros
             vDist = (vclStruct.XYZDisplacement); en micrómetros  ;vtotal = vclStruct.XYZDisplacement/vclStruct.dT
             DISTvect[i] = total(vDist); en pixeles
             ;desp_dist = desp/dist ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
             DESP_DISTvect[i] = DESPvect[i]/DISTvect[i] ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
           endif
;          ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- FIN

             ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - INICIO
             ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - INICIO
             if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 20)) then begin ; PChenlo
                  ;No motiles
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
               endif
           ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - FIN
           ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - FIN
             
             
             ;SUSANA - LIN - INICIO
             ;LIN
             ; MOR @todo - need to update the functions calling LIN, WOB, STR in C_sTrackGroupObject function in order to have
             ; the correct access to these calculations 
             
             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
             if (VCLvect[i] ne 0) then begin
               ;LINvect[i] = vslVect[i]/VCLvect[i]
               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
               ;WOBvect[i] = VAPvect[i]/VCLvect[i]
               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               LINvect[i] = 0
               WOBvect[i] = 0
             endelse
             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
             ; MOR STR calculation - BEGIN
             if (VAPvect[i] ne 0) then begin
               ;STRvect[i] = vslVect[i]/VAPvect[i]
               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               STRvect[i] = 0
            endelse
             ; MOR STR calculation - END
             
             ; MOR - create a vector to store the reason why something was classified as 'NC' - BEGIN
;             overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
             ; legend 
             ; classified for analysis - 0
             ; less than 2 data points - 1
             ; appeared late in time - 2
             ; tracked less than 50% of total time sample - 3
             ; too near the border at time of first track, if it is after the 1st frame - 4
             ; MOR - create a vector to store the reason why something was classified as 'NC' - END
           ;@TODO Susana and/or Victor: spermatozoa should be categorized as   for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
;            print,'D VCL <'+STRING(veloLimitsVCL[1])+' y/o '+' VSL <='+STRING(veloLimitsVSL[0])
;            print,'C VCL >='+STRING(veloLimitsVCL[2])+' y <'+STRING(veloLimitsVCL[3])+'   y/o   VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'C VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'B VCL >='+STRING(veloLimitsVCL[4])+' y <'+STRING(veloLimitsVCL[5])
;            print,'A VCL >='+STRING(veloLimitsVCL[6])

;             print,'--- velocidades'
;             print,veloLimitsVSL[0]
;             print,veloLimitsVSL[1]
;             
;             print,veloLimitsVCL[0]
;             print,veloLimitsVCL[1]
;             print,veloLimitsVCL[2] ;igual [1]
;             print,veloLimitsVCL[3] 
;             print,veloLimitsVCL[4] ;igual [3]
;             print,veloLimitsVCL[5] ;
;             print,veloLimitsVCL[6]

             case 1 of
             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
            ; MOR - 6Sept2010 - previous check for 'NC' classification
            ;((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) ): begin
               ; MOR - store reason for 'NC' - BEGIN
               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
                     overallType[i] = 1
                     col = [0,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1
                     break 
               endif
               
               if ((countBorder gt 0)) then begin
                     overallType[i] = 4
                     col = [0,255,120]
                     thickk = 2
                     ;vclVectTipo[i] = 'NCpp' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     cuentaTOsNC += 1
                     break
               endif
               
               if( (firstAppeared ge minTime)) then begin 
                     overallType[i] = 2
                     col = [0,120,255]
                     thickk = 2;
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               if ((timesTracked le minNumFramesTracked)) then begin 
                     overallType[i] = 3
                     col = [120,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif
               ; MOR - store reason for 'NC' - END
             end
;            ( (OFvect[i] le 1.5)  and (OFvect[i] ge 0.12) ): begin
;              ; con flujo laminar ? podria usar todos desde cero hasta 1.5 de speed2
;              col = [0,120,120]
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'IM' ;Susana
;              end
             ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - INICIO
             ;for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
             ;
             
             (vslTotal le veloLimitsVSL[1]): begin
              ;print,veloLimitsVSL[1];debe ser entre 14 y 3.9
              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
              ;col = [255,255,255]
              thickk = 4
              vclVectTipo[i] = 'IM' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado
              if ((VCLvect[i] ge veloLimitsVCL[3])) then begin  ; Default CEDAI
                ; ¿exijo ALHvect[i] ge 0.1?  para dejar solo los que se mueven mas ampliamente
                ;print, veloLimitsVCL[3]; debe ser >=8
                col = [0,255,0] ; No progresivos verdes "Tipo C"
                thickk = 4
                vclVectTipo[i] = 'NP' ;Susana NP*
                ;vslReasign[i]=i ; Solo #TrackObjectReasignado 
              endif
              end
;            (vslTotal le veloLimitsVSL[0]): begin
;              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
;              ;col = [255,255,255]
;              thickk = 4
;              vclVectTipo[i] = 'IM' ;Susana
;              vslReasign[i]=i ; Solo #TrackObjectReasignado
;              end
;            ((vslTotal gt veloLimitsVSL[0]) and (vslTotal le veloLimitsVSL[1])): begin
;              ;para ¿TipoB?  que deben ser TipoC
;              col = [0,255,0] ; No progresivos verdes "Tipo C"
;              ;col = [255,255,255]
;              thickk = 8;default=4
;              vclVectTipo[i] = 'NP' ;Susana
;              vslReasign[i]=i ; Solo #TrackObjectReasignado 
;              end
              ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - FIN
            ;(VCLvect[i] lt veloLimitsVCL[1]): begin; EN USO, previo Ajuste URUGUAY
            ;veloLimVSLb = [0.0,4.0,6.0]*xReal
            ;veloLimitsVCL2 = [0,4., 4.,5., 5.,30., 30.,10000.]*xReal
            (VCLvect[i] lt veloLimitsVCL[3]): begin ; URUGUAY
              ;MAYORIA PROBABLEMENTE YA FUERON ASIGNADOS AL FILTRAR POR VSL
              col = [0,0,255] ;inmoviles azules "Tipo D"
              ;thickk = 5;4 ;default
              thickk = 2;4 ;default
              vclVectTipo[i] = 'IM' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[2]) and (VCLvect[i] lt veloLimitsVCL[3]): begin
              ;GRUPO IMPROBABLE, PORQUE YA QUITÉ LOS QUE TIENEN VSL <=3.33UM/S y en paso anterior están los con VCL<veloLimitsVCL[3] !!!
              ; este grupo no existe
              ; ¿exijo ALHvect[i] ge 0.1?
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'NP' ;Susana
              print,'ocurrió lo imposible, se clasificó com NP, ¡¡ VSL puede ser mayor que VCL !!'
              end
            ;((VCLvect[i] ge veloLimitsVCL[6]) || ((VCLvect[i] ge veloLimitsVCL[4]) and (VCLvect[i] lt veloLimitsVCL[5]))): begin  ;cedai
            ((VCLvect[i] ge veloLimitsVCL[4])): begin  ;cedai
              col = [255,0,0] ;Progresivos rapidos rojos "Tipo A + B"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'PR' ;Susana
              end
            ELSE: begin
              ;ocurrió lo imposible, no se clasificó
              col = [125,255,5] ; No progresivos verdes "Tipo C"
              thickk = 10
              vclVectTipo[i] = 'NC2' ;Susana
              print,'ocurrió lo imposible, no se clasificó - ver NC2'
            end
           endcase
           
           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'IM') ) then begin  
            ;and (STRvect[i] lt 50) 
            cuentaCirc += 1
            thickk = 2
           endif
           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
           
           if (vslTotal gt vslMaxim) then begin 
            vslMaxim = vslTotal
            vslMaximPos = i
           endif
           
           
           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
             np = (*state.poCurrTrackGroup)->getObjNum()
             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
             miX = (*VAPStruct.pXYZValues)[*,0]
             miY = (*VAPStruct.pXYZValues)[*,1]
             miZ = (*VAPStruct.pXYZValues)[*,2]
             miT = (*VAPStruct.pTValues)
             
             np = n_elements(miX)
             for j = 0, (np-1) do begin
;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
              if(vclVectTipo[i] ne 'NC') then begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL)+STRING(9B)+strcompress(xBox,/REMOVE_ALL)+STRING(9B)+strcompress(yBox,/REMOVE_ALL)+STRING(9B)+strcompress(xBox*yBox,/REMOVE_ALL);+STRING(10B) ;para IGOR   con xBox e yBox
               printF, Unit,temp
              endif
             endfor
;            ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - BEGIN
;             if(vclVectTipo[i] ne 'NC') then begin ;guardo coordenadas de los clasificados
;             for j = 0, (np-1) do begin
;                temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem);+STRING(10B) ;para IGOR
;               printF, Unit,temp
;              endfor
;             endif
;             ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - END           
           
           ; MOR - get entire time vector
           timeTrack[i] = n_elements(*VAPStruct.ptValues)
           
           
              ; Susana - Write OpticalFlowParameter in image - BEGIN 
              ;;v_OFmean = mean(trackParamsOF_i)
              ;;s_TagOF = strcompress("OF"+"_"+string(v_OFmean, format = '(F6.2)'),/rem)
              ;s_TagOF = strcompress("OF"+"_"+string(OFvect[i], format = '(F6.2)'),/rem)
              
                s_TagOF = strcompress("LIN"+"_"+string(LINvect[i], format = '(F6.2)'),/rem)
              
              ;;s_TagOF = strcompress("VCL"+"_"+string(VCLvect[i], format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VSL"+"_"+string(vslTotal, format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VAP"+"_"+string(VAPvect[i], format = '(F6.2)'),/rem)
               
              ;;;if((vclVectTipo[i] ne 'PR') and (vclVectTipo[i] ne 'IM')) then begin
              if((LINvect[i] gt 110.) and ((vslTotal/VCLvect[i]) lt 1)) then begin
              ;;if(OFvect[i] le 1.5) then begin  ;if((OFvect[i] le 1.5)  and (OFvect[i] ge 0.12)) then begin
                ;;oPlot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                Plot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [15, 15], locations = [((*vclStruct.pXYZValues)[0,0]+5)/xReal,((*vclStruct.pXYZValues)[0,1]+1)/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
                
                oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [16, 16], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
              endif
              ; Susana - Write OpticalFlowParameter in image - END 
            
            ;vvalor = 100
            ;if ((vslVect[i] ge vvalor) and (vclVectTipo[i] ne 'NC')) then thickk = 10 ; borrar, solo para destacar
            
           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
           if  (vclVectTipo[i] ne 'NC') then begin
           ; MOR - 05Jul2010
           ; convert track xyz to pixels for plotting against image
              if(vclVectTipo[i] eq 'IM') then begin
                ;;oSym = obj_new('IDLgrSymbol', data = 3, size = 2.5, thick = thickk);original data = 3=>Dot
                oSym = obj_new('IDLgrSymbol', data = 3, size = 5.5, thick = thickk); Dot de mayor tamaño; en uso
                ;oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);    solo para ver mejor los números borrar
              endif else begin
                ;oSym = obj_new('IDLgrSymbol', data = 2, thick = thickk);data = 2=>asterisco   thick=>line thickness
                oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);data = 0 =>no symbol   thick=>line thickness
              endelse
             
             ;if(i eq 53) then col=[0,255,255] 
             
              ;if(vclVectTipo[i] eq 'PR') then begin; borrar
              ;if(i eq 420) then begin; borrar
              ;if( i eq 352) then begin;
               oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oModel->add, oPlot
              ;endif
              ; Susana - figura para Maurcio Cerda, VCL VAP y VSL en colores sobre TrackObject - INICIO
              ; no funciona sobreponer trayectorias, la primera tapa la segunda
;              if(i eq 325) then begin;
              ;trayectoria curvilinea, VCL
              ;oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              ;oModel->add, oPlot
              
              ;trayectoria promedio, VAP
              ;oPlot = obj_new('IDLgrPlot', datax = (*VAPStruct.pXYZValues)[*,0]/xReal, datay = (*VAPStruct.pXYZValues)[*,1]/yReal, color = [255,255,50], thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym); amarillo
              ;oModel->add, oPlot
              
              ;trayectoria lineal, VSL
              ;oPlot = obj_new('IDLgrPlot', datax = [((*vclStruct.pXYZValues)[0,0]/xReal),((*vclStruct.pXYZValues)[(size((*vclStruct.pXYZValues)[*,0])-1),0]/xReal)], datay = [((*vclStruct.pXYZValues)[0,1]/yReal),((*vclStruct.pXYZValues)[(size((*vclStruct.pXYZValues)[*,1])-1),1]/yReal)], color = [0,255,100], thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              ;oModel->add, oPlot
              
;              endif
              ; Susana - figura para Maurcio Cerda, VCL VAP y VSL en colores sobre TrackObject - FIN
              ;endif ; borrar 
              
              ; Susana - Incluyo LIN temporalmente  INICIO
;              if((vclVectTipo[i] eq 'B')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;              if((vclVectTipo[i] eq 'NP')) then begin
;              ;if(i eq 50) then begin; borrar
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)'),/rem)+strcompress('_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' LIN_'+string(LINvect[i], format = '(F6.1)'),/rem)+strcompress('_ALH_'+string(ALHvect[i], format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [((*vclStruct.pXYZValues)[0,0]/xReal)-10,((*vclStruct.pXYZValues)[0,1]/xReal)-10],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;endif
;             ; Susana - Incluyo LIN temporalmente  INICIO
              

              
;             if((vclVectTipo[i] eq 'NP')) then begin; borrar
;             if(i ge 300 and i le 390) then begin; borrar
;;;             ; Susana - add TO number - INICIO
;;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [15, 15], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [255,255,255], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;              ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [10, 10], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             ;oPlot = obj_new('IDLgrText', STRINGS = '   '+strcompress('   '+string(i, format = '(I6)'),/rem),color = [0, 255, 0], CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;             
;             v_TOsSelectos = [97,135,146,241,242,232,251,237,209,148,191,104,118] ; DF1
;              v_TOsSelectos = [35,293,74,77,70,90,96,248,247,253,228,237,222,185,228,237,142,116,146,139,215,222,285,282,259,74,259,168,5,30,2,86,229,166,167,180,210,275]
;              v_TOsSelectos = [2,22,25,53,59,63,70,113,124,131,132,141,144,183,184,188,208,223,227,262,200] ; 27EneroM1_1   seleccionados por Luis Sarabia desde trayectoris de 5 segundos
;             v_TOsSelectos = [14,8];AB1
;             v_whereTOsSelectos = where(v_TOsSelectos eq i, countTOsSelectos)
;             if (countTOsSelectos gt 0) then begin
;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [0,1.0,0],UPDIR = [1.0, 1.0, 0.0])  ; Susana - With ROI number  vertical paralelo a Y
;             oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [11, 11], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [1.0,0.0,0],UPDIR = [-1.0, -1.0, 0.0])  ; Susana - With ROI number  horizontal con flip vertical para verlos bien en trayectories
;             oModel->add, oPlot
;             endif
;;;             ; Susana - add TO number - FIN
;             endif ; borrar


;              if(i eq 194) then begin
;                print,'TO='+string(i)+' ---------'
;                print,vclVectTipo[i]
;                print,vslTotal
;                print,VCLvect[i]
;                print,VAPvect[i]
;                print,LINvect[i]
;                print,STRvect[i]
;                ;print,(vslTotal/VCLvect[i])*100. ; LIN
;                ;print,(vslTotal/VAPvect[i])*100. ;STR
;                print,xBox_vect[i]
;                print,yBox_vect[i]
;                print,yBoxyBox_vect[i]
;                print,ALHvect[i]
;                print,' --------------------'
;              endif

           endif
           
           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
           ;name = 'CoordsTOs_'
           name = 'CoordsTOsOMS_'+nom
           file = strCompress(tempFileName + name + '.dat', /remove)
           
           get_lun, U
            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            printF, U, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
           
           ; MOR - write to file the track objects overallType of classification - BEGIN    
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           printF, U1,temp 
           ; MOR - write to file the track objects overallType of classification - END
           
         endfor
;         print, countTONC_pocosPuntos ;Susana
;         print, countTONC_firstAppeared
;         print, countTONC_timesTracked
         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         close, Unit
         FREE_LUN, Unit
         
         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         ; MOR - close up file with overalltype classification - BEGIN
         close, U1
         FREE_LUN, U1
         ; MOR - close up file with overalltype classification - END         
         

         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ; MOR - 9Feb 2011 - change so the last directory name prints on Motil.tiff file
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         name = 'VelocityDataOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceC'+STRING(9B)+'TOVeloc'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(9B)+'xBoxVeloc_pix'+STRING(9B)+'yBoxVeloc_pix'+STRING(9B)+'xBox_X_yBoxVeloc_pix'+STRING(9B)+'desp'+STRING(9B)+'dist'+STRING(9B)+'desp_dist'+STRING(9B)+s_OFParameter+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR  ; Susana  - con xBox e yBox sin SIZE
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]*yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESP_DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(OFvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR   Susana- con xBox e yBox sin Size
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         
         
         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = '_z'+strcompress(string(zPos),/REMOVE_ALL)+'_' ; susana incluye zPos a nombre imagen
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])
         
         name = 'Motil_MovVarsOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceB'+STRING(9B)+'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'TypeMVars'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
         
           
         ;Susana INICIO --  Guardando #TrackObjects Reasignados
         name = 'NumTrackObjectsVSLOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit,transpose(vslReasign)
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN --  Guardando #TrackObjects Reasignados
                 
         
         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
         np = (*state.poCurrTrackGroup)->getObjNum()
         meanVAP_A = 0; promedio tipos A
         meanVAP_B = 0; promedio tipos B
         meanVAP_C = 0; promedio tipos C
         meanVAP_D = 0; promedio tipos D
         meanVAP_NC = 0; promedio tipos NC
         
         meanVCL_A = 0; promedio tipos A
         meanVCL_B = 0; promedio tipos B
         meanVCL_C = 0; promedio tipos C
         meanVCL_D = 0; promedio tipos D
         meanVCL_NC = 0; promedio tipos NC
         
         meanVSL_A = 0; promedio tipos A
         meanVSL_B = 0; promedio tipos B
         meanVSL_C = 0; promedio tipos C
         meanVSL_D = 0; promedio tipos D
         meanVSL_NC = 0; promedio tipos NC
         
         meanLIN_A = 0; promedio LIN tipos A
         meanLIN_B = 0; promedio LIN tipos B
         meanLIN_C = 0; promedio LIN tipos C
         meanLIN_D = 0; promedio LIN tipos D
         meanLIN_NC = 0; promedio LIN tipos NC
         
         meanSTR_A = 0; promedio STR tipos A
         meanSTR_B = 0; promedio STR tipos B
         meanSTR_C = 0; promedio STR tipos C
         meanSTR_D = 0; promedio STR tipos D
         meanSTR_NC = 0; promedio STR tipos NC
         
         meanWOB_A = 0; promedio WOB tipos A
         meanWOB_B = 0; promedio WOB tipos B
         meanWOB_C = 0; promedio WOB tipos C
         meanWOB_D = 0; promedio WOB tipos D
         meanWOB_NC = 0; promedio WOB tipos NC
         
         meanVAP_Acnt = 0; counter para tipos A
         meanVAP_Bcnt = 0; counter para tipos B
         meanVAP_Ccnt = 0; counter para tipos C
         meanVAP_Dcnt = 0; counter para tipos D
         meanVAP_NCcnt = 0; counter para tipos NC
         ;desde aqui descomenté -Susana
         for h = 0, (np-1) do begin
          case 1 of
            (vclVectTipo[h] eq 'PR'): begin
              meanVAP_A += VAPvect[h]
              meanVCL_A += VCLvect[h]
              meanVSL_A += vslVect[h]
              meanLIN_A +=  LINvect[h]
              meanSTR_A +=  STRvect[h]
              meanWOB_A +=  WOBvect[h]
              ;meanALH_A += ALHvect[h]
              meanVAP_Acnt += 1
              end
            (vclVectTipo[h] eq 'B'): begin
              meanVAP_B += VAPvect[h]
              meanVCL_B += VCLvect[h]
              meanVSL_B += vslVect[h]
              meanLIN_B += LINvect[h]
              meanSTR_B +=  STRvect[h]
              meanWOB_B +=  WOBvect[h]
              ;meanALH_B += ALHvect[h]
              meanVAP_Bcnt += 1
              end
            (vclVectTipo[h] eq 'NP'): begin
              meanVAP_C += VAPvect[h]
              meanVCL_C += VCLvect[h]
              meanVSL_C += vslVect[h]
              meanLIN_C += LINvect[h]
              meanSTR_C +=  STRvect[h]
              meanWOB_C +=  WOBvect[h]
              ;meanALH_C += ALHvect[h]
              meanVAP_Ccnt += 1
              end
            (vclVectTipo[h] eq 'IM'): begin
              meanVAP_D += VAPvect[h]
              meanVCL_D += VCLvect[h]
              meanVSL_D += vslVect[h]
              meanLIN_D += LINvect[h]
              meanSTR_D +=  STRvect[h]
              meanWOB_D +=  WOBvect[h]
              ;meanALH_D += ALHvect[h]
              meanVAP_Dcnt += 1
              end
            (vclVectTipo[h] eq 'NC'): begin
              meanVAP_NC += VAPvect[h]
              meanVCL_NC += VCLvect[h]
              meanVSL_NC += vslVect[h]
              meanLIN_NC += LINvect[h]
              meanSTR_NC +=  STRvect[h]
              meanWOB_NC +=  WOBvect[h]
              ;meanALH_NC += ALHvect[h]
              meanVAP_NCcnt += 1
              end
           endcase
         endfor
         ;hasta aqui descomenté--Susana
         meanVAP_A /= meanVAP_Acnt
         meanVAP_B /= meanVAP_Bcnt
         meanVAP_C /= meanVAP_Ccnt
         meanVAP_D /= meanVAP_Dcnt
         meanVAP_NC /= meanVAP_NCcnt
         
         meanVCL_A /= meanVAP_Acnt
         meanVCL_B /= meanVAP_Bcnt
         meanVCL_C /= meanVAP_Ccnt
         meanVCL_D /= meanVAP_Dcnt
         meanVCL_NC /= meanVAP_NCcnt
         
         meanVSL_A /= meanVAP_Acnt
         meanVSL_B /= meanVAP_Bcnt
         meanVSL_C /= meanVAP_Ccnt
         meanVSL_D /= meanVAP_Dcnt
         meanVSL_NC /= meanVAP_NCcnt
         
         meanLIN_A /= meanVAP_Acnt
         meanLIN_B /= meanVAP_Bcnt
         meanLIN_C /= meanVAP_Ccnt
         meanLIN_D /= meanVAP_Dcnt
         meanLIN_NC /= meanVAP_NCcnt
         
         meanSTR_A /= meanVAP_Acnt
         meanSTR_B /= meanVAP_Bcnt
         meanSTR_C /= meanVAP_Ccnt
         meanSTR_D /= meanVAP_Dcnt
         meanSTR_NC /= meanVAP_NCcnt
         
         meanWOB_A /= meanVAP_Acnt
         meanWOB_B /= meanVAP_Bcnt
         meanWOB_C /= meanVAP_Ccnt
         meanWOB_D /= meanVAP_Dcnt
         meanWOB_NC /= meanVAP_NCcnt
         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
         
         
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         
         ; MOR - comment out old path for saving, try path where images are saved instead 
         write_tiff, tempFileName + 'Motil_trajectoryOMS'+nom+'.tiff', outImage_1
         ;write_png, tempFileName + 'Motil_trajectoryOMS'+nom+'.png', outImage_1
         
;         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - INICIO
;         for v=0, totalTNum-1 do begin
;            widget_control, stack_tlb, get_uValue = stackState, /no_copy
;              imagev = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = v, chPos = chPos, zPos = zPos)
;            widget_control, stack_tlb, set_uValue = stackState, /no_copy
;            
;            ;oImagev = obj_new('IDLgrImage',imagev, xcoord_conv = xs, ycoord_conv = ys)
;            oImage->setProperty, data = imagev
;            oBuffer->draw, oView      
;            oOImage = oBuffer->read()
;            oOImage->getProperty, data = outImage_v
;         
;              write_tiff, tempFileName + '\Video\Motil_trajectoryOMS_'+string(v,format='("",I3.3)')+'.tiff', outImage_v
;         endfor
;         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - FIN
         
;         ;Susana Comenta esto para poder correr lo de victor más adelante .. INICIO
;         ; MOR - destroy objects created for plotting to free up memory
;         obj_destroy, [oBuffer, oView]
;         ;Susana Comenta esto para poder correr lo de victor más adelante .. FIN
         
         ; MOR - add creation of VSL histogram - BEGIN
         path = tempFileName + 'VSL_HistoOMS'+nom+'.tiff'
         
         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Original MIA - Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 ;Aqui reemplazo ne por eq, es decir envio al histograma todos los trackobjects menos los reasignados
         whClassified = where((vclVectTipo ne 'NC'), count); Susana - envío todos los clasificados
         if(whClassified[0] ne -1) then begin; Susana1
          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
;         vect = vslVect ; MOR - commented out previous code
          name = "rectilinear velocity VSL [um s-1]"
          ;name = "rectilinear velocity VSL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          s_Total = 'OMS'+nom
          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
          CEDAI_histogramPlot_VSL_OMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName, veloLimVSLb = veloLimVSLb,s_Total = s_Total
          ;CEDAI_histogramPlot_VSL_OMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = ([2.5, 4.,6.]*xReal), linesFlag = linesFlag, tempFileName = tempFileName,s_Total = s_Total ; URUGUAY - eje x de histograma de 0 a 6
          ; MOR - add creation of VSL histogram - END
          v_histogVSL_OK = 1
         endif; Susana1
         ;endif else begin; Susana1
            ;
         ;endelse; Susana1
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;path = s_getPathForSystem() + 'VCL_HistoOMS.tiff'
         ;path = tempFileName + 'VCL_HistoOMS.tiff'
         path = tempFileName + 'VCL_HistoOMS'+nom+'.tiff'
         
         v_histogVCL_OK = 0 ; Susana - controlar si existe histograma VCL de clasificados no reasignados
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
         whClassified = where((vclVectTipo ne 'NC'), count) ; MOR - 21 June 2011
         if(whClassified[0] ne -1) then begin; Susana2
          ; Susana - A B C o D, no reasignado
          vect = VCLvect[whClassified]
          name = "curvilinear velocity VCL [um s-1]"
          ;name = "curvilinear velocity VCL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          linesFlag = 1
          s_Total = 'OMS'+nom
          CEDAI_histogramPlotOMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName, veloLimVCLb = veloLimVCLb,s_Total = s_Total
          v_histogVCL_OK = 1
         endif; Susana2
         ;endif else begin; Susana2
            ;nada
            ;El problema de no haber alternativa es que no crea imágenes como VCL_HistoOMS.tiff
         ;endelse; Susana2
         
         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
         
         ;Susana INICIO    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom2 = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
         nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
         ;name = 'DatosPChenlo_2011'
         name = 'DatosOMS_'+nom2
         
         ;Susana - reemplazo tempFileName INICIO
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         ;Susana - reemplazo tempFileName FIN
         
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         get_lun, unitPCh
         OPENR, unitPCh, file, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitPCh, file
           ;printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         
         ;openW, unitPCh, file
         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
         ;recuentos
         aux = where(vclVectTipo eq 'PR', tipoA)
         aux = where(vclVectTipo eq 'B', tipoB)
         aux = where(vclVectTipo eq 'NP', tipoC)
         aux = where(vclVectTipo eq 'IM', tipoD)
         aux = where(vclVectTipo eq 'NC', tipoNC)
         ;porcentajes
         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
         ;print,tipoA+tipoB+tipoC+tipoD  corresponde a vect_elements
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         ;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana FIN    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         
         
         ;dummy = where(vect ge veloLimitsVCL[6], count)
         dummy = where(vclVectTipo eq 'PR', count) ;Susana
         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_0 = 'PR:';Progressively Motil
         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
;         dummy = where(vclVectTipo eq 'B', count) ;Susana
;         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
;         string_1 = 'Type B:' 
;         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[2] and vect lt veloLimitsVCL[3], count)   ;Susana
         dummy = where(vclVectTipo eq 'NP', count) ;Susana
         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_2 = 'NP:';Non-Progressively Motil
         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[0] and vect lt veloLimitsVCL[1], count)   ;Susana
         dummy = where(vclVectTipo eq 'IM', count) ;Susana
         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_3 = 'IM:';Immotile 
         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Motil_TextOMS'+nom+'.tiff'
         fileNameConc = tempFileName + 'Concentr_TextOMS'+nom+'.tiff'
         
         ;SUSANA - Contabilizando trackobjects NC (No Clasificados) 
         dummy = where(vclVectTipo eq 'NC', count) ;Susana
         ;string_21 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_21 = strcompress( string(fix(count)), /remove_all) + ' sperms'
         ;string_23 = 'NC       :'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - INICIO
         string_23 =strcompress('#Traj.Circ.:' + string(fix(cuentaCirc)), /remove_all) + '(LIN<50) '+ '#NC:'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - FIN
         
         ;aqui estaba ;SUSANA- incluyo concentración - INICIO
         
         ; MOR - 12Feb 2011- average number of objects found per time
         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
         totalNumObjs.avg = stats[0]*conFactor; recuento promedio, promedia recuento espermios en todos los tiempos
         totalNumObjs.std = sqrt(stats[1]*conFactor)
         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
         totalNumObjs.se = sqrt(totalNumObjs.avg/conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         
         ;SUSANA- incluyo concentración -  1 cabeza/campo corresponde a 0.3143 millones/ml para campo 650.138umX483.436um en Makler 10prof - INICIO
         ;string_3AA = strcompress('Conc.: ')
         string_3AA = strcompress('Sperms:')
         string_7AA = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem)
         string_11AA = strcompress('10!U6!N/mL', /rem)
         
         ;;string_11AA = strcompress('[10!U6!N/mL]', /rem)+' ('+strcompress(string(totalNumObjs.avg/conFactor, format = '(I6.0)'), /rem)+' sperms)'
         ;string_11AA = strcompress('[10!U6!N/mL]', /rem); No informo el total de sperms usado para cálculo de concetracion
         
         ;SUSANA- incluyo concentración - FIN
         CEDAI_textPlotOMS, filename, string_0, string_2, string_3, string_3AA, string_4, string_6, string_7, string_7AA, string_8, string_10, string_11, string_11AA,string_21,string_23, nom; ordenamiento de A+B
         CEDAI_textPlotOMS_Conc, fileNameConc, string_3AA, string_7AA, string_11AA, nom; ordenamiento de A+B  - concentration
         
         
         string_0 = 'Concentration Statistics Summary'
;         string_1 = strcompress('Frames ') 
         string_2 = strcompress('Mean Conc.: ') ;string_2 = strcompress('Mean: ') 
;         string_3 = strcompress('SDev ') 
         string_4 = strcompress('Std. Error: ')
;         string_5 = strcompress('Median ') 
;         string_6 = strcompress('Min ') 
;         string_7 = strcompress('Max ') 
         
         string_8 = strcompress(string(totalNumObjs.number, format = '(D6.2)'), /rem) 
         string_9 = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem) 
         string_10 = strcompress(string(totalNumObjs.std, format = '(I6.2)'), /rem) 
         string_11 = strcompress(string(totalNumObjs.se, format = '(D6.2)'), /rem) 
         string_12 = strcompress(string(totalNumObjs.med, format = '(I6.2)'), /rem) 
         string_13 = strcompress(string(totalNumObjs.mini, format = '(I6.2)'), /rem) 
         string_14 = strcompress(string(totalNumObjs.maxi, format = '(I6.2)'), /rem) 
         
         string_15 = strcompress('[10E6/mL]', /rem) 
       
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Concentration_Text_ShortOMS'+nom+'.tiff'
         
         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
         
         ; MOR - 12Feb2011 write total count data to .dat file 
         dum = max(timeTrack, maxI)
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
         
         name = 'ConcentrationOMS'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit, 'zslice'+STRING(9B)+'Time' +string(9B)+ 'Count'+STRING(13B)
         indU = uniq(*numObjs.pTValues)       
         for i = 0, n_elements(indU)-1 do begin
            temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            printF, Unit,temp
         endfor
         close, Unit
         FREE_LUN, Unit
         
         ; MOR - save results in the same directory as images - BEGIN
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_Short.bmp'
         imageLeft =tempFileName + 'Motil_trajectoryOMS'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_HistoOMS'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoOMS'+nom+'.tiff'
         textRight = tempFileName + 'Motil_TextOMS'+nom+'.tiff'
         fileName = tempFileName + 'Motil_ShortOMS'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Concentration_Text_ShortOMS'+nom+'.tiff' 
         
         
         CEDAI_combineImages_Motil_Short, background, imageLeft, TextRight, fileName, textLeft
         
        ; MOR - save results in the same directory as images - BEGIN
        ;; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_ColorNaranjo.bmp'
         
;         imageLeft =tempFileName + 'Motil_trajectoryOMS.tiff'
         imageLeft =tempFileName + 'Motil_trajectoryOMS'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_HistoOMS'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoOMS'+nom+'.tiff'
         textRight = tempFileName + 'Motil_TextOMS'+nom+'.tiff'
         textRight2 = tempFileName + 'Concentration_Text'+nom+'.tiff'
         
         fileName = tempFileName + 'Motil_newOMS'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana 
         endif; Susana 
         
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         v_guardaPNG = 1; 1=Guarda png también    0=No guarda png
         CEDAI_combineImages_motil_OMS, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft,fileNameConc,v_guardaPNG; incluye concentracion
         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - INICIO
         for v=0, totalTNum-1 do begin
            widget_control, stack_tlb, get_uValue = stackState, /no_copy
              imagev = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = v, chPos = chPos, zPos = zPos)
            widget_control, stack_tlb, set_uValue = stackState, /no_copy
            
            ;oImagev = obj_new('IDLgrImage',imagev, xcoord_conv = xs, ycoord_conv = ys)
            oImage->setProperty, data = imagev
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_v
            
            snomImageLeft =strcompress(tempFileName + '\Video\Motil_trajectoryOMS_'+string(v,format='("",I3.3)')+'.tiff') ;Talvez esta cambie después ??
              write_tiff, snomImageLeft, outImage_v
              fileNameV = tempFileName + '\Video\Motil_newOMS'+string(v,format='("",I3.3)')+'.tiff' 
              v_guardaPNG = 0
              CEDAI_combineImages_motil_OMS, background, snomImageLeft, imageRight, imageRight2, TextRight, fileNameV, textLeft,fileNameConc,v_guardaPNG
         endfor
         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - FIN
         ;Susana Incluye esto para poder correr lo de victor .. INICIO
         ; MOR - destroy objects created for plotting to free up memory
         obj_destroy, [oBuffer, oView]
         ;Susana Incluye esto para poder correr lo de victor .. FIN
         
         
         
         ;SUSANA - Incluyo VAP promedio por grupos LO ESTOY INTENTANDO, AUN NO CREO EL HISTOGRAMA PENDIENTES TAMBIEN HISTOGRAMAS STR Y WOB
         ;SUSANA - add creation of ALH histogram - BEGIN
         veloLimitsVCL = [0,50., 50.,200., 200.,400., 400.,10000.] ; no tengo rangos para ALH :( 
         path = tempFileName + 'histo_ALH'+nom+'.tiff'
         
         vect = ALHvect
         name = "ALH Amplitude of Lateral Head displacement [um]"+nom
         
         linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
         ;CEDAI_histogramPlot_ALH, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
         ;Falta generar un graficador de histograma para ALH o llamar internamente el GraphicModel ALH
         ;SUSANA - add creation of VSL histogram - END
         
         
         fileName = tempFileName + 'Motil_MovVariables_Text'+nom+'.tiff' ; MovementsVariables
         
         string_0 = 'PR:';Progressively Motil 
         ;string_4 = string(meanVAP_A, format = '(F6.2)') + ' um/s'
         string_4 = string(meanVAP_A, format = '(F6.1)')
         string_8 = strcompress(string(fix(meanLIN_A), format = '(F6.1)'), /remove_all)
         string_8_1 = strcompress(string(fix(meanSTR_A), format = '(F6.1)'), /remove_all)
         string_8_2 = strcompress(string(fix(meanWOB_A), format = '(F6.1)'), /remove_all)
         string_8_3 = strcompress(string(fix(meanVCL_A), format = '(F6.1)'), /remove_all)
         string_8_4 = strcompress(string(fix(meanVSL_A), format = '(F6.1)'), /remove_all)
         
;         string_1 = 'Type B:' 
;         string_5 = string(meanVAP_B, format = '(F6.1)')
;         string_9 = strcompress(string(fix(meanLIN_B), format = '(F6.1)'), /remove_all)
;         string_9_1 = strcompress(string(fix(meanSTR_B), format = '(F6.1)'), /remove_all)
;         string_9_2 = strcompress(string(fix(meanWOB_B), format = '(F6.1)'), /remove_all)
;         string_9_3 = strcompress(string(fix(meanVCL_B), format = '(F6.1)'), /remove_all)
;         string_9_4 = strcompress(string(fix(meanVSL_B), format = '(F6.1)'), /remove_all)
         
         string_1  = 'NP:' ;Non-Progressively Motil (
         ;string_6 = string(meanVAP_C, format = '(F6.2)') + ' um/s'
         string_5 = string(meanVAP_C, format = '(F6.1)')
         string_9 = strcompress(string(fix(meanLIN_C), format = '(F6.1)'), /remove_all)
         string_9_1 = strcompress(string(fix(meanSTR_C), format = '(F6.1)'), /remove_all)
         string_9_2 = strcompress(string(fix(meanWOB_C), format = '(F6.1)'), /remove_all)
         string_9_3 = strcompress(string(fix(meanVCL_C), format = '(F6.1)'), /remove_all)
         string_9_4 = strcompress(string(fix(meanVSL_C), format = '(F6.1)'), /remove_all)
         
         string_2 = 'IM:'  ; Immotile Talvez no deba incluirlo                              ; string_3
         string_6 = string(meanVAP_D, format = '(F6.1)') ;string_7
         string_10 = strcompress(string(fix(meanLIN_D), format = '(F6.1)'), /remove_all)    ;string_11
         string_10_1 = strcompress( string(fix(meanSTR_D), format = '(F6.1)'), /remove_all) ;string_11_1
         string_10_2 = strcompress( string(fix(meanWOB_D), format = '(F6.1)'), /remove_all) ;string_11_2
         string_10_3 = strcompress(string(fix(meanVCL_D), format = '(F6.1)'), /remove_all)  ;string_11_3
         string_10_4 = strcompress(string(fix(meanVSL_D), format = '(F6.1)'), /remove_all)  ;string_11_4
         
         string_3 = ''; y si uso este para la concentración???
         string_7 = ''
         string_11 = ''
         string_11_1 = ''
         string_11_2 = ''
         string_11_3 = ''
         string_11_4 = ''
         
         string_21 = strcompress('VAP')
         string_23 = strcompress('VSL')
         string_27 = strcompress('VCL')
         string_28 = strcompress('LIN')
         string_29 = strcompress('STR')
         string_30 = strcompress('WOB')
         string_31 = ' um/s'
         string_32 = ' %'
         ;CEDAI_textPlotMotil, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         CEDAI_textPlotMotilOMS, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         
         ; MOR - save results in the same directory as images - BEGIN
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         
         imageLeft =tempFileName + 'Motil_trajectoryOMS'+nom+'.tiff' ;Talvez esta cambie después ??
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_MovVariables.bmp'
         
         imageRight =  tempFileName + 'VCL_HistoOMS'+nom+'.tiff' ;'histo_ALH.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoOMS'+nom+'.tiff'
         textRight = tempFileName + 'Motil_MovVariables_Text'+nom+'.tiff'
         fileName = tempFileName + 'Motil_MovVariables'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png';Susana 
         endif; Susana
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, TextRight, filename,1
         
       ;Track MotilOMS -- FIN
      endcase
      
      'Track Motil Total OMS':begin
          ;Add track objects of other Z slices
          widget_control, wTopBase, set_uValue = state, /no_copy
          
          s_ROITM_Update, wTopBase, FMULTIPLE = 1b
          
          widget_control, wTopBase, get_uValue = state, /no_copy
          ;print, 'correctly entered track motility after adding the graphic model'
         
         ;for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
         ;print, 'correctly entered track motility after adding the graphic model'
         ;veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010 EN USO CEDAI CALIBRACIÓN previo URUGUAY
         veloLimitsVCL = [0,4., 4.,5., 5.,30., 30.,10000.]*xReal ; MOR - 9Feb2011 - limits were found using pixels April 2010  URUGUAY
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
         
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
         ; do not include in analysis if started after the xx frame in time
         ; number of frames
         minTimeF = 5;  will be value in [frames]
         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]
         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
         minNumFramesTracked = floor((tEnd-tStart)*0.50);  value in [frames]
         ; how close to the border the object is 
         closeBorder = 9; [pixels] -- this value is the same as max_Dist in C_sROIParam_ObjTrackObject.pro    
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_z'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; Susana - NO incluyo zslice al final, es informe del total, de todos zslices
         
         ; write to file the track objects overallType of classification
         name = 'overallTypeTOsTotalOMS_'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U1
         openW, U1, file
         printF, U1, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'TypeNC'
         ; threshold area [in pixels] that the track covers
         areaThreshold = 3; this is 2x2 pixels large, Default = 2
         ; MOR - determine cutoffs for not including TOs in analysis - END
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]
         
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - INICIO
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - FIN
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange

         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
        
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
                                
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         
         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])
         name = 'CoordenadasTOsTotalOMS_'+nom ; incluyo nombre ultima carpeta en nombre archivo es 
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         get_lun, unit
         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
         ;;;;;;; MOR - temporarily printint the color code -----
         printF, Unit, 'zsliceA'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type'+STRING(9B)+'xBox_pix'+STRING(9B)+'yBox_pix'+STRING(9B)+'xBox_X_yBox_pix';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         vslMaxim = 0
         vslMaximPos = 0
         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
         ; --- Susana Creando vector para VSL --- INICIO
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         umbralVCL = 50 ;10
         ; MOR - 9Feb2011 - limits were found using pixels April 2010
         ;;veloLimitsVSL = [2.5,4.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC EN USO previo reajuste tabla por Uruguay
         veloLimitsVSL = [4.0,4.0,6.0]*xReal ;máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC ; URUGUAY  EN USOOOOOO
         ;veloLimitsVSL = [0.0,14.0,14.0];máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC ; URUGUAY  correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         ; --- Susana Creando vector para VSL --- FIN
         
         ; --- Susana - Para Barras de Rangos en Histogramas VSL y VCL - INICIO
         veloLimVSLb = [0.0,4.0,6.0]*xReal
         veloLimVCLb = veloLimitsVCL
         ;veloLimVSLb = [0.0,14.0,14.0];correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         ;veloLimVCLb = [0,3.33, 3.33,8., 8.,25., 25.,10000.];correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         ; --- Susana - Para Barras de Rangos en Histogramas VSL y VCL - FIN
         
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
         vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad A,B,C y D
         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         ; MOR - add vector for storing vsl values for the histogram - BEGIN
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum());
         ; MOR - add vector for storing vsl values for the histogram - END
         
         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
         
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
         
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - BEGIN
         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - END 
         
         ; MOR - 12Feb2011 - get concentration in time
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
         
         xBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBoxyBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         xBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBoxyBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana

         ; Susana - Optical Flow 1/2 -  BEGIN - 
           OFvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;OpticalFlow
           ;Sizevect = make_array((*state.poCurrTrackGroup)->getObjNum());Size
           DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Distancia
           DESPvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Desplazamiento
           DESP_DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); DESP/DIST
          
           s_OFParameter = 'Object Speed2 [pixelsPframe]'
           ;s_OFParameter = 'Object Vy [pixelsPframe]'
           strIDOF = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zpos) + '_'  + 'Clus' + string(clusPos) + '_', /rem)
            case selROIGroupObj of
             'oROI2DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Track Objects from Masks')
             'oROI3DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
             endcase
           ; sort objects in trackIDOF
           if (trackIDOF[0] ne -1) then begin
              whtrackIDOF = where(trackIDOF ne -1, count)
              if (count gt 0) then trackIDOF = trackIDOF[whtrackIDOF]
              sorttrackIDOF = fix(trackIDOF[uniq(trackIDOF, sort(trackIDOF))])
           endif

           case selROIGroupObj of
               'oROI2DGroup':trackParamsOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_OFParameter)
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase
           
           case selROIGroupObj of
               'oROI2DGroup':trackParamsTIME = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Object Time [s]')
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase           
           if (count gt 0) then begin
               trackParamsOF = trackParamsOF[whtrackIDOF]
               trackParamsTIME = trackParamsTIME[whtrackIDOF]
           endif
           
;           ; Size 1/2 - BEGIN - 
;           ;Size no me sirve, ya que es tamaño de cabecita brillante, no area de trayectoria
;           s_SizeParameter = 'Object Size [x²]';  'Object Size [x²]'  'Object Size [Pixel²]'   'Object Size [%]'
;           case selROIGroupObj of
;               'oROI2DGroup':trackParamsSize = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_SizeParameter)
;               ;'oROI3DGroup':trackParamsSize = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
;           endcase
;           if (count gt 0) then begin
;               trackParamsSize = trackParamsSize[whtrackIDOF]
;           endif
;           ; Size 1/2 - END -


         ; Susana - Optical Flow 1/2 -  END - 
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana
         
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
           vslTotal = vslVect[i] ;MOR changing calculation
           
           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
           
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
           
;                      ; Susana - Optical Flow 2/2 -  BEGIN - 
;            if (sorttrackIDOF[0] ne -1) then begin
;                whParamOF = where(trackIDOF eq sorttrackIDOF[i])
;                if (whParamOF[0] eq -1) then begin
;                    trackParamsOF_i = [min(trackParamsOF),max(trackParamsOF)]
;                    ;trackParamsSize_i = [min(trackParamsSize),max(trackParamsSize)]; Susana - Size 2/2       ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;                    trackParamsTIME_i = [min(trackParamsTIME),max(trackParamsTIME)]
;                endif else begin 
;                    trackParamsOF_i = trackParamsOF[whParamOF]
;                    ;trackParamsSize_i = trackParamsSize[whParamOF]; Susana - Size 2/2      ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;                    trackParamsTIME_i = trackParamsTIME[whParamOF]
;                endelse
;            endif
;            OFvect[i] = mean(trackParamsOF_i)
;            ;Sizevect[i] = trackParamsSize_i    ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;            trackParamsTIME_i_delta = 0
;            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
;              trackParamsTIME_i_delta = trackParamsTIME_i[1:N_ELEMENTS(trackParamsTIME_i)-1] - trackParamsTIME_i[0:N_ELEMENTS(trackParamsTIME_i)-2]
;            endif
;            ;endif else begin 
;            ;endelse
;            
;           ; Susana - Optical Flow 2/2 -  END - 
           
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
           ; check at what time/frame the object started being tracked
           firstAppeared = (*vclStruct.pTValues)[0]
           
           ; check how many frames the object was tracked
           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0]);
           
           ; check that all elements of the TO are not 'too close' to the border
           ;2D case --- @todo - need to figure out the 3D case
           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
           countBorder = 0
           if(firstAppeared gt 0) then begin
               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
               xO = (*vclStruct.pXYZValues)[0,0]/xReal
               yO = (*vclStruct.pXYZValues)[0,1]/yReal
               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
           endif
           ; create a bounding box of the track path -- convert trajectory to pixels
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
            
            ;xBox = 10000
            xBox = 0 ; Susana
            ;yBox = 10000
            yBox = 0 ;Susana
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
               ; find bounding center of mass values in each dimension
               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
               ; represent in whole pixels rather than subpixel positions
               xMin = floor(xMin)
               xMax = ceil(xMax)
               yMin = floor(yMin)
               yMax = ceil(yMax)
               ; check in each dimension what the number of pixels are
               xBox = xMax - xMin 
               yBox = yMax - yMin
            endif
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
           
           xBox_vect[i] = xBox ; Susana - evaluando xBox e yBox
           yBox_vect[i] = yBox ; Susana - evaluando xBox e yBox
           yBoxyBox_vect[i] = xBox_vect[i]*yBox_vect[i]
           
           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
           ;ALH , creo que no lo informo aùn no
           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)

           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
           
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
               if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
               endif else begin
                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
               endelse
             
;           ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- INICIO
            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
             ;desp=sqrt( ( (vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) )^2 ); en micrómetros, usando VSL
             ;DESPvect[i] = sqrt( ((((((*vclStruct.pXYZValues)[*,0])[(n_elements((*vclStruct.pXYZValues)[*,0]))-1])-(((*vclStruct.pXYZValues)[*,0])[0])))^2) + ((((((*vclStruct.pXYZValues)[*,1])[(n_elements((*vclStruct.pXYZValues)[*,1]))-1])-(((*vclStruct.pXYZValues)[*,1])[0])))^2) )
             DESPvect[i] = ((vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) ); en micrómetros, usando VSL    vslTotal*total(vclStruct.dT)
             
             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta))/xReal   ;en pixeles
             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta)); en micrómetros
             vDist = (vclStruct.XYZDisplacement); en micrómetros  ;vtotal = vclStruct.XYZDisplacement/vclStruct.dT
             DISTvect[i] = total(vDist); en pixeles
             ;desp_dist = desp/dist ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
             DESP_DISTvect[i] = DESPvect[i]/DISTvect[i] ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
           endif
;          ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- FIN

             ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - INICIO
             ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - INICIO
             if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 20)) then begin ; PChenlo
                  ;No motiles
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
               endif
           ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - FIN
           ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - FIN
             
             
             ;SUSANA - LIN - INICIO
             ;LIN
             ; MOR @todo - need to update the functions calling LIN, WOB, STR in C_sTrackGroupObject function in order to have
             ; the correct access to these calculations 
             
             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
             if (VCLvect[i] ne 0) then begin
               ;LINvect[i] = vslVect[i]/VCLvect[i]
               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
               ;WOBvect[i] = VAPvect[i]/VCLvect[i]
               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               LINvect[i] = 0
               WOBvect[i] = 0
             endelse
             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
             ; MOR STR calculation - BEGIN
             if (VAPvect[i] ne 0) then begin
               ;STRvect[i] = vslVect[i]/VAPvect[i]
               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               STRvect[i] = 0
            endelse
             ; MOR STR calculation - END
             
             ; MOR - create a vector to store the reason why something was classified as 'NC' - BEGIN
;             overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
             ; legend 
             ; classified for analysis - 0
             ; less than 2 data points - 1
             ; appeared late in time - 2
             ; tracked less than 50% of total time sample - 3
             ; too near the border at time of first track, if it is after the 1st frame - 4
             ; MOR - create a vector to store the reason why something was classified as 'NC' - END
           ;@TODO Susana and/or Victor: spermatozoa should be categorized as   for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
;            print,'D VCL <'+STRING(veloLimitsVCL[1])+' y/o '+' VSL <='+STRING(veloLimitsVSL[0])
;            print,'C VCL >='+STRING(veloLimitsVCL[2])+' y <'+STRING(veloLimitsVCL[3])+'   y/o   VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'C VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'B VCL >='+STRING(veloLimitsVCL[4])+' y <'+STRING(veloLimitsVCL[5])
;            print,'A VCL >='+STRING(veloLimitsVCL[6])

;             print,'--- velocidades'
;             print,veloLimitsVSL[0]
;             print,veloLimitsVSL[1]
;             
;             print,veloLimitsVCL[0]
;             print,veloLimitsVCL[1]
;             print,veloLimitsVCL[2] ;igual [1]
;             print,veloLimitsVCL[3] 
;             print,veloLimitsVCL[4] ;igual [3]
;             print,veloLimitsVCL[5] ;
;             print,veloLimitsVCL[6]

             case 1 of
             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
            ; MOR - 6Sept2010 - previous check for 'NC' classification
            ;((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) ): begin
               ; MOR - store reason for 'NC' - BEGIN
               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
                     overallType[i] = 1
                     col = [0,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1
                     break 
               endif
               
               if ((countBorder gt 0)) then begin
                     overallType[i] = 4
                     col = [0,255,120]
                     thickk = 2
                     ;vclVectTipo[i] = 'NCpp' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     cuentaTOsNC += 1
                     break
               endif
               
               if( (firstAppeared ge minTime)) then begin 
                     overallType[i] = 2
                     col = [0,120,255]
                     thickk = 2;
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               if ((timesTracked le minNumFramesTracked)) then begin 
                     overallType[i] = 3
                     col = [120,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif
               ; MOR - store reason for 'NC' - END
             end
;            ( (OFvect[i] le 1.5)  and (OFvect[i] ge 0.12) ): begin
;              ; con flujo laminar ? podria usar todos desde cero hasta 1.5 de speed2
;              col = [0,120,120]
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'IM' ;Susana
;              end
             ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - INICIO
             ;for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
             ;
             
             (vslTotal le veloLimitsVSL[1]): begin
              ;print,veloLimitsVSL[1];debe ser entre 14 y 3.9
              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
              ;col = [255,255,255]
              thickk = 4
              vclVectTipo[i] = 'IM' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado
              if ((VCLvect[i] ge veloLimitsVCL[3])) then begin  ; Default CEDAI
                ; ¿exijo ALHvect[i] ge 0.1?  para dejar solo los que se mueven mas ampliamente
                ;print, veloLimitsVCL[3]; debe ser >=8
                col = [0,255,0] ; No progresivos verdes "Tipo C"
                thickk = 4
                vclVectTipo[i] = 'NP' ;Susana NP*
                ;vslReasign[i]=i ; Solo #TrackObjectReasignado 
              endif
              end
;            (vslTotal le veloLimitsVSL[0]): begin
;              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
;              ;col = [255,255,255]
;              thickk = 4
;              vclVectTipo[i] = 'IM' ;Susana
;              vslReasign[i]=i ; Solo #TrackObjectReasignado
;              end
;            ((vslTotal gt veloLimitsVSL[0]) and (vslTotal le veloLimitsVSL[1])): begin
;              ;para ¿TipoB?  que deben ser TipoC
;              col = [0,255,0] ; No progresivos verdes "Tipo C"
;              ;col = [255,255,255]
;              thickk = 8;default=4
;              vclVectTipo[i] = 'NP' ;Susana
;              vslReasign[i]=i ; Solo #TrackObjectReasignado 
;              end
              ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - FIN
            ;(VCLvect[i] lt veloLimitsVCL[1]): begin; EN USO, previo Ajuste URUGUAY
            ;veloLimVSLb = [0.0,4.0,6.0]*xReal
            ;veloLimitsVCL2 = [0,4., 4.,5., 5.,30., 30.,10000.]*xReal
            (VCLvect[i] lt veloLimitsVCL[3]): begin ; URUGUAY
              ;MAYORIA PROBABLEMENTE YA FUERON ASIGNADOS AL FILTRAR POR VSL
              col = [0,0,255] ;inmoviles azules "Tipo D"
              ;thickk = 5;4 ;default
              thickk = 2;4 ;default
              vclVectTipo[i] = 'IM' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[2]) and (VCLvect[i] lt veloLimitsVCL[3]): begin
              ;GRUPO IMPROBABLE, PORQUE YA QUITÉ LOS QUE TIENEN VSL <=3.33UM/S
              ; ¿exijo ALHvect[i] ge 0.1?
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'NP' ;Susana
              print,'ocurrió lo imposible, se clasificó com NP, ¡¡ VSL puede ser mayor que VCL !!'
              end
            ;((VCLvect[i] ge veloLimitsVCL[6]) || ((VCLvect[i] ge veloLimitsVCL[4]) and (VCLvect[i] lt veloLimitsVCL[5]))): begin  ;cedai
            ((VCLvect[i] ge veloLimitsVCL[4])): begin  ;cedai
              col = [255,0,0] ;Progresivos rapidos rojos "Tipo A + B"
              ;thickk = 3 ;default
              thickk = 2
              vclVectTipo[i] = 'PR' ;Susana
              end
            ELSE: begin
              ;ocurrió lo imposible, no se clasificó
              col = [125,255,5] ; No progresivos verdes "Tipo C"
              thickk = 10
              vclVectTipo[i] = 'NC2' ;Susana
              print,'ocurrió lo imposible, no se clasificó - ver NC2'
            end
           endcase
           
           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'IM') ) then begin  
            ;and (STRvect[i] lt 50) 
            cuentaCirc += 1
            thickk = 2
           endif
           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
           
           if (vslTotal gt vslMaxim) then begin 
            vslMaxim = vslTotal
            vslMaximPos = i
           endif
           
           
           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
             np = (*state.poCurrTrackGroup)->getObjNum()
             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
             miX = (*VAPStruct.pXYZValues)[*,0]
             miY = (*VAPStruct.pXYZValues)[*,1]
             miZ = (*VAPStruct.pXYZValues)[*,2]
             miT = (*VAPStruct.pTValues)
             
             np = n_elements(miX)
             for j = 0, (np-1) do begin
;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
              if(vclVectTipo[i] ne 'NC') then begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL)+STRING(9B)+strcompress(xBox,/REMOVE_ALL)+STRING(9B)+strcompress(yBox,/REMOVE_ALL)+STRING(9B)+strcompress(xBox*yBox,/REMOVE_ALL);+STRING(10B) ;para IGOR   con xBox e yBox
               printF, Unit,temp
              endif
             endfor
;            ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - BEGIN
;             if(vclVectTipo[i] ne 'NC') then begin ;guardo coordenadas de los clasificados
;             for j = 0, (np-1) do begin
;                temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem);+STRING(10B) ;para IGOR
;               printF, Unit,temp
;              endfor
;             endif
;             ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - END           
           
           ; MOR - get entire time vector
           timeTrack[i] = n_elements(*VAPStruct.ptValues)
           
           
              ; Susana - Write OpticalFlowParameter in image - BEGIN 
              ;;v_OFmean = mean(trackParamsOF_i)
              ;;s_TagOF = strcompress("OF"+"_"+string(v_OFmean, format = '(F6.2)'),/rem)
              ;s_TagOF = strcompress("OF"+"_"+string(OFvect[i], format = '(F6.2)'),/rem)
              
                s_TagOF = strcompress("LIN"+"_"+string(LINvect[i], format = '(F6.2)'),/rem)
              
              ;;s_TagOF = strcompress("VCL"+"_"+string(VCLvect[i], format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VSL"+"_"+string(vslTotal, format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VAP"+"_"+string(VAPvect[i], format = '(F6.2)'),/rem)
               
              ;;;if((vclVectTipo[i] ne 'PR') and (vclVectTipo[i] ne 'IM')) then begin
              if((LINvect[i] gt 110.) and ((vslTotal/VCLvect[i]) lt 1)) then begin
              ;;if(OFvect[i] le 1.5) then begin  ;if((OFvect[i] le 1.5)  and (OFvect[i] ge 0.12)) then begin
                ;;oPlot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                Plot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [15, 15], locations = [((*vclStruct.pXYZValues)[0,0]+5)/xReal,((*vclStruct.pXYZValues)[0,1]+1)/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
                
                oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [16, 16], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
              endif
              ; Susana - Write OpticalFlowParameter in image - END 
            
            ;vvalor = 100
            ;if ((vslVect[i] ge vvalor) and (vclVectTipo[i] ne 'NC')) then thickk = 10 ; borrar, solo para destacar
            
           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
           if  (vclVectTipo[i] ne 'NC') then begin
           ; MOR - 05Jul2010
           ; convert track xyz to pixels for plotting against image
              if(vclVectTipo[i] eq 'IM') then begin
                ;;oSym = obj_new('IDLgrSymbol', data = 3, size = 2.5, thick = thickk);original data = 3=>Dot
                oSym = obj_new('IDLgrSymbol', data = 3, size = 5.5, thick = thickk); Dot de mayor tamaño; en uso
                ;oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);    solo para ver mejor los números borrar
              endif else begin
                ;oSym = obj_new('IDLgrSymbol', data = 2, thick = thickk);data = 2=>asterisco   thick=>line thickness
                oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);data = 0 =>no symbol   thick=>line thickness
              endelse
             
             ;if(i eq 53) then col=[0,255,255] 
             
              ;if(vclVectTipo[i] eq 'PR') then begin; borrar
              ;if(i eq 420) then begin; borrar
              ;if( i eq 352) then begin;
               oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oModel->add, oPlot
              ;endif
              ; Susana - figura para Maurcio Cerda, VCL VAP y VSL en colores sobre TrackObject - INICIO
              ; no funciona sobreponer trayectorias, la primera tapa la segunda
;              if(i eq 325) then begin;
              ;trayectoria curvilinea, VCL
              ;oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              ;oModel->add, oPlot
              
              ;trayectoria promedio, VAP
              ;oPlot = obj_new('IDLgrPlot', datax = (*VAPStruct.pXYZValues)[*,0]/xReal, datay = (*VAPStruct.pXYZValues)[*,1]/yReal, color = [255,255,50], thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym); amarillo
              ;oModel->add, oPlot
              
              ;trayectoria lineal, VSL
              ;oPlot = obj_new('IDLgrPlot', datax = [((*vclStruct.pXYZValues)[0,0]/xReal),((*vclStruct.pXYZValues)[(size((*vclStruct.pXYZValues)[*,0])-1),0]/xReal)], datay = [((*vclStruct.pXYZValues)[0,1]/yReal),((*vclStruct.pXYZValues)[(size((*vclStruct.pXYZValues)[*,1])-1),1]/yReal)], color = [0,255,100], thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              ;oModel->add, oPlot
              
;              endif
              ; Susana - figura para Maurcio Cerda, VCL VAP y VSL en colores sobre TrackObject - FIN
              ;endif ; borrar 
              
              ; Susana - Incluyo LIN temporalmente  INICIO
;              if((vclVectTipo[i] eq 'B')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;              if((vclVectTipo[i] eq 'NP')) then begin
;              ;if(i eq 50) then begin; borrar
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)'),/rem)+strcompress('_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' LIN_'+string(LINvect[i], format = '(F6.1)'),/rem)+strcompress('_ALH_'+string(ALHvect[i], format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [((*vclStruct.pXYZValues)[0,0]/xReal)-10,((*vclStruct.pXYZValues)[0,1]/xReal)-10],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;endif
;             ; Susana - Incluyo LIN temporalmente  INICIO
              

              
;             if((vclVectTipo[i] eq 'NP')) then begin; borrar
;             if(i ge 300 and i le 390) then begin; borrar
;;;             ; Susana - add TO number - INICIO
;;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [15, 15], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [255,255,255], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;              ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [10, 10], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             ;oPlot = obj_new('IDLgrText', STRINGS = '   '+strcompress('   '+string(i, format = '(I6)'),/rem),color = [0, 255, 0], CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;             
;             v_TOsSelectos = [97,135,146,241,242,232,251,237,209,148,191,104,118] ; DF1
;              v_TOsSelectos = [35,293,74,77,70,90,96,248,247,253,228,237,222,185,228,237,142,116,146,139,215,222,285,282,259,74,259,168,5,30,2,86,229,166,167,180,210,275]
;              v_TOsSelectos = [2,22,25,53,59,63,70,113,124,131,132,141,144,183,184,188,208,223,227,262,200] ; 27EneroM1_1   seleccionados por Luis Sarabia desde trayectoris de 5 segundos
;             v_TOsSelectos = [14,8];AB1
;             v_whereTOsSelectos = where(v_TOsSelectos eq i, countTOsSelectos)
;             if (countTOsSelectos gt 0) then begin
;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [0,1.0,0],UPDIR = [1.0, 1.0, 0.0])  ; Susana - With ROI number  vertical paralelo a Y
;             oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [11, 11], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [1.0,0.0,0],UPDIR = [-1.0, -1.0, 0.0])  ; Susana - With ROI number  horizontal con flip vertical para verlos bien en trayectories
;             oModel->add, oPlot
;             endif
;;;             ; Susana - add TO number - FIN
;             endif ; borrar


;              if(i eq 194) then begin
;                print,'TO='+string(i)+' ---------'
;                print,vclVectTipo[i]
;                print,vslTotal
;                print,VCLvect[i]
;                print,VAPvect[i]
;                print,LINvect[i]
;                print,STRvect[i]
;                ;print,(vslTotal/VCLvect[i])*100. ; LIN
;                ;print,(vslTotal/VAPvect[i])*100. ;STR
;                print,xBox_vect[i]
;                print,yBox_vect[i]
;                print,yBoxyBox_vect[i]
;                print,ALHvect[i]
;                print,' --------------------'
;              endif

           endif
           
           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
           ;name = 'CoordsTOs_'
           name = 'CoordsTOsTotalOMS_'+nom
           file = strCompress(tempFileName + name + '.dat', /remove)
           
           get_lun, U
            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            printF, U, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
           
           ; MOR - write to file the track objects overallType of classification - BEGIN    
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           printF, U1,temp 
           ; MOR - write to file the track objects overallType of classification - END
           
         endfor
;         print, countTONC_pocosPuntos ;Susana
;         print, countTONC_firstAppeared
;         print, countTONC_timesTracked
         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         close, Unit
         FREE_LUN, Unit
         
         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         ; MOR - close up file with overalltype classification - BEGIN
         close, U1
         FREE_LUN, U1
         ; MOR - close up file with overalltype classification - END         
         

         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ; MOR - 9Feb 2011 - change so the last directory name prints on Motil.tiff file
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         name = 'VelocityDataTotalOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceC'+STRING(9B)+'TOVeloc'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(9B)+'xBoxVeloc_pix'+STRING(9B)+'yBoxVeloc_pix'+STRING(9B)+'xBox_X_yBoxVeloc_pix'+STRING(9B)+'desp'+STRING(9B)+'dist'+STRING(9B)+'desp_dist'+STRING(9B)+s_OFParameter+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR  ; Susana  - con xBox e yBox sin SIZE
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]*yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESP_DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(OFvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR   Susana- con xBox e yBox sin Size
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         
         
         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = '_z'+strcompress(string(zPos),/REMOVE_ALL)+'_' ; susana incluye zPos a nombre imagen
         nom = '' ; susana NO incluye zPos a nombre imagen, es el resultado de todos los zslices ..
         name = 'Motil_MovVarsTotalOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceB'+STRING(9B)+'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'TypeMVars'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
         
           
         ;Susana INICIO --  Guardando #TrackObjects Reasignados
         name = 'NumTrackObjectsVSLTotalOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit,transpose(vslReasign)
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN --  Guardando #TrackObjects Reasignados
                 
         
         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
         np = (*state.poCurrTrackGroup)->getObjNum()
         meanVAP_A = 0; promedio tipos A
         meanVAP_B = 0; promedio tipos B
         meanVAP_C = 0; promedio tipos C
         meanVAP_D = 0; promedio tipos D
         meanVAP_NC = 0; promedio tipos NC
         
         meanVCL_A = 0; promedio tipos A
         meanVCL_B = 0; promedio tipos B
         meanVCL_C = 0; promedio tipos C
         meanVCL_D = 0; promedio tipos D
         meanVCL_NC = 0; promedio tipos NC
         
         meanVSL_A = 0; promedio tipos A
         meanVSL_B = 0; promedio tipos B
         meanVSL_C = 0; promedio tipos C
         meanVSL_D = 0; promedio tipos D
         meanVSL_NC = 0; promedio tipos NC
         
         meanLIN_A = 0; promedio LIN tipos A
         meanLIN_B = 0; promedio LIN tipos B
         meanLIN_C = 0; promedio LIN tipos C
         meanLIN_D = 0; promedio LIN tipos D
         meanLIN_NC = 0; promedio LIN tipos NC
         
         meanSTR_A = 0; promedio STR tipos A
         meanSTR_B = 0; promedio STR tipos B
         meanSTR_C = 0; promedio STR tipos C
         meanSTR_D = 0; promedio STR tipos D
         meanSTR_NC = 0; promedio STR tipos NC
         
         meanWOB_A = 0; promedio WOB tipos A
         meanWOB_B = 0; promedio WOB tipos B
         meanWOB_C = 0; promedio WOB tipos C
         meanWOB_D = 0; promedio WOB tipos D
         meanWOB_NC = 0; promedio WOB tipos NC
         
         meanVAP_Acnt = 0; counter para tipos A
         meanVAP_Bcnt = 0; counter para tipos B
         meanVAP_Ccnt = 0; counter para tipos C
         meanVAP_Dcnt = 0; counter para tipos D
         meanVAP_NCcnt = 0; counter para tipos NC
         ;desde aqui descomenté -Susana
         for h = 0, (np-1) do begin
          case 1 of
            (vclVectTipo[h] eq 'PR'): begin
              meanVAP_A += VAPvect[h]
              meanVCL_A += VCLvect[h]
              meanVSL_A += vslVect[h]
              meanLIN_A +=  LINvect[h]
              meanSTR_A +=  STRvect[h]
              meanWOB_A +=  WOBvect[h]
              ;meanALH_A += ALHvect[h]
              meanVAP_Acnt += 1
              end
            (vclVectTipo[h] eq 'B'): begin
              meanVAP_B += VAPvect[h]
              meanVCL_B += VCLvect[h]
              meanVSL_B += vslVect[h]
              meanLIN_B += LINvect[h]
              meanSTR_B +=  STRvect[h]
              meanWOB_B +=  WOBvect[h]
              ;meanALH_B += ALHvect[h]
              meanVAP_Bcnt += 1
              end
            (vclVectTipo[h] eq 'NP'): begin
              meanVAP_C += VAPvect[h]
              meanVCL_C += VCLvect[h]
              meanVSL_C += vslVect[h]
              meanLIN_C += LINvect[h]
              meanSTR_C +=  STRvect[h]
              meanWOB_C +=  WOBvect[h]
              ;meanALH_C += ALHvect[h]
              meanVAP_Ccnt += 1
              end
            (vclVectTipo[h] eq 'IM'): begin
              meanVAP_D += VAPvect[h]
              meanVCL_D += VCLvect[h]
              meanVSL_D += vslVect[h]
              meanLIN_D += LINvect[h]
              meanSTR_D +=  STRvect[h]
              meanWOB_D +=  WOBvect[h]
              ;meanALH_D += ALHvect[h]
              meanVAP_Dcnt += 1
              end
            (vclVectTipo[h] eq 'NC'): begin
              meanVAP_NC += VAPvect[h]
              meanVCL_NC += VCLvect[h]
              meanVSL_NC += vslVect[h]
              meanLIN_NC += LINvect[h]
              meanSTR_NC +=  STRvect[h]
              meanWOB_NC +=  WOBvect[h]
              ;meanALH_NC += ALHvect[h]
              meanVAP_NCcnt += 1
              end
           endcase
         endfor
         ;hasta aqui descomenté--Susana
         meanVAP_A /= meanVAP_Acnt
         meanVAP_B /= meanVAP_Bcnt
         meanVAP_C /= meanVAP_Ccnt
         meanVAP_D /= meanVAP_Dcnt
         meanVAP_NC /= meanVAP_NCcnt
         
         meanVCL_A /= meanVAP_Acnt
         meanVCL_B /= meanVAP_Bcnt
         meanVCL_C /= meanVAP_Ccnt
         meanVCL_D /= meanVAP_Dcnt
         meanVCL_NC /= meanVAP_NCcnt
         
         meanVSL_A /= meanVAP_Acnt
         meanVSL_B /= meanVAP_Bcnt
         meanVSL_C /= meanVAP_Ccnt
         meanVSL_D /= meanVAP_Dcnt
         meanVSL_NC /= meanVAP_NCcnt
         
         meanLIN_A /= meanVAP_Acnt
         meanLIN_B /= meanVAP_Bcnt
         meanLIN_C /= meanVAP_Ccnt
         meanLIN_D /= meanVAP_Dcnt
         meanLIN_NC /= meanVAP_NCcnt
         
         meanSTR_A /= meanVAP_Acnt
         meanSTR_B /= meanVAP_Bcnt
         meanSTR_C /= meanVAP_Ccnt
         meanSTR_D /= meanVAP_Dcnt
         meanSTR_NC /= meanVAP_NCcnt
         
         meanWOB_A /= meanVAP_Acnt
         meanWOB_B /= meanVAP_Bcnt
         meanWOB_C /= meanVAP_Ccnt
         meanWOB_D /= meanVAP_Dcnt
         meanWOB_NC /= meanVAP_NCcnt
         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
         
         
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         
         ; MOR - comment out old path for saving, try path where images are saved instead 
         write_tiff, tempFileName + 'Motil_trajectoryTotalOMS.tiff', outImage_1
         
;         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - INICIO
;         for v=0, totalTNum-1 do begin
;            widget_control, stack_tlb, get_uValue = stackState, /no_copy
;              imagev = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = v, chPos = chPos, zPos = zPos)
;            widget_control, stack_tlb, set_uValue = stackState, /no_copy
;            
;            ;oImagev = obj_new('IDLgrImage',imagev, xcoord_conv = xs, ycoord_conv = ys)
;            oImage->setProperty, data = imagev
;            oBuffer->draw, oView      
;            oOImage = oBuffer->read()
;            oOImage->getProperty, data = outImage_v
;         
;              write_tiff, tempFileName + '\Video\Motil_trajectoryTotalOMS_'+string(v,format='("",I3.3)')+'.tiff', outImage_v
;         endfor
;         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - FIN
         
;         ;Susana Comenta esto para poder correr lo de victor más adelante .. INICIO
;         ; MOR - destroy objects created for plotting to free up memory
;         obj_destroy, [oBuffer, oView]
;         ;Susana Comenta esto para poder correr lo de victor más adelante .. FIN
         
         ; MOR - add creation of VSL histogram - BEGIN
         path = tempFileName + 'VSL_HistoTotalOMS.tiff';path = tempFileName + 'VSL_HistoTotalOMS'+nom+'.tiff'
         
         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Original MIA - Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 ;Aqui reemplazo ne por eq, es decir envio al histograma todos los trackobjects menos los reasignados
         whClassified = where((vclVectTipo ne 'NC'), count); Susana - envío todos los clasificados
         if(whClassified[0] ne -1) then begin; Susana1
          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
;         vect = vslVect ; MOR - commented out previous code
          name = "rectilinear velocity VSL [um s-1]"
          ;name = "rectilinear velocity VSL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          s_Total = 'TotalOMS'; rotulo para nombre de archivo
          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
          CEDAI_histogramPlot_VSL_OMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName, veloLimVSLb = veloLimVSLb, s_Total = s_Total
          ;CEDAI_histogramPlot_VSL_OMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = ([2.5, 4.,6.]*xReal), linesFlag = linesFlag, tempFileName = tempFileName ; URUGUAY - eje x de histograma de 0 a 6
          ; MOR - add creation of VSL histogram - END
          v_histogVSL_OK = 1
         endif; Susana1
         ;endif else begin; Susana1
            ;
         ;endelse; Susana1
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;path = s_getPathForSystem() + 'VCL_HistoOMS.tiff'
         ;path = tempFileName + 'VCL_HistoOMS.tiff'
         path = tempFileName + 'VCL_HistoTotalOMS.tiff';path = tempFileName + 'VCL_HistoTotalOMS'+nom+'.tiff'
         
         
         v_histogVCL_OK = 0 ; Susana - controlar si existe histograma VCL de clasificados no reasignados
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
         whClassified = where((vclVectTipo ne 'NC'), count) ; MOR - 21 June 2011
         if(whClassified[0] ne -1) then begin; Susana2
          ; Susana - A B C o D, no reasignado
          vect = VCLvect[whClassified]
          name = "curvilinear velocity VCL [um s-1]"
          ;name = "curvilinear velocity VCL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          s_Total = 'TotalOMS'; rotulo para nombre de archivo
          linesFlag = 1
          CEDAI_histogramPlotOMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName, veloLimVCLb = veloLimVCLb, s_Total = s_Total
          v_histogVCL_OK = 1
         endif; Susana2
         ;endif else begin; Susana2
            ;nada
            ;El problema de no haber alternativa es que no crea imágenes como VCL_HistoOMS.tiff
         ;endelse; Susana2
         
         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
         
         ;Susana INICIO    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom2 = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
         nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
         ;name = 'DatosPChenlo_2011'
         name = 'DatosTotalOMS_'+nom2
         
         ;Susana - reemplazo tempFileName INICIO
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         ;Susana - reemplazo tempFileName FIN
         
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         get_lun, unitPCh
         OPENR, unitPCh, file, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitPCh, file
           ;printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         
         ;openW, unitPCh, file
         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
         ;recuentos
         aux = where(vclVectTipo eq 'PR', tipoA)
         aux = where(vclVectTipo eq 'B', tipoB)
         aux = where(vclVectTipo eq 'NP', tipoC)
         aux = where(vclVectTipo eq 'IM', tipoD)
         aux = where(vclVectTipo eq 'NC', tipoNC)
         ;porcentajes
         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
         ;print,tipoA+tipoB+tipoC+tipoD  corresponde a vect_elements
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         ;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana FIN    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         
         
         ;dummy = where(vect ge veloLimitsVCL[6], count)
         dummy = where(vclVectTipo eq 'PR', count) ;Susana
         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_0 = 'PR:';Progressively Motil
         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
;         dummy = where(vclVectTipo eq 'B', count) ;Susana
;         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
;         string_1 = 'Type B:' 
;         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[2] and vect lt veloLimitsVCL[3], count)   ;Susana
         dummy = where(vclVectTipo eq 'NP', count) ;Susana
         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_2 = 'NP:';Non-Progressively Motil
         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[0] and vect lt veloLimitsVCL[1], count)   ;Susana
         dummy = where(vclVectTipo eq 'IM', count) ;Susana
         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_3 = 'IM:';Immotile 
         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Motil_TextTotalOMS.tiff';fileName = tempFileName + 'Motil_TextTotalOMS'+nom+'.tiff'
         fileNameConc = tempFileName + 'Concentr_TextTotalOMS.tiff'
         
         ;SUSANA - Contabilizando trackobjects NC (No Clasificados) 
         dummy = where(vclVectTipo eq 'NC', count) ;Susana
         ;string_21 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_21 = strcompress( string(fix(count)), /remove_all) + ' sperms'
         ;string_23 = 'NC       :'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - INICIO
         string_23 =strcompress('#Traj.Circ.:' + string(fix(cuentaCirc)), /remove_all) + '(LIN<50) '+ '#NC:'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - FIN
         
         ;aqui estaba ;SUSANA- incluyo concentración - INICIO
         
         ; MOR - 12Feb 2011- average number of objects found per time
         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
         totalNumObjs.avg = stats[0]*conFactor; recuento promedio, promedia recuento espermios en todos los tiempos
         totalNumObjs.std = sqrt(stats[1]*conFactor)
         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
         totalNumObjs.se = sqrt(totalNumObjs.avg/conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         
         ;SUSANA- incluyo concentración -  1 cabeza/campo corresponde a 0.3143 millones/ml para campo 650.138umX483.436um en Makler 10prof - INICIO
         ;string_3AA = strcompress('Conc.: ')
         string_3AA = strcompress('Sperms:')
         string_7AA = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem)
         string_11AA = strcompress('10!U6!N/mL', /rem)
         
         ;;string_11AA = strcompress('[10!U6!N/mL]', /rem)+' ('+strcompress(string(totalNumObjs.avg/conFactor, format = '(I6.0)'), /rem)+' sperms)'
         ;string_11AA = strcompress('[10!U6!N/mL]', /rem); No informo el total de sperms usado para cálculo de concetracion
         
         ;SUSANA- incluyo concentración - FIN
         CEDAI_textPlotOMS, filename, string_0, string_2, string_3, string_3AA, string_4, string_6, string_7, string_7AA, string_8, string_10, string_11, string_11AA,string_21,string_23, nom; ordenamiento de A+B
         CEDAI_textPlotOMS_Conc, fileNameConc, string_3AA, string_7AA, string_11AA, nom; ordenamiento de A+B  - concentration
         
         
         string_0 = 'Concentration Statistics Summary'
;         string_1 = strcompress('Frames ') 
         string_2 = strcompress('Mean Conc.: ') ;string_2 = strcompress('Mean: ') 
;         string_3 = strcompress('SDev ') 
         string_4 = strcompress('Std. Error: ')
;         string_5 = strcompress('Median ') 
;         string_6 = strcompress('Min ') 
;         string_7 = strcompress('Max ') 
         
         string_8 = strcompress(string(totalNumObjs.number, format = '(D6.2)'), /rem) 
         string_9 = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem) 
         string_10 = strcompress(string(totalNumObjs.std, format = '(I6.2)'), /rem) 
         string_11 = strcompress(string(totalNumObjs.se, format = '(D6.2)'), /rem) 
         string_12 = strcompress(string(totalNumObjs.med, format = '(I6.2)'), /rem) 
         string_13 = strcompress(string(totalNumObjs.mini, format = '(I6.2)'), /rem) 
         string_14 = strcompress(string(totalNumObjs.maxi, format = '(I6.2)'), /rem) 
         
         string_15 = strcompress('[10E6/mL]', /rem) 
       
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Concentration_Text_ShortTotalOMS.tiff'
         
         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
         
         ; MOR - 12Feb2011 write total count data to .dat file 
         dum = max(timeTrack, maxI)
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
         
         name = 'ConcentrationTotalOMS'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit, 'zslice'+STRING(9B)+'Time' +string(9B)+ 'Count'+STRING(13B)
         indU = uniq(*numObjs.pTValues)       
         for i = 0, n_elements(indU)-1 do begin
            temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            printF, Unit,temp
         endfor
         close, Unit
         FREE_LUN, Unit
         
         ; MOR - save results in the same directory as images - BEGIN
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_Short.bmp'
         imageLeft =tempFileName + 'Motil_trajectoryTotalOMS.tiff';'+nom+'
         imageRight =  tempFileName + 'VCL_HistoTotalOMS.tiff';imageRight =  tempFileName + 'VCL_HistoTotalOMS'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoTotalOMS.tiff'
         textRight = tempFileName + 'Motil_TextTotalOMS.tiff'
         fileName = tempFileName + 'Motil_ShortTotalOMS.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Concentration_Text_ShortTotalOMS.tiff' 
         
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - INICIO -------------------
         imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motil_trajectoryTotal_Blanca.tiff'
         ;imageRight = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoTotal_Blanca.tiff'
         ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoTotal_Blanca.tiff'
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - FIN -------------------
         CEDAI_combineImages_Motil_Short, background, imageLeft, TextRight, fileName, textLeft
         
        ; MOR - save results in the same directory as images - BEGIN
        ;; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_ColorNaranjo.bmp'
         
;         imageLeft =tempFileName + 'Motil_trajectoryTotalOMS.tiff'
         imageLeft =tempFileName + 'Motil_trajectoryTotalOMS.tiff'
         imageRight =  tempFileName + 'VCL_HistoTotalOMS.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoTotalOMS.tiff'
         textRight = tempFileName + 'Motil_TextTotalOMS.tiff'
         textRight2 = tempFileName + 'Concentration_TextTotalOMS.tiff'
         
         ;fileName = tempFileName + 'Motil_newTotalOMS'+nom+'.tiff' 
         fileName = tempFileName + 'Motil_newTotalOMS.tiff'
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana 
         endif; Susana 
         
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - INICIO -------------------
         imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motil_trajectoryTotal_Blanca.tiff'
         ;imageRight = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoTotal_Blanca.tiff'
         ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoTotal_Blanca.tiff'
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - FIN -------------------
         v_guardaPNG = 1; 1=Guarda png también    0=No guarda png
         CEDAI_combineImages_motil_OMS, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft,fileNameConc,v_guardaPNG; incluye concentracion
         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - INICIO
         for v=0, totalTNum-1 do begin
            widget_control, stack_tlb, get_uValue = stackState, /no_copy
              imagev = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = v, chPos = chPos, zPos = zPos)
            widget_control, stack_tlb, set_uValue = stackState, /no_copy
            
            ;oImagev = obj_new('IDLgrImage',imagev, xcoord_conv = xs, ycoord_conv = ys)
            oImage->setProperty, data = imagev
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_v
            
            snomImageLeft =strcompress(tempFileName + '\Video\Motil_trajectoryTotalOMS_'+string(v,format='("",I3.3)')+'.tiff') ;Talvez esta cambie después ??
              write_tiff, snomImageLeft, outImage_v
              fileNameV = tempFileName + '\Video\Motil_newTotalOMS'+string(v,format='("",I3.3)')+'.tiff'
              
              ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - INICIO -------------------
              snomImageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motil_trajectoryTotal_Blanca.tiff'
              ;fileNameV = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motil_trajectoryTotal_Blanca.tiff'
              ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - FIN ------------------- 
              v_guardaPNG = 0
              CEDAI_combineImages_motil_OMS, background, snomImageLeft, imageRight, imageRight2, TextRight, fileNameV, textLeft,fileNameConc,v_guardaPNG
         endfor
         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - FIN
         ;Susana Incluye esto para poder correr lo de victor .. INICIO
         ; MOR - destroy objects created for plotting to free up memory
         obj_destroy, [oBuffer, oView]
         ;Susana Incluye esto para poder correr lo de victor .. FIN
         
         
         
         ;SUSANA - Incluyo VAP promedio por grupos LO ESTOY INTENTANDO, AUN NO CREO EL HISTOGRAMA PENDIENTES TAMBIEN HISTOGRAMAS STR Y WOB
         ;SUSANA - add creation of ALH histogram - BEGIN
         veloLimitsVCL = [0,50., 50.,200., 200.,400., 400.,10000.] ; no tengo rangos para ALH :( 
         path = tempFileName + 'histo_ALH'+nom+'.tiff'
         
         vect = ALHvect
         name = "ALH Amplitude of Lateral Head displacement [um]"+nom
         
         linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
         ;CEDAI_histogramPlot_ALH, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
         ;Falta generar un graficador de histograma para ALH o llamar internamente el GraphicModel ALH
         ;SUSANA - add creation of VSL histogram - END
         
         
         fileName = tempFileName + 'Motil_MovVariables_TextTotalOMS.tiff' ; MovementsVariables
         
         string_0 = 'PR:';Progressively Motil 
         ;string_4 = string(meanVAP_A, format = '(F6.2)') + ' um/s'
         string_4 = string(meanVAP_A, format = '(F6.1)')
         string_8 = strcompress(string(fix(meanLIN_A), format = '(F6.1)'), /remove_all)
         string_8_1 = strcompress(string(fix(meanSTR_A), format = '(F6.1)'), /remove_all)
         string_8_2 = strcompress(string(fix(meanWOB_A), format = '(F6.1)'), /remove_all)
         string_8_3 = strcompress(string(fix(meanVCL_A), format = '(F6.1)'), /remove_all)
         string_8_4 = strcompress(string(fix(meanVSL_A), format = '(F6.1)'), /remove_all)
         
;         string_1 = 'Type B:' 
;         string_5 = string(meanVAP_B, format = '(F6.1)')
;         string_9 = strcompress(string(fix(meanLIN_B), format = '(F6.1)'), /remove_all)
;         string_9_1 = strcompress(string(fix(meanSTR_B), format = '(F6.1)'), /remove_all)
;         string_9_2 = strcompress(string(fix(meanWOB_B), format = '(F6.1)'), /remove_all)
;         string_9_3 = strcompress(string(fix(meanVCL_B), format = '(F6.1)'), /remove_all)
;         string_9_4 = strcompress(string(fix(meanVSL_B), format = '(F6.1)'), /remove_all)
         
         string_1  = 'NP:' ;Non-Progressively Motil (
         ;string_6 = string(meanVAP_C, format = '(F6.2)') + ' um/s'
         string_5 = string(meanVAP_C, format = '(F6.1)')
         string_9 = strcompress(string(fix(meanLIN_C), format = '(F6.1)'), /remove_all)
         string_9_1 = strcompress(string(fix(meanSTR_C), format = '(F6.1)'), /remove_all)
         string_9_2 = strcompress(string(fix(meanWOB_C), format = '(F6.1)'), /remove_all)
         string_9_3 = strcompress(string(fix(meanVCL_C), format = '(F6.1)'), /remove_all)
         string_9_4 = strcompress(string(fix(meanVSL_C), format = '(F6.1)'), /remove_all)
         
         string_2 = 'IM:'  ; Immotile Talvez no deba incluirlo                              ; string_3
         string_6 = string(meanVAP_D, format = '(F6.1)') ;string_7
         string_10 = strcompress(string(fix(meanLIN_D), format = '(F6.1)'), /remove_all)    ;string_11
         string_10_1 = strcompress( string(fix(meanSTR_D), format = '(F6.1)'), /remove_all) ;string_11_1
         string_10_2 = strcompress( string(fix(meanWOB_D), format = '(F6.1)'), /remove_all) ;string_11_2
         string_10_3 = strcompress(string(fix(meanVCL_D), format = '(F6.1)'), /remove_all)  ;string_11_3
         string_10_4 = strcompress(string(fix(meanVSL_D), format = '(F6.1)'), /remove_all)  ;string_11_4
         
         string_3 = ''; y si uso este para la concentración???
         string_7 = ''
         string_11 = ''
         string_11_1 = ''
         string_11_2 = ''
         string_11_3 = ''
         string_11_4 = ''
         
         string_21 = strcompress('VAP')
         string_23 = strcompress('VSL')
         string_27 = strcompress('VCL')
         string_28 = strcompress('LIN')
         string_29 = strcompress('STR')
         string_30 = strcompress('WOB')
         string_31 = ' um/s'
         string_32 = ' %'
         ;CEDAI_textPlotMotil, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         CEDAI_textPlotMotilOMS, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         
         ; MOR - save results in the same directory as images - BEGIN
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         
         imageLeft =tempFileName + 'Motil_trajectoryTotalOMS'+nom+'.tiff' ;Talvez esta cambie después ??
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_MovVariables.bmp'
         
         imageRight =  tempFileName + 'VCL_HistoTotalOMS.tiff' ;'histo_ALH.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoTotalOMS.tiff'
         textRight = tempFileName + 'Motil_MovVariables_TextTotalOMS.tiff'
         fileName = tempFileName + 'Motil_MovVariablesTotalOMS.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png';Susana 
         endif; Susana
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana
          
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - INICIO -------------------
         imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motil_trajectoryTotal_Blanca.tiff'
         ;imageRight = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoTotal_Blanca.tiff'
         ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoTotal_Blanca.tiff'
         ;Susana - aqui reemplazo imágenes de trayectoria e histogramas .. por imágenes en blanco - FIN -------------------
         CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, TextRight, filename,1
         
       ;Track MotilTotalOMS -- FIN
      endcase
      'Track Motil OMS Ajustada':begin; IMPORTANTE: LOS RANGOS DE VELOCIDADES PARA CLASIFICACION SON INGRESADOS DIRECTAMENTE EN UM/S NO EN PIXELES COMO EN TRACK MOTIL, TRACK MOTIL TOTAL, TRACK MOTIL OMS Y TRACK MOTIL OMS TOTAL
         ;for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
         ;;veloLimitsVCL = [0,3.33, 3.33,8.0, 8.0,25., 25.,10000.] ; MOR - 9Feb2011 - limits were found using pixels April 2010  URUGUAY    ;correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         veloLimitsVCL = [0,3.33, 3.33,9.0, 9.0,25., 25.,10000.] ; MOR - 9Feb2011 - limits were found using pixels April 2010  URUGUAY    ;correccion 22 Mayo 2013
         ;;veloLimitsVCL = [0,3.33, 3.33,10.0, 10.0,25., 25.,10000.]; 24 Mayo valores Luis Sarabia, The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
         pParamStruct = (*state.poCurrTrackGroup)->getpParamStruct()
         
         tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
         tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
         tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
         vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = initModel, objNum = 0)
         
         ; MOR - determine cutoffs for not including TOs in analysis - BEGIN
         ; do not include in analysis if started after the xx frame in time
         ; number of frames
         minTimeF = 5;  will be value in [frames]
         minTime = double(minTimeF) * (1.0*(tStart+tInt)/(1.0*tEnd)) ;[s]
         ;minTime = 0.16666668 ; solo de prueba para 5 segundos
         ; only keep those TOs which have enough (50% of total number of frames) frames to determine velocity 
         minNumFramesTracked = floor((tEnd-tStart)*0.50);  value in [frames]
         ;minNumFramesTracked = 15; solo de prueba para 5 segundos

         
         ; how close to the border the object is 
         closeBorder = 9; [pixels] -- this value is the same as max_Dist in C_sROIParam_ObjTrackObject.pro    
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_z'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         
         ; write to file the track objects overallType of classification
         name = 'overallTypeTOsOMS_'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)  
         get_lun, U1
         openW, U1, file
         printF, U1, 'zslice'+STRING(9B)+'TO'+STRING(9B)+'TypeNC'
         ; threshold area [in pixels] that the track covers
         areaThreshold = 3; this is 2x2 pixels large, Default = 2
         ; MOR - determine cutoffs for not including TOs in analysis - END
         
         image_size = size(image,/dim)
         image_x = image_size[0]
         image_y = image_size[1]
         yr = [0, max(image_y)]
         xr = [0, max(image_x)]
         k = 1
         y = .45
         xs = [k+45,y*2]
         ys = [k+45,y*2]
         
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)

         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - INICIO
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - FIN
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange

         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
        
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image - END       
                                
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oImage
         oModel->add, oPlot
         oModel->add, oXAxis
         oModel->add, oYAxis
         oModel->add, oYAxisUp
         oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         
         ;Susana INICIO PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2])
         name = 'CoordenadasTOsOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         get_lun, unit
         openW, Unit, file
;         np = (*state.poCurrTrackGroup)->getObjNum()
         ;;printF, Unit, 'Ts'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
         ;;;;;;; MOR - temporarily printint the color code -----
         printF, Unit, 'zsliceA'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'Type'+STRING(9B)+'xBox_pix'+STRING(9B)+'yBox_pix'+STRING(9B)+'xBox_X_yBox_pix';+STRING(10B) ;13B  carriage return     10B Linefeed  9B Horizontal Tab ;ParaIGOR
         ;Susana FIN PARTE_IdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         vslMaxim = 0
         vslMaximPos = 0
         cuentaCirc = 0 ;contador de espermatozoides con trajectoria circular
         ; --- Susana Creando vector para VSL --- INICIO
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;contenero de VSLs
         ;crear contenedor de clasificacion de Tipo VCL ABCD para llenar en siguiente evaluacion for-endfor  y reutilizar al generar los rotulos de %s
         vclVectTipo = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         vclVectTipoVSL = make_array((*state.poCurrTrackGroup)->getObjNum(),/STRING)
         umbralVCL = 50 ;10
         ; MOR - 9Feb2011 - limits were found using pixels April 2010
         
         veloLimitsVSL = [0.0,5.6,5.6];máxima VSL que podria presentar un TipoC en 1 segundo, sustituye umbralVSL   y  umbralVSL_TipoC ; URUGUAY  correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         ;;veloLimitsVSL = [0.0,10.0,10.0]; 24 Mayo valores Luis Sarabia, The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
         ; --- Susana Creando vector para VSL --- FIN
         
         ; --- Susana - Para Barras de Rangos en Histogramas VSL y VCL - INICIO
         ;veloLimVSLb = [0.0,5.6,5.6];correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         veloLimVSLb = veloLimitsVSL; 24 Mayo valores Luis Sarabia, The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
         
         ;;veloLimVCLb = [0,3.33, 3.33,8., 8.,25., 25.,10000.];correccion 2 Mayo 2013  PARA ABARCAR MAS NOPROGRESIVOS
         veloLimVCLb = [0,3.33, 3.33,9., 9.,25., 25.,10000.];correccion 22 Mayo 2013
         ;veloLimVCLb = [0,3.33, 3.33,10., 10.,25., 25.,10000.]; 24 Mayo valores Luis Sarabia nooooo , The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
         ; --- Susana - Para Barras de Rangos en Histogramas VSL y VCL - FIN
         
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- INICIO --
         np = (*state.poCurrTrackGroup)->getObjNum()
         ;vslReasign = make_array(np+10000,/INTEGER) ; MOR - 22 June 2011 - comment out
;         vslReasign[*] = -1  ; MOR - 22 June 2011 - comment out
         vslReasign = make_array( (*state.poCurrTrackGroup)->getObjNum(),/INTEGER, value = -1)
         minTimes = 3   ;Solo usado para guardar solo coordenadas de TOs que con más de minTimes puntos...     minTimes inicialmente era el umbral apariciones (puntos) de un TO sobre el cual es aceptado y considerado para calcular % de TiposMovilidad A,B,C y D
         cuentaTOsNC=0   ;cuenta TOs NoClasificados por problemas de calculo en alguna de las velocidades, VSL  VCL  o VAP
         ; --- Susana Creando array para guardar #TrackObjects reasignados usando VSL -- FIN --
         
         VCLvect = make_array((*state.poCurrTrackGroup)->getObjNum())
         ; MOR - add vector for storing vsl values for the histogram - BEGIN
         vslVect = make_array((*state.poCurrTrackGroup)->getObjNum());
         ; MOR - add vector for storing vsl values for the histogram - END
         
         VAPvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector VAP
         
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- INICIO
         ALHvect = make_array((*state.poCurrTrackGroup)->getObjNum());Susana  vector ALH
         LINvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding LIN 
         WOBvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  WOB
         STRvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding  STR
         
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - BEGIN
         overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
         ; MOR - 7Sept2010 - generate ROIs for each track object, calculate the area covered by
         ; the track and use this for classification of type of sperm - END 
         
         ; MOR - 12Feb2011 - get concentration in time
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = 0)
         timeTrack = make_array((*state.poCurrTrackGroup)->getObjNum()) ; MOR vector holding max number of times object was tracked
         ;SUSANA - incluyo ALH LIN STR WOB Parte 1- FIN
         
         xBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         yBoxyBox_vect = make_array((*state.poCurrTrackGroup)->getObjNum())
         xBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         yBoxyBox_vect[*] = 0 ; Susana - creating for evaluate xBox and yBox
         
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana

         ; Susana - Optical Flow 1/2 -  BEGIN - 
           OFvect = make_array((*state.poCurrTrackGroup)->getObjNum()) ;OpticalFlow
           ;Sizevect = make_array((*state.poCurrTrackGroup)->getObjNum());Size
           DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Distancia
           DESPvect = make_array((*state.poCurrTrackGroup)->getObjNum()); Desplazamiento
           DESP_DISTvect = make_array((*state.poCurrTrackGroup)->getObjNum()); DESP/DIST
          
           s_OFParameter = 'Object Speed2 [pixelsPframe]'
           ;s_OFParameter = 'Object Vy [pixelsPframe]'
           strIDOF = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zpos) + '_'  + 'Clus' + string(clusPos) + '_', /rem)
            case selROIGroupObj of
             'oROI2DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Track Objects from Masks')
             'oROI3DGroup':trackIDOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
             endcase
           ; sort objects in trackIDOF
           if (trackIDOF[0] ne -1) then begin
              whtrackIDOF = where(trackIDOF ne -1, count)
              if (count gt 0) then trackIDOF = trackIDOF[whtrackIDOF]
              sorttrackIDOF = fix(trackIDOF[uniq(trackIDOF, sort(trackIDOF))])
           endif

           case selROIGroupObj of
               'oROI2DGroup':trackParamsOF = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_OFParameter)
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase
           
           case selROIGroupObj of
               'oROI2DGroup':trackParamsTIME = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + 'Object Time [s]')
               ;'oROI3DGroup':trackParamsOF = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
           endcase           
           if (count gt 0) then begin
               trackParamsOF = trackParamsOF[whtrackIDOF]
               trackParamsTIME = trackParamsTIME[whtrackIDOF]
           endif
           
;           ; Size 1/2 - BEGIN - 
;           ;Size no me sirve, ya que es tamaño de cabecita brillante, no area de trayectoria
;           s_SizeParameter = 'Object Size [x²]';  'Object Size [x²]'  'Object Size [Pixel²]'   'Object Size [%]'
;           case selROIGroupObj of
;               'oROI2DGroup':trackParamsSize = *roiState.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + s_SizeParameter)
;               ;'oROI3DGroup':trackParamsSize = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDOF + '3D Track Objects from Masks')
;           endcase
;           if (count gt 0) then begin
;               trackParamsSize = trackParamsSize[whtrackIDOF]
;           endif
;           ; Size 1/2 - END -


         ; Susana - Optical Flow 1/2 -  END - 
         countTONC_pocosPuntos = 0 ;Susana
         countTONC_firstAppeared = 0 ;Susana
         countTONC_timesTracked = 0 ;Susana
         
         for i = 0, (*state.poCurrTrackGroup)->getObjNum()-1 do begin
           vslStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VSL', objNum = i)
           vslVect[i] = VSLStruct.VSL ;MOR changing calculation
           vslTotal = vslVect[i] ;MOR changing calculation
           
           VAPStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VAP', objNum = i)
           VAPvect[i] = mean(VAPStruct.VAP) ;MOR changing calculation
           
           vclStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = i)
           
;                      ; Susana - Optical Flow 2/2 -  BEGIN - 
;            if (sorttrackIDOF[0] ne -1) then begin
;                whParamOF = where(trackIDOF eq sorttrackIDOF[i])
;                if (whParamOF[0] eq -1) then begin
;                    trackParamsOF_i = [min(trackParamsOF),max(trackParamsOF)]
;                    ;trackParamsSize_i = [min(trackParamsSize),max(trackParamsSize)]; Susana - Size 2/2       ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;                    trackParamsTIME_i = [min(trackParamsTIME),max(trackParamsTIME)]
;                endif else begin 
;                    trackParamsOF_i = trackParamsOF[whParamOF]
;                    ;trackParamsSize_i = trackParamsSize[whParamOF]; Susana - Size 2/2      ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;                    trackParamsTIME_i = trackParamsTIME[whParamOF]
;                endelse
;            endif
;            OFvect[i] = mean(trackParamsOF_i)
;            ;Sizevect[i] = trackParamsSize_i    ;Size no me sirve, no es area de trayectoria sinó que de punto brillante de cabeza
;            trackParamsTIME_i_delta = 0
;            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
;              trackParamsTIME_i_delta = trackParamsTIME_i[1:N_ELEMENTS(trackParamsTIME_i)-1] - trackParamsTIME_i[0:N_ELEMENTS(trackParamsTIME_i)-2]
;            endif
;            ;endif else begin 
;            ;endelse
;            
;           ; Susana - Optical Flow 2/2 -  END - 
           
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - BEGIN
           ; check at what time/frame the object started being tracked
           firstAppeared = (*vclStruct.pTValues)[0]
           
           ; check how many frames the object was tracked
           timesTracked = n_elements((*vclStruct.pXYZValues)[*,0]);
           ; check that all elements of the TO are not 'too close' to the border
           ;2D case --- @todo - need to figure out the 3D case
           ; determine the distance of the center of mass of the TOs at each time from the borders of the image
           ; for all of the track objects that appeared after the first frame/time, check if they entered from the border to the FOV
           countBorder = 0
           if(firstAppeared gt 0) then begin
               xE = sqrt((image_x - (*vclStruct.pXYZValues)[0,0]/xReal)^2) ;¿para cada TO en imagen o para un mismo TO? calculo la distancia hacia bordes   image_x = image_size[0] en x
               yE = sqrt((image_y - (*vclStruct.pXYZValues)[0,1]/yReal)^2)
               xO = (*vclStruct.pXYZValues)[0,0]/xReal
               yO = (*vclStruct.pXYZValues)[0,1]/yReal
               onBorder  = where([xE, yE, xO, yO] lt closeBorder, countBorder)
           endif
           ; create a bounding box of the track path -- convert trajectory to pixels
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- INICIO
            
            ;xBox = 10000
            xBox = 0 ; Susana
            ;yBox = 10000
            yBox = 0 ;Susana
            ; Susana incluye estas líneas pues no se creó xBox anteriormente con muestra 28Ene2010_M3_2 -- FIN
           if (n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) then begin
               ; find bounding center of mass values in each dimension
               xMax = max((*vclStruct.pXYZValues)[*,0]/xReal, min = xMin)
               yMax = max((*vclStruct.pXYZValues)[*,1]/yReal, min = yMin)
               ; represent in whole pixels rather than subpixel positions
               xMin = floor(xMin)
               xMax = ceil(xMax)
               yMin = floor(yMin)
               yMax = ceil(yMax)
               ; check in each dimension what the number of pixels are
               xBox = xMax - xMin 
               yBox = yMax - yMin
            endif
           ; MOR - remove any track objects which do not have sufficient number of frames for analysis - END      
           
           xBox_vect[i] = xBox ; Susana - evaluando xBox e yBox
           yBox_vect[i] = yBox ; Susana - evaluando xBox e yBox
           yBoxyBox_vect[i] = xBox_vect[i]*yBox_vect[i]
           
           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- INICIO
           ;ALH , creo que no lo informo aùn no
           ALHStruct = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track ALH', objNum = i)
           if ALHStruct.ok then ALH = make_array((*state.poCurrTrackGroup)->getObjNum())      
           if ALHStruct.ok then ALHvect[i] = mean(ALHStruct.ALH)

           ;SUSANA - incluyo ALH LIN STR WOB Parte 2- FIN
           
           if vclStruct.ok then vtotal = vclStruct.XYZDisplacement/vclStruct.dT ;MOR changing calculation
           
           ; MOR - (areaGeo[i] < areaThreshold) then label velocities as 0 - BEGIN
;               if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and (xBox lt areaThreshold and yBox lt areaThreshold)) then begin 
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
;               endif else begin
                  VCLvect[i] = mean(vtotal) ;MOR changing calculation
;               endelse
;             if(i eq 70) then begin
;              print, 'TO 200  timesTracked  VSL  VCL ------------------'
;              print,timesTracked
;              print,vslTotal
;              print,VCLvect[i]
;              print,string(mean(vtotal))
;              print,string(mean(vtotal), format = '(F6.6)'); no funciona
;              print,string(mean(vtotal), format = '(D6.6)'); no funciona
;            endif
;           ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- INICIO
            if (N_ELEMENTS(trackParamsTIME_i) gt 1) then begin
             ;desp=sqrt( ( (vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) )^2 ); en micrómetros, usando VSL
             ;DESPvect[i] = sqrt( ((((((*vclStruct.pXYZValues)[*,0])[(n_elements((*vclStruct.pXYZValues)[*,0]))-1])-(((*vclStruct.pXYZValues)[*,0])[0])))^2) + ((((((*vclStruct.pXYZValues)[*,1])[(n_elements((*vclStruct.pXYZValues)[*,1]))-1])-(((*vclStruct.pXYZValues)[*,1])[0])))^2) )
             DESPvect[i] = ((vslTotal)*((*vclStruct.pTValues)[(n_elements((*vclStruct.pXYZValues)[*,0]))-1]-(*vclStruct.pTValues)[0]) ); en micrómetros, usando VSL    vslTotal*total(vclStruct.dT)
             
             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta))/xReal   ;en pixeles
             ;vDist = ((vclStruct.XYZDisplacement)*(trackParamsTIME_i_delta)); en micrómetros
             vDist = (vclStruct.XYZDisplacement); en micrómetros  ;vtotal = vclStruct.XYZDisplacement/vclStruct.dT
             DISTvect[i] = total(vDist); en pixeles
             ;desp_dist = desp/dist ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
             DESP_DISTvect[i] = DESPvect[i]/DISTvect[i] ; si desp/dist es cercana a 1  la trayectoria es muy recta y potencial DRIFT
           endif
;          ; Susana - DRIFT Por encargo de Mauricio Cerda, relación entre desplazamientoTotal y distanciaRecorrida- FIN

             ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - INICIO
             ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - INICIO
             if((n_elements((*vclStruct.pXYZValues)[*,0]) gt 1) and ((xBox*yBox) le 20)) then begin ; PChenlo
                  ;No motiles
;                  VCLvect[i] = 0
;                  vslVect[i] = 0  
;                  VAPvect[i] = 0
;                  vslTotal = 0
               endif
           ; Susana - Pruebo ajustar velocidades de TOs con pequeñas Areas de Trayectoria, para ser clasificados como estáticos - FIN
           ;COMENTADO PARA OBTENER DATO NUEVOS DE CALIBRACIÓN PARA RODRIGO - 20 JULIO 2012 - FIN
             
             
             ;SUSANA - LIN - INICIO
             ;LIN
             ; MOR @todo - need to update the functions calling LIN, WOB, STR in C_sTrackGroupObject function in order to have
             ; the correct access to these calculations 
             
             ; MOR LIN, WOB, calculation - use VCL, VSL, VAP defined in 'Track Motil' rather than  getXXX() function - BEGIN
             if (VCLvect[i] ne 0) then begin
               ;LINvect[i] = vslVect[i]/VCLvect[i]
               LINvect[i] = (vslVect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
               ;WOBvect[i] = VAPvect[i]/VCLvect[i]
               WOBvect[i] = (VAPvect[i]*100)/VCLvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               LINvect[i] = 0
               WOBvect[i] = 0
             endelse
             ; MOR LIN, WOB calculation - use VCL & VSL defined in 'Track Motil' rather than getXXX() function - END
             ; MOR STR calculation - BEGIN
             if (VAPvect[i] ne 0) then begin
               ;STRvect[i] = vslVect[i]/VAPvect[i]
               STRvect[i] = (vslVect[i]*100.)/VAPvect[i] ;SUSANA - expresando de 0 a 100% ???
             endif else begin
               STRvect[i] = 0
            endelse
             ; MOR STR calculation - END
             
             ; MOR - create a vector to store the reason why something was classified as 'NC' - BEGIN
;             overallType = make_array((*state.poCurrTrackGroup)->getObjNum(),value = 0)   
             ; legend 
             ; classified for analysis - 0
             ; less than 2 data points - 1
             ; appeared late in time - 2
             ; tracked less than 50% of total time sample - 3
             ; too near the border at time of first track, if it is after the 1st frame - 4
             ; MOR - create a vector to store the reason why something was classified as 'NC' - END
           ;@TODO Susana and/or Victor: spermatozoa should be categorized as   for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
;            print,'D VCL <'+STRING(veloLimitsVCL[1])+' y/o '+' VSL <='+STRING(veloLimitsVSL[0])
;            print,'C VCL >='+STRING(veloLimitsVCL[2])+' y <'+STRING(veloLimitsVCL[3])+'   y/o   VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'C VSL >'+STRING(veloLimitsVSL[0])+' y <='+STRING(veloLimitsVSL[1])
;            print,'B VCL >='+STRING(veloLimitsVCL[4])+' y <'+STRING(veloLimitsVCL[5])
;            print,'A VCL >='+STRING(veloLimitsVCL[6])

;             print,'--- velocidades'
;             print,veloLimitsVSL[0]
;             print,veloLimitsVSL[1]
;             
;             print,veloLimitsVCL[0]
;             print,veloLimitsVCL[1]
;             print,veloLimitsVCL[2] ;igual [1]
;             print,veloLimitsVCL[3] 
;             print,veloLimitsVCL[4] ;igual [3]
;             print,veloLimitsVCL[5] ;
;             print,veloLimitsVCL[6]

             case 1 of
             ((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) or (firstAppeared ge minTime) or (timesTracked le minNumFramesTracked) or countBorder gt 0): begin
            ; MOR - 6Sept2010 - previous check for 'NC' classification
             ;print,'TO es NC por minTime o minNumFramesTracked o countBorder ?? #'+STRING(i)
            ;((VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1) ): begin
               ; MOR - store reason for 'NC' - BEGIN
               if ( (VCLvect[i] eq -1) or (vslTotal eq -1) or (VAPvect[i] eq -1)) then begin 
                     overallType[i] = 1
                     col = [0,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC' 
                     cuentaTOsNC += 1
                     break 
               endif
               
               if ((countBorder gt 0)) then begin
                     overallType[i] = 4
                     col = [0,255,120]
                     thickk = 2
                     ;vclVectTipo[i] = 'NCpp' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     vclVectTipo[i] = 'NC' ;Susana NC no clasificable por pocos puntos, dañando calculo de VCL VAP o VSL
                     cuentaTOsNC += 1
                     break
               endif
               
               if( (firstAppeared ge minTime)) then begin 
                     overallType[i] = 2
                     col = [0,120,255]
                     thickk = 2;
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif

               if ((timesTracked le minNumFramesTracked)) then begin 
                     overallType[i] = 3
                     col = [120,255,255]
                     thickk = 2
                     vclVectTipo[i] = 'NC'
                     cuentaTOsNC += 1
                     break
               endif
               ; MOR - store reason for 'NC' - END
             end
;            ( (OFvect[i] le 1.5)  and (OFvect[i] ge 0.12) ): begin
;              ; con flujo laminar ? podria usar todos desde cero hasta 1.5 de speed2
;              col = [0,120,120]
;              ;thickk = 3 ;default
;              thickk = 2
;              vclVectTipo[i] = 'IM' ;Susana
;              end
             ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - INICIO
             ;for A and B => Progresssively Motil (PR)     C => Non-Progressively Motil (NP)   D => Immotile (IM)
             ;
             
             (vslTotal le veloLimitsVSL[1]): begin
              ;print,veloLimitsVSL[1];debe ser entre 14 y 3.9
              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
              ;col = [255,255,255]
              thickk = 2;4
              vclVectTipo[i] = 'IM' ;Susana
              vslReasign[i]=i ; Solo #TrackObjectReasignado
              if ((VCLvect[i] ge veloLimitsVCL[3])) then begin  ; Default CEDAI
                ; ¿exijo ALHvect[i] ge 0.1?  para dejar solo los que se mueven mas ampliamente
                ;print, veloLimitsVCL[3]; debe ser >=8
                col = [0,255,0] ; No progresivos verdes "Tipo C"
                thickk = 2;4
                vclVectTipo[i] = 'NP' ;Susana NP*
                ;vslReasign[i]=i ; Solo #TrackObjectReasignado 
              endif
              end
              
;              (LINvect[i] lt 0.58): begin; 24 Mayo valores Luis Sarabia, The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
;                ;¿trayectorias en pequeño círculo?
;                col = [0,255,0] ; No progresivos verdes "Tipo C"; 24 Mayo valores Luis Sarabia, The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
;                thickk = 2; 24 Mayo valores Luis Sarabia, The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
;                vclVectTipo[i] = 'NP' ;Susana NP*; 24 Mayo valores Luis Sarabia, The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
;                ;vslReasign[i]=i ; Solo #TrackObjectReasignado 
;              end; 24 Mayo valores Luis Sarabia, The open Andrology Journal, 2010 Volumen 2 pag 42-43, NO PROGRESIVO   VSL < o = 10 um  o VSL > 10um con LIN < 0.58
              
;            (vslTotal le veloLimitsVSL[0]): begin
;              col = [0,0,255] ;inmoviles azules "Tipo D" ;Susana
;              ;col = [255,255,255]
;              thickk = 4
;              vclVectTipo[i] = 'IM' ;Susana
;              vslReasign[i]=i ; Solo #TrackObjectReasignado
;              end
;            ((vslTotal gt veloLimitsVSL[0]) and (vslTotal le veloLimitsVSL[1])): begin
;              ;para ¿TipoB?  que deben ser TipoC
;              col = [0,255,0] ; No progresivos verdes "Tipo C"
;              ;col = [255,255,255]
;              thickk = 8;default=4
;              vclVectTipo[i] = 'NP' ;Susana
;              vslReasign[i]=i ; Solo #TrackObjectReasignado 
;              end
              ; SUSANA - COMENTO TEPORALMENTE LINEAS DE REASIGACIÓN SEGÚN VSL  - FIN
            ;(VCLvect[i] lt veloLimitsVCL[1]): begin; EN USO, previo Ajuste URUGUAY
            ;veloLimVSLb = [0.0,4.0,6.0]*xReal
            ;veloLimitsVCL2 = [0,4., 4.,5., 5.,30., 30.,10000.]*xReal
            (VCLvect[i] lt veloLimitsVCL[3]): begin ; URUGUAY
              ;MAYORIA PROBABLEMENTE YA FUERON ASIGNADOS AL FILTRAR POR VSL
              col = [0,0,255] ;inmoviles azules "Tipo D"
              ;thickk = 5;4 ;default
              thickk = 2;4 ;default
              vclVectTipo[i] = 'IM' ;Susana
              end
            (VCLvect[i] ge veloLimitsVCL[2]) and (VCLvect[i] lt veloLimitsVCL[3]): begin ; de veloLimitsVCL posiciones [2] y [3] son actualmente iguales
              ;GRUPO IMPROBABLE, PORQUE YA QUITÉ LOS QUE TIENEN VSL <=3.33UM/S
              ;ACÁ PODRÍA INCLUIR LOS NO PROGRESIVOS POR TRAJECTORIA EN PEQUEÑO CIRCULO
              ; ¿exijo ALHvect[i] ge 0.1?
              col = [0,255,0] ; No progresivos verdes "Tipo C"
              thickk = 2
              vclVectTipo[i] = 'NP' ;Susana
              print,'ocurrió lo imposible, se clasificó com NP, ¡¡ VSL puede ser mayor que VCL !!'
              end
            ;((VCLvect[i] ge veloLimitsVCL[6]) || ((VCLvect[i] ge veloLimitsVCL[4]) and (VCLvect[i] lt veloLimitsVCL[5]))): begin  ;cedai
            ((VCLvect[i] ge veloLimitsVCL[4])): begin  ;cedai
              col = [255,0,0] ;Progresivos rapidos rojos "Tipo A + B"
              thickk = 2
              vclVectTipo[i] = 'PR' ;Susana
              end
            ELSE: begin
              ;ocurrió lo imposible, no se clasificó
              col = [125,255,5] ; No progresivos verdes "Tipo C"
              thickk = 10
              vclVectTipo[i] = 'NC2' ;Susana
              print,'ocurrió lo imposible, no se clasificó - ver NC2'
            end
           endcase
           
;           ; SUSANA - destaco espermatozoides con trajectoria circular - INICIO
;           if( (LINvect[i] lt 50) and (vclVectTipo[i] ne 'NC') and (vclVectTipo[i] ne 'IM') ) then begin  
;            ;and (STRvect[i] lt 50) 
;            cuentaCirc += 1
;            thickk = 2
;           endif
;           ; SUSANA - destaco espermatozoides con trajectoria circular - FIN
           
;           if (vslTotal gt vslMaxim) then begin 
;            vslMaxim = vslTotal
;            vslMaximPos = i
;           endif
           
           
           ;Susana INICIO PARTE_IIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
             np = (*state.poCurrTrackGroup)->getObjNum()
             ;printF, Unit, 'T(s)'+' '+'TO'+' '+'X'+' '+'Y'+' '+'Z'+STRING(13B) ;13B  carriage return     10B Linefeed
             miX = (*VAPStruct.pXYZValues)[*,0]
             miY = (*VAPStruct.pXYZValues)[*,1]
             miZ = (*VAPStruct.pXYZValues)[*,2]
             miT = (*VAPStruct.pTValues)
             
             np = n_elements(miX)
             for j = 0, (np-1) do begin
;              if((VCLvect[i] ne -1) or (vslTotal ne -1) or (VAPvect[i] ne -1)) then begin ;guardo coordenadas de los clasificados
              if(vclVectTipo[i] ne 'NC') then begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL)+STRING(9B)+strcompress(xBox,/REMOVE_ALL)+STRING(9B)+strcompress(yBox,/REMOVE_ALL)+STRING(9B)+strcompress(xBox*yBox,/REMOVE_ALL);+STRING(10B) ;para IGOR   con xBox e yBox
               printF, Unit,temp
              endif
             endfor
;            ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - BEGIN
;             if(vclVectTipo[i] ne 'NC') then begin ;guardo coordenadas de los clasificados
;             for j = 0, (np-1) do begin
;                temp = strcompress(string(miT[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(miX[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miY[j]),/REMOVE_ALL)+STRING(9B)+strcompress(string(miZ[j]),/REMOVE_ALL)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem);+STRING(10B) ;para IGOR
;               printF, Unit,temp
;              endfor
;             endif
;             ; MOR - 6Sept2010 - only store coordinates for TOs which are used in the analysis - END           
           
           ; MOR - get entire time vector
           timeTrack[i] = n_elements(*VAPStruct.ptValues)
           
           
              ; Susana - Write OpticalFlowParameter in image - BEGIN 
              ;;v_OFmean = mean(trackParamsOF_i)
              ;;s_TagOF = strcompress("OF"+"_"+string(v_OFmean, format = '(F6.2)'),/rem)
              ;s_TagOF = strcompress("OF"+"_"+string(OFvect[i], format = '(F6.2)'),/rem)
              
                s_TagOF = strcompress("LIN"+"_"+string(LINvect[i], format = '(F6.2)'),/rem)
              
              ;;s_TagOF = strcompress("VCL"+"_"+string(VCLvect[i], format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VSL"+"_"+string(vslTotal, format = '(F6.2)'),/rem)
              ;;s_TagOF = strcompress("VAP"+"_"+string(VAPvect[i], format = '(F6.2)'),/rem)
              
              
              ;xReal = xReal*(-1);Susana - línea Temporal para volver positivas las coordenadas de segmentaciones y tracking de estandarizacion -COMENTAR esta linea
              ;yReal = yReal*(-1);Susana - línea Temporal para volver positivas las coordenadas de segmentaciones y tracking de estandarizacion -COMENTAR esta linea
              
              
              ;;;if((vclVectTipo[i] ne 'PR') and (vclVectTipo[i] ne 'IM')) then begin
              if((LINvect[i] gt 110.) and ((vslTotal/VCLvect[i]) lt 1)) then begin
              ;;if(OFvect[i] le 1.5) then begin  ;if((OFvect[i] le 1.5)  and (OFvect[i] ge 0.12)) then begin
                ;;oPlot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                Plot = obj_new('IDLgrText', STRINGS = s_TagOF,color = [0,255,125], CHAR_DIMENSIONS = [15, 15], locations = [((*vclStruct.pXYZValues)[0,0]+5)/xReal,((*vclStruct.pXYZValues)[0,1]+1)/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
                
                oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [16, 16], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                oModel->add, oPlot
              endif
              ; Susana - Write OpticalFlowParameter in image - END 
            
            ;vvalor = 100
            ;if ((vslVect[i] ge vvalor) and (vclVectTipo[i] ne 'NC')) then thickk = 10 ; borrar, solo para destacar
            
           ;MOR - 6 Sept 2010 - only plot TOs that are classified for analysis
           if  (vclVectTipo[i] ne 'NC') then begin
           ; MOR - 05Jul2010
           ; convert track xyz to pixels for plotting against image
              if(vclVectTipo[i] eq 'IM') then begin
                ;oSym = obj_new('IDLgrSymbol', data = 3, size = 2.5, thick = thickk);original data = 3=>Dot
                ;;oSym = obj_new('IDLgrSymbol', data = 3, size = 5.5, thick = thickk); Dot de mayor tamaño; en uso
                oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);    solo para ver mejor los números borrar
              endif else begin
                ;oSym = obj_new('IDLgrSymbol', data = 2, thick = thickk);data = 2=>asterisco   thick=>line thickness
                oSym = obj_new('IDLgrSymbol', data = 0, thick = thickk);data = 0 =>no symbol   thick=>line thickness
              endelse
             
             ;if(i eq 53) then col=[0,255,255] 
             
              ;if(vclVectTipo[i] ne 'PR') then begin; borrar
              ;if(i eq 420) then begin; borrar
              ;if( i eq 352) then begin;
               oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              oModel->add, oPlot
              ;endif
              ; Susana - figura para Maurcio Cerda, VCL VAP y VSL en colores sobre TrackObject - INICIO
              ; no funciona sobreponer trayectorias, la primera tapa la segunda
;              if(i eq 325) then begin;
              ;trayectoria curvilinea, VCL
              ;oPlot = obj_new('IDLgrPlot', datax = (*vclStruct.pXYZValues)[*,0]/xReal, datay = (*vclStruct.pXYZValues)[*,1]/yReal, color = col, thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              ;oModel->add, oPlot
              
              ;trayectoria promedio, VAP
              ;oPlot = obj_new('IDLgrPlot', datax = (*VAPStruct.pXYZValues)[*,0]/xReal, datay = (*VAPStruct.pXYZValues)[*,1]/yReal, color = [255,255,50], thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym); amarillo
              ;oModel->add, oPlot
              
              ;trayectoria lineal, VSL
              ;oPlot = obj_new('IDLgrPlot', datax = [((*vclStruct.pXYZValues)[0,0]/xReal),((*vclStruct.pXYZValues)[(size((*vclStruct.pXYZValues)[*,0])-1),0]/xReal)], datay = [((*vclStruct.pXYZValues)[0,1]/yReal),((*vclStruct.pXYZValues)[(size((*vclStruct.pXYZValues)[*,1])-1),1]/yReal)], color = [0,255,100], thick = thickk, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
              ;oModel->add, oPlot
              
;              endif
              ; Susana - figura para Maurcio Cerda, VCL VAP y VSL en colores sobre TrackObject - FIN
              ;endif ; borrar 
              
              ; Susana - Incluyo LIN temporalmente  INICIO
;              if((vclVectTipo[i] eq 'B')) then begin
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [255,255,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;              if((vclVectTipo[i] eq 'NP')) then begin
;              ;if(i eq 50) then begin; borrar
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VCL'+string(VCLvect[i], format = '(F6.1)'),/rem)+strcompress('_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                ;oPlot = obj_new('IDLgrText', STRINGS = strcompress(' VAP'+string(VAPvect[i], format = '(F6.1)')+'_VSL'+string(vslTotal, format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oPlot = obj_new('IDLgrText', STRINGS = strcompress(' LIN_'+string(LINvect[i], format = '(F6.1)'),/rem)+strcompress('_ALH_'+string(ALHvect[i], format = '(F6.1)'),/rem),color = [0,255,125], CHAR_DIMENSIONS = [13, 13], locations = [((*vclStruct.pXYZValues)[0,0]/xReal)-10,((*vclStruct.pXYZValues)[0,1]/xReal)-10],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                oModel->add, oPlot
;              endif
;endif
;             ; Susana - Incluyo LIN temporalmente  INICIO
              

              
;             if((vclVectTipo[i] ne 'PR') ) then begin; borrar
;             if(i ge 300 and i le 390) then begin; borrar
;;;             ; Susana - add TO number - INICIO
;;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [15, 15], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [255,255,255], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;              ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [10, 10], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;;             ;oPlot = obj_new('IDLgrText', STRINGS = '   '+strcompress('   '+string(i, format = '(I6)'),/rem),color = [0, 255, 0], CHAR_DIMENSIONS = [20, 20], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;             
;;             v_TOsSelectos = [97,135,146,241,242,232,251,237,209,148,191,104,118] ; DF1
;              v_TOsSelectos = [35,293,74,77,70,90,96,248,247,253,228,237,222,185,228,237,142,116,146,139,215,222,285,282,259,74,259,168,5,30,2,86,229,166,167,180,210,275]
;;             v_TOsSelectos = [14,8];AB1
;              v_TOsSelectos = [2,22,53,59,63,113,124,131,132,141,144,183,184,188,208,227,262] ; 27EneroM1_1   seleccionados por Luis Sarabia desde trayectoris de 5 segundos
;             v_whereTOsSelectos = where(v_TOsSelectos eq i, countTOsSelectos)
;             if (countTOsSelectos gt 0) then begin
;;;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [13, 13], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [0,1.0,0],UPDIR = [1.0, 1.0, 0.0])  ; Susana - With ROI number  vertical paralelo a Y
;             ;oPlot = obj_new('IDLgrText', STRINGS = strcompress('     '+string(i, format = '(I6)'),/rem),color = [0,0,0], CHAR_DIMENSIONS = [11, 11], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [1.0,0.0,0],UPDIR = [-1.0, -1.0, 0.0])  ; Susana - With ROI number  horizontal con flip vertical para verlos bien en trayectories
             oPlot = obj_new('IDLgrText', STRINGS = ' _'+strcompress(string(i, format = '(I6)'),/rem),color = col, CHAR_DIMENSIONS = [11, 11], locations = [(*vclStruct.pXYZValues)[0,0]/xReal,(*vclStruct.pXYZValues)[0,1]/xReal],xcoord_conv = xs, ycoord_conv = ys,BASELINE = [1.0,0.0,0],UPDIR = [-1.0, -1.0, 0.0])  ; Susana - With ROI number  horizontal con flip vertical para verlos bien en trayectories
             oModel->add, oPlot
;             endif
;;;             ; Susana - add TO number - FIN
;             endif ; borrar


;              if(i eq 194) then begin
;                print,'TO='+string(i)+' ---------'
;                print,vclVectTipo[i]
;                print,vslTotal
;                print,VCLvect[i]
;                print,VAPvect[i]
;                print,LINvect[i]
;                print,STRvect[i]
;                ;print,(vslTotal/VCLvect[i])*100. ; LIN
;                ;print,(vslTotal/VAPvect[i])*100. ;STR
;                print,xBox_vect[i]
;                print,yBox_vect[i]
;                print,yBoxyBox_vect[i]
;                print,ALHvect[i]
;                print,' --------------------'
;              endif

           endif
           
           ; MOR 17Aug 2010 - write raw trajectory to file - BEGIN
           ;name = 'CoordsTOs_'
           name = 'CoordsTOsOMS_'+nom
           file = strCompress(tempFileName + name + '.dat', /remove)
           
           get_lun, U
            ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         if(i eq 0) then begin
            openW, U, file
            printF, U, 'zslice'+STRING(9B)+'Ts'+STRING(9B)+'TOc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'R'+STRING(9B)+'G'+STRING(9B)+'B'
         endif else begin
            openW, U, file, /APPEND
         endelse
         ; print coordinates for every track object for every time
         lrr = n_elements(*VAPStruct.ptValues) 
             for j = 0, (lrr-1) do begin
               temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*VAPStruct.ptValues)[j]),/rem)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,0]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,1]),/rem)+STRING(9B)+strcompress(string((*VAPStruct.pXYZValues)[j,2]),/rem)+STRING(9B)+strcompress(col[0],/rem)+STRING(9B)+strcompress(col[1],/rem)+STRING(9B)+strcompress(col[2],/rem)
               printF, U,temp
             endfor   
         close, U
         FREE_LUN, U
         
         ; MOR write raw trajectory to file - END
           
           ; MOR - write to file the track objects overallType of classification - BEGIN    
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/rem)+STRING(9B)+strcompress(string(overallType[i]),/rem)
           printF, U1,temp 
           ; MOR - write to file the track objects overallType of classification - END
           
         endfor
;         print, countTONC_pocosPuntos ;Susana
;         print, countTONC_firstAppeared
;         print, countTONC_timesTracked
         ;Susana INICIO PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         close, Unit
         FREE_LUN, Unit
         
         ;Susana FIN PARTE_IIIdeIII   Guardando Coordenadas TODOS TrackObjects--------------
         
         ; MOR - close up file with overalltype classification - BEGIN
         close, U1
         FREE_LUN, U1
         ; MOR - close up file with overalltype classification - END         
         

         ;Susana INICIO   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         ;pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ; MOR - 9Feb 2011 - change so the last directory name prints on Motil.tiff file
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])+'_zslice'+strcompress(STRING(zPos),/rem) ; Susana - incluyo zslice al final
         name = 'VelocityDataOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceC'+STRING(9B)+'TOVeloc'+STRING(9B)+'VCL'+STRING(9B)+'VSL'+STRING(9B)+'VAP'+STRING(9B)+'xBoxVeloc_pix'+STRING(9B)+'yBoxVeloc_pix'+STRING(9B)+'xBox_X_yBoxVeloc_pix'+STRING(9B)+'desp'+STRING(9B)+'dist'+STRING(9B)+'desp_dist'+STRING(9B)+s_OFParameter+STRING(13B) ;13B  carriage return     10B Linefeed Para IGOR  ; Susana  - con xBox e yBox sin SIZE
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]*yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESP_DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(OFvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR   Susana- con xBox e yBox sin Size
           ;temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(VCLvect[i], format = '(F6.6)'),/REMOVE_ALL)+STRING(9B)+strcompress(string(vslVect[i], format = '(F6.6)'),/REMOVE_ALL)+STRING(9B)+strcompress(string(VAPvect[i], format = '(F6.6)'),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(xBox_vect[i]*yBox_vect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESPvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(DESP_DISTvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(OFvect[i]),/REMOVE_ALL);+STRING(13B) ;Para IGOR   Susana- con xBox e yBox sin Size
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos VCL VSL y VAP MenosPesado--------------
         
         
         ;Susana INICIO   Guardando Datos Juntos LIN STR y WOB --------------
         ;bslachs  total de backslashs
         ;lbSlashs arreglo con largos de cada string separado
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = '_z'+strcompress(string(zPos),/REMOVE_ALL)+'_' ; susana incluye zPos a nombre imagen
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; comentar esta linea y descomentar la anterior***********************
         name = 'Motil_MovVarsOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         openW, Unit, file
         np = (*state.poCurrTrackGroup)->getObjNum()
         printF, Unit, 'zsliceB'+STRING(9B)+'TO'+STRING(9B)+'LIN'+STRING(9B)+'STR'+STRING(9B)+'WOB'+STRING(9B)+'ALH'+STRING(9B)+'TypeMVars'+STRING(13B) ;Falta implementar +STRING(9B)+'BCF'  13B  carriage return     10B Linefeed Para IGOR
         
         
         for i = 0, (np-1) do begin
          if(vclVectTipo[i] ne 'NC') then begin
           temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i]),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           ;temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(i),/REMOVE_ALL)+STRING(9B)+strcompress(string(LINvect[i], format = '(F6.6)'),/REMOVE_ALL)+STRING(9B)+strcompress(string(STRvect[i], format = '(F6.6)'),/REMOVE_ALL)+STRING(9B)+strcompress(string(WOBvect[i], format = '(F6.6)'),/REMOVE_ALL)+STRING(9B)+strcompress(string(ALHvect[i], format = '(F6.6)'),/REMOVE_ALL)+STRING(9B)+strcompress(vclVectTipo[i],/REMOVE_ALL);+STRING(13B);Falta implementar +STRING(9B)+strcompress(string(BCFvect[i]),/REMOVE_ALL) ;Para IGOR
           printF, Unit,temp
          endif
         endfor
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN   Guardando Datos Juntos LIN STR y WOB -------------- 
         
           
         ;Susana INICIO --  Guardando #TrackObjects Reasignados
         name = 'NumTrackObjectsVSLOMS_'+nom ;incluyo nombre ultima carpeta en nombre archivo
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit,transpose(vslReasign)
         close, Unit
         FREE_LUN, Unit
         ;Susana FIN --  Guardando #TrackObjects Reasignados
                 
         
         ;SUSANA INICIO  Promedio VAP por grupo de movilidad--------------
         np = (*state.poCurrTrackGroup)->getObjNum()
         meanVAP_A = 0; promedio tipos A
         meanVAP_B = 0; promedio tipos B
         meanVAP_C = 0; promedio tipos C
         meanVAP_D = 0; promedio tipos D
         meanVAP_NC = 0; promedio tipos NC
         
         meanVCL_A = 0; promedio tipos A
         meanVCL_B = 0; promedio tipos B
         meanVCL_C = 0; promedio tipos C
         meanVCL_D = 0; promedio tipos D
         meanVCL_NC = 0; promedio tipos NC
         
         meanVSL_A = 0; promedio tipos A
         meanVSL_B = 0; promedio tipos B
         meanVSL_C = 0; promedio tipos C
         meanVSL_D = 0; promedio tipos D
         meanVSL_NC = 0; promedio tipos NC
         
         meanLIN_A = 0; promedio LIN tipos A
         meanLIN_B = 0; promedio LIN tipos B
         meanLIN_C = 0; promedio LIN tipos C
         meanLIN_D = 0; promedio LIN tipos D
         meanLIN_NC = 0; promedio LIN tipos NC
         
         meanSTR_A = 0; promedio STR tipos A
         meanSTR_B = 0; promedio STR tipos B
         meanSTR_C = 0; promedio STR tipos C
         meanSTR_D = 0; promedio STR tipos D
         meanSTR_NC = 0; promedio STR tipos NC
         
         meanWOB_A = 0; promedio WOB tipos A
         meanWOB_B = 0; promedio WOB tipos B
         meanWOB_C = 0; promedio WOB tipos C
         meanWOB_D = 0; promedio WOB tipos D
         meanWOB_NC = 0; promedio WOB tipos NC
         
         meanVAP_Acnt = 0; counter para tipos A
         meanVAP_Bcnt = 0; counter para tipos B
         meanVAP_Ccnt = 0; counter para tipos C
         meanVAP_Dcnt = 0; counter para tipos D
         meanVAP_NCcnt = 0; counter para tipos NC
         ;desde aqui descomenté -Susana
         for h = 0, (np-1) do begin
          case 1 of
            (vclVectTipo[h] eq 'PR'): begin
              meanVAP_A += VAPvect[h]
              meanVCL_A += VCLvect[h]
              meanVSL_A += vslVect[h]
              meanLIN_A +=  LINvect[h]
              meanSTR_A +=  STRvect[h]
              meanWOB_A +=  WOBvect[h]
              ;meanALH_A += ALHvect[h]
              meanVAP_Acnt += 1
              end
            (vclVectTipo[h] eq 'B'): begin
              meanVAP_B += VAPvect[h]
              meanVCL_B += VCLvect[h]
              meanVSL_B += vslVect[h]
              meanLIN_B += LINvect[h]
              meanSTR_B +=  STRvect[h]
              meanWOB_B +=  WOBvect[h]
              ;meanALH_B += ALHvect[h]
              meanVAP_Bcnt += 1
              end
            (vclVectTipo[h] eq 'NP'): begin
              meanVAP_C += VAPvect[h]
              meanVCL_C += VCLvect[h]
              meanVSL_C += vslVect[h]
              meanLIN_C += LINvect[h]
              meanSTR_C +=  STRvect[h]
              meanWOB_C +=  WOBvect[h]
              ;meanALH_C += ALHvect[h]
              meanVAP_Ccnt += 1
              end
            (vclVectTipo[h] eq 'IM'): begin
              meanVAP_D += VAPvect[h]
              meanVCL_D += VCLvect[h]
              meanVSL_D += vslVect[h]
              meanLIN_D += LINvect[h]
              meanSTR_D +=  STRvect[h]
              meanWOB_D +=  WOBvect[h]
              ;meanALH_D += ALHvect[h]
              meanVAP_Dcnt += 1
              end
            (vclVectTipo[h] eq 'NC'): begin
              meanVAP_NC += VAPvect[h]
              meanVCL_NC += VCLvect[h]
              meanVSL_NC += vslVect[h]
              meanLIN_NC += LINvect[h]
              meanSTR_NC +=  STRvect[h]
              meanWOB_NC +=  WOBvect[h]
              ;meanALH_NC += ALHvect[h]
              meanVAP_NCcnt += 1
              end
           endcase
         endfor
         ;hasta aqui descomenté--Susana
         meanVAP_A /= meanVAP_Acnt
         meanVAP_B /= meanVAP_Bcnt
         meanVAP_C /= meanVAP_Ccnt
         meanVAP_D /= meanVAP_Dcnt
         meanVAP_NC /= meanVAP_NCcnt
         
         meanVCL_A /= meanVAP_Acnt
         meanVCL_B /= meanVAP_Bcnt
         meanVCL_C /= meanVAP_Ccnt
         meanVCL_D /= meanVAP_Dcnt
         meanVCL_NC /= meanVAP_NCcnt
         
         meanVSL_A /= meanVAP_Acnt
         meanVSL_B /= meanVAP_Bcnt
         meanVSL_C /= meanVAP_Ccnt
         meanVSL_D /= meanVAP_Dcnt
         meanVSL_NC /= meanVAP_NCcnt
         
         meanLIN_A /= meanVAP_Acnt
         meanLIN_B /= meanVAP_Bcnt
         meanLIN_C /= meanVAP_Ccnt
         meanLIN_D /= meanVAP_Dcnt
         meanLIN_NC /= meanVAP_NCcnt
         
         meanSTR_A /= meanVAP_Acnt
         meanSTR_B /= meanVAP_Bcnt
         meanSTR_C /= meanVAP_Ccnt
         meanSTR_D /= meanVAP_Dcnt
         meanSTR_NC /= meanVAP_NCcnt
         
         meanWOB_A /= meanVAP_Acnt
         meanWOB_B /= meanVAP_Bcnt
         meanWOB_C /= meanVAP_Ccnt
         meanWOB_D /= meanVAP_Dcnt
         meanWOB_NC /= meanVAP_NCcnt
         ;SUSANA FIN  Promedio VAP por grupo de movilidad--------------
         
         
         ;print, vclVectTipo
         oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255])
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         
         ; MOR - comment out old path for saving, try path where images are saved instead 
         write_tiff, tempFileName + 'Motil_trajectoryOMS'+nom+'.tiff', outImage_1
         
;         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - INICIO
;         for v=0, totalTNum-1 do begin
;            widget_control, stack_tlb, get_uValue = stackState, /no_copy
;              imagev = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = v, chPos = chPos, zPos = zPos)
;            widget_control, stack_tlb, set_uValue = stackState, /no_copy
;            
;            ;oImagev = obj_new('IDLgrImage',imagev, xcoord_conv = xs, ycoord_conv = ys)
;            oImage->setProperty, data = imagev
;            oBuffer->draw, oView      
;            oOImage = oBuffer->read()
;            oOImage->getProperty, data = outImage_v
;         
;              write_tiff, tempFileName + '\Video\Motil_trajectoryOMS_'+string(v,format='("",I3.3)')+'.tiff', outImage_v
;         endfor
;         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - FIN
         
;         ;Susana Comenta esto para poder correr lo de victor más adelante .. INICIO
;         ; MOR - destroy objects created for plotting to free up memory
;         obj_destroy, [oBuffer, oView]
;         ;Susana Comenta esto para poder correr lo de victor más adelante .. FIN
         
         ; MOR - add creation of VSL histogram - BEGIN
         path = tempFileName + 'VSL_HistoOMS'+nom+'.tiff'
         
         v_histogVSL_OK = 0 ;Susana, ==1 si se creó histogram    ==0 si no hay TOs con VSLs muy bajas y no se creó histograma
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign ne -1), count) ; MOR - 21 June 2011 ; Original MIA - Susana comenta por creer que hay error, lo corrijo en linea siguiente  si metí la pata lo siento
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 ;Aqui reemplazo ne por eq, es decir envio al histograma todos los trackobjects menos los reasignados
         whClassified = where((vclVectTipo ne 'NC'), count); Susana - envío todos los clasificados
         if(whClassified[0] ne -1) then begin; Susana1
          vect = vslVect(whClassified) ; MOR - only pass in VSL velocities which were used in the classification
;         vect = vslVect ; MOR - commented out previous code
          name = "rectilinear velocity VSL [um s-1]"
          ;name = "rectilinear velocity VSL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          s_Total = 'OMS'+nom
          linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
          CEDAI_histogramPlot_VSL_OMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName, veloLimVSLb = veloLimVSLb,s_Total = s_Total
          ;CEDAI_histogramPlot_VSL_OMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = ([2.5, 4.,6.]*xReal), linesFlag = linesFlag, tempFileName = tempFileName,s_Total = s_Total ; URUGUAY - eje x de histograma de 0 a 6
          ; MOR - add creation of VSL histogram - END
          v_histogVSL_OK = 1
         endif; Susana1
         ;endif else begin; Susana1
            ;
         ;endelse; Susana1
         
         ; MOR - comment out old path for saving histogram, change to image path instead
         ;path = s_getPathForSystem() + 'VCL_HistoOMS.tiff'
         ;path = tempFileName + 'VCL_HistoOMS.tiff'
         path = tempFileName + 'VCL_HistoOMS'+nom+'.tiff'
         
         v_histogVCL_OK = 0 ; Susana - controlar si existe histograma VCL de clasificados no reasignados
         ;whClassified = where((vclVectTipo ne 'NC') and (vslReasign eq -1), count) ; MOR - 21 June 2011 
         whClassified = where((vclVectTipo ne 'NC'), count) ; MOR - 21 June 2011
         if(whClassified[0] ne -1) then begin; Susana2
          ; Susana - A B C o D, no reasignado
          vect = VCLvect[whClassified]
          name = "curvilinear velocity VCL [um s-1]"
          ;name = "curvilinear velocity VCL [um s-1]"+nom  ; Susana- si incluyo nom esto aparece en rotulo eje x de histograma
          linesFlag = 1
          s_Total = 'OMS'+nom
          CEDAI_histogramPlotOMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName, veloLimVCLb = veloLimVCLb,s_Total = s_Total
          v_histogVCL_OK = 1
         endif; Susana2
         ;endif else begin; Susana2
            ;nada
            ;El problema de no haber alternativa es que no crea imágenes como VCL_HistoOMS.tiff
         ;endelse; Susana2
         
         vect_elements = n_elements(VCLvect) - cuentaTOsNC ; Susana - total de espermatozoides clasificados en tipos de movilidad (no incluye NC)
         
         ;Susana INICIO    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom2 = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; ultima carpeta
         nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ;penultima carpeta
         name = 'DatosOMSPChenlo_2011'  ; comentar esta linea y descomentar la anterior***********************
         ;name = 'DatosOMS_'+nom2
         
         ;Susana - reemplazo tempFileName INICIO
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         file = strCompress(nomFolder + name + '.dat', /remove)   ;Susana, guarda archivo en la misma carpeta de datos
         ;Susana - reemplazo tempFileName FIN
         
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         get_lun, unitPCh
         OPENR, unitPCh, file, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, unitPCh, file
           ;printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         
         ;openW, unitPCh, file
         ;printF, unitPCh,'IdMuestra'+STRING(9B)+'#A'+STRING(9B)+'#B'+STRING(9B)+'#C'+STRING(9B)+'#D'+STRING(9B)+'#NC'+STRING(9B)+'%A'+STRING(9B)+'%B'+STRING(9B)+'%C'+STRING(9B)+'%D';
         ;recuentos
         aux = where(vclVectTipo eq 'PR', tipoA)
         aux = where(vclVectTipo eq 'B', tipoB)
         aux = where(vclVectTipo eq 'NP', tipoC)
         aux = where(vclVectTipo eq 'IM', tipoD)
         aux = where(vclVectTipo eq 'NC', tipoNC)
         ;porcentajes
         tipoAP = string(100.*tipoA / vect_elements, format = '(F6.2)') + ' %'
         tipoBP = string(100.*tipoB / vect_elements, format = '(F6.2)') + ' %'
         tipoCP = string(100.*tipoC / vect_elements, format = '(F6.2)') + ' %'
         tipoDP = string(100.*tipoD / vect_elements, format = '(F6.2)') + ' %'
         ;print,tipoA+tipoB+tipoC+tipoD  corresponde a vect_elements
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         ;;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana FIN    Guardando Recuentos y Porcentajes A B C y D   de  todos datos PChenlo--------------
         
         
         ;dummy = where(vect ge veloLimitsVCL[6], count)
         dummy = where(vclVectTipo eq 'PR', count) ;Susana
         string_8 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_0 = 'PR:';Progressively Motil
         string_4 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
;         dummy = where(vclVectTipo eq 'B', count) ;Susana
;         string_9 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
;         string_1 = 'Type B:' 
;         string_5 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[2] and vect lt veloLimitsVCL[3], count)   ;Susana
         dummy = where(vclVectTipo eq 'NP', count) ;Susana
         string_10 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_2 = 'NP:';Non-Progressively Motil
         string_6 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         
         ;dummy = where(vect ge veloLimitsVCL[0] and vect lt veloLimitsVCL[1], count)   ;Susana
         dummy = where(vclVectTipo eq 'IM', count) ;Susana
         string_11 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_3 = 'IM:';Immotile 
         string_7 = string(100.*count / vect_elements, format = '(F6.2)') + ' %'
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Motil_TextOMS'+nom+'.tiff'
         fileNameConc = tempFileName + 'Concentr_TextOMS'+nom+'.tiff'
         
         ;SUSANA - Contabilizando trackobjects NC (No Clasificados) 
         dummy = where(vclVectTipo eq 'NC', count) ;Susana
         ;string_21 = strcompress('(' + string(fix(count)), /remove_all) + ' sperms)'
         string_21 = strcompress( string(fix(count)), /remove_all) + ' sperms'
         ;string_23 = 'NC       :'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - INICIO
         string_23 =strcompress('#Traj.Circ.:' + string(fix(cuentaCirc)), /remove_all) + '(LIN<50) '+ '#NC:'
         ;SUSANA - incluyo texto con recuento de espermatozoides con trajectoria circular - FIN
         
         ;aqui estaba ;SUSANA- incluyo concentración - INICIO
         
         ; MOR - 12Feb 2011- average number of objects found per time
         totalNumObjs = {avg: 0., std: 0., number: 0., med:0., mini:0., maxi:0., se:0.} 
         stats = moment(*numObjs.pNumObjs, sdev = totalNumObjs.std)
         totalNumObjs.avg = stats[0]*conFactor; recuento promedio, promedia recuento espermios en todos los tiempos
         totalNumObjs.std = sqrt(stats[1]*conFactor)
         totalNumObjs.number = n_elements(*numObjs.pNumObjs)
         totalNumObjs.med = median(*numObjs.pNumObjs*conFactor) 
         totalNumObjs.mini = min(*numObjs.pNumObjs*conFactor)
         totalNumObjs.maxi= max(*numObjs.pNumObjs*conFactor)
         totalNumObjs.se = sqrt(totalNumObjs.avg/conFactor) ; sqrt(average count) for a Poisson process or std of sample / sqrt(number)
         
         ;SUSANA- incluyo concentración -  1 cabeza/campo corresponde a 0.3143 millones/ml para campo 650.138umX483.436um en Makler 10prof - INICIO
         ;string_3AA = strcompress('Conc.: ')
         string_3AA = strcompress('Sperms:')
         string_7AA = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem)
         string_11AA = strcompress('10!U6!N/mL', /rem)
         
         ;;string_11AA = strcompress('[10!U6!N/mL]', /rem)+' ('+strcompress(string(totalNumObjs.avg/conFactor, format = '(I6.0)'), /rem)+' sperms)'
         ;string_11AA = strcompress('[10!U6!N/mL]', /rem); No informo el total de sperms usado para cálculo de concetracion
         
         ;SUSANA- incluyo concentración - FIN
         CEDAI_textPlotOMS, filename, string_0, string_2, string_3, string_3AA, string_4, string_6, string_7, string_7AA, string_8, string_10, string_11, string_11AA,string_21,string_23, nom; ordenamiento de A+B
         CEDAI_textPlotOMS_Conc, fileNameConc, string_3AA, string_7AA, string_11AA, nom; ordenamiento de A+B  - concentration
         
         
         string_0 = 'Concentration Statistics Summary'
;         string_1 = strcompress('Frames ') 
         string_2 = strcompress('Mean Conc.: ') ;string_2 = strcompress('Mean: ') 
;         string_3 = strcompress('SDev ') 
         string_4 = strcompress('Std. Error: ')
;         string_5 = strcompress('Median ') 
;         string_6 = strcompress('Min ') 
;         string_7 = strcompress('Max ') 
         
         string_8 = strcompress(string(totalNumObjs.number, format = '(D6.2)'), /rem) 
         string_9 = strcompress(string(totalNumObjs.avg, format = '(D6.2)'), /rem) 
         string_10 = strcompress(string(totalNumObjs.std, format = '(I6.2)'), /rem) 
         string_11 = strcompress(string(totalNumObjs.se, format = '(D6.2)'), /rem) 
         string_12 = strcompress(string(totalNumObjs.med, format = '(I6.2)'), /rem) 
         string_13 = strcompress(string(totalNumObjs.mini, format = '(I6.2)'), /rem) 
         string_14 = strcompress(string(totalNumObjs.maxi, format = '(I6.2)'), /rem) 
         
         string_15 = strcompress('[10E6/mL]', /rem) 
       
         ; MOR - comment out all previously defined save paths
         fileName = tempFileName + 'Concentration_Text_ShortOMS'+nom+'.tiff'
         
         CEDAI_textPlot_Count_Short, filename, string_0, string_2, string_4, string_9, string_11, string_15
         
         ; MOR - 12Feb2011 write total count data to .dat file 
         dum = max(timeTrack, maxI)
         numObjs = (*state.poCurrTrackGroup)->getTrackParam(paramName = 'Track Concentration', objNum = maxI)
         
         name = 'ConcentrationOMS'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, unit
         openW, Unit, file
         printF, Unit, 'zslice'+STRING(9B)+'Time' +string(9B)+ 'Count'+STRING(13B)
         indU = uniq(*numObjs.pTValues)       
         for i = 0, n_elements(indU)-1 do begin
            temp = strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string((*numObjs.pTValues)[indU[i]]),/rem)+STRING(9B)+strcompress(string((*numObjs.pnumObjs)[i]),/rem)+STRING(13B)
            printF, Unit,temp
         endfor
         close, Unit
         FREE_LUN, Unit
         
         ; MOR - save results in the same directory as images - BEGIN
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_Short.bmp'
         imageLeft =tempFileName + 'Motil_trajectoryOMS'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_HistoOMS'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoOMS'+nom+'.tiff'
         textRight = tempFileName + 'Motil_TextOMS'+nom+'.tiff'
         fileName = tempFileName + 'Motil_ShortOMS'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Concentration_Text_ShortOMS'+nom+'.tiff' 
         
         
         CEDAI_combineImages_Motil_Short, background, imageLeft, TextRight, fileName, textLeft
         
        ; MOR - save results in the same directory as images - BEGIN
        ;; background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_Background.bmp'
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_ColorNaranjo.bmp'
         
;         imageLeft =tempFileName + 'Motil_trajectoryOMS.tiff'
         imageLeft =tempFileName + 'Motil_trajectoryOMS'+nom+'.tiff'
         imageRight =  tempFileName + 'VCL_HistoOMS'+nom+'.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoOMS'+nom+'.tiff'
         textRight = tempFileName + 'Motil_TextOMS'+nom+'.tiff'
         textRight2 = tempFileName + 'Concentration_Text'+nom+'.tiff'
         
         fileName = tempFileName + 'Motil_newOMS'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png'; Susana 
         endif; Susana 
         
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         v_guardaPNG = 1; 1=Guarda png también    0=No guarda png
         CEDAI_combineImages_motil_OMS, background, imageLeft, imageRight, imageRight2, TextRight, fileName, textLeft,fileNameConc,v_guardaPNG; incluye concentracion ; ejemplo, guardo Motil_newOMS5s.png y Motil_newOMS5s.tiff
         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - INICIO
         for v=0, totalTNum-1 do begin
            widget_control, stack_tlb, get_uValue = stackState, /no_copy
              imagev = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = v, chPos = chPos, zPos = zPos)
            widget_control, stack_tlb, set_uValue = stackState, /no_copy
            
            ;oImagev = obj_new('IDLgrImage',imagev, xcoord_conv = xs, ycoord_conv = ys)
            oImage->setProperty, data = imagev
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_v
            
            snomImageLeft =strcompress(tempFileName + '\Video\Motil_trajectoryOMS_'+string(v,format='("",I3.3)')+'.tiff') ;Talvez esta cambie después ??
              write_tiff, snomImageLeft, outImage_v
              fileNameV = tempFileName + '\Video\Motil_newOMS'+string(v,format='("",I3.3)')+'.tiff' 
              v_guardaPNG = 0
              CEDAI_combineImages_motil_OMS, background, snomImageLeft, imageRight, imageRight2, TextRight, fileNameV, textLeft,fileNameConc,v_guardaPNG
         endfor
         ;VICTOR Castañeda guarda imágenes sobreposición cada tiempo con trayectorias - FIN
         ;Susana Incluye esto para poder correr lo de victor .. INICIO
         ; MOR - destroy objects created for plotting to free up memory
         obj_destroy, [oBuffer, oView]
         ;Susana Incluye esto para poder correr lo de victor .. FIN
         
         
         
         ;SUSANA - Incluyo VAP promedio por grupos LO ESTOY INTENTANDO, AUN NO CREO EL HISTOGRAMA PENDIENTES TAMBIEN HISTOGRAMAS STR Y WOB
         ;SUSANA - add creation of ALH histogram - BEGIN
         veloLimitsVCL = [0,50., 50.,200., 200.,400., 400.,10000.] ; no tengo rangos para ALH :( 
         path = tempFileName + 'histo_ALH'+nom+'.tiff'
         
         vect = ALHvect
         name = "ALH Amplitude of Lateral Head displacement [um]"+nom
         
         linesFlag = 1 ; MOR - whether to place vertical lines at the designated partitions given by veloLimitsVCL, =1 (yes), =0(no)
         ;CEDAI_histogramPlot_ALH, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, linesFlag = linesFlag, tempFileName = tempFileName
         ;Falta generar un graficador de histograma para ALH o llamar internamente el GraphicModel ALH
         ;SUSANA - add creation of VSL histogram - END
         
         
         fileName = tempFileName + 'Motil_MovVariables_Text'+nom+'.tiff' ; MovementsVariables
         
         string_0 = 'PR:';Progressively Motil 
         ;string_4 = string(meanVAP_A, format = '(F6.2)') + ' um/s'
         string_4 = string(meanVAP_A, format = '(F6.1)')
         string_8 = strcompress(string(fix(meanLIN_A), format = '(F6.1)'), /remove_all)
         string_8_1 = strcompress(string(fix(meanSTR_A), format = '(F6.1)'), /remove_all)
         string_8_2 = strcompress(string(fix(meanWOB_A), format = '(F6.1)'), /remove_all)
         string_8_3 = strcompress(string(fix(meanVCL_A), format = '(F6.1)'), /remove_all)
         string_8_4 = strcompress(string(fix(meanVSL_A), format = '(F6.1)'), /remove_all)
         
;         string_1 = 'Type B:' 
;         string_5 = string(meanVAP_B, format = '(F6.1)')
;         string_9 = strcompress(string(fix(meanLIN_B), format = '(F6.1)'), /remove_all)
;         string_9_1 = strcompress(string(fix(meanSTR_B), format = '(F6.1)'), /remove_all)
;         string_9_2 = strcompress(string(fix(meanWOB_B), format = '(F6.1)'), /remove_all)
;         string_9_3 = strcompress(string(fix(meanVCL_B), format = '(F6.1)'), /remove_all)
;         string_9_4 = strcompress(string(fix(meanVSL_B), format = '(F6.1)'), /remove_all)
         
         string_1  = 'NP:' ;Non-Progressively Motil (
         ;string_6 = string(meanVAP_C, format = '(F6.2)') + ' um/s'
         string_5 = string(meanVAP_C, format = '(F6.1)')
         string_9 = strcompress(string(fix(meanLIN_C), format = '(F6.1)'), /remove_all)
         string_9_1 = strcompress(string(fix(meanSTR_C), format = '(F6.1)'), /remove_all)
         string_9_2 = strcompress(string(fix(meanWOB_C), format = '(F6.1)'), /remove_all)
         string_9_3 = strcompress(string(fix(meanVCL_C), format = '(F6.1)'), /remove_all)
         string_9_4 = strcompress(string(fix(meanVSL_C), format = '(F6.1)'), /remove_all)
         
         string_2 = 'IM:'  ; Immotile Talvez no deba incluirlo                              ; string_3
         string_6 = string(meanVAP_D, format = '(F6.1)') ;string_7
         string_10 = strcompress(string(fix(meanLIN_D), format = '(F6.1)'), /remove_all)    ;string_11
         string_10_1 = strcompress( string(fix(meanSTR_D), format = '(F6.1)'), /remove_all) ;string_11_1
         string_10_2 = strcompress( string(fix(meanWOB_D), format = '(F6.1)'), /remove_all) ;string_11_2
         string_10_3 = strcompress(string(fix(meanVCL_D), format = '(F6.1)'), /remove_all)  ;string_11_3
         string_10_4 = strcompress(string(fix(meanVSL_D), format = '(F6.1)'), /remove_all)  ;string_11_4
         
         string_3 = ''; y si uso este para la concentración???
         string_7 = ''
         string_11 = ''
         string_11_1 = ''
         string_11_2 = ''
         string_11_3 = ''
         string_11_4 = ''
         
         string_21 = strcompress('VAP')
         string_23 = strcompress('VSL')
         string_27 = strcompress('VCL')
         string_28 = strcompress('LIN')
         string_29 = strcompress('STR')
         string_30 = strcompress('WOB')
         string_31 = ' um/s'
         string_32 = ' %'
         ;CEDAI_textPlotMotil, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         CEDAI_textPlotMotilOMS, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32
         
         ; MOR - save results in the same directory as images - BEGIN
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Motility_MovVariables_Background.bmp'
         
         imageLeft =tempFileName + 'Motil_trajectoryOMS'+nom+'.tiff' ;Talvez esta cambie después ??
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Motility_MovVariables.bmp'
         
         imageRight =  tempFileName + 'VCL_HistoOMS'+nom+'.tiff' ;'histo_ALH.tiff'
         imageRight2 =  tempFileName + 'VSL_HistoOMS'+nom+'.tiff'
         textRight = tempFileName + 'Motil_MovVariables_Text'+nom+'.tiff'
         fileName = tempFileName + 'Motil_MovVariables'+nom+'.tiff' 
         ; MOR - save results in the same directory as images - END
         
         if(v_histogVSL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VSL , abro una imagen de disco duro; Susana 
            ;imageRight2 =  s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.tiff'; Susana 
            imageRight2 =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VSL_HistoVacio.png';Susana 
         endif; Susana
         if(v_histogVCL_OK eq 0) then begin; Susana 
         ; Si no existe histograma VCL , abro una imagen de disco duro; Susana 
            imageRight =  s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\VCL_HistoVacio.png'; Susana 
         endif; Susana 
         CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, TextRight, filename,1
         
       ;Track MotilOMS Ajustada -- FIN
      endcase
      else: print,-1
      endcase
      
      widget_control, state.wListSelectTrackModels, get_uValue = uValue, /no_copy
      *uValue.id = [*uValue.id]
      widget_control, state.wListSelectTrackModels, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy

      widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROITM_AddGraphicModel_Event, ev
   widget_control, ev.id, get_value = model
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, state.wListSelectTrackModels, get_uValue = uValue, /no_copy
        whModel = where(*uValue.value eq model)
        if (whModel[0] eq -1) then begin
           *uValue.value = [*uValue.value, model]
        endif
        if ((*uValue.value)[0] eq '-NO SELECTION-') then *uValue.value = (*uValue.value)[1:*]
        if (whModel[0] ne -1) then begin
           listSelect = whModel[0]
           uValue.active = whModel[0]
        endif else begin
           listSelect = n_elements(*uValue.value)-1
           uValue.active = listSelect
        endelse
        state.fListSelect = 3
      widget_control, state.wListSelectTrackModels, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy
   widget_control, ev.top, set_uValue = state, /no_copy
   s_ROITM_InitGraphicModel, ev.top
end


pro s_ROITM_List_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, selROIGroupObj = selROIGroupObj
    fUpdateViewWin = 0b
    widget_control, ev.id, get_uValue = uValue, /no_copy
    uValue.active = ev.index

    case uValue.name of
       'Available Track Objects':begin
          fListSelect = 0
          if ((*uValue.value)[ev.index] ne '-NO SELECTION-') then begin
            case ev.Clicks of
                1:begin
                   widget_control, ev.top, get_uValue = state, /no_copy
                   if (state.oldTrackObjPos ne state.currTrackObjPos) then begin
                      state.oldTrackObjPos = state.currTrackObjPos
                      state.currTrackObjPos = (*uValue.value)[ev.index]
                      state.fUpdateTrackObj = 1b
                   endif else state.fUpdateTrackObj = 0b
                   widget_control, ev.top, set_uValue = state, /no_copy
                endcase
                2:
                else:
             endcase
          endif
       endcase
       'Selected Track-Parameter-Objects':begin
          fListSelect = 1
          if ((*uValue.value)[ev.index] ne '-NO SELECTION-') then begin
             case ev.Clicks of
                1:fUpdateViewWin = 1b
                2:begin
                     widget_control, ev.top, get_uValue = state, /no_copy
                        oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
                        title = state.sTopBaseTitle
                        paramTableUValue = {groupLeader:ev.top,$
                                                 sTopBaseTitle:title,$
                                                 callName:'s_ROI_TrackManipulator_Window',$
                                                 pwIDParamNumList:ptr_new(),$
                                                 wTopBaseID:-1,$
                                                 oContainer:oParamContainer,$
                                                 Index:ev.index,$
                                                 wTableID:-1 }

                        widget_control, ev.id, set_uValue = uValue, /no_copy
                          if state.fParamWidget then begin
                             s_ROIM_ParameterWidget, paramTableUValue = paramTableUValue
                             state.child_ParamWidget_tlb = paramTableUValue.wTopBaseID
                          endif
                          if state.fParamTable then begin
                             s_ROITM_ParamTableWidget, paramTableUValue = paramTableUValue
                             state.child_ParamTableWidget_tlb = paramTableUValue.wTopBaseID
                          endif
                        widget_control, ev.id, get_uValue = uValue, /no_copy
                     widget_control, ev.top, set_uValue = state, /no_copy
                endcase
                else:
             endcase
          endif
       endcase
       'Selected Track-Parameters':begin
          fListSelect = 2
          if ((*uValue.value)[ev.index] ne '-NO SELECTION-') then begin
             case ev.Clicks of
                1:fUpdateViewWin = 1b
                2:begin
                     widget_control, ev.top, get_uValue = state, /no_copy

                     widget_control, state.wListTrackGroupParams, get_uValue = uValueGroupParam, /no_copy
                       selectParamIndex = uValueGroupParam.active
                     widget_control, state.wListTrackGroupParams, set_uValue = uValueGroupParam, /no_copy

                     oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()
                     title = state.sTopBaseTitle
                     paramTableUValue = {groupLeader:ev.top,$          ; widget Group LeaderID
                                         sTopBaseTitle:title,$
                                         callName:'s_ROI_SubParam',$  ; this widget Pro Name
                                         pwIDParamNumList:ptr_new(),$  ; pointer to corellate created widget ID's and Parameter Numbers
                                         wTopBaseID:-1,$
                                         oContainer:oParamContainer,$ ; Filter Container Object
                                         index:selectParamIndex,$      ; Index of Filter Object in Filter Container Object
                                         subParamIndex:ev.index,$      ; Index of Filter Object in Filter Container Object
                                         wTableID:-1}

                     widget_control, ev.id, set_uValue = uValue, /no_copy
                     if state.fParamWidget then begin
                        s_ROIM_ParameterWidget, paramTableUValue = paramTableUValue
                        state.child_ParamWidget_tlb = paramTableUValue.wTopBaseID
                     endif
                     if state.fParamTable then begin
                        s_ROIM_ParamTableWidget, paramTableUValue = paramTableUValue
                        state.child_ParamTableWidget_tlb = paramTableUValue.wTopBaseID
                     endif
                     widget_control, ev.id, get_uValue = uValue, /no_copy
                     widget_control, ev.top, set_uValue = state, /no_copy
                 endcase
                 else:
              endcase
           endif
         endcase
       'Selected Graphic Models': begin
          print, 'do we go here?' ; MOR - 24Mar2011
          endcase
       
    endcase
    widget_control, ev.id, set_uValue = uValue, /no_copy
    if fUpdateViewWin then begin
       widget_control, ev.top, get_uValue = state, /no_copy
         state.fListSelect = fListSelect
         widget_control, state.stack_tlb, get_uValue = stateStack, /no_copy
            dummy = stateStack.child_ViewWindow_tlb
         widget_control, state.stack_tlb, set_uValue = stateStack, /no_copy
       widget_control, ev.top, set_uValue = state, /no_copy
       if widget_info(dummy, /valid) then zimage_colors, {redraw_image, top:dummy}
    endif
    widget_control, ev.top, get_uValue = state, /no_copy
       fUpdateTrackObj = state.fUpdateTrackObj
    widget_control, ev.top, set_uValue = state, /no_copy
    if fUpdateTrackObj then s_ROITM_UpdateGraphicModel, ev.top
    s_ROITM_UpdateWidgets, ev.top
end;-s_ROITM_List_Event------------------------------------------------------------------


pro s_ROITM_UpdateWidgets, wTopBase

   widget_control, wTopBase, get_uValue = state, /no_copy
      stack_tlb = state.stack_tlb
   widget_control, wTopBase, set_uValue = state, /no_copy
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, selROIGroupObj = selROIGroupObj, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos,$
                                             totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum

   widget_control, wTopBase, get_uValue = state, /no_copy

       oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()

       ROIParamList = '-NO SELECTION-'
       if obj_valid(oParamContainer) then for i = 0, (oParamContainer->count())-1 do $
          ROIParamList = [ROIParamList, (*((oParamContainer->get(position = i))->getpParamStruct())).name]
       if (n_elements(ROIParamList) gt 1) then ROIParamList = ROIParamList[1:*]

       widget_control, state.wListTrackGroupParams, get_uValue = uValue, /no_copy
         *uValue.value = ROIParamList
         listSelect = uValue.active < (n_elements(*uValue.value)-1) > 0
       widget_control, state.wListTrackGroupParams, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy

       oROIParam = oParamContainer->get(position = listSelect)
       widget_control, state.wListTrackSubParams, get_uValue = uValueSub, /no_copy
         if obj_valid(oROIParam) then $
            ROISubParamList = paramNameListSet( *(*(oROIParam->getpParamStruct())).pNames, *(*(oROIParam->getpParamStruct())).pActive) $
            else ROISubParamList = '-NO SELECTION-'
         *uValueSub.value = ROISubParamList
         uValueSub.active = uValueSub.active < (n_elements(*uValueSub.value)-1) > 0
       widget_control, state.wListTrackSubParams, set_list_select = uValueSub.active, set_value = *uValueSub.value, set_uValue = uValueSub, /no_copy

       widget_control, state.wListSelectTrackModels, get_uValue = uValue, /no_copy
         listSelect = uValue.active < (n_elements(*uValue.value)-1) > 0
       widget_control, state.wListSelectTrackModels, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy

         ; fUpdate Track-Object Parameter Table Widgets

       if (widget_info(state.child_ROI3DGroupTableWidget_tlb, /valid) and obj_valid(*state.poCurrTrackGroup)) then begin
         paramTableUValue = {groupLeader:state.wTopBase,$
                                 paramNames:(*state.poCurrTrackGroup)->getParameterNameList(),$
                                 paramAsStruct:(*state.poCurrTrackGroup)->getParamAsStruct(),$
                                 name:'oROITrackGroup',$
                                 wTopBase:state.child_ROI3DGroupTableWidget_tlb}
         widget_control, wTopBase, set_uValue = state, /no_copy
          s_OPTW_Update, paramTableUValue = paramTableUValue
         widget_control, wTopBase, get_uValue = state, /no_copy
       endif

       if widget_info(state.child_ParamWidget_tlb, /valid) then begin
          oParamContainer = (*state.poCurrTrackGroup)->getParamContainer()

          tlb = state.child_ParamWidget_tlb
          widget_control, wTopBase, set_uValue = state, /no_copy
             s_ROIM_ParameterWidget_Refresh, tlb, oContainer = oParamContainer
          widget_control, wTopBase, get_uValue = state, /no_copy
       endif
       
       ; MOR - 24Mar2011 - BEGIN
;       if widget_info(state.wTrackChild, /valid) then begin
;          tlb = state.wTrackChild
;          print, 'write a function to do a widget refresh here'
;       endif
       ; MOR - END
       

   widget_control, wTopBase, set_uValue = state, /no_copy, /upDate
   s_ROITM_UpdateXYZPlots, wTopBase
end ;-s_ROITM_UpdateWidgets------------------------------------------------------------------


pro s_ROITM_Update, wTopBase, fMultiple = fMultiple, stack_tlb = stack_tlb

;;;;;;;;;;;;;;;;;;;;; TODO - MOR - 22 MAR 2011 
; NEED TO MAKE A FUNCTION TO UPDATE THE STATUS CODE TIME BY TIME WHICH IS CALLED FROM THIS FUNCTION

   widget_control, wTopBase, get_uValue = state, /no_copy
      stack_tlb = state.stack_tlb
   widget_control, wTopBase, set_uValue = state, /no_copy

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos,$
                                             totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
                                             selClusName = selClusName, selROIGroupObj = selROIGroupObj
   widget_control, wTopBase, get_uValue = state, /no_copy

      startZ  = keyword_set(fMultiple) ?  0 : zPos
      endZ    = keyword_set(fMultiple) ?  totalZNum-1:zPos

   for k=startZ, endZ do begin
      ; fUpdate selected Track name
      state.strID = strCompress('Ch' + string(chPos) + '_' + 'Z' + string(k) + '_'  + 'Clus' + string(clusPos) + '_', /rem)
      state.sTopBaseTitle = 's_Track |->' + state.strID
      widget_control, state.wTopBase, tlb_set_title = state.sTopBaseTitle

         ; decide about updates
      fUpdate = 1b
      if (state.currZPos ne zPos) or (state.currChPos ne chPos) or (state.currClusPos ne clusPos) then begin
         state.fUpDateTrackGroup = 1b
         state.fKeepTrackParamThreshs = 1b
      endif

      if (state.currTPos ne tPos) then begin
         state.fUpDateTrackGroupProps = 1b
         state.fKeepTrackParamThreshs = 1b
      endif

      if( keyword_set(fMultiple)) then begin
        state.fUpDateTrackGroup = 1b
        state.fKeepTrackParamThreshs = 1b
      endif

      state.currTPos = tPos
      state.currChPos = chPos
      state.currZPos = zPos
      state.currClusPos = clusPos

        ; update TrackGroup
      if state.fUpDateTrackGroup then begin
         if ptr_valid(state.poCurrTrackGroup) then begin
            if obj_valid(*state.poCurrTrackGroup) then begin
                 ;if not(obj_isa(*state.poCurrTrackGroup, 'C_sTrackGroupObj')) then begin
                    if( (keyword_set(fMultiple) && k eq startZ) || ~keyword_set(fMultiple)) then begin
                      obj_destroy, *state.poCurrTrackGroup
                      *state.poCurrTrackGroup = obj_new('C_sTrackGroupObj')
                    endif
                 ;endif
                 
                 paramStruct = (*state.poCurrTrackGroup)->getParamAsStruct()
                 paramStruct.a = tPos
                 paramStruct.b = chPos
                 paramStruct.c = k
                 paramStruct.d = clusPos
                 paramStruct.e = 0
                 paramStruct.f = totalTNum
                 paramStruct.g = 1
                 (*state.poCurrTrackGroup)->setParamAsStruct, paramStruct
  
                 widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
                    poStackTrackContainer = stateObj.poStackTrackContainer
                 widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy
  
                 ;(*state.poCurrTrackGroup)->initObjects, poStackTrackContainer = poStackTrackContainer, strID = state.strID, selROIGroupObj = selROIGroupObj, topEvent = wTopBase
                 (*state.poCurrTrackGroup)->initObjects, poStackTrackContainer = poStackTrackContainer, strID = state.strID, selROIGroupObj = selROIGroupObj; Susana , quito lo incluido por felipe santibañes
            endif
         endif
      endif
   endfor

         ;fUpdate wListTrackObj
       widget_control, state.wListTrackObj, get_uValue = uValueROI, /no_copy
         ROIGroupList = ['-NO TRACK OBJECT-']
         for i = 1, (*state.poCurrTrackGroup)->getObjectNumber() do ROIGroupList = [ROIGroupList, strCompress('TrackObject_'+ string(i), /rem)]
         if (n_elements(ROIGroupList) gt 1) then ROIGroupList = ROIGroupList[1:*]
         if ptr_valid(uValueROI.value) then *uValueROI.value = ROIGroupList else uValueROI.value = ptr_new(ROIGroupList, /no_copy)
       widget_control, state.wListTrackObj, set_list_select = uValueROI.active, set_value = *uValueROI.value, set_uValue = uValueROI, /no_copy


         ;calculate Params
       if state.fRestoreAndCalculate then begin
         if obj_valid(*state.poCurrTrackGroup) then begin
          ev = {top:state.wTopBase, id:state.wCalcAllParams, silent:1b}
          widget_control, wTopBase, set_uValue = state, /no_copy
              s_ROI_TrackManipulator_Window_Button_Event, ev
          widget_control, wTopBase, get_uValue = state, /no_copy
         endif
       endif

         ;filter Params
       if (state.fParamFilter and not(state.fRestoreAndCalculate)) then begin
          ev = {top:state.wTopBase, id:state.wApplyAllParamFilters, silent:1b}
          widget_control, wTopBase, set_uValue = state, /no_copy
             s_ROI_TrackManipulator_Window_Button_Event, ev
          widget_control, wTopBase, get_uValue = state, /no_copy
       endif

       if state.fSaveResults then if obj_valid(*state.poCurrTrackGroup) then begin
          poDummy = state.poCurrTrackGroup
          if (state.currTrackGroupFileName ne s_getPathForSystem()) then begin
             oTrackGroup = *state.poCurrTrackGroup
             save, oTrackGroup, filename = state.currTrackGroupFileName
          endif
       endif

   widget_control, wTopBase, set_uValue = state, /no_copy, /update
   if fUpdate then s_ROITM_UpdateWidgets, wTopBase
end


pro s_ROITM_Resize_Event, ev
   widget_control, ev.top, get_uValue = state, /no_copy

      if (!VERSION.OS_FAMILY eq 'Windows') then begin
         widget_control, state.wListBase_1, scr_xsize = ev.x-10, scr_ysize = floor((ev.y-43)/4.)
         widget_control, state.wListBase_2, scr_xsize = ev.x-10, scr_ysize = floor(2.*(ev.y-43)/4.)
         widget_control, state.wListBase_3, scr_xsize = ev.x-10, scr_ysize = floor((ev.y-43)/4.)
      endif

      evx = ev.x-13
      evy = floor((ev.y-170)/4.)
      widget_control, state.wListTrackObj, scr_xsize = evx, scr_ysize = evy
      widget_control, state.wListTrackGroupParams, scr_xsize = evx, scr_ysize = evy+5
      widget_control, state.wListTrackSubParams, scr_xsize = evx, scr_ysize = evy+10
      widget_control, state.wListSelectTrackModels, scr_xsize = evx, scr_ysize = evy+5
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_ROITM_cleanUp, wTopBase
    widget_control, wTopBase, get_uValue = status, /no_copy

    if (n_elements(status) ne 0) then begin
       if widget_info(status.groupLeader, /valid_id) then begin
         widget_control, status.groupLeader, get_uValue = statusParent, /no_copy
         if (n_elements(statusParent) ne 0) then begin
            if statusParent.fROITrackWindow then statusParent.fROITrackWindow = s_ToggleButtonOnOffState(statusParent.wROITrackWindowOnOff)
            widget_control, status.groupLeader, set_uValue = statusParent, /no_copy
         endif
       endif

       if obj_valid(*status.poCurrTrackGroup) then begin
          obj_destroy, *status.poCurrTrackGroup
          if ptr_valid(status.poCurrTrackGroup) then ptr_free, status.poCurrTrackGroup
       endif

       if obj_valid(*status.poCurrROIGraphicModel) then begin
          for i = (*status.poCurrROIGraphicModel->count())-1,0,-1 do begin
             oDummy = *status.poCurrROIGraphicModel->get(position = i)
             (*status.poCurrROIGraphicModel)->remove, position = i
             obj_destroy, oDummy
          endfor
          obj_destroy, *status.poCurrROIGraphicModel
          if ptr_valid(status.poCurrROIGraphicModel) then ptr_free, status.poCurrROIGraphicModel
       endif

       for i = 0,n_tags(status)-1 do begin
           case size(status.(i), /tname) of
          'POINTER':ptr_free, status.(i)
          'OBJREF':obj_destroy, status.(i)
          else:
          endcase
        endfor
       if widget_info(status.groupLeader, /valid_id) then widget_control, status.groupLeader, /map
    endif
end


pro s_ROI_TrackManipulator_Window, groupLeader = groupLeader, basePosition = basePosition, wTopBase=wTopBase

       ;Check the validity of the group identifier.
    if (n_elements(groupLeader) eq 0) then return
    if not(widget_info(groupLeader, /valid_id)) then return
    if not(keyWord_set(basePosition)) then basePosition = [0,0]
    wTopBase = widget_base(title = 's_Track |->', xpad = 2, ypad = 2, xOffset = basePosition[0], MBar = menuBase, Group_Leader = groupLeader, tlb_size_events = 1, /column)

        ; Create menu buttons
    wMenuOptionsButton = widget_button(menuBase, value = 'Options', event_pro = 's_ROI_TrackManipulator_Window_Button_Event')
       void = widget_button(wMenuOptionsButton, value = 'Open ROI-Params', uVal = 'OPENPARAMS')
       void = widget_button(wMenuOptionsButton, value = 'Save ROI-Params as...', uVal = 'SAVEPARAMSAS')
       void = widget_button(wMenuOptionsButton, value = 'Open ROI-Stack-Track', uVal = 'OPENROISTACKTRACK', /sep)
       void = widget_button(wMenuOptionsButton, value = 'Save ROI-Stack-Track as...', uVal = 'SAVEROIROISTACKTRACKAS')
       wSaveStackTrackParamsAs = widget_button(wMenuOptionsButton, value = 'Save ROI-Stack-Track Params as ASCII...', uVal = 'SAVEROIROISTACKTRACKPARAMSASASCII', sensitive = 0)
       wShowROIObjectDataButton = widget_button(wMenuOptionsButton, value = 'Show ROI-Parameter Data as ....', /menu, /sep)
       wParamTableOnOff = widget_button(wShowROIObjectDataButton, value = '... Table (off)', uVal = 'SHOWASDATATABLEONOFF')
       wParamWidgetOnOff = widget_button(wShowROIObjectDataButton, value = '... Widgets (on )', uVal = 'SHOWASWIDGETONOFF')

    wMenuParamButton = widget_button(menuBase, value = 'Parameters', event_pro = 's_ROI_TrackManipulator_Window_Button_Event')
       wCalcSelectedParam = widget_button(wMenuParamButton, value = 'Calculate Selected Parameter', uVal = 'CALCULATESELECTEDPARAMETER')
       wCalcAllParams = widget_button(wMenuParamButton, value = 'Calculate All Parameters', uVal = 'CALCULATEALLPARAMETERS')
       void = widget_button(wMenuParamButton, value = 'Apply Selected Parameter Filter', uVal = 'APPLYSELECTEDPARAMETERFILTER', /sep)
       wApplyAllParamFilters = widget_button(wMenuParamButton, value = 'Apply All Parameter Filters', uVal = 'APPLYALLPARAMETERFILTERS')
       wKeepVisualParamsOnOff = widget_button(wMenuParamButton, value = '...|-> Keep Visualization Parameters (off)', uVal = 'KEEPVISUALIZATIONPARAMSONOFF', /sep)
       wKeepSelectROIParamsFiltersOnOff = widget_button(wMenuParamButton, value = '...|-> Keep Selected ROI-Param-Filters (off)', uVal = 'KEEPSELECTEDROIPARAMFILTERSONOFF')
       wKeepSelectROIParamsOnOff = widget_button(wMenuParamButton, value = '...|-> Keep Selected ROI-Params (off)', uVal = 'KEEPSELECTEDROIPARAMSONOFF')
       wComplementROIParamsOnOff = widget_button(wMenuParamButton, value = '...|-> Complement ROI-Params (off)', uVal = 'COMPLEMENTROIPARAMSONOFF')
       wRestoreAndCalculateOnOff = widget_button(wMenuParamButton, value = '...|-> Calculate Parameters (off)', uVal = 'CALCULATEPARAMETERSONOFF')
       wParamFilterOnOff = widget_button(wMenuParamButton, value = '...|-> Parameter Filter (off)', uVal = 'PARAMETERFILTERONOFF')
       wSaveResultsOnOff = widget_button(wMenuParamButton, value = '...|-> Save Results Automatically (off)', uVal = 'SaveResultsONOFF')
       wStartFromSelectedTimeOnOff = widget_button(wMenuParamButton, value = '...|-> Start from Selected Time (off)', uVal = 'STARTFROMSELECTEDTIMEONOFF', /sep)
       void = widget_button(wMenuParamButton, value = '...||-> Apply to ...', /menu)
       void_2 = widget_button(void, value = '... all Times', uVal = 'APPLYTOALLTIMES')
       void_2 = widget_button(void, value = '... all ZSlices', uVal = 'APPLYTOALLZSLICES')
       void_2 = widget_button(void, value = '... all ZSlices and Times', uVal = 'APPLYTOALLZSLICESANDTIMES')
       void_2 = widget_button(void, value = '... all ZSlices and Channels', uVal = 'APPLYTOALLZSLICESANDCHANNELS')
       void_2 = widget_button(void, value = '... all ZSlices and Channels and Times', uVal = 'APPLYTOALLZSLICESANDCHANNELSANDTIMES')

    wMenuButton = widget_button(menuBase, value = 'Analyse', event_pro = 's_ROI_TrackManipulator_Window_Button_Event')
       wROIModelWindowOnOff = widget_button(wMenuButton, value = 'ROI Model Window (off)', uVal = 'ROIMODELWINDOWONOFF')

    wListBase_1 = widget_base(wTopBase, /column, /align_center)
       void = widget_button(wListBase_1, value = ' |-> Save Track-Group <-| ', uValue = 'SAVETRACKGROUP', event_pro = 's_ROI_TrackManipulator_Window_Button_Event')
       wListTrackObj = widget_list(wListBase_1, xSize = 30, ySize = 2, value = ['-NO GROUP-'],$
                        uValue = {name:'Available Track Objects', value:ptr_new(['-NO GROUP-'], /no_copy), active:0 },$
                        event_pro = 's_ROITM_List_Event',kill_notify = 'CleanList')

    paramNameList = s_ROITM_getROIParamNameList()
    wListBase_2 = widget_base(wTopBase, /column, /align_center)
       void = widget_button(wListBase_2, value = 'Add Track-Parameter |->', /menu, event_pro = 's_ROITM_AddParam_Event')
       for i = 0, n_elements(paramNameList)-1 do dummy = widget_button(void, value = paramNameList[i])
       wListTrackGroupParams = widget_list(wListBase_2, xSize = 30, ySize = 4, value = ['-NO SELECTION-'],$
                             uValue = {name:'Selected Track-Parameter-Objects', value:ptr_new(['-NO SELECTION-'], /no_copy), active:0},$
                             event_pro = 's_ROITM_List_Event', kill_notify = 'CleanList')

       void = widget_label(wListBase_2, value = 'Track-Sub-Parameters |-> ')
       wListTrackSubParams = widget_list(wListBase_2, xSize = 30, ySize = 4, value = ['-NO SELECTION-'],$
                             uValue = {name:'Selected Track-Parameters', value:ptr_new(['-NO SELECTION-'], /no_copy), active:0},$
                             event_pro = 's_ROITM_List_Event', kill_notify = 'CleanList')

    graphicNameList = s_ROITM_getROIGraphicNameList()
    wListBase_3 = widget_base(wTopBase, /column, /align_center)
       void = widget_button(wListBase_3, value = 'Add Graphic Model |->', /menu, event_pro = 's_ROITM_AddGraphicModel_Event')
       for i = 0, n_elements(graphicNameList)-1 do dummy = widget_button(void, value = graphicNameList[i])
       wListSelectTrackModels = widget_list(wListBase_3, xSize = 30, ySize = 4, value = ['-NO SELECTION-'],$
                             uValue = {name:'Selected Graphic Models', value:ptr_new(['-NO SELECTION-'], /no_copy), id:ptr_new([-1l], /no_copy), active:0},$
                             event_pro = 's_ROITM_List_Event', kill_notify = 'CleanList')

    wButtonBase = widget_base(wTopBase, /row, event_pro = 's_ROI_TrackManipulator_Window_Button_Event')
       void = widget_button(wButtonBase, value = 'Delete', uVal = 'DELETE')
       void = widget_button(wButtonBase, value = 'Up', uVal = 'MOVEUP')
       void = widget_button(wButtonBase, value = 'Down', uVal = 'MOVEDOWN')

       wXYZAxisButton = widget_button(wButtonBase, value = 'X-Axis', /menu)
       wXAxisMixedButton = widget_button(wXYZAxisButton, value = 'X-Axis |-> -NO SELECTION-', uValue = 'SETXMIXEDAXIS')
       wYAxisMixedButton = widget_button(wXYZAxisButton, value = 'Y-Axis |-> -NO SELECTION-', uValue = 'SETYMIXEDAXIS', sensitive = 0)
       wZAxisMixedButton = widget_button(wXYZAxisButton, value = 'Z-Axis |-> -NO SELECTION-', uValue = 'SETZMIXEDAXIS', sensitive = 0)
       void = widget_button(wXYZAxisButton, value = '|-> Mixed Parameter Plot', /menu)
       wXYMixedDataPlotOnOff = widget_button(void, value = 'XY-Mixed Data Plot (off)', uValue = 'XYMIXEDDATAPLOTONOFF')

       ; Realize the widgets.
    widget_control, wTopBase, /realize
    
       ; Initialize state & widgets
    state = {groupLeader:groupLeader,$
          ROI_tlb:groupLeader,$
          stack_tlb:0l,$
          wTopBase:wTopBase,$
          sTopBaseTitle:'X',$
          sInfoString:'X',$
          sInfoDim:'X',$
          currentStackTrackFileName:s_getPathForSystem(),$
          wListBase_1:wListBase_1,$
          wListBase_2:wListBase_2,$
          wListBase_3:wListBase_3,$
          fListSelect:0,$
          wListTrackObj:wListTrackObj,$
          child_ROI2DGroupTableWidget_tlb:-1L,$
          child_ROI3DGroupTableWidget_tlb:-1L,$
          wListTrackGroupParams:wListTrackGroupParams,$
          wListTrackSubParams:wListTrackSubParams,$
          wListSelectTrackModels:wListSelectTrackModels,$
          wCalcSelectedParam:wCalcSelectedParam,$
          wCalcAllParams:wCalcAllParams,$
          wApplyAllParamFilters:wApplyAllParamFilters,$
          wSaveStackTrackParamsAs:wSaveStackTrackParamsAs,$

          wRestoreAndCalculateOnOff:wRestoreAndCalculateOnOff,$
             fRestoreAndCalculate:0b,$
          wParamFilterOnOff:wParamFilterOnOff,$
             fParamFilter:0b,$
          wParamWidgetOnOff:wParamWidgetOnOff,$
             fParamWidget:1b,$
             child_ParamWidget_tlb:-1L,$
          wParamTableOnOff:wParamTableOnOff,$
             fParamTable:0b,$
             child_ParamTableWidget_tlb:-1L,$

          wKeepVisualParamsOnOff:wKeepVisualParamsOnOff,$
             fKeepVisualParams:0b,$
          wKeepSelectROIParamsFiltersOnOff:wKeepSelectROIParamsFiltersOnOff,$
             fKeepSelectParamFilters:0b,$
             wKeepSelectROIParamsOnOff:wKeepSelectROIParamsOnOff,$
             fKeepSelectParams:0b,$
          wComplementROIParamsOnOff:wComplementROIParamsOnOff,$
             fComplParams:0b,$
             wSaveResultsOnOff:wSaveResultsOnOff,$
             fSaveResults:0b,$

          wStartFromSelectedTimeOnOff:wStartFromSelectedTimeOnOff,$
             fStartFromSelectedTime:0b,$
          wXYZAxisButton:wXYZAxisButton,$
          wXAxisMixedButton:wXAxisMixedButton,$
          wYAxisMixedButton:wYAxisMixedButton,$
          wZAxisMixedButton:wZAxisMixedButton,$
          fXYZMixedSensitive:0,$
          wXYMixedDataPlotOnOff:wXYMixedDataPlotOnOff,$
             fXYMixedDataPlotControl:0b,$
             child_MixedDataPlot_tlb:-1L,$
          fRefreshMixedData:1b,$
          child_ObjViewWindow_tlb:-1L,$
             selXYZObjectROIParamName:['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-'],$
             selXYZMixedROIParamName:['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-'],$
             selXYZGroupROIParamName:['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-'],$

             currTPos:-1,$
             currChPos:-1,$
             currZPos:-1,$
             currClusPos:-1,$
             currTrackObjPos:0,$

             oldTPos:-1,$
             oldChPos:-1,$
             oldZPos:-1,$
             oldClusPos:-1,$
             oldTrackObjPos:-1,$
             
             fUpdateTrackObj:0b,$


             strID:'NO SELECTION',$
             poCurrTrackGroup:ptr_new(obj_new('C_sTrackGroupObj'),/no_copy),$
             currTrackGroupFileName:s_getPathForSystem(),$

             fUpDateTrackGroup:1b,$
             fUpDateTrackGroupProps:0b,$
             poCurrROI3DGroup:ptr_new(),$

             fKeepTrackParamThreshs:0b,$

             poCurrROIGraphicModel:ptr_new(obj_new()),$
             graphicNameList:graphicNameList,$
             wTrackPlot:-1}; MOR - 24Mar2011
             

    if widget_info(state.ROI_tlb, /valid) then begin
       widget_control, state.ROI_tlb, get_uValue = ROIState
          state.stack_tlb = ROIState.stack_tlb
          ROIState.child_ROITrackWindow_tlb = wTopBase
       widget_control, state.ROI_tlb, set_uValue = ROIState
    endif else state.stack_tlb = -1

       ; Store the state structure in the uValue of the wTopBase. fUpdate Correct_And_Segment widgets, & Call XManager.
    widget_control, wTopBase, set_uValue = state, /no_copy
    XManager, 's_ROITM_Resize', wTopBase, cleanUp = 's_ROITM_cleanUp', /no_block, group_Leader = groupLeader
    s_ROITM_Update, wTopBase
end