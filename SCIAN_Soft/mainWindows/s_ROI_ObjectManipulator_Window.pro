;_____________________________IOISIOI____________________
; NAME:
;      s_ROI_ObjectManipulator_Window and Events
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

function s_ROIOM_getROIParamNameList
   return, ['C_sROIParam_ObjAABM_QualityIndicators',$
            'C_sROIParam_ObjAABM_Descriptors',$
            'C_sROIParam_ObjActiveContour',$
            'C_sROIParam_ObjSize',$
            'C_sROIParam_ObjPerimeter',$
            'C_sROIParam_ObjChannelIntensity',$
            'C_sROIParam_ObjChannelIntensityRatio',$
            'C_sROIParam_ObjChannelScale',$
            'C_sROIParam_ObjColocalization',$
            'C_sROIParam_ObjMomentsOfMorphology',$
            'C_sROIParam_ObjMomentsOfSizeMorphology',$
            'C_sROIParam_ObjMomentsOfIntensity',$
            'C_sROIParam_ObjNumber',$
            'C_sROIParam_ObjNumberInCluster',$
            'C_sROIParam_ObjTextureSimple',$
            'C_sROIParam_ObjTextureHaralick',$
            'C_sROIParam_ObjTime',$
            'C_sROIParam_ObjTrackObject',$
            'C_sROIParam_3DObjBoxModel',$
            'C_sROIParam_3DObjBottleModel',$
            'C_sROIParam_3DObjChannelIntensity',$
            'C_sROIParam_3DObjChannelIntensityRatio',$
            'C_sROIParam_3DObjMomentsOfMorphology',$
            'C_sROIParam_3DObjNumber',$
            'C_sROIParam_3DObjNumberWSons',$
            'C_sROIParam_3DObjNumberWLateralidad',$
            'C_sROIParam_3DObjOverlap',$
            'C_sROIParam_3DObjGraphSkelDir',$            
            'C_sROIParam_3DObjSurface',$
            'C_sROIParam_3DObjSurfaceACtests',$
            'C_sROIParam_3DObjOpticalFlow',$
            'C_sROIParam_3DObjTime',$
            'C_sROIParam_3DObjTracking',$
            'C_sROIParam_3DObjVolume',$
            'C_sROIParam_3DTrackObject',$
            'C_sROIParam_InterObjBorderNeighbour',$
            'C_sROIParam_InterObjBorderDist',$
            'C_sROIParam_InterObjCenterDist',$
            'C_sROIParam_InterObjDipole',$
            'C_sROIParam_GroupPhase',$
            'C_sROIParam_GroupObjNumber',$
            'C_sROIParam_ObjOpticalFlow',$
            'C_sROIParam_ObjTrackParameters']
end


function s_ROIOM_getROIGraphicNameList
    return, ['2D Object Model',$
         '2D Border Model',$
         '2D Border Neighbour Model',$
         '2D Border Distance Model',$
         '2D Center Distance Model',$
         '2D Image Model',$
         '2D Sub Phase Model',$
         '2D Active Contours',$
         '2D Adjacent Active Contours',$
         '2D OF',$
         '2D AAC+OF',$
         '2D Track Path',$
         '2D Voronoi Polygons from ROI mass center',$
         '3D Arbritary Axis AMA Model',$
         '3D Balls Model',$
         '3D Balls Min-Max Model',$
         '3D Border Model',$
         '3D Cells Division',$
         '3D Fill Volume Model',$
         '3D Intensity Model',$
         '3D Intensity UnDrifted Model',$
         '3D Load OBJ_OFF Model',$
         '3D Optical Flow Model',$
         '3D PA Box Model',$
         '3D PA Bottle Model',$
         '3D Plane Model Base',$
         '3D Plane Model Complementary',$
         '3D Plane Model Orthogonal',$
         '3D UnDrifted Plane Model Base',$
         '3D UnDrifted Plane Model Complementary',$
         '3D UnDrifted Plane Model Orthogonal',$                   
         '3D Principal Axis (Group)',$
         '3D Principal Axis (Obj)',$
         '3D Principal UnDrifted Axis (Estimated Group)',$
         '3D Principal UnDrifted Axis (Group)',$
         '3D Principal UnDrifted Axis (Obj)',$
         '3D Point Model',$
         '3D Point Model Gallery',$
         '3D Project Model Gallery',$
         '3D ROI-Intensity Model',$
         '3D ROI-Intensity Model by Object',$
         '3D Graph Skeleton Model',$
         '3D Skeleton From Mesh Model',$
         '3D SkelTree From Mesh Model',$
         '3D Sphere Projection',$
         '3D Surface AC Model',$
         '3D Surface Center Distance I',$
         '3D Surface Center Distance II',$
         '3D Surface Curvature Model',$
         '3D Surface Distance Model',$
         '3D Surface Intensity Model',$
         '3D Surface Intensity Model (Binary)',$
         ;'3D Surface Compute-Mesh Model',$
         '3D Surface Mesh Model',$ ;other objects are using this data model
         '3D Surface Mesh-Fix Model',$
         ;'3D Surface Mesh Model OvrC',$
         ;'3D Surface Model',$
         '3D Modelling CPM_DFCs Model',$
         '3D UnDrifted TorsionSphere Ref 0',$
         '3D UnDrifted Surface Center Distance I',$
         '3D UnDrifted Surface Center Distance I Ref 0',$
         '3D UnDrifted Surface Center Distance II',$
         '3D UnDrifted Surface Center Distance II Ref 0',$
         '3D Surface UnDrifted Model',$
         '3D Surface AC UnDrifted Model',$         
         '3D Tracking Estimated Model',$
         '3D Tracking Estimator Trajectory Model',$
         '3D Track By Deformation Model',$
         '3D Track UnDrifted By Deformation Model',$
         '3D Track Group UnDrifted Model',$
         '3D Track Smooth UnDrifted Model',$     
         '3D Track UnDrifted Model',$
         '3D Track Model',$
         '3D Tracking ImageJ Model']
end

function s_ROIOM_getROIObjModelParamList, ROI_tlb = ROI_tlb
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb,  ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos
    if obj_valid(poCurrROI3DGroup) then objectModelList = (poCurrROI3DGroup)->getObjectModelParamList(ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos)
    return, objectModelList
end

function s_ROIOM_getROIObjModelList, ROI_tlb = ROI_tlb
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then objectModelList = (*poCurrROI3DGroup)->getObjectModelList(ROIObjPos = ROIObjPos)
    return, objectModelList
end

function s_ROIOM_getROIObjectNumberList, ROI_tlb = ROI_tlb
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    if obj_valid(*poCurrROI3DGroup) then return, (*poCurrROI3DGroup)->getObjectNumberVector() else return, -1
end

function s_ROIOM_getoROIObjectModelContainer, ROI_tlb = ROI_tlb
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then return, (*poCurrROI3DGroup)->getoROIObjectModelContainer(ROIObjPos = ROIObjPos) else return, -1
end

function s_ROIOM_getoROIObjectModel, ROI_tlb = ROI_tlb
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos
    if obj_valid(poCurrROI3DGroup) then return, (poCurrROI3DGroup)->getoROIObjectModel(ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos) else return, -1
end

pro s_ROIOM_addObjectToGroup, ROI_tlb = ROI_tlb, ROIObjPos = ROIObjPos
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    if obj_valid(poCurrROI3DGroup) then (poCurrROI3DGroup)->addObjectToGroup, ROIObjPos = ROIObjPos
end

pro s_ROIOM_deleteObjectInGroup, ROI_tlb = ROI_tlb, ROIObjPos = ROIObjPos
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    if (n_elements(ROIObjPos) eq 0) then s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(poCurrROI3DGroup) then (poCurrROI3DGroup)->deleteObjectInGroup, ROIObjPos = ROIObjPos
end

pro s_ROIOM_addModelToObject, ROI_tlb = ROI_tlb, selROIObjModel = selROIObjModel, position = position
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then (*poCurrROI3DGroup)->addModelToObject, ROIObjPos = ROIObjPos, selROIObjModel = selROIObjModel, position = position
end

pro s_ROIOM_deleteModelInObject, ROI_tlb = ROI_tlb, ROIObjModelPos = ROIObjModelPos
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    if (n_elements(ROIObjModelPos) eq 0) then s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos else $
       s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then (*poCurrROI3DGroup)->deleteModelInObject, ROIObjModelPos = ROIObjModelPos, ROIObjPos = ROIObjPos
end

pro s_ROIOM_setObjParamsFromModels, ROI_tlb = ROI_tlb, all = all
    widget_control, ROI_tlb, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       poCurrROI3DGroup = state.poCurrROI3DGroup
    widget_control, ROI_tlb, set_uValue = state, /no_copy
    if (n_elements(all) eq 0) then s_ISM_getProjectInfo, stack_tlb = stack_tlb, ROIObjPos = ROIObjPos
    if obj_valid(*poCurrROI3DGroup) then (*poCurrROI3DGroup)->setObjParamsFromModels, ROIObjPos = ROIObjPos
end


function s_ROIOM_getStackTrackContainer, wTopBase = wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       oStackTrackContainer = *state.poStackTrackContainer
    widget_control, wTopBase, set_uValue = state, /no_copy
    save, oStackTrackContainer, filename = s_getPathForSystem()+'obj.tmp'
    restore, s_getPathForSystem()+'obj.tmp', restored_objects = oStackTrackContainer, /relaxed
    for i = 0, (n_elements(oStackTrackContainer)-1) do if obj_isa(oStackTrackContainer[i], 'C_sROIParam_StackTrackContainer') then return, oStackTrackContainer[i]
end


function s_ROIOM_getSelectedXYZObjectROIParamName, wTopBase = wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       widget_control, state.wXAxisObjectButton, get_value = dataName
         selXYZDataROIParamName = strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )
       widget_control, state.wYAxisObjectButton, get_value = dataName
         selXYZDataROIParamName = [selXYZDataROIParamName, strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )]
       widget_control, state.wZAxisObjectButton, get_value = dataName
         selXYZDataROIParamName = [selXYZDataROIParamName, strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )]
    widget_control, wTopBase, set_uValue = state, /no_copy
    return, selXYZDataROIParamName
end


function s_ROIOM_getSelectedXYZGroupROIParamName, wTopBase = wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       widget_control, state.wXAxisGroupButton, get_value = dataName
         selXYZGroupROIParamName = strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )
       widget_control, state.wYAxisGroupButton, get_value = dataName
         selXYZGroupROIParamName = [selXYZGroupROIParamName, strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )]
       widget_control, state.wZAxisGroupButton, get_value = dataName
         selXYZGroupROIParamName = [selXYZGroupROIParamName, strMid(dataName,(strPos(dataName,'|-> ', /reverse_s))+4, (strlen(dataName)-strPos(dataName,'|-> ', /reverse_s)-1) )]
    widget_control, wTopBase, set_uValue = state, /no_copy
    return, selXYZGroupROIParamName
end


function s_ROIOM_getSelectedXYZMixedROIParamName, wTopBase = wTopBase
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


pro s_ROIOM_passROIStackTrack, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, selClusName = selClusName
    widget_control, wTopBase, get_uValue = state, /no_copy

       if ((state.fROIParamWindow eq 0) or not(widget_info(state.child_ROIParamWindow_tlb, /valid))) then begin
         if (state.fROIParamWindow eq 0) then state.fROIParamWindow = s_ToggleButtonOnOffState(state.wROIParamWindowOnOff)
          ; close active window
         if widget_info(state.child_ROIParamWindow_tlb, /valid) then begin
          widget_control, state.child_ROIParamWindow_tlb, /destroy
          state.child_ROIParamWindow_tlb = -1l
         endif
          ; open new active window
         widget_control, wTopBase, tlb_get_size = baseSize, tlb_get_Offset = baseOffset
         widget_control, wTopBase, set_uValue = state, /no_copy
          s_ROI_ParameterManipulator_Window, groupLeader = wTopBase, application_tlb = application_tlb, basePosition = (baseSize+baseOffset)
         widget_control, wTopBase, get_uValue = state, /no_copy
         state.child_ROIParamWindow_tlb = application_tlb
       endif

         ; save StackTrackContainer
       oStackTrackContainer = *state.poStackTrackContainer
    widget_control, wTopBase, set_uValue = state, /no_copy
    save, oStackTrackContainer, fileName = s_getPathForSystem()+'obj.tmp'
    widget_control, wTopBase, get_uValue = state, /no_copy


       ; delete active data set in ROIPM
    widget_control, state.child_ROIParamWindow_tlb, get_uValue = stateParam, /no_copy
         ev_struct = {______fBUTTON,$
                        name:strCompress(selClusName+'|#_'+string(tPos+1), /rem),$
                        select:1,$
                        fUpdate:0,$
                        top:stateParam.wTopBase,$
                        id:stateParam.wDeleteActiveDataSet }
       widget_control, state.child_ROIParamWindow_tlb, set_uValue = stateParam, /no_copy
       widget_control, wTopBase, set_uValue = state, /no_copy
         s_ROI_ParameterManipulator_Window_event, ev_struct
       widget_control, wTopBase, get_uValue = state, /no_copy

         ; open new active data set in ROIPM
       widget_control, state.child_ROIParamWindow_tlb, get_uValue = stateParam, /no_copy
         ev_struct.id = stateParam.wPassActiveDataSet
         ev_struct.fUpdate = 1b
       widget_control, state.child_ROIParamWindow_tlb, set_uValue = stateParam, /no_copy
       widget_control, wTopBase, set_uValue = state, /no_copy
         s_ROI_ParameterManipulator_Window_event, ev_struct
       widget_control, wTopBase, get_uValue = state, /no_copy
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROIOM_UpdateCurrentROIPhaseModel, wTopBase = wTopBase, fUpDateROI3DGroup = fUpDateROI3DGroup, fUpDateROI2DGroup = fUpDateROI2DGroup
    widget_control, wTopBase, get_uValue = state, /no_copy
       if (n_elements(fUpDateROI2DGroup) ne 0) then state.fUpDateROI2DGroup = fUpDateROI2DGroup
       if (n_elements(fUpDateROI3DGroup) ne 0) then state.fUpDateROI3DGroup = fUpDateROI3DGroup
       stack_tlb = state.stack_tlb

           ; get includeModelList
       widget_control, state.wListSelectROIGraphicModels, get_uValue = uListValue, /no_copy
          if ((*uListValue.value)[0] eq '-NO SELECTION-') then begin
             *uListValue.value = '2D Object Model'
             uListValue.active = 0
          endif
          includeModelList = *uListValue.value
       widget_control, state.wListSelectROIGraphicModels, set_list_select = uListValue.active, set_value = *uListValue.value, set_uValue = uListValue, /no_copy

       if (state.fUpDateROI2DGroup and obj_valid((*state.poCurrROI2DGroup))) then begin
          poDummy = state.poCurrROI2DGroup
          widget_control, wTopBase, set_uValue = state, /no_copy
            *poDummy->manageCurrentPhaseModel, includeModelList = includeModelList, stack_tlb = stack_tlb
          widget_control, wTopBase, get_uValue = state, /no_copy
       endif

       if (state.fUpDateROI3DGroup and obj_valid((*state.poCurrROI3DGroup))) then begin
          poDummy = state.poCurrROI3DGroup
          widget_control, wTopBase, set_uValue = state, /no_copy
            *poDummy->manageCurrentPhaseModel, includeModelList = includeModelList, stack_tlb = stack_tlb
          widget_control, wTopBase, get_uValue = state, /no_copy
       endif

       state.fUpDateROI3DGroup = 0b
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROIOM_UpdateXYZObjectPlotWin, wTopBase = wTopBase

    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       fXObjHistControl = state.fXObjHistControl
       fYObjHistControl = state.fYObjHistControl
       fZObjHistControl = state.fZObjHistControl
       fKeepROI2DParamThreshs = state.fKeepROI2DParamThreshs
       state.fKeepROI2DParamThreshs = 0b
       fKeepROI3DParamThreshs = state.fKeepROI3DParamThreshs
       state.fKeepROI3DParamThreshs = 0b
       child_XDataHist_tlb = state.child_XDataHist_tlb
       child_YDataHist_tlb = state.child_YDataHist_tlb
       child_ZDataHist_tlb = state.child_ZDataHist_tlb
       child_ObjViewWindow_tlb = state.child_ObjViewWindow_tlb
       selXYZObjectROIParamName = state.selXYZObjectROIParamName
       fRefreshHistData = state.fRefreshHistData
       fUpDateROI2DGroup = state.fUpDateROI2DGroup
       fUpDateROI3DGroup = state.fUpDateROI3DGroup
    widget_control, wTopBase, set_uValue = state, /no_copy

    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos,$
                                                         selROIGroupObj = selROIGroupObj,$
                                                         ROIObjParamPos = ROIObjParamPos, selROIObjParam = selROIObjParam,$
                                                         ROIObjSubParamPos = ROIObjSubParamPos, selROIObjSubParam = selROIObjSubParam,$
                                                         oCurrROI3DGroup = oCurrROI3DGroup

    if fXObjHistControl then begin
       if not(widget_info(child_XDataHist_tlb, /valid)) then begin
          widget_control, wTopBase, tlb_Get_Offset = Offsets, tlb_Get_Size = tblSize
          s_HistSingleParamPlot_Window, stack_tlb = stack_tlb,$
                                        selROIGroupObj = selROIGroupObj,$
                                        ROIObjParamPos = ROIObjParamPos,$
                                        ROIObjSubParamPos = ROIObjSubParamPos,$
                                        selXYZObjectROIParamName = selXYZObjectROIParamName[0],$
                                        groupLeader = wTopBase, basePosition = [Offsets[0], Offsets[1]+tblSize[1]],$
                                        application_tlb = child_XDataHist_tlb, child_ObjViewWindow_tlb = child_ObjViewWindow_tlb
       endif else s_HistSingleParamPlot_Window_Update, hist_tlb = child_XDataHist_tlb,$
                                                       fKeepROI2DParamThreshs = fKeepROI2DParamThreshs,$
                                                       fKeepROI3DParamThreshs = fKeepROI3DParamThreshs,$
                                                       selROIGroupObj = selROIGroupObj,$
                                                       ROIObjParamPos = ROIObjParamPos,$
                                                       ROIObjSubParamPos = ROIObjSubParamPos,$
                                                       selXYZObjectROIParamName = selXYZObjectROIParamName[0]
    endif

    if fYObjHistControl then begin
       if not(widget_info(child_YDataHist_tlb, /valid)) then begin
           widget_control, wTopBase, tlb_Get_Offset = Offsets, tlb_Get_Size = tblSize
         s_HistSingleParamPlot_Window, stack_tlb = stack_tlb,$
                                       selROIGroupObj = selROIGroupObj,$
                                       ROIObjParamPos = ROIObjParamPos,$
                                       ROIObjSubParamPos = ROIObjSubParamPos,$
                                       selXYZObjectROIParamName = selXYZObjectROIParamName[1],$
                                       groupLeader = wTopBase, basePosition = [Offsets[0], Offsets[1]+tblSize[1]],$
                                       application_tlb = child_YDataHist_tlb, child_ObjViewWindow_tlb = child_ObjViewWindow_tlb
       endif else s_HistSingleParamPlot_Window_Update, hist_tlb = child_YDataHist_tlb,$
                                                                   fKeepROI2DParamThreshs = fKeepROI2DParamThreshs,$
                                                                   fKeepROI3DParamThreshs = fKeepROI3DParamThreshs,$
                                                                   selROIGroupObj = selROIGroupObj,$
                                                                   ROIObjParamPos = ROIObjParamPos,$
                                                                   ROIObjSubParamPos = ROIObjSubParamPos,$
                                                                   selXYZObjectROIParamName = selXYZObjectROIParamName[1]
    endif

    if fZObjHistControl then begin
       if not(widget_info(child_ZDataHist_tlb, /valid)) then begin
           widget_control, wTopBase, tlb_Get_Offset = Offsets, tlb_Get_Size = tblSize
         s_HistSingleParamPlot_Window, stack_tlb = stack_tlb,$
                                       selROIGroupObj = selROIGroupObj,$
                                       ROIObjParamPos = ROIObjParamPos,$
                                       ROIObjSubParamPos = ROIObjSubParamPos,$
                                       selXYZObjectROIParamName = selXYZObjectROIParamName[2],$
                                       groupLeader = wTopBase, basePosition = [Offsets[0], Offsets[1]+tblSize[1]],$
                                       application_tlb = child_ZDataHist_tlb, child_ObjViewWindow_tlb = child_ObjViewWindow_tlb
       endif else s_HistSingleParamPlot_Window_Update, hist_tlb = child_ZDataHist_tlb,$
                                                                  fKeepROI2DParamThreshs = fKeepROI2DParamThreshs,$
                                                                  fKeepROI3DParamThreshs = fKeepROI3DParamThreshs,$
                                                                  selROIGroupObj = selROIGroupObj,$
                                                                  ROIObjParamPos = ROIObjParamPos,$
                                                                  ROIObjSubParamPos = ROIObjSubParamPos,$
                                                                  selXYZObjectROIParamName = selXYZObjectROIParamName[2]
    endif

       ;initialize ObjViewWindow
    if not(widget_info(child_ObjViewWindow_tlb, /valid)) then begin
       fUpDateROI3DGroup = 1b
       fRefreshHistData = 1b
       s_ROIOM_UpdateCurrentROIPhaseModel, wTopBase = wTopBase, fUpDateROI3DGroup = fUpDateROI3DGroup
       widget_control, wTopBase, get_uValue = state, /no_copy
          stack_tlb = state.stack_tlb
          case selROIGroupObj of
             'oROI2DGroup':(*state.poCurrROI2DGroup)->setGroupObjColorsInParamValOrder, ROIObjParamPos = ROIObjParamPos, ROIObjSubParamPos = ROIObjSubParamPos
             'oROI3DGroup':(*state.poCurrROI3DGroup)->setGroupObjColorsInParamValOrder, ROIObjParamPos = ROIObjParamPos, ROIObjSubParamPos = ROIObjSubParamPos, stack_tlb = stack_tlb
          endcase
          s_objworld, init_oModel = *state.poCurrROIGraphicModel, application_TLB = child_ObjViewWindow_tlb, groupLeader = wTopBase, basePosition = [Offsets[0] + tblSize[0] - 15, tblSize[1] - tblSize[1] ]

       widget_control, wTopBase, set_uValue = state, /no_copy
       if widget_info(child_XDataHist_tlb, /valid) then widget_control, child_XDataHist_tlb, /show
       if widget_info(child_YDataHist_tlb, /valid) then widget_control, child_YDataHist_tlb, /show
       if widget_info(child_ZDataHist_tlb, /valid) then widget_control, child_ZDataHist_tlb, /show
       widget_control, wTopBase, /show
    endif

    if (fRefreshHistData and widget_info(child_ObjViewWindow_tlb, /valid)) then begin
       widget_control, wTopBase, get_uValue = state, /no_copy
       widget_control, child_ObjViewWindow_tlb, get_uValue = stateObjWin, /no_copy
   
       if (stateObjWin.strSelected ne 'CurrentTopModel selected') then eventStruct_2 = {id:stateObjWin.wDraw,$
                                                                                        top:state.child_ObjViewWindow_tlb,$
                                                                                        handler:state.child_ObjViewWindow_tlb,$
                                                                                        type:0,$
                                                                                        x:1, y:1,$
                                                                                        press:4b,$
                                                                                        release:0b,$
                                                                                        clicks:1b,$
                                                                                        modifiers:0l }

       widget_control, child_ObjViewWindow_tlb, set_uValue = stateObjWin, /no_copy
       widget_control, wTopBase, set_uValue = state, /no_copy
       if fUpDateROI2DGroup or fUpDateROI3DGroup then s_ROIOM_UpdateCurrentROIPhaseModel, wTopBase = wTopBase
       if (n_elements(eventStruct_2) ne 0) then s_objworldEvent, eventStruct_2
    endif

    widget_control, wTopBase, get_uValue = state, /no_copy
       if widget_info(child_XDataHist_tlb, /valid) then begin
         widget_control, child_XDataHist_tlb, get_uValue = stateHist, /no_copy
          stateHist.child_ObjViewWindow_tlb = child_ObjViewWindow_tlb
         widget_control, child_XDataHist_tlb, set_uValue = stateHist, /no_copy
         state.child_XDataHist_tlb = child_XDataHist_tlb
       endif
       if widget_info(child_YDataHist_tlb, /valid) then begin
         widget_control, child_YDataHist_tlb, get_uValue = stateHist, /no_copy
          stateHist.child_ObjViewWindow_tlb = child_ObjViewWindow_tlb
         widget_control, child_YDataHist_tlb, set_uValue = stateHist, /no_copy
         state.child_YDataHist_tlb = child_YDataHist_tlb
       endif
       if widget_info(child_ZDataHist_tlb, /valid) then begin
         widget_control, child_ZDataHist_tlb, get_uValue = stateHist, /no_copy
          stateHist.child_ObjViewWindow_tlb = child_ObjViewWindow_tlb
         widget_control, child_ZDataHist_tlb, set_uValue = stateHist, /no_copy
         state.child_ZDataHist_tlb = child_ZDataHist_tlb
       endif
       if widget_info(child_ObjViewWindow_tlb, /valid) then state.child_ObjViewWindow_tlb = child_ObjViewWindow_tlb

;     if widget_info(state.child_ParamWidget_tlb, /valid) then begin
;      o2DParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
;      tlb = state.child_ParamWidget_tlb
;      widget_control, wTopBase, set_uValue = state, /no_copy
;          s_ROIM_ParameterWidget_Refresh, tlb, oContainer = o2DParamContainer
;      widget_control, wTopBase, get_uValue = state, /no_copy
;     endif

       state.fRefreshHistData = fRefreshHistData

       widget_control, state.child_ObjViewWindow_tlb, get_uValue = stateObjView, /no_copy
          demo_draw, stateObjView.win, stateObjView.oScene, debug = stateObjView.debug
          s_objworld_update_wListObject, stateObjView
       widget_control, state.child_ObjViewWindow_tlb, set_uValue = stateObjView, /no_copy

    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROIOM_UpdateXYGroupParamPlotWin, wTopBase = wTopBase

    selXYZGroupROIParamName = s_ROIOM_getSelectedXYZGroupROIParamName(wTopBase = wTopBase)
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       fXYGroupDataPlotControl = state.fXYGroupDataPlotControl
       child_GroupDataPlot_tlb = state.child_GroupDataPlot_tlb
       child_ObjViewWindow_tlb = state.child_ObjViewWindow_tlb
       state.selXYZGroupROIParamName = selXYZGroupROIParamName
       fRefreshGroupData = state.fRefreshGroupData
       state.fRefreshGroupData = 0b
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos

    if fXYGroupDataPlotControl then begin
       if widget_info(child_GroupDataPlot_tlb, /valid) then begin
         if fRefreshGroupData then begin
            oStackTrackContainer = s_ROIOM_GetStackTrackContainer(wTopBase = wTopBase)
            s_GroupParamPlot_Window_Update, group_tlb = child_GroupDataPlot_tlb, selectedParamName = selXYZGroupROIParamName, stackPosition = stackPosition, oStackTrackContainer = oStackTrackContainer
         endif else s_GroupParamPlot_Window_Update, group_tlb = child_GroupDataPlot_tlb, selectedParamName = selXYZGroupROIParamName, tPos = tPos
       endif else begin
         oStackTrackContainer = s_ROIOM_GetStackTrackContainer(wTopBase = wTopBase)
           widget_control, wTopBase, tlb_Get_Offset = Offsets, tlb_Get_Size = tblSize
         s_GroupParamPlot_Window, oStackTrackContainer, stack_tlb = stack_tlb, tPos = tPos,$
                                  selectedParamName = selXYZGroupROIParamName,$
                                  groupLeader = wTopBase, basePosition = [Offsets[0] + 15, tblSize[1]],$
                                  application_tlb = child_GroupDataPlot_tlb, child_ObjViewWindow_tlb = child_ObjViewWindow_tlb
       endelse
    endif

    widget_control, wTopBase, get_uValue = state, /no_copy
       if widget_info(child_GroupDataPlot_tlb, /valid) then state.child_GroupDataPlot_tlb = child_GroupDataPlot_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROIOM_UpdateXYMixedParamPlotWin, wTopBase = wTopBase

    selXYZMixedROIParamName = s_ROIOM_getSelectedXYZMixedROIParamName(wTopBase = wTopBase)
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


pro s_ROIOM_UpdateXYZPlots, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, totalTNum = totalTNum, selROIGroupObj = selROIGroupObj
    widget_control, wTopBase, get_uValue = state, /no_copy
       case selROIGroupObj of
         'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
         'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
       endcase
       if ((state.fXObjHistControl + state.fYObjHistControl + state.fYObjHistControl) gt 0) then state.fRefreshHistData = 1b
       if widget_info(state.child_GroupDataPlot_tlb, /valid) then state.fRefreshGroupData = 1b
       if widget_info(state.child_MixedDataPlot_tlb, /valid) then state.fRefreshMixedData = 1b
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROIOM_UpdateXYZButtons, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, selROIGroupObj = selROIGroupObj,$
                                                         ROIObjParamPos = ROIObjParamPos, selROIObjParam = selROIObjParam,$
                                                         ROIObjSubParamPos = ROIObjSubParamPos, selROIObjSubParam = selROIObjSubParam

    selROIObjSubParam = strMid(selROIObjSubParam, 6)

    selXYZObjectROIParamSubName = s_ROIOM_getSelectedXYZObjectROIParamName(wTopBase = wTopBase)
    selXYZGroupROIParamSubName = s_ROIOM_getSelectedXYZGroupROIParamName(wTopBase = wTopBase)
    selXYZMixedROIParamSubName = s_ROIOM_getSelectedXYZMixedROIParamName(wTopBase = wTopBase)

    widget_control, wTopBase, get_uValue = state, /no_copy
       case selROIGroupObj of
         'oROI2DGroup':oROIParam = ((*state.poCurrROI2DGroup)->getParamContainer())->get(position = ROIObjParamPos)
         'oROI3DGroup':oROIParam = ((*state.poCurrROI3DGroup)->getParamContainer())->get(position = ROIObjParamPos)
       endcase

       if not(obj_valid(oROIParam)) then begin
         selROIObjParam = '-NO SELECTION-'
         selROIObjSubParam = '-NO SELECTION-'
       endif

       if ((strPos(selROIObjParam,'Object') ge 0) or (strPos(selROIObjParam,'Inter-Object') eq 0) or (selROIObjSubParam eq '-NO SELECTION-') ) then begin
         case state.fXYZObjectSensitive of
          0:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZObjectROIParamSubName[0]
                   widget_control, state.wXAxisObjectButton, sensitive = 0, set_value = 'X-Axis |-> ' + selROIObjSubParam
                   widget_control, state.wYAxisObjectButton, sensitive = 1
                   widget_control, state.wZAxisObjectButton, sensitive = 1
              endcase
          1:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZObjectROIParamSubName[1]
                   widget_control, state.wXAxisObjectButton, sensitive = 1
                   widget_control, state.wYAxisObjectButton, sensitive = 0, set_value = 'Y-Axis |-> ' + selROIObjSubParam
                   widget_control, state.wZAxisObjectButton, sensitive = 1
              endcase
          2:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZObjectROIParamSubName[2]
                   widget_control, state.wXAxisObjectButton, sensitive = 1
                   widget_control, state.wYAxisObjectButton, sensitive = 1
                   widget_control, state.wZAxisObjectButton, sensitive = 0, set_value = 'z-Axis |-> ' + selROIObjSubParam
              endcase
         endcase
       endif

       if (strPos(selROIObjParam,'Group') eq 0) then begin
         case state.fXYZGroupSensitive of
          0:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZGroupROIParamSubName[0]
                   widget_control, state.wXAxisGroupButton, sensitive = 0, set_value = 'X-Axis |-> ' + selROIObjSubParam
                   widget_control, state.wYAxisGroupButton, sensitive = 1
                   widget_control, state.wZAxisGroupButton, sensitive = 1
              endcase
          1:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZGroupROIParamSubName[1]
                   widget_control, state.wXAxisGroupButton, sensitive = 1
                   widget_control, state.wYAxisGroupButton, sensitive = 0, set_value = 'Y-Axis |-> ' + selROIObjSubParam
                   widget_control, state.wZAxisGroupButton, sensitive = 1
              endcase
          2:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZGroupROIParamSubName[1]
                   widget_control, state.wXAxisGroupButton, sensitive = 1
                   widget_control, state.wYAxisGroupButton, sensitive = 1
                   widget_control, state.wZAxisGroupButton, sensitive = 0, set_value = 'z-Axis |-> ' + selROIObjSubParam
              endcase
         endcase
       endif

       case state.fXYZMixedSensitive of
          0:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZGroupROIParamSubName[0]
                   widget_control, state.wXAxisMixedButton, sensitive = 0, set_value = 'X-Axis |-> ' + selROIObjSubParam
                   widget_control, state.wYAxisMixedButton, sensitive = 1
                   widget_control, state.wZAxisMixedButton, sensitive = 1
          endcase
          1:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZGroupROIParamSubName[1]
                   widget_control, state.wXAxisMixedButton, sensitive = 1
                   widget_control, state.wYAxisMixedButton, sensitive = 0, set_value = 'Y-Axis |-> ' + selROIObjSubParam
                   widget_control, state.wZAxisMixedButton, sensitive = 1
          endcase
          2:begin
                   if (n_elements(selROIObjSubParam) le 0) then selROIObjSubParam = selXYZGroupROIParamSubName[1]
                   widget_control, state.wXAxisMixedButton, sensitive = 1
                   widget_control, state.wYAxisMixedButton, sensitive = 1
                   widget_control, state.wZAxisMixedButton, sensitive = 0, set_value = 'z-Axis |-> ' + selROIObjSubParam
          endcase
       endcase

       fXYZHistOn = (state.fXObjHistControl + state.fYObjHistControl + state.fZObjHistControl) gt 0
       fXYGroupDataOn = state.fXYGroupDataPlotControl gt 0
       fXYMixedDataOn = state.fXYMixedDataPlotControl gt 0
       widget_control, wTopBase, set_uValue = state, /no_copy
       selXYZObjectROIParamName = s_ROIOM_getSelectedXYZObjectROIParamName(wTopBase = wTopBase)
       selXYZMixedROIParamName = s_ROIOM_getSelectedXYZMixedROIParamName(wTopBase = wTopBase)
       selXYZGroupROIParamName = s_ROIOM_getSelectedXYZGroupROIParamName(wTopBase = wTopBase)
       widget_control, wTopBase, get_uValue = state, /no_copy
       state.selXYZObjectROIParamName = selXYZObjectROIParamName
       state.selXYZMixedROIParamName = selXYZMixedROIParamName
       state.selXYZGroupROIParamName = selXYZMixedROIParamName
    widget_control, wTopBase, set_uValue = state, /no_copy

    if fXYZHistOn then s_ROIOM_UpdateXYZObjectPlotWin, wTopBase = wTopBase
    if fXYGroupDataOn then s_ROIOM_UpdateXYGroupParamPlotWin, wTopBase = wTopBase
    if fXYMixedDataOn then s_ROIOM_UpdateXYMixedParamPlotWin, wTopBase = wTopBase
end


pro s_ROI_ObjectManipulator_Window_Button_Event, ev
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
       'SAVEROIGROUP':begin
                        case selROIGroupObj of
                           'oROI2DGroup':begin
                              poDummy = state.poCurrROI2DGroup
                              widget_control, ev.top, set_uValue = state, /no_copy
                                *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
                              widget_control, ev.top, get_uValue = state, /no_copy
                              oROIGroup = *state.poCurrROI2DGroup
                              save, oROIGroup, filename = state.currROI2DGroupFileName
                            endcase
                           'oROI3DGroup':begin
                              poDummy = state.poCurrROI3DGroup
                              widget_control, ev.top, set_uValue = state, /no_copy
                                *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
                              widget_control, ev.top, get_uValue = state, /no_copy
                              oROIGroup = *state.poCurrROI3DGroup
                              save, oROIGroup, filename = state.currROI3DGroupFileName
                            endcase
                           else:
                        endcase
                        widget_control, ev.top, set_uValue = state, /no_copy
                           s_ROIOM_UpdateCurrentROIPhaseModel, wTopBase = ev.top, fUpDateROI3DGroup = 1
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'SAVEROIGROUPAS':begin
                        file = dialog_pickfile( /write, path = state.currROI2DGroupFileName, filter = '*.sav')
                        if (file ne '') then begin
                           if ( (strMid(file, strlen(file)-4, 4)) ne '.sav') then file = file + '.sav'
                           case selROIGroupObj of
                             'oROI2DGroup':begin
                                  poDummy = state.poCurrROI2DGroup
                                  widget_control, wTopBase, set_uValue = state, /no_copy
                                    *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
                                  widget_control, wTopBase, get_uValue = state, /no_copy
                                  state.currROI2DGroupFileName = file
                                  oROIGroup = *state.poCurrROI2DGroup
                                  save, oROIGroup, filename = state.currROI2DGroupFileName
                              endcase
                             'oROI3DGroup':begin
                                  poDummy = state.poCurrROI3DGroup
                                  widget_control, wTopBase, set_uValue = state, /no_copy
                                    *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
                                  widget_control, wTopBase, get_uValue = state, /no_copy
                                  state.currROI3DGroupFileName = file
                                  oROIGroup = *state.poCurrROI3DGroup
                                  save, oROIGroup, filename = state.currROI3DGroupFileName
                              endcase
                           endcase
                        endif
                        widget_control, ev.top, set_uValue = state, /no_copy
                           s_ROIOM_UpdateCurrentROIPhaseModel, wTopBase = ev.top, fUpDateROI3DGroup = 1
                        widget_control, ev.top, get_uValue = state, /no_copy
                        fUpdate = 0b
          endcase
       'SAVEROIPARAMSAS':begin
                        file = dialog_pickfile( /write, path = state.currROI2DGroupFileName, filter = '*.sav')
                        if (file ne '') then begin
                           if ( (strMid(file, strlen(file)-4, 4)) ne '.sav') then file = file + '.sav'
                           case selROIGroupObj of
                             'oROI2DGroup':begin
                                  o2DParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                                  save, o2DParamContainer, filename = file
                              endcase
                             'oROI3DGroup':begin
                                  o3DParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                                  save, o3DParamContainer, filename = file
                              endcase
                           endcase
                        endif
                        fUpdate = 0b
          endcase
       'OPENROIPARAMS':begin
                        file = dialog_pickfile( /read, path = state.currROI2DGroupFileName, filter = '*.sav')
                        if (file ne '') then begin
                           case selROIGroupObj of
                              'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                              'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                           endcase
                           for i = oParamContainer->count()-1, 0, -1  do begin
                              oParamContainer->remove, position = i
                              obj_destroy, (oParamContainer->get(position = i))
                           endfor
                           restore, file, restored_objects = oNewParamContainer, /relaxed
                           oNewParamContainer = oNewParamContainer[0]
                           case selROIGroupObj of
                              'oROI2DGroup':for i = 0, oNewParamContainer->count()-1 do oParamContainer->add, (oNewParamContainer->get(position = i))
                              'oROI3DGroup':for i = 0, oNewParamContainer->count()-1 do oParamContainer->add, (oNewParamContainer->get(position = i))
                           endcase
                        endif
        endcase
       'OPENROISTACKTRACK':begin
                        fileName = dialog_pickfile( /read, path = state.currROI2DGroupFileName, filter = '*.sav')
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
                           state.fRefreshGroupData = 1b
                           state.fRefreshMixedData = 1b
                           if (*state.poStackTrackContainer->getfAllParamControl() gt 0) then widget_control, state.wSaveStackTrackParamsAs, sensitive = 1 else widget_control, state.wSaveStackTrackParamsAs, sensitive = 0
                        endif
        endcase
       'SAVEROIROISTACKTRACKAS':begin
                        file = dialog_pickfile( /write, path = state.currROI2DGroupFileName, filter = '*.sav')
                        if (file ne '') then begin
                           oStackTrackContainer = *state.poStackTrackContainer
                           if ( (strMid(file, strlen(file)-4, 4)) ne '.sav') then file = file + '.sav'
                           save, oStackTrackContainer, filename = file
                           state.currROI2DGroupFileName = file
                        endif
                        fUpdate = 0b
        endcase
       'SAVEROIROISTACKTRACKPARAMSASASCII':begin
                        file = dialog_pickfile( /write, path = state.currROI2DGroupFileName, filter = '*.dat')
                        if (file ne '') then begin
                           oStackTrackContainer = *state.poStackTrackContainer

                           pActiveTrackParamNameList = oStackTrackContainer->getpActiveTrackParamNameList()
                           totalTNum = oStackTrackContainer->getTotalStackNumber()

                           if ((n_elements(*pActiveTrackParamNameList) ge 1) and (totalTNum ge 1)) then begin
                              pAllParams = ptrArr(n_elements(*pActiveTrackParamNameList))
                              for i = 0, n_elements(*pActiveTrackParamNameList)-1 do begin
                                 oStackTrack = oStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = (*pActiveTrackParamNameList)[i])
                                 if obj_valid(oStackTrack) then pAllParams[i] = ptr_new(oStackTrack->getAllParamVect(), /no_copy)

                                 openW,2, strCompress(file + strCompress( (*pActiveTrackParamNameList)[i] + '_rawData.dat',/rem))
                                 printF, 2, (*pActiveTrackParamNameList)[i]
                                 printF, 2, transpose(*(pAllParams)[i])
                                 close, 2
                              endfor
                           endif

                           for i = 0, n_elements(*pActiveTrackParamNameList)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
                        endif
                        fUpdate = 0b
        endcase
        ;---------------------- CEDAI-INICIO -------------------------
        'SAVEROIROISIZERELATIONASASCII':begin
             ;TODO SUSANA - FRAGMENTACION
             sortTrackParams = -1
             if obj_valid(*state.poStackTrackContainer) then begin
               
               ;Susana - informe de FRAGMENTACION  - INICIO
                 ;v_thrClassif = 0.9 ; umbral de razón BORDER/CORE para clasificacion en color, fragmentado y noFragmentado  - MomentsMorphology Normalizado
                 v_thrClassif = 3. ; 10x=10   20x=   umbral de razón BORDER/CORE para clasificacion en color, fragmentado y noFragmentado  - MomentsMorphology no normalizado
                 
                 widget_control, stack_tlb, get_uValue = stackState, /no_copy ; creo estructura stackState
                 tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
                 pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
                 nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; penultima carpeta cuando la ultima carpeta se llama stack
                 ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; ultima carpeta
                 name = 'CoordsVitROIs_'+nom
                 file = strCompress(tempFileName + name + '.dat', /remove)
                 get_lun, U
                 openW, U, file
                 ;;;printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xcore'+STRING(9B)+'Ycore'+STRING(9B)+'Zcore'+STRING(9B)+'Xborder'+STRING(9B)+'Yborder'+STRING(9B)+'Zborder'+STRING(9B)+strcompress('sizeBordeActualTime',/rem)+STRING(9B)+strcompress('sizeCoreActualTime',/rem)+STRING(9B)+strcompress('sizeBordeActualTime_div_sizeCoreActualTime',/rem)
                 ;;printF, U, 'tPos'+STRING(9B)+'clusPosBorder'+STRING(9B)+'nROIc'+STRING(9B)+'Xcore'+STRING(9B)+'Ycore'+STRING(9B)+'Zcore'+STRING(9B)+'Xborder'+STRING(9B)+'Yborder'+STRING(9B)+'Zborder'+STRING(9B)+strcompress('FirstMomentBordeActualTime',/rem)+STRING(9B)+strcompress('FirstMomentCoreActualTime',/rem)+STRING(9B)+strcompress('FirstMomentBordeActualTime_div_FirstMomentCoreActualTime',/rem)
                 ;printF, U, 'tPos'+STRING(9B)+'clusPosBorder'+STRING(9B)+'nROIc'+STRING(9B)+'Xcore'+STRING(9B)+'Ycore'+STRING(9B)+'Zcore'+STRING(9B)+'Xborder'+STRING(9B)+'Yborder'+STRING(9B)+'Zborder'+STRING(9B)+strcompress('FstMomentBordeTime',/rem)+STRING(9B)+strcompress('FstMomentCoreTime',/rem)+STRING(9B)+strcompress('FstMomentBorde_div_Core',/rem)
                 printF, U, 'tPosCoord'+STRING(9B)+'clusPosBorder'+STRING(9B)+'nROIcCoord'+STRING(9B)+'Xcore'+STRING(9B)+'Ycore'+STRING(9B)+'Zcore'+STRING(9B)+'Xborder'+STRING(9B)+'Yborder'+STRING(9B)+'Zborder'+STRING(9B)+strcompress('FstMomentBordeTime',/rem)+STRING(9B)+strcompress('FstMomentCoreTime',/rem)+STRING(9B)+strcompress('FstMomentBorde_div_Core',/rem)
                 
                 ;sizeParamsCore object_sizeX2 object_sizePorc
                 file2 = strCompress(tempFileName + name+'IGOR' + '.dat', /remove)
                 get_lun, UIgor
                 openW, UIgor, file2
                 ;printF, UIgor, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'FragmentadoNoFragmentado'+STRING(9B)+'sizeParamsBorder'+STRING(9B)+'object_sizeX2Border'+STRING(9B)+'object_sizePorcBorder'+STRING(9B)+'sizeParamsCore'+STRING(9B)+'object_sizeX2Core'+STRING(9B)+'object_sizePorcCore'
                 printF, UIgor, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'FragmentadoNoFragmentado'+STRING(9B)+'sizeParamsBorder'+STRING(9B)+'object_sizeX2Border'+STRING(9B)+'object_sizePorcBorder'+STRING(9B)+'sizeParamsCore'+STRING(9B)+'object_sizeX2Core'+STRING(9B)+'object_sizePorcCore'
                 ; - write raw trajectory to file I - BEGIN
                 ;save directory with stack info for saving images with results
                 tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
                 ;Susana - informe de FRAGMENTACION  - FIN
                 
                 
                 ;Susana - ClusterPosition for CORE and BORDER Sperms - BEGIN
                 (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                 
               stackName =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
               stackComments =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Comments'))[0]]
               
               xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
               yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
               xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
               yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
               xReal = xRe/xPixel
               yReal = yRe/yPixel
               
               controlCurt1 = 0
               controlCurt2 = 0
               numObjsBORDERT = 0 ; ALIVE TOTAL COUNTIN, ALL IMAGES
               numObjsCORET = 0 ; DIED TOTAL COUNTIN, ALL IMAGES
               
               widget_control, stack_tlb, set_uValue = stackState, /no_copy ; libero widget
               clusPosStrCORE = 2 ; CORE CLUSTER
               clusPosStrBORDER = 0 ; BORDER CLUSTER
               if (strlen(stackComments) gt 0) then begin
                 clusPosStrTEM = 2 ; CORE CLUSTER
                 clusPosStrCORE = strPos(stackComments, '_C', /reverse_search)
                 ;if (clusPosStrCORE ne -1) then clusPosStr = strMid(stackComments, clusPosStrCORE, strlen(stackComments)-strPos(stackComments, '_C', /reverse_search))
                 if (clusPosStrCORE ne -1) then clusPosStr = strMid(stackComments,(clusPosStrCORE+strlen('_C')),10)
                 ;clusPosStrCORE = fix(clusPosStrTEM)
                 ;clusPosStrCORE = fix(clusPosStr)
                 clusPosStrCORE = fix(1)
                 
                 clusPosStrTEM = 0 ; BORDER CLUSTER
                 clusPosStrBORDER = strPos(stackComments, '_B', /reverse_search)
                 ;if (clusPosStrBORDER ne -1) then clusPosStr = strMid(stackComments, clusPosStrBORDER, strlen(stackComments)-strPos(stackComments, '_B', /reverse_search))
                 if (clusPosStrBORDER ne -1) then clusPosStr = strMid(stackComments,(clusPosStrBORDER+strlen('_B')),10)
                 ;clusPosStrBORDER = fix(clusPosStrTEM)
                 ;clusPosStrBORDER = fix(clusPosStr)
                 clusPosStrBORDER = fix(0)
               endif
               ;Susana - ClusterPosition for CORE and BORDER Sperms - END
               
               
               ; Params for Core
               strIDCore = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPosStrCORE) + '_', /rem); Susana - aqui permitir realizar informe para zpos chpos actual
               case selROIGroupObj of
               'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Track Objects from Masks'); Susana - ¿track objects? el análisis de Caro ¿requiere Tracking? no tiene sentido creo
               'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '3D Track Objects from Masks')
               endcase
               
               ; find object order in trackParams
               if (n_elements(trackParams) ne 0) then begin
                  whNotMinusOne = where(trackParams ne -1, count)
                  if (count gt 0) then trackParams = trackParams[whNotMinusOne]
                  sortTrackParams = fix(trackParams[uniq(trackParams, sort(trackParams))]); ordena de menor a mayor, solo los no repetidos
               endif
               
                 xParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '2D x-center position')
                 if (count gt 0) then xParamsCore = xParamsCore[whNotMinusOne]
  
                 yParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '2D y-center position')
                 if (count gt 0) then yParamsCore = yParamsCore[whNotMinusOne]
  
                 zParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '2D z-center position')
                 if (count gt 0) then zParamsCore = zParamsCore[whNotMinusOne]

                 case selROIGroupObj of
                 'oROI2DGroup':tParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Time [s]')
                 'oROI3DGroup':tParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '3D Object Time [s]')
                 endcase
                 if (count gt 0) then tParamsCore = tParamsCore[whNotMinusOne] ;Susana - ¿falta un else  para protejer de error?
                 
                 sizeParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Size [Pixel²]')
                 if (count gt 0) then sizeParamsCore = sizeParamsCore[whNotMinusOne]
                 
                 object_sizeX2Core = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Size [x²]') ;object size [x²] paramVect * (xPixSize*yPixSize)
                 if (count gt 0) then object_sizeX2Core = object_sizeX2Core[whNotMinusOne]
                 
                 object_sizePorcCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Size [%]');object size [%] paramVect / (pixArea/100.)
                 if (count gt 0) then object_sizePorcCore = object_sizePorcCore[whNotMinusOne]
                 
                 
                 ;srta SUSANA FIX
                 ;FirstMomentParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Moment µ 02')
                 FirstMomentParamsCore = (*state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Moment µ 02') > *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Moment µ 20'))
                 if (count gt 0) then FirstMomentParamsCore = FirstMomentParamsCore[whNotMinusOne]

                 ; Create structure for save params required .... same dimensions than tParamCore...
                 parametroDeseado = tParamsCore * 0.0 ;Susana - Solo creo estructura de iguales dimensines, puedo reemplazar por makearray
                 
                 strIDBorde = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPosStrBORDER) + '_', /rem)
                 case selROIGroupObj of
                   'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Track Objects from Masks')
                   'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '3D Track Objects from Masks')
               endcase
                 
                  ; find object order in trackParams
               if (n_elements(trackParams) ne 0) then begin
                  whNotMinusOne = where(trackParams ne -1, count)
                  if (count gt 0) then trackParams = trackParams[whNotMinusOne]
                  sortTrackParams = fix(trackParams[uniq(trackParams, sort(trackParams))])
               endif
               
                 xParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '2D x-center position')
                 if (count gt 0) then xParamsBorde = xParamsBorde[whNotMinusOne]
  
                 yParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '2D y-center position')
                 if (count gt 0) then yParamsBorde = yParamsBorde[whNotMinusOne]
  
                 zParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '2D z-center position')
                 if (count gt 0) then zParamsBorde = zParamsBorde[whNotMinusOne]
               
                 case selROIGroupObj of
                 'oROI2DGroup':tParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Object Time [s]')
                 'oROI3DGroup':tParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '3D Object Time [s]')
                 endcase
                 if (count gt 0) then tParamsBorde = tParamsBorde[whNotMinusOne]
                 
                 sizeParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Object Size [Pixel²]')
                 if (count gt 0) then sizeParamsBorde = sizeParamsBorde[whNotMinusOne]
                 
                 object_sizeX2Border = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Object Size [x²]') ;object size [x²] paramVect * (xPixSize*yPixSize)
                 if (count gt 0) then object_sizeX2Border = object_sizeX2Border[whNotMinusOne]
                 
                 object_sizePorcBorder = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Object Size [%]');object size [%] paramVect / (pixArea/100.)
                 if (count gt 0) then object_sizePorcBorder = object_sizePorcBorder[whNotMinusOne]
                 
                 
                 ;FirstMomentParamsBorde = (*state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Moment µ 02') > *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Moment µ 20'))
                 FirstMomentParamsBorde = (*state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Moment µ 02') > *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Moment µ 20'))
                 if (count gt 0) then FirstMomentParamsBorde = FirstMomentParamsBorde[whNotMinusOne]
                 
                 ;FragNoFrag = make_array(n_elements(parametroDeseado),/STRING,VALUE = '')
                 FragNoFrag = make_array(n_elements(parametroDeseado),VALUE = 'NC')
                 clusPosStrBORDER_Class = make_array(n_elements(parametroDeseado),VALUE = -1) ; guardaré segun clasificacion
                       
               numObjsFragmentados = 0
               numObjsNonFragmentados = 0
               ; With all required params ... continue with calcs
               ; For all time
               tParamsCoreMin = min(tParamsCore, max = tParamsCoreMax)
               ;radius = 10 ; circle radius
               radius = 3 ; circle radius
               points = (2 * !PI /99.0 * FINDGEN(100)) ; used for draw circles
               
               for tiempoCoreActual = tParamsCoreMin, tParamsCoreMax do begin
                 ;verify CoreRois with actualTime
                 ; Susana - marcando imágenes - INICIO
                 countFrag = 0
                 countNonFrag = 0
                 
                 numObjsBORDER = 0 ; Susana - contador objetos BORDER
                 numObjsCORE = 0 ; Susana - contador objetos CORE
                 widget_control, stack_tlb, get_uValue = stackState, /no_copy ;  widget
                 image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = tiempoCoreActual, chPos = chPos, zPos = zPos) ; Susana - generando imágenes de informe
                 tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D; no funciona esto?
                 widget_control, stack_tlb, set_uValue = stackState, /no_copy ; libero widget
                 
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
                 sb = 50 ; scale bar, pixels
                 sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
                 pixsSB  = floor(1.0*(sb/xReal))
                 boxS = bytarr(3, pixsSB, 5)
                 boxS[0,*,*] = 0
                 boxS[1, *,*] = 0
                 boxS[2,*,*] = 0
                 position = [0.1, 0.1, 0.95, 0.95]
                 oPlot->GetProperty, XRange=xrange
                 oPlot->GetProperty, YRange=yrange
                 xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
                 ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
                 
                 oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
                 oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
                 oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
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
                 oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
                 xParamsROI_circle = 0.
                 yParamsROI_circle = 0.
                 ; Susana - marcando imágenes - FIN
                 
                 whActualCoreTime = where(tParamsCore eq tiempoCoreActual, countCore)
                 if (countCore gt 0) then begin ; There are elements with this value
                   xCoreActualTime    = xParamsCore[whActualCoreTime]
                   yCoreActualTime    = yParamsCore[whActualCoreTime]
                   zCoreActualTime    = zParamsCore[whActualCoreTime]
                   sizeCoreActualTime = sizeParamsCore[whActualCoreTime]
                   FirstMomentCoreActualTime = FirstMomentParamsCore[whActualCoreTime]
                   
                   whActualBordeTime = where(tParamsBorde eq tiempoCoreActual, countBorde)
                   if (countBorde gt 0) then begin ; There are elements with this value
                     xBordeActualTime    = xParamsBorde[whActualBordeTime]
                     yBordeActualTime    = yParamsBorde[whActualBordeTime]
                     zBordeActualTime    = zParamsBorde[whActualBordeTime]
                     sizeBordeActualTime = sizeParamsBorde[whActualBordeTime]
                     FirstMomentBordeActualTime = FirstMomentParamsBorde[whActualBordeTime]
                     
                     ; we have Rois for actual time in Core and Borde                 
                     ; for each CoreRoi .. we need to find the nearest Borde ROI
                     for selectedCoreRoi = 0, countCore-1 do begin
                       ; Calc distances
                       ; actual ROI position
                       xCoreSelected = xCoreActualTime[selectedCoreRoi]
                       yCoreSelected = yCoreActualTime[selectedCoreRoi]
                       
                       ; Distance for Axis ... to actual position
                       distanceX = xBordeActualTime - xCoreSelected
                       distanceY = yBordeActualTime - yCoreSelected
                       ; Calc Distance for ACtualCore ROI to Border Rois
                       distances = sqrt((distanceX*distanceX) + (distanceY*distanceY))
                       
                       ; Find MIN value---- SO.... the nearest element in the Borde
                       minDistance = min(distances,indiceBordeAtMINDISTANCE) ; Susana - indiceBordeAtMINDISTANCE posicion del elemento mínimo
                       
                       ; CALC the required relation of parasms between the CoreRoiSelected and the Nearest BordeROI
                       ;parametroDeseado[whActualCoreTime[selectedCoreRoi]] = sizeBordeActualTime[indiceBordeAtMINDISTANCE] / sizeCoreActualTime[selectedCoreRoi]
                       parametroDeseado[whActualCoreTime[selectedCoreRoi]] = FirstMomentBordeActualTime[indiceBordeAtMINDISTANCE] / FirstMomentCoreActualTime[selectedCoreRoi]; Moments of Morphology - First eigen values
                       if(minDistance ge 15) then parametroDeseado[whActualCoreTime[selectedCoreRoi]] = -1 ; Marco aquellos CORE no pareados
                       
                       ; Susana - Draw x and y circle coordinates - BEGIN
                       
                       
                       xParamsROI_circle = xCoreActualTime[selectedCoreRoi] + radius * cos(points)
                       ;xParamsROI_circle = xCoreActualTime[selectedCoreRoi] + sizeCoreActualTime[selectedCoreRoi]/30 * cos(points)
                       ;xParamsROI_circle = xCoreActualTime[selectedCoreRoi] + FirstMomentCoreActualTime[selectedCoreRoi] * cos(points)
                       
                       yParamsROI_circle = yCoreActualTime[selectedCoreRoi] + radius * sin(points)
                       ;yParamsROI_circle = yCoreActualTime[selectedCoreRoi] + sizeCoreActualTime[selectedCoreRoi]/30 * sin(points)
                       ;yParamsROI_circle = yCoreActualTime[selectedCoreRoi] + FirstMomentCoreActualTime[selectedCoreRoi] * sin(points)
                       
                       ;col = [0,255,0] ;BORDER, Verdes
                       col = [255,0,0] ;BORDER, RED, NonFragmented
                       ;col = [0,round(255-(255*parametroDeseado[whActualCoreTime[selectedCoreRoi]]/10)),0] ;BORDER, Verdes según razón
                       if((parametroDeseado[whActualCoreTime[selectedCoreRoi]] le v_thrClassif) and (parametroDeseado[whActualCoreTime[selectedCoreRoi]] gt -1)) then col = [0,255,0] ; GREEN Color Classification Fragmented
                       if(parametroDeseado[whActualCoreTime[selectedCoreRoi]] eq -1) then col = [0,0,255] ; CORE WITHOUT BORDER - BLUE
                       
                       
                       if ((n_elements(parametroDeseado) ne 0) and (parametroDeseado[whActualCoreTime[selectedCoreRoi]] ne -1)) then begin
                          ;whNotMinusOneFrag = where(parametroDeseado[whActualCoreTime] le v_thrClassif, countFrag)
                          whNotMinusOneFrag = where(parametroDeseado[whActualCoreTime[selectedCoreRoi]] le v_thrClassif, countFragTemp)
                          ;if (count gt 0) then FragNoFrag = parametroDeseado[whNotMinusOne]
                          ;if (countFragTemp gt 0) then FragNoFrag[whNotMinusOneFrag] = "Fragmentado"
                          if (countFragTemp gt 0) then begin
                           ;FragNoFrag[whNotMinusOneFrag] = "Fragmentado"
                           FragNoFrag[whActualCoreTime[selectedCoreRoi[whNotMinusOneFrag]]] = 'Fragmentado'
                           ;clusPosStrBORDER_Class[whNotMinusOneFrag] = clusPosStrBORDER;"Fragmentado"
                           clusPosStrBORDER_Class[whActualCoreTime[selectedCoreRoi[whNotMinusOneFrag]]] = clusPosStrBORDER;"Fragmentado"
                           countFrag += countFragTemp
                          endif
                          
                          ;if (countFragTemp gt 0) then clusPosStrBORDER_Class[whNotMinusOneFrag] = clusPosStrBORDER;"Fragmentado"
                         ; if(tiempoCoreActual eq 77)  then begin
                         ;   print,countFragTemp
                          ;endif
                          ;whNotMinusOneNonFrag = where(parametroDeseado[whActualCoreTime] gt v_thrClassif, countNonFrag)
                          whNotMinusOneNonFrag = where(parametroDeseado[whActualCoreTime[selectedCoreRoi]] gt v_thrClassif, countNonFragTemp)
                          ;if (countNonFragTemp gt 0) then FragNoFrag[whNotMinusOneNonFrag] = "NoFragmentado"
                          if (countNonFragTemp gt 0) then begin 
                            FragNoFrag[whActualCoreTime[selectedCoreRoi[whNotMinusOneNonFrag]]] = "NoFragmentado"
                            clusPosStrBORDER_Class[whActualCoreTime[selectedCoreRoi[whNotMinusOneNonFrag]]] = clusPosStrCORE;"NoFragmentado"
                            countNonFrag += countNonFragTemp
                          endif
                          
                          ;if (countNonFragTemp gt 0) then clusPosStrBORDER_Class[whNotMinusOneNonFrag] = clusPosStrCORE;"NoFragmentado"
                          ;if(tiempoCoreActual eq 77)  then begin
                          ;  print,countNonFragTemp
                          ;endif
                          ;sortTrackParams = fix(parametroDeseado[uniq(parametroDeseado, sort(parametroDeseado))])
                       endif
                       
                       
                       oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                       oModel->add, oPlot
                       
                       ;xParamsROI_circle = xBordeActualTime[indiceBordeAtMINDISTANCE] + sizeBordeActualTime[indiceBordeAtMINDISTANCE]/30 * cos(points) ; draw a circle
                       xParamsROI_circle = xBordeActualTime[indiceBordeAtMINDISTANCE] + FirstMomentBordeActualTime[indiceBordeAtMINDISTANCE] * cos(points) ; draw a circle
                       
                       ;yParamsROI_circle = yBordeActualTime[indiceBordeAtMINDISTANCE] + sizeBordeActualTime[indiceBordeAtMINDISTANCE]/30 * sin(points) ; draw a circle
                       yParamsROI_circle = yBordeActualTime[indiceBordeAtMINDISTANCE] + FirstMomentBordeActualTime[indiceBordeAtMINDISTANCE] * sin(points) ; draw a circle
                       ;;col = [255,0,0] ;BORDER, RED
                       ;col = [0,255,0] ;BORDER, GREEN este comentado
                       
                       ;;oPlot = obj_new('IDLgrText', STRINGS = ' '+string(parametroDeseado[whActualCoreTime[selectedCoreRoi]], format = '(F6.3)'),color = col, CHAR_DIMENSIONS = [20, 20], locations = [xCoreActualTime[selectedCoreRoi],yCoreActualTime[selectedCoreRoi],0], xcoord_conv = xs, ycoord_conv = ys)
                       ;oPlot = obj_new('IDLgrText', STRINGS = ' '+string(parametroDeseado[whActualCoreTime[selectedCoreRoi]], format = '(F6.3)')+'_R'+strcompress(string(selectedCoreRoi),/rem),color = col, CHAR_DIMENSIONS = [20, 20], locations = [xCoreActualTime[selectedCoreRoi],yCoreActualTime[selectedCoreRoi],0], xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                       oPlot = obj_new('IDLgrText', STRINGS = ' '+string(parametroDeseado[whActualCoreTime[selectedCoreRoi]], format = '(F6.1)')+'_R'+strcompress(string(selectedCoreRoi),/rem),color = col, CHAR_DIMENSIONS = [20, 20], locations = [xCoreActualTime[selectedCoreRoi],yCoreActualTime[selectedCoreRoi],0], xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
                       
                       oModel->add, oPlot
                       
                       ; - write raw trajectory to file II - BEGIN
                       ;;;temp = strcompress(string(tiempoCoreActual),/rem)+STRING(9B)+strcompress(string(clusPosStrBORDER),/rem)+STRING(9B)+strcompress(string(selectedCoreRoi),/rem)+STRING(9B)+strcompress(string(xParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(yParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(zParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(sizeBordeActualTime[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(sizeCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(parametroDeseado[whActualCoreTime[selectedCoreRoi]]),/rem)
                       ;;temp = strcompress(string(tiempoCoreActual),/rem)+STRING(9B)+strcompress(string(clusPosStrBORDER),/rem)+STRING(9B)+strcompress(string(selectedCoreRoi),/rem)+STRING(9B)+strcompress(string(xParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(yParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(zParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(xParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(yParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(zParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(sizeBordeActualTime[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(sizeCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(parametroDeseado[whActualCoreTime[selectedCoreRoi]]),/rem)
                       ;temp = strcompress(string(tiempoCoreActual),/rem)+STRING(9B)+strcompress(string(clusPosStrBORDER),/rem)+STRING(9B)+strcompress(string(selectedCoreRoi),/rem)+STRING(9B)+strcompress(string(xParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(yParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(zParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(xParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(yParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(zParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(FirstMomentBordeActualTime[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(FirstMomentCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(parametroDeseado[whActualCoreTime[selectedCoreRoi]]),/rem)
                       temp = strcompress(string(tiempoCoreActual),/rem)+STRING(9B)+strcompress(string(clusPosStrBORDER),/rem)+STRING(9B)+strcompress(string(selectedCoreRoi),/rem)+STRING(9B)+strcompress(string(xCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(yCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(zCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(xParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(yParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(zParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(FirstMomentBordeActualTime[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(FirstMomentCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(parametroDeseado[whActualCoreTime[selectedCoreRoi]]),/rem)
                       printF, U,temp
                       
                       ;temp2 = strcompress(string(tiempoCoreActual),/rem)+STRING(9B)+strcompress(string(clusPosStrBORDER_Class[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(selectedCoreRoi),/rem)+STRING(9B)+strcompress(string(xCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(yCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(zParamsCore[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(FragNoFrag[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(sizeParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizeX2Border[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizePorcBorder[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(sizeParamsCore[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizeX2Core[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizePorcCore[indiceBordeAtMINDISTANCE]),/rem)
                       temp2 = strcompress(string(tiempoCoreActual),/rem)+STRING(9B)+strcompress(string(clusPosStrBORDER_Class[whActualCoreTime[selectedCoreRoi]]),/rem)+STRING(9B)+strcompress(string(selectedCoreRoi),/rem)+STRING(9B)+strcompress(string(xCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(yCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(zCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(FragNoFrag[whActualCoreTime[selectedCoreRoi]]),/rem)+STRING(9B)+strcompress(string(sizeParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizeX2Border[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizePorcBorder[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(sizeParamsCore[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizeX2Core[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizePorcCore[indiceBordeAtMINDISTANCE]),/rem)
                       printF, UIgor,temp2
                       ; write raw trajectory to file II - END 
                       ; Susana - Draw x and y circle coordinates - END
                       
                       ; Susana - evaluar si aqui es buena posicion para guardar datos =======================================
                       numObjsBORDER = size(xParamsROI,/N_ELEMENTS)
                       numObjsBORDERT += numObjsBORDER
                       string_8 = strcompress(string(fix(numObjsBORDER)), /remove_all) + ' sperms'
                       string_BORDER = 'Recuento Cluster :'+strcompress(string(fix(clusPosStrBORDER)), /remove_all)+' BORDER  #='+string_8
                       ;print,string_BORDER+' time='+string(fix(tiempoCoreActual))
                       ;/*------------------------ informe  FIN
                       ;evaluando hasta aqui=======================================
                     endfor
                       oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
                       oView->add, oModel
                       oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
                       oBuffer->draw, oView      
                       oOImage = oBuffer->read()
                       oOImage->getProperty, data = outImage_1
                       write_tiff, tempFileName + 'Fragmentation_trajectoryT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff', outImage_1
                       
                       string_0 = 'Fragmentation Summary'+' Time '+string(round(tiempoCoreActual)); tiempoCoreActual
                       string_2 = strcompress('Fragmented'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
                       string_4 = strcompress('Total Counting'+STRING(9B)+': ')
                       ;string_441 = '# Fragmented   '+strcompress(': '+strcompress(string(numObjsBORDER, format = '(I6)'), /rem))
                       ;string_442 = '# Non-Fragmented   '+strcompress(': '+strcompress(string(numObjsCORE, format = '(I6)'), /rem))
                       ;string_9 = strcompress(string( (numObjsBORDER*100.)/(numObjsCORE+numObjsBORDER), format = '(D6.2)'), /rem)
                       ;string_11 = strcompress(string(numObjsCORE+numObjsBORDER, format = '(I6.2)'), /rem)
                       string_441 = '# Fragmented   '+strcompress(': '+strcompress(string(countFrag, format = '(I6)'), /rem))
                       string_442 = '# Non-Fragmented   '+strcompress(': '+strcompress(string(countNonFrag, format = '(I6)'), /rem))
                       string_9 = strcompress(string( (countFrag*100.)/(countNonFrag+countFrag), format = '(D6.2)'), /rem)
                       string_11 = strcompress(string(countNonFrag+countFrag, format = '(I6.2)'), /rem)
                       string_15 = strcompress('[%]', /rem)
                       string_16 = strcompress('[sperms]', /rem)
                       ;strcompress(string(tiempoCoreActual, format = '(I6.2)'), /rem)
                       fileName = tempFileName + 'Fragmentation_Text_ShortT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff'  
                       CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
                       ; Susana - 'Fragmentation_Text_Short.tiff'  FIN
                       
                       ; save results in the same directory as images - BEGIN
                       ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_Short.bmp'
                       background = s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_Short.bmp'
                       imageLeft =tempFileName + 'Fragmentation_trajectoryT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff'
                       fileName = tempFileName + 'Fragmentation_ShortT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff' 
                       textLeft = tempFileName + 'Fragmentation_Text_ShortT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff' 
                       
                       CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
                       oView->remove, oModel
                       
                       
                       numObjsFragmentados += countFrag
                       numObjsNonFragmentados += countNonFrag
                   endif
                 endif
               endfor
             endif
             ; - write raw trajectory to file IV - BEGIN
             close, U
             FREE_LUN, U
             
             close, UIgor
             FREE_LUN, UIgor
             
             print,' Total Frag   Total NonFrag   TotalSperms'
             print,numObjsFragmentados
             print,numObjsNonFragmentados
             print,numObjsFragmentados+numObjsNonFragmentados
             
             ; - write raw trajectory to file IV - END
             print, 'Fragmentation END'; Write data to file.... ""parametroDeseado & Time & other required data..."""
        endcase
        
;        'SAVEROIROISIZEROUNDCELLSASASCII':begin
;             
;             ;TODO SUSANA - ; CELULAS REDONDAS PROBANDO - FIN
;             sortTrackParams = -1
;             if obj_valid(*state.poStackTrackContainer) then begin
;                 v_thrClassif = 3. ; 10x=10   20x=   umbral de razón BORDER/CORE para clasificacion en color, fragmentado y noFragmentado  - MomentsMorphology no normalizado
;                 
;                 widget_control, stack_tlb, get_uValue = stackState, /no_copy ; creo estructura stackState
;                 tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
;                 pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
;                 nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; penultima carpeta cuando la ultima carpeta se llama stack
;                 ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; ultima carpeta
;                 name = 'CoordsRoundCellsROIs_'+nom
;                 file = strCompress(tempFileName + name + '.dat', /remove)
;                 get_lun, U
;                 openW, U, file
;                 printF, U, 'tPosCoord'+STRING(9B)+'clusPosBorder'+STRING(9B)+'nROIcCoord'+STRING(9B)+'Xcore'+STRING(9B)+'Ycore'+STRING(9B)+'Zcore'+STRING(9B)+'Xborder'+STRING(9B)+'Yborder'+STRING(9B)+'Zborder'+STRING(9B)+strcompress('FstMomentBordeTime',/rem)+STRING(9B)+strcompress('FstMomentCoreTime',/rem)+STRING(9B)+strcompress('FstMomentBorde_div_Core',/rem)
;                 
;                 ;sizeParamsCore object_sizeX2 object_sizePorc
;                 file2 = strCompress(tempFileName + name+'IGOR' + '.dat', /remove)
;                 get_lun, UIgor
;                 openW, UIgor, file2
;                 printF, UIgor, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'FragmentadoNoFragmentado'+STRING(9B)+'sizeParamsBorder'+STRING(9B)+'object_sizeX2Border'+STRING(9B)+'object_sizePorcBorder'+STRING(9B)+'sizeParamsCore'+STRING(9B)+'object_sizeX2Core'+STRING(9B)+'object_sizePorcCore'
;                 ; - write raw trajectory to file I - BEGIN
;                 ;save directory with stack info for saving images with results
;                 tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
;                 ;Susana - informe de FRAGMENTACION  - FIN
;                 
;                 
;                 ;Susana - ClusterPosition for CORE and BORDER ROUND CELLS - BEGIN
;                 (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
;                 
;               stackName =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
;               stackComments =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Comments'))[0]]
;               
;               xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
;               yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
;               xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
;               yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
;               xReal = xRe/xPixel
;               yReal = yRe/yPixel
;               
;               controlCurt1 = 0
;               controlCurt2 = 0
;               numObjsBORDERT = 0 ; ALIVE TOTAL COUNTIN, ALL IMAGES
;               numObjsCORET = 0 ; DIED TOTAL COUNTIN, ALL IMAGES
;               
;               widget_control, stack_tlb, set_uValue = stackState, /no_copy ; libero widget
;               clusPosStrCORE = 2 ; CORE CLUSTER
;               clusPosStrBORDER = 0 ; BORDER CLUSTER
;               if (strlen(stackComments) gt 0) then begin
;                 clusPosStrTEM = 2 ; CORE CLUSTER
;                 clusPosStrCORE = strPos(stackComments, '_C', /reverse_search)
;                 if (clusPosStrCORE ne -1) then clusPosStr = strMid(stackComments,(clusPosStrCORE+strlen('_C')),10)
;                 clusPosStrCORE = fix(1)
;                 
;                 clusPosStrTEM = 0 ; BORDER CLUSTER
;                 clusPosStrBORDER = strPos(stackComments, '_B', /reverse_search)
;                 if (clusPosStrBORDER ne -1) then clusPosStr = strMid(stackComments,(clusPosStrBORDER+strlen('_B')),10)
;                 clusPosStrBORDER = fix(0)
;               endif
;               ;Susana - ClusterPosition for CORE and BORDER ROUND CELLS - END
;               
;               clusPosStrCORE = 17 ; ROUND CELLS convolución
;               clusPosStrBORDE = 5 ; ROUND CELLS con filtros convencionales
;               ; Params for Core
;               strIDCore = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPosStrCORE) + '_', /rem); Susana - aqui permitir realizar informe para zpos chpos actual
;               case selROIGroupObj of
;               'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Track Objects from Masks'); Susana - ¿track objects? el análisis de Caro ¿requiere Tracking? no tiene sentido creo
;               'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '3D Track Objects from Masks')
;               endcase
;               
;               ; find object order in trackParams
;               if (n_elements(trackParams) ne 0) then begin
;                  whNotMinusOne = where(trackParams ne -1, count)
;                  if (count gt 0) then trackParams = trackParams[whNotMinusOne]
;                  sortTrackParams = fix(trackParams[uniq(trackParams, sort(trackParams))]); ordena de menor a mayor, solo los no repetidos
;               endif
;               
;                 xParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '2D x-center position')
;                 if (count gt 0) then xParamsCore = xParamsCore[whNotMinusOne]
;  
;                 yParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '2D y-center position')
;                 if (count gt 0) then yParamsCore = yParamsCore[whNotMinusOne]
;  
;                 zParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '2D z-center position')
;                 if (count gt 0) then zParamsCore = zParamsCore[whNotMinusOne]
;
;                 case selROIGroupObj of
;                 'oROI2DGroup':tParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Time [s]')
;                 'oROI3DGroup':tParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + '3D Object Time [s]')
;                 endcase
;                 if (count gt 0) then tParamsCore = tParamsCore[whNotMinusOne] ;Susana - ¿falta un else  para protejer de error?
;                 
;                 sizeParamsCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Size [Pixel²]')
;                 if (count gt 0) then sizeParamsCore = sizeParamsCore[whNotMinusOne]
;                 
;                 object_sizeX2Core = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Size [x²]') ;object size [x²] paramVect * (xPixSize*yPixSize)
;                 if (count gt 0) then object_sizeX2Core = object_sizeX2Core[whNotMinusOne]
;                 
;                 object_sizePorcCore = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Object Size [%]');object size [%] paramVect / (pixArea/100.)
;                 if (count gt 0) then object_sizePorcCore = object_sizePorcCore[whNotMinusOne]
;                 
;                 
;                 FirstMomentParamsCore = (*state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Moment µ 02') > *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Moment µ 20'))
;                 if (count gt 0) then FirstMomentParamsCore = FirstMomentParamsCore[whNotMinusOne]
;
;                 ; Create structure for save params required .... same dimensions than tParamCore...
;                 parametroDeseado = tParamsCore * 0.0 ;Susana - Solo creo estructura de iguales dimensines, puedo reemplazar por makearray
;                 
;;                 strIDBorde = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPosStrBORDER) + '_', /rem)
;;                 case selROIGroupObj of
;;                   'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Track Objects from Masks')
;;                   'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '3D Track Objects from Masks')
;;                 endcase
;;                 
;;                  ; find object order in trackParams
;;               if (n_elements(trackParams) ne 0) then begin
;;                  whNotMinusOne = where(trackParams ne -1, count)
;;                  if (count gt 0) then trackParams = trackParams[whNotMinusOne]
;;                  sortTrackParams = fix(trackParams[uniq(trackParams, sort(trackParams))])
;;               endif
;;               
;;                 xParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '2D x-center position')
;;                 if (count gt 0) then xParamsBorde = xParamsBorde[whNotMinusOne]
;;  
;;                 yParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '2D y-center position')
;;                 if (count gt 0) then yParamsBorde = yParamsBorde[whNotMinusOne]
;;  
;;                 zParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '2D z-center position')
;;                 if (count gt 0) then zParamsBorde = zParamsBorde[whNotMinusOne]
;;               
;;                 case selROIGroupObj of
;;                 'oROI2DGroup':tParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Object Time [s]')
;;                 'oROI3DGroup':tParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + '3D Object Time [s]')
;;                 endcase
;;                 if (count gt 0) then tParamsBorde = tParamsBorde[whNotMinusOne]
;;                 
;;                 sizeParamsBorde = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Object Size [Pixel²]')
;;                 if (count gt 0) then sizeParamsBorde = sizeParamsBorde[whNotMinusOne]
;;                 
;;                 object_sizeX2Border = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Object Size [x²]') ;object size [x²] paramVect * (xPixSize*yPixSize)
;;                 if (count gt 0) then object_sizeX2Border = object_sizeX2Border[whNotMinusOne]
;;                 
;;                 object_sizePorcBorder = *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Object Size [%]');object size [%] paramVect / (pixArea/100.)
;;                 if (count gt 0) then object_sizePorcBorder = object_sizePorcBorder[whNotMinusOne]
;;                 
;                 FirstMomentParamsBorde = (*state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Moment µ 02') < *state.poStackTrackContainer->getParamFromParamName(paramName = strIDCore + 'Moment µ 20'))
;                 ;FirstMomentParamsBorde = (*state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Moment µ 02') > *state.poStackTrackContainer->getParamFromParamName(paramName = strIDBorde + 'Moment µ 20'))
;                 if (count gt 0) then FirstMomentParamsBorde = FirstMomentParamsBorde[whNotMinusOne]
;                 
;                 FragNoFrag = make_array(n_elements(parametroDeseado),VALUE = 'NC')
;                 clusPosStrBORDER_Class = make_array(n_elements(parametroDeseado),VALUE = -1) ; guardaré segun clasificacion
;                       
;               numObjsFragmentados = 0
;               numObjsNonFragmentados = 0
;               ; With all required params ... continue with calcs
;               ; For all time
;               tParamsCoreMin = min(tParamsCore, max = tParamsCoreMax)
;               radius = 3 ; circle radius
;               points = (2 * !PI /99.0 * FINDGEN(100)) ; used for draw circles
;               
;               for tiempoCoreActual = tParamsCoreMin, tParamsCoreMax do begin
;                 ;verify CoreRois with actualTime
;                 ; Susana - marcando imágenes - INICIO
;                 countFrag = 0
;                 countNonFrag = 0
;                 
;                 ;numObjsBORDER = 0 ; Susana - contador objetos BORDER
;                 numObjsCORE = 0 ; Susana - contador objetos CORE
;                 widget_control, stack_tlb, get_uValue = stackState, /no_copy ;  widget
;                 image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = tiempoCoreActual, chPos = chPos, zPos = zPos) ; Susana - generando imágenes de informe
;                 tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D; no funciona esto?
;                 widget_control, stack_tlb, set_uValue = stackState, /no_copy ; libero widget
;                 
;                 image_size = size(image,/dim)
;                 image_x = image_size[0]
;                 image_y = image_size[1]
;                 yr = [0, max(image_y)]
;                 xr = [0, max(image_x)]
;                 k = 1
;                 y = .45
;                 xs = [k+45,y*2]
;                 ys = [k+45,y*2]
;                 
;                 oFont = obj_new('IDLgrFont','helvetica',size=14)
;                 oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
;                 oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
;                 oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
;                 oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
;                 oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
;                 oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
;                 oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
;                 oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)
;                 sb = 50 ; scale bar, pixels
;                 sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
;                 pixsSB  = floor(1.0*(sb/xReal))
;                 boxS = bytarr(3, pixsSB, 5)
;                 boxS[0,*,*] = 0
;                 boxS[1, *,*] = 0
;                 boxS[2,*,*] = 0
;                 position = [0.1, 0.1, 0.95, 0.95]
;                 oPlot->GetProperty, XRange=xrange
;                 oPlot->GetProperty, YRange=yrange
;                 xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
;                 ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
;                 
;                 oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
;                 oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
;                 oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
;                 oModel = obj_new('IDLgrModel')
;                 oModel->remove, /all
;                 oModel->add, oImage
;                 oModel->add, oPlot
;                 oModel->add, oXAxis
;                 oModel->add, oYAxis
;                 oModel->add, oYAxisUp
;                 oModel->add, oXAxisUp
;                 oModel->add, oImage_box
;                 oModel->add, oText_box
;                 ;oModel->add, oFont_box
;                 oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
;                 xParamsROI_circle = 0.
;                 yParamsROI_circle = 0.
;                 ; Susana - marcando imágenes - FIN
;                 
;                 whActualCoreTime = where(tParamsCore eq tiempoCoreActual, countCore)
;                 if (countCore gt 0) then begin ; There are elements with this value
;                   xCoreActualTime    = xParamsCore[whActualCoreTime]
;                   yCoreActualTime    = yParamsCore[whActualCoreTime]
;                   zCoreActualTime    = zParamsCore[whActualCoreTime]
;                   sizeCoreActualTime = sizeParamsCore[whActualCoreTime]
;                   FirstMomentCoreActualTime = FirstMomentParamsCore[whActualCoreTime]
;                   
;;                   whActualBordeTime = where(tParamsBorde eq tiempoCoreActual, countBorde)
;;                   if (countBorde gt 0) then begin ; There are elements with this value
;;                     xBordeActualTime    = xParamsBorde[whActualBordeTime]
;;                     yBordeActualTime    = yParamsBorde[whActualBordeTime]
;;                     zBordeActualTime    = zParamsBorde[whActualBordeTime]
;;                     sizeBordeActualTime = sizeParamsBorde[whActualBordeTime]
;;                     FirstMomentBordeActualTime = FirstMomentParamsBorde[whActualBordeTime]
;                     
;                     ; we have Rois for actual time in Core and Borde                 
;                     ; for each CoreRoi .. we need to find the nearest Borde ROI
;                     for selectedCoreRoi = 0, countCore-1 do begin
;                       ; Calc distances
;                       ; actual ROI position
;                       xCoreSelected = xCoreActualTime[selectedCoreRoi]
;                       yCoreSelected = yCoreActualTime[selectedCoreRoi]
;                       
;                       ; Distance for Axis ... to actual position
;;                       distanceX = xBordeActualTime - xCoreSelected
;;                       distanceY = yBordeActualTime - yCoreSelected
;;                       ; Calc Distance for ACtualCore ROI to Border Rois
;;                       distances = sqrt((distanceX*distanceX) + (distanceY*distanceY))
;;                       
;;                       ; Find MIN value---- SO.... the nearest element in the Borde
;;                       minDistance = min(distances,indiceBordeAtMINDISTANCE) ; Susana - indiceBordeAtMINDISTANCE posicion del elemento mínimo
;;                       
;;                       ; CALC the required relation of parasms between the CoreRoiSelected and the Nearest BordeROI
;;                       if(minDistance ge 15) then parametroDeseado[whActualCoreTime[selectedCoreRoi]] = -1 ; Marco aquellos CORE no pareados
;;                       
;;                       ; Susana - Draw x and y circle coordinates - BEGIN
;                       
;                       
;                       xParamsROI_circle = xCoreActualTime[selectedCoreRoi] + radius * cos(points)
;                       
;                       yParamsROI_circle = yCoreActualTime[selectedCoreRoi] + radius * sin(points)
;                       
;                       ;col = [0,255,0] ;BORDER, Verdes
;                       col = [255,0,0] ;BORDER, RED, NonFragmented
;                       ;col = [0,round(255-(255*parametroDeseado[whActualCoreTime[selectedCoreRoi]]/10)),0] ;BORDER, Verdes según razón
;;                       if((parametroDeseado[whActualCoreTime[selectedCoreRoi]] le v_thrClassif) and (parametroDeseado[whActualCoreTime[selectedCoreRoi]] gt -1)) then col = [0,255,0] ; GREEN Color Classification Fragmented
;;                       if(parametroDeseado[whActualCoreTime[selectedCoreRoi]] eq -1) then col = [0,0,255] ; CORE WITHOUT BORDER - BLUE
;;                       
;;                       
;;                       if ((n_elements(parametroDeseado) ne 0) and (parametroDeseado[whActualCoreTime[selectedCoreRoi]] ne -1)) then begin
;;                          whNotMinusOneFrag = where(parametroDeseado[whActualCoreTime[selectedCoreRoi]] le v_thrClassif, countFragTemp)
;;                          if (countFragTemp gt 0) then begin
;;                           FragNoFrag[whActualCoreTime[selectedCoreRoi[whNotMinusOneFrag]]] = 'Fragmentado'
;;                           clusPosStrBORDER_Class[whActualCoreTime[selectedCoreRoi[whNotMinusOneFrag]]] = clusPosStrBORDER;"Fragmentado"
;;                           countFrag += countFragTemp
;;                          endif
;;                          
;;                          whNotMinusOneNonFrag = where(parametroDeseado[whActualCoreTime[selectedCoreRoi]] gt v_thrClassif, countNonFragTemp)
;;                          if (countNonFragTemp gt 0) then begin 
;;                            FragNoFrag[whActualCoreTime[selectedCoreRoi[whNotMinusOneNonFrag]]] = "NoFragmentado"
;;                            clusPosStrBORDER_Class[whActualCoreTime[selectedCoreRoi[whNotMinusOneNonFrag]]] = clusPosStrCORE;"NoFragmentado"
;;                            countNonFrag += countNonFragTemp
;;                          endif
;;                       endif
;                       
;                       
;                       oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
;                       oModel->add, oPlot
;                       
;                       xParamsROI_circle = xBordeActualTime[indiceBordeAtMINDISTANCE] + FirstMomentBordeActualTime[indiceBordeAtMINDISTANCE] * cos(points) ; draw a circle
;                       
;                       yParamsROI_circle = yBordeActualTime[indiceBordeAtMINDISTANCE] + FirstMomentBordeActualTime[indiceBordeAtMINDISTANCE] * sin(points) ; draw a circle
;                       ;;col = [255,0,0] ;BORDER, RED
;                       ;col = [0,255,0] ;BORDER, GREEN este comentado
;                       
;                       oPlot = obj_new('IDLgrText', STRINGS = ' '+string(parametroDeseado[whActualCoreTime[selectedCoreRoi]], format = '(F6.1)')+'_R'+strcompress(string(selectedCoreRoi),/rem),color = col, CHAR_DIMENSIONS = [20, 20], locations = [xCoreActualTime[selectedCoreRoi],yCoreActualTime[selectedCoreRoi],0], xcoord_conv = xs, ycoord_conv = ys)  ; Susana - With ROI number
;                       
;                       oModel->add, oPlot
;                       
;                       ; - write raw trajectory to file II - BEGIN
;                       temp = strcompress(string(tiempoCoreActual),/rem)+STRING(9B)+strcompress(string(clusPosStrBORDER),/rem)+STRING(9B)+strcompress(string(selectedCoreRoi),/rem)+STRING(9B)+strcompress(string(xCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(yCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(zCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(xParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(yParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(zParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(FirstMomentBordeActualTime[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(FirstMomentCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(parametroDeseado[whActualCoreTime[selectedCoreRoi]]),/rem)
;                       printF, U,temp
;                       
;                       temp2 = strcompress(string(tiempoCoreActual),/rem)+STRING(9B)+strcompress(string(clusPosStrBORDER_Class[whActualCoreTime[selectedCoreRoi]]),/rem)+STRING(9B)+strcompress(string(selectedCoreRoi),/rem)+STRING(9B)+strcompress(string(xCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(yCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(zCoreActualTime[selectedCoreRoi]),/rem)+STRING(9B)+strcompress(string(FragNoFrag[whActualCoreTime[selectedCoreRoi]]),/rem)+STRING(9B)+strcompress(string(sizeParamsBorde[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizeX2Border[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizePorcBorder[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(sizeParamsCore[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizeX2Core[indiceBordeAtMINDISTANCE]),/rem)+STRING(9B)+strcompress(string(object_sizePorcCore[indiceBordeAtMINDISTANCE]),/rem)
;                       printF, UIgor,temp2
;                       ; write raw trajectory to file II - END 
;                       ; Susana - Draw x and y circle coordinates - END
;                       
;                       ; Susana - evaluar si aqui es buena posicion para guardar datos =======================================
;                       numObjsBORDER = size(xParamsROI,/N_ELEMENTS)
;                       numObjsBORDERT += numObjsBORDER
;                       string_8 = strcompress(string(fix(numObjsBORDER)), /remove_all) + ' ROUND CELLS'
;                       string_BORDER = 'Recuento Cluster :'+strcompress(string(fix(clusPosStrBORDER)), /remove_all)+' BORDER  #='+string_8
;                       ;print,string_BORDER+' time='+string(fix(tiempoCoreActual))
;                       ;/*------------------------ informe  FIN
;                       ;evaluando hasta aqui=======================================
;                     endfor
;                       oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
;                       oView->add, oModel
;                       oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
;                       oBuffer->draw, oView      
;                       oOImage = oBuffer->read()
;                       oOImage->getProperty, data = outImage_1
;                       write_tiff, tempFileName + 'ROUND_CELLS_trajectoryT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff', outImage_1
;                       
;                       string_0 = 'ROUND_CELLS Summary'+' Time '+string(round(tiempoCoreActual)); tiempoCoreActual
;                       string_2 = strcompress('Fragmented'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
;                       string_4 = strcompress('Total Counting'+STRING(9B)+': ')
;                       string_441 = '# Fragmented   '+strcompress(': '+strcompress(string(countFrag, format = '(I6)'), /rem))
;                       string_442 = '# Non-Fragmented   '+strcompress(': '+strcompress(string(countNonFrag, format = '(I6)'), /rem))
;                       string_9 = strcompress(string( (countFrag*100.)/(countNonFrag+countFrag), format = '(D6.2)'), /rem)
;                       string_11 = strcompress(string(countNonFrag+countFrag, format = '(I6.2)'), /rem)
;                       string_15 = strcompress('[%]', /rem)
;                       string_16 = strcompress('[ROUND CELLS]', /rem)
;                       fileName = tempFileName + 'ROUND_CELLS_Text_ShortT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff'  
;                       CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
;                       ; Susana - 'ROUND_CELLS_Text_Short.tiff'  FIN
;                       
;                       ; save results in the same directory as images - BEGIN
;                       background = s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_RoundCells_Short.bmp'
;                       imageLeft =tempFileName + 'ROUND_CELLS_trajectoryT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff'
;                       fileName = tempFileName + 'ROUND_CELLS_ShortT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff' 
;                       textLeft = tempFileName + 'ROUND_CELLS_Text_ShortT'+strcompress(string(tiempoCoreActual, format = '(I6.2)'),/REMOVE_ALL)+'.tiff' 
;                       
;                       CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
;                       oView->remove, oModel
;                       
;                       
;                       numObjsFragmentados += countFrag
;                       numObjsNonFragmentados += countNonFrag
;                   endif
;                 endif
;               endfor
;             endif
;             ; - write raw trajectory to file IV - BEGIN
;             close, U
;             FREE_LUN, U
;             
;             close, UIgor
;             FREE_LUN, UIgor
;             
;             print,' Total Frag   Total NonFrag   TotalSperms'
;             print,numObjsFragmentados
;             print,numObjsNonFragmentados
;             print,numObjsFragmentados+numObjsNonFragmentados
;             
;             ; - write raw trajectory to file IV - END
;             print, 'Fragmentation END'; Write data to file.... ""parametroDeseado & Time & other required data..."""
;             ; CELULAS REDONDAS PROBANDO - FIN
;        endcase
       'SAVEROIROISTACKTRACKPARAMOBJASINFOVITAL':begin ;'SAVEROIROISTACKTRACKPARAMOBJASASCII':begin
       ;Susana inventando INICIO -- informe de vitalidad
         sortTrackParams = -1
         if obj_valid(*state.poStackTrackContainer) then begin
;           s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
;                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
;                                              clusPos = clusPos, segPos = segPos, selROIGroupObj = selROIGroupObj, ROIObjParamPos = ROIObjParamPos,$
;                                              selROIObjSubParam = selROIObjSubParam, ROIObjSubParamPos = ROIObjSubParamPos
              
;         TODO alternative for Susana: compute the mass centers using the MoM parameters object              
;         nROIs = selROIGroupObj->count()
;         if nROIs gt 0 then begin
;           oMoMs = obj_new('C_sROIParam_ObjMomentsOfMorphology')
;           oMoMs->apply, C_SROIGROUPOBJ=selROIGroupObj, position = [14, 15]
;           res = oMoMs->getpValueStruct()
;           obj_destroy, oMoMs
;         endif
         
         widget_control, stack_tlb, get_uValue = stackState, /no_copy ; creo estructura stackState
         
         ; - write raw trajectory to file I - BEGIN
         ;save directory with stack info for saving images with results
         tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
         
         ; testName = last folder name
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; penultima carpeta cuando la ultima carpeta se llama stack
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; ultima carpeta
         name = 'CoordsVitROIs_'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, U
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         openW, U, file
         ;openW, U, file, /APPEND
         ;printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
         printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'VivoMuerto'
         ; write raw trajectory to file I - END
         
         image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
         ; MOR - 05Jul2010
         ; get real pixel info to convert the track object coordinates back to pixel coords for plotting results
         (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
         
         ;Susana - ClusterPosition for Alive and Dead Sperms - BEGIN
         stackName =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
         stackComments =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Comments'))[0]]
         if (strlen(stackComments) gt 0) then begin
          clusPosStr = 13
          vClusPosDead = strPos(stackComments, '_D', /reverse_search)
          if (vClusPosDead eq -1) then vClusPosDead = strPos(stackComments, '_D', /reverse_search)
          add = 2 ;add = strlen('_D')
          ;if (vClusPosDead ne -1) then timeStr = s_getLeftNumberFromStringAsString(strMid(stackComments, vClusPosDead+add))
          ;;if (vClusPosDead ne -1) then clusPosStr = strMid(stackComments, vClusPosDead+add,strPos(stackComments, '_D', /reverse_search))
          if (vClusPosDead ne -1) then clusPosStr = strMid(stackComments, vClusPosDead+add,strlen(stackComments)-strPos(stackComments, '_D', /reverse_search))
          vClusPosDead = fix(clusPosStr)
          
          clusPosStr = 11
          vClusPosAliv = strPos(stackComments, '_A', /reverse_search)
          if (vClusPosAliv eq -1) then vClusPosAliv = strPos(stackComments, '_A', /reverse_search)
          add = 2 ;add = strlen('_A')
          if (vClusPosAliv ne -1) then clusPosStr = strMid(stackComments, vClusPosAliv+add,(strPos(stackComments, '_D', /reverse_search)-strlen('_A') ) )
         endif
         vClusPosAliv = fix(clusPosStr)
         ;vClusPosAliv = 11 ;20x Clus11   10x Clus13
         ;vClusPosDead = 13 ;20x Clus13   10x Clus12
         vClusPosAliv = 12 ;20x Clus11   10x Clus13    ConvolEnRegiones=9
         vClusPosDead = 11 ;20x Clus13   10x Clus12   ConvolEnRegiones=12
         ;Susana - ClusterPosition for Alive and Dead Sperms - END
         
         xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
         yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
         xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
         yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
         xReal = xRe/xPixel
         yReal = yRe/yPixel
         
         ;Susana - control de inicio de datos x e y segun time -- INICIO
         ;Susana - asumo que vivos estan en Cluster11 y muertos en cluster13, y que estoy ejecutando menu desde cualquiera de esos dos clusters
         ;vClusPosAliv = 11 ; Vivos en Clus 11 20x
         strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosAliv) + '_', /rem)
;         case selROIGroupObj of
;            'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
;           ;'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
;         endcase
         t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
         
         radius = 4 ; circle radius
         points = (2 * !PI /99.0 * FINDGEN(100)) ; used for draw circles
         controlCurt1 = 0
         controlCurt2 = 0
         numObjsAliveT = 0 ; ALIVE TOTAL COUNTIN, ALL IMAGES
         numObjsDiedT = 0 ; DIED TOTAL COUNTIN, ALL IMAGES
          
         ;k = 0
         for vtime = 0,totalTNum-1 do begin ; viajo tiempo a tiempo
          numObjsAlive = 0
          numObjsDied = 0
          
          image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = vtime, chPos = chPos, zPos = zPos)
          ;MOR - save directory with stack info for saving images with results
          tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D; no funciona esto
          
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
          ;Susana - sb in micrometers - BEGIN
          sb = 50 ; sb in micrometers 
          sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
          pixsSB  = floor(1.0*(sb/xReal))
          boxS = bytarr(3, pixsSB, 5)
          boxS[0,*,*] = 0
          boxS[1, *,*] = 0
          boxS[2,*,*] = 0
          ;Susana - sb in micrometers - END
          
          position = [0.1, 0.1, 0.95, 0.95]
          oPlot->GetProperty, XRange=xrange
          oPlot->GetProperty, YRange=yrange
          xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
          ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
          
          oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
          oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
          oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
          ;IDLgrROI 
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
          oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
          
          ;======================================================================
          ;Susana - Alive sperms Cluster 11 -- BEGIN
          ;vClusPosAliv = 11 ; Vivos en Clus 11 20x
          strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosAliv) + '_', /rem)
          case selROIGroupObj of
            'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
            ;'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
          endcase
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          if(t[0] ne -1) then begin
            
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
            
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt1 = -1
            ENDIF ELSE BEGIN
              controlCurt1 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [0,255,0] ;Vivos, Verdes
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
                
                ; - write raw trajectory to file II - BEGIN
                ;printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
                ;;temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPos),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)
                ;temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPosAliv),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)
                temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPosAliv),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)+STRING(9B)+strcompress('Vivo',/rem)
                printF, U,temp
                ; write raw trajectory to file II - END 
              endfor
              
              numObjsAlive = size(xParamsROI,/N_ELEMENTS)
              numObjsAliveT += numObjsAlive
              string_8 = strcompress(string(fix(numObjsAlive)), /remove_all) + ' sperms'
              ;string_Alive = 'Recuento Cluster :'+strcompress(string(fix(clusPos)), /remove_all)+'  #='+string_8
              string_Alive = 'Recuento Cluster :'+strcompress(string(fix(vClusPosAliv)), /remove_all)+' Alive  #='+string_8
              print,string_Alive+' time='+string(fix(vtime))
            ENDELSE;IF ( curT gt 0)
          endif ;if(t[0] ne -1) begin
          ;Susana - Alive sperms Cluster 11 -- END
          
          ;Susana - Died sperms Cluster 13 -- BEGIN
          ;vClusPosDead = 13 ; Muertos en Clus 13 20x 
          strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosDead) + '_', /rem)
;          case selROIGroupObj of
;            'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
;            ;'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
;          endcase
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          ;if(t[0] ne -1) then begin  ; original con problema si no hay ROIs en primer time
          if(t[0] eq -1) and (n_elements(t)eq 1) then begin  ; no hay tiempos 
            ; nada
          endif else begin
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
           
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt2 = -1
            ENDIF ELSE BEGIN 
              controlCurt2 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [255,0,0] ;Muertos, Rojos
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
                
                ; - write raw trajectory to file II - BEGIN
                ;;printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
                ;temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPosDead),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)
                temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPosDead),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)+STRING(9B)+strcompress('Muerto',/rem)
                printF, U,temp
                ; write raw trajectory to file II - END 
              endfor
              numObjsDied = size(xParamsROI,/N_ELEMENTS)
              numObjsDiedT += numObjsDied
              string_8 = strcompress(string(fix(numObjsDied)), /remove_all) + ' sperms'
              string_Died = 'Recuento Cluster :'+strcompress(string(fix(vClusPosDead)), /remove_all)+' Died  #='+string_8
              print,string_Died+' time='+string(fix(vtime))
            ENDELSE;IF ( curT gt 0)
          ;endif ;if(t[0] ne -1) then begin  ; original con problema si no hay ROIs en primer time
          endelse
          
          ;Susana - Died sperms Cluster 13-- END 
          ;======================================================================
          
          IF ( (controlCurt1 eq 0) or (controlCurt2 eq 0)) THEN BEGIN 
            ;oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255]) ; Susana - comenta para ver foto completa
            oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
            oView->add, oModel
            oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_1
            write_tiff, tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
            
            ; Susana - 'Vitality_Text_Short.tiff'   PARA CADA IMAGEN  INICIO
            string_totalCounting = 'Total Counting'
            string_lived = '# Alive       '
            string_dead = '# Dead      '
            string_0 = 'Vitality Summary'+' Time '+string(vtime); vtime
            string_2 = strcompress('Alive'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
            ;string_4 = strcompress('Total Counting'+STRING(9B)+': ')
            string_4 = strcompress(string_totalCounting)
            string_441 = string_lived+strcompress(': '+strcompress(string(numObjsAlive, format = '(I6)'), /rem))
            string_442 = string_dead+strcompress(': '+strcompress(string(numObjsDied, format = '(I6)'), /rem))
            string_9 = strcompress(string( (numObjsAlive*100.)/(numObjsDied+numObjsAlive), format = '(D6.2)'), /rem)
            string_11 = strcompress(string(numObjsDied+numObjsAlive, format = '(I6.2)'), /rem)
            string_15 = strcompress('[%]', /rem)
            string_16 = strcompress('[sperms]', /rem)
            
            fileName = tempFileName + 'Vitality_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'  
            CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
            ; Susana - 'Vitality_Text_Short.tiff'  FIN
            
            ; MOR - save results in the same directory as images - BEGIN
            background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_Short.bmp'
            imageLeft =tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'
            fileName = tempFileName + 'Vitality_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            ; MOR - save results in the same directory as images - END
            textLeft = tempFileName + 'Vitality_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            
            CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
            oView->remove, oModel
          ENDIF
         endfor
         
         ; - write raw trajectory to file IV - BEGIN
         close, U
         FREE_LUN, U
         ; - write raw trajectory to file IV - END
         ;/*------------------------ informe  FIN
         
         ; SUMMARY REPORT - BEGIN
         ; Susana - SUMMARY REPORT WITHOUT IMAGE
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
         ;Susana - sb in micrometers - BEGIN
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - END
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange
         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
         
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         ;IDLgrROI 
         ; MOR - modified LB code for generating scale bar below the trajectory image - END         
                                   
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         ;oModel->add, oImage;comentado
         
         oModel->add, oPlot
         
         ;oModel->add, oXAxis;comentado
         ;oModel->add, oYAxis;comentado
         ;oModel->add, oYAxisUp;comentado
         ;oModel->add, oXAxisUp;comentado
         
         oModel->add, oImage_box
         oModel->add, oText_box
         
         ;oModel->add, oFont_box;comentado
         oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
         oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         ;write_tiff, tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
         
         
         ; Susana - 'Vitality_Text_Short.tiff'  INICIO
         string_0 = 'Vitality Summary'+' TOTAL '
         string_2 = strcompress('Alive'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
         ;string_4 = strcompress(string_totalCounting+STRING(9B)+': ')
         string_4 = strcompress(string_totalCounting)
         string_441 = string_lived+strcompress(': '+strcompress(string(numObjsAliveT, format = '(I6)'), /rem))
         string_442 = string_dead+strcompress(': '+strcompress(string(numObjsDiedT, format = '(I6)'), /rem))
         ;string_9 = strcompress(string( (numObjsAliveT*100)/(numObjsDiedT+numObjsAliveT), format = '(D6.2)'), /rem)
         string_9 = strcompress(string( (numObjsAliveT*1.)/(numObjsDiedT+numObjsAliveT)*100, format = '(D6.2)'), /rem)
         string_11 = strcompress(string(numObjsDiedT+numObjsAliveT, format = '(I6.2)'), /rem)
         string_15 = strcompress('[%]', /rem)
         string_16 = strcompress('[sperms]', /rem)
         
         fileName = tempFileName + 'Vitality_Text_ShortTOTAL'+'.tiff'  
         CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
         ; Susana - 'Vitality_Text_Short.tiff'  FIN
         
         
         ; MOR - save results in the same directory as images - BEGIN
         ;background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_ShortTotal.bmp'
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_TotalVitality_Short.bmp'
         ;imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VitalidadEspermatica_Zoom'+'.png'
         ;imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\VitalidadEspermatica_Zoom_phixr'+'.png'
         ;imageLeft = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\Vitality_trajectory4Image'+'.bmp'
         
         ;Susana - dibujo imagen representativa vitalidad INICIO

         ;Susana - dibujo imagen representativa vitalidad
         
         fileName = tempFileName + 'Vitality_ShortTOTAL'+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Vitality_Text_ShortTOTAL'+'.tiff'  
         
         ;CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
         CEDAI_combineImages_Vital, background,imageLeft, fileName, textLeft
         oView->remove, oModel
         ; SUMMARY REPORT - END
         
         
         ; Susana - Archivo de todos los stacks procesados en subcarpeta previa -- INICIO --
         ;Susana INICIO    Guardando Recuentos y Vivos y Muertos --------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom2 = strMid(tempFileName,pos[bslachs-3],lbSlashs[bslachs-3]) ; penultima carpeta
         ;nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ; ultimacarpeta
         name = 'DatosVitalidad_'+nom2
         
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
           printF, unitPCh,'Fecha'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#Alive'+STRING(9B)+'#Died'+STRING(9B)+'#Total'+STRING(9B)+'%Alive'+STRING(9B)+'%Died';
          ENDIF ELSE BEGIN
           close, unitPCh
           FREE_LUN, unitPCh
           openW, unitPCh, file,/APPEND
          ENDELSE
         ;Susana - si existe hago append, y si no creo archivo - INICIO
         ; Susana - Archivo de todos los stacks procesados en subcarpeta previa -- INICIO --
         
         ;porcentajes
         numObjsTotal = (numObjsDiedT+numObjsAliveT)
         numObjsAliveT_P = string((100.*numObjsAliveT / numObjsTotal), format = '(D6.2)') + ' %' ;(F6.2)
         numObjsDiedT_P = string((100.*numObjsDiedT / numObjsTotal), format = '(D6.2)') + ' %'
         
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         ;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoA),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoB),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoC),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoD),/REMOVE_ALL)+STRING(9B)+strcompress(string(tipoNC),/REMOVE_ALL)+STRING(9B)+strcompress(tipoAP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoBP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoCP,/REMOVE_ALL)+STRING(9B)+strcompress(tipoDP,/REMOVE_ALL);+STRING(10B) ;para IGOR
         temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(numObjsAliveT),/REMOVE_ALL)+STRING(9B)+strcompress(string(numObjsDiedT),/REMOVE_ALL)+STRING(9B)+strcompress(numObjsTotal,/REMOVE_ALL)+STRING(9B)+strcompress(numObjsAliveT_P,/REMOVE_ALL)+STRING(9B)+strcompress(numObjsDiedT_P,/REMOVE_ALL);+STRING(10B) ;para IGOR
         printF, unitPCh, temp
         close, unitPCh
         FREE_LUN, unitPCh
         ;Susana INICIO    Guardando Recuentos y Vivos y Muertos --------------
         
         
         widget_control, stack_tlb, set_uValue = stackState, /no_copy
         endif
         ;if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
       endcase
       
       'SAVEROIROISTACKTRACKPARAMOBJASINFOVITALSUPERSTACK':begin ;'SAVEROIROISTACKTRACKPARAMOBJASASCII':begin
       ;Susana inventando INICIO -- informe de vitalidad SuperStack
       ; A cada 30 imágenes (0 a 29) guarda los recuentos totales de vivos y muertos en archivo con idMuestra que corresponde solo a un número, debo después buscar equivalencia en otro archivo ...
         sortTrackParams = -1
         if obj_valid(*state.poStackTrackContainer) then begin
;           s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
;                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
;                                              clusPos = clusPos, segPos = segPos, selROIGroupObj = selROIGroupObj, ROIObjParamPos = ROIObjParamPos,$
;                                              selROIObjSubParam = selROIObjSubParam, ROIObjSubParamPos = ROIObjSubParamPos
              
;         TODO alternative for Susana: compute the mass centers using the MoM parameters object              
;         nROIs = selROIGroupObj->count()
;         if nROIs gt 0 then begin
;           oMoMs = obj_new('C_sROIParam_ObjMomentsOfMorphology')
;           oMoMs->apply, C_SROIGROUPOBJ=selROIGroupObj, position = [14, 15]
;           res = oMoMs->getpValueStruct()
;           obj_destroy, oMoMs
;         endif
         
         v_idMuestra = 0
         v_cuentaImagenes = 0
         
         widget_control, stack_tlb, get_uValue = stackState, /no_copy ; creo estructura stackState
         
         ; - write raw trajectory to file I - BEGIN
         ;save directory with stack info for saving images with results
         tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
         
         ; testName = last folder name
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; penultima carpeta cuando la ultima carpeta se llama stack
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; ultima carpeta
         name = 'CoordsVitROIs_'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, U
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         openW, U, file
         ;openW, U, file, /APPEND
         ;printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
         printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'+STRING(9B)+'VivoMuerto'
         ; write raw trajectory to file I - END
         
         image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
         ; MOR - 05Jul2010
         ; get real pixel info to convert the track object coordinates back to pixel coords for plotting results
         (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
         
         ;Susana - ClusterPosition for Alive and Dead Sperms - BEGIN
         stackName =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
         stackComments =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Comments'))[0]]
         if (strlen(stackComments) gt 0) then begin
          clusPosStr = 13
          vClusPosDead = strPos(stackComments, '_D', /reverse_search)
          if (vClusPosDead eq -1) then vClusPosDead = strPos(stackComments, '_D', /reverse_search)
          add = 2 ;add = strlen('_D')
          ;if (vClusPosDead ne -1) then timeStr = s_getLeftNumberFromStringAsString(strMid(stackComments, vClusPosDead+add))
          ;;if (vClusPosDead ne -1) then clusPosStr = strMid(stackComments, vClusPosDead+add,strPos(stackComments, '_D', /reverse_search))
          if (vClusPosDead ne -1) then clusPosStr = strMid(stackComments, vClusPosDead+add,strlen(stackComments)-strPos(stackComments, '_D', /reverse_search))
          vClusPosDead = fix(clusPosStr)
          
          clusPosStr = 11
          vClusPosAliv = strPos(stackComments, '_A', /reverse_search)
          if (vClusPosAliv eq -1) then vClusPosAliv = strPos(stackComments, '_A', /reverse_search)
          add = 2 ;add = strlen('_A')
          if (vClusPosAliv ne -1) then clusPosStr = strMid(stackComments, vClusPosAliv+add,(strPos(stackComments, '_D', /reverse_search)-strlen('_A') ) )
         endif
         vClusPosAliv = fix(clusPosStr)
         
         vClusPosAliv = 20 ;20x Clus11   10x Clus13    ConvolEnRegiones=9
         vClusPosDead = 19 ;20x Clus13   10x Clus12   ConvolEnRegiones=12
         ;Susana - ClusterPosition for Alive and Dead Sperms - END
         
         xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
         yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
         xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
         yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
         xReal = xRe/xPixel
         yReal = yRe/yPixel
         
         ;Susana - control de inicio de datos x e y segun time -- INICIO
         ;Susana - asumo que vivos estan en Cluster11 y muertos en cluster13, y que estoy ejecutando menu desde cualquiera de esos dos clusters
         ;vClusPosAliv = 11 ; Vivos en Clus 11 20x
         strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosAliv) + '_', /rem)
;         case selROIGroupObj of
;            'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
;           ;'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
;         endcase
         t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
         
         radius = 4 ; circle radius
         points = (2 * !PI /99.0 * FINDGEN(100)) ; used for draw circles
         controlCurt1 = 0
         controlCurt2 = 0
         numObjsAliveT = 0 ; ALIVE TOTAL COUNTIN, ALL IMAGES
         numObjsDiedT = 0 ; DIED TOTAL COUNTIN, ALL IMAGES
          
         ;k = 0
         for vtime = 0,totalTNum-1 do begin ; viajo tiempo a tiempo
          numObjsAlive = 0
          numObjsDied = 0
          
          image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = vtime, chPos = chPos, zPos = zPos)
          ;MOR - save directory with stack info for saving images with results
          tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D; no funciona esto
          
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
          ;Susana - sb in micrometers - BEGIN
          sb = 50 ; sb in micrometers 
          sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
          pixsSB  = floor(1.0*(sb/xReal))
          boxS = bytarr(3, pixsSB, 5)
          boxS[0,*,*] = 0
          boxS[1, *,*] = 0
          boxS[2,*,*] = 0
          ;Susana - sb in micrometers - END
          
          position = [0.1, 0.1, 0.95, 0.95]
          oPlot->GetProperty, XRange=xrange
          oPlot->GetProperty, YRange=yrange
          xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
          ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
          
          oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
          oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
          oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
          ;IDLgrROI 
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
          oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
          
          ;======================================================================
          ;Susana - Alive sperms Cluster 11 -- BEGIN
          ;vClusPosAliv = 11 ; Vivos en Clus 11 20x
          strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosAliv) + '_', /rem)
          case selROIGroupObj of
            'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
            ;'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
          endcase
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          if(t[0] ne -1) then begin
            v_cuentaImagenes = v_cuentaImagenes+1
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
            
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt1 = -1
            ENDIF ELSE BEGIN
              controlCurt1 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [0,255,0] ;Vivos, Verdes
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
                
                ; - write raw trajectory to file II - BEGIN
                temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPosAliv),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)+STRING(9B)+strcompress('Vivo',/rem)
                printF, U,temp
                ; write raw trajectory to file II - END 
              endfor
              
              numObjsAlive = size(xParamsROI,/N_ELEMENTS)
              numObjsAliveT += numObjsAlive
              string_8 = strcompress(string(fix(numObjsAlive)), /remove_all) + ' sperms'
              string_Alive = 'Recuento Cluster :'+strcompress(string(fix(vClusPosAliv)), /remove_all)+' Alive  #='+string_8
              print,string_Alive+' time='+string(fix(vtime))
            ENDELSE;IF ( curT gt 0)
          endif ;if(t[0] ne -1) begin
          ;Susana - Alive sperms Cluster 11 -- END
          
          ;Susana - Died sperms Cluster 13 -- BEGIN
          ;vClusPosDead = 13 ; Muertos en Clus 13 20x 
          strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosDead) + '_', /rem)
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          if(t[0] eq -1) and (n_elements(t)eq 1) then begin  ; no hay tiempos 
            ; nada
          endif else begin
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt2 = -1
            ENDIF ELSE BEGIN 
              controlCurt2 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [255,0,0] ;Muertos, Rojos
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
                
                ; - write raw trajectory to file II - BEGIN
                temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPosDead),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)+STRING(9B)+strcompress('Muerto',/rem)
                printF, U,temp
                ; write raw trajectory to file II - END 
              endfor
              numObjsDied = size(xParamsROI,/N_ELEMENTS)
              numObjsDiedT += numObjsDied
              string_8 = strcompress(string(fix(numObjsDied)), /remove_all) + ' sperms'
              string_Died = 'Recuento Cluster :'+strcompress(string(fix(vClusPosDead)), /remove_all)+' Died  #='+string_8
              print,string_Died+' time='+string(fix(vtime))
            ENDELSE;IF ( curT gt 0)
          ;endif ;if(t[0] ne -1) then begin  ; original con problema si no hay ROIs en primer time
          endelse
          
          ;Susana - Died sperms Cluster 13-- END 
          ;======================================================================
          
          IF ( (controlCurt1 eq 0) or (controlCurt2 eq 0)) THEN BEGIN 
            oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
            oView->add, oModel
            oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_1
            write_tiff, tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
            
            ; Susana - 'Vitality_Text_Short.tiff'   PARA CADA IMAGEN  INICIO
            string_totalCounting = 'Total Counting'
            string_lived = '# Alive       '
            string_dead = '# Dead      '
            string_0 = 'Vitality Summary'+' Time '+string(vtime); vtime
            string_2 = strcompress('Alive'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
            ;string_4 = strcompress('Total Counting'+STRING(9B)+': ')
            string_4 = strcompress(string_totalCounting)
            string_441 = string_lived+strcompress(': '+strcompress(string(numObjsAlive, format = '(I6)'), /rem))
            string_442 = string_dead+strcompress(': '+strcompress(string(numObjsDied, format = '(I6)'), /rem))
            string_9 = strcompress(string( (numObjsAlive*100.)/(numObjsDied+numObjsAlive), format = '(D6.2)'), /rem)
            string_11 = strcompress(string(numObjsDied+numObjsAlive, format = '(I6.2)'), /rem)
            string_15 = strcompress('[%]', /rem)
            string_16 = strcompress('[sperms]', /rem)
            
            fileName = tempFileName + 'Vitality_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'  
            CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
            ; Susana - 'Vitality_Text_Short.tiff'  FIN
            
            ; MOR - save results in the same directory as images - BEGIN
            background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_Short.bmp'
            imageLeft =tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'
            fileName = tempFileName + 'Vitality_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            ; MOR - save results in the same directory as images - END
            textLeft = tempFileName + 'Vitality_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            
            CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
            oView->remove, oModel
          ENDIF
         endfor
         
         ; - write raw trajectory to file IV - BEGIN
         close, U
         FREE_LUN, U
         ; - write raw trajectory to file IV - END
         ;/*------------------------ informe  FIN
         
         ; SUMMARY REPORT - BEGIN
         ; Susana - SUMMARY REPORT WITHOUT IMAGE
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
         ;Susana - sb in micrometers - BEGIN
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - END
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange
         xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
         
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         ;IDLgrROI 
         ; MOR - modified LB code for generating scale bar below the trajectory image - END         
                                   
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         oModel->add, oPlot
         oModel->add, oImage_box
         oModel->add, oText_box
         
         oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
         oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         ;write_tiff, tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
         
         
         ; Susana - 'Vitality_Text_Short.tiff'  INICIO
         string_0 = 'Vitality Summary'+' TOTAL '
         string_2 = strcompress('Alive'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
         string_4 = strcompress(string_totalCounting)
         string_441 = string_lived+strcompress(': '+strcompress(string(numObjsAliveT, format = '(I6)'), /rem))
         string_442 = string_dead+strcompress(': '+strcompress(string(numObjsDiedT, format = '(I6)'), /rem))
         string_9 = strcompress(string( (numObjsAliveT*1.)/(numObjsDiedT+numObjsAliveT)*100, format = '(D6.2)'), /rem)
         string_11 = strcompress(string(numObjsDiedT+numObjsAliveT, format = '(I6.2)'), /rem)
         string_15 = strcompress('[%]', /rem)
         string_16 = strcompress('[sperms]', /rem)
         
         fileName = tempFileName + 'Vitality_Text_ShortTOTAL'+'.tiff'  
         CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
         ; Susana - 'Vitality_Text_Short.tiff'  FIN
         
         
         ; MOR - save results in the same directory as images - BEGIN
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_TotalVitality_Short.bmp'
         ;Susana - dibujo imagen representativa vitalidad INICIO

         ;Susana - dibujo imagen representativa vitalidad
         
         fileName = tempFileName + 'Vitality_ShortTOTAL'+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Vitality_Text_ShortTOTAL'+'.tiff'  
         
         CEDAI_combineImages_Vital, background,imageLeft, fileName, textLeft
         oView->remove, oModel
         ; SUMMARY REPORT - END
         
         
         ; Susana - Archivo de todos los stacks procesados en subcarpeta previa -- INICIO --
         ;Susana INICIO    Guardando Recuentos y Vivos y Muertos --------------
         ;todos juntos en un mismo archivo, si se informamás de una vez un misma muestra basta hacer un ordenar en EXCEL por columnaA y luego por ColumnaB, se eliminann las más antiguas segun columna A 
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom2 = strMid(tempFileName,pos[bslachs-3],lbSlashs[bslachs-3]) ; penultima carpeta
         ;nom2 = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1])  ; ultimacarpeta
         
         ; Susana - guardo un archivo con recuentos de cada 30 imágens, es decir, el total para cada muestra -- INICIO
         if(v_cuentaImagenes eq 30) then begin ; desde  1 a 30
          name = 'DatosVitalidadSuperStack_'+nom2
          
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
            printF, unitPCh,'Fecha'+STRING(9B)+'InitTime'+STRING(9B)+'IdMuestra'+STRING(9B)+'zslice'+STRING(9B)+'#Alive'+STRING(9B)+'#Died'+STRING(9B)+'#Total'+STRING(9B)+'%Alive'+STRING(9B)+'%Died';
           ENDIF ELSE BEGIN
            close, unitPCh
            FREE_LUN, unitPCh
            openW, unitPCh, file,/APPEND
           ENDELSE
          ;Susana - si existe hago append, y si no creo archivo - INICIO
          ; Susana - Archivo de todos los stacks procesados en subcarpeta previa -- INICIO --
         
          ;porcentajes
          numObjsTotal = (numObjsDiedT+numObjsAliveT)
          numObjsAliveT_P = string((100.*numObjsAliveT / numObjsTotal), format = '(D6.2)') + ' %' ;(F6.2)
          numObjsDiedT_P = string((100.*numObjsDiedT / numObjsTotal), format = '(D6.2)') + ' %'
          
          s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
          ;temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(nom,/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(numObjsAliveT),/REMOVE_ALL)+STRING(9B)+strcompress(string(numObjsDiedT),/REMOVE_ALL)+STRING(9B)+strcompress(numObjsTotal,/REMOVE_ALL)+STRING(9B)+strcompress(numObjsAliveT_P,/REMOVE_ALL)+STRING(9B)+strcompress(numObjsDiedT_P,/REMOVE_ALL);+STRING(10B) ;para IGOR
          temp = strcompress(string(s_tiempo),/rem)+STRING(9B)+strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(v_idMuestra),/REMOVE_ALL)+STRING(9B)+strcompress(string(zPos),/REMOVE_ALL)+STRING(9B)+strcompress(string(numObjsAliveT),/REMOVE_ALL)+STRING(9B)+strcompress(string(numObjsDiedT),/REMOVE_ALL)+STRING(9B)+strcompress(numObjsTotal,/REMOVE_ALL)+STRING(9B)+strcompress(numObjsAliveT_P,/REMOVE_ALL)+STRING(9B)+strcompress(numObjsDiedT_P,/REMOVE_ALL);+STRING(10B) ;para IGOR
          
          printF, unitPCh, temp
          close, unitPCh
          FREE_LUN, unitPCh
          
          numObjsAliveT = 0
          numObjsDiedT = 0
          numObjsTotal = 0
          numObjsAliveT_P = 0
          numObjsDiedT_P = 0
          
          v_idMuestra = v_idMuestra+1
          ;Susana INICIO    Guardando Recuentos y Vivos y Muertos --------------
          
         end
         widget_control, stack_tlb, set_uValue = stackState, /no_copy
         endif
         ;if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
       endcase
       
       'SAVEROIROISTACKTRACKPARAMOBJASROIcorrds':begin
       ;basado en SAVEROIROISTACKTRACKPARAMOBJASINFOVITAL
       ;Guardo solo coordenadas de ROIs de todos los clusters ... para evaluar valor de convolucion
       ; guarda en archivo CoordsVitAllROIs_*.dat
       ;Un archivo por Cluster ¿con Modelo2D?, el archivo contiene todos los Times para ese cluster
       ;Susana inventando INICIO -- 
         sortTrackParams = -1
         if obj_valid(*state.poStackTrackContainer) then begin
;           s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
;                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
;                                              clusPos = clusPos, segPos = segPos, selROIGroupObj = selROIGroupObj, ROIObjParamPos = ROIObjParamPos,$
;                                              selROIObjSubParam = selROIObjSubParam, ROIObjSubParamPos = ROIObjSubParamPos
         
         widget_control, stack_tlb, get_uValue = stackState, /no_copy ; creo estructura stackState
         
         ; - write raw trajectory to file I - BEGIN
         ;save directory with stack info for saving images with results
         tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
         
         ; testName = last folder name
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; penultima carpeta cuando la ultima carpeta se llama stack
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; ultima carpeta
;         name = 'CoordsVit_AllROIs_'+nom
;         file = strCompress(tempFileName + name + '.dat', /remove)
;         get_lun, UAllROIs
;         openW, UAllROIs, file
;         printF, UAllROIs, 'tPos_AllROIs'+STRING(9B)+'clusPos_AllROIs'+STRING(9B)+'nROI_AllROIs'+STRING(9B)+'X_AllROIs'+STRING(9B)+'Y_AllROIs'+STRING(9B)+'Z_AllROIs'
;         close, UAllROIs
;         FREE_LUN, UAllROIs
         ; write raw trajectory to file I - END
         
         ; MOR - 05Jul2010
         ; get real pixel info to convert the track object coordinates back to pixel coords for plotting results
         (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
         oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
        ; widget_control, stack_tlb, set_uValue = stackState, /no_copy
         oImage->get, pParamStruct = pParamStruct
         totalClusNum = (oImage->getSegClusterNumber())-1
         widget_control, stack_tlb, set_uValue = stackState, /no_copy
         ;totalClusNum = ((*stackState.pImageStackInfoObject)->getSegClusterNumber())-1
         
;         ;Susana - ClusterPosition for Alive and Dead Sperms - BEGIN
;         stackName =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
;         stackComments =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Comments'))[0]]
;         xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
;         yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
;         xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
;         yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
;         xReal = xRe/xPixel
;         yReal = yRe/yPixel
         
        
         ;controlCurt2 = 0
         ;numObjsAliveT = 0 ; ALIVE TOTAL COUNTIN, ALL IMAGES
         ;numObjsDiedT = 0 ; DIED TOTAL COUNTIN, ALL IMAGES
         
         for vclusPos = 0, totalClusNum do begin
          
         
          ;Susana - control de inicio de datos x e y segun time -- INICIO
         ;Susana - asumo que vivos estan en Cluster11 y muertos en cluster13, y que estoy ejecutando menu desde cualquiera de esos dos clusters
         strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vclusPos) + '_', /rem)
         case selROIGroupObj of
            'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
           ;'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
         endcase
         t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
         
         if (n_elements(t) gt 0) then begin
;           name = 'CoordsVit_AllROIs_ClusPos'+strcompress(string(vclusPos),/rem)+'_'+nom
;           file = strCompress(tempFileName + name + '.dat', /remove)
;           get_lun, UAllROIs
;           openW, UAllROIs, file
;           printF, UAllROIs, 'tPos_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'clusPos_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'nROI_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'X_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'Y_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'Z_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'nROI_Clus'+strcompress(string(vclusPos),/rem)+'Total'
;           close, UAllROIs
;           FREE_LUN, UAllROIs
          ;contr
          v_cuentaROIs = 0
          for vtime = 0,totalTNum-1 do begin ; viajo tiempo a tiempo
           ;recuper estructura de ROIS Group
           C_sROI2DGroupObjActual = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb,tPos = vtime, chPos = chPos, zPos = zPos, clusPos = vclusPos)  ;, aqui falta filename)  al final
           numObjetosActual = C_sROI2DGroupObjActual->count()
           xyzDim = 1l * C_sROI2DGroupObjActual->getxyzDim()
           if ((v_cuentaROIs eq 0) and (numObjetosActual gt 0)) then begin
            name = 'CoordsVit_AllROIs_ClusPos'+strcompress(string(vclusPos),/rem)+'_'+nom
            file = strCompress(tempFileName + name + '.dat', /remove)
            get_lun, UAllROIs
            openW, UAllROIs, file
            printF, UAllROIs, 'tPos_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'clusPos_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'nROI_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'X_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'Y_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'Z_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'nROI_Clus'+strcompress(string(vclusPos),/rem)+'Total'
            close, UAllROIs
            FREE_LUN, UAllROIs
           endif
;           ;Alternativa 1
;           image = C_sROI2DGroupObjActual->getGroupMaskIntensity()
;           
;           xyzDim = 1l * size(image, /dim)
;           
;           ;Alt 2
;           xyzDim = [0L,0L]
;           xyzDim[0] = *(C_sROI2DGroupObjActual->getpParamStruct()).pValues[(where(*(C_sROI2DGroupObjActual->getpParamStruct()).pNames eq 'x-Size [pixel]'))[0]]
;           xyzDim[1] = *(C_sROI2DGroupObjActual->getpParamStruct()).pValues[(where(*(C_sROI2DGroupObjActual->getpParamStruct()).pNames eq 'y-Size [pixel]'))[0]]
            
           ;recorrer ROIs
           for j = 0L,(numObjetosActual-1L) do begin
             pPoints = *((C_sROI2DGroupObjActual->get(position = j))->getpWherePoints()) ;puntos de ROI j  indices no coordenadaws
             nPoints = n_elements(pPoints)
             
             ;construyo (traduzco) coordenadas en x e y
             xyzPoints = intArr(2, nPoints)
             xyzPoints[0,*] = pPoints mod xyzDim[0]            ;coordenadas en x
             xyzPoints[1,*] = floor( (pPoints mod (xyzDim[0] * xyzDim[1])) / (1.*xyzDim[0])) ;coordenadas en y
             
             xcoord = 0.
             ycoord = 0.
             xcoord=xyzPoints[0,*]  ; coordenadas x de todos los ROIs en time vtime
             ycoord=xyzPoints[1,*]  ; coordenadas y de todos los ROIs en time vtime
             ;if(vclusPos eq 14) then begin ;imagen completa, no es mascara, no me interesa
             ; print,(xyzDim[0]*xyzDim[1])
             ; print,nPoints
             ;endif
             if ((nPoints gt 0) and (nPoints lt (xyzDim[0]*xyzDim[1]) )) then begin ;nPoints? ¿452400?
            get_lun, UAllROIs
            OPENR, UAllROIs, file, ERROR = err  
            IF (err NE 0) THEN BEGIN
             openW, UAllROIs, file
             printF, UAllROIs, 'tPos_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'clusPos_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'nROI_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'X_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'Y_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'Z_Clus'+strcompress(string(vclusPos),/rem)+STRING(9B)+'nROI_Clus'+strcompress(string(vclusPos),/rem)+'Total'
            ENDIF ELSE BEGIN
             close, UAllROIs
             FREE_LUN, UAllROIs
             openW, UAllROIs, file,/APPEND
            ENDELSE
            
             for k = 0L, (nPoints-1L) do begin  ;pPoints
             ;for k = 0L, (n_elements(xcoord)-1L) do begin  ;pPoints
               ;evalúo o guardo las corrdenadas de cada ROI
               ;¿aqui filtro solo de un ROI  ?
               ;printF, U, 'tPos_AllROIs'+STRING(9B)+'clusPos_AllROIs'+STRING(9B)+'nROI_AllROIs'+STRING(9B)+'X_AllROIs'+STRING(9B)+'Y_AllROIs'+STRING(9B)+'Z_AllROIs'
               temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(strcompress(string(vclusPos),/rem),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xcoord[k]),/rem)+STRING(9B)+strcompress(string(ycoord[k]),/rem)+STRING(9B)+strcompress(string(0),/rem)+STRING(9B)+strcompress(string(v_cuentaROIs),/rem)
               printF,UAllROIs,temp
             endfor  ;de for k=0, pPoints-1 do begin
             close, UAllROIs
             FREE_LUN, UAllROIs
             v_cuentaROIs +=1
             endif
           endfor ;de for j = 0, numObjetosActual-1 do begin
           ;widget_control, stack_tlb, set_uValue = stackState, /no_copy
          endfor ; de  for vtime = 0,totalTNum-1 do begin
          print,'--ROIs en cluster '+string(vclusPos)+'---'
          print,v_cuentaROIs
          ;widget_control, stack_tlb, set_uValue = stackState, /no_copy
          endif
         endfor ;de cluster
         
         print,'END of - ALL ROIs Coords Files -'
         ; - write raw trajectory to file IV - BEGIN
         ;close, UAllROIs
        ; FREE_LUN, UAllROIs
         ; - write raw trajectory to file IV - END
         endif
         
       endcase
       
       'SAVEROIROISTACKTRACKPARAMOBJASINFOVITALMARCIA':begin ;'SAVEROIROISTACKTRACKPARAMOBJASASCII':begin
       ;Susana inventando INICIO -- informe pitados marcia
         sortTrackParams = -1
         if obj_valid(*state.poStackTrackContainer) then begin
;           s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
;                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
;                                              clusPos = clusPos, segPos = segPos, selROIGroupObj = selROIGroupObj, ROIObjParamPos = ROIObjParamPos,$
;                                              selROIObjSubParam = selROIObjSubParam, ROIObjSubParamPos = ROIObjSubParamPos
        
        widget_control, stack_tlb, get_uValue = stackState, /no_copy ; creo estructura stackState
         
         ; - write alive and dead count to file I - BEGIN
         ;save directory with stack info for saving images with results
         tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
         ; testName = last folder name
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; penultima carpeta
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; ultima carpeta
         ;nom = tempFileName ; ultima carpeta
         name = 'VitROIsMARCIA_10x_'+nom
         file10x = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, U10x
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         openW, U10x, file10x
         ;openW, U10x, file10x, /APPEND
         ;printF, U10x, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
         printF, U10x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'RctoMuertos'+STRING(9B)+'RctoVivos'+STRING(9B)+'PVitalidad' ;PVitalidad=porcentaje de vitalidad
         
         name = 'VitROIsMARCIA_20x_'+nom
         file20x = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, U20x
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         openW, U20x, file20x
         printF, U20x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'RctoMuertos'+STRING(9B)+'RctoVivos'+STRING(9B)+'PVitalidad' ;PVitalidad=porcentaje de vitalidad
         
         ; write alive and dead count to file I - END
         
         
         image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
         ; MOR - 05Jul2010
         ; get real pixel info to convert the track object coordinates back to pixel coords for plotting results
         (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
         
         xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
         yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
         xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
         yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
         xReal = xRe/xPixel
         yReal = yRe/yPixel
         
         ;Susana - control de inicio de datos x e y segun time -- INICIO
         ;Susana - vivos en Cluster0_&_Channel_001 y muertos en Cluster0_&_Channel_000, y que estoy ejecutando menu desde cualquiera de esos dos clusters
         vClusPosAliv=0 
         ;strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(0) + '_', /rem)
         strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(1) + '_', /rem) ;vivos en Cluster1_&_Channel_001 y muertos en Cluster1_&_Channel_000,
         t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
         
         radius = 4 ; circle radius
         points = (2 * !PI /99.0 * FINDGEN(100)) ; used for draw circles
         controlCurt1 = 0
         controlCurt2 = 0
         numObjsAliveT = 0 ; ALIVE TOTAL COUNTIN, ALL IMAGES
         numObjsDiedT = 0 ; DIED TOTAL COUNTIN, ALL IMAGES
          
         for vtime = 0,totalTNum-1 do begin ; viajo tiempo a tiempo
          numObjsAlive = 0
          numObjsDied = 0
          
          image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = vtime, chPos = chPos, zPos = zPos)
          ;MOR - save directory with stack info for saving images with results
          tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
          
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
          ;Susana - sb in micrometers - BEGIN
          sb = 50 ; sb in micrometers 
          sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
          pixsSB  = floor(1.0*(sb/xReal))
          boxS = bytarr(3, pixsSB, 5)
          boxS[0,*,*] = 0
          boxS[1, *,*] = 0
          boxS[2,*,*] = 0
          ;Susana - sb in micrometers - END
          
          position = [0.1, 0.1, 0.95, 0.95]
          oPlot->GetProperty, XRange=xrange
          oPlot->GetProperty, YRange=yrange
          xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
          ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
          
          oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
          oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
          oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
          ;IDLgrROI 
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
          oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
          
          
          
          
          
          ;======================================================================
          ;Susana - Alive sperms Cluster 11 -- BEGIN
          ;vivos en Cluster0_&_Channel_001 y muertos en Cluster0_&_Channel_000
          ;strID = strCompress('Ch' + string(1) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(0) + '_', /rem)
          strID = strCompress('Ch' + string(1) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(1) + '_', /rem) ;vivos en Cluster1_&_Channel_001 y muertos en Cluster1_&_Channel_000
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          if(t[0] ne -1) then begin          
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
            
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt1 = -1
            ENDIF ELSE BEGIN
              controlCurt1 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [0,255,0] ;Vivos, Verdes
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
              endfor
              
              numObjsAlive = size(xParamsROI,/N_ELEMENTS)
              numObjsAliveT += numObjsAlive
              string_8 = strcompress(string(fix(numObjsAlive)), /remove_all) + ' sperms'
              ;string_Alive = 'Recuento Cluster :'+strcompress(string(fix(clusPos)), /remove_all)+'  #='+string_8
              string_Alive = 'Recuento Cluster :'+strcompress(string(fix(vClusPosAliv)), /remove_all)+' Alive  #='+string_8
              print,string_Alive+' time='+string(fix(vtime))
              
              
              
              
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - BEGIN
              ;ejemplo de nombre :  4883_101_t000_ch01_z000.tif
              ;y recupero solo nombre inicial y objetivo 20x o 10x   desde el nombre imagen
              ;tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamNameList(tPos = tPos, chPos = chPos, zPos = zPos)).A
              tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = vtime, chPos = 0, zPos = 0)).A
              
              vExists = strPos(tempImageName, '_t', /reverse_search)
              if (vExists eq -1) then vExists = strPos(tempImageName, '_t', /reverse_search)
              add = 2 ;add = strlen('_t')
              ;if (vExists ne -1) then clusPosStr = strMid(stackComments, vExists+add,strlen(stackComments)-strPos(stackComments, '_t', /reverse_search))
              vExists = strPos(tempImageName, '_10', /reverse_search)
              ;if (vExists ne -1) then clusPosStr = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) ),(strPos(tempImageName, '_t', /reverse_search) )-(strPos(tempImageName, '_10', /reverse_search) ) )
              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) )+1,2 ) ; largo=2 pues sé que es 10 (10x) o 20 (20x)
              
              vExists = strPos(tempImageName, '_20', /reverse_search)
              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_20', /reverse_search) )+1,2 )
              pX = fix(nX)
              vExists = strPos(tempImageName, '_t', /reverse_search)
              ;sampleName = strMid(tempImageName, (strPos(tempImageName, '_'+nX, /reverse_search) )+1,(strPos(tempImageName, '_t', /reverse_search) ) )
              if (vExists ne -1) then sampleName = strMid(tempImageName, 0,(strPos(tempImageName, '_t', /reverse_search) ) )
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - END
            ENDELSE;IF ( curT gt 0)
          endif ;if(t[0] ne -1) begin
          ;Susana - Alive sperms Cluster 11 -- END
          
          ;Susana - Died sperms Cluster 13 -- BEGIN
          vClusPosDead = 0 ; Muertos en Clus 13 20x 
          ;muertos en Cluster0_&_Channel_000
          ;strID = strCompress('Ch' + string(0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(0) + '_', /rem)
          strID = strCompress('Ch' + string(0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(1) + '_', /rem) ;muertos en Cluster1_&_Channel_000
;          case selROIGroupObj of
;            'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
;            ;'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
;          endcase
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          if(t[0] ne -1) then begin
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
           
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt2 = -1
            ENDIF ELSE BEGIN 
              controlCurt2 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [255,0,0] ;Muertos, Rojos
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
              endfor
              numObjsDied = size(xParamsROI,/N_ELEMENTS)
              numObjsDiedT += numObjsDied
              string_8 = strcompress(string(fix(numObjsDied)), /remove_all) + ' sperms'
              string_Died = 'Recuento Cluster :'+strcompress(string(fix(vClusPosDead)), /remove_all)+' Died  #='+string_8
              print,string_Died+' time='+string(fix(vtime))
              
              
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - BEGIN
              ;ejemplo de nombre :  4883_101_t000_ch01_z000.tif
              ;y recupero solo nombre inicial y objetivo 20x o 10x   desde el nombre imagen
              ;tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamNameList(tPos = tPos, chPos = chPos, zPos = zPos)).A
              tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = vtime, chPos = 0, zPos = 0)).A
              
              vExists = strPos(tempImageName, '_t', /reverse_search)
              if (vExists eq -1) then vExists = strPos(tempImageName, '_t', /reverse_search)
              add = 2 ;add = strlen('_t')
              ;if (vExists ne -1) then clusPosStr = strMid(stackComments, vExists+add,strlen(stackComments)-strPos(stackComments, '_t', /reverse_search))
              vExists = strPos(tempImageName, '_10', /reverse_search)
              ;if (vExists ne -1) then clusPosStr = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) ),(strPos(tempImageName, '_t', /reverse_search) )-(strPos(tempImageName, '_10', /reverse_search) ) )
              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) )+1,2 ) ; largo=2 pues sé que es 10 (10x) o 20 (20x)
              
              vExists = strPos(tempImageName, '_20', /reverse_search)
              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_20', /reverse_search) )+1,2 )
              pX = fix(nX)
              vExists = strPos(tempImageName, '_t', /reverse_search)
              ;sampleName = strMid(tempImageName, (strPos(tempImageName, '_'+nX, /reverse_search) )+1,(strPos(tempImageName, '_t', /reverse_search) ) )
              if (vExists ne -1) then sampleName = strMid(tempImageName, 0,(strPos(tempImageName, '_t', /reverse_search) ) )
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - END
            ENDELSE;IF ( curT gt 0)
          endif ;if(t[0] ne -1) then begin
          ;Susana - Died sperms Cluster 13-- END 
          ;======================================================================
          
          IF ( (controlCurt1 eq 0) or (controlCurt2 eq 0)) THEN BEGIN 
            ;oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255]) ; Susana - comenta para ver foto completa
            oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
            oView->add, oModel
            oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_1
            ;write_tiff, tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
            write_tiff, tempFileName + strcompress('Trajectory_'+sampleName,/REMOVE_ALL)+'.tiff', outImage_1
            
            ; Susana - 'Vitality_Text_Short.tiff'  INICIO
            string_0 = 'Vitality Summary'+' Time '+string(vtime); vtime
            string_2 = strcompress('Alive'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
            string_4 = strcompress('Total Counting'+STRING(9B)+': ')
            string_441 = '# Alive   '+strcompress(': '+strcompress(string(numObjsAlive, format = '(I6)'), /rem))
            string_442 = '# Died   '+strcompress(': '+strcompress(string(numObjsDied, format = '(I6)'), /rem))
            string_9 = strcompress(string( (numObjsAlive*100.)/(numObjsDied+numObjsAlive), format = '(D6.2)'), /rem)
            string_11 = strcompress(string(numObjsDied+numObjsAlive, format = '(I6.2)'), /rem)
            string_15 = strcompress('[%]', /rem)
            string_16 = strcompress('[sperms]', /rem)
            
            ;fileName = tempFileName + 'Vitality_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'  
            fileName = tempFileName + strcompress('Text_Short_'+sampleName,/REMOVE_ALL)+'.tiff'
            CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
            ; Susana - 'Vitality_Text_Short.tiff'  FIN
            
            ; MOR - save results in the same directory as images - BEGIN
            background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_Short.bmp'
            ;imageLeft =tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'
            imageLeft =tempFileName + strcompress('Trajectory_'+sampleName,/REMOVE_ALL)+'.tiff'
            
            ;fileName = tempFileName + 'Vitality_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            fileName = tempFileName + strcompress(sampleName+'_Vitality',/REMOVE_ALL)+'.tiff'
            
            ; MOR - save results in the same directory as images - END
            ;textLeft = tempFileName + 'Vitality_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            textLeft = tempFileName + strcompress('Text_Short_'+sampleName,/REMOVE_ALL)+'.tiff'
            
            CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
            oView->remove, oModel
            
            
              ; - write alive and dead count to file II - BEGIN
              ;'IMAGE'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'Dead'+STRING(9B)+'Alive'+STRING(9B)+'Vitality'
              temp = strcompress(sampleName,/rem)+STRING(9B)+strcompress(string(nX),/rem)+STRING(9B)+strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(numObjsDied),/rem)+STRING(9B)+strcompress(string(numObjsAlive),/rem)+STRING(9B)+strcompress(string( (numObjsAlive*100.)/(numObjsDied+numObjsAlive), format = '(D6.2)'),/rem)
              CASE pX OF 
                 10: printF, U10x,temp
                 20: printF, U20x,temp
              ENDCASE 
              ; write alive and dead count to file II - END 
          ENDIF
         endfor
         
         ; - write alive and dead count to file IV - BEGIN
         close, U10x
         FREE_LUN, U10x
         
         close, U20x
         FREE_LUN, U20x
         ; - write alive and dead count to file IV - END
         ;/*------------------------ informe  FIN
         
         ; SUMMARY REPORT - BEGIN
         ; Susana - SUMMARY REPORT WITHOUT IMAGE
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         ;oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         ;oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         ;oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         ;oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         ;oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         ;oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - BEGIN
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - END
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange
         ;xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ;ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
         
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         ;IDLgrROI 
         ; MOR - modified LB code for generating scale bar below the trajectory image - END         
                                   
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         ;oModel->add, oImage
         oModel->add, oPlot
         ;oModel->add, oXAxis
         ;oModel->add, oYAxis
         ;oModel->add, oYAxisUp
         ;oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
         oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         ;write_tiff, tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
         
         ; Susana - 'Vitality_Text_Short.tiff'  INICIO
         string_0 = 'Vitality Summary'+' TOTAL '
         string_2 = strcompress('Alive'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
         string_4 = strcompress('Total Counting'+STRING(9B)+': ')
         string_441 = '# Alive   '+strcompress(': '+strcompress(string(numObjsAliveT, format = '(I6)'), /rem))
         string_442 = '# Died   '+strcompress(': '+strcompress(string(numObjsDiedT, format = '(I6)'), /rem))
         ;string_9 = strcompress(string( (numObjsAliveT*100)/(numObjsDiedT+numObjsAliveT), format = '(D6.2)'), /rem)
         string_9 = strcompress(string( (numObjsAliveT*1.)/(numObjsDiedT+numObjsAliveT)*100, format = '(D6.2)'), /rem)
         string_11 = strcompress(string(numObjsDiedT+numObjsAliveT, format = '(I6.2)'), /rem)
         string_15 = strcompress('[%]', /rem)
         string_16 = strcompress('[sperms]', /rem)
         
         fileName = tempFileName + 'Vitality_Text_ShortTOTAL'+'.tiff'  
         CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
         ; Susana - 'Vitality_Text_Short.tiff'  FIN
         
         ; MOR - save results in the same directory as images - BEGIN
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_Short.bmp'
         ;imageLeft =tempFileName + 'Vitality_trajectoryTOTAL'+'.tiff'; NO INCLUYO IMAGEN
         fileName = tempFileName + 'Vitality_ShortTOTAL'+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Vitality_Text_ShortTOTAL'+'.tiff'  
         
         ;CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
         CEDAI_combineImages_Vital, background, fileName, textLeft
         oView->remove, oModel
         ; SUMMARY REPORT - END
         
         widget_control, stack_tlb, set_uValue = stackState, /no_copy
         endif
         ;if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
       endcase
       
       'SAVEROIROISTACKTRACKPARAMOBJASINFOVITALMARCIA2':begin ;'SAVEROIROISTACKTRACKPARAMOBJASASCII':begin
       ;busca numero de muestra en nombre de carpeta anterior y no en nombre de imagenes
       ;Susana inventando INICIO -- informe pitados marcia
         sortTrackParams = -1
         if obj_valid(*state.poStackTrackContainer) then begin
;           s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
;                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
;                                              clusPos = clusPos, segPos = segPos, selROIGroupObj = selROIGroupObj, ROIObjParamPos = ROIObjParamPos,$
;                                              selROIObjSubParam = selROIObjSubParam, ROIObjSubParamPos = ROIObjSubParamPos
        
        widget_control, stack_tlb, get_uValue = stackState, /no_copy ; creo estructura stackState
         ; Susana EDITAR nuevo informe MARCIA2
         ; - write alive and dead count to file I - BEGIN
         ;save directory with stack info for saving images with results
         tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
         ; testName = last folder name
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         
         ; SUSANA, descomentar siguiente linea y comentar la subsiguiente
         nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; penultima carpeta  EN USO PARA 30 FOTOS ¡¡¡¡¡
         ;nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; ultima carpeta  TEMPORALMETNE HABILITADA
         
         ;nom = tempFileName ; ultima carpeta
;         name = 'VitROIsMARCIA_10x_'+nom
;         file10x = strCompress(tempFileName + name + '.dat', /remove)
;         get_lun, U10x
;         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
;         openW, U10x, file10x
;         ;openW, U10x, file10x, /APPEND
;         ;printF, U10x, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
;         printF, U10x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'RctoMuertos'+STRING(9B)+'RctoVivos'+STRING(9B)+'PVitalidad' ;PVitalidad=porcentaje de vitalidad
         
         ;NUEVO INICIO ---
         name = 'CoordsVitROIs_'+nom
         file = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, U
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         openW, U, file
         ;openW, U, file, /APPEND
         printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
         ; write raw trajectory to file I - END
         
         ;NUEVO FIN ---
         
         name = 'Vitality_Results_'+nom
         file20x = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, U20x
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         openW, U20x, file20x
         printF, U20x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'RctoMuertos'+STRING(9B)+'RctoVivos'+STRING(9B)+'PVitalidad' ;PVitalidad=porcentaje de vitalidad
         
         ; write alive and dead count to file I - END
         
         
         image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
         ; MOR - 05Jul2010
         ; get real pixel info to convert the track object coordinates back to pixel coords for plotting results
         (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
         
         ;Susana - ClusterPosition for Alive and Dead Sperms - BEGIN
         stackName =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
         stackComments =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Comments'))[0]]
         if (strlen(stackComments) gt 0) then begin
          clusPosStr = 13
          vClusPosDead = strPos(stackComments, '_D', /reverse_search)
          if (vClusPosDead eq -1) then vClusPosDead = strPos(stackComments, '_D', /reverse_search)
          add = 2 ;add = strlen('_D')
          if (vClusPosDead ne -1) then clusPosStr = strMid(stackComments, vClusPosDead+add,strlen(stackComments)-strPos(stackComments, '_D', /reverse_search))
          vClusPosDead = fix(clusPosStr)
          
          clusPosStr = 11
          vClusPosAliv = strPos(stackComments, '_A', /reverse_search)
          if (vClusPosAliv eq -1) then vClusPosAliv = strPos(stackComments, '_A', /reverse_search)
          add = 2 ;add = strlen('_A')
          if (vClusPosAliv ne -1) then clusPosStr = strMid(stackComments, vClusPosAliv+add,(strPos(stackComments, '_D', /reverse_search)-strlen('_A') ) )
         endif
         vClusPosAliv = fix(clusPosStr)
         ;vClusPosAliv = 11 ;20x Clus11   10x Clus13
         ;vClusPosDead = 13 ;20x Clus13   10x Clus12
         ;Susana - ClusterPosition for Alive and Dead Sperms - END

         xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
         yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
         xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
         yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
         xReal = xRe/xPixel
         yReal = yRe/yPixel
         
         ;Susana - control de inicio de datos x e y segun time -- INICIO
         ;Susana - vivos en Cluster0_&_Channel_001 y muertos en Cluster0_&_Channel_000, y que estoy ejecutando menu desde cualquiera de esos dos clusters
         ;strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(0) + '_', /rem)
         strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(1) + '_', /rem) ;vivos en Cluster1_&_Channel_001 y muertos en Cluster1_&_Channel_000,
         t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
         
         ;radius = 4 ; circle radius
         radius = 1.5 ; circle radius
         points = (2 * !PI /99.0 * FINDGEN(100)) ; used for draw circles
         controlCurt1 = 0
         controlCurt2 = 0
         numObjsAliveT = 0 ; ALIVE TOTAL COUNTIN, ALL IMAGES
         numObjsDiedT = 0 ; DIED TOTAL COUNTIN, ALL IMAGES
          
         for vtime = 0,totalTNum-1 do begin ; viajo tiempo a tiempo
          numObjsAlive = 0
          numObjsDied = 0
          
          image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = vtime, chPos = chPos, zPos = zPos)
          ;MOR - save directory with stack info for saving images with results
          tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
          
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
          ;Susana - sb in micrometers - BEGIN
          sb = 50 ; sb in micrometers 
          sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
          pixsSB  = floor(1.0*(sb/xReal))
          boxS = bytarr(3, pixsSB, 5)
          boxS[0,*,*] = 0
          boxS[1, *,*] = 0
          boxS[2,*,*] = 0
          ;Susana - sb in micrometers - END
          
          position = [0.1, 0.1, 0.95, 0.95]
          oPlot->GetProperty, XRange=xrange
          oPlot->GetProperty, YRange=yrange
          xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
          ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
          
          oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
          oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
          oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
          ;IDLgrROI 
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
          oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
          
          
          
          
          
          ;======================================================================
          ;Susana - Alive sperms Cluster 11 -- BEGIN
          ;vivos en Cluster0_&_Channel_001 y muertos en Cluster0_&_Channel_000
          ;strID = strCompress('Ch' + string(1) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(1) + '_', /rem) ;vivos en Cluster1_&_Channel_001 y muertos en Cluster1_&_Channel_000
          strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosAliv) + '_', /rem)

          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          ;if(t[0] ne -1) then begin ;Comentado Susana, si no encuentra ROI en primera imagen tendré un -1 en t[0] lo que no significa que tenga en las demás imágenes       
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
            
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt1 = -1
            ENDIF ELSE BEGIN
              controlCurt1 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
;              IF (vtime ge 21) THEN BEGIN
;                print,'chiii'
;              ENDIF
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [0,255,0] ;Vivos, Verdes
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
                
                ;NUEVO INICIO ---
                ; - write raw trajectory to file II - BEGIN
                ;printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
                ;temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPos),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)
                temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPosAliv),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)
                printF, U,temp
                ; write raw trajectory to file II - END 
                
                ;NUEVO FIN ---
              endfor
              
              numObjsAlive = size(xParamsROI,/N_ELEMENTS)
              numObjsAliveT += numObjsAlive
              string_8 = strcompress(string(fix(numObjsAlive)), /remove_all) + ' sperms'
              ;string_Alive = 'Recuento Cluster :'+strcompress(string(fix(clusPos)), /remove_all)+'  #='+string_8
              string_Alive = 'Recuento Cluster :'+strcompress(string(fix(vClusPosAliv)), /remove_all)+' Alive  #='+string_8
              print,string_Alive+' time='+string(fix(vtime))
              
              
              
              
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - BEGIN
              ;ejemplo de nombre :  4883_101_t000_ch01_z000.tif
              ;y recupero solo nombre inicial y objetivo 20x o 10x   desde el nombre imagen
              ;tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamNameList(tPos = tPos, chPos = chPos, zPos = zPos)).A
              tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = vtime, chPos = 0, zPos = 0)).A
              nX = 20  ;dejo fijo en 20x
              vExists = strPos(tempImageName, '_t', /reverse_search)
              if (vExists eq -1) then vExists = strPos(tempImageName, '_t', /reverse_search)
              add = 2 ;add = strlen('_t')
              ;if (vExists ne -1) then clusPosStr = strMid(stackComments, vExists+add,strlen(stackComments)-strPos(stackComments, '_t', /reverse_search))
              vExists = strPos(tempImageName, '_10', /reverse_search)
              ;if (vExists ne -1) then clusPosStr = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) ),(strPos(tempImageName, '_t', /reverse_search) )-(strPos(tempImageName, '_10', /reverse_search) ) )
              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) )+1,2 ) ; largo=2 pues sé que es 10 (10x) o 20 (20x)
              
              vExists = strPos(tempImageName, '_20', /reverse_search)
              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_20', /reverse_search) )+1,2 )
              pX = fix(nX)
              vExists = strPos(tempImageName, '_t', /reverse_search)
              ;sampleName = strMid(tempImageName, (strPos(tempImageName, '_'+nX, /reverse_search) )+1,(strPos(tempImageName, '_t', /reverse_search) ) )
              if (vExists ne -1) then sampleName = strMid(tempImageName, 0,(strPos(tempImageName, '_t', /reverse_search) ) )
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - END
            ENDELSE;IF ( curT gt 0)
          ;endif ;if(t[0] ne -1) begin ;Comentado Susana, si no encuentra ROI en primera imagen tendré un -1 en t[0] lo que no significa que tenga en las demás imágenes
          ;Susana - Alive sperms Cluster 11 -- END
          
          ;Susana - Died sperms Cluster 13 -- BEGIN
          ;muertos en Cluster0_&_Channel_000
          ;strID = strCompress('Ch' + string(0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(1) + '_', /rem) ;muertos en Cluster1_&_Channel_000
      strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosDead) + '_', /rem)
      case selROIGroupObj of
       'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
       'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
     endcase
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          ;if(t[0] ne -1) then begin ;Comentado Susana, si no encuentra ROI en primera imagen tendré un -1 en t[0] lo que no significa que tenga en las demás imágenes
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
           
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt2 = -1
            ENDIF ELSE BEGIN 
              controlCurt2 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
;              IF (vtime ge 21) THEN BEGIN
;                print,'chiii'
;              ENDIF
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [255,0,0] ;Muertos, Rojos
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
                
                ;NUEVO INICIO ---
                ; - write raw trajectory to file II - BEGIN
                ;printF, U, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
                temp = strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(vClusPosDead),/rem)+STRING(9B)+strcompress(string(j),/rem)+STRING(9B)+strcompress(string(xParamsROI[j]),/rem)+STRING(9B)+strcompress(string(yParamsROI[j]),/rem)+STRING(9B)+strcompress(string(0),/rem)
                printF, U,temp
                ; write raw trajectory to file II - END 
                ;NUEVO FIN ---
                
              endfor
              numObjsDied = size(xParamsROI,/N_ELEMENTS)
              numObjsDiedT += numObjsDied
              string_8 = strcompress(string(fix(numObjsDied)), /remove_all) + ' sperms'
              string_Died = 'Recuento Cluster :'+strcompress(string(fix(vClusPosDead)), /remove_all)+' Died  #='+string_8
              print,string_Died+' time='+string(fix(vtime))
              
              
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - BEGIN
              ;ejemplo de nombre :  4883_101_t000_ch01_z000.tif
              ;y recupero solo nombre inicial y objetivo 20x o 10x   desde el nombre imagen
              ;tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamNameList(tPos = tPos, chPos = chPos, zPos = zPos)).A
              tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = vtime, chPos = 0, zPos = 0)).A
              
              ;vExists = strPos(tempImageName, '_t', /reverse_search)
              ;if (vExists eq -1) then vExists = strPos(tempImageName, '_t', /reverse_search)
              ;add = 2 ;add = strlen('_t')
              ;vExists = strPos(tempImageName, '_10', /reverse_search)
              ;if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) )+1,2 ) ; largo=2 pues sé que es 10 (10x) o 20 (20x)
              
              ;vExists = strPos(tempImageName, '_20', /reverse_search)
              ;if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_20', /reverse_search) )+1,2 )
              nX = 20  ;dejo fijo en 20x
              pX = fix(nX)
              vExists = strPos(tempImageName, '_t', /reverse_search)
              if (vExists ne -1) then sampleName = strMid(tempImageName, 0,(strPos(tempImageName, '_t', /reverse_search) ) )
              ;sampleName = nom
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - END
            ENDELSE;IF ( curT gt 0)
          ;endif ;if(t[0] ne -1) then begin ;Comentado Susana, si no encuentra ROI en primera imagen tendré un -1 en t[0] lo que no significa que tenga en las demás imágenes
          ;Susana - Died sperms Cluster 13-- END 
          ;======================================================================
          
          IF ( (controlCurt1 eq 0) or (controlCurt2 eq 0)) THEN BEGIN 
            ;oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255]) ; Susana - comenta para ver foto completa
            oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
            oView->add, oModel
            oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_1
            ;write_tiff, tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
            write_tiff, tempFileName + strcompress('Trajectory_'+nom+sampleName,/REMOVE_ALL)+'.tiff', outImage_1
            
            ; Susana - 'Vitality_Text_Short.tiff'  INICIO
            string_0 = 'Vitality Summary'+' Time '+string(vtime); vtime
            string_2 = strcompress('Alive'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
            string_4 = strcompress('Total Counting'+STRING(9B)+': ')
            string_441 = '# Alive   '+strcompress(': '+strcompress(string(numObjsAlive, format = '(I6)'), /rem))
            string_442 = '# Died   '+strcompress(': '+strcompress(string(numObjsDied, format = '(I6)'), /rem))
            string_9 = strcompress(string( (numObjsAlive*100.)/(numObjsDied+numObjsAlive), format = '(D6.2)'), /rem)
            string_11 = strcompress(string(numObjsDied+numObjsAlive, format = '(I6.2)'), /rem)
            string_15 = strcompress('[%]', /rem)
            string_16 = strcompress('[sperms]', /rem)
            
            ;fileName = tempFileName + 'Vitality_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'  
            fileName = tempFileName + strcompress('Text_Short_'+nom+sampleName,/REMOVE_ALL)+'.tiff'
            CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
            ; Susana - 'Vitality_Text_Short.tiff'  FIN
            
            ; MOR - save results in the same directory as images - BEGIN
            background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_Short.bmp'
            ;imageLeft =tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'
            imageLeft =tempFileName + strcompress('Trajectory_'+nom+sampleName,/REMOVE_ALL)+'.tiff'
            
            ;fileName = tempFileName + 'Vitality_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            fileName = tempFileName + strcompress(nom+sampleName+'_Vitality',/REMOVE_ALL)+'.tiff'
            
            ; MOR - save results in the same directory as images - END
            ;textLeft = tempFileName + 'Vitality_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            textLeft = tempFileName + strcompress('Text_Short_'+nom+sampleName,/REMOVE_ALL)+'.tiff'
            
            CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
            oView->remove, oModel
            
            
              ; - write alive and dead count to file II - BEGIN
              ;'IMAGE'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'Dead'+STRING(9B)+'Alive'+STRING(9B)+'Vitality'
              ;temp = strcompress(sampleName,/rem)+STRING(9B)+strcompress(string(nX),/rem)+STRING(9B)+strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(numObjsDied),/rem)+STRING(9B)+strcompress(string(numObjsAlive),/rem)+STRING(9B)+strcompress(string( (numObjsAlive*100.)/(numObjsDied+numObjsAlive), format = '(D6.2)'),/rem)
              temp = strcompress(nom,/rem)+STRING(9B)+strcompress(string(nX),/rem)+STRING(9B)+strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(numObjsDied),/rem)+STRING(9B)+strcompress(string(numObjsAlive),/rem)+STRING(9B)+strcompress(string( (numObjsAlive*100.)/(numObjsDied+numObjsAlive), format = '(D6.2)'),/rem)
              CASE pX OF 
                 ;10: printF, U10x,temp
                 20: printF, U20x,temp
              ENDCASE 
              ; write alive and dead count to file II - END 
          ENDIF
         endfor
         
         ; - write alive and dead count to file IV - BEGIN
         ;close, U10x
         ;FREE_LUN, U10x
         
         close, U20x
         FREE_LUN, U20x
         ; - write alive and dead count to file IV - END
         ;/*------------------------ informe  FIN
         
         ; SUMMARY REPORT - BEGIN
         ; Susana - SUMMARY REPORT WITHOUT IMAGE
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         ;oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         ;oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         ;oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         ;oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         ;oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         ;oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - BEGIN
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - END
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange
         ;xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ;ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
         
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         ;IDLgrROI 
         ; MOR - modified LB code for generating scale bar below the trajectory image - END         
                                   
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         ;oModel->add, oImage
         oModel->add, oPlot
         ;oModel->add, oXAxis
         ;oModel->add, oYAxis
         ;oModel->add, oYAxisUp
         ;oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
         oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         ;write_tiff, tempFileName + 'Vitality_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
         
         ; Susana - 'Vitality_Text_Short.tiff'  INICIO
         string_0 = 'Vitality Summary'+' TOTAL '
         string_2 = strcompress('Alive'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
         string_4 = strcompress('Total Counting'+STRING(9B)+': ')
         string_441 = '# Alive   '+strcompress(': '+strcompress(string(numObjsAliveT, format = '(I6)'), /rem))
         string_442 = '# Died   '+strcompress(': '+strcompress(string(numObjsDiedT, format = '(I6)'), /rem))
         ;string_9 = strcompress(string( (numObjsAliveT*100)/(numObjsDiedT+numObjsAliveT), format = '(D6.2)'), /rem)
         string_9 = strcompress(string( (numObjsAliveT*1.)/(numObjsDiedT+numObjsAliveT)*100, format = '(D6.2)'), /rem)
         string_11 = strcompress(string(numObjsDiedT+numObjsAliveT, format = '(I6.2)'), /rem)
         string_15 = strcompress('[%]', /rem)
         string_16 = strcompress('[sperms]', /rem)
         
         fileName = tempFileName + 'Vitality_Text_ShortTOTAL'+'.tiff'  
         CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
         ; Susana - 'Vitality_Text_Short.tiff'  FIN
         
         ; MOR - save results in the same directory as images - BEGIN
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Vitality_Short.bmp'
         ;imageLeft =tempFileName + 'Vitality_trajectoryTOTAL'+'.tiff'; NO INCLUYO IMAGEN
         fileName = tempFileName + 'Vitality_ShortTOTAL'+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Vitality_Text_ShortTOTAL'+'.tiff'  
         
         ;CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
         CEDAI_combineImages_Vital, background, fileName, textLeft
         oView->remove, oModel
         ; SUMMARY REPORT - END
         
         
         ; write alive and dead count to file SOLO VALOR UNICO DE TODAS LAS FOTOS - BEGIN
         ;name = 'Vitality_ResultsTotal_'+nom; incluyo nombre de imagen
         name = 'Vitality_ResultsTotal_'; no incluyo nombre imagen, es un archivo para append
         ;file20x = strCompress(tempFileName + name + '.dat', /remove)
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nomFolder = strMid(tempFileName,pos[bslachs-3],lbSlashs[bslachs-3]) ; antepenultima carpeta
         
         ; SUSANA, descomentar siguiente linea y comentar la subsiguiente
         a = total(lbSlashs)-lbSlashs[bslachs-1]-lbSlashs[bslachs-2]+2 ; EN USO 30 Fotos
         ;a = total(lbSlashs)-lbSlashs[bslachs-1]+2 ;TEMPORALMENTE ACTIVADA
         
         nomFolder = strMid(tempFileName,0,a) ; hasta antepenultima carpeta
         
         file20x = strCompress(nomFolder+ '\' + name + '.dat', /remove)
         get_lun, U20x
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         
        
         
          OPENR, U20x, file20x, ERROR = err  
          IF (err NE 0) THEN BEGIN
           openW, U20x, file20x
           printF, U20x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'RctoMuertos'+STRING(9B)+'RctoVivos'+STRING(9B)+'PVitalidad'+STRING(9B)+'Fecha' ;PVitalidad=porcentaje de vitalidad
          ENDIF ELSE BEGIN
           close, U20x
           FREE_LUN, U20x
           openW, U20x, file20x,/APPEND
          ENDELSE
        ; openW, U20x, file20x
        ; openW, U20x, file20x,/APPEND
        ; printF, U20x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'RctoMuertos'+STRING(9B)+'RctoVivos'+STRING(9B)+'PVitalidad'+STRING(9B)+'Fecha' ;PVitalidad=porcentaje de vitalidad
         s_tiempo = strjoin(strsplit(SYSTIME(),' ',/extract),'_')
         ;temp = strcompress(sampleName,/rem)+STRING(9B)+strcompress(string(nX),/rem)+STRING(9B)+strcompress(string(numObjsDiedT),/rem)+STRING(9B)+strcompress(string(numObjsAliveT),/rem)+STRING(9B)+strcompress(string( (numObjsAliveT*100.)/(numObjsDiedT+numObjsAliveT), format = '(D6.2)'),/rem)+STRING(9B)+strcompress(string(s_tiempo),/rem)
         temp = strcompress(nom,/rem)+STRING(9B)+strcompress(string(nX),/rem)+STRING(9B)+strcompress(string(numObjsDiedT),/rem)+STRING(9B)+strcompress(string(numObjsAliveT),/rem)+STRING(9B)+strcompress(string( (numObjsAliveT*100.)/(numObjsDiedT+numObjsAliveT), format = '(D6.2)'),/rem)+STRING(9B)+strcompress(string(s_tiempo),/rem)
         printF, U20x,temp
         close, U20x
         FREE_LUN, U20x
         ; write alive and dead count to file SOLO VALOR UNICO DE TODAS LAS FOTOS - END
         
         ; - write raw trajectory to file IV - BEGIN
         close, U
         FREE_LUN, U
         ; - write raw trajectory to file IV - END
         
         widget_control, stack_tlb, set_uValue = stackState, /no_copy
         endif
         ;if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
       endcase
       
       'SAVEROIROISTACKTRACKPARAMOBJASINFOVITAL_CARO':begin ;'SAVEROIROISTACKTRACKPARAMOBJASASCII':begin
       ;Susana inventando INICIO -- informe pitados marcia
         sortTrackParams = -1
         if obj_valid(*state.poStackTrackContainer) then begin
;           s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
;                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
;                                              clusPos = clusPos, segPos = segPos, selROIGroupObj = selROIGroupObj, ROIObjParamPos = ROIObjParamPos,$
;                                              selROIObjSubParam = selROIObjSubParam, ROIObjSubParamPos = ROIObjSubParamPos
        
        widget_control, stack_tlb, get_uValue = stackState, /no_copy ; creo estructura stackState
         
         ; - write NonFragment and Fragmented count to file I - BEGIN
         ;save directory with stack info for saving images with results
         tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
         ; testName = last folder name
         pos = STRSPLIT(tempFileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
         ;nom = strMid(tempFileName,pos[bslachs-2],lbSlashs[bslachs-2]) ; penultima carpeta
         nom = strMid(tempFileName,pos[bslachs-1],lbSlashs[bslachs-1]) ; ultima carpeta
         name = 'FragROIsCARO_10x_'+nom
         file10x = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, U10x
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         openW, U10x, file10x
         ;openW, U10x, file10x, /APPEND
         ;printF, U10x, 'tPos'+STRING(9B)+'clusPos'+STRING(9B)+'nROIc'+STRING(9B)+'Xc'+STRING(9B)+'Yc'+STRING(9B)+'Zc'
         printF, U10x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'RctoFragmented'+STRING(9B)+'RctoNoFragmented'+STRING(9B)+'PFragmentacion' ;PFragmentacion=porcentaje de fragmentacion
         
         name = 'FragROIsCARO_20x_'+nom
         file20x = strCompress(tempFileName + name + '.dat', /remove)
         get_lun, U20x
         ;13B  carriage return     10B Linefeed  9B Horizontal Tab 
         openW, U20x, file20x
         ;printF, U20x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'RctoFragmented'+STRING(9B)+'RctoNoFragmented'+STRING(9B)+'PFragmentacion' ;PFragmentacion=porcentaje de fragmentacion
         printF, U20x, 'nomImagen'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'RctoFragmented'+STRING(9B)+'RctoNoFragmented'+STRING(9B)+'PFragmentacion'+STRING(9B)+'PromFragmented'+STRING(9B)+'PromNoFragmented' ;PFragmentacion=porcentaje de fragmentacion
         ; write NonFragment and Fragmented count to file I - END
         
         
         image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
         ; MOR - 05Jul2010
         ; get real pixel info to convert the track object coordinates back to pixel coords for plotting results
         (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
         
         xPixel =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
         yPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
         xRe =  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]]
         yRe = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]
         xReal = xRe/xPixel
         yReal = yRe/yPixel
         
         ;Susana - control de inicio de datos x e y segun time -- INICIO
         ;Susana - NoFragmented en Cluster0_&_Channel_001 y Fragmented en Cluster0_&_Channel_000, y que estoy ejecutando menu desde cualquiera de esos dos clusters
         ;vClusPosFragment=0 
         ;strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(0) + '_', /rem)
         strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(3) + '_', /rem) ;NoFragmented en Cluster3_&_Channel_000 y Fragmented en Cluster1_&_Channel_000,
         t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
         
         radius = 8 ; circle radius
         points = (2 * !PI /99.0 * FINDGEN(100)) ; used for draw circles
         controlCurt1 = 0
         controlCurt2 = 0
         numObjsNonFragmentedT = 0 ; NONFRAGMENT TOTAL COUNTIN, ALL IMAGES
         numObjsFragmentedT = 0 ; FRAGMENTED TOTAL COUNTIN, ALL IMAGES
         numObjsFragmented = 0
         numObjsNonFragmented = 0
         promFragmentedSizeROIT = 0
         promNonFragmentedSizeROIT = 0
         for vtime = 0,totalTNum-1 do begin ; viajo tiempo a tiempo
          numObjsFragmented = 0
;          numObjsFragmented = 0
          ;sizeObjsFragmented = 0 ;para sizeROI acumulado y luego promdiar
          
          image = (*stackState.pImageStackInfoObject)->C_sImageStackObject::getSelectedImage(tPos = vtime, chPos = chPos, zPos = zPos)
          ;MOR - save directory with stack info for saving images with results
          tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
          
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
          ;Susana - sb in micrometers - BEGIN
          sb = 50 ; sb in micrometers     reemplazado por 120 pixeles == 50 micrómetros
          sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
          pixsSB  = floor(1.0*(sb/xReal))
          boxS = bytarr(3, pixsSB, 5)
          boxS[0,*,*] = 0
          boxS[1, *,*] = 0
          boxS[2,*,*] = 0
          ;Susana - sb in micrometers - END
          
          position = [0.1, 0.1, 0.95, 0.95]
          oPlot->GetProperty, XRange=xrange
          oPlot->GetProperty, YRange=yrange
          xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
          ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
          
          oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
          oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
          oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
          ;IDLgrROI 
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
          oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
          
          
          
          
          
          ;======================================================================
          ;Susana - Fragmented sperms Cluster 3 -- BEGIN
          ;Fragmented en Cluster3_&_Channel_000 y Non-Fragmented en Cluster5_&_Channel_000
          vClusPosFragment = 3
          strID = strCompress('Ch' + string(0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosFragment) + '_', /rem)
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          if(t[0] ne -1) then begin          
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
            ROIsize = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Size [x²]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
            ;ROIsize *= xReal
            
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt1 = -1
            ENDIF ELSE BEGIN
              controlCurt1 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
              sizeROI = sqrt(ROIsize[curT])
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                radius = sizeROI[j];ajusto radio a ROIsize
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [0,255,0] ;Vivos, Verdes
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
              endfor
              
              numObjsFragmented = size(xParamsROI,/N_ELEMENTS)
              numObjsFragmentedT += numObjsFragmented
              string_8 = strcompress(string(fix(numObjsFragmented)), /remove_all) + ' sperms'
              ;string_Fragmented = 'Recuento Cluster :'+strcompress(string(fix(clusPos)), /remove_all)+'  #='+string_8
              string_Fragmented = 'Recuento Cluster :'+strcompress(string(fix(vClusPosFragment)), /remove_all)+' Fragmented  #='+string_8
              print,string_Fragmented+' time='+string(fix(vtime))
              promFragmentedSizeROI = mean(sizeROI,/Double)
              promFragmentedSizeROIT += total(sizeROI,/Double)
              
              
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - BEGIN
              ;ejemplo de nombre :  4883_101_t000_ch01_z000.tif
              ;y recupero solo nombre inicial y objetivo 20x o 10x   desde el nombre imagen
              ;tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamNameList(tPos = tPos, chPos = chPos, zPos = zPos)).A
              
              tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = vtime, chPos = 0, zPos = 0)).A
              sampleName = 'NoEncontrado'
              ;sampleName = tempImageName
              
              ;vExists = strPos(tempImageName, '_t', /reverse_search)
              ;if (vExists eq -1) then vExists = strPos(tempImageName, '_t', /reverse_search)
;              add = 2 ;add = strlen('_t')
;              vExists = strPos(tempImageName, '_10', /reverse_search)
;              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) )+1,2 ) ; largo=2 pues sé que es 10 (10x) o 20 (20x)
;              
;              vExists = strPos(tempImageName, '_20', /reverse_search)
;              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_20', /reverse_search) )+1,2 )
;              pX = fix(nX)
              vExists = strPos(tempImageName, '_t', /reverse_search)
              ;sampleName = strMid(tempImageName, (strPos(tempImageName, '_'+nX, /reverse_search) )+1,(strPos(tempImageName, '_t', /reverse_search) ) )
              if (vExists ne -1) then sampleName = strMid(tempImageName, (strPos(tempImageName, '_t', /reverse_search)+1 ), 4)
              
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - END
            ENDELSE;IF ( curT gt 0)
          endif ;if(t[0] ne -1) begin
          ;Susana - Fragmented sperms Cluster 3 -- END
          
          ;Susana - NonFragmented sperms Cluster 5 -- BEGIN
          vClusPosNonFragment= 5 ; Non-Fragmented en Clus 5 20x 
          ;Fragmented en Cluster0_&_Channel_000
          strID = strCompress('Ch' + string(0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(vClusPosNonFragment) + '_', /rem) ;Non-Fragmented en Cluster5_&_Channel_000
;          case selROIGroupObj of
;            'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
;            ;'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
;          endcase
          t = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
          if(t[0] ne -1) then begin
            xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
            xParams*=xReal
            ;if (count gt 0) then xParams = xParams[whNotMinusOne]
            yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
            yParams*=yReal
            ;if (count gt 0) then yParams = yParams[whNotMinusOne]
            zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
            ;zParams*=zReal
            ;if (count gt 0) then zParams = zParams[whNotMinusOne]
            ROIsize = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Size [x²]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
            ROIsizePix = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Size [Pixel²]') ; todos los ROIs de Time000 (tpos=0) contienen el valor cero
            ;ROIsize *= xReal
            curT = where(t eq vtime) ; puntos donde t==tPos
            IF (curT[0] eq -1) THEN BEGIN
              controlCurt2 = -1
            ENDIF ELSE BEGIN 
              controlCurt2 = 0
              xParamsROI = xParams[curT] ; solo lo de xParams cuyos t son ==tPos
              yParamsROI = yParams[curT] ; solo lo de yParams cuyos t son ==tPos
              sizeROI = sqrt(ROIsize[curT])
              for j = 0,(size(xParamsROI,/N_ELEMENTS)-1) do begin
                ; create x and y circle coordinates
                radius = sizeROI[j];ajusto radio a ROIsize
                xParamsROI_circle = xParamsROI[j] + radius * cos(points)
                yParamsROI_circle = yParamsROI[j] + radius * sin(points)
                
                ;include a new circle/sperm on plot
                col = [255,0,0] ;Muertos, Rojos
                oPlot = obj_new('IDLgrPlot', datax = xParamsROI_circle/xReal, datay = yParamsROI_circle/yReal, color = col, thick = 2, xcoord_conv = xs, ycoord_conv = ys, symbol = oSym)
                oModel->add, oPlot
              endfor
              numObjsNonFragmented = size(xParamsROI,/N_ELEMENTS)
              numObjsNonFragmentedT += numObjsNonFragmented
              string_8 = strcompress(string(fix(numObjsNonFragmented)), /remove_all) + ' sperms'
              string_NonFragmented = 'Recuento Cluster :'+strcompress(string(fix(vClusPosNonFragment)), /remove_all)+' Non Fragmented  #='+string_8
              print,string_NonFragmented+' time='+string(fix(vtime))
              promNonFragmentedSizeROI = mean(sizeROI,/Double)
              promNonFragmentedSizeROIT += total(sizeROI,/Double)
              
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - BEGIN
              ;ejemplo de nombre :  4883_101_t000_ch01_z000.tif
              ;y recupero solo nombre inicial y objetivo 20x o 10x   desde el nombre imagen
              ;tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamNameList(tPos = tPos, chPos = chPos, zPos = zPos)).A
              tempImageName = ((*stackState.pImageStackInfoObject)->getSelectedImageParamAsStruct(tPos = vtime, chPos = 0, zPos = 0)).A
              sampleName = 'NoEncontrado'
              ;vExists = strPos(tempImageName, '_t', /reverse_search)
              ;if (vExists eq -1) then vExists = strPos(tempImageName, '_t', /reverse_search)
;              add = 2 ;add = strlen('_t')
;              vExists = strPos(tempImageName, '_10', /reverse_search)
;              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_10', /reverse_search) )+1,2 ) ; largo=2 pues sé que es 10 (10x) o 20 (20x)
;              vExists = strPos(tempImageName, '_20', /reverse_search)
;              if (vExists ne -1) then nX = strMid(tempImageName, (strPos(tempImageName, '_20', /reverse_search) )+1,2 )
;              pX = fix(nX)
              vExists = strPos(tempImageName, '_t', /reverse_search)
              ;sampleName = strMid(tempImageName, (strPos(tempImageName, '_'+nX, /reverse_search) )+1,(strPos(tempImageName, '_t', /reverse_search) ) )
              if (vExists ne -1) then sampleName = strMid(tempImageName, (strPos(tempImageName, '_t', /reverse_search)+1 ), 4)
              
              ; Susana - Recupero nombre de una de las imágenes del Time, cualquiera - END
            ENDELSE;IF ( curT gt 0)
          endif ;if(t[0] ne -1) then begin
          ;Susana - Non Fragmented sperms Cluster 5-- END 
          ;======================================================================
          
          IF ( (controlCurt1 eq 0) or (controlCurt2 eq 0)) THEN BEGIN 
            ;oView = obj_new('IDLgrView', viewplane_rect = [xr[0]-.6, yr[0]-1.8, xr[1]+2.45, yr[1]+2.15], color = [255,255,255]) ; Susana - comenta para ver foto completa
            oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
            oView->add, oModel
            oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
            oBuffer->draw, oView      
            oOImage = oBuffer->read()
            oOImage->getProperty, data = outImage_1
            ;write_tiff, tempFileName + 'Fragmentation_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
            write_tiff, tempFileName + strcompress('Trajectory_'+sampleName,/REMOVE_ALL)+'.tiff', outImage_1
            
            ; Susana - 'Fragmentation_Text_Short.tiff'  INICIO
            string_0 = 'Fragmentation Summary'+' Time '+string(vtime); vtime
            string_2 = strcompress('Fragmented'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
            string_4 = strcompress('Total Counting'+STRING(9B)+': ')
            string_441 = '# Fragmented  '+strcompress(': '+strcompress(string(numObjsFragmented, format = '(I6)'), /rem))
            string_442 = '# Non-Fragmented  '+strcompress(': '+strcompress(string(numObjsNonFragmented, format = '(I6)'), /rem))
            string_443 = 'mean Size '+strcompress(': '+strcompress(string(promFragmentedSizeROI, format = '(D6.2)'), /rem))+' [!9m!X'+'m]'; !9m!X'    letra  mu
            string_444 = 'mean Size '+strcompress(': '+strcompress(string(promNonFragmentedSizeROI, format = '(D6.2)'), /rem))+' [!9m!X'+'m]'; !9m!X'    letra  mu
            string_9 = strcompress(string( (numObjsFragmented*100.)/(numObjsFragmented+numObjsNonFragmented), format = '(D6.2)'), /rem)
            string_11 = strcompress(string(numObjsFragmented+numObjsNonFragmented, format = '(I6.2)'), /rem)
            string_15 = strcompress('[%]', /rem)
            string_16 = strcompress('[sperms]', /rem)
            
            ;fileName = tempFileName + 'Fragmentation_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'  
            fileName = tempFileName + strcompress('Text_Short_'+sampleName,/REMOVE_ALL)+'.tiff'
            ;CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
            CEDAI_textPlot_Count_Short_Fragm, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442,string_443,string_444
            ; Susana - 'Fragmentation_Text_Short.tiff'  FIN
            
            ; MOR - save results in the same directory as images - BEGIN
            background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Fragmentation_Short.bmp'
            ;imageLeft =tempFileName + 'Fragmentation_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff'
            imageLeft =tempFileName + strcompress('Trajectory_'+sampleName,/REMOVE_ALL)+'.tiff'
            
            ;fileName = tempFileName + 'Fragmentation_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            fileName = tempFileName + strcompress(sampleName+'_Fragmentation',/REMOVE_ALL)+'.tiff'
            
            ; MOR - save results in the same directory as images - END
            ;textLeft = tempFileName + 'Fragmentation_Text_ShortT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff' 
            textLeft = tempFileName + strcompress('Text_Short_'+sampleName,/REMOVE_ALL)+'.tiff'
            
            CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
            oView->remove, oModel
            
            nX = 20  ;Susana ya no necesito objetivo 10x o 20x
            pX = 20  ;Susana ya no necesito objetivo 10x o 20x
              ; - write NonFragment and Fragmented count to file II - BEGIN
              ;'IMAGE'+STRING(9B)+'Objetivo'+STRING(9B)+'tPos'+STRING(9B)+'Fragmented'+STRING(9B)+'Fragmented'+STRING(9B)+'Fragmentation'
              ;temp = strcompress(sampleName,/rem)+STRING(9B)+strcompress(string(nX),/rem)+STRING(9B)+strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(numObjsFragmented),/rem)+STRING(9B)+strcompress(string(numObjsFragmented),/rem)+STRING(9B)+strcompress(string( (numObjsFragmented*100.)/(numObjsFragmented+numObjsFragmented), format = '(D6.2)'),/rem)
              temp = strcompress(sampleName,/rem)+STRING(9B)+strcompress(string(nX),/rem)+STRING(9B)+strcompress(string(vtime),/rem)+STRING(9B)+strcompress(string(numObjsFragmented),/rem)+STRING(9B)+strcompress(string(numObjsNonFragmented),/rem)+STRING(9B)+strcompress(string((numObjsFragmented*100.)/(numObjsFragmented+numObjsNonFragmented), format = '(D6.2)'),/rem)+STRING(9B)+strcompress(string(promFragmentedSizeROI*1., format = '(D6.2)'),/rem)+STRING(9B)+strcompress(string(promNonFragmentedSizeROI*1., format = '(D6.2)'),/rem)
              
              CASE pX OF 
                 10: printF, U10x,temp
                 20: printF, U20x,temp
              ENDCASE 
              ; write NonFragment and Fragmented count to file II - END 
          ENDIF
         endfor
         
         ; - write NonFragment and Fragmented count to file IV - BEGIN
         close, U10x
         FREE_LUN, U10x
         
         close, U20x
         FREE_LUN, U20x
         ; - write NonFragment and Fragmented count to file IV - END
         ;/*------------------------ informe  FIN
         
         ; SUMMARY REPORT - BEGIN
         ; Susana - SUMMARY REPORT WITHOUT IMAGE
         oFont = obj_new('IDLgrFont','helvetica',size=14)
         oImage = obj_new('IDLgrImage',image, xcoord_conv = xs, ycoord_conv = ys)
         oPlot = obj_new('IDLgrPlot', datay = [0,0], datax = [0, image_x], color = [0,0,0] , xcoord_conv = xs, ycoord_conv = ys)
         ;oXTitle = obj_new('IDLgrText', 'x-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         ;oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oxtitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         ;oYTitle = obj_new('IDLgrText', 'y-position [pix]', color = plotcolor, xcoord_conv = xs, ycoord_conv = ys,font = oFont)
         ;oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oytitle, color = plotcolor, location = [0, 0], xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact)
         ;oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotcolor, location = [xr[1], 0], tickdir = 1, xcoord_conv = xs, ycoord_conv = ys, ticklen  = 30, /exact, /notext)
         ;oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotcolor, location = [0, yr[1]], tickdir = 1, ticklen = 30, xcoord_conv = xs, ycoord_conv = ys, /exact, /notext)
         
         ; MOR - modified LB code for generating scale bar below the trajectory image
         ; want a scale bar that is 50 um - determine how many pixels that is - BEGIN
         ;Susana - sb in micrometers - BEGIN
         sb = 50 ; sb in micrometers 
         sbText = strcompress(string(sb),/rem)+' '+strcompress('um',/rem)
         pixsSB  = floor(1.0*(sb/xReal))
         boxS = bytarr(3, pixsSB, 5)
         boxS[0,*,*] = 0
         boxS[1, *,*] = 0
         boxS[2,*,*] = 0
         ;Susana - sb in micrometers - END
         
         position = [0.1, 0.1, 0.95, 0.95]
         oPlot->GetProperty, XRange=xrange
         oPlot->GetProperty, YRange=yrange
         ;xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
         ;ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
         
         oFont_box = obj_new('IDLgrFont','helvetica*bold',size=14)
         oImage_box = obj_new('IDLgrImage',boxS,location = [0.0, -25.0], XCoord_Conv=xs, YCoord_Conv=ys)
         oText_box = obj_new('IDLgrText', sbText, location = [0.0, -45.0], color = plotcolor, xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
         ;IDLgrROI 
         ; MOR - modified LB code for generating scale bar below the trajectory image - END         
                                   
         oModel = obj_new('IDLgrModel')
         oModel->remove, /all
         ;oModel->add, oImage
         oModel->add, oPlot
         ;oModel->add, oXAxis
         ;oModel->add, oYAxis
         ;oModel->add, oYAxisUp
         ;oModel->add, oXAxisUp
         oModel->add, oImage_box
         oModel->add, oText_box
         ;oModel->add, oFont_box
         oSym = obj_new('IDLgrSymbol', data = 3, size = 1, thick = 1);data3 = Period (Dot)
         oView = obj_new('IDLgrView', viewplane_rect = [-1., yr[0]-1.8, xr[1]+25.45, yr[1]+25.15], color = [255,255,255]) ; Susana - ver foto completa
         oView->add, oModel
         oBuffer = obj_new('IDLgrBuffer', dimensions = [800, 600])
         oBuffer->draw, oView      
         oOImage = oBuffer->read()
         oOImage->getProperty, data = outImage_1
         ;write_tiff, tempFileName + 'Fragmentation_trajectoryT'+strcompress(string(vtime),/REMOVE_ALL)+'.tiff', outImage_1
         
         ; Susana - 'Fragmentation_Text_Short.tiff'  INICIO
         promNonFragmentedSizeROIT /=numObjsNonFragmentedT
         promFragmentedSizeROIT /=numObjsFragmentedT
         string_0 = 'Fragmentation Summary'+' TOTAL '
         string_2 = strcompress('Fragmented'+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+STRING(9B)+': ') ;+STRING(9B)  horizontal tab
         string_4 = strcompress('Total Counting'+STRING(9B)+': ')
         string_441 = '# Fragmented  '+strcompress(': '+strcompress(string(numObjsFragmentedT, format = '(I6)'), /rem))
         string_442 = '# Non-Fragmented  '+strcompress(': '+strcompress(string(numObjsNonFragmentedT, format = '(I6)'), /rem))
         string_443 = 'mean Size '+strcompress(': '+strcompress(string(promFragmentedSizeROIT, format = '(D6.2)'), /rem))+' [!9m!X'+'m]'; !9m!X'    letra  mu
         string_444 = 'mean Size '+strcompress(': '+strcompress(string(promNonFragmentedSizeROIT, format = '(D6.2)'), /rem))+' [!9m!X'+'m]'; !9m!X'    letra  mu
         
         string_9 = strcompress(string( (numObjsFragmentedT*100)/(numObjsFragmentedT+numObjsNonFragmentedT), format = '(D6.2)'), /rem)
         string_11 = strcompress(string(numObjsFragmentedT+numObjsNonFragmentedT, format = '(I6.2)'), /rem)
         string_15 = strcompress('[%]', /rem)
         string_16 = strcompress('[sperms]', /rem)
         
         fileName = tempFileName + 'Fragmentation_Text_ShortTOTAL'+'.tiff'  
         ;CEDAI_textPlot_Count_Short_Vital, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442
         CEDAI_textPlot_Count_Short_Fragm, filename, string_0, string_2, string_4, string_9, string_11, string_15,string_16,string_441,string_442,string_443,string_444
         ; Susana - 'Fragmentation_Text_Short.tiff'  FIN
         
         ; MOR - save results in the same directory as images - BEGIN
         background = s_getPathForSystem() + '\SCIAN_Code\SCIAN_Soft\imageDat\CEDAI_Background_Fragmentation_Short.bmp'
         ;imageLeft =tempFileName + 'Fragmentation_trajectoryTOTAL'+'.tiff'; NO INCLUYO IMAGEN
         fileName = tempFileName + 'Fragmentation_ShortTOTAL'+'.tiff' 
         ; MOR - save results in the same directory as images - END
         textLeft = tempFileName + 'Fragmentation_Text_ShortTOTAL'+'.tiff'  
         
         ;CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft
         CEDAI_combineImages_Vital, background, fileName, textLeft
         oView->remove, oModel
         ; SUMMARY REPORT - END
         
         widget_control, stack_tlb, set_uValue = stackState, /no_copy
         endif
         ;if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
       endcase
        ;---------------------- CEDAI-FIN -------------------------
        'SAVEROIROISTACKTRACKPARAMOBJASASCII':begin

                        sortTrackParams = -1
                        if obj_valid(*state.poStackTrackContainer) then begin

                          strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem)
                          case selROIGroupObj of
                          'oROI2DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
                          'oROI3DGroup':trackParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
                          endcase

                             ; find object order in trackParams
                          if (n_elements(trackParams) ne 0) then begin
                             whNotMinusOne = where(trackParams ne -1, count)
                             if (count gt 0) then trackParams = trackParams[whNotMinusOne]
                             sortTrackParams = fix(trackParams[uniq(trackParams, sort(trackParams))])
                          endif

                          xParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D x-center position')
                          if (count gt 0) then xParams = xParams[whNotMinusOne]

                          yParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D y-center position')
                          if (count gt 0) then yParams = yParams[whNotMinusOne]

                          zParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '2D z-center position')
                          if (count gt 0) then zParams = zParams[whNotMinusOne]

                          case selROIGroupObj of
                          'oROI2DGroup':tParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + 'Object Time [s]')
                          'oROI3DGroup':tParams = *state.poStackTrackContainer->getParamFromParamName(paramName = strID + '3D Object Time [s]')
                          endcase
                          if (count gt 0) then tParams = tParams[whNotMinusOne]

                        endif

                        if (sortTrackParams[0] ne -1) then begin
                           pAllParams = ptrArr(n_elements(sortTrackParams))
                           for i = 0, n_elements(sortTrackParams)-1 do begin
                             whParam = where(trackParams eq sortTrackParams[i])
                             if (whParam[0] ne -1) then pAllParams[i] = ptr_new([transpose(tParams[whParam]), transpose(xParams[whParam]), transpose(yParams[whParam]), transpose(zParams[whParam])], /no_copy) $
                             else begin
                                tParamsMin = min(tParams, max = tParamsMax)
                                xParamsMin = min(xParams, max = xParamsMax)
                                yParamsMin = min(yParams, max = yParamsMax)
                                zParamsMin = min(zParams, max = zParamsMax)
                                pAllParams[i] = ptr_new([transpose([tParamsMin, tParamsMax]), transpose([xParamsMin, xParamsMax]), transpose([yParamsMin, yParamsMax]), transpose([zParamsMin, zParamsMax])], /no_copy)
                             endelse
                           endfor
                        endif

                        file = pickfile(/write, path = state.currROI2DGroupFileName, filter = '*.dat', file = 'file Name', /noconf)
                        if (file ne '') then begin
                           for i = 0, n_elements(sortTrackParams)-1 do begin
                              objStr = strCompress(string(floor(sortTrackParams[i]/10.)) + string(sortTrackParams[i] mod 10), /rem)
                              while(strLen(objStr) lt 4) do objStr = strCompress('0' + objStr, /rem)
                              openW,2, strCompress(file + strCompress('2D x-center position' + '_Obj_' + '_' + '2D y-center position' + '_Obj_' + objStr + '.dat', /remove))
                              printF, 2, [transpose(make_array(n_elements((*pAllParams[i])[0,*]), type = size((*pAllParams[i])[0,*], /type)) + (*pAllParams[i])[0,*]),$
                                          transpose(make_array(n_elements((*pAllParams[i])[1,*]), type = size((*pAllParams[i])[1,*], /type)) + (*pAllParams[i])[1,*]),$
                                          transpose(make_array(n_elements((*pAllParams[i])[2,*]), type = size((*pAllParams[i])[2,*], /type)) + (*pAllParams[i])[2,*]),$
                                          transpose(make_array(n_elements((*pAllParams[i])[3,*]), type = size((*pAllParams[i])[3,*], /type)) + (*pAllParams[i])[3,*])]
                              close, 2
                           endfor
                        endif else a = s_apop_shout('Not saved !')

                        if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
       endcase
       'SAVEGRAPHICSTATE':begin
                         
                         oPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Base')
                         oPlaneComplementaryModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Complementary')
                         oOrthogonalPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Orthogonal')
                         oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
                              if not(obj_valid(oMeshModel)) then oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface Model')
                              if not(obj_valid(oMeshModel)) then oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
                              if not(obj_valid(oMeshModel) and (obj_valid(oPlaneModel) or obj_valid(oOrthogonalPlaneModel))) then Result = DIALOG_MESSAGE('You must load some Surface Model and Plane Model')
;                              if not(obj_valid(oMeshModel)) and (obj_valid(oPlaneModel) or obj_valid(oPlaneModel)) then Result = DIALOG_MESSAGE('You must load some Surface Model and Plane Model')
                              
                        if obj_valid(oMeshModel) and (obj_valid(oPlaneModel) or obj_valid(oOrthogonalPlaneModel)) then begin  
                            oMeshModel->getproperty, TRANSFORM = ctm_mesh_model
                            if obj_valid(oPlaneModel) then oPlaneModel->getproperty, TRANSFORM = ctm_plano_model
                            if obj_valid(oOrthogonalPlaneModel) then oOrthogonalPlaneModel->getproperty, TRANSFORM = ctm_OrthogonalPlano_model
                            if obj_valid(oPlaneComplementaryModel) then oPlaneComplementaryModel->getproperty, TRANSFORM = ctm_PlanoComplementario_model
                            
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                             oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = 0, chPos = 0, zPos = 0)
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
        
                            path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
                            ;name = strCompress('GraphicState_Time_' + strCompress(tPos)  + '_Cluster_' + strCompress(clusPos) + '.sav', /rem)
                            filename = strCompress(path + strCompress('GraphicState_Time_' + strCompress(tPos) + '.sav', /rem))
                            
                            openw, 3, filename
                            PRINTF, 3, ctm_mesh_model
                            if obj_valid(oPlaneModel) then PRINTF, 3, ctm_plano_model
                            if obj_valid(oOrthogonalPlaneModel) then PRINTF, 3, ctm_OrthogonalPlano_model
                            if obj_valid(oPlaneComplementaryModel) then PRINTF, 3, ctm_PlanoComplementario_model

                            close, 3
                          endif
        endcase
        'APPLYGRAPHICSTATE':begin
                         
                        oPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Base')
                        oOrthogonalPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Orthogonal')
                        oPlaneComplementaryModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Complementary')
                        oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
                              if not(obj_valid(oMeshModel)) then oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface Model')
                              if not(obj_valid(oMeshModel)) then oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
                              if not(obj_valid(oMeshModel) and (obj_valid(oPlaneModel) or obj_valid(oOrthogonalPlaneModel)))  then Result = DIALOG_MESSAGE('You must load some Surface Model and Plane Model')
                        ctm_mesh=make_array(4,4,/double)
                        ctm_plane=make_array(4,4,/double)
                        ctm_planeComplementary=make_array(4,4,/double)
                        ctm_orthogonalplane=make_array(4,4,/double)
                        
                        if obj_valid(oMeshModel) and (obj_valid(oPlaneModel) or obj_valid(oOrthogonalPlaneModel)) then begin  
                            
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                        
                            path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
                            filename = strCompress(path + strCompress('GraphicState_Time_' + strCompress(tPos) + '.sav', /rem))
                            flag = FILE_TEST(filename)
                            line = make_Array(4,/double)
                            i=0
                            if ~flag then Result = DIALOG_MESSAGE('File ' + name + ' Not Found')
                            if flag then begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                while ~ EOF(inunit) do begin
                                   READF, inunit, line 
                                   if( i le 3) then ctm_mesh[*,i]=line
                                   if((i gt 3) and (i le 7) ) then ctm_plane[*,i-4]=line
                                   if((i gt 7) and (i le 11) ) then ctm_orthogonalplane[*,i-8]=line
                                   if((i gt 11) ) then ctm_planeComplementary[*,i-12]=line
                                   i++ 
                                endwhile    
                                FREE_LUN, inunit
                                
                                if obj_valid(oPlaneModel) then oPlaneModel->setproperty, TRANSFORM = ctm_plane
                                if obj_valid(oOrthogonalPlaneModel) then oOrthogonalPlaneModel->setproperty, TRANSFORM = ctm_orthogonalplane
                                if obj_valid(oPlaneComplementaryModel) then oPlaneComplementaryModel->setproperty, TRANSFORM = ctm_planeComplementary
                                
                                oMeshModel->setproperty, TRANSFORM = ctm_mesh    
                                
                           endif
                         endif
        endcase
        'APPLYSELECTGRAPHICSTATE':begin
                         
                        oPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Base')
                        oOrthogonalPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Orthogonal')
                        oPlaneComplementaryModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Complementary')
                        oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
                              if not(obj_valid(oMeshModel)) then oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface Model')
                              if not(obj_valid(oMeshModel)) then oMeshModel = *(state.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
                              if not(obj_valid(oMeshModel) and (obj_valid(oPlaneModel) or obj_valid(oOrthogonalPlaneModel)))  then Result = DIALOG_MESSAGE('You must load some Surface Model and Plane Model')
                        ctm_mesh=make_array(4,4,/double)
                        ctm_plane=make_array(4,4,/double)
                        ctm_planeComplementary=make_array(4,4,/double)
                        ctm_orthogonalplane=make_array(4,4,/double)
                        
                        if obj_valid(oMeshModel) and (obj_valid(oPlaneModel) or obj_valid(oOrthogonalPlaneModel)) then begin  
                            
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                            
                            name = strCompress('GraphicState_Time_' + strCompress(tPos) + '.sav', /rem)
                            filename = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]], get_path = path, filter = '*.sav')
                            flag = FILE_TEST(filename)
                            line = make_Array(4,/double)
                            i=0
                            if flag eq 0 then Result = DIALOG_MESSAGE('File ' + name + ' Not Found')
                            if flag then begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                while ~ EOF(inunit) do begin
                                   READF, inunit, line 
                                   if( i le 3) then ctm_mesh[*,i]=line
                                   if((i gt 3) and (i le 7) ) then ctm_plane[*,i-4]=line
                                   if((i gt 7) and (i le 11) ) then ctm_orthogonalplane[*,i-8]=line
                                   if((i gt 11) ) then ctm_planeComplementary[*,i-12]=line

                                   i++ 
                                endwhile    
                                FREE_LUN, inunit
                                    
                                if obj_valid(oPlaneModel) then oPlaneModel->setproperty, TRANSFORM = ctm_plane
                                if obj_valid(oOrthogonalPlaneModel) then oOrthogonalPlaneModel->setproperty, TRANSFORM = ctm_orthogonalplane
                                if obj_valid(oPlaneComplementaryModel) then oPlaneComplementaryModel->setproperty, TRANSFORM = ctm_planeComplementary
                                
                                oMeshModel->setproperty, TRANSFORM = ctm_mesh    
                           endif
                         endif
        endcase

        'ONOFFCOLORBYPARAM':begin
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                            
                            
                           state.fParamSkelByColorParamOnOff = s_ToggleButtonOnOffState(state.wParamSkelByColorParamOnOff)
                           fUpdate = 0b

                           vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep())
                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep())
                           
                            filename = strCompress(vPath+ strcompress('_Skeleton_' + path_sep()+'_Params' + path_sep() +'ColoredParams_' + '.sav', /rem))

                            flag = FILE_TEST(filename)
                            params = [0,0] ; usar color... y cual param
                            if flag eq 0 then begin
                            ; crear
                              GET_LUN, outunit
                              openw,  outunit, filename
                              PRINTF, outunit, params
                              close,  outunit
                            
                            endif else begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                   READF, inunit, params 
                                FREE_LUN, inunit
                                    
                                ;guardar nuevos valores
                                if(state.fParamSkelByColorParamOnOff eq 1) then params[0] = 1
                                if(state.fParamSkelByColorParamOnOff eq 0) then params[0] = 1

                                GET_LUN, outunit
                                openw,  outunit, filename
                                PRINTF, outunit, params
                                close,  outunit
                           endelse
        endcase

        'APPLYCOLORPARAM0':begin
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                           fUpdate = 0b

                           vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep())
                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep())
                           
                            filename = strCompress(vPath+strcompress('_Skeleton_' + path_sep()+'_Params' + path_sep() +'ColoredParams_' + '.sav', /rem))

                            flag = FILE_TEST(filename)
                            params = [0,0] ; usar color... y cual param
                            if flag eq 0 then begin
                            ; crear
                              GET_LUN, outunit
                              openw,  outunit, filename
                              PRINTF, outunit, params
                              close,  outunit
                            
                            endif else begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                   READF, inunit, params 
                                FREE_LUN, inunit
                                    
                                ;guardar nuevos valores
                                params[1] = 0

                                GET_LUN, outunit
                                openw,  outunit, filename
                                PRINTF, outunit, params
                                close,  outunit
                           endelse
        endcase
        'APPLYCOLORPARAM1':begin
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                           fUpdate = 0b

                           vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep())
                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep())
                           
                            filename = strCompress(vPath+strcompress('_Skeleton_' + path_sep()+'_Params' + path_sep() +'ColoredParams_' + '.sav', /rem))

                            flag = FILE_TEST(filename)
                            params = [0,0] ; usar color... y cual param
                            if flag eq 0 then begin
                            ; crear
                              GET_LUN, outunit
                              openw,  outunit, filename
                              PRINTF, outunit, params
                              close,  outunit
                            
                            endif else begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                   READF, inunit, params 
                                FREE_LUN, inunit
                                    
                                ;guardar nuevos valores
                                params[1] = 1

                                GET_LUN, outunit
                                openw,  outunit, filename
                                PRINTF, outunit, params
                                close,  outunit
                           endelse

        endcase
        'APPLYCOLORPARAM2':begin
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                           fUpdate = 0b

                           vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep())
                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep())
                           
                            filename = strCompress(vPath+strcompress('_Skeleton_' + path_sep()+'_Params' + path_sep() +'ColoredParams_' + '.sav', /rem))

                            flag = FILE_TEST(filename)
                            params = [0,0] ; usar color... y cual param
                            if flag eq 0 then begin
                            ; crear
                              GET_LUN, outunit
                              openw,  outunit, filename
                              PRINTF, outunit, params
                              close,  outunit
                            
                            endif else begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                   READF, inunit, params 
                                FREE_LUN, inunit
                                    
                                ;guardar nuevos valores
                                params[1] = 2

                                GET_LUN, outunit
                                openw,  outunit, filename
                                PRINTF, outunit, params
                                close,  outunit
                           endelse
        
        endcase
        'APPLYCOLORPARAM3':begin
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                           fUpdate = 0b

                           vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep())
                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep())
                           
                            filename = strCompress(vPath+strcompress('_Skeleton_' + path_sep()+'_Params' + path_sep() +'ColoredParams_' + '.sav', /rem))

                            flag = FILE_TEST(filename)
                            params = [0,0] ; usar color... y cual param
                            if flag eq 0 then begin
                            ; crear
                              GET_LUN, outunit
                              openw,  outunit, filename
                              PRINTF, outunit, params
                              close,  outunit
                            
                            endif else begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                   READF, inunit, params 
                                FREE_LUN, inunit
                                    
                                ;guardar nuevos valores
                                params[1] = 3

                                GET_LUN, outunit
                                openw,  outunit, filename
                                PRINTF, outunit, params
                                close,  outunit
                           endelse
        
        endcase
        'APPLYCOLORPARAM4':begin
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                           fUpdate = 0b

                           vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep())
                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep())
                           
                            filename = strCompress(vPath+strcompress('_Skeleton_' + path_sep()+'_Params' + path_sep() +'ColoredParams_' + '.sav', /rem))

                            flag = FILE_TEST(filename)
                            params = [0,0] ; usar color... y cual param
                            if flag eq 0 then begin
                            ; crear
                              GET_LUN, outunit
                              openw,  outunit, filename
                              PRINTF, outunit, params
                              close,  outunit
                            
                            endif else begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                   READF, inunit, params 
                                FREE_LUN, inunit
                                    
                                ;guardar nuevos valores
                                params[1] = 4

                                GET_LUN, outunit
                                openw,  outunit, filename
                                PRINTF, outunit, params
                                close,  outunit
                           endelse
        
        endcase
        'APPLYCOLORPARAM5':begin
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                           fUpdate = 0b

                           vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep())
                           if(FILE_TEST(strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+'_Skeleton_' + path_sep()+'_Params' + path_sep())
                           
                            filename = strCompress(vPath+strcompress('_Skeleton_' + path_sep()+'_Params' + path_sep() +'ColoredParams_' + '.sav', /rem))

                            flag = FILE_TEST(filename)
                            params = [0,0] ; usar color... y cual param
                            if flag eq 0 then begin
                            ; crear
                              GET_LUN, outunit
                              openw,  outunit, filename
                              PRINTF, outunit, params
                              close,  outunit
                            
                            endif else begin
                                GET_LUN, inunit
                                openr, inunit, filename
                                   READF, inunit, params 
                                FREE_LUN, inunit
                                    
                                ;guardar nuevos valores
                                params[1] = 5

                                GET_LUN, outunit
                                openw,  outunit, filename
                                PRINTF, outunit, params
                                close,  outunit
                           endelse
        
        endcase

       'FLIPANGLES':begin
                            widget_control, stack_tlb, get_uValue = stackState, /no_copy
                             (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                            widget_control, stack_tlb, set_uValue = stackState, /no_copy
                            vPath   = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
                            vFolder = strCompress(vPath+strcompress('_Tracking' + path_sep()   +'_Params' + path_sep(), /rem))
                            s_FixHorizontalFlipAngles, vFolder = vFolder  
         endcase
       'UPDATEROISTACKTRACKPARAMSONOFF':state.fUpdateStackTrackParams = s_ToggleButtonOnOffState(state.wUpdateStackTrackParamsOnOff)
       'RESTOREROIGROUPFROMIMAGEMASK':begin
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           imageStackInfoObject = *stackState.pimageStackInfoObject
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                           ; get Cluster Mask & Image
                        clusterMask = imageStackInfoObject->getSelectedClusterMask(tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos)
                        intImage = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)

                           ; initialize new ROI-Group Object
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oROIGroup = s_initROI2DGroupObjFromImageObj(stackState = stackState, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                                                               clusPos = clusPos, mask = clusterMask)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                           ; add ROI-Objects
                        widget_control, ev.top, set_uValue = state, /no_copy
                           oROIGroup->addROIObjectsFromSegImage, maskImage = clusterMask, intImage = intImage
                        widget_control, ev.top, get_uValue = state, /no_copy

                           ; transfer ROI-Param-Container
                        o2DParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                                 save, o2DParamContainer, filename = s_getPathForSystem()+'obj.tmp'
                                 restore, s_getPathForSystem()+'obj.tmp', restored_objects = o2DParamContainer, /relaxed
                        oROIGroup->setParamContainer, object = o2DParamContainer[0]

                           ; set new poCurrROI2DGroup
                        obj_destroy, *state.poCurrROI2DGroup
                        ptr_free, state.poCurrROI2DGroup
                        state.poCurrROI2DGroup = ptr_new(oROIGroup, /no_copy)

                        if (state.fRestoreAndCalculate) then begin
                             ; calculate poCurrROI2DGroup-Parameters
                           widget_control, ev.top, set_uValue = state, /no_copy
                             widget_control, ev.id, set_uValue = 'CALCULATEALLROIPARAMETERS'
                             s_ROI_ObjectManipulator_Window_Button_Event, ev
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif

                        widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.id, set_uValue = 'SAVEROIGROUP'
                             s_ROI_ObjectManipulator_Window_Button_Event, ev
                           widget_control, ev.id, set_uValue = 'RESTOREROIGROUPFROMIMAGEMASK'
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'APPLYTOALLZSLICESANDCHANNELSANDTIMES':begin
                        if ((selROIGroupObj eq 'oROI2DGroup') or (state.fApplyToROI2DGroups)) then zSliceNumber = totalZNum-1 else zSliceNumber = 0
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
                        if ((selROIGroupObj eq 'oROI2DGroup') or (state.fApplyToROI2DGroups)) then zSliceNumber = totalZNum-1 else zSliceNumber = 0
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
                        if ((selROIGroupObj eq 'oROI2DGroup') or (state.fApplyToROI2DGroups)) then zSliceNumber = totalZNum-1 else zSliceNumber = 0
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
                        if ((selROIGroupObj eq 'oROI2DGroup') or (state.fApplyToROI2DGroups)) then zSliceNumber = totalZNum-1 else zSliceNumber = 0
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
       'XDATAHISTONOFF':begin    ; Switch Flag (1/0 ON/OFF) for the View-Histogram-Window
          state.fXObjHistControl = s_ToggleButtonOnOffState(state.wXDataHistOnOff)
          if (state.fXObjHistControl eq 0) then begin
             if obj_valid(*state.poCurrROI2DGroup) then begin
                poDummy = state.poCurrROI2DGroup
                widget_control, ev.top, set_uValue = state, /no_copy
                  *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
                widget_control, ev.top, get_uValue = state, /no_copy
             endif
             if obj_valid(*state.poCurrROI3DGroup) then begin
                poDummy = state.poCurrROI3DGroup
                widget_control, ev.top, set_uValue = state, /no_copy
                  *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
                widget_control, ev.top, get_uValue = state, /no_copy
             endif
             child_XDataHist_tlb = state.child_XDataHist_tlb
             child_ObjViewWindow_tlb = state.child_ObjViewWindow_tlb
             allHistsOff = (state.fYObjHistControl + state.fZObjHistControl) eq 0
             widget_control, ev.id, set_uValue = uValue, /no_copy
             widget_control, ev.top, set_uValue = state, /no_copy
                if widget_info(child_XDataHist_tlb, /valid) then s_HistSingleParamPlot_Window_cleanUp, child_XDataHist_tlb
                if ((allHistsOff) and widget_info(child_ObjViewWindow_tlb, /valid)) then widget_control, child_ObjViewWindow_tlb, /destroy
             widget_control, ev.id, get_uValue = uValue, /no_copy
             widget_control, ev.top, get_uValue = state, /no_copy
             state.child_XDataHist_tlb = -1
          endif
        endcase
       'YDATAHISTONOFF':begin    ; Switch Flag (1/0 ON/OFF) for the View-Histogram-Window
          state.fYObjHistControl = s_ToggleButtonOnOffState(state.wYDataHistOnOff)
          if (state.fYObjHistControl eq 0) then begin
              child_YDataHist_tlb = state.child_YDataHist_tlb
              child_ObjViewWindow_tlb = state.child_ObjViewWindow_tlb
              allHistsOff = (state.fXObjHistControl + state.fZObjHistControl) eq 0
              widget_control, ev.id, set_uValue = uValue, /no_copy
              widget_control, ev.top, set_uValue = state, /no_copy
                 if widget_info(child_YDataHist_tlb, /valid) then s_HistSingleParamPlot_Window_cleanUp, child_YDataHist_tlb
                 if ((allHistsOff) and (widget_info(child_ObjViewWindow_tlb, /valid))) then widget_control, child_ObjViewWindow_tlb, /destroy
              widget_control, ev.id, get_uValue = uValue, /no_copy
              widget_control, ev.top, get_uValue = state, /no_copy
              state.child_YDataHist_tlb = -1
          endif
        endcase
       'ZDATAHISTONOFF':begin    ; Switch Flag (1/0 ON/OFF) for the View-Histogram-Window
          state.fZObjHistControl = s_ToggleButtonOnOffState(state.wZDataHistOnOff)
          if (state.fZObjHistControl eq 0) then begin
              child_ZDataHist_tlb = state.child_ZDataHist_tlb
              child_ObjViewWindow_tlb = state.child_ObjViewWindow_tlb
              allHistsOff = (state.fXObjHistControl + state.fYObjHistControl) eq 0
              widget_control, ev.id, set_uValue = uValue, /no_copy
              widget_control, ev.top, set_uValue = state, /no_copy
                 if widget_info(child_ZDataHist_tlb, /valid) then s_HistSingleParamPlot_Window_cleanUp, child_ZDataHist_tlb
                 if ((allHistsOff) and (widget_info(child_ObjViewWindow_tlb, /valid))) then widget_control, child_ObjViewWindow_tlb, /destroy
              widget_control, ev.id, get_uValue = uValue, /no_copy
              widget_control, ev.top, get_uValue = state, /no_copy
              state.child_ZDataHist_tlb = -1
          endif
        endcase
       'XYGROUPDATAPLOTONOFF':begin        ; Switch Flag (1/0 ON/OFF) for the View-Group-Data-Plot
          state.fXYGroupDataPlotControl = s_ToggleButtonOnOffState(state.wXYGroupDataPlotOnOff)
          if (state.fXYGroupDataPlotControl eq 0) then begin
              child_GroupDataPlot_tlb = state.child_GroupDataPlot_tlb
              widget_control, ev.id, set_uValue = uValue, /no_copy
              widget_control, ev.top, set_uValue = state, /no_copy
                 if widget_info(child_GroupDataPlot_tlb, /valid) then s_GroupParamPlot_Window_cleanUp, child_GroupDataPlot_tlb
              widget_control, ev.id, get_uValue = uValue, /no_copy
              widget_control, ev.top, get_uValue = state, /no_copy
              state.child_GroupDataPlot_tlb = -1
          endif
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
              case selROIGroupObj of
                 'oROI2DGroup':poDummy = state.poCurrROI2DGroup
                 'oROI3DGroup':poDummy = state.poCurrROI3DGroup
              endcase
              widget_control, ev.top, set_uValue = state, /no_copy
                 (*poDummy)->calculateGroupParameters, selROIParamName = strMid(selROIObjSubParam, 6, strlen(selROIObjSubParam)-6), stack_tlb = stack_tlb
              widget_control, ev.top, get_uValue = state, /no_copy

              if state.fParamFilter then begin
                 widget_control, ev.top, set_uValue = state, /no_copy
                   widget_control, ev.id, set_uValue = 'APPLYSELECTEDPARAMETERFILTER'
                   s_ROI_ObjectManipulator_Window_Button_Event, ev
                   widget_control, ev.id, set_uValue = 'CALCULATESELECTEDPARAMETER'
                 widget_control, ev.top, get_uValue = state, /no_copy
              endif
          endif
       endcase
       'CALCULATEALLROIPARAMETERS':begin
                     case selROIGroupObj of
                        'oROI2DGroup':poDummy = state.poCurrROI2DGroup
                        'oROI3DGroup':poDummy = state.poCurrROI3DGroup
                     endcase
                     widget_control, ev.top, set_uValue = state, /no_copy
                      (*poDummy)->calculateGroupParameters, stack_tlb = stack_tlb
                     widget_control, ev.top, get_uValue = state, /no_copy

                        if state.fParamFilter then begin
                            widget_control, ev.top, set_uValue = state, /no_copy
                               s_ROIOM_UpdateWidgets, ev.top
                               widget_control, ev.id, set_uValue = 'APPLYSELECTEDPARAMETERFILTER'
                               s_ROI_ObjectManipulator_Window_Button_Event, ev
                               widget_control, ev.id, set_uValue = 'CALCULATEALLROIPARAMETERS'
                            widget_control, ev.top, get_uValue = state, /no_copy
                        endif

                        widget_control, state.wListSelectROIGroupParams, get_uValue = uValueList, /no_copy
                           uValueList.active = ROIObjParamPos
                        widget_control, state.wListSelectROIGroupParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy
                        widget_control, state.wListSelectROIGroupSubParams, get_uValue = uValueList, /no_copy
                           uValueList.active = ROIObjSubParamPos
                        widget_control, state.wListSelectROIGroupSubParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy
                        if ( (where(tag_names(ev) eq 'SILENT'))[0] ne -1) then fUpdate = 0b
          endcase
       'APPLYSELECTEDPARAMETERFILTER':begin
                        if (strMid(selROIObjSubParam, 0, 5) eq '(on )') then begin

                             ; is Param 3D or 2D?
                           f3D = (strPos(selROIObjSubParam, '3D') ne -1)

                             ;get selected oParamContainer and ROIParam
                           if f3D then oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer() $
                             else oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                           oROIParam = oParamContainer->get(position = ROIObjParamPos)

                           if (((*(oROIParam->getpParamStruct())).type eq '3D ROI-Parameter-Method') or $
                               ((*(oROIParam->getpParamStruct())).type eq 'Single ROI-Parameter-Method') or $
                               ((*(oROIParam->getpParamStruct())).type eq 'Inter ROI-Parameter-Method')) then begin

                             if f3D then (*state.poCurrROI3DGroup)->selectObjectsInThresholdIntervals, selROIParamName = strMid(selROIObjSubParam, 6, strlen(selROIObjSubParam)-6) $
                                else (*state.poCurrROI2DGroup)->selectObjectsInThresholdIntervals, selROIParamName = strMid(selROIObjSubParam, 6, strlen(selROIObjSubParam)-6)

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
                        endif
                        state.fUpDateROI3DGroup = 1b
                        state.fUpDateROI3DGroupProperties = 1b
                        if ((where(tag_names(ev) eq 'SILENT'))[0] ne -1) then fUpdate = 0b
          endcase
       'APPLYALLPARAMETERFILTERS':begin
                        case selROIGroupObj of
                           'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                           'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                        endcase

                        for i = 0, (oParamContainer->count())-1 do begin
                           widget_control, state.wListSelectROIGroupParams, get_uValue = uValueList, /no_copy
                             uValueList.active = i
                           widget_control, state.wListSelectROIGroupParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy

                           case selROIGroupObj of
                             'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                             'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                           endcase

                           oROIParam = oParamContainer->get(position = i)
                           widget_control, state.wListSelectROIGroupSubParams, get_uValue = uValueSub, /no_copy
                              ROISubParamList = paramNameListSet( *(*(oROIParam->getpParamStruct())).pNames, *(*(oROIParam->getpParamStruct())).pActive)
                              *uValueSub.value = ROISubParamList
                              uValueSub.active = uValueSub.active < (n_elements(*uValueSub.value)-1) > 0
                           widget_control, state.wListSelectROIGroupSubParams, set_list_select = uValueSub.active, set_value = *uValueSub.value, set_uValue = uValueSub, /no_copy

                           whROIParamActive = where(*(*(oROIParam->getpParamStruct())).pActive)
                           if (whROIParamActive[0] ne -1) then begin
                             for j = 0, n_elements(whROIParamActive)-1 do begin

                              widget_control, state.wListSelectROIGroupSubParams, get_uValue = uValueList, /no_copy
                                 uValueList.active = whROIParamActive[j]
                              widget_control, state.wListSelectROIGroupSubParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy

                              widget_control, ev.top, set_uValue = state, /no_copy
                                 widget_control, ev.id, set_uValue = 'APPLYSELECTEDPARAMETERFILTER'
                                    s_ROI_ObjectManipulator_Window_Button_Event, ev
                                 widget_control, ev.id, set_uValue = 'APPLYALLPARAMETERFILTERS'
                             widget_control, ev.top, get_uValue = state, /no_copy

                             endfor
                           endif
                        endfor

                        widget_control, state.wListSelectROIGroupParams, get_uValue = uValueList, /no_copy
                           uValueList.active = ROIObjParamPos
                        widget_control, state.wListSelectROIGroupParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy
                        widget_control, state.wListSelectROIGroupSubParams, get_uValue = uValueList, /no_copy
                           uValueList.active = ROIObjSubParamPos
                        widget_control, state.wListSelectROIGroupSubParams, set_list_select = uValueList.active, set_uValue = uValueList, /no_copy
                        state.fUpDateROI3DGroup = 1b
                        if ((where(tag_names(ev) eq 'SILENT'))[0] ne -1) then fUpdate = 0b
          endcase
       'DELETE':begin
                         case state.fListSelect of
                           3:begin
                                 widget_control, state.wListSelectROIGraphicModels, get_uValue = uListValue, /no_copy
                                    if ((*uListValue.value)[0] ne '-NO SELECTION-') then begin
                                       if (strPos((*uListValue.value)[uListValue.active],'3D') ne -1) then state.fUpDateROI3DGroup = 1b
                                       case uListValue.active of
                                          0:if (n_elements(*uListValue.value) eq 1) then *uListValue.value = '-NO SELECTION-' else *uListValue.value = (*uListValue.value)[1:*]
                                          n_elements(*uListValue.value)-1:*uListValue.value = (*uListValue.value)[0:n_elements(*uListValue.value)-2]
                                          else:*uListValue.value = [(*uListValue.value)[0:uListValue.active-1], (*uListValue.value)[uListValue.active+1:*]]
                                       endcase
                                       uListValue.active = (uListValue.active-1) > 0
                                    endif
                                 widget_control, state.wListSelectROIGraphicModels, set_list_select = uListValue.active, set_value = *uListValue.value, set_uValue = uListValue, /no_copy
                           endcase
                           else:begin
                              case selROIGroupObj of
                                 'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                                 'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                              endcase

                              count = oParamContainer->count()-1
                              if ((ROIObjParamPos ge 0) and (ROIObjParamPos le count)) then begin
                                 obj_destroy, oParamContainer->get(position = (ROIObjParamPos > 0))
                                 oParamContainer->remove, position = (ROIObjParamPos > 0)

                                 widget_control, state.wListSelectROIGroupParams, get_uValue = uvalueFileParameter, /no_copy
                                 if (ROIObjParamPos eq count) then uvalueFileParameter.active = (ROIObjParamPos - 1)>0 else $
                                   uvalueFileParameter.active = (ROIObjParamPos)<count
                                 widget_control, state.wListSelectROIGroupParams, set_uValue = uvalueFileParameter, /no_copy
                              endif
                           endcase
                         endcase
                         state.fUpDateROI3DGroup = 1b
                         state.fUpDateROI3DGroupProperties = 1b
         endcase
       'MOVEUP':begin
                        case selROIGroupObj of
                          'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                          'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                        endcase

                        count = oParamContainer->count()-1
                        if (ROIObjParamPos gt 0) then begin
                          oParamContainer->move, ROIObjParamPos, ROIObjParamPos-1
                          widget_control, state.wListSelectROIGroupParams, get_uValue = uvalueFileParameter, /no_copy
                           uvalueFileParameter.active = ROIObjParamPos-1
                          widget_control, state.wListSelectROIGroupParams, set_uValue = uvalueFileParameter, /no_copy
                        endif
         endcase
       'MOVEDOWN':begin
                        case selROIGroupObj of
                          'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                          'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                        endcase

                        count = oParamContainer->count()-1
                        if (ROIObjParamPos lt count) then begin
                          oParamContainer->move, ROIObjParamPos, ROIObjParamPos+1
                          widget_control, state.wListSelectROIGroupParams, get_uValue = uvalueFileParameter, /no_copy
                           uvalueFileParameter.active = ROIObjParamPos+1
                          widget_control, state.wListSelectROIGroupParams, set_uValue = uvalueFileParameter, /no_copy
                        endif
         endcase
       'USE2DROIOBJECTSONOFF':begin
                        state.fUse2DROIObjects = s_ToggleButtonOnOffState(state.wUse2DROIObjectsOnOff)
                        if (state.fUse2DROIObjects) then begin
                           if (state.fUseSavedClusterMasks) then state.fUseSavedClusterMasks = s_ToggleButtonOnOffState(state.wUseSavedClusterMasksOnOff)
                           if (state.fUse3DROISlices) then state.fUse3DROISlices = s_ToggleButtonOnOffState(state.wUse3DROISlicesOnOff)
                        endif else state.fUse3DROISlices = s_ToggleButtonOnOffState(state.wUse3DROISlicesOnOff)
                        fUpdate = 0b
         endcase
       'USESAVEDCLUSTERMASKSONOFF':begin
                        state.fUseSavedClusterMasks = s_ToggleButtonOnOffState(state.wUseSavedClusterMasksOnOff)
                        if (state.fUseSavedClusterMasks) then begin
                           if (state.fUse2DROIObjects) then state.fUse2DROIObjects = s_ToggleButtonOnOffState(state.wUse2DROIObjectsOnOff)
                           if (state.fUse3DROISlices) then state.fUse3DROISlices = s_ToggleButtonOnOffState(state.wUse3DROISlicesOnOff)
                        endif else state.fUse2DROIObjects = s_ToggleButtonOnOffState(state.wUse2DROIObjectsOnOff)
                        fUpdate = 0b
         endcase
       'USE3DROISLICESONOFF':begin
                        state.fUse3DROISlices = s_ToggleButtonOnOffState(state.wUse3DROISlicesOnOff)
                        if (state.fUse3DROISlices) then begin
                           if (state.fUse2DROIObjects) then state.fUse2DROIObjects = s_ToggleButtonOnOffState(state.wUse2DROIObjectsOnOff)
                           if (state.fUseSavedClusterMasks) then state.fUseSavedClusterMasks = s_ToggleButtonOnOffState(state.wUseSavedClusterMasksOnOff)
                        endif else state.fUse2DROIObjects = s_ToggleButtonOnOffState(state.wUse2DROIObjectsOnOff)
                        fUpdate = 0b
         endcase
       'SAVECLUSTERMASKSFROMROIOBJECTS':begin
                        state.fSaveClusterMasksFromROIObject = s_ToggleButtonOnOffState(state.wSaveClusterMasksFromROIObjectOnOff)
                        fUpdate = 0b
         endcase
       'UPDATE2DROIOBJECTWITHIMAGEPARAMSONOFF':begin
                        state.fUpdate2DROIObjectsWithImageParamsControl = s_ToggleButtonOnOffState(state.wUpdate2DROIObjectsWithImageParamsOnOff)
                        fUpdate = 0b
         endcase
       'UPDATE3DROIWITHGROWTHFACTORNOFF':begin
                        state.fUpdate3DROIWithGrowthFactor = s_ToggleButtonOnOffState(state.wUpdate3DROIWithGrowthFactorOnOff)
                        fUpdate = 0b
         endcase
       'KEEPVISUALIZATIONPARAMSONOFF':begin
                        state.fKeepVisualParams = s_ToggleButtonOnOffState(state.wKeepVisualParamsOnOff)
                        fUpdate = 0b
         endcase
       'KEEPSELECTEDROIPARAMFILTERSONOFF':begin
                        state.fKeepSelectROIParamFilters = s_ToggleButtonOnOffState(state.wKeepSelectROIParamsFiltersOnOff)
                        fUpdate = 0b
         endcase
       'KEEPSELECTEDROIPARAMSONOFF':begin
                        state.fKeepSelectROIParamsControl = s_ToggleButtonOnOffState(state.wKeepSelectROIParamsOnOff)
                        if (state.fComplementROIParamsControl) then state.fComplementROIParamsControl = s_ToggleButtonOnOffState(state.wComplementROIParamsOnOff)
                        if ((state.fKeepSelectROIParamsControl) and not(state.fRestoreAndCalculate) ) then state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        if (not(state.fKeepSelectROIParamsControl) and (state.fRestoreAndCalculate) ) then state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        if ((state.fKeepSelectROIParamsControl) and not(state.fSaveResultsAutomatically) ) then state.fSaveResultsAutomatically = s_ToggleButtonOnOffState(state.wSaveResultsAutomaticallyOnOff)
                        if (not(state.fKeepSelectROIParamsControl) and (state.fSaveResultsAutomatically) ) then state.fSaveResultsAutomatically = s_ToggleButtonOnOffState(state.wSaveResultsAutomaticallyOnOff)
                        fUpdate = 0b
         endcase
       'COMPLEMENTROIPARAMSONOFF':begin
                        state.fComplementROIParamsControl = s_ToggleButtonOnOffState(state.wComplementROIParamsOnOff)
                        if (state.fKeepSelectROIParamsControl) then state.fKeepSelectROIParamsControl = s_ToggleButtonOnOffState(state.wKeepSelectROIParamsOnOff)
                        if ((state.fComplementROIParamsControl) and not(state.fRestoreAndCalculate) ) then state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        if (not(state.fComplementROIParamsControl) and (state.fRestoreAndCalculate) ) then state.fRestoreAndCalculate = s_ToggleButtonOnOffState(state.wRestoreAndCalculateOnOff)
                        if ((state.fComplementROIParamsControl) and not(state.fSaveResultsAutomatically) ) then state.fSaveResultsAutomatically = s_ToggleButtonOnOffState(state.wSaveResultsAutomaticallyOnOff)
                        if (not(state.fComplementROIParamsControl) and (state.fSaveResultsAutomatically) ) then state.fSaveResultsAutomatically = s_ToggleButtonOnOffState(state.wSaveResultsAutomaticallyOnOff)
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
       'SAVERESULTSAUTOMATICALLYONOFF':begin
                        state.fSaveResultsAutomatically = s_ToggleButtonOnOffState(state.wSaveResultsAutomaticallyOnOff)
                        fUpdate = 0b
         endcase
       'APPLYTOROI2DGROUPSONOFF':begin
                        state.fApplyToROI2DGroups = s_ToggleButtonOnOffState(state.wApplyToROI2DGroupsOnOff)
                        fUpdate = 0b
         endcase
       'APPLYTOROI3DGROUPSONOFF':begin
                        state.fApplyToROI3DGroups = s_ToggleButtonOnOffState(state.wApplyToROI3DGroupsOnOff)
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
       ; FASL REFERENCE SET INVISIBLE PLANES
       'ONOFFVISIBLEPLANEBASE':begin
                         state.fParamPB = s_ToggleButtonOnOffState(state.wParamPBOnOff)
                         fUpdate = 0b
                        
                         oPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Base')
                              
                         if (obj_valid(oPlaneModel)) then begin  
                            if(state.fParamPB eq 1) then oPlaneModel->setproperty, HIDE = 0
                            if(state.fParamPB eq 0) then oPlaneModel->setproperty, HIDE = 1
                         endif                          
         endcase
       'ONOFFVISIBLEPLANEORTH':begin
                         state.fParamPO = s_ToggleButtonOnOffState(state.wParamPOOnOff)
                         fUpdate = 0b
                        
                         oPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Orthogonal')
                              
                         if (obj_valid(oPlaneModel)) then begin  
                            if(state.fParamPB eq 1) then oPlaneModel->setproperty, HIDE = 0
                            if(state.fParamPB eq 0) then oPlaneModel->setproperty, HIDE = 1
                         endif                          
         endcase
       'ONOFFVISIBLEPLANECOMP':begin
                         state.fParamPC = s_ToggleButtonOnOffState(state.wParamPCOnOff)
                         fUpdate = 0b

                         oPlaneModel = *(state.poCurrROIGraphicModel)->getByName('3D Plane Model Complementary')
                              
                         if (obj_valid(oPlaneModel)) then begin  
                            if(state.fParamPB eq 1) then oPlaneModel->setproperty, HIDE = 0
                            if(state.fParamPB eq 0) then oPlaneModel->setproperty, HIDE = 1
                         endif                          
         endcase
        
       'ACTIVATEALLSTACKSTRACKONOFF':begin
                        state.fStackTrackCollectAllParam = s_ToggleButtonOnOffState(state.wStackTrackCollectAllParamOnOff)
                        fUpdate = 0b
         endcase
       'SETXOBJAXIS':begin
                        state.fXYZObjectSensitive = 0
                        widget_control, state.wXYZAxisButton, set_value = 'X-Axis |-> '
         endcase
       'SETYOBJAXIS':begin
                        state.fXYZObjectSensitive = 1
                        widget_control, state.wXYZAxisButton, set_value = 'Y-Axis |-> '
         endcase
       'SETZOBJAXIS':begin
                        state.fXYZObjectSensitive = 2
                        widget_control, state.wXYZAxisButton, set_value = 'z-Axis |-> '
         endcase
       'SETXGROUPAXIS':begin
                        state.fXYZGroupSensitive = 0
                        widget_control, state.wXYZAxisButton, set_value = 'X-Axis |-> '
         endcase
       'SETYGROUPAXIS':begin
                        state.fXYZGroupSensitive = 1
                        widget_control, state.wXYZAxisButton, set_value = 'Y-Axis |-> '
         endcase
       'SETZGROUPAXIS':begin
                        state.fXYZGroupSensitive = 2
                        widget_control, state.wXYZAxisButton, set_value = 'z-Axis |-> '
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
       'ROITRACKWINDOWONOFF':begin
                        state.fROITrackWindow = s_ToggleButtonOnOffState(state.wROITrackWindowOnOff)
                             if state.fROITrackWindow then begin
                                widget_control, ev.top, tlb_get_size = baseSize, tlb_get_Offset = baseOffset
                                widget_control, ev.top, set_uValue = state, /no_copy
                                s_ROI_TrackManipulator_Window, groupLeader = ev.top, basePosition = (baseSize+baseOffset), wTopBase=wTopBase
                                save, wTopBase, FILENAME='C:\RSI\trackWidgetNumber.sav'
                                widget_control, ev.top, get_uValue = state, /no_copy
                             endif else begin
                                if widget_info(state.child_ROITrackWindow_tlb, /valid_id) then begin
                                widget_control, state.child_ROITrackWindow_tlb, /destroy
                                state.child_ROITrackWindow_tlb = -1l
                               endif
                             endelse
                        fUpdate = 0b
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
       'ROIPARAMWINDOWONOFF':begin
                        state.fROIParamWindow = s_ToggleButtonOnOffState(state.wROIParamWindowOnOff)
                             if state.fROIParamWindow then begin
                                widget_control, ev.top, tlb_get_size = baseSize, tlb_get_Offset = baseOffset
                                widget_control, ev.top, set_uValue = state, /no_copy
                                 s_ROI_ParameterManipulator_Window, groupLeader = ev.top, application_tlb = application_tlb, basePosition = (baseSize+baseOffset)
                                widget_control, ev.top, get_uValue = state, /no_copy
                                state.child_ROIParamWindow_tlb = application_tlb
                             endif else begin
                                if widget_info(state.child_ROIParamWindow_tlb, /valid_id) then begin
                                widget_control, state.child_ROIParamWindow_tlb, /Destroy
                                state.child_ROIParamWindow_tlb = -1l
                               endif
                             endelse
                        fUpdate = 0b
         endcase
       'ROICONNECTSTACKPARAMONOFF':begin
                        state.fROIConnectStackParam = s_ToggleButtonOnOffState(state.wROIConnectStackParamOnOff)
                        if state.fROIConnectStackParam then begin
                           wTopBase = state.wTopBase
                           widget_control, ev.top, set_uValue = state, /no_copy
                             s_ROIOM_passROIStackTrack, wTopBase
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
                        fUpdate = 0b
         endcase
       'MOVIEUSETIMECOUNTERONOFF':begin
                        state.fMovieUseTimeCounter = s_ToggleButtonOnOffState(state.wMovieUseTimeCounterOnOff)
                        fUpdate = 0b
         endcase
       'MOVIEKEEPOBJECTPOSITIONONOFF':begin
                        state.fMovieKeepObjectPosition = s_ToggleButtonOnOffState(state.wMovieKeepObjectPositionOnOff)
                        fUpdate = 0b
         endcase
       'MOVIEKEEPGALLERYPOSITIONONOFF':begin
                        state.fMovieKeepGalleryPosition = s_ToggleButtonOnOffState(state.wMovieKeepGalleryPositionOnOff)
                        fUpdate = 0b
         endcase
       'MOVIEKEEPSPHEREPOSITIONONOFF':begin
                        state.fMovieKeepSpherePosition = s_ToggleButtonOnOffState(state.wMovieKeepSpherePositionOnOff)
                        fUpdate = 0b
         endcase
       'MOVIEKEEPKEEPSINGLEPARAMHISTONOFF':begin
                        state.fMovieIncludeSingleParamHist = s_ToggleButtonOnOffState(state.wMovieIncludeSingleParamHistOnOff)
                        fUpdate = 0b
         endcase
       'MOVIEKEEPMIXEDPARAMPLOTONOFF':begin
                        state.fMovieMixedParamPlot = s_ToggleButtonOnOffState(state.wMovieMixedParamPlotOnOff)
                        fUpdate = 0b
         endcase
       'MOVIEONOFF':begin
                        state.fMovieControl = s_ToggleButtonOnOffState(state.wMovieOnOff)
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
    if fUpdate then s_ROIOM_UpdateWidgets, ev.top
end


pro s_ROIOM_AddParameterMethod_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    widget_control, ev.id, get_value = selROIParameter
    oROIParam = obj_new(selROIParameter)
    pParamStruct = oROIParam->getpParamStruct()
    widget_control, ev.top, get_uValue = state, /no_copy
       case (*pParamStruct).type of
         '3D ROI-Parameter-Method':begin
              if obj_valid(*state.poCurrROI3DGroup) then if obj_isa(*state.poCurrROI3DGroup,'C_sROI3DGroupObj') then begin
                 o3DParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                 o3DParamContainer->add, oROIParam
              endif
         endcase
         else:begin
                 o2DParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                 o2DParamContainer->add, oROIParam
         endcase
       endcase
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ROIOM_UpdateWidgets, ev.top
end


pro s_ROIOM_AddGraphicModel_Event, ev
   widget_control, ev.id, get_value = model
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, state.wListSelectROIGraphicModels, get_uValue = uValue, /no_copy
        whModel = where(*uValue.value eq model)
        if (whModel[0] eq -1) then begin
           *uValue.value = [*uValue.value, model]
           if (strPos(model,'3D') ne -1) then state.fUpDateROI3DGroup = 1b
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
        state.fUpDateROI3DGroup = 1b
        state.fUpDateROI3DGroupProperties = 1b
      widget_control, state.wListSelectROIGraphicModels, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy
   widget_control, ev.top, set_uValue = state, /no_copy
   s_ROIOM_UpdateWidgets, ev.top
end


pro s_ROIOM_List_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, selROIGroupObj = selROIGroupObj
    fUpdateViewWin = 0b
    widget_control, ev.id, get_uValue = uValue, /no_copy
    uValue.active = ev.index

    case uValue.name of
       'Available ROI-Groups':begin
          fListSelect = 0
          if ((*uValue.value)[ev.index] ne '-NO SELECTION-') then begin
            case ev.Clicks of
                1:
                2:
                else:
             endcase
          endif
       endcase
       'Selected ROI-Parameter-Objects':begin
          fListSelect = 1
          if ((*uValue.value)[ev.index] ne '-NO SELECTION-') then begin
             case ev.Clicks of
                1:fUpdateViewWin = 1b
                2:begin
                     widget_control, ev.top, get_uValue = state, /no_copy

                        case selROIGroupObj of
                          'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                          'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                        endcase

                        title = state.sTopBaseTitle
                        paramTableUValue = {groupLeader:ev.top,$
                                                 sTopBaseTitle:title,$
                                                 callName:'s_ROI_ObjectManipulator_Window',$
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
                             s_ROIOM_ParamTableWidget, paramTableUValue = paramTableUValue
                             state.child_ParamTableWidget_tlb = paramTableUValue.wTopBaseID
                          endif
                        widget_control, ev.id, get_uValue = uValue, /no_copy
                     widget_control, ev.top, set_uValue = state, /no_copy
                endcase
                else:
             endcase
          endif
       endcase
       'Selected ROI-Sub-Parameters':begin
          fListSelect = 2
          if ((*uValue.value)[ev.index] ne '-NO SELECTION-') then begin
             case ev.Clicks of
                1:fUpdateViewWin = 1b
                2:begin
                     widget_control, ev.top, get_uValue = state, /no_copy

                     widget_control, state.wListSelectROIGroupParams, get_uValue = uValueGroupParam, /no_copy
                       selectParamIndex = uValueGroupParam.active
                     widget_control, state.wListSelectROIGroupParams, set_uValue = uValueGroupParam, /no_copy

                     case selROIGroupObj of
                       'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
                       'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                     endcase
                     title = state.sTopBaseTitle
                     paramTableUValue = {groupLeader:ev.top,$          ; widget Group LeaderID
                                             sTopBaseTitle:title,$
                                             callName:'s_ROI_SubParam',$  ; this widget Pro Name
                                             pwIDParamNumList:ptr_new(),$  ; pointer to corellate created widget ID's and Parameter Numbers
                                             wTopBaseID:-1,$
                                             oContainer:oParamContainer,$ ; Filter Container Object
                                             Index:selectParamIndex,$      ; Index of Filter Object in Filter Container Object
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
       'Selected Graphic Models':begin
          fListSelect = 3
          if ((*uValue.value)[ev.index] ne '-NO SELECTION-') then begin
             case ev.Clicks of
                1:fUpdateViewWin = 1b
                2:begin
                       widget_control, ev.top, get_uValue = state, /no_copy
                       case strPos((*uValue.value)[ev.index],'3D') of
                           -1:begin
                              paramTableUValue = {groupLeader:state.wTopBase,$
                                                            paramNames:(*state.poCurrROI2DGroup)->getParameterNameList(),$
                                                            paramAsStruct:(*state.poCurrROI2DGroup)->getParamAsStruct(),$
                                                            name:'oROI2DGroup',$
                                                            wTopBase:-1}
                                  s_ObjParamTableWidget, paramTableUValue = paramTableUValue
                              state.child_ROI2DGroupTableWidget_tlb = paramTableUValue.wTopBase
                             endcase
                           else:begin
                              paramTableUValue = {groupLeader:state.wTopBase,$
                                                            paramNames:(*state.poCurrROI3DGroup)->getParameterNameList(),$
                                                            paramAsStruct:(*state.poCurrROI3DGroup)->getParamAsStruct(),$
                                                            name:'oROI3DGroup',$
                                                            wTopBase:-1}
                                  s_ObjParamTableWidget, paramTableUValue = paramTableUValue
                              state.child_ROI3DGroupTableWidget_tlb = paramTableUValue.wTopBase
                           endcase
                        endcase
                        widget_control, ev.top, set_uValue = state, /no_copy
                endcase
                else:
             endcase
          endif
        endcase
    endcase
    widget_control, ev.id, set_uValue = uValue, /no_copy
    if fUpdateViewWin then begin
       widget_control, ev.top, get_uValue = state, /no_copy
         state.fListSelect = fListSelect
         state.fUpDateROI3DGroup = 1b
         state.fUpDateROI3DGroupProperties = 1b
         widget_control, state.stack_tlb, get_uValue = stateStack, /no_copy
            dummy = stateStack.child_ViewWindow_tlb
         widget_control, state.stack_tlb, set_uValue = stateStack, /no_copy
       widget_control, ev.top, set_uValue = state, /no_copy
       if widget_info(dummy, /valid) then zimage_colors, {redraw_image, top:dummy}
    endif
    s_ROIOM_UpdateWidgets, ev.top
end;-s_ROIOM_List_Event------------------------------------------------------------------


pro s_ROIOM_UpdateWidgets, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, selROIGroupObj = selROIGroupObj, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos,$
                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum

    widget_control, wTopBase, get_uValue = state, /no_copy

       if (n_elements(selROIGroupObj) le 0) then selROIGroupObj = 'oROI2DGroup'
       case selROIGroupObj of
         'oROI2DGroup':if obj_valid(*state.poCurrROI2DGroup) then oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
         'oROI3DGroup':if obj_valid(*state.poCurrROI3DGroup) then oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
       endcase

       ROIParamList = '-NO SELECTION-'
       if obj_valid(oParamContainer) then for i = 0, (oParamContainer->count())-1 do $
          ROIParamList = [ROIParamList, (*((oParamContainer->get(position = i))->getpParamStruct())).name]
       if (n_elements(ROIParamList) gt 1) then ROIParamList = ROIParamList[1:*]

       widget_control, state.wListSelectROIGroupParams, get_uValue = uValue, /no_copy
         *uValue.value = ROIParamList
         listSelect = uValue.active < (n_elements(*uValue.value)-1) > 0
       widget_control, state.wListSelectROIGroupParams, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy

       oROIParam = oParamContainer->get(position = listSelect)
       widget_control, state.wListSelectROIGroupSubParams, get_uValue = uValueSub, /no_copy
         if obj_valid(oROIParam) then $
            ROISubParamList = paramNameListSet( *(*(oROIParam->getpParamStruct())).pNames, *(*(oROIParam->getpParamStruct())).pActive) $
            else ROISubParamList = '-NO SELECTION-'
         *uValueSub.value = ROISubParamList
         uValueSub.active = uValueSub.active < (n_elements(*uValueSub.value)-1) > 0
       widget_control, state.wListSelectROIGroupSubParams, set_list_select = uValueSub.active, set_value = *uValueSub.value, set_uValue = uValueSub, /no_copy

       widget_control, state.wListSelectROIGraphicModels, get_uValue = uValue, /no_copy
         listSelect = uValue.active < (n_elements(*uValue.value)-1) > 0
       widget_control, state.wListSelectROIGraphicModels, set_list_select = listSelect, set_value = *uValue.value, set_uValue = uValue, /no_copy

         ; fUpdate ROI-Object Parameter Table Widgets
       if (widget_info(state.child_ROI2DGroupTableWidget_tlb, /valid) and obj_valid(*state.poCurrROI2DGroup)) then begin
         paramTableUValue = {groupLeader:state.wTopBase,$
                                 paramNames:(*state.poCurrROI2DGroup)->getParameterNameList(),$
                                 paramAsStruct:(*state.poCurrROI2DGroup)->getParamAsStruct(),$
                                 name:'oROIDGroup',$
                                 wTopBase:state.child_ROI2DGroupTableWidget_tlb }
         widget_control, wTopBase, set_uValue = state, /no_copy
            s_OPTW_Update, paramTableUValue = paramTableUValue
         widget_control, wTopBase, get_uValue = state, /no_copy
       endif
       if (widget_info(state.child_ROI3DGroupTableWidget_tlb, /valid) and obj_valid(*state.poCurrROI3DGroup)) then begin
         paramTableUValue = {groupLeader:state.wTopBase,$
                                 paramNames:(*state.poCurrROI3DGroup)->getParameterNameList(),$
                                 paramAsStruct:(*state.poCurrROI3DGroup)->getParamAsStruct(),$
                                 name:'oROI3DGroup',$
                                 wTopBase:state.child_ROI3DGroupTableWidget_tlb}
         widget_control, wTopBase, set_uValue = state, /no_copy
          s_OPTW_Update, paramTableUValue = paramTableUValue
         widget_control, wTopBase, get_uValue = state, /no_copy
       endif

       if widget_info(state.child_ParamWidget_tlb, /valid) then begin
         case selROIGroupObj of
          'oROI2DGroup':oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
          'oROI3DGroup':oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
         endcase
         tlb = state.child_ParamWidget_tlb
         widget_control, wTopBase, set_uValue = state, /no_copy
          s_ROIM_ParameterWidget_Refresh, tlb, oContainer = oParamContainer
         widget_control, wTopBase, get_uValue = state, /no_copy
       endif

         ;fUpdate oStackTrackContainer
       if state.fUpdateStackTrackParams then begin
         if not(ptr_valid(state.poStackTrackContainer)) then state.poStackTrackContainer = ptr_new(obj_new('C_sROIParam_StackTrackContainer'), /no_copy)
         if not(obj_valid(*state.poStackTrackContainer)) then begin
            ptr_free, state.poStackTrackContainer
            poStackTrackContainer = ptr_new(obj_new('C_sROIParam_StackTrackContainer'), /no_copy)
         endif
         if not(obj_isa(*state.poStackTrackContainer, 'C_sROIParam_StackTrackContainer')) then begin
            obj_destroy, *state.poStackTrackContainer
            ptr_free, state.poStackTrackContainer
            state.poStackTrackContainer = ptr_new(obj_new('C_sROIParam_StackTrackContainer'), /no_copy)
         endif
         currentStackTrackFileName = state.currentStackTrackFileName
         *state.poStackTrackContainer->getStackTrackContainer, stack_tlb = stack_tlb, currentStackTrackFileName = currentStackTrackFileName
         state.currentStackTrackFileName = currentStackTrackFileName
         *state.poStackTrackContainer->setTrackParameters, poCurrROI2DGroup = state.poCurrROI2DGroup,$
                                                           poCurrROI3DGroup = state.poCurrROI3DGroup,$
                                                           tPos = tPos,$
                                                           chPos = chPos,$
                                                           zPos = zPos,$
                                                           clusPos = clusPos,$
                                                           totalTNum = totalTNum,$
                                                           fAllParamControl = state.fStackTrackCollectAllParam

         oStackTrackContainer = *state.poStackTrackContainer
         save, oStackTrackContainer, filename = state.currentStackTrackFileName, /compress
         state.fRefreshGroupData = 1b
         state.fRefreshMixedData = 1b
         if (*state.poStackTrackContainer->getfAllParamControl() gt 0) then begin
            widget_control, state.wSaveStackTrackParamsAs, sensitive = 1
            widget_control, state.wSaveStackTrackParamObjAs, sensitive = 1
            widget_control, state.wSaveStackTrackParamObjAsInforme, sensitive = 1; Susana - Informe Vitalidad
            widget_control, state.wSaveStackTrackParamObjAsAllROIsVital, sensitive = 1; Susana - Informe Vitalidad solo coordenadas
            widget_control, state.wSaveStackTrackParamObjAsFragmentation, sensitive = 1; Susana - Informe Fragmentation
            ;widget_control, state.wSaveStackTrackParamObjAsROUNDCELLS, sensitive = 1; Susana - Informe ROUND CELLS
            ;widget_control, state.wSaveStackTrackParamObjAsInformeMARCIA, sensitive = 1; Susana - Informe Vitalidad
            ;widget_control, state.wSaveStackTrackParamObjAsInformeMARCIA2, sensitive = 1; Susana - Informe Vitalidad
            ;widget_control, state.wSaveStackTrackParamObjAsInformeCARO, sensitive = 1; Susana - Informe Vitalidad
         endif else begin
            widget_control, state.wSaveStackTrackParamsAs, sensitive = 0
            widget_control, state.wSaveStackTrackParamObjAs, sensitive = 0
            widget_control, state.wSaveStackTrackParamObjAsInforme, sensitive = 0; Susana - Informe Vitalidad
            widget_control, state.wSaveStackTrackParamObjAsAllROIsVital, sensitive = 0; Susana - Informe Vitalidad solo coordenadas
            widget_control, state.wSaveStackTrackParamObjAsFragmentation, sensitive = 0; Susana - Informe Fragmentation
            ;widget_control, state.wSaveStackTrackParamObjAsROUNDCELLS, sensitive = 0; Susana - Informe ROUND CELLS
            ;widget_control, state.wSaveStackTrackParamObjAsInformeMARCIA, sensitive = 0; Susana - Informe Vitalidad
            ;widget_control, state.wSaveStackTrackParamObjAsInformeMARCIA2, sensitive = 0; Susana - Informe Vitalidad
            ;widget_control, state.wSaveStackTrackParamObjAsInformeCARO, sensitive = 0; Susana - Informe Vitalidad
         endelse
       endif

       if widget_info(state.child_ROITrackWindow_tlb, /valid) then begin
          tlb = state.child_ROITrackWindow_tlb
          widget_control, wTopBase, set_uValue = state, /no_copy
             s_ROITM_Update, tlb
          widget_control, wTopBase, get_uValue = state, /no_copy
       endif

    widget_control, wTopBase, set_uValue = state, /no_copy, /upDate
    s_ROIOM_UpdateXYZButtons, wTopBase
    s_ROIOM_UpdateXYZPlots, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy, /upDate

         ; Movie Control
       if (state.fMovieControl and widget_info(state.child_ObjViewWindow_tlb, /valid)) then begin

         widget_control, state.child_ObjViewWindow_tlb, get_uValue = stateObjWin, /no_copy
          winMain = stateObjWin.win
          oScene = stateObjWin.oScene
          debug = stateObjWin.debug
          winMain->getProperty, dimension = mainDims, resolution = mainRes, color_model = cm, n_colors = icolors
          oClipboard = obj_new('IDLgrClipboard', dimensions = mainDims, resolution = mainRes, color_model = cm, n_colors = icolors)
         widget_control, state.child_ObjViewWindow_tlb, set_uValue = stateObjWin, /no_copy

         if (state.fMovieIncludeSingleParamHist and widget_info(state.child_XDataHist_tlb, /valid)) then begin
          widget_control, state.child_XDataHist_tlb, get_uValue = stateXHist, /no_copy
              wXHistTopBase = stateXHist.wTopBase
          widget_control, state.child_XDataHist_tlb, set_uValue = stateXHist, /no_copy
          oSingleHistView = s_HistSingleParamPlot_getHistViewObj(wXHistTopBase, win = win)
          win->getProperty, dimension = wdims, resolution = res
          oClipboard->setProperty, dimensions = wdims, resolution = res
          oClipboard->draw, oSingleHistView, filename = s_getPathForSystem()+'objSinglePlot.bmp'
         endif

         if (state.fMovieMixedParamPlot and widget_info(state.child_MixedDataPlot_tlb, /valid)) then begin
           widget_control, state.child_MixedDataPlot_tlb, get_uValue = stateMixed, /no_copy
                           wMixedTopBase = stateMixed.wTopBase
           widget_control, state.child_MixedDataPlot_tlb, set_uValue = stateMixed, /no_copy
           oGroupView = s_GroupParamPlot_getHistViewObj(wMixedTopBase, win = win)
           win->getProperty, dimension = wdims, resolution = res
           oClipboard->setProperty, dimensions = wdims, resolution = res
           oClipboard->draw, oGroupView, filename = s_getPathForSystem()+'objMixedPlot.bmp'
         endif

         if state.fMovieUseTimeCounter then begin
           time = strCompress(string(tPos), /rem)
           for i = strLen(time), 3 do time = strCompress('0'+time, /rem)
           fileName = strCompress(s_getPathForSystem()+strcompress('movie_Ch' + string(chPos) + '_t' + time +'.bmp', /rem))
         endif else begin
           time = strCompress(string(tPos*totalZNum+zPos), /rem)
           for i = strLen(time), 3 do time = strCompress('0'+time, /rem)
           fileName = strCompress(s_getPathForSystem()+strcompress('movie_Ch' + string(chPos) + '_' + time +'.bmp', /rem))
         endelse
         s_objworld_updateMovieViews, state.child_ObjViewWindow_tlb, infoString = state.sInfoString, infoDim = state.sInfoDim
         oAlias = 1
         s_objworldSceneAntiAlias, winMain, oScene, 8, oAlias = oAlias
         oClipboard->setProperty, dimensions = mainDims, resolution = mainRes
         oClipboard->draw, oAlias, fileName = fileName
         obj_destroy, oAlias
         obj_destroy, oClipboard
       endif
    widget_control, wTopBase, set_uValue = state, /no_copy, /upDate
end ;-s_ROIOM_UpdateWidgets------------------------------------------------------------------


pro s_ROIOM_InitializeWidgets, wTopBase

    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       fROIConnectStackParam = state.fROIConnectStackParam
       widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
         imageStackInfoObject = *stackState.pimageStackInfoObject
       widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos,$
                                              totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
                                              selClusName = selClusName, selROIGroupObj = selROIGroupObj

    widget_control, wTopBase, get_uValue = state, /no_copy

         ; decide about updates
       fUpdate = 1b
       if ((state.fXObjHistControl + state.fYObjHistControl + state.fYObjHistControl) gt 0) then state.fRefreshHistData = 1b
       if ((state.currentTimePosition ne tPos) or (state.currentClusterPosition ne clusPos) or $
          (state.currentChannelPosition ne chPos)) then state.fUpDateROI3DGroup = 1b
       if ((state.currentTimePosition ne tPos) or (state.currentClusterPosition ne clusPos) or $
          (state.currentChannelPosition ne chPos) or (state.currentZSlicePosition ne zPos)) then state.fKeepROI2DParamThreshs = 1b
       if (state.currentZSlicePosition ne zPos) then begin
          state.fUpDateROI2DGroup = 1b
          state.fKeepROI2DParamThreshs = 1b
       endif
       if ((state.currentTimePosition eq tPos) and (state.currentClusterPosition eq clusPos) and $
          (state.currentChannelPosition eq chPos) and (state.currentZSlicePosition eq zPos)) then begin
          state.fUpDateROI3DGroup = 1b
          state.fKeepROI3DParamThreshs = 1b
          state.fKeepROI2DParamThreshs = 1b
       endif
       if (state.currentTimePosition ne tPos) then begin
          state.fKeepROI3DParamThreshs = 1b
          state.fKeepROI2DParamThreshs = 1b
       endif

       state.currentTimePosition = tPos
       state.currentChannelPosition = chPos
       state.currentZSlicePosition = zPos
       state.currentClusterPosition = clusPos

         ; fUpdate selected image name
       oImage = imageStackInfoObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
       if obj_valid(oImage) then begin
          oImage->get, pParamStruct = pImageParamStruct
          state.sTopBaseTitle = 's_ROI |-> ' + selClusName+'_'+ *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'Name'))[0]]
          widget_control, state.wTopBase, tlb_set_title = state.sTopBaseTitle
       endif

         ; fUpdate s_objectWorld Objects
       if (state.fRefreshHistData and widget_info(state.child_ObjViewWindow_tlb, /valid)) then begin
          widget_control, state.child_ObjViewWindow_tlb, get_uValue = stateObjWin, /no_copy
          if (stateObjWin.strSelected ne 'CurrentTopModel selected') then begin
             eventStruct_2 = {id:stateObjWin.wDraw,$
                              top:state.child_ObjViewWindow_tlb,$
                              handler:state.child_ObjViewWindow_tlb,$
                              type:0,$
                              x:1, y:1,$
                              press:4b,$
                              release:0b,$
                              clicks:1b,$
                              modifiers:0l}
             widget_control, state.child_ObjViewWindow_tlb, set_uValue = stateObjWin, /no_copy
             widget_control, wTopBase, set_uValue = state, /no_copy
               s_objworldEvent, eventStruct_2
             widget_control, wTopBase, get_uValue = state, /no_copy
          endif else widget_control, state.child_ObjViewWindow_tlb, set_uValue = stateObjWin, /no_copy
       endif

         ; fUpdate oROI3DGroup
       if (state.fUpDateROI3DGroup and ((selROIGroupObj eq 'oROI3DGroup') or (selROIGroupObj eq '-NO GROUP-') or (state.fApplyToROI3DGroups)) ) then begin
          if ptr_valid(state.poCurrROI3DGroup) then if obj_valid(*state.poCurrROI3DGroup) then begin
                ; keep or complement oParamContainers
             if (state.fKeepSelectROIParamsControl or state.fComplementROIParamsControl) then begin
                oParamContainer = (*state.poCurrROI3DGroup)->getParamContainer()
                save, oParamContainer, filename = s_getPathForSystem()+'obj.tmp'
                restore, s_getPathForSystem()+'obj.tmp', restored_objects = paramContObj, /relaxed
                for i = 0, (n_elements(paramContObj)-1) do if obj_isa(paramContObj[i], 'IDL_Container') then oParamContainer = paramContObj[i]
             endif
                ; keep Visualisation Parameters
             if state.fKeepVisualParams then begin
                volState = *((*state.poCurrROI3DGroup)->getpVolState())
                paramStruct = (*state.poCurrROI3DGroup)->getParamAsStruct()
             endif

                ; delete CurrentPhaseModel and CurrentROI3DGroup
             poDummy = state.poCurrROI3DGroup
             widget_control, wTopBase, set_uValue = state, /no_copy
               *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
             widget_control, wTopBase, get_uValue = state, /no_copy
             obj_destroy, *state.poCurrROI3DGroup
          endif
             ; get new oROI3DGroup
          oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = state.stack_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos, fileName = fileName)
          if obj_valid(oROI3DGroup) then begin
             state.currROI3DGroupFileName = fileName
             if ptr_valid(state.poCurrROI3DGroup) then ptr_free, state.poCurrROI3DGroup
             state.poCurrROI3DGroup = ptr_new(oROI3DGroup, /no_copy)
             if state.fUpdate3DROIWithGrowthFactor then (*state.poCurrROI3DGroup)->update3DModelWithGrowthFactor, maxGrowthFactor = maxGrowthFactor
                ; pass or complement oParamContainer
             if obj_valid(oParamContainer) then begin
                case 1 of
                   state.fKeepSelectROIParamsControl:(*state.poCurrROI3DGroup)->setParamContainer, object = oParamContainer
                   state.fComplementROIParamsControl:(*state.poCurrROI3DGroup)->complementGroupParameters, oParamContainerReference = oParamContainer
                   state.fKeepSelectROIParamFilters:(*state.poCurrROI3DGroup)->setParamFilters, oParamContainerReference = oParamContainer
                endcase
             endif
                ; pass Visualisation Parameters
             if (state.fKeepVisualParams) then begin
                if (n_tags(volState) gt 0) then (*state.poCurrROI3DGroup)->setpVolState, pVolState = ptr_new(volState, /no_copy)
                if (n_tags(paramStruct) gt 0) then (*state.poCurrROI3DGroup)->keepVisParamStruct, paramStruct
             endif
          endif else state.poCurrROI3DGroup = ptr_new('-1', /no_copy)
       endif

         ; fUpdate oROI2DGroup
       if (state.fUpDateROI2DGroup and ((selROIGroupObj eq 'oROI2DGroup') or (selROIGroupObj eq '-NO GROUP-') or state.fApplyToROI2DGroups)) then begin
         if obj_valid(*state.poCurrROI2DGroup) then begin
            ; keep or complement oParamContainers
          if (state.fKeepSelectROIParamsControl or state.fComplementROIParamsControl or state.fKeepSelectROIParamFilters) then begin
              oParamContainer = (*state.poCurrROI2DGroup)->getParamContainer()
              save, oParamContainer, filename = s_getPathForSystem()+'obj.tmp'
              restore, s_getPathForSystem()+'obj.tmp', restored_objects = paramContObj, /relaxed
              for i = 0, (n_elements(paramContObj)-1) do if (obj_isa(paramContObj[i], 'IDL_Container')) then oParamContainer = paramContObj[i]
          endif
            ; delete CurrentPhaseModel and CurrentROI2DGroup

          poDummy = state.poCurrROI2DGroup
          widget_control, wTopBase, set_uValue = state, /no_copy
            *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
          widget_control, wTopBase, get_uValue = state, /no_copy
          obj_destroy, *state.poCurrROI2DGroup
         endif
          ; initialize new ROI-Group Object or get new saved oROI2DGroup
         case 1 of
            state.fUse2DROIObjects:oROI2DGroup = $
                  s_ISegM_GetROI2DGroup(stack_tlb = state.stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, fileName = fileName)
            state.fUseSavedClusterMasks:oROI2DGroup = $
                 s_initROI2DGroupObjFromImageObj(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                    clusPos = clusPos, fileName = fileName)
            state.fUse3DROISlices:begin
                 oROI2DGroup = s_ISegM_GetROI2DGroup(stack_tlb = state.stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, fileName = fileName)
                 if (obj_valid(oROI2DGroup) and obj_valid(*state.poCurrROI3DGroup)) then $
                   oROI2DGroup = s_initROI2DGroupObjFromImageObj(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos,$
                                                                          maskImage = (*state.poCurrROI3DGroup)->getGroupMask(zPos = zPos),$
                                                                          intImage = (*state.poCurrROI3DGroup)->getGroupMaskIntensity(zPos = zPos),$
                                                                          objNumberVector = (*state.poCurrROI3DGroup)->getObjectNumberVector(),$
                                                                          fileName = fileName)
           endcase
         endcase
         if obj_valid(oROI2DGroup) then begin
            state.currROI2DGroupFileName = fileName
            if ptr_valid(state.poCurrROI2DGroup) then *state.poCurrROI2DGroup = oROI2DGroup else state.poCurrROI2DGroup = ptr_new(oROI2DGroup, /no_copy)
              ; pass or complement oParamContainer or Parameter Filters
            if obj_valid(oParamContainer) then begin
              case 1 of
                 state.fKeepSelectROIParamsControl:(*state.poCurrROI2DGroup)->setParamContainer, object = oParamContainer
                 state.fComplementROIParamsControl:(*state.poCurrROI2DGroup)->complementGroupParameters, oParamContainerReference = oParamContainer
                 state.fKeepSelectROIParamFilters:(*state.poCurrROI2DGroup)->setParamFilters, oParamContainerReference = oParamContainer
              endcase
            endif
         endif else state.poCurrROI2DGroup = ptr_new( obj_new('C_sROIGroupObj'), /no_copy)
       endif

         ; fUpdate 2DROI-Objects with Image Params
       if (state.fUpdate2DROIObjectsWithImageParamsControl) then begin
         if ((state.fUpDateROI2DGroup) and ((selROIGroupObj eq 'oROI2DGroup') or (selROIGroupObj eq '-NO GROUP-') or (state.fApplyToROI2DGroups)) ) then begin
          pROIGroupParamStruct = (*state.poCurrROI2DGroup)->getpParamStruct()
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'Name'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'Name'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'Time Position'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'Time Number'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'Channel Position'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'Channel Number'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'z-Slice Position'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'z-Slice Number'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'Time After Start [s]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'Time after start [s]'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'x-Size [pixel]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'x-Size [pixel]'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'y-Size [pixel]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'y-Size [pixel]'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'x-Size [real]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'x-Size [real]'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'y-Size [real]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'y-Size [real]'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'ShowBox in Original Picture [x0]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'xSegmentBox [x0]'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'ShowBox in Original Picture [x1]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'xSegmentBox [x1]'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'ShowBox in Original Picture [y0]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'ySegmentBox [y0]'))[0]]
          *(*pROIGroupParamStruct).pValues[(where(*(*pROIGroupParamStruct).pNames eq 'ShowBox in Original Picture [y1]'))[0]] = *(*pImageParamStruct).pValues[(where( *(*pImageParamStruct).pNames eq 'ySegmentBox [y1]'))[0]]
          (*state.poCurrROI2DGroup)->setRealROIGroupShowBoxSize
         endif
       endif

        ; fUpdate Movie Params
       if (state.fMovieControl and widget_info(state.child_ObjViewWindow_tlb, /valid)) then begin
         widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
          (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
         widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
            stackName = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
         pParamStruct = (*state.poCurrROI2DGroup)->getpParamStruct()
         time = string(*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time After Start [s]'))[0]])
         time = strCompress(strMid(time, 0, strPos(time, '.')+3), /rem)
         xyzSize = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]],$
                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] ]
         xyzReal = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ROI-Group ShowBox xSize [real]'))[0]],$
                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ROI-Group ShowBox ySize [real]'))[0]],$
                    (xyzSize[2]-1) * *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Interval [real]'))[0]] ]
         zInt = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Position [real]'))[0]] *  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Interval [real]'))[0]]
         zInt = strCompress(strMid(zInt, 0, strPos(zInt, '.')+3), /rem)
         xyzReal = strCompress(strMid(xyzReal[0], 0, strPos(xyzReal[0], '.')+3)+' × ', /rem) + strCompress(strMid(xyzReal[1], 0, strPos(xyzReal[1], '.')+3 )+' × ', /rem) + strCompress(strMid(xyzReal[2], 0, strPos(xyzReal[2], '.')+3 ), /rem)
         state.sInfoString = strCompress(stackName+'|->Time:'+string(tPos)+' ['+time+'s]'+' |  Ch:'+string(chPos)+' |  zSlice:'+string(zPos)+' ['+zInt+'µm]'+' |  Cluster:'+string(clusPos))
         state.sInfoDim = strCompress(string(xyzSize[0])+'×'+string(xyzSize[1])+'×'+string(xyzSize[2])+' [xyz-Pixel] |  '+xyzReal+'[real]')
       endif

         ;fUpdate wListROIGroup
       widget_control, state.wListROIGroup, get_uValue = uValueROI, /no_copy
         ROIGroupList = ['-NO GROUP-']
         if obj_valid(*state.poCurrROI2DGroup) then ROIGroupList = [ROIGroupList, 'oROI2DGroup']
         if obj_valid(*state.poCurrROI3DGroup) then ROIGroupList = [ROIGroupList, 'oROI3DGroup']
         if (n_elements(ROIGroupList) gt 1) then ROIGroupList = ROIGroupList[1:*]
         if ptr_valid(uValueROI.value) then *uValueROI.value = ROIGroupList else uValueROI.value = ptr_new(ROIGroupList, /no_copy)
       widget_control, state.wListROIGroup, set_list_select = uValueROI.active, set_value = *uValueROI.value, set_uValue = uValueROI, /no_copy

         ;calculate Params
       if state.fRestoreAndCalculate then begin
         if ( ((selROIGroupObj eq 'oROI2DGroup') or (state.fApplyToROI2DGroups)) and (state.fUpDateROI2DGroup) and obj_valid(*state.poCurrROI2DGroup) ) then begin
          widget_control, state.wListROIGroup, get_uValue = uValueList, /no_copy
              uValueList.active = (where(*uValueList.value eq 'oROI2DGroup'))[0]
          widget_control, state.wListROIGroup, set_uValue = uValueList, /no_copy
          ev = {top:state.wTopBase, id:state.wCalcAllROIParams, silent:1b}
          widget_control, wTopBase, set_uValue = state, /no_copy
              s_ROI_ObjectManipulator_Window_Button_Event, ev
          widget_control, wTopBase, get_uValue = state, /no_copy
         endif
         if ( ((selROIGroupObj eq 'oROI3DGroup') or (state.fApplyToROI3DGroups)) and (state.fUpDateROI3DGroup) and obj_valid(*state.poCurrROI3DGroup) ) then begin
          widget_control, state.wListROIGroup, get_uValue = uValueList, /no_copy
              uValueList.active = (where(*uValueList.value eq 'oROI3DGroup'))[0]
          widget_control, state.wListROIGroup, set_uValue = uValueList, /no_copy
          ev = {top:state.wTopBase, id:state.wCalcAllROIParams, silent:1b}
          widget_control, wTopBase, set_uValue = state, /no_copy
              s_ROI_ObjectManipulator_Window_Button_Event, ev
          widget_control, wTopBase, get_uValue = state, /no_copy
         endif
       endif

         ;filter Params
       if (state.fParamFilter and not(state.fRestoreAndCalculate)) then begin
         if ( ((selROIGroupObj eq 'oROI2DGroup') or state.fApplyToROI2DGroups) and (state.fUpDateROI2DGroup) and obj_valid(*state.poCurrROI2DGroup) ) then begin
              widget_control, wTopBase, set_uValue = state, /no_copy
              s_ROIOM_UpdateWidgets, wTopBase
              widget_control, wTopBase, get_uValue = state, /no_copy
          widget_control, state.wListROIGroup, get_uValue = uValueList, /no_copy
              uValueList.active = (where(*uValueList.value eq 'oROI2DGroup'))[0]
          widget_control, state.wListROIGroup, set_uValue = uValueList, /no_copy
          ev = {top:state.wTopBase, id:state.wApplyAllParamFilters, silent:1b}
          widget_control, wTopBase, set_uValue = state, /no_copy
              s_ROI_ObjectManipulator_Window_Button_Event, ev
          widget_control, wTopBase, get_uValue = state, /no_copy
         endif
         if ( ((selROIGroupObj eq 'oROI3DGroup') or state.fApplyToROI3DGroups) and (state.fUpDateROI3DGroup) and obj_valid(*state.poCurrROI3DGroup) ) then begin
            widget_control, wTopBase, set_uValue = state, /no_copy
            s_ROIOM_UpdateWidgets, wTopBase
            widget_control, wTopBase, get_uValue = state, /no_copy
            widget_control, state.wListROIGroup, get_uValue = uValueList, /no_copy
               uValueList.active = (where(*uValueList.value eq 'oROI3DGroup'))[0]
            widget_control, state.wListROIGroup, set_uValue = uValueList, /no_copy
            ev = {top:state.wTopBase, id:state.wApplyAllParamFilters, silent:1b}
            widget_control, wTopBase, set_uValue = state, /no_copy
               s_ROI_ObjectManipulator_Window_Button_Event, ev
            widget_control, wTopBase, get_uValue = state, /no_copy
         endif
       endif

       if state.fSaveResultsAutomatically then begin
          if (((selROIGroupObj eq 'oROI2DGroup') or state.fApplyToROI2DGroups) and obj_valid(*state.poCurrROI2DGroup)) then begin
           poDummy = state.poCurrROI2DGroup
           widget_control, wTopBase, set_uValue = state, /no_copy
             *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
           widget_control, wTopBase, get_uValue = state, /no_copy
           if (state.currROI2DGroupFileName ne s_getPathForSystem()) then begin
              oROIGroup = *state.poCurrROI2DGroup
              save, oROIGroup, filename = state.currROI2DGroupFileName
           endif

           widget_control, wTopBase, set_uValue = state, /no_copy
              s_ROIOM_UpdateCurrentROIPhaseModel, wTopBase = wTopBase, fUpDateROI2DGroup = 1
           widget_control, wTopBase, get_uValue = state, /no_copy
          endif
          if (((selROIGroupObj eq 'oROI3DGroup') or state.fApplyToROI3DGroups) and obj_valid(*state.poCurrROI3DGroup)) then begin
           poDummy = state.poCurrROI3DGroup
           widget_control, wTopBase, set_uValue = state, /no_copy
             *poDummy->manageCurrentPhaseModel, includeModelList = '', stack_tlb = stack_tlb
           widget_control, wTopBase, get_uValue = state, /no_copy
           oROIGroup = *state.poCurrROI3DGroup
           save, oROIGroup, filename = state.currROI3DGroupFileName
           widget_control, wTopBase, set_uValue = state, /no_copy
              s_ROIOM_UpdateCurrentROIPhaseModel, wTopBase = wTopBase, fUpDateROI3DGroup = 1
           widget_control, wTopBase, get_uValue = state, /no_copy
         endif
       endif

       if state.fSaveClusterMasksFromROIObject then imageStackInfoObject->saveSelectedClusterMask, tPos = tPos,$
                                                            chPos = chPos, zPos = zPos, clusPos = clusPos,$
                                                            mask = (*state.poCurrROI2DGroup->getGroupMask() gt 0)*255

   widget_control, wTopBase, set_uValue = state, /no_copy, /update
   if fUpdate then s_ROIOM_UpdateWidgets, wTopBase
   if fROIConnectStackParam then s_ROIOM_passROIStackTrack, wTopBase
end;-s_ROIOM_InitializeWidgets------------------------------------------------------------------


pro s_ROIOM_Resize_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy

    ; Don't resize listbase on UNIX
    if (!VERSION.OS_FAMILY eq 'Windows') then begin
       widget_control, state.wListBase_1, scr_xsize = ev.x-10, scr_ysize = floor((ev.y-43)/4.)
       widget_control, state.wListBase_2, scr_xsize = ev.x-10, scr_ysize = floor(2.*(ev.y-43)/4.)
       widget_control, state.wListBase_3, scr_xsize = ev.x-10, scr_ysize = floor((ev.y-43)/4.)
    endif

       evx = ev.x-13
       evy = floor((ev.y-170)/4.)
       widget_control, state.wListROIGroup, scr_xsize = evx, scr_ysize = evy
       widget_control, state.wListSelectROIGroupParams, scr_xsize = evx, scr_ysize = evy+5
       widget_control, state.wListSelectROIGroupSubParams, scr_xsize = evx, scr_ysize = evy+10
       widget_control, state.wListSelectROIGraphicModels, scr_xsize = evx, scr_ysize = evy+5
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_ROIOM_cleanUp, wTopBase
    widget_control, wTopBase, get_uValue = status, /no_copy

    if (n_elements(status) ne 0) then begin
       if widget_info(status.groupLeader, /valid_id) then begin
         widget_control, status.groupLeader, get_uValue = statusParent, /no_copy
         if (n_elements(statusParent) ne 0) then begin
            if statusParent.fROIObjectWindow then statusParent.fROIObjectWindow = s_ToggleButtonOnOffState(statusParent.wROIObjectWindowOnOff)
            widget_control, status.groupLeader, set_uValue = statusParent, /no_copy
         endif
       endif

       if obj_valid(*status.poCurrROI2DGroup) then begin
          obj_destroy, *status.poCurrROI2DGroup
          if ptr_valid(status.poCurrROI2DGroup) then ptr_free, status.poCurrROI2DGroup
       endif

       if obj_valid(*status.poCurrROI3DGroup) then begin
          obj_destroy, *status.poCurrROI3DGroup
          if ptr_valid(status.poCurrROI3DGroup) then ptr_free, status.poCurrROI3DGroup
       endif

       if obj_valid(*status.poStackTrackContainer) then begin
          obj_destroy, *status.poStackTrackContainer
          if ptr_valid(status.poStackTrackContainer) then ptr_free, status.poStackTrackContainer
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


pro s_ROI_ObjectManipulator_Window, groupLeader = groupLeader, basePosition = basePosition

       ;Check the validity of the group identifier.
    if (n_elements(groupLeader) eq 0) then return
    if not(widget_info(groupLeader, /valid_id)) then return
    if not(keyWord_set(basePosition)) then basePosition = [0,0]
    wTopBase = widget_base(title = 's_ROI |->', xpad = 2, ypad = 2, xOffset = basePosition[0], MBar = menuBase, Group_Leader = groupLeader, tlb_size_events = 1, /column)

        ; Create menu buttons
    wMenuButton = widget_button(menuBase, value = 'Options', event_pro = 's_ROI_ObjectManipulator_Window_Button_Event')
       void = widget_button(wMenuButton, value = 'For 2DROIs:', uVal = 'VOID', sensitive = 0)
       wUse2DROIObjectsOnOff = widget_button(wMenuButton, value = 'Use 2D ROI-Objects (on )', uVal = 'USE2DROIOBJECTSONOFF')
       wUseSavedClusterMasksOnOff = widget_button(wMenuButton, value = 'Use Saved Cluster Masks (off)', uVal = 'USESAVEDCLUSTERMASKSONOFF')
       wUse3DROISlicesOnOff = widget_button(wMenuButton, value = 'Use 3D ROI-Slices (off)', uVal = 'USE3DROISLICESONOFF')
       wUpdate2DROIObjectsWithImageParamsOnOff = widget_button(wMenuButton, value = 'fUpdate 2D ROI-Object with Image-Parameter (off)', uVal = 'UPDATE2DROIOBJECTWITHIMAGEPARAMSONOFF')
       wSaveClusterMasksFromROIObjectOnOff = widget_button(wMenuButton, value = 'Save Cluster Masks from ROI-Objects (off)', uVal = 'SAVECLUSTERMASKSFROMROIOBJECTS')
       void = widget_button(wMenuButton, value = 'For 3DROIs:', uVal = 'VOID', sensitive = 0)
       wUpdate3DROIWithGrowthFactorOnOff = widget_button(wMenuButton, value = 'fUpdate 3D ROI Objects with Growth-Factor (off)', uVal = 'UPDATE3DROIWITHGROWTHFACTORNOFF')
       void = widget_button(wMenuButton, value = 'Open ROI-Params', uVal = 'OPENROIPARAMS', /sep)
       void = widget_button(wMenuButton, value = 'Save ROI-Params as...', uVal = 'SAVEROIPARAMSAS')
       void = widget_button(wMenuButton, value = 'Open ROI-Stack-Track', uVal = 'OPENROISTACKTRACK', /sep)
       void = widget_button(wMenuButton, value = 'Save ROI-Stack-Track as...', uVal = 'SAVEROIROISTACKTRACKAS')
       wSaveStackTrackParamsAs = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Params as ASCII...', uVal = 'SAVEROIROISTACKTRACKPARAMSASASCII', sensitive = 0)
       wSaveStackTrackParamObjAs = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as ASCII...', uVal = 'SAVEROIROISTACKTRACKPARAMOBJASASCII', sensitive = 0)
    
       RefMenuTemp = widget_button(wMenuButton, value = '||-> Graphic Drift Correction Options ...', /menu , /sep)
           
       void = widget_button(RefMenuTemp, value = 'Save Graphic State', uVal = 'SAVEGRAPHICSTATE', /sep)
       void = widget_button(RefMenuTemp, value = 'Apply Graphic State', uVal = 'APPLYGRAPHICSTATE')
       void = widget_button(RefMenuTemp, value = 'Apply Select Graphic State', uVal = 'APPLYSELECTGRAPHICSTATE')
       
       wParamPBOnOff = widget_button(RefMenuTemp, value = 'Show 3D Plane Model Base (on )', uVal = 'ONOFFVISIBLEPLANEBASE', /sep)
       wParamPOOnOff = widget_button(RefMenuTemp, value = 'Show Visible 3D Plane Model Orthogonal (on )', uVal = 'ONOFFVISIBLEPLANEORTH')
       wParamPCOnOff = widget_button(RefMenuTemp, value = 'Show Visible 3D Plane Model Complementary (on )', uVal = 'ONOFFVISIBLEPLANECOMP')

       RefMenuTemp = widget_button(wMenuButton, value = '||-> Estimated Tracking Params Options ...', /menu , /sep)
           
       void = widget_button(RefMenuTemp, value = 'Flip Horizontally Angles', uVal = 'FLIPANGLES', /sep)

       RefMenuTemp = widget_button(wMenuButton, value = '||-> Skeleton Options ...', /menu , /sep)
       RefColorMenuTemp = widget_button(RefMenuTemp, value = '||-> Color Options ...', /menu , /sep)
       wParamSkelByColorParamOnOff = widget_button(RefColorMenuTemp, value = 'Use Color by Saved params (off)', uVal = 'ONOFFCOLORBYPARAM', /sep)
       void = widget_button(RefColorMenuTemp, value = 'Apply color by... Number Edge', uVal = 'APPLYCOLORPARAM0', /sep)
       void = widget_button(RefColorMenuTemp, value = 'Apply color by... Bifurc Number', uVal = 'APPLYCOLORPARAM1')
       void = widget_button(RefColorMenuTemp, value = 'Apply color by... Edge Lenght [Voxel]', uVal = 'APPLYCOLORPARAM2')
       void = widget_button(RefColorMenuTemp, value = 'Apply color by... Edge Lenght [x²]', uVal = 'APPLYCOLORPARAM3')
       void = widget_button(RefColorMenuTemp, value = 'Apply color by... Edge Angle [Degrees]', uVal = 'APPLYCOLORPARAM4')
       void = widget_button(RefColorMenuTemp, value = 'Apply color by... Deep Level', uVal = 'APPLYCOLORPARAM5')
       
       ; ----------- CEDAI - INICIO --------------
       wSaveStackTrackParamObjAsInforme = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as Info Vital...', uVal = 'SAVEROIROISTACKTRACKPARAMOBJASINFOVITAL', sensitive = 0); susana inventando Informe Vitalidad
       wSaveStackTrackParamObjAsInforme = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as Info Vital SuperStack...', uVal = 'SAVEROIROISTACKTRACKPARAMOBJASINFOVITALSUPERSTACK', sensitive = 0); susana inventando Informe Vitalidad
       wSaveStackTrackParamObjAsAllROIsVital = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as AllROIs Vital...', uVal = 'SAVEROIROISTACKTRACKPARAMOBJASROIcorrds', sensitive = 0); susana inventando Informe Vitalidad
       
       wSaveStackTrackParamObjAsFragmentation = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as Info Fragmentation...', uVal = 'SAVEROIROISIZERELATIONASASCII', sensitive = 0); susana inventando Informe Vitalidad
       ;wSaveStackTrackParamObjAsROUNDCELLS = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as Info ROUND CELLS...', uVal = 'SAVEROIROISIZEROUNDCELLSASASCII', sensitive = 0); susana inventando Informe ROUND CELLS
       ;wSaveStackTrackParamObjAsInformeMARCIA = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as Info Vital MARCIA...', uVal = 'SAVEROIROISTACKTRACKPARAMOBJASINFOVITALMARCIA', sensitive = 0); susana inventando Informe Vitalidad
       ;wSaveStackTrackParamObjAsInformeMARCIA2 = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as Info Vital MARCIA 2...', uVal = 'SAVEROIROISTACKTRACKPARAMOBJASINFOVITALMARCIA2', sensitive = 0); susana inventando Informe Vitalidad
       ;wSaveStackTrackParamObjAsInformeCARO = widget_button(wMenuButton, value = 'Save ROI-Stack-Track Param Objects as Info Vital CARO...', uVal = 'SAVEROIROISTACKTRACKPARAMOBJASINFOVITAL_CARO', sensitive = 0); susana inventando Informe Vitalidad
       ; ----------- CEDAI - FIN --------------
       
       wShowROIObjectDataButton = widget_button(wMenuButton, value = 'Show ROI-Parameter Data as ....', /menu, /sep)
       wParamTableOnOff = widget_button(wShowROIObjectDataButton, value = '... Table (off)', uVal = 'SHOWASDATATABLEONOFF')
       wParamWidgetOnOff = widget_button(wShowROIObjectDataButton, value = '... Widgets (on )', uVal = 'SHOWASWIDGETONOFF')

       wMenuButton = widget_button(menuBase, value = 'Parameters', event_pro = 's_ROI_ObjectManipulator_Window_Button_Event')
       wCalcSelectedROIParam = widget_button(wMenuButton, value = 'Calculate Selected ROI-Parameter', uVal = 'CALCULATESELECTEDPARAMETER')
       wCalcAllROIParams = widget_button(wMenuButton, value = 'Calculate All ROI-Parameters', uVal = 'CALCULATEALLROIPARAMETERS')
       void = widget_button(wMenuButton, value = 'Apply Selected Parameter Filter', uVal = 'APPLYSELECTEDPARAMETERFILTER', /sep)
       wApplyAllParamFilters = widget_button(wMenuButton, value = 'Apply All Parameter Filters', uVal = 'APPLYALLPARAMETERFILTERS')
       wKeepVisualParamsOnOff = widget_button(wMenuButton, value = '...|-> Keep Visualization Parameters (off)', uVal = 'KEEPVISUALIZATIONPARAMSONOFF', /sep)
       wKeepSelectROIParamsFiltersOnOff = widget_button(wMenuButton, value = '...|-> Keep Selected ROI-Param-Filters (off)', uVal = 'KEEPSELECTEDROIPARAMFILTERSONOFF')
       wKeepSelectROIParamsOnOff = widget_button(wMenuButton, value = '...|-> Keep Selected ROI-Params (off)', uVal = 'KEEPSELECTEDROIPARAMSONOFF')
       wComplementROIParamsOnOff = widget_button(wMenuButton, value = '...|-> Complement ROI-Params (off)', uVal = 'COMPLEMENTROIPARAMSONOFF')
       wRestoreAndCalculateOnOff = widget_button(wMenuButton, value = '...|-> Calculate Parameters (off)', uVal = 'CALCULATEPARAMETERSONOFF')
       wParamFilterOnOff = widget_button(wMenuButton, value = '...|-> Parameter Filter (off)', uVal = 'PARAMETERFILTERONOFF')
       wSaveResultsAutomaticallyOnOff = widget_button(wMenuButton, value = '...|-> Save Results Automatically (off)', uVal = 'SAVERESULTSAUTOMATICALLYONOFF')
       wApplyToROI2DGroupsOnOff = widget_button(wMenuButton, value = '...|-> Apply to 2D ROI-Groups (on )', uVal = 'APPLYTOROI2DGROUPSONOFF')
       wApplyToROI3DGroupsOnOff = widget_button(wMenuButton, value = '...|-> Apply to 3D ROI-Groups (on )', uVal = 'APPLYTOROI3DGROUPSONOFF')
       wStackTrackCollectAllParamOnOff = widget_button(wMenuButton, value = '...|-> Activate All Stack-Track-Params (on )', uVal = 'ACTIVATEALLSTACKSTRACKONOFF')
       wUpdateStackTrackParamsOnOff = widget_button(wMenuButton, value = '...|-> Updade Stack-Track-Params (on )', uVal = 'UPDATEROISTACKTRACKPARAMSONOFF')
       void = widget_button(wMenuButton, value = 'Movie Parameters:', sensitive = 0, /sep)
       wMovieUseTimeCounterOnOff = widget_button(wMenuButton, value = '...|-> Use Time Counter (off)', uVal = 'MOVIEUSETIMECOUNTERONOFF')
       wMovieKeepObjectPositionOnOff = widget_button(wMenuButton, value = '...|-> Keep Object Positions (off)', uVal = 'MOVIEKEEPOBJECTPOSITIONONOFF')
       wMovieKeepGalleryPositionOnOff = widget_button(wMenuButton, value = '...|-> Keep Gallery Positions (off)', uVal = 'MOVIEKEEPGALLERYPOSITIONONOFF')
       wMovieKeepSpherePositionOnOff = widget_button(wMenuButton, value = '...|-> Keep Sphere Positions (off)', uVal = 'MOVIEKEEPSPHEREPOSITIONONOFF')
       wMovieIncludeSingleParamHistOnOff = widget_button(wMenuButton, value = '...|-> Keep Single Parameter Histogram (off)', uVal = 'MOVIEKEEPKEEPSINGLEPARAMHISTONOFF')
       wMovieMixedParamPlotOnOff = widget_button(wMenuButton, value = '...|-> Keep Mixed Parameter Plot (off)', uVal = 'MOVIEKEEPMIXEDPARAMPLOTONOFF')
       wMovieOnOff = widget_button(wMenuButton, value = '...|-> Make Movie (off)', uVal = 'MOVIEONOFF')
       wStartFromSelectedTimeOnOff = widget_button(wMenuButton, value = '...|-> Start from Selected Time (off)', uVal = 'STARTFROMSELECTEDTIMEONOFF', /sep)
       void = widget_button(wMenuButton, value = '...||-> Apply to ...', /menu)
       void_2 = widget_button(void, value = '... all Times', uVal = 'APPLYTOALLTIMES')
       void_2 = widget_button(void, value = '... all ZSlices', uVal = 'APPLYTOALLZSLICES')
       void_2 = widget_button(void, value = '... all ZSlices and Times', uVal = 'APPLYTOALLZSLICESANDTIMES')
       void_2 = widget_button(void, value = '... all ZSlices and Channels', uVal = 'APPLYTOALLZSLICESANDCHANNELS')
       void_2 = widget_button(void, value = '... all ZSlices and Channels and Times', uVal = 'APPLYTOALLZSLICESANDCHANNELSANDTIMES')

    wMenuButton = widget_button(menuBase, value = 'Analyse', event_pro = 's_ROI_ObjectManipulator_Window_Button_Event')
       wROITrackWindowOnOff = widget_button(wMenuButton, value = 'ROI Track Window (off)', uVal = 'ROITRACKWINDOWONOFF')
       wROIModelWindowOnOff = widget_button(wMenuButton, value = 'ROI Model Window (off)', uVal = 'ROIMODELWINDOWONOFF')
       wROIConnectStackParamOnOff = widget_button(wMenuButton, value = '...|-> Connect Selected Stack-Picture (off)', uVal = 'ROICONNECTSTACKPARAMONOFF', /sep)
       wROIParamWindowOnOff = widget_button(wMenuButton, value = '...||-> ROI Parameter Window (off)', uVal = 'ROIPARAMWINDOWONOFF')

    wListBase_1 = widget_base(wTopBase, /column, /align_center)
       void = widget_button(wListBase_1, value = ' |-> Save ROI-Group <-| ', uValue = 'SAVEROIGROUP', event_pro = 's_ROI_ObjectManipulator_Window_Button_Event')
       wListROIGroup = widget_list(wListBase_1, xSize = 30, ySize = 2, value = ['-NO GROUP-'],$
                        uValue = {name:'Available ROI-Groups', value:ptr_new(  ['-NO GROUP-'], /no_copy), active:0 },$
                        event_pro = 's_ROIOM_List_Event',kill_notify = 'CleanList')

    paramNameList = s_ROIOM_getROIParamNameList()
    wListBase_2 = widget_base(wTopBase, /column, /align_center)
       void = widget_button(wListBase_2, value = 'Add ROI-Parameter Object |->', /menu, event_pro = 's_ROIOM_AddParameterMethod_Event')
       dummy2D = widget_button(void, value = 'Add 2D ROI-Parameter Object |->', /menu)
       dummy3D = widget_button(void, value = 'Add 3D ROI-Parameter Object |->', /menu)
       for i = 0, n_elements(paramNameList)-1 do begin
        if(strPos(paramNameList[i], '3D', /reverse_search) eq -1) then begin
          dummy = widget_button(dummy2D, value = paramNameList[i])
        endif else dummy = widget_button(dummy3D, value = paramNameList[i])
       endfor
;       for i = 0, n_elements(paramNameList)-1 do dummy = widget_button(void, value = paramNameList[i])

       wListSelectROIGroupParams = widget_list(wListBase_2, xSize = 30, ySize = 4, value = ['-NO SELECTION-'],$
                             uValue = {name:'Selected ROI-Parameter-Objects', value:ptr_new(['-NO SELECTION-'], /no_copy), active:0},$
                             event_pro = 's_ROIOM_List_Event', kill_notify = 'CleanList')

       void = widget_label(wListBase_2, value = 'ROI-Sub-Parameters |-> ')
       wListSelectROIGroupSubParams = widget_list(wListBase_2, xSize = 30, ySize = 4, value = ['-NO SELECTION-'],$
                             uValue = {name:'Selected ROI-Sub-Parameters', value:ptr_new(['-NO SELECTION-'], /no_copy), active:0 },$
                             event_pro = 's_ROIOM_List_Event', kill_notify = 'CleanList')

    graphicNameList = s_ROIOM_getROIGraphicNameList()
    wListBase_3 = widget_base(wTopBase, /column, /align_center)
       void = widget_button(wListBase_3, value = 'Add Graphic Model |->', /menu, event_pro = 's_ROIOM_AddGraphicModel_Event')
       dummy2D = widget_button(void, value = 'Add 2D Graphic Model |->', /menu)
       dummy3D = widget_button(void, value = 'Add 3D Graphic Model |->', /menu)
       
       dummy3DModelling = widget_button(dummy3D, value = 'Add Modelling 3D Graphic Model |->', /menu,/sep)
       dummy3DUnDrifted = widget_button(dummy3D, value = 'Add UnDrifted 3D Graphic Model |->', /menu,/sep)
       for i = 0, n_elements(graphicNameList)-1 do begin
          selectedDummy = dummy2D
          case 1b of
            (strPos(graphicNameList[i], 'Modelling', /reverse_search) ne -1): selectedDummy = dummy3DModelling
            (strPos(graphicNameList[i], 'UnDrifted', /reverse_search) ne -1): selectedDummy = dummy3DUnDrifted
            (strPos(graphicNameList[i], '3D', /reverse_search) ne -1): selectedDummy = dummy3D
            else: selectedDummy = dummy2D            
          endcase          
          dummy = widget_button(selectedDummy, value = graphicNameList[i])

      ;XXX  Deprecating... by FASL... testing for now.. Dont delete me please
;              if(strPos(graphicNameList[i], '3D', /reverse_search) eq -1) then begin
;                  dummy = widget_button(dummy2D, value = graphicNameList[i])
;               endif else begin
;                  if(strPos(graphicNameList[i], 'UnDrifted', /reverse_search) eq -1) then begin
;                      dummy = widget_button(dummy3D, value = graphicNameList[i])
;                  endif else begin
;                      dummy = widget_button(dummy3DUnDrifted, value = graphicNameList[i])
;                  end
;               endelse
       endfor

       wListSelectROIGraphicModels = widget_list(wListBase_3, xSize = 30, ySize = 4, value = ['-NO SELECTION-'],$
                             uValue = {name:'Selected Graphic Models', value:ptr_new(['-NO SELECTION-'], /no_copy), active:0},$
                             event_pro = 's_ROIOM_List_Event', kill_notify = 'CleanList')

    wButtonBase = widget_base(wTopBase, /row, event_pro = 's_ROI_ObjectManipulator_Window_Button_Event')
       void = widget_button(wButtonBase, value = 'Delete', uVal = 'DELETE')
       void = widget_button(wButtonBase, value = 'Up', uVal = 'MOVEUP')
       void = widget_button(wButtonBase, value = 'Down', uVal = 'MOVEDOWN')
       wXYZAxisButton = widget_button(wButtonBase, value = 'X-Axis', /menu)
       wXAxisObjectButton = widget_button(wXYZAxisButton, value = 'X-Axis |-> -NO SELECTION-', uVal = 'SETXOBJAXIS')
       wYAxisObjectButton = widget_button(wXYZAxisButton, value = 'Y-Axis |-> -NO SELECTION-', uVal = 'SETYOBJAXIS', sensitive = 0)
       wZAxisObjectButton = widget_button(wXYZAxisButton, value = 'Z-Axis |-> -NO SELECTION-', uVal = 'SETZOBJAXIS', sensitive = 0)
       void = widget_button(wXYZAxisButton, value = '|-> Object Param Histogram', /menu)
       wXDataHistOnOff = widget_button(void, value = 'X-Data Histogram Window (off)', uValue = 'XDATAHISTONOFF')
       wYDataHistOnOff = widget_button(void, value = 'Y-Data Histogram Window (off)', uValue = 'YDATAHISTONOFF')
       wZDataHistOnOff = widget_button(void, value = 'Z-Data Histogram Window (off)', uValue = 'ZDATAHISTONOFF')
       wXAxisGroupButton = widget_button(wXYZAxisButton, value = 'X-Axis |-> -NO SELECTION-', uValue = 'SETXGROUPAXIS', /sep)
       wYAxisGroupButton = widget_button(wXYZAxisButton, value = 'Y-Axis |-> -NO SELECTION-', uValue = 'SETYGROUPAXIS', sensitive = 0)
       wZAxisGroupButton = widget_button(wXYZAxisButton, value = 'Z-Axis |-> -NO SELECTION-', uValue = 'SETZGROUPAXIS', sensitive = 0)
       void = widget_button(wXYZAxisButton, value = '|-> Group Parameter Plot', /menu)
       wXYGroupDataPlotOnOff = widget_button(void, value = 'XY-Group Data Plot (off)', uValue = 'XYGROUPDATAPLOTONOFF')
       wXAxisMixedButton = widget_button(wXYZAxisButton, value = 'X-Axis |-> -NO SELECTION-', uValue = 'SETXMIXEDAXIS', /sep)
       wYAxisMixedButton = widget_button(wXYZAxisButton, value = 'Y-Axis |-> -NO SELECTION-', uValue = 'SETYMIXEDAXIS', sensitive = 0)
       wZAxisMixedButton = widget_button(wXYZAxisButton, value = 'Z-Axis |-> -NO SELECTION-', uValue = 'SETZMIXEDAXIS', sensitive = 0)
       void = widget_button(wXYZAxisButton, value = '|-> Mixed Parameter Plot', /menu)
       wXYMixedDataPlotOnOff = widget_button(void, value = 'XY-Mixed Data Plot (off)', uValue = 'XYMIXEDDATAPLOTONOFF')

       ; Realize the widgets.
    widget_control, wTopBase, /realize

       ; Initialize state & widgets
    state = {groupLeader:groupLeader,$
          seg_tlb:groupLeader,$
          stack_tlb:0l,$
          wTopBase:wTopBase,$
          sTopBaseTitle:'X',$
          sInfoString:'X',$
          sInfoDim:'X',$
          currROI2DGroupFileName:s_getPathForSystem(),$
          currROI3DGroupFileName:s_getPathForSystem(),$
          currentStackTrackFileName:s_getPathForSystem(),$
          wListBase_1:wListBase_1,$
          wListBase_2:wListBase_2,$
          wListBase_3:wListBase_3,$
          fListSelect:0,$
          wListROIGroup:wListROIGroup,$
          child_ROI2DGroupTableWidget_tlb:-1L,$
          child_ROI3DGroupTableWidget_tlb:-1L,$
          wListSelectROIGroupParams:wListSelectROIGroupParams,$
          wListSelectROIGroupSubParams:wListSelectROIGroupSubParams,$
          wListSelectROIGraphicModels:wListSelectROIGraphicModels,$
          wCalcSelectedROIParam:wCalcSelectedROIParam,$
          wCalcAllROIParams:wCalcAllROIParams,$
          wApplyAllParamFilters:wApplyAllParamFilters,$
          wSaveStackTrackParamsAs:wSaveStackTrackParamsAs,$
          wSaveStackTrackParamObjAs:wSaveStackTrackParamObjAs,$
          
          ; ---------------- CEDAI - INICIO ---------------
           wSaveStackTrackParamObjAsInforme:wSaveStackTrackParamObjAsInforme,$; susana inventando Informe Vitalidad
          wSaveStackTrackParamObjAsAllROIsVital:wSaveStackTrackParamObjAsAllROIsVital,$; susana inventando Informe Vitalidad solo ROIS coordenadas
          wSaveStackTrackParamObjAsFragmentation:wSaveStackTrackParamObjAsFragmentation,$; susana inventando Informe Fragmentation
          ;wSaveStackTrackParamObjAsROUNDCELLS:wSaveStackTrackParamObjAsROUNDCELLS,$; susana inventando Informe ROUND CELLS
          ;wSaveStackTrackParamObjAsInformeMARCIA:wSaveStackTrackParamObjAsInformeMARCIA,$; susana inventando Informe Vitalidad MARCIA
          ;wSaveStackTrackParamObjAsInformeMARCIA2:wSaveStackTrackParamObjAsInformeMARCIA2,$; susana inventando Informe Vitalidad MARCIA 2
          ;wSaveStackTrackParamObjAsInformeCARO:wSaveStackTrackParamObjAsInformeCARO,$; susana inventando Informe CARO
          ; ---------------- CEDAI - FIN  ---------------
          
          wUpdateStackTrackParamsOnOff:wUpdateStackTrackParamsOnOff,$
             fUpdateStackTrackParams:1b,$
          wUse2DROIObjectsOnOff:wUse2DROIObjectsOnOff,$
             fUse2DROIObjects:1b,$
          wUseSavedClusterMasksOnOff:wUseSavedClusterMasksOnOff,$
             fUseSavedClusterMasks:0b,$
          wUse3DROISlicesOnOff:wUse3DROISlicesOnOff,$
             fUse3DROISlices:0b,$
          wSaveClusterMasksFromROIObjectOnOff:wSaveClusterMasksFromROIObjectOnOff,$
             fSaveClusterMasksFromROIObject:0b,$
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
        
          wParamPBOnOff:wParamPBOnOff,$
             fParamPB:0b,$
          wParamPOOnOff:wParamPOOnOff,$
             fParamPO:0b,$
          wParamPCOnOff:wParamPCOnOff,$
             fParamPC:0b,$
          wParamSkelByColorParamOnOff:wParamSkelByColorParamOnOff, $
             fParamSkelByColorParamOnOff:0b,$
          wUpdate2DROIObjectsWithImageParamsOnOff:wUpdate2DROIObjectsWithImageParamsOnOff,$
             fUpdate2DROIObjectsWithImageParamsControl:0b,$
          wUpdate3DROIWithGrowthFactorOnOff:wUpdate3DROIWithGrowthFactorOnOff,$
             fUpdate3DROIWithGrowthFactor:0b,$
          wKeepVisualParamsOnOff:wKeepVisualParamsOnOff,$
             fKeepVisualParams:0b,$
          wKeepSelectROIParamsFiltersOnOff:wKeepSelectROIParamsFiltersOnOff,$
             fKeepSelectROIParamFilters:0b,$
             wKeepSelectROIParamsOnOff:wKeepSelectROIParamsOnOff,$
             fKeepSelectROIParamsControl:0b,$
          wComplementROIParamsOnOff:wComplementROIParamsOnOff,$
             fComplementROIParamsControl:0b,$
             wSaveResultsAutomaticallyOnOff:wSaveResultsAutomaticallyOnOff,$
             fSaveResultsAutomatically:0b,$
             wApplyToROI2DGroupsOnOff:wApplyToROI2DGroupsOnOff,$
             fApplyToROI2DGroups:1b,$
             wApplyToROI3DGroupsOnOff:wApplyToROI3DGroupsOnOff,$
             fApplyToROI3DGroups:1b,$
          wStackTrackCollectAllParamOnOff:wStackTrackCollectAllParamOnOff,$
             fStackTrackCollectAllParam:1b,$
          wMovieUseTimeCounterOnOff:wMovieUseTimeCounterOnOff,$
             fMovieUseTimeCounter:0b,$
          wMovieKeepObjectPositionOnOff:wMovieKeepObjectPositionOnOff,$
             fMovieKeepObjectPosition:0b,$
          wMovieKeepGalleryPositionOnOff:wMovieKeepGalleryPositionOnOff,$
             fMovieKeepGalleryPosition:0b,$
          wMovieKeepSpherePositionOnOff:wMovieKeepSpherePositionOnOff,$
             fMovieKeepSpherePosition:0b,$
          xyzrSpherePos:[1.,1.,1.,1.,0.,0.,0.,0.],$
          wMovieIncludeSingleParamHistOnOff:wMovieIncludeSingleParamHistOnOff,$
             fMovieIncludeSingleParamHist:0b,$
          wMovieMixedParamPlotOnOff:wMovieMixedParamPlotOnOff,$
             fMovieMixedParamPlot:0b,$
          wMovieOnOff:wMovieOnOff,$
             fMovieControl:0b,$
          wStartFromSelectedTimeOnOff:wStartFromSelectedTimeOnOff,$
             fStartFromSelectedTime:0b,$
          wXYZAxisButton:wXYZAxisButton,$
          wXAxisObjectButton:wXAxisObjectButton,$
          wYAxisObjectButton:wYAxisObjectButton,$
          wZAxisObjectButton:wZAxisObjectButton,$
          fXYZObjectSensitive:0,$
          wXAxisGroupButton:wXAxisGroupButton,$
          wYAxisGroupButton:wYAxisGroupButton,$
          wZAxisGroupButton:wZAxisGroupButton,$
          fXYZGroupSensitive:0,$
          wXAxisMixedButton:wXAxisMixedButton,$
          wYAxisMixedButton:wYAxisMixedButton,$
          wZAxisMixedButton:wZAxisMixedButton,$
          fXYZMixedSensitive:0,$
          wXDataHistOnOff:wXDataHistOnOff,$
             fXObjHistControl:0b,$
             child_XDataHist_tlb:-1L,$
          wYDataHistOnOff:wYDataHistOnOff,$
             fYObjHistControl:0b,$
             child_YDataHist_tlb:-1L,$
          wZDataHistOnOff:wZDataHistOnOff,$
             fZObjHistControl:0b,$
             child_ZDataHist_tlb:-1L,$
          wXYGroupDataPlotOnOff:wXYGroupDataPlotOnOff,$
             fXYGroupDataPlotControl:0b,$
             child_GroupDataPlot_tlb:-1L,$
          wXYMixedDataPlotOnOff:wXYMixedDataPlotOnOff,$
             fXYMixedDataPlotControl:0b,$
             child_MixedDataPlot_tlb:-1L,$
          fRefreshHistData:1b,$
          fRefreshGroupData:1b,$
          fRefreshMixedData:1b,$
          child_ObjViewWindow_tlb:-1L,$
             selXYZObjectROIParamName:['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-'],$
             selXYZMixedROIParamName:['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-'],$
             selXYZGroupROIParamName:['-NO SELECTION-','-NO SELECTION-','-NO SELECTION-'],$
          wROIModelWindowOnOff:wROIModelWindowOnOff,$
             fROIModelWindow:0b,$
             child_ROIModelWindow_tlb:-1L,$
          wROITrackWindowOnOff:wROITrackWindowOnOff,$
             fROITrackWindow:0b,$
             child_ROITrackWindow_tlb:-1L,$
          wROIParamWindowOnOff:wROIParamWindowOnOff,$
             fROIParamWindow:0b,$
             child_ROIParamWindow_tlb:-1L,$
          wROIConnectStackParamOnOff:wROIConnectStackParamOnOff,$
             fROIConnectStackParam:0b,$
             poCurrROI2DGroup:ptr_new(obj_new('C_sROIGroupObj'),/no_copy),$
             poCurrROI3DGroup:ptr_new(),$
             currentTimePosition:-1,$
             currentChannelPosition:-1,$
             currentZSlicePosition:-1,$
             currentClusterPosition:-1,$
             fUpDateROI2DGroup:1b,$
             fUpDateROI2DGroupProperties:0b,$
             fUpDateROI3DGroup:1b,$
             fUpDateROI3DGroupProperties:0b,$
             fKeepROI2DParamThreshs:0b,$
             fKeepROI3DParamThreshs:0b,$
             poStackTrackContainer:ptr_new(obj_new('C_sROIParam_StackTrackContainer'), /no_copy),$
             poCurrROIGraphicModel:ptr_new(obj_new()),$
          graphicNameList:graphicNameList}

    if widget_info(state.seg_tlb, /valid) then begin
       widget_control, state.seg_tlb, get_uValue = segState
          state.stack_tlb = segState.stack_tlb
          segState.child_ROIObjectWindow_tlb = wTopBase
       widget_control, state.seg_tlb, set_uValue = segState
    endif else state.stack_tlb = -1

       ; Store the state structure in the uValue of the wTopBase. fUpdate Correct_And_Segment widgets, & Call XManager.
    widget_control, wTopBase, set_uValue = state, /no_copy
    XManager, 's_ROIOM_Resize', wTopBase, cleanUp = 's_ROIOM_cleanUp', /no_block, group_Leader = groupLeader
    s_ROIOM_InitializeWidgets, wTopBase
end