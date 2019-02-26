;_____________________________IOISIOI____________________
; NAME:
;      s_Image_SegmentationManipulator_Window
;
; PURPOSE:
;       - Sets or Destroys Image Parameter Structure
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________


function s_ISegM_Image_getFilterObjectList
    return, ['C_sImageFilter_ActiveContours',$
          'C_sImageFilter_AC_TSnakes',$
          'C_sImageFilter_AngularCut',$
          'C_sImageFilter_BorderDistance',$
          'C_sImageFilter_BorderDivCenterPix',$
          'C_sImageFilter_BorderDivCenterPixII',$
          'C_sImageFilter_ByteScale',$
          'C_sImageFilter_ByteScaleNonZero',$
          'C_sImageFilter_Canny',$
          'C_sImageFilter_ChanVese',$
          'C_sImageFilter_Cut',$
          'C_sImageFilter_CutSelective',$
          'C_sImageFilter_CutSelectiveCircle',$
          'C_sImageFilter_Congrid',$
          'C_sImageFilter_Convol',$
          'C_sImageFilter_CornerIntensity',$
          'C_sImageFilter_DrawSkeletonDist',$
          'C_sImageFilter_DriftCorrection',$
          'C_sImageFilter_DriftCorrection_byIJ',$
          'C_sImageFilter_Eccentricity',$
          'C_sImageFilter_EllipsenessEstimator',$
          'C_sImageFilter_FastFourier',$
          'C_sImageFilter_FrangiVessel',$
          'C_sImageFilter_FillRemove',$
          'C_sImageFilter_FlipRotateZoom',$
          'C_sImageFilter_FrameIntensity',$
          'C_sImageFilter_1stDeviation',$
          'C_sImageFilter_1stDeviationKernel',$
          'C_sImageFilter_GaussianKernel',$
          ;'C_sImageFilter_GradientVectorFlow',$
          'C_sImageFilter_PatternKernel',$
          'C_sImageFilter_PatternKernelSaver',$
          'C_sImageFilter_GaussianKernel_FRAP',$
          'C_sImageFilter_GradientImage',$
          'C_sImageFilter_HaarWavelet',$
          'C_sImageFilter_HistEqualize',$
          'C_sImageFilter_HistEqualizeAdapt',$
          'C_sImageFilter_Identity',$
          'C_sImageFilter_ImageBorderDistance',$
          'C_sImageFilter_ImageColocalization',$
          'C_sImageFilter_ImageColocDeltaR',$
          'C_sImageFilter_ImageColocCostes',$
          'C_sImageFilter_ImageColocWindow',$
          'C_sImageFilter_ImageCrossCorrelation',$
          'C_sImageFilter_ImageEqualize',$
          'C_sImageFilter_ImageFillNeighbours',$
          'C_sImageFilter_ImageFillNeighboursRange',$ ; Susana - new filter based o C_sImageFilter_ImageFillNeighbours
          'C_sImageFilter_ImageFillNeighboursRangeTCh',$ ; Susana - new filter based o C_sImageFilter_ImageFillNeighbours, filtro incluye tPos clusPos zPos
          'C_sImageFilter_ImageErasePixel',$ ; Susana - Creating new filter based on C_sImageFilter_ImageFillNeighbours filter
          'C_sImageFilter_ImageFillLimitedNeighbours',$
          'C_sImageFilter_ImageFRET',$
          'C_sImageFilter_ImageMath',$
          'C_sImageFilter_ImageMaskMath',$
          'C_sImageFilter_ImagePSFSignalSimulator',$
          'C_sImageFilter_ImagePSF_3D_SignalSimulator',$
          'C_sImageFilter_ImageIsoAlpha',$
          'C_sImageFilter_ImageDifference',$
          'C_sImageFilter_ImageZProjection',$
          ;'C_sImageFilter_Image_Find_Diffusion_Constants',$  ;  NO existe?????
          'C_sImageFilter_Image_Photobleaching_Correction',$
          'C_sImageFilter_Image_TZ_IntensityProfile',$
          'C_sImageFilter_Image_TChZ_Projection',$
          'C_sImageFilter_IntensitySpan',$
          'C_sImageFilter_IntensitySteps',$
          'C_sImageFilter_Inverse',$
          'C_sImageFilter_IsoAlpha',$
          'C_sImageFilter_IsoAlphaWeight',$
          'C_sImageFilter_kMeans',$
          'C_sImageFilter_LocalMetaFilterROIs',$
          'C_sImageFilter_LogicalFilter',$
          'C_sImageFilter_MathematicalFilter',$
          ;'C_sImageFilter_LogicalFilterInTime',$ ; Al parecer tampoco existe????
          'C_sImageFilter_MorphDistance',$
          'C_sImageFilter_MorphTophat',$
          'C_sImageFilter_MeanTimeNormalization',$
          'C_sImageFilter_Median',$
          'C_sImageFilter_NoiseGaussAddition',$
          'C_sImageFilter_NoiseSimulator',$
          'C_sImageFilter_OpenClose',$
          'C_sImageFilter_OpenCloseAdvanced',$
          'C_sImageFilter_OpticalFlowC',$
          'C_sImageFilter_OpticalFlowAlphaTest',$
          'C_sImageFilter_OpticalFlowFrontTest',$
          'c_sImageFilter_PatternKernel',$
          'C_sImageFilter_Roberts',$
          'C_sImageFilter_sMeanImage',$
          'C_sImageFilter_ScaleIntensity',$
          'C_sImageFilter_ScrambleImage',$
          'C_sImageFilter_ShiftXY',$
          'C_sImageFilter_SizeSelect',$
          'C_sImageFilter_Skeleton',$
          'C_sImageFilter_SkeletonMedialAxis',$
          'C_sImageFilter_Sobel',$
          'C_sImageFilter_SomitesLocatorOnBF',$   
          'C_sImageFilter_SpikeFilter',$
          'C_sImageFilter_SwitchObjectNumbers',$
          'C_sImageFilter_Threshold',$
          'C_sImageFilter_Threshold_Binary',$
          'C_sImageFilter_Threshold_FullScale',$
          'C_sImageFilter_ThresholdOtsu',$
          'C_sImageFilter_ThinningACL80',$
          'C_sImageFilter_ThinningFrap',$
          'C_sImageFilter_ThinOnly',$
          'C_sImageFilter_TouchBorder',$
          'C_sImageFilter_Void',$
          'C_sImageFilter_WaterShed',$
          'C_sImageFilter_ImageVoronoiByMorphDistance',$
          ; NC filters adapted to real use... FASL... XD
          'C_sImageFilter_CentredImages',$
          'C_sImageFilter_ConfinedGrowthFilter',$
          'C_sImageFilter_DynamicIndex',$
          'NC_C_sImageFilter_ByteScale',$
          'NC_C_sImageFilter_FillRemove',$
          'NC_C_sImageFilter_FillNeighbours',$
          'NC_C_sImageFilter_1stDeviationKernel',$
          'NC_C_sImageFilter_OpenClose',$
          'NC_C_sImageFilter_OpenCloseAdvanced',$
          'NC_C_sImageFilter_SizeSelect',$
          'NC_C_sImageFilter_ThresholdOtsu',$
          'NC_C_sImageFilter_TouchBorder']
end


pro s_ISegM_SaveROI2DGroup, stack_tlb = stack_tlb, oROI2DGroup = oROI2DGroup, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not(obj_valid(oImage)) then return
    parameterPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]

    oImage->get, pParamStruct = pParamStruct
    imageName = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
    save, oROI2DGroup, fileName = strCompress(parameterPath + strCompress('oROI2DGroup' + '_Clus' + string(clusPos)+'_' +  imageName + '.sav',/rem))
    obj_destroy, oROI2DGroup
end


function s_initROI2DGroupObjFromImageObj, stack_tlb = stack_tlb, saveGroup = saveGroup, fApplyForAllClusters = fApplyForAllClusters, objNumberVector = objNumberVector,$
                                          tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, maskImage = maskImage, intImage = intImage, fileName = fileName

    clusStart = clusPos
    if (n_elements(fApplyForAllClusters) ne 0) then if (fApplyForAllClusters eq 1) then clusStart = 0

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       if (n_elements(maskImage) eq 0) then maskImage = (*stackState.pImageStackInfoObject)->getSelectedClusterMask(tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusStart)
       if (n_elements(intImage) eq 0) then intImage = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not(obj_valid(oImage)) then return, -1

    dimMaskImage = size(maskImage, /dim)
    xyzPixSize = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] < dimMaskImage[0],$
                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] < dimMaskImage[1],$
                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] ]
    zSliceInterval = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Interval [real]'))[0]]
    parameterPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]
    oImage->get, pParamStruct = pParamStruct

    totalClusNum = clusPos
    if (n_elements(fApplyForAllClusters) ne 0) then if fApplyForAllClusters then totalClusNum = (oImage->getSegClusterNumber())-1

    for clusCount = clusStart, totalClusNum do begin
       widget_control, stack_tlb, get_uValue = stackState, /no_copy
         if (clusCount ne clusStart) then maskImage = (*stackState.pImageStackInfoObject)->getSelectedClusterMask(tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusCount)
       widget_control, stack_tlb, set_uValue = stackState, /no_copy

       oName = strCompress('_Clus' + string(clusCount)+'_', /rem) +  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
       oROI2DGroup = obj_new('C_sROIGroupObj',$
                              name = oName,$
                              tPos = tPos,$
                              chPos = chPos,$
                              zPos = zPos,$
                              zSliceInterval = zSliceInterval,$
                              zSliceReal = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Position [real]'))[0]],$
                              timeAfterStart = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]],$
                              xyzPixSize = xyzPixSize,$
                              frameRealSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]],$
                                                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] ],$
                              xyzShowBoxPosition = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]],$
                                                                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]],$
                                                                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]],$
                                                                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]],$
                                                                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'zSegmentBox [z0]'))[0]],$
                                                                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'zSegmentBox [z1]'))[0]] ],$
                              ROIGroupShowBoxPixSize = [size(maskImage, /dim)],$
                              hideOnOff = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Hide On/Off'))[0]] )

         ; TODO add eightNeighbor option to s_Seg->Option->Create"DModel !!!
       oROI2DGroup->addROIObjectsFromSegImage, maskImage = maskImage, intImage = congrid(intImage, dimMaskImage[0], dimMaskImage[1]), objNumberVector = objNumberVector, eightNeighbor = 1b
       if (n_elements(saveGroup) ne 0) then s_ISegM_SaveROI2DGroup, stack_tlb = stack_tlb, oROI2DGroup = oROI2DGroup, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusCount
    endfor

    fileName = strCompress(parameterPath + strCompress('oROI2DGroup' + oName + '.sav', /rem))
    if obj_valid(oROI2DGroup) then return, oROI2DGroup else return, -1
end


function s_initROI2DGroupObjFromPaintedMasks, stack_tlb = stack_tlb, saveGroup = saveGroup, fApplyForAllClusters = fApplyForAllClusters, objNumberVector = objNumberVector,$
                                              tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, maskImage = maskImage, intImage = intImage, fileName = fileName,$
                                              maskPathCacheStruct = maskPathCacheStruct
  clusStart = clusPos
  if (n_elements(fApplyForAllClusters) ne 0) then if (fApplyForAllClusters eq 1) then clusStart = 0

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    if (n_elements(maskImage) eq 0) then maskImage = (*stackState.pImageStackInfoObject)->getSelectedClusterMask(tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusStart)
    if (n_elements(intImage) eq 0) then intImage = (*stackState.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
    oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  if ~obj_valid(oImage) then return, -1

  dimMaskImage   = size(maskImage, /dim)
  xyzPixSize     = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] < dimMaskImage[0],$
                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]] < dimMaskImage[1],$
                    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] ]
  zSliceInterval = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Interval [real]'))[0]]
  parameterPath  = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]
  oImage->get, pParamStruct = pParamStruct

  totalClusNum = clusPos
  if (n_elements(fApplyForAllClusters) ne 0) then if fApplyForAllClusters then totalClusNum = (oImage->getSegClusterNumber())-1

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = 0)
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  if not(obj_valid(oImage)) then return, -1

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, totalZNum = totalZNum
;  totalZNum = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]

  zSliceInterval = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Interval [real]'))[0]]
  oImage->get, pParamStruct = pParamStruct

  clusCount    = clusPos
  totalClusNum = clusPos

  if (n_elements(fApplyForAllClusters) ne 0) then if fApplyForAllClusters then begin
    clusCount = 0
    totalClusNum = (oImage->getSegClusterNumber())-1
  endif

  for clusCount = clusCount, totalClusNum do begin
    oROI2DGroup = obj_new('C_sROIGroupObj', $;clusPos            = clusCount,$
                                            tPos               = tPos,$
                                            chPos              = chPos,$
                                            zPos               = zPos,$
                                            zSliceInterval     = zSliceInterval,$
                                            zSliceReal         = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Position [real]'))[0]],$
                                            timeAfterStart     = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]],$
                                            xyzPixSize         = xyzPixSize,$
                                            frameRealSize      = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]],$
                                                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]]],$
                                            xyzShowBoxPosition = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x0]'))[0]],$
                                                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'xSegmentBox [x1]'))[0]],$
                                                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y0]'))[0]],$
                                                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ySegmentBox [y1]'))[0]],$
                                                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'zSegmentBox [z0]'))[0]],$
                                                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'zSegmentBox [z1]'))[0]]],$
                                            ROIGroupShowBoxPixSize = [size(maskImage, /dim)],$
                                            hideOnOff = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Hide On/Off'))[0]] )

     oROI2DGroup->init2DModelsFromPaintedMasks, stack_tlb = stack_tlb, maskPathCacheStruct = maskPathCacheStruct, fValueType = fValueType

     if (n_elements(saveGroup) ne 0) then s_ISegM_SaveROI2DGroup, stack_tlb = stack_tlb, oROI2DGroup = oROI2DGroup, tPos = tPos, chPos = chPos, clusPos = clusCount, zPos = zPos
  endfor
  return, obj_valid(oROI2DGroup) ? oROI2DGroup : -1
end


function s_ISegM_GetROI2DGroup, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, fileName = fileName
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if (n_elements(clusPos) eq 0) then clusPos = 0
    if not(obj_valid(oImage)) then return, -1
    parameterPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]

    oImage->get, pParamStruct = pParamStruct
    imageName = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
    fileName = strCompress(parameterPath + strCompress('oROI2DGroup' + '_Clus' + string(clusPos)+'_' +  imageName + '.sav', /rem))
    openr, 1, fileName, error = err
    close, /all
    if (err eq 0) then begin
       restore, fileName = fileName, restored_objects = oROI2DGroup, /relaxed
       for i = 0, (n_elements(oROI2DGroup)-1) do if obj_isa(oROI2DGroup[i], 'C_sROIGroupObj') then return, oROI2DGroup[i]
    endif else return, -1
end


pro s_ISegM_SaveROI3DGroup, stack_tlb = stack_tlb, oROI3DGroup = oROI3DGroup, tPos = tPos, chPos = chPos, clusPos = clusPos
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = 0)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not(obj_valid(oImage)) then return
    parameterPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]

    oImage->get, pParamStruct = pParamStruct
    imageName = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
    save, oROI3DGroup, fileName = strCompress(parameterPath + strCompress('oROI3DGroup' + '_Clus' + string(clusPos)+'_' +  imageName + '.sav', /rem))
    obj_destroy, oROI3DGroup
end


function s_initEmptyROI3DGroupObj, stack_tlb = stack_tlb, saveGroup = saveGroup, tPos = tPos, chPos = chPos, zSliceInitial = zSliceInitial, clusPos = clusPos

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zSliceInitial)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not (obj_valid(oImage)) then return, -1

    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, totalZNum = totalZNum
;    totalZNum = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]
    zSliceInterval = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'z-Interval [real]'))[0]]
    oImage->get, pParamStruct = pParamStruct

    oROI3DGroup = obj_new('C_sROI3DGroupObj', clusPos = clusPos,$
                                              tPos = tPos,$
                                              chPos = chPos,$
                                              zPos = zSliceInitial,$
                                              zSliceInitial = zSliceInitial,$
                                              timeAfterStart = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]],$
                                              xyzFramePixSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                                                                  *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
                                                                  totalZNum ],$
                                              xyzFrameRealSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]],$
                                                                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]],$
                                                                   totalZNum * zSliceInterval] )

    if (n_elements(saveGroup) ne 0) then s_ISegM_SaveROI3DGroup, stack_tlb = stack_tlb, oROI3DGroup = oROI3DGroup, tPos = tPos, chPos = chPos, clusPos = clusPos
    if obj_valid(oROI3DGroup) then return, oROI3DGroup else return, -1
end


function s_initROI3DGroupObjFrom2DObjects, stack_tlb = stack_tlb, saveGroup = saveGroup, fApplyForAllClusters = fApplyForAllClusters, fUpdate3DROIWithGrowthFactor = fUpdate3DROIWithGrowthFactor,$
                                              tPos = tPos, chPos = chPos, zSliceInitial = zSliceInitial,$
                                              clusPos = clusPos, fAcceptAllObjects = fAcceptAllObjects

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zSliceInitial)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not(obj_valid(oImage)) then return, -1

    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, totalZNum = totalZNum
    zSliceInterval = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Interval [real]'))[0]]
    oImage->get, pParamStruct = pParamStruct

    clusCount = clusPos
    totalClusNum = clusPos
    if (n_elements(fApplyForAllClusters) ne 0) then if fApplyForAllClusters then begin
       clusCount = 0
       totalClusNum = (oImage->getSegClusterNumber())-1
    endif

    for clusCount = clusCount, totalClusNum do begin
       oROI3DGroup = obj_new('C_sROI3DGroupObj', clusPos = clusCount,$
                                                 tPos = tPos,$
                                                 chPos = chPos,$
                                                 zPos = zSliceInitial,$
                                                 zSliceInitial = zSliceInitial,$
                                                 timeAfterStart = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]],$
                                                 xyzFramePixSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                                                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
                                                                     totalZNum ],$
                                                 xyzFrameRealSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]],$
                                                                      *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]],$
                                                                      totalZNum * zSliceInterval] )

       oROI3DGroup->initialize3DModels, stack_tlb = stack_tlb, fAcceptAllObjects = fAcceptAllObjects
       if (n_elements(fUpdate3DROIWithGrowthFactor) ne 0) then if (fUpdate3DROIWithGrowthFactor eq 1) then oROI3DGroup->update3DModelWithGrowthFactor, maxGrowthFactor = maxGrowthFactor
       if (n_elements(saveGroup) ne 0) then s_ISegM_SaveROI3DGroup, stack_tlb = stack_tlb, oROI3DGroup = oROI3DGroup, tPos = tPos, chPos = chPos, clusPos = clusCount
    endfor
    if obj_valid(oROI3DGroup) then return, oROI3DGroup else return, -1
end


function s_initROI3DGroupObjFromPaintedMasks, stack_tlb = stack_tlb, saveGroup = saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                              tPos = tPos, chPos = chPos,clusPos = clusPos, fValueType = fValueType

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = 0)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not(obj_valid(oImage)) then return, -1

    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, totalZNum = totalZNum
;    totalZNum = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]

    zSliceInterval = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Interval [real]'))[0]]
    oImage->get, pParamStruct = pParamStruct

    clusCount = clusPos
    totalClusNum = clusPos
    if (n_elements(fApplyForAllClusters) ne 0) then if fApplyForAllClusters then begin
       clusCount = 0
       totalClusNum = (oImage->getSegClusterNumber())-1
    endif

    for clusCount = clusCount, totalClusNum do begin
       oROI3DGroup = obj_new('C_sROI3DGroupObj', clusPos = clusCount,$
                                                 tPos = tPos,$
                                                 chPos = chPos,$
                                                 zPos = 0,$
                                                 zSliceInitial = 0,$
                                                 timeAfterStart = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]],$
                                                 xyzFramePixSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                                                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
                                                                     totalZNum ],$
                                                 xyzFrameRealSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]],$
                                                                      *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]],$
                                                                      totalZNum * zSliceInterval] )

       oROI3DGroup->init3DModelsFromPaintedMasks, stack_tlb = stack_tlb, maskPathCacheStruct = maskPathCacheStruct, fValueType = fValueType
       
       if (n_elements(saveGroup) ne 0) then s_ISegM_SaveROI3DGroup, stack_tlb = stack_tlb, oROI3DGroup = oROI3DGroup, tPos = tPos, chPos = chPos, clusPos = clusCount
    endfor
    if obj_valid(oROI3DGroup) then return, oROI3DGroup else return, -1
end


function s_initROI3DGroupObjFromNumberedMasks, stack_tlb = stack_tlb, saveGroup = saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                 tPos = tPos, chPos = chPos,clusPos = clusPos

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = 0)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not(obj_valid(oImage)) then return, -1

    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, totalZNum = totalZNum
    totalZNum = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]
    zSliceInterval = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Interval [real]'))[0]]
    oImage->get, pParamStruct = pParamStruct

    clusCount = clusPos
    totalClusNum = clusPos
    if (n_elements(fApplyForAllClusters) ne 0) then if fApplyForAllClusters then begin
       clusCount = 0
       totalClusNum = (oImage->getSegClusterNumber())-1
    endif

    for clusCount = clusCount, totalClusNum do begin
       oROI3DGroup = obj_new('C_sROI3DGroupObj', clusPos = clusCount,$
                                                 tPos = tPos,$
                                                 chPos = chPos,$
                                                 zPos = 0,$
                                                 zSliceInitial = 0,$
                                                 timeAfterStart = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time after start [s]'))[0]],$
                                                 xyzFramePixSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                                                                     *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
                                                                     totalZNum ],$
                                                 xyzFrameRealSize = [ *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]],$
                                                                      *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]],$
                                                                      totalZNum * zSliceInterval] )

       oROI3DGroup->init3DModelsFromNumberedMasks, stack_tlb = stack_tlb
       if (n_elements(saveGroup) ne 0) then s_ISegM_SaveROI3DGroup, stack_tlb = stack_tlb, oROI3DGroup = oROI3DGroup, tPos = tPos, chPos = chPos, clusPos = clusCount
    endfor
    if obj_valid(oROI3DGroup) then return, oROI3DGroup else return, -1
end


function s_ISegM_GetROI3DGroup, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos, fileName = fileName
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = 0)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not(obj_valid(oImage)) then return, -1
    parameterPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]

    oImage->get, pParamStruct = pParamStruct
    imageName = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
    fileName = strCompress(parameterPath + strCompress('oROI3DGroup' + '_Clus' + string(clusPos)+'_' +  imageName + '.sav', /rem))
    openr, 1, fileName, error = err
    close, /all
    if (err eq 0) then begin
       restore, fileName = fileName, restored_objects = oROI3DGroup, /relaxed
       for i = 0L, (n_elements(oROI3DGroup)-1) do if obj_isa(oROI3DGroup[i], 'C_sROI3DGroupObj') then return, oROI3DGroup[i]
    endif else return, -1
end


pro s_ISegM_makeAndSaveImageMask, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos,$
                                  fApplyForAllClusters = fApplyForAllClusters, fSaveSegWithName = fSaveSegWithName, objValid = objValid

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if not(obj_valid(oImage)) then return

    clusCount = clusPos
    totalClusNum = clusPos
    if (n_elements(fApplyForAllClusters) ne 0) then if fApplyForAllClusters then begin
       clusCount = 0
       totalClusNum = (oImage->getSegClusterNumber())-1
    endif
    maskPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]]
    oImage->get, pParamStruct = pParamStruct
    imageName = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]

    for clusCount = clusCount, totalClusNum do begin
       s_ISM_setSelectedImageClusterSegPosition, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusCount
       if fSaveSegWithName then begin
         s_ISM_getProjectInfo, stack_tlb = stack_tlb, segPos = segPos
         fileName = strCompress(maskPath + strCompress('_Clus' + string(clusCount)+'_', /rem) + strcompress('_SegNum' + string(segPos)+'_',/rem) + imageName+'.tif')
       endif else fileName = strCompress(maskPath + strCompress('_Clus' + string(clusCount)+'_', /rem) + imageName + '.tif')

       widget_control, stack_tlb, get_uValue = stackState, /no_copy
         ev_struct = {top: stackState.wTopBase,$
                      id: stackState.wListzSlice,$
                      handler: stackState.wTopBase,$
                      index: zPos,$
                      clicks: 1}
       widget_control, stack_tlb, set_uValue = stackState, /no_copy
       s_ISM_List_Event, ev_struct

       widget_control, stack_tlb, get_uValue = stackState, /no_copy
       widget_control, stackState.child_ViewWindow_tlb, get_uValue = stateView, /no_copy
          if (max(*stateView.subimage) ne 1) then write_tiff, fileName, *stateView.subimage else write_tiff, fileName, ((*stateView.subimage) * 255)
       widget_control, stackState.child_ViewWindow_tlb, set_uValue = stateView, /no_copy
       widget_control, stack_tlb, set_uValue = stackState, /no_copy
    endfor
end


pro s_ISegM_CopyPasteSegMethod, wTopBase, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy

    if obj_valid(oImage) then begin
       widget_control, wTopBase, get_uValue = state, /no_copy
       case 1 of
         state.fCopySingleClusterProcedure and obj_valid(state.oCopySingleClusterContainer):  begin
              dummy = state.oCopySingleClusterContainer
              save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
              nCluster = (oImage->getSegClusterNumber())-1
              if (state.fActiveSingleCluster gt nCluster) then for i = nCluster, (state.fActiveSingleCluster-1) do oImage->addSegContainerObj
              oImage->deleteSegContainerObj, position = state.fActiveSingleCluster
              restore, s_getPathForSystem()+'obj.tmp', restored_objects = dummy, /relaxed
              if (n_elements(clusPos) gt 0) then oImage->addSegContainerObj, object = dummy[0], position = clusPos else oImage->addSegContainerObj, object = dummy[0], position = state.fActiveSingleCluster
         endcase
         state.fCopyAllClusterProcedures and obj_valid(state.oCopyAllClusterContainer): begin
              dummy = state.oCopyAllClusterContainer
              save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
              restore, s_getPathForSystem()+'obj.tmp', restored_objects = dummy, /relaxed
              oImage->setSegContainer, object = dummy[0]
         endcase
         (n_elements(clusPos) gt 0): begin
              nCluster = (oImage->getSegClusterNumber())-1
              if (clusPos gt nCluster) then for i = nCluster, (clusPos-1) do oImage->addSegContainerObj
              oImage->deleteSegContainerObj, position = clusPos
              restore, s_getPathForSystem()+'obj.tmp', restored_objects = dummy, /relaxed
              oImage->addSegContainerObj, object = dummy[0], position = clusPos
         endcase
         else:
       endcase
    widget_control, wTopBase, set_uValue = state, /no_copy
    endif
end


pro s_ISegM_PrepareSegmentationConditions, ev
    widget_control, ev.top, get_uValue = state, /no_copy

         ; secure that View Window is switched on
       widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
         if not(widget_info(stackState.child_ViewWindow_tlb, /valid)) then begin
          ev_struct = {top: stackState.wTopBase,$
                       id: stackState.wViewWindowButtonOnOff}
          widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
          widget_control, ev.top, set_uValue = state, /no_copy
          s_ISM_Window_Button_Event, ev_struct
          widget_control, ev.top, get_uValue = state, /no_copy
          widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
         endif
       widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

         ; secure that the selected Segmentation Boxes in View Window are considered
       widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
         widget_control, stackState.child_ViewWindow_tlb, get_uValue = stateView, /no_copy
         if not(stateView.fShowSegBox) then begin
            ev_struct = {top: stackState.child_ViewWindow_tlb, id: stateView.wShowSegBoxOnOff}
            widget_control, stackState.child_ViewWindow_tlb, set_uValue = stateView, /no_copy
              widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                 widget_control, ev.top, set_uValue = state, /no_copy
                   s_Image_SZC_optControl, ev_struct
                 widget_control, ev.top, get_uValue = state, /no_copy
              widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
            widget_control, stackState.child_ViewWindow_tlb, get_uValue = stateView, /no_copy
          endif
         widget_control, stackState.child_ViewWindow_tlb, set_uValue = stateView, /no_copy
       widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

         ; secure that the Zoom Boxes in View Window is drawn
       widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
       widget_control, stackState.child_ViewWindow_tlb, get_uValue = stateView, /no_copy
       if not(widget_info(stateView.child_zoomWindow_tlb, /valid)) then begin
          stateView.zoomXSize = 10
          stateView.zoomYSize = 10
          wBaseID = stackState.child_ViewWindow_tlb
          if ptr_valid(stateView.subimage) then *stateView.subimage = bytArr(10,10)  else stateView.subimage = ptr_new( bytArr(10,10), /no_copy)
          widget_control, stackState.child_ViewWindow_tlb, set_uValue = stateView, /no_copy
              widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                 widget_control, ev.top, set_uValue = state, /no_copy
                   s_Image_SZC_updateZoomWindow, wBaseID = wBaseID
                 widget_control, ev.top, get_uValue = state, /no_copy
              widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
          widget_control, stackState.child_ViewWindow_tlb, get_uValue = stateView, /no_copy
       endif
       widget_control, stackState.child_ViewWindow_tlb, set_uValue = stateView, /no_copy
       widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Image_SegmentationManipulator_Window_Button_Event, ev

    widget_control, ev.top, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                               totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
                                               clusPos = clusPos, segPos = segPos
    update = 1b
    widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue
    case uValue of
       'OPENALLCLUSTERPROCEDURES': begin
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        file = dialog_pickFile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]], get_path = path, filter = '*.sav')
                        if (file ne '') then begin
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           if ( (strMid(file, strlen(file)-4, 4)) ne '.sav') then file = file + '.sav'
                           restore, file, restored_objects = dummy, /relaxed
                           dummy = dummy[0]
                           if (obj_class(dummy) eq 'IDL_CONTAINER') then begin
                              if (obj_class(dummy->get(position = 0)) eq 'IDL_CONTAINER') then begin
                              widget_control, state.wListSegCluster, get_uValue = uvalueCluster, /no_copy
                                  uvalueCluster.active = 0
                              widget_control, state.wListSegCluster, set_uValue = uvalueCluster, /no_copy
                              oImage->setSegContainer, object = dummy
                              endif else dummy = dialog_message('Choose correct file type !  ')
                           endif else dummy = dialog_message('Choose correct file type !  ')
                        endif
          endcase
       'SAVEFORSELECTEDZSLICE': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           file = dialog_pickFile( /write, file = 'void.tif', path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]], get_path = maskPath, filter = '*.tif')
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        if (file ne '') then begin
                           fCreateAndSave2DModel = state.fCreateAndSave2DModel
                           fCreateAndSavePainted2DModelObject = state.fCreateAndSavePainted2DModelObject
                           fCreateAndSave3DModel = state.fCreateAndSave3DModel
                           fCreateAndSavePainted3DModelObject = state.fCreateAndSavePainted3DModelObject
                           fCreateAndSave3DModelFromNumberedMasks = state.fCreateAndSave3DModelFromNumberedMasks
                           fAcceptAllObjects = state.fAcceptAllObjects
                           fApplyForAllClusters = state.fApplyForAllClusters
                           fUpdate3DROIWithGrowthFactor = state.fUpdate3DROIWithGrowthFactor
                           zSliceInitial = state.fZSliceInitial
                           fCreateAndSaveEmpty3DModelObject = state.fCreateAndSaveEmpty3DModelObject
                           fSaveSegWithName = state.fSaveSegWithName
                           fValueType = state.fValueDataRoi
                           
                           stack_tlb = state.stack_tlb
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = maskPath
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = maskPath

                           widget_control, ev.top, set_uValue = state, /no_copy
                             s_ISegM_PrepareSegmentationConditions, ev
                             s_ISegM_makeAndSaveImageMask, stack_tlb = stack_tlb, fApplyForAllClusters = fApplyForAllClusters, fSaveSegWithName = fSaveSegWithName,$
                                                           tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
                             if fCreateAndSave2DModel then $
                                oROI2DGroup = s_initROI2DGroupObjFromImageObj(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                              tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos)
                             if fCreateAndSavePainted2DModelObject then $
                                oROI2DGroup = s_initROI2DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                  tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, maskPathCacheStruct = maskPathCacheStruct)
                             if fCreateAndSaveEmpty3DModelObject then $
                                oROI3DGroup = s_initEmptyROI3DGroupObj(stack_tlb = stack_tlb, /saveGroup,$
                                                                       tPos = tPos, chPos = chPos, zSliceInitial = zSliceInitial, clusPos = clusPos)
                             if fCreateAndSave3DModel then $
                                oROI3DGroup = s_initROI3DGroupObjFrom2DObjects(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters, fAcceptAllObjects = fAcceptAllObjects,$
                                                                               fUpdate3DROIWithGrowthFactor = fUpdate3DROIWithGrowthFactor,$
                                                                               tPos = tPos, chPos = chPos, zSliceInitial = zSliceInitial, clusPos = clusPos)
                             if fCreateAndSavePainted3DModelObject then $
                                oROI3DGroup = s_initROI3DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                  tPos = tPos, chPos = chPos, clusPos = clusPos,fValueType = fValueType)
                             if fCreateAndSave3DModelFromNumberedMasks then $
                                oROI3DGroup = s_initROI3DGroupObjFromNumberedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                   tPos = tPos, chPos = chPos, clusPos = clusPos)
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
          endcase
       'SAVEFORALLZSLICES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           file = dialog_pickFile( /write, file = 'void.tif', path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]], get_path = maskPath, filter = '*.tif')
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        if (file ne '') then begin
                           fCreateAndSave2DModel = state.fCreateAndSave2DModel
                           fCreateAndSavePainted2DModelObject = state.fCreateAndSavePainted2DModelObject
                           fCreateAndSave3DModel = state.fCreateAndSave3DModel
                           fCreateAndSavePainted3DModelObject = state.fCreateAndSavePainted3DModelObject
                           fCreateAndSave3DModelFromNumberedMasks = state.fCreateAndSave3DModelFromNumberedMasks
                           fAcceptAllObjects = state.fAcceptAllObjects
                           fApplyForAllClusters = state.fApplyForAllClusters
                           fUpdate3DROIWithGrowthFactor = state.fUpdate3DROIWithGrowthFactor
                           zSliceInitial = state.fZSliceInitial
                           fSaveSegWithName = state.fSaveSegWithName
                           fValueType = state.fValueDataRoi
                           stack_tlb = state.stack_tlb
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = maskPath
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = maskPath

                           if state.fFromCurrentPosition then zStart = zPos else zStart = 0

                           widget_control, ev.top, set_uValue = state, /no_copy
                             s_ISegM_PrepareSegmentationConditions, ev
                             for i = zStart, totalZNum-1 do begin
                                s_ISegM_makeAndSaveImageMask, stack_tlb = stack_tlb, fApplyForAllClusters = fApplyForAllClusters, fSaveSegWithName = fSaveSegWithName,$
                                                              tPos = tPos, chPos = chPos, zPos = i, clusPos = clusPos
                                if fCreateAndSave2DModel then $
                                   oROI2DGroup = s_initROI2DGroupObjFromImageObj(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                 tPos = tPos, chPos = chPos, zPos = i, clusPos = clusPos)
                                if fCreateAndSavePainted2DModelObject then $
                                  oROI2DGroup = s_initROI2DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                    tPos = tPos, chPos = chPos, zPos = i, clusPos = clusPos, maskPathCacheStruct = maskPathCacheStruct)
                             endfor
                             if fCreateAndSave3DModel then $
                                oROI3DGroup = s_initROI3DGroupObjFrom2DObjects(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters, fAcceptAllObjects = fAcceptAllObjects,$
                                                                        fUpdate3DROIWithGrowthFactor = fUpdate3DROIWithGrowthFactor,$
                                                                        tPos = tPos, chPos = chPos, zSliceInitial = zSliceInitial, clusPos = clusPos)
                             if fCreateAndSavePainted3DModelObject then $
                                oROI3DGroup = s_initROI3DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                        tPos = tPos, chPos = chPos, clusPos = clusPos, fValueType = fValueType)
                             if fCreateAndSave3DModelFromNumberedMasks then $
                                oROI3DGroup = s_initROI3DGroupObjFromNumberedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                        tPos = tPos, chPos = chPos, clusPos = clusPos)
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
          endcase
       'SAVEFORALLZSLICESANDTIMES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           file = dialog_pickFile( /write, file = 'void.tif', path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]], get_path = maskPath, filter = '*.tif')
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        if (file ne '') then begin
                           fCreateAndSave2DModel = state.fCreateAndSave2DModel
                           fCreateAndSavePainted2DModelObject = state.fCreateAndSavePainted2DModelObject
                           fCreateAndSave3DModel = state.fCreateAndSave3DModel
                           fCreateAndSavePainted3DModelObject = state.fCreateAndSavePainted3DModelObject
                           fCreateAndSave3DModelFromNumberedMasks = state.fCreateAndSave3DModelFromNumberedMasks
                           fAcceptAllObjects = state.fAcceptAllObjects
                           fApplyForAllClusters = state.fApplyForAllClusters
                           fUpdate3DROIWithGrowthFactor = state.fUpdate3DROIWithGrowthFactor
                           zSliceInitial = state.fZSliceInitial
                           fSaveSegWithName = state.fSaveSegWithName
                           fValueType = state.fValueDataRoi
                           stack_tlb = state.stack_tlb
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = maskPath
                             *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = maskPath

                           if (state.fFromCurrentPosition eq 1) then begin
                              tStart = tPos
                              zStart = zPos
                           endif else begin
                              tStart = 0
                              zStart = 0
                           endelse
                           widget_control, ev.top, set_uValue = state, /no_copy
                              s_ISegM_PrepareSegmentationConditions, ev
                              for k = tStart, totalTNum-1 do begin
                                 s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZNum
                                 for i = zStart, totalZNum-1 do begin
                                    s_ISegM_makeAndSaveImageMask, stack_tlb = stack_tlb, fApplyForAllClusters = fApplyForAllClusters, fSaveSegWithName = fSaveSegWithName,$
                                                                  tPos = k, chPos = chPos, zPos = i, clusPos = clusPos
                                    if fCreateAndSave2DModel then $
                                       oROI2DGroup = s_initROI2DGroupObjFromImageObj(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                     tPos = k, chPos = chPos, zPos = i, clusPos = clusPos)
                                    if fCreateAndSavePainted2DModelObject then $
                                       oROI2DGroup = s_initROI2DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                         tPos = k, chPos = chPos, zPos = i, clusPos = clusPos, maskPathCacheStruct = maskPathCacheStruct)
                                 endfor
                                 if fCreateAndSave3DModel then $
                                    oROI3DGroup = s_initROI3DGroupObjFrom2DObjects(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters, fAcceptAllObjects = fAcceptAllObjects,$
                                                                                      fUpdate3DROIWithGrowthFactor = fUpdate3DROIWithGrowthFactor,$
                                                                                      tPos = k, chPos = chPos, zSliceInitial = zSliceInitial, clusPos = clusPos)
                                 if fCreateAndSavePainted3DModelObject then $
                                    oROI3DGroup = s_initROI3DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                         tPos = k, chPos = chPos, clusPos = clusPos, fValueType = fValueType)
                                 if fCreateAndSave3DModelFromNumberedMasks then $
                                    oROI3DGroup = s_initROI3DGroupObjFromNumberedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                          tPos = tPos, chPos = chPos, clusPos = clusPos)
                              endfor
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
          endcase
       'SAVEFORALLZSLICESANDCHANNELS': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           file = dialog_pickFile( /write, file = 'void.tif', path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]], get_path = maskPath, filter = '*.tif')
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        if (file ne '') then begin
                           fCreateAndSave2DModel = state.fCreateAndSave2DModel
                           fCreateAndSavePainted2DModelObject = state.fCreateAndSavePainted2DModelObject
                           fCreateAndSave3DModel = state.fCreateAndSave3DModel
                           fCreateAndSavePainted3DModelObject = state.fCreateAndSavePainted3DModelObject
                           fCreateAndSave3DModelFromNumberedMasks = state.fCreateAndSave3DModelFromNumberedMasks
                           fAcceptAllObjects = state.fAcceptAllObjects
                           fApplyForAllClusters = state.fApplyForAllClusters
                           fUpdate3DROIWithGrowthFactor = state.fUpdate3DROIWithGrowthFactor
                           zSliceInitial = state.fZSliceInitial
                           fSaveSegWithName = state.fSaveSegWithName
                           fValueType = state.fValueDataRoi
                           stack_tlb = state.stack_tlb
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = maskPath
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = maskPath

                           if state.fFromCurrentPosition then begin
                              chStart = chPos
                              zStart = zPos
                           endif else begin
                              chStart = 0
                              zStart = 0
                           endelse
                           widget_control, ev.top, set_uValue = state, /no_copy
                              s_ISegM_PrepareSegmentationConditions, ev
                              for j = chStart, totalChNum-1  do begin
                                 for i = zStart, totalZNum-1  do begin
                                    s_ISegM_makeAndSaveImageMask, stack_tlb = stack_tlb, fApplyForAllClusters = fApplyForAllClusters, fSaveSegWithName = fSaveSegWithName,$
                                                                  tPos = tPos, chPos = j, zPos = i, clusPos = clusPos

                                    if fCreateAndSave2DModel then $
                                       oROI2DGroup = s_initROI2DGroupObjFromImageObj(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                     tPos = tPos, chPos = j, zPos = i, clusPos = clusPos)
                                    if fCreateAndSavePainted2DModelObject then $
                                       oROI2DGroup = s_initROI2DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                         tPos = tPos, chPos = j, zPos = i, clusPos = clusPos, maskPathCacheStruct = maskPathCacheStruct)
                                 endfor
                                 if fCreateAndSave3DModel then $
                                    oROI3DGroup = s_initROI3DGroupObjFrom2DObjects(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters, fAcceptAllObjects = fAcceptAllObjects,$
                                                                                      fUpdate3DROIWithGrowthFactor = fUpdate3DROIWithGrowthFactor,$
                                                                                      tPos = tPos, chPos = j, zSliceInitial = zSliceInitial, clusPos = clusPos)
                                 if fCreateAndSavePainted3DModelObject then $
                                    oROI3DGroup = s_initROI3DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                         tPos = tPos, chPos = j, clusPos = clusPos, fValueType = fValueType)
                                 if fCreateAndSave3DModelFromNumberedMasks then $
                                    oROI3DGroup = s_initROI3DGroupObjFromNumberedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                          tPos = tPos, chPos = chPos, clusPos = clusPos)
                              endfor
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
          endcase
       'SAVEFORALLZSLICESANDCHANNELSANDTIMES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           file = dialog_pickFile( /write, file = 'void.tif', path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]], get_path = maskPath, filter = '*.tif')
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        if (file ne '') then begin
                           fCreateAndSave2DModel = state.fCreateAndSave2DModel
                           fCreateAndSavePainted2DModelObject = state.fCreateAndSavePainted2DModelObject
                           fCreateAndSave3DModel = state.fCreateAndSave3DModel
                           fCreateAndSavePainted3DModelObject = state.fCreateAndSavePainted3DModelObject
                           fCreateAndSave3DModelFromNumberedMasks = state.fCreateAndSave3DModelFromNumberedMasks
                           fAcceptAllObjects = state.fAcceptAllObjects
                           fApplyForAllClusters = state.fApplyForAllClusters
                           fUpdate3DROIWithGrowthFactor = state.fUpdate3DROIWithGrowthFactor
                           zSliceInitial = state.fZSliceInitial
                           fSaveSegWithName = state.fSaveSegWithName
                           fValueType = state.fValueDataRoi
                           stack_tlb = state.stack_tlb
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = maskPath
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = maskPath

                           if state.fFromCurrentPosition then begin
                              tStart = tPos
                              chStart = chPos
                              zStart = zPos
                           endif else begin
                              tStart = 0
                              chStart = 0
                              zStart = 0
                           endelse
                           widget_control, ev.top, set_uValue = state, /no_copy
                             s_ISegM_PrepareSegmentationConditions, ev
                             for k = tStart, totalTNum-1  do begin
                                s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalChNum = totalChNum
                                for j = chStart, totalChNum-1  do begin
                                   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZNum
                                   for i = zStart, totalZNum-1  do begin
                                      s_ISegM_makeAndSaveImageMask, stack_tlb = stack_tlb, fApplyForAllClusters = fApplyForAllClusters, fSaveSegWithName = fSaveSegWithName,$
                                                                    tPos = k, chPos = j, zPos = i, clusPos = clusPos
                                      if fCreateAndSave2DModel then $
                                         oROI2DGroup = s_initROI2DGroupObjFromImageObj(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                       tPos = k, chPos = j, zPos = i, clusPos = clusPos)
                                      if fCreateAndSavePainted2DModelObject then $
                                         oROI2DGroup = s_initROI2DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                           tPos = k, chPos = j, zPos = i, clusPos = clusPos, maskPathCacheStruct = maskPathCacheStruct)
                                   endfor
                                   if fCreateAndSave3DModel then $
                                      oROI3DGroup = s_initROI3DGroupObjFrom2DObjects(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters, fAcceptAllObjects = fAcceptAllObjects,$
                                                                                        fUpdate3DROIWithGrowthFactor = fUpdate3DROIWithGrowthFactor,$
                                                                                        tPos = k, chPos = j, zSliceInitial = zSliceInitial, clusPos = clusPos)
                                   if fCreateAndSavePainted3DModelObject then $
                                      oROI3DGroup = s_initROI3DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                           tPos = k, chPos = j, clusPos = clusPos, fValueType = fValueType)
                                   if fCreateAndSave3DModelFromNumberedMasks then $
                                      oROI3DGroup = s_initROI3DGroupObjFromNumberedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                              tPos = tPos, chPos = chPos, clusPos = clusPos)
                                endfor
                             endfor
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
          endcase
       'SAVEFORZSLICETHROUGHTIMES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           file = dialog_pickFile( /write, file = 'void.tif', path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]], get_path = maskPath, filter = '*.tif')
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        if (file ne '') then begin
                           fCreateAndSave2DModel = state.fCreateAndSave2DModel
                           fCreateAndSavePainted2DModelObject = state.fCreateAndSavePainted2DModelObject
                           fCreateAndSave3DModel = state.fCreateAndSave3DModel
                           fCreateAndSavePainted3DModelObject = state.fCreateAndSavePainted3DModelObject
                           fCreateAndSave3DModelFromNumberedMasks = state.fCreateAndSave3DModelFromNumberedMasks
                           fCreateAndSaveEmpty3DModelObject = state.fCreateAndSaveEmpty3DModelObject
                           fAcceptAllObjects = state.fAcceptAllObjects
                           fApplyForAllClusters = state.fApplyForAllClusters
                           fUpdate3DROIWithGrowthFactor = state.fUpdate3DROIWithGrowthFactor
                           zSliceInitial = state.fZSliceInitial
                           fSaveSegWithName = state.fSaveSegWithName
                           fValueType = state.fValueDataRoi
                           stack_tlb = state.stack_tlb
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Mask Path'))[0]] = maskPath
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = maskPath

                           widget_control, ev.top, set_uValue = state, /no_copy
                              s_ISegM_PrepareSegmentationConditions, ev
                              for k = 0, totalTNum-1  do begin
                                 s_ISegM_makeAndSaveImageMask, stack_tlb = stack_tlb, fApplyForAllClusters = fApplyForAllClusters, fSaveSegWithName = fSaveSegWithName,$
                                                               tPos = k, chPos = chPos, zPos = zPos, clusPos = clusPos
                                 if fCreateAndSave2DModel then $
                                    oROI2DGroup = s_initROI2DGroupObjFromImageObj(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                  tPos = k, chPos = chPos, zPos = zPos, clusPos = clusPos)
                                 if fCreateAndSavePainted2DModelObject then $
                                    oROI2DGroup = s_initROI2DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                      tPos = k, chPos = chPos, zPos = zPos, clusPos = clusPos, maskPathCacheStruct = maskPathCacheStruct)
                                 if fCreateAndSaveEmpty3DModelObject then $
                                    oROI3DGroup = s_initEmptyROI3DGroupObj(stack_tlb = stack_tlb, /saveGroup,$
                                                                           tPos = tPos, chPos = chPos, zSliceInitial = zSliceInitial, clusPos = clusPos)
                                 if fCreateAndSave3DModel then $
                                    oROI3DGroup = s_initROI3DGroupObjFrom2DObjects(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters, fAcceptAllObjects = fAcceptAllObjects,$
                                                                                   fUpdate3DROIWithGrowthFactor = fUpdate3DROIWithGrowthFactor,$
                                                                                   tPos = k, chPos = chPos, zSliceInitial = zSliceInitial, clusPos = clusPos)
                                 if fCreateAndSavePainted3DModelObject then $
                                    oROI3DGroup = s_initROI3DGroupObjFromPaintedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                      tPos = k, chPos = j, clusPos = clusPos, fValueType = fValueType)
                                 if fCreateAndSave3DModelFromNumberedMasks then $
                                    oROI3DGroup = s_initROI3DGroupObjFromNumberedMasks(stack_tlb = stack_tlb, /saveGroup, fApplyForAllClusters = fApplyForAllClusters,$
                                                                                       tPos = tPos, chPos = chPos, clusPos = clusPos)
                              endfor
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
          endcase
       'SAVEALLCLUSTERPROCEDURES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        file = dialog_pickFile( /write, file = 'void.tif', path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]], get_path = path, filter = '*.sav')
                        if (file ne '') then begin
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           if ( (strMid(file, strlen(file)-4, 4)) ne '.sav') then file = file + '.sav'
                           dummy = oImage->getSegContainer()
                           save, dummy, fileName = file
                           dummy = -1
                        endif
          endcase
       'SAVESINGLECLUSTERPROCEDURE': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        file = dialog_pickFile( /write, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]], get_path = path, filter = '*.sav')
                        if (file ne '') then begin
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           if ( (strMid(file, strlen(file)-4, 4)) ne '.sav') then file = file + '.sav'
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strMid(file,(strPos(file,path_sep(), /reverse_s))+1, (strPos(file,'.', /reverse_s)-strPos(file,path_sep(), /reverse_s)-1) )
                           dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = file
                           dummy = -1
                        endif
          endcase
       'ALLZSLICESANDCHANNELSANDTIMES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           for k = 0, totalTNum-1  do $
                              for j = 0, totalChNum-1  do $
                                 for i = 0, totalZNum-1  do $
                                    s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = k, chPos = j, zPos = i, clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ALLZSLICESANDCHANNELS': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           for j = 0, totalChNum-1  do $
                              for i = 0, totalZNum-1  do $
                                 s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = j, zPos = i, clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ALLZSLICESANDTIMES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           for k = 0, totalTNum-1  do $
                              for i = 0, totalZNum-1  do $
                                 s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = k, chPos = chPos, zPos = i, clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ALLZSLICES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           for i = 0, totalZNum-1  do $
                              s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = i, clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ZSLICEBEFORE': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = ((zPos-1)>0), clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ZSLICEBELOW': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = (zPos+1)<(totalZNum-1), clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ALLZSLICESBEFORE': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           for i = 0, zPos-1  do $
                              s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = i, clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ALLZSLICESBELOW': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           for i = zPos, totalZNum-1 do $
                              s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = i, clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ZSLICETHROUGHTIMES': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           for k = 0, totalTNum-1  do $
                              s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = k, chPos = chPos, zPos = zPos, clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ALLSLICETHROUGHTIMES': begin
                        update = 0b
                        for k = 0, totalTNum-1  do for i = 0, totalZNum-1  do begin
                           widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                              oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = i)
                           widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                           dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                           widget_control, ev.top, set_uValue = state, /no_copy
                              s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = k, chPos = chPos, zPos = i, clusPos = clusPos
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endfor
          endcase
       'ZSLICETHROUGHCHANNELS': begin
                        update = 0b
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        dummy = oImage->getSegContainerObj(active = clusPos)
                        save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                        widget_control, ev.top, set_uValue = state, /no_copy
                           for k = 0, totalChNum-1 do $
                              s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = k, zPos = zPos, clusPos = clusPos
                        widget_control, ev.top, get_uValue = state, /no_copy
          endcase
       'ALLSLICETHROUGHCHANNELS': begin
                        update = 0b
                        for k = 0, totalChNum-1  do for i = 0, totalZNum-1  do begin
                           widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                              oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = i)
                           widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                           dummy = oImage->getSegContainerObj(active = clusPos)
                           save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
                           widget_control, ev.top, set_uValue = state, /no_copy
                              s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = k, zPos = i, clusPos = clusPos
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endfor
          endcase
       'COPYALLCLUSTERPROCEDURESONOFF': begin
                           state.fCopyAllClusterProcedures = s_ToggleButtonOnOffState(state.wCopyAllClusterProceduresOnOff)

                           if obj_valid(state.oCopyAllClusterContainer) then begin
                             for j = (state.oCopyAllClusterContainer->count())-1, 0, -1 do begin
                              oSingleClusterContainer = state.oCopyAllClusterContainer->get(position = j)
                              if obj_valid(oSingleClusterContainer) then begin
                                  for i = (oSingleClusterContainer->count())-1, 0, -1 do begin
                                     obj = (oSingleClusterContainer)->get(position = i)
                                     oSingleClusterContainer->remove, position = i
                                     obj_destroy, obj
                                  endfor
                                  state.oCopyAllClusterContainer->remove, position = j
                                  obj_destroy, oSingleClusterContainer
                              endif
                             endfor
                             obj_destroy, state.oCopyAllClusterContainer
                           endif

                           if state.fCopyAllClusterProcedures then begin
                             if state.fCopySingleClusterProcedure then begin
                              state.fCopySingleClusterProcedure = s_ToggleButtonOnOffState(state.wCopySingleClusterProcedureOnOff)
                              if obj_valid(state.oCopySingleClusterContainer) then begin
                                  for i = (state.oCopySingleClusterContainer->count())-1, 0, -1 do begin
                                     obj = (state.oCopySingleClusterContainer)->get(position = i)
                                     (state.oCopySingleClusterContainer)->remove, position = i
                                     obj_destroy, obj
                                  endfor
                                  obj_destroy, state.oCopySingleClusterContainer
                              endif
                              state.fActiveSingleCluster = -1
                             endif

                             widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                                oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                             widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                             oSegContainer = oImage->getSegContainer()
                                save, oSegContainer, fileName = s_getPathForSystem()+'obj.tmp'
                                restore, s_getPathForSystem()+'obj.tmp', restored_objects = dummy, /relaxed
                             state.oCopyAllClusterContainer = dummy[0]
                           endif
          endcase
       'COPYSINGLECLUSTERPROCEDURESONOFF':  begin
                           dummy = s_ToggleButtonOnOffState(state.wCopySingleClusterProcedureOnOff)
                           state.fCopySingleClusterProcedure = dummy

                           if obj_valid(state.oCopySingleClusterContainer) then begin
                             for i = (state.oCopySingleClusterContainer->count())-1, 0, -1 do begin
                               obj = (state.oCopySingleClusterContainer)->get(position = i)
                               (state.oCopySingleClusterContainer)->remove, position = i
                               obj_destroy, obj
                             endfor
                             obj_destroy, state.oCopySingleClusterContainer
                           endif
                           state.fActiveSingleCluster = -1

                           if state.fCopySingleClusterProcedure then begin
                             if state.fCopyAllClusterProcedures then begin
                              state.fCopyAllClusterProcedures = s_ToggleButtonOnOffState(state.wCopyAllClusterProceduresOnOff)
                              for j = (state.oCopyAllClusterContainer->count())-1, 0, -1 do begin
                                  oSingleClusterContainer = state.oCopyAllClusterContainer->get(position = j)
                                  if obj_valid(oSingleClusterContainer) then begin
                                     for i = (oSingleClusterContainer->count())-1, 0, -1 do begin
                                       obj = oSingleClusterContainer->get(position = i)
                                       oSingleClusterContainer->remove, position = i
                                       obj_destroy, obj
                                     endfor
                                     state.oCopyAllClusterContainer->remove, position = j
                                     obj_destroy, oSingleClusterContainer
                                  endif
                              endfor
                              obj_destroy, state.oCopyAllClusterContainer
                             endif

                             widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                              oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                             widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                             oSegContainer = oImage->getSegContainerObj(active = clusPos)
                             save, oSegContainer, fileName = s_getPathForSystem()+'obj.tmp'
                             restore, s_getPathForSystem()+'obj.tmp', restored_objects = dummy, /relaxed
                             state.oCopySingleClusterContainer = dummy[0]
                             state.fActiveSingleCluster = clusPos
                           endif
          endcase
       'OPENSINGLECLUSTERPROCEDURE': begin
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        file = dialog_pickFile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]], get_path = path, filter = '*.sav')
                        if (file ne '') then begin
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]] = path
                           if ( strMid(file, strlen(file)-4, 4) ne '.sav') then file = file + '.sav'
                           *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]] = strMid(file,(strPos(file,path_sep(), /reverse_s))+1, (strPos(file,'.', /reverse_s)-strPos(file,path_sep(), /reverse_s)-1) )
                           restore, file, restored_objects = dummies, /relaxed
                           if obj_valid(dummies[0]) then for i = 0, n_elements(dummies)-1 do if (obj_class(dummies[i]) eq 'IDL_CONTAINER') then dummy = dummies[i]
                           if (obj_class(dummy) eq 'IDL_CONTAINER') then begin
                              if (strMid(obj_class(dummy->get(position = 0)), 0, 14) eq 'C_SIMAGEFILTER') then begin
                                  oImage->deleteSegContainerObj, position = clusPos
                                  oImage->addSegContainerObj, object = dummy, position = clusPos
                              endif else dummy = dialog_message('Choose correct file type !  ')
                           endif else dummy = dialog_message('Choose correct file type !  ')
                        endif
          endcase
       'ADDCLUSTER': begin
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        oImage->addSegContainerObj
                        widget_control, state.wListSegCluster, get_uValue = uvalueCluster, /no_copy
                           uvalueCluster.active = (oImage->getSegClusterNumber())-1
                        widget_control, state.wListSegCluster, set_uValue = uvalueCluster, /no_copy
          endcase
       'DELETECLUSTER': begin
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        if (oImage->getSegClusterNumber() gt 1) then begin
                           widget_control, state.wListSegCluster, get_uValue = uvalueCluster, /no_copy
                           oImage->DeleteSegContainerObj, position = uvalueCluster.active
                           uvalueCluster.active = ((oImage->getSegClusterNumber())-1) > 0
                           widget_control, state.wListSegCluster, set_uValue = uvalueCluster, /no_copy
                        endif
          endcase
       'SELECTTHRESHFROMHIST': begin
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           if widget_info(stackState.child_ViewWindow_tlb, /valid) then begin
                             widget_control, stackState.child_ViewWindow_tlb, get_uValue = viewState, /no_copy
                             if widget_info(viewState.child_HistSegWin_tlb, /valid) then begin
                              widget_control, viewState.child_HistSegWin_tlb, get_uValue = histState, /no_copy
                                 oHistTheshold = histState.oHistPlotView->getThresholdObj()
                                 save, oHistTheshold, fileName = s_getPathForSystem()+'obj.tmp'
                              widget_control, viewState.child_HistSegWin_tlb, set_uValue = histState, /no_copy
                             endif
                             widget_control, stackState.child_ViewWindow_tlb, set_uValue = viewState, /no_copy
                           endif
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                        if obj_valid(oHistTheshold) then begin
                           widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                             oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                           widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                           restore, s_getPathForSystem()+'obj.tmp', restored_objects = oThresh, /relaxed
                           oSegContainer = oImage->getSegContainerObj(active = clusPos)
                           oSegContainer->add, oThresh[0]

                           widget_control, state.wListSelectSegObj, get_uValue = uvalueFileParameter, /no_copy
                             uvalueFileParameter.active = oSegContainer->count()-1
                           widget_control, state.wListSelectSegObj, set_uValue = uvalueFileParameter, /no_copy
                        endif
       endcase
       'COPYSELECTEDMETHOD': begin
                   widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                      oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                   widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                   oSegContainer = oImage->getSegContainerObj(active = clusPos)
                   oSegMethod = oSegContainer->get(position = segPos)
                   if obj_valid(oSegMethod) then begin
                     segPos = segPos + 1
                     save, oSegMethod, fileName = s_getPathForSystem() + 'obj.tmp'
                     restore, s_getPathForSystem() + 'obj.tmp', restored_objects = oSegMethod, /relaxed
                     oSegContainer->add, oSegMethod[0], position = segPos
                     widget_control, state.wListSelectSegObj, get_uValue = uvalueFileParameter, /no_copy
                       uvalueFileParameter.active = segPos
                     widget_control, state.wListSelectSegObj, set_uValue = uvalueFileParameter, /no_copy
                   endif
         endcase
       'DELETE': begin
                 widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                   oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                 widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

                 oSegContainer = oImage->getSegContainerObj(active = clusPos)
                 count = oSegContainer->IDL_Container::count()-1
                 if ((segPos ge 0) and (segPos le count)) then begin
                    oSeg = oSegContainer->get(position = segPos)
                    oSegContainer->remove, position = segPos
                    if obj_valid(oSeg) then obj_destroy, oSeg
                    widget_control, state.wListSelectSegObj, get_uValue = uvalueFileParameter, /no_copy
                    if (segPos eq count) then uvalueFileParameter.active = (segPos - 1) > 0  $
                        else uvalueFileParameter.active = segPos < count
                    widget_control, state.wListSelectSegObj, set_uValue = uvalueFileParameter, /no_copy
                 endif
         endcase
       'MOVEUP': begin
                 if (segPos gt 0) then begin
                    widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                    widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                    oSegContainer = oImage->getSegContainerObj(active = clusPos)
                    oSegContainer->IDL_Container::move, segPos, segPos-1
                    segPos = segPos - 1
                    widget_control, state.wListSelectSegObj, get_uValue = uvalueFileParameter, /no_copy
                    uvalueFileParameter.active = segPos
                    widget_control, state.wListSelectSegObj, set_uValue = uvalueFileParameter, /no_copy
                 endif
         endcase
       'MOVEDOWN': begin
                        widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                        widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                        oSegContainer = oImage->getSegContainerObj(active = clusPos)

                        if (segPos lt (oSegContainer->count()-1)) then begin
                           oSegContainer->IDL_Container::move, segPos, segPos+1
                           segPos = segPos + 1
                           widget_control, state.wListSelectSegObj, get_uValue = uvalueFileParameter, /no_copy
                             uvalueFileParameter.active = segPos
                           widget_control, state.wListSelectSegObj, set_uValue = uvalueFileParameter, /no_copy
                        endif
         endcase
       'APPLYSEGMENTATIONONOFF': begin
                        state.fApplySeg = s_ToggleButtonOnOffState(state.wApplySegOnOff)
                        if state.fApplySeg then begin
                           if state.fApplySavedClusterMask then state.fApplySavedClusterMask = s_ToggleButtonOnOffState(state.wApplySavedClusterMasksOnOff)
                           if state.fApplyPaintedMask then state.fApplyPaintedMask = s_ToggleButtonOnOffState(state.wApplyPaintedMaskOnOff)
                           if state.fApplySavedROI2D then state.fApplySavedROI2D = s_ToggleButtonOnOffState(state.wApplySavedROI2DOnOff)
                           if state.fApplySavedROI3D then state.fApplySavedROI3D = s_ToggleButtonOnOffState(state.wApplySavedROI3DOnOff)
                        endif
         endcase
       'APPLYSAVEDCLUSTERMASKSONONOFF': begin
                        state.fApplySavedClusterMask = s_ToggleButtonOnOffState(state.wApplySavedClusterMasksOnOff)
                        if state.fApplySavedClusterMask then begin
                           if state.fApplySeg then state.fApplySeg = s_ToggleButtonOnOffState(state.wApplySegOnOff)
                           if state.fApplyPaintedMask then state.fApplyPaintedMask = s_ToggleButtonOnOffState(state.wApplyPaintedMaskOnOff)
                           if state.fApplySavedROI2D then state.fApplySavedROI2D = s_ToggleButtonOnOffState(state.wApplySavedROI2DOnOff)
                           if state.fApplySavedROI3D then state.fApplySavedROI3D = s_ToggleButtonOnOffState(state.wApplySavedROI3DOnOff)
                        endif
         endcase
       'APPLYPAINTEDMASKONONOFF': begin
                        state.fApplyPaintedMask = s_ToggleButtonOnOffState(state.wApplyPaintedMaskOnOff)
                        if state.fApplyPaintedMask then begin
                           if state.fApplySeg then state.fApplySeg = s_ToggleButtonOnOffState(state.wApplySegOnOff)
                           if state.fApplySavedClusterMask then state.fApplySavedClusterMask = s_ToggleButtonOnOffState(state.wApplySavedClusterMasksOnOff)
                           if state.fApplySavedROI2D then state.fApplySavedROI2D = s_ToggleButtonOnOffState(state.wApplySavedROI2DOnOff)
                           if state.fApplySavedROI3D then state.fApplySavedROI3D = s_ToggleButtonOnOffState(state.wApplySavedROI3DOnOff)
                        endif
         endcase
       'APPLYSAVEDROI2DONONOFF': begin
                        state.fApplySavedROI2D = s_ToggleButtonOnOffState(state.wApplySavedROI2DOnOff)
                        if state.fApplySavedROI2D then begin
                           if state.fApplySeg then state.fApplySeg = s_ToggleButtonOnOffState(state.wApplySegOnOff)
                           if state.fApplySavedClusterMask then state.fApplySavedClusterMask = s_ToggleButtonOnOffState(state.wApplySavedClusterMasksOnOff)
                           if state.fApplyPaintedMask then state.fApplyPaintedMask = s_ToggleButtonOnOffState(state.wApplyPaintedMaskOnOff)
                           if state.fApplySavedROI3D then state.fApplySavedROI3D = s_ToggleButtonOnOffState(state.wApplySavedROI3DOnOff)
                        endif
         endcase
       'APPLYSAVEDROI3DONONOFF': begin
                        state.fApplySavedROI3D = s_ToggleButtonOnOffState(state.wApplySavedROI3DOnOff)
                        if state.fApplySavedROI3D then begin
                           if state.fApplySeg then state.fApplySeg = s_ToggleButtonOnOffState(state.wApplySegOnOff)
                           if state.fApplySavedClusterMask then state.fApplySavedClusterMask = s_ToggleButtonOnOffState(state.wApplySavedClusterMasksOnOff)
                           if state.fApplyPaintedMask then state.fApplyPaintedMask = s_ToggleButtonOnOffState(state.wApplyPaintedMaskOnOff)
                           if state.fApplySavedROI2D then state.fApplySavedROI2D = s_ToggleButtonOnOffState(state.wApplySavedROI2DOnOff)
                        endif
         endcase

       'VALUEDATAROI_USE_LOWDATA_ONONOFF': begin
                        state.fValueDataRoi_LowData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_LowData_OnOff)
                        if(state.fValueDataRoi_LowData_OnOff) then begin
                          state.fValueDataRoi = 0
                          if state.fValueDataRoi_HighData_OnOff then begin
                             state.fValueDataRoi_HighData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighData_OnOff)
                          endif
                        endif else begin
                          if not state.fValueDataRoi_HighData_OnOff then begin
                             state.fValueDataRoi_HighData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighData_OnOff)
                          endif
                        endelse

                        if state.fValueDataRoi_LowRate_OnOff then begin
                           state.fValueDataRoi_LowRate_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_LowRate_OnOff)
                        endif
                        if state.fValueDataRoi_HighRate_OnOff then begin
                           state.fValueDataRoi_HighRate_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighRate_OnOff)
                        endif
         endcase
       'VALUEDATAROI_USE_HIGHDATA_ONONOFF': begin
                        state.fValueDataRoi = 1
                        if not state.fValueDataRoi_HighData_OnOff then begin                        
                          state.fValueDataRoi_HighData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighData_OnOff)
                        endif

                        if state.fValueDataRoi_LowData_OnOff then begin
                           state.fValueDataRoi_LowData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_LowData_OnOff)
                        endif
                        if state.fValueDataRoi_LowRate_OnOff then begin
                           state.fValueDataRoi_LowRate_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_LowRate_OnOff)
                        endif
                        if state.fValueDataRoi_HighRate_OnOff then begin
                           state.fValueDataRoi_HighRate_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighRate_OnOff)
                        endif
         endcase
       'VALUEDATAROI_USE_LOWRATE_ONONOFF': begin
                        state.fValueDataRoi_LowRate_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_LowRate_OnOff)

                        if(state.fValueDataRoi_LowRate_OnOff) then begin
                          state.fValueDataRoi = 2
                          if state.fValueDataRoi_HighData_OnOff then begin
                             state.fValueDataRoi_HighData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighData_OnOff)
                          endif
                        endif else begin
                          if not state.fValueDataRoi_HighData_OnOff then begin
                             state.fValueDataRoi_HighData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighData_OnOff)
                          endif
                        endelse

                        if state.fValueDataRoi_LowData_OnOff then begin
                           state.fValueDataRoi_LowData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_LowData_OnOff)
                        endif
                        if state.fValueDataRoi_HighRate_OnOff then begin
                           state.fValueDataRoi_HighRate_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighRate_OnOff)
                        endif
         endcase
       'VALUEDATAROI_USE_HIGHRATE_ONONOFF': begin
                        state.fValueDataRoi_HighRate_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighRate_OnOff)

                        if(state.fValueDataRoi_HighRate_OnOff) then begin
                          state.fValueDataRoi = 3
                          if state.fValueDataRoi_HighData_OnOff then begin
                             state.fValueDataRoi_HighData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighData_OnOff)
                          endif
                        endif else begin
                          if not state.fValueDataRoi_HighData_OnOff then begin
                             state.fValueDataRoi_HighData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_HighData_OnOff)
                          endif
                        endelse

                        if state.fValueDataRoi_LowData_OnOff then begin
                           state.fValueDataRoi_LowData_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_LowData_OnOff)
                        endif
                        if state.fValueDataRoi_LowRate_OnOff then begin
                           state.fValueDataRoi_LowRate_OnOff = s_ToggleButtonOnOffState(state.wValueDataRoi_LowRate_OnOff)
                        endif
         endcase
         
       'SAVERESULTWITHSEGNAMEONOFF': state.fSaveSegWithName = s_ToggleButtonOnOffState(state.wSaveSegWithNameOnOff)
       'APPLYLASTSEGMENTATIONSTEPONOFF': state.fApplyLastSegmentationStep = s_ToggleButtonOnOffState(state.wApplyLastSegmentationStepOnOff)
       'APPLYFORALLCLUSTERSONOFF': state.fApplyForAllClusters = s_ToggleButtonOnOffState(state.wApplyForAllClustersOnOff)
       'CREATEANDSAVE2DMODELONOFF': state.fCreateAndSave2DModel = s_ToggleButtonOnOffState(state.wCreateAndSave2DModelObjectOnOff)
       'CREATEANDSAVE2DMODELFROMPAINTEDMASKSONOFF': state.fCreateAndSavePainted2DModelObject = s_ToggleButtonOnOffState(state.wCreateAndSavePainted2DModelObjectOnOff)
       'ACCEPTALLOBJECTSONOFF': state.fAcceptAllObjects = s_ToggleButtonOnOffState(state.wAcceptAllObjectsOnOff)
       'UPDATE3DROIWITHGROWTHFACTORONOFF':   state.fUpdate3DROIWithGrowthFactor = s_ToggleButtonOnOffState(state.wUpdate3DROIWithGrowthFactorOnOff)
       'CREATEANDSAVE3DMODELONOFF': begin
                        state.fCreateAndSave3DModel = s_ToggleButtonOnOffState(state.wCreateAndSave3DModelObjectOnOff)
                        if state.fCreateAndSave3DModel then begin
                           if state.fCreateAndSavePainted3DModelObject then state.fCreateAndSavePainted3DModelObject = s_ToggleButtonOnOffState(state.wCreateAndSavePainted3DModelObjectOnOff)
                           if state.fCreateAndSave3DModelFromNumberedMasks then state.fCreateAndSave3DModelFromNumberedMasks = s_ToggleButtonOnOffState(state.wCreateAndSave3DModelFromNumberedMasksOnOff)
                           state.fZSliceInitial = zPos
                           widget_control, state.wSaveSegmentation, set_value = 'Save Segmentation: (z-Slice '+ strcompress(string(zPos), /rem) +')'
                        endif
         endcase
       'CREATEANDSAVE3DMODELFROMPAINTEDMASKSONOFF':begin
                        state.fCreateAndSavePainted3DModelObject = s_ToggleButtonOnOffState(state.wCreateAndSavePainted3DModelObjectOnOff)
                        if state.fCreateAndSavePainted3DModelObject then begin
                           if state.fCreateAndSave3DModel then state.fCreateAndSave3DModel = s_ToggleButtonOnOffState(state.wCreateAndSave3DModelObjectOnOff)
                           if state.fCreateAndSave3DModelFromNumberedMasks then state.fCreateAndSave3DModelFromNumberedMasks = s_ToggleButtonOnOffState(state.wCreateAndSave3DModelFromNumberedMasksOnOff)
                        endif
         endcase
       'CREATEANDSAVE3DMODELFROMNUMBEREDMASKSONOFF':begin
                        state.fCreateAndSave3DModelFromNumberedMasks = s_ToggleButtonOnOffState(state.wCreateAndSave3DModelFromNumberedMasksOnOff)
                        if state.fCreateAndSave3DModelFromNumberedMasks then begin
                           if state.fCreateAndSave3DModel then state.fCreateAndSave3DModel = s_ToggleButtonOnOffState(state.wCreateAndSave3DModelObjectOnOff)
                           if state.fCreateAndSavePainted3DModelObject then state.fCreateAndSavePainted3DModelObject = s_ToggleButtonOnOffState(state.wCreateAndSavePainted3DModelObjectOnOff)
                        endif
         endcase
       'CREATEANDSAVEEMPTY3DMODELONOFF': state.fCreateAndSaveEmpty3DModelObject = s_ToggleButtonOnOffState(state.wCreateAndSaveEmpty3DModelObjectOnOff)
       'SAVEFORSELECTEDIMAGE': state.fSaveForSelectedImage = s_ToggleButtonOnOffState(state.wSaveForSelectedImageOnOff)
       'FROMCURRENTPOSITION': state.fFromCurrentPosition = s_ToggleButtonOnOffState(state.wFromCurrentPositionOnOff)
       'SHOWASWIDGETONOFF': state.fParamWidget = s_ToggleButtonOnOffState(state.wParamWidgetOnOff)
       'SHOWASDATATABLEONOFF': state.fParamTable = s_ToggleButtonOnOffState(state.wParamTableOnOff)
       'ROIOBJECTWINDOWONOFF': begin
                        state.fROIObjectWindow = s_ToggleButtonOnOffState(state.wROIObjectWindowOnOff)
                             if state.fROIObjectWindow then begin
                                widget_control, ev.top, tlb_get_size = baseSize, tlb_get_Offset = baseOffset
                                widget_control, ev.top, set_uValue = state, /no_copy
                                s_ROI_ObjectManipulator_Window, groupLeader = ev.top, basePosition = (baseSize+baseOffset)
                                widget_control, ev.top, get_uValue = state, /no_copy
                             endif else begin
                                if widget_info(state.child_ROIObjectWindow_tlb, /valid_id) then begin
                                   widget_control, state.child_ROIObjectWindow_tlb, /destroy
                                   state.child_ROIObjectWindow_tlb = -1l
                                endif
                             endelse
                          widget_control, ev.top, set_uValue = state, /no_copy
                          return
         endcase
         else:
       endcase

    widget_control, ev.top, set_uValue = state, /no_copy
    if update then begin
       widget_control, stack_tlb, get_uValue = stackState, /no_copy
         dummy = stackState.child_ViewWindow_tlb
       widget_control, stack_tlb, set_uValue = stackState, /no_copy
       if widget_info(dummy, /valid) then zimage_colors, {redraw_image, top : dummy}
       s_ISegM_UpdateWidgets, ev.top
    endif
end


pro s_ISM_PassSegMethod_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, ev.top, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos

    widget_control, ev.id, get_Value = selectedFilter
    widget_control, ev.top, get_uValue = state, /no_copy
         widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
            oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
         widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

         oSegContainer = oImage->getSegContainerObj(active = clusPos)
         oSegContainer->add, obj_new(selectedFilter)

         widget_control, state.wListSelectSegObj, get_uValue = uvalueFileParameter, /no_copy
            uvalueFileParameter.active = oSegContainer->count()-1
         widget_control, state.wListSelectSegObj, set_uValue = uvalueFileParameter, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       dummy = stackState.child_ViewWindow_tlb
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    if widget_info(dummy, /valid) then zimage_colors, {redraw_image, top : dummy}
    s_ISegM_UpdateWidgets, ev.top
end


pro s_ISegM_List_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
       fCopySingleClusterProcedure = state.fCopySingleClusterProcedure and obj_valid(state.oCopySingleClusterContainer)
    widget_control, ev.top, set_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue, /no_copy
       uValue.active = ev.index
    widget_control, ev.id, set_uValue = uValue, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                               totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum,$
                                               clusPos = clusPos, segPos = segPos

    widget_control, ev.id, get_uValue = uValue, /no_copy

    if ((*uValue.value)[ev.index] eq '-NO SELECTION-') then begin
       widget_control, ev.id, set_uValue = uValue, /no_copy
       return
    endif

    fUpdate = 0b
    case uValue.name of
    'ClusterSelected':begin
                      case ev.Clicks of
                      1:begin
                         fUpdate = 1b
                         if fCopySingleClusterProcedure then s_ISegM_CopyPasteSegMethod, ev.top, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
                         widget_control, ev.id, set_uValue = uValue, /no_copy
                           s_ISegM_UpdateWidgets, ev.top
                         widget_control, ev.id, get_uValue = uValue, /no_copy
                      endcase
                      else:
                      endcase
    endcase
    'MethodsSelected':begin
                      case ev.Clicks of
                      1:fUpdate = 1b
                      2:begin
                         widget_control, ev.top, get_uValue = state, /no_copy
                         widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                           oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
                         widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
                         
                         ; FASL FIXING FIXME XXX ... USE AN OBJECT OF FILTER TO DEFINE 
                         ;DEPRECATED COMPONENTS::::::
                        ;container     =  oImage->getSegContainerObj(active = clusPos)
                        ;tempContainer = OBJ_NEW(OBJ_CLASS(contenedor->Get(/ALL)))
                         paramTableUValue = {groupLeader: ev.top,$
                                                 sTopBaseTitle : state.sTopBaseTitle,$
                                                 callName: 's_Image_SegmentationManipulator_Window',$  ; this widget Pro Name
                                                 pwIDParamNumList:ptr_new(),$                          ; pointer to corellate created widget ID's and Parameter Numbers
                                                 wTopBaseID : -1,$
                                                 oContainer : (oImage->getSegContainerObj(active = clusPos)),$   ;  Filter Container Object
                                                 Index: ev.index,$             ; Index of Filter Object in Filter Container Object
                                                 wTableID: -1}
                                                 
                         widget_control, ev.id, set_uValue = uValue, /no_copy
                         if state.fParamWidget then begin
                            s_ISegM_ParameterWidget, paramTableUValue = paramTableUValue
                            state.child_ParamWidget_tlb = paramTableUValue.wTopBaseID
                         endif
                         if state.fParamTable then begin
                            s_ISegM_ParamTableWidget, paramTableUValue = paramTableUValue
                            state.child_ParamTableWidget_tlb = paramTableUValue.wTopBaseID
                         endif
                         widget_control, ev.id, get_uValue = uValue, /no_copy
                         widget_control, ev.top, set_uValue = state, /no_copy
                       endcase
                       else:
                       endcase
    endcase
    else:
    endcase
    if fUpdate then begin
       widget_control, ev.top, get_uValue = state, /no_copy
          widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
             child_ViewWindow_tlb = stackState.child_ViewWindow_tlb
             child_Colocalization_tlb = stackState.child_Colocalization_tlb
          widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
       widget_control, ev.top, set_uValue = state, /no_copy

       widget_control, ev.id, set_uValue = uValue, /no_copy
          if widget_info(child_ViewWindow_tlb, /valid) then zimage_colors, {redraw_image, top: child_ViewWindow_tlb}
          if widget_info(child_Colocalization_tlb, /valid) then s_Coloc_update, child_Colocalization_tlb
          s_ISegM_UpdateDataTableWidgets, ev.top
       widget_control, ev.id, get_uValue = uValue, /no_copy
    endif
    widget_control, ev.id, set_uValue = uValue, /no_copy
end;-s_ISegM_List_Event------------------------------------------------------------------


pro s_ISegM_UpdateDataTableWidgets, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                              clusPos = clusPos, segPos = segPos

    widget_control, wTopBase, get_uValue = state, /no_copy
       widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
         oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
       widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

       if (state.fParamTable and widget_info(state.child_ParamTableWidget_tlb, /valid)) then begin
          child_ParamTableWidget_tlb = state.child_ParamTableWidget_tlb
          sTopBaseTitle = state.sTopBaseTitle
          widget_control, wTopBase, set_uValue = state, /no_copy
          s_ISegM_ParamTableWidget_DifferenceUpdate, newParamTableUValue = {sTopBaseTitle: sTopBaseTitle,$
                                                 wTopBaseID: child_ParamTableWidget_tlb,$
                                                 oContainer: oImage->getSegContainerObj(active = clusPos),$
                                                 Index: segPos}
          widget_control, wTopBase, get_uValue = state, /no_copy
       endif
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ISegM_KeepSegmentationProcedure, wTopBase, oldStackImageActive = oldStackImageActive
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                      clusPos = clusPos, segPos = segPos

    widget_control, wTopBase, get_uValue = state, /no_copy
       widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
         oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
       widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
    widget_control, wTopBase, set_uValue = state, /no_copy

    dummy = oImage->getSegContainer()
    save, dummy, fileName = s_getPathForSystem()+'obj.tmp'
    restore, s_getPathForSystem()+'obj.tmp', restored_objects = dummy, /relaxed
    oImage->setSegContainer, object = dummy[0]

    widget_control, wTopBase, get_uValue = state, /no_copy
       if widget_info(state.child_ParamWidget_tlb, /valid) then begin
         widget_control, state.child_ParamWidget_tlb, /destroy
         paramTableUValue = {groupLeader: wTopBase,$       ; widget Group LeaderID
                              callName: 's_Image_SegmentationManipulator_Window',$  ; this widget Pro Name
                              sTopBaseTitle : state.sTopBaseTitle,$
                              pwIDParamNumList:ptr_new(),$                           ; pointer to corellate created widget ID's and Parameter Numbers
                              wTopBaseID : -1,$
                              oContainer : oImage->getSegContainerObj(active = clusPos),$ ;  Filter Container Object
                              Index: segPos,$               ; Index of Filter Object in Filter Container Object
                              wTableID: -1}
         s_ISegM_ParameterWidget, paramTableUValue = paramTableUValue
         state.child_ParamWidget_tlb = paramTableUValue.wTopBaseID
       endif
       if widget_info(state.child_ParamTableWidget_tlb, /valid) then begin
         widget_control, state.child_ParamTableWidget_tlb, /destroy
         paramTableUValue = {groupLeader: wTopBase,$       ; widget Group LeaderID
                              callName: 's_Image_SegmentationManipulator_Window',$                                              ; this widget Pro Name
                              sTopBaseTitle : state.sTopBaseTitle,$
                              pwIDParamNumList:ptr_new(),$                           ; pointer to corellate created widget ID's and Parameter Numbers
                              wTopBaseID : -1,$
                              oContainer : oImage->getSegContainerObj(active = clusPos),$ ;  Filter Container Object
                              Index: segPos,$               ; Index of Filter Object in Filter Container Object
                              wTableID: -1}
         s_ISegM_ParamTableWidget, paramTableUValue = paramTableUValue
         state.child_ParamTableWidget_tlb = paramTableUValue.wTopBaseID
       endif
    widget_control, wTopBase, set_uValue = state, /no_cop
end


pro s_ISegM_UpdateWidgets, wTopBase, oldStackImageActive = oldStackImageActive
    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = state.stack_tlb
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, segPos = segPos
    s_ISegM_CopyPasteSegMethod, wTopBase, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos;, clusPos = clusPos
    widget_control, wTopBase, get_uValue = state, /no_copy

       widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
         oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
       widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

       if not(obj_valid(oImage)) then begin
         widget_control, wTopBase, set_uValue = state, /no_copy
         return
       endif

       oImage->get, pParamStruct = pParamStruct
       state.sTopBaseTitle = 's_Seg |-> ' + *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
       widget_control, state.wTopBase, tlb_set_title = state.sTopBaseTitle
       pParamStruct = -1

       for i = 0, (oImage->getSegClusterNumber())-1 do if (i eq 0) then clusterList = 'Cluster_0' else clusterList = [clusterList, strCompress('Cluster_'+string(i), /rem)]
       widget_control, state.wListSegCluster, set_value = clusterList

       widget_control, state.wListSegCluster, get_uValue = uvalueCluster, /no_copy
         if (ptr_valid(uvalueCluster.value)) then *uvalueCluster.value = clusterList else uvalueCluster.value = ptr_new(clusterList, /no_copy)
         oSegContainer = oImage->getSegContainerObj(active = uvalueCluster.active)
       widget_control, state.wListSegCluster, set_list_select = uvalueCluster.active, set_uValue = uvalueCluster, /no_copy

       filterList = '-NO SELECTION-'
       presentList = strUpCase(s_ISegM_Image_GetFilterObjectList())

       for i = 0, (oSegContainer->count())-1 do begin
         oSeg = oSegContainer->get(position = i)
         if ( (where(obj_class(oSeg) eq presentList))[0] eq -1) then begin
          oSegContainer->remove, position = i
          if obj_valid(oSeg) then obj_destroy, oSeg
          i = i -1
         endif else filterList = [filterList, (*(oSeg->getpParamStruct())).name]
       endfor
       if (n_elements(filterList) gt 1) then filterList = filterList[1:*]

       widget_control, state.wListSelectSegObj, get_uValue = uValue, /no_copy
         uValue.name = 'MethodsSelected'
         if ptr_valid(uValue.value) then ptr_free, uValue.value
         uValue.value = ptr_new(filterList, /no_copy)
         if (state.fApplyLastSegmentationStep) then uValue.active = n_elements(*uValue.value)-1
       widget_control, state.wListSelectSegObj, set_list_select = uValue.active, set_value = *uValue.value, set_uValue = uValue, /no_copy

       child_ROIObjectWindow_tlb = state.child_ROIObjectWindow_tlb
       if state.fSaveForSelectedImage then begin
         state.fSaveForSelectedImage = 0
         ev_struct = {top: wTopBase,$
                     id: state.wSaveForSelectedZSlice }
         widget_control, wTopBase, set_uValue = state, /no_copy
          s_Image_SegmentationManipulator_Window_Button_Event, ev_struct
         widget_control, wTopBase, get_uValue = state, /no_copy
         state.fSaveForSelectedImage = 1
       endif
    widget_control, wTopBase, set_uValue = state, /no_copy, /update

    if widget_info(child_ROIObjectWindow_tlb, /valid) then s_ROIOM_InitializeWidgets, child_ROIObjectWindow_tlb
    s_ISegM_UpdateDataTableWidgets, wTopBase
end


pro s_ISegM_Resize_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
    
    ; Don't resize listbase on UNIX
    if (!VERSION.OS_FAMILY eq 'Windows') then begin
       widget_control, state.wListBase_1, scr_xsize = ev.x-10, scr_ysize = floor((ev.y-42)/2.)
       widget_control, state.wListBase_2, scr_xsize = ev.x-10, scr_ysize = floor((ev.y-42)/2.)
    endif
    
       evx = ev.x-13
       evy = floor((ev.y-100)/2.)
       widget_control, state.wListSegCluster, scr_xsize = evx, scr_ysize = evy
       widget_control, state.wListSelectSegObj, scr_xsize = evx, scr_ysize = evy
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_ISegM_cleanUp, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
    if (n_elements(state) gt 0) then if (widget_info(state.stack_tlb, /valid_id)) then begin
       widget_control, state.stack_tlb, get_uValue = statusParent, /no_copy
       if (n_elements(statusParent) ne 0) then begin
         if s_ToggleButtonOnOffState(statusParent.wSegmentationOnOff) then void = s_ToggleButtonOnOffState(statusParent.wSegmentationOnOff)
         widget_control, state.stack_tlb, set_uValue = statusParent, /no_copy
       endif
    endif
    if ptr_valid(state.pImageStackInfoObject) then if obj_valid(*state.pImageStackInfoObject) then obj_destroy, *state.pImageStackInfoObject
    if ptr_valid(state.pImageStackInfoObject) then ptr_free, state.pImageStackInfoObject

    if obj_valid(state.oCopyAllClusterContainer) then begin
       for j = (state.oCopyAllClusterContainer->count())-1, 0, -1 do begin
         oSingleClusterContainer = state.oCopyAllClusterContainer->get(position = j)
         if obj_valid(oSingleClusterContainer) then begin
           for i = (oSingleClusterContainer->count())-1, 0, -1 do begin
              obj = (oSingleClusterContainer)->get(position = i)
              oSingleClusterContainer->remove, position = i
              obj_destroy, obj
           endfor
           state.oCopyAllClusterContainer->remove, position = j
           obj_destroy, oSingleClusterContainer
         endif
       endfor
       obj_destroy, state.oCopyAllClusterContainer
    endif

    if obj_valid(state.oCopySingleClusterContainer) then begin
       for i = (state.oCopySingleClusterContainer->count())-1, 0, -1 do begin
         obj = (state.oCopySingleClusterContainer)->get(position = i)
         (state.oCopySingleClusterContainer)->remove, position = i
         obj_destroy, obj
       endfor
       obj_destroy, state.oCopySingleClusterContainer
    endif

    for i = 0,n_tags(state)-1 do begin
        case size(state.(i), /tname) of
         'POINTER': ptr_free, state.(i)
         'OBJREF': obj_destroy, state.(i)
         else:
        endcase
     endfor
    if widget_info(state.stack_tlb, /valid_id) then widget_control, state.stack_tlb, /map
end


pro s_Image_SegmentationManipulator_Window, groupLeader = groupLeader, basePosition = basePosition,$
                                            application_tlb = application_tlb, pImageStackInfoObject = pImageStackInfoObject

    ;Check the validity of the group identifier.
    ngroup = n_elements(groupLeader)
    if (ngroup ne 0) then begin
       if not(widget_info(groupLeader, /valid_id)) then begin
          print,'Error: the group identifier is not valid.  |--> Returning to the main application.'
          return
       endif
    endif else groupLeader = 0l

    if (not(keyWord_set(basePosition))) then basePosition = [0,0]
    if not(keyWord_set((pImageStackInfoObject))) then pImageStackInfoObject = ptr_new( obj_new('C_sImageStackObject'), /no_copy)

    wTopBase = widget_base(title = 's_Seg |->',$
                           xpad = 2, ypad = 2,$
                           xOffset = basePosition[0],$
                           mBar = menuBase,$
                           group_leader = groupLeader,$
                           tlb_size_events = 1, /column)

    application_tlb = wTopBase ; Return parameter.

        ; Create menu buttons
    wMenuButton = widget_button(menuBase, value = 'Options', event_pro = 's_Image_SegmentationManipulator_Window_Button_Event')
       wApplySegOnOff = widget_button(wMenuButton, value = 'Use Selected Segmentation (on )', uval = 'APPLYSEGMENTATIONONOFF')
       wApplySavedClusterMasksOnOff = widget_button(wMenuButton, value = 'Use Saved Cluster Masks (off)', uval = 'APPLYSAVEDCLUSTERMASKSONONOFF')
       wApplyPaintedMaskOnOff = widget_button(wMenuButton, value = 'Use Painted Masks (off)', uval = 'APPLYPAINTEDMASKONONOFF')
       wApplySavedROI2DOnOff = widget_button(wMenuButton, value = 'Use Saved ROI2D (off)', uval = 'APPLYSAVEDROI2DONONOFF')
       wApplySavedROI3DOnOff = widget_button(wMenuButton, value = 'Use Saved ROI3D (off)', uval = 'APPLYSAVEDROI3DONONOFF')
       wMenuValueDataROI     = widget_button(wMenuButton, value = 'Value Data ROI as ....', /menu, /sep)
       wValueDataRoi_LowData_OnOff = widget_button(wMenuValueDataROI, value = 'Use low values (off)', uval = 'VALUEDATAROI_USE_LOWDATA_ONONOFF')       
       wValueDataRoi_HighData_OnOff = widget_button(wMenuValueDataROI, value = 'Use high values. Default (on )', uval = 'VALUEDATAROI_USE_HIGHDATA_ONONOFF')
       wValueDataRoi_LowRate_OnOff = widget_button(wMenuValueDataROI, value = 'Use low rate values (off)', uval = 'VALUEDATAROI_USE_LOWRATE_ONONOFF')
       wValueDataRoi_HighRate_OnOff = widget_button(wMenuValueDataROI, value = 'Use high rate values (off)', uval = 'VALUEDATAROI_USE_HIGHRATE_ONONOFF')
       
       wMenuButton_2 = widget_button(wMenuButton, value = 'Show Segmentation Data as ....', /menu, /sep)
          wParamTableOnOff = widget_button(wMenuButton_2, value = '... Table (off)', uval = 'SHOWASDATATABLEONOFF')
          wParamWidgetOnOff = widget_button(wMenuButton_2, value = '... Widgets (on )', uval = 'SHOWASWIDGETONOFF')
       void = widget_button(wMenuButton, value = 'Open All Cluster Procedures', uval = 'OPENALLCLUSTERPROCEDURES', /sep)
       void = widget_button(wMenuButton, value = 'Save All Cluster Procedures as...', uval = 'SAVEALLCLUSTERPROCEDURES')
       void = widget_button(wMenuButton, value = 'Open Single Cluster Procedure', uval = 'OPENSINGLECLUSTERPROCEDURE')
       void = widget_button(wMenuButton, value = 'Save Single Cluster Procedure as ...', uval = 'SAVESINGLECLUSTERPROCEDURE')
       void = widget_button(wMenuButton, value = 'Pass Segmentation:', sensitive = 0, /sep)
       wCopyAllClusterProceduresOnOff = widget_button(wMenuButton, value = '...|-> Copy All Cluster Procedures (off)', uval = 'COPYALLCLUSTERPROCEDURESONOFF')
       wCopySingleClusterProcedureOnOff = widget_button(wMenuButton, value = '...|-> Copy Single Cluster Procedure (off)', uval = 'COPYSINGLECLUSTERPROCEDURESONOFF')
       wMenuButton_2 = widget_button(wMenuButton, value = '...||-> Paste Segmentation Procedures ... ', /menu)
          void = widget_button(wMenuButton_2, value = '... to ZSlice before', uval = 'ZSLICEBEFORE')
          void = widget_button(wMenuButton_2, value = '... to ZSlice below', uval = 'ZSLICEBELOW')
          void = widget_button(wMenuButton_2, value = '... to all ZSlices', uval = 'ALLZSLICES', /sep)
          void = widget_button(wMenuButton_2, value = '... to all ZSlices and Times', uval = 'ALLZSLICESANDTIMES')
          void = widget_button(wMenuButton_2, value = '... to all ZSlices and Channels', uval = 'ALLZSLICESANDCHANNELS')
          void = widget_button(wMenuButton_2, value = '... to all ZSlices and Channels and Times', uval = 'ALLZSLICESANDCHANNELSANDTIMES')
          void = widget_button(wMenuButton_2, value = '... to all ZSlices before', uval = 'ALLZSLICESBEFORE', /sep)
          void = widget_button(wMenuButton_2, value = '... to all ZSlices below', uval = 'ALLZSLICESBELOW')
          void = widget_button(wMenuButton_2, value = '... selected ZSlice through Times', uval = 'ZSLICETHROUGHTIMES', /sep)
          void = widget_button(wMenuButton_2, value = '... all ZSlices through Times', uval = 'ALLSLICETHROUGHTIMES')
          void = widget_button(wMenuButton_2, value = '... selected ZSlice through Channels', uval = 'ZSLICETHROUGHCHANNELS', /sep)
          void = widget_button(wMenuButton_2, value = '... all ZSlices through Channels', uval = 'ALLSLICETHROUGHCHANNELS')
       wSaveSegWithNameOnOff = widget_button(wMenuButton, value = '...|-> Include Segmentation Name (off)', uval = 'SAVERESULTWITHSEGNAMEONOFF', /sep)
       wApplyLastSegmentationStepOnOff = widget_button(wMenuButton, value = '...|-> Apply Last Segmentation Step (on )', uval = 'APPLYLASTSEGMENTATIONSTEPONOFF')
       wApplyForAllClustersOnOff = widget_button(wMenuButton, value = '...|-> Apply For All Clusters (off)', uval = 'APPLYFORALLCLUSTERSONOFF')
       void = widget_button(wMenuButton, value = '    For 2DROIs:', uval = 'VOID', sensitive = 0)
       wCreateAndSave2DModelObjectOnOff = widget_button(wMenuButton, value = '...|-> Create 2D Model Object (off)', uval = 'CREATEANDSAVE2DMODELONOFF')
       wCreateAndSavePainted2DModelObjectOnOff = widget_button(wMenuButton, value = '...|-> Create 2D Model Object from Painted Masks (off)', uval = 'CREATEANDSAVE2DMODELFROMPAINTEDMASKSONOFF')
       void = widget_button(wMenuButton, value = '    For 3DROIs:', uval = 'VOID', sensitive = 0)
       wAcceptAllObjectsOnOff = widget_button(wMenuButton, value = '...|-> Accept all Objects for 3D Model (off)', uval = 'ACCEPTALLOBJECTSONOFF')
       wUpdate3DROIWithGrowthFactorOnOff = widget_button(wMenuButton, value = '...|-> Update 3D ROI Objects with Growth-Factor (off)', uval = 'UPDATE3DROIWITHGROWTHFACTORONOFF')
       wCreateAndSave3DModelObjectOnOff = widget_button(wMenuButton, value = '...|-> Create 3D Model Object from 2D Models (off)', uval = 'CREATEANDSAVE3DMODELONOFF')
       wCreateAndSavePainted3DModelObjectOnOff = widget_button(wMenuButton, value = '...|-> Create 3D Model Object from Painted Masks (off)', uval = 'CREATEANDSAVE3DMODELFROMPAINTEDMASKSONOFF')
       wCreateAndSave3DModelFromNumberedMasksOnOff = widget_button(wMenuButton, value = '...|-> Create 3D Model Object from Numbered Masks (off)', uval = 'CREATEANDSAVE3DMODELFROMNUMBEREDMASKSONOFF')
       wCreateAndSaveEmpty3DModelObjectOnOff = widget_button(wMenuButton, value = '...|-> Create Empty 3D Model (off)', uval = 'CREATEANDSAVEEMPTY3DMODELONOFF')
       wSaveSegmentation = widget_button(wMenuButton, value = 'Save Segmentation: (z-Slice 0)', sensitive = 0)
       wMenuButton_2 = widget_button(wMenuButton, value = '...||-> Save Segmentation Results ... ', /menu)
       wSaveForSelectedZSlice = widget_button(wMenuButton_2, value = '... for Selected ZSlice', uval = 'SAVEFORSELECTEDZSLICE')
          void = widget_button(wMenuButton_2, value = '... for ZSlice through Times', uval = 'SAVEFORZSLICETHROUGHTIMES')
          void = widget_button(wMenuButton_2, value = '... for all ZSlices', uval = 'SAVEFORALLZSLICES', /sep)
          void = widget_button(wMenuButton_2, value = '... for all ZSlices and Times', uval = 'SAVEFORALLZSLICESANDTIMES')
          void = widget_button(wMenuButton_2, value = '... for all ZSlices and Channels', uval = 'SAVEFORALLZSLICESANDCHANNELS')
          void = widget_button(wMenuButton_2, value = '... for all ZSlices and Channels and Times', uval = 'SAVEFORALLZSLICESANDCHANNELSANDTIMES')
       wSaveForSelectedImageOnOff = widget_button(wMenuButton_2, value = '... for Selected Image (off)',  uval = 'SAVEFORSELECTEDIMAGE', /sep)
       wFromCurrentPositionOnOff = widget_button(wMenuButton_2, value = '... from Current Position (off)',  uval = 'FROMCURRENTPOSITION')

    wMenuButton = widget_button(menuBase, value = 'Analyse', event_pro = 's_Image_SegmentationManipulator_Window_Button_Event')
    wROIObjectWindowOnOff = widget_button(wMenuButton, value = 'ROI Object Window (off)', uval = 'ROIOBJECTWINDOWONOFF')

    wListBase_1 = widget_base(wTopBase, /column, /align_center)
    void = widget_button(wListBase_1, value = 'Segmentation Cluster |-> ', /menu, event_pro = 's_Image_SegmentationManipulator_Window_Button_Event')
    dummy = widget_button(void, value = 'Add New Cluster', uval = 'ADDCLUSTER')
    dummy = widget_button(void, value = 'Delete Cluster', uval = 'DELETECLUSTER')
    wListSegCluster = widget_list(wListBase_1, xSize = 30, ySize = 2, value = ['Cluster_0'],$
                      uValue = {name:'ClusterSelected', value : ptr_new(  ['Cluster_0'], /no_copy), active:0 },$
                      event_pro = 's_ISegM_List_Event',$
                      kill_notify = 'CleanList')

    filterObjectList = s_ISegM_Image_GetFilterObjectList()
    wListBase_2 = widget_base(wTopBase, /column, /align_center)
    void = widget_button(wListBase_2, value = 'Add Segmentation Method |->', /menu, event_pro = 's_ISM_PassSegMethod_Event')
    
    dummySingle = widget_button(void, value = 'Single Image Seg. Method |->', /menu)
    dummyMultiple = widget_button(void, value = 'Multiple Image Seg. Method |->', /menu)

    dummyDebugLevel = widget_button(void, value = 'Debugging Seg. Method |->', /menu)
    dummyAlphaDeb = widget_button(dummyDebugLevel, value = 'Alpha Debugging Seg. Method |->', /menu)
          dummyAlphaSingle   =   widget_button(dummyAlphaDeb, value = 'Single Image Seg. Method |->', /menu)    
          dummyAlphaMultiple =   widget_button(dummyAlphaDeb, value = 'Multiple Image Seg. Method |->', /menu)
    
    dummyBetadeb = widget_button(dummyDebugLevel, value = 'Beta Release Seg. Method |->', /menu)
          dummyBetaSingle   =   widget_button(dummyBetadeb, value = 'Single Image Seg. Method |->', /menu)    
          dummyBetaMultiple =   widget_button(dummyBetadeb, value = 'Multiple Image Seg. Method |->', /menu)
    
    ; FASL Filters Revision
    dummyFASLdeb =   widget_button(dummyDebugLevel, value = 'FASL_revision (No USE) Seg. Method |->', /menu)
           dummyNCdeb   =   widget_button(dummyFASLdeb, value = 'NC Seg. Method |->', /menu)
    for i = 0, n_elements(filterObjectList)-1 do begin
      objDummy = obj_new(filterObjectList(i))
      textDummyD = objDummy->getImageFilterDevelopmentalState()
      textDummyT = objDummy->getImageFilterType()
      case textDummyD of
        'Alpha_Debugging_Filter_Method': begin
          if(textDummyT eq 'Single_Image_Filter_Method') then dummy = widget_button(dummyAlphaSingle, value = filterObjectList[i]) 
          if(textDummyT eq 'Multiple_Image_Filter_Method') then dummy = widget_button(dummyAlphaMultiple, value = filterObjectList[i]) 
        endcase
        'Beta_Release_Filter_Method': begin
          if(textDummyT eq 'Single_Image_Filter_Method') then dummy = widget_button(dummyBetaSingle, value = filterObjectList[i]) 
          if(textDummyT eq 'Multiple_Image_Filter_Method') then dummy = widget_button(dummyBetaMultiple, value = filterObjectList[i]) 
        endcase
        'FASL_NC_Release_Filter_Method': begin
          dummy = widget_button(dummyNCdeb, value = filterObjectList[i])
        endcase
        else: begin
          if(textDummyT eq 'Single_Image_Filter_Method') then dummy = widget_button(dummySingle, value = filterObjectList[i]) 
          if(textDummyT eq 'Multiple_Image_Filter_Method') then dummy = widget_button(dummyMultiple, value = filterObjectList[i]) 
        endcase
      endcase      
      obj_destroy, objDummy
    endfor
    wListSelectSegObj = widget_list(wListBase_2, xSize = 30, ySize = 4, value = ['-NO SELECTION-'],$
                        uValue = {name:'MethodsSelected', value : ptr_new(  ['-NO SELECTION-'], /no_copy), active:0 },$
                        event_pro = 's_ISegM_List_Event',$
                        kill_notify = 'CleanList')

    wButtonBase = widget_base(wTopBase, /row, event_pro = 's_Image_SegmentationManipulator_Window_Button_Event')
    void = widget_button(wButtonBase, value = 'Delete', uval = 'DELETE', /dyn)
    void = widget_button(wButtonBase, value = 'Up', uval = 'MOVEUP', /dyn)
    void = widget_button(wButtonBase, value = 'Down', uval = 'MOVEDOWN', /dyn)
    wAddSegMethodButton = widget_button(wButtonBase, value = 'Copy', /menu)
    void = widget_button(wAddSegMethodButton, value = 'Copy Threshold from Histogram', uval = 'SELECTTHRESHFROMHIST')
    void = widget_button(wAddSegMethodButton, value = 'Copy Selected Method', uval = 'COPYSELECTEDMETHOD')

       ; Realize the widgets.
    widget_control, wTopBase, /realize

       ; Initialize state & widgets
    state = {groupLeader:groupLeader,$       ; The Group Leader Base widget ID.
             stack_tlb:groupLeader,$
             wTopBase:wTopBase,$                 ; This Base widget ID.
             sTopBaseTitle:'X',$                 ; This Top Base-Widget Title.
             wListBase_1:wListBase_1,$
             wListBase_2:wListBase_2,$
             wListSegCluster:wListSegCluster,$
             wListSelectSegObj:wListSelectSegObj,$
             wSaveSegmentation:wSaveSegmentation,$
              fZSliceInitial:0b,$
             wSaveSegWithNameOnOff:wSaveSegWithNameOnOff,$
              fSaveSegWithName:0b,$
             wSaveForSelectedImageOnOff:wSaveForSelectedImageOnOff,$
              fSaveForSelectedImage:0b,$
             wFromCurrentPositionOnOff:wFromCurrentPositionOnOff,$
              fFromCurrentPosition:0b,$
             wSaveForSelectedZSlice:wSaveForSelectedZSlice,$
             
             fValueDataRoi : 1,$
             wValueDataRoi_LowData_OnOff:wValueDataRoi_LowData_OnOff,$
              fValueDataRoi_LowData_OnOff:0b,$
             wValueDataRoi_HighData_OnOff:wValueDataRoi_HighData_OnOff,$            
              fValueDataRoi_HighData_OnOff:1b,$
             wValueDataRoi_LowRate_OnOff:wValueDataRoi_LowRate_OnOff,$              
              fValueDataRoi_LowRate_OnOff:0b,$
             wValueDataRoi_HighRate_OnOff:wValueDataRoi_HighRate_OnOff,$              
              fValueDataRoi_HighRate_OnOff:0b,$
             
             wParamWidgetOnOff:wParamWidgetOnOff,$
              fParamWidget:1b,$
              child_ParamWidget_tlb:-1L,$
             wParamTableOnOff:wParamTableOnOff,$
              fParamTable:0b,$
              child_ParamTableWidget_tlb:-1L,$
             wApplySegOnOff:wApplySegOnOff,$
              fApplySeg:1b,$
             wApplySavedClusterMasksOnOff:wApplySavedClusterMasksOnOff,$
              fApplySavedClusterMask:0b,$
             wApplyPaintedMaskOnOff:wApplyPaintedMaskOnOff,$
              fApplyPaintedMask:0b,$
             wApplySavedROI2DOnOff:wApplySavedROI2DOnOff,$
              fApplySavedROI2D:0b,$
             wApplySavedROI3DOnOff:wApplySavedROI3DOnOff,$
              fApplySavedROI3D:0b,$
             wCreateAndSave2DModelObjectOnOff:wCreateAndSave2DModelObjectOnOff,$
              fCreateAndSave2DModel:0b,$
             wCreateAndSavePainted2DModelObjectOnOff:wCreateAndSavePainted2DModelObjectOnOff,$
              fCreateAndSavePainted2DModelObject: 0b,$
             wUpdate3DROIWithGrowthFactorOnOff:wUpdate3DROIWithGrowthFactorOnOff,$
              fUpdate3DROIWithGrowthFactor:0b,$
             wAcceptAllObjectsOnOff:wAcceptAllObjectsOnOff,$
              fAcceptAllObjects:0b,$
             wCreateAndSave3DModelObjectOnOff:wCreateAndSave3DModelObjectOnOff,$
              fCreateAndSave3DModel:0b,$
             wCreateAndSavePainted3DModelObjectOnOff:wCreateAndSavePainted3DModelObjectOnOff,$
              fCreateAndSavePainted3DModelObject: 0b,$
             wCreateAndSave3DModelFromNumberedMasksOnOff:wCreateAndSave3DModelFromNumberedMasksOnOff,$
              fCreateAndSave3DModelFromNumberedMasks: 0b,$
             wCreateAndSaveEmpty3DModelObjectOnOff:wCreateAndSaveEmpty3DModelObjectOnOff,$
              fCreateAndSaveEmpty3DModelObject:0b,$
             wApplyLastSegmentationStepOnOff:wApplyLastSegmentationStepOnOff,$
              fApplyLastSegmentationStep:1b,$
             wApplyForAllClustersOnOff:wApplyForAllClustersOnOff,$
              fApplyForAllClusters:0b,$
             wROIObjectWindowOnOff:wROIObjectWindowOnOff,$
              fROIObjectWindow:0b,$
              child_ROIObjectWindow_tlb:-1L,$
             wCopyAllClusterProceduresOnOff:wCopyAllClusterProceduresOnOff,$
              fCopyAllClusterProcedures:0b,$
              oCopyAllClusterContainer:obj_new(),$
             wCopySingleClusterProcedureOnOff:wCopySingleClusterProcedureOnOff,$
              fCopySingleClusterProcedure:0b,$
              oCopySingleClusterContainer:obj_new(),$
             fActiveSingleCluster:-1,$
             pImageStackInfoObject:pImageStackInfoObject}

    widget_control, wTopBase, set_uValue = state, /no_copy
    s_ISegM_UpdateWidgets, wTopBase
    XManager, 's_ISegM_Resize', wTopBase, CleanUp = 's_ISegM_cleanUp', /no_block, Group_Leader = groupLeader
end