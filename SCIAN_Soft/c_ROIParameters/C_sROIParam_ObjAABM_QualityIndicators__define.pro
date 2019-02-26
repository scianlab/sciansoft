; (this is art) _____________________________ IOISIOI _____________________________ (this is art)
; NAME:
;      C_sROIParam_ObjAABM_QualityIndicators
;
; PURPOSE:
;       - Calculation of quality indicators
;
; AUTHOR:
;     J. Jara (2013)
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjAABM_QualityIndicators')
;
; METHOHDS:
;
; NOTES:
;     Experimental version. Not ready for use yet.
;
; (this is art) _____________________________ IOISIOI _____________________________ (this is art)
function C_sROIParam_ObjAABM_QualityIndicators::getParamNames
  return, ['Haussdorff',$
           'Dice',$
           'NSD',$
           'Jaccard',$
           'Neighbor Number',$
           'Shared Boundary %']
end


pro C_sROIParam_ObjAABM_QualityIndicators::apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = C_sROIGroupObj, stack_tlb = stack_tlb, $
                                                  pRoiCoordsX = pRoiCoordsX, pRoiCoordsY = pRoiCoordsY, baseFileName = baseFileName
  paramNames = self->getParamNames()
  nParams    = n_elements(paramNames)

  whParam = [-1]
  for i = 0, nParams-1 do $
    whParam = [whParam, (where(*(*self.pParamStruct).pNames eq paramNames[i]))[0]]

  if (nParams gt 1) then whParam = whParam[1:*]

    ; check active parameter vector, called "position" (how intuitive, Steff!)
  whParamActive = whParam * 0
  case (n_elements(position) gt 0) of
    1   : if (position[0] eq -1) then return else whParamActive[position] = 1
    else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
  endcase
  wherePA = where(whParamActive eq 1)

    ; Check pointers
  if ~ptr_valid((*self.pParamStruct).pROINumberVect) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
  if (wherePA[0] eq -1) then return
  for i = 0, n_elements(wherePA)-1 do $
    if ~ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

    ; Return if there are no objects
  nRois = C_sROIGroupObj->count()
  if (nRois lt 1) then begin
    print, 'No ROIs in this C_sROIGroupObj to calculate parameters!'
    ;*(*self.pParamStruct).pROICoordinateMatrix = [-1,-1]
    ;*(*self.pParamStruct).pROIConnectMatrix = [-1,-1]
    *(*(*self.pValueStruct)[0]).pROIParamVect = -1
    *(*(*self.pValueStruct)[1]).pROIParamVect = -1
    return
  endif

  pps   = C_sROIGroupObj->getPparamStruct()
  whPos = (where(*((*pps).pNames) eq 'Channel Position'))[0]
  chPos = *(((*pps).pValues)[whPos])
  whPos = (where(*((*pps).pNames) eq 'z-Slice Position'))[0]
  zPos  = *(((*pps).pValues)[whPos])
  whPos = (where(*((*pps).pNames) eq 'Time Position'))[0]
  tPos  = *(((*pps).pValues)[whPos])
  whPos = (where(*((*pps).pNames) eq 'Name'))[0]
  ; Don't wknow why the ROI group doesn't have a Cluster Position as property... performing name parsing as workaround.
  name  = *(((*pps).pValues)[whPos])
  nameSpl = strSplit(name, '_', /EXTRACT)
  clusPos = s_getRightNumberFromString(nameSpl[0])
  ; XXX [JJ] clusPos is not an atribute in C_sROIGroupObj class (2012.08)! Workaround: get it from s_ISM_getProjectInfo 
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, clusPos = clusPos

  fUseSelectedChPos = 1b
  if keyword_set(fUseSelectedClusPos) then clusPos2 = 1 else clusPos2 = clusPos; TODO "parametrize" these two
  if keyword_set(fUseSelectedChPos)   then chPos2   = 2 else chPos2   = chPos

  baseName = 'oROI2DGroup_'
  chStr1   = 'ch' + string(chPos) + '_'
  chStr2   = 'ch' + string(chPos2) + '_'
  tStr     = 't' + string(tPos) + '_'
  zStr     = 'z' + string(zPos) + '_'
  ; XXX [JJ] clusPos is not an atribute in C_sROIGroupObj class (2012.08)! Workaround: get it from s_ISM_getProjectInfo 
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, clusPos = clusPos, stateObj_tlb = stateObj_tlb
  clusStr1     = 'Clus' + string(clusPos)
  clusStr2     = 'Clus' + string(clusPos2)
  ROIgroupStr  = strCompress(chStr1 + clusStr1, /REMOVE_ALL)
  ROIgroup2Str = strCompress(chStr2 + clusStr2, /REMOVE_ALL)
  fileExt      = '.csv'
  paramName    = '_AABMqualityIndicators'
  sep  = path_sep()
  path = file_dirName(baseFileName)
  ;roiGroupFilePathNameBase = strCompress(baseFileName + chStr1 + tStr + zStr + clusStr1, /REMOVE_ALL)
  ;path = s_getPathForProjectGeneric()
  ;paramFilePathNameBase = strCompress(path + sep + '_AAB_' + sep + paramName + '_' + clusStr1 + fileExt, /REMOVE_ALL) ; Victor's
  paramFilePathNameBase = strCompress(baseFileName + paramName + fileExt, /REMOVE_ALL) ; TODO JJ's TMP
  fFileExists = file_test(paramFilePathNameBase)
  openW, wUnit, paramFilePathNameBase, /APPEND, /GET_LUN
  if ~fFileExists then printf, wUnit, 'Object ID;Time;Z-slice;Channel;Hausdorff Distance;Dice Coefficient;NSD;Jaccard Index;Neighbor ROIs'

  ; AABM object container (global) parameters
  fComputeOnly              = 1b ; If set, no graphic output will be produced
  fSaveContours             = 0b ; Not yet
  fDoVectorFieldCalculation = 0b ; Can force or avoid vector field calculation
  fDoContourAdjustment      = 0b ; Option to avoid contour adjustment.
  fUseAABcolorCodes         = 1b ; Option to use AABM color codes for contour vertices (see documentation in C_sAABContainer__Define).

  aabmAlpha = 0.05
  aabmBeta  = 0.05
  aabmGamma = 1.0
  aabmKappa = 1.0
  aabmContourMaxIterations    = 5
  aabmContourConvergenceLimit = 0.01
  aabmPerimeterFactor         = 2.0

  aabmVFtype = 'EP-GVF'; 'GVF', 'GGVF' or 'EP-GVF'
  aabmVFmaxIterations      = 300
  aabmVFconvergenceLimit   = 0.01
  aabmVFmu                 = 0.1
  aabmEPGVFnoiseCut        = 0.1
  aabmEPGVFnoiseRange      = 0.2
  ;aabmEPGVFnumericalStep     = 1   ; optional parameter, could be left undefined
  aambEPGVFsigmaGaussianBlur = 0   ; 0-> no blur before vector field computation
  aabmFverbose               = 1b  ; set verbose mode ON while doing my PhD thesis ;)
  aabmFnoInterpolateAtFirst  = 1b  ; 1-> avoid interpolation while setting contours
  aabmMinContourPointCount   = 16
  aabmPointSamplingDistance  = 1.0
  aabmProximityDistance      = 3.0 ; adjacency detection threshold distance, in pixels
  aabmSeparationDistance     = 0.0 ; adjacency adjustment distance, in pixels

  fUseInputParamContours = (n_elements(pRoiCoordsX) eq nRois) and (n_elements(pRoiCoordsY) eq nRois)
  fUseGraphicModel = ~fUseInputParamContours 
  fDestroyOAAB     = 0b

  if fUseGraphicModel then begin
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
    widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
      oAABMModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('2D Adjacent Active Contours') ;AAB model
    widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
    widget_control, stack_tlb, get_uValue = stateStack, /no_copy
      image = (*stateStack.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
    widget_control, stack_tlb, set_uValue = stateStack, /no_copy

    if fUseGraphicModel and ~obj_valid(oAABMModel) then begin
      fDestroyOAAB = 1b
      oAABM = obj_new('C_sAABContainer',$
                    image,$
                    alphaVal = aabmAlpha,$
                    betaVal  = aabmBeta,$
                    gammaVal = aabmGamma,$
                    kappaVal = fDoVectorFieldCalculation gt 0 ? aabmKappa : 0.0,$
                    proximityDist           = aabmProximityDistance,$
                    separationDist          = aabmSeparationDistance,$
                    contourMaxIterations    = aabmContourMaxIterations,$
                    vfMaxIterations         = aabmVFmaxIterations,$
                    contourSamplingDistance = aabmPointSamplingDistance,$
                    fVerbose                = aabmFverbose,$
                    roiGroupFilePath        = roiGroupFilePathNameBase)

      ; Image vector field calculation.
      if fDoVectorFieldCalculation then $
      fVFok = oAABM->calcVectorField(type = aabmVFtype, mu = aabmVFmu, iterations = aabmVFmaxIterations, convergenceLimit = aabmVFconvergenceLimit)

      fNobjSet = oAABM->preSetNumContours(nRois)
      for i = 0L, nRois-1 do begin
        pObjectBorderPolygon = (C_sROIGroupObj->get(position = i))->getpObjectBorderPolygonList()
        fContourSet = oAABM->setContour(transpose((*pObjectBorderPolygon[0])[0,*]), transpose((*pObjectBorderPolygon[0])[1,*]), i, FNOINTERPOLATE = aabmFnoInterpolateAtFirst)
      endfor

      fAABMsuccess = oAABM->adjustContours(fDebug = aabmFverbose)
      if ~fAABMsuccess then stop
      aabmContours = oAABM->getContours(nContours = nObjOut, pAABMvertexColors = pContourColors)
      pRoiCoordsX = aabmContours.pX
      pRoiCoordsY = aabmContours.pY
    endif
  endif

  ; Make vectors for storing the calculated parameters
  diceCoeffVector    = fltArr(nRois)
  hausdorffDstVector = fltArr(nRois)
  nsdVector          = fltArr(nRois)
  neighborRoisVector = fltArr(nRois)
  jaccardVector      = fltArr(nRois)
  neighborNumVector  = fltArr(nRois)
  freeBoundaryVector = fltArr(nRois)

  if (n_elements(pRoiCoordsX) ne nRois) or (n_elements(pRoiCoordsY) ne nRois) then stop

  for i = 0L, nRois-1 do begin

    pObjectBorderPolygon = (C_sROIGroupObj->get(position = i))->getpObjectBorderPolygonList()
    p1x = transpose((*pObjectBorderPolygon[0])[0,*])
    p1y = transpose((*pObjectBorderPolygon[0])[1,*])

    ; Resample polygon points to make "fair" comparison
    polygonLineSample, p1x, p1y, p1xInt, p1yInt, nPointsPerPix = aabmPointSamplingDistance, /fCloseOutput
    nPtsClosed = n_elements(p1xInt)
    p1xInt = p1xInt[0:nPtsClosed-1]
    p1yInt = p1yInt[0:nPtsClosed-1]

    if obj_valid(oAABM) then begin
      p2x = *((aabmOut.pX)[i])
      p2y = *((aabmOut.pY)[i])
    endif else begin
      p2x = reform(*pRoiCoordsX[i])
      p2y = reform(*pRoiCoordsY[i])
    endelse

    diceCoeffVector[i]    = calcDiceCoefficientForPolygons(p1x, p1y, p2x, p2y)
    jaccardVector[i]      = calcJaccardSimCoefficientForPolygons(p1x, p1y, p2x, p2y)
    hausdorffDstVector[i] = s_HausdorffDistanceFor2Dpoints(p1xInt, p1yInt, p2x, p2y)
    nsdVector[i]          = calcNSDfromPolygonToPolygon(p1xInt, p1yInt, p2x, p2y, /fPlotWindow, winCaption = 'ROI ' + StrCompress(string(i), /remove_all))
    neighborNumVector[i]  = -1.0
    freeBoundaryVector[i] = -1.0
    sc = ';'
    fmtFlt = '(F13.3)'
    fmtInt = '(I3)'
    printf, wUnit, strCompress(string(i) + sc $
                   + string(tPos) + sc $
                   + string(zPos) + sc $
                   + string(chPos) + sc $
                   + string(hausdorffDstVector[i], format = fmtFloat) + sc $
                   + string(diceCoeffVector[i], format = fmtFloat) + sc $
                   + string(nsdVector[i], format = fmtFloat) + sc $
                   + string(jaccardVector[i], format = fmtFloat) + sc $
                   + string(neighborRoisVector[i], format = fmtInt), /rem)
  endfor
  close, wUnit
  free_lun, wUnit
  print, 'C_sROIParam_ObjAABM_QualityIndicators ' + fileExt + ' file done.'
  if (fDestroyOAAB ne 0) then obj_destroy, oAABM
;stop

  ; TODO 1. Fill parameter vector values. 2. Convert to real units... xySizePerPixel
  nStructs = n_elements(*self.pValueStruct)

  *(*(*self.pValueStruct)[0]).pROIParamVect = diceCoeffVector
  *(*(*self.pValueStruct)[1]).pROIParamVect = hausdorffDstVector
  *(*(*self.pValueStruct)[2]).pROIParamVect = nsdVector
  *(*(*self.pValueStruct)[3]).pROIParamVect = jaccardVector
  *(*(*self.pValueStruct)[4]).pROIParamVect = neighborNumVector
  *(*(*self.pValueStruct)[5]).pROIParamVect = freeBoundaryVector

end


pro C_sROIParam_ObjAABM_QualityIndicators::cleanup

  for i = 0, n_tags((*self.pParamStruct))-1 do begin
    case size((*self.pParamStruct).(i), /tname) of
      'POINTER': ptr_free, (*self.pParamStruct).(i)
      'OBJREF' : obj_destroy, (*self.pParamStruct).(i)
      else:
    endcase
  endfor

  for j = 0, n_elements(*self.pValueStruct)-1 do begin
    for i = 0, n_tags((*(*self.pValueStruct)[j]))-1 do begin
      case size((*(*self.pValueStruct)[j]).(i), /tname) of
        'POINTER': ptr_free, (*(*self.pValueStruct)[j]).(i)
        'OBJREF' : obj_destroy, (*(*self.pValueStruct)[j]).(i)
        else:
      endcase
    endfor
    ptr_free, (*self.pValueStruct)[j]
  endfor

  ptr_free, self.pValueStruct
  ptr_free, self.pParamStruct

end


function C_sROIParam_ObjAABM_QualityIndicators::init

  paramType  = 'Inter ROI-Parameter-Method' ; 'Inter ROI-Parameter-Method'
  paramNames = self->getParamNames()

  s_makeDefaultROIParamStruct, 'AABM Quality Indicators',$
                               paramType,$
                               paramNames,$
                               replicate('Inter ROI-Parameter-Method', n_elements(paramNames)),$
                               pParamStruct = pPS,$
                               pValueStruct = pVS
  self.pParamStruct = pPS
  self.pValueStruct = pVS
  return, 1
end


pro C_sROIParam_ObjAABM_QualityIndicators__define
  tmp = {C_sROIParam_ObjAABM_QualityIndicators, $
         pParamStruct : ptr_new(), $
         pValueStruct : ptr_new(), $
         inherits C_sROIParam}
end
