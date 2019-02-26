; (this is art) _____________________________ IOISIOI _____________________________ (this is art)
; NAME:
;      C_sROIParam_ObjAABM_Descriptors__define.pro
;
; PURPOSE:
;       - Calculation of quality indicators
;
; AUTHOR:
;     J. Jara (2013)
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjAABM_Descriptors__define.pro')
;
; METHOHDS:
;
; NOTES:
;     Experimental version. Not ready for use yet.
;
; (this is art) _____________________________ IOISIOI _____________________________ (this is art)
function C_sROIParam_ObjAABM_Descriptors::getParamNames
  return, ['Number of Neighbors',$
           'Shared Boundary [%]',$
           'Number of Junctions',$
           'Avg. Junction Angle [deg]',$
           'Max. Junction Angle [deg]',$
           'Min. Junction Angle [deg]',$
           'Voronoi Polygon Area [pix]',$
           'Voronoi Polygon Area [x^2]']
end


pro C_sROIParam_ObjAABM_Descriptors::apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = C_sROIGroupObj, stack_tlb = stack_tlb, baseFileName = baseFileName

  paramNames = self->getParamNames()
  nParams    = n_elements(paramNames)

  whParam = [-1]
  for i = 0, nParams-1 do $
    whParam = [whParam, (where(*(*self.pParamStruct).pNames eq paramNames[i]))[0]]

  if (nParams gt 1) then whParam = whParam[1:*]

    ; check Active Parameter
  whParamActive = whParam * 0
  case (n_elements(position) gt 0) of
    1   : if (position[0] eq -1) then return else whParamActive[position] = 1
    else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
  endcase
  wherePA = where(whParamActive eq 1)

    ; check Pointers
  if ~ptr_valid((*self.pParamStruct).pROINumberVect) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
  if (wherePA[0] eq -1) then return
  for i = 0, n_elements(wherePA)-1 do $
    if ~ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

    ; Return if not enough objects exist to calculate parameters
  nObjects = C_sROIGroupObj->count()
  if (nObjects lt 1) then begin
    print, 'No ROIs in this C_sROIGroupObj to calculate parameters!'
    stop ; try for loop to set all paramvects to -1...
    ;*(*self.pParamStruct).pROICoordinateMatrix = [-1,-1]
    ;*(*self.pParamStruct).pROIConnectMatrix = [-1,-1]
    for i = 0u, nParams-1 do $
      *(*(*self.pValueStruct)[i]).pROIParamVect = -1
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
  if keyword_set(fUseSelectedClusPos) then clusPos2 = 1 else clusPos2 = clusPos; TODO "parametrize"
  if keyword_set(fUseSelectedChPos)   then chPos2   = 2 else chPos2   = chPos  ; TODO "parametrize"

  baseName = 'oROI2DGroup_'
  chStr    = 'ch' + string(chPos) + '_'
  chStr2   = 'ch' + string(chPos2) + '_'
  tStr     = 't' + string(tPos) + '_'
  zStr     = 'z' + string(zPos) + '_'
  ; XXX [JJ] clusPos is not an atribute in C_sROIGroupObj class (2012.08)! Workaround: get it from s_ISM_getProjectInfo 
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, clusPos = clusPos, stateObj_tlb = stateObj_tlb
  clusStr  = 'Clus' + string(clusPos)
  clusStr2 = 'Clus' + string(clusPos2)
  ROIgroupStr  = strCompress(chStr + clusStr, /REMOVE_ALL)
  ROIgroup2Str = strCompress(chStr2 + clusStr2, /REMOVE_ALL)
  fileExt = '.csv'
  paramPrefix = 'C_sROIParam_ObjAABM_Descriptors__define_'
  path = file_dirname(baseFileName)
  pathSep = path_sep()
  ;roiGroupFilePathNameBase = strCompress(baseFileName + chStr + tStr + zStr + clusStr, /REMOVE_ALL)
;path = s_getPathForProjectGeneric()
  folderPath = path + pathSep + '_AAB_' + pathSep
  paramFilePathNameBase = strCompress(folderPath + paramPrefix + clusStr + fileExt, /REMOVE_ALL)
  fFileExists = file_test(paramFilePathNameBase)
  fWriteToFile = 1b
  if fWriteToFile then begin
    if file_test(folderPath, /DIRECTORY) eq 0 then file_mkDir, folderPath
    openW, wUnit, paramFilePathNameBase, /APPEND, /GET_LUN
    if ~fFileExists then printf, wUnit, 'Object,time,z_slice,channel,Haussdorf,Dice,NSD,Jaccard,neighborRois'
  endif

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

  if min(wherePA) gt 5 then goto, g

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
  widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
    oAABMModel  = *(stateObjWindow.poCurrROIGraphicModel)->getByName('2D Adjacent Active Contours') ;AAB model
  widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
  widget_control, stack_tlb, get_uValue = stateStack, /no_copy
    image = (*stateStack.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
  widget_control, stack_tlb, set_uValue = stateStack, /no_copy

  fDestroyOAAB = 0b
  if ~obj_valid(oAABMModel) then begin
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

    fNobjSet = oAABM->preSetNumContours(nObjects)
    for i = 0L, nObjects-1 do begin
      pObjectBorderPolygon = (C_sROIGroupObj->get(position = i))->getpObjectBorderPolygonList()
      fContourSet = oAABM->setContour(transpose((*pObjectBorderPolygon[0])[0,*]), transpose((*pObjectBorderPolygon[0])[1,*]), i, fNoInterpolate = aabmFnoInterpolateAtFirst)
    endfor

    fAABMsuccess = oAABM->adjustContours(fDebug = aabmFverbose)
    if ~fAABMsuccess then stop
    aabmContours = oAABM->getContours(nContours = nObjOut, pAABMvertexColors = pContourColors)
  endif

  ; Make vectors for storing the calculated parameters
  ;diceCoeffVector     = fltArr(nObjects)
  ;hausdorffDistVector = fltArr(nObjects)
  ;nsdVector           = fltArr(nObjects)
  ;jaccardVector       = fltArr(nObjects)
  ;neighborRoisVector  = fltArr(nObjects)
  neighborNumVector   = fltArr(nObjects)
  freeBoundaryVector  = fltArr(nObjects)

  for i = 0L, nObjects-1 do begin

    pObjectBorderPolygon = (C_sROIGroupObj->get(position = i))->getpObjectBorderPolygonList()
    p1x = transpose((*pObjectBorderPolygon[0])[0,*])
    p1y = transpose((*pObjectBorderPolygon[0])[1,*])

    if obj_valid(oAABM) then begin
      p2x = *(aabmContours.pX)[i]
      p2y = *(aabmContours.pY)[i]
    endif else begin
      (oAABMModel->get(position = i))->getProperty, data = polygon2
      p2x = transpose(polygon2[0,*])
      p2y = transpose(polygon2[1,*])
    endelse

    ;diceCoeffVector[i]     = calcDiceCoefficientForPolygons(p1x, p1y, p2x, p2y)
    ;hausdorffDistVector[i] = s_HausdorffDistance2Dpoints([p1x, p1y], [p2x, p2y])
    ;nsdVector[i]           = calcNSDfromPolygonToPolygon(p1x, p1y, p2x, p2y)
    ;jaccardVector[i]       = calcJaccardSimCoefficientForPolygons(p1x, p1y, p2x, p2y)
    neighborNumVector[i]   = -1.0
    freeBoundaryVector[i]  = -1.0
    if fWriteToFile then $
      printf, wUnit, (string(i)+','+string(tPos)+','+string(zPos)+','+string(chPos)+','+string(hausdorffDistVector[i])+','+string(diceCoeffVector[i])+','+string(nsdVector[i])+','+string(jaccardVector[i])+','+string(neighborRoisVector[i]))
  endfor
  if fWriteToFile then close, wUnit
  if (fDestroyOAAB ne 0) then obj_destroy, oAABM
;stop

  ; TODO 1. Fill parameter vector values. 2. Convert to real units... xySizePerPixel
g: nStructs = n_elements(*self.pValueStruct)

  *(*(*self.pValueStruct)[0]).pROIParamVect = replicate(-1.0, nObjects)
  *(*(*self.pValueStruct)[1]).pROIParamVect = replicate(-1.0, nObjects)
  *(*(*self.pValueStruct)[2]).pROIParamVect = replicate(-1.0, nObjects)
  *(*(*self.pValueStruct)[3]).pROIParamVect = replicate(-1.0, nObjects)
  *(*(*self.pValueStruct)[4]).pROIParamVect = replicate(-1.0, nObjects);neighborNumVector
  *(*(*self.pValueStruct)[5]).pROIParamVect = replicate(-1.0, nObjects);freeBoundaryVector

  whVoronoiParams = where((wherePA eq 6) or (wherePA eq 7), countVoronoiParams)
  if (countVoronoiParams gt 0) then begin

    moments2D = s_2Dmoments(C_sROIGroupObj->getGroupMask(), order = 3, fAreaNorm = 1b)
    xMassCenterCoords = moments2D[*,1]
    yMassCenterCoords = moments2D[*,2]

;    xMassCenterCoords = fltArr(nObjects)
;    yMassCenterCoords = fltArr(nObjects)
;    for i = 0L, nObjects-1 do begin
;      pObjectBorderPolygon = (C_sROIGroupObj->get(position = i))->getpObjectBorderPolygonList()
;      polyCentroid = polygonCentroid(transpose((*pObjectBorderPolygon[0])[0,*]), transpose((*pObjectBorderPolygon[0])[1,*]))
;      xMassCenterCoords[i] = polyCentroid[0]
;      yMassCenterCoords[i] = polyCentroid[1]
;    endfor
    roiGroupObjParamStruct = *(C_sROIGroupObj->getpParamStruct())
    xMax = *(roiGroupObjParamStruct.pValues[(where(*(((roiGroupObjParamStruct)).pNames) eq 'x-Size [pixel]'))[0]])
    yMax = *(roiGroupObjParamStruct.pValues[(where(*(((roiGroupObjParamStruct)).pNames) eq 'y-Size [pixel]'))[0]])
    fClipConvexHull = 1b
    voronoiPolys = voronoiPolygons(xMassCenterCoords, yMassCenterCoords, xMaxCoord = xMax, yMaxCoord = yMax, fClipConvexHull = fClipConvexHull, polyAreas = voronoiPolyAreas)
    *(*(*self.pValueStruct)[6]).pROIParamVect = voronoiPolyAreas
    *(*(*self.pValueStruct)[7]).pROIParamVect = voronoiPolyAreas * xySizePerPixel
  endif else begin
    *(*(*self.pValueStruct)[6]).pROIParamVect = [-1]
    *(*(*self.pValueStruct)[7]).pROIParamVect = [-1]
  endelse
end


pro C_sROIParam_ObjAABM_Descriptors::cleanup

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


function C_sROIParam_ObjAABM_Descriptors::init

  paramType  = 'Inter ROI-Parameter-Method' ; 'Inter ROI-Parameter-Method'
  paramNames = self->getParamNames()

  s_makeDefaultROIParamStruct, 'AABM Descriptors',$
                               paramType,$
                               paramNames,$
                               replicate('Inter ROI-Parameter-Method', n_elements(paramNames)),$
                               pParamStruct = pPS,$
                               pValueStruct = pVS
  self.pParamStruct = pPS
  self.pValueStruct = pVS
  return, 1
end


pro C_sROIParam_ObjAABM_Descriptors__define
  tmp = {C_sROIParam_ObjAABM_Descriptors, $
         pParamStruct: ptr_new(), $
         pValueStruct: ptr_new(), $
         inherits C_sROIParam}
end
