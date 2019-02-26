;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_InterClusObjBorderDist
;
; PURPOSE:
;       - Calculation of Inter Object (among different clusters) Border Distance
;
; AUTHOR:
;     J. Jara (2013)
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_InterClusObjBorderDist')
;
; METHOHDS:
;
; NOTES:
;     Experimental version. Not ready for use yet.
;
;_____________________________IOISIOI____________________
function C_sROIParam_InterClusObjBorderDist::getParamNames
  return, ['Avg. Distance [pixel]',$
           'Min. Distance [pixel]',$
           'Avg. Distance [x]',$
           'Min. Distance [x]']
end


pro C_sROIParam_InterClusObjBorderDist::apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = C_sROIGroupObj, stack_tlb = stack_tlb

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
    print, 'Not enough objects to calculate parameters: at least 2 objects must exist!'
    ;*(*self.pParamStruct).pROICoordinateMatrix = [-1,-1]
    ;*(*self.pParamStruct).pROIConnectMatrix = [-1,-1]
    *(*(*self.pValueStruct)[0]).pROIParamVect = -1
    *(*(*self.pValueStruct)[1]).pROIParamVect = -1
    return
  endif

  pps = C_sROIGroupObj->getPparamStruct()
  whPos = (where(*((*pps).pNames) eq 'Channel Position'))[0]
  chPos = *(((*pps).pValues)[whPos])
  whPos = (where(*((*pps).pNames) eq 'z-Slice Position'))[0]
  zPos  = *(((*pps).pValues)[whPos])
  whPos = (where(*((*pps).pNames) eq 'Time Position'))[0]
  tPos  = *(((*pps).pValues)[whPos])
  whPos = (where(*((*pps).pNames) eq 'Name'))[0]
  ; Don't wknow why the ROI group doesn't have a Cluster Position as property... performing stupid name parsing as workaround.
  name  = *(((*pps).pValues)[whPos])
  nameSpl = strSplit(name, '_', /EXTRACT)
  clusPos = s_getRightNumberFromString(nameSpl[0])

  fUseSelectedChPos = 1b
  if keyword_set(fUseSelectedClusPos) then clusPos2 = 1 else clusPos2 = clusPos; TODO "parametrize"
  if keyword_set(fUseSelectedChPos)   then chPos2 = 2   else chPos2   = chPos  ; TODO "parametrize"

  baseName = 'oROI2DGroup_'
  chStr    = 'ch' + string(chPos) + '_'
  chStr2   = 'ch' + string(chPos2) + '_'
  tStr     = 't' + string(tPos) + '_'
  zStr     = 'z' + string(zPos) + '_'
  clusStr  = 'Clus' + string(clusPos)
  clusStr2 = 'Clus' + string(clusPos2)
  ROIgroupStr  = strCompress(chStr + clusStr, /REMOVE_ALL)
  ROIgroup2Str = strCompress(chStr2 + clusStr2, /REMOVE_ALL)
  fileExt = '.dat'
  fileNameMin = strCompress('C:\RSI\C_sROIParam_InterClusObjBorderDist_' + ROIgroupStr + '_' + ROIgroup2Str + '_minDistanceMatrix' + fileExt, /REMOVE_ALL)
  fileNameAvg = strCompress('C:\RSI\C_sROIParam_InterClusObjBorderDist_' + ROIgroupStr + '_' + ROIgroup2Str + '_avgDistanceMatrix' + fileExt, /REMOVE_ALL)

  C_sROIGroupObj2 = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb, tPos = tPos, zPos = zPos, chPos = chPos2, clusPos = clusPos2, fileName = fileName)

  nObjects2 = C_sROIGroupObj2->count()
  if (nObjects2 lt 1) then begin
    print, 'Not enough objects in 2nd cluster to calculate parameters: at least 1 object must exist!'
    ;*(*self.pParamStruct).pROICoordinateMatrix = [-1,-1]
    ;*(*self.pParamStruct).pROIConnectMatrix = [-1,-1]
    *(*(*self.pValueStruct)[0]).pROIParamVect = replicate(-1, nObjects)
    *(*(*self.pValueStruct)[1]).pROIParamVect = replicate(-1, nObjects)
    return
  endif

  avgDistanceMatrix = make_array(nObjects2, nObjects, /float)
  minDistanceMatrix = make_array(nObjects2, nObjects, /float)
  minOverallVector = make_array(nObjects2, value = 10000000.0)
  ; TODO xySizePerPixel

  for i = 0L, nObjects-1 do begin
    iRoi = C_sROIGroupObj->get(position = i)
    iRoi->getProperty, data = iPoints
    nPoints = n_elements(iPoints[0,*])
    distanceArr = make_array(nPoints, /float)

    for j = 0L, nObjects2-1 do begin
      jRoi = C_sROIGroupObj2->get(position = j)
      jRoi->getProperty, data = jPoints
      for p = 0L, nPoints-1 do begin
        distanceArr[p] = min(sqrt((jPoints[0,*] - iPoints[0,p])^2 + (jPoints[1,*] - iPoints[1,p])^2))
      endfor
      minDistanceMatrix[j,i] = min(distanceArr, max = maxDistance)
      avgDistanceMatrix[j,i] = total(distanceArr) / nPoints
    endfor ; Careful when debugging, the values in distanceArr values overwritten, no reinitialization is being made.
  endfor

  for j = 0L, nObjects2-1 do $
    minOverallVector[j] <= min(minDistanceMatrix[j,*])

  openW, wUnit, fileNameMin, /GET_LUN
  for i = 0L, nObjects-1 do begin
    printf, wUnit, 'Object ' + strTrim(string(i+1))
    for j = 0L, nObjects2-1 do begin
      printf, wUnit, strTrim(string(minDistanceMatrix[j,i]), 2)
    endfor
  endfor
  printf, wUnit, 'Overall'
  for j = 0L, nObjects2-1 do $
    printf, wUnit, strTrim(string(minOverallVector[j]), 2)
  close, wUnit

  openW, wUnit, fileNameAvg, /GET_LUN
  for i = 0L, nObjects-1 do begin
    printf, wUnit, 'Object ' + strTrim(string(i+1))
    for j = 0L, nObjects2-1 do $
      printf, wUnit, strTrim(string(avgDistanceMatrix[j,i]), 2)
  endfor
  close, wUnit

  free_lun, wUnit
  distanceArr = -1
  nStructs = n_elements(*self.pValueStruct)

  ; TODO JJ mock info. by now...
  for i = 0, nStructs-1 do $
    *(*(*self.pValueStruct)[i]).pROIParamVect = replicate(1.0, nObjects)
  ;*(*(*self.pValueStruct)[0]).pROIParamVect = replicate(1.0, nObjects)
  ;*(*(*self.pValueStruct)[1]).pROIParamVect = replicate(1.0, nObjects)

end


pro C_sROIParam_InterClusObjBorderDist::cleanup

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


function C_sROIParam_InterClusObjBorderDist::init

  paramType  = 'Inter ROI-Parameter-Method' ; 'Inter ROI-Parameter-Method'
  paramNames = self->getParamNames()

  s_makeDefaultROIParamStruct, 'Inter-Group Object Border Distance',$
                               paramType,$
                               paramNames,$
                               replicate('Inter ROI-Parameter-Method', n_elements(paramNames)),$
                               pParamStruct = pPS,$
                               pValueStruct = pVS
  self.pParamStruct = pPS
  self.pValueStruct = pVS
  return, 1
end


pro C_sROIParam_InterClusObjBorderDist__define
  tmp = {C_sROIParam_InterClusObjBorderDist, $
         pParamStruct : ptr_new(), $
         pValueStruct : ptr_new(), $
         inherits C_sROIParam}
end
