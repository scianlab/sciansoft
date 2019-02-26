; ____________________ (this is art) IOISIOI (this is art) ____________________
; NAME
;     C_sROIParam_ObjOverlap
;
; PURPOSE
;     Calculation of Object Intersection/Overlap with other Objects in the Group
;
; AUTHOR
;     Jorge Jara (2012)
;     e_mail: jjara@dcc.uchile.cl
;
; CALLING SEQUENCE
;     oParam = obj_new('C_sROIParam_3DObjOverlap')
;
; METHOHDS
;     init, apply, getParamNames
;
; NOTES
;     Polygon computations are implemented at the moment only for ROIs without holes.
;
; ____________________ (this is art) IOISIOI (this is art) ____________________

function C_sROIParam_3DObjOverlap::getParamNames
  return, [$;'Polygonal intersection areas [N]',$
           ;'Polygonal intersection ROIs [N]',$
           ;'Polygonal intersection size [voxel]',$
           ;'Polygonal intersection [x^3]',$
           ;'Polygonal intersection [%]',$
           ;'Voxel intersection areas [N]',$
           '3D Voxel intersection ROIs [N]'];,$
           ;'Voxel intersection size [voxel]',$
           ;'Voxel intersection size [x^3]',$
           ;'Voxel intersection [%]']
end


pro C_sROIParam_3DObjOverlap::apply, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position, stack_tlb = stack_tlb

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
  pParamStruct = C_sROI3DGroupObj->getpParamStruct()

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
  widget_control, stack_tlb, set_uValue = stackState, /no_copy

  imgPparamStruct = oImage->getpParamStruct()
  imageStr = *((*imgPparamStruct).pValues) [(where(*((*imgPparamStruct).pNames) eq 'Name'))[0]] + '_'

  baseName = 'oROI3DGroup_'
  clusStr  = 'Clus' + string(clusPos) + '_'
  chStr    = 'ch' + string(chPos) + '_'
  tStr     = 't' + string(tPos) + '_'
  zStr     = 'z' + string(zPos)
  ROIgroupStr = strCompress(baseName + imageStr + chStr + tStr + zStr, /REMOVE_ALL)

  fLogToFile = 0b
  strDate  = sysTime()
  splDate  = strSplit(strDate)
  strY     = strMid(strDate, splDate[4])
  strM     = strMid(strDate, splDate[1], splDate[2]-splDate[1])
  strD     = strMid(strDate, splDate[2], splDate[3]-splDate[2])
  strHMS   = strMid(strDate, splDate[3], splDate[4]-splDate[3])
  colonPos = strPos(strHMS, ':')
  strPut, strHMS, '_', colonPos
  colonPos = strPos(strHMS, ':')
  strPut, strHMS, '_', colonPos
  strStartDate = strCompress(strY + '-' + strM + '-' + strD + '-' + strHMS, /REMOVE_ALL)
  logExt = '.txt'
  basePath = 'C:\RSI\'
  logBaseName =  basePath + 'log_C_sROIParam_3DObjOverlap_' + strStartDate + '_' + ROIgroupStr
  sep = ';'

    ; Hard-data log file with a header for the coeffs. (columms)
  logDataFileName = logBaseName + '_data' + logExt
  logHeader = ''

  paramNames = self->getParamNames()
  nParams = n_elements(paramNames)

  logHeader = strJoin(paramNames, sep)
  if (fLogToFile eq 1) then fileLogger, logHeader, logDataFileName

  whParam = [-1]
  for i = 0, nParams-1 do $
    whParam = [whParam, (where(*(*self.pParamStruct).pNames eq paramNames[i]))[0]]

  if (nParams ge 1) then whParam = whParam[1:*]

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

  nObjects = C_sROI3DGroupObj->count()
  if (nObjects lt 1) then begin
    *(*self.pParamStruct).pROINumberVect = -1
    for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    if (fLogToFile eq 1) then fileLogger, 'No ROIs', logDataFileName
    return
  endif

  ; set Object Number Vector
  *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()

  ; set Object Parameter Vectors
  for i = 0, n_elements(whParam)-1 do *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float, value = -1.)

  whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [pixel]'))[0]
  xDimVox  = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [pixel]'))[0]
  yDimVox  = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'z-Size [pixel]'))[0]
  zDimVox  = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [real]'))[0]
  xDimReal = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [real]'))[0]
  yDimReal = *(((*pParamStruct).pValues)[whereDim])
  whereDim = (where((*(*pParamStruct).pNames) eq 'z-Size [real]'))[0]
  zDimReal = *(((*pParamStruct).pValues)[whereDim])

  xVoxelSize  = xDimReal / xDimVox
  yVoxelSize  = yDimReal / yDimVox
  zVoxelSize  = zDimReal / zDimVox
  xyzVoxelSize = xVoxelSize * yVoxelSize * zVoxelSize

    ; Check which kind of computations need to be performed
  ;fDoPolygonCalcs = whParamActive[0] or whParamActive[1] or whParamActive[2] or whParamActive[3] or whParamActive[4]
  ;fDoVoxelCalcs   = whParamActive[5] or whParamActive[6] or whParamActive[7] or whParamActive[8] or whParamActive[9]
  fDoPolygonCalcs = 0b
  fDoVoxelCalcs   = 1b

  if (fDoPolygonCalcs eq 1) then begin
    print, 'Not yet implemented...'
  endif

  ; TODO Pablo|Pao, set these two things as needed.
  chPos2   = 2;5
  clusPos2 = 0;1
  C_sROI3DGroupObj2 = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos2, clusPos = clusPos2)

  if ~obj_valid(C_sROI3DGroupObj2) then begin
    errMsg = 'Unable to find ROI group for channel ' + string(chPos2) + ' - cluster ' + string(clusPos2)
    print, errMsg
    if (fLogToFile eq 1) then fileLogger, errMsg, logDataFileName
    return
  endif

  fUseLib = 0

  if (fDoVoxelCalcs eq 1) then begin
    calcROIoverlapBetweenROIgroups, C_sROI3DGroupObj, C_sROI3DGroupObj2,$
                                    intersectionRegionCount = intersectionRegionCount,$
                                    intersectionObjectCount = intersectionObjectCount,$
                                    intersectionSize        = intersectionSize,$
                                    intersectionFraction    = intersectionFraction,$
                                    fUseLib = fUseLib

    ;print, 'intersectionRegionCount', intersectionRegionCount
    ;print, 'intersectionObjectCount', intersectionObjectCount
    ;print, 'intersectionSize', intersectionSize
    ;print, 'intersectionFraction', intersectionFraction
    if (fLogToFile eq 1) then for i = 0L, nObjects-1 do begin
      logMsg = strTrim(string(intersectionRegionCountP[i]),2) + sep $
             + strTrim(string(intersectionObjectCountP[i]),2) + sep $
             + strTrim(string(intersectionSizeP[i]),2) + sep $
             + strTrim(string(intersectionSizeP[i] * xyzVoxelSize),2) + sep $
             + strTrim(string(100 * intersectionFractionP[i]),2) + sep $
             + strTrim(string(intersectionRegionCount[i]),2) + sep $
             + strTrim(string(intersectionObjectCount[i]),2) + sep $
             + strTrim(string(intersectionSize[i]),2) + sep $
             + strTrim(string(intersectionSize[i] * xyzVoxelSize),2) + sep $
             + strTrim(string(100 * intersectionFraction[i]),2)
      if (fLogToFile eq 1) then fileLogger, logMsg, logDataFileName
    endfor
    if whParamActive[0] then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[*] = intersectionObjectCount
;    if whParamActive[5] then (*(*(*self.pValueStruct)[whParam[5]]).pROIParamVect)[*] = intersectionRegionCount
;    if whParamActive[7] then (*(*(*self.pValueStruct)[whParam[7]]).pROIParamVect)[*] = intersectionSize
;    if whParamActive[8] then (*(*(*self.pValueStruct)[whParam[8]]).pROIParamVect)[*] = intersectionSize * xyzVoxelSize
;    if whParamActive[9] then (*(*(*self.pValueStruct)[whParam[9]]).pROIParamVect)[*] = intersectionFraction
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = -1.0
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[i] = -1.0
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = -1.0
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[i] = -1.0
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[i] = -1.0
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[5]]).pROIParamVect)[i] = intersectionRegionCount[i]
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[6]]).pROIParamVect)[i] = intersectionObjectCount[i]
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[7]]).pROIParamVect)[i] = intersectionSize[i]
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[8]]).pROIParamVect)[i] = intersectionSize[i] * xyzVoxelSize
;    for i = 0L, nObjects-1 do (*(*(*self.pValueStruct)[whParam[9]]).pROIParamVect)[i] = intersectionFraction[i]
  endif

end


function C_sROIParam_3DObjOverlap::init

  paramName = '3D Object Overlap'
  paramType = '3D ROI-Parameter-Method'
  subParamNames = self->getParamNames()

  s_makeDefaultROIParamStruct, paramName,$
                               paramType,$
                               subParamNames,$
                               make_array(n_elements(subParamNames), /string, value = paramType),$
                               pParamStruct = pPS,$
                               pValueStruct = pVS
  self.pParamStruct = pPS
  self.pValueStruct = pVS
  return, 1
end


pro C_sROIParam_3DObjOverlap__define
  tmp = {C_sROIParam_3DObjOverlap, pParamStruct: ptr_new(),$
                                   pValueStruct: ptr_new(),$
                                   inherits C_sROIParam}
end
