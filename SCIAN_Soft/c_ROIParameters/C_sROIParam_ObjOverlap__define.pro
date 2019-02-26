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
;     oParam = obj_new('C_sROIParam_ObjOverlap')
;
; METHOHDS
;     init, apply, getParamNames
;
; NOTES
;     Polygon computations are implemented at the moment only for ROIs without holes.
;
; ____________________ (this is art) IOISIOI (this is art) ____________________

function C_sROIParam_ObjOverlap::getParamNames
  return, ['Polygonal intersection areas [N]',$
           'Polygonal intersection ROIs [N]',$
           'Polygonal intersection size [pixel]',$
           'Polygonal intersection [x^2]',$
           'Polygonal intersection [%]',$
           'Pixel intersection areas [N]',$
           'Pixel intersection ROIs [N]',$
           'Pixel intersection size [pixel]',$
           'Pixel intersection size [x^2]',$
           'Pixel intersection [%]']
end


pro C_sROIParam_ObjOverlap::apply, C_sROIGroupObj = C_sROIGroupObj, position = position, stack_tlb = stack_tlb

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
  pParamStruct = C_sROIGroupObj->getpParamStruct()

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
  widget_control, stack_tlb, set_uValue = stackState, /no_copy

  imgPparamStruct = oImage->getpParamStruct()
  imageStr = *((*imgPparamStruct).pValues) [(where(*((*imgPparamStruct).pNames) eq 'Name'))[0]] + '_'

  baseName = 'oROI2DGroup_'
  clusStr  = 'Clus' + string(clusPos) + '_'
  chStr    = 'ch' + string(chPos) + '_'
  tStr     = 't' + string(tPos) + '_'
  zStr     = 'z' + string(zPos)
  ROIgroupStr = strCompress(baseName + imageStr + chStr + tStr + zStr, /REMOVE_ALL)

  fLogToFile = 1b
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
  logBaseName =  basePath + 'log_C_sROIParam_ObjOverlap_' + strStartDate + '_' + ROIgroupStr
  sep = ';'

    ; Hard-data log file with a header for the coeffs. (columms)
  logDataFileName = logBaseName + '_data' + logExt
  logHeader = ''

  paramNames = self->getParamNames()
  nParams = n_elements(paramNames)

  logHeader = strJoin(paramNames, sep)
  fileLogger, logHeader, logDataFileName

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

  nObjects = C_sROIGroupObj->count()
  if (nObjects lt 1) then begin
    *(*self.pParamStruct).pROINumberVect = -1
    for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    fileLogger, 'No ROIs', logDataFileName
    return
  endif

    ; set Object Number Vector
  *(*self.pParamStruct).pROINumberVect = (C_sROIGroupObj->getObjectNumberVector())
  whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [pixel]'))[0]
  xDimPix  = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [pixel]'))[0]
  yDimPix  = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [real]'))[0]
  xDimReal = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [real]'))[0]
  yDimReal = *(((*pParamStruct).pValues)[whereDim])

  xPixelSize  = xDimReal / xDimPix
  yPixelSize  = yDimReal / yDimPix
  xyPixelSize = xPixelSize * yPixelSize

    ; Check which kind of computations need to be performed
  fDoPolygonCalcs = whParamActive[0] or whParamActive[1] or whParamActive[2] or whParamActive[3] or whParamActive[4]
  fDoPixelCalcs   = whParamActive[5] or whParamActive[6] or whParamActive[7] or whParamActive[8] or whParamActive[9]

  if (fDoPolygonCalcs eq 1) then begin
    calcPolyOverlapInROIgroup, C_sROIGroupObj, intersectionRegionCount = intersectionRegionCountP,$
                                               intersectionObjectCount = intersectionObjectCountP,$
                                               intersectionSize = intersectionSizeP,$
                                               intersectionFraction = intersectionFractionP
    ;print, 'intersectionRegionCount', intersectionRegionCount
    ;print, 'intersectionObjectCount', intersectionObjectCount
    ;print, 'intersectionSize', intersectionSize
    ;print, 'intersectionFraction', intersectionFraction
    if whParamActive[0] then *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = intersectionRegionCountP
    if whParamActive[1] then *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = intersectionObjectCountP
    if whParamActive[2] then *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = intersectionSizeP
    if whParamActive[3] then *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = intersectionSizeP * xyPixelSize
    if whParamActive[4] then *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = intersectionFractionP
  endif

  if (fDoPixelCalcs eq 1) then begin
    calcROIoverlapInROIgroup, C_sROIGroupObj, intersectionRegionCount = intersectionRegionCount,$
                                              intersectionObjectCount = intersectionObjectCount,$
                                              intersectionSize = intersectionSize,$
                                              intersectionFraction = intersectionFraction
    ;print, 'intersectionRegionCount', intersectionRegionCount
    ;print, 'intersectionObjectCount', intersectionObjectCount
    ;print, 'intersectionSize', intersectionSize
    ;print, 'intersectionFraction', intersectionFraction
    if (fLogToFile eq 1) then for i = 0L, nObjects-1 do begin
      logMsg = strTrim(string(intersectionRegionCountP[i]),2) + sep $
             + strTrim(string(intersectionObjectCountP[i]),2) + sep $
             + strTrim(string(intersectionSizeP[i]),2) + sep $
             + strTrim(string(intersectionSizeP[i] * xyPixelSize),2) + sep $
             + strTrim(string(100 * intersectionFractionP[i]),2) + sep $
             + strTrim(string(intersectionRegionCount[i]),2) + sep $
             + strTrim(string(intersectionObjectCount[i]),2) + sep $
             + strTrim(string(intersectionSize[i]),2) + sep $
             + strTrim(string(intersectionSize[i] * xyPixelSize),2) + sep $
             + strTrim(string(100 * intersectionFraction[i]),2)
      fileLogger, logMsg, logDataFileName
    endfor
    if whParamActive[5] then *(*(*self.pValueStruct)[whParam[5]]).pROIParamVect = intersectionRegionCount
    if whParamActive[6] then *(*(*self.pValueStruct)[whParam[6]]).pROIParamVect = intersectionObjectCount
    if whParamActive[7] then *(*(*self.pValueStruct)[whParam[7]]).pROIParamVect = intersectionSize
    if whParamActive[8] then *(*(*self.pValueStruct)[whParam[8]]).pROIParamVect = intersectionSize * xyPixelSize
    if whParamActive[9] then *(*(*self.pValueStruct)[whParam[9]]).pROIParamVect = intersectionFraction
  endif

end


function C_sROIParam_ObjOverlap::init

  paramType  = 'Single ROI-Parameter-Method'
  paramNames = self->getParamNames()

  s_makeDefaultROIParamStruct, 'Object Intersection',$
                               paramType,$
                               paramNames,$
                               make_array(n_elements(paramNames), /string, value = paramType),$
                               pParamStruct = pPS,$
                               pValueStruct = pVS
  self.pParamStruct = pPS
  self.pValueStruct = pVS
  return, 1
end


pro C_sROIParam_ObjOverlap__define
  tmp = {C_sROIParam_ObjOverlap, pParamStruct: ptr_new(),$
                                 pValueStruct: ptr_new(),$
                                 inherits C_sROIParam}
end
