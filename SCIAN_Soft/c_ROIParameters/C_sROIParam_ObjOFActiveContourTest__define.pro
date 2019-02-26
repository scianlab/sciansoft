; ____________________ (this is art) IOISIOI (this is art) ____________________
; NAME:
;    C_sROIParam_ObjOFActiveContourTest
;
; PURPOSE:
;    - Encapsulation for 2D optical flow / active contours segmentation and 
;      testing against defined ground-truth segmentations.
;
; AUTHOR:
;    Jorge Jara (2012)
;    e_mail: jjaraw@gmail.com
;
; CALLING SEQUENCE:
;    result = obj_new('C_sROIParam_ObjOFActiveContourTest')
;
; METHOHDS:
;
; DEPENDENCIES:
;    C_sActiveContour__Define
;    Optical flow functions (s_opticalFlow.pro)
;
; ____________________ (this is art) IOISIOI (this is art) ____________________

pro C_sROIParam_ObjOFActiveContourTest::apply, C_sROIGroupObj = C_sROIGroupObj, stack_tlb = stack_tlb

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
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
  logBaseName = 'c:\RSI\log_C_sROIParam_ObjOFActiveContourTest_' + strStartDate + '_' + ROIgroupStr
  sep = ';'

    ; Setup data log 
  logFileName = logBaseName + logExt
  fileLogger, 'INFO: started at ' + strDate, logFileName
    ; Hard-data log file with a header for the coeffs. (columms)
  logDataFileName = logBaseName + '_data' + logExt
  fileLogger, 'Hausdorff dist. [pix]' + sep + 'Hausdorff dist. [um]' + sep + 'Dice coeff.' + sep + 'Jaccard coeff.' + sep + 'ROI-ROI intersections', logDataFileName

  whParam = [(where(*(*self.pParamStruct).pNames eq 'Object AC-Perimeter'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Curvature-Circular'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Curvature-Derivative'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Mean-Curv-Circular'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Mean-Curv-Derivative'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Inflection-Points'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Mean-Inflection-Points'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Average-Gradient-X'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Average-Gradient-Y'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Curv-Circular_Perimeter'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-Curv-Derivative_Perimeter'))[0],$
             (where(*(*self.pParamStruct).pNames eq 'Object AC-P²A'))[0]]

    ; check Active Parameter
  whParamActive = whParam * 0
  case (n_elements(position) gt 0) of
    1   : if (position[0] eq -1) then return else whParamActive[position] = 1
    else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
  endcase

    ; check Pointers
  wherePA = where(whParamActive eq 1)
  if ~(ptr_valid((*self.pParamStruct).pROINumberVect)) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
  if (wherePA[0] eq -1) then return
  for i = 0, n_elements(wherePA)-1 do $
    if ~ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

  nObjects = C_sROIGroupObj->count()
  if (nObjects lt 1) then begin
    *(*self.pParamStruct).pROINumberVect = -1
    for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    fileLogger, 'INFO: no ROI objects found (returning).', logFileName
    fileLogger, 'INFO: no ROI objects found (returning).', logDataFileName
    return
  endif else begin
    ; Make arrays to indicate if a given ROI is available from one time to another
    baseROINum = make_array(nObjects, /int)
    for i = 0L, nObjects-1 do $
      baseROINum[i] = (C_sROIGroupObj->get(position = i))->getNumber()
    roiAvailable = baseROINum
  endelse

    ; set Object Number Vector
  *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

  if (total(whParamActive[*]) lt 1) then begin
    fileLogger, 'INFO: no ROI parameters active for computation (returning).', logFileName
    return
  endif

  pParamStruct = C_sROIGroupObj->getpParamStruct()

  tPos    = *(((*pParamStruct).pValues)[(where(*(*pParamStruct).pNames eq 'Time Position'))[0]])
  chPos   = *(((*pParamStruct).pValues)[(where(*(*pParamStruct).pNames eq 'Channel Position'))[0]])
  zPos    = *(((*pParamStruct).pValues)[(where(*(*pParamStruct).pNames eq 'z-Slice Position'))[0]])
  ; XXX [JJ] clusPos is not an atribute in C_sROIGroupObj class (2012.08)! Workaround: get it from s_ISM_getProjectInfo 
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, clusPos = clusPos

  whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [pixel]'))[0]
  xDimPix  = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [pixel]'))[0]
  yDimPix  = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [real]'))[0]
  xDimReal = *(((*pParamStruct).pValues)[whereDim]) > 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [real]'))[0]
  yDimReal = *(((*pParamStruct).pValues)[whereDim])
  xPixelSize = xDimReal / xDimPix
  yPixelSize = yDimReal / yDimPix

  acAlpha = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Alpha'))[0]]
  acBeta  = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Beta'))[0]]
  acGamma = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Gamma'))[0]]
  acKappa = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Kappa'))[0]]
  acMu    = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Mu'))[0]]
  acSnakeIter = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Snake_Iterations'))[0]]
  acGVFiter   = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'GVF_Iterations'))[0]]
  acPerimeterFactor = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Perimeter_Factor'))[0]]

;  fKeepGVF = (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where(*(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Keep_GGVF'))[0]] eq 1
;  if (~fKeepGVF) then begin
;    if ptr_valid((*self.pParamStruct).pSnake) then begin
;      if obj_valid(*(*self.pParamStruct).pSnake) then obj_destroy, *(*self.pParamStruct).pSnake
;      ptr_free, (*self.pParamStruct).pSnake
;    endif
;  endif

  fUseDefaultTZ = 1b
  fUseDefaultCh = 1b

    ; Time window and intervals
  tPosCur     = tPos
  tStep       = 1
  totalTsteps = 1
  tPosNext    = tPos + tStep

    ; Input image for active contour external force
  acImageZPos    = (fUseDefaultTZ eq 1) ? zPos : 0
  acImageTPos    = (fUseDefaultTZ eq 1) ? tPos : 0
  acImageChPos   = (fUseDefaultCh eq 1) ? chPos : 0
  acImageClusPos = 0
  acImageSegPos  = 0; TODO careful with this hard-coded value
  acImageTPosNext = acImageTPos + tStep ; Maybe we'll need it different at some "time" ;)

    ; Input image for active contour initial contours (binary masks)
  acMaskZPos    = (fUseDefaultTZ eq 1) ? zPos : 0
  acMaskTPos    = (fUseDefaultTZ eq 1) ? tPos : 0
  acMaskChPos   = (fUseDefaultCh eq 1) ? chPos : 0
  acMaskClusPos = 0
  acMaskSegPos  = 1

    ; Input reference/control segmentation image. We could also assume it is located in a folder or read some ROI group...
  controlZPos    = (fUseDefaultTZ eq 1) ? zPos : 0
  controlTPos    = (fUseDefaultTZ eq 1) ? tPos : 0
  controlChPos   = (fUseDefaultCh eq 1) ? chPos : 0
  controlClusPos = chPos

  fUseSavedMasks = 1b ; not used yet
  fUseSavedROIs  = 1b ; not used yet
  fCompareExternaloundariesOnly = 1b ; In case of multiple polygon boundaries (ROIs with holes, for instance). For now just 1 :)

    ; New parameters for AC,OF... must to create some room for them, as well as the OF and a long etc.
  fDisplayWindows = 1b
  fShowGVFWindow  = 0b
  fShowACWindow   = 1b
  fShowOFWindow   = 0b
  winSnake     = 1
  winSnakeProj = 2
  winControl   = 3
  winOF        = 4
  winGVF       = 5
  winCtrlFill  = 6
  winSnakeFill = 7 
  winCtrlFillAll  = 8
  winSnakeFillAll = 9
  fSaveContourImages = 1b

  minSnakePtsCount = 16
  acGVFmethod     = 1 ; 0->GVF, 1->GGVF, 2->EP-GGVF
  epgvfNoiseCut   = 0.10d
  epgvfNoiseRange = 0.05d
  epgvfError = 0.01d
  epgvfStep  = 0.01d
  sigmaSq = 0

    ; Optical flow parameters
  ofAlpha  = 20.0
  ofMethod = 3           ; 0-> HS, 1-> LK, 2-> CLG, 3-> Multiscale
  ofMultiscaleMethod = 1 ; 0-> HS, 1-> CLG, 2->LK
  ofIterations       = 200
  ofScales           = 4
  ofRhoCLG           = 3.0
  ofSigmaCLG         = 0.0
  ofTauLK            = 1.0

    ; Contour projection parameters
  contourProjectionMethod = 1 ; 0->no projection, 1->OF interpolation, 2->OF neighborhood averaging
  ofAvgRadius             = 4 ; square neighborhood size for case 2 of contourProjectionMethod

    ; output data folder name
  slash = path_sep()

  fileLogger, 'INFO: parameters of this run... ', logFileName
  strAcAlpha = 'acAlpha: '     + strTrim(string(acAlpha), 2)
  fileLogger, strAcAlpha, logFileName
  strAcBeta  = 'acBeta: '      + strTrim(string(acBeta), 2)
  fileLogger, strAcBeta, logFileName
  strAcGamma = 'acGamma: '     + strTrim(string(acGamma), 2)
  fileLogger, strAcGamma, logFileName
  strAcKappa = 'acKappa: '     + strTrim(string(acKappa), 2)
  fileLogger, strAcKappa, logFileName
  strAcIter  = 'acIter: '      + strTrim(string(acSnakeIter), 2)
  fileLogger, strAcIter, logFileName
  strGVFiter = 'acGVFiter: '   + strTrim(string(acGVFiter), 2)
  fileLogger, strGVFiter, logFileName
  strGVF     = 'acGVFmethod: ' + (acGVFmethod eq 0 ? 'GVF' : (acGVFmethod eq 1 ? 'GGVF' : (acGVFmethod eq 2 ? 'EPGGVF' : 'INVALIDCODE')))
  fileLogger, strGVF, logFileName
  strMu      = 'acMu: '        + strTrim(string(acMu), 2)            ; GVF/GGVF
  fileLogger, strMu, logFileName

  strEPGVFst = 'EPGVFStep: '   + strTrim(string(epgvfStep), 2)       ; EPGGVF
  fileLogger, strEPGVFst, logFileName
  strEPGVFer = 'EPGVFerror: '  + strTrim(string(epgvfError), 2)      ; EPGGVF
  fileLogger, strEPGVFer, logFileName
  strEPGVFnc = 'EPGVFncut: '   + strTrim(string(epgvfNoiseCut), 2)   ; EPGGVF
  fileLogger, strEPGVFnc, logFileName
  strEPGVFnr = 'EPGVFnrange: ' + strTrim(string(epgvfNoiseRange), 2) ; EPGGVF
  fileLogger, strEPGVFnr, logFileName
  strEPGVFsg = 'EPGVFssq: '    + strTrim(string(sigmaSq), 2)         ; EPGGVF
  fileLogger, strEPGVFsg, logFileName

  strOF      = 'ofMethod: '    + (ofMethod eq 0 ? 'HS' : (ofMethod eq 1 ? 'LK' : (ofMethod eq 2 ? 'CLG' : 'INVALIDCODE')))
  fileLogger, strOF, logFileName
  strOFscale = 'ofScales: '    + strTrim(string(ofScales), 2)
  fileLogger, strOFscale, logFileName

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
  widget_control, stack_tlb, set_uValue = stackState, /no_copy

  snakeXcoords     = ptrArr(1+totalTsteps, nObjects)
  snakeYcoords     = ptrArr(1+totalTsteps, nObjects)
  snakeProjXcoords = ptrArr(1+totalTsteps, nObjects) ; first and last positions will not be used
  snakeProjYcoords = ptrArr(1+totalTsteps, nObjects) ; first and last positions will not be used
  controlXcoords   = ptrArr(1+totalTsteps, nObjects)
  controlYcoords   = ptrArr(1+totalTsteps, nObjects)
  diceCoeff = dblArr(1+totalTsteps, nObjects)
  jaccCoeff = dblArr(1+totalTsteps, nObjects)
  sensCoeff = dblArr(1+totalTsteps, nObjects)
  specCoeff = dblArr(1+totalTsteps, nObjects)
  hausDist  = dblArr(1+totalTsteps, nObjects)

    ; Initialize contours for the first ("0") time frame in two steps...
    ; First the intensity image
  if (acImageSegPos ge 0) then begin ; and assuming that acImageClusPos ge 0...
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
      oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = acImageTPos, chPos = acImageChPos, zPos = acImageZPos)
      intImage0 = oImage->applyImageSegmentation(selectedStackObject = (*stackState.pImageStackInfoObject), stack_tlb = stack_tlb,$
                                               tPos    = acImageTPos,$
                                               zPos    = acImageZPos,$
                                               chPos   = acImageChPos,$
                                               clusPos = acImageClusPos,$
                                               segPos  = acImageSegPos)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
  endif else begin
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
      intImage0 = (*stateStack.pImageStackInfoObject)->getSelectedImage(tPos = acImageTPos, chPos = acImageChPos, zPos = acImageZPos)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
  endelse

  imgSize  = size(intImage0, /DIMENSIONS)
  maxSz = imgSize[0] > imgSize[1]
  minSz = imgSize[0] < imgSize[1]
  winFact  = 1.0
  if (maxSz lt 512) and (minSz lt 512) then winFact = 2.0
  if (maxSz lt 512) and (minSz lt 256) then winFact = 3.0
  if (maxSz lt 256) and (minSz lt 256) then winFact = 4.0
  if (maxSz lt 128) and (minSz lt 128) then winFact = 5.0
  xWinSize = round(imgSize[0] * winFact)
  yWinSize = round(imgSize[1] * winFact)
;nObjects = 6; TODO hard-coded mod
  for i = 0L, nObjects-1 do begin
      ; Get boundary polygon(s) for each ROI. Polygon 0 is the outer boundary, while the remaining (if any) are holes.
    pBorderPolygon = (C_sROIGroupObj->get(position = i))->getpObjectBorderPolygonList()
      ; adjust contours for object (*pBorderPolygon[j = 0]) and holes (*pBorderPolygon[j > 0])
    nPolygons = (fCompareExternaloundariesOnly eq 1) ? 1 : (size(pBorderPolygon, /DIMENSIONS))[0]
    snakeXcoords[0,i]   = ptr_new(ptrArr(nPolygons))
    snakeYcoords[0,i]   = ptr_new(ptrArr(nPolygons))
    controlXcoords[0,i] = ptr_new(ptrArr(nPolygons))
    controlYcoords[0,i] = ptr_new(ptrArr(nPolygons))
    snakeProjXcoords[0,i] = ptr_new(ptrArr(nPolygons))
    snakeProjYcoords[0,i] = ptr_new(ptrArr(nPolygons))

    for j = 0, nPolygons-1 do begin
      xCoords = transpose((*pBorderPolygon[j])[0,*])
      yCoords = transpose((*pBorderPolygon[j])[1,*])
      (*(controlXcoords[0,i]))[j] = ptr_new(xCoords)
      (*(controlYcoords[0,i]))[j] = ptr_new(yCoords)
      (*(snakeProjXcoords[0,i]))[j] = ptr_new(xCoords, /NO_COPY)
      (*(snakeProjYcoords[0,i]))[j] = ptr_new(yCoords, /NO_COPY)
   endfor
  endfor


    ; MAIN LOOP OVER TIME, 0 stands for the initial time frame
  for t = 0, totalTsteps do begin

    fileLogger, 't' + sep + strTrim(string(t),2) + sep + 'tPosCur' + sep + strTrim(string(tPosCur),2), logDataFileName
      ; Get intensity images
    if (acImageSegPos ge 0) then begin
      widget_control, stack_tlb, get_uValue = stackState, /no_copy
        oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = acImageTPos, chPos = acImageChPos, zPos = acImageZPos)
        imageCur = oImage->applyImageSegmentation(selectedStackObject = (*stackState.pImageStackInfoObject), stack_tlb = stack_tlb,$
                                                  tPos    = acImageTPos,$
                                                  zPos    = acImageZPos,$
                                                  chPos   = acImageChPos,$
                                                  clusPos = acImageClusPos,$
                                                  segPos  = acImageSegPos)
        if (t lt totalTsteps) then $
        imageNext = oImage->applyImageSegmentation(selectedStackObject = (*stackState.pImageStackInfoObject), stack_tlb = stack_tlb,$
                                                   tPos    = acImageTPosNext,$
                                                   zPos    = acImageZPos,$
                                                   chPos   = acImageChPos,$
                                                   clusPos = acImageClusPos,$
                                                   segPos  = acImageSegPos)
      widget_control, stack_tlb, set_uValue = stackState, /no_copy
    endif else begin
      widget_control, stack_tlb, get_uValue = stateStack, /no_copy
        imageCur = (*stateStack.pImageStackInfoObject)->getSelectedImage(tPos = tPosCur, chPos = acImageChPos, zPos = acImageZPos)
        if (t lt totalTsteps) then $
        imageNext = (*stateStack.pImageStackInfoObject)->getSelectedImage(tPos = tPosNext, chPos = acImageChPos, zPos = acImageZPos)
      widget_control, stack_tlb, set_uValue = stateStack, /no_copy
    endelse

      ; Perform segmentation
    oSnake = obj_new('C_sActiveContour', imageCur, [-1], [-1], alpha = acAlpha, beta = acBeta, gamma = acGamma, kappa = acKappa, iterations = acSnakeIter, gvf_iterations = acGVFiter, mu = acMu)
    case acGVFmethod of
      0: oSnake->calcGVF
      1: oSnake->calcGGVF
      2: oSnake->calcEPGVF, iterations = acGVFiter, step = epgvfStep, errorTol = epgvfError, noiseCut = epgvfNoiseCut, noiseRange = epgvfNoiseRange, edgeMapSigmaSq = sigmaSq
    endcase
    if (fDisplayWindows eq 1) then begin
      pGGVF = oSnake->getpGGVF()
      vfU = *pGGVF.pU
      vfV = *pGGVF.pV
    endif 

    for i = 0L, nObjects-1 do begin

        ; Get boundary polygon(s) for each ROI. Polygon 0 is the outer boundary, while the remaining (if any) are holes.
      nPolygons = (fCompareExternaloundariesOnly eq 1) ? 1 : n_elements(*(snakeProjXcoords[t,i]))
      for j = 0, nPolygons-1 do begin
        xCoords = *((*(snakeProjXcoords[t,i]))[j])
        yCoords = *((*(snakeProjYcoords[t,i]))[j])
          ; get the initial segmentation
        oSnake->setContour, xCoords, yCoords
        npts = round(oSnake->getPerimeter() * acPerimeterFactor) > minSnakePtsCount
        oSnake->arcSample, points = npts
        segmentedContour = oSnake->adjustContour()
        (*(snakeXcoords[t,i]))[j] = ptr_new(segmentedContour.x, /NO_COPY)
        (*(snakeYcoords[t,i]))[j] = ptr_new(segmentedContour.y, /NO_COPY)
      endfor
    endfor
    obj_destroy, oSnake

      ; Get the reference segmentation (control).
    oROIgroupCtrl = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb,$
                                          tPos      = tPosCur,$
                                          chPos     = controlChPos,$
                                          zPos      = controlZPos,$
                                          clusPos   = controlClusPos,$
                                          fileName  = fileName)

;    for i = 0, nObjects-1 do $
;      roiAvailable[i] = getROIobj(oROIgroupCtrl, baseROINum[i])

    if (t gt 0) then for i = 0L, nObjects-1 do begin
      oControlROI = getROIobjByNumber(oROIgroupCtrl, baseROINum[i])
      roiAvailable[i] = obj_valid(oControlROI) ? oControlROI->getNumber() : -1
      if ~obj_valid(oControlROI) then continue
      ;if (roiAvailable[i] eq -1) then continue
      ;oControlROI = oROIgroupCtrl->get(position = roiAvailable[i])
      ;oControlROI = oROIgroupCtrl->get(position = i)

      pBorderPolygonCtrl = oControlROI->getpObjectBorderPolygonList()
      nPolygonsCtrl      = (fCompareExternaloundariesOnly eq 1) ? 1 : (size(pBorderPolygonCtrl, /dim))[0]
      controlXcoords[t,i] = ptr_new(ptrArr(nPolygonsCtrl))
      controlYcoords[t,i] = ptr_new(ptrArr(nPolygonsCtrl))
      for j = 0, nPolygonsCtrl-1 do begin
        xCoords = transpose((*pBorderPolygonCtrl[j])[0,*])
        yCoords = transpose((*pBorderPolygonCtrl[j])[1,*])
        ptr_free, pBorderPolygonCtrl[j]
        ;iPlot, [xCoords, xCoords[0]], [yCoords, yCoords[0]], THICK = 2, SYM_INDEX = 2, SYM_SIZE = 0.1, SYM_THICK = 3, /NO_SAVEPROMPT
        (*(controlXcoords[t,i]))[j] = ptr_new(xCoords, /NO_COPY)
        (*(controlYcoords[t,i]))[j] = ptr_new(yCoords, /NO_COPY)
      endfor
      obj_destroy, oControlROI
    endfor
    obj_destroy, oROIgroupCtrl

      ; Compare reference against obtained segmentations
      ; TODO only the external boundary is being used.
    for i = 0L, nObjects-1 do begin
      if (roiAvailable[i] eq -1) then begin
        hd = -1
        dc = -1
        jc = -1
        sens = -1
        goto, lg
      endif
      j = 0
      ; Dice coefficient
      ctrlX = *((*(controlXcoords[t,i]))[j])
      ctrlY = *((*(controlYcoords[t,i]))[j])
      snakeX = *((*(snakeXcoords[t,i]))[j])
      snakeY = *((*(snakeYcoords[t,i]))[j])
      dc = calcDiceCoefficientForPolygons(ctrlX, ctrlY, snakeX, snakeY)

      ; TODO DLL... Jaccard similarity
      jc = calcJaccardSimCoefficientForPolygons(ctrlX, ctrlY, snakeX, snakeY)

      ; Sensitivity/Specificity
      xPoly = *((*controlXcoords[t,i])[j])
      yPoly = *((*controlYcoords[t,i])[j])
      window, winCtrlFill, xPos = 0, yPos = 0, title = 'ROI-sensitivity image -control at time ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize, retain = 2 ; avoid strange behaviours with this
      wset, winCtrlFill
      polyFill, [xPoly, xPoly[0]], [yPoly, yPoly[0]]
      rasterControlRoi = tvrd()
      xPoly = *((*snakeXcoords[t,i])[j])
      yPoly = *((*snakeYcoords[t,i])[j])
      window, winSnakeFill, xPos = 0, yPos = 0, title = 'ROI-sensitivity image -snake at time ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize, retain = 2 ; avoid strange behaviours with this
      wset, winSnakeFill
      polyFill, [xPoly, xPoly[0]], [yPoly, yPoly[0]]
      rasterSnakeRoi = tvrd()
      whereTProi = where((rasterControlRoi and rasterSnakeRoi) ne 0, tpRoiPixCount)
      whereGTroi = where(rasterControlRoi ne 0, gtRoiPixCount)
      sens = tpRoiPixCount / (gtRoiPixCount)

      ; Hausdorff distance
      hd = s_HausdorffDistance2Dpoints([transpose(ctrlX), transpose(ctrlY)],$
                                       [transpose(snakeX), transpose(snakeY)])
lg:   ctrlX = -1
      ctrlY = -1
      snakeX = -1
      snakeY = -1
      hausDist[t,i]  = hd
      jaccCoeff[t,i] = jc
      diceCoeff[t,i] = dc
      logMsg = strTrim(string(hd),2) + sep + strTrim(string(xPixelSize * hd),2) + sep + strTrim(string(dc),2) + sep + strTrim(string(jc),2) + sep + strTrim(string(sens),2)
      fileLogger, logMsg, logDataFileName
    endfor

    if (t lt totalTsteps) then $
      ; Compute OF
    case ofMethod of
      0: opticalFlowHS,  imageCur, imageNext, ofU, ofV, alpha = ofAlpha, iterations = ofIterations
      1: opticalFlowLK,  imageCur, imageNext, ofU, ofV, tau = ofTauLK, iterations = ofIterations
      2: opticalFlowCLG, imageCur, imageNext, ofU, ofV, alpha = ofAlpha, iterations = ofIterations, rho = ofRhoCLG, sigma = ofSigmaCLG
      3: opticalFlowMultiscale, imageCur, imageNext, ofScales, ofMultiscaleMethod, ofU, ofV, alpha = ofAlpha, iterations = ofIterations, tau = ofTauLK
    endcase

    if (fDisplayWindows eq 1) then begin
      i = 0
      j = 0 ; TODO forced to only plot external boundaries... should be enough for now... :)
      if (roiAvailable[0] ne -1) then begin
        window, winControl, xPos = 0, yPos = 0, title = 'Control segmentation at time ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize
        xPoly = *((*controlXcoords[t,i])[j])
        yPoly = *((*controlYcoords[t,i])[j])
        plot, [xPoly, xPoly[0]], [yPoly, yPoly[0]], xMargin = 0, yMargin = 0, xRange = [0, imgSize[0]-1], yRange = [0, imgSize[1]-1], xStyle = 1, yStyle = 1
        window, winCtrlFillAll, xPos = 0, yPos = 0, title = 'Control fill image at time ' + strTrim(string(tPosCur), 2), xSize = imgSize[0], ySize = imgSize[1], retain = 2 ; avoid strange behaviours with this
        wset, winCtrlFillAll
        polyFill, [xPoly, xPoly[0]], [yPoly, yPoly[0]]

        window, winSnake, xPos = 0, yPos = 0, title = '2D contour segmentation at time ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize
        xPoly = *((*snakeXcoords[t,i])[j])
        yPoly = *((*snakeYcoords[t,i])[j])
        plot, [xPoly, xPoly[0]], [yPoly, yPoly[0]], xMargin = 0, yMargin = 0, xRange = [0, imgSize[0]-1], yRange = [0, imgSize[1]-1], xStyle = 1, yStyle = 1
        window, winSnakeFillAll, xPos = 0, yPos = 0, title = 'Snake fill image at time ' + strTrim(string(tPosCur), 2), xSize = imgSize[0], ySize = imgSize[1], retain = 2 ; avoid strange behaviours with this
        wset, winSnakeFillAll
        polyFill, [xPoly, xPoly[0]], [yPoly, yPoly[0]]

        window, winSnakeProj, xPos = 0, yPos = 0, title = '2D OF-projected contour at time ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize
        xPoly = *((*snakeProjXcoords[t,i])[j])
        yPoly = *((*snakeProjYcoords[t,i])[j])
        plot, [xPoly, xPoly[0]], [yPoly, yPoly[0]], xMargin = 0, yMargin = 0, xRange = [0, imgSize[0]-1], yRange = [0, imgSize[1]-1], xStyle = 1, yStyle = 1

      endif else begin
        mockX = [0,0]
        mockY = [0,0]
        window, winControl, xPos = 0, yPos = 0, title = 'Control segmentation at t ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize
        plot, mockX, mockY, xMargin = 0, yMargin = 0, xRange = [0, imgSize[0]-1], yRange = [0, imgSize[1]-1], xStyle = 1, yStyle = 1
        window, winCtrlFillAll, title = 'Control fill image', xSize = imgSize[0], ySize = imgSize[1], retain = 2 ; avoid strange behaviours with this
        wset, winCtrlFillAll
        polyFill, mockX, mockY
        window, winSnake, xPos = 0, yPos = 0, title = '2D contour segmentation at t ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize
        plot, mockX, mockY, xMargin = 0, yMargin = 0, xRange = [0, imgSize[0]-1], yRange = [0, imgSize[1]-1], xStyle = 1, yStyle = 1
        window, winSnakeFillAll, xPos = 0, yPos = 0, title = 'Snake fill image at t ' + strTrim(string(tPosCur), 2), xSize = imgSize[0], ySize = imgSize[1], retain = 2 ; avoid strange behaviours with this
        wset, winSnakeFillAll
        polyFill, mockX, mockY
        window, winSnakeProj, xPos = 0, yPos = 0, title = '2D OF-projected contour, t ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize
        plot, mockX, mockY, xMargin = 0, yMargin = 0, xRange = [0, imgSize[0]-1], yRange = [0, imgSize[1]-1], xStyle = 1, yStyle = 1
      endelse

      if (t lt totalTsteps) and (fShowOFWindow eq 1) then begin
        ;window, winOF, xPos = 0, yPos = 0, title = 'Optical flow field, t ' + strTrim(string(tPosCur), 2) + '-' + strTrim(string(tPosCur+tStep), 2), xSize = xWinSize, ySize = yWinSize
        ;velovect, ofU, ofV, xMargin = 0, yMargin = 0, xRange = [0, imgSize[0]-1], yRange = [0, imgSize[1]-1], xStyle = 1, yStyle = 1
        opticalFlowShowOF, ofU, ofV, image1 = imageCur
      endif

      if (fShowGVFWindow eq 1) then begin
        window, winGVF, xPos = 0, yPos = 0, title = 'GVF field at t ' + strTrim(string(tPosCur), 2), xSize = xWinSize, ySize = yWinSize
        velovect, vfU, vfV, xMargin = 0, yMargin = 0, xRange = [0, imgSize[0]-1], yRange = [0, imgSize[1]-1], xStyle = 1, yStyle = 1
      endif

      if (nObjects gt 1) then for i = 1L, nObjects-1 do begin
        if (roiAvailable[i] eq -1) then continue

        wset, winControl
        xPoly = *((*controlXcoords[t,i])[j])
        yPoly = *((*controlYcoords[t,i])[j])
        oPlot, [xPoly, xPoly[0]], [yPoly, yPoly[0]]
        wset, winCtrlFillAll
        polyFill, [xPoly, xPoly[0]], [yPoly, yPoly[0]]

        wset, winSnake
        xPoly = *((*snakeXcoords[t,i])[j])
        yPoly = *((*snakeYcoords[t,i])[j])
        oPlot, [xPoly, xPoly[0]], [yPoly, yPoly[0]]
        wset, winSnakeFillAll
        polyFill, [xPoly, xPoly[0]], [yPoly, yPoly[0]]

        wset, winSnakeProj
        xPoly = *((*snakeProjXcoords[t,i])[j])
        yPoly = *((*snakeProjYcoords[t,i])[j])
        oPlot, [xPoly, xPoly[0]], [yPoly, yPoly[0]]
      endfor

      if (fSaveContourImages eq 1) then begin
        dumIrgb = bytArr(3, imgSize[0], imgSize[1])
        wSet, winSnakeProj
        dumSnakeProj = tvrd()
        dumIrgb[2,*,*] = dumSnakeProj
        write_tiff, 'c:\RSI\contourSnakeProj_' + ROIgroupStr + '.tif', dumIrgb

        wSet, winSnake
        dumSnake = tvrd()
        dumIrgb[*] = 0
        dumIrgb[1,*,*] = dumSnake
        write_tiff, 'c:\RSI\contourSnake_' + ROIgroupStr + '.tif', dumIrgb

        wSet, winControl
        dumControl = tvrd()
        dumIrgb[*] = 0
        dumIrgb[0,*,*] = dumControl
        write_tiff, 'c:\RSI\contourControl_' + ROIgroupStr + '.tif', dumIrgb

        dumIrgb[*] = 0
        dumIrgb[0,*,*] = dumControl
        dumIrgb[1,*,*] = dumSnake
        write_tiff, 'c:\RSI\contourOvrSnakeControl_' + ROIgroupStr + '.tif', dumIrgb

      endif

      xPoly = -1
      yPoly = -1
      vfU = -1
      vfV = -1
    endif

    wset, winCtrlFillAll
    gtImage = tvrd()
    wset, winSnakeFillAll
    segImage = tvrd()

    whereTP = where((gtImage and segImage) ne 0, countTP)
    whereTN = where((gtImage or segImage) eq 0, countTN)
    whereGTP = where(gtImage ne 0, countGTP)
    whereGTN = where(gtImage eq 0, countGTN)
    countFP = countGTP - countTP
    countFN = countGTN - countTN
    sensCoeff = (1.0 * countTP) / (countTP + countFN)
    specCoeff = (1.0 * countTN) / (countTN + countFP)

    logMsg = 'total sensitivity' + sep + strTrim(string(sensCoeff),2) + sep + 'specificity' + sep + strTrim(string(specCoeff),2)
    fileLogger, logMsg, logDataFileName

    if (t lt totalTsteps) then $ 
    for i = 0L, nObjects-1 do begin

        ; Get boundary polygon(s) for each ROI. Polygon 0 is the outer boundary, while the remaining (if any) are holes.
      nPolygons = (fCompareExternaloundariesOnly eq 1) ? 1 : n_elements(*(snakeXcoords[t,i]))
      snakeProjXcoords[t+1,i] = ptr_new(ptrArr(nPolygons))
      snakeProjYcoords[t+1,i] = ptr_new(ptrArr(nPolygons))
      for j = 0, nPolygons-1 do begin
        xCoords = *((*snakeXcoords[t,i])[j])
        yCoords = *((*snakeYcoords[t,i])[j])
        ; project contours with OF vector field
        case contourProjectionMethod of
          0: print, 'WARNING: no projection has been made' ; Do nothing with the contours

          1: begin ; Interpolation of the OF field
             dx = interpolate(ofU, xCoords, yCoords, cubic = -0.5)
             dy = interpolate(ofV, xCoords, yCoords, cubic = -0.5)
             xCoords = xCoords + dx
             yCoords = yCoords + dy
             endcase

          2: begin ; Rounding of contour vertex positions, averaging of the OF field
             xr = round(xCoords)
             yr = round(yCoords)
             nVert = n_elements(xCoords)
             ofPosCount = 0
             dx = 0.0d
             dy = 0.0d
             for v = 0L, nVert-1 do begin
               ; OF interpolation
               ; Watch out with boundary cases.
               for ofx = -ofAvgRadius, ofAvgRadius do begin
                 px = xr[v] + ofx
                 if (px ge 0) and (px lt imgSize[0]) then $
                 for ofy = -ofAvgRadius, ofAvgRadius do begin
                   py = yr[v] + ofy
                   if (py ge 0) and (py lt imgSize[1]) then begin
                     dx += ofU[px, py]
                     dy += ofV[px, py]
                     ofPosCount += 1
                   endif
                 endfor
               endfor
               if (ofPosCount eq 0) then stop ; This should not happen... BUT...
               dx /= ofPosCount
               dy /= ofPosCount
               newX = xCoords[v] + dx
               newY = yCoords[v] + dy
               if (newX lt 0) or (newX gt (imgSize[0]-1)) or (newY lt 0) or (newY gt (imgSize[1]-1)) then begin
                 errMes = 'WARNING: avoided projection of 2D-contour for ROI ' + strTrim(string(i), 2) $
                        + ' at time ' + strTrim(string(t), 2) $
                        + ' (tPos '   + strTrim(string(tPosCur), 2) + ')' $
                        + ', vertex index ' + strTrim(string(v)) $
                        + ' from [' + strTrim(string(xCoords[v]), 2) + ',' + strTrim(string(yCoords[v]), 2) + ']' $
                        + ' to ['   + strTrim(string(newX), 2)       + ',' + strTrim(string(newY, 2)) + ']'
                 fileLogger, errMes, errorLogFileName
               endif
               xCoords[v] = (newX > 0) < (imgSize[0]-1)
               yCoords[v] = (newY > 0) < (imgSize[1]-1)
             endfor
             endcase
          else: stop
        endcase
        polyFixWinID = 27
        rasterizeAndRecoverPoly, xCoords, yCoords, imgSize[0], imgSize[1], polyFixWinID, xOutArr, yOutArr, nOut
        wDelete, polyFixWinID
        ;wSet, polyFixWinID
        ;plot, xCoords, yCoords
        ;oPlot, *xOutArr[0], *yOutArr[0]
        xCoordsRep = *xOutArr[0]
        yCoordsRep = *yOutArr[0]
        (*(snakeProjXcoords[t+1,i]))[j] = ptr_new(xCoordsRep, /NO_COPY)
        (*(snakeProjYcoords[t+1,i]))[j] = ptr_new(yCoordsRep, /NO_COPY)
      endfor

    endfor

      ; Now advance in time
    tPosCur += tStep
    tPosNext = tPosCur + tStep
    acImageTPos    += tStep
    acImageTPosNext = acImageTPos + tStep

    if (t lt totalTsteps) then for i = 0L, nObjects-1 do begin
      nPolygons = (fCompareExternaloundariesOnly eq 1) ? 1 : n_elements(*(snakeProjXcoords[t,i]))
      snakeXcoords[t+1,i]   = ptr_new(ptrArr(nPolygons))
      snakeYcoords[t+1,i]   = ptr_new(ptrArr(nPolygons))
      controlXcoords[t+1,i] = ptr_new(ptrArr(nPolygons))
      controlYcoords[t+1,i] = ptr_new(ptrArr(nPolygons))
    endfor
    stop
  endfor
  fileLogger, 'INFO: finished at ' + sysTime(), logFileName

  for i = 0, nObjects-1 do begin
    for t = 0, totalTsteps do begin ; Yes, without the "-1"
      nElem = n_elements(*(snakeProjXcoords[t,i]))
      for j = 0, nElem-1 do begin
        ptr_free, (*snakeProjXcoords[t,i])[j]
        ptr_free, (*snakeProjYcoords[t,i])[j]
      endfor
      ptr_free, snakeProjXcoords[t,i]
      ptr_free, snakeProjYcoords[t,i]

      nElem = n_elements(*(snakeXcoords[t,i]))
      for j = 0, nElem-1 do begin
        ptr_free, (*snakeXcoords[t,i])[j]
        ptr_free, (*snakeYcoords[t,i])[j]
      endfor
      ptr_free, snakeXcoords[t,i]
      ptr_free, snakeYcoords[t,i]

      if ~ptr_valid(controlXcoords[t,i]) then continue
      nElem = n_elements(*(controlXcoords[t,i]))
      for j = 0, nElem-1 do begin
        ptr_free, (*controlXcoords[t,i])[j]
        ptr_free, (*controlYcoords[t,i])[j]
      endfor
      ptr_free, controlXcoords[t,i]
      ptr_free, controlYcoords[t,i]
    endfor
  endfor
  snakeProjXcoords = -1
  snakeProjYcoords = -1
  snakeXcoords = -1
  snakeYcoords = -1
  controlXcoords = -1
  controlYcoords = -1

  return
  ; window, 31, xsize = xWinSize, ysize = yWinSize
  ; tvscl, congrid(intImage, xWinSize, yWinSize, /interp, /center)

  paramMatrix = make_array(n_elements(whParam), nObjects, /float)
    ; Arbitrary minimun point count for the contour polygons
  for i = 0, nObjects-1 do begin

    pObjectBorderPolygon = (C_sROIGroupObj->get(position = i))->getpObjectBorderPolygonList()
    (*(*self.pParamStruct).pSnake)->setContour, transpose((*pObjectBorderPolygon[0])[0,*]), transpose((*pObjectBorderPolygon[0])[1,*])
    npts = round(((*(*self.pParamStruct).pSnake)->getPerimeter()) * acPerimeterFactor) > minSnakePtsCount
    (*(*self.pParamStruct).pSnake)->arcSample, points = npts
    fContour = (*(*self.pParamStruct).pSnake)->adjustContour()
    xcoords =  (*(*self.pParamStruct).pSnake)->getXcoords()
    ycoords =  (*(*self.pParamStruct).pSnake)->getYcoords()
    ;plot, 0.5+xcoords, 0.5+ycoords, xstyle=1, ystyle=1, xrange=[0, dimI[0]], yrange=[0, dimI[1]], position = [0, 0, xWinSize, yWinSize], /noErase

      ; distance between i, i+1 along the snake
    pointDist = (*(*self.pParamStruct).pSnake)->getDistance(xyRes = [xPixelSize, yPixelSize])
    paramMatrix[0,i] = total(pointDist)
    minP = min(pointDist, max = maxP)
    ;plots, 0.5+xcoords, 0.5+ycoords, color = (pointDist-minP) / ((maxP-minP) / 255.)

      ; curvature along the snake with triangles
    pointCir3  = (*(*self.pParamStruct).pSnake)->getCir3(xyRes = [xPixelSize, yPixelSize])
    pointCurv3 = pointCir3 * 0.
    whereN0    = where(pointCir3 ne 0)
    if (whereN0[0] ne -1) then pointCurv3[whereN0] = 1. / pointCir3[whereN0]
    ;plots, 0.5+xcoords, 0.5+ycoords, color = (pointCurv3-min(pointCurv3)) / ((max(pointCurv3)-min(pointCurv3)) / 255.), thick = 2
    paramMatrix[ 1,i] = total(pointCurv3)

      ; curvature along the snake with derivatives
    pointCurvD = abs((*(*self.pParamStruct).pSnake)->getCurvD(xyRes = [xPixelSize, yPixelSize]))
    paramMatrix[ 2,i] = total(pointCurvD)
    ;plots, 0.5+xcoords, 0.5+ycoords, color = (pointCurvD-min(pointCurvD)) / ((max(pointCurvD)-min(pointCurvD)) / 255.), thick = 2

      ; normalized curvatures along the snake
    paramMatrix[ 3,i] = (moment(pointCurv3))[0]
    paramMatrix[ 4,i] = (moment(pointCurvD))[0]

      ; inflection points
    pointInflex = (*(*self.pParamStruct).pSnake)->getInflexPoints()
    whereGT0 = where(pointInflex gt 0, countGT0)
    paramMatrix[ 5,i] = countGT0

    if (countGT0 gt 0) then for j = 0, n_elements(whereGT0)-1 do $
      plots, [0.5 + (xcoords)[whereGT0[j]], 0.5 + (xcoords)[whereGT0[j]]],$
             [0.5 + (ycoords)[whereGT0[j]], 0.5 + (ycoords)[whereGT0[j]]],$
             psym = 1, color = [255,255,255], thick = 1.

    paramMatrix[ 6,i] = n_elements(whereGT0) / paramMatrix[0,i]
    paramMatrix[ 7,i] = 0.
    paramMatrix[ 8,i] = 0.
    paramMatrix[ 9,i] = paramMatrix[1,i] / paramMatrix[0,i]
    paramMatrix[10,i] = paramMatrix[2,i] / paramMatrix[0,i]

      ; perimeter^2 x area
    oRoi = obj_new('IDLanROI',xcoords * xPixelSize, ycoords * yPixelSize)
    result = oRoi->computeGeometry(area = thisArea)
    obj_destroy, oRoi
    paramMatrix[11,i] = paramMatrix[0,i]^2 / thisArea
  endfor

  ;if (~fKeepGVF) then begin
  ;  if ptr_valid((*self.pParamStruct).pSnake) then begin
  ;    if obj_valid(*(*self.pParamStruct).pSnake) then obj_destroy, *(*self.pParamStruct).pSnake
  ;    ptr_free, (*self.pParamStruct).pSnake
  ;  endif
  ;endif

  for i = 0, n_elements(whParam)-1 do $
    if whParamActive[i] then *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float) + paramMatrix[i,*]
end


function getROIobjByNumber, oROIgroup, lookupNumber
  nObj = oROIgroup->count()
  for i = 0L, nObj-1 do begin
    if (((oROIgroup->get(position = i))->getNumber()) eq lookupNumber) then begin
      print, 'ROI number ', lookupNumber, ' found at position ', i
      return, oROIgroup->get(position = i)
    endif
  endfor
  print, 'ROI number ', lookupNumber, ' not found'
  return, -1
end


pro rasterizeAndRecoverPoly, xCoords, yCoords, xSize, ySize, winID, xOutArr, yOutArr, nPolysOut
  window, winID, XSIZE = xSize, YSIZE = ySize, retain = 2 ; avoid strange behaviours with this
  wset, winID
  polyfill, xCoords, yCoords
  image = tvrd()
  ; Get blob indices:
  b = label_region(image)
  ; Get population and members of each blob:
  h = histogram(b, REVERSE_INDICES=r)
  ; Each region
  nPolysOut = n_elements(h) - 1 ; 0 stands for the background
  xOutArr = ptrArr(nPolysOut)
  yOutArr = ptrArr(nPolysOut)
  print, 'recovering ', nPolysOut, ' polygons'
  nPolysOut = 1
  for i = 1, nPolysOut do begin ; start from 1... 0 is background
    ;Find subscripts of members of region i.
    p = r[r[i]:r[i+1]-1]
    ; Pixels of region i
    q = image[p]
    print, 'Region ', i, ', Size = ', h[i]
    contour, b, LEVEL = i, PATH_INFO = pathInfo, PATH_XY = pathXY, /PATH_DATA_COORDS
    line = [LindGen(PathInfo(0).N), 0]
    xOutArr[i-1] = ptr_new(reform((pathXY(*, pathInfo(0).OFFSET + line))[0, *]))
    yOutArr[i-1] = ptr_new(reform((pathXY(*, pathInfo(0).OFFSET + line))[1, *]))
  endfor
end


function C_sROIParam_ObjOFActiveContourTest::init

  paramType      = 'Single ROI-Parameter-Method'   ; To be used several times...

  ROIParamStruct = {name: 'Object AC-Parameters',$ ; Parameters class Name.
                    type: paramType,$
                    pWidgetType   : ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                    pNames        : ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                    pActive       : ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin          : ptr_new(),$    ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax          : ptr_new(),$    ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues       : ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                    pSnake        : ptr_new(),$    ; Pointer on Snake-Object.
                    pROINumberVect: ptr_new()}     ; Pointer on ROI-Obj Number Vector.

  ROIParamNames = ['Object AC-Perimeter',$
                   'Object AC-Curvature-Circular',$
                   'Object AC-Curvature-Derivative',$
                   'Object AC-Mean-Curv-Circular',$
                   'Object AC-Mean-Curv-Derivative',$
                   'Object AC-Inflection-Points',$
                   'Object AC-Mean-Inflection-Points',$
                   'Object AC-Average-Gradient-X',$
                   'Object AC-Average-Gradient-Y',$
                   'Object AC-Curv-Circular_Perimeter',$
                   'Object AC-Curv-Derivative_Perimeter',$
                   'Object AC-P²A']

  ROIParamWidgetType = make_array(n_elements(ROIParamNames), /string, value = 'widget_slider')
  self.pValueStruct  = ptr_new(ptrArr(n_elements(ROIParamNames)))

  ROIParamActive = [1,1,1,1,1,1,1,1,1,1,1,1]
  ROIParamMin    = [0,0,0,0,0,0,0,0,0,0,0,0]
  ROIParamMax    = [0,0,0,0,0,0,0,0,0,0,0,0]
  ROIParamValues = [0,0,0,0,0,0,0,0,0,0,0,0]
  pROINumberVect = [-1]

  ROIParamStruct.pNames         = ptr_new(ROIParamNames) ; Without the NO_COPY flag on purpose
  ROIParamStruct.pWidgetType    = ptr_new(ROIParamWidgetType, /no_copy)
  ROIParamStruct.pActive        = ptr_new(ROIParamActive, /no_copy)
  ROIParamStruct.pMin           = ptr_new(ROIParamMin, /no_copy)
  ROIParamStruct.pMax           = ptr_new(ROIParamMax, /no_copy)
  ROIParamStruct.pValues        = ptr_new(ROIParamValues, /no_copy)
  ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
  self.pParamStruct             = ptr_new(ROIParamStruct, /no_copy)

  ROIValueStruct = {name         : ROIParamNames[0],$
                    type         : paramType,$
                    pWidgetType  : ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames       : ptr_new(),$ ; Pointer on ROI-Obj Parameter Names.
                    pActive      : ptr_new(),$ ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin         : ptr_new(),$ ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax         : ptr_new(),$ ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues      : ptr_new(),$ ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()}  ; Pointer on ROI-Obj Parameter Vector.

  ROIValueNames = ['Alpha',$
                   'Beta',$
                   'Gamma',$
                   'Kappa',$
                   'Mu',$
                   'Snake_Iterations',$
                   'GVF_Iterations',$
                   'Perimeter_Factor',$
                   'Keep_GGVF',$
                   'Threshold_1a','Threshold_1b',$
                   'Threshold_2a','Threshold_2b',$
                   'Threshold_3a','Threshold_3b',$
                   'Threshold_4a','Threshold_4b']

  ROIValueWidgetType = make_array(n_elements(ROIValueNames), /string, value = 'widget_slider')
  ROIValueActive     = [1b, 1b, 1b, 1b, 1b, 1b, 1b, 1b, 1b, 1b, 0b, 0b, 0b, 0b, 0b, 0b, 0b]
  ROIValueMin        = make_array(n_elements(ROIValueNames), /float) ; 0-valued array
  ROIValueMax        = [3.0, 3.0, 3.0, 3.0, 1.0 , 500, 500, 5., 1., 1., 1., 1., 1., 1., 1., 1., 1.]
  ROIValueValues     = [0.1, 0.8, 1.0, 1.0, 0.05, 20 , 100, 1. ,0., 0., 1., 0., 1., 0., 1., 0., 1.]
  pROIParamVect      = [-1]

  ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
  ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
  ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
  ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
  ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
  ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
  ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

  (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy) 

  for i = 1, n_elements(ROIParamNames)-1 do begin
    ROIValueStruct = s_makeDefaultROIValueStruct(ROIParamNames[i], paramType)
    (*self.pValueStruct)[i] = ptr_new(ROIValueStruct, /NO_COPY) ; It has to be made by using a named variable
  endfor
  return, 1
end


pro C_sROIParam_ObjOFActiveContourTest__define
  tmp = {C_sROIParam_ObjOFActiveContourTest,$
         pParamStruct: ptr_new(),$
         pValueStruct: ptr_new(),$
         inherits C_sROIParam}
end
