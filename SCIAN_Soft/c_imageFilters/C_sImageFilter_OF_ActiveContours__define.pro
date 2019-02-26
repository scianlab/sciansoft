; _____________________________ IOISIOI _____________________________ (this is art)
;
; NAME
; C_sImageFilter_OF_ActiveContours
;
; PURPOSE
; - Optical Flow projected Active Contours Filter Class.
;
; CALLING SEQUENCE
;
; result = obj_new('C_sImageFilter_OF_ActiveContours')
;
; METHODS
;
; function->apply, pImageData = pImageData ;pImageData Pointer on Image Data Matrix
; pro->set, pParamStruct = pParamStruct    ;pParamStruct Pointer on ImageFilterStruct-Data
; pro->get, pParamStruct = pParamStruct    ;pParamStruct Pointer on ImageFilterStruct-Data
;
; _____________________________ IOISIOI _____________________________ (this is art)

function C_sImageFilter_OF_ActiveContours::getImageFilterType
  return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_OF_ActiveContours::checkParamsApplied
; compares the parameter values between the current values and the values applied in the last segmentation
  if ptr_valid(self.pParamApplied) then begin
    if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
      *self.pParamApplied = *(*self.pParamStruct).pValues
      return, 1
    endif
  endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
  return, 0
end


function C_sImageFilter_OF_ActiveContours::apply, image = image,$
                                                  selectedStackObject = selectedStackObject,$
                                                  tPos = tPos,$
                                                  chPos = chPos,$
                                                  zPos = zPos,$
                                                  clusPos = clusPos,$
                                                  segPos = segPos,$
                                                  cut_x = cut_x,$
                                                  cut_y = cut_y,$
                                                  stack_tlb = stack_tlb
    ; check the input image dimensions
  if (size(image, /n_dim) ne 2) or (max(image) eq 0) then return, image
  szI = size(image, /dim)

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Keep_Segmented_Image'))[0]
  if (whParam eq -1) then fKeepSegImage = 0b else $
    fKeepSegImage = ~((*(*self.pParamStruct).pActive)[whParam]) ? 0b : (*(*self.pParamStruct).pValues)[whParam]

    ; if parameters/input image unchanged since the last segmentation appliance of this filter,
    ; there's no need to spend time computing the contours again and return the previosly segmented image
  if (~(self->checkParamsApplied()) and ptr_valid(self.pSegImage)) then begin
    szSeg = size(*self.pSegImage, /dim)
    if (n_elements(szSeg) gt 1) then if ((szI[0] eq szSeg[0]) and (szI[1] eq szSeg[1])) then return, *self.pSegImage
  endif

    ; get parameters for the filter
  whereAlpha = (where((*(*self.pParamStruct).pNames) eq 'Alpha'))[0]
  whereBeta  = (where((*(*self.pParamStruct).pNames) eq 'Beta'))[0]
  whereGamma = (where((*(*self.pParamStruct).pNames) eq 'Gamma'))[0]
  whereKappa = (where((*(*self.pParamStruct).pNames) eq 'Kappa'))[0]
  whereMu    = (where((*(*self.pParamStruct).pNames) eq 'Mu'))[0]
  whereSnakeIterations = (where((*(*self.pParamStruct).pNames) eq 'Snake_Iterations'))[0]
  whereVFiterations    = (where((*(*self.pParamStruct).pNames) eq 'VF_Iterations'))[0]
  whereUserDefVFImage  = (where((*(*self.pParamStruct).pNames) eq 'Userdef_Image_for_VF'))[0]
  whereVFImageCh       = (where((*(*self.pParamStruct).pNames) eq 'VF_Image_Channel'))[0]
  whereVFImageClus     = (where((*(*self.pParamStruct).pNames) eq 'VF_Image_Cluster'))[0]
  whereVFImageSegPos   = (where((*(*self.pParamStruct).pNames) eq 'VF_Image_segPos'))[0]
  whereOFuserDef = (where((*(*self.pParamStruct).pNames) eq 'OF_image_userDef'))[0]
  whereOFclusPos = (where((*(*self.pParamStruct).pNames) eq 'OF_imageClusPos'))[0]
  whereOFsegPos  = (where((*(*self.pParamStruct).pNames) eq 'OF_imageSegPos'))[0]
  whereOFchPos   = (where((*(*self.pParamStruct).pNames) eq 'OF_imageChPos'))[0]
  whereTstep     = (where((*(*self.pParamStruct).pNames) eq 't_step_for_OF_projection'))[0]
  f_iWin = 0b

  tStep = (*(*self.pParamStruct).pValues)[whereTstep] > 0
  if (tPos-tStep lt 0) then begin
    print, 'Error, this filter cannot be applied for the this time frame in the sequence.'
    return, image
  endif

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Display_Contour_Windows'))[0]
  if (*(*self.pParamStruct).pActive)[whParam] then begin
    displayWindows = 1
    (*(*self.pParamStruct).pValues)[whParam] = displayWindows
  endif else displayWindows = 0

  perimeterFactor = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Perimeter_Factor'))[0]]
  maskRule        = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Mask_Rule'))[0]]

    ; determine the size ratio between image dims and real dims
  pParamStruct = selectedStackObject->getpParamStruct()
  whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [pixel]'))[0]
  if (whereDim ne -1) then pixelXDim = *(((*pParamStruct).pValues)[whereDim]) else pixelXDim = 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [pixel]'))[0]
  if (whereDim ne -1) then pixelYDim = *(((*pParamStruct).pValues)[whereDim]) else pixelYDim = 1
  whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [real]'))[0]
  if (whereDim ne -1) then realXDim = *(((*pParamStruct).pValues)[whereDim]) else realXDim = pixelXDIM
  whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [real]'))[0]
  if (whereDim ne -1) then realYDim = *(((*pParamStruct).pValues)[whereDim]) else realYDim = pixelYDIM
  whereDim = (where((*(*pParamStruct).pNames) eq 'z-Interval [real]'))[0]
  if (whereDim ne -1) then realZDim = *(((*pParamStruct).pValues)[whereDim]) else realZDim = 1

  xSizeRatio = pixelXdim / realXdim
  ySizeRatio = pixelYdim / realYdim
  framePixSize = [szI, 1]
  xyzSizePerPixel = [1,1,1]

    ; TODO Clever creation/restoring of segmented ROI objects, masks, images... careful with maskImage/intImage
  oROI2DGroup1ini = obj_new('C_sROIGroupObj', xyzPixSize = [pixelXdim, pixelYdim, 1], ZSliceInterval = realZDim)
  oROI2DGroup1ini->addROIObjectsFromSegImage, maskImage = image, intImage = image, objNumberVector = objNumberVector;, roiType = 1

  totalContours = oROI2DGroup1ini->count()
  if (totalContours lt 1) then begin
    print, 'No contours found for input image at time ', tPos-tStep, '. Returning...'
    return, image
  endif

  contours_t1_ini = ptrArr(totalContours)
  for i = 0L, totalContours-1 do begin
    oROI = oROI2DGroup1ini->get(position = i)
    pBorderPolygon = oROI->getpObjectBorderPolygonList()
    nPolygons = (size(pBorderPolygon, /dim))[0]
    ;xPos = transpose((*pBorderPolygon[0])[0,*])
    ;yPos = transpose((*pBorderPolygon[0])[1,*])
    ;snake->setSnakeCoords, xPos, yPos
    contours_t1_ini[i] = ptr_new(ptrArr(nPolygons))
    for j = 0, nPolygons-1 do begin
      ;x = reform((*pBorderPolygon[j])[0,*])
      ;y = reform((*pBorderPolygon[j])[1,*])
      (*(contours_t1_ini[0]))[j] = pBorderPolygon[j]
    endfor
  endfor
     ; create the output mask image as an 0-value array.
  if ptr_valid(self.pSegImage) $
  then *self.pSegImage = bytArr(szI[0], szI[1]) $
  else  self.pSegImage = ptr_new(bytArr(szI[0], szI[1]), /no_copy)

  intImage1 = (selectedStackObject->getSelectedImage(tPos = tPos-tStep, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
  intImage2 = (selectedStackObject->getSelectedImage(tPos = tPos,       chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]

  fUserDefImageForVF = (*(*self.pParamStruct).pActive)[whereUserDefVFImage] eq 1
  vfClusPos = (*(*self.pParamStruct).pValues)[whereVFImageClus]   > 0; 4
  vfChPos   = (*(*self.pParamStruct).pValues)[whereVFImageCh]     > 0
  vfSegPos  = (*(*self.pParamStruct).pValues)[whereVFImageSegPos] > 0
  if (fUserDefImageForVF eq 1) then begin
    oImage1  = selectedStackObject->getSelectedImageObject(tPos = tPos-tStep, chPos = chPos, zPos = zPos)
    vfImage1 = oImage1->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                               stack_tlb = stack_tlb,$
                                               tPos = tPos-tStep,$
                                               zPos = zPos,$
                                               chPos = vfChPos,$
                                               clusPos = vfClusPos,$
                                               segPos = vfSegPos,$
                                               cut_x = cut_x, cut_y = cut_y)
    oImage2  = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    vfImage2 = oImage2->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                               stack_tlb = stack_tlb,$
                                               tPos = tPos,$
                                               zPos = zPos,$
                                               chPos = vfChPos,$
                                               clusPos = vfClusPos,$
                                               segPos = vfSegPos,$
                                               cut_x = cut_x, cut_y = cut_y)
  endif else begin
    vfImage1 = intImage1
    vfImage2 = intImage2
  endelse

  ofAlpha = 40.0
  ofIterations = 200
  ofScales = 1
  ofMethod = 0

  fCustomOFimage = (*(*self.pParamStruct).pActive)[whereOFuserDef]
  ofChPos   = (*(*self.pParamStruct).pValues)[whereOFchPos]   > 0
  ofClusPos = (*(*self.pParamStruct).pValues)[whereOFclusPos] > 0
  ofSegPos  = (*(*self.pParamStruct).pValues)[whereOFsegPos]  > 0
  if (fCustomOFimage eq 1) then begin
    oOFimage1 = selectedStackObject->getSelectedImageObject(tPos = tPos - tStep, chPos = ofChPos, zPos = zPos)
    ofImage1 = oOFimage1->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                      stack_tlb = stack_tlb,$
                                      tPos = tPos-tStep,$
                                      zPos = zPos,$
                                      chPos = ofChPos,$
                                      clusPos = ofClusPos,$
                                      segPos = ofSegPos,$
                                      cut_x = cut_x, cut_y = cut_y)
    oOFimage2 = selectedStackObject->getSelectedImageObject(tPos = tPos - tStep, chPos = ofChPos, zPos = zPos)
    ofImage2 = oOFimage2->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                      stack_tlb = stack_tlb,$
                                      tPos = tPos,$
                                      zPos = zPos,$
                                      chPos = ofChPos,$
                                      clusPos = ofClusPos,$
                                      segPos = ofSegPos,$
                                      cut_x = cut_x, cut_y = cut_y)
  endif else begin
    ofImage1 = intImage1
    ofImage2 = intImage2
  endelse

  intImage1 = -1
  intImage2 = -1

  opticalFlowMultiscale, ofImage1, ofImage2, ofScales, ofMethod, ofU, ofV, alpha = ofAlpha, iterations = ofIterations
  ofTitle = 'Optical flow for time frames ' + strCompress(string(tPos-tStep) + '-' + string(tPos), /remove_all)
  if (f_iWin eq 1) then $
    iVector, ofU, ofV, auto_color = 2, rgb_table = 34, data_location = 0, LENGTH_SCALE=vecScale, HEAD_SIZE=vecHeadScale, /HEAD_PROPORTIONAL, TITLE=ofTitle, /NO_SAVEPROMPT
;  nContours = oROI2DGroup1ini->count()
;  if nContours gt 0 then for i = 0L, nContours-1 do begin
;    o = oROI2DGroup1ini->get(position = i)
;    pBorderPolygon = (oROIgroupIni->get(position = i))->getpObjectBorderPolygonList()
;    for j = 0, (size(pBorderPolygon, /dim))[0]-1 do begin
;    xPos = transpose((*pBorderPolygon[0])[0,*])
;    yPos = transpose((*pBorderPolygon[0])[1,*])
;    iPlot, [xCoords, xCoords[0]], [yCoords, yCoords[0]], THICK = 2, SYM_INDEX = 2, SYM_SIZE = 0.1, SYM_THICK = 3, /OVERPLOT$
;    endfor
;  endfor
  self->adjustROIcontours, vfImage1, contours_t1_ini,$;oROI2DGroup1ini,$
                                     contours_t1_opt,$;oROI2DGroup1opt,$
                                     (*(*self.pParamStruct).pValues)[whereAlpha],$
                                     (*(*self.pParamStruct).pValues)[whereBeta],$
                                     (*(*self.pParamStruct).pValues)[whereGamma],$
                                     (*(*self.pParamStruct).pValues)[whereKappa],$
                                     (*(*self.pParamStruct).pValues)[whereSnakeIterations],$
                                     -1,$;vfMethod
                                     30,$;winID
                                     pSegmentedMask1, perimeterFactor, 'Active contour segmentation at t=' + string(tPos-tStep)
;                                     (*(*self.pParamStruct).pValues)[whereMu],$
;                                     (*(*self.pParamStruct).pValues)[whereGVFiterations]
    ; TODO mark invalid/collapsed (?) contours
  ;oROI2DGroup2ini = obj_new('C_sROIGroupObj', xyzPixSize = [pixelXdim, pixelYdim, 1], ZSliceInterval = realZDim)
  ;oROI2DGroup2ini = obj_new('IDLanROIGroup')
  contours_t2_ini = ptrArr(totalContours)
  for i = 0L, totalContours-1 do begin

    pPolygons = *(contours_t1_opt[i])
    nPolygons = n_elements(pPolygons)

    contours_t2_ini[i] = ptr_new(ptrArr(nPolygons))
    for j = 0, nPolygons-1 do begin
      polygon = *((*(contours_t1_opt[i]))[j])
      nVert = n_elements(polygon[0,*])
      for v = 0L, nVert-1 do begin
        xr = round(polygon[0,v])
        yr = round(polygon[1,v])
        ; TODO OF interpolation
        polygon[0,v] += ofU[xr, yr]
        polygon[1,v] += ofV[xr, yr]
      endfor
      (*(contours_t2_ini[i]))[j] = ptr_new(polygon, /no_copy)
    endfor

;    oROI = oROI2DGroup1opt->get(position = i)
;    oROI->getProperty, data = roiCoords
;    vCount = n_elements(roiCoords[0,*])
;    ; projection with OF field
;    for v = 0L, vCount-1 do begin
;      xr = round(roiCoords[0,v])
;      yr = round(roiCoords[1,v])
;      ; TODO OF interpolation
;      roiCoords[0,v] += ofU[xr, yr]
;      roiCoords[1,v] += ofV[xr, yr]
;    endfor
;    newRoi = obj_new()
;    contourRoi = obj_new('IDLanROI', roiCoords[0,*], roiCoords[1,*], roiCoords[2,*], type = 1) ; Don't forget the z component :)
;    roiMaskImage = contourRoi->ComputeMask(DIMENSIONS = szI, mask_rule = 2)
;    whRoi = where(roiMaskImage ne 0)
;    savRoi = obj_new('C_sROIObject', wherePoints = whRoi, pointValues = image[whRoi], XYZSIZEPERPIXEL=xyzPixSize, FRAMEPIXSIZE=FRAMEPIXSIZE)
;    oROI2DGroup2ini->add, savRoi
  endfor
  self->adjustROIcontours, vfImage2, contours_t2_ini,$;oROI2DGroup2ini,$
                                     contours_t2_opt,$;oROI2DGroup2opt,$
                                     (*(*self.pParamStruct).pValues)[whereAlpha],$
                                     (*(*self.pParamStruct).pValues)[whereBeta],$
                                     (*(*self.pParamStruct).pValues)[whereGamma],$
                                     (*(*self.pParamStruct).pValues)[whereKappa],$
                                     (*(*self.pParamStruct).pValues)[whereSnakeIterations],$
                                     -1, $;vfMethod
                                     31,$;winID
                                     pSegmentedMask1, perimeterFactor, 'Active contour segmentation at t=' + string(tPos)
  segImage = image * 0
  ;nContoursOpt = oROI2DGroup2opt->count()
  nContoursOpt = n_elements(contours_t2_opt)
  fact = 4
  xWinSize = szI[0] * fact
  yWinSize = szI[1] * fact

  if (nContoursOpt gt 0) then begin
    window, 1, XSIZE = xWinSize, YSIZE = yWinSize
    for i = 0L, nContoursOpt-1 do begin

      pPolygons = *(contours_t2_opt[i])
      nPolygons = n_elements(pPolygons)

      for j = 0, nPolygons-1 do begin
        polygon = *((*(contours_t2_opt[i]))[j])
        x = reform(polygon[0,*])
        y = reform(polygon[1,*])
        plot, [x,x[0]], [y,y[0]], xStyle = 1, yStyle = 1,xRange = [0, szI[0]], yRange = [0, szI[1]], position = [0, 0, xWinSize, yWinSize], color = 255, thick = 2, /noErase
      endfor

      ;oROI = oROI2DGroup2opt->get(position = i)
      ;pBorderPolygon = (oROI2DGroup2opt->get(position = i))->getpObjectBorderPolygonList()
        ; adjust contours for object (*pBorderPolygon[j = 0]) and holes (*pBorderPolygon[j > 0])
;      for j = 0, (size(pBorderPolygon, /dim))[0]-1 do begin
;        x = reform((*pBorderPolygon[j])[0,*])
;        y = reform((*pBorderPolygon[j])[1,*])
;        plot, [x,x[0]], [y,y[0]], xStyle = 1, yStyle = 1,xRange = [0, szI[0]], yRange = [0, szI[1]], position = [0, 0, xWinSize, yWinSize], color = 255,/noErase, thick = 3
;      endfor
      ;iPlot, [xCoords, xCoords[0]], [yCoords, yCoords[0]], THICK = 2, SYM_INDEX = 2, SYM_SIZE = 0.1, SYM_THICK = 3, /OVERPLOT$
    endfor
  endif
  if (fKeepSegImage eq 0) then begin
    segImage = *self.pSegImage
    *self.pSegImage = [0]
    return, segImage
  endif else return, *self.pSegImage

end


pro C_sImageFilter_OF_ActiveContours::adjustROIcontours, image, contoursIni, contoursOpt, cAlpha, cBeta, cGamma, cKappa, cIterations, vfMethod, showWin, pOutImage, perimeterFactor, plotTitle
  if (cIterations lt 1) then begin
    print, 'No iterations will be performed (', cIterations, '). Returning...'
    return
  endif
  ;totalContours = contoursIni->count()
  totalContours = n_elements(contoursIni)
  if (totalContours lt 1) then begin
    print, 'No contours to perform adjustment. Returning...'
    return
  endif

  szI = size(image, /dim)
  framePixSize = [szI, 1]
  xyzSizePerPixel = [1,1,1]
    ; an arbitrary value to amplify the display image (only for this filter)
  imageArea = szI[0] * szI[1]
  areaThreshLo = 10000
  areaThreshHi = 160000
  if (imageArea le areaThreshLo) then f = 8.5 else f = 6
  if (imageArea gt areaThreshHi) then f = 1

    ; window dimensions
  xWinSize = szI[0] * f
  yWinSize = szI[1] * f

  ;contoursOpt = obj_new('IDLanROIGroup')
  ;contoursOpt = obj_new('C_sROIGroupObj', xyzPixSize = [pixelXdim, pixelYdim, 1], ZSliceInterval = realZDim)
  ;contoursOpt = obj_new('C_sROIGroupObj', xyzPixSize = [1, 1, 1], ZSliceInterval = 1)
  contoursOpt = ptrArr(totalContours)
  snake = obj_new('C_sActiveContour', image, alpha = cAlpha, beta = cBeta, gamma = cGamma, kappa = cKappa, iterations = cIterations)

   ; TODO make smart selection (based on the vfMethod parameter) between GVF, GGVF, EP-GVF...
  snake->calcGGVF

  if keyword_set(showWin) and (showWin gt 0) then begin
    window, showWin, xpos=0, ypos=0, xsize = xWinSize, ysize = yWinSize
    pGGVF = snake->getpGGVF()
    veloVect, *pGGVF.pU, *pGGVF.pV
  endif

    ; iterate for each ROI object in the input mask to compute the contours
  for i = 0L, totalContours-1 do begin

    pPolygons = *(contoursIni[i])
    nPolygons = n_elements(pPolygons)
    nPolygons = 1 ; TODO processing only boundaries for now
    contoursOpt[i] = ptr_new(ptrArr(nPolygons))
    ;oROI = contoursIni->get(position = i)
    ;pBorderPolygon = (contoursIni->get(position = i))->getpObjectBorderPolygonList()
    ;nPolygons = (size(pBorderPolygon, /dim))[0]
    ;xPos = transpose((*pBorderPolygon[0])[0,*])
    ;yPos = transpose((*pBorderPolygon[0])[1,*])
    ;snake->setSnakeCoords, xPos, yPos

      ; adjust contours for object (*pBorderPolygon[j = 0]) and holes (*pBorderPolygon[j > 0])
    for j = 0, nPolygons-1 do begin

      snake->setContour, transpose((*(pPolygons[0]))[0,*]), transpose((*(pPolygons[0]))[1,*])
      npts = round((snake->getPerimeter()) * perimeterFactor) > 16
      snake->arcSample, points = npts
      result = snake->adjustContour(plotContour = 1)
      xCoords = snake->getXcoords()
      yCoords = snake->getYcoords()
      if keyword_set(showWin) and (showWin gt 0) then begin
        wSet, showWin
        oplot, xCoords, yCoords
        ;iImage, image, /NO_SAVEPROMPT
        vecScale = 0.4
        vecHeadScale = 0.5
        bgColor = [255,255,255]
        contourColor = [0,0,0]
;        iPlot, [xCoords, xCoords[0]], [yCoords, yCoords[0]], THICK = 2, SYM_INDEX = 2, SYM_SIZE = 0.1, SYM_THICK = 3, TITLE = plotTitle,$
;               xMargin = [0, 0], yMargin = [0, 0], BACKGROUND_COLOR = bgColor, COLOR = contourColor, /NO_SAVEPROMPT
;        iVector, *pGGVF.pU, *pGGVF.pV, /overplot, auto_color = 2, rgb_table = 34, data_location = 0, LENGTH_SCALE=vecScale, HEAD_SIZE=vecHeadScale, /HEAD_PROPORTIONAL
      endif

        ; generate the ROI image
      ;contourRoi = obj_new('IDLanROI', xCoords, yCoords)
      contourRoi = obj_new('IDLgrPolygon', xCoords, yCoords)
      ;roiMaskImage = contourRoi->ComputeMask(DIMENSIONS = szI, mask_rule = 2)
      ;whRoi = where(roiMaskImage ne 0)
      ;savRoi = obj_new('C_sROIObject', wherePoints = whRoi, pointValues = image[whRoi], XYZSIZEPERPIXEL=xyzPixSize, FRAMEPIXSIZE=FRAMEPIXSIZE)
      ;if (j eq 0) $
      ;then segImage = contourRoi->computeMask(mask_rule = maskRule, mask_in = *self.pSegImage) gt 0 $
      ;else segImage -= ((contourRoi->computeMask(mask_rule = maskRule, dimensions = szI[0:1])) gt 0)

      ;contoursOpt->add, savRoi
      *(contoursOpt[i]) = ptr_new([transpose(xCoords), transpose(yCoords)])

      obj_destroy, contourRoi
      ;if ptr_valid(pBorderPolygon[j]) then ptr_free, pBorderPolygon[j]
    endfor
    ;pBorderPolygon = -1
  endfor

;  if keyword_set(showWin) and (showWin gt 0) then begin
;    wSet, showWin
    ;plot, 0.5 + xPos, 0.5 + yPos, xStyle = 1, yStyle = 1, xRange = [0, szI[0]], yRange = [0, szI[1]], position = [0, 0, xWinSize, yWinSize], color = 175, /noErase
    ;plot, 0.5 + xInt, 0.5 + yInt, xStyle = 1, yStyle = 1, xRange = [0, szI[0]], yRange = [0, szI[1]], position = [0, 0, xWinSize, yWinSize], color = 175, /noErase
;    plot, [xCoords,xCoords[0]], [yCoords,yCoords[0]], xStyle = 1, yStyle = 1,xRange = [0, szI[0]], yRange = [0, szI[1]], position = [0, 0, xWinSize, yWinSize], color = 255,/noErase, thick = 2
    ;iImage, image, /NO_SAVEPROMPT
    ;iVector, *pGGVF.pU, *pGGVF.pV, /overplot
;  endif

  if obj_valid(oROI2DGroupIn) then obj_destroy, oROI2DGroupIn
  print, 'Finalized with ', totalContours, ' contour(s) segmented'
  obj_destroy, snake

end


function C_sImageFilter_OF_ActiveContours::init

  ImageFilterStruct = {Name:'C_OF_ActiveContours',$ ; Filter Name.
                       pWidgetType: ptr_new(),$  ; Pointer on Filter Parameter Widget Type.
                       pNames     : ptr_new(),$  ; Pointer on Filter Parameter Names.
                       pActive    : ptr_new(),$  ; Pointer on Filter Parameter Active Bool.
                       pMin       : ptr_new(),$  ; Pointer on Filter Parameter Min_Values.
                       pMax       : ptr_new(),$  ; Pointer on Filter Parameter Max_Values.
                       pDeltas    : ptr_new(),$  ; Pointer on Filter Parameter Max_Values.
                       pValues    : ptr_new()}   ; Pointer on Filter Parameter Values.

    ; Parameters of ActiveContours.
  FilterParamNames = ['Alpha',$
                      'Beta',$
                      'Gamma',$
                      'Kappa',$
                      'Mu',$
                      'Snake_Iterations',$
                      'VF_Iterations',$
                      'Perimeter_Factor',$
                      'Mask_Rule',$
                      'Display_Contour_Windows',$
                      'Keep_Segmented_Image',$
                      'Userdef_Image_for_VF',$
                      'VF_Image_segPos',$
                      'VF_Image_Cluster',$
                      'VF_Image_Channel',$
                      't_step_for_OF_projection',$
                      'OF_image_userDef',$
                      'OF_imageChPos',$
                      'OF_imageClusPos',$
                      'OF_imageSegPos']

  FilterParamWidgetType = make_array(n_elements(FilterParamNames), /string, value = 'widget_slider')

  FilterParamValues = [0.01,$
                       0.5,$
                       1.0,$
                       1.0,$
                       0.05,$
                       15,$
                       30,$
                       1.0,$
                       2,$
                       0.,$
                       1., 0, 0, 0, 0, 1,$
                       0, 0, 0, 0]

  FilterParamActive = [1,$
                       1,$
                       1,$
                       1,$
                       1,$
                       1,$
                       1,$
                       1,$
                       1,$
                       0,$
                       1, 1, 1, 0, 0, 1,$
                       1, 0, 0, 1]

  FilterParamMin = [0.,$
                    0.,$
                    0.,$
                    0.,$
                    0.01,$
                    0,$
                    1,$
                    0.01,$
                    0.,$
                    0.,$
                    0, 0, 0, 0, 0, 0,$
                    0, 0, 0, 0]

  FilterParamMax = [5.,$
                    5.,$
                    2.,$
                    2.,$
                    5.,$
                    1000,$
                    1000,$
                    10.,$
                    2,$
                    1,$
                    1, 1, 100, 100, 100, 100,$
                    1, 100, 100, 100]

  FilterParamDeltas = [0.01,$
                       0.01,$
                       0.01,$
                       0.01,$
                       0.01,$
                       1,$
                       1,$
                       0.1,$
                       1,$
                       1,$
                       1, 1, 1, 1, 1, 1,$
                       1, 1, 1, 1]

  ImageFilterStruct.pWidgetType = ptr_new(FilterParamWidgetType, /no_copy)
  ImageFilterStruct.pActive = ptr_new(FilterParamActive, /no_copy)
  ImageFilterStruct.pValues = ptr_new(FilterParamValues, /no_copy)
  ImageFilterStruct.pDeltas = ptr_new(FilterParamDeltas, /no_copy)
  ImageFilterStruct.pNames = ptr_new(FilterParamNames, /no_copy)
  ImageFilterStruct.pMin = ptr_new(FilterParamMin, /no_copy)
  ImageFilterStruct.pMax = ptr_new(FilterParamMax, /no_copy)

  self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_OF_ActiveContours__define
  tmp = {C_sImageFilter_OF_ActiveContours,$
         pParamStruct : ptr_new(),$
         pParamApplied: ptr_new(),$
         pSegImage    : ptr_new(),$
         pContours    : ptrArr(2,2),$
         inherits C_sImageFilter}
end
