;_____________________________IOISIOI____________________
; NAME:
; C_sImageFilter_AdjacentActiveContours
;
; PURPOSE:
; - ActiveContours-Filter-Class.
;
; CALLING SEQUENCE:
;
; result = obj_new('C_sImageFilter_AdjacentActiveContours')
;
; METHODS:
;
; function->apply, pImageData = pImageData ;pImageData Pointer on Image Data Matrix
; pro->set, pParamStruct = pParamStruct    ;pParamStruct Pointer on ImageFilterStruct-Data
; pro->get, pParamStruct = pParamStruct    ;pParamStruct Pointer on ImageFilterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_AdjacentActiveContours::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


; checkParamsApplied
; Compares the parameter values between the current values and the values applied in the last segmentation
;
; RETURN VALUE:
;  0 if the parameters values have not changed since the last check.
;  1 otherwise (including the first-time parameter setting case).
function C_sImageFilter_AdjacentActiveContours::checkParamsApplied
  if ptr_valid(self.pParamApplied) then begin
    if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
       *self.pParamApplied = *(*self.pParamStruct).pValues
       return, 1
     endif else return, 0
  endif else begin
    self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
    return, 1
  endelse
end


function C_sImageFilter_AdjacentActiveContours::apply, image = image,$
                                                        selectedStackObject = selectedStackObject,$
                                                        tPos = tPos,$
                                                        chPos = chPos,$
                                                        zPos = zPos,$
                                                        clusPos = clusPos,$
                                                        segPos = segPos,$
                                                        cut_x = cut_x,$
                                                        cut_y = cut_y,$
                                                        stack_tlb = stack_tlb
  paramNames = *(*self.pParamStruct).pNames

  wherePlay = (where(paramNames eq 'Play'))[0]
  play = (*(*self.pParamStruct).pValues)[wherePlay]
  if (play eq 0) then return, image
  if (play eq 1) then (*(*self.pParamStruct).pValues)[wherePlay] = 0

    ; check the input image dimensions
  if (size(image, /n_dim) ne 2) or (max(image) eq 0) then return, image
  szI = size(image, /dim)

  whParam = (where(paramNames eq 'Keep_Segmented_Image'))[0]
  if (whParam eq -1) $
  then fKeepSegImage = 0b $
  else if ~((*(*self.pParamStruct).pActive)[whParam]) $
       then fKeepSegImage = 0b $
       else fKeepSegImage = (*(*self.pParamStruct).pValues)[whParam]

    ; if parameters/image didn't change since the last segmentation, then return the stored (segmented) image.
  if ((self->checkParamsApplied() eq 0) and ptr_valid(self.pSegImage)) then begin
    szSeg = size(*self.pSegImage, /dim)
    if (n_elements(szSeg) gt 1) then if ((szI[0] eq szSeg[0]) and (szI[1] eq szSeg[1])) then return, *self.pSegImage
  endif

    ; get parameters for the filter
  whereAlpha = (where(paramNames eq 'Elasticity'))[0]
  whereBeta  = (where(paramNames eq 'Rigidity'))[0]
  whereGamma = (where(paramNames eq 'Viscosity'))[0]
  whereKappa = (where(paramNames eq 'VF_weight'))[0]
  whereContourIterations = (where(paramNames eq 'Contour_Iterations'))[0]
  whereVFiterations      = (where(paramNames eq 'VF_Iterations'))[0]

  whParam = (where(paramNames eq 'VF_type'))[0]
  vfType  = round((*(*self.pParamStruct).pValues)[whParam] > 0)

  whParam = (where(paramNames eq 'Display_Contour_Windows'))[0]
  if (*(*self.pParamStruct).pActive)[whParam] then displayWin = round((*(*self.pParamStruct).pValues)[whParam] > 0) else displayWin = -1

  perimeterFactor = (*(*self.pParamStruct).pValues)[(where(paramNames eq 'Perimeter_Factor'))[0]]
  maskRule        = (*(*self.pParamStruct).pValues)[(where(paramNames eq 'Mask_Rule'))[0]]

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

  whParam = (where(paramNames eq 'Use_Custom_Mask_Image'))[0]
  fUseCustomMaskImage = (*(*self.pParamStruct).pActive)[whParam]

  case fUseCustomInputMaskImage of
    0   : inputMask = image
    else: begin
      whParam        = (where(paramNames eq 'Mask_Image_clusPos'))[0]
      maskClusterPos = (*(*self.pParamStruct).pValues)[whParam]
      whParam        = (where(paramNames eq 'Mask_Image_segPos'))[0]
      maskSegPos     = (*(*self.pParamStruct).pValues)[whParam] ; TODO make a clever way to enable default/non-defined value

      selectedStackObject->getSelectedClusterMask, mask = maskImage, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = maskClusterPos
      if ((n_elements(maskImage) eq 1) and (maskImage[0] eq -1)) then begin
        oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
        maskImage = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                   stack_tlb = stack_tlb,$
                                                   tPos = tPos,$
                                                   chPos = chPos,$
                                                   zPos = zPos,$
                                                   clusPos = maskClusterPos,$
                                                   segPos = maskSegPos,$
                                                   cut_x = cut_x, cut_y = cut_y)
      endif else maskImage = maskImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    endcase
  endcase

  ; create the ROI object to get the masks, images, ...
  oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [pixelXdim, pixelYdim, 1], ZSliceInterval = realZDim)
  oROI2DGroup->addROIObjectsFromSegImage, maskImage = maskImage, intImage = image, objNumberVector = objNumberVector
  totalContours = oROI2DGroup->count()

  if (totalContours lt 1) then begin
    obj_destroy, oROI2DGroup
    return, image
  endif

  whParam = (where(paramNames eq 'Use_Custom_VF_Image'))[0]
  fUseCustomImageForVectorfield = (*(*self.pParamStruct).pActive)[whParam]

  case fUseCustomImageForVectorfield of
    0b  : intImage = (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    else: begin
      whParam      = (where(paramNames eq 'VF_Image_clusPos'))[0]
      vfClusterPos = (*(*self.pParamStruct).pValues)[whParam]
      whParam      = (where(paramNames eq 'VF_Image_segPos'))[0]
      vfSegPos     = (*(*self.pParamStruct).pValues)[whParam] ; TODO make a clever way to enable default/non-defined value

      selectedStackObject->getSelectedClusterMask, mask = intImage, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = vfClusterPos
      if ((n_elements(intImage) eq 1) and (intImage[0] eq -1)) then begin
        oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
        intImage = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                  tPos = tPos,$
                                                  chPos = chPos,$
                                                  zPos = zPos,$
                                                  clusPos = vfClusterPos,$
                                                  segPos = fvSegPos,$
                                                  cut_x = cut_x, cut_y = cut_y)
      endif else intImage = intImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    endcase
  endcase

  snakes = obj_new('C_sAABContainer',$
                   intImage,$
                   alphaVal = (*(*self.pParamStruct).pValues)[whereAlpha],$
                   betaVal  = (*(*self.pParamStruct).pValues)[whereBeta],$
                   gammaVal = (*(*self.pParamStruct).pValues)[whereGamma],$
                   kappaVal = (*(*self.pParamStruct).pValues)[whereKappa],$
                   muVal    = 0.1,$
                   contourIterations = (*(*self.pParamStruct).pValues)[whereContourIterations],$
                   vfIterations      = (*(*self.pParamStruct).pValues)[whereVFiterations])

  if (~obj_valid(snakes)) then begin
    print, 'Error creating active contours object. Returning the input image... '
    return, image
  endif

  case vfType of
    0   : snakes->calcGVF
    1   : snakes->calcGGVF
    2   : snakes->calcEPGVF
    else: snakes->calcGVF
  endcase

    ; create the segmented image
  if ptr_valid(self.pSegImage) $
  then *self.pSegImage = bytArr(szI[0], szI[1]) $
  else self.pSegImage = ptr_new(bytArr(szI[0], szI[1]), /no_copy)

    ; an arbitrary value to amplify the (optional) display images
  imageArea = szI[0] * szI[1]
  winSizeFactor = (imageArea gt 160000) ? 1 : ((imageArea le 10000) ? 7 : 3)
  xWinSize = szI[0] * winSizeFactor
  yWinSize = szI[1] * winSizeFactor

    ; plot GVF
  snakes->plotGVF, plotContour = displayWin

    ; add the contours for each ROI object in the input mask
  for i = 0, totalContours-1 do begin

    pBorderPolygon = (oROI2DGroup->get(position = i))->getpObjectBorderPolygonList()

      ; Object outer contour: (*pBorderPolygon[j = 0])
      ; Object inner contour (*pBorderPolygon[j > 0])
    for j = 0, (size(pBorderPolygon, /dim))[0]-1 do begin

      snakes->addContour, transpose((*pBorderPolygon[j])[0,*]), transpose((*pBorderPolygon[j])[1,*])
      ;npts = round((snake->getPerimeter()) * perimeterFactor) > 16
      ;snakes->arcSample, points = npts
      ptr_free, pBorderPolygon[j]
    endfor
  endfor
  obj_destroy, oROI2DGroup

  result = snakes->adjustContour(plotContour = displayWin)
  x = snakes->getXcoords()
  y = snakes->getYcoords()
  
  ; generate the ROI image
  contourRoi = obj_new('IDLanROI', x, y)
  if (j eq 0) $
    then *self.pSegImage = contourRoi->computeMask(mask_rule = maskRule, mask_in = *self.pSegImage) gt 0 $
  else *self.pSegImage -= ((contourRoi->computeMask(mask_rule = maskRule, dimensions = szI[0:1])) gt 0)
  obj_destroy, contourROI

  if (displayWin eq 0) or (displayWin eq 1) then begin
    pGGVF = snakes->getpGGVF()
    iImage, intImage, /no_save
    iVector, *pGGVF.pU, *pGGVF.pV, /overplot
  endif

  obj_destroy, snakes

  if ~fKeepSegImage then begin
    segImage = *self.pSegImage
    *self.pSegImage = [0]
    return, segImage
  endif else begin
    return, *self.pSegImage
  endelse

end


pro C_sImageFilter_AdjacentActiveContours::cleanup
  ptr_free, self.pParamApplied
  ptr_free, self.pSegImage
end


function C_sImageFilter_AdjacentActiveContours::init

  ImageFilterStruct = {Name: 'C_AdjacentActiveContours',$; Filter Name.
                       pWidgetType : ptr_new(),$         ; Pointer on Filter Parameter Widget Type.
                       pNames      : ptr_new(),$         ; Pointer on Filter Parameter Names.
                       pActive     : ptr_new(),$         ; Pointer on Filter Parameter Active Bool.
                       pMin        : ptr_new(),$         ; Pointer on Filter Parameter Min_Values.
                       pMax        : ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                       pDeltas     : ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                       pValues     : ptr_new()}          ; Pointer on Filter Parameter Values.

    ; Parameters of AdjacentActiveContours.
  FilterParamNames = ['Play',$
                      'Elasticity',$
                      'Rigidity',$
                      'Viscosity',$
                      'VF_weight',$
                      'VF_sigma',$
                      'VF_convergenceLimit',$
                      'EPGVF_noiseCut',$
                      'EPGVF_noiseRange',$
                      'VF_Iterations',$
                      'Contour_Iterations',$
                      'Perimeter_Factor',$
                      'Mask_Rule',$
                      'Use_Custom_Mask_Image',$
                      'Mask_Image_segPos',$
                      'Mask_Image_clusPos',$
                      'Mask_Image_segPos',$
                      'Use_Custom_VF_image',$
                      'VF_Image_segPos',$
                      'VF_Image_clusPos',$
                      'VF_Image_segPos',$
                      'Display_Contour_Windows',$
                      'Keep_Segmented_Image'$
                       ]

  FilterParamWidgetType = make_array(n_elements(FilterParamNames), /string, value = 'widget_slider')
                      ;alpha beta  gamma kappa  mu   iter vfiter perim mask disp keep
  FilterParamValues = [ 0.2, 0.2 , 0.5 ,  1.  ,  1.  ,  5, 30,   .2 ,  2 ,  2., 1.]
  FilterParamActive = [1   , 1   , 1   ,  1   ,  1   ,  1,  1,  1   ,  1 ,  0 , 1]
  FilterParamMin    = [0.  , 0.  , 0.  ,  0.  ,  0.01,  1,  1,  0.01,  0.,  0., 0]
  FilterParamMax    = [5.  , 5.  , 5.  , 10.  , 10.  , 20, 90, 10.  ,  2 ,  2 , 1]
  FilterParamDeltas = [0.01, 0.01, 0.01,  0.01,  0.01,  1,  1,  0.1  , 1 ,  1 , 1]

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


pro C_sImageFilter_AdjacentActiveContours__define
  tmp = {C_sImageFilter_AdjacentActiveContours,$
         pParamStruct  : ptr_new(),$
         pParamApplied : ptr_new(),$
         pSegImage     : ptr_new(),$
         inherits C_sImageFilter}
end
