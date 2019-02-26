;_____________________________IOISIOI____________________
; NAME:
; C_sImageFilter_ActiveContours
;
; PURPOSE:
; - ActiveContours-Filter-Class.
;
; CALLING SEQUENCE:
;
; result = obj_new('C_sImageFilter_ActiveContours')
;
; METHODS:
;
; function->apply, pImageData = pImageData ;pImageData Pointer on Image Data Matrix
; pro->set, pParamStruct = pParamStruct    ;pParamStruct Pointer on ImageFilterStruct-Data
; pro->get, pParamStruct = pParamStruct    ;pParamStruct Pointer on ImageFilterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ActiveContours::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_ActiveContours::checkParamsApplied
; compares the parameter values between the current values and the values applied in the last segmentation
   if ptr_valid(self.pParamApplied) then begin
      if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
         *self.pParamApplied = *(*self.pParamStruct).pValues
         return, 1
      endif
   endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
   return, 0
end


function C_sImageFilter_ActiveContours::apply, image = image,$
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

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Keep Segmented Image'))[0]
   if (whParam eq -1) then fKeepSegImage = 0b else $
      if not((*(*self.pParamStruct).pActive)[whParam]) then fKeepSegImage = 0b else fKeepSegImage = (*(*self.pParamStruct).pValues)[whParam]

      ; if parameters/input image unchanged since the last segmentation, no need to calculate contours again
   if ( not(self->checkParamsApplied()) and ptr_valid(self.pSegImage) ) then begin
      szSeg = size(*self.pSegImage, /dim)
      if (n_elements(szSeg) gt 1) then if ((szI[0] eq szSeg[0]) and (szI[1] eq szSeg[1])) then return, *self.pSegImage
   endif

      ; get parameters for the filter
   whereAlpha = (where((*(*self.pParamStruct).pNames) eq 'Alpha'))[0]
   whereBeta = (where((*(*self.pParamStruct).pNames) eq 'Beta'))[0]
   whereGamma = (where((*(*self.pParamStruct).pNames) eq 'Gamma'))[0]
   whereKappa = (where((*(*self.pParamStruct).pNames) eq 'Kappa'))[0]
   whereMu = (where((*(*self.pParamStruct).pNames) eq 'Mu'))[0]
   whereSnakeIterations = (where((*(*self.pParamStruct).pNames) eq 'Snake_Iterations'))[0]
   whereGVFiterations = (where((*(*self.pParamStruct).pNames) eq 'GVF_Iterations'))[0]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Display_Contour_Windows'))[0]
   if (*(*self.pParamStruct).pActive)[whParam] then disWin = round((*(*self.pParamStruct).pValues)[whParam] > 0) else disWin = -1

   perimeterFactor = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Perimeter_Factor'))[0]]
   maskRule = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Mask_Rule'))[0]]

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

      ; create the ROI object to get the masks, images, ...
   oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [pixelXdim, pixelYdim, 1], ZSliceInterval = realZDim)

   oROI2DGroup->addROIObjectsFromSegImage, maskImage = image, intImage = image, objNumberVector = objNumberVector
   totalContours = oROI2DGroup->count()

   fUseOriginalImageForVectorfield = 1b
   case fUseOriginalImageForVectorfield of
   1b: intImage = (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
   else: begin
      clusterPos = 4
      selectedStackObject->getSelectedClusterMask, mask = intImage, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusterPos
      if ((n_elements(intImage) eq 1) and (intImage[0] eq -1)) then begin
          oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
          intImage = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                            tPos = tPos,$
                                                            chPos = chPos,$
                                                            zPos = zPos,$
                                                            clusPos = clusterPos,$
                                                            cut_x = cut_x, cut_y = cut_y)
      endif else intImage = intImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
   endcase
   endcase

   if (totalContours ge 1) then begin
      snake = obj_new('C_sActiveContour', intImage,$
                                          alpha = (*(*self.pParamStruct).pValues)[whereAlpha],$
                                          beta = (*(*self.pParamStruct).pValues)[whereBeta],$
                                          gamma = (*(*self.pParamStruct).pValues)[whereGamma],$
                                          kappa = (*(*self.pParamStruct).pValues)[whereKappa],$
                                          mu = (*(*self.pParamStruct).pValues)[whereMu],$
                                          iterations = (*(*self.pParamStruct).pValues)[whereSnakeIterations],$
                                          gvf_iterations = (*(*self.pParamStruct).pValues)[whereGVFiterations])

      if not(obj_valid(snake)) then return, image
      ;snake->calcGGVF
      snake->calcEPGVF

         ; create the mask image for this filter
      if ptr_valid(self.pSegImage) then *self.pSegImage = bytArr(szI[0], szI[1]) $
         else self.pSegImage = ptr_new(bytArr(szI[0], szI[1]), /no_copy)

         ; an arbitrary value to amplify the display image (only for this filter)
      imageArea = szI[0] * szI[1]
      if imageArea le 10000 then f = 7 else f = 3
      if imageArea gt 160000 then f = 1

         ; window dimensions
      xWinSize = szI[0] * f
      yWinSize = szI[1] * f

         ; plot GVF
      snake->plotGVF, plotContour = disWin

         ; iterate for each ROI object in the input mask to compute the contours
      for i = 0, totalContours-1 do begin
         pBorderPolygon = (oROI2DGroup->get(position = i))->getpObjectBorderPolygonList()

            ; adjust contours for object (*pBorderPolygon[j = 0]) and holes (*pBorderPolygon[j > 0])
         for j = 0, (size(pBorderPolygon, /dim))[0]-1 do begin

            snake->setContour, transpose((*pBorderPolygon[j])[0,*]), transpose((*pBorderPolygon[j])[1,*])
            npts = round((snake->getPerimeter()) * perimeterFactor) > 16
            snake->arcSample, points = npts
            ;intCoords = snake->getCoords()
            ;xInt = intCoords[0]
            ;xInt = intCoords[1]
            result = snake->adjustContour(plotContour = disWin)
            x = snake->getXcoords()
            y = snake->getYcoords()

               ; generate the ROI image
            contourRoi = obj_new('IDLanROI', x, y)
            if (j eq 0) then *self.pSegImage = contourRoi->computeMask(mask_rule = maskRule, mask_in = *self.pSegImage) gt 0 $
               else *self.pSegImage -= ((contourRoi->computeMask(mask_rule = maskRule, dimensions = szI[0:1])) gt 0)
            obj_destroy, contourROI

            if ptr_valid(pBorderPolygon[j]) then ptr_free, pBorderPolygon[j]
         endfor
      endfor

      if (disWin gt 0) then begin
         pGGVF = snake->getpGGVF()
         ;iimage, intImage, /no_save
         ivector, *pGGVF.pU, *pGGVF.pV, auto_color = 2, rgb_table = 34, /NO_SAVEPROMPT;/overplot
      endif

         ; display segmentation results
      live_info, error = error, properties = prop, window_in = '|-> Seg Results'
;      if (disWin eq 1) then begin
;         showImage = congrid(100*image, xWinSize, yWinSize)
;         showImage = tvRd()
;         wDelete, winID
;         if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "|-> Seg Results" does not exist. Operation not  performed.')) then begin
;           live_image, showImage, draw_dimension = [xWinSize + 50, yWinSize + 50], /no_select, reference_out = refOut, title = '|-> Seg Results'
;            live_info, refOut.vis, properties = variable, window_in = refOut.Win
;            variable.color = 'Light Gray'
;            live_control, refOut.vis, properties = variable, window_in = refOut.Win
;         endif else begin
;            live_control, showImage, /update, window_in = '|-> Seg Results'
;         endelse
;      endif else begin
;         if not((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "|-> Seg Results" does not exist. Operation not  performed.')) then $
;            live_destroy, window_in = '|-> Seg Results'
;      endelse

      if obj_valid(oROI2DGroup) then obj_destroy, oROI2DGroup
   endif else return, image

   obj_destroy, snake
   if not(fKeepSegImage) then begin
      segImage = *self.pSegImage
      *self.pSegImage = [0]
      return, segImage
   endif else begin
      return, *self.pSegImage
   endelse
end


function C_sImageFilter_ActiveContours::init

   ImageFilterStruct = {Name:'C_ActiveContours',$ ; Filter Name.
                        pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Widget Type.
                        pNames:ptr_new(),$        ; Pointer on Filter Parameter Names.
                        pActive:ptr_new(),$       ; Pointer on Filter Parameter Active Bool.
                        pMin:ptr_new(),$          ; Pointer on Filter Parameter Min_Values.
                        pMax:ptr_new(),$          ; Pointer on Filter Parameter Max_Values.
                        pDeltas:ptr_new(),$       ; Pointer on Filter Parameter Max_Values.
                        pValues:ptr_new()}        ; Pointer on Filter Parameter Values.

      ; Parameters of ActiveContours.
   FilterParamNames = ['Alpha',$
                       'Beta',$
                       'Gamma',$
                       'Kappa',$
                       'Mu',$
                       'Snake_Iterations',$
                       'GVF_Iterations',$
                       'Perimeter_Factor',$
                       'Mask_Rule',$
                       'Display_Contour_Windows',$
                       'Keep Segmented Image']

   FilterParamWidgetType = make_array(n_elements(FilterParamNames), /string, value = 'widget_slider')
                     ;alpha beta gamma kappa mu iter gvfiter perim mask disp keep 
   FilterParamValues = [0.2, 0.2, 0.5, 1.,  1.,   5,  30, .2,   2,  2., 1.]
   FilterParamActive = [1,   1,   1,   1,   1,    1,  1,  1,    1,  0,  1]
   FilterParamMin    = [0.,  0.,  0.,  0.,  0.01, 1,  1,  0.01, 0., 0., 0]
   FilterParamMax    = [5.,  5.,  5.,  10., 10.,   20, 90, 10.,  2,  2,  1]
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
                        1]

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


pro C_sImageFilter_ActiveContours__define
   tmp = {C_sImageFilter_ActiveContours,$
          pParamStruct:ptr_new(),$
          pParamApplied:ptr_new(),$
          pSegImage:ptr_new(),$
          inherits C_sImageFilter}
end