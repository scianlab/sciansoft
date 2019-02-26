;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_OpticalFlowAlphaTest
;
; PURPOSE:
;       - Identity-Filter-Class. See Identity.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_GradientImage' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_OpticalFlowAlphaTest::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end




function C_sImageFilter_OpticalFlowAlphaTest::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

 
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Shift_Range'))[0]
   shiftRange =  (*(*self.pParamStruct).pValues)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Shift_StepSize'))[0]
   shiftStepSize = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Alpha_Min'))[0]
   alphaMin = (*(*self.pParamStruct).pValues)[whParam]
   if(alphaMin eq 0) then alphaMin = 0.00001 
    
    
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Alpha_Max'))[0]
   alphaMax = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Alpha_StepSize'))[0] 
   alphaStepSize = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'OF_Iterations'))[0] 
   ofIterations = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Cluster_Nr_Segmentation'))[0] 
   segmentationCluster = (*(*self.pParamStruct).pValues)[whParam]
  
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Segmentation_Position'))[0] 
   segmentationPos = (*(*self.pParamStruct).pValues)[whParam]
  
   alphaRange = abs(alphaMax-alphaMin)
  
   print, 'shift range: ', shiftRange
   print, 'shift step size: ', shiftStepSize
   print, 'alpha max: ', alphaMax
   print, 'alpha step size: ', alphaStepSize
   print, 'iterations: ', ofIterations 
   print, 'Segmentation Cluster Nr: ', segmentationCluster 
   print, 'Segmentation Position: ', segmentationPos
   
   if(segmentationCluster eq clusPos) then segmentationCluster = -1
   
   oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)   
   segmentedImage = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                         tPos = tPos ,$
                                                         chPos = chPos ,$
                                                         zPos = zPos ,$
                                                         clusPos = segmentationCluster,$
                                                         segPos = segmentationPos,$
                                                         cut_x = cut_x, cut_y = cut_y)
    
    
    
    
  ; groundTruth = read_tiff('C:\RSI\janscheer\ofTest_Model2\original50Point\groundtruth_pt_cut.tif')
    
   currentShift = shiftStepSize
   groundtruth = segmentedImage
   ;groundTruth = read_tiff('C:\RSI\janscheer\ofTests_fromScratch\R1_R2_dendrita\cut_rotated\_Clus0__Clus0_dendrita_GABAR1_t024_ch00_z000.tif')
    
          ; --- Setup section (parameters not needed frequently) ---------------------------
   verbose = 1             ; (bool) display info like elapsed time and stats
   saveFlows = 1
   doShift = 0
   vectorColorTrick = 1    ; false vectors to have full range of orientations (and constant scale)
   boundaryConditions = 1  ; consider boundary conditions in Horn1981
   stopCrit = 0            ; 0 - num iterations / 1 - no improvement
   


   ind = where(groundtruth ne 0)
   ; Construct Pixel Distance Matrix
   pixRad = shiftRange
   
   ;distMatrix = dblArr(2*pixRad+1, 2*pixRad+1)
   distArrSize = 0
   for o = 1, pixRad, 1 do distArrSize += (o+1)
   distArr = [0]
   distArr_counter = 0
   ind_gt = where(groundtruth gt 0)
   ; initial groundtruth
   groundtruth_x = fltArr(size(groundtruth, /DIMENSION))
   groundtruth_y = fltArr(size(groundtruth, /DIMENSION))
   
   ;-------------------------------------------------
   window, 20, ysize = 400, xsize=400
   
   
   window, 21, ysize = 400, xsize=400
   tvscl, congrid(segmentedImage, 400,400)   
   ;--------------------------------------------------
   ; define k-,l-,dist-, & value- vectors
   correlDim = 0
   kVect = [0]
   lVect = [0]
   distVect = [0]
   for k = -pixRad,pixRad do for l = -pixRad,pixRad do begin
      if ((1.*(k*k+l*l)) le pixRad^2) then begin
         correlDim += 1
         kVect = [kVect, k]
         lVect = [lVect, l]
         distVect = [distVect, sqrt(1.*(k*k+l*l))]
         h = where(distArr eq sqrt(1.*(k*k+l*l)))
         if(size(h,/dimension) le 0) then begin
            distArr = [distArr,sqrt(1.*(k*k+l*l))]
            distArr_counter +=1
         endif
         
      endif
   endfor
   distArr = distArr[sort(distArr)]
   kVect = kVect[1:*]
   lVect = lVect[1:*]
   distVect = distVect[1:*]
   DDAVals = fltArr(correlDim > 1, 2)
   
   ; sort vectors
   sortDV = sort(distVect)
   distVect = distVect[sortDV]
   kVect = kVect[sortDV]
   lVect = lVect[sortDV]
   distances = size(distVect, /DIMENSION)
   ;construct alpha matrix
   alpha_matrixAEE = dblArr(round(alphaRange/alphaStepSize)+2, distances)
   alpha_matrixAEEDiff = dblArr(round(alphaRange/alphaStepSize)+2, distances)
   alpha_matrixAAE = dblArr(round(alphaRange/alphaStepSize)+2, distances) 
   alpha_values = dblArr(round(alphaRange/alphaStepSize)+1)
   alphaPosition = 1
   window, 22, ysize = 400, xsize=400
   
   imageSize = size(image, /DIMENSION)
   
   xSize = imageSize[0]-1
   ySize = imageSize[1]-1
   window, 11, xpos = 0,ysize = 400, xsize=400
   window, 12, xpos = 400,ysize = 400, xsize=400  
   window, 18, xpos = 800,ysize = 400, xsize=400 ; surface gradient
   window, 17, xpos = 800, ypos=400, ysize = 400, xsize=400 ; congrid gradient
   imageOrg = image
   for i = 1, correlDim-1 do begin
   print, 'distance: ', distVect[i] ,' shift x: ', kVect[i], ' shift y: ', lVect[i]
      
      if(kVect[i] eq lVect[i]) then print, 'DIAG'
      
      for currentAlpha = alphaMin, alphaMax, alphaStepSize do begin  
        image = imageOrg
        print, 'alpha: ', currentAlpha 
        
        
        ; create shifted image
        shiftedImage = shift(image, kVect[i], lVect[i])     
         ; reset segmented image to original segmentation
        segmentedImage = groundtruth
        indSeg = where(segmentedImage ne 0)
        indSegNot = where(segmentedImage le 0)
        ; initial groundtruth array
        groundtruth_x = fltArr(size(groundtruth, /DIMENSION))
        groundtruth_y = fltArr(size(groundtruth, /DIMENSION))
        
         ; delete lines and rows of shifted non-sense
         ; in segmentedImage and in shiftedImage
         ; produced by the shift function
         if(kVect[i] gt 0) then begin
            shiftedImage(0:kVect[i], *) = 0
            segmentedImage(0:kVect[i], *) = 0
            ;image(0:kVect[i], *) = 0
         endif else if(kVect[i] lt 0) then begin
            shiftedImage([xsize+kVect[i]]:xsize, *) = 0
            segmentedImage([xsize+kVect[i]]:xsize, *) = 0
            ;image([xsize+kVect[i]]:xsize, *) = 0
         endif

        if(lVect[i] gt 0) then begin
            shiftedImage(*, 0:lVect[i]) = 0
            segmentedImage(*, 0:lVect[i]) = 0
            ;image(*, 0:lVect[i]) = 0
         endif else if(lVect[i] lt 0) then begin
            shiftedImage(*, [ysize+lVect[i]]:ysize) = 0
            segmentedImage(*, [ysize+lVect[i]]:ysize) = 0
            ;image(*, [ysize+lVect[i]]:ysize) = 0
         endif
        
         ; set window and show image
         wset, 20 ;original
         tvscl, congrid(image, 400,400)
         
          ; set alpha
         alpha_values[alphaPosition-1] = currentAlpha
         ; calculate optical flow
         oOF = obj_new('C_sOpticalFlow')
         k=oOF->calc(image, image_2 = shiftedImage , alpha = currentAlpha, iteration = ofIterations,$
                    boundaryConditions = boundaryConditions, doShift = doShift, verbose = verbose, stopCrit = stopCrit)
         u = oOF->get_u()
         v = oOF->get_v()
         obj_destroy, oOF 
         ; construct spacial kernels
         gradientKernelX = 1./2*[[-1, 0, 1]]
         gradientKernelY = 1./2*[[-1], [0], [1]]
         ;imageSum = image+shiftedImage
         ; spacial gradients of image at t0
         image_Ex = convol(image, gradientKernelX)
         image_Ey = convol(image, gradientKernelY)

         ; magnitude of gradient
         gradientMagnitude = sqrt(image_Ex*image_Ex+image_Ey*image_Ey)
         ; normalize to 255
         gradientMagnitude = gradientMagnitude*255/max(gradientMagnitude)
         
          ; calculate magnitude of the optical flow
         ofMagnitude = sqrt(u*u+v*v)
         ; normalized to 255
         nofMagnitude = ofMagnitude/max(ofMagnitude)
         
         if(kVect[i] gt 0) then begin
            nofMagnitude(0:kVect[i], *) = 0
            gradientMagnitude(0:kVect[i], *) = 0
            ;image(0:kVect[i], *) = 0
         endif else if(kVect[i] lt 0) then begin
            nofMagnitude([xsize+kVect[i]]:xsize, *) = 0
            gradientMagnitude([xsize+kVect[i]]:xsize, *) = 0
            ;image([xsize+kVect[i]]:xsize, *) = 0
         endif

        if(lVect[i] gt 0) then begin
            nofMagnitude(*, 0:lVect[i]) = 0
            gradientMagnitude(*, 0:lVect[i]) = 0
            ;image(*, 0:lVect[i]) = 0
         endif else if(lVect[i] lt 0) then begin
            nofMagnitude(*, [ysize+lVect[i]]:ysize) = 0
            gradientMagnitude(*, [ysize+lVect[i]]:ysize) = 0
            ;image(*, [ysize+lVect[i]]:ysize) = 0
         endif
         
         ; test
         aparture = gradientMagnitude;nofMagnitude*
         
         ; show images
         wset, 18 ;original
         surface, aparture;tvscl, congrid(image, 400,400)
         wset, 17
         tvscl, congrid(aparture, 400,400)
         
         ;isurface, aparture


         ; copy for visualization
         ofMagnitude2 = ofMagnitude
         ; calculate ground truth magnitude
         gtMagnitude = sqrt(kVect[i]*kVect[i]+lVect[i]*lVect[i])
         ; reduce only to ROI
         ofMagnitude2(indSegNot)=0

         ; calculate low values of optical flow magnitude in the ROI
         ;ind_low = where(aparture(indSeg) le 0.2*max(aparture(indSeg)))
         ; size of index array referencing low values 
         size_u = 0;size(ind_low, /DIMENSION)
         ; set values to zero to demonstrate where significant/unsignificant OF-values serve for further predictions
         if(size_u gt 0) then ofMagnitude2(indSeg(ind_low)) = 0   
         
         ; set window and show the magnitude in the ROI, but only where values are significant
         wset, 11
         tvscl, congrid(ofMagnitude2, 400, 400)
         
         ; show of magnitude
         wset, 12
         tvscl, congrid(ofMagnitude, 400, 400)

         ; in the segmented image, set unsignificant values to 0
         if(size_u gt 0) then segmentedImage(indSeg(ind_low)) = 0
         ; get indeces for ground truth
         ind = where(segmentedImage ne 0)
         size_ind = size(ind, /DIMENSION)
         ; show shifted image
         wset, 22
         tvscl, congrid(shiftedImage, 400,400)  
         ; set groundtruth in segmented image regions
         if(size_ind gt 0) then groundtruth_y(ind) = lVect[i]
         if(size_ind gt 0) then groundtruth_x(ind)= kVect[i]
         ; erase groundtruth pixels with minor of estimations
         ;if(size_u gt 0) then begin
         ;   groundtruth_x(ind(ind_low)) = 0
         ;   groundtruth_y(ind(ind_low)) = 0
         ;endif
         
         ; for visualization
         if(size_u gt 0) then segmentedImage(indSeg(ind_low)) = 2
         wset, 21
         tvscl, congrid(segmentedImage, 400,400)   
         
         avgEndPointError = mean( sqrt( (u-groundtruth_x)^2 + (v-groundtruth_y)^2))
         if(size_ind gt 0) then avgEndPointErrorInROI = mean( sqrt( (u(ind)-groundtruth_x(ind))^2 + (v(ind)-groundtruth_y(ind))^2))  

         if(size_ind gt 0) then avgEndPointErrorInROI = mean( sqrt( (mean(u(ind)) - kVect[i])^2 + (mean(v(ind)) - lVect[i])^2))  


         if(size_ind gt 0) then avgAngularErrorInROI = mean(acos((v(ind)*groundtruth_y(ind)+u(ind)*groundtruth_x(ind)+1) / (sqrt(u(ind)^2+v(ind)^2+1)*sqrt(groundtruth_y(ind)^2+groundtruth_x(ind)^2+1))))  
         alpha_matrixAEEDiff[0, i] = distVect[i]
         alpha_matrixAEEDiff[alphaPosition,i] = distVect[i]-avgEndPointErrorInROI
         alpha_matrixAEE[0, i] = distVect[i]
         alpha_matrixAEE[alphaPosition,i] = avgEndPointErrorInROI
         alpha_matrixAAE[0, i] = distVect[i]
         alpha_matrixAAE[alphaPosition,i] = avgAngularErrorInROI   
   
         alphaPosition +=1 
       endfor
       alphaPosition = 1
   endfor

   
   print, alpha_matrixAEE
  
   openW,2, strCompress('c:\rsi\OF_print.dat', /rem)
   printF, 2, alpha_matrixAEE ;[alpha_matrixAEE[0,*],alpha_matrixAEE[0,*]-alpha_matrixAEE[1:*,*]]
   close, 2
   
   ;print, '--------- ----------- ---------- ------------ ---------- ------------'
   ;print, alpha_matrixAAE
   ;window, 15
   distances = alpha_matrixAEE(0, *)
   iplot, distances, distances, yrange=[0,max(distances)], name='control', view_title='Dynamic Alpha Plot - Average Endpoint Error', linestyle=solid, background_color = [255,255,128], dimension = [1200,1000], /insert_legend
   iplot, distances, distances, yrange=[0,max(distances)], name='control', xtitle='Pixel Distance d', ytitle='Difference = |Control - AEE|', $
         sym_index = 3, sym_size =2.5, background_color = [255,255,128], dimension = [1200,1000], /insert_legend, /overplot
         
   
   for i = 1, round(alphaRange/alphaStepSize)+1 do begin
      col = [10*i,0,0]
      name = strCompress(alpha_values[i-1])
      vector = alpha_matrixAEEDiff[i, *]
      ;diff_vector = distances-vector
      iplot, distances, vector, color = col,name=name, /overplot, /scatter, sym_color = col, sym_index = 3, view_zoom = 1.0, sym_size =2.0, xtickvalues=distArr,/insert_legend, /fit_to_view
   endfor
    
   
   ;-------------------------------------------------------------------------------------------------------
   bestAlpha = alphaMin
   bestAlphaPosition = 0
   bestPixelEstimation = 0
   bestAAEvectorDiff = 0
   alpha_matrixWeights = dblArr(1,round(alphaRange/alphaStepSize)+1)
   errorSum = 10000
   ;
   ; Find best Alpha
   print, 'Best Alpha Search.... '
   for i = 1, round(alphaRange/alphaStepSize)+1 do begin ; For each Alpha tested
   
      vector = alpha_matrixAEE[i, *]   ; Get Vector with Estimations Errors (AEE)
      pixelEstimation = 0 ; set Estimtaion distance for this vector to 0              
      currentErrorSum = 0 ; set Error Sum to 0
      for d = 1, distArr_counter do begin ; For each distance & direcction (min. 4 values)
         ind_alphDist = where(distVect eq distArr[d]); get indices in vector
         alphaDistErrors = vector[0,ind_alphDist]
         currentErrorSum += total(alphaDistErrors)
         
         ind_bigError = where(alphaDistErrors gt 0.5)
         if(size(ind_bigError, /dimension) le 0) then begin
            pixelEstimation = distArr[d]
         endif else begin
            pixelEstimation = distArr[d-1]
            break
         endelse
      endfor
      print, 'With Alpha: ', alpha_values[i-1], ' reaching a max Estimation of : ', pixelEstimation,' with AAE Error Sum: ',currentErrorSum
      if(pixelEstimation gt bestPixelEstimation) then begin
         print, '----- Pixel Estimation has improved!---> Before: ',pixelEstimation, ' >= ', bestPixelEstimation
         ; pixel estimation has improved!
         bestPixelEstimation = pixelEstimation
         bestAlpha = alpha_values[i-1]
         errorSum = currentErrorSum
         bestAAEvectorDiff = alpha_matrixAEEDiff[i, *] ; remember the best predictions (difference between control and AAE)
      endif else if(pixelEstimation ge bestPixelEstimation && currentErrorSum lt errorSum) then begin 
         print, '-----Now AEE error sum was better!---> Before: ',errorSum, ' >= ', currentErrorSum
          ; pixel estimation can be the same as before, but Error sum has improved!
         bestPixelEstimation = pixelEstimation
         bestAlpha = alpha_values[i-1]
         errorSum = currentErrorSum
         bestAAEvectorDiff = alpha_matrixAEEDiff[i, *] ; remember the best predictions (difference between control and AAE)
      endif
     
   endfor
 

        print, '----------------------------------------------------------------------'
        print, 'Optimal Alpha for Optical Flow: ', bestAlpha
        print, 'Allows Estimation of Pixel Displacements up to: ',bestPixelEstimation, ' pixels. ' 
        print, '----------------------------------------------------------------------
        iplot, distances, bestAAEvectorDiff, color = [0,255,0],name=bestAlpha, /overplot, /scatter, sym_color = [0,255,0], sym_index = 3, view_zoom = 1.0, sym_size =2.0, /insert_legend, /fit_to_view
 
        
    return, 0
end


function C_sImageFilter_OpticalFlowAlphaTest::init

    filterStruct = {Name: 'C_OpticalFlowAlphaTest',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new(),$         ; Pointer on Filter Parameter Values.
                          pDeltas:ptr_new()}
      
    ; Parameters of Identity.
    filterParamWidgetType = ['widget_slider','widget_slider','widget_slider','widget_slider','widget_slider','widget_slider','widget_slider', 'widget_slider']
    filterParamNames = ['Shift_Range','Shift_StepSize','Alpha_Min','Alpha_Max', 'Alpha_StepSize','OF_Iterations','Cluster_Nr_Segmentation', 'Segmentation_Position']
    filterParamActive = [1,1,1,1,1.,1,1,1]
    filterParamMin = [1,1,1,1,0.,200,0,0]
    filterParamMax = [50,1,10000,10000,100.,20000,100,100]
    filterParamValues = [10,1,10,200,10.,200,0,0]
    filterParamDeltas = [0.01,$
                        0.01,$
                        0.01,$
                        0.01,$
                        0.01,$
                        1,$
                        1,$
                        1]
    
    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
    filterStruct.pDeltas = ptr_new(filterParamDeltas, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_OpticalFlowAlphaTest__define
  tmp = {C_sImageFilter_OpticalFlowAlphaTest, pParamStruct: ptr_new(), inherits C_sImageFilter}
end