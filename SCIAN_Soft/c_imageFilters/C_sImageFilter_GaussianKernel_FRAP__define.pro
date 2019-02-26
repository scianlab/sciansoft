;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_GaussianKernel_FRAP
;
; PURPOSE:
;       - Identity-Filter-Class. See Identity.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_GaussianKernel_FRAP' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_GaussianKernel_FRAP::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end



function C_sImageFilter_GaussianKernel_FRAP::makeGaussianKernel, sigma = sigma, accuracy = accuracy, maxRadius = maxRadius

    
    print, 'Make Gaussian Kernel (1D)'
    accuracy = 0.001
    kRadius = ceil(sigma*sqrt(-2*alog(accuracy)))+1;
    if (maxRadius < 50) then maxRadius = 50;         // too small maxRadius would result in inaccurate sum.
    if (kRadius > maxRadius) then kRadius = maxRadius;
    

    
    kernel = FLTARR(2, kRadius);
    for i = 0, kRadius-1, 1 do begin          ;Gaussian function     
        kernel[0,i] = float(exp(-0.5*i*i/sigma/sigma))       
    endfor
   
    if (kRadius < maxRadius && kRadius > 3) then begin   ;// edge correction
            sqrtSlope = double(1.7976931348623157E308);double((2-2^52)*2^1023); Double.MAX_VALUE;
            r = kRadius;
            while (r gt kRadius/2) do begin
                r--;
                a = sqrt(kernel[0, r])/(kRadius-r)
                if (a lt sqrtSlope) then sqrtSlope = a else break

            endwhile
            for r1 = r+2, kRadius-1, 1 do begin
                kernel[0, r1] = float(((kRadius-r1)*(kRadius-r1)*sqrtSlope*sqrtSlope));
            endfor
        endif
        
        sum = 0   ;// sum over all kernel elements for normalization
        if (kRadius lt maxRadius) then begin
            sum = kernel[0,0];
            for i=1, kRadius-1, 1 do begin
                sum += 2*kernel[0,i];
            endfor
        endif else begin
            sum = sigma * sqrt(2*!pi);
        endelse
        
        rsum = 0.5 + 0.5*kernel[0, 0]/sum;
        for i=0, kRadius-1, 1 do begin ; i++) {
            v = double(kernel[0,i]/sum);
            kernel[0,i] = float(v);
            rsum -= v;
            kernel[1,i] = float(rsum);
            ;//IJ.log("k["+i+"]="+(float)v+" sum="+(float)rsum);
        endfor
 
        print, kernel

    return, kernel
end

function C_sImageFilter_GaussianKernel_FRAP::printGaussValue, x = x, sigma = sigma
   normConst = 1 / double(sqrt(2*!pi)*sigma)
   return, normConst * exp(- (x^2. / (2.*sigma*sigma)))
end


;function C_sImageFilter_GaussianKernel_FRAP::diffuse, image = image, mask = mask, kernel = kernel
;
;  ; get dimensions of image and kernel
;  dimI = size(image, /dim)
;  dimK = size(kernel, /dim)
;  result = make_array(dimI)
;  
;  if (dimK[0] ne 1) then begin
;      pixRad = floor(dimK[0]/2)
;      paddedImage = s_expand_mirror(image, pixRad)
;      paddedMask = s_expand_mirror(mask, pixRad)
;    endif else begin
;      pixRad = 0
;      paddedImage = image
;      paddedMask = mask
;    endelse
;  
;  dimIpad = size(paddedImage, /dim)
;  tempResult = make_array(dimIpad)
;  
;  tempPad = make_array(dimK)
;  
;  ; finds locations where we need to apply the kernel
;  inROI = where(mask ne 0, count)
;  ; finds edges where we need to apply boundary conditions
;;  edges = roberts(paddedMask)
;;  onEdge = where(edges ne 0, count1)
;;  ind = array_indices(paddedMask, edges)
;  
;  findNeighbors =  s_ObjBorder_4or8(mask, eight = eight)
;  findEdges = laplacian(paddedImage);
;  ; to find edges in segmentation
;  inverseMask[where(paddedMask)] = 0 
;  edges = paddedMask
;  edges[where(inverseMask * paddedMask)] = 1
;  
;  for i = 0, dimIpad[0]-1-dimK[0] do begin
;    ; find local extremum indices
;;    lclmin = extrema( reform(paddedMask[i,*],dimIPad[1]) )
;;    lclmax = extrema( reform(paddedMask[i,*], dimIPad[1]))
;    
;    for j = 0, dimIpad[1]-1-dimK[0] do begin
;        ; ! todo : handle edges of mask
;        ; handle mass conservation
;        ; pad image in order to convolve when kernel grows in time
;        ;tempPad = paddedImage[ i:i+dimK[0]-1 ,j:j+dimK[0]-1 ]* paddedMask[ i:i+dimK[0]-1 ,j:j+dimK[0]-1 ]
;        tempPad = paddedImage[ i:i+dimK[0]-1 ,j:j+dimK[0]-1 ]* paddedMask[ i:i+dimK[0]-1 ,j:j+dimK[0]-1 ]
;
;        tempResult[i,j] = convol(tempPad, kernel, /edge_truncate) 
;        
;        ; handle boundaries
;        if(edges[i,j] eq 1) then begin
;;          tempResult[i,j] = 
;        endif
;     
;    
;    endfor
;  endfor
;
;    ;tempResult = convol(paddedImage * paddedMask , kernel, /edge_truncate, /center) 
;  
;    ; get the same dimensions as the original image
;    result = tempResult[ pixRad:dimIpad[0]-1-pixRad , pixRad:dimIpad[1]-1-pixRad ]
;   return, result
;end


function C_sImageFilter_GaussianKernel_FRAP::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y
 
   

   ; Get Filter Parameters, Values and Active-State
                      
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Generate_FRAP_Region'))[0]
   doFRAPRegion = (*(*self.pParamStruct).pValues)[whParam]   

   whParam = (where((*(*self.pParamStruct).pNames) eq 'FRAP_Region_x-center [pix]'))[0]
   xcFRAP = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'FRAP_Region_y-center [pix]'))[0]
   ycFRAP = (*(*self.pParamStruct).pValues)[whParam]   
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'FRAP_Region_Radius (2sig) [pix]'))[0]
   radFRAP = (*(*self.pParamStruct).pValues)[whParam]    
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Measure_Intensities'))[0]
   doMeasureInt = (*(*self.pParamStruct).pValues)[whParam]   

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Measure_Intensities_NumOfIntervals'))[0]
   numOfIntervals = (*(*self.pParamStruct).pValues)[whParam]   
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Do_Photobleaching_Correction'))[0]
   doPhotoBleachCorrection = (*(*self.pParamStruct).pValues)[whParam]   

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Photobleaching_Correction_Region_xMin [pix]'))[0]
   xMinPhoto = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Photobleaching_Correction_Region_xMax [pix]'))[0]
   xMaxPhoto = (*(*self.pParamStruct).pValues)[whParam]   
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Photobleaching_Correction_Region_yMin [pix]'))[0]
   yMinPhoto = (*(*self.pParamStruct).pValues)[whParam]      

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Photobleaching_Correction_Region_yMax [pix]'))[0]
   yMaxPhoto = (*(*self.pParamStruct).pValues)[whParam]    
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Fit_Find_Diffusion_Constant'))[0]
   doFitCalculateDiff = (*(*self.pParamStruct).pValues)[whParam]   

   whParam = (where((*(*self.pParamStruct).pNames) eq 'FRAP_Begin [frame]'))[0]
   beginFrameFRAP = (*(*self.pParamStruct).pValues)[whParam]  

   whParam = (where((*(*self.pParamStruct).pNames) eq 'FRAP_End [frame]'))[0]
   endFrameFRAP = (*(*self.pParamStruct).pValues)[whParam] 
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Model_Diffusion_Kernel'))[0]
   doDiffKernel = (*(*self.pParamStruct).pValues)[whParam]      
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Diffusion_Coeff_[um2s-1]'))[0]
   diffCoeff = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Diffusion_Begin [frame]'))[0]
   timeDiffBegin = floor((*(*self.pParamStruct).pValues)[whParam])
 
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Diffusion_End [frame]'))[0]
   timeDiffEnd = floor((*(*self.pParamStruct).pValues)[whParam])
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Find_Diffusion_Model_vs_Images'))[0]
   doModel = (*(*self.pParamStruct).pValues)[whParam]

   
   ; MOR - 21Feb2011 - get pixel and time interval info.
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
   whereDim = (where((*(*pParamStruct).pNames) eq 'Stack Path'))[0]
   if (whereDim ne -1) then fnDir = *(((*pParamStruct).pValues)[whereDim]) else fnDir = s_getPathForSystem()
   

   xSizeRatio = pixelXdim / realXdim
   ySizeRatio = pixelYdim / realYdim
   
   xPixSize = realXdim/pixelXdim
   
   whereDim = (where((*(*pParamStruct).pNames) eq 'Time Interval [s]'))[0]
   if (whereDim ne -1) then realTimeInt = *(((*pParamStruct).pValues)[whereDim]) else realTimeInt = 1
   
   ; define incoming image as the segmentation
   imageMask = image 
;   imageMask = image[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
   
   ; get intensity image that corresponds to mask
   imageFull = selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
   imageCut = imageFull[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
   dimI = size(imageFull, /dim)
   dimICut = size(imageCut, /dim)

   ; check if the FRAP region option has been selected, if so, generate mask with FRAP region specifications
   if(doFRAPRegion) then begin
        circMask = bytarr(dimI)
        pDrawObj = ptr_new(/no_copy)
        pDrawObj = obj_new('C_sFrapCircle')
        pDrawObj->SetProperty, x=xcFRAP, y = ycFRAP, r = radFRAP
        circMask = pDrawObj->GetCircle(rad1 = radFRAP,pMaskData = imageFull)
        circMask = circMask[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
   endif
    
   if(doMeasureInt) then begin
    
    if(obj_valid(pDrawObj) eq 0) then begin
        circMask = bytarr(dimI)
        pDrawObj = ptr_new(obj_new('C_sFrapCircle'), /no_copy)
        pDrawObj->SetProperty, x=xcFRAP, y = ycFRAP, r = radFRAP
        circMask = pDrawObj->GetCircle(rad1 = radFRAP,pMaskData = imageFull)
        circMask = circMask[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
     endif    
       pDrawObj->SetProperty, dr = numOfIntervals
       ; can generate something to pass in the distance map here (pDistMatrix)
       ; intRad - vector with mean radius of concentric circle measurements
       ; intCon - vector with mean intensities in concentric circle averaged
       ; intCount - vector with number of pixels used in each concentric circle
       DistMatrix =  fltarr(dimICut) ; this currently gets filled inside GetIntensities, but could
       ; be commented out and passed in instead
       pDrawObj->GetIntensities, ImageData = imageCut,$
                                 DistMatrix = DistMatrix, MaskData = imageMask,$
                                 intRad = intRad, intCon = intCon, intCount = intCount                              
   endif
   
   image *= 1.    ; Image values double precision

  ; to convert from real units to pix and frame units
  convFactor = (realTimeInt /(xPixSize^2))
    
;  for k = timeBegin, totalTNum-1 do begin
       ; result should be in pixels
      
      sigma = sqrt(diffCoeff*convFactor)  ; Calculate Sigma of Gauss distribution for FilterSize
      ; MOR- why are we allowing for a non symmetric kernel -- should make this odd if it is even?
      filterSize = round(2.* sqrt(diffCoeff*convFactor)); sigma = sqrt(4*D*t) & sigma = filtersize/4
      if((filterSize mod 2) eq 0) then filterSize++ 
      if (filterSize lt 3) then filterSize = 3  
    
      kernel = dblArr(filterSize, filterSize)

 
   ; Construct Filter (filterSize x filterSize) and distribute Gauss values
   sigma = filterSize / 4.  ; Calculate Sigma of Gauss distribution for FilterSize
   kernel = dblArr(filterSize, filterSize)  
      for i = 0, filterSize-1, 1 do begin
         for j = 0, filterSize-1, 1 do begin
          d = sqrt( (i-floor(filterSize/2))^2 + (j-floor(filterSize/2))^2 )     
          g = self->printGaussValue(x = d , sigma = sigma)
          kernel[i,j] = g    
         endfor    
      endfor
      kernel /=(total(kernel))

     
     ;tPos_end
     pParamStruct = selectedStackObject->getpParamStruct()
     totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]
     
     imageCorr = make_array(totalTimes, /float)
     
     for i = tPos, totalTimes-1 do begin
       imageFull = selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = zPos)
       
       imageCut = imageFull[xMinPhoto[0]:xMaxPhoto[0], yMinPhoto[0]:yMaxPhoto[0]]
       
       imageCorr[i] = mean(imageCut)
     endfor
     
     path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
     name = strCompress('meanIntensity' + '.dat', /rem)
     filename = strCompress(path + name)
     
     OPENW, 1, filename
     PRINTF, 1, imageCorr
     close, /all
     
     X = float(indgen(totalTimes)) 

     ;Provide an initial guess of the function's parameters. 
     A = [10.0, 160.0, 2.0] 
     weights = make_array(totalTimes, /float, value=1.0)
     
     ;Compute the parameters. 
     yfit = curvefit(X, imageCorr, weights, A, FUNCTION_NAME='gfunct') 
     print, 'Function parameters: ', A 
     
     return,image     
end


function C_sImageFilter_GaussianKernel_FRAP::init

    filterStruct = {Name: 'C_GaussianKernel_FRAP',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of Identity.

    filterParamWidgetType = ['widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', $
                        'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider']
    filterParamActive = [1,1,1,1,1, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0]
    ; MOR - 29Jun2011 - 19 options thus far - some are for indicating active options, others for obtaining user values
    filterParamNames = ['Generate_FRAP_Region','FRAP_Region_x-center [pix]','FRAP_Region_y-center [pix]','FRAP_Region_Radius (2sig) [pix]',$
                        'Measure_Intensities','Measure_Intensities_NumOfIntervals',$
                        'Do_Photobleaching_Correction','Photobleaching_Correction_Region_xMin [pix]','Photobleaching_Correction_Region_xMax [pix]',$
                        'Photobleaching_Correction_Region_yMin [pix]','Photobleaching_Correction_Region_yMax [pix]',$
                        'Fit_Find_Diffusion_Constant','FRAP_Begin [frame]','FRAP_End [frame]',$
                        'Model_Diffusion_Kernel','Diffusion_Coeff_[um2s-1]','Diffusion_Begin [frame]','Diffusion_End [frame]',$
                        'Find_Diffusion_Model_vs_Images']
    filterParamMin = [0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0]
    filterParamMax = [1,1500,1500,1500,1, 1000,1,1000,1000,1000, 1,1000,1000,1,1, 1000,1000,1000,1]
    filterParamValues = [1,10,10,5,1, 4,0,20,25,20, 25,0,2,3,0, 0.1,3,4,0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_GaussianKernel_FRAP__define
  tmp = {C_sImageFilter_GaussianKernel_FRAP, pParamStruct: ptr_new(), inherits C_sImageFilter}
end