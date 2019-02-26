;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_GaussianBlur
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

function C_sImageFilter_GaussianBlur::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end



function C_sImageFilter_GaussianBlur::makeGaussianKernel, sigma = sigma, accuracy = accuracy, maxRadius = maxRadius

    
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

function C_sImageFilter_GaussianBlur::printGaussValue, x = x, sigma = sigma
   normConst = 1 / (double(sqrt(2*!pi)*sigma))
   exponent = - (x^2 / (2*sigma*sigma))
   factor = exp(exponent)
   result = normConst * factor  
   return, result
end




function C_sImageFilter_GaussianBlur::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y
   image -= 13
   ind = where(image lt 0, count)
   if (count gt 0) then image[ind]=0
   ; Get Filter Parameters, Values and Active-State
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Filter_Size'))[0]
   filterSize = (*(*self.pParamStruct).pValues)[whParam]
   ; Allow Only Odd Filter Size (Set Filter on User Interface (Feedback)
   if(filterSize MOD 2 eq 0) then begin
      filterSize +=1
      (*(*self.pParamStruct).pValues)[whParam] = filterSize
   endif
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Eccentricity'))[0]
   ecc = (*(*self.pParamStruct).pValues)[whParam]
   eccActive = (*(*self.pParamStruct).pActive)[whParam]
    
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Rotation_Theta'))[0]
   theta = (*(*self.pParamStruct).pValues)[whParam]
   thetaActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Max_Theta'))[0]
   thetaMax = (*(*self.pParamStruct).pValues)[whParam]
   thetaMaxActive = (*(*self.pParamStruct).pActive)[whParam]
   
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Gauss_Tubulus'))[0]
   tubulus = (*(*self.pParamStruct).pValues)[whParam]
   tubulusActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Show_Gauss_Kernel'))[0]
   showKernelActive = (*(*self.pParamStruct).pActive)[whParam]
   
   ;---------------------------------------------------------------------------------------------
   ; Print Filter Size
   print, 'Filter Size: ', filterSize
   ; Construct Filter (filterSize x filterSize) and distribute Gauss values
   sigma = filterSize / 4.  ; Calculate Sigma of Gauss distribution for FilterSize
   kernel = dblArr(filterSize, filterSize)  
   for i = 0, filterSize-1, 1 do begin
      for j = 0, filterSize-1, 1 do begin
       d = sqrt( (i-floor(filterSize/2))^2 + (j-floor(filterSize/2))^2 )     
       g = self->printGaussValue(sigma = sigma, x = d)
       kernel[i,j] = g    
      endfor    
   endfor
   ; Normalize Kernel (Total Sum is 1)   
   kernel /=(total(kernel))   
    
    
   ;---------------------------------------------------------------------------------------------
   ; Print Eccentricity   
   print, 'Eccentricity: ', ecc
   ; Construct vertically compressed Fitler according to Eccentricity value 
   if(ecc gt 0 && eccActive eq 1) then begin
      x = ceil((1-ecc)*filterSize) ; Width of new distribution
      if( x MOD 2 eq 0) then x += 1 ; Width must be odd value
      ; Compress Gauss values to desired Filter Eccentricity (obtain center; use interpolation)    
      kernel = congrid(kernel, x, filterSize, /CENTER, /INTERP)
      ; Normalize Kernel (Total Sum is 1)  
      kernel /= total(kernel)   
      ; Construct Filter (filterSize x filterSize) and seed compressed Gauss distribution
      newKernel = dblArr(filterSize, filterSize)
      offsetX1 = round((filterSize-x)/2)
      offsetX2 = round(offsetX1+x-1)
      offsetY2 = round(filterSize-1) 
      newKernel[offsetX1:offsetX2 , *] = kernel
      kernel = newKernel
      ; Normalize
      kernel /= total(kernel) 
   endif
   
   ;--------------------------------------------------------------------------------------------- 
      
   ; Center of Filter
   center = floor(filterSize/2) 
   
   ; Tubulus Form   
   if(tubulusActive) then begin
      centerRow = kernel(*,center)
      offset = filterSize-1
      for k = 0, filterSize-1, 1 do begin
      
         kernel(0:offset, k)= centerRow
      endfor
   endif
   

   ; Simple Kernel Rotation
   if(theta gt 0 && thetaActive && thetaMaxActive eq 0) then begin      
       ; Rotate by theta, obtain the center, interpolate
       kernel = rot(kernel, theta, 1.0, center, center, /INTERP)
       ; Normalize
       kernel /= total(kernel)  
   endif
   

   
   ;---------------------------------------------------------------------------------------------
   image *= 1.    ; Image values double precision
   if(thetaMax gt 0 && thetaMaxActive eq 1) then begin
       ; Number of Theta Rotation Steps
       sizeTheta = 180/theta       
       if(sizeTheta gt 180) then sizeTheta=1 ; Max Theta 180
       ; Create Theta Angulars in Array
       thetaArr = intArr(sizeTheta)
       for u = 0, sizeTheta-1, 1 do begin
         thetaArr(u) = u * theta
       endfor
       ; Keep original image and kernel for convolution and rotation
       imageOrg = image
       kernelOrg = kernel
       for i = 0, sizeTheta-1, 1 do begin
         ; Rotate kernel & Normalize
         kernel = rot(kernelOrg, thetaArr(i), 1.0, center, center, /INTERP)
         kernel /= total(kernel)  
         
         ; Show Kernel in Window
         if(showKernelActive eq 1) then begin
             window, 12, xsize=400, ysize = 400
             tvscl, congrid(kernel, 400,400)
         endif
         
         print, 'Kernel Rotation [° Theta] :', thetaArr(i)
         ;print, 'Gaussian Kernel: '
         ;print, kernel   
         
         ; Copy the maximum values of the convolved image with the rotated gaussian in orignal image
         image >= convol(imageOrg, kernel, /EDGE_TRUNCATE) 
       endfor   
 
     endif else begin
         print, 'Kernel Rotation [° Theta] :', theta
         print, 'Gaussian Kernel: '

         ; Show Kernel in Window
         if(showKernelActive eq 1) then begin
             window, 12, xsize=400, ysize = 400
             tvscl, congrid(kernel, 400,400)
         endif
         ; Convolve image with kernel (simply rotated or not)
         image = convol(image, kernel, /EDGE_TRUNCATE) 
     endelse
   
   return, image
end


function C_sImageFilter_GaussianBlur::init

    filterStruct = {Name: 'C_GaussianBlur',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of Identity.
    filterParamWidgetType = ['widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider']
    filterParamNames = ['Filter_Size', 'Eccentricity', 'Rotation_Theta', 'Max_Theta', 'Gauss_Tubulus', 'Show_Gauss_Kernel']
    filterParamActive = [1, 0, 0, 0, 0, 0]
    filterParamMin = [3, 0., 0, 0, 0, 0]
    filterParamMax = [50, 1., 180, 1, 1, 1]
    filterParamValues = [3, 0., 0, 1, 0, 0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_GaussianBlur__define
  tmp = {C_sImageFilter_GaussianBlur, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
