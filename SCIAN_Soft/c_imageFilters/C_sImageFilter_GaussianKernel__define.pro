;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_GaussianKernel
;
; PURPOSE:
;       - Identity-Filter-Class. See Identity.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2011)
;     e_mail: shaertel@physik.uni-bremen.de
;     Jan Scheer (2011)
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_GradientImage' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_GaussianKernel::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_GaussianKernel::checkParamsApplied
; compares the parameter values between the current values and the values applied in the last segmentation
   if ptr_valid(self.pParamApplied) then begin
      if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
         *self.pParamApplied = *(*self.pParamStruct).pValues
         return, 1
      endif
   endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
   return, 0
end



function C_sImageFilter_GaussianKernel::printGaussValue, x = x, sigma = sigma
   normConst = 1 / double(sqrt(2*!pi)*sigma)
   return, normConst * exp(- (x^2. / (2.*sigma*sigma)))
end



function C_sImageFilter_GaussianKernel::printLOGValue, x = x, y = y, sigma = sigma
   ; http://homepages.inf.ed.ac.uk/rbf/HIPR2/log.htm
   normConst = 1 / (double(!pi*(sigma^4)))
   exponent = - (x^2+y^2) / (2*sigma*sigma)
   factor = exp(exponent)
   factor2 = (1 - (   (x^2 + y^2) / (2*sigma*sigma) ))
   result = normConst * factor2 * factor  
   return, result
end





function C_sImageFilter_GaussianKernel::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

   

   ; Get Filter Parameters, Values and Active-State
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Filter_Size'))[0]
   filterSize = (*(*self.pParamStruct).pValues)[whParam]
   filterSize = round(filterSize)
    (*(*self.pParamStruct).pValues)[whParam] = filterSize
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Eccentricity'))[0]
   ecc = (*(*self.pParamStruct).pValues)[whParam]
   eccActive = (*(*self.pParamStruct).pActive)[whParam]
   ; fEccA.... 
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Rotation_Theta'))[0]
   theta = (*(*self.pParamStruct).pValues)[whParam]
   theta = round(theta)
   thetaActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Max_Theta'))[0]
   thetaMax = (*(*self.pParamStruct).pValues)[whParam]
   thetaMaxActive = (*(*self.pParamStruct).pActive)[whParam]
   
   if(thetaMaxActive && thetaMax gt 0 && theta eq 0) then begin
      theta = 15
      whParam = (where((*(*self.pParamStruct).pNames) eq 'Rotation_Theta'))[0]
      (*(*self.pParamStruct).pValues)[whParam] = 15
   endif
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Gauss_Tubulus'))[0]
   tubulus = (*(*self.pParamStruct).pValues)[whParam]
   tubulusActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Show_Kernel'))[0]
   showKernelActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Gradient_Kernel'))[0]
   gradientKernelActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Laplace_Kernel'))[0]
   laplaceActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'LoG_Kernel'))[0]
   logActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Invert_Kernel'))[0]
   invert = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Filter_Size'))[0]
   ; Allow Only Odd Filter Size (Set Filter on User Interface (Feedback)
   if((laplaceActive eq 0) && (filterSize MOD 2 eq 0)) then begin
      filterSize +=1
      (*(*self.pParamStruct).pValues)[whParam] = filterSize
   endif
   
   ; LOG odd
   if(logActive ne 0) then begin
      if(filterSize MOD 2 eq 0) then filterSize +=1
   endif
   ; Laplace may only have odd filtersize and modulo 3 compatible
   if(laplaceActive && ((filterSize MOD 3 ne 0) || (filterSize MOD 2 eq 0))  ) then begin  
      ydelta = filterSize MOD 3      
      if((filterSize+ydelta) MOD 3 eq 0) then filterSize += ydelta else filterSize -=ydelta    
      if(filterSize MOD 2 eq 0) then filterSize += 3
       (*(*self.pParamStruct).pValues)[whParam] = filterSize
   endif
   ; Make it Sure!!!
   if(tubulusActive) then begin
      laplaceActive = 0
      whParam = (where((*(*self.pParamStruct).pNames) eq 'Laplace_Kernel'))[0]
      (*(*self.pParamStruct).pActive)[whParam] = 0
      gradientKernelActive = 0
      whParam = (where((*(*self.pParamStruct).pNames) eq 'Gradient_Kernel'))[0]
      (*(*self.pParamStruct).pActive)[whParam] = 0
      logActive = 0
      whParam = (where((*(*self.pParamStruct).pNames) eq 'LoG_Kernel'))[0]
      (*(*self.pParamStruct).pActive)[whParam] = 0
   endif
   
   if(gradientKernelActive) then begin
      laplaceActive = 0
      whParam = (where((*(*self.pParamStruct).pNames) eq 'Laplace_Kernel'))[0]
      (*(*self.pParamStruct).pActive)[whParam] = 0
      logActive = 0
      whParam = (where((*(*self.pParamStruct).pNames) eq 'LoG_Kernel'))[0]
      (*(*self.pParamStruct).pActive)[whParam] = 0
   endif
   
   if(logActive) then begin
      laplaceActive = 0
      whParam = (where((*(*self.pParamStruct).pNames) eq 'Laplace_Kernel'))[0]
      (*(*self.pParamStruct).pActive)[whParam] = 0
   endif
    if(laplaceActive) then begin
      logActive = 0
      whParam = (where((*(*self.pParamStruct).pNames) eq 'LoG_Kernel'))[0]
      (*(*self.pParamStruct).pActive)[whParam] = 0
   endif
   
   
   
   
   
   
   ; check the input image dimensions
   if (size(image, /n_dim) ne 2) or (max(image) eq 0) then return, image
   szI = size(image, /dim)
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Keep Segmented Image'))[0]
    if (whParam eq -1) then fKeepSegImage = 0b else $
       if not((*(*self.pParamStruct).pActive)[whParam]) then fKeepSegImage = 0b else fKeepSegImage = (*(*self.pParamStruct).pValues)[whParam]

      ; if parameters/input image unchanged since the last segmentation appliance of this filter,
      ; there's no need to spend time computing the contours again and return the previosly segmented image

       if ( not(self->checkParamsApplied()) and ptr_valid(self.pSegImage) and fKeepSegImage) then begin
           szSeg = size(*self.pSegImage, /dim)
           print, tPos
           if (n_elements(szSeg) gt 1) then if ((szI[0] eq szSeg[0]) and (szI[1] eq szSeg[1])) then return, *self.pSegImage
       endif

    
    ; create the mask image for this filter
     ;if ptr_valid(self.pSegImage) then *self.pSegImage = bytArr(szI[0], szI[1]) $
     ;else self.pSegImage = ptr_new(bytArr(szI[0], szI[1]), /no_copy)  
  
   
   if ( (fKeepSegImage and (not(ptr_valid(self.pSegImage)))) or (not(fKeepSegImage)) ) then begin ; calculate if previous calculated content does not exist or if "keep image = 0"
   ;---------------------------------------------------------------------------------------------
   ; Print Filter Size
   print, 'Filter Size: ', filterSize
   ; Construct Filter (filterSize x filterSize) and distribute Gauss values
   sigma = filterSize / 4.  ; Calculate Sigma of Gauss distribution for FilterSize
   kernel = dblArr(filterSize, filterSize)  
   laplacekernel = dblArr(filterSize, filterSize)
   if(logActive eq 0) then begin
      for i = 0, filterSize-1, 1 do begin
         for j = 0, filterSize-1, 1 do begin
          d = sqrt( (i-floor(filterSize/2))^2 + (j-floor(filterSize/2))^2 )     
          g = self->printGaussValue(sigma = sigma, x = d)
          kernel[i,j] = g    
         endfor    
      endfor
      kernel /=(total(kernel))
    endif else begin
        for i = 0, filterSize-1, 1 do begin
         for j = 0, filterSize-1, 1 do begin
          d = sqrt( (i-floor(filterSize/2))^2 + (j-floor(filterSize/2))^2 )     
           g = self->printLOGValue(sigma = sigma, x = d, y = d)
          kernel[i,j] = g    
         endfor    
      endfor
    endelse
    
   if(invert) then kernel = -kernel
   ; Normalize Kernel (Total Sum is 1)   
      
    
    ; 
       
   ;---------------------------------------------------------------------------------------------
   ; Print Eccentricity   
   print, 'Eccentricity: ', ecc
   ; Construct vertically compressed Fitler according to Eccentricity value 
   if(ecc ge 0) then begin   ;  && eccActive eq 1
      x = ceil((1-ecc)*filterSize) ; Width of new distribution
      if( x MOD 2 eq 0) then x += 1 ; Width must be odd value
      ; Compress Gauss values to desired Filter Eccentricity (obtain center; use interpolation)    
      kernel = congrid(kernel, x, filterSize, /CENTER, /INTERP)
      
      if(laplaceActive) then begin       
         ; normalize compressed kernel for further laplace construction
         kernel /= total(kernel) 
         y = round(filterSize/3)
         
         lKernel = congrid(kernel, x, y, /CENTER, /INTERP)
         lKernel /= total(lKernel)      
         offsetX1 = round((filterSize-x)/2)
         offsetX2 = round(offsetX1+x-1)
         offsetY1 = y
         offsetY2 = 2*y-1
         laplaceKernel[offsetX1:offsetX2 ,offsetY1:offsetY2] = lKernel
         ; negative kernel part
         nKernel = -lKernel*0.5
         ; mirror
         offsetY1 = 0
         offsetY2 = y-1
         laplaceKernel[offsetX1:offsetX2 ,offsetY1:offsetY2] = nKernel
         
         offsetY1 = 2*y
         offsetY2 = round(filterSize-1)
         laplaceKernel[offsetX1:offsetX2 ,offsetY1:offsetY2] = nKernel
         kernel = laplacekernel
         kernel = kernel*0.5
      endif else begin
         
         if(ecc gt 0) then begin; Normalize Kernel (Total Sum is 1)  
         kernel /= total(kernel)   
         ; Construct Filter (filterSize x filterSize) and seed compressed Gauss distribution
         newKernel = dblArr(filterSize, filterSize)
         offsetX1 = round((filterSize-x)/2)
         offsetX2 = round(offsetX1+x-1)
    
         newKernel[offsetX1:offsetX2 , *] = kernel
         kernel = newKernel
         ; Normalize
         kernel /= total(kernel) 
         
         endif
         
      endelse
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

   ;---------------------------------------Symmetric Kernel :: Gradient Like Kernel------------------------------------   
   if(gradientKernelActive) then begin
      kernel(*, center)= 0
      degree = 360.
      kernel(*, center:*)= -kernel(*, center:*)
   endif else begin
      degree = 180.
   endelse
   
         
   ;------------------------------------------- Rrrrotate (Sinlge Time and Normalize)------------------------------------------------------------
   ; 1 Time Kernel Rotation only
   if(theta gt 0 && thetaActive && thetaMaxActive eq 0) then begin      
       ; Rotate by theta, obtain the center, interpolate
       kernel = rot(kernel, theta, 1.0, center, center, /INTERP)
       
       ; Normalize kernel after rotation
       if(laplaceActive eq 0 && gradientKernelActive eq 0 && logACtive eq 0) then begin
         kernel /= total(kernel)  ; Only Gauss
       endif else begin
         ; Special Normalization
         neg_ind = where(kernel lt 0)
         pos_ind = where(kernel gt 0)
         
         neg_nums = kernel(neg_ind)
         pos_nums = kernel(pos_ind)
         
         neg_nums /= -total(neg_nums)
         pos_nums /= total(pos_nums)
         
         kernel(neg_ind) = neg_nums
         kernel(pos_ind) = pos_nums    
       endelse
       
   endif
   
   imageCov=0
   ;------------------------------------------------Rrrrrrotate in Steps and Normalize) ---------------------------------------------
   image *= 1.    ; Image values double precision
   if(thetaMax gt 0 && thetaMaxActive eq 1) then begin

       ; Keep original image and kernel for convolution and rotation
       imageOrg = image
       kernelOrg = kernel
      ; Begin rotation convolution
       for i = 0, degree-1, theta do begin
         ; (1) Rotate kernel
         kernel = rot(kernelOrg, i, 1.0, center, center, /INTERP)
         
         ; (2) Normalize kernel after rotation
             if(laplaceActive eq 0 && gradientKernelActive eq 0 && logActive eq 0) then begin
               kernel /= total(kernel)  ; Only Gauss
             endif else begin
               ; Special Normalization
               neg_ind = where(kernel lt 0)
               pos_ind = where(kernel gt 0)
               
               neg_nums = kernel(neg_ind)
               pos_nums = kernel(pos_ind)
               
               neg_nums /= -total(neg_nums)
               pos_nums /= total(pos_nums)
               
               kernel(neg_ind) = neg_nums
               kernel(pos_ind) = pos_nums    
             endelse
             
         ; (3) Show Kernel in Window
         if(showKernelActive eq 1) then begin
             window, 12, xsize=200, ysize = 200
             tvscl, congrid(kernel, 200,200)
         endif      
         print, 'Kernel Rotation [° Theta]: ', i    
         print, 'Kernel Sum: ', total(kernel)
         ; (4) Copy the maximum values of the convolved image with the rotated gaussian in orignal image
         if(i eq 0) then begin
            imageCov = convol(imageOrg, kernel, /EDGE_TRUNCATE)
         endif else begin
            imageCov >= convol(imageOrg, kernel, /EDGE_TRUNCATE)    
         endelse
       endfor   
     endif else begin
         print, 'Kernel Rotation [° Theta]: ', theta
         print, 'Gaussian Kernel: '
         print, 'Kernel Sum: ', total(kernel)
         ; Show Kernel in Window
         if(showKernelActive eq 1) then begin
             window, 12, xsize=200, ysize = 200
             tvscl, congrid(kernel, 200,200)
         endif
         ; Convolve image with kernel (simply rotated or not)
         imageCov = convol(image, kernel, /EDGE_TRUNCATE) 
         
     endelse

     
 endif

  
   
  
    ; Return Pointer on Calculated Image   or calculated image
    if fKeepSegImage then begin ; 
      if (ptr_valid(self.pSegImage)) then  *self.pSegImage = imageCov else self.pSegImage = ptr_new(imageCov) ; Construct Pointer with calculated content
      return, *self.pSegImage
   endif else begin ; return calculated image
      return, imageCov 
   endelse 

end


function C_sImageFilter_GaussianKernel::init

    filterStruct = {Name: 'C_GaussianKernel',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of Identity.
    filterParamWidgetType = ['widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider','widget_slider', 'widget_slider', 'widget_slider']
    filterParamNames = ['Filter_Size', 'Eccentricity', 'Rotation_Theta', 'Max_Theta', 'Gauss_Tubulus', 'Show_Kernel', 'Gradient_Kernel', 'Laplace_Kernel', 'LoG_Kernel', 'Invert_Kernel', 'Keep Segmented Image']
    filterParamActive = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    filterParamMin = [3, 0., 0, 0, 0, 0, 0, 0, 0, 0, 1]
    filterParamMax = [50, 1., 360, 1, 1, 1, 0, 0, 1, 1, 1]
    filterParamValues = [3, 0., 0, 1, 0, 0, 0, 0, 0, 0, 0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    ;self.pParamApplied = ptr_new(ImageFilterStruct, /no_copy);
    ;self.pSegImage = ptr_new(ImageFilterStruct, /no_copy);
    return, 1
end

pro C_sImageFilter_GaussianKernel__define
  tmp = {C_sImageFilter_GaussianKernel, pParamStruct: ptr_new(), pParamApplied: ptr_new(), pSegImage: ptr_new(), inherits C_sImageFilter}
end