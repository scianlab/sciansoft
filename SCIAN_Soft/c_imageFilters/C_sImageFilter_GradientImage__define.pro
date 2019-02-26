;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_GradientImage
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
; METHODS:
;   function ->apply, pImageData = pImageData      ;pImageData   Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_GradientImage::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_GradientImage::SobelGradientX, image = image
    vector1x = [-1, 0, +1]
    vector2x = [-2, 0, +2]
    vector3x = [-1, 0, +1]    
    kernelX = [[vector1x], [vector2x], [vector3x]] 
    filteredX = Convol(FLOAT(image), kernelX, Center=1)
    return, filteredX
end

function C_sImageFilter_GradientImage::SobelGradientY, image = image
    vector1y = [1, 2, 1]
    vector2y = [0, 0, 0]
    vector3y = [-1, -2, -1]  
    kernelY = [[vector1y], [vector2y], [vector3y]] 
    filteredY = Convol(FLOAT(image), kernelY, Center=1)
    return, filteredY
end


function C_sImageFilter_GradientImage::GaussianSmoothing, image = image
    vector1 = [1, 4, 7, 4, 1]
    vector2 = [4, 16, 26, 16, 4]
    vector3 = [7, 26, 41, 26, 7]
    vector4 = [4, 16, 26, 16, 4]
    vector5 = [1, 4, 7, 4, 1]  
    kernelY = [[vector1], [vector2], [vector3], [vector4], [vector5]] 
    kernelY = FLOAT(kernelY) / FLOAT(273)
    filteredGaussian = Convol(FLOAT(image), kernelY, Center=1)
    return, BYTE(filteredGaussian)
end


function C_sImageFilter_GradientImage::SeparatedPixelDifferenceX, image=image
    vector1x = [0, 0, 0]
    vector2x = [-1, 0, 1]
    vector3x = [0, 0, 0]    
    kernelX = [[vector1x], [vector2x], [vector3x]] 
    filteredX = Convol(FLOAT(image), kernelX, Center=1)
    return, filteredX
end


function C_sImageFilter_GradientImage::SeparatedPixelDifferenceY, image=image
    vector1y = [0, 1, 0]
    vector2y = [0, 0, 0]
    vector3y = [0, -1, 0]  
    kernelY = [[vector1y], [vector2y], [vector3y]] 
    print, kernelY
    filteredY = Convol(FLOAT(image), kernelY, Center=1)
    return, filteredY
end


function C_sImageFilter_GradientImage::apply, image = image 
   
 
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Gaussian_Smoothing'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then begin 
      print, "gaussian smoothing"
      image = self->GaussianSmoothing(image=image)
     endif
   
    filtered = image      
    sobelImageX = 0 
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Gradient_X'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then begin 
      print, "calculate sobel gradient x"
      sobelImageX = self->SobelGradientX(image=image)
      filtered = abs(sobelImageX)
     endif
      
    sobelImageY = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Gradient_Y'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then begin 
      print, "calculate sobel gradient y"
      sobelImageY = self->SobelGradientY(image=image)
      filtered = filtered + abs(sobelImageY) 
     endif
    
    ;if(filtered eq 0) then begin
    ; filtered = image
    ;endif
     
    return, filtered
end


function C_sImageFilter_GradientImage::init

    filterStruct = {Name: 'C_GradientImage',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of Identity.
    filterParamWidgetType = ['widget_slider','widget_slider','widget_slider']
    filterParamNames = ['Gaussian_Smoothing','Gradient_X','Gradient_Y']
    filterParamActive = [0,1,1]
    filterParamMin = [0,0,0]
    filterParamMax = [1,1,1]
    filterParamValues = [0,0,0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_GradientImage__define
  tmp = {C_sImageFilter_GradientImage, pParamStruct: ptr_new(), inherits C_sImageFilter}
end