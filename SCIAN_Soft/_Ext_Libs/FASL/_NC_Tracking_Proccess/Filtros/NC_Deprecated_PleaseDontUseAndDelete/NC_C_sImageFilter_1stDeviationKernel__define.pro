;_____________________________IOISIOI____________________
; NAME:
;      NC_C_sImageFilter_1stDeviationKernel
;
; PURPOSE:
;       - 1stDeviation-Filter-Class. See 1stDeviation.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('NC_C_sImageFilter_1stDeviationKernel' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function NC_C_sImageFilter_1stDeviationKernel::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function NC_C_sImageFilter_1stDeviationKernel::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
    return,  'FASL_NC_Release_Filter_Method'
end

function NC_C_sImageFilter_1stDeviationKernel::apply, image = image
    image = bytScl(image, top = 255, /nan) 
    
    if (size(image, /n_dim) ne 2) then return, image

    dimI = size (image, /dim)
    wherePixRad = (where((*(*self.pParamStruct).pNames) eq 'Pixel_Radius'))[0]
    pixRad = round((*(*self.pParamStruct).pValues)[wherePixRad])

    kernel = make_array(2*pixRad+1, 2*pixRad+1, /float)
    for j = 0, 2*pixRad do for i = 0, 2*pixRad do kernel[i,j] = sqrt( (i-pixRad)^2 + (j-pixRad)^2 )

    kernel = -1. * (kernel lt (pixRad+1)) * (kernel ge pixRad)
    kernel[pixRad, pixRad] = -1. * total(kernel)

print, kernel

    alpha = convol(1.*image, kernel, center = 1, /edge_wrap)

    whereMinMax = (where((*(*self.pParamStruct).pNames) eq 'Image_*_Balance'))[0]
    if ((*(*self.pParamStruct).pActive)[whereMinMax] eq 1) then begin
       balanceFactor = (*(*self.pParamStruct).pValues)[whereMinMax]
       minA = min(alpha, max = maxA)
       minI = min(image, max = maxI)
       alpha = ((1./balanceFactor)>.001) * (1.*alpha - minA) / ((maxA - minA) > (1/256.))
       image = (1./((1.-balanceFactor)>.001)) * (1.*image - minI) / ((maxI - minI) > (1/256.))
       alpha *= image
    endif

    whereMinMax = (where((*(*self.pParamStruct).pNames) eq 'Image_+_Balance'))[0]
    if ((*(*self.pParamStruct).pActive)[whereMinMax] eq 1) then begin
       balanceFactor = (*(*self.pParamStruct).pValues)[whereMinMax]
       minA = min(alpha, max = maxA)
       minI = min(image, max = maxI)
       alpha = (1.*balanceFactor) * (1.*alpha - minA) / ((maxA - minA) > (1/256.))
       image = (1.-balanceFactor) * (1.*image - minI) / ((maxI - minI) > (1/256.))
       alpha += image
    endif
    return, bytScl(alpha, top = 255, /nan) 
    ;alpha
end


function NC_C_sImageFilter_1stDeviationKernel::init

    filterStruct = {Name: 'C_1stDeviationKernal',$     ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of 1stDeviation.
    filterParamWidgetType = make_array(3, /string, value = 'widget_slider')

    filterParamNames = ['Pixel_Radius', 'Image_*_Balance', 'Image_+_Balance']

    filterParamActive = [1,0,0]
    filterParamMin = [1.,0,0]
    filterParamMax = [100,1,1]
    filterParamValues = [1.,0,0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro NC_C_sImageFilter_1stDeviationKernel__define
  tmp = {NC_C_sImageFilter_1stDeviationKernel, pParamStruct: ptr_new(), inherits C_sImageFilter}
end