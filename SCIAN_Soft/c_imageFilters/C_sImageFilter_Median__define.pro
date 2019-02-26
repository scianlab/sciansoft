;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Median
;
; PURPOSE:
;       - Median-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Median' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_Median::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Median::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    arraySize = (*(*self.pParamStruct).pValues)[0]
    expSize = floor(arraySize/2.)
    dimI = size(image, /dim)
    exp_image = s_Expand_Mirror(image, expSize)

    return, (median(exp_image, 2 > arraySize, /even))[expSize : expSize + dimI[0]-1, expSize : expSize + dimI[1]-1 ]
;   return, (median(exp_image, 2 > arraySize))[expSize : expSize + dimI[0]-1, expSize : expSize + dimI[1]-1 ]
end


function C_sImageFilter_Median::init
    filterStruct = {Name: 'C_Median',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}         ; Pointer on Filter Parameter Values.

    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['Median_Voxel']
    filterParamActive = [1]                        ;   (always active)
    filterParamMin = [2]
    filterParamMax = [100]
    filterParamValues = [2]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Median__define
  tmp = {C_sImageFilter_Median, pParamStruct: ptr_new(), inherits C_sImageFilter}
end