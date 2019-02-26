;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_IntensitySpan
;
; PURPOSE:
;       - CorrectImage-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_IntensitySpan' )
;
; METHOHDS:
;_____________________________IOISIOI____________________


function C_sImageFilter_IntensitySpan::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_IntensitySpan::apply, image = image

    if (size(image, /n_dim) ne 2) then return, image

    whParam = (where((*(*self.pParamStruct).pNames) eq '(X+)* or (X*)+'))[0]
       flag = (*(*self.pParamStruct).pActive)[whParam]
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Additive_Drift'))[0]
       addDrift = 1. * (*(*self.pParamStruct).pValues)[whParam]
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Multiplicative_Drift'))[0]
       multDrift = 1. * (*(*self.pParamStruct).pValues)[whParam]

    if (flag) then image = round ( (temporary(image) + addDrift) * multDrift) $
          else  image = round( (temporary(image) * multDrift) + addDrift)

    whereImage = where(image gt 255)
    if (whereImage[0] ne -1) then image[whereImage] = 255
    whereImage = where(image lt 0)
    if (whereImage[0] ne -1) then image[whereImage] = 0

    return, image
end


function C_sImageFilter_IntensitySpan::init

    filterStruct = {Name: 'C_IntensitySpan',$       ;  Filter Name.
                   pWidgetType: ptr_new(),$       ; Pointer on Filter Parameter Names.
                   pImageFilterDataIn: ptr_new(),$     ; Pointer on original Data.
                   pImageFilterDataOut: ptr_new(),$       ; Pointer on filtered Data.
                   pNames: ptr_new(),$     ; Pointer on Filter Parameter Names.
                   pActive: ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                   pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                   pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                   pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of sMeanImage.
    filterParamWidgetType = make_array(3, /string, value = 'widget_slider')

    filterParamNames = ['(X+)* or (X*)+',$     ;  Defines order of procedures
                 'Additive_Drift',$       ;     Defines the additive factor
                 'Multiplicative_Drift']          ;  Defines the multiplicative factor

    filterParamActive = [1, 1, 1]
    filterParamMin = [0.,  -255, 0.]
    filterParamMax = [1, 255., 10.]
    filterParamValues = [1., 0., 1.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_IntensitySpan__define
  tmp = {C_sImageFilter_IntensitySpan, pParamStruct: ptr_new(), inherits C_sImageFilter}
end