;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_IntensitySteps
;
; PURPOSE:
;
;
; AUTHOR:
;     Yerko Covacevich (2007)
;     e_mail:ycovacev@dcc.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_IntensitySteps' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_IntensitySteps::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_IntensitySteps::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Intensity Steps'))[0]
    stepSize = round((*(*self.pParamStruct).pValues)[whParam]) > 0

    return, floor(image / stepSize) * stepSize
end


function C_sImageFilter_IntensitySteps::init
    filterStruct = {Name:'C_IntensitySteps',$     ;  Filter Name.
                    pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                    pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                    pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                    pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                    pMax:ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                    pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['Intensity Steps']
    filterParamActive = [1]
    filterParamMin = [1]
    filterParamMax = [50]
    filterParamValues = [5]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_IntensitySteps__define
  tmp = {C_sImageFilter_IntensitySteps, pParamStruct:ptr_new(), inherits C_sImageFilter}
end