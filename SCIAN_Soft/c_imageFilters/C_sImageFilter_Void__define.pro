;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Void
;
; PURPOSE:
;       - Void-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Void' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_Void::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Void::apply, image = image
    return, image * 0
end


function C_sImageFilter_Void::init
    filterStruct = {Name: 'C_Void',$     ;  Filter Name.
                    pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                    pNames: ptr_new(),$   ; Pointer on Filter Parameter Names.
                    pActive: ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                    pMin: ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                    pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                    pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['Max_Value_Norm']   ;   Normalize Void Values -> [0 'no gradient', 1 'perfect']
    filterParamActive = [1]                 ;   (always active)
    filterParamMin = [1.]
    filterParamMax = [ 4.*255.]
    filterParamValues = [ 4.*255.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Void__define
  tmp = {C_sImageFilter_Void, pParamStruct: ptr_new(), inherits C_sImageFilter}
end