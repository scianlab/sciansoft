;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Roberts
;
; PURPOSE:
;       - Roberts-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Roberts' )
;
; METHODS:
;   function  ->apply, image = image                                   ; Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_Roberts::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Roberts::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    dimI = size(image, /dim)
    exp_image = s_Expand_Mirror(image, 1)
    return, (roberts(exp_image))[1 : dimI[0], 1 : dimI[1]] * ( 1. / (*(*self.pParamStruct).pValues)[0] )
end


function C_sImageFilter_Roberts::init

    filterStruct = {Name: 'C_Roberts',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$       ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.


       ; Parameters of C_Roberts.
    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['Max_Value_Norm']   ;   Normalize Sobel Values -> [0 'no gradient', 1 'perfect']
    filterParamActive = [1]                        ;   ( active)
    filterParamMin = [1.]
    filterParamMax = [ 2.*255.]
    filterParamValues = [ 2.*255.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Roberts__define
  tmp = {C_sImageFilter_Roberts, pParamStruct: ptr_new(), inherits C_sImageFilter}
end