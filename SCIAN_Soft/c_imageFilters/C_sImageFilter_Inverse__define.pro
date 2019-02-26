;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Inverse
;
; PURPOSE:
;       - Inverse-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Inverse' )
;
; METHOHDS:
;   function  ->apply, image = image                                   ; Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_Inverse::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Inverse::apply, image = image
    
    if (size(image, /n_dim) ne 2) then return, image
    if (max(image) gt 255) then begin
      return, 4095 - image 
    endif else if(max(image) le 255 && max(image) gt 1) then begin
      return, 255 - image
    endif else if(max(image) eq 1) then begin
      return, 1-image
    endif ;else if(not(max(image) le 255) && not(max(image) eq 1) && max(image)
   ; else if (max(image) le 1) then return, 1-image
    ;if (size(image, /n_dim) ne 2) then return, image
    ;;we suppose image range [0, max] for simple use
    ;return, max(image) - image
    
    
    return, 1 - image
end


function C_sImageFilter_Inverse::init

    filterStruct = {Name: 'C_Inverse',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of C_Inverse.
    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['VOID']       ;     Normalize Sobel Values -> [0 'no gradient', 1 'perfect']
    filterParamActive = [0]                        ;   ( active)
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

pro C_sImageFilter_Inverse__define
  tmp = {C_sImageFilter_Inverse, pParamStruct: ptr_new(), inherits C_sImageFilter}
end