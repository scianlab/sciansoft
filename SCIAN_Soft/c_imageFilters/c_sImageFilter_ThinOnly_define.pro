;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ThinOnly
;
; PURPOSE:
;       - Thin-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2010)
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ThinOnly' )
;
; METHOHDS:
;   function  ->apply, image = image                                   ; Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ThinOnly::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_ThinOnly::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image
   if ((size(image, /dim))[0] le 4) then return, image
   if ((size(image, /dim))[1] le 4) then return, image
   
   return, thin(image, /NEIGHBOR_COUNT)
   
;Result = THIN( Image [, /NEIGHBOR_COUNT] [, /PRUNE] ) 
end


function C_sImageFilter_ThinOnly::init

    filterStruct = {Name: 'C_ThinOnly',$       ;  Filter Name.
                           pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$       ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.


       ; Parameters of C_Roberts.
    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['Max_Value_Norm']   ;   Normalize Sobel Values -> [0 'no gradient', 1 'perfect']
    filterParamActive = [1]                        ;   ( active)
    filterParamMin = [1.]
    filterParamMax = [2.*255.]
    filterParamValues = [2.*255.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ThinOnly__define
  tmp = {C_sImageFilter_ThinOnly, pParamStruct: ptr_new(), inherits C_sImageFilter}
end