;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ByteScale
;
; PURPOSE:
;       - ByteScale-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ByteScale' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ByteScale::getImageFilterType
    return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_ByteScale::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   minValue = min(image, max = maxValue)
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Min_Value'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then minValue = (*(*self.pParamStruct).pValues)[whParam]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Max_Value'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then maxValue = (*(*self.pParamStruct).pValues)[whParam]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Top_Value'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then topValue = (*(*self.pParamStruct).pValues)[whParam] else topValue = 255
	
   return, bytScl(image, min = minValue, max = maxValue, top = topValue, /nan)
end


function C_sImageFilter_ByteScale::init

    filterStruct = {Name:'C_ByteScale',$     ;  Filter Name.
                   pWidgetType:ptr_new(),$       ; Pointer on Filter Parameter Names.
                   pNames:ptr_new(),$     ; Pointer on Filter Parameter Names.
                   pImagefilterParamType:ptr_new(),$    ; Pointer on Filter Parameter Type (Bool, Byte, Int, Long, Float, Double).
                   pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                   pMin:ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                   pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                   pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of sMeanImage.
    filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
    filterParamNames = ['Max_Value','Min_Value','Top_Value']
    filterParamType = ['Float', 'Float', 'Float']
    filterParamActive = [0,0,1]
    filterParamMin = [-255.,-254., 1.]
    filterParamMax = [4096., 254., 255.]
    filterParamValues = [0., 0., 255]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pImagefilterParamType = ptr_new(filterParamType, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ByteScale__define
   tmp = {C_sImageFilter_ByteScale, pParamStruct:ptr_new(), inherits C_sImageFilter}
end