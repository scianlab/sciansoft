;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Canny
;
; PURPOSE:
;       - Canny-Filter-Class.
;
; AUTHOR:
;     Hector Moraga (2011)
;     e_mail: hector.moraga.aros@gmail.com
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Canny' )
;
; METHODS:
;   function  ->apply, image = image                                   ; Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________
function C_sImageFilter_Canny::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Canny::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image
    
    dimI = size(image, /dim)
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Low'))[0]
    low =(*(*self.pParamStruct).pValues)[whParam]
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'High'))[0]
    high=(*(*self.pParamStruct).pValues)[whParam]
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Sigma'))[0]
    sigmax=(*(*self.pParamStruct).pValues)[whParam]
    
    return, (canny(s_Expand_Mirror(image, 2), HIGH = high, LOW = low, SIGMA = sigmax))[2:dimI[0]+1, 2:dimI[1]+1]
end

function C_sImageFilter_Canny::init
    filterStruct = {Name: 'C_Canny',$       ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}         ; Pointer on Filter Parameter Values.

    ; Parameters of C_sImageFilter_Canny.   
    filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
    filterParamNames = ['Low', 'High', 'Sigma']   ;Low Threshold, High Threshold, Sigma
    filterParamActive = [1, 1, 1]
    filterParamMin = [0, 0, 0]
    filterParamMax = [1, 1, 10]
    filterParamValues = [0.2, 0.9, 1.4]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_Canny__define
   tmp = {C_sImageFilter_Canny, pParamStruct:ptr_new(), inherits C_sImageFilter}
end