;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_CutSelective
;
; PURPOSE:
;       - Cut-Filter-Class.
;
; Filtro que me recorta la imagen con los margenes negros (de valor cero) con valores left, right, top, bottom (en pixeles)
;
; AUTHOR:
;     Hector Moraga A. (2011)
;     e_mail: hmoraga@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_CutSelective' )
;
; METHODS:
;   function ->apply, pImageData = pImageData      ;pImageData   Pointer on Image Data Matrix
;
;
;_____________________________IOISIOI____________________


function C_sImageFilter_CutSelective::getImageFilterType
   return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_CutSelective::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   dimI = size(image, /dim)
   whParam = (where((*(*self.pParamStruct).pNames) eq 'left'))[0]
   if ((*(*self.pParamStruct).pValues)[whParam] lt 0) then return, image else left=(*(*self.pParamStruct).pValues)[whParam]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'right'))[0]
   if ((*(*self.pParamStruct).pValues)[whParam] gt dimI[0]-1) then return, image else right=(*(*self.pParamStruct).pValues)[whParam]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'top'))[0]
   if ((*(*self.pParamStruct).pValues)[whParam]lt 0) then return, image else top=(*(*self.pParamStruct).pValues)[whParam]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'bottom'))[0]
   if ((*(*self.pParamStruct).pValues)[whParam] gt dimI[1]-1) then return, image else bottom=(*(*self.pParamStruct).pValues)[whParam]
   if (dimI[0]-right lt left) then begin
      print, 'Error: Left > Right'
      return, image
   endif
   if (dimI[1]-bottom lt top) then begin
      print, 'Error: Top > Bottom'
      return, image
   endif
   if ((left ne 0) or (right ne 0) or (top ne 0) or (bottom ne 0)) then begin
      if (left ne 0) then image[0:left,*] = 0
      if (right ne 0) then image[dimI[0]-1-right:dimI[0]-1,*] = 0
      if (top ne 0) then image[*,0:top] = 0
      if (bottom ne 0) then image[*,dimI[1]-1-bottom:dimI[1]-1] = 0
   end
   
   return, image
end


function C_sImageFilter_CutSelective::init

    filterStruct = {Name: 'C_CutSelective',$   ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pImageFilterDataIn:ptr_new(),$       ; Pointer on original Data.
                           pImageFilterDataOut:ptr_new(),$     ; Pointer on filtered Data.
                           pNames:ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}     ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(4, /string, value = 'widget_slider')
    filterParamNames = ['left',$   ;   Pixel position of the left cut position.
                        'right',$   ;   Pixel position of the right cut position.
                        'top',$   ;  Pixel position of the up position.
                        'bottom']   ;   Pixel position of the down cut position.

    filterParamActive = [1, 1, 1, 1]
    filterParamMin = [0, 0, 0, 0]
    filterParamMax = [10000, 10000, 10000, 10000]
    filterParamValues = [0,0,0,0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_CutSelective__define
   tmp = {C_sImageFilter_CutSelective, pParamStruct: ptr_new(), inherits C_sImageFilter}
end