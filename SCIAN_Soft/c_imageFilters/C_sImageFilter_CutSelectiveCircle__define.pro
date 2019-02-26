;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_CutSelectiveCircle
;
; PURPOSE:
;       - Cut-Filter-Class.
;
; Filtro que me recorta la imagen con un circulo, dado el centro y el radio
;
; AUTHOR:
;     lbriones
;     e_mail: lbriones@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_CutSelectiveCircle' )
;
; METHODS:
;   function ->apply, pImageData = pImageData      ;pImageData   Pointer on Image Data Matrix
;
;
;_____________________________IOISIOI____________________


function C_sImageFilter_CutSelectiveCircle::getImageFilterType
   return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_CutSelectiveCircle::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   dimI = size(image, /dim)
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Center X'))[0]
   if ((*(*self.pParamStruct).pValues)[whParam]lt -10000) then return, image else cx=(*(*self.pParamStruct).pValues)[whParam]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Center Y'))[0]
   if ((*(*self.pParamStruct).pValues)[whParam] gt 10000) then return, image else cy=(*(*self.pParamStruct).pValues)[whParam]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Radius'))[0]
   if ((*(*self.pParamStruct).pValues)[whParam]lt 0) then return, image else r=(*(*self.pParamStruct).pValues)[whParam]
   
   centro=[cx,cy]
   img=make_array(dimI)
    ;punto a b esta dentro si: (a - x)^2 + (b - y)^2 = (r)^2
    for i=centro[0]-r, centro[0]+r do begin
      for j=centro[1]-r, centro[1]+r do begin
        if (i ge 0) and (j ge 0) and (i lt dimI[0]) and (j lt dimI[1]) and ((i - centro[0])^2 + (j - centro[1])^2) le (r^2) then begin
          image[i,j]=0
        endif
      endfor
    endfor
      
   return, image
end


function C_sImageFilter_CutSelectiveCircle::init

    filterStruct = {Name: 'C_CutSelectiveCircle',$   ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pImageFilterDataIn:ptr_new(),$       ; Pointer on original Data.
                           pImageFilterDataOut:ptr_new(),$     ; Pointer on filtered Data.
                           pNames:ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}     ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
    filterParamNames = ['Center X',$
                        'Center Y',$
                        'Radius']   

    filterParamActive = [1, 1, 1]
    filterParamMin = [-10000.0, -10000.0, 0.0]
    filterParamMax = [ 10000.0, 10000.0, 10000.0]
    filterParamValues = [0.0,0.0,0.0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_CutSelectiveCircle__define
   tmp = {C_sImageFilter_CutSelectiveCircle, pParamStruct: ptr_new(), inherits C_sImageFilter}
end