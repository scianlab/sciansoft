;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_FlipRotateZoom
;
; PURPOSE:
;       - Fill-Filter-Class. See Fill.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2007)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_FlipRotateZoom' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_FlipRotateZoom::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_FlipRotateZoom::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   dimI = size(image, /dim)
   alpha = image
   whereName = (where((*(*self.pParamStruct).pNames) eq 'Flip_Horizontally'))[0]
   if ((*(*self.pParamStruct).pActive)[whereName]) then alpha = alpha[*, dimI[1] - make_array(dimI[1], /int, /index)]

   whereName = (where((*(*self.pParamStruct).pNames) eq 'Flip_Vertically'))[0]
   if ((*(*self.pParamStruct).pActive)[whereName]) then alpha = alpha[dimI[0] - make_array(dimI[0], /int, /index), *]

   whereName = (where((*(*self.pParamStruct).pNames) eq 'Rotate'))[0]
   if ((*(*self.pParamStruct).pActive)[whereName]) then begin
      fRot = 1b
      angle = (*(*self.pParamStruct).pValues)[whereName] > 0 < 360
   endif else angle = 0

   fRot = 0b
   whereName = (where((*(*self.pParamStruct).pNames) eq 'Zoom'))[0]
   if ((*(*self.pParamStruct).pActive)[whereName]) then begin
      fRot = 1b
      zoom = (*(*self.pParamStruct).pValues)[whereName] > 0.01
   endif else zoom = 1.

   if fRot then alpha = rot(alpha, angle, zoom, cubic = -.5, missing = 0)

   return, alpha
end


function C_sImageFilter_FlipRotateZoom::init

   filterStruct = {Name:'C_FlipRotateZoom',$     ;  Filter Name.
                   pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Names.
                   pNames:ptr_new(),$     ; Pointer on Filter Parameter Names.
                   pActive:ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                   pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                   pMax:ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                   pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

   filterParamWidgetType = make_array(4, /string, value = 'widget_slider')
   filterParamNames = ['Flip_Horizontally', 'Flip_Vertically', 'Rotate', 'Zoom']
   filterParamActive = [1,0,0,1]
   filterParamMin = [0,0,0,0.01]
   filterParamMax = [1,1,360,100.]
   filterParamValues = [1,1,0.,1.]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end


pro C_sImageFilter_FlipRotateZoom__define
  tmp = {C_sImageFilter_FlipRotateZoom, pParamStruct:ptr_new(), inherits C_sImageFilter}
end