;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_WaterShed
;
; PURPOSE:
;       - WaterShed-Filter-Class. See WaterShed.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_WaterShed' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_WaterShed::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_WaterShed::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'InverseIntensities_OnOff'))[0]
    if (((*(*self.pParamStruct).pActive)[whParam]) and ((*(*self.pParamStruct).pValues)[whParam])) then $
       newImage = (-1*image) + (max(image) + min(image)) else newImage = image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Connectivity_4_or_8'))[0]
    if (round((*(*self.pParamStruct).pValues)[whParam]) eq 4) then newImage = watershed(newImage, connectivity = 4) $
       else newImage = watershed(newImage, connectivity = 8)

    whParam = (where((*(*self.pParamStruct).pNames) eq 'SetBordersToZero_OnOff'))[0]
    if (((*(*self.pParamStruct).pActive)[whParam]) and ((*(*self.pParamStruct).pValues)[whParam])) then begin
       dimI = size(image, /dim)
       newImage[ [0, dimI[0]-1], *] = 1
       newImage[ *, [0, dimI[1]-1]] = 1
       whereZero = where(newImage eq 0)
       if (whereZero[0] ne -1) then begin
         newImage = image
         newImage[whereZero] = 0
       endif
    endif

    return, newImage
end


function C_sImageFilter_WaterShed::init

    filterStruct = {Name: 'C_WaterShed',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of WaterShed.
    filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
    filterParamNames = ['Connectivity_4_or_8', 'InverseIntensities_OnOff','SetBordersToZero_OnOff']
    filterParamActive = [1,1,1]
    filterParamMin = [4,0,0]
    filterParamMax = [8,1,1]
    filterParamValues = [4,1,1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_WaterShed__define
  tmp = {C_sImageFilter_WaterShed, pParamStruct: ptr_new(), inherits C_sImageFilter}
end