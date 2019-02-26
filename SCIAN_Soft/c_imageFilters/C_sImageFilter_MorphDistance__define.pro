;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_MorphDistance
;
; PURPOSE:
;       - MorphDistance-Filter-Class. See MorphDistance.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_MorphDistance' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_MorphDistance::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_MorphDistance::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    neighbor = ((*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Neighbor'))[0]] > 0) < 4

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Backgroud_OnOff'))[0]
    if (neighbor ne 4) then begin
       if (((*(*self.pParamStruct).pActive)[whParam]) and ((*(*self.pParamStruct).pValues)[whParam])) then return, morph_distance(image, neighbor_sampling = neighbor, /back, /no_copy) $
         else return, morph_distance(image, neighbor_sampling = neighbor, /no_copy)
    endif else begin
       if (((*(*self.pParamStruct).pActive)[whParam]) and ((*(*self.pParamStruct).pValues)[whParam])) then $
         return, ((morph_distance(image, neighbor_sampling = 0, /back) + $
                 morph_distance(image, neighbor_sampling = 1, /back) + $
                 morph_distance(image, neighbor_sampling = 2, /back) + $
                 morph_distance(image, neighbor_sampling = 3, /back)) * .25) else $
         return, ((morph_distance(image, neighbor_sampling = 0) + $
                 morph_distance(image, neighbor_sampling = 1) + $
                 morph_distance(image, neighbor_sampling = 2) + $
                 morph_distance(image, neighbor_sampling = 3)) * .25)
    endelse
end


function C_sImageFilter_MorphDistance::init

    filterStruct = {Name: 'C_MorphDistance',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}     ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(2, /string, value = 'widget_slider')
    filterParamNames = ['Neighbor', 'Backgroud_OnOff']
    filterParamActive = [1,1]
    filterParamMin = [0,0]
    filterParamMax = [4,1]
    filterParamValues = [0,1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_MorphDistance__define
  tmp = {C_sImageFilter_MorphDistance, pParamStruct: ptr_new(), inherits C_sImageFilter}
end