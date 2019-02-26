;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageVoronoiByMorphDistance
;
; PURPOSE:
;       - MorphDistance-Filter-Class. See MorphDistance.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ImageVoronoiByMorphDistance' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageVoronoiByMorphDistance::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_ImageVoronoiByMorphDistance::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_ImageVoronoiByMorphDistance::apply, image = image
    
    if (size(image, /n_dim) ne 2) then return, image

    neighbor = ((*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Neighbor'))[0]] > 0) < 4

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Backgroud_OnOff'))[0]

    imagen        = BYTSCL(image)
    mapaDistancia = MORPH_DISTANCE(imagen, /BACKGROUND,NEIGHBOR_SAMPLING = neighbor) 
   
    labelMask = label_region(imagen)
    imagenFinal = imagen * 0
    maxLM = max(labelMask)
    
    if (maxLM ge 1) then begin
      idx=where(image eq 1.0)
      mapaDistancia(idx)=0
      imagenFinal = WATERSHED( mapaDistancia, NREGIONS=nregions ) 
    endif else return, image   
    
    return, imagenFinal
end


function C_sImageFilter_ImageVoronoiByMorphDistance::init

    filterStruct = {Name: 'C_ImageVoronoiByMorphDistance',$       ;  Filter Name.
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
    filterParamValues = [3,1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageVoronoiByMorphDistance__define
  tmp = {C_sImageFilter_ImageVoronoiByMorphDistance, pParamStruct: ptr_new(), inherits C_sImageFilter}
end