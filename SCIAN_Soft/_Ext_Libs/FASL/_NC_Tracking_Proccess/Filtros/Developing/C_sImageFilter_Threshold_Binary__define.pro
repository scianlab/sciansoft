;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Threshold_Binary
;
; PURPOSE:
;       - Threshold-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;     modified by FASL 2011
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Threshold_Binary' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_Threshold_Binary::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_Threshold_Binary::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Threshold_Binary::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    dimI = size(image, /dim)
    ; Change FASL for intersections of thresholds
    byteImage = bytArr(dimI[0], dimI[1]) * 0b
    for i = 0, n_elements (*(*self.pParamStruct).pNames)-1, 2 do begin
        ; FASL Changes ... rescale the limits for threshold to the MinMAx for current Image
        tempValueMax = max(image, min = tempValueMin)
        ;if((*(*self.pParamStruct).pMax)[i]   ne tempValueMax)then (*(*self.pParamStruct).pValues)[i+1] = tempValueMax
        ;if((*(*self.pParamStruct).pMin)[i]   ne tempValueMin)then (*(*self.pParamStruct).pValues)[i] = tempValueMin
        (*(*self.pParamStruct).pMax)[i]   = tempValueMax
        (*(*self.pParamStruct).pMax)[i+1] = tempValueMax
        (*(*self.pParamStruct).pMin)[i]   = tempValueMin
        (*(*self.pParamStruct).pMin)[i+1] = tempValueMin  
        
        if (*(*self.pParamStruct).pActive)[i] then begin

           thres_min = ((*(*self.pParamStruct).pValues)[i] ) < ((*(*self.pParamStruct).pValues)[i+1] )
           thres_max = ((*(*self.pParamStruct).pValues)[i] ) > ((*(*self.pParamStruct).pValues)[i+1] )
           
           byteImage >= ( (image ge thres_min) * (image le thres_max) )
        endif
    endfor
    return, byteImage
end

function C_sImageFilter_Threshold_Binary::applySelect, image = image, select = select

    if ((n_elements(image) ne -1) and (select ge 0)  and (select le 3)) then begin
   
       i = 2 * select
        ; FASL Changes ... rescale the limits for threshold to the MinMAx for current Image
        tempValueMax = max(image, min = tempValueMin)
        ;if((*(*self.pParamStruct).pMax)[i]   ne tempValueMax)then (*(*self.pParamStruct).pValues)[i+1] = tempValueMax
        ;if((*(*self.pParamStruct).pMin)[i]   ne tempValueMin)then (*(*self.pParamStruct).pValues)[i] = tempValueMin
        (*(*self.pParamStruct).pMax)[i]   = tempValueMax
        (*(*self.pParamStruct).pMax)[i+1] = tempValueMax
        (*(*self.pParamStruct).pMin)[i]   = tempValueMin
        (*(*self.pParamStruct).pMin)[i+1] = tempValueMin  
        
           thres_min = ((*(*self.pParamStruct).pValues)[i] ) < ((*(*self.pParamStruct).pValues)[i+1] )
           thres_max = ((*(*self.pParamStruct).pValues)[i] ) > ((*(*self.pParamStruct).pValues)[i+1] )
       return, ((image ge thres_min) * (image le thres_max) )
    endif else return, image
end


function C_sImageFilter_Threshold_Binary::init
    filterStruct = {Name: 'C_Threshold_Binary',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of C_Threshold.
    filterParamWidgetType = make_array(8, /string, value = 'widget_slider')
    filterParamNames = ['Threshold_1a', 'Threshold_1b',$   ;      4 possible Threshold Intervals -> [Threshold_1a-Threshold_1b]
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    filterParamActive = [1,1,$                     ;   (only 1st Threshold active)
                        0,0,$
                        0,0,$
                        0,0 ]
    filterParamMin = [0., 0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0. ]
                        ; Suggestion... max = 255
    filterParamMax = [1., 1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1. ]
    filterParamValues = [0., 1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Threshold_Binary__define
  tmp = {C_sImageFilter_Threshold_Binary, pParamStruct: ptr_new(), inherits C_sImageFilter}
end