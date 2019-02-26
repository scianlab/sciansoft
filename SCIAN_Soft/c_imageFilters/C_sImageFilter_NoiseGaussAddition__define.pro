;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_NoiseGaussAddition  (Steffen\s_Surface\sImageFilters_apop_iso.pro)
;
; PURPOSE:
;       - NoiseCreator-Filter-Class.
;
; AUTHOR:
;     loyarzo (2003)
;     e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_NoiseGaussAddition' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_NoiseGaussAddition::getImageFilterType
    return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_NoiseGaussAddition::apply, image = image

    momentImage = moment(image)
    whParam = (where((*(*self.pParamStruct).pNames) eq 'MeanIntensity'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
       meanIntensity = round((*(*self.pParamStruct).pValues)[whParam]) > 0 else MeanIntensity = momentImage[0]
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Standard Deviation of Signal-Noise'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
         SDSN = round((*(*self.pParamStruct).pValues)[whParam]) else SDSN = momentImage[1]
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Standard Deviation of Background-Noise'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
         SDBN = round((*(*self.pParamStruct).pValues)[whParam]) else SDBN = momentImage[1]

    whereSignal = where(image gt 0)
    whereBackgroud = where(image eq 0)

    if (whereSignal[0] ne -1) then image[whereSignal] = meanIntensity + ( randomN(seed, n_elements(whereSignal)) * sqrt(SDSN) )
    if (whereBackgroud[0] ne -1) then image[whereBackgroud] = meanIntensity + ( randomN(seed, n_elements(whereBackgroud)) * sqrt(SDBN) )

    if (whereSignal[0] ne -1) then begin
       momentSignal = moment( image[whereSignal] )
       print, 'The mean intensity of the signal is: ', momentSignal[0], '  with a gaussean noise of SD: ', momentSignal[1]
    endif
    if (whereBackgroud[0] ne -1) then begin
       momentBackgroud = moment( image[whereBackgroud] )
       print, 'The mean intensity of the background is: ', momentBackgroud[0], '  with a gaussean noise of SD ', momentBackgroud[1]
    endif

    return, image
end


function C_sImageFilter_NoiseGaussAddition::init
    ImageFilterStruct = {Name: 'C_NoiseGaussAddition',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$       ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
    filterParamNames = ['MeanIntensity' ,$        ;     Mean Intensity of Image Regions
                 'Standard Deviation of Signal-Noise' ,$
                 'Standard Deviation of Background-Noise' ]
    filterParamActive = [1,1,1]
    filterParamMin = [0.,0.,0.]
    filterParamMax = [255.,200.,200.]
    filterParamValues = [155.,10.,5.]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_NoiseGaussAddition__define
  tmp = {C_sImageFilter_NoiseGaussAddition, pParamStruct: ptr_new(), inherits C_sImageFilter}
end