;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_kMeans
;
; PURPOSE:
;       - 1stDeviation-Filter-Class. See 1stDeviation.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_kMeans' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_kMeans::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_kMeans::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'kMeans_Top'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then kMeans_Top = round((*(*self.pParamStruct).pValues)[whParam]) else kMeans_Top = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'kMeans_Bottom'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then kMeans_Bottom = round((*(*self.pParamStruct).pValues)[whParam]) else kMeans_Bottom = 0
    whereSetBottomToParam = (where((*(*self.pParamStruct).pNames) eq 'kMeans_SetBottomTo'))[0]
       if ((*(*self.pParamStruct).pActive)[whereSetBottomToParam]) then kMeans_SetBottomTo = round((*(*self.pParamStruct).pValues)[whereSetBottomToParam])

    dimI = size (image, /dim)
    imageSort = image[sort(image)]

    imageSize = dimI[0] * dimI[1]

    kMeans_Top = round(imageSize *  kMeans_Top / 100.)
    kMeans_Bottom = round(imageSize *  kMeans_Bottom / 100.)

    kMeans_TopValue = imageSort[imageSize - 1 - kMeans_Top]
    kMeans_BottomValue = imageSort[kMeans_Bottom]

    whereImage = where(image gt kMeans_TopValue)
       if (whereImage[0] ne -1) then image[whereImage] = kMeans_TopValue
    whereImage = where(image lt kMeans_BottomValue)
       if (whereImage[0] ne -1) then image[whereImage] = kMeans_BottomValue

    maxImage = max(image)
    whereSetTopToParam = (where((*(*self.pParamStruct).pNames) eq 'kMeans_SetTopTo'))[0]
    if ((*(*self.pParamStruct).pActive)[whereSetTopToParam]) then begin
       image = temporary(image) + ((255 - maxImage) - (255 - round((*(*self.pParamStruct).pValues)[whereSetTopToParam])) )
       whereImage = where(image lt 0)
       if (whereImage[0] ne -1) then image[whereImage] = 0
    endif

    minImage = min(image)
    whereSetBottomToParam = (where((*(*self.pParamStruct).pNames) eq 'kMeans_SetBottomTo'))[0]
    if ((*(*self.pParamStruct).pActive)[whereSetBottomToParam]) then begin
       image = temporary(image) - ( minImage - round((*(*self.pParamStruct).pValues)[whereSetBottomToParam]) )
       whereImage = where(image gt 255)
       if (whereImage[0] ne -1) then image[whereImage] = 255
    endif

    return, image
end


function C_sImageFilter_kMeans::init

    filterStruct = {Name: 'C_kMeans',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$       ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$       ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of 1stDeviation.
    filterParamWidgetType = make_array(4, /string, value = 'widget_slider')

    filterParamNames = ['kMeans_Top',$
                      'kMeans_Bottom',$
                      'kMeans_SetTopTo',$
                      'kMeans_SetBottomTo']

    filterParamActive = [1,$
                    1,$
                    1,$
                    1]

    filterParamMin = [0.,$
                    0,$
                    100,$
                    0.]

    filterParamMax = [100.,$
                    100.,$
                    255.,$
                    150.]

    filterParamValues = [1.,$
                    1.,$
                    255.,$
                    0.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_kMeans__define
  tmp = {C_sImageFilter_kMeans, pParamStruct: ptr_new(), inherits C_sImageFilter}
end