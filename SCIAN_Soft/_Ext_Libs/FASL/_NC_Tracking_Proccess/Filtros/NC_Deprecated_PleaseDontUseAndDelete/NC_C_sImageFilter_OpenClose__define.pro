;_____________________________IOISIOI____________________
; NAME:
;      NC_C_sImageFilter_OpenClose
;
; PURPOSE:
;       - OpenClose-Filter-Class. See OpenClose.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('NC_C_sImageFilter_OpenClose' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData     ;pImageData Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function NC_C_sImageFilter_OpenClose::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function NC_C_sImageFilter_OpenClose::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
    return,  'FASL_NC_Release_Filter_Method'
end

function NC_C_sImageFilter_OpenClose::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    ; ordnet jedem Pixel in image:= 0, wenn # gl. Nachbarpunkte > toleranz
    ;        jedem Pixel = 0  := 1, wenn # gl. Nachbarpunkte < = toleranz
    ;        jedem Pixel = 1  := 2, wenn # gl. Nachbarpunkte < = toleranz

    ;toleranz = 0 isoliert single Punkte ohne gleiche Nachbarpunkte.
    ;toleranz = 1 isoliert Punkte mit höchstens einem gleichen Nachbarpunkt...
    ;toleranz = 7 isoliert zusammenhängend Randpunkte:Innerer Rand von 1 = 2 / äußerer Rand = 1
    ;                    :Innerer Rand von 0 = 1 / äußerer Rand = 2
    ;toleranz = 8 isoliert all Punkte 0/1 mit gleichen Nachbarpunkten = 2
    ;                    :Innerer Rand von 1 = 1 / äußerer Rand = 0
    ;                    :Innerer Rand von 0 = 0 / äußerer Rand = 1

    image = image ne 0
    dimI = size(image, /dim)

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Pixel_Neighbor'))[0]
    (*(*self.pParamStruct).pValues)[whParam] = (*(*self.pParamStruct).pValues)[whParam] > 1
    pixelNeighbor = (*(*self.pParamStruct).pValues)[whParam]

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Tolerance'))[0]
    tolerance = round((*(*self.pParamStruct).pValues)[whParam])

    alphan1 = intArr(dimI[0],dimI[1])
    alpha = bytArr(dimI[0],dimI[1])
    n = 0
       ; Summierung ungleicher Nachbarn in pixelNeighbor
    for k = -pixelNeighbor, pixelNeighbor do for l = -pixelNeighbor, pixelNeighbor do begin
       if (floor(sqrt(k*k+l*l)) le pixelNeighbor) then begin
          alphan1 -= image
          alphan1 += shift(image,k,l)
          n += 1          ; alphan1(x,x) = 0 für all Punkte mit gleichen Nachbarn
       endif              ; alphan1(x,x) = N für all 0-Punkte mit N Nachbarn = 1
    endfor                ; alphan1(x,x) = -N für all 1-Punkte mit N Nachbarn = 0

    alpha = alphan1 ge (n-1-tolerance)
    alpha += (alphan1 le (-1* (n-1-tolerance))) * 2

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Erode'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then alpha = (image - (alpha eq 2)) ne 0

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Delate'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then alpha = (image + (alpha eq 1)) ne 0

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Erode and Delate'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then alpha = (image - (alpha eq 2) + (alpha eq 1)) ne 0

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Open'))[0]
    whereClose = (where((*(*self.pParamStruct).pNames) eq 'Close'))[0]
    if ( ((*(*self.pParamStruct).pActive)[whParam]) or ((*(*self.pParamStruct).pActive)[whereClose]) ) then begin

       if ((*(*self.pParamStruct).pActive)[whParam]) then alpha = (image - (alpha eq 2)) ne 0
       if ((*(*self.pParamStruct).pActive)[whereClose]) then alpha = (image + (alpha eq 1)) ne 0

       n = 0
       image = alpha ne 0
       alphan1 *= 0
       for k = -pixelNeighbor, pixelNeighbor do for l = -pixelNeighbor,pixelNeighbor do begin
          if (floor(sqrt(k*k+l*l)) le pixelNeighbor) then begin
             alphan1 -= image
             alphan1 += shift(image,k,l)
             n += 1
          endif
       endfor
       alpha = alphan1 ge (n-1-tolerance)
       alpha += (alphan1 le (-1* (n-1-tolerance))) * 2

       if (*(*self.pParamStruct).pActive)[whereClose] then alpha = (image - (alpha eq 2)) ne 0
       if (*(*self.pParamStruct).pActive)[whParam] then alpha = (image + (alpha eq 1)) ne 0
    endif

    return, alpha
end

function NC_C_sImageFilter_OpenClose::init
   filterStruct = {Name:'C_OpenClose',$   ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

      ; Parameters of OpenClose.
   filterParamWidgetType = make_array(7, /string, value = 'widget_slider')

   filterParamNames = ['Erode',$
                      'Delate',$
                      'Erode and Delate',$
                      'Open',$
                      'Close',$
                      'Tolerance',$
                      'Pixel_Neighbor']

   filterParamActive = [1,0,0,0,0,1,1.]
   filterParamMin = [0,0,0,0,0,0,0.]
   filterParamMax = [1,1,1,1,1,8,1000.]
   filterParamValues = [1.,0,0,0,0,8,1]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro NC_C_sImageFilter_OpenClose__define
   tmp = {NC_C_sImageFilter_OpenClose, pParamStruct:ptr_new(), inherits C_sImageFilter}
end