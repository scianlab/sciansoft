;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ThresholdOtsu
;
; PURPOSE:
;       - Threshold-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2011)
;     e_mail: shaertel@physik.uni-bremen.de
;     modified by FASL, JJ 2011
;     Jan Scheer
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ThresholdOtsu' )
;
; METHOHDS:
;   This algorithm is an implementation of Otsu thresholding technique 
;   based on the minimization of inter-class variance [otsu79].
;
;_____________________________IOISIOI____________________

function C_sImageFilter_ThresholdOtsu::getImageFilterType
    return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_ThresholdOtsu::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image
    whParam = (where((*(*self.pParamStruct).pNames) eq '0_LE_-_1_GE'))[0]
    geThreshold = *(((*self.pParamStruct).pValues)[whParam])

    dimI = size(image, /dim)
    width = dimI(0)
    height = dimI(1)
    ; Change FASL for intersections of thresholds
    byteImage = bytArr(dimI[0], dimI[1]); * 0b

    N = width * height
    hist = histogram(image)
    histMax = 0
    count = n_elements(hist)
    for i = 0L, count-1L, 1L do begin
       if ((hist[i] gt 0) and (i gt histMax)) then histMax = i
    endfor

    sum = 0.0
    for t = 0L, count-1L, 1L do sum += t * hist[t]

    sumB = 0.0
    wB = 0 ;background
    bF = 0 ;foreground

    varMax = 0.0
    threshold = 0

    for t = 0, 255, 1 do begin
         wB += hist[t] ;Weight Background
         if (wB eq 0) then continue

         wF = N - wB ; Weight Foreground
         if (wF eq 0) then break

         sumB += float(t * hist[t])

         mB = float(sumB) / wB       ; Mean Background
         mF = float(sum - sumB) / wF ; Mean Foreground

         ; Calculate Between Class Variance
         varBetween = float(wB) * float(wF) * (mB - mF) * (mB - mF)

         if (varBetween gt varMax) then begin
            varMax = varBetween
            threshold = t
         endif
    endfor
    print, 'Otsu Threshold: ', threshold

    indF = (geThreshold eq 1) ? where(image ge threshold, numPix) : where(image le threshold, numPix)

    if (numPix gt 0) then byteImage[indF] = 1

    return, byteImage
end


function C_sImageFilter_ThresholdOtsu::init
    filterStruct = {Name: 'C_ThresholdOtsu',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$      ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$     ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$        ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$        ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}      ; Pointer on Filter Parameter Values.

    ; Parameters
    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['0_LE_-_1_GE']
    filterParamActive = [1]
    filterParamMin = [0]
    filterParamMax = [1]
    filterParamValues = [1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ThresholdOtsu__define
  tmp = {C_sImageFilter_ThresholdOtsu, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
