;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_SubtractHistogramMaximum
;
; PURPOSE:
;       - Threshold-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2011)
;     e_mail: shaertel@physik.uni-bremen.de
;     modified by FASL 2011
;     Jan Scheer
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_SubtractHistogramMaximum' )
;
; METHOHDS:
;   This algorithm is an implementation of Otsu thresholding technique 
;   based on the minimization of inter-class variance [otsu79].
;
;_____________________________IOISIOI____________________

function C_sImageFilter_SubtractHistogramMaximum::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_SubtractHistogramMaximum::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    dimI = size(image, /dim)
    width = dimI(0)
    height = dimI(1)
   
   

    hist = histogram(image)
    ; determine background intensity
    maxh = max(hist)
    ind_maxh = where(hist eq maxh)
    bgInt  = ind_maxh[0]
    ; subtract / clip to 0
    indeces = where(image le bgInt)
    print, 'Subtract Background Intensity: '+bgInt+'.' 
    bgSubtractedImage = (image - bgInt)
    sizeInd = size(indeces, /dimension)
    if(sizeInd ge 1) then bgSubtractedImage(indeces) = 0
    
    return_image = bgSubtractedImage
 

    return, return_image
end




function C_sImageFilter_SubtractHistogramMaximum::init
    filterStruct = {Name: 'C_SubtractHistogramMaximum',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

    ; Parameters
    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['VOID']         
    filterParamActive = [0]                   
    filterParamMin = [1.]
    filterParamMax = [ 2.*255.]
    filterParamValues = [ 2.*255.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_SubtractHistogramMaximum__define
  tmp = {C_sImageFilter_SubtractHistogramMaximum, pParamStruct: ptr_new(), inherits C_sImageFilter}
end