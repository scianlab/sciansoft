;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_MeanTimeNormalization
;
; PURPOSE:
;       - MeanTimeNormalization-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_MeanTimeNormalization' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_MeanTimeNormalization::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_MeanTimeNormalization::apply, image = image,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

    pParamStruct = selectedStackObject->getpParamStruct()
    totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]

    meanImageName = strCompress(*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]] + strcompress('MeanImage_ch' + string(chPos) + '_z' + string(zPos) +'.tif', /rem))
    firstImagePosition = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq '1st_Mean_Image_Position'))[0]] > 0
    lastImagePosition = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Last_Mean_Image_Position'))[0]] < (totalTimes-1)
    if (firstImagePosition gt lastImagePosition) then firstImagePosition = lastImagePosition
    (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq '1st_Mean_Image_Position'))[0]] = firstImagePosition
    (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Last_Mean_Image_Position'))[0]] = lastImagePosition
    firstOld = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq '1st_Old'))[0]]
    lastOld = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Last_Old'))[0]]

    if ((query_tiff (meanImageName) eq 1) and (firstOld eq firstImagePosition) and (lastOld eq lastImagePosition) )then begin
       meanImage = 1.* read_tiff(meanImageName)
    endif else begin
       (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq '1st_Old'))[0]] = firstImagePosition
       (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Last_Old'))[0]] = lastImagePosition

       meanImage = fltArr((size(image, /dim))[0], (size(image, /dim))[1])
       meanFactor = 1. / ((lastImagePosition - firstImagePosition) > 1)
       for i = firstImagePosition, lastImagePosition do begin
          ; get Image Object from Container
         oImage = selectedStackObject->getSelectedImageObject(tPos = i, chPos = chPos, zPos = zPos)
         image_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                            tPos = i ,$
                                                            chPos = chPos ,$
                                                            zPos = zPos ,$
                                                            clusPos = clusPos,$
                                                            segPos = segPos-1,$
                                                            cut_x = cut_x, cut_y = cut_y)

         meanImage = temporary(meanImage) + (image_2 * meanFactor)
       endfor
       write_tiff, meanImageName, meanImage
    endelse

    return, (image - meanImage) > 0
end


function C_sImageFilter_MeanTimeNormalization::init
    filterStruct = {Name: 'C_MeanTimeNormalization',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(4, /string, value = 'widget_slider')
    filterParamNames = ['1st_Mean_Image_Position', 'Last_Mean_Image_Position', '1st_Old', 'Last_Old']
    filterParamActive = [1,1,1,1]
    filterParamValues = [0, 5, 0, 5]
    filterParamMin = [0, 0, 0, 0]
    filterParamMax = [10000, 10000, 10000, 10000]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_MeanTimeNormalization__define
  tmp = {C_sImageFilter_MeanTimeNormalization, pParamStruct: ptr_new(), inherits C_sImageFilter}
end