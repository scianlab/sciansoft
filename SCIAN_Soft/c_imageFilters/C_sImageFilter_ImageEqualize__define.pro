;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageEqualize
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;   Dr. Steffen Härtel (2001)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;        result = obj_new('C_sImageFilter_ImageEqualize' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageEqualize::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ImageEqualize::apply, image = image,$
                        selectedStackObject = selectedStackObject ,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos ,$
                        chPos = chPos ,$
                        zPos = zPos ,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y


   whParam = (where((*(*self.pParamStruct).pNames) eq 'Set max to:'))[0]
   maxVal = round((*(*self.pParamStruct).pValues)[whParam])

       ; get Image Object from Container
   chPos = abs(chPos - 1)
   oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
   image_2 = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject,$
                           tPos = tPos,$
                           chPos = chPos,$
                           zPos = zPos,$
                           clusPos = clusPos,$
                           segPos = segPos-1,$
                           cut_x = cut_x, cut_y = cut_y)

   meanI1 = (moment(image))[0]
   meanI2 = (moment(image_2))[0]
   image *= (meanI2/meanI1) < maxVal
   return, image
end


function C_sImageFilter_ImageEqualize::init
   ImageFilterStruct = {Name: 'C_ImageEqualize',$   ;  Filter Name.
               pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
               pNames: ptr_new(),$   ; Pointer on Filter Parameter Names.
               pActive: ptr_new(),$     ; Pointer on Filter Parameter Active Bool.
               pMin: ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
               pMax: ptr_new(),$   ; Pointer on Filter Parameter Max_Values.
               pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

     ; Parameters of C_ImageEqualize.
   filterParamWidgetType = make_array(1, /string, value = 'widget_slider')
   filterParamNames = ['Set max to:']
   filterParamActive = [1]
   filterParamMin = [0.]
   filterParamMax = [4096]
   filterParamValues = [255]

   ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageEqualize__define
   tmp = {C_sImageFilter_ImageEqualize, pParamStruct: ptr_new(), inherits C_sImageFilter}
end