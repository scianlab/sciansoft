;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageDifference
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;   Dr. Steffen Härtel (2001)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;        result = obj_new('C_sImageFilter_ImageDifference' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageDifference::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ImageDifference::apply, image = image,$
                        selectedStackObject = selectedStackObject ,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos ,$
                        chPos = chPos ,$
                        zPos = zPos ,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y

   whPos = (where((*(*self.pParamStruct).pNames) eq '2nd_Image_Position'))[0]
   pParamStruct = selectedStackObject->getpParamStruct()
   totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]
   if ((*(*self.pParamStruct).pActive)[whPos]) then  whPos = ((tPos + (*(*self.pParamStruct).pValues)[whPos]) >0) < (totalTimes-1) $
     else whPos = (tPos>0) < (totalTimes-1)

   oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
   image_2 = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject,$
                                             tPos = whPos,$
                                             chPos = chPos,$
                                             zPos = zPos,$
                                             clusPos = clusPos,$
                                             segPos = segPos-1,$
                                             cut_x = cut_x, cut_y = cut_y)

   whZeroToInt = (where((*(*self.pParamStruct).pNames) eq 'Zero_to_Int'))[0]
   if ( (*(*self.pParamStruct).pActive)[whZeroToInt]) then whZeroToInt = (*(*self.pParamStruct).pValues)[whZeroToInt]  $
     else whZeroToInt = 0.

   whMinMaxRange = (where((*(*self.pParamStruct).pNames) eq 'MinMax_Range'))[0]
   if ( (*(*self.pParamStruct).pActive)[whMinMaxRange]) then whMinMaxRange = (*(*self.pParamStruct).pValues)[whMinMaxRange]  $
     else whMinMaxRange = 0.

   if (whMinMaxRange eq 0.) then image = ((1. * image_2) - image) + whZeroToInt else $
     image = ((((1. * image_2) - image) + whZeroToInt) < (whZeroToInt+whMinMaxRange)) > (whZeroToInt-whMinMaxRange)

   image[0,0] = (whZeroToInt+whMinMaxRange)
   image[1,0] = (whZeroToInt-whMinMaxRange)
   return, image
end


function C_sImageFilter_ImageDifference::init
   ImageFilterStruct = {Name: 'C_ImageDifference',$    ;  Filter Name.
               pWidgetType: ptr_new(),$    ; Pointer on Filter Parameter Names.
               pNames: ptr_new(),$     ; Pointer on Filter Parameter Names.
               pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
               pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
               pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
               pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

     ; Parameters of C_ImageColocalization.
   filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
   filterParamNames = ['2nd_Image_Position', 'Zero_to_Int', 'MinMax_Range']
   filterParamActive = [1, 1, 1]
   filterParamMin = [-5, 0, 0]
   filterParamMax = [5, 255, 255]
   filterParamValues = [1, 127, 20]

   ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageDifference__define
   tmp = {C_sImageFilter_ImageDifference, pParamStruct: ptr_new(), inherits C_sImageFilter}
end