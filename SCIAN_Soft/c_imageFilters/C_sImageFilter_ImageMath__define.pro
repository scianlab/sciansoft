;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageMath
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ImageMath' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageMath::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ImageMath::apply, image = image,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

    pParamStruct = selectedStackObject->getpParamStruct()
    totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]-1
    totalChannels = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]]-1

       ; get first image
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = time_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = channel_1

    if (time_1 eq -1) or (channel_1 eq -1) then image1 = image else $
       image1 = (selectedStackObject->getSelectedImage(tPos = time_1, chPos = channel_1, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]

       ; get second image
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_2 = (tPos < totalTimes)
    (*(*self.pParamStruct).pValues)[whParam] = time_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_2 = (chPos < totalChannels)
    (*(*self.pParamStruct).pValues)[whParam] = channel_2

    if (time_2 eq -1) or (channel_2 eq -1) then image1 = image else $
       image2 = (selectedStackObject->getSelectedImage(tPos = time_2, chPos = channel_2, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]

      ; get logical operator
    whereLog = (where((*(*self.pParamStruct).pNames) eq '0->Copy_1->EQ_2->AND_3->OR_4->NOT_5->Grow_6->Shrink'))[0]
    if (whereLog[0] eq -1) then whereLog = whParam+1
    if ( (*(*self.pParamStruct).pActive)[whereLog]) then logOperator = round((*(*self.pParamStruct).pValues)[whereLog]) else logOperator = 0

    case logOperator of
       0: return, 1. * image1 + image2
       1: return, 1. * image1 - image2
       2: return, 1. * image1 * image2
       3: return, 1. * image1 / (image2 > 1.)
       else: return, image1
    endcase
end


function C_sImageFilter_ImageMath::init
    ImageFilterStruct = {Name: 'C_ImageMath',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(5, /string, value = 'widget_slider')
    filterParamNames = ['1st_Time',$
                        '1st_Channel',$
                        '2nd_Time',$
                        '2nd_Channel',$
                        '0->Sum_1->Dif_2->Mult_3->Div']
    filterParamActive = [1, 1, 1, 1, 1]
    filterParamMin = [0, 0, 0, 0, 0]
    filterParamMax = [1000, 1000, 1000, 1000, 3]
    filterParamValues = [0, 0, 0, 0, 0]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageMath__define
  tmp = {C_sImageFilter_ImageMath, pParamStruct: ptr_new(), inherits C_sImageFilter}
end