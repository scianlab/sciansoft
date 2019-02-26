;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_SwitchObjectNumbers
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_SwitchObjectNumbers' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_SwitchObjectNumbers::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_SwitchObjectNumbers::apply, image = image,$
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

       ; get mask
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_1 = (tPos < totalTimes)
    (*(*self.pParamStruct).pValues)[whParam] = time_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_1 = (chPos < totalChannels)
    (*(*self.pParamStruct).pValues)[whParam] = channel_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Cluster'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_1 = round((*(*self.pParamStruct).pValues)[whParam]) else cluster_1 = clusPos

    selectedStackObject->getSelectedClusterMask, mask = mask, tPos = time_1, chPos = channel_1, zPos = zPos, clusPos = cluster_1
    if ((n_elements(mask) eq 1) and (mask[0] eq -1)) then begin
       oImage = selectedStackObject->getSelectedImageObject(tPos = time_1, chPos = channel_1, zPos = zPos)
       mask = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                         tPos = time_1,$
                                                         chPos = channel_1,$
                                                         zPos = zPos,$
                                                         clusPos = cluster_1,$
                                                         cut_x = cut_x, cut_y = cut_y)
    endif else mask = mask[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]

    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Object_Old'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then oldNum1 = (*(*self.pParamStruct).pValues)[whParam] else oldNum1 = -1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Object_New'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then newNum1 = (*(*self.pParamStruct).pValues)[whParam] else newNum1 = -1
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Object_Old'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then oldNum2 = (*(*self.pParamStruct).pValues)[whParam] else oldNum2 = -1
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Object_New'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then newNum2 = (*(*self.pParamStruct).pValues)[whParam] else newNum2 = -1

    case 1 of
       (oldNum1 ne -1) and (newNum1 ne -1) and (oldNum2 ne -1) and (newNum2 ne -1): begin
            whereOldNum1 = where(mask eq oldNum1, count1)
            whereOldNum2 = where(mask eq oldNum2, count2)
            if (count1 ne 0) then mask[whereOldNum1] = newNum1
            if (count2 ne 0) then mask[whereOldNum2] = newNum2
         endcase
       (oldNum1 ne -1) and (newNum1 ne -1): begin
            whereOldNum1 = where(mask eq oldNum1, count)
            if (count ne 0) then mask[whereOldNum1] = newNum1
         endcase
       (oldNum2 ne -1) and (newNum2 ne -1): begin
            whereOldNum2 = where(mask eq oldNum2, count)
            if (count ne 0) then mask[whereOldNum2] = newNum2
         endcase
       else: return, mask
    endcase
    return, mask
end


function C_sImageFilter_SwitchObjectNumbers::init
    ImageFilterStruct = {Name:'C_SwitchObjectNumbers',$   ; Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}         ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(7, /string, value = 'widget_slider')
    filterParamNames = ['1st_Time',$
                        '1st_Channel',$
                        '1st_Cluster',$
                        '1st_Object_Old',$
                        '1st_Object_New',$
                        '2nd_Object_Old',$
                        '2nd_Object_New']

    filterParamActive = [1, 1, 1, 1, 1, 1, 1]
    filterParamMin = [0, 0, 0, 0, 0, 0, 0]
    filterParamMax = [1000, 1000, 1000, 1000, 1000, 1000, 1000]
    filterParamValues = [0, 0, 0, 0, 0, 0, 0]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_SwitchObjectNumbers__define
   tmp = {C_sImageFilter_SwitchObjectNumbers, pParamStruct:ptr_new(), inherits C_sImageFilter}
end