;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_MathematicalFilter
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_MathematicalFilter' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_MathematicalFilter::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_MathematicalFilter::apply, image = image,$
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

       ; decide if 1st_Self_or 2nd_Self
    fSelf = 0b
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Self_or 2nd_Self'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then fSelf = (*(*self.pParamStruct).pValues)[whParam]

       ; get first mask
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_1 = (tPos < totalTimes)
    (*(*self.pParamStruct).pValues)[whParam] = time_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_1 = (chPos < totalChannels)
    (*(*self.pParamStruct).pValues)[whParam] = channel_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Cluster'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_1 = round((*(*self.pParamStruct).pValues)[whParam]) else cluster_1 = clusPos

    if (fSelf eq 0) or (fSelf eq 2) then begin
       selectedStackObject->getSelectedClusterMask, mask = mask_1, tPos = time_1, chPos = channel_1, zPos = zPos, clusPos = cluster_1
       if ((n_elements(mask_1) eq 1) and (mask_1[0] eq -1)) then begin
          oImage = selectedStackObject->getSelectedImageObject(tPos = time_1, chPos = channel_1, zPos = zPos)
          mask_1 = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                            tPos = time_1,$
                                                            chPos = channel_1,$
                                                            zPos = zPos,$
                                                            clusPos = cluster_1,$
                                                            cut_x = cut_x, cut_y = cut_y)
       endif else mask_1 = mask_1[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    endif
    if (fSelf eq 1) then mask_1 = image

       ; get second mask
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_2 = (tPos < totalTimes)
    (*(*self.pParamStruct).pValues)[whParam] = time_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_2 = (chPos < totalChannels)
    (*(*self.pParamStruct).pValues)[whParam] = channel_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Cluster'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_2 = round((*(*self.pParamStruct).pValues)[whParam]) else cluster_2 = clusPos
    
    if (fSelf eq 0) or (fSelf eq 1) then begin
       selectedStackObject->getSelectedClusterMask, mask = mask_2, tPos = time_2, chPos = channel_2, zPos = zPos, clusPos = cluster_2
       if ((n_elements(mask_2) eq 1) and (mask_2[0] eq -1)) then begin
          oImage = selectedStackObject->getSelectedImageObject(tPos = time_2, chPos = channel_2, zPos = zPos)
          mask_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                            tPos = time_2,$
                                                            chPos = channel_2,$
                                                            zPos = zPos,$
                                                            clusPos = cluster_2,$
                                                            cut_x = cut_x, cut_y = cut_y)
       endif else mask_2 = mask_2[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    endif
    if (fSelf eq 2) then mask_2 = image

      ; get logical operator
    whParam = (where((*(*self.pParamStruct).pNames) eq '0->Copy_1->EQ_2->AND_3->OR_4->NOT_5->Grow_6->Shrink_7->Touch_8->Mult_9->KeepBigger'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then logOperator = round((*(*self.pParamStruct).pValues)[whParam]) else logOperator = 0

    case logOperator of
       0: return, mask_1
       1: return, (mask_1 ne 0) eq (mask_2 ne 0)
       2: return, (mask_1 ne 0) * (mask_2 ne 0)
       3: return, (mask_1 ne 0) > (mask_2 ne 0)
       4: begin
          whParam = where(mask_2 ne 0)
          if (whParam[0] ne -1) then mask_1[whParam] = 0
          return, mask_1
         endcase
       5: return, (mask_2 ne 0) * (mask_1 eq 0)
       6: return, (mask_2 eq 0) * (mask_1 ne 0)
       7: begin
          labelMask_2 = label_region(mask_2)
          maxLM = max(labelMask_2)
          if (maxLM ge 1) then begin
             mask_1 = (mask_1 ne 0) + (mask_2 ne 0)
             for i = 1, maxLM do begin
                whereObj = where(labelMask_2 eq i)
                if (max(mask_1[whereObj]) eq 1) then mask_2[whereObj] = 0
             endfor
             return, (mask_2 ne 0)
          endif else return, mask_2
       endcase
       8: return, mask_2 * mask_1
       9: return, mask_1 > mask_2
       else: return, mask_1
    endcase
end


function C_sImageFilter_MathematicalFilter::init
    ImageFilterStruct = {Name: 'C_MathematicalFilter',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(8, /string, value = 'widget_slider')
    filterParamNames = ['1st_Time',$
                        '1st_Channel',$
                        '1st_Cluster',$
                        '2nd_Time',$
                        '2nd_Channel',$
                        '2nd_Cluster',$
                        '1st_Self_or 2nd_Self',$
                        '0->Copy_1->EQ_2->AND_3->OR_4->NOT_5->Grow_6->Shrink_7->Touch_8->Mult_9->KeepBigger']
    filterParamActive = [1, 1, 1, 1, 1, 1, 0, 1]
    filterParamMin = [0, 0, 0, 0, 0, 0, 1, 0]
    filterParamMax = [1000, 1000, 1000, 1000, 1000, 1000, 2, 9]
    filterParamValues = [0, 0, 0, 0, 0, 0, 1, 0]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_MathematicalFilter__define
  tmp = {C_sImageFilter_MathematicalFilter, pParamStruct: ptr_new(), inherits C_sImageFilter}
end