;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Recovery_FullScale
;
; PURPOSE:
;       - use a binary mask over a colored mask.
;
; AUTHOR:
;     Felipe Santibañez
;     e_mail: fsantibanez@med.uchile.cl
;     modified by FASL 2011
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Recovery_FullScale' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_Recovery_FullScale::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_Recovery_FullScale::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_Recovery_FullScale::apply, image = image,$
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

    colorFondo = 0.0d
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Background Color'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then colorFondo = (*(*self.pParamStruct).pValues)[whParam]
    
       ; decide if use last binary or selected binary
    fUseLast = 0b
    whParam = (where((*(*self.pParamStruct).pNames) eq 'LastBinarymask or SelectedBinaryMask'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then fUseLast = (*(*self.pParamStruct).pValues)[whParam]

       ; get first mask
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_1 = (tPos < totalTimes)
    (*(*self.pParamStruct).pValues)[whParam] = time_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_1 = (chPos < totalChannels)
    (*(*self.pParamStruct).pValues)[whParam] = channel_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Cluster'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_1 = round((*(*self.pParamStruct).pValues)[whParam]) else cluster_1 = clusPos

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
     
     whParam = (where((*(*self.pParamStruct).pNames) eq 'Background Color'))[0]     
     tempValueMax = max(mask_1, min = tempValueMin)
     (*(*self.pParamStruct).pMax)[whParam] = tempValueMax

       ; get second mask
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_2 = (tPos < totalTimes)
    (*(*self.pParamStruct).pValues)[whParam] = time_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_2 = (chPos < totalChannels)
    (*(*self.pParamStruct).pValues)[whParam] = channel_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Cluster'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_2 = round((*(*self.pParamStruct).pValues)[whParam]) else cluster_2 = clusPos
    
    if (fUseLast eq 1) then begin
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
    endif else mask_2 = image
  
  ; mask_1 is the FullScale Image
  ; mask_2 is the binary DATA
  mask1 = double(mask_1)*double(mask_2)
  indices = where(mask_2 eq 0)
  if(colorFondo ge 0) then mask1[indices] = colorFondo
  return, mask_1
end

function C_sImageFilter_Recovery_FullScale::init
    ImageFilterStruct = {Name: 'C_sImageFilter_Recovery_FullScale',$   ;  Filter Name.
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
                        'LastBinarymask or SelectedBinaryMask',$
                        'Background Color']
    filterParamActive = [1, 1, 1, 1, 1, 1, 1, 0]
    filterParamMin = [0, 0, 0, 0, 0, 0, 0, -1]
    filterParamMax = [1000, 1000, 1000, 1000, 1000, 1000, 1, 255]
    filterParamValues = [0, 0, 0, 0, 0, 0, 0, 0]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Recovery_FullScale__define
  tmp = {C_sImageFilter_Recovery_FullScale, pParamStruct: ptr_new(), inherits C_sImageFilter}
end