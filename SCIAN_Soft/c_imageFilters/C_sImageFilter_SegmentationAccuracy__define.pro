;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_SegmentationAccuracy
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;     Jan Scheer
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ImageMath' )
;
; METHOHDS:
;
; USAGE:
;   (1) Specify Manually created Mask (Time, Channel, Z, CLuster, SegPos). Must be Binary Image
;   (2) Specify Automatically calculated Mask (Time, Channel, Z, CLuster, SegPos). To use the Original Image, set Time or Channel to -1.
;   Switch Time (checkbox) off if filter should be copied in Time Series 
;_____________________________IOISIOI____________________

function C_sImageFilter_SegmentationAccuracy::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end



function C_sImageFilter_SegmentationAccuracy::apply, image = image,$
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
    
    
    ; get image mask
    whParam = (where((*(*self.pParamStruct).pNames) eq 'ManualMask_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_1 = tPos
    (*(*self.pParamStruct).pValues)[whParam] = time_1

    whParam = (where((*(*self.pParamStruct).pNames) eq 'ManualMask_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = channel_1  
    whParam = (where((*(*self.pParamStruct).pNames) eq 'ManualMask_Z'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then z_1 = (round((*(*self.pParamStruct).pValues)[whParam])) else z_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = z_1
    whParam = (where((*(*self.pParamStruct).pNames) eq 'ManualMask_Cluster'))[0]
     if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_1 = (round((*(*self.pParamStruct).pValues)[whParam])) else cluster_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = cluster_1
    whParam = (where((*(*self.pParamStruct).pNames) eq 'ManualMask_SegPos'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then segPos_1 = (round((*(*self.pParamStruct).pValues)[whParam])) else segPos_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = segPos_1
    
    
    if(clusPos eq cluster_1 && tpos eq time_1 && segPos eq segPos_1 ) then begin
      print, 'The referenced mask cannot be the filter itself!'
      return, 0
    endif
    ; fix time for time series
    ;time_1 = tPos
    if (time_1 eq -1) or (channel_1 eq -1) then begin
      ;manual_mask = image 
    endif else begin
       ;image1 = (selectedStackObject->getSelectedImage(tPos = time_1, chPos = channel_1, zPos = zPos, clusPos = cluster_1, segPos = segPos_1))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
       oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = channel_1, zPos = z_1)
       manual_mask = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                         tPos = time_1 ,$
                                                         chPos = channel_1 ,$
                                                         zPos = z_1 ,$
                                                         clusPos = cluster_1,$
                                                         segPos = segPos_1,$
                                                         cut_x = cut_x, cut_y = cut_y)
    endelse


    ; get second image
     whParam = (where((*(*self.pParamStruct).pNames) eq 'AutomatedMask_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_2 = tPos
    (*(*self.pParamStruct).pValues)[whParam] = time_2
    whParam = (where((*(*self.pParamStruct).pNames) eq 'AutomatedMask_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_2 = -1
    (*(*self.pParamStruct).pValues)[whParam] = channel_2 
    whParam = (where((*(*self.pParamStruct).pNames) eq 'AutomatedMask_Z'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then z_2 = (round((*(*self.pParamStruct).pValues)[whParam])) else z_2 = -1
    (*(*self.pParamStruct).pValues)[whParam] = z_2
    whParam = (where((*(*self.pParamStruct).pNames) eq 'AutomatedMask_Cluster'))[0]
     if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_2 = (round((*(*self.pParamStruct).pValues)[whParam])) else cluster_2 = -1
    (*(*self.pParamStruct).pValues)[whParam] = cluster_2
    whParam = (where((*(*self.pParamStruct).pNames) eq 'AutomatedMask_SegPos'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then segPos_2 = (round((*(*self.pParamStruct).pValues)[whParam])) else segPos_2 = -1
    (*(*self.pParamStruct).pValues)[whParam] = segPos_2
    
    
    if(clusPos eq cluster_2 && tpos eq time_2 && segPos eq segPos_2 ) then begin
      print, 'The referenced 2nd image cannot be the filter itself!'
      return, 0
    endif
     ; fix time for time series
    ;time_2 = tPos
    if (time_2 eq -1) or (channel_2 eq -1) then begin
      ;image_2 = image 
    endif else begin
       ;image1 = (selectedStackObject->getSelectedImage(tPos = time_1, chPos = channel_1, zPos = zPos, clusPos = cluster_1, segPos = segPos_1))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
       oImage = selectedStackObject->getSelectedImageObject(tPos = time_2, chPos = channel_2, zPos = z_2)
       automated_mask = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                         tPos = time_2 ,$
                                                         chPos = channel_2 ,$
                                                         zPos = z_2 ,$
                                                         clusPos = cluster_2,$
                                                         segPos = segPos_2,$
                                                         cut_x = cut_x, cut_y = cut_y)
                                                      
    endelse
    
    ;automated_mask =shift(automated_mask,5,0)
    
    manual_vis = manual_mask
    window, 10, ysize=400, xsize=400
    tvscl, congrid(manual_mask, 400,400)
    
    auto_vis = automated_mask
    window, 11, ysize=400, xsize=400
    tvscl, congrid(automated_mask, 400,400)

    
    return_image = image
    
;    ind_manualPositive = where(manual_mask gt 0)
;    ind_manualNegative = where(manual_mask eq 0)
;    ind_manualPositiveSize = size(ind_manualPositive, /dimension)
;    ind_manualNegativeSize = size(ind_manualNegative, /dimension)
;    Print, 'Object (white): manually segmented/classified pixels.............:   ', ind_manualPositiveSize
;    Print, 'Background (black): manually segmented/classified pixels.........:   ', ind_manualNegativeSize
;    
;    
;    ind_automatedPositive = where(automated_mask gt 0)
;    ind_automatedNegative = where(automated_mask eq 0)
;    ind_automatedPositiveSize = size(ind_automatedPositive, /dimension)
;    ind_automatedNegativeSize = size(ind_automatedNegative, /dimension)
;    Print, 'Object (white): automatically segmented/classified pixels........:   ', ind_automatedPositiveSize
;    Print, 'Background (black): automatically segmented/classified pixels....:   ', ind_automatedNegativeSize
;    

   
   
    ; Specifity: correct positives

    correctPositives = LOGICAL_AND(automated_mask, manual_mask)
    window, 18, xpos=400, ysize=400, xsize=400
    tvscl, congrid(correctPositives, 400,400)
    ind_correctPositives = where(correctPositives gt 0)
    ind_correctPositivesSize = size(ind_correctPositives, /dimension)
    
    falsePositives = LOGICAL_AND(automated_mask, not(manual_mask))
    falsePositives(ind_correctPositives)=0
    window, 19, xpos=400, ysize=400, xsize=400
    tvscl, congrid(falsePositives, 400,400)
    ind_falsePositives = where(falsePositives gt 0)
    ind_falsePositivesSize = size(ind_falsePositives, /dimension)

    
    correctNegatives = not(LOGICAL_OR(automated_mask, manual_mask))
    window, 20, ysize=400, xsize=400
    tvscl, congrid(correctNegatives, 400,400)
    ind_correctNegatives = where(correctNegatives gt 0)
    ind_correctNegativesSize = size(ind_correctNegatives, /dimension)
    
    dummy = NOT(LOGICAL_AND(manual_mask,correctPositives))
    falseNegatives = LOGICAL_AND(manual_mask,dummy)
    window, 21, ysize=400, xsize=400
    tvscl, congrid(falseNegatives, 400,400) 
    ind_falseNegatives = where(falseNegatives gt 0)
    ind_falseNegativesSize = size(ind_falseNegatives, /dimension)

    
    sensivity = (float(ind_correctPositivesSize) / (ind_correctPositivesSize+ind_falseNegativesSize))
    specificity = (float(ind_correctNegativesSize) / (ind_correctNegativesSize+ind_falsePositivesSize))
    print, "Segmentation Sensivity: ", sensivity*100, '%'
    print, "Segmentation Specificity: ", specificity*100, '%'
    
    ;ind_same = where(automated_mask and manual_mask)
    ;ind_same_size = size(ind_same, /dimension)
    
    ;perecent = float(ind_same_size) / ind_man_size
    ;print, "Segmentation Accuracy: ", perecent*100, '%'

    return, return_image
end


function C_sImageFilter_SegmentationAccuracy::init
    ImageFilterStruct = {Name: 'C_SegmentationAccuracy',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(10, /string, value = 'widget_slider')
    filterParamNames = ['ManualMask_Time',$
                        'ManualMask_Channel',$
                        'ManualMask_Z',$
                        'ManualMask_Cluster',$
                        'ManualMask_SegPos',$
                        'AutomatedMask_Time',$
                        'AutomatedMask_Channel',$
                        'AutomatedMask_Z',$
                        'AutomatedMask_Cluster',$
                        'AutomatedMask_SegPos']
                        
    filterParamActive = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    filterParamMin = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    filterParamMax = [1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000]
    filterParamValues = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_SegmentationAccuracy__define
  tmp = {C_sImageFilter_SegmentationAccuracy,$
          pParamStruct:ptr_new(),$
          inherits C_sImageFilter}
end