;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageMaskMath
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
;   (1) Specify Image Mask (Time, Channel, Z, CLuster, SegPos). Must be Binary Image
;   (2) Specify Second Image (Time, Channel, Z, CLuster, SegPos). To use the Original Image, set Time or Channel to -1.
;   (3) Select ONE of the Math Calculations to perform Image Mask Math
;   Switch Time (checkbox) off if filter should be copied in Time Series 
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageMaskMath::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_ImageMaskMath::checkParamsApplied
; compares the parameter values between the current values and the values applied in the last segmentation
   if ptr_valid(self.pParamApplied) then begin
      if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
         *self.pParamApplied = *(*self.pParamStruct).pValues
         return, 1
      endif
   endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
   return, 0
end


function C_sImageFilter_ImageMaskMath::apply, image = image,$
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
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Mask_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_1 = tPos
    (*(*self.pParamStruct).pValues)[whParam] = time_1

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Mask_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = channel_1  
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Mask_Z'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then z_1 = (round((*(*self.pParamStruct).pValues)[whParam])) else z_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = z_1
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Mask_Cluster'))[0]
     if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_1 = (round((*(*self.pParamStruct).pValues)[whParam])) else cluster_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = cluster_1
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Mask_SegPos'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then segPos_1 = (round((*(*self.pParamStruct).pValues)[whParam])) else segPos_1 = -1
    (*(*self.pParamStruct).pValues)[whParam] = segPos_1
    
    
    if(clusPos eq cluster_1 && tpos eq time_1 && segPos eq segPos_1 ) then begin
      print, 'The referenced mask cannot be the filter itself!'
      return, 0
    endif
    ; fix time for time series
    ;time_1 = tPos
    if (time_1 eq -1) or (channel_1 eq -1) then begin
     image_mask = image 
    endif else begin
       ;image1 = (selectedStackObject->getSelectedImage(tPos = time_1, chPos = channel_1, zPos = zPos, clusPos = cluster_1, segPos = segPos_1))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
       oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = channel_1, zPos = z_1)
       image_mask = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                         tPos = time_1 ,$
                                                         chPos = channel_1 ,$
                                                         zPos = z_1 ,$
                                                         clusPos = cluster_1,$
                                                         segPos = segPos_1,$
                                                         cut_x = cut_x, cut_y = cut_y)
    endelse


    ; get second image
     whParam = (where((*(*self.pParamStruct).pNames) eq '2ndImage_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_2 = tPos
    (*(*self.pParamStruct).pValues)[whParam] = time_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2ndImage_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_2 = -1
    (*(*self.pParamStruct).pValues)[whParam] = channel_2 
    whParam = (where((*(*self.pParamStruct).pNames) eq '2ndImage_Z'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then z_2 = (round((*(*self.pParamStruct).pValues)[whParam])) else z_2 = -1
    (*(*self.pParamStruct).pValues)[whParam] = z_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2ndImage_Cluster'))[0]
     if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_2 = (round((*(*self.pParamStruct).pValues)[whParam])) else cluster_2 = -1
    (*(*self.pParamStruct).pValues)[whParam] = cluster_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2ndImage_SegPos'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then segPos_2 = (round((*(*self.pParamStruct).pValues)[whParam])) else segPos_2 = -1
    (*(*self.pParamStruct).pValues)[whParam] = segPos_2
    
    
    if(clusPos eq cluster_2 && tpos eq time_2 && segPos eq segPos_2 ) then begin
      print, 'The referenced 2nd image cannot be the filter itself!'
      return, 0
    endif
     ; fix time for time series
    time_2 = tPos
    if (time_2 eq -1) or (channel_2 eq -1) then begin
      image_2 = image 
    endif else begin
       ;image1 = (selectedStackObject->getSelectedImage(tPos = time_1, chPos = channel_1, zPos = zPos, clusPos = cluster_1, segPos = segPos_1))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
       oImage = selectedStackObject->getSelectedImageObject(tPos = time_2, chPos = channel_2, zPos = z_2)
       image_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                         tPos = time_2 ,$
                                                         chPos = channel_2 ,$
                                                         zPos = z_2 ,$
                                                         clusPos = cluster_2,$
                                                         segPos = segPos_2,$
                                                         cut_x = cut_x, cut_y = cut_y)
                                                      
    endelse
    
    
    return_image = image
    ; get logical operator
;    whereLog = (where((*(*self.pParamStruct).pNames) eq 'SubstractBackground_Original'))[0]
;    if (whereLog[0] eq -1) then whereLog = whParam+1
;    if ( (*(*self.pParamStruct).pActive)[whereLog]) then begin
;          ; indeces in mask (object)
;          ind_mask = where(image_mask gt 0)
;          ; indeces out of mask (background)
;          ind_outOfMask = where(image_mask eq 0)
;          ; intensities in original image OUTSIDE of mask
;          OrgIntensitiesOutsideOfMask = image(ind_outOfMask)
;          ; histogram of these intensities (background object)
;          histoOutsideOriginal = histogram(OrgIntensitiesOutsideOfMask)
;          ; determine background intensity
;          maxh = max(histoOutsideOriginal)
;          ind_maxh = where(histoOutsideOriginal eq maxh)
;          
;         ;          intensitiesInMask = image(ind_mask)
;         ;          histInside = histogram(intensitiesInMask)
;         ;          maxi = max(histInside)
;         ;          ind_maxi = where(histInside eq maxi)
;
;          bgInt  = ind_maxh[0]
;          ; determine indeces of intensities in original image <= background
;          indeces = where(image le bgInt)  
;          print, 'Subtract Background Intensity: '+bgInt+'.' 
;          ; subtract / clip to 0
;          bgSubtractedImage = (image-bgInt)
;          bgSubtractedImage(indeces) = 0
;          return_image = bgSubtractedImage
;          image = return_image
;    endif
    
    whereLog = (where((*(*self.pParamStruct).pNames) eq 'SubstractBackground_in2ndImage'))[0]
    if (whereLog[0] eq -1) then whereLog = whParam+1
    if ( (*(*self.pParamStruct).pActive)[whereLog]) then begin
          ; indeces in mask (object)
          ind_mask = where(image_mask gt 0)
          ; indeces out of mask (background)
          ind_outOfMask = where(image_mask eq 0)
           ; intensities in 2nd image OUTSIDE of mask
          intensitiesOutsideOfMask = image_2(ind_outOfMask)
          ; histogram of these intensities (background object)
          histoOutsideOriginal = histogram(intensitiesOutsideOfMask)
          ; determine background intensity
          maxh = max(histoOutsideOriginal)
          ind_maxh = where(histoOutsideOriginal eq maxh)
          bgInt  = ind_maxh[0]
           ; subtract / clip to 0
          indeces = where(image_2 le bgInt)
         ;          
         ;                   intensitiesInMask = image(ind_mask)
         ;                   histInside = histogram(intensitiesInMask)
         ;                   maxi = max(histInside)
         ;                   ind_maxi = where(histInside eq maxi)
         ;                    bgInt  = ind_maxi[0]  
          print, 'Subtract Background Intensity: '+bgInt+'.' 
          bgSubtractedImage = (image_2 - bgInt)
          sizeInd = size(indeces, /dimension)
          if(sizeInd ge 1) then bgSubtractedImage(indeces) = 0
          return_image = bgSubtractedImage
    endif
    
    whereLog = (where((*(*self.pParamStruct).pNames) eq 'SubstractMask_in2ndImage'))[0]
    if (whereLog[0] eq -1) then whereLog = whParam+1
    if ( (*(*self.pParamStruct).pActive)[whereLog]) then begin
          ind_mask = where(image_mask gt 0)
          ind_outOfMask = where(image_mask eq 0)
          image_2(ind_mask) = 0  
          return_image = image_2
    endif
    
;    whereLog = (where((*(*self.pParamStruct).pNames) eq 'SubstractMask_inOriginal'))[0]
;    if (whereLog[0] eq -1) then whereLog = whParam+1
;    if ( (*(*self.pParamStruct).pActive)[whereLog]) then begin
;          ind_mask = where(image_mask gt 0)
;          ind_outOfMask = where(image_mask eq 0)
;          image(ind_mask) = 0  
;          return_image = image
;    endif
    
    whereLog = (where((*(*self.pParamStruct).pNames) eq 'SubstractForeground_in2ndImage'))[0]
    if (whereLog[0] eq -1) then whereLog = whParam+1
    if ( (*(*self.pParamStruct).pActive)[whereLog]) then begin
          ; indeces in mask (object)
          ind_mask = where(image_mask gt 0)
          ; indeces out of mask (background)
          ind_outOfMask = where(image_mask eq 0)
          ; intensities in original image OUTSIDE of mask
          OrgIntensitiesInsideOfMask = image(ind_mask)
          ; histogram of these intensities (background object)
          histoInsideOriginal = histogram(OrgIntensitiesInsideOfMask)
          ; determine background intensity
          maxh = max(histoInsideOriginal)
          ind_maxh = where(histoInsideOriginal eq maxh)
          bgInt  = ind_maxh[0]
          ; determine indeces of intensities in original image <= background
          indeces = where(image le bgInt)  
          print, 'Subtract Background Intensity: '+bgInt+'.' 
          ; subtract / clip to 0
          bgSubtractedImage = (image-bgInt)
          bgSubtractedImage(indeces) = 0
          return_image = bgSubtractedImage
    endif
    
    
    whereLog = (where((*(*self.pParamStruct).pNames) eq 'MultiplyMask_with2ndImage'))[0]
    if (whereLog[0] eq -1) then whereLog = whParam+1
    if ( (*(*self.pParamStruct).pActive)[whereLog]) then begin
          ; indeces in mask (object)
          ind_mask = where(image_mask gt 0)
          ; indeces out of mask (background)
          ind_outOfMask = where(image_mask eq 0)
          image_2(ind_outOfMask) = 0
          return_image = image_2
    endif
    
   return, return_image
end


function C_sImageFilter_ImageMaskMath::init
    ImageFilterStruct = {Name: 'C_ImageMaskMath',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(14, /string, value = 'widget_slider')
    filterParamNames = ['Mask_Time',$
                        'Mask_Channel',$
                        'Mask_Z',$
                        'Mask_Cluster',$
                        'Mask_SegPos',$
                        '2ndImage_Time',$
                        '2ndImage_Channel',$
                        '2ndImage_Z',$
                        '2ndImage_Cluster',$
                        '2ndImage_SegPos',$
                        'SubstractMask_in2ndImage',$
                        'SubstractBackground_in2ndImage',$
                        'SubstractForeground_in2ndImage', $
                        'MultiplyMask_with2ndImage']
    filterParamActive = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
    filterParamMin = [0, 0, 0, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0]
    filterParamMax = [1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1, 1, 1, 1]
    filterParamValues = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    ;self.pParamApplied = ptr_new(ImageFilterStruct, /no_copy);
    ;self.pSegImage = ptr_new(ImageFilterStruct, /no_copy);
    return, 1
end

pro C_sImageFilter_ImageMaskMath__define
  tmp = {C_sImageFilter_ImageMaskMath,$
          pParamStruct:ptr_new(),$
          inherits C_sImageFilter}
  ;tmp = {C_sImageFilter_ImageMaskMath, pParamStruct: ptr_new(), pParamApplied: ptr_new(), pSegImage: ptr_new(), inherits C_sImageFilter}
end