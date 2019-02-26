;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageIsoAlpha
;
; PURPOSE:
;       - ImageIsoAlpha-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ImageIsoAlpha' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageIsoAlpha::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_ImageIsoAlpha::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y,$
                                     fKeepFilterMatrix = fKeepFilterMatrix

   if (n_elements(fKeepFilterMatrix) eq 0) then fKeepFilterMatrix = 0b

       ; get Image Object from Container
    case chPos of
       0 : chPos = 1
       else: chPos = (chPos -1) > 0
    endcase
    oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    if (obj_valid(oImage)) then image_2 = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject,$
                                                         tPos = tPos,$
                                                         chPos = chPos,$
                                                         zPos = zPos,$
                                                         clusPos = clusPos,$
                                                         segPos = segPos-1,$
                                                         cut_x = cut_x, cut_y = cut_y) $
       else image_2 = image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'PixRad1'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then pixRad1 = round((*(*self.pParamStruct).pValues)[whParam]) > 0 else pixRad1 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'PixRad2'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then pixRad2 = round((*(*self.pParamStruct).pValues)[whParam]) > (pixRad1+1) else pixRad2 = 1 > (pixRad1+1)
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Int1 Channel0'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then int1ch0 = round((*(*self.pParamStruct).pValues)[whParam]) > 0 else int1ch0 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Int2 Channel0'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then int2ch0 = round((*(*self.pParamStruct).pValues)[whParam]) > (int1ch0+1) else int2ch0 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Int1 Channel1'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then int1ch1 = round((*(*self.pParamStruct).pValues)[whParam]) > 0 else int1ch1 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Int2 Channel1'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then int2ch1 = round((*(*self.pParamStruct).pValues)[whParam]) > (int1ch1+1) else int2ch1 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Return AlphaN1'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then fReturnAlpha1 = 1b else fReturnAlpha1 = 0b
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Return AlphaN2'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then fReturnAlpha2 = 1b else fReturnAlpha2 = 0b
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Random Values for Channel 0'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then randomValues0 = 1b else randomValues0 = 0b
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Random Values for Channel 1'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then randomValues1 = 1b else randomValues1 = 0b

   dimImage   = size(image, /dim)
   if (randomValues0) then begin
     image = randomn(seed, imageDim[0], imageDim[1])
     image = round((image-min(image))/(max(image)-min(image))*255.)
     window, 10
     plot, image
;   plot, histogram(image)
     tvscl, image
   endif
   if (randomValues1) then begin
     image_2 = randomn(seed, imageDim[0], imageDim[1])
     image_2 = round((image_2-min(image_2))/(max(image_2)-min(image_2))*255.)
     window, 11
     plot, histogram(image_2)
     tvscl, image_2
   endif

   distImage = fltArr(dimImage[0],dimImage[1])    ; Distance Matrix
   normImage1  = 1. * image
   normImage2  = 1. * image_2
   alphan1   = distImage + 1.   ; Matrix for rad1
   alphan2  = alphan1   ; Matrix for rad2

   expImage_1  = s_Expand_Mirror(image, PixRad2)
    expImage_2  = s_Expand_Mirror(image_2, pixRad2)
   r1MaxCh0 = sqrt(pixRad1^2+int1ch0^2+int1ch1^2)
   r2MaxCh0 = sqrt(pixRad2^2+int2ch0^2+int2ch1^2)
   for k=-pixRad2,pixRad2 do for l=-pixRad2,pixRad2 do begin
           pixDist = 1.0 * (k*k+l*l)
           if (pixDist le (pixRad2)^2 ) and (pixDist ne 0) then begin
             distImage = sqrt(  ((expImage_1(pixRad2+k : pixRad2+k+dimImage[0]-1, pixRad2+l : pixRad2+l+dimImage[1]-1) - normImage1 )^2) + $
                             ((expImage_2(pixRad2+k : pixRad2+k+dimImage[0]-1, pixRad2+l : pixRad2+l+dimImage[1]-1) - normImage2 )^2) + pixDist)
         alphan1 = temporary(alphan1) + (distImage le r1MaxCh0)
         alphan2 = temporary(alphan2) + (distImage le r2MaxCh0)
      endif
   endfor
   if (fReturnAlpha1) then return, alphan1
   if (fReturnAlpha2) then return, alphan2
   return, alog(alphan2/alphan1) / alog((r2MaxCh0>2)/((r1MaxCh0>1)))
end


function C_sImageFilter_ImageIsoAlpha::init
    imageFilterStruct = {Name: 'C_ImageIsoAlpha',$   ;  filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on filter Parameter Min_Values.
                           pMax:ptr_new(),$    ; Pointer on filter Parameter Max_Values.
                           pValues:ptr_new()}    ; Pointer on filter Parameter Values.

    filterParamWidgetType = make_array(10, /string, value = 'widget_slider')
    filterParamNames = ['PixRad1', 'PixRad2', 'Int1 Channel0', 'Int2 Channel0', 'Int1 Channel1', 'Int2 Channel1',$
                       'Return AlphaN1', 'Return AlphaN2', 'Random Values for Channel 0', 'Random Values for Channel 1']
    filterParamActive = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0]
    filterParamMin =   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    filterParamMax =   [254, 255, 49, 50, 1, 1, 1, 1, 1, 1]
    filterParamValues =    [0, 1, 0, 1, 0, 1, 0, 0, 0, 0]

    imageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    imageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    imageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    imageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    imageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    imageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(imageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageIsoAlpha__define
    tmp = {C_sImageFilter_ImageIsoAlpha, pParamStruct: ptr_new(), inherits C_sImageFilter}
end