;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_BorderDistance
;
; PURPOSE:
;       - BorderDistance-Filter-Class. See BorderDistance.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2011)
;     e_mail: shartel@med.uchile.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_BorderDistance' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function c_sImageFilter_ImageBorderDistance::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function c_sImageFilter_ImageBorderDistance::calculate, maskImage = maskImage, frapImage = frapImage

   dimImage = size(maskImage, /dim) 

      ; make sure that images are binary
   maskImage gt= 0
   frapImage gt= 0
   
      ; define mask in and out frap-region
   frapMaskIn = (frapImage * maskImage) gt 0
   frapMaskOut = maskImage - frapMaskIn

   distance = 1.
   returnImage = make_array(dimImage, /float) 
   repeat begin
   
         ; define border of frap-region 4 and 8 and only 8
      border4 = shift(frapMaskOut,1,0) > shift(frapMaskOut,-1,0) > shift(frapMaskOut,0,1) > shift(frapMaskOut,0,-1)
      border8 = border4 > shift(frapMaskOut,1,1) > shift(frapMaskOut,-1,1) > shift(frapMaskOut,1,-1) > shift(frapMaskOut,-1,-1)
      border8Only = border8 ne border4
      
      border4 *= frapMaskIn
      border8Only *= frapMaskIn
      border8 *= frapMaskIn
   
         ; check connectivity with at least one 4-neigbour to avoid ROI jumping and update border8Only
      border8 *= (shift(border8,1,0) > shift(border8,-1,0) > shift(border8,0,1) > shift(border8,0,-1))
      border8Only *= border8
      border4 *= border8
      
      returnImage += ((border4 * distance) + (border8Only * (distance + sqrt(2.))))
      
;      window, 12, xsize = dimImage[0]*4, ysize = dimImage[1]*4
;      a = congrid(frapMaskOut, dimImage[0]*4, dimImage[1]*4)
;      b = a * 2 + congrid(frapMaskIn, dimImage[0]*4, dimImage[1]*4)
;      c = b * 2 + congrid(border8Only, dimImage[0]*4, dimImage[1]*4) 
;      tvscl, a
;      tvscl, b   
;      tvscl, c
;      tvscl, congrid(returnImage, dimImage[0]*4, dimImage[1]*4)
      
      frapMaskOut += border8
      frapMaskIn -= border8
      distance += 1.
   
   endrep until (max(border8) eq 0)

   return, returnImage
end


function c_sImageFilter_ImageBorderDistance::apply, image = image, maskImage = maskImage, frapImage = frapImage,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

    if (size(image, /n_dim) ne 2) then return, image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'xxx'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then xxx = (*(*self.pParamStruct).pValues)[whParam] + 1 $
       else xxx = 1

    whParam = (where((*(*self.pParamStruct).pNames) eq 'xx'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then xx = (*(*self.pParamStruct).pValues)[whParam] $
       else xx = 4

    if (n_elements(maskImage) eq 0) then begin 
       selectedStackObject->getSelectedClusterMask, mask = maskImage, tPos = 0, chPos = 0, zPos = 0, clusPos = 0
       if ((n_elements(maskImage) eq 1) and (maskImage[0] eq -1)) then begin
          maskImage = image
       endif else maskImage = maskImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    endif

    if (n_elements(frapImage) eq 0) then begin 
       selectedStackObject->getSelectedClusterMask, mask = frapImage, tPos = 0, chPos = 0, zPos = 0, clusPos = 1
       if ((n_elements(frapImage) eq 1) and (frapImage[0] eq -1)) then begin
          frapImage = image
       endif else frapImage = frapImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    endif

    return, self->calculate(maskImage = maskImage, frapImage = frapImage)
end


function c_sImageFilter_ImageBorderDistance::init

    filterStruct = {Name: 'C_ImageBorderDistance',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of BorderDistance.
    filterParamWidgetType = make_array(2, /string, value = 'widget_slider')
    filterParamNames = ['xxx', 'xx']
    filterParamActive = [1,0]
    filterParamMin = [4,0]
    filterParamMax = [8,1000]
    filterParamValues = [8,10]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end


pro c_sImageFilter_ImageBorderDistance__define
  tmp = {c_sImageFilter_ImageBorderDistance, pParamStruct: ptr_new(), inherits C_sImageFilter}
end