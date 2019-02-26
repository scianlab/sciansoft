;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageZProjection
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;   Dr. Steffen Härtel (2009)
;   e_mail:shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;   result = obj_new('C_sImageFilter_ImageZProjection')
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageZProjection::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ImageZProjection::apply, image = image,$
                        selectedStackObject = selectedStackObject,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos,$
                        chPos = chPos,$
                        zPos = zPos,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y, track = track

   whPos = (where((*(*self.pParamStruct).pNames) eq 'Max Projection'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then maxP = 1b else maxP = 0b

   whPos = (where((*(*self.pParamStruct).pNames) eq 'Mean Projection'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then maxP = 0b else maxP = 1b

  if n_elements(track) eq 0 then s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalZNum = totalZNum $
  else s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZNum

   image = image * 0 ; 
   case maxP of
   0b: image = ((selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = 0))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] / (1.*totalZNum))
   1b: image = (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = 0))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
   endcase
   for i = 1, totalZNum - 1 do begin
      case maxP of
      0b: image += ( (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] / (1.*totalZNum))
      1b: image >= (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
      endcase
   endfor
   
   return, image
end


function C_sImageFilter_ImageZProjection::init
   ImageFilterStruct = {Name:'C_ImageZProjection',$    ;  Filter Name.
               pWidgetType:ptr_new(),$    ; Pointer on Filter Parameter Names.
               pNames:ptr_new(),$     ; Pointer on Filter Parameter Names.
               pActive:ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
               pMin:ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
               pMax:ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
               pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

     ; Parameters of C_ImageColocalization.
   filterParamWidgetType = make_array(2, /string, value = 'widget_slider')
   filterParamNames = ['Max Projection', 'Mean Projection']
   filterParamActive = [1, 0]
   filterParamMin = [0, 0]
   filterParamMax = [1, 1]
   filterParamValues = [1, 0]

   ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageZProjection__define
   tmp = {C_sImageFilter_ImageZProjection, pParamStruct:ptr_new(), inherits C_sImageFilter}
end