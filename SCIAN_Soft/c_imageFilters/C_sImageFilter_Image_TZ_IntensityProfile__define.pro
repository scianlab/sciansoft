;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Image_TChZ_Projection
;
; PURPOSE:
;       - Chech Photobleaching etc...
;
; AUTHOR:
;   Dr. Steffen Härtel (2011)
;   e_mail:shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;   result = obj_new('C_sImageFilter_Image_TChZ_Projection')
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_Image_TZ_IntensityProfile::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_Image_TZ_IntensityProfile::apply, image = image,$
                        selectedStackObject = selectedStackObject,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos,$
                        chPos = chPos,$
                        zPos = zPos,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y

   whPos = (where((*(*self.pParamStruct).pNames) eq 'Plot Window'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then fPlot = 1b else fPlot = 0b

   fProf = 0
   whPos = (where((*(*self.pParamStruct).pNames) eq 'T Profile'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then fTProf = 1b else fTProf = 0b
   whPos = (where((*(*self.pParamStruct).pNames) eq 'Z Profile'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then fZProf = 1b else fZProf = 0b

   dimI = size(image, /dim)
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum

   case 1 of
   fTProf and fZProf: begin
      intProf = make_array(totalTNum, /float)
      for i = 0, totalTNum - 1 do for j = 0, totalZNum - 1 do $
         intProf[i] += (moment(((selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = j))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]])))[0]            
      intProf /= totalZNum
      if fPlot then begin
         iPlot, intProf, title = 'Intensity Profile [z, t]'
         intDif = shift(intProf, -1) - intProf
         intDif[totalTNum-1] = 0
         iPlot, intDif, title = 'Delta Intensity [z, t]'
         iPlot, intDif / (moment(intProf))[0] * 100., title = 'Delta Intensity % [z, t]'
      endif
   endcase
   fTProf: begin
      intProf = make_array(totalTNum, /float)
      for i = 0, totalTNum - 1 do $
         intProf[i] = (moment(((selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]])))[0]            
      if fPlot then begin
         iPlot, intProf, title = 'Intensity Profile [t]'
         intDif = shift(intProf, -1) - intProf
         intDif[totalTNum-1] = 0
         iPlot, intDif, title = 'Delta Intensity [t]'
         iPlot, intDif / (moment(intProf))[0] * 100., title = 'Delta Intensity % [t]'
      endif
   endcase
   fZProf: begin
      intProf = make_array(totalZNum, /float)
      for i = 0, totalZNum - 1 do $
         intProf[i] = (moment(((selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]])))[0]            
      if fPlot then begin
         iPlot, intProf, title = 'Intensity Profile [z]'
         intDif = shift(intProf, -1) - intProf
         intDif[totalZNum-1] = 0
         iPlot, intDif, title = 'Delta Intensity [z]'
         iPlot, intDif / (moment(intProf))[0] * 100., title = 'Delta Intensity % [z]'
      endif
   endcase
   else:
   endcase
   
   return, image
end


function C_sImageFilter_Image_TZ_IntensityProfile::init
   ImageFilterStruct = {Name:'C_Image_TZ_IntensityProfile',$    ;  Filter Name.
                        pWidgetType:ptr_new(),$  ; Pointer on Filter Parameter Names.
                        pNames:ptr_new(),$    ; Pointer on Filter Parameter Names.
                        pActive:ptr_new(),$   ; Pointer on Filter Parameter Active Bool.
                        pMin:ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                        pMax:ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                        pValues:ptr_new()}    ; Pointer on Filter Parameter Values.

     ; Filer-Parameters
   filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
   filterParamNames = ['T Profile','Z Profile', 'Plot Window']
   filterParamActive = [1, 0, 0]
   filterParamMin = [0, 0, 0]
   filterParamMax = [1, 1, 1]
   filterParamValues = [1, 0, 0]

   ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
   return, 1
end


pro C_sImageFilter_Image_TZ_IntensityProfile__define
   tmp = {C_sImageFilter_Image_TZ_IntensityProfile, pParamStruct:ptr_new(), inherits C_sImageFilter}
end