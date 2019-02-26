;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_HistEqualizeAdapt
;
; PURPOSE:
;       - Image-Filter-Class.
;
; AUTHOR:
;   Dr. Steffen Härtel (2001)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;        result = obj_new('C_sImageFilter_HistEqualizeAdapt' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_HistEqualizeAdapt::getImageFilterType
   return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_HistEqualizeAdapt::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   type = size(image, /type)
   case type of
   1: mVal = 255
   else: mVal = 4096
   endcase

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Clip'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then clipVal = (*(*self.pParamStruct).pValues)[whParam] > 0 < mVal else clipVal = 0

   whParam = (where((*(*self.pParamStruct).pNames) eq 'N_Regions'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then nRegions = (((*(*self.pParamStruct).pValues)[whParam] > 1) < max(size(image, /dim))) else nRegions = 12

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Top_Value'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then topVal = ((*(*self.pParamStruct).pValues)[whParam] > 1) else topVal = mVal

   return, adapt_hist_equal(image, clip = clipVal, nRegions = nRegions, top = topVal)
end


function C_sImageFilter_HistEqualizeAdapt::init
   ImageFilterStruct = {Name:'C_HistEqualizeAdapt',$   ;  Filter Name.
               pWidgetType:ptr_new(),$ ; Pointer on Filter Parameter Names.
               pNames:ptr_new(),$  ; Pointer on Filter Parameter Names.
               pActive:ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
               pMin:ptr_new(),$   ; Pointer on Filter Parameter Min_Values.
               pMax:ptr_new(),$   ; Pointer on Filter Parameter Max_Values.
               pValues:ptr_new()} ; Pointer on Filter Parameter Values.

     ; Parameters of C_HistEqualizeAdapt.
   filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
   filterParamNames = ['Clip', 'N_Regions', 'Top_Value']
   filterParamActive = [0,0,1]
   filterParamMin = [1,0,0]
   filterParamMax = [255,255,255]
   filterParamValues = [0,12,255]

   ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_HistEqualizeAdapt__define
   tmp = {C_sImageFilter_HistEqualizeAdapt, pParamStruct: ptr_new(), inherits C_sImageFilter}
end