;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ScaleIntensity
;
; PURPOSE:
;       - ByteScale-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2011)
;     e_mail:shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ScaleIntensity' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ScaleIntensity::getImageFilterType
    return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_ScaleIntensity::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image
    
   minOld = min(image, max = maxOld)
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Min_Old'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then minOld = (*(*self.pParamStruct).pValues)[whParam]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Max_Old'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then maxOld = (*(*self.pParamStruct).pValues)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Max_Min_Symmetry'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then begin
      maxVal = abs(minOld) > abs(maxOld)
      maxOld = maxVal
      minOld = - maxVal
   endif

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Min_Old'))[0]
   (*(*self.pParamStruct).pValues)[whParam] = minOld
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Max_Old'))[0]
   (*(*self.pParamStruct).pValues)[whParam] = maxOld
    
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Min_New'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then minNew = (*(*self.pParamStruct).pValues)[whParam] else minNew = -1.
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Max_New'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then maxNew = (*(*self.pParamStruct).pValues)[whParam] else maxNew = 1.
    
   difOld = (maxOld - minOld) > 0.0001
   difNew = maxNew - minNew
	
   image -= minOld
	image *= (difNew/difOld)
   image += minNew

   return, image
end


function C_sImageFilter_ScaleIntensity::init

    filterStruct = {Name:'C_ScaleIntensity',$     ;  Filter Name.
                   pWidgetType:ptr_new(),$       ; Pointer on Filter Parameter Names.
                   pNames:ptr_new(),$     ; Pointer on Filter Parameter Names.
                   pImagefilterParamType:ptr_new(),$    ; Pointer on Filter Parameter Type (Bool, Byte, Int, Long, Float, Double).
                   pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                   pMin:ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                   pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                   pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of sMeanImage.
    filterParamWidgetType = make_array(5, /string, value = 'widget_slider')
    filterParamNames = ['Min_Old','Max_Old','Max_Min_Symmetry','Min_New','Max_New']
    filterParamType = ['Float', 'Float', 'Float', 'Float', 'Float']
    filterParamActive = [0,0,1,1,1]
    filterParamMin = [-10000.,0.,0.,-10000.,0.]
    filterParamMax = [0.,10000.,1.,0.,10000.]
    filterParamValues = [0.,255.,1.,-1.,1.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pImagefilterParamType = ptr_new(filterParamType, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ScaleIntensity__define
   tmp = {C_sImageFilter_ScaleIntensity, pParamStruct:ptr_new(), inherits C_sImageFilter}
end