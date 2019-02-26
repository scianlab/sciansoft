;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_WaterShed_ST
;
; PURPOSE:
;       - WaterShed_ST-Filter-Class. See WaterShed_ST.pro
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sImageFilter_WaterShed_ST' )
;
; METHOHDS:
;	function	->apply, pImageData = pImageData							   ;pImageData 			   Pointer on Image Data Matrix
;	pro					->set, pParamStruct = pParamStruct			;pParamStruct Pointer on ImageFilterStruct-Data
;	pro					->get, pParamStruct = pParamStruct			;pParamStruct Pointer on ImageFilterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_WaterShed_ST::getImageFilterType
	return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_WaterShed_ST::apply, image = image
	if (size(image, /n_dim) ne 2) then return, image

	whParam = (where((*(*self.pParamStruct).pNames) eq 'InverseIntensities_OnOff'))[0]
	if (((*(*self.pParamStruct).pActive)[whParam]) and ((*(*self.pParamStruct).pValues)[whParam])) then $
		newImage = (-1*image) + (max(image) + min(image)) else newImage = image

	whParam = (where((*(*self.pParamStruct).pNames) eq 'Connectivity_4_or_8'))[0]
;	if (round((*(*self.pParamStruct).pValues)[whParam]) eq 4) then newImage = WaterShed_ST(newImage, connectivity = 4) $
;		else newImage =  WaterShed_ST(newImage, connectivity = 8)

	whParam = (where((*(*self.pParamStruct).pNames) eq 'SetBordersToZero_OnOff'))[0]
	if (((*(*self.pParamStruct).pActive)[whParam]) and ((*(*self.pParamStruct).pValues)[whParam])) then begin
		sizeImage = size(image, /dim)
		newImage[ [0, sizeImage[0]-1], *] = 1
		newImage[ *, [0, sizeImage[1]-1]] = 1
		whereZero = where(newImage eq 0)
		if (whereZero[0] ne -1) then image[whereZero] = 0
	endif else image = newImage

	return, image
end

pro C_sImageFilter_WaterShed_ST::set, pParamStruct = pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
pro C_sImageFilter_WaterShed_ST::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end
function C_sImageFilter_WaterShed_ST::getpParamStruct
   return, self.pParamStruct
end

pro C_sImageFilter_WaterShed_ST::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
	    case size((*self.pParamStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pParamStruct).(i)
	        'OBJREF': obj_destroy, (*self.pParamStruct).(i)
	        else:
	 	 endcase
	 endfor
	ptr_free, self.pParamStruct
end

function C_sImageFilter_WaterShed_ST::init

	ImageFilterStruct = {Name:'C_WaterShed_ST',$		;  Filter Name.
								pWidgetType:ptr_new(),$		; Pointer on Filter Parameter Names.
								pNames:ptr_new(),$		; Pointer on Filter Parameter Names.
								pActive:ptr_new(),$		 ; Pointer on Filter Parameter Active Bool.
								pMin:ptr_new(),$	   ; Pointer on Filter Parameter Min_Values.
								pMax:ptr_new(),$		  ; Pointer on Filter Parameter Max_Values.
								pValues:ptr_new()}		  ; Pointer on Filter Parameter Values.

		; Parameters of WaterShed_ST.
	FilterParamWidgetType = make_array(3, /string, value = 'widget_slider')
	FilterParamNames = ['Connectivity_4_or_8', 'InverseIntensities_OnOff','SetBordersToZero_OnOff']
	FilterParamActive = [1,1,1]
	FilterParamMin = [4,0,0]
	FilterParamMax = [8,1,1]
	FilterParamValues =	[4,1,1]

	ImageFilterStruct.pWidgetType = ptr_new(FilterParamWidgetType, /no_copy)
	ImageFilterStruct.pNames = ptr_new(FilterParamNames, /no_copy)
	ImageFilterStruct.pActive = ptr_new(FilterParamActive, /no_copy)
	ImageFilterStruct.pMin = ptr_new(FilterParamMin, /no_copy)
	ImageFilterStruct.pMax = ptr_new(FilterParamMax, /no_copy)
	ImageFilterStruct.pValues = ptr_new(FilterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_WaterShed_ST__define
   tmp = {C_sImageFilter_WaterShed_ST, pParamStruct: ptr_new()}
end