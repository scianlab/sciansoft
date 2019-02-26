;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_MorphTophat
;
; PURPOSE:
;       - MorphTophat-Filter-Class. See MorphTophat.pro
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sImageFilter_MorphTophat' )
;
; METHOHDS:
;	function	->apply, pImageData = pImageData							   ;pImageData 			   Pointer on Image Data Matrix
;	pro					->set, pParamStruct = pParamStruct			;pParamStruct Pointer on ImageFilterStruct-Data
;	pro					->get, pParamStruct = pParamStruct			;pParamStruct Pointer on ImageFilterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_MorphTophat::getImageFilterType
	return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_MorphTophat::apply, image = image
	if (size(image, /n_dim) ne 2) then return, image
  
	neighbor = ((*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Neighbor'))[0]] > 0) < 100 ; era 4? por que?
   if(neighbor eq 0) then neighbor = 1
   
   r = neighbor
   ; Create a binary disc of given radius. 
   disc = SHIFT(DIST(2*r+1), r, r) LE r 
   neighbor = disc
   
	whParam = (where((*(*self.pParamStruct).pNames) eq 'Backgroud_OnOff'))[0]
	   ;if (r ne 100) then begin
      if (((*(*self.pParamStruct).pActive)[whParam]) and ((*(*self.pParamStruct).pValues)[whParam])) then return, Morph_Tophat(image, neighbor, /back, /no_copy) $
	   else return, morph_Tophat(image, neighbor, /no_copy)
	   
         ;endif else begin
   ;		if (((*(*self.pParamStruct).pActive)[whParam]) and ((*(*self.pParamStruct).pValues)[whParam])) then $
   ;			return, ((morph_Tophat(image, neighbor_sampling = 0, /back) + $
   ;						morph_Tophat(image, neighbor_sampling = 1, /back) + $
   ;						morph_Tophat(image, neighbor_sampling = 2, /back) + $
   ;						morph_Tophat(image, neighbor_sampling = 3, /back)) * .25) else $
   ;			return, ((morph_Tophat(image, neighbor_sampling = 0) + $
   ;						morph_Tophat(image, neighbor_sampling = 1) + $
   ;						morph_Tophat(image, neighbor_sampling = 2) + $
   ;						morph_Tophat(image, neighbor_sampling = 3)) * .25)
   ;	endelse
end


pro C_sImageFilter_MorphTophat::set, pParamStruct = pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
pro C_sImageFilter_MorphTophat::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end
function C_sImageFilter_MorphTophat::getpParamStruct
   return, self.pParamStruct
end

pro C_sImageFilter_MorphTophat::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
	    case size((*self.pParamStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pParamStruct).(i)
	        'OBJREF': obj_destroy, (*self.pParamStruct).(i)
	        else:
	 	 endcase
	 endfor
	ptr_free, self.pParamStruct
end

function C_sImageFilter_MorphTophat::init

	ImageFilterStruct = {  Name:'C_MorphTophat',$		;  Filter Name.
										pWidgetType:ptr_new(),$		; Pointer on Filter Parameter Names.
										pNames:ptr_new(),$		; Pointer on Filter Parameter Names.
										pActive:ptr_new(),$		 ; Pointer on Filter Parameter Active Bool.
										pMin:ptr_new(),$	   ; Pointer on Filter Parameter Min_Values.
										pMax:ptr_new(),$		  ; Pointer on Filter Parameter Max_Values.
										pValues:ptr_new()  $		  ; Pointer on Filter Parameter Values.
										}

		; Parameters of MorphTophat.
	FilterParamWidgetType = ['widget_slider','widget_slider']
	FilterParamNames = ['Neighbor', 'Backgroud_OnOff']
	FilterParamActive = [1,1]
	FilterParamMin = [0,0]
	FilterParamMax = [100,1]; eroe 4,1
	FilterParamValues =	[0,1]

	ImageFilterStruct.pWidgetType = ptr_new(FilterParamWidgetType, /no_copy)
	ImageFilterStruct.pNames = ptr_new(FilterParamNames, /no_copy)
	ImageFilterStruct.pActive = ptr_new(FilterParamActive, /no_copy)
	ImageFilterStruct.pMin = ptr_new(FilterParamMin, /no_copy)
	ImageFilterStruct.pMax = ptr_new(FilterParamMax, /no_copy)
	ImageFilterStruct.pValues = ptr_new(FilterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_MorphTophat__define
   tmp = {C_sImageFilter_MorphTophat, pParamStruct: ptr_new(), inherits C_sImageFilter}
end