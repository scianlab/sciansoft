; Archivo probablemente deprecado...
; fecha revision 2012/12
; si ve esto en unos meses y nadie a reclamado nada..
; borrar archivo
; el contenido de este archivo fue convertido para
;ser una herencia de ROIParam y se encuentra
; con funciones duplicadas en C_sROIParam_ObjTextureHaralick
; FASL... para valida renombro las funciones
; en caso de que algun error de compilacion mantuviese
; el uso de este archivo hasta ahora y rastrearlo

;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjTextureHaralick_DEPRECADO
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sROIParam_ObjTextureHaralick_DEPRECADO' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjTextureHaralick_DEPRECADO::apply, whereXY = whereXY, objNumber = objNumber, single = single, mask = mask, intensityMask = intensityMask, group = group, position = position, xySizePerPixel = xySizePerPixel

   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do  whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

	whParamActive = whParam * 0
	if (n_elements(position) gt 0) then begin
		if (position[0] ne -1) then for i = 0, n_elements(position)-1 do whParamActive[position[i]] = 1
	endif else for i = 0, n_elements(whParam)-1 do whParamActive[i] = (*(*self.pParamStruct).pActive)[whParam[i]]

   if ptr_valid((*self.pParamStruct).pROINumberVect) then ptr_free, (*self.pParamStruct).pROINumberVect

	if (whParamActive[0]) then $
		if ptr_valid((*(*self.pValueStruct)[whParam[0]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[0]]).pROIParamVect
	if (whParamActive[1]) then $
		if ptr_valid((*(*self.pValueStruct)[whParam[1]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[1]]).pROIParamVect
	if (whParamActive[2]) then begin
		if ptr_valid((*(*self.pValueStruct)[whParam[2]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[2]]).pROIParamVect
		(*(*(*self.pValueStruct)[whParam[2]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
		(*(*(*self.pValueStruct)[whParam[2]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]
		factor = 1.*xySizePerPixel[0]*xySizePerPixel[1]
	endif

	if keyword_set(single) then begin
		(*self.pParamStruct).pROINumberVect = ptr_new( objNumber, /no_copy)
		if (whParamActive[0]) then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( total(intensityMask[whereXY]), /no_copy)
		if (whParamActive[1]) then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( 1.*total(intensityMask[whereXY])/n_elements(whereXY) , /no_copy)
		if (whParamActive[1]) then (*(*self.pValueStruct)[whParam[2]]).pROIParamVect = ptr_new( 1.*total(intensityMask[whereXY])/(n_elements(whereXY)*factor), /no_copy)
	endif

	if keyword_set(group) then begin
		hmax = max(mask)
		if (hmax eq 0) then begin
			(*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
			if (whParamActive[0]) then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( -1, /no_copy)
			if (whParamActive[1]) then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( -1, /no_copy)
			if (whParamActive[2]) then (*(*self.pValueStruct)[whParam[2]]).pROIParamVect = ptr_new( -1, /no_copy)
		endif else begin
			hmin = min(mask[where(mask ne 0)])
			hist = histogram(mask, min = hmin, max = hmax, reverse_indices = revInd )
			histInt = hist * 0.
			whereHist = where(hist ne 0)
			(*self.pParamStruct).pROINumberVect = ptr_new( (whereHist + 1))
			for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(intensityMask[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
			if whParamActive[0] then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( histInt[whereHist], /no_copy)
			if whParamActive[1] then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( 1.*histInt[whereHist]/hist[whereHist], /no_copy)
			if whParamActive[2] then (*(*self.pValueStruct)[whParam[2]]).pROIParamVect = ptr_new( 1.*histInt[whereHist]/(hist[whereHist]*factor), /no_copy)
		endelse
	endif
end


pro C_sROIParam_ObjTextureHaralick_DEPRECADO::updateParamValueStruct, objectNumberVector = objectNumberVector, position = position, all = all
	n_objectNumberVector = n_elements(objectNumberVector)
	if (n_objectNumberVector le 0) then return

	flagVector = bytArr(n_elements(*(*self.pParamStruct).pROINumberVect))
	if not((n_objectNumberVector eq 1) and (objectNumberVector[0] eq -1)) then begin
		for i = 0, n_objectNumberVector-1 do begin
			whereNumberVector = (where(  (*(*self.pParamStruct).pROINumberVect) eq objectNumberVector[i]  ))[0]
			if (whereNumberVector ne -1) then flagVector[whereNumberVector] = 1b
		endfor
	endif

	if (total(flagVector) gt 0) then begin
		whereFlagVector = where(flagVector)
		*(*self.pParamStruct).pROINumberVect = (*(*self.pParamStruct).pROINumberVect)[whereFlagVector]
		if (keyWord_set(position)) then *(*(*self.pValueStruct)[position]).pROIParamVect =  (*(*(*self.pValueStruct)[position]).pROIParamVect)[whereFlagVector]
		if (keyWord_set(all)) then $
			for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do begin
				if ( (*(*self.pParamStruct).pActive)[i]  ) then $
					*(*(*self.pValueStruct)[i]).pROIParamVect =  (*(*(*self.pValueStruct)[i]).pROIParamVect)[whereFlagVector]
			endfor
	endif else begin
		if keyWord_set(position) then begin
			if ptr_valid((*(*self.pValueStruct)[position]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[position]).pROIParamVect
			(*(*self.pValueStruct)[position]).pROIParamVect = ptr_new( -1, /no_copy)
		endif
		if keyWord_set(all) then $
			for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do begin
				if ptr_valid((*(*self.pValueStruct)[i]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[i]).pROIParamVect
				(*(*self.pValueStruct)[i]).pROIParamVect = ptr_new( -1, /no_copy)
			endfor
	endelse
end


function C_sROIParam_ObjTextureHaralick_DEPRECADO::init

	ROIParamStruct = {name:'Haralicks Texture Features',$		;  ROI Name.
								type:	'Single ROI-Parameter-Method',$
								pWidgetType:	ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pNames:	ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:	 ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:	 ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:		 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:	 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROINumberVect:ptr_new()  	}		; Pointer on ROI-Obj Number Vector

	self.pValueStruct = ptr_new(ptrArr(6))
	ROIValueWidgetType = make_array(6, /string, value = 'widget_slider')

	ROIParamNames = ['Entropy', 'Contrast', 'Chi²', 'Kappa', 'Inverse Moment Difference', '2nd Angular Moment' ]

	ROIParamActive = [1,0,0,0,0,0]
   ROIParamMin = [0,0,0,0,0,0]
	ROIParamMax = [0,0,0,0,0,0]
	ROIParamValues = [0,0,0,0,0,0]
   pROINumberVect = [-1]

	ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
	ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
	ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
	ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
	ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
	ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
	ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

	ROIValueStruct = {name:'Entropy',$
								type:'Single ROI-Parameter-Method',$
								pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect:ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = ['widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider']
	ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
							'Threshold_2a', 'Threshold_2b',$
							'Threshold_3a', 'Threshold_3b',$
							'Threshold_4a', 'Threshold_4b']
	ROIValueActive = [0,0,$
									0,0,$
									0,0,$
									0,0 ]
	ROIValueMin = 	[0., 0.,$
									0.,0.,$
									0.,0.,$
									0.,0. ]
	ROIValueMax =	[1., 1.,$
									1.,1.,$
									1.,1.,$
									1.,1. ]
	ROIValueValues =[0., 1.,$
									0.,1.,$
									0.,1.,$
									0.,1. ]

    pROIParamVect = [-1]

	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
	ROIValueStruct.pNames = ptr_new(ROIValueNames)
	ROIValueStruct.pActive = ptr_new(ROIValueActive)
	ROIValueStruct.pMin = ptr_new(ROIValueMin)
	ROIValueStruct.pMax = ptr_new(ROIValueMax)
	ROIValueStruct.pValues = ptr_new(ROIValueValues)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

	ROIValueStruct = {name:'Contrast',$
								type:'Single ROI-Parameter-Method',$
								pWidgetType:	ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:	ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:	 ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:	 ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:		 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:	 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect:ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = ['widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider']
	ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
									'Threshold_2a', 'Threshold_2b',$
									'Threshold_3a', 'Threshold_3b',$
									'Threshold_4a', 'Threshold_4b']
	ROIValueActive = [0,0,$
									0,0,$
									0,0,$
									0,0 ]
	ROIValueMin = 	[0., 0.,$
									0.,0.,$
									0.,0.,$
									0.,0. ]
	ROIValueMax =	[1., 1.,$
									1.,1.,$
									1.,1.,$
									1.,1. ]
	ROIValueValues =[0., 1.,$
									0.,1.,$
									0.,1.,$
									0.,1. ]

    pROIParamVect = [-1]

	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
	ROIValueStruct.pNames = ptr_new(ROIValueNames)
	ROIValueStruct.pActive = ptr_new(ROIValueActive)
	ROIValueStruct.pMin = ptr_new(ROIValueMin)
	ROIValueStruct.pMax = ptr_new(ROIValueMax)
	ROIValueStruct.pValues = ptr_new(ROIValueValues)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

	ROIValueStruct = {name:'Chi²',$
								type:'Single ROI-Parameter-Method',$
								pWidgetType:	ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:	ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:	 ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:	 ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:		 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:	 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect:ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = ['widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider']
	ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
									'Threshold_2a', 'Threshold_2b',$
									'Threshold_3a', 'Threshold_3b',$
									'Threshold_4a', 'Threshold_4b']
	ROIValueActive = [0,0,$
									0,0,$
									0,0,$
									0,0 ]
	ROIValueMin = 	[0., 0.,$
									0.,0.,$
									0.,0.,$
									0.,0. ]
	ROIValueMax =	[1., 1.,$
									1.,1.,$
									1.,1.,$
									1.,1. ]
	ROIValueValues =[0., 1.,$
									0.,1.,$
									0.,1.,$
									0.,1. ]

    pROIParamVect = [-1]

	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
	ROIValueStruct.pNames = ptr_new(ROIValueNames)
	ROIValueStruct.pActive = ptr_new(ROIValueActive)
	ROIValueStruct.pMin = ptr_new(ROIValueMin)
	ROIValueStruct.pMax = ptr_new(ROIValueMax)
	ROIValueStruct.pValues = ptr_new(ROIValueValues)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)

	ROIValueStruct = {name:'Kappa',$
								type:'Single ROI-Parameter-Method',$
								pWidgetType:	ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:	ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:	 ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:	 ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:		 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:	 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect:ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = ['widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider']
	ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
									'Threshold_2a', 'Threshold_2b',$
									'Threshold_3a', 'Threshold_3b',$
									'Threshold_4a', 'Threshold_4b']
	ROIValueActive = [0,0,$
									0,0,$
									0,0,$
									0,0 ]
	ROIValueMin = 	[0., 0.,$
									0.,0.,$
									0.,0.,$
									0.,0. ]
	ROIValueMax =	[1., 1.,$
									1.,1.,$
									1.,1.,$
									1.,1. ]
	ROIValueValues =[0., 1.,$
									0.,1.,$
									0.,1.,$
									0.,1. ]

    pROIParamVect = [-1]

	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
	ROIValueStruct.pNames = ptr_new(ROIValueNames)
	ROIValueStruct.pActive = ptr_new(ROIValueActive)
	ROIValueStruct.pMin = ptr_new(ROIValueMin)
	ROIValueStruct.pMax = ptr_new(ROIValueMax)
	ROIValueStruct.pValues = ptr_new(ROIValueValues)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

	ROIValueStruct = {name:'Inverse Moment Difference',$
								type:'Single ROI-Parameter-Method',$
								pWidgetType:	ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:	ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:	 ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:	 ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:		 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:	 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect:ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = ['widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider']
	ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
									'Threshold_2a', 'Threshold_2b',$
									'Threshold_3a', 'Threshold_3b',$
									'Threshold_4a', 'Threshold_4b']
	ROIValueActive = [0,0,$
									0,0,$
									0,0,$
									0,0 ]
	ROIValueMin = 	[0., 0.,$
									0.,0.,$
									0.,0.,$
									0.,0. ]
	ROIValueMax =	[1., 1.,$
									1.,1.,$
									1.,1.,$
									1.,1. ]
	ROIValueValues =[0., 1.,$
									0.,1.,$
									0.,1.,$
									0.,1. ]

    pROIParamVect = [-1]

	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
	ROIValueStruct.pNames = ptr_new(ROIValueNames)
	ROIValueStruct.pActive = ptr_new(ROIValueActive)
	ROIValueStruct.pMin = ptr_new(ROIValueMin)
	ROIValueStruct.pMax = ptr_new(ROIValueMax)
	ROIValueStruct.pValues = ptr_new(ROIValueValues)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[4] = ptr_new(ROIValueStruct, /no_copy)

	ROIValueStruct = {name:'2nd Angular Moment',$
								type:'Single ROI-Parameter-Method',$
								pWidgetType:	ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:	ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:	 ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:	 ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:		 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:	 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect:ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = ['widget_slider', 'widget_slider', ROIValueWidgetType]
	ROIValueNames = ['x-Size per Pixel', 'y-Size per Pixel', ROIValueNames]
	ROIValueActive = [1, 1, ROIValueActive]
	ROIValueMin = 	[0., 0., ROIValueMin]
	ROIValueMax =	[1000., 1000., ROIValueMax]
	ROIValueValues =[1., 1., ROIValueValues]

    pROIParamVect = [-1]
	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
	ROIValueStruct.pNames = ptr_new(ROIValueNames)
	ROIValueStruct.pActive = ptr_new(ROIValueActive)
	ROIValueStruct.pMin = ptr_new(ROIValueMin)
	ROIValueStruct.pMax = ptr_new(ROIValueMax)
	ROIValueStruct.pValues = ptr_new(ROIValueValues)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

	(*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

	ROIValueStruct = {name:'Object I-Density [I/x²]',$
								type:'Single ROI-Parameter-Method',$
								pWidgetType:	ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:	ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:	 ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:	 ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:		 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:	 ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect:ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = ['widget_slider', 'widget_slider', ROIValueWidgetType]
	ROIValueNames = ['x-Size per Pixel', 'y-Size per Pixel', ROIValueNames]
	ROIValueActive = [1, 1, ROIValueActive]
	ROIValueMin = 	[0., 0., ROIValueMin]
	ROIValueMax =	[1000., 1000., ROIValueMax]
	ROIValueValues =[1., 1., ROIValueValues]

    pROIParamVect = [-1]
	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
	ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
	ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
	ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
	ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
	ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

	(*self.pValueStruct)[5] = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_ObjTextureHaralick_DEPRECADO__define
   tmp = {C_sROIParam_ObjTextureHaralick_DEPRECADO, pParamStruct:ptr_new(),$
      											 pValueStruct:ptr_new()}
end