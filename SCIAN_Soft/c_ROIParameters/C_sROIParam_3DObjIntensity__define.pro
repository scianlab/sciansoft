;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjIntensity
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sROIParam_3DObjIntensity' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

; PLEASE CHECK THIS PROC::::: AT THE END VARIABLE "i" WAS USED... BUT THIS VALUE IS 2??????
; FASL_ TODO
pro C_sROIParam_3DObjIntensity::apply, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position

	if ptr_valid((*self.pParamStruct).pROINumberVect) then ptr_free, (*self.pParamStruct).pROINumberVect

	whParam = [(where( *(*self.pParamStruct).pNames eq '3D Volume [Voxel]'))[0] ,$
				  (where( *(*self.pParamStruct).pNames eq '3D Volume [x³]'))[0] ]

	whParamActive = whParam * 0
	if (n_elements(position) gt 0) then begin
		if (position[0] ne -1) then for i = 0, n_elements(position)-1 do whParamActive[position[i]] = 1
	endif else for i = 0, n_elements(whParam)-1 do whParamActive[i] = (*(*self.pParamStruct).pActive)[whParam[i]]

	nObjects = C_sROI3DGroupObj->count()
	if ((nObjects lt 1) or (total(whParamActive) eq 0)) then begin
		(*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
		for i = 0, 1 do if (whParamActive[i]) then (*(*self.pValueStruct)[whParam[i]]).pROIParamVect = ptr_new( -1, /no_copy)
	endif else begin

			; set Object Number Vector
		(*self.pParamStruct).pROINumberVect = ptr_new( (C_sROI3DGroupObj->getObjectNumberVector()) , /no_copy)

		if (whParamActive[1]) then begin
			xSizePerPixel = *(*self.pParamStruct).pValues[where(*(*self.pParamStruct).pNames eq 'xSize [µm]')] * 1. / $
								 *(*self.pParamStruct).pValues[where(*(*self.pParamStruct).pNames eq 'xSize [Pixel]')]
			ySizePerPixel = *(*self.pParamStruct).pValues[where(*(*self.pParamStruct).pNames eq 'ySize [µm]')] * 1. / $
								 *(*self.pParamStruct).pValues[where(*(*self.pParamStruct).pNames eq 'ySize [Pixel]')]
			zSizePerPixel = *(*self.pParamStruct).pValues[where(*(*self.pParamStruct).pNames eq 'zSize [µm]')] * 1. / $
								 *(*self.pParamStruct).pValues[where(*(*self.pParamStruct).pNames eq 'zSize [Pixel]')]
		endif

			; Object-Loop
		nObjects = n_elements(*(*self.pParamStruct).pROINumberVect)
		paramVect = fltArr(nObjects)
		for objNumber = 0, nObjects-1 do $
			paramVect[objNumber] = n_elements( *((C_sROI3DGroupObj->get(position = objNumber))->getpWherePoints()) )

			; 3D Volume [Voxel]
		if (whParamActive[0]) then $
			(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = ptr_new(paramVect)

			; 3D Volume [x³]
		if (whParamActive[0]) then $
			(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = ptr_new((paramVect*xSizePerPixel*ySizePerPixel*zSizePerPixel), /no_copy)
	endelse
end


pro C_sROIParam_3DObjIntensity::applyThresholdFilter, position = position

	if (position eq -1) then pos = where( *(*self.pParamStruct).pActive eq 1) else pos = position
	whParamActive = where( *(*self.pParamStruct).pActive eq 1)

	if (total(pos) gt -1) then for i = 0, n_elements(pos)-1 do begin
		flagVector = bytArr(n_elements(*(*(*self.pValueStruct)[pos[i]]).pROIParamVect))
		if (n_elements(flagVector) le 0) then return

		if ( (((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]]) + $
				((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]]) + $
				((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]]) + $
				((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]])) le 0 ) then begin
			flagVector[*] = 1
		endif else begin
			if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]]) then begin
		   		thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]] ) < $
		   							((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1b'))[0]] )
		   		thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]] ) > $
		   							((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1b'))[0]] )
		   		flagVector = temporary(flagVector) > ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
			endif
			if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]]) then begin
		   		thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]] ) < $
		   							((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2b'))[0]] )
		   		thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]] ) > $
		   							((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2b'))[0]] )
		   		flagVector = temporary(flagVector) > ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
			endif
			if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]]) then begin
		   		thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]] ) < $
		   							((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3b'))[0]] )
		   		thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]] ) > $
		   							((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3b'))[0]] )
		   		flagVector = temporary(flagVector) > ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
			endif
			if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]]) then begin
		   		thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]] ) < $
		   							((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4b'))[0]] )
		   		thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]] ) > $
		   							((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4b'))[0]] )
		   		flagVector = temporary(flagVector) > ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
			endif
		endelse
		if (total(flagVector) gt 0) then begin
			whereFlagVector = where(flagVector)
			*(*self.pParamStruct).pROINumberVect = (*(*self.pParamStruct).pROINumberVect)[whereFlagVector]
			for j = 0, n_elements(whParamActive)-1 do $
				*(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect =  (*(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect)[whereFlagVector]
		endif else begin
			if ptr_valid((*self.pParamStruct).pROINumberVect) then ptr_free, (*self.pParamStruct).pROINumberVect
			(*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
			for j = 0, n_elements(whParamActive)-1 do begin
				if ptr_valid((*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect
				(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect = ptr_new( -1, /no_copy)
			endfor
		endelse
	endfor
end


pro C_sROIParam_3DObjIntensity::updateParamValueStruct, objectNumberVector = objectNumberVector, position = position, all = all
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
		if (keyWord_set(position)) then begin
			if ptr_valid((*(*self.pValueStruct)[position]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[position]).pROIParamVect
			(*(*self.pValueStruct)[position]).pROIParamVect = ptr_new( -1, /no_copy)
		endif
		if (keyWord_set(all)) then $
			for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do begin
				if ptr_valid((*(*self.pValueStruct)[i]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[i]).pROIParamVect
				(*(*self.pValueStruct)[i]).pROIParamVect = ptr_new( -1, /no_copy)
			endfor
	endelse
end


pro C_sROIParam_3DObjIntensity::setpValueStruct, pValueStruct
	whereValueStructName = (where( *(*self.pParamStruct).pNames eq *pValueStruct.name))[0]
	if ptr_valid((*self.pValueStruct)[position]) then ptr_free, (*self.pValueStruct)[position]
    (*self.pValueStruct)[position] = ptr_new(*pValueStruct, /no_copy)
end
function C_sROIParam_3DObjIntensity::getpValueStruct, position = position
	if (ptr_valid((*self.pValueStruct)[position])) then return, (*self.pValueStruct)[position]  else return, -1
end
pro C_sROIParam_3DObjIntensity::setpParamStruct, pParamStruct
	if ptr_valid(self.pParamStruct) then ptr_free, self.pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
function C_sROIParam_3DObjIntensity::getpParamStruct
   return, self.pParamStruct
end

pro C_sROIParam_3DObjIntensity::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
	    case size((*self.pParamStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pParamStruct).(i)
	        'OBJREF': obj_destroy, (*self.pParamStruct).(i)
	        else:
	 	 endcase
	 endfor
	for j = 0, n_elements(*self.pValueStruct)-1 do begin
		for i = 0, n_tags(((*self.pValueStruct)[j]))-1 do begin
			case size(((*self.pValueStruct)[j]).(i), /tname) of
		        'POINTER': ptr_free, ((*self.pValueStruct)[j]).(i)
		        'OBJREF': obj_destroy, ((*self.pValueStruct)[j]).(i)
		        else:
		 	endcase
		 endfor
		 ptr_free, (*self.pValueStruct)[j]
	endfor
	ptr_free, self.pValueStruct
	ptr_free, self.pParamStruct
end


function C_sROIParam_3DObjIntensity::init

	ROIParamStruct = {name:'3D Object Intensity',$		;  ROI Name.
								type: 	'3D ROI-Parameter-Method',$
								pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROINumberVect: ptr_new()  	}		; Pointer on ROI-Obj Number Vector

	self.pValueStruct = ptr_new(ptrArr(3))
   	ROIParamWidgetType = ['widget_slider',$
   										'widget_slider',$
   										'widget_slider']
	ROIParamNames = [ '3D Intensity',$
	 							'3D Intensity [Voxel]',$
								'3D Intensity [x³]' ]
	ROIParamActive = [1,0,0]
    ROIParamMin = [0,0,0]
	ROIParamMax = [0,0,0]
	ROIParamValues = [0,0,0]
    pROINumberVect = [-1]

	ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
	ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
	ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
	ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
	ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
	ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
	ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

	ROIValueStruct = {name: '3D Volume [Voxel]',$
								type: '3D ROI-Parameter-Method',$
								pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect: ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

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
	ROIValueMin = 	[0.,0.,$
									0.,0.,$
									0.,0.,$
									0.,0. ]
	ROIValueMax =	[1.,1.,$
									1.,1.,$
									1.,1.,$
									1.,1. ]
	ROIValueValues =[0.,1.,$
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


	ROIValueStruct = {name: '3D Volume [x³]',$
								type: '3D ROI-Parameter-Method',$
								pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
								pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROIParamVect: ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = ['widget_slider', 'widget_slider', 'widget_slider', ROIValueWidgetType]
	ROIValueNames = ['x-Size per Pixel', 'y-Size per Pixel',  'z-Size per Pixel',ROIValueNames]
	ROIValueActive = [1, 1, 1, ROIValueActive]
	ROIValueMin = 	[0., 0., 0., ROIValueMin]
	ROIValueMax =	[1000., 1000., 1000., ROIValueMax]
	ROIValueValues =[1., 1., 1., ROIValueValues]

    pROIParamVect = [-1]
	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
	ROIValueStruct.pNames = ptr_new(ROIValueNames)
	ROIValueStruct.pActive = ptr_new(ROIValueActive)
	ROIValueStruct.pMin = ptr_new(ROIValueMin)
	ROIValueStruct.pMax = ptr_new(ROIValueMax)
	ROIValueStruct.pValues = ptr_new(ROIValueValues)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

	(*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end

pro C_sROIParam_3DObjIntensity__define
   tmp = {C_sROIParam_3DObjIntensity, pParamStruct: ptr_new(),$
      											 pValueStruct: ptr_new()}
end