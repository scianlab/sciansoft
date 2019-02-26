;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjClusterGrowth
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sROIParam_ObjClusterGrowth' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjClusterGrowth::apply, whereXY = whereXY, objNumber = objNumber, single = single, mask = mask, group = group, position = position, xySizePerPixel = xySizePerPixel

	if ptr_valid((*self.pParamStruct).pROINumberVect) then ptr_free, (*self.pParamStruct).pROINumberVect

	whParam = [  (where( *(*self.pParamStruct).pNames eq 'Number of Cells in Cluster [Pixel]'))[0] ,$
								(where( *(*self.pParamStruct).pNames eq 'Number of Cells in Cluster [µm²]'))[0] ]

	whParamActive = whParam * 0
	if (n_elements(position) gt 0) then begin
		if (position[0] ne -1) then for i = 0, n_elements(position)-1 do whParamActive[position[i]] = 1
	endif else for i = 0, n_elements(whParam)-1 do whParamActive[i] = (*(*self.pParamStruct).pActive)[whParam[i]]

	if (whParamActive[0]) then begin
		if ptr_valid((*(*self.pValueStruct)[whParam[0]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[0]]).pROIParamVect
		basicPixCellSize = 1. * (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Size of Basic Cell [Pixel]'))[0]]
		if (basicPixCellSize eq 0) then basicPixCellSize = 1.
	endif
	if (whParamActive[1]) then begin
		if ptr_valid((*(*self.pValueStruct)[whParam[1]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[1]]).pROIParamVect
		(*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
		(*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]
		basicRealCellSize = (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'Size of Basic Cell [µm²]'))[0]]
		factor = 1.*xySizePerPixel[0]*xySizePerPixel[1]
		if (basicRealCellSize eq 0) then basicRealCellSize = 1.
		if (factor eq 0) then factor = 1.
		basicRealCellSize = temporary(basicRealCellSize) / factor
	endif

	if keyword_set(single) then begin
		(*self.pParamStruct).pROINumberVect = ptr_new( objNumber, /no_copy)
		if (whParamActive[0]) then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( n_elements(whereXY)/basicPixCellSize, /no_copy)
		if (whParamActive[1]) then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( n_elements(whereXY)*(factor/basicRealCellSize), /no_copy)
	endif

	if keyword_set(group) then begin
		hmax = max(mask)
		if (hmax eq 0) then begin
			(*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
			if (whParamActive[0]) then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( -1, /no_copy)
			if (whParamActive[1]) then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( -1, /no_copy)
		endif else begin
			hmin = min(mask[where(mask ne 0)])
			hist = histogram(mask, min = hmin, max = hmax)
			whereHist = where(hist ne 0)
			(*self.pParamStruct).pROINumberVect = ptr_new( (whereHist + hmin))
			if (whParamActive[0]) then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( hist[whereHist]/basicPixCellSize, /no_copy)
			if (whParamActive[1]) then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( hist[whereHist]*(factor/basicRealCellSize), /no_copy)
		endelse
	endif
end


pro C_sROIParam_ObjClusterGrowth::applyThresholdFilter, position = position

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


pro C_sROIParam_ObjClusterGrowth::updateParamValueStruct, objectNumberVector = objectNumberVector, position = position, all = all
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


pro C_sROIParam_ObjClusterGrowth::setpValueStruct, pValueStruct
	whereValueStructName = (where( *(*self.pParamStruct).pNames eq *pValueStruct.name))[0]
	if ptr_valid((*self.pValueStruct)[position]) then ptr_free, (*self.pValueStruct)[position]
    (*self.pValueStruct)[position] = ptr_new(*pValueStruct, /no_copy)
end
function C_sROIParam_ObjClusterGrowth::getpValueStruct, position = position
	if (ptr_valid((*self.pValueStruct)[position])) then return, (*self.pValueStruct)[position]  else return, -1
end
pro C_sROIParam_ObjClusterGrowth::setpParamStruct, pParamStruct
	if ptr_valid(self.pParamStruct) then ptr_free, self.pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
function C_sROIParam_ObjClusterGrowth::getpParamStruct
   return, self.pParamStruct
end

pro C_sROIParam_ObjClusterGrowth::cleanup
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


function C_sROIParam_ObjClusterGrowth::init

	ROIParamStruct = {name:'Object Number in Cluster',$		;  ROI Name.
								type: 	'Single ROI-Parameter-Method',$
								pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
								pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
								pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
								pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
								pValues:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
								pROINumberVect: ptr_new()  	}		; Pointer on ROI-Obj Number Vector

	self.pValueStruct = ptr_new(ptrArr(2))
   	ROIParamWidgetType = make_array(2, /string, value = 'widget_slider')
	ROIParamNames = [ 'Number of Cells in Cluster [Pixel]', 'Number of Cells in Cluster [µm²]']
	ROIParamActive = [1,0]
    ROIParamMin = [0,0]
	ROIParamMax = [0,0]
	ROIParamValues = [0,0]
    pROINumberVect = [-1]

	ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
	ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
	ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
	ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
	ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
	ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
	ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

	ROIValueStruct = {name: 'Number of Cells in Cluster [Pixel]',$
									type: 'Single ROI-Parameter-Method',$
									pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
									pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
									pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
									pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
									pValues:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
									pROIParamVect: ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = make_array(9, /string, value = 'widget_slider')
	ROIValueNames = ['Size of Basic Cell [Pixel]',$
									'Threshold_1a', 'Threshold_1b',$
									'Threshold_2a', 'Threshold_2b',$
									'Threshold_3a', 'Threshold_3b',$
									'Threshold_4a', 'Threshold_4b']
	ROIValueActive = make_array(9, /byte, value = 0)
	ROIValueMin = 	make_array(9, /float, value = 0)
	ROIValueMax =	make_array(9, /float, value = 1)
	ROIValueValues =[ 1.,$
									0.,1.,$
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


	ROIValueStruct = {name: 'Number of Cells in Cluster [µ]',$
									type: 'Single ROI-Parameter-Method',$
									pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter WidgetType.
									pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
									pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
									pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
									pValues:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Values.
									pROIParamVect: ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

   	ROIValueWidgetType = make_array(11, /string, value = 'widget_slider')
	ROIValueNames = ['Size of Basic Cell [µm²]',$
									'x-Size per Pixel', 'y-Size per Pixel',$
									'Threshold_1a', 'Threshold_1b',$
									'Threshold_2a', 'Threshold_2b',$
									'Threshold_3a', 'Threshold_3b',$
									'Threshold_4a', 'Threshold_4b']
	ROIValueActive = make_array(11, /byte, value = 0)
	ROIValueMin = 	make_array(11, /float, value = 0)
	ROIValueMax =	make_array(11, /float, value = 1)
	ROIValueMax[0:3] = 1000
	ROIValueValues =[1., 1., 1.,$
									0.,1.,$
									0.,1.,$
									0.,1.,$
									0.,1. ]

    pROIParamVect = [-1]
	ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
	ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
	ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
	ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
	ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
	ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_ObjClusterGrowth__define
   tmp = {C_sROIParam_ObjClusterGrowth, pParamStruct: ptr_new(),$
      											 pValueStruct: ptr_new()}
end