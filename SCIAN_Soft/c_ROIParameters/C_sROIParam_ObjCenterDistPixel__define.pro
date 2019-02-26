;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjCenterDistPixel
;
; PURPOSE:
;       - Calculation of Object Size in real Dimensions
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sROIParam_ObjCenterDistPixel' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sROIParam_ObjCenterDistPixel::getROIParamType
	return, 'Group ROI-Parameter-Method'
end

pro C_sROIParam_ObjCenterDistPixel::apply, centerCoordinates = centerCoordinates

	if ptr_valid((*self.pValueStruct).pROINumberVect) then ptr_free, (*self.pValueStruct).pROINumberVect
	if ptr_valid((*self.pValueStruct).pROIParamVect) then ptr_free, (*self.pValueStruct).pROIParamVect
	if ptr_valid((*self.pValueStruct).pROICoordinateVect) then ptr_free, (*self.pValueStruct).pROICoordinateVect

	dimCenCoo = size(centerCoordinates, /dim)
	if (dimCenCoo[1] le 1) then begin
		(*self.pValueStruct).pROINumberVect = ptr_new( -1, /no_copy)
		(*self.pValueStruct).pROIParamVect = ptr_new( [-1,-1], /no_copy)
		(*self.pValueStruct).pROICoordinateVect = ptr_new( [-1,-1], /no_copy)
	endif

		; Define Distance Matrix: [i,j] defines distance between Object i and j (- 1. is default)
	distanceMatrix = fltArr(dimCenCoo[1] , dimCenCoo[1]) - 1.

		; Calculate Center Distance for each object i
	for i = 0, dimCenCoo[1]-2 do $
			distanceMatrix[i, i+1:*] = sqrt( (centerCoordinates[1, i+1 : *] - centerCoordinates[1,i])^2 + $
														 (centerCoordinates[2, i+1 : *] - centerCoordinates[2,i])^2		)

	distanceMatrix = (distanceMatrix) + transpose(distanceMatrix)

	a = fltArr(dimCenCoo[1]) + centerCoordinates[0,*]
	(*self.pValueStruct).pROINumberVect = ptr_new(a, /no_copy)
	(*self.pValueStruct).pROIParamVect = ptr_new( distanceMatrix, /no_copy)
	a = centerCoordinates[1:2,*]
	(*self.pValueStruct).pROICoordinateVect = ptr_new(a, /no_copy)
end

pro C_sROIParam_ObjCenterDistPixel::updateParamValueStruct, objectNumberVector = objectNumberVector
end

pro C_sROIParam_ObjCenterDistPixel::setpValueStruct, pValueStruct
	if ptr_valid(self.pValueStruct) then ptr_free, self.pValueStruct
    self.pValueStruct = ptr_new(*pValueStruct, /no_copy)
end
function C_sROIParam_ObjCenterDistPixel::getpValueStruct
   return, self.pValueStruct
end
pro C_sROIParam_ObjCenterDistPixel::setpParamStruct, pParamStruct
	if ptr_valid(self.pParamStruct) then ptr_free, self.pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
function C_sROIParam_ObjCenterDistPixel::getpParamStruct
   return, self.pParamStruct
end

pro C_sROIParam_ObjCenterDistPixel::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
	    case size((*self.pParamStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pParamStruct).(i)
	        'OBJREF': obj_destroy, (*self.pParamStruct).(i)
	        else:
	 	 endcase
	 endfor
	for i = 0,n_tags((*self.pValueStruct))-1 do begin
	    case size((*self.pValueStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pValueStruct).(i)
	        'OBJREF': obj_destroy, (*self.pValueStruct).(i)
	        else:
	 	 endcase
	 endfor
	ptr_free, self.pValueStruct
	ptr_free, self.pParamStruct
end

function C_sROIParam_ObjCenterDistPixel::init

	ROIParamStruct = {  Name:'Object-Center Distances [Pixel]',$		;  ROI Name.
									pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
									pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
									pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
									pValues:ptr_new()  $		  ; Pointer on ROI-Obj Parameter Values.
								}

   	ROIParamWidgetType = ['widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider',$
									'widget_slider', 'widget_slider']
	ROIParamNames = ['Number of Nearest Neigbours',$
									'Threshold_1a', 'Threshold_1b',$
									'Threshold_2a', 'Threshold_2b',$
									'Threshold_3a', 'Threshold_3b',$
									'Threshold_4a', 'Threshold_4b']
	ROIParamActive = [1,$
									0,0,$
									0,0,$
									0,0,$
									0,0 ]
	ROIParamMin = 	[0.,$
									0.,0.,$
									0.,0.,$
									0.,0.,$
									0.,0. ]
	ROIParamMax =	[50.,$
									1.,1.,$
									1.,1.,$
									1.,1.,$
									1.,1. ]
	ROIParamValues =[6.,$
									0.,1.,$
									0.,1.,$
									0.,1.,$
									0.,1. ]

	ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
	ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
	ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
	ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
	ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
	ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

	ROIValueStruct = {name: 'Object-Center Distances [Pixel]',$
								type: 'Group ROI-Parameter-Method',$
								pROINumberVect: ptr_new(),$		; Pointer on ROI-Obj Number Vector
								pROIParamVect: ptr_new(),$		; Pointer on ROI-Obj Number Vector
								pROICoordinateVect: ptr_new()  }		; Pointer on ROI-Obj Parameter Vector.

    pROINumberVect = [-1]
    pROIParamVect = [-1]
    pROICoordinateVect = [-1]
	ROIValueStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
	ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
	ROIValueStruct.pROICoordinateVect = ptr_new(pROICoordinateVect, /no_copy)

    self.pValueStruct = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_ObjCenterDistPixel__define
   tmp = {C_sROIParam_ObjCenterDistPixel,  pParamStruct: ptr_new(),$
      																 pValueStruct: ptr_new()}
end



