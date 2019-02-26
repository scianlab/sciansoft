;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjCenterDistReal
;
; PURPOSE:
;       - Calculation of Object Size in real Dimensions
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sROIParam_ObjCenterDistReal' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sROIParam_ObjCenterDistReal::getROIParamType
	return, 'Group ROI-Parameter-Method'
end

function C_sROIParam_ObjCenterDistReal::apply, centerCoordinates = centerCoordinates, xySizePerPixel = xySizePerPixel
	dimCenCoo = size(centerCoordinates, /dim)
	if (dimCenCoo[1] le 1) then return, {name: 'Object-Center Distances [x]',$
													type: 'Group ROI-Parameter-Method',$
													objNumberVector: centerCoordinates[0,0],$
													distanceMatrix: -1.,$
													centerCoordinates: centerCoordinates[1:2, 0] }

		; Define Distance Matrix: [i,j] defines distance between Object i and j (- 1. is default)
	distanceMatrix = fltArr(dimCenCoo[1] , dimCenCoo[1]) -1.

		; Calculate Center Distance for each object i
	for i = 0, dimCenCoo[1]-2 do $
			distanceMatrix[i, i+1:*] = sqrt( ( xySizePerPixel[0] * (centerCoordinates[1, i+1 : *] - centerCoordinates[1,i]))^2 + $
														 (xySizePerPixel[1] * (centerCoordinates[2, i+1 : *] - centerCoordinates[2,i]))^2		)

	distanceMatrix = (distanceMatrix) + transpose(distanceMatrix)

	return, {name: 'Object-Center Distances [x]',$
				type: 'Group ROI-Parameter-Method',$
				objNumberVector : centerCoordinates[0,*],$
				distanceMatrix: distanceMatrix,$
				centerCoordinates: round(centerCoordinates[1:2, *])}
end

pro C_sROIParam_ObjCenterDistReal::set, pParamStruct = pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end

pro C_sROIParam_ObjCenterDistReal::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end

pro C_sROIParam_ObjCenterDistReal::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
	    case size((*self.pParamStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pParamStruct).(i)
	        'OBJREF': obj_destroy, (*self.pParamStruct).(i)
	        else:
	 	 endcase
	 endfor
	ptr_free, self.pParamStruct
end

function C_sROIParam_ObjCenterDistReal::init

	ROIParamStruct = {  Name:'Object-Center Distances [x]',$		;  ROI Name.
									pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
									pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
									pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
									pValues:ptr_new()}		  ; Pointer on ROI-Obj Parameter Values.
								
	ROIParamWidgetType = ['widget_slider']
	ROIParamNames = ['Void']
	ROIParamActive = [1b]
	ROIParamMin = [0b]
	ROIParamMax = [1b]
	ROIParamValues =[0b]

	ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
	ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
	ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
	ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
	ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
	ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)
    return, 1
end

pro C_sROIParam_ObjCenterDistReal__define
   tmp = {C_sROIParam_ObjCenterDistReal, pParamStruct: ptr_new()}
end



