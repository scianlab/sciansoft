;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_LipidPhasePercent
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sROIParam_LipidPhasePercent' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sROIParam_LipidPhasePercent::getROIParamType
	return, 'Group ROI-Parameter-Method'
end

function C_sROIParam_LipidPhasePercent::apply, mask = mask, xySizePerPixel = xySizePerPixel

	if (n_elements(mask) eq 0) then return, {name: 'Lipid Phase [%]',$
														type: 'Group ROI-Parameter-Method',$
														paramType: 'Single Parameter',$
														parameter:  -1}

	whereSubPhase = where(mask eq 0, complement = wherePhase)

	if (total(whereSubPhase) eq -1) then parameter = 1.
	if (total(wherePhase) eq -1) then parameter = 0.
	if ((total(whereSubPhase) ne -1) and (total(wherePhase) ne -1)) then parameter = 1.*n_elements(wherePhase) / (n_elements(whereSubPhase)+n_elements(wherePhase))

	lipidArea1 = *(*self.pParamStruct).pValues[where(*(*self.pParamStruct).pNames eq 'Area/Lipid 1 [Ang²]')]
	lipidArea2 = *(*self.pParamStruct).pValues[where(*(*self.pParamStruct).pNames eq 'Area/Lipid 2 [Ang²]')]
	inverseActive = *(*self.pParamStruct).pActive[where(*(*self.pParamStruct).pNames eq 'Inverse Phase')]

	totalArea = (dimMask[0] *xySizePerPixel[0]) * (dimMask[1] *xySizePerPixel[1])
	totalArea1 = totalArea * parameter
	totalArea2 = totalArea * (1.-parameter)

	numLipid1 = 1. * totalArea1 / lipidArea1
	numLipid2 = 1. * totalArea2 / lipidArea2

	percLipid1 = 100.*numLipid1 / (numLipid1+numLipid2)

	if (inverseActive) then begin
		parameter = 100.-percLipid1
		name = 'Sub-Phase Area [%]'
	endif else begin
		parameter = percLipid1
		name = 'Phase Area [%]'
	endelse

	return, {name: name,$
				type: 'Group ROI-Parameter-Method',$
				paramType: 'Single Parameter',$
				parameter:  parameter}
end

pro C_sROIParam_LipidPhasePercent::set, pParamStruct = pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end

pro C_sROIParam_LipidPhasePercent::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end

pro C_sROIParam_LipidPhasePercent::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
	    case size((*self.pParamStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pParamStruct).(i)
	        'OBJREF': obj_destroy, (*self.pParamStruct).(i)
	        else:
	 	 endcase
	 endfor
	ptr_free, self.pParamStruct
end

function C_sROIParam_LipidPhasePercent::init

	ROIParamStruct = {  Name:'Lipid Phase [%]',$		;  ROI Name.
									pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
									pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
									pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
									pValues:ptr_new()  $		  ; Pointer on ROI-Obj Parameter Values.
								}

	ROIParamWidgetType = ['widget_slider',$
										'widget_slider',$
										'widget_slider']
	ROIParamNames = ['Area/Lipid 1 [Ang²]', 'Area/Lipid 2 [Ang²]', 'Inverse Phase']
	ROIParamActive = [1b, 1b, 0b]
	ROIParamMin = [0, 0, 0]
	ROIParamMax = [500,500, 1]
	ROIParamValues =[51, 84, 0]

	ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
	ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
	ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
	ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
	ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
	ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)
    return, 1
end

pro C_sROIParam_LipidPhasePercent__define
   tmp = {C_sROIParam_LipidPhasePercent, pParamStruct: ptr_new()}
end