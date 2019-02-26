;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_Time
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sROIParam_Time' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sROIParam_Time::getROIParamType
	return, 'Group ROI-Parameter-Method'
end

function C_sROIParam_Time::apply, time = time

	if (n_elements(time) ne 0) then return, {name: 'Time [s]',$
															type: 'Group ROI-Parameter-Method',$
															paramType: 'Single Parameter',$
															parameter: time }
	return, {name: 'Time [s]',$
				type: 'Group ROI-Parameter-Method',$
				paramType: 'Single Parameter',$
				parameter: -1 }
end

pro C_sROIParam_Time::set, pParamStruct = pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end

pro C_sROIParam_Time::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end

pro C_sROIParam_Time::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
	    case size((*self.pParamStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pParamStruct).(i)
	        'OBJREF': obj_destroy, (*self.pParamStruct).(i)
	        else:
	 	 endcase
	 endfor
	ptr_free, self.pParamStruct
end

function C_sROIParam_Time::init

	ROIParamStruct = {  Name:'Time [s]',$		;  ROI Name.
									pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
									pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
									pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
									pValues:ptr_new()  $		  ; Pointer on ROI-Obj Parameter Values.
								}

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

pro C_sROIParam_Time__define
   tmp = {C_sROIParam_Time, pParamStruct: ptr_new()}
end