;_____________________________IOISIOI____________________
; NAME:
;      s_IStackM_ParameterTable
;
; PURPOSE:
;       - Interactive Parameter Manipulator as Widgets
;
; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________


pro s_IStackM_ParameterButtonWidget_Event, ev
	case (widget_info(ev.id, /uName)) of
	   'RETURNANDACCEPTCHANGES': begin
						widget_control, ev.top, get_uvalue = paramTableUValue, /no_copy
						case paramTableUValue.name of
						'Stack': begin
										s_ISM_getProjectInfo, stack_tlb = paramTableUValue.groupLeader, tPos = tPos, chPos = chPos , zPos = zPos
										widget_control,  paramTableUValue.wTableID, get_value = paramStruct
										widget_control,  paramTableUValue.groupLeader, get_uValue = state, /no_copy
											(*state.pImageStackInfoObject)->setParamAsStruct, paramStruct
											(*state.pImageStackInfoObject)->updateTimeChannelZStackDimensions, tPos = tPos, chPos = chPos , zPos = zPos
										widget_control,  paramTableUValue.groupLeader, set_uValue = state, /no_copy
										s_ISM_setSelectedImageClusterSegPosition, stack_tlb = paramTableUValue.groupLeader, tPos = tPos, chPos = chPos , zPos = zPos
										s_ISM_UpdateWidgets, paramTableUValue.groupLeader
								endcase
						'Image': begin
										s_ISM_getProjectInfo, stack_tlb = paramTableUValue.groupLeader, tPos = tPos, chPos = chPos , zPos = zPos
										widget_control,  paramTableUValue.wTableID, get_value = paramStruct
										widget_control,  paramTableUValue.groupLeader, get_uValue = state, /no_copy
											(*state.pImageStackInfoObject)->setSelectedImageParamAsStruct, paramStruct, tPos = tPos, chPos = chPos, zPos = zPos
										widget_control,  paramTableUValue.groupLeader, set_uValue = state, /no_copy
								endcase
						'oROI2DGroup': begin
										widget_control,  paramTableUValue.wTableID, get_value = paramStruct
										widget_control,  paramTableUValue.groupLeader, get_uValue = state, /no_copy
											 (*state.poCurrROI2DGroup)->setParamAsStruct, paramStruct
										widget_control,  paramTableUValue.groupLeader, set_uValue = state, /no_copy
								endcase
						'oROI3DGroup': begin
										widget_control,  paramTableUValue.wTableID, get_value = paramStruct
										widget_control,  paramTableUValue.groupLeader, get_uValue = state, /no_copy
											 (*state.poCurrROI3DGroup)->setParamAsStruct, paramStruct
										widget_control,  paramTableUValue.groupLeader, set_uValue = state, /no_copy
								endcase
						endcase
						widget_control, ev.top, set_uvalue = paramTableUValue, /no_copy
					    widget_control, ev.top, /destroy
	   				endcase
	   	else:
	endcase
end


pro s_IStackM_ParamTableWidget_Event, ev
	if ((where(tag_names(ev) eq 'CH'))[0] ne -1) then begin
		case (ev.ch) of
		   13: begin
					widget_control, ev.top, get_uvalue = paramTableUValue, /no_copy
					groupLeader = paramTableUValue.groupLeader
					case paramTableUValue.name of
							'oROI2DGroup': begin
											widget_control,  paramTableUValue.wTableID, get_value = paramStruct
											widget_control,  paramTableUValue.groupLeader, get_uValue = state, /no_copy
												 (*state.poCurrROI2DGroup)->setParamAsStruct, paramStruct
											widget_control,  paramTableUValue.groupLeader, set_uValue = state, /no_copy
										widget_control, ev.top, set_uvalue = paramTableUValue, /no_copy
										s_ROIOM_UpdateWidgets,  groupLeader
									endcase
							'oROI3DGroup': begin
											widget_control,  paramTableUValue.wTableID, get_value = paramStruct
											widget_control,  paramTableUValue.groupLeader, get_uValue = state, /no_copy
												 (*state.poCurrROI3DGroup)->setParamAsStruct, paramStruct
											widget_control,  paramTableUValue.groupLeader, set_uValue = state, /no_copy
										widget_control, ev.top, set_uvalue = paramTableUValue, /no_copy
										s_ROIOM_UpdateWidgets,  groupLeader
									endcase
							else:
	  				endcase
				endcase
		   	else:
		endcase
	endif
end


pro s_IStackM_PTW_Resize_Event, ev
	widget_control, ev.top, get_uvalue = paramTableUValue, /no_copy
		if (tag_names(ev, /structure_name) eq 'WIDGET_BASE') then widget_control, paramTableUValue.wTableID, scr_xsize=ev.x, column_width =ev.x-120, scr_ysize=ev.y-20
	widget_control, ev.top, set_uvalue = paramTableUValue, /no_copy
end


pro s_IStackM_ParamTableWidget, paramTableUValue = paramTableUValue

	wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters',  /column, tlb_size_events =  1, group_leader = paramTableUValue.groupLeader, MBar = menuBase)

		void = widget_button(wTopBase,  value = 'Return and accept changes', uName =  'RETURNANDACCEPTCHANGES', event_pro = 's_IStackM_ParameterButtonWidget_Event')

		paramTableUValue.wTableID = widget_table(wTopBase,  row_labels = paramTableUValue.paramNames,$
																														event_pro = 's_IStackM_ParamTableWidget_Event',$
																														column_labels = ['Parameter Values' ],$
																														column_width = StrLen('Parameter Values')*7,$
																														/resizeable_columns,$
																														/column_major,$
																														value =  paramTableUValue.paramAsStruct,$
																														/all_events, /editable, /frame, /no_copy)
		; Dereference Pointer. Realize and register the widget.
	widget_control, wTopBase, set_uvalue =  paramTableUValue, /Realize
	XManager, 's_IStackM_PTW_Resize', wTopBase, /No_Block, group_leader = paramTableUValue.groupLeader
end
