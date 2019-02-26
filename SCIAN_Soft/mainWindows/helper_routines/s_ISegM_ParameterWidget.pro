;_____________________________IOISIOI____________________
; NAME:
;      s_ISegM_ParameterWidget
;
; PURPOSE:
;       - Interactive Parameter Manipulator as Widgets
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________

pro s_ISegM_ParameterWidget_event, ev

    widget_control, ev.top, get_uvalue = paramTableUValue
    widget_control, ev.id, get_uvalue = eventUValue
    case eventUValue.paramType of
       'widget_slider': begin
                 oAllFilterActive = (paramTableUValue.oContainer->get(Position = paramTableUValue.index))
                 oAllFilterActive->get, pParamStruct = a
                 (*(*a).pValues)[eventUValue.paramNum] = ev.value
                 case paramTableUValue.callName of
                   's_HistPlot_Window': s_HistPlot_Window_Update, paramTableUValue.groupLeader
                   's_Image_SegmentationManipulator_Window': begin
                      widget_control, paramTableUValue.groupLeader, get_uvalue = stateParent, /no_copy
                      widget_control, stateParent.groupLeader, get_uvalue = stateTopParent, /no_copy
                         dummy = stateTopParent.child_ViewWindow_tlb
                      widget_control, stateParent.groupLeader, set_uvalue = stateTopParent, /no_copy
                      widget_control, paramTableUValue.groupLeader, set_uvalue = stateParent, /no_copy
                      if widget_info(dummy, /valid) then zimage_colors, {redraw_image, top : dummy}
                    endcase
                 endcase
         endcase
       'boolOnOff': begin
                 oAllFilterActive = paramTableUValue.oContainer->get(Position = paramTableUValue.index)
                 oAllFilterActive->get, pParamStruct = a
                 (*((*a).pActive))[eventUValue.paramNum] = ev.select
                 widget_control, eventUValue.paramSubWidID, sensitive = ev.select
                 if (paramTableUValue.callName eq 's_HistPlot_Window') then s_HistPlot_Window_Update_ThreshWids, paramTableUValue.groupLeader
         endcase
       else:
    endcase
    s_ISegM_ParameterWidget_Update, ev.top
end

pro s_ISegM_PW_Resize_Event, ev
    widget_control, ev.top, get_uvalue = paramTableUValue, /no_copy
       if (tag_names(ev, /structure_name) eq 'WIDGET_BASE') then widget_control, paramTableUValue.wTopBaseID, scr_ysize = ev.y + 20
    widget_control, ev.top, set_uvalue = paramTableUValue, /no_copy
end

pro s_ISegM_PW_cleanUp, tlb
    widget_control, tlb, get_uvalue = paramTableUValue
    if (n_elements(paramTableUValue) ne 0) then begin
       if ptr_valid(paramTableUValue.pwIDParamNumList) then ptr_free, paramTableUValue.pwIDParamNumList
       if (paramTableUValue.callName eq 's_HistPlot_Window') then begin
         if widget_info(paramTableUValue.groupLeader, /valid) then begin
          widget_control, paramTableUValue.groupLeader, get_uvalue = stateParent, /no_copy
              if (n_elements(stateParent) ne 0) then begin
                 if (stateParent.fParamWidget) then stateParent.fParamWidget = s_ToggleButtonOnOffState(stateParent.wParamWidgetOnOffButton)
                 widget_control, paramTableUValue.groupLeader, set_uvalue = stateParent, /no_copy
              endif
         endif
       endif
    endif
    if widget_info(tlb, /valid) then widget_control, tlb, /destroy
end

pro s_ISegM_ParameterWidget_Update, tlb
    widget_control, tlb, get_uvalue = paramTableUValue, /no_copy

    oAllFilterActive = paramTableUValue.oContainer->get(Position = paramTableUValue.index)
    oAllFilterActive->get, pParamStruct = a

    for i = 0, n_elements((*(*a).pWidgetType))-1 do begin
       if ((*(*a).pWidgetType)[i] eq 'widget_slider') then begin
          if ( (*(*a).pValues)[i] gt (*(*a).pMax)[i]) then (*(*a).pMax)[i] = (*(*a).pValues)[i]
          if ( (*(*a).pValues)[i] lt (*(*a).pMin)[i]) then (*(*a).pMin)[i] = (*(*a).pValues)[i]
          if ((*(*a).pMax)[i] eq (*(*a).pMin)[i]) then (*(*a).pMax)[i] = (*(*a).pMin)[i] + .001
          widget_control, 1l*(*paramTableUValue.pwIDParamNumList)[i,0],$
                                        set_slider_max = (*(*a).pMax)[i],$
                                        set_slider_min = (*(*a).pMin)[i],$
                                        set_value = (*(*a).pValues)[i],$
                                        sensitive = (*((*a).pActive))[i]
          widget_control, 1l*(*paramTableUValue.pwIDParamNumList)[i,1],$
                                        set_value = (*((*a).pActive))[i]
       endif
    endfor
    widget_control, tlb, set_uvalue = paramTableUValue, /no_copy
end


pro s_ISegM_ParameterWidget, paramTableUValue = paramTableUValue

    oAllFilterActive = paramTableUValue.oContainer->get(Position = paramTableUValue.index)
    oAllFilterActive->get, pParamStruct = a

    wTopBase = widget_base(Title = (*a).Name+'_Filter_Parameter ->', /column,$
                           Group_Leader = paramTableUValue.groupLeader,$
                           scr_ysize = ( n_elements((*(*a).pWidgetType))*150 < 300  ),$
                           y_scroll_size = ( n_elements((*(*a).pWidgetType))*150 < 300  )-50,$
                           tlb_size_events = 1,/frame)

    pwIDParamNumList = intArr(n_elements((*(*a).pWidgetType)), 2)
    for i = 0, n_elements((*(*a).pWidgetType))-1 do begin
       if ((*(*a).pWidgetType)[i] eq 'widget_slider') then begin
          wSubBase = widget_base(wTopBase, /column, /frame, event_pro = 's_ISegM_ParameterWidget_event')
          wBGroup = cw_bgroup( wSubBase, (*(*a).pNames)[i], /nonexclusive)
          if ((*(*a).pValues)[i] gt (*(*a).pMax)[i]) then (*(*a).pMax)[i] = (*(*a).pValues)[i]
          if ((*(*a).pValues)[i] lt (*(*a).pMin)[i]) then (*(*a).pMin)[i] = (*(*a).pValues)[i]
          if ((*(*a).pMax)[i] eq (*(*a).pMin)[i]) then (*(*a).pMax)[i] = (*(*a).pMin)[i] + .001
          wFSlider = cw_fslider(wSubBase, maximum = (*(*a).pMax)[i],$
                                minimum = (*(*a).pMin)[i],$
                                value = (*(*a).pValues)[i],$
                                edit = 1,$
                                uValue = {paramNum: i, paramType: 'widget_slider'})
          widget_control, wFSlider, sensitive = (*((*a).pActive))[i]
          widget_control, wBGroup, set_value = (*(*a).pActive)[i],$
                           set_uvalue = {paramNum: i, paramType: 'boolOnOff', paramSubWidID: wFSlider}
          pwIDParamNumList[i,0] = wFSlider
          pwIDParamNumList[i,1] = wBGroup
       endif
    endfor

    paramTableUValue.pwIDParamNumList = ptr_new(pwIDParamNumList, /no_copy)
    paramTableUValue.wTopBaseID = wTopBase
    widget_control, wTopBase, set_uvalue = paramTableUValue, /realize
    XManager, 's_ISegM_PW_Resize', wTopBase, CleanUp = 's_ISegM_PW_cleanUp', /just_reg, /no_block, group_leader = paramTableUValue.groupLeader
end
