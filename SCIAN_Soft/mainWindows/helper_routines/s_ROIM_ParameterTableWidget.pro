;_____________________________IOISIOI____________________
; NAME:
;      s_ROIM_ParameterWidget
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


pro s_ROIM_ParamTableWidget_event, ev

       ; What Action is wanted?
    uname = widget_info(ev.id, /uname)
    dummy = (where(tag_names(ev) eq'CH'))[0]
    if (dummy ne -1) then if (ev.ch eq 13) then begin
       widget_control, ev.top, get_uValue = paramTableUValue, /no_copy
         widget_control, paramTableUValue.wTableID, Get_Value = ParamValues
          oAllFilterActive = (paramTableUValue.oContainer->get(Position = paramTableUValue.index))
          case paramTableUValue.callName of
              's_HistPlot_Window': a = oAllFilterActive->getpParamStruct()
              's_Image_SegmentationManipulator_Window': a = oAllFilterActive->getpParamStruct()
              's_ROI_ObjectManipulator_Window': a = oAllFilterActive->getpParamStruct()
              'ROIObjModelParams': a = oAllFilterActive->getpValueStruct(position = paramTableUValue.subParamIndex)
              's_ROI_SubParam': a = oAllFilterActive->getpValueStruct(position = paramTableUValue.subParamIndex)
          endcase
       widget_control, ev.top, set_uValue = paramTableUValue, /no_copy

         *((*a).pActive) = temporary(*((*a).pActive))*0 + ParamValues[*,0]
         *((*a).pMin) = temporary(*((*a).pMin))*0  + ParamValues[*,1]
         *((*a).pMax) = temporary(*((*a).pMax))*0  + ParamValues[*,2]
         *((*a).pValues) = temporary(*((*a).pValues))*0  + ParamValues[*,3]
         a = -1

       widget_control, ev.top, get_uValue = paramTableUValue, /no_copy
       case paramTableUValue.callName of
         's_HistPlot_Window': begin
                 groupLeader = paramTableUValue.groupLeader
                 widget_control, ev.top, set_uValue = paramTableUValue, /no_copy
                   s_HistPlot_Window_Update, groupLeader
                 widget_control, ev.top, get_uValue = paramTableUValue, /no_copy
         endcase
         's_Image_SegmentationManipulator_Window': begin
                 widget_control, paramTableUValue.groupLeader, get_uValue = stateParent, /no_copy
                   widget_control, stateParent.groupLeader, get_uValue = stateTopParent, /no_copy
                    dummy = stateTopParent.child_ViewWindow_tlb
                    widget_control, stateParent.groupLeader, set_uValue = stateTopParent, /no_copy
                 widget_control, paramTableUValue.groupLeader, set_uValue = stateParent, /no_copy
                 widget_control, ev.top, set_uValue = paramTableUValue, /no_copy
                   if (widget_info(dummy, /valid)) then zimage_colors, {redraw_image, top: dummy}
                 widget_control, ev.top, get_uValue = paramTableUValue, /no_copy
         endcase
         'ROIObjModelParams':
         's_ROI_SubParam':
       endcase
       widget_control, ev.top, set_uValue = paramTableUValue, /no_copy
    endif
end

pro s_ROIM_PTW_Resize_Event, ev
    widget_control, ev.top, get_uValue = paramTableUValue, /no_copy
       if (tag_names(ev, /structure_name) eq 'WIDGET_BASE') then widget_control, paramTableUValue.wTableID, scr_xsize = ev.x;, column_width = ev.x-120, scr_ysize = ev.y-20
    widget_control, ev.top, set_uValue = paramTableUValue, /no_copy
end

pro s_ROIM_PTW_cleanUp, tlb
    widget_control, tlb, get_uValue = paramTableUValue
    if (n_elements(paramTableUValue) ne 0) then begin
       if ptr_valid(paramTableUValue.pwIDParamNumList) then ptr_free, paramTableUValue.pwIDParamNumList
     if (paramTableUValue.callName eq 's_HistPlot_Window') then begin
         if widget_info(paramTableUValue.groupLeader, /valid) then begin
          widget_control, paramTableUValue.groupLeader, get_uValue = stateParent, /no_copy
              if (n_elements(stateParent) ne 0) then begin
                 if (stateParent.fParamTable) then stateParent.fParamTable = s_ToggleButtonOnOffState(stateParent.wParamTableOnOffButton)
                 widget_control, paramTableUValue.groupLeader, set_uValue = stateParent, /no_copy
              endif
         endif
       endif
    endif
    if widget_info(tlb, /valid) then widget_control, tlb, /destroy
end

pro s_ROIM_ParamTableWidget_Update, tlb
    widget_control, tlb, get_uValue = paramTableUValue, /no_copy

    oAllFilterActive = paramTableUValue.oContainer->get(Position = paramTableUValue.index)
    case paramTableUValue.callName of
       's_HistPlot_Window': a = oAllFilterActive->getpParamStruct()
       's_Image_SegmentationManipulator_Window': a = oAllFilterActive->getpParamStruct()
       's_ROI_ObjectManipulator_Window': a = oAllFilterActive->getpParamStruct()
       'ROIObjModelParams': a = oAllFilterActive->getpValueStruct(position = paramTableUValue.subParamIndex)
       's_ROI_SubParam': a = oAllFilterActive->getpValueStruct(position = paramTableUValue.subParamIndex)
       's_HistSingleParamPlot_Window': oAllFilterActive->get, pParamStruct = a
    endcase

    widget_control, paramTableUValue.wTableID, set_value = [ [*((*a).pActive)],$
                                             [*((*a).pMin)],$
                                             [*((*a).pMax)],$
                                             [*((*a).pValues)]  ]

    widget_control, tlb, set_uValue = paramTableUValue, /no_copy
end

pro s_ROIM_ParamTableWidget_DifferenceUpdate, newParamTableUValue = newParamTableUValue
    widget_control, newParamTableUValue.wTopBaseID, get_uValue = paramTableUValue, /no_copy

    oAllFilterActive = newParamTableUValue.oContainer->get(Position = $
                   ((newParamTableUValue.oContainer->count()-1) < newParamTableUValue.index))
    a = oAllFilterActive->getpParamStruct()

    widget_control, paramTableUValue.wTopBaseID, tlb_set_title = (*a).Name+'_Filter_Parameter ->' + newParamTableUValue.sTopBaseTitle
    new = n_elements(*((*a).pNames))
    widget_control, paramTableUValue.wTableID, table_xSize = n_elements(*((*a).pNames)),$
                                            column_labels = *((*a).pNames),$
                                            column_width = strLen( *((*a).pNames))*7,$
                                            set_value = [ [*((*a).pActive)],$
                                            [*((*a).pMin)],$
                                            [*((*a).pMax)],$
                                            [*((*a).pValues)]]

    paramTableUValue.sTopBaseTitle = newParamTableUValue.sTopBaseTitle
    paramTableUValue.oContainer = newParamTableUValue.oContainer
    paramTableUValue.index = newParamTableUValue.index
    widget_control, newParamTableUValue.wTopBaseID, set_uValue = paramTableUValue, /no_copy
end


pro s_ROIM_ParamTableWidget, paramTableUValue = paramTableUValue

    oAllFilterActive = paramTableUValue.oContainer->get(Position = paramTableUValue.index)

    case paramTableUValue.callName of
       's_HistPlot_Window': a = oAllFilterActive->getpParamStruct()
       's_Image_SegmentationManipulator_Window': a = oAllFilterActive->getpParamStruct()
       's_ROI_ObjectManipulator_Window': a = oAllFilterActive->getpParamStruct()
       'ROIObjModelParams': a = oAllFilterActive->getpValueStruct(position = paramTableUValue.subParamIndex)
       's_ROI_SubParam': a = oAllFilterActive->getpValueStruct(position = paramTableUValue.subParamIndex)
    endcase

    wTopBase = Widget_Base(title = (*a).Name+'_Filter_Parameter ->' + paramTableUValue.sTopBaseTitle,$
                             /column, tlb_size_events = 1,$
                             Group_Leader = paramTableUValue.groupLeader, MBar = menubase)

    wTableID = widget_table(wTopBase, event_pro = 's_ROIM_ParamTableWidget_event',$
                                     column_labels = *((*a).pNames),$
                                     column_width = strLen( *((*a).pNames))*7,$
                                     /resizeable_columns,$
                                     x_scroll_size = 4,$
                                     row_labels = ['Param. Active','Param. min','Param. max','Param. Values' ],$
                                     value = [ [*((*a).pActive)],$
                                             [*((*a).pMin)],$
                                             [*((*a).pMax)],$
                                             [*((*a).pValues)]  ],$
                                     uvalue = 'All Filter Param Table',$
                                     all_events = 1, /editable, /frame)

    paramTableUValue.wTableID = wTableID
    paramTableUValue.wTopBaseID = wTopBase
    widget_control, wTopBase, set_uValue = paramTableUValue, /Realize
    XManager, 's_ROIM_PTW_Resize', wTopBase, CleanUp = 's_ROIM_PTW_cleanUp', /No_Block, group_leader = paramTableUValue.groupLeader
end