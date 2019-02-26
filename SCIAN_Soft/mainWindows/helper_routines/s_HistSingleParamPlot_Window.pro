;_____________________________IOISIOI____________________
; NAME:
;       s_HistSingleParamPlot_Window
;
; PURPOSE:
;       Interactive Histogram Plot
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       s_HistSingleParamPlot_Window, imageData
;
;  KEYWORD PARAMETERS:
;        None
;_____________________________IOISIOI____________________

pro s_HistSingleParamPlot_updateROIGroupObjActive, tlb = tlb, activeVector = activeVector
    widget_control, tlb, get_uValue = state, /no_copy
       oROIParam = state.oROIGroup->getSelectedROIObjFromParamName(selXYZObjectROIParamName = state.selXYZObjectROIParamName, position = position)
    widget_control, tlb, set_uValue = state, /no_copy
    if obj_valid(oROIParam) then begin
       pParamStruct = oROIParam->getpParamStruct()
       *(*pParamStruct).pActive = activeVector
    endif
end


pro s_HistSingleParamPlot_refreshPlot, tlb

   widget_control, tlb, get_uValue = state, /no_copy
      widget_control, state.wDraw, get_value = oHistWindow
      oHistWindow->draw, state.oHistSingleParamPlotView->getHistViewObj()
      if state.fMap then begin
         widget_control, state.wSelectButtonBase, map = 0
         widget_control, state.wSelectButtonBase, /map
      endif else widget_control, state.wSelectButtonBase, map = 0
      if widget_info(state.child_ObjViewWindow_tlb, /valid) then begin

         oThresh = (state.oHistSingleParamPlotView->getFilterContainerObj())->get(position = 0)

         for i = 0, 3 do $
            if (state.fThresholdSliderNumControl[i]) then *(state.pWhereThreshArr)[i] = where(oThresh->applySelect(image = *state.pHistData, select = i) eq 1) $
               else *(state.pWhereThreshArr)[i] = -1

         widget_control, state.ROIOM_tlb, get_uValue = stateROIOM, /no_copy
            case state.selROIGroupObj of
              'oROI2DGroup':(*stateROIOM.poCurrROI2DGroup)->setGroupObjColorsInParamValOrder, ROIObjParamPos = state.ROIObjParamPos, ROIObjSubParamPos = state.ROIObjSubParamPos,$
                                                             pWhereThreshArr = state.pWhereThreshArr, pColorArr = state.pColorArr
              'oROI3DGroup':(*stateROIOM.poCurrROI3DGroup)->setGroupObjColorsInParamValOrder, ROIObjParamPos = state.ROIObjParamPos, ROIObjSubParamPos = state.ROIObjSubParamPos,$
                                                             pWhereThreshArr = state.pWhereThreshArr, pColorArr = state.pColorArr, stack_tlb = stateROIOM.stack_tlb
            endcase
         widget_control, state.ROIOM_tlb, set_uValue = stateROIOM, /no_copy

;         widget_control, state.child_ObjViewWindow_tlb, get_uValue = stateObjView, /no_copy
;            demo_draw, stateObjView.win, stateObjView.oScene, debug = stateObjView.debug
;            s_objworld_update_wListObject, stateObjView
;         widget_control, state.child_ObjViewWindow_tlb, set_uValue = stateObjView, /no_copy
      endif
      child_TableWidget_tlb = state.child_ParamTableWidget_tlb
      if state.fGaussFitControl then state.oHistSingleParamPlotView->setGaussFitValues
   widget_control, tlb, set_uValue = state, /no_copy
   if widget_info(child_TableWidget_tlb, /valid) then s_ROIM_ParamTableWidget_Update, child_TableWidget_tlb
end


pro s_HistSingleParamPlot_Window_Resize_Event, event
    widget_control, event.top, get_uValue = state, /no_copy
       widget_control, state.wDraw, scr_xsize = event.x, scr_ysize = event.y
       widget_control, state.wDraw, get_value = oHistWindow
       oHistWindow->draw, (state.oHistSingleParamPlotView->getHistViewObj())
    widget_control, event.top, set_uValue = state, /no_copy
    s_HistSingleParamPlot_refreshPlot, event.top
end


pro s_HistSingleParamPlot_DrawWindow_event, event

   widget_control, event.top, get_uValue = state, /no_copy
      stack_tlb = state.stack_tlb
   widget_control, event.top, set_uValue = state, /no_copy
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb

   case ([ 'PRESS', 'RELEASE', 'MOTION', 'SCROLL' ])[event.type] of
      'PRESS':begin
              ; Turn draw MOTION events ON if left mouse button pressed.
          if (event.press eq 1) then begin
              widget_control, event.id, draw_Motion_Events = 1
              widget_control, event.top, get_uValue = state, /no_copy
                 state.xs = event.x
                 widget_control, state.wDraw, get_value = oHistWindow
                 pick = oHistWindow->PickData( state.oHistSingleParamPlotView->getHistViewObj(),$
                                               state.oHistSingleParamPlotView->getHistPlotObject(),$
                                               [state.xs, event.y], dataxyz )
                 state.xsData = dataxyz[0]
              widget_control, event.top, set_uValue = state, /no_copy
          endif
      endcase
      'RELEASE':begin
              ; Turn motion events off if left mouse button pressed.
            if (event.release eq 1) then begin
               widget_control, event.id, Draw_Motion_Events = 0
               widget_control, event.top, get_uValue = state, /no_copy
                 state.xd = event.x
                 widget_control, state.wDraw, get_value = oHistWindow
                 pick = oHistWindow->PickData( state.oHistSingleParamPlotView->getHistViewObj(),$
                                               state.oHistSingleParamPlotView->getHistPlotObject(),$
                                               [state.xd, event.y], dataxyz )
                 state.xdData = dataxyz[0]
                 state.oHistSingleParamPlotView->setSingleThreshValues, newXValues = [state.xsData, state.xdData], flagArr = state.fThresholdSliderNumControl

                 if state.fGaussFitControl then state.oHistSingleParamPlotView->setGaussFitValues

                 if (state.selXYZObjectROIParamName eq '-NO SELECTION-') then begin
                   widget_control, event.top, set_uValue = state, /no_copy
                   return
                 endif
              widget_control, event.top, set_uValue = state, /no_copy

              s_HistSingleParamPlot_setParamThreshFromThreshValues, event.top
              widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
                 stateObj.fUpDateROI3DGroup = 1b
                 stateObj.fUpDateROI3DGroupProperties = 1b
              widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy
              s_ROIOM_UpdateXYZObjectPlotWin, wTopBase = stateObj_tlb
          endif
      endcase
      'MOTION':begin
              widget_control, event.top, get_uValue = state, /no_copy
                 state.xd = event.x
                 widget_control, state.wDraw, get_value = oHistWindow
                 pick = oHistWindow->PickData( state.oHistSingleParamPlotView->getHistViewObj(),$
                                               state.oHistSingleParamPlotView->getHistPlotObject(),$
                                               [state.xd, event.y], dataxyz)
                 state.xdData = dataxyz[0]
                 state.oHistSingleParamPlotView->setSingleThreshValues, newXValues = [state.xsData, state.xdData], flagArr = state.fThresholdSliderNumControl
              widget_control, event.top, set_uValue = state, /no_copy
              s_HistSingleParamPlot_setParamThreshFromThreshValues, event.top

              widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
                 stateObj.fUpDateROI3DGroup = 0b
              widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy
              s_ROIOM_UpdateXYZObjectPlotWin, wTopBase = stateObj_tlb
          endcase
    endcase

    if (event.press eq 4) then begin
       widget_control, event.top, get_uValue = state, /no_copy
       state.fMap = 1b - state.fMap
       widget_control, event.top, set_uValue = state, /no_copy
    endif

    s_HistSingleParamPlot_refreshPlot, event.top
end


pro s_HistSingleParamPlot_updateCommunicationWithROIGroupWindow, tlb

    widget_control, tlb, get_uValue = state, /no_copy

       if (state.fKeepSliderValuesControl and widget_info(state.ROIOM_tlb, /valid)) then begin
         widget_control, state.ROIOM_tlb, get_uValue = stateROIOM, /no_copy
          case state.selROIGroupObj of
          'oROI2DGroup':oROIParam = ((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->get(position = state.ROIObjParamPos)
          'oROI3DGroup':oROIParam = ((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->get(position = state.ROIObjParamPos)
          endcase
         widget_control, state.ROIOM_tlb, set_uValue = stateROIOM, /no_copy

         dummy = oROIParam
         save, dummy, filename = s_getPathForSystem()+'obj.tmp'
         restore, s_getPathForSystem()+'obj.tmp', restored_objects = oROIParam, /relaxed
         oROIParam = oROIParam[0]
         pROIParamStruct = oROIParam->getpParamStruct()

         widget_control, state.ROIOM_tlb, get_uValue = stateROIOM, /no_copy
          case state.selROIGroupObj of
          'oROI2DGroup':(((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->get(position = state.ROIObjParamPos))->setpParamStruct, pROIParamStruct
          'oROI3DGroup':(((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->get(position = state.ROIObjParamPos))->setpParamStruct, pROIParamStruct
          endcase
         widget_control, state.ROIOM_tlb, set_uValue = stateROIOM, /no_copy
       endif

    widget_control, tlb, set_uValue = state, /no_copy
end


function s_HistSingleParamPlot_getHistViewObj, tlb, win = win
    widget_control, tlb, get_uValue = state, /no_copy
       oView = state.oHistSingleParamPlotView->getHistViewObj()
       win = state.win
    widget_control, tlb, set_uValue = state, /no_copy
    return, oView
end
function s_HistSingleParamPlot_getHistModelObj, tlb
    widget_control, tlb, get_uValue = state, /no_copy
       oModel = state.oHistSingleParamPlotView->getHistModelObj()
    widget_control, tlb, set_uValue = state, /no_copy
    return, oModel
end


pro s_HistSingleParamPlot_setParamThreshFromThreshValues, tlb
    widget_control, tlb, get_uValue = state, /no_copy

       widget_control, state.ROIOM_tlb, get_uValue = stateROIOM, /no_copy
       case state.selROIGroupObj of
         'oROI2DGroup':oROIParam = ((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->get(position = state.ROIObjParamPos)
         'oROI3DGroup':begin
              if not(obj_valid(*stateROIOM.poCurrROI3DGroup)) then begin
                 widget_control, state.ROIOM_tlb, set_uValue = stateROIOM, /no_copy
                 widget_control, tlb, set_uValue = state, /no_copy
                 return
              endif else oROIParam = ((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->get(position = state.ROIObjParamPos)
          endcase
       endcase
       widget_control, state.ROIOM_tlb, set_uValue = stateROIOM, /no_copy

       pROIValueStruct = oROIParam->getpValueStruct(position = state.ROIObjSubParamPos)
       oThresh = (state.oHistSingleParamPlotView->getFilterContainerObj())->get(position = 0)
       oThresh->get, pParamStruct = pThreshParamStruct

       if ((*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_1a'))[0]]) then begin
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_1a'))[0]] = 1
         (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_1a'))[0]] = $
          (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_1a'))[0]]
         (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_1b'))[0]] = $
          (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_1b'))[0]]
       endif else begin
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_1a'))[0]] = 0
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_1b'))[0]] = 0
       endelse

       if ((*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_2a'))[0]]) then begin
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_2a'))[0]] = 1
         (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_2a'))[0]] = $
          (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_2a'))[0]]
         (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_2b'))[0]] = $
          (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_2b'))[0]]
       endif else begin
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_2a'))[0]] = 0
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_2b'))[0]] = 0
       endelse

       if ((*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_3a'))[0]]) then begin
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_3a'))[0]] = 1
         (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_3a'))[0]] = $
          (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_3a'))[0]]
         (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_3b'))[0]] = $
          (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_3b'))[0]]
       endif else begin
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_3a'))[0]] = 0
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_3b'))[0]] = 0
       endelse

       if ((*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_4a'))[0]]) then begin
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_4a'))[0]] = 1
         (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_4a'))[0]] = $
          (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_4a'))[0]]
         (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_4b'))[0]] = $
          (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_4b'))[0]]
       endif else begin
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_4a'))[0]] = 0
         (*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_4b'))[0]] = 0
       endelse

    widget_control, tlb, set_uValue = state, /no_copy
    s_HistSingleParamPlot_updateCommunicationWithROIGroupWindow, tlb
end


pro s_HistSingleParamPlot_setThreshFromParamThreshValues, tlb
    widget_control, tlb, get_uValue = state, /no_copy

       widget_control, state.ROIOM_tlb, get_uValue = stateROIOM, /no_copy
       case state.selROIGroupObj of
         'oROI2DGroup':oROIParam = ((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->get(position = state.ROIObjParamPos)
         'oROI3DGroup':oROIParam = ((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->get(position = state.ROIObjParamPos)
       endcase
       widget_control, state.ROIOM_tlb, set_uValue = stateROIOM, /no_copy

       if not(obj_valid(oROIParam)) then begin
         widget_control, tlb, set_uValue = state, /no_copy
         return
       endif
       pROIValueStruct = oROIParam->getpValueStruct(position = state.ROIObjSubParamPos)
       oThresh = (state.oHistSingleParamPlotView->getFilterContainerObj())->get(position = 0)
       oThresh->get, pParamStruct = pThreshParamStruct

       if ((*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_1a'))[0]]) then begin
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_1a'))[0]] = 1
         (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_1a'))[0]] = $
          (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_1a'))[0]]
         (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_1b'))[0]] = $
          (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_1b'))[0]]
       endif else begin
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_1a'))[0]] = 0
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_1b'))[0]] = 0
       endelse

       if ((*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_2a'))[0]]) then begin
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_2a'))[0]] = 1
         (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_2a'))[0]] = $
          (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_2a'))[0]]
         (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_2b'))[0]] = $
          (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_2b'))[0]]
       endif else begin
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_2a'))[0]] = 0
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_2b'))[0]] = 0
       endelse

       if ((*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_3a'))[0]]) then begin
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_3a'))[0]] = 1
         (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_3a'))[0]] = $
          (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_3a'))[0]]
         (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_3b'))[0]] = $
          (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_3b'))[0]]
       endif else begin
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_3a'))[0]] = 0
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_3b'))[0]] = 0
       endelse

       if ((*(*pROIValueStruct).pActive)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_4a'))[0]]) then begin
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_4a'))[0]] = 1
         (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_4a'))[0]] = $
          (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_4a'))[0]]
         (*(*pThreshParamStruct).pValues)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_4b'))[0]] = $
          (*(*pROIValueStruct).pValues)[(where((*(*pROIValueStruct).pNames) eq 'Threshold_4b'))[0]]
       endif else begin
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_4a'))[0]] = 0
         (*(*pThreshParamStruct).pActive)[(where((*(*pThreshParamStruct).pNames) eq 'Threshold_4b'))[0]] = 0
       endelse

    widget_control, tlb, set_uValue = state, /no_copy
end


pro s_HistSingleParamPlot_Window_SelectControl, event

    widget_control, event.top, get_uValue = state, /no_copy
       selXYZObjectROIParamName = state.selXYZObjectROIParamName
    widget_control, event.top, set_uValue = state, /no_copy
    if (selXYZObjectROIParamName eq '-NO SELECTION-') then return

    case tag_names(event, /struct) of
       'WIDGET_BUTTON':begin
          widget_control, event.id, get_uValue = uValue, /no_copy
              case uValue of
                 'SLIDER1ONOFF':begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fThresholdSliderNumControl[0] = s_ToggleButtonOnOffState(state.wThresholdSlider1OnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SLIDER2ONOFF':begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fThresholdSliderNumControl[1] = s_ToggleButtonOnOffState(state.wThresholdSlider2OnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SLIDER3ONOFF':begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fThresholdSliderNumControl[2] = s_ToggleButtonOnOffState(state.wThresholdSlider3OnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SLIDER4ONOFF':begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fThresholdSliderNumControl[3] = s_ToggleButtonOnOffState(state.wThresholdSlider4OnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'GAUSSFITONOFF':begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fGaussFitControl = s_ToggleButtonOnOffState(state.wGaussFitOnOff)
                        state.oHistSingleParamPlotView->GaussOnOff, hide = 1b - state.fGaussFitControl
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'KEEPSLIDERVALUESONOFF':begin      ; Switch Flag (1/0 ON/OFF) for keep Threshold Slider On Off
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fKeepSliderValuesControl = s_ToggleButtonOnOffState(state.wKeepSliderValuesOnOff)
                        state.oHistSingleParamPlotView->get, pParamStruct = pParamStruct
                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'fKeepSliderValues'))[0]] = state.fKeepSliderValuesControl
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'PARAMETERTABLEONOFF':begin      ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fParamTable = s_ToggleButtonOnOffState(state.wParamTableOnOff)
                        if state.fParamTable then begin

                           paramTableUValue = {groupLeader:state.wTopBase,$                         ; this widget Group LeaderID
                                                 callName:'s_HistSingleParamPlot_Window',$                                          ; this widget Pro Name
                                                 sTopBaseTitle:'s_HistSingleParamPlot_Data',$
                                                 pwIDParamNumList:ptr_new(),$                             ; pointer to corellate created widget ID's and Parameter Numbers
                                                 oContainer:state.oHistSingleParamPlotView->getFilterContainerObj(),$   ;  Filter Container Object
                                                 wTopBaseID:0,$
                                                 Index:0,$                                                 ; Index of Filter Object in Filter Container Object
                                                 wTableID:-1    }                                          ; WidgetID of the Parameter Widget

                           widget_control, event.id, set_uValue = uValue, /no_copy
                           widget_control, event.top, set_uValue = state, /no_copy
                             s_ISegM_ParamTableWidget, paramTableUValue = paramTableUValue
                           widget_control, event.id, get_uValue = uValue, /no_copy
                           widget_control, event.top, get_uValue = state, /no_copy
                           state.child_ParamTableWidget_tlb = paramTableUValue.wTopBaseID
                        endif else begin
                           if widget_info(state.child_ParamTableWidget_tlb, /valid) then s_ISegM_PTW_cleanUp, state.child_ParamTableWidget_tlb
                           state.child_ParamTableWidget_tlb = -1
                        endelse
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SAVEDATAASASCII':begin
                    ; Parameter_Save
                    widget_control, event.top, get_uValue = state, /no_copy
                        file = pickfile(/write, path = state.path, filter = '*.dat', file = 'file Name', /noconf)
                        if (file eq '') or (file eq (state.path+'\file Name')) then begin
                           a = s_apop_shout('Not saved !')
                        endif else begin
                           state.path = strMid(file,0,(rStrPos(file,'\'))+1)

                           ROIOM_tlb = state.ROIOM_tlb
                           f3D = (strPos(selXYZObjectROIParamName, '3D') ne -1)

                             ; get oROIParam
                           widget_control, ROIOM_tlb, get_uValue = stateROIOM, /no_copy
                             case f3D of
                              0:if obj_valid((*stateROIOM.poCurrROI2DGroup)) then if ( ((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->count() gt 0) then $
                                       oROIParam = ((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->get(position = state.ROIObjParamPos)
                              1:if obj_valid((*stateROIOM.poCurrROI3DGroup)) then if ( ((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->count() gt 0) then $
                                       oROIParam = ((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->get(position = state.ROIObjParamPos)
                             endcase
                           widget_control, ROIOM_tlb, set_uValue = stateROIOM, /no_copy

                            if (obj_valid(oROIParam) and (state.selXYZObjectROIParamName ne '-NO SELECTION-')) then begin
                             oHistPlot = state.oHistSingleParamPlotView->getHistPlotObject()
                             oHistPlot->getProperty, data = AxisValues
                             saveData = fltarr(2,n_elements(*(*(oROIParam->getpValueStruct(position = state.ROIObjSubParamPos))).pROIParamVect))
                             saveData[0,*] = *(*(oROIParam->getpParamStruct())).pROINumberVect
                             saveData[1,*] = *(*(oROIParam->getpValueStruct(position = state.ROIObjSubParamPos))).pROIParamVect
                             openW,2, strCompress(file + strcompress(state.selXYZObjectROIParamName + '_rawData.dat', /rem))
                             printF, 2, saveData
;                             printF, 2, fltarr(1, nData) + transpose(*(*(oROIParam->getpValueStruct(position = state.ROIObjSubParamPos))).pROIParamVect)
                             close, 2
;                          openW,2, strCompress(file + state.selXYZObjectROIParamName + '_histData.dat', /remove)
;                          printF, 2, transpose(fltArr( (size(AxisValues, /dim))[1] ) + AxisValues[0,*])
;                          close, 2
;                          openW,2, strCompress(file + 'Frequency_histData.dat', /remove)
;                          printF, 2, transpose(fltArr( (size(AxisValues, /dim))[1] ) + AxisValues[1,*])
;                          close, 2
                           endif else    a = s_apop_shout('Not saved')
                        ;info.pXRadData
                        endelse
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 else:
               endcase
          widget_control, event.id, set_uValue = uValue, /no_copy
          widget_control, event.top, get_uValue = state, /no_copy
              state.oHistSingleParamPlotView->flagSetThreshValuesOnOff, flagArr = state.fThresholdSliderNumControl
          widget_control, event.top, set_uValue = state, /no_copy
         endcase
       '':begin
          widget_control, event.top, get_uValue = state, /no_copy
              state.oHistSingleParamPlotView->get, pParamStruct = pParamStruct
              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'HistNBins'))[0]] = event.value
              state.oHistSingleParamPlotView->calculateHistValues, histData = *state.pHistData
          widget_control, event.top, set_uValue = state, /no_copy
       endcase
       else:
    endcase

    widget_control, event.top, get_uValue = state, /no_copy
       fKeepSliderValuesControl = state.fKeepSliderValuesControl
       ROIOM_tlb = state.ROIOM_tlb
    widget_control, event.top, set_uValue = state, /no_copy
    if fKeepSliderValuesControl then s_HistSingleParamPlot_setParamThreshFromThreshValues, event.top

    s_HistSingleParamPlot_refreshPlot, event.top
    s_ROIOM_updateXYZPlots, ROIOM_tlb
end


pro s_HistSingleParamPlot_Window_cleanUp, tlb
    widget_control, tlb, get_uValue = state, /no_copy
       if (n_elements(state) ne 0) then begin
         if obj_valid(state.oHistSingleParamPlotView) then obj_destroy, state.oHistSingleParamPlotView
         if ptr_valid(state.pHistData) then ptr_free, state.pHistData
         for i = 0,3 do ptr_free, (state.pColorArr)[i]
         ptr_free, state.pColorArr
         for i = 0,3 do ptr_free, (state.pWhereThreshArr)[i]
         ptr_free, state.pWhereThreshArr

         if widget_info(state.ROIOM_tlb, /valid) then begin
          widget_control, state.ROIOM_tlb, get_uValue = stateParent, /no_copy
              if (n_elements(stateParent) ne 0) then begin
                 if ((where(tag_names(stateParent) eq 'child_HistSegWin_tlb'))[0] ne -1) then stateParent.child_HistSegWin_tlb = -1
                 widget_control, state.ROIOM_tlb, set_uValue = stateParent, /no_copy
              endif
         endif
       endif
    if widget_info(tlb, /valid) then widget_control, tlb, /destroy
end


pro s_HistSingleParamPlot_Window_Update, hist_tlb = hist_tlb,$
                                         selROIGroupObj = selROIGroupObj,$
                                         fKeepROI2DParamThreshs = fKeepROI2DParamThreshs,$
                                         fKeepROI3DParamThreshs = fKeepROI3DParamThreshs,$
                                         ROIObjParamPos = ROIObjParamPos,$
                                         ROIObjSubParamPos = ROIObjSubParamPos,$
                                         selXYZObjectROIParamName = selXYZObjectROIParamName

    if not(widget_info(hist_tlb, /valid)) then return

    widget_control, hist_tlb, get_uValue = state, /no_copy
       sameParam = (selXYZObjectROIParamName eq state.selXYZObjectROIParamName)
       state.selXYZObjectROIParamName = selXYZObjectROIParamName
       state.selROIGroupObj = selROIGroupObj
       state.ROIObjParamPos = ROIObjParamPos
       state.ROIObjSubParamPos = ROIObjSubParamPos
       fKeepSliderValuesControl = state.fKeepSliderValuesControl
       ROIOM_tlb = state.ROIOM_tlb
       stack_tlb = state.stack_tlb
       f3D = (strPos(selXYZObjectROIParamName, '3D') ne -1)
    widget_control, hist_tlb, set_uValue = state, /no_copy

       ; control Parameter Selection
    if (selXYZObjectROIParamName eq '-NO SELECTION-') then begin
       widget_control, hist_tlb, get_uValue = state, /no_copy
           *state.pHistData = 0
         state.oHistSingleParamPlotView->calculateNewHistValues, histData = *state.pHistData
         (state.oHistSingleParamPlotView->getxTitleObj())->setProperty, strings = selXYZObjectROIParamName
       widget_control, hist_tlb, set_uValue = state, /no_copy
       s_HistSingleParamPlot_refreshPlot, hist_tlb
       return
    endif

       ; get oROIParam
    widget_control, ROIOM_tlb, get_uValue = stateROIOM, /no_copy
       case f3D of
         0:if obj_valid((*stateROIOM.poCurrROI2DGroup)) then if ( ((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->count() gt 0) then $
                 oROIParam = ((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->get(position = ROIObjParamPos)
         1:if obj_valid((*stateROIOM.poCurrROI3DGroup)) then if ( ((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->count() gt 0) then $
                 oROIParam = ((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->get(position = ROIObjParamPos)
       endcase
    widget_control, ROIOM_tlb, set_uValue = stateROIOM, /no_copy

       ; get histData
    if obj_valid(oROIParam) then histData = *(*(oROIParam->getpValueStruct(position = ROIObjSubParamPos))).pROIParamVect else histData = 0
    if (histData[0] eq -1) then begin
       widget_control, ROIOM_tlb, get_uValue = stateROIOM, /no_copy
       case f3D of
            0:poDummy = stateROIOM.poCurrROI2DGroup
            1:poDummy = stateROIOM.poCurrROI3DGroup
       endcase
       widget_control, ROIOM_tlb, set_uValue = stateROIOM, /no_copy
            (*poDummy)->calculateGroupParameters, selROIParamName = (*(oROIParam->getpValueStruct(position = ROIObjSubParamPos))).name, stack_tlb = stack_tlb
       histData = *(*(oROIParam->getpValueStruct(position = ROIObjSubParamPos))).pROIParamVect
    endif

       ; update histData
    widget_control, hist_tlb, get_uValue = state, /no_copy
       *state.pHistData = HistData
       state.oHistSingleParamPlotView->calculateNewHistValues, histData = *state.pHistData
       (state.oHistSingleParamPlotView->getxTitleObj())->SetProperty, Strings = 'N = ' + strCompress(string(n_elements(*state.phistData)), /rem)+' |-> ' + selXYZObjectROIParamName
    widget_control, hist_tlb, set_uValue = state, /no_copy

       ; keep SliderValues?
    case f3D of
       0:if ((fKeepSliderValuesControl) and (sameParam) and (fKeepROI2DParamThreshs)) then s_HistSingleParamPlot_setParamThreshFromThreshValues, hist_tlb $
          else s_HistSingleParamPlot_setThreshFromParamThreshValues, hist_tlb
       1:if ((fKeepSliderValuesControl) and (sameParam) and (fKeepROI3DParamThreshs)) then s_HistSingleParamPlot_setParamThreshFromThreshValues, hist_tlb $
          else s_HistSingleParamPlot_setThreshFromParamThreshValues, hist_tlb
    endcase

       ; update SliderValues
    widget_control, hist_tlb, get_uValue = state, /no_copy
       state.oHistSingleParamPlotView->setThreshPlotValues
       state.oHistSingleParamPlotView->setGaussFitValues
    widget_control, hist_tlb, set_uValue = state, /no_copy

    s_HistSingleParamPlot_Window_Update_ThreshWids, hist_tlb
    s_HistSingleParamPlot_refreshPlot, hist_tlb
end


pro s_HistSingleParamPlot_Window_Update_ThreshWids, hist_tlb
       widget_control, hist_tlb, get_uValue = state, /no_copy
         oThresh = state.oHistSingleParamPlotView->getThresholdObj()
         oThresh->get, pParamStruct = pParamStruct

         if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]) then begin
          if (state.fThresholdSliderNumControl[0] eq 0) then state.fThresholdSliderNumControl[0] = s_ToggleButtonOnOffState(state.wThresholdSlider1OnOff)
         endif else if (state.fThresholdSliderNumControl[0] eq 1) then state.fThresholdSliderNumControl[0] = s_ToggleButtonOnOffState(state.wThresholdSlider1OnOff)
         if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]]) then begin
          if (state.fThresholdSliderNumControl[1] eq 0) then state.fThresholdSliderNumControl[1] = s_ToggleButtonOnOffState(state.wThresholdSlider2OnOff)
         endif else if (state.fThresholdSliderNumControl[1] eq 1) then state.fThresholdSliderNumControl[1] = s_ToggleButtonOnOffState(state.wThresholdSlider2OnOff)
         if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]) then begin
          if (state.fThresholdSliderNumControl[2] eq 0) then state.fThresholdSliderNumControl[2] = s_ToggleButtonOnOffState(state.wThresholdSlider3OnOff)
         endif else if (state.fThresholdSliderNumControl[2] eq 1) then state.fThresholdSliderNumControl[2] = s_ToggleButtonOnOffState(state.wThresholdSlider3OnOff)
         if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]) then begin
          if (state.fThresholdSliderNumControl[3] eq 0) then state.fThresholdSliderNumControl[3] = s_ToggleButtonOnOffState(state.wThresholdSlider4OnOff)
         endif else if (state.fThresholdSliderNumControl[3] eq 1) then state.fThresholdSliderNumControl[3] = s_ToggleButtonOnOffState(state.wThresholdSlider4OnOff)
       widget_control, hist_tlb, /update
       widget_control, state.wThresholdSlider1OnOff, /update
       widget_control, hist_tlb, set_uValue = state, /no_copy
end


pro s_HistSingleParamPlot_Window, stack_tlb = stack_tlb,$
                                            selROIGroupObj = selROIGroupObj,$
                                            ROIObjParamPos = ROIObjParamPos,$
                                            ROIObjSubParamPos = ROIObjSubParamPos,$
                                            selXYZObjectROIParamName = selXYZObjectROIParamName,$
                                            groupLeader = groupLeader, basePosition = basePosition,$
                                            application_tlb = application_tlb,$
                                            child_ObjViewWindow_tlb = child_ObjViewWindow_tlb

       ;Check the validity of the group identifier.
    if (n_elements(groupLeader) ne 0) then begin
       if (widget_info(groupLeader, /valid) ne 1) then begin
            print,'Error:the group identifier is not valid.  |--> Returning to the main application.'
            return
        endif
    endif else groupLeader = 0l

       ;Check the validity of the group identifier.
    if (n_elements(child_ObjViewWindow_tlb) ne 0) then begin
       if (widget_info(child_ObjViewWindow_tlb, /valid) ne 1) then child_ObjViewWindow_tlb = -1l
    endif else child_ObjViewWindow_tlb = -1l

       ; Create Hist Window.
    wTopBase = widget_base(Title = 's_Histogram |->', xOffset = basePosition[0], yOffset = basePosition[1], group_leader = groupLeader, tlb_size_events = 1)

    wSelectButtonBase = widget_base(wTopBase, map = 0, event_pro = 's_HistSingleParamPlot_Window_SelectControl', column = 1, /frame)
       void = widget_button(wSelectButtonBase, value = 'Export Data', /menu)
         void = widget_button(void, value = 'Save Data as ASCII', uValue = 'SAVEDATAASASCII')
       wSelectButton = widget_button(wSelectButtonBase, value = 'Select Bin Interval', uValue = '0')
         wBinSliderButton = cw_fslider(wSelectButtonBase, minimum = 2, maximum = 1000, scroll = 1, /edit)
       void = widget_button(wSelectButtonBase, Value = 'Options', /menu)
         wThresholdSlider1OnOff = widget_button(void, Value = 'Slider 1 (off)', uValue = 'SLIDER1ONOFF', /sep)
         wThresholdSlider2OnOff = widget_button(void, Value = 'Slider 2 (off)', uValue = 'SLIDER2ONOFF')
         wThresholdSlider3OnOff = widget_button(void, Value = 'Slider 3 (off)', uValue = 'SLIDER3ONOFF')
         wThresholdSlider4OnOff = widget_button(void, Value = 'Slider 4 (off)', uValue = 'SLIDER4ONOFF')
         wKeepSliderValuesOnOff = widget_button(void, Value = 'Keep Slider Values (on )', uValue = 'KEEPSLIDERVALUESONOFF')
         wParamTableOnOff = widget_button(void, Value = 'Threshold Table (off)', uValue = 'PARAMETERTABLEONOFF')
         wGaussFitOnOff = widget_button(void, Value = 'Gauss Fit (on )', uValue = 'GAUSSFITONOFF', /sep)

    wHistBase_1 = widget_base(wTopBase, /frame)
    wDraw = widget_draw(wHistBase_1, XSize = 350, YSize = 200, retain = 2, graphics_level = 2, expose_events = 1,$
                        button_events = 1, event_pro = 's_HistSingleParamPlot_DrawWindow_event')

    widget_control, wTopBase, /realize
    widget_control, wDraw, get_value = win

    widget_control, groupLeader, get_uValue = stateROIOM, /no_copy
    case selROIGroupObj of
       'oROI2DGroup':oROIParam = ((*stateROIOM.poCurrROI2DGroup)->getParamContainer())->get(position = ROIObjParamPos)
       'oROI3DGroup':oROIParam = ((*stateROIOM.poCurrROI3DGroup)->getParamContainer())->get(position = ROIObjParamPos)
    endcase
    widget_control, groupLeader, set_uValue = stateROIOM, /no_copy

    if (obj_valid(oROIParam) and (selXYZObjectROIParamName ne '-NO SELECTION-')) then $
       histData = *(*(oROIParam->getpValueStruct(position = ROIObjSubParamPos))).pROIParamVect $
       else histData = 0

    if (histData[0] eq -1) then begin
       widget_control, groupLeader, get_uValue = stateROIOM, /no_copy
       case selROIGroupObj of
          'oROI2DGroup':poDummy = stateROIOM.poCurrROI2DGroup
          'oROI3DGroup':poDummy = stateROIOM.poCurrROI3DGroup
       endcase
       widget_control, groupLeader, set_uValue = stateROIOM, /no_copy
       (*poDummy)->calculateGroupParameters, selROIParamName = (*(oROIParam->getpValueStruct(position = ROIObjSubParamPos))).name, stack_tlb = stack_tlb
       histData = *(*(oROIParam->getpValueStruct(position = ROIObjSubParamPos))).pROIParamVect
    endif

    oHistSingleParamPlotView = obj_new('C_sHistPlotViewObject')
    (oHistSingleParamPlotView->getxTitleObj())->SetProperty, strings = 'N = '+ strCompress(string(n_elements(histData)), /rem)+' |-> ' + selXYZObjectROIParamName
    oHistSingleParamPlotView->calculateNewHistValues, histData = histData
    widget_control, wDraw, get_value = oHistWindow
    oHistWindow->draw, (oHistSingleParamPlotView->getHistViewObj())

    pColorArr = ptrArr(4)
    pColorArr[0] = ptr_new([255,0,0], /no_copy)
    pColorArr[1] = ptr_new([0,255,0], /no_copy)
    pColorArr[2] = ptr_new([0,0,255], /no_copy)
    pColorArr[3] = ptr_new([255,255,0], /no_copy)
    pWhereThreshArr = ptrArr(4)
    for i = 0,3 do pWhereThreshArr[i] = ptr_new(-1, /no_copy)

    state = {ROIOM_tlb:groupLeader,$
          wTopBase:wTopBase,$
          stack_tlb:stack_tlb,$
          path:s_getPathForSystem(),$
          selXYZObjectROIParamName:selXYZObjectROIParamName,$
          selROIGroupObj:selROIGroupObj,$
          ROIObjParamPos:ROIObjParamPos,$
          ROIObjSubParamPos:ROIObjSubParamPos,$
          wDraw:wDraw,$
          win:win,$
          wSelectButtonBase:wSelectButtonBase,$
          wParamTableOnOff:wParamTableOnOff,$
             fParamTable:0b,$
             child_ParamTableWidget_tlb:-1L,$
          wKeepSliderValuesOnOff:wKeepSliderValuesOnOff,$
             fKeepSliderValuesControl:1b,$
          wThresholdSlider1OnOff:wThresholdSlider1OnOff,$
          wThresholdSlider2OnOff:wThresholdSlider2OnOff,$
          wThresholdSlider3OnOff:wThresholdSlider3OnOff,$
          wThresholdSlider4OnOff:wThresholdSlider4OnOff,$
             fThresholdSliderNumControl:[0b,0b,0b,0b],$
          wGaussFitOnOff:wGaussFitOnOff,$
             fGaussFitControl:1b,$
          pColorArr:pColorArr,$
          pWhereThreshArr:pWhereThreshArr,$
          oHistSingleParamPlotView:oHistSingleParamPlotView,$
          pHistData:ptr_new(histData, /no_copy),$
          xs:0,$
          xd:0,$
          xsData:0.,$
          xdData:0.,$
          fMap:0b,$ ;   Flag to map control buttons
          child_ObjViewWindow_tlb:child_ObjViewWindow_tlb }

    oHistSingleParamPlotView->get, pParamStruct = pParamStruct
    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'fKeepSliderValues'))[0]] = state.fKeepSliderValuesControl
    widget_control, wBinSliderButton, set_value = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'HistNBins'))[0]]

    application_tlb = wTopBase
    widget_control, wTopBase, set_uValue = state
    s_HistSingleParamPlot_setThreshFromParamThreshValues, wTopBase
    oHistSingleParamPlotView->setThreshPlotValues
    state.oHistSingleParamPlotView->setGaussFitValues
    s_HistSingleParamPlot_Window_Update_ThreshWids, wTopBase
    s_HistSingleParamPlot_refreshPlot, wTopBase
    XManager, 's_HistSingleParamPlot_Window_Resize', wTopBase, group_leader = groupLeader, cleanup = 's_HistSingleParamPlot_Window_cleanUp', /no_block
end;-s_HistSingleParamPlot_Window, event------------------------------

