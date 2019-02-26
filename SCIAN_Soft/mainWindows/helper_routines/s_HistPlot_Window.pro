;_____________________________IOISIOI____________________
; NAME:
;       s_HistPlot_Window
;
; PURPOSE:
;       Interactive Histogram Plot
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
; mods by FASL (2011)
;     e_mail: fsantibanez@med.uchile.cl
; CALLING SEQUENCE:
;       s_HistPlot_Window, imageData
;
;  KEYWORD PARAMETERS:
;        None
;_____________________________IOISIOI____________________

pro s_HistPlot_refreshPlot, tlb
    widget_control, tlb, get_uValue = state, /no_copy
       widget_control, state.wHistDraw, get_value = oHistWindow
       if state.fMap then begin
          widget_control, state.wSelectButtonBase, map = 0
          widget_control, state.wSelectButtonBase, /map
       endif else widget_control, state.wSelectButtonBase, map = 0
          if widget_info(state.zoomWid_tlb, /valid) then begin
             widget_control, state.zoomWid_tlb, get_value = windowID
             Wset, windowID
             oThresh = (state.oHistPlotView->getFilterContainerObj())->get(position = 0)
             sizeHist = size(*state.pHistData, /Dim)
             for i = 3,0,-1 do begin
                if state.fThresholdSliderNumControl[i] then begin
                   whereThresh = where(oThresh->applySelect(image = *state.pHistData, select = i) eq 1, count)
                   if (count gt 0) then begin
                      a = *state.pHistData
                      
                      ; FASL REFERENCE... IF we use max(a) for thresholding then posible max valid values will be pinted with selected tresh
                      ; and so ... we can make diference betwen thresholds and max values.... 
                      ; Initial Fix.. assign the last value of current TBL for trheshold using max(a) + Step for it...
                      ; step.. need be bigger than max(a) and the enough bigger to be binned to different color with max(a value)
                      ; as TBL has 255 values we needs than (max(a) + Step)- min(a) will be mapped to 255 bins and
                      ; at least "nBinsFree" bins of distance betwen (max(a) + Step) and max(a)

                      borders = s_LimitsBinsColor(minValue = min(a), maxValue = max(a), freeBinsLow = 0, freeBinsHigh = 5)
                      a[whereThresh] = max(a) + borders[1]
                      tvscl, a
                      i = -1
                   endif
                endif
             endfor
          endif
       child_ParamWidget_tlb = state.child_ParamWidget_tlb
       child_TableWidget_tlb = state.child_ParamTableWidget_tlb
       if state.fGaussFit then state.oHistPlotView->setGaussFitValues
       oHistWindow->draw, state.oHistPlotView->getHistViewObj()
       if (state.fSaveHistPlot or state.fSaveDataAsASCII or state.fSaveImageDataAsASCII or state.fKruskalWallisHTest) then begin
          stack_tlb = state.stack_tlb
          widget_control, tlb, set_uValue = state, /no_copy
          widget_control, stack_tlb, get_uValue = stackState, /no_copy
          (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
          path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]
          widget_control, stack_tlb, set_uValue = stackState, /no_copy
             s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
          widget_control, tlb, get_uValue = state, /no_copy
          fileClue = strCompress(strCompress('_clus' + string(clusPos)+'_', /rem) $
                                               + strCompress('ch' + string(chPos)+'_', /rem) $
                                               + strCompress('z' + string(zPos)+'_', /rem) $
                                               + strCompress('t' + string(tPos), /rem) )
          if state.fSaveHistPlot then begin
             oHistWindow->getProperty, image_data = snapShot
             write_tiff, strCompress(path + strcompress('Hist' + fileClue + '.tif', /rem)), reverse(snapShot, 3)
          endif
          if state.fSaveDataAsASCII then begin
             oHistPlot = state.oHistPlotView->getHistPlotObject()
             oHistPlot->getProperty, data = data
             openW,2, strCompress(path + strcompress('Hist' + fileClue + '.dat', /rem))
             printF, 2, ['x-axis', 'y-axis']
             printF, 2, data[0:1, *]
             close, 2
          endif
          if state.fSaveImageDataAsASCII then begin
             openW, 2,  strCompress(path + strcompress('ImageInt' + fileClue + '.dat', /rem))
             printF, 2, ['ImageInt']
             printF, 2, make_array(1, n_elements(*state.pHistData), /float) +  (*state.pHistData)[*]
             close, 2
          endif
          if state.fKruskalWallisHTest then begin
             dimKWM = size(*state.pKWMatrix, /dim)

             print, ' - - - - - - - - Kruskal-Wallis H-Test- - - - - -'
             result = kw_test(*state.pKWMatrix, missing = -1.)
             if (result[1] le 0.05 ) then print, 'Kruskal-Wallis H-Test resolved that selected  populations are statistially diffentent (p < 0.05, p = ', result[1],') !' $
               else print, 'Kruskal-Wallis H-Test resolved that the selected populations  are not statistially diffentent (p > 0.05, p = ', result[1],') !'
             print, result
             print, ' - - - - - - - - Kruskal-Wallis H-Test end- - -'

             print, ' - - - - - - - - - Students T-Test - - - - - - - - -'
             if (dimKWM[0] eq 1) then begin
                x = make_array(dimKWM[1]) + (*state.pKWMatrix)[0,*]
                y = make_array(dimKWM[1]) + (*state.pKWMatrix)[0,*]
             endif else begin
                x = make_array(dimKWM[1]) + (*state.pKWMatrix)[dimKWM[0]-1,*]
                y = make_array(dimKWM[1]) + (*state.pKWMatrix)[dimKWM[0]-2,*]
             endelse
             whereDat = where(x ne -1., count)
             if (count gt 0) then x = x[whereDat]
             whereDat = where(y ne -1., count)
             if (count gt 0) then y = y[whereDat]

             result = tm_test(x, y)
             stat = moment(x, sdev = sdev)
             print, 'mean(x) +- sd:', stat[0], '+-', sdev
             stat = moment(y, sdev = sdev)
             print, 'mean(y) +- sd:', stat[0], '+-', sdev
             if (result[1] le 0.05 ) then print, 'Students T-Test resolved that the last two populations selected are statistially diffentent (p < 0.05, p = ', result[1],') !' $
               else print, 'Students T-Test resolved that the last two populations selected are not statistially diffentent (p > 0.05, p = ', result[1],') !'
             print, ' - - - - - - - - - Students T-Test end - - - - - -'

          endif
       endif
    widget_control, tlb, set_uValue = state, /no_copy
    if widget_info(child_ParamWidget_tlb, /valid) then s_ISegM_ParameterWidget_Update, child_ParamWidget_tlb
    if widget_info(child_TableWidget_tlb, /valid) then s_ISegM_ParamTableWidget_Update, child_TableWidget_tlb
end

pro s_HistPlot_Window_Resize_Event, event
    widget_control, event.top, get_uValue = state, /no_copy
       widget_control, state.wHistDraw, scr_xsize = event.x, scr_ysize = event.y
       widget_control, state.wHistDraw, get_value = oHistWindow
       oHistWindow->draw, (state.oHistPlotView->getHistViewObj())
    widget_control, event.top, set_uValue = state, /no_copy
    s_HistPlot_refreshPlot, event.top
end

pro s_HistPlot_DrawWindow_event, event
    case ((['PRESS', 'RELEASE', 'MOTION', 'SCROLL'])[event.type]) of
       'PRESS': begin
              ; Turn draw MOTION events ON if left mouse button pressed.
           if (event.press eq 1) then begin
              widget_control, event.id, Draw_Motion_Events = 1
              widget_control, event.top, get_uValue = state, /no_copy
                 state.xs = event.x
                 widget_control, state.wHistDraw, get_value = oHistWindow
                 pick = oHistWindow->PickData(state.oHistPlotView->getHistViewObj(), state.oHistPlotView->getHistPlotObject(),[state.xs, event.y], dataxyz)
                 state.xsData = dataxyz[0]
              widget_control, event.top, set_uValue = state, /no_copy
           endif
        endcase
       'RELEASE': begin
              ; Turn motion events off if left mouse button pressed.
            if (event.release eq 1) then begin
               widget_control, event.id, Draw_Motion_Events = 0
               widget_control, event.top, get_uValue = state, /no_copy
                 state.xd = event.x
                 widget_control, state.wHistDraw, get_value = oHistWindow
                 pick = oHistWindow->PickData( state.oHistPlotView->getHistViewObj(),$
                                            state.oHistPlotView->getHistPlotObject(),$
                                            [state.xd, event.y], dataxyz)
                 state.xdData = dataxyz[0]
                 state.oHistPlotView->setSingleThreshValues, newXValues = [state.xsData, state.xdData], flagArr = state.fThresholdSliderNumControl
                 if state.fGaussFit then state.oHistPlotView->setGaussFitValues
              widget_control, event.top, set_uValue = state, /no_copy
          endif
         endcase
       'MOTION': begin
              widget_control, event.top, get_uValue = state, /no_copy
                 state.xd = event.x
                 widget_control, state.wHistDraw, get_value = oHistWindow
                 pick = oHistWindow->PickData( state.oHistPlotView->getHistViewObj(),$
                                            state.oHistPlotView->getHistPlotObject(),$
                                            [state.xd, event.y], dataxyz)
                 state.xdData = dataxyz[0]
                 state.oHistPlotView->setSingleThreshValues, newXValues = [state.xsData, state.xdData], flagArr = state.fThresholdSliderNumControl
              widget_control, event.top, set_uValue = state, /no_copy
          endcase
       endcase

    if (event.press eq 4) then begin
       widget_control, event.top, get_uValue = state, /no_copy
       state.fMap = 1-state.fMap
       widget_control, event.top, set_uValue = state, /no_copy
    endif
    s_HistPlot_refreshPlot, event.top
end


pro s_HistPlot_Window_SelectControl, event
    case tag_names(event, /struct) of
       'WIDGET_BUTTON': begin
          widget_control, event.id, get_uValue = uValue, /no_copy
              case uValue of
                 'SLIDER1ONOFF': begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fThresholdSliderNumControl[0] = s_ToggleButtonOnOffState(state.wThresholdSlider1OnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                   endcase
                 'SLIDER2ONOFF': begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fThresholdSliderNumControl[1] = s_ToggleButtonOnOffState(state.wThresholdSlider2OnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                   endcase
                 'SLIDER3ONOFF': begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fThresholdSliderNumControl[2] = s_ToggleButtonOnOffState(state.wThresholdSlider3OnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                   endcase
                 'SLIDER4ONOFF': begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fThresholdSliderNumControl[3] = s_ToggleButtonOnOffState(state.wThresholdSlider4OnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                   endcase
                 'GAUSSFITONOFF': begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fGaussFit = s_ToggleButtonOnOffState(state.wGaussFitOnOff)
                        state.oHistPlotView->GaussOnOff, hide = 1-state.fGaussFit
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'OMITZEROVALUEONOFF': begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fOmitZero = s_ToggleButtonOnOffState(state.wOmitZeroOnOff)
                        state.oHistPlotView->omitZeroValueOnOff, hide = state.fOmitZero
                        state.oHistPlotView->calculateHistValues, histData = *state.pHistData
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SAVEHISTPLOTONOFF': begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fSaveHistPlot = s_ToggleButtonOnOffState(state.wSaveHistPlotOnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SAVEDATAASASCIIONOFF': begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fSaveDataAsASCII = s_ToggleButtonOnOffState(state.wSaveDataAsASCIIOnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SAVEIMAGEDATAASASCIIONOFF': begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fSaveImageDataAsASCII = s_ToggleButtonOnOffState(state.wSaveImageDataAsASCIIOnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'KRUSKALWALLISHTESTONOFF':begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fKruskalWallisHTest = s_ToggleButtonOnOffState(state.wKruskalWallisHTestOnOff)
                        if state.fKruskalWallisHTest then begin
                           *state.pKWMatrix = make_array(1, n_elements(*state.pHistData), /float) + (*state.pHistData)[*]
                        endif else begin
                           *state.pKWMatrix = make_array(1, 2, value = -1.)
                        endelse
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'KEEPSLIDERVALUESONOFF': begin      ; Switch Flag (1/0 ON/OFF) for keep Threshold Slider On Off
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fKeepSliderValuesControl = s_ToggleButtonOnOffState(state.wKeepSliderValuesOnOff)
                        state.oHistPlotView->get, pParamStruct = pParamStruct
                        *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'fKeepSliderValues'))[0]] = state.fKeepSliderValuesControl
                    widget_control, event.top, set_uValue = state, /no_copy
                   endcase
                 'THRESHOLDSLIDERONOFF': begin        ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fParamWidget = s_ToggleButtonOnOffState(state.wParamWidgetOnOff)
                        if state.fParamWidget then begin

                           paramTableUValue = {groupLeader : state.wTopBase,$                              ; this widget Group LeaderID
                                                 callName: 's_HistPlot_Window',$                                             ; this widget Pro Name
                                                 sTopBaseTitle : 's_HistPlot_Data',$
                                                 pwIDParamNumList:ptr_new(),$                             ; pointer to corellate created widget ID's and Parameter Numbers
                                                 oContainer : state.oHistPlotView->getFilterContainerObj(),$  ;  Filter Container Object
                                                 wTopBaseID : 0,$
                                                 Index : 0,$                                                 ; Index of Filter Object in Filter Container Object
                                                 wTableID : -1    }                                          ; WidgetID of the Parameter Widget

                           widget_control, event.id, set_uValue = uValue, /no_copy
                           widget_control, event.top, set_uValue = state, /no_copy
                             s_ISegM_ParameterWidget, paramTableUValue = paramTableUValue
                           widget_control, event.id, get_uValue = uValue, /no_copy
                           widget_control, event.top, get_uValue = state, /no_copy
                           state.child_ParamWidget_tlb = paramTableUValue.wTopBaseID
                        endif else begin
                           if widget_info(state.child_ParamWidget_tlb, /valid) then s_ISegM_PW_cleanUp, state.child_ParamWidget_tlb
                           state.child_ParamWidget_tlb = -1
                        endelse
                    widget_control, event.top, set_uValue = state, /no_copy
                  endcase
                 'ParameterTableONOFF': begin      ; Switch Flag (1/0 ON/OFF) for Thrshold Slider On Off Box Control
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fParamTable = s_ToggleButtonOnOffState(state.wParamTableOnOff)
                        if state.fParamTable then begin

                           paramTableUValue = {groupLeader : state.wTopBase,$                         ; this widget Group LeaderID
                                                 callName: 's_HistPlot_Window',$                                         ; this widget Pro Name
                                                 sTopBaseTitle : 's_HistPlot_Data',$
                                                 pwIDParamNumList:ptr_new(),$                             ; pointer to corellate created widget ID's and Parameter Numbers
                                                 oContainer : state.oHistPlotView->getFilterContainerObj(),$  ;  Filter Container Object
                                                 wTopBaseID : 0,$
                                                 Index : 0,$                                                 ; Index of Filter Object in Filter Container Object
                                                 wTableID : -1    }                                          ; WidgetID of the Parameter Widget

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
                 else:
               endcase
          widget_control, event.id, set_uValue = uValue, /no_copy
          widget_control, event.top, get_uValue = state, /no_copy
              state.oHistPlotView->flagSetThreshValuesOnOff, flagArr = state.fThresholdSliderNumControl
          widget_control, event.top, set_uValue = state, /no_copy
         endcase
       '': begin
          widget_control, event.top, get_uValue = state, /no_copy
              state.oHistPlotView->get, pParamStruct = pParamStruct
              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'HistNBins'))[0]] = event.value
              state.oHistPlotView->calculateHistValues, histData = *state.pHistData
          widget_control, event.top, set_uValue = state, /no_copy
       endcase
       else:
    endcase
    s_HistPlot_refreshPlot, event.top
end

pro s_HistPlot_Window_cleanUp, tlb
    widget_control, tlb, get_uValue = state, /no_copy
       if (n_elements(state) ne 0) then begin
         if obj_valid(state.oHistPlotView) then obj_destroy, state.oHistPlotView
         if ptr_valid(state.pHistData) then ptr_free, state.pHistData

         if widget_info(state.groupLeader, /valid) then begin
          widget_control, state.groupLeader, get_uValue = stateParent, /no_copy
              if (n_elements(stateParent) ne 0) then begin
                 if ((where(tag_names(stateParent) eq 'child_histogramWindow_tlb'))[0] ne -1) then stateParent.child_histogramWindow_tlb = -1
                 widget_control, state.groupLeader, set_uValue = stateParent, /no_copy
              endif
         endif
       endif
    if widget_info(tlb, /valid) then widget_control, tlb, /destroy
end

pro s_HistPlot_Window_Update, histData = histData, hist_tlb
    widget_control, hist_tlb, get_uValue = state, /no_copy
       if (n_elements(histData) ne 0) then *state.pHistData = histData
       widget_control, state.wHistDraw, get_value = oHistWindow

      if state.fKruskalWallisHTest then begin
         dimKWM = size(*state.pKWMatrix, /dim)
         HD = make_array(n_elements(*state.pHistData), /float) + (*state.pHistData)[*]
         dimHD = size(HD, /dim)
         case 1 of
            dimKWM[1] eq dimHD[0]: *state.pKWMatrix = [*state.pKWMatrix, transpose(HD)]
            dimKWM[1] gt dimHD[0]: *state.pKWMatrix = [*state.pKWMatrix, transpose([HD, make_array(dimKWM[1]-dimHD[0], value = -1.)])]
            else:begin
               dummy = make_array(dimKWM[0], dimHD[0], value = -1.)
               dummy[0:dimKWM[0]-1, 0:dimKWM[1]-1] = *state.pKWMatrix
               *state.pKWMatrix = dummy
               dummy = 0b
               *state.pKWMatrix = [*state.pKWMatrix, transpose(HD)]
            endcase
         endcase
      endif
      state.oHistPlotView->calculateNewHistValues, histData = *state.pHistData
    widget_control, hist_tlb, set_uValue = state, /no_copy
    s_HistPlot_refreshPlot, hist_tlb
end

pro s_HistPlot_Window_Update_ThreshWids, hist_tlb
       widget_control, hist_tlb, get_uValue = state, /no_copy
         oTheshold = state.oHistPlotView->getThresholdObj()
         oTheshold->get, pParamStruct = pParamStruct

         if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]) then $
          if (state.fThresholdSliderNumControl[0] eq 0) then state.fThresholdSliderNumControl[0] = s_ToggleButtonOnOffState(state.wThresholdSlider1OnOff) else $
          if (state.fThresholdSliderNumControl[0] eq 1) then state.fThresholdSliderNumControl[0] = s_ToggleButtonOnOffState(state.wThresholdSlider1OnOff)
         if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]]) then $
          if (state.fThresholdSliderNumControl[1] eq 0) then state.fThresholdSliderNumControl[1] = s_ToggleButtonOnOffState(state.wThresholdSlider2OnOff) else $
          if (state.fThresholdSliderNumControl[1] eq 1) then state.fThresholdSliderNumControl[1] = s_ToggleButtonOnOffState(state.wThresholdSlider2OnOff)
         if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]) then $
          if (state.fThresholdSliderNumControl[2] eq 0) then state.fThresholdSliderNumControl[2] = s_ToggleButtonOnOffState(state.wThresholdSlider3OnOff) else $
          if (state.fThresholdSliderNumControl[2] eq 1) then state.fThresholdSliderNumControl[2] = s_ToggleButtonOnOffState(state.wThresholdSlider3OnOff)
         if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]) then $
          if (state.fThresholdSliderNumControl[3] eq 0) then state.fThresholdSliderNumControl[3] = s_ToggleButtonOnOffState(state.wThresholdSlider4OnOff) else $
          if (state.fThresholdSliderNumControl[3] eq 1) then state.fThresholdSliderNumControl[3] = s_ToggleButtonOnOffState(state.wThresholdSlider4OnOff)
         child_ParamWidget_tlb = state.child_ParamWidget_tlb
       widget_control, hist_tlb, set_uValue = state, /no_copy
       if widget_info(child_ParamWidget_tlb, /valid) then s_ISegM_ParameterWidget_Update, child_ParamWidget_tlb
end


pro s_HistPlot_Window, histData, groupLeader = groupLeader, stack_tlb = stack_tlb, basePosition = basePosition, application_tlb = application_tlb, zoomWid_tlb = zoomWid_tlb

       ;Check the validity of the group identifier.
    if (n_elements(groupLeader) ne 0) then begin
       if (widget_info(groupLeader, /valid) ne 1) then begin
            print,'Error: the group identifier is not valid.  |--> Returning to the main application.'
            return
        endif
    endif else groupLeader = 0l

       ;Check the validity of the group identifier.
    if (n_elements(zoomWid_tlb) ne 0) then begin
       if not(widget_info(zoomWid_tlb, /valid)) then drawWidID = -1l
    endif else drawWidID = -1l

       ; Create Hist Window.
    wTopBase = widget_base(Title = 's_Histogram |->', xOffset = basePosition[0], yOffset = basePosition[1], group_leader = groupLeader, tlb_size_events = 1)

    wSelectButtonBase = widget_base(wTopBase, map = 0, event_pro = 's_HistPlot_Window_SelectControl', column = 1, /frame)
       wSelectButton = widget_button(wSelectButtonBase, value = 'Select Bin Interval', uValue = '0')
         wBinSliderButton = cw_fslider(wSelectButtonBase, minimum = 2, maximum = 1000, scroll = 1, /edit)
       void = widget_button(wSelectButtonBase, Value = 'Options', /menu)
         void2 = widget_button(void, Value = 'Slider Options', /menu)
         wParamWidgetOnOff = widget_button(void2, Value = 'Threshold Sliders (off)', uValue = 'THRESHOLDSLIDERONOFF')
         wParamTableOnOff = widget_button(void2, Value = 'Threshold Table (off)', uValue = 'ParameterTableONOFF')
         wKeepSliderValuesOnOff = widget_button(void2, Value = 'Keep Slider Values (on )', uValue = 'KEEPSLIDERVALUESONOFF', /Separator)
         wThresholdSlider1OnOff = widget_button(void, Value = 'Slider 1 (off)', uValue = 'SLIDER1ONOFF', /Separator)
         wThresholdSlider2OnOff = widget_button(void, Value = 'Slider 2 (off)', uValue = 'SLIDER2ONOFF')
         wThresholdSlider3OnOff = widget_button(void, Value = 'Slider 3 (off)', uValue = 'SLIDER3ONOFF')
         wThresholdSlider4OnOff = widget_button(void, Value = 'Slider 4 (off)', uValue = 'SLIDER4ONOFF')
         wGaussFitOnOff = widget_button(void, Value = 'Gauss Fit (on )', uValue = 'GAUSSFITONOFF', /sep)
         wOmitZeroOnOff = widget_button(void, Value = 'Omit Zero Values (off)', uValue = 'OMITZEROVALUEONOFF')
         wSaveHistPlotOnOff = widget_button(void, Value = 'Save Hist Plot (off)', uValue = 'SAVEHISTPLOTONOFF', /sep)
         wSaveDataAsASCIIOnOff = widget_button(void, Value = 'Save Data as ASCII (off)', uValue = 'SAVEDATAASASCIIONOFF')
         wSaveImageDataAsASCIIOnOff = widget_button(void, Value = 'Save Image Data as ASCII (off)', uValue = 'SAVEIMAGEDATAASASCIIONOFF')
         wKruskalWallisHTestOnOff = widget_button(void, Value = 'Kruskal-Wallis H-Test (off)', uValue = 'KRUSKALWALLISHTESTONOFF')

    wHistBase_1 = widget_base(wTopBase, /frame)
    wHistDraw = widget_draw(wHistBase_1, XSize = 350, YSize = 200, retain = 2, graphics_level = 2, expose_events = 1,$
                                                  button_events = 1, event_pro = 's_HistPlot_DrawWindow_event')

    widget_control, wTopBase, /realize
    widget_control, wHistDraw, get_value = oHistWindow

    oHistPlotView = obj_new('C_sHistPlotViewObject')
    oHistPlotView->calculateNewHistValues, histData = histData
    oHistWindow->draw, (oHistPlotView->getHistViewObj())

    state = {groupLeader:groupLeader,$
          wTopBase:wTopBase,$
          stack_tlb:stack_tlb,$
          pHistData:ptr_new(histData, /no_copy),$
          wHistDraw:wHistDraw,$
          wSelectButtonBase:wSelectButtonBase,$
          wParamWidgetOnOff:wParamWidgetOnOff,$
          fParamWidget:0b,$
          child_ParamWidget_tlb:-1L,$
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
              fGaussFit:1b,$
          wOmitZeroOnOff:wOmitZeroOnOff,$
              fOmitZero:1b,$
          wSaveHistPlotOnOff:wSaveHistPlotOnOff,$
              fSaveHistPlot:0b,$
          wSaveDataAsASCIIOnOff:wSaveDataAsASCIIOnOff,$
              fSaveDataAsASCII:0b,$
          wSaveImageDataAsASCIIOnOff:wSaveImageDataAsASCIIOnOff,$
              fSaveImageDataAsASCII:0b,$
          wKruskalWallisHTestOnOff:wKruskalWallisHTestOnOff,$
             fKruskalWallisHTest:0b,$
             pKWMatrix:ptr_new( make_array(1,2, value = -1.), /no_copy),$
          oHistPlotView:oHistPlotView,$
          xs:0,$
          xd:0,$
          xsData:0.,$
          xdData:0.,$
          fMap:0b,$ ; Flag to map control buttons
          zoomWid_tlb:zoomWid_tlb }

    oHistPlotView->get, pParamStruct = pParamStruct
    *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'fKeepSliderValues'))[0]] = state.fKeepSliderValuesControl
    widget_control, wBinSliderButton, set_value = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'HistNBins'))[0]]

    application_tlb = wTopBase
    widget_control, wTopBase, set_uValue = state
    s_HistPlot_Window_Update_ThreshWids, wTopBase
    XManager, 's_HistPlot_Window_Resize', wTopBase, Group_Leader = groupLeader, cleanup = 's_HistPlot_Window_cleanUp', /no_block
end;-s_HistPlot_Window, event------------------------------

