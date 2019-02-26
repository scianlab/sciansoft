;_____________________________IOISIOI____________________
; NAME:
;      s_ObjParamTableWidget
;
; PURPOSE:
;       - Interactive Parameter Manipulator as Widgets
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________

pro s_ObjParamTableWidget_Event, ev
    case widget_info(ev.id, /uName) of
       'RETURNANDACCEPTCHANGES':begin
                 widget_control, ev.top, get_uValue = state, /no_copy
                 case state.paramTableUValue.name of
                 'Stack':begin
                           s_ISM_getProjectInfo, stack_tlb = state.groupLeader, tPos = tPos, chPos = chPos, zPos = zPos
                           widget_control, state.wTableID, get_value = paramStruct
                           widget_control, state.groupLeader, get_uValue = stateStack, /no_copy
                              (*stateStack.pImageStackInfoObject)->setParamAsStruct, paramStruct
                              (*stateStack.pImageStackInfoObject)->updateTimeChannelZStackDimensions, tPos = tPos, chPos = chPos, zPos = zPos
                           widget_control, state.groupLeader, set_uValue = stateStack, /no_copy
                           s_ISM_setSelectedImageClusterSegPosition, stack_tlb = state.groupLeader, tPos = tPos, chPos = chPos, zPos = zPos

                           widget_control, state.paramTableUValue.ev.id, set_uValue = 'CALCULATEALL'
                           buttonEV = state.paramTableUValue.ev
                           groupLeader = state.groupLeader
                           widget_control, ev.top, set_uValue = state, /no_copy

                           s_ISM_Window_Button_Event, buttonEV

                           widget_control, ev.top, get_uValue = state, /no_copy
                             widget_control, state.paramTableUValue.ev.id, set_uValue = 'SETSTACKINFORMATION'
                           widget_control, ev.top, set_uValue = state, /no_copy

                           widget_control, ev.top, /destroy

                           s_ISM_UpdateWidgets, groupLeader
                 endcase
                 'Image':begin
                           s_ISM_getProjectInfo, stack_tlb = state.groupLeader, tPos = tPos, chPos = chPos, zPos = zPos
                           widget_control, state.wTableID, get_value = paramStruct
                           widget_control, state.groupLeader, get_uValue = stateStack, /no_copy
                              (*stateStack.pImageStackInfoObject)->setSelectedImageParamAsStruct, paramStruct, tPos = tPos, chPos = chPos, zPos = zPos
                           widget_control, state.groupLeader, set_uValue = stateStack, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.top, /destroy
                 endcase
                 'oROI2DGroup':begin
                           widget_control, state.wTableID, get_value = paramStruct
                           widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                              (*stateOM.poCurrROI2DGroup)->setParamAsStruct, paramStruct
                           widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.top, /destroy
                 endcase
                 'oROI3DGroup':begin
                           widget_control, state.wTableID, get_value = paramStruct
                           widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                              (*stateOM.poCurrROI3DGroup)->setParamAsStruct, paramStruct
                           widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.top, /destroy
                 endcase
                 'Colocalization':begin
                           widget_control, state.wTableID, get_value = paramStruct
                           widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                              stateOM.fViewDataTable = s_ToggleButtonOnOffState(stateOM.wViewDataTableOnOff)
                           widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           widget_control, ev.top, /destroy
                 endcase
                 else:
              endcase
       endcase
       'SHOWRADIUSPLOT':begin
              widget_control, ev.top, get_uValue = state, /no_copy
                 if ((state.tableEv.SEL_LEFT ge 0) and (state.tableEv.SEL_RIGHT ge 0)) then begin
                    widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                    if (n_elements(stateOM) ne 0) then begin
                           ; define plotDataMatrix
                           ; MOR - 03 Sept 2010 - make it a long integer array in order to capture more info.
                        ;radCount = intArr(stateOM.radiusValue + 1)
                        radCount = ulonArr(stateOM.radiusValue + 1)
                        for k = -long(stateOM.radiusValue),long(stateOM.radiusValue) do for l = -long(stateOM.radiusValue),long(stateOM.radiusValue) do begin
                           rad = floor(sqrt(1.*(k*k+l*l)))
                           if ( (1.*(k*k+l*l)) le (long(stateOM.radiusValue))^2 ) then radCount[rad] = radCount[rad] +1
                        endfor
                        plotDataMatrix = fltArr(max(radCount), long(stateOM.radiusValue) + 1)

                           ; fill plotDataMatrix
                        for row = state.tableEv.SEL_LEFT, state.tableEv.SEL_RIGHT do for column = state.tableEv.SEL_TOP, state.tableEv.SEL_BOTTOM do begin
                           ; MOR - 03Sept2010 - make them long integer
                           ;radCount[*] = 0
                           ;plotDataMatrix[*] = 0.
                           ;i = 0
                           radCount[*] = 0L
                           plotDataMatrix[*] = 0.
                           i = 0L
                           for k = -long(stateOM.radiusValue),long(stateOM.radiusValue) do for l = -long(stateOM.radiusValue),long(stateOM.radiusValue) do begin
                             if ( (1.*(k*k+l*l)) le (long(stateOM.radiusValue))^2 ) then begin
                                rad = floor(sqrt(1.*(k*k+l*l)))
                                plotDataMatrix[radCount[rad], rad] = state.paramTableUValue.colocRandomParams[i, row, column]
                                radCount[rad] += 1
                                i += 1
                             endif
                           endfor

                           xValues = [0]
                           yValues = plotDataMatrix[0,0]
                           yMeanValues = yValues
                           ySDValues = 0
                           for i = 1, stateOM.radiusValue do begin
                              xValues = [xValues, make_array(radCount[i], /int, value = i)]
                              yValues = [yValues, make_array(radCount[i], /float) + plotDataMatrix[0:radCount[i]-1, i]  ]
                              mom = moment(plotDataMatrix[0:radCount[i]-1, i])
                              yMeanValues = [yMeanValues, mom[0]]
                              ySDValues = [ySDValues, sqrt(mom[1])]
                           endfor

                             ; plot s_Coloc
                           style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type:1, symbol_size :.01},$
                                                                                 visualization_properties = {color:'White'},$
                                                                                 xAxis_properties = {axisTitle:'distance [pixel]'},$
                                                                                 yAxis_properties = {axisTitle:state.paramTableUValue.paramColumnNames[row] },$
                                                                                 legend_properties = {hide:1})
                           live_plot, yValues, independent = xValues,$
                                               /scatter, draw_dimension = [400,300], /no_select,$
                                               xRange = [-.25, stateOM.radiusValue + .25],$
                                               style = style,$
                                               reference_out = refOut,$
                                               title = 's_Coloc |-> ' +  state.paramTableUValue.paramColumnNames[row] + ' | ' + state.paramTableUValue.paramRowNames[column]

                           if (n_elements(refOut) gt 0) then begin
                              live_info, refOut.graphic[0], properties = variable, window_in = refOut.Win
                              variable.color = 'Light Gray'
                              live_control, refOut.graphic[0], properties = variable, window_in = refOut.Win
                              live_oplot, yMeanValues,$
                                          name = {i:'mean', data :'mean'},$
                                          subType = 'ScatterPlot',$
                                          reference_out = refOut,$
                                          window_in = refOut.Win, /no_select
                              live_info, refOut.graphic[1], properties = variable, window_in = refOut.Win
                              variable.color = 'Black'
                              variable.symbol_type = 6
                              variable.symbol_size = 0.02
                              live_control, refOut.graphic[1], properties = variable, window_in = refOut.Win
                              live_oplot, yMeanValues+ySDValues,$
                                          name = {i:'sdPlus', data :'sdPlus'},$
                                          subType = 'ScatterPlot',$
                                          reference_out = refOut,$
                                          window_in = refOut.Win, /no_select
                              live_info, refOut.graphic[2], properties = variable, window_in = refOut.Win
                              variable.color = 'Black'
                              variable.symbol_size = 0.02
                              live_control, refOut.graphic[2], properties = variable, window_in = refOut.Win
                              live_oplot, yMeanValues-ySDValues,$
                                          name = {i:'sdMinus', data:'sdMinus'},$
                                          subType = 'ScatterPlot',$
                                          reference_out = refOut,$
                                          window_in = refOut.Win, /no_select
                              live_info, refOut.graphic[3], properties = variable, window_in = refOut.Win
                              variable.color = 'Black'
                              variable.symbol_size = 0.02
                              live_control, refOut.graphic[3], properties = variable, window_in = refOut.Win
                           endif

                            ; plot s_Coloc PDF (Probability Density Function)
                           xIntervall = 50
                           maxY = max(yValues, min = minY)
                           PDFValues = histogram(yValues, min = minY, max = maxY, nbins = 50, locations = xLoc)
                           PDFValues = PDFValues * (1./total(PDFValues))
                           PDFValuesMax = max(PDFValues)

                            ; find p < 0.05-Values
                           pVal = 0.025
                           sortValues = yValues[sort(yValues)]
                           szYValues = n_elements(yValues)
                           pLimit = round(szYValues * pVal)
                           pLimitSortMin = sortValues[pLimit]
                           pLimitSortMax = sortValues[szYValues-1-pLimit]

                           openW,2, strCompress('c:\rsi\DDA.dat', /rem)
                           printF, 2, ['r-values', 'y-values']
                           printF, 2, [transpose(xValues), transpose(yValues)]
                           close, 2

                           openW,2, strCompress('c:\rsi\DDAMeanSD.dat', /rem)
                           printF, 2, ['r-value', 'mean-values', 'SD-values']
                           printF, 2, [transpose(xValues[uniq(xValues, sort(xValues))]), transpose(yMeanValues), transpose(ySDValues)]
                           close, 2

                           openW,2, strCompress('c:\rsi\pLimits.dat', /rem)
                           printF, 2, ['p-MinMax']
                           printF, 2, transpose([pLimitSortMin, pLimitSortMax])
                           close, 2

                           print, '___________________________________________'
                           print, 'PDF-Statistics of:' +  state.paramTableUValue.paramColumnNames[row] + ' for ' + state.paramTableUValue.paramRowNames[column]
                           print, 'Lower limit for p < ' + strCompress(string(pVal)) + ' is:' + strCompress(string(pLimitSortMin))
                           print, 'Upper limit for p < ' + strCompress(string(pVal)) + ' is:' + strCompress(string(pLimitSortMax))
                           if ((yValues[0] le pLimitSortMin) or (yValues[0] ge pLimitSortMin)) then $
                              print, 'The value ' + strCompress(string(yValues[0])) + ' is statistically different at the p < ' + strCompress(string(pVal)) + ' level.' else $
                              print, strCompress(string(yValues[0])) + ' is NOT statistically different at the p < ' + strCompress(string(pVal)) + ' level.'
                           print, 'The p-levels for ' + strCompress(string(yValues[0])) + ' are:'
                           print, 'p > ' + strCompress(string((where(sortValues eq yValues[0]))[0]*1./szYValues))
                           print, 'p < ' + strCompress(string((szYValues - (where(sortValues eq yValues[0]))[0]*1.)/szYValues))
                           print, '___________________________________________'

                           pValLineVal = [.75,0.]*PDFValuesMax
                           pValXLineVal = [yValues[0],yValues[0]]
                           pValLineDown = [.5,0.]*PDFValuesMax
                           pValXLineDown = [pLimitSortMin,pLimitSortMin]
                           pValLineUp = [.5,0.]*PDFValuesMax
                           pValXLineUp = [pLimitSortMax,pLimitSortMax]

                           style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type :1, symbol_size :.01},$
                                                      visualization_properties = {color :'White'},$
                                                      xAxis_properties = {axisTitle:state.paramTableUValue.paramColumnNames[row], exact:1},$
                                                      yAxis_properties = {axisTitle:'PDF', exact:1},$
                                                      legend_properties = {hide:1})

                           live_plot, PDFValues, independent = xLoc,$
                                                          /histogram, draw_dimension = [400,300], /no_select,$
                                                          xRange = [min(xLoc), max(xLoc)],$
                                                          style = style,$
                                                          reference_out = refOut,$
                                                          title = 's_Coloc |R-PDF-> ' +  state.paramTableUValue.paramColumnNames[row] + ' | ' + state.paramTableUValue.paramRowNames[column]

                           if (n_elements(refOut) gt 0) then begin
                             live_oplot, pValLineUp, independent = pValXLineUp,$
                                            subType = 'LinePlot',$
                                            reference_out = refOut,$
                                            window_in = refOut.Win, /no_select
                             live_info, refOut.graphic[1], properties = variable, window_in = refOut.Win
                             variable.color = 'Red'
                             live_control, refOut.graphic[1], properties = variable, window_in = refOut.Win
                             live_oplot, pValLineDown, independent = pValXLineDown,$
                                            subType = 'LinePlot',$
                                            reference_out = refOut,$
                                            window_in = refOut.Win, /no_select
                             live_info, refOut.graphic[2], properties = variable, window_in = refOut.Win
                             variable.color = 'Red'
                             live_control, refOut.graphic[2], properties = variable, window_in = refOut.Win
                             live_oplot, pValLineVal, independent = pValXLineVal,$
                                            subType = 'LinePlot',$
                                            reference_out = refOut,$
                                            window_in = refOut.Win, /no_select
                             live_info, refOut.graphic[3], properties = variable, window_in = refOut.Win
                             variable.color = 'Green'
                             live_control, refOut.graphic[3], properties = variable, window_in = refOut.Win
                             live_oplot, [pValLineVal[0], pValLineVal[0]], independent = [pValXLineVal[0],pValXLineVal[0]],$
                                            subType = 'ScatterPlot',$
                                            reference_out = refOut,$
                                            window_in = refOut.Win, /no_select
                             live_info, refOut.graphic[4], properties = variable, window_in = refOut.Win
                             variable.color = 'Green'
                             variable.symbol_type = 6
                             variable.symbol_size = 0.02
                             live_control, refOut.graphic[4], properties = variable, window_in = refOut.Win
                           endif
                        endfor
                    endif
                   widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                 endif
              widget_control, ev.top, set_uValue = state, /no_copy
            endcase
           'SHOWPDFPLOT':begin
              widget_control, ev.top, get_uValue = state, /no_copy
                 if ((state.tableEv.SEL_LEFT ge 0) and (state.tableEv.SEL_RIGHT ge 0)) then begin
                    widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                    if (n_elements(stateOM) ne 0) then begin
                       dimRP = size(state.paramTableUValue.colocRandomParams, /dim)
                       for row = state.tableEv.SEL_LEFT, state.tableEv.SEL_RIGHT do for column = state.tableEv.SEL_TOP, state.tableEv.SEL_BOTTOM do begin

                           yValues = state.paramTableUValue.colocRandomParams[*, row, column]
                           yNoShiftVal = state.paramTableUValue.param[row, column]
                           maxY = max(yValues, min = minY)

                            ; plot s_Coloc PDF (Probability Density Function)
                           xIntervall = 50
                           PDFValues = histogram(yValues, min = minY, max = maxY, nbins = 50, locations = xLoc)
                           PDFValues = PDFValues * (1./total(PDFValues))
                           PDFValuesMax = max(PDFValues)

                            ; find p < 0.05-Values
                           pVal = 0.025
                           sortValues = yValues[sort(yValues)]
                           szYValues = n_elements(yValues)
                           pLimit = floor(szYValues * pVal)
                           pLimitSortMin = sortValues[pLimit]
                           pLimitSortMax = sortValues[szYValues-1-pLimit]

                           openW,2, strCompress('c:\rsi\PDF.dat', /rem)
                           printF, 2, ['y-values']
                           printF, 2, [transpose(yValues)]
                           close, 2

                           openW,2, strCompress('c:\rsi\PDFpLimits.dat', /rem)
                           printF, 2, ['NoShiftVal','p-Min','p-Max']
                           printF, 2, [yNoShiftVal, pLimitSortMin, pLimitSortMax]
                           close, 2

                           print, '___________________________________________'
                           print, 'PDF-Statistics of:' +  state.paramTableUValue.paramColumnNames[row] + ' for ' + state.paramTableUValue.paramRowNames[column]
                           print, 'Lower limit for p < ' + strCompress(string(pVal)) + ' is:' + strCompress(string(pLimitSortMin))
                           print, 'Upper limit for p < ' + strCompress(string(pVal)) + ' is:' + strCompress(string(pLimitSortMax))
                           if ((yNoShiftVal le pLimitSortMin) or (yNoShiftVal ge pLimitSortMin)) then $
                              print, 'The value ' + strCompress(string(yNoShiftVal)) + ' is statistically different at the p < ' + strCompress(string(pVal)) + ' level.' else $
                              print, strCompress(string(yNoShiftVal)) + ' is NOT statistically different at the p < ' + strCompress(string(pVal)) + ' level.'
                           print, 'The p-levels for ' + strCompress(string(yNoShiftVal)) + ' are:'
                           print, 'p > ' + strCompress(string((where(sortValues eq yNoShiftVal))[0]*1./szYValues))
                           print, 'p < ' + strCompress(string((szYValues - (where(sortValues eq yNoShiftVal))[0]*1.)/szYValues))
                           print, '___________________________________________'

                           pValLineVal = [.75,0.]*PDFValuesMax
                           pValXLineVal = [yNoShiftVal,yNoShiftVal]
                           pValLineDown = [.5,0.]*PDFValuesMax
                           pValXLineDown = [pLimitSortMin,pLimitSortMin]
                           pValLineUp = [.5,0.]*PDFValuesMax
                           pValXLineUp = [pLimitSortMax,pLimitSortMax]

                           style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type :1, symbol_size :.01},$
                                                       visualization_properties = {color :'White'},$
                                                       xAxis_properties = {axisTitle:state.paramTableUValue.paramColumnNames[row], exact:1},$
                                                       yAxis_properties = {axisTitle:'PDF', exact:1},$
                                                       legend_properties = {hide:1})

                           live_plot, PDFValues, independent = xLoc,$
                                                       /histogram, draw_dimension = [400,300], /no_select,$
                                                       xRange = [min(xLoc), max(xLoc)],$
                                                       style = style,$
                                                       reference_out = refOut,$
                                                       title = 's_Coloc |PDF-> ' +  state.paramTableUValue.paramColumnNames[row] + ' | ' + state.paramTableUValue.paramRowNames[column]

                           if (n_elements(refOut) gt 0) then begin
                             live_oplot, pValLineUp, independent = pValXLineUp,$
                                            subType = 'LinePlot',$
                                            reference_out = refOut,$
                                            window_in = refOut.Win, /no_select
                             live_info, refOut.graphic[1], properties = variable, window_in = refOut.Win
                             variable.color = 'Red'
                             live_control, refOut.graphic[1], properties = variable, window_in = refOut.Win
                             live_oplot, pValLineDown, independent = pValXLineDown,$
                                            subType = 'LinePlot',$
                                            reference_out = refOut,$
                                            window_in = refOut.Win, /no_select
                             live_info, refOut.graphic[2], properties = variable, window_in = refOut.Win
                             variable.color = 'Red'
                             live_control, refOut.graphic[2], properties = variable, window_in = refOut.Win
                             live_oplot, pValLineVal, independent = pValXLineVal,$
                                            subType = 'LinePlot',$
                                            reference_out = refOut,$
                                            window_in = refOut.Win, /no_select
                             live_info, refOut.graphic[3], properties = variable, window_in = refOut.Win
                             variable.color = 'Green'
                             live_control, refOut.graphic[3], properties = variable, window_in = refOut.Win
                             live_oplot, [pValLineVal[0], pValLineVal[0]], independent = [pValXLineVal[0],pValXLineVal[0]],$
                                            subType = 'ScatterPlot',$
                                            reference_out = refOut,$
                                            window_in = refOut.Win, /no_select
                             live_info, refOut.graphic[4], properties = variable, window_in = refOut.Win
                             variable.color = 'Green'
                             variable.symbol_type = 6
                             variable.symbol_size = 0.02
                             live_control, refOut.graphic[4], properties = variable, window_in = refOut.Win
                           endif
                        endfor
                    endif
                   widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                 endif
              widget_control, ev.top, set_uValue = state, /no_copy
            endcase
           'APPLYCHANGES':begin
              widget_control, ev.top, get_uValue = state, /no_copy
              case state.paramTableUValue.name of
                 'Colocalization Background Correction':begin
                           groupLeader = state.groupLeader
                           widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                             widget_control, state.wTableID, get_value = paramValues
                             stateOM.backgroudValues = paramValues
                           widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           s_channel_backgroudCorrection, groupLeader
                 endcase
                 'ROI-Mask':begin
                           groupLeader = state.groupLeader
                           widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                             widget_control, state.wTableID, get_value = paramValues
                             stateOM.maskSpecifierValue[state.paramTableUValue.uNum, *] = paramValues
                           widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           s_Coloc_update, groupLeader
                 endcase
                 else:
              endcase
         endcase
         else:
    endcase
end


pro s_ObjParamTableWidgetButton_Event, ev
    if ((where(tag_names(ev) eq 'TYPE'))[0] ne -1) and (((where(tag_names(ev) eq 'SEL_BOTTOM'))[0] ne -1)) then begin
       if (((ev.type eq 4) or (ev.type eq 3)) and (ev.sel_bottom ne -1)) then begin
         widget_control, ev.top, get_uValue = state, /no_copy
          widget_control, ev.id, get_uValue = uVal, /no_copy
              case state.paramTableUValue.name of
                 'Colocalization Background Correction':
                 'Random Colocalization Radius-Means':begin
                   state.tableEv.SEL_LEFT = ev.SEL_LEFT
                   state.tableEv.SEL_TOP = ev.SEL_TOP
                   state.tableEv.SEL_RIGHT = ev.SEL_RIGHT
                   state.tableEv.SEL_BOTTOM = ev.SEL_BOTTOM
                  endcase
                 'Random Colocalization PDF-Statistics':begin
                   state.tableEv.SEL_LEFT = ev.SEL_LEFT
                   state.tableEv.SEL_TOP = ev.SEL_TOP
                   state.tableEv.SEL_RIGHT = ev.SEL_RIGHT
                   state.tableEv.SEL_BOTTOM = ev.SEL_BOTTOM
                  endcase
                 'Colocalization Random and Radius Values':
                 'Random Colocalization Means':
                 'Random Colocalization Standard Deviation':
                 'Colocalization':
                 'ROI-Mask':
                 'RankOrder':
                 else:uVal = state.paramTableUValue.paramNames[ev.sel_bottom]
              endcase
          widget_control, ev.id, set_uValue = uVal, /no_copy
         widget_control, ev.top, set_uValue = state, /no_copy
       endif
    end

    if ((where(tag_names(ev) eq 'CH'))[0] ne -1) then begin
       case (ev.ch) of
          13:begin
              widget_control, ev.top, get_uValue = state, /no_copy
              groupLeader = state.groupLeader
              case state.paramTableUValue.name of
                   'Colocalization Random and Radius Values':begin
                           if (widget_info(state.paramTableUValue.groupLeader, /valid)) then begin
                             widget_control, state.paramTableUValue.groupLeader, get_uValue = stateOM, /no_copy
                              if (n_elements(stateOM) ne 0) then begin
                              widget_control, state.wTableID, get_value = paramValues
                                  paramValues = paramValues > 1
                                  stateOM.randomValue = paramValues[0]
                                  stateOM.radiusValue = paramValues[1]
                              widget_control, state.wTableID, set_value = paramValues
                              widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
                             endif else widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           endif
                   endcase
                   'Colocalization Background Correction':begin
                           if (widget_info(state.paramTableUValue.groupLeader, /valid)) then begin
                             widget_control, state.paramTableUValue.groupLeader, get_uValue = stateOM, /no_copy
                              if (n_elements(stateOM) ne 0) then begin
                              widget_control, state.wTableID, get_value = paramValues
                                  stateOM.backgroudValues = paramValues
                              widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
                             endif else widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           endif
                   endcase
                   'ROI-Mask':begin
                           if (widget_info(state.paramTableUValue.groupLeader, /valid)) then begin
                             widget_control, state.paramTableUValue.groupLeader, get_uValue = stateOM, /no_copy
                              if (n_elements(stateOM) ne 0) then begin
                              widget_control, state.wTableID, get_value = paramValues
                                  stateOM.maskSpecifierValue[state.paramTableUValue.uNum, *] = paramValues
                              widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
                             endif else widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
                           widget_control, ev.top, set_uValue = state, /no_copy
                           endif
                   endcase
                   'oROI2DGroup':begin
                             widget_control, state.wTableID, get_value = paramStruct
                             widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                               (*stateOM.poCurrROI2DGroup)->setParamAsStruct, paramStruct
                             widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                             widget_control, ev.top, set_uValue = state, /no_copy
                             s_ROIOM_UpdateWidgets, groupLeader
                   endcase
                   'oROI3DGroup':begin
                             widget_control, state.wTableID, get_value = paramStruct
                             widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                             (*stateOM.poCurrROI3DGroup)->setParamAsStruct, paramStruct
                             stateOM.fUpDateROI3DGroup = 1b
                             stateOM.fUpDateROI3DGroupProperties = 1b
                             pParamStruct = (*stateOM.poCurrROI3DGroup)->getpParamStruct()

                             if widget_info(state.child_ChannelBlend_tlb, /valid_id) then begin
                                pVolState = (*stateOM.poCurrROI3DGroup)->getpVolState()
                                widget_control, state.child_ChannelBlend_tlb, get_uValue = stateBlend, /no_copy
                                   (*pVolState).opacFlag[0] = stateBlend.opacFlag[0]
                                   (*pVolState).colTbl[0] = stateBlend.colTbl[0]
                                   (*pVolState).rgbValues[0,*,*] = stateBlend.rgbValues[0,*,*]
                                   (*pVolState).gamma[0] = stateBlend.gamma[0]
                                   (*pVolState).opacGamma[0] = stateBlend.opacGamma[0]
                                   (*pVolState).stretchBotTop[0] = stateBlend.stretchBotTop[0]
                                   (*pVolState).opacStretchBotTop[0] = stateBlend.opacStretchBotTop[0]
                                   (*pVolState).blendValues[0] = stateBlend.blendValues[0]
                                   (*pVolState).opacValues[0,*] = stateBlend.opacValues[0,*]
                                   (*pVolState).bottomValues[0] = stateBlend.bottomValues[0]
                                   (*pVolState).topValues[0] = stateBlend.topValues[0]
                                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq '1st Volume Color'))[0]] = stateBlend.coltbl[0]
                                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq '1st Volume Opacity'))[0]] = stateBlend.blendValues[0]

                                   if (*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq '2nd Volume Object'))[0]] ne 'NO SELECTION') and $
                                      (n_elements(stateBlend.coltbl) ge 2) then begin
                                      *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq '2nd Volume Color'))[0]] = stateBlend.coltbl[1]
                                      *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq '2nd Volume Opacity'))[0]] = stateBlend.blendValues[1]
                                      (*pVolState).opacFlag[1] = stateBlend.opacFlag[1]
                                      (*pVolState).colTbl[1] = stateBlend.colTbl[1]
                                      (*pVolState).rgbValues[1,*,*] = stateBlend.rgbValues[1,*,*]
                                      (*pVolState).gamma[1] = stateBlend.gamma[1]
                                      (*pVolState).opacGamma[1,*] = stateBlend.opacGamma[1,*]
                                      (*pVolState).stretchBotTop[1] = stateBlend.stretchBotTop[1]
                                      (*pVolState).opacStretchBotTop[1] = stateBlend.opacStretchBotTop[1]
                                      (*pVolState).blendValues[1] = stateBlend.blendValues[1]
                                      (*pVolState).opacValues[1,*] = stateBlend.opacValues[1,*]
                                      (*pVolState).bottomValues[1] = stateBlend.bottomValues[1]
                                      (*pVolState).topValues[1] = stateBlend.topValues[1]
                                   endif
                                widget_control, state.child_ChannelBlend_tlb, set_uValue = stateBlend, /no_copy
                             endif

                             widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                             widget_control, ev.top, set_uValue = state, /no_copy
                             s_ROIOM_UpdateWidgets, groupLeader

                             widget_control, ev.top, get_uValue = state, /no_copy
                                widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                                   paramStruct = (*stateOM.poCurrROI3DGroup)->getParamAsStruct()
                                widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                                widget_control, state.wTableID, set_value = paramStruct, /no_copy
                             widget_control, ev.top, set_uValue = state, /no_copy
                        endcase
                   else:widget_control, ev.top, set_uValue = state, /no_copy
                endcase
          endcase
             else:
       endcase
    endif
end


pro s_Visualization_Event, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       widget_control, state.wTableID, get_uValue = uVal, /no_copy

;         if (uVal eq '1st Volume Object') then begin
              state.fChannelBlend = s_ToggleButtonOnOffState(state.wChannelBlendOnOff)
              if (state.fChannelBlend eq 1) then begin
                 widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                   s_ISM_getProjectInfo, stack_tlb = stateOM.stack_tlb, zPos = zPos
                   image1 = (*stateOM.poCurrROI3DGroup)->getGroupMaskIntensity(zPos = zPos)
                   pVolState = (*stateOM.poCurrROI3DGroup)->getpVolState()
                 widget_control, state.groupLeader, set_uValue = stateOM, /no_copy

                 whereName = (where( state.paramTableUValue.paramNames eq '2nd Volume Object'))[0]
                 if (whereName ne -1) then begin
                   widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                      stack_tlb = stateOM.stack_tlb
                      pParamStruct = (*stateOM.poCurrROI3DGroup)->getpParamStruct()
                   widget_control, state.groupLeader, set_uValue = stateOM, /no_copy
                   strSelect = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq '2nd Volume Object'))[0]]
                   case 1 of
                    ( (strPos(strSelect, 'Channel') ne -1) or (strPos(strSelect, 'channel') ne -1) ) :begin
                             strNum = s_getRightNumberFromString(strSelect)
                             tPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Position'))[0]]
                             clusPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Cluster Position'))[0]]
                             oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = strNum, clusPos = clusPos, fileName = fileName)
                           endcase
                    ( (strPos(strSelect, 'Time') ne -1) or (strPos(strSelect, 'time') ne -1) ) :begin
                             strNum = s_getRightNumberFromString(strSelect)
                             chPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Position'))[0]]
                             clusPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Cluster Position'))[0]]
                             oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = strNum, chPos = chPos, clusPos = clusPos, fileName = fileName)
                           endcase
                    ( (strPos(strSelect, 'Cluster') ne -1) or (strPos(strSelect, 'cluster') ne -1) ) :begin
                             strNum = s_getRightNumberFromString(strSelect)
                             tPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Position'))[0]]
                             chPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Position'))[0]]
                             oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, clusPos = strNum, fileName = fileName)
                           endcase
                    else:
                   endcase

                   if obj_valid(oROI3DGroup) then begin
                      image2 = oROI3DGroup->getGroupMaskIntensity(zPos = zPos)
                      szI = size(image1, /dim)
                      images = bytArr(2, szI[0], szI[1])
                      images[0,*,*] = image1
                      images[1,*,*] = image2
                      widget_control, state.wTableID, set_uValue = uVal, /no_copy
                          widget_control, ev.top, set_uValue = state, /no_copy
                             s_Coloc_Window, images, application_tlb = application_tlb, groupLeader = ev.top, pPreState = pVolState, stack_tlb = stack_tlb
                          widget_control, ev.top, get_uValue = state, /no_copy
                      widget_control, state.wTableID, get_uValue = uVal, /no_copy
                      state.child_ChannelBlend_tlb = application_tlb
                   endif else begin
                      szI = size(image1, /dim)
                      images = bytArr(1, szI[0], szI[1]) + image1
                      widget_control, state.wTableID, set_uValue = uVal, /no_copy
                          widget_control, ev.top, set_uValue = state, /no_copy
                          s_Coloc_Window, images, application_tlb = application_tlb, groupLeader = ev.top, pPreState = pVolState, stack_tlb = stack_tlb
                          widget_control, ev.top, get_uValue = state, /no_copy
                      widget_control, state.wTableID, get_uValue = uVal, /no_copy
                      state.child_ChannelBlend_tlb = application_tlb
                   endelse

                 endif
              endif else begin
                 if widget_info(state.child_ChannelBlend_tlb, /valid_id) then begin
                    widget_control, state.child_ChannelBlend_tlb, /destroy
                    state.child_ChannelBlend_tlb = -1l
                 endif
              endelse
;           endif
       widget_control, state.wTableID, set_uValue = uVal, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_OPTW_Resize_Event, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      case state.paramTableUValue.name of
         'Colocalization Background Correction':
         'RankOrder':
         else: if (tag_names(ev, /structure_name) eq 'WIDGET_BASE') then widget_control, state.wTableID, scr_xsize = ev.x, column_width = ev.x-120, scr_ysize = ev.y-20
      endcase
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_OPTW_cleanUp, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       case state.paramTableUValue.name of
         'ROI-Mask':begin
          if (widget_info(state.paramTableUValue.groupLeader, /valid)) then begin
              widget_control, state.paramTableUValue.groupLeader, get_uValue = stateOM, /no_copy
               if (n_elements(stateOM) ne 0) then begin
                 if (stateOM.fROIMasks[state.paramTableUValue.uNum, 5]) then stateOM.fROIMasks[state.paramTableUValue.uNum, 5] = s_ToggleButtonOnOffState(stateOM.wROIMasksOnOff[state.paramTableUValue.uNum, 5])
                 widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
              endif
          endif
         endcase
         'Colocalization':begin
          if (widget_info(state.paramTableUValue.groupLeader, /valid)) then begin
              widget_control, state.paramTableUValue.groupLeader, get_uValue = stateOM, /no_copy
               if (n_elements(stateOM) ne 0) then begin
                 if (stateOM.fViewDataTable) then stateOM.fViewDataTable = s_ToggleButtonOnOffState(stateOM.wViewDataTableOnOff)
                 widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
              endif
          endif
         endcase
         'Colocalization Random and Radius Values':begin
          if (widget_info(state.paramTableUValue.groupLeader, /valid)) then begin
              widget_control, state.paramTableUValue.groupLeader, get_uValue = stateOM, /no_copy
               if (n_elements(stateOM) ne 0) then begin
                 if (stateOM.fRandomAndRadius) then stateOM.fRandomAndRadius = s_ToggleButtonOnOffState(stateOM.wRandomAndRadiusOnOff)
                 widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
              endif
          endif
         endcase
         'Colocalization Background Correction':begin
          if (widget_info(state.paramTableUValue.groupLeader, /valid)) then begin
              widget_control, state.paramTableUValue.groupLeader, get_uValue = stateOM, /no_copy
               if (n_elements(stateOM) ne 0) then begin
                 if (stateOM.fBackGroudCorrection) then stateOM.fBackGroudCorrection = s_ToggleButtonOnOffState(stateOM.wBackGroudCorrectionOnOff)
                 widget_control, state.paramTableUValue.groupLeader, set_uValue = stateOM, /no_copy
              endif
          endif
         endcase
         else:
       endcase
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_OPTW_update, paramTableUValue = paramTableUValue
    widget_control, paramTableUValue.wTopBase, get_uValue = state, /no_copy

       case state.paramTableUValue.name of
         'Stack':widget_control, state.wTableID, set_value = paramTableUValue.paramAsStruct, /no_copy
         'Image':widget_control, state.wTableID, set_value = paramTableUValue.paramAsStruct, /no_copy
         'oROI2DGroup':widget_control, state.wTableID, set_value = paramTableUValue.paramAsStruct, /no_copy
         'Colocalization':widget_control, state.wTableID, set_value = paramTableUValue.param, /no_copy
         'Colocalization Background Correction':widget_control, state.wTableID, set_value = paramTableUValue.param, /no_copy
         'ROI-Mask':widget_control, state.wTableID, set_value = paramTableUValue.param, /no_copy
         'RankOrder':widget_control, state.wTableID, set_value = paramTableUValue.param, /no_copy
         'oROI3DGroup':begin
               widget_control, state.wTableID, set_value = paramTableUValue.paramAsStruct, /no_copy
              if widget_info(state.child_ChannelBlend_tlb, /valid) then begin

                 widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                    s_ISM_getProjectInfo, stack_tlb = stateOM.stack_tlb, zPos = zPos
                    image1 = (*stateOM.poCurrROI3DGroup)->getGroupMaskIntensity(zPos = zPos)
                 widget_control, state.groupLeader, set_uValue = stateOM, /no_copy

                 whereName = (where( state.paramTableUValue.paramNames eq '2nd Volume Object'))[0]
                 if (whereName ne -1) then begin
                   widget_control, state.groupLeader, get_uValue = stateOM, /no_copy
                     pParamStruct = (*stateOM.poCurrROI3DGroup)->getpParamStruct()
                    strSelect = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq '2nd Volume Object'))[0]]
                    case 1 of
                        ( (strPos(strSelect, 'Channel') ne -1) or (strPos(strSelect, 'channel') ne -1) ) :begin
                              strNum = s_getRightNumberFromString(strSelect)
                              tPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Position'))[0]]
                              clusPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Cluster Position'))[0]]
                              oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stateOM.stack_tlb, tPos = tPos, chPos = strNum, clusPos = clusPos, fileName = fileName)
                             endcase
                        ( (strPos(strSelect, 'Time') ne -1) or (strPos(strSelect, 'time') ne -1) ) :begin
                              strNum = s_getRightNumberFromString(strSelect)
                              chPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Position'))[0]]
                              clusPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Cluster Position'))[0]]
                              oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stateOM.stack_tlb, tPos = strNum, chPos = chPos, clusPos = clusPos, fileName = fileName)
                             endcase
                        ( (strPos(strSelect, 'Cluster') ne -1) or (strPos(strSelect, 'cluster') ne -1) ) :begin
                              strNum = s_getRightNumberFromString(strSelect)
                              tPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Time Position'))[0]]
                              chPos = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Channel Position'))[0]]
                              oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stateOM.stack_tlb, tPos = tPos, chPos = chPos, clusPos = strNum, fileName = fileName)
                             endcase
                        else:
                    endcase
                   widget_control, state.groupLeader, set_uValue = stateOM, /no_copy

                   if obj_valid(oROI3DGroup) then begin
                      image2 = oROI3DGroup->getGroupMaskIntensity(zPos = zPos)
                      szI = size(image1, /dim)
                      images = bytArr(2, szI[0], szI[1])
                      images[0,*,*] = image1
                      images[1,*,*] = image2
                      s_Coloc_update, state.child_ChannelBlend_tlb, newImages = images
                   endif else begin
                      szI = size(image1, /dim)
                      images = bytArr(1, szI[0], szI[1]) + image1
                      s_Coloc_update, state.child_ChannelBlend_tlb, newImages = images
                   endelse
                 endif
              endif
          endcase
          else:
         endcase
    widget_control, paramTableUValue.wTopBase, set_uValue = state, /no_copy
end


pro s_ObjParamTableWidget, paramTableUValue = paramTableUValue

    wChannelBlendOnOff = -1
    case paramTableUValue.name of
       'ROI-Mask':begin
                 title = paramTableUValue.name + ':Ch_' + strCompress(string(paramTableUValue.uNum)) + ' |-> Parameters'
                 wTopBase = widget_base(title = title, /column, tlb_frame_attr = 1, tlb_size_events = 1, group_leader = paramTableUValue.groupLeader, MBar = menuBase)
                 void = widget_button(wTopBase, value = 'Apply changes', uName = 'APPLYCHANGES', event_pro = 's_ObjParamTableWidget_Event')
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramRowNames,$
                                                            column_labels = ['Image Identifier' ],$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 100,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'Random Colocalization Radius-Means':begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Mean-Parameters', /column, tlb_size_events = 1, MBar = menuBase)
                 void = widget_button(wTopBase, value = 'Show Radius Plot', uName = 'SHOWRADIUSPLOT', event_pro = 's_ObjParamTableWidget_Event')
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramRowNames,$
                                                            column_labels = paramTableUValue.paramColumnNames,$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 95,$
                                                            /resizeable_columns,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'Random Colocalization PDF-Statistics':begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> No-Shift-Parameters', /column, tlb_size_events = 1, MBar = menuBase)
                 void = widget_button(wTopBase, value = 'Show PDF Plot', uName = 'SHOWPDFPLOT', event_pro = 's_ObjParamTableWidget_Event')
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramRowNames,$
                                                            column_labels = paramTableUValue.paramColumnNames,$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 95,$
                                                            /resizeable_columns,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'Random Colocalization Values': begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters', /column, tlb_size_events = 1, MBar = menuBase)
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramRowNames,$
                                                            column_labels = paramTableUValue.paramColumnNames,$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 95,$
                                                            /resizeable_columns,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'Random Colocalization Standard Deviation': begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters', /column, tlb_size_events = 1, MBar = menuBase)
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramRowNames,$
                                                            column_labels = paramTableUValue.paramColumnNames,$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 95,$
                                                            /resizeable_columns,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'Colocalization Random and Radius Values':begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters', /column, tlb_size_events = 1, MBar = menuBase)
                 wTableID = widget_table(wTopBase, row_labels = ['Values' ],$
                                                            column_labels = paramTableUValue.paramColumnNames,$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 120,$
                                                            /resizeable_columns,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'Random Colocalization Means':begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters', /column, tlb_size_events = 1, MBar = menuBase)
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramRowNames,$
                                                            column_labels = paramTableUValue.paramColumnNames,$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 95,$
                                                            /resizeable_columns,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'Colocalization':begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters', /column, tlb_size_events = 1, group_leader = paramTableUValue.groupLeader, MBar = menuBase)
                 void = widget_button(wTopBase, value = 'Return and accept changes', uName = 'RETURNANDACCEPTCHANGES', event_pro = 's_ObjParamTableWidget_Event')
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramRowNames,$
                                                            column_labels = paramTableUValue.paramColumnNames,$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 95,$
                                                            /resizeable_columns,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'Colocalization Background Correction':begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters', /column, tlb_frame_attr = 1, tlb_size_events = 1, group_leader = paramTableUValue.groupLeader, MBar = menuBase)
                 void = widget_button(wTopBase, value = 'Apply changes', uName = 'APPLYCHANGES', event_pro = 's_ObjParamTableWidget_Event')
                 wTableID = widget_table(wTopBase, column_labels = paramTableUValue.paramColumnNames,$
                                                            row_labels = ['Background Values' ],$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 100,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       'RankOrder':begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters', /column, tlb_size_events = 1, MBar = menuBase)
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramRowNames,$
                                                            column_labels = paramTableUValue.paramColumnNames,$
                                                            value = paramTableUValue.param,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_width = 95,$
                                                            /resizeable_columns,$
                                                            uValue = paramTableUValue.name,$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
       else:begin
                 wTopBase = widget_base(title = paramTableUValue.name + ' |-> Parameters', /column, tlb_size_events = 1, group_leader = paramTableUValue.groupLeader, MBar = menuBase)
                 if ((paramTableUValue.name eq 'oROI2DGroup') or (paramTableUValue.name eq 'oROI3DGroup')) then $
                    wChannelBlendOnOff = widget_button(wTopBase, value = 'Visualization (off)', uName = 'VISUALIZATION', event_pro = 's_Visualization_Event')
                 void = widget_button(wTopBase, value = 'Return and accept changes', uName = 'RETURNANDACCEPTCHANGES', event_pro = 's_ObjParamTableWidget_Event')
                 wTableID = widget_table(wTopBase, row_labels = paramTableUValue.paramNames,$
                                                            event_pro = 's_ObjParamTableWidgetButton_Event',$
                                                            column_labels = ['Parameter Values' ],$
                                                            column_width = StrLen('Parameter Values')*7,$
                                                            /resizeable_columns,$
                                                            /column_major,$
                                                            value = paramTableUValue.paramAsStruct,$
                                                            uValue = paramTableUValue.paramNames[0],$
                                                            /all_events, /editable, /frame, /no_copy)
       endcase
    endcase

    paramTableUValue.wTopBase = wTopBase

    state = {wTopBase:wTopBase,$
                 groupLeader:paramTableUValue.groupLeader,$
                 wTableID:wTableID,$
                 tableEv:{SEL_LEFT:-1l, SEL_TOP:-1l, SEL_RIGHT:-1l, SEL_BOTTOM:-1l},$
                 wChannelBlendOnOff:wChannelBlendOnOff,$
                 fChannelBlend:0b,$
                 child_ChannelBlend_tlb:0l,$
                 paramTableUValue:paramTableUValue}

    widget_control, wTopBase, /realize
    widget_control, wTopBase, set_uValue = state, /no_copy
    XManager, 's_OPTW_Resize', wTopBase, CleanUp = 's_OPTW_cleanUp', /no_Block, group_leader = paramTableUValue.groupLeader

    case paramTableUValue.name of
       'Random Colocalization Values' :XManager, 's_OPTW_Resize', wTopBase, /no_Block
       else:XManager, 's_OPTW_Resize', wTopBase, CleanUp = 's_OPTW_cleanUp', /no_Block, group_leader = paramTableUValue.groupLeader
    endcase
end
