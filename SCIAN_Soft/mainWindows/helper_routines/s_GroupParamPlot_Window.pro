;_____________________________IOISIOI____________________
; NAME:
;       s_GroupParamPlot_Window
;
; PURPOSE:
;       Interactive Histogram Plot
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       s_GroupParamPlot_Window, imageData
;
;  KEYWORD PARAMETERS:
;        None
;_____________________________IOISIOI____________________

pro s_GroupParamPlot_refreshPlot, group_tlb
   widget_control, group_tlb, get_uValue = state, /no_copy
      widget_control, state.wDraw, get_value = oDrawWindow
      oDrawWindow->draw, (state.oXYGroupParamPlotView->getXYGroupViewObj())
      if state.fMap then begin
         widget_control, state.wSelectButtonBase, map = 0
         widget_control, state.wSelectButtonBase, /map
      endif else widget_control, state.wSelectButtonBase, map = 0
   widget_control, group_tlb, set_uValue = state, /no_copy
end

pro s_GroupParamPlot_Window_Resize_Event, event
   widget_control, event.top, get_uValue = state, /no_copy
      widget_control, state.wDraw, scr_xsize=event.x, scr_ysize=event.y
      widget_control, state.wDraw, get_value = oDrawWindow
      oDrawWindow->draw, (state.oXYGroupParamPlotView->getXYGroupViewObj())
   widget_control, event.top, set_uValue = state, /no_copy
   s_GroupParamPlot_refreshPlot, event.top
end


pro s_GroupParamPlot_Window_drawEvent, event
   if (event.press eq 4) then begin
      widget_control, event.top, get_uValue = state, /no_copy
      state.fMap = 1 - state.fMap
      widget_control, event.top, set_uValue = state, /no_copy
   endif
   s_GroupParamPlot_refreshPlot, event.top
end


function s_GroupParamPlot_getHistViewObj, tlb, win = win
   widget_control, tlb, get_uValue = state, /no_copy
      oView = state.oXYGroupParamPlotView->getXYGroupViewObj()
      win = state.win
   widget_control, tlb, set_uValue = state, /no_copy
   return, oView
end


pro s_GroupParamPlot_Window_selectControl, event

    widget_control, event.top, get_uValue = state, /no_copy
       selectedXYParamNames = state.selectedXYParamNames
    widget_control, event.top, set_uValue = state, /no_copy
    if (selectedXYParamNames[0] eq '-NO SELECTION-') or (selectedXYParamNames[1] eq '-NO SELECTION-') then return

    case tag_names(event, /struct) of
       'WIDGET_BUTTON':begin
          widget_control, event.id, get_uValue = uValue, /no_copy
              case uValue of
                 'MEANVALUETOLINEPLOT':begin      ; Copy Data to Live_Plot_Window
                    widget_control, event.top, get_uValue = state, /no_copy
                        oXYPlot = state.oXYGroupParamPlotView->getXYGroupPlotObject()
                        oXYPlot->getProperty, data = xyzValues
                        oXYErrorPlot = state.oXYGroupParamPlotView->getXYGroupErrorUpPlot()
                        oXYErrorPlot->getProperty, data = xyzError
                        sizeArr = size(xyzValues, /dim)
                        xname = state.selectedXYParamNames[0]
                        yname = state.selectedXYParamNames[1]
                        state.oXYGroupParamPlotView->get, pParamStruct = pParamStruct

                        if (state.fNormalizeToFirstValue and ((xyzValues[1,*])[0] ne 0)) then begin
                            xyzValues[1,*] = xyzValues[1,*] / (xyzValues[1,*])[0]
                            yRange = [min( xyzValues[1,*]), max( xyzValues[1,*])]
                        endif else yRange = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'XYGroupOMinY'))[0]], *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'XYGroupOMaxY'))[0]]]
                        live_plot, fltArr(sizeArr[1]) + xyzValues[1,*],$
                                   independent = fltArr(sizeArr[1]) + xyzValues[0,*],$
                                   reference_out = refOut,$
                                   xRange = [*(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'XYGroupOMinX'))[0]], *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'XYGroupOMaxX'))[0]]],$
                                   yRange = yRange,$
                                   draw_dimension = [400,200], /no_select

                        live_info, refOut.xaxis, properties = variable, window_in = refOut.Win
                        variable.exact = 1
                        variable.axisTitle = xname
                        live_control, refOut.xaxis, properties = variable, window_in = refOut.Win
                        live_info, refOut.yaxis, properties = variable, window_in = refOut.Win
                        variable.exact = 1
                        variable.axisTitle = yname
                        live_control, refOut.yaxis, properties = variable, window_in = refOut.Win
                        live_info, refOut.legend, properties = variable, window_in = refOut.Win
                        variable.hide = 1
                        live_control, refOut.legend, properties = variable, window_in = refOut.Win
                        live_info, refOut.vis, properties = variable, window_in = refOut.Win
                        variable.color = 'Light Gray'
                        live_control, refOut.vis, properties = variable, window_in = refOut.Win

                        if state.fSavePlotDataAsASCII then begin
                           file = pickfile(/write, path = state.path, filter = '*.dat', file = 'file Name', /noconf)
                           if (file eq '') or (file eq (state.path+'\file Name')) then begin
                              a = s_apop_shout('Not saved !')
                           endif else begin
                              state.path = strMid(file,0,(rStrPos(file,'\'))+1)
                              if obj_valid(state.oXYGroupParamPlotView) then begin
                                 openW,2, strCompress(file + strcompress(xname + '_' + yname + '_mean.dat', /remove))
                                 printF, 2, [transpose(fltArr(sizeArr[1]) + xyzValues[0,*]), transpose(fltArr(sizeArr[1]) + xyzValues[1,*])]
                                 close, 2
                              endif else a = s_apop_shout('Not saved')
                           endelse
                        endif

                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'ALLOBJECTTRACKVALUETOLINEPLOT':begin      ; Copy Object Track Data to Live_Plot_Window
                    widget_control, event.top, get_uValue = state, /no_copy

                        widget_control, state.ROIOM_tlb, get_uValue = ROIOMstate, /no_copy
                           if obj_valid(*ROIOMstate.poStackTrackContainer) then begin
                             if (strPos(state.selectedXYParamNames[1], '3D') eq -1) then $
                                oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + 'Track Objects from Masks') $
                                else oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + '3D Track Objects from Masks')
                             if obj_valid(oStackTrack) then trackParams = oStackTrack->getAllParamVect()
                             if (n_elements(trackParams) ne 0) then begin
                                whereNotMinusOne = where(trackParams ne -1)
                                if (whereNotMinusOne[0] ne -1) then trackParams = trackParams[whereNotMinusOne]
                                sortTrackParams = fix(trackParams[uniq(trackParams, sort(trackParams))])
                             endif else sortTrackParams = -1
                             oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[0] + state.selectedXYParamNames[0])
                             if obj_valid(oStackTrack) then begin
                              xParams = oStackTrack->getAllParamVect()
                              if (whereNotMinusOne[0] ne -1) then xParams = xParams[whereNotMinusOne]
                             endif
                             oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + state.selectedXYParamNames[1])
                             if obj_valid(oStackTrack) then begin
                              yParams = oStackTrack->getAllParamVect()
                              if (whereNotMinusOne[0] ne -1) then yParams = yParams[whereNotMinusOne]
                             endif
                           endif else begin
                             widget_control, state.ROIOM_tlb, set_uValue = ROIOMstate, /no_copy
                             return
                           endelse
                        widget_control, state.ROIOM_tlb, set_uValue = ROIOMstate, /no_copy

                        if state.fNormalizeToFirstValue then yRange = [1.,1.] else yRange = [min(yParams), max(yParams)]
                        if ((n_elements(trackParams) ne -1) and (n_elements(xParams) ne -1) and (n_elements(yParams) ne -1)) then begin
                           pAllParams = ptrArr(n_elements(trackParams)<25)
                           for i = 1, n_elements(sortTrackParams)<25 do begin
                             whParam = where(trackParams eq sortTrackParams[i-1])
                             if (whParam[0] ne -1) then pAllParams[i-1] = ptr_new( [transpose(xParams[whParam]), transpose(yParams[whParam])], /no_copy) $
                                else pAllParams[i-1] = ptr_new( [transpose([min(xParams),max(xParams)] ), transpose([min(yParams),max(yParams)])], /no_copy)
                             if ((state.fNormalizeToFirstValue) and (((*pAllParams[i-1])[1,*])[0] ne 0)) then begin
                                (*pAllParams[i-1])[1,*] = (*pAllParams[i-1])[1,*] / ((*pAllParams[i-1])[1,*])[0]
                                yRange[0] <= min((*pAllParams[i-1])[1,*], max = maxyRange)
                                yRange[1] >= maxyRange
                             endif
                           endfor
                        endif

                        live_plot, make_array(n_elements((*pAllParams[0])[1,*]), type = size((*pAllParams[0])[1,*], /type)) + (*pAllParams[0])[1,*],$
                                   independent = make_array(n_elements((*pAllParams[0])[0,*]), type = size((*pAllParams[0])[0,*], /type)) + (*pAllParams[0])[0,*],$
                                   reference_out = refOut,$
                                   XRange = [min(xParams),max(xParams)], YRange = yRange,$
                                   name = {i:'', data:'object '+ strCompress(string(floor((sortTrackParams[0])/10.))+string((sortTrackParams[0]) mod 10), /rem)},$
                                   draw_dimension = [400,200], /no_select

                        live_info, refOut.xaxis, properties = variable, window_in = refOut.Win
                        variable.exact = 1
                        variable.axisTitle = state.selectedXYParamNames[0]
                        live_control, refOut.xaxis, properties = variable, window_in = refOut.Win
                        live_info, refOut.yaxis, properties = variable, window_in = refOut.Win
                        variable.exact = 1
                        variable.axisTitle = state.selectedXYParamNames[1]
                        live_control, refOut.yaxis, properties = variable, window_in = refOut.Win
                        live_info, refOut.legend, properties = variable, window_in = refOut.Win
                        live_control, refOut.legend, properties = variable, window_in = refOut.Win
                        live_info, refOut.vis, properties = variable, window_in = refOut.Win
                        variable.color = 'Light Gray'
                        live_control, refOut.vis, properties = variable, window_in = refOut.Win
                        if state.fUseBlackAndGreyValues then begin
                           live_info, refOut.graphic, properties = variable, window_in = refOut.Win
                           variable.color = 'Dark Gray'
                           live_control, refOut.graphic, properties = variable, window_in = refOut.Win
                        endif

                        for i = 1, (n_elements(sortTrackParams)-1)<24 do begin
                           if (n_elements((*pAllParams[i])[1,*]) eq 1) then $
                             live_oplot, make_array(n_elements((*pAllParams[i])[1,*]), type = size((*pAllParams[i])[1,*], /type)) + (*pAllParams[i])[1,*],$
                                        independent = make_array(n_elements((*pAllParams[i])[0,*]), type = size((*pAllParams[i])[0,*], /type)) + (*pAllParams[i])[0,*],$
                                        name = {i:'', data:'object ' + strCompress(string(floor(sortTrackParams[i]/10.))+string(sortTrackParams[i] mod 10), /rem)},$
                                        reference_out = refOut,$
                                        subType = 'ScatterPlot',$
                                        window_in = refOut.Win, /no_select else $
                             live_oplot, make_array(n_elements((*pAllParams[i])[1,*]), type = size((*pAllParams[i])[1,*], /type)) + (*pAllParams[i])[1,*],$
                                        independent = make_array(n_elements((*pAllParams[i])[0,*]), type = size((*pAllParams[i])[0,*], /type)) + (*pAllParams[i])[0,*],$
                                        name = {i:'', data:'object ' + strCompress(string(floor(sortTrackParams[i]/10.))+string(sortTrackParams[i] mod 10), /rem)},$
                                        reference_out = refOut,$
                                        window_in = refOut.Win, /no_select
                           if state.fUseBlackAndGreyValues then begin
                             live_info, refOut.graphic[i], properties = variable, window_in = refOut.Win
                             variable.color = 'Dark Gray'
                             live_control, refOut.graphic[i], properties = variable, window_in = refOut.Win
                           endif
                        endfor

                        if state.fSavePlotDataAsASCII then begin
                           file = pickfile(/write, path = state.path, filter = '*.dat', file = 'file Name', /noconf)
                           if (file eq '') or (file eq (state.path+'\file Name')) then begin
                             a = s_apop_shout('Not saved !')
                           endif else begin
                             state.path = strMid(file,0,(rStrPos(file,'\'))+1)
                              if obj_valid(state.oXYGroupParamPlotView) then begin
                              for i = 1, (n_elements(sortTrackParams)-1)<24 do begin
                                  openW,2, strCompress(file + strcompress(state.selectedXYParamNames[0] + '_' + state.selectedXYParamNames[1] + $
                                                            '_Object',/rem) + strCompress(string(floor(sortTrackParams[i]/10.))+string(sortTrackParams[i] mod 10), /rem) + '.dat')
                                  printF, 2, [transpose(make_array(n_elements((*pAllParams[i])[0,*]), type = size((*pAllParams[i])[0,*], /type)) + (*pAllParams[i])[0,*]),$
                                              transpose(make_array(n_elements((*pAllParams[i])[1,*]), type = size((*pAllParams[i])[1,*], /type)) + (*pAllParams[i])[1,*])]
                                  close, 2
                              endfor
                             endif else a = s_apop_shout('Not saved')
                           endelse
                        endif

                        if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'MEANANDALLOBJECTTRACKVALUETOLINEPLOT':begin
                    widget_control, event.top, get_uValue = state, /no_copy

                        oXYPlot = state.oXYGroupParamPlotView->getXYGroupPlotObject()
                        oXYPlot->getProperty, data = xyzValues
                        oXYErrorPlot = state.oXYGroupParamPlotView->getXYGroupErrorUpPlot()
                        oXYErrorPlot->getProperty, data = xyzError
                        sizeArr = size(xyzValues, /dim)
                        state.oXYGroupParamPlotView->get, pParamStruct = pParamStruct

                        widget_control, state.ROIOM_tlb, get_uValue = ROIOMstate, /no_copy
                           if obj_valid(*ROIOMstate.poStackTrackContainer) then begin
                              if (strPos(state.selectedXYParamNames[1], '3D') eq -1) then $
                                 oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + 'Track Objects from Masks') $
                                 else  oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + '3D Track Objects from Masks')
                              if obj_valid(oStackTrack) then trackParams = oStackTrack->getAllParamVect()
                              if (n_elements(trackParams) ne -1) then begin
                                 whereNotMinusOne = where(trackParams ne -1)
                                 if (whereNotMinusOne[0] ne -1) then trackParams = trackParams[whereNotMinusOne]
                                 sortTrackParams = fix(trackParams[uniq(trackParams, sort(trackParams))])
                              endif else sortTrackParams = -1
                              oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[0] + state.selectedXYParamNames[0])
                              if obj_valid(oStackTrack) then begin
                                 xParams = oStackTrack->getAllParamVect()
                                 if (whereNotMinusOne[0] ne -1) then xParams = xParams[whereNotMinusOne]
                              endif
                              oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + state.selectedXYParamNames[1])
                              if obj_valid(oStackTrack) then begin
                                 yParams = oStackTrack->getAllParamVect()
                                 if (whereNotMinusOne[0] ne -1) then yParams = yParams[whereNotMinusOne]
                              endif
                           endif else begin
                              widget_control, state.ROIOM_tlb, set_uValue = ROIOMstate, /no_copy
                              return
                           endelse
                        widget_control, state.ROIOM_tlb, set_uValue = ROIOMstate, /no_copy

                        if state.fNormalizeToFirstValue then begin
                           yRange = [1., 1.]
                           xArray = xParams[uniq(xParams, sort(xParams))]
                           meanSum = make_array(n_elements(xArray), type = size(xArray, /type))
                           meanNum = intArr(n_elements(xArray))
                        endif else yRange = [min(yParams), max(yParams)]

                        if state.fNormalizeToFirstValue then yRange = [1., 1.] else yRange = [min(yParams), max(yParams)]
                        if ((n_elements(trackParams) ne -1) and (n_elements(xParams) ne -1) and (n_elements(yParams) ne -1)) then begin
                           pAllParams = ptrArr(n_elements(trackParams)<25)
                           for i = 1, n_elements(sortTrackParams)<25 do begin
                             whParam = where(trackParams eq sortTrackParams[i-1])
                             if (whParam[0] ne -1) then pAllParams[i-1] = ptr_new( [transpose(xParams[whParam]), transpose(yParams[whParam])], /no_copy) $
                              else pAllParams[i-1] = ptr_new( [transpose([min(xParams),max(xParams)] ), transpose([min(yParams),max(yParams)])], /no_copy)
                             if ( state.fNormalizeToFirstValue and (((*pAllParams[i-1])[1,*])[0] ne 0)) then begin
                              (*pAllParams[i-1])[1,*] = (*pAllParams[i-1])[1,*] / ((*pAllParams[i-1])[1,*])[0]
                              yRange[0] = yRange[0] < min((*pAllParams[i-1])[1,*])
                              yRange[1] = yRange[1] > max((*pAllParams[i-1])[1,*])
                             endif
                           endfor
                        endif

                        live_plot, make_array(n_elements((*pAllParams[0])[1,*]), type = size((*pAllParams[0])[1,*], /type)) + (*pAllParams[0])[1,*],$
                                   independent = make_array(n_elements((*pAllParams[0])[0,*]), type = size((*pAllParams[0])[0,*], /type)) + (*pAllParams[0])[0,*],$
                                   reference_out = refOut,$
                                   XRange = [min(xParams),max(xParams)],$
                                   YRange = yRange,$
                                   name = {i:'', data:'object ' + strCompress(string(floor(sortTrackParams[0]/10.))+string(sortTrackParams[0] mod 10), /rem)},$
                                   draw_dimension = [400,200], /no_select

                        live_info, refOut.xaxis, properties = variable, window_in = refOut.Win
                        variable.exact = 1
                        variable.axisTitle = state.selectedXYParamNames[0]
                        live_control, refOut.xaxis, properties = variable, window_in = refOut.Win
                        live_info, refOut.yaxis, properties = variable, window_in = refOut.Win
                        variable.exact = 1
                        variable.axisTitle = state.selectedXYParamNames[1]
                        live_control, refOut.yaxis, properties = variable, window_in = refOut.Win
                        live_info, refOut.legend, properties = variable, window_in = refOut.Win
                        live_control, refOut.legend, properties = variable, window_in = refOut.Win
                        live_info, refOut.vis, properties = variable, window_in = refOut.Win
                        variable.color = 'Light Gray'
                        live_control, refOut.vis, properties = variable, window_in = refOut.Win
                        if state.fUseBlackAndGreyValues then begin
                           live_info, refOut.graphic, properties = variable, window_in = refOut.Win
                           variable.color = 'Dark Gray'
                           live_control, refOut.graphic, properties = variable, window_in = refOut.Win
                        endif

                        for i = 1, (n_elements(sortTrackParams)-1)<24 do begin
                           if (n_elements((*pAllParams[i])[1,*]) eq 1) then $
                             live_oplot, make_array(n_elements((*pAllParams[i])[1,*]), type = size((*pAllParams[i])[1,*], /type)) + (*pAllParams[i])[1,*],$
                                        independent = make_array(n_elements((*pAllParams[i])[0,*]), type = size((*pAllParams[i])[0,*], /type)) + (*pAllParams[i])[0,*],$
                                        name = {i:'', data:'object ' + strCompress(string(floor(sortTrackParams[i]/10.))+string(sortTrackParams[i] mod 10), /rem)},$
                                        reference_out = refOut,$
                                        subType = 'ScatterPlot',$
                                        window_in = refOut.Win, /no_select else $
                             live_oplot, make_array(n_elements((*pAllParams[i])[1,*]), type = size((*pAllParams[i])[1,*], /type)) + (*pAllParams[i])[1,*],$
                                        independent = make_array(n_elements((*pAllParams[i])[0,*]), type = size((*pAllParams[i])[0,*], /type)) + (*pAllParams[i])[0,*],$
                                        name = {i:'', data:'object ' + strCompress(string(floor(sortTrackParams[i]/10.))+string(sortTrackParams[i] mod 10), /rem)},$
                                        reference_out = refOut,$
                                        window_in = refOut.Win, /no_select
                           if state.fUseBlackAndGreyValues then begin
                              live_info, refOut.graphic[i], properties = variable, window_in = refOut.Win
                              variable.color = 'Dark Gray'
                              live_control, refOut.graphic[i], properties = variable, window_in = refOut.Win
                           endif
                        endfor

                        if (state.fNormalizeToFirstValue and ((xyzValues[1,*])[0] ne 0)) then xyzValues[1,*] /= (xyzValues[1,*])[0]
                        live_oplot, fltArr(sizeArr[1]) + xyzValues[1,*],$
                                    independent = fltArr(sizeArr[1]) + xyzValues[0,*],$
                                    window_in = refOut.Win,$
                                    reference_out = refOut,$
                                    name = {i:'', data:'mean'}, /no_select

                        live_info, refOut.graphic[n_elements(refOut.graphic)-1], properties = variable, window_in = refOut.Win
                        variable.thick = 2
                        variable.color = 'Black'
                        live_control, refOut.graphic[n_elements(refOut.graphic)-1], properties = variable, window_in = refOut.Win

                        if (state.fSavePlotDataAsASCII) then begin
                           file = pickfile(/write, path = state.path, filter = '*.dat', file = 'file Name', /noconf)
                           if (file eq '') or (file eq (state.path+'\file Name')) then begin
                              a = s_apop_shout('Not saved !')
                           endif else begin
                              state.path = strMid(file,0,(rStrPos(file,'\'))+1)
                              if obj_valid(state.oXYGroupParamPlotView) then begin
                                 openW,2, strCompress(file + strcompress(state.selectedXYParamNames[0] + '_' + state.selectedXYParamNames[1] + '_mean.dat', /rem))
                                 printF, 2, [transpose(fltArr(sizeArr[1]) + xyzValues[0,*]), transpose(fltArr(sizeArr[1]) + xyzValues[1,*])]
                                 close, 2
                                 for i = 0, (n_elements(sortTrackParams)-1)<24 do begin
                                     openW,2, strCompress(file + strcompress(state.selectedXYParamNames[0] + '_' + state.selectedXYParamNames[1] + $
                                                          '_Object',/rem) + strCompress(string(floor(sortTrackParams[i]/10.))+string(sortTrackParams[i] mod 10), /rem) + '.dat')
                                     printF, 2, [transpose(make_array(n_elements((*pAllParams[i])[0,*]), type = size((*pAllParams[i])[0,*], /type)) + (*pAllParams[i])[0,*]),$
                                                 transpose(make_array(n_elements((*pAllParams[i])[1,*]), type = size((*pAllParams[i])[1,*], /type)) + (*pAllParams[i])[1,*])]
                                     close, 2
                                 endfor
                             endif else a = s_apop_shout('Not saved')
                           endelse
                        endif

                        if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SAVEOBJECTDATAASASCII':begin
                    widget_control, event.top, get_uValue = state, /no_copy

                        widget_control, state.ROIOM_tlb, get_uValue = ROIOMstate, /no_copy
                           if obj_valid(*ROIOMstate.poStackTrackContainer) then begin
                             if (strPos(state.selectedXYParamNames[1], '3D') eq -1) then $
                                oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + 'Track Objects from Masks') $
                                else oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + '3D Track Objects from Masks')
                             if obj_valid(oStackTrack) then trackParams = oStackTrack->getAllParamVect()
                             if (n_elements(trackParams) ne 0) then begin
                                whereNotMinusOne = where(trackParams ne -1)
                                if (whereNotMinusOne[0] ne -1) then trackParams = trackParams[whereNotMinusOne]
                                sortTrackParams = fix(trackParams[uniq(trackParams, sort(trackParams))])
                             endif else sortTrackParams = -1
                             oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[0] + state.selectedXYParamNames[0])
                             if obj_valid(oStackTrack) then begin
                              xParams = oStackTrack->getAllParamVect()
                              if (whereNotMinusOne[0] ne -1) then xParams = xParams[whereNotMinusOne]
                             endif
                             oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1] + state.selectedXYParamNames[1])
                             if obj_valid(oStackTrack) then begin
                              yParams = oStackTrack->getAllParamVect()
                              if (whereNotMinusOne[0] ne -1) then yParams = yParams[whereNotMinusOne]
                             endif
                           endif else begin
                             widget_control, state.ROIOM_tlb, set_uValue = ROIOMstate, /no_copy
                             return
                           endelse
                        widget_control, state.ROIOM_tlb, set_uValue = ROIOMstate, /no_copy

                        yRange = [1.,1.]
                        if not(state.fNormalizeToFirstValue) then begin
                           yRange[0] = min(yParams, max = maxyParams)
                           yRange[1] = maxyParams
                        endif

                        if ((n_elements(trackParams) ne -1) and (n_elements(xParams) ne -1) and (n_elements(yParams) ne -1)) then begin
                           pAllParams = ptrArr(n_elements(trackParams))
                           for i = 1, n_elements(sortTrackParams) do begin
                             whParam = where(trackParams eq sortTrackParams[i-1])
                             if (whParam[0] ne -1) then pAllParams[i-1] = ptr_new( [transpose(xParams[whParam]), transpose(yParams[whParam])], /no_copy) $
                             else begin
                                xParamsMin = min(xParams, max = xParamsMax)
                                yParamsMin = min(yParams, max = yParamsMax)
                                pAllParams[i-1] = ptr_new( [transpose([xParamsMin, xParamsMin] ), transpose([yParamsMin, yParamsMax])], /no_copy)
                             endelse
                           endfor
                        endif

                        file = pickfile(/write, path = state.path, filter = '*.dat', file = 'file Name', /noconf)
                        if (file eq '') or (file eq (state.path+'\file Name')) then begin
                           a = s_apop_shout('Not saved !')
                        endif else begin
                           state.path = strMid(file,0,(rStrPos(file,'\'))+1)
                           if obj_valid(state.oXYGroupParamPlotView) then begin
                              for i = 0, n_elements(sortTrackParams)-1 do begin
                                 objStr = strCompress(string(floor(sortTrackParams[i]/10.))+string(sortTrackParams[i] mod 10), /rem)
                                 while(strLen(objStr) lt 4) do objStr = strCompress('0' + objStr, /rem)
                                 openW,2, strCompress(file + strcompress(state.selectedXYParamNames[0] + '_' + state.selectedXYParamNames[1] + $
                                                      '_Obj_' + objStr + '.dat', /rem))
                                 printF, 2, [transpose(make_array(n_elements((*pAllParams[i])[0,*]), type = size((*pAllParams[i])[0,*], /type)) + (*pAllParams[i])[0,*]),$
                                             transpose(make_array(n_elements((*pAllParams[i])[1,*]), type = size((*pAllParams[i])[1,*], /type)) + (*pAllParams[i])[1,*])]
                                 close, 2
                              endfor
                           endif else a = s_apop_shout('Not saved')
                        endelse

                        if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'UNSBLACKANDGREYVALUESONOFF':begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fUseBlackAndGreyValues = s_ToggleButtonOnOffState(state.wUseBlackAndGreyValuesOnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'NORMALIZETOFIRSTDATAVALUEONOFF':begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fNormalizeToFirstValue = s_ToggleButtonOnOffState(state.wNormalizeToFirstValueOnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SAVEPLOTDATAASASCIIONOFF':begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fSavePlotDataAsASCII = s_ToggleButtonOnOffState(state.wSavePlotDataAsASCIIOnOff)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'KEEPSELECTEDOBJECTSONOFF':begin
                    widget_control, event.top, get_uValue = state, /no_copy
                        state.fKeepSelectedObjectsControl = s_ToggleButtonOnOffState(state.wKeepSelectedObjectsOnOffButton)
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 'SAVEPLOTDATAASASCII':begin
                    ; Parameter_Save
                    widget_control, event.top, get_uValue = state, /no_copy
                        file = pickfile(/write, path = state.path, filter = '*.dat', file = 'file Name', /noconf)
                        if (file eq '') or (file eq (state.path+'\file Name')) then begin
                           a = s_apop_shout('Not saved !')
                        endif else begin
                           state.path = strMid(file,0,(rStrPos(file,'\'))+1)
                           if obj_valid(state.oXYGroupParamPlotView) then begin
                              oXYPlot = state.oXYGroupParamPlotView->getXYGroupPlotObject()
                              oXYPlot->getProperty, data = xyzValues
                              oXYErrorPlot = state.oXYGroupParamPlotView->getXYGroupErrorUpPlot()
                              oXYErrorPlot->getProperty, data = xyzError

;                          openW,2, strCompress(file + (state.selectedXYParamNames)[0] + '_XData.dat', /remove)
;                          printF, 2, xyzValues[0,*]
;                          close, 2
;                          openW,2, strCompress(file + (state.selectedXYParamNames)[1] + '_YData.dat', /remove)
;                          printF, 2, xyzValues[1,*]
;                          close, 2
;                          openW,2, strCompress(file + (state.selectedXYParamNames)[1] + '_YError.dat', /remove)
;                          printF, 2, xyzError[1,*] - xyzValues[1,*]
;                          close, 2

                              openW,2, strCompress(file + strcompress((state.selectedXYParamNames)[0] + '_' + (state.selectedXYParamNames)[1] + '_XYError.dat', /rem))
                              printF, 2, [xyzValues[0:1,*], xyzError[1,*] - xyzValues[1,*]]
                              close, 2

                           endif else a = s_apop_shout('Not saved')
                        ;info.pXRadData
                        endelse
                    widget_control, event.top, set_uValue = state, /no_copy
                 endcase
                 else:
               endcase
          widget_control, event.id, set_uValue = uValue, /no_copy
         endcase
       '':begin
          widget_control, event.top, get_uValue = state, /no_copy
              state.oXYGroupParamPlotView->get, pParamStruct = pParamStruct
              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'HistNBins'))[0]] = event.value
              state.oXYGroupParamPlotView->calculateHistValues, histData = *state.pHistData
          widget_control, event.top, set_uValue = state, /no_copy
       endcase
       else:
    endcase

    s_GroupParamPlot_refreshPlot, event.top
end


pro s_GroupParamPlot_Window_cleanUp, group_tlb
   widget_control, group_tlb, get_uValue = state, /no_copy
      if (n_elements(state) ne 0) then begin
         if obj_valid(state.oXYGroupParamPlotView) then obj_destroy, state.oXYGroupParamPlotView
         if ptr_valid(state.pHistData) then ptr_free, state.pHistData
      endif
   if widget_info(group_tlb, /valid) then widget_control, group_tlb, /destroy
end


pro s_GroupParamPlot_Window_update, group_tlb = group_tlb, selectedXYParamNames = selectedXYParamNames, tPos = tPos,$
                                                 chPos = chPos, zPos = zPos, clusPos = clusPos

    widget_control, group_tlb, get_uValue = state, /no_copy
       (state.oXYGroupParamPlotView->getxTitleObj())->setProperty, strings = selectedXYParamNames[0]
       (state.oXYGroupParamPlotView->getyTitleObj())->setProperty, strings = selectedXYParamNames[1]

       if ((selectedXYParamNames[0] eq '-NO SELECTION-') or (selectedXYParamNames[1] eq '-NO SELECTION-')) then begin
         widget_control, group_tlb, set_uValue = state, /no_copy
         s_GroupParamPlot_refreshPlot, group_tlb
         return
       endif

         ; calculate Plot-Values
       state.selectedXYParamNames = selectedXYParamNames
       widget_control, state.ROIOM_tlb, get_uValue = ROIOMstate, /no_copy
         if obj_valid(*ROIOMstate.poStackTrackContainer) then begin
            if (strPos(selectedXYParamNames[0], '3D') ne -1) then state.strID[0] = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem) $
               else state.strID[0] = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zPos>0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem)
            oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[0]+selectedXYParamNames[0])
            if obj_valid(oStackTrack) then begin
               pValueStruct = oStackTrack->getpValueStruct()
               xData = *(*(pValueStruct)).pROIParamVect
               xError = *(*(pValueStruct)).pROIErrorVect
               if (strPos(selectedXYParamNames[1], '3D') ne -1) then state.strID[1] = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem) $
                  else state.strID[1] = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zPos>0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem)
               oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = state.strID[1]+selectedXYParamNames[1])
               if obj_valid(oStackTrack) then begin
                  pValueStruct = oStackTrack->getpValueStruct()
                  yData = *(*(pValueStruct)).pROIParamVect
                  yError = *(*(pValueStruct)).pROIErrorVect
               endif else begin
                  yData = xData
                  yError = yError
               endelse
            endif
         endif
         if (n_elements(xData) le 0) then begin
            xData = [1.,2.,3.]
            yData = [1.,2.,3.]
            xError = [.5, 5,.5]
            yError = [.5, 5,.5]
         endif

       widget_control, state.ROIOM_tlb, set_uValue = ROIOMstate, /no_copy
       state.oXYGroupParamPlotView->calculateNewGroupParamValues, xData = xData, yData = yData, xError = xError, yError = yError, tPos = tPos
    widget_control, group_tlb, set_uValue = state, /no_copy

    s_GroupParamPlot_refreshPlot, group_tlb
end


pro s_GroupParamPlot_Window, stack_tlb = stack_tlb,$
                             selectedXYParamNames = selectedXYParamNames,$
                             ROIOM_tlb = ROIOM_tlb,$
                             basePosition = basePosition,$
                             application_tlb = application_tlb,$
                             tPos = tPos,$
                             chPos = chPos,$
                             zPos = zPos,$
                             clusPos = clusPos

       ;Check the validity of the group identifier.
   if (n_elements(ROIOM_tlb) ne 0) then begin
      if (widget_info(ROIOM_tlb, /valid) ne 1) then begin
         print,'Error:the group identifier is not valid.  |--> Returning to the main application.'
         return
       endif
   endif else ROIOM_tlb = 0l

     ; Create Hist Window.
   wTopBase = widget_base(Title = 's_XYGroupParamPlot |->', xOffset = basePosition[0], yOffset = basePosition[1], group_leader = ROIOM_tlb, tlb_size_events = 1)

   wSelectButtonBase = widget_base(wTopBase, map = 0, event_pro = 's_GroupParamPlot_Window_selectControl', column = 1, /frame)
      void = widget_button(wSelectButtonBase, value = 'Open New Data Window', /menu)
         wUseBlackAndGreyValuesOnOff = widget_button(void, value = '...|-> Use Black and Grey Values (on )', uValue = 'UNSBLACKANDGREYVALUESONOFF')
         wNormalizeToFirstValueOnOff = widget_button(void, value = '...|-> Normalize to First Data Value (on )', uValue = 'NORMALIZETOFIRSTDATAVALUEONOFF')
         wSavePlotDataAsASCIIOnOff = widget_button(void, value = '...|-> Save Plot Data as ASCII (off)', uValue = 'SAVEPLOTDATAASASCIIONOFF')
         void2 = widget_button(void, value = '...||-> Mean Value to Line_Plot', uValue = 'MEANVALUETOLINEPLOT')
         void2 = widget_button(void, value = '...||-> All Object Track Values to Line_Plot', uValue = 'ALLOBJECTTRACKVALUETOLINEPLOT')
         void2 = widget_button(void, value = '...||-> Mean and All Object Track Values to Line_Plot', uValue = 'MEANANDALLOBJECTTRACKVALUETOLINEPLOT')
      void = widget_button(wSelectButtonBase, value = 'Export Data', /menu)
         void2 = widget_button(void, value = 'Save Plot Data as ASCII', uValue = 'SAVEPLOTDATAASASCII')
         void2 = widget_button(void, value = 'Save Object Data as ASCII', uValue = 'SAVEOBJECTDATAASASCII')

    wBase_1 = widget_base(wTopBase, /frame)
    wDraw = widget_draw(wBase_1, XSize = 350, YSize = 200, retain = 2, graphics_level = 2, expose_events = 1,$
                        button_events = 1, event_pro = 's_GroupParamPlot_Window_drawEvent')
    widget_control, wTopBase, /realize
    widget_control, wDraw, get_value = win

    oXYGroupParamPlotView = obj_new('C_XYGroupViewObject')
    (oXYGroupParamPlotView->getxTitleObj())->setProperty, strings = selectedXYParamNames[0]
    (oXYGroupParamPlotView->getyTitleObj())->setProperty, strings = selectedXYParamNames[1]

       ; calculate Plot-Values
    strID = selectedXYParamNames
    widget_control, ROIOM_tlb, get_uValue = ROIOMstate, /no_copy
       if (obj_valid(*ROIOMstate.poStackTrackContainer) and (selectedXYParamNames[0] ne '-NO SELECTION-') and (selectedXYParamNames[1] ne '-NO SELECTION-')) then begin
          if (strPos(selectedXYParamNames[0], '3D') ne -1) then (strID)[0] = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem) $
             else (strID)[0] = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zPos>0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem)
          oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = strID[0]+selectedXYParamNames[0])
          if obj_valid(oStackTrack) then begin
             pValueStruct = oStackTrack->getpValueStruct()
             xData = *(*(pValueStruct)).pROIParamVect
             xError = *(*(pValueStruct)).pROIErrorVect
             if (strPos(selectedXYParamNames[1], '3D') ne -1) then (strID)[1] = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem) $
                 else (strID)[1] = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zPos>0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem)
             oStackTrack = *ROIOMstate.poStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = strID[1]+selectedXYParamNames[1])
             if obj_valid(oStackTrack) then begin
                pValueStruct = oStackTrack->getpValueStruct()
                yData = *(*(pValueStruct)).pROIParamVect
                yError = *(*(pValueStruct)).pROIErrorVect
             endif else begin
                yData = xData
                yError = yError
             endelse
          endif
       endif
       if (n_elements(xData) le 0) then begin
          xData = [1.,2.,3.]
          yData = [1.,2.,3.]
          xError = [.5,.5,.5]
          yError = [.5,.5,.5]
       endif
    widget_control, ROIOM_tlb, set_uValue = ROIOMstate, /no_copy

    oXYGroupParamPlotView->calculateNewGroupParamValues, xData = xData, yData = yData, xError = xError, yError = yError, tPos = tPos

    widget_control, wDraw, get_value = oDrawWindow
    oDrawWindow->draw, (oXYGroupParamPlotView->getXYGroupViewObj())

    state = {ROIOM_tlb:ROIOM_tlb,$
          wTopBase:wTopBase,$
          stack_tlb:stack_tlb,$
          path:s_getPathForSystem(),$
          wDraw:wDraw,$
          win:win,$
          wSelectButtonBase:wSelectButtonBase,$
          wUseBlackAndGreyValuesOnOff:wUseBlackAndGreyValuesOnOff,$
             fUseBlackAndGreyValues:1b,$
          wNormalizeToFirstValueOnOff:wNormalizeToFirstValueOnOff,$
             fNormalizeToFirstValue:1b,$
          wSavePlotDataAsASCIIOnOff:wSavePlotDataAsASCIIOnOff,$
             fSavePlotDataAsASCII:0b,$
          oXYGroupParamPlotView:oXYGroupParamPlotView,$
          selectedXYParamNames:selectedXYParamNames,$
          strID:strID,$
          pHistData:ptr_new(histData, /no_copy),$
          fMap:0b}

    oXYGroupParamPlotView->get, pParamStruct = pParamStruct
    application_tlb = wTopBase
    widget_control, wTopBase, set_uValue = state
    s_GroupParamPlot_refreshPlot, wTopBase
    XManager, 's_GroupParamPlot_Window_Resize', wTopBase, group_leader = ROIOM_tlb, cleanup = 's_GroupParamPlot_Window_cleanUp', /no_block
end;-s_GroupParamPlot_Window, event------------------------------

