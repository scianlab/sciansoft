;_____________________________IOISIOI____________________
; NAME:
;      C_sHistPlotViewObject
;
; PURPOSE:
;       - create flexible Histogram Plot Window
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = Obj_New('C_sHistPlotViewObject' )
;
; METHOHDS:
;_____________________________IOISIOI____________________


pro C_sHistPlotViewObject::get, pParamStruct = pParamStruct
    pParamStruct = self.pParamStruct
end

function C_sHistPlotViewObject::getThresholdObj
    return, self.oFilterContainer->get(position = 0)
end

function C_sHistPlotViewObject::getFilterContainerObj
    return, self.oFilterContainer
end

pro C_sHistPlotViewObject::setThresholdObj, oThreshold = oThreshold
    oThresh = self.oFilterContainer->get(position = 0)
    if obj_valid(oThresh) then obj_destroy, oThresh
    self.oFilterContainer->remove, /all
    self.oFilterContainer->add, oThreshold
end

function C_sHistPlotViewObject::getHistViewObj
    return, self.oHistView
end

function C_sHistPlotViewObject::getHistModelObj
    return, self.oHistModel
end

function C_sHistPlotViewObject::getHistPlotObject
    return, self.oHistPlot
end

function C_sHistPlotViewObject::getxTitleObj
    return, self.xTitleObj
end

function C_sHistPlotViewObject::normalizeRange, range, position = position
    On_Error, 1
    if N_Params() eq 0 then Message, 'Please pass range vector as argument.'
    if (n_elements(position) eq 0) then position = [0.0, 1.0] else position = float(position)
    range = float(range)
    return, [((position[0]*range[1])-(position[1]*range[0])) / (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]
end


pro C_sHistPlotViewObject::setHistAxis
    self.oHistPlot->getProperty, xRange = xRange, yRange = yRange, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    dif = xRange[1]-xRange[0]
    xRange[0] -= 0.1*dif
    xRange[1] += 0.1*dif
    dif = yRange[1]-yRange[0]
    yRange[0] -= 0.1*dif
    yRange[1] += 0.1*dif

    self.oHistxAxis->setProperty, range = xRange, xCoord_conv = xCoord_conv
    self.oHistyAxis->setProperty, range = yRange, yCoord_conv = yCoord_conv

    self.oHistxAxis->getProperty, CRange = xRange
    self.oHistyAxis->getProperty, CRange = yRange
    xCoord_conv = self->normalizeRange(xRange, Position = [-0.5,0.9])
    yCoord_conv = self->normalizeRange(yRange, Position = [-0.5,0.9])
    self.oHistxAxis->setProperty, location = [9999.0, -0.5, -0.5], xCoord_conv = xCoord_conv
    self.oHistyAxis->setProperty, location = [-0.5, 9999.0, -0.5], yCoord_conv = yCoord_conv
    self.oHistPlot->setProperty, yCoord_conv = yCoord_conv, xCoord_conv = xCoord_conv
    self.oThreshPlot_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
end


pro C_sHistPlotViewObject::calculateHistValues, histData = histData, updateOMinMax = updateOMinMax

    histMaxX = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistOMaxX'))[0]]
    histMinX = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistOMinX'))[0]]
    histNBins = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistNBins'))[0]]

    if (*(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'fOmitZero'))[0]] ) then begin
       pos = where(histData ne 0)
       if (pos[0] eq -1) then pos[0] = 0
    endif else pos = make_array(n_elements(histData), /index, /long)

    if (histMaxX eq histMinX) then begin
       histBinSize = (histMaxX+2) / histNBins
       yAxisValues = [0, n_elements(histData), 0]
       if (histMinX eq 0) then eps = 1. else eps = histMinX*.1
       xAxisValues = [histMinX-eps, histMinX, histMinX+eps]
    endif else begin
       histBinSize = (1.*histMaxX-histMinX) / histNBins
       yAxisValues = histogram(histData[pos]*1., binSize = histBinSize, min = histMinX, max = histMaxX, omin = omin, omax = omax, /nan)
       xAxisValues = (findgen(n_elements(yAxisValues)) * ((histMaxX-histMinX)/n_elements(yAxisValues))) + histMinX
    endelse

    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistBinSize'))[0]] = histBinSize

    if (n_elements(updateOMinMax) ne 0) then begin
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistOMaxX'))[0]] = omax
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistOMinX'))[0]] = omin
    endif

    maxX = max(xAxisValues, min = minX)
    maxY = max(yAxisValues, min = minY)
    yMaxThresh = maxY/45.
    xCoord_conv = [ -.5, 1./(maxX-minX) ]
    yCoord_conv = [ -.5, 1./(maxY-minY) ]

    self.oHistPlot->setProperty, dataX = xAxisValues, dataY = yAxisValues, /histogram, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_1->setProperty, dataY = -[yMaxThresh,yMaxThresh]*1.
    self.oThreshPlot_2->setProperty, dataY = -[yMaxThresh,yMaxThresh]*2.
    self.oThreshPlot_3->setProperty, dataY = -[yMaxThresh,yMaxThresh]*3.
    self.oThreshPlot_4->setProperty, dataY = -[yMaxThresh,yMaxThresh]*4.
    self->setHistAxis
    self->setThreshPlotValues
end


pro C_sHistPlotViewObject::calculateNewHistValues, histData = histData
    histMaxX = max(histData, min = histMinX)
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistMaxX'))[0]] = histMaxX * 1.
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistMinX'))[0]] = histMinX * 1.
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistOMaxX'))[0]] = histMaxX * 1.
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistOMinX'))[0]] = histMinX * 1.
    self->initializeThresholdValues
    self->calculateHistValues, histData = histData
end


pro C_sHistPlotViewObject::setHistPlotParams
    self.oHistView->setProperty, color = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'ViewColor'))[0]]
    self.oHistPlot->setProperty, color = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistPlotColor'))[0]]
    self.oHistxAxis->setProperty, color = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'xyAxisColor'))[0]], /exact, ticklen = 0.05, minor = 4, tickDir = 1
    self.oHistxAxis->getProperty, ticktext = tikk
    tikk->setProperty, recompute_dimensions = 2
    self.oHistxAxis->setProperty, ticktext = tikk
    self.oHistyAxis->setProperty, color = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'xyAxisColor'))[0]], /exact, ticklen = 0.05, minor = 4, tickDir = 1
    self.oHistyAxis->getProperty, ticktext = tikk
    tikk->setProperty, recompute_dimensions = 2
    self.oHistyAxis->setProperty, ticktext = tikk
end


pro C_sHistPlotViewObject::setSingleThreshValues, newXValues = newXValues, flagArr = flagArr
    if (total(flagArr) ne 0) then begin
       oThresh = self.oFilterContainer->get(position = 0)
       oThresh->get, pParamStruct = pParamStruct
       n = where(flagArr eq 1)
       case n[n_elements(n)-1] of
         0: begin  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = newXValues[0]
                  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = newXValues[1]
          endcase
         1: begin  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = newXValues[0]
                 (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = newXValues[1]
          endcase
         2: begin  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = newXValues[0]
                 (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = newXValues[1]
          endcase
         3: begin  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = newXValues[0]
                 (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = newXValues[1]
          endcase
       endcase
       self->setThreshPlotValues
    endif
end


pro C_sHistPlotViewObject::flagSetThreshValuesOnOff, flagArr = flagArr
    oThresh = self.oFilterContainer->get(position = 0)
    oThresh->get, pParamStruct = pParamStruct
    (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = flagArr[0]
    (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = flagArr[0]
    (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = flagArr[1]
    (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = flagArr[1]
    (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = flagArr[2]
    (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = flagArr[2]
    (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = flagArr[3]
    (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = flagArr[3]
    self->setThreshPlotValues
end


pro C_sHistPlotViewObject::setGaussFitValues
    oThresh = self.oFilterContainer->get(position = 0)
    oThresh->get, pParamStruct = pParamStruct

    if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]) then begin
       xMin = min( [(*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]],$
                    (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]]], max = xMax)
       self.oHistPlot->getProperty, data = axisValues
       offSet = (where(axisValues[1,*] gt 0.))[0]
       if (offSet ne -1) then offSet = axisValues[0,offSet]
       whereMinX = (where(axisValues[0,*] ge xMin))[0]
       whereMaxX = (where(axisValues[0,*] ge xMax))[0]
       if (whereMaxX gt (whereMinX+2)) then begin
         newXAxisValues = (axisValues[0,*])[whereMinX : whereMaxX]
         newYAxisValues = (axisValues[1,*])[whereMinX : whereMaxX]

         e_xmean = total(1.0 * newXAxisValues * newYAxisValues) / total(newYAxisValues)
         afit = gaussFit(newXAxisValues, newYAxisValues, aparam, nterms = 3, estimates = [max(newYAxisValues), e_xmean, (xMax-xMin)/3.])
         self.oGaussPlot_1->setProperty, datax = newXAxisValues, dataY = afit, hide = 0
         self.oGaussTitle_1->setProperty, strings = strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  +- '+string(aparam[2]/sqrt(total(newYAxisValues)))+ '(se)  |  '+string(aparam[1]-offSet)+'(µ-OffSet)'),$
                                                                                 vertical_alignment = -13.5, color = [255,0,0], alignment = -0.7, hide = 0
         print, strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  +- '+string(aparam[2]/sqrt(total(newYAxisValues)))+ '(se)  |  '+string(aparam[1]-offSet)+'(µ-OffSet)')
       endif else begin
         self.oGaussPlot_1->setProperty, hide = 1
         self.oGaussTitle_1->setProperty, hide = 1
       endelse
    endif else begin
       self.oGaussPlot_1->setProperty, hide = 1
       self.oGaussTitle_1->setProperty, hide = 1
    endelse

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]]) ) then begin
       xMin = min( [(*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]],$
                    (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]]], max = xMax)
       self.oHistPlot->getProperty, data = axisValues
       offSet = (where(axisValues[1,*] gt 0.))[0]
       if (offSet ne -1) then offSet = axisValues[0,offSet]
       whereMinX = (where(axisValues[0,*] ge xMin))[0]
       whereMaxX = (where(axisValues[0,*] ge xMax))[0]
       if (whereMaxX gt (whereMinX+2)) then begin
         newXAxisValues = (axisValues[0,*])[whereMinX : whereMaxX]
         newYAxisValues = (axisValues[1,*])[whereMinX : whereMaxX]
         e_xmean = total(1.0 * newXAxisValues * newYAxisValues) / total(newYAxisValues)
         afit = gaussFit(newXAxisValues, newYAxisValues, aparam, nterms = 3, estimates = [max(newYAxisValues), e_xmean, (xMax-xMin)/3.])
         self.oGaussPlot_2->setProperty, datax = newXAxisValues, dataY = afit, hide = 0
         self.oGaussTitle_2->setProperty, strings = strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  +- '+string(aparam[1]-offSet)+'(µ-OffSet)'), vertical_alignment = -12., color = [0,255,0], alignment = -.2, hide = 0
       endif else begin
         self.oGaussPlot_2->setProperty, hide = 1
         self.oGaussTitle_2->setProperty, hide = 1
       endelse
    endif else begin
       self.oGaussPlot_2->setProperty, hide = 1
       self.oGaussTitle_2->setProperty, hide = 1
    endelse

    if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]) then begin
       xMin = min( [(*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]],$
                    (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]]], max = xMax)
       self.oHistPlot->getProperty, data = axisValues
       offSet = (where(axisValues[1,*] gt 0.))[0]
       if (offSet ne -1) then offSet = axisValues[0,offSet]
       whereMinX = (where(axisValues[0,*] ge xMin))[0]
       whereMaxX = (where(axisValues[0,*] ge xMax))[0]
       if (whereMaxX gt (whereMinX+2)) then begin
         newXAxisValues = (axisValues[0,*])[whereMinX : whereMaxX]
         newYAxisValues = (axisValues[1,*])[whereMinX : whereMaxX]

         e_xmean = total(1.0 * newXAxisValues * newYAxisValues) / total(newYAxisValues)
         afit = gaussFit(newXAxisValues, newYAxisValues, aparam, nterms = 3, estimates = [max(newYAxisValues), e_xmean, (xMax-xMin)/3.])

         self.oGaussPlot_3->setProperty, datax = newXAxisValues, dataY = afit, hide = 0
         self.oGaussTitle_3->setProperty, strings = strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  +- '+string(aparam[2]/sqrt(total(newYAxisValues)))+ '(se)  |  '+string(aparam[1]-offSet)+'(µ-OffSet)'),$
                                                                                 vertical_alignment = -10.5, color = [0,0,200], alignment = -.2, hide = 0
       endif else begin
         self.oGaussPlot_3->setProperty, hide = 1
         self.oGaussTitle_3->setProperty, hide = 1
       endelse
    endif else begin
       self.oGaussPlot_3->setProperty, hide = 1
       self.oGaussTitle_3->setProperty, hide = 1
    endelse

    if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]) then begin
       xMin = min( [(*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]],$
                    (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]]], max = xMax)
       self.oHistPlot->getProperty, data = axisValues
       offSet = (where(axisValues[1,*] gt 0.))[0]
       if (offSet ne -1) then offSet = axisValues[0,offSet]
       whereMinX = (where(axisValues[0,*] ge xMin))[0]
       whereMaxX = (where(axisValues[0,*] ge xMax))[0]
       if (whereMaxX gt (whereMinX+2)) then begin
         newXAxisValues = (axisValues[0,*])[whereMinX : whereMaxX]
         newYAxisValues = (axisValues[1,*])[whereMinX : whereMaxX]

         e_xmean = total(1.0 * newXAxisValues * newYAxisValues) / total(newYAxisValues)
         afit = gaussFit(newXAxisValues, newYAxisValues, aparam, nterms = 3, estimates = [max(newYAxisValues), e_xmean, (xMax-xMin)/3.])

         self.oGaussPlot_4->setProperty, datax = newXAxisValues, dataY = afit, hide = 0
         self.oGaussTitle_4->setProperty, strings = strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  +- '+string(aparam[2]/sqrt(total(newYAxisValues)))+ '(se)  |  '+string(aparam[1]-offSet)+'(µ-OffSet)'),$
                                                                                                      vertical_alignment = -9., color = [255,255,0], alignment = -.2, hide = 0
       endif else begin
         self.oGaussPlot_4->setProperty, hide = 1
         self.oGaussTitle_4->setProperty, hide = 1
       endelse
    endif else begin
       self.oGaussPlot_4->setProperty, hide = 1
       self.oGaussTitle_4->setProperty, hide = 1
    endelse
end


pro C_sHistPlotViewObject::setThreshPlotValues
    oThresh = self.oFilterContainer->get(position = 0)
    oThresh->get, pParamStruct = pParamStruct

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]]) ) then begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = 1
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = 1
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]]
       self.oThreshPlot_1->setProperty, dataX = [xMin, xMax], hide = 0
    endif else begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = 0
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = 0
       self.oThreshPlot_1->setProperty, hide = 1
    endelse

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]]) ) then begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = 1
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = 1
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]]
       self.oThreshPlot_2->setProperty, dataX = [xMin, xMax], hide = 0
    endif else begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = 0
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = 0
       self.oThreshPlot_2->setProperty, hide = 1
    endelse

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]]) ) then begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = 1
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = 1
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]]
       self.oThreshPlot_3->setProperty, dataX = [xMin, xMax], hide = 0
    endif else begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = 0
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = 0
       self.oThreshPlot_3->setProperty, hide = 1
    endelse

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]]) ) then begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = 1
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = 1
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]]
       self.oThreshPlot_4->setProperty, dataX = [xMin, xMax], hide = 0
    endif else begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = 0
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = 0
       self.oThreshPlot_4->setProperty, hide = 1
    endelse
end


pro C_sHistPlotViewObject::initializeThresholdValues
    if (*(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'fKeepSliderValues'))[0]] eq 0) then begin
       minThres = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistOMinX'))[0]]
       maxThres = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'HistOMaxX'))[0]]
       difThres = (1.*maxThres-minThres)/10.

       oThresh = self.oFilterContainer->get(position = 0)
       oThresh->get, pParamStruct = pParamStruct
       *(*pParamStruct).pMin = fltArr(8) + minThres
       *(*pParamStruct).pMax = fltArr(8) + maxThres

       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = minThres+difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = minThres+difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = minThres+difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = minThres+difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = minThres+2*difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = minThres+2*difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = minThres+2*difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = minThres+2*difThres
    endif
end


function C_sHistPlotViewObject::getParameterNameList
    return, ['ViewColor',$
             'HistPlotColor',$
             'HistNBins',$
             'HistBinSize',$
             'HistMinX',$
             'HistMaxX',$
             'HistOMinX',$
             'HistOMaxX',$
             'xyAxisColor',$
             'fKeepSliderValues',$
             'fOmitZero' ]
end


pro C_sHistPlotViewObject::GaussOnOff, hide = hide
    self.oGaussTitle_1->setProperty, hide = hide
    self.oGaussTitle_2->setProperty, hide = hide
    self.oGaussTitle_3->setProperty, hide = hide
    self.oGaussTitle_4->setProperty, hide = hide
    self.oGaussPlot_1->setProperty, hide = hide
    self.oGaussPlot_2->setProperty, hide = hide
    self.oGaussPlot_3->setProperty, hide = hide
    self.oGaussPlot_4->setProperty, hide = hide
end


pro C_sHistPlotViewObject::omitZeroValueOnOff, hide = hide
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'fOmitZero'))[0]] = hide
end


pro C_sHistPlotViewObject::cleanup
    ptr_free, (*self.pParamStruct).pNames
    for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
    ptr_free, self.pParamStruct
    obj_destroy, self.oHistView
    obj_destroy, self.oHistModel
    obj_destroy, self.oHistPlot
    obj_destroy, self.oThreshPlot_1
    obj_destroy, self.oThreshPlot_2
    obj_destroy, self.oThreshPlot_3
    obj_destroy, self.oThreshPlot_4
    obj_destroy, self.oGaussTitle_1
    obj_destroy, self.oGaussTitle_2
    obj_destroy, self.oGaussTitle_3
    obj_destroy, self.oGaussTitle_4
    obj_destroy, self.oGaussPlot_1
    obj_destroy, self.oGaussPlot_2
    obj_destroy, self.oGaussPlot_3
    obj_destroy, self.oGaussPlot_4
    obj_destroy, self.xTitleObj
    obj_destroy, self.oHistxAxis
    obj_destroy, self.yTitleObj
    obj_destroy, self.oHistyAxis
end


function C_sHistPlotViewObject::init
    ParameterNameList = self->getParameterNameList()
    ParamStruct = {HistPlotViewParamStruct,$
                    pValues : ptrArr(n_elements(ParameterNameList), /allocate),$         ; Pointer on Filter Parameter Values.
                    pNames : ptr_new( ParameterNameList, /no_copy)  }     ; Pointer on Filter Parameter Names.

    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'ViewColor'))[0]] = [200,200,200]
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'xyAxisColor'))[0]] = [80,80,80]
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'HistPlotColor'))[0]] = [0,0,0]
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'HistNBins'))[0]] = 256.
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'HistMinX'))[0]] = 0.
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'HistMaxX'))[0]] = 10.
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'HistOMinX'))[0]] = 0.
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'HistOMaxX'))[0]] = 10.
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'HistBinSize'))[0]] = (10.-0.)/256
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'fKeepSliderValues'))[0]] = 0b
    *ParamStruct.pValues[(where(*ParamStruct.pNames eq 'fOmitZero'))[0]] = 0b
    self.pParamStruct = ptr_new(ParamStruct, /no_copy)

    self.oHistView = obj_new('IDLgrView')
    self.oHistModel = obj_new('IDLgrModel')
    self.oHistPlot = obj_new('IDLgrPlot')
    self.oThreshPlot_1 = obj_new('IDLgrPlot', color = [255,0,0], thick = 2)
    self.oThreshPlot_2 = obj_new('IDLgrPlot', color = [0,255,0], thick = 2)
    self.oThreshPlot_3 = obj_new('IDLgrPlot', color = [0,0,255], thick = 2)
    self.oThreshPlot_4 = obj_new('IDLgrPlot', color = [255,255,0], thick = 2)

    self.oGaussPlot_1 = obj_new('IDLgrPlot', color = [255,0,0], thick = 2)
    self.oGaussTitle_1 = obj_new('IDLgrText', color = [255,0,0], recompute_dimensions = 2, alignment = 0.0)
    self.oGaussPlot_2 = obj_new('IDLgrPlot', color = [0,255,0], thick = 2)
    self.oGaussTitle_2 = obj_new('IDLgrText', color = [0,255,0], recompute_dimensions = 2, alignment = 0.0)
    self.oGaussPlot_3 = obj_new('IDLgrPlot', color = [0,0,255], thick = 2)
    self.oGaussTitle_3 = obj_new('IDLgrText', color = [0,0,255], recompute_dimensions = 2, alignment = 0.0)
    self.oGaussPlot_4 = obj_new('IDLgrPlot', color = [255,255,0], thick = 2)
    self.oGaussTitle_4 = obj_new('IDLgrText', color = [255,255,0], recompute_dimensions = 2, alignment = 0.0)

    self.xTitleObj = obj_new('IDLgrText', 'Image Values', recompute_dimensions = 2)
    self.oHistxAxis = obj_new('IDLgrAxis', 0, title = self.xTitleObj)
    self.yTitleObj = obj_new('IDLgrText', 'Frequency', recompute_dimensions = 2)
    self.oHistyAxis = obj_new('IDLgrAxis', 1, title = self.yTitleObj)

    self.oHistModel->add, self.oGaussPlot_1
    self.oHistModel->add, self.oGaussPlot_2
    self.oHistModel->add, self.oGaussPlot_3
    self.oHistModel->add, self.oGaussPlot_4
    self.oHistModel->add, self.oGaussTitle_1
    self.oHistModel->add, self.oGaussTitle_2
    self.oHistModel->add, self.oGaussTitle_3
    self.oHistModel->add, self.oGaussTitle_4
    self.oHistModel->add, self.oHistxAxis
    self.oHistModel->add, self.oHistyAxis
    self.oHistModel->add, self.oHistPlot

    self.oHistModel->add, self.oThreshPlot_1
    self.oHistModel->add, self.oThreshPlot_2
    self.oHistModel->add, self.oThreshPlot_3
    self.oHistModel->add, self.oThreshPlot_4
    self.oHistView->add, self.oHistModel
    self->setHistPlotParams

    self.oFilterContainer = obj_new('IDL_Container')
    self.oFilterContainer->add, obj_new('C_sImageFilter_Threshold')
    self->initializeThresholdValues
    return, 1
end


pro C_sHistPlotViewObject__define
   tmp = {C_sHistPlotViewObject, pParamStruct:ptr_new(),$
                                 oHistView:obj_new(),$
                                 oHistModel:obj_new(),$
                                 xTitleObj:obj_new(),$
                                 oHistxAxis:obj_new(),$
                                 yTitleObj:obj_new(),$
                                 oHistyAxis:obj_new(),$
                                 oHistPlot:obj_new(),$
                                 oThreshPlot_1:obj_new(),$
                                 oThreshPlot_2:obj_new(),$
                                 oThreshPlot_3:obj_new(),$
                                 oThreshPlot_4:obj_new(),$
                                 oGaussTitle_1:obj_new(),$
                                 oGaussTitle_2:obj_new(),$
                                 oGaussTitle_3:obj_new(),$
                                 oGaussTitle_4:obj_new(),$
                                 oGaussPlot_1: obj_new(),$
                                 oGaussPlot_2: obj_new(),$
                                 oGaussPlot_3: obj_new(),$
                                 oGaussPlot_4: obj_new(),$
                                 oFilterContainer:obj_new()}
end