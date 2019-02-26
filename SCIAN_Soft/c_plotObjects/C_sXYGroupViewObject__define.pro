;_____________________________IOISIOI____________________
; NAME:
;      C_XYGroupViewObject
;
; PURPOSE:
;       - create flexible XY - Data Plot Window
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = Obj_New('C_XYGroupViewObject' )
;
; METHOHDS:
;_____________________________IOISIOI____________________


pro C_XYGroupViewObject::get, pParamStruct = pParamStruct
    pParamStruct = self.pParamStruct
end

function C_XYGroupViewObject::getThresholdObj
    return, self.oFilterContainer->get(position = 0)
end

function C_XYGroupViewObject::getFilterContainerObj
    return, self.oFilterContainer
end

pro C_XYGroupViewObject::setThresholdObj, oThreshold = oThreshold
    oThresh = self.oFilterContainer->get(position = 0)
    if obj_valid(oThresh) then obj_destroy, oThresh
    self.oFilterContainer->remove, /all
    self.oFilterContainer->add, oThreshold
end

function C_XYGroupViewObject::getXYGroupViewObj
    return, self.oXYGroupView
end
function C_XYGroupViewObject::getXYGroupPlotObject
    return, self.oXYGroupPlot
end
function C_XYGroupViewObject::getXYGroupErrorUpPlot
    return, self.oXYGroupErrorUpPlot
end
function C_XYGroupViewObject::getxTitleObj
    return, self.xTitleObj
end
function C_XYGroupViewObject::getyTitleObj
    return, self.yTitleObj
end


function C_XYGroupViewObject::normalizeRange, range, position = position
    on_error, 1
    if n_params() eq 0 then message, 'Please pass range vector as argument.'
    if (n_elements(position) eq 0) then position = [0.0, 1.0] else position = float(position)
    range = float(range)
    return, [((position[0]*range[1])-(position[1]*range[0])) / (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]
end


pro C_XYGroupViewObject::setXYGroupAxis
    self.oXYGroupPlot->getProperty, xRange = xRange, yRange = yRange, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    dif = xRange[1]-xRange[0]
    xRange[0] = xRange[0] - 0.1*dif
    xRange[1] = xRange[1] + 0.1*dif
    dif = yRange[1]-yRange[0]
    yRange[0] = yRange[0] - 0.1*dif
    yRange[1] = yRange[1] + 0.1*dif

    self.oXYGroupxAxis->setProperty, range = xRange, xCoord_conv = xCoord_conv
    self.oXYGroupyAxis->setProperty, range = yRange, yCoord_conv = yCoord_conv

    self.oXYGroupxAxis->getProperty, cRange = xRange
    self.oXYGroupyAxis->getProperty, cRange = yRange
    xCoord_conv = self->normalizeRange(xRange, Position = [-0.5,0.9])
    yCoord_conv = self->normalizeRange(yRange, Position = [-0.5,0.9])
    self.oXYGroupxAxis->setProperty, location = [9999.0, -0.5, -0.5], xCoord_conv = xCoord_conv
    self.oXYGroupyAxis->setProperty, location = [-0.5, 9999.0, -0.5], yCoord_conv = yCoord_conv
    self.oXYGroupPlot->setProperty, yCoord_conv = yCoord_conv, xCoord_conv = xCoord_conv
    self.oXYGroupSequencePlot->setProperty, yCoord_conv = yCoord_conv, xCoord_conv = xCoord_conv
    self.oXYGroupErrorUpPlot->setProperty, yCoord_conv = yCoord_conv, xCoord_conv = xCoord_conv
    self.oXYGroupErrorDownPlot->setProperty, yCoord_conv = yCoord_conv, xCoord_conv = xCoord_conv
    self.oThreshPlot_X1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_X2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_X3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_X4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_Y1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_Y2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_Y3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oThreshPlot_Y4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
end


pro C_XYGroupViewObject::calculateXYGroupValues, updateOMinMax = updateOMinMax, xData = xData, yData = yData, xError = xError, yError = yError

    maxDataX = max(1.*xData, min = minDataX)
    maxDataY = max(1.*yData, min = minDataY)
    if (maxDataX eq minDataX) then begin
       xData = [xData[0]-abs(xData[0]*.01), xData, xData[n_elements(xData)-1]+abs(xData[0]*.01)]
       yData = [yData[0], yData, yData[n_elements(yData)-1]]
       maxDataX = max(1.*xData, min = minDataX)
    endif
    if (maxDataY eq minDataY) then begin
       yData = [yData[0]-abs(yData[0]*.01), yData, yData[n_elements(yData)-1]+abs(yData[0]*.01)]
       xData = [xData[0], xData, xData[n_elements(xData)-1]]
       maxDataY = max(1.*yData, min = minDataY)
    endif

    if (n_elements(updateOMinMax) ne 0) then begin
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupMaxX'))[0]] = maxDataX
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupMinX'))[0]] = minDataX
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMaxX'))[0]] = maxDataX + (maxDataX - minDataX) * .05
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMinX'))[0]] = minDataX  - (maxDataX - minDataX) * .05
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupMaxY'))[0]] = maxDataY
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupMinY'))[0]] = minDataY
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMaxY'))[0]] = maxDataY + (maxDataY - minDataY) * .05
       *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMinY'))[0]] = minDataY - (maxDataY - minDataY) * .05
    endif

    XYGroupOMaxX = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMaxX'))[0]]
    XYGroupOMinX = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMinX'))[0]]
    XYGroupOMaxY = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMaxY'))[0]]
    XYGroupOMinY = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMinY'))[0]]

    xcoord_conv = [ -.5, 1./(maxDataX-minDataX) ]
    ycoord_conv = [ -.5, 1./(maxDataY-minDataY) ]
    self.oXYGroupPlot->setProperty, dataX = xData, dataY = yData, xcoord_conv = xcoord_conv, ycoord_conv = ycoord_conv
    self.oXYGroupSymbol->setProperty, size = [(maxDataX-minDataX)/40., (maxDataY-minDataY)/40. ,1.]

    nData = n_elements(xData)-1
    dif = (maxDataX-minDataX) / 40.
    polyX = [-dif, dif, dif, -dif, -dif] +  xData[self.tPos > 0 < nData]
    dif = (maxDataY-minDataY) / 40.
    polyY = [-dif, -dif, dif, dif, -dif] +  yData[self.tPos > 0 < nData]

    if (n_elements(yError) eq 0) then yError = xError
    self.oXYGroupSequencePlot->setProperty, dataX = polyX, dataY = polyY, xcoord_conv = xcoord_conv, ycoord_conv = ycoord_conv
    self.oXYGroupErrorUpPlot->setProperty, dataX = xData, dataY = yData + yError, xcoord_conv = xcoord_conv, ycoord_conv = ycoord_conv
    self.oXYGroupErrorDownPlot->setProperty, dataX = xData, dataY = yData - yError, xcoord_conv = xcoord_conv, ycoord_conv = ycoord_conv

    self.oGaussTitle_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussTitle_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_1->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_2->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_3->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv
    self.oGaussPlot_4->setProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv

    yMaxThresh = maxDataY/45.
    self.oThreshPlot_X1->setProperty, dataY = -[yMaxThresh, yMaxThresh]*1.
    self.oThreshPlot_X2->setProperty, dataY = -[yMaxThresh, yMaxThresh]*2.
    self.oThreshPlot_X3->setProperty, dataY = -[yMaxThresh, yMaxThresh]*3.
    self.oThreshPlot_X4->setProperty, dataY = -[yMaxThresh, yMaxThresh]*4.
    xMaxThresh = maxDataX/45.
    self.oThreshPlot_Y1->setProperty, dataX = -[xMaxThresh, xMaxThresh]*1., dataY = -[yMaxThresh, yMaxThresh]*1.
    self.oThreshPlot_Y2->setProperty, dataX = -[xMaxThresh, xMaxThresh]*2., dataY = -[yMaxThresh, yMaxThresh]*2.
    self.oThreshPlot_Y3->setProperty, dataX = -[xMaxThresh, xMaxThresh]*3., dataY = -[yMaxThresh, yMaxThresh]*3.
    self.oThreshPlot_Y4->setProperty, dataX = -[xMaxThresh, xMaxThresh]*4., dataY = -[yMaxThresh, yMaxThresh]*4.

    self->setXYGroupAxis
    self->setThreshPlotValues
end


pro C_XYGroupViewObject::calculateNewGroupParamValues, xData = xData, yData = yData, xError = xError, yError = yError, tPos = tPos

    self.tPos = tPos
    XYGroupMaxX = max(1.*xData, min = XYGroupMinX)
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupMaxX'))[0]] = XYGroupMaxX
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupMinX'))[0]] = XYGroupMinX
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMaxX'))[0]] = XYGroupMaxX + (XYGroupMaxX - XYGroupMinX) * .05
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMinX'))[0]] = XYGroupMinX  - (XYGroupMaxX - XYGroupMinX) * .05

    XYGroupMaxY = max(1.*yData, min = XYGroupMinY)
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupMaxY'))[0]] = XYGroupMaxY
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupMinY'))[0]] = XYGroupMinY
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMaxY'))[0]] = XYGroupMaxY + (XYGroupMaxY - XYGroupMinY) * .05
    *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMinY'))[0]] = XYGroupMinY - (XYGroupMaxY - XYGroupMinY) * .05

    self->initializeThresholdValues
    self->calculateXYGroupValues, xData = xData, yData = yData, xError = xError, yError = yError
end


pro C_XYGroupViewObject::setXYGroupPlotParams
    self.oXYGroupView->setProperty, color = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'ViewColor'))[0]]
    self.oXYGroupPlot->setProperty, color = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupPlotColor'))[0]]
    self.oXYGroupxAxis->setProperty, color = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'xyAxisColor'))[0]], /exact, ticklen = 0.05, minor = 4, tickDir = 1
    self.oXYGroupxAxis->getProperty, ticktext = tikk
    tikk->setProperty, recompute_dimensions = 2
    self.oXYGroupxAxis->setProperty, ticktext = tikk
    self.oXYGroupyAxis->setProperty, color = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'xyAxisColor'))[0]], /exact, ticklen = 0.05, minor = 4, tickDir = 1
    self.oXYGroupyAxis->getProperty, ticktext = tikk
    tikk->setProperty, recompute_dimensions = 2
    self.oXYGroupyAxis->setProperty, ticktext = tikk
end


pro C_XYGroupViewObject::setSingleThreshValues, newXValues = newXValues, flagArr = flagArr
    if (total(flagArr) ne 0) then begin
       oThresh = self.oFilterContainer->get(position = 0)
       oThresh->get, pParamStruct = pParamStruct
       n = where(flagArr eq 1)
       case n[n_elements(n)-1] of
         0: begin (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = newXValues[0]
                  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = newXValues[1]
          endcase
         1: begin (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = newXValues[0]
                  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = newXValues[1]
          endcase
         2: begin (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = newXValues[0]
                  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = newXValues[1]
          endcase
         3: begin (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = newXValues[0]
                  (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = newXValues[1]
          endcase
       endcase
       self->setThreshPlotValues
    endif
end


pro C_XYGroupViewObject::flagSetThreshValuesOnOff, flagArr = flagArr
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


pro C_XYGroupViewObject::setGaussFitValues

    oThresh = self.oFilterContainer->get(position = 0)
    oThresh->get, pParamStruct = pParamStruct

    if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]) then begin
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]]
       xMin = xMin < xMax
       xMax = xMin > xMax
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
         self.oGaussTitle_1->setProperty, strings = strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  |  '+string(aparam[1]-offSet)+'(µ-OffSet)'), vertical_alignment = -13.5, color = [255,0,0], alignment = -.2, hide = 0
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
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]]
       xMin = xMin < xMax
       xMax = xMin > xMax
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
         self.oGaussTitle_2->setProperty, strings = strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  |  '+string(aparam[1]-offSet)+'(µ-OffSet)'), vertical_alignment = -12., color = [0,255,0], alignment = -.2, hide = 0
       endif else begin
         self.oGaussPlot_2->setProperty, hide = 1
         self.oGaussTitle_2->setProperty, hide = 1
       endelse
    endif else begin
       self.oGaussPlot_2->setProperty, hide = 1
       self.oGaussTitle_2->setProperty, hide = 1
    endelse

    if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]) then begin
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]]
       xMin = xMin < xMax
       xMax = xMin > xMax
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
         self.oGaussTitle_3->setProperty, strings = strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  |  '+string(aparam[1]-offSet)+'(µ-OffSet)'), vertical_alignment = -10.5, color = [0,0,200], alignment = -.2, hide = 0
       endif else begin
         self.oGaussPlot_3->setProperty, hide = 1
         self.oGaussTitle_3->setProperty, hide = 1
       endelse
    endif else begin
       self.oGaussPlot_3->setProperty, hide = 1
       self.oGaussTitle_3->setProperty, hide = 1
    endelse

    if ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]) then begin
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]]
       xMin = xMin < xMax
       xMax = xMin > xMax
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
         self.oGaussTitle_4->setProperty, strings = strCompress(string(aparam[1])+'(µ) +-'+string(aparam[2])+ '(sd)  |  '+string(aparam[1]-offSet)+'(µ-OffSet)'), vertical_alignment = -9., color = [255,255,0], alignment = -.2, hide = 0
       endif else begin
         self.oGaussPlot_4->setProperty, hide = 1
         self.oGaussTitle_4->setProperty, hide = 1
       endelse
    endif else begin
       self.oGaussPlot_4->setProperty, hide = 1
       self.oGaussTitle_4->setProperty, hide = 1
    endelse
end


pro C_XYGroupViewObject::setThreshPlotValues
    oThresh = self.oFilterContainer->get(position = 0)
    oThresh->get, pParamStruct = pParamStruct

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]]) ) then begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = 1
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = 1
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]]
       self.oThreshPlot_X1->setProperty, dataX = [xMin, xMax], hide = 0
    endif else begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = 0
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = 0
       self.oThreshPlot_X1->setProperty, hide = 1
    endelse

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]]) ) then begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = 1
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = 1
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]]
       self.oThreshPlot_X2->setProperty, dataX = [xMin, xMax], hide = 0
    endif else begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = 0
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = 0
       self.oThreshPlot_X2->setProperty, hide = 1
    endelse

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]]) ) then begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = 1
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = 1
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]]
       self.oThreshPlot_X3->setProperty, dataX = [xMin, xMax], hide = 0
    endif else begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = 0
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = 0
       self.oThreshPlot_X3->setProperty, hide = 1
    endelse

    if ( ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]) or $
         ((*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]]) ) then begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = 1
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = 1
       xMin = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]]
       xMax = (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]]
       self.oThreshPlot_X4->setProperty, dataX = [xMin, xMax], hide = 0
    endif else begin
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = 0
       (*(*pParamStruct).pActive[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = 0
       self.oThreshPlot_X4->setProperty, hide = 1
    endelse
end


pro C_XYGroupViewObject::initializeThresholdValues

    if (*(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'fKeepSliderValues'))[0]] eq 0) then begin
       minThres = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMinX'))[0]]
       maxThres = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMaxX'))[0]]
       difThres = (1.*maxThres-minThres)/10.

       oThresh = self.oFilterContainer->get(position = 0)
       oThresh->get, pParamStruct = pParamStruct
       (*(*pParamStruct).pMin)[*] = fltArr(8) + minThres
       (*(*pParamStruct).pMax)[*] = fltArr(8) + maxThres

       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1a'))[0]] = minThres+difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2a'))[0]] = minThres+difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3a'))[0]] = minThres+difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4a'))[0]] = minThres+difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_1b'))[0]] = minThres+2*difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_2b'))[0]] = minThres+2*difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_3b'))[0]] = minThres+2*difThres
       (*(*pParamStruct).pValues[0])[(where( *(*pParamStruct).pNames eq 'Threshold_4b'))[0]] = minThres+2*difThres
    endif

    if (*(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'fKeepSliderValues'))[0]] eq 0) then begin
       minThres = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMinY'))[0]]
       maxThres = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'XYGroupOMaxY'))[0]]
       difThres = (1.*maxThres-minThres)/10.

       oThresh = self.oFilterContainer->get(position = 1)
       oThresh->get, pParamStruct = pParamStruct
       (*(*pParamStruct).pMin)[*] = fltArr(8) + minThres
       (*(*pParamStruct).pMax)[*] = fltArr(8) + maxThres

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

function C_XYGroupViewObject::getParameterNameList
    return, ['ViewColor',$
             'XYGroupPlotColor'   ,$
             'XYGroupMinX'    ,$
             'XYGroupMaxX'    ,$
             'XYGroupOMinX'   ,$
             'XYGroupOMaxX'   ,$
             'XYGroupMinY'    ,$
             'XYGroupMaxY'    ,$
             'XYGroupOMinY'   ,$
             'XYGroupOMaxY'   ,$
             'xyAxisColor'    ,$
             'fKeepSliderValues' ]
end


pro C_XYGroupViewObject::cleanup
    ptr_free, (*self.pParamStruct).pNames
    for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
    ptr_free, self.pParamStruct
    obj_destroy, self.oXYGroupView
    obj_destroy, self.oXYGroupModel
    obj_destroy, self.oXYGroupPlot
    obj_destroy, self.oXYGroupSymbol
    obj_destroy, self.oXYGroupSequencePlot
    obj_destroy, self.oXYGroupErrorUpPlot
    obj_destroy, self.oXYGroupErrorDownPlot
    obj_destroy, self.oThreshPlot_X1
    obj_destroy, self.oThreshPlot_X2
    obj_destroy, self.oThreshPlot_X3
    obj_destroy, self.oThreshPlot_X4
    obj_destroy, self.oThreshPlot_Y1
    obj_destroy, self.oThreshPlot_Y2
    obj_destroy, self.oThreshPlot_Y3
    obj_destroy, self.oThreshPlot_Y4
    obj_destroy, self.oGaussTitle_1
    obj_destroy, self.oGaussTitle_2
    obj_destroy, self.oGaussTitle_3
    obj_destroy, self.oGaussTitle_4
    obj_destroy, self.oGaussPlot_1
    obj_destroy, self.oGaussPlot_2
    obj_destroy, self.oGaussPlot_3
    obj_destroy, self.oGaussPlot_4
    obj_destroy, self.xTitleObj
    obj_destroy, self.oXYGroupxAxis
    obj_destroy, self.yTitleObj
    obj_destroy, self.oXYGroupyAxis
end


function C_XYGroupViewObject::init
    parameterNameList = self->getParameterNameList()
    paramStruct = {XYGroupPlotViewParamStruct,$
                   pValues : ptrArr(n_elements(parameterNameList), /allocate),$   ; Pointer on Filter Parameter Values.
                   pNames : ptr_new( parameterNameList, /no_copy)  }                 ; Pointer on Filter Parameter Names.

    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ViewColor'))[0]] = [180,180,180]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'xyAxisColor'))[0]] = [80,80,80]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupPlotColor'))[0]] = [0,0,0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupMinX'))[0]] = 0.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupMaxX'))[0]] = 10.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupOMinX'))[0]] = 0.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupOMaxX'))[0]] = 10.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupMinY'))[0]] = 0.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupMaxY'))[0]] = 10.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupOMinY'))[0]] = 0.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'XYGroupOMaxY'))[0]] = 10.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'fKeepSliderValues'))[0]] = 0b

    self.pParamStruct = ptr_new(paramStruct, /no_copy)

    self.oXYGroupView = obj_new('IDLgrView')
    self.oXYGroupModel = obj_new('IDLgrModel')
    self.oXYGroupSymbol = obj_new('IDLgrSymbol', data = 1, color = [0,0,0], thick = 1)
    self.oXYGroupPlot = obj_new('IDLgrPlot', symbol = self.oXYGroupSymbol)
    self.oXYGroupSequencePlot = obj_new('IDLgrPlot', color = [255,0,0], thick = 1)
    self.oXYGroupErrorUpPlot = obj_new('IDLgrPlot', color = [120,120,120], linestyle = 1)
    self.oXYGroupErrorDownPlot = obj_new('IDLgrPlot', color = [120,120,120], linestyle = 1)
    self.oThreshPlot_X1 = obj_new('IDLgrPlot', color = [255,0,0], thick = 2)
    self.oThreshPlot_X2 = obj_new('IDLgrPlot', color = [0,255,0], thick = 2)
    self.oThreshPlot_X3 = obj_new('IDLgrPlot', color = [0,0,255], thick = 2)
    self.oThreshPlot_X4 = obj_new('IDLgrPlot', color = [255,255,0], thick = 2)
    self.oThreshPlot_Y1 = obj_new('IDLgrPlot', color = [255,0,0], thick = 2)
    self.oThreshPlot_Y2 = obj_new('IDLgrPlot', color = [0,255,0], thick = 2)
    self.oThreshPlot_Y3 = obj_new('IDLgrPlot', color = [0,0,255], thick = 2)
    self.oThreshPlot_Y4 = obj_new('IDLgrPlot', color = [255,255,0], thick = 2)

    self.oGaussPlot_1 = obj_new('IDLgrPlot', color = [255,0,0], thick = 2)
    self.oGaussTitle_1 = obj_new('IDLgrText', color = [255,0,0], recompute_dimensions = 2, alignment = 0.0)
    self.oGaussPlot_2 = obj_new('IDLgrPlot', color = [0,255,0], thick = 2)
    self.oGaussTitle_2 = obj_new('IDLgrText', color = [0,255,0], recompute_dimensions = 2, alignment = 0.0)
    self.oGaussPlot_3 = obj_new('IDLgrPlot', color = [0,0,255], thick = 2)
    self.oGaussTitle_3 = obj_new('IDLgrText', color = [0,0,255], recompute_dimensions = 2, alignment = 0.0)
    self.oGaussPlot_4 = obj_new('IDLgrPlot', color = [255,255,0], thick = 2)
    self.oGaussTitle_4 = obj_new('IDLgrText', color = [255,255,0], recompute_dimensions = 2, alignment = 0.0)

    self.xTitleObj = obj_new('IDLgrText', 'Image Values', recompute_dimensions = 2)
    self.oXYGroupxAxis = obj_new('IDLgrAxis', 0, title = self.xTitleObj)
    self.yTitleObj = obj_new('IDLgrText', 'Frequency', recompute_dimensions = 2)
    self.oXYGroupyAxis = obj_new('IDLgrAxis', 1, title = self.yTitleObj)

    self.oXYGroupModel->add, self.oXYGroupxAxis
    self.oXYGroupModel->add, self.oXYGroupyAxis
    self.oXYGroupModel->add, self.oXYGroupSequencePlot
    self.oXYGroupModel->add, self.oXYGroupErrorUpPlot
    self.oXYGroupModel->add, self.oXYGroupErrorDownPlot
    self.oXYGroupModel->add, self.oXYGroupPlot

    self.oXYGroupModel->add, self.oThreshPlot_X1
    self.oXYGroupModel->add, self.oThreshPlot_X2
    self.oXYGroupModel->add, self.oThreshPlot_X3
    self.oXYGroupModel->add, self.oThreshPlot_X4
    self.oXYGroupModel->add, self.oThreshPlot_Y1
    self.oXYGroupModel->add, self.oThreshPlot_Y2
    self.oXYGroupModel->add, self.oThreshPlot_Y3
    self.oXYGroupModel->add, self.oThreshPlot_Y4
    self.oXYGroupModel->add, self.oGaussPlot_1
    self.oXYGroupModel->add, self.oGaussPlot_2
    self.oXYGroupModel->add, self.oGaussPlot_3
    self.oXYGroupModel->add, self.oGaussPlot_4
    self.oXYGroupModel->add, self.oGaussTitle_1
    self.oXYGroupModel->add, self.oGaussTitle_2
    self.oXYGroupModel->add, self.oGaussTitle_3
    self.oXYGroupModel->add, self.oGaussTitle_4
    self.oXYGroupView->add, self.oXYGroupModel
    self->setXYGroupPlotParams

    self.oFilterContainer = obj_new('IDL_Container')
    self.oFilterContainer->add, obj_new('C_sImageFilter_Threshold')
    self.oFilterContainer->add, obj_new('C_sImageFilter_Threshold')
    self->initializeThresholdValues
    return, 1
end


pro C_XYGroupViewObject__define
   tmp = {C_XYGroupViewObject, pParamStruct:ptr_new(),$
                                 oXYGroupView:obj_new(),$
                                 oXYGroupModel:obj_new(),$
                                 xTitleObj:obj_new(),$
                                 oXYGroupxAxis:obj_new(),$
                                 yTitleObj:obj_new(),$
                                 oXYGroupyAxis:obj_new(),$
                                 oXYGroupSymbol:obj_new(),$
                                 oXYGroupPlot:obj_new(),$
                                 oXYGroupSequencePlot:obj_new(),$
                                 tPos: 1 ,$
                                 oXYGroupErrorUpPlot:obj_new(),$
                                 oXYGroupErrorDownPlot:obj_new(),$
                                 oThreshPlot_X1:obj_new(),$
                                 oThreshPlot_X2:obj_new(),$
                                 oThreshPlot_X3:obj_new(),$
                                 oThreshPlot_X4:obj_new(),$
                                 oThreshPlot_Y1:obj_new(),$
                                 oThreshPlot_Y2:obj_new(),$
                                 oThreshPlot_Y3:obj_new(),$
                                 oThreshPlot_Y4:obj_new(),$
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
