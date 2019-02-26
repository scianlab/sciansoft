pro CEDAI_histogramPlot_VSL_OMS, path = path, vect = vect, name = name, veloLimitsVCL = veloLimitsVCL, veloLimitsVSL = veloLimitsVSL, linesFlag = linesFlag, tempFileName = tempFileName, veloLimVSLb = veloLimVSLb,s_Total = s_Total

; for debugging activate:
;    path = s_getPathForSystem() + 'VCL_HistoDebug.tiff'
;    name = 'hello'
;    tempFileName = path
;    vect = indgen(15, /float)
;    linesFlag = 1
;    veloLimitsVCL = [0,1., 1.,5., 5.,30., 30.,10000.]
;    veloLimitsVSL = [2.5, 4], modificado a [2.5,6.0]*xReal Marzo 2013 para ver menores a 3.33
;;    
   index = where(finite(vect), count)
   vect = vect(index)

  ; MOR - pass in the flag for whether to draw vertical lines on the histogram
   ;flag1 = linesFlag
   if linesFlag eq 1 then begin
    index = where(vect ge veloLimitsVCL[n_elements(veloLimitsVCL)-2], count)
    if (count gt 0) then vect[index] = veloLimitsVCL[n_elements(veloLimitsVCL)-2]
   endif

   file = strCompress(tempFileName + name + s_Total + '.dat', /remove)   
   get_lun, unit
   openW, Unit, file
   printF, Unit, name
   printF, Unit, transpose(vect)
   close, Unit

   ; MOR- rebin so that there are 26 bins and so that we out the bin locations 
   if linesFlag then begin
      ;hist = histogram(vect, nbins = veloLimitsVCL[n_elements(veloLimitsVCL)-2]+1, max = max(veloLimitsVSL), min = 0, locations = locBins, reverse_indices = ri)
      hist = histogram(vect, nbins = veloLimitsVCL[n_elements(veloLimitsVCL)-2]+1, max = max(vect)+1, min = 0., locations = locBins, reverse_indices = ri)
   endif else begin
      hist = histogram(vect, nbins = n_elements(vect), locations = locBins, reverse_indices = ri)
   end

   ; select the appearance of the histogram (0 = gradient/1 = solid)
   flag2 = 1
   binSize = locBins(2)-locBins(1) 
   datas_old = hist
   datas_y = 1.*datas_old/total(datas_old) ; MOR - normalize so on a 0,1 scale based on frequency
   datas_x = indgen(n_elements(datas_y))*binSize
;print,where(datas_y eq max(datas_y),count)
   Z = 1./(max(datas_y)-min(datas_y))
   K = 1.15
   Y = 0.8
   xs = [K, Y]
   ys = [K, Y]

   xsize = n_elements(datas_x)
   ysize = n_elements(datas_y)

   ;yr = [0, max(datas_y)+.1] ; MOR - commented out - make a percentage, do not add a fixed amnt.
   ysf = 1.05;
   yr = [0, max(datas_y)*ysf] ; MOR - make the range in Y 10% more than the maximum
;   xr = [0, n_elements(datas_y)*binSize]
    xr = [0, max(datas_x)]
   ;LC = 180 ; MOR - commented out - make grey color darker 
   LC = 150

   oPlot = obj_new('IDLgrPlot', xRange = xr, yRange = yr, dataY = [0, 0], color = [0, 0, 0], histogram = 1, xcoord_conv = xs, ycoord_conv = ys)

   plotColor = [0, 0, 0]
   
   ; MOR - 7Sept2010 - increase the font displayed on the plot - BEGIN
   oFont_box = obj_new('IDLgrFont','helvetica',size=18)
   oXTitle = obj_new('IDLgrText', name, color = [0, 0, 0], xcoord_conv = xs, ycoord_conv = ys, font  = oFont_box)
   oXAxis = obj_new('IDLgrAxis', 0, range = xr, title = oXTitle, color = plotColor, location = [0, 0], XCOORD_CONV = xs, YCOORD_CONV = ys, TICKLEN = max(datas_y)*0.02, /exact)
   oYTitle = obj_new('IDLgrText', 'relative frequency', color = plotColor, XCOORD_CONV = xs, YCOORD_CONV = ys, font  = oFont_box)
   ; MOR - 7Sept2010 - increase the font displayed on the plot - END
    
   oYAxis = obj_new('IDLgrAxis', 1, range = yr, title = oYTitle, color = plotColor, location = [0, 0], ticklen = max(datas_y)*0.1, XCOORD_CONV = xs, YCOORD_CONV = ys, /exact)
   oYAxisUp = obj_new('IDLgrAxis', 1, range = yr, color = plotColor, location = [xr[1], 0], TICKDIR = 1, ticklen = max(datas_y)*0.1, XCOORD_CONV = xs, YCOORD_CONV = ys, /exact, /noText)
   oXAxisUp = obj_new('IDLgrAxis', 0, range = xr, color = plotColor, location = [0, yr[1]], TICKDIR = 1, ticklen = max(datas_y)*0.02, XCOORD_CONV = xs, YCOORD_CONV = ys, /exact, /noText)
   ; MOR - commented above defined lines in order to make them depend on max datas-y value instead - BEGIN
   if linesFlag then begin
      oLine1 = obj_new('IDLgrPolyline', [veloLimitsVSL[0],veloLimitsVSL[0]], [0, max(datas_y)*ysf], color = [LC, LC, LC], symbol = sym, XCOORD_CONV = xs, YCOORD_CONV = ys, LINESTYLE = [2, 'F0F0'X])
      ; MOR - color the regions with blocks
      opac = 1.0
      ; type D - blue
      startD = 0
      ;endptD = veloLimitsVSL[0]; default
      ;endptD = veloLimVSLb[0]
      endptD = veloLimVSLb[1]
      clr = [0, 0, 255]; blue range - IM
      ;;oBack1 =  obj_new('IDLgrPolygon', [startD, startD, endptD, endptD], [0,max(datas_y)*ysf , max(datas_y)*ysf,0], style = 2, color = clr, alpha_channel = 1, XCOORD_CONV = xs1, YCOORD_CONV = ys1)
      ;oBack1 =  obj_new('IDLgrPolygon', [startD, startD, endptD, endptD], [20*((max(datas_y)*ysf)/20),20.2*((max(datas_y)*ysf)/20) , 20.2*((max(datas_y)*ysf)/20),20*((max(datas_y)*ysf)/20)], style = 2, color = clr, alpha_channel = 1, XCOORD_CONV = xs1, YCOORD_CONV = ys1)
      oBack1 =  obj_new('IDLgrPolygon', [startD, startD, endptD, endptD], [20.2*((max(datas_y)*ysf)/20),20.5*((max(datas_y)*ysf)/20) , 20.5*((max(datas_y)*ysf)/20),20.2*((max(datas_y)*ysf)/20)], style = 2, color = clr, alpha_channel = 1, XCOORD_CONV = xs1, YCOORD_CONV = ys1)
      
      ; type C - green
      ;startC = veloLimitsVSL[0]
      startC = veloLimVSLb[0]
      ;endptC = veloLimitsVSL[1]
      endptC = veloLimVSLb[1]
      clr = [0, 255, 0] ; green range - NP
      ;oBack2 =  obj_new('IDLgrPolygon', [startC, startC, endptC, endptC], [0,max(datas_y)*ysf , max(datas_y)*ysf,0], style = 2, color = clr, alpha_channel = opac, XCOORD_CONV = xs1, YCOORD_CONV = ys1)           
      oBack2 =  obj_new('IDLgrPolygon', [startC, startC, endptC, endptC], [20.7*((max(datas_y)*ysf)/20),21.0*((max(datas_y)*ysf)/20) , 21.0*((max(datas_y)*ysf)/20),20.7*((max(datas_y)*ysf)/20)], style = 2, color = clr, alpha_channel = opac, XCOORD_CONV = xs1, YCOORD_CONV = ys1)
      
      ; type A+B+C - red
      ;startAB = veloLimitsVSL[1]
      startAB = veloLimVSLb[1]
      
      endptAB = max(vect)+1;veloLimitsVSL[2]
      clr = [255, 0, 0] ; green range - NP
      ;oBack3 =  obj_new('IDLgrPolygon', [startAB, startAB, endptAB, endptAB], [0,max(datas_y)*ysf , max(datas_y)*ysf,0], style = 2, color = clr, alpha_channel = opac, XCOORD_CONV = xs1, YCOORD_CONV = ys1)
      oBack3 =  obj_new('IDLgrPolygon', [startAB, startAB, endptAB, endptAB], [21.2*((max(datas_y)*ysf)/20),21.5*((max(datas_y)*ysf)/20) , 21.5*((max(datas_y)*ysf)/20),21.2*((max(datas_y)*ysf)/20)], style = 2, color = clr, alpha_channel = opac, XCOORD_CONV = xs1, YCOORD_CONV = ys1)
   endif
   ; MOR - mods to line definition - END
   
   
    ; Because we may not be using exact axis ranging, the axes
    ; may extend further than the xrange and yrange. Get the
    ; actual axis range so that the plot, etc. can be scaled
    ; appropriately.
    position = [0.2, 0.2, 0.95, 0.95]
    oXAxis->GetProperty, CRange=xrange
    oYAxis->GetProperty, CRange=yrange

    ; Set up the scaling so that the axes for the plot and the
    ; plot data extends from 0->1 in the X and Y directions.

    xs1 = FSC_Normalize(xr, Position = [position[0], position[2]])
    ys1 = FSC_Normalize(yr, Position = [position[1], position[3]])

    ; Scale the plot data and axes into 0->1.

    oPlot->SetProperty, XCoord_Conv=xs1, YCoord_Conv=ys1
    oXAxis->SetProperty, XCoord_Conv=xs1, YCoord_Conv=ys1
    oXTitle->SetProperty, XCoord_Conv=xs1, YCoord_Conv=ys1
    oYTitle->SetProperty, XCoord_Conv=xs1, YCoord_Conv=ys1
    oYAxis->SetProperty, XCoord_Conv=xs1, YCoord_Conv=ys1
    oXAxisUp->SetProperty, XCoord_Conv=xs1,YCoord_Conv=ys1
    oYAxisUp->SetProperty, XCoord_Conv=xs1,YCoord_Conv=ys1
    if linesFlag then begin
      oLine1->SetProperty, XCoord_Conv=xs1,YCoord_Conv=ys1
      ;oLine2->SetProperty, XCoord_Conv=xs1,YCoord_Conv=ys1 
      ;oLine3->SetProperty, XCoord_Conv=xs1,YCoord_Conv=ys1
      ;
      oBack1->SetProperty, XCoord_Conv=xs1,YCoord_Conv=ys1
      oBack2->SetProperty, XCoord_Conv=xs1,YCoord_Conv=ys1 
      oBack3->SetProperty, XCoord_Conv=xs1,YCoord_Conv=ys1
    endif

   oModel = obj_new('IDLgrModel')
   oModel->remove, /all
   oModel->add, oPlot
   oModel->add, oXAxis
   oModel->add, oYAxis
   oModel->add, oYAxisUp
   oModel->add, oXAxisUp
   if linesFlag then begin
      oModel->add, oLine1
      ;oModel->add, oLine2
      ;oModel->add, oLine3
   endif
   xDim = n_elements(datas_x)
   yDim = n_elements(datas_y)
  
   start = 0;
   endpt = start + binSize
   for k = 0, xDim-1 do begin
      ;oPlotColorbar = obj_new('IDLgrPolygon', [start, start, endpt, endpt], [0, datas_y[k], datas_y[k],0], style = 2, color = [100, 100,100], XCOORD_CONV = xs1, YCOORD_CONV = ys1)
      if(k lt xDim-1) then begin
        oPlotColorbar = obj_new('IDLgrPolygon', [locBins[k],locBins[k], locBins[k+1], locBins[k+1]], [0, datas_y[k], datas_y[k],0], style = 2, color = [100, 100,100], XCOORD_CONV = xs1, YCOORD_CONV = ys1)
        oModel->add, oPlotColorbar
      end
;      start += binSize 
;      endpt = start + binSize
  endfor
;    ; MOR - 22 June 2011 - make histogram - BEGIN
;   tempnbins = veloLimitsVCL[n_elements(veloLimitsVCL)-2]+1
;   binsize = n_elements(datas_x) / (tempnbins -1)
;   start = xr[0]
;   endpt = start + binsize
;   for j=0,n_Elements(datas_x)-1 do begin
;
;    x = [start, start, endpt, endpt, start]
;    y = [0, datas_y[j], datas_y[j], 0, 0]
;    oPlotColorbar =  obj_new('IDLgrPolygon', [start, start, endpt, endpt], [0,datas_y[j] , datas_y[j],0], style = 2, color = [100, 100,100],  XCOORD_CONV = xs1, YCOORD_CONV = ys1)
;    oModel->add, oPlotColorbar       
;    start = start + binsize
;    endpt = start + binsize
;   endfor
;   ; MOR - 22 June 2011 - make histogram - END
  
  
  
  ; MOR  make histogram a single grey color - END
    ; MOR - need to place background after histogram plot in order to not eliminate the histogram
  if(linesFlag) then begin
      oModel->add, oBack1
      oModel->add, oBack2
      oModel->add, oBack3
   endif
; MOR  make histogram a single grey color - END

   oView = obj_new('IDLgrView', viewplane_rect = [xr[0], yr[0], 1.15, 1.15], color = [255, 255, 255])    
   oView->add, oModel
   oBuffer = obj_new('IDLgrBuffer', dimensions = [519, 380])
   oBuffer->draw, oView
   oOImage = oBuffer->Read()
   oOImage->GetProperty, data = outImage_1
   write_png, path, outImage_1
   
   ; MOR - destroy the objects
   obj_destroy, [oView, oBuffer]
   
end

