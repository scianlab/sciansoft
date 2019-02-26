;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageColocCostes
;
; PURPOSE:
;      Calculate Manders Colocalization Coefficient according to Costes et al. 2004
;
; AUTHOR:
;      Dr. Steffen Härtel (2005)
;      e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ImageColocCostes' )
;
; METHOHDS:
; Costes SV, Daelemans D, Cho EH, Dobbin Z, Pavlakis G, Lockett S.
; Automatic and quantitative measurement of protein-protein colocalization in live cells.
; Biophys J. 2004 Jun;86(6):3993-4003. Related Articles, Links
;_____________________________IOISIOI____________________


function C_sImageFilter_ImageColocCostes::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_ImageColocCostes::getColocValues
   return, *(*self.pParamStruct).pColocValues
end


function C_sImageFilter_ImageColocCostes::apply, image = image, imCh1 = imCh1,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

   dimI = size(image, /dim)
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Show Costes Histogram'))[0]
   fShowColocHist = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Show rUpDwn(T) Plot'))[0]
   fUpDwnPlot = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Show Scatter Histogram'))[0]
   fShowScatterHist = (*(*self.pParamStruct).pActive)[whParam]

   whParamColocWin = (where((*(*self.pParamStruct).pNames) eq 'Open Colocalization Window'))[0]
   fOpenColocWin = (*(*self.pParamStruct).pActive)[whParamColocWin]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Update Always'))[0]
   fUpdateAlways = (*(*self.pParamStruct).pActive)[whParam]

   if (n_elements(imCh1) eq 0) then begin
     ; get Image Object from Container
      chPos = abs(chPos - 1)
      oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
      if obj_valid(oImage) then begin
         clusterObj = oImage->getSegContainerObj(active = clusPos)
         if obj_valid(clusterObj) then segNum = (clusterObj->count() - 1) else segNum = -1
         for i = 0, segNum do begin
            segObj = clusterObj->get(position = i)
            if obj_valid(segObj) then if(obj_isa(segObj,'C_SIMAGEFILTER_IMAGEPSFSIGNALCREATOR')) then begin
               pParamStruct = segObj->getpParamStruct()
               dummy = (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]]
               (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]] = 0b
            endif
         endfor
         imCh1 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                             tPos = tPos,$
                                                             chPos = chPos,$
                                                             zPos = zPos,$
                                                             clusPos = clusPos,$
                                                             segPos = segPos-1,$
                                                             cut_x = cut_x, cut_y = cut_y)
         for i = 0, segNum do begin
            segObj = clusterObj->get(position = i)
            if obj_valid(segObj) then if obj_isa(segObj, 'C_SIMAGEFILTER_IMAGEPSFSIGNALCREATOR') then begin
               pParamStruct = segObj->getpParamStruct()
               (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]] = dummy
            endif
         endfor
      endif else imCh1 = image
   endif

   ; assure that image-values are inside the MinMax-Range
   maxIVal = 255.
   image = (image > 0) < maxIVal
   imCh1 = (imCh1 > 0) < maxIVal
   maxCh0 = max(image, min = minCh0)
   maxCh1 = max(imCh1, min = minCh1)
   xRange = [0,maxCh0]
   yRange = [0,maxCh1]

   ; consider only hist2D with dim ge 2
   if ((minCh0 lt (maxCh0-1)) and (minCh1 lt (maxCh1-1))) then begin

     ; don't calculate the same histogram twice
      hist2D = hist_2d(image, imCh1, bin1 = 1, bin2 = 1, min1 = 0, min2 = 0, max1 = maxIVal, max2 = maxIVal)
      if fUpdateAlways then (*(*self.pParamStruct).pHist2D)[0] = hist2D[0]-1
      if (total(hist2D ne *(*self.pParamStruct).pHist2D) ne 0) then *(*self.pParamStruct).pHist2D = hist2D else return, (image gt (*(*self.pParamStruct).pColocValues)[0]) + (imCh1 gt (*(*self.pParamStruct).pColocValues)[1])

     ; set stop-criterium & initial xy-Values

      stop = [minCh0, minCh1] + 1

      rAll = correlate(image[*], imch1[*])
      abFit = linFit(image[*], imch1[*], chisq = var)
      xabFit = indgen(maxCh0)
      yabFit = abFit[0] + xabFit * abFit[1]

      if (abFit[1] gt 1) then xyStep = [1./abFit[1], 1.] else xyStep = [1., abFit[1]]

      xy = [maxCh0, max(yabFit)] - 1

      if ((xy[0] gt stop[0]) or (xy[1] gt stop[1])) then begin

       ; start with at least 5 data points inside the upper quarter of the histogram.
         repeat (xy = (xy - xyStep) > stop) until ( (n_elements(where(hist2D[ xy[0]:*, xy[1]:*] ne 0)) ge 5) or ((xy[0] le stop[0]) and (xy[1] le stop[1])) )

         xyStart = xy
         rStat = fltArr(1,4) + [-1,-1,-1,-1]
         rStop = 0

         if fShowScatterHist then begin
            iPlot, image[*], imch1[*], xRange = xRange, yRange = yRange,$
                   xTitle = 'I [Ch(X)] (y = a + bx: a =' + strCompress(string(abFit[0])) + ', b =' + strCompress(string(abFit[1])) + ', PC =' + strCompress(string(rAll)) + ')',$
                   yTitle = 'I [Ch(abs(X-1))]',$
                   title = 's_ImageColoc |-> Scatter Histogram', identifier = id, /scatter, /no_save
            iPlot, xabFit, yabFit, xRange = xRange, yRange = yRange,$
                   color = [255,0,0], overplot = id
         endif

         if fShowColocHist then begin
            repeat begin

             ; upper Histogram-Regression
               xUPWhere = where(image ge xy[0], complement = xDWNWhere)
               yUPWhere = where(imCh1[xUPWhere] ge xy[1])

               xVec = (image[xUPWhere])[yUPWhere]
               yVec = (imCh1[xUPWhere])[yUPWhere]
               rUp = correlate(xVec,yVec)
               xyNext = (xy - xyStep) > stop

             ; lower histogram-regression
               yDWNWhere = where(imCh1[xUPWhere] lt xy[1], nyDWN)
               if (nyDWN gt 0) then begin
                  xVecDWN = [image[xDWNWhere], (image[xUPWhere])[yDWNWhere]]
                  yVecDWN = [imCh1[xDWNWhere], (imCh1[xUPWhere])[yDWNWhere]]
               endif else begin
                  xVecDWN = image[xDWNWhere]
                  yVecDWN = imCh1[xDWNWhere]
               endelse
               rDwn = correlate(xVecDWN,yVecDWN)

               print, 'rUp:', rUp
               print, 'rDwn:', rDwn
               print, 'xy:', xy
               print, 'Nextxy', xyNext

               rStat = [[rStat], fltarr(1,4) + [xy, rUp, rDwn]]

               xCutx = [xy[0], xy[0]]
               xCuty = [xy[1], maxCh1]
               yCutx = [xy[0], maxCh0]
               yCuty = [xy[1], xy[1]]

               if (*(*self.pParamStruct).pITool eq 'new') then begin
                  iPlot, image[*], imch1[*], xRange = xRange, yRange = yRange,$
                     xTitle = 'I [Ch0] (y = a + bx: a =' + strCompress(string(abFit[0])) + $
                                                    ', b =' + strCompress(string(abFit[1])) + $
                                                    ',rUp =' + strCompress(string(rUp)) + ')',$
                     yTitle = 'I [Ch1]',$
                     title = 's_ImageColoc |-> Costes Threshold Histogram', identifier = id, /scatter, /no_save
                  iPlot, xabFit, yabFit, xRange = xRange, yRange = yRange,color = [0,255,0], overplot = id
                  iPlot, xVec, yVec, xRange = xRange, yRange = yRange,color = [255,0,0], /scatter, overplot = id
                  iPlot, xCutx, xCuty, xRange = xRange, yRange = yRange,color = [255,0,0], linestyle = 0, overplot = id
                  iPlot, yCutx, yCuty, xRange = xRange, yRange = yRange,color = [255,0,0], linestyle = 0, overplot = id
                  *(*self.pParamStruct).pITool = id
               endif else begin
                  itCurrent, *(*self.pParamStruct).pITool
                  iPlot, image[*], imch1[*], xRange = xRange, yRange = yRange, view_number = 1,$
                     xTitle = 'I [Ch0] (y = a + bx: a =' + strCompress(string(abFit[0])) + ', b =' + strCompress(string(abFit[1])) + ', rUp =' + strCompress(string(rUp)) + ')',$
                     yTitle = 'I [Ch1]', identifier = *(*self.pParamStruct).pITool, color = [0,0,0], /scatter
                  iPlot, xVec, yVec, xRange = xRange, yRange = yRange,color = [255,0,0], /scatter, overplot = *(*self.pParamStruct).pITool
                  iPlot, xabFit, yabFit, xRange = xRange, yRange = yRange,color = [0,255,0], overplot = *(*self.pParamStruct).pITool
                  iPlot, xCutx, xCuty, xRange = xRange, yRange = yRange,color = [255,0,0], overplot = *(*self.pParamStruct).pITool
                  iPlot, yCutx, yCuty, xRange = xRange, yRange = yRange,color = [255,0,0], overplot = *(*self.pParamStruct).pITool
               endelse

               xy = xyNext
            endRep until ((rDwn le rStop) or ((xy[0] le stop[0]) and (xy[1] le stop[1])))
         endif else begin
            repeat begin
             ; upper Histogram-Regression
               xUPWhere = where(image ge xy[0], complement = xDWNWhere)
               yUPWhere = where(imCh1[xUPWhere] ge xy[1])

               xVec = (image[xUPWhere])[yUPWhere]
               yVec = (imCh1[xUPWhere])[yUPWhere]
               rUp = correlate(xVec,yVec)
               xyNext = (xy - xyStep) > stop

             ; lower histogram-regression
               yDWNWhere = where(imCh1[xUPWhere] lt xy[1], nyDWN)
               if (nyDWN gt 0) then begin
                  xVecDWN = [image[xDWNWhere], (image[xUPWhere])[yDWNWhere]]
                  yVecDWN = [imCh1[xDWNWhere], (imCh1[xUPWhere])[yDWNWhere]]
               endif else begin
                  xVecDWN = image[xDWNWhere]
                  yVecDWN = imCh1[xDWNWhere]
               endelse
               rDwn = correlate(xVecDWN,yVecDWN)

               print, 'rUp:', rUp
               print, 'rDwn:', rDwn
               print, 'xy:', xy
               print, 'Nextxy', xyNext

               rStat = [[rStat], fltarr(1,4) + [xy, rUp, rDwn]]

               xy = xyNext
            endRep until ((rDwn le rStop) or ((xy[0] le stop[0]) and (xy[1] le stop[1])))
         endelse

         rStat = rStat[1:*,*]
         if fUpDwnPlot then begin
            yrRange = [min(rStat[*,2:3]) < rAll < 0,  max(rStat[*,2:3]) > rAll > 0]
            iPlot, rStat[*,0], rStat[*,0]*0+rAll, xRange = xRange, yRange = yrRange ,$
                xTitle = 'I [Ch0]: Costes stop at T[Ch0] = ' + strCompress(string(xy[0])) + ', T[Ch1] = ' + strCompress(string(xy[1])) ,$
                yTitle = 'r [PC]',$
                title = 's_ImageColoc |-> Costes Threshold rAll, rUp, rDwn',$
                name = 'rAll', identifier = id2, color = [0,0,0], /no_save, /insert_legend
            iPlot, rStat[*,0], rStat[*,2], name = 'rUp', xRange = xRange, yRange = yrRange, color = [255,0,0], linestyle = 0, overplot = id2, /insert_legend
            iPlot, rStat[*,0], rStat[*,3], name = 'rDwn', xRange = xRange, yRange = yrRange, color = [0,255,0], linestyle = 0, overplot = id2, /insert_legend
         endif

         if fOpenColocWin then begin
            images = make_array(2, dimI[0], dimI[1], type = size(image, /type))
            images[0,*,*] = image
            images[1,*,*] = imch1

            dimIs = size(images, /dim)
            if widget_info(long((*(*self.pParamStruct).pValues)[whParamColocWin]), /valid_id) then begin
              if ( ((*(*self.pParamStruct).pColocWinParam)[0] ne dimIs[0]) or ((*(*self.pParamStruct).pColocWinParam)[1] ne dimIs[1]) $
                or ((*(*self.pParamStruct).pColocWinParam)[2] ne dimIs[2]) ) then begin
                if (widget_info(long((*(*self.pParamStruct).pValues)[whParamColocWin]), /valid_id)) then widget_control, (*(*self.pParamStruct).pValues)[whParamColocWin], /destroy
                s_Coloc_Window, images, stack_tlb = stack_tlb, application_tlb = application_tlb
                (*(*self.pParamStruct).pValues)[whParamColocWin] = application_tlb
                *(*self.pParamStruct).pColocWinParam = [dimIs[0], dimIs[1], dimIs[2]]
              endif else s_Coloc_update, (*(*self.pParamStruct).pValues)[whParamColocWin], newImages = images
            endif else begin
              s_Coloc_Window, images, stack_tlb = stack_tlb, application_tlb = application_tlb
              (*(*self.pParamStruct).pValues)[whParamColocWin] = application_tlb
              *(*self.pParamStruct).pColocWinParam = [dimIs[0], dimIs[1], dimIs[2]]
            endelse
         endif else begin
            if widget_info(long((*(*self.pParamStruct).pValues)[whParamColocWin]), /valid_id) then widget_control, (*(*self.pParamStruct).pValues)[whParamColocWin], /destroy
            (*(*self.pParamStruct).pActive)[whParamColocWin] = 0
            (*(*self.pParamStruct).pValues)[whParamColocWin] = 0
         endelse

         (*(*self.pParamStruct).pColocValues)[0:1] = xy
         (*(*self.pParamStruct).pColocValues)[2] = total(xVec) / total(image)
         (*(*self.pParamStruct).pColocValues)[3] = total(yVec) / total(imCh1)
      endif

      print, '___________________________________'
      print, 'Results of Costes'
      print, 'rStop, rUp, rDwn', rStop, rUp, rDwn
      print, 'Costes Thresholds: ', (*(*self.pParamStruct).pColocValues)[0], (*(*self.pParamStruct).pColocValues)[1]
      print, 'Manders Coefficient Channel 0: ', (*(*self.pParamStruct).pColocValues)[2]
      print, 'Manders Coefficient Channel 1: ', (*(*self.pParamStruct).pColocValues)[3]
      print, 'maxR: ', max(rStat[*,2]), max(rStat[*,3])

      openW,2, 'c:\RSI\CostesRStat.dat'
      printF, 2, ['x', 'y', 'rUp', 'rDwn']
      printF, 2, transpose(rStat)
      close, 2
      openW,2, 'c:\RSI\CostesCh0.dat'
      printF, 2, transpose(image[*])
      close, 2
      openW,2, 'c:\RSI\CostesCh1.dat'
      printF, 2, transpose(imCh1[*])
      close, 2
   endif
   return, (image gt (*(*self.pParamStruct).pColocValues)[0]) + (imCh1 gt (*(*self.pParamStruct).pColocValues)[1])
end


function C_sImageFilter_ImageColocCostes::init
   filterStruct = {Name:'C_ImageColocCostes',$; filter Name.
              pWidgetType:ptr_new(),$  ; Pointer on filter Parameter Names.
              pNames:ptr_new(),$; Pointer on filter Parameter Names.
              pActive:ptr_new(),$; Pointer on filter Parameter Active Bool.
              pMin:ptr_new(),$ ; Pointer on filter Parameter Min_Values.
              pMax:ptr_new(),$ ; Pointer on filter Parameter Max_Values.
              pValues:ptr_new(),$ ; Pointer on filter Parameter Values.
              pColocValues:ptr_new(),$ ; Pointer on Colocalization Values:[Threshold, MandersX, MandersY]
              pColocWinParam:ptr_new(),$ ;Pointer on Parameter Vector of Colocalization Window.
              pITool:ptr_new(),$ ; Pointer on Colocalization Values:[Threshold, MandersX, MandersY]
              pHist2D:ptr_new()}; Pointer on Parameter Vector of Colocalization Window.

    filterParamWidgetType = make_array(5, /string, value = 'widget_slider')
    filterParamNames = ['Show Costes Histogram','Show rUpDwn(T) Plot','Show Scatter Histogram','Open Colocalization Window','Update Always']
    filterParamActive = [0,1,0,1,0]
    filterParamMin = [0,0,0,0,0]
    filterParamMax = [1000,1000,1000,1000,1]
    filterParamValues = [0,0,0,0,0]

    a = [-1.]
    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
    filterStruct.pColocValues = ptr_new([a,a,a,a])
    filterStruct.pColocWinParam = ptr_new(a)
    filterStruct.pITool = ptr_new('new', /no_copy)
    filterStruct.pHist2D = ptr_new(a, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageColocCostes__define
   tmp = {C_sImageFilter_ImageColocCostes, pParamStruct:ptr_new(), inherits C_sImageFilter}
end