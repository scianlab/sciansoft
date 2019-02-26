;_____________________________IOISIOI____________________
; NAME:
;     C_sImageFilter_ImageCrossCorrelation
;
; PURPOSE:
;      - ImageColocalization-Filter-Class.
;
; AUTHOR:
;    Dr. Steffen Härtel (2005)
;    e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;    result = obj_new('C_sImageFilter_ImageCrossCorrelation' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageCrossCorrelation::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


pro C_sImageFilter_ImageCrossCorrelation::getMatrixValues, ch0_vect = ch0_vect, ch1_vect = ch1_vect
   dimM = size(*(*self.pParamStruct).pCorrelVectXAxis, /dim)
   if (n_elements(dimM) lt 1) then return
   ch0_vect = (*(*self.pParamStruct).pCorrelVectXAxis)
   ch1_vect = (*(*self.pParamStruct).pCorrelVectYAxis)
end


function C_sImageFilter_ImageCrossCorrelation::apply, image = image,$
                            selectedStackObject = selectedStackObject,$
                            stack_tlb = stack_tlb,$
                            tPos = tPos,$
                            chPos = chPos,$
                            zPos = zPos,$
                            clusPos = clusPos,$
                            segPos = segPos,$
                            cut_x = cut_x, cut_y = cut_y

   whParam = where((*(*self.pParamStruct).pNames) eq 'Pixel Radius')
   pixRad = round((*(*self.pParamStruct).pValues)[whParam[0]])
   whParam = where((*(*self.pParamStruct).pNames) eq 'Cross Correlation')
   fCrossCorrelation = (*(*self.pParamStruct).pActive)[whParam[0]]
   whParam = where((*(*self.pParamStruct).pNames) eq 'Show Plot')
   fShowPlot = (*(*self.pParamStruct).pActive)[whParam[0]]

      ; get Pixel Size / Sampling Distance of the Image
   selectedStackObject->get, pParamStruct = pParamStruct
   xPixSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] / $
              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
   yPixSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] / $
              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
   pixSize = (xPixSize + yPixSize) / 2.

   count = 0
   pixRadDist = pixRad * pixSize
   for k = -pixRad, pixRad do for l = -pixRad, pixRad do if (sqrt((k*xPixSize)^2+(l*xPixSize)^2) le pixRadDist) then count = count + 1

      ; define Correlation Vectors
   if not(ptr_valid((*self.pParamStruct).pCorrelVectXAxis)) then (*self.pParamStruct).pCorrelVectXAxis = ptr_new(make_array(count, /float), /no_copy) $
      else *(*self.pParamStruct).pCorrelVectXAxis = make_array(count, /float)
   if not(ptr_valid((*self.pParamStruct).pCorrelVectYAxis)) then (*self.pParamStruct).pCorrelVectYAxis = ptr_new(make_array(count, /float), /no_copy) $
      else *(*self.pParamStruct).pCorrelVectYAxis = make_array(count, /float)

   dimI = size(image, /dim)
   meanF = (moment(image))[0]
   meanFSquare = meanF^2
   normI = image - meanF
   count = 0
   for k = -pixRad, pixRad do for l = -pixRad, pixRad do begin
      pixDist = sqrt((k*xPixSize)^2+(l*xPixSize)^2)
      if (pixDist le pixRadDist) then begin
         (*(*self.pParamStruct).pCorrelVectXAxis)[count] = pixDist
         (*(*self.pParamStruct).pCorrelVectYAxis)[count] = (moment( normI * shift(normI, k, l)))[0] / meanFSquare
         count += 1
      endif
   endfor

   ch0_xAxis = *(*self.pParamStruct).pCorrelVectXAxis
   ch0_yAxis = *(*self.pParamStruct).pCorrelVectYAxis

      ; Gauss-Fit of G0
   GVect = [reverse(ch0_yAxis), ch0_yAxis[1:*]]
   xVect = [-reverse(ch0_xAxis), ch0_xAxis[1:*]]
   maxG = max(GVect)
   maxAt = 0.
   sdG = .12
   PSFGauss = gaussFit(xVect, GVect, fitValues, estimates = [maxG, 0., sdG], nterms = 3, chisq = chisq, sigma = sigma)
   print, '_______________________'
   print, 'Result of Gauss-Fit for G(x): A0*exp(-.5*((x-A1)/A2)^2)'
   print, 'G0: ', strCompress(string(fitValues[0])), '  A1: ', strCompress(string(fitValues[1])), '  A2: ', strCompress(string(fitValues[2]))
   print, 'chisq: ', chisq, 'sigma: ', sigma
   print, 'r0 = sqrt(2)*a2: ', sqrt(2.)*fitValues[2]
   ch0_xAxisGauss = make_array(pixRad*10, /float, /index)/10. * pixSize
   ch0_yAxisGauss = fitValues[0] * exp(-.5*((ch0_xAxisGauss - fitValues[1])/fitValues[2])^2)

   if fShowPlot then begin
      live_info, error = error, properties = prop, window_in = 'Auto-Correlation Selected Channel'
      if (error ne '') then begin
       style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type : 3, symbol_size : 0.01},$
                      visualization_properties = {color : 'Light Red'},$
                      xAxis_properties = {axisTitle :'r0(1/e²) at' +  string(sqrt(2.)*fitValues[2])+'[µm];' +  string(sqrt(2.)*fitValues[2]/pixSize)+'[dx]', exact: 1, compute_range : 0},$
                      yAxis_properties = {axisTitle :'G0 [dr]', exact: 1, compute_range : 0},$
                      legend_properties = {hide :1})
       live_plot, ch0_yAxis, independent = ch0_xAxis,/scatter, draw_dimension = [300,300], /no_select,$
                  style = style, title = 'Auto-Correlation Selected Channel'
;       live_oPlot, ch0_yAxisMean, independent = ch0_xAxisMean, /no_select,$
;                   subType = 'LinePlot',window_in = 'Auto-Correlation Selected Channel'
       live_oPlot, ch0_yAxisGauss, independent = ch0_xAxisGauss, /no_select,$
                   subType = 'LinePlot',window_in = 'Auto-Correlation Selected Channel'
      endif else begin
       live_control, ch0_XAxisGauss, /update, window_in = 'Auto-Correlation Selected Channel'
       live_control, ch0_yAxisGauss, /update, window_in = 'Auto-Correlation Selected Channel'
;       live_control, ch0_xAxisMean, /update, window_in = 'Auto-Correlation Selected Channel'
;       live_control, ch0_yAxisMean, /update, window_in = 'Auto-Correlation Selected Channel'
       live_control, ch0_yAxis, /update, window_in = 'Auto-Correlation Selected Channel'
       live_control, ch0_xAxis, /update, window_in = 'Auto-Correlation Selected Channel'
      endelse
      live_info, 'CH0_XAXIS Axis', properties = variable, window_in = 'Auto-Correlation Selected Channel'
      variable.minRange = min(ch0_xAxis) - .05*(max(ch0_xAxis)-min(ch0_xAxis))
      variable.maxRange = max(ch0_xAxis) + .05*(max(ch0_xAxis)-min(ch0_xAxis))
      variable.axistitle = 'r0(1/e²) at' +  strCompress(string(sqrt(2.)*fitValues[2]))+'[µm];' +  strCompress(string(sqrt(2.)*fitValues[2]/pixSize))+'[dx]'
      live_control, 'CH0_XAXIS Axis', properties = variable, window_in = 'Auto-Correlation Selected Channel'
      live_info, 'Y Axis', properties = variable, window_in = 'Auto-Correlation Selected Channel'
      variable.minRange = min(ch0_yAxis) - .05*(max(ch0_yAxis)-min(ch0_yAxis))
      variable.maxRange = max(ch0_yAxis) + .05*(max(ch0_yAxis)-min(ch0_yAxis))
      live_control, 'Y Axis', properties = variable, window_in = 'Auto-Correlation Selected Channel'
   endif

   if (fCrossCorrelation) then begin
      selectedchPos = chPos
      case chPos of
         0: chPos = 1
         else: chPos = (chPos -1) > 0
      endcase

      oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
      if obj_valid(oImage) then begin
       clusterObj = oImage->getSegContainerObj(active = clusPos)
       if obj_valid(clusterObj) then segNum = (clusterObj->count() - 1) else segNum = -1
       for i = 0, segNum do begin
        segObj = clusterObj->get(position = i)
        if obj_valid(segObj) then if(obj_isa(segObj, 'C_SIMAGEFILTER_IMAGEPSFSIGNALCREATOR')) then begin
           pParamStruct = segObj->getpParamStruct()
           dummy = (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]]
           (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]] = 0b
           segObj->set, pParamStruct = pParamStruct
        endif
       endfor
       image_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
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
             segObj->set, pParamStruct = pParamStruct
          endif
       endfor
      endif else image_2 = image

      meanF_2 = (moment(image_2))[0]
      meanFSquare_2 = meanF_2^2
      normI_2 = image_2 - meanF_2
      expImage_2 = s_Expand_Mirror(normI_2, pixRad, mirrorType = 2)
      count = 0
      for k = - pixRad, pixRad do for l = - pixRad, pixRad do begin
         pixDist = sqrt((k*xPixSize)^2+(l*xPixSize)^2)
         if (pixDist le pixRadDist) then begin
            (*(*self.pParamStruct).pCorrelVectXAxis)[count] = pixDist
            (*(*self.pParamStruct).pCorrelVectYAxis)[count] = (moment( normI_2 * expImage_2( pixRad + k : pixRad+k+dimI[0]-1, pixRad+l : pixRad+l+dimI[1]-1 ) ))[0] / meanFSquare_2
         count += 1
         endif
      endfor

      ch1_xAxis = *(*self.pParamStruct).pCorrelVectXAxis
      ch1_yAxis = *(*self.pParamStruct).pCorrelVectYAxis

       ; Gauss-Fit of G0
      GVect = [reverse(ch1_yAxis), ch1_yAxis[1:*]]
      xVect = [-reverse(ch1_xAxis), ch1_xAxis[1:*]]
      maxG = max(GVect)
      maxAt = 0.
      sdG = .12
      PSFGauss = gaussFit(xVect, GVect, fitValues, estimates = [maxG, 0., sdG], nterms = 3, chisq = chisq, sigma = sigma)
      print, '_______________________'
      print, 'Result of Gauss-Fit for G(x): A0*exp(-.5*((x-A1)/A2)^2)'
      print, 'G0: ', strCompress(string(fitValues[0])), '  A1: ', strCompress(string(fitValues[1])), '  A2: ', strCompress(string(fitValues[2]))
      print, 'chisq: ', chisq, 'sigma: ', sigma
      print, 'r0 = sqrt(2)*a2: ', sqrt(2.)*fitValues[2]
      ch1_xAxisGauss = make_array(pixRad*10, /float, /index)/10. * pixSize
      ch1_yAxisGauss = fitValues[0] * exp(-.5*((ch1_xAxisGauss - fitValues[1])/fitValues[2])^2)

      if fShowPlot then begin
       live_info, error = error, properties = prop, window_in = 'Auto-Correlation Second Channel'
       if (error ne '') then begin
          style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type : 3, symbol_size : 0.01},$
                       visualization_properties = {color : 'Light Red'},$
                       xAxis_properties = {axisTitle :'correlation distance [dx]', exact: 1, compute_range : 0},$
                       yAxis_properties = {axisTitle :'G1 [dr]', exact: 1, compute_range : 0},$
                       legend_properties = {hide :1})
          live_plot, ch1_yAxis, independent = ch1_xAxis,$
                       /scatter, draw_dimension = [300,300], /no_select,$
                       style = style, title = 'Auto-Correlation Second Channel'
;          live_oPlot, ch1_yAxisMean, independent = ch1_xAxisMean, /no_select,$
;                       subType = 'LinePlot', window_in = 'Auto-Correlation Second Channel'
          live_oPlot, ch1_yAxisGauss, independent = ch1_xAxisGauss, /no_select,$
                       subType = 'LinePlot', window_in = 'Auto-Correlation Second Channel'
       endif else begin
          live_control, ch1_yAxis, /update, window_in = 'Auto-Correlation Second Channel'
          live_control, ch1_xAxis, /update, window_in = 'Auto-Correlation Second Channel'
          live_control, ch1_yAxisGauss, /update, window_in = 'Auto-Correlation Second Channel'
          live_control, ch1_xAxisGauss, /update, window_in = 'Auto-Correlation Second Channel'
       endelse
       live_info, 'CH1_XAXIS Axis', properties = variable, window_in = 'Auto-Correlation Second Channel'
       variable.minRange = min(ch1_xAxis) - .05*(max(ch1_xAxis)-min(ch1_xAxis))
       variable.maxRange = max(ch1_xAxis) + .05*(max(ch1_xAxis)-min(ch1_xAxis))
       variable.axistitle = 'r0(1/e²) at' +  strCompress(string(sqrt(2.)*fitValues[2]))+'[µm]/' +  strCompress(string(sqrt(2.)*fitValues[2]/pixSize))+'[dx]'
       live_control, 'CH1_XAXIS Axis', properties = variable, window_in = 'Auto-Correlation Second Channel'
       live_info, 'Y Axis', properties = variable, window_in = 'Auto-Correlation Second Channel'
       variable.minRange = min(ch1_yAxis) - .05*(max(ch1_yAxis)-min(ch1_yAxis))
       variable.maxRange = max(ch1_yAxis) + .05*(max(ch1_yAxis)-min(ch1_yAxis))
       live_control, 'Y Axis', properties = variable, window_in = 'Auto-Correlation Second Channel'
      endif

      count = 0
      meanFSquare = meanF * meanF_2
      for k = - pixRad, pixRad do for l = - pixRad, pixRad do begin
         pixDist = sqrt((k*xPixSize)^2+(l*xPixSize)^2)
         if (pixDist le pixRadDist) then begin
            (*(*self.pParamStruct).pCorrelVectXAxis)[count] = pixDist
            (*(*self.pParamStruct).pCorrelVectYAxis)[count] = (moment( normI * expImage_2( pixRad + k : pixRad+k+dimI[0]-1, pixRad+l : pixRad+l+dimI[1]-1 ) ))[0] / meanFSquare
            count = count + 1
         endif
      endfor

      chCG_xAxis = *(*self.pParamStruct).pCorrelVectXAxis
      chCG_yAxis = *(*self.pParamStruct).pCorrelVectYAxis

       ; Gauss-Fit of GX
      GVect = [reverse(chCG_yAxis), chCG_yAxis[1:*]]
      xVect = [-reverse(chCG_xAxis), chCG_xAxis[1:*]]
      maxG = max(GVect)
      maxAt = 0.
      sdG = .12
      PSFGauss = gaussFit(xVect, GVect, fitValues, estimates = [maxG, 0., sdG, 0.], nterms = 4, chisq = chisq, sigma = sigma)
      print, '_______________________'
      print, 'Result of Gauss-Fit for G(x): A0*exp(-.5*((x-A1)/A2)^2) + A3'
      print, 'G0: ', strCompress(string(fitValues[0])), '  A1: ', strCompress(string(fitValues[1])), '  A2: ', strCompress(string(fitValues[2])), '  A3: ', strCompress(string(fitValues[3]))
      print, 'chisq: ', chisq, 'sigma: ', sigma
      print, 'r0 = sqrt(2)*a2: ', sqrt(2.)*fitValues[2]
      chCG_xAxisGauss = make_array(pixRad*10, /float, /index)/10. * pixSize
      chCG_yAxisGauss = fitValues[0] * exp(-.5*((chCG_xAxisGauss - fitValues[1])/fitValues[2])^2) + fitValues[3]

      if (fShowPlot) then begin
       live_info, error = error, properties = prop, window_in = 'Cross-Correlation'
       if (error ne '') then begin
         style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type : 3, symbol_size : 0.01},$
                       visualization_properties = {color : 'Light Red'},$
                       xAxis_properties = {axisTitle :'correlation distance [dx]', exact: 1, compute_range : 0},$
                       yAxis_properties = {axisTitle :'Cross-G [dx]', exact: 1, compute_range : 0},$
                       legend_properties = {hide :1})
         live_plot, chCG_yAxis, independent = chCG_xAxis,$
                       /scatter, draw_dimension = [300,300], /no_select,$
                       style = style, title = 'Cross-Correlation'
         live_oPlot, chCG_yAxisGauss, independent = chCG_xAxisGauss, /no_select,$
                       subType = 'LinePlot',$
                       window_in = 'Cross-Correlation'
       endif else begin
          live_control, chCG_yAxis, /update, window_in = 'Cross-Correlation'
          live_control, chCG_xAxis, /update, window_in = 'Cross-Correlation'
          live_control, chCG_yAxisGauss, /update, window_in = 'Cross-Correlation'
          live_control, chCG_xAxisGauss, /update, window_in = 'Cross-Correlation'
       endelse
       live_info, 'CHCG_XAXIS Axis', properties = variable, window_in = 'Cross-Correlation'
       variable.minRange = min(chCG_xAxis) - .05*(max(chCG_xAxis)-min(chCG_xAxis))
       variable.maxRange = max(chCG_xAxis) + .05*(max(chCG_xAxis)-min(chCG_xAxis))
       variable.axistitle = '[dx], r0(1/e²) at' +  strCompress(string(sqrt(2.)*fitValues[2]))+'[µm]' +  strCompress(string(sqrt(2.)*fitValues[2]/pixSize))+'[dx]'
       live_control, 'CHCG_XAXIS Axis', properties = variable, window_in = 'Cross-Correlation'
       live_info, 'Y Axis', properties = variable, window_in = 'Cross-Correlation'
       variable.minRange = min(chCG_yAxis) - .05*(max(chCG_yAxis)-min(chCG_yAxis))
       variable.maxRange = max(chCG_yAxis) + .05*(max(chCG_yAxis)-min(chCG_yAxis))
       live_control, 'Y Axis', properties = variable, window_in = 'Cross-Correlation'
      endif
   endif
   return, image
end


function C_sImageFilter_ImageCrossCorrelation::init
   filterStruct = {Name: 'C_ImageCrossCorrelation',$   ;  filter Name.
                     pWidgetType: ptr_new(),$    ; Pointer on filter Parameter Names.
                     pNames: ptr_new(),$   ; Pointer on filter Parameter Names.
                     pActive: ptr_new(),$   ; Pointer on filter Parameter Active Bool.
                     pMin: ptr_new(),$   ; Pointer on filter Parameter Min_Values.
                     pMax: ptr_new(),$   ; Pointer on filter Parameter Max_Values.
                     pValues: ptr_new(),$   ; Pointer on filter Parameter Values.
                     pCorrelVectXAxis: ptr_new(),$   ; Pointer on xAxisVector.
                     pCorrelVectYAxis: ptr_new()}   ; Pointer on yAxisVector.

   filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
   filterParamNames = ['Pixel Radius', 'Cross Correlation' ,'Show Plot']
   filterParamActive = [1, 1, 1]
   filterParamMin = [2, 0, 0]
   filterParamMax = [100, 1, 1]
   filterParamValues = [5, 1, 1]

   a = [-1]
   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
   filterStruct.pCorrelVectXAxis = ptr_new(1.*a)
   filterStruct.pCorrelVectYAxis = ptr_new(a, /no_copy)
   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_ImageCrossCorrelation__define
   tmp = {C_sImageFilter_ImageCrossCorrelation, pParamStruct: ptr_new(), inherits C_sImageFilter}
end