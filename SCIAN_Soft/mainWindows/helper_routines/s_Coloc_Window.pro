pro s_Coloc_mergeColorTables, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       if state.fMergeColorTables then begin

           ; get Channel0 Image only
          oImage = (state.thisView->get(position = 0))->getByName('Image_Channel1')
          oImage->getProperty, data = thisData, /no_copy
          dummy = thisData[3,*,*]
          thisData[3,*,*] = 0.
          oImage->setProperty, data = thisData, /no_copy
          state.thisWindow->draw, state.thisView
          oImage->getProperty, data = thisData, /no_copy
          thisData[3,*,*] = dummy
          oImage->setProperty, data = thisData, /no_copy
          state.thisWindow->getProperty, image_data = snapChannel0

           ; get Channel1 Image only
          oImage = (state.thisView->get(position = 0))->getByName('Image_Channel0')
          oImage->getProperty, data = thisData, /no_copy

          dummy = thisData[3,*,*]
          thisData[3,*,*] = 0.
          oImage->setProperty, data = thisData, /no_copy
          state.thisWindow->draw, state.thisView
          oImage->getProperty, data = thisData, /no_copy
          thisData[3,*,*] = dummy
          oImage->setProperty, data = thisData, /no_copy
          state.thisWindow->getProperty, image_data = snapChannel1

;          snapChannel1 = shift(snapChannel1, 0,4,-2)

          snapChannel0 <= 255
          snapChannel1 <= 255
          mergeChannel = (snapChannel0 + snapChannel1) < 255

          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 0'
          if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> Channel 0" does not exist.  Operation not performed.')) then $
             live_image, snapChannel0, draw_dimensions = [512,512], dimensions = [1.,1.], title = 's_ImageColoc |-> Channel 0' $
             else live_control, snapChannel0, /update, window_in = 's_ImageColoc |-> Channel 0'
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 1'
          if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> Channel 1" does not exist.  Operation not performed.')) then $
             live_image, snapChannel1, draw_dimensions = [512,512], dimensions = [1.,1.], title = 's_ImageColoc |-> Channel 1' $
             else live_control, snapChannel1, /update, window_in = 's_ImageColoc |-> Channel 1'
          live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Merge Channel'
          if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> Merge Channel" does not exist.  Operation not performed.')) then $
             live_image, mergeChannel, draw_dimensions = [512,512], dimensions = [1.,1.], title = 's_ImageColoc |-> Merge Channel' $
             else live_control, mergeChannel, /update, window_in = 's_ImageColoc |-> Merge Channel'
       endif else begin
          newImages = state.images
          widget_control, ev.top, set_uValue = state, /no_copy
             s_Coloc_update, ev.top, newImages = newImages
          widget_control, ev.top, get_uValue = state, /no_copy
       endelse
    widget_control, ev.top, set_uValue = state, /no_copy
end


function s_Coloc_getDimI, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      dimI = state.dimI
   widget_control, wTopBase, set_uValue = state, /no_copy
   return, dimI
end


pro s_Coloc_getChannelValues, wTopBase, getChannelValuesOnly = getChannelValuesOnly, ch0_vect = ch0_vect, ch1_vect = ch1_vect
     widget_control, wTopBase, get_uValue = state, /no_copy

         ch0_vect = make_array(state.dimI[1] * state.dimI[2], /byte) + state.images[0,*,*]
         ch1_vect = make_array(state.dimI[1] * state.dimI[2], /byte) + state.images[1,*,*]

         if (n_elements(getChannelValuesOnly) ne 0) then begin
           widget_control, wTopBase, set_uValue = state, /no_copy
           return
         endif
         wereValues = where(ch0_vect ge (state.bottomValues)[0])
         if (wereValues[0] ne -1) then begin
           ch0_vect = ch0_vect[wereValues]
           ch1_vect = ch1_vect[wereValues]
         endif
         wereValues = where(ch1_vect ge (state.bottomValues)[1])
         if (wereValues[0] ne -1) then begin
           ch0_vect = ch0_vect[wereValues]
           ch1_vect = ch1_vect[wereValues]
         endif
         wereValues = where(ch0_vect le (state.topValues)[0])
         if (wereValues[0] ne -1) then begin
           ch0_vect = ch0_vect[wereValues]
           ch1_vect = ch1_vect[wereValues]
         endif
         wereValues = where(ch1_vect le (state.topValues)[1])
         if (wereValues[0] ne -1) then begin
           ch0_vect = ch0_vect[wereValues]
           ch1_vect = ch1_vect[wereValues]
         endif
     widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_calcuateColocalizationValues, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy

         ;get channel-values
       ch0_vect = make_array(state.dimI[1] * state.dimI[2], /long) + state.images[0,*,*]
       ch1_vect = make_array(state.dimI[1] * state.dimI[2], /long) + state.images[1,*,*]

         ;make ROI
       ROIArray = make_array(state.dimI[1], state.dimI[2], /byte) + state.ROIMasks[0,*,*] + state.ROIMasks[1,*,*] * 2
       whereChannel_0 = where(state.ROIMasks[0,*,*])
       whereChannel_1 = where(state.ROIMasks[1,*,*])

         ;'area [pixel]'
       state.colocalizationParams[0,*] = 1.*[n_elements(ROIArray),$
                                             total(state.ROIMasks[0,*,*]),$
                                             total(state.ROIMasks[1,*,*]),$
                                             total(ROIArray eq 3),$
                                             total(ROIArray gt 0),$
                                             total(ROIArray eq 1),$
                                             total(ROIArray eq 2),$
                                             total(ROIArray eq 0)]
         ;'area [%total]'
       state.colocalizationParams[1,*] = state.colocalizationParams[0,*] / state.colocalizationParams[0,0] * 100.
         ;'area [%ROI_0]'
       state.colocalizationParams[2,*] = state.colocalizationParams[0,*] / state.colocalizationParams[0,1] * 100.
         ;'area [%ROI_1]'
       state.colocalizationParams[3,*] = state.colocalizationParams[0,*] / state.colocalizationParams[0,2] * 100.

         ;'Pearsons Correlation Coefficient in Intersection' ;n = long(n_elements(whereBoth))
                                            ;state.colocalizationParams.k = ( (n * total(ch0_vect[whereBoth] * ch1_vect[whereBoth])) -  (total(ch0_vect[whereBoth]) * total(ch1_vect[whereBoth])) ) / $
                                            ;sqrt( (n * total(ch0_vect[whereBoth]^2) - (total(ch0_vect[whereBoth]))^2) * (n * total(ch1_vect[whereBoth]^2) - (total(ch1_vect[whereBoth]))^2) )
       whereBoth = where(ROIArray eq 3)
       if (n_elements(ch0_vect) gt 0) then state.colocalizationParams[4,0] = correlate(ch0_vect, ch1_vect, /double) $
         else state.colocalizationParams[4,0] = 0.
       if (whereChannel_0[0] ne -1) then state.colocalizationParams[4,1] = correlate(ch0_vect[whereChannel_0], ch1_vect[whereChannel_0], /double) $
         else state.colocalizationParams[4,1] = 0.
       if (whereChannel_1[0] ne -1) then state.colocalizationParams[4,2] = correlate(ch0_vect[whereChannel_1], ch1_vect[whereChannel_1], /double) $
         else state.colocalizationParams[4,2] = 0.
       if (whereBoth[0] ne -1) then state.colocalizationParams[4,3] = correlate(ch0_vect[whereBoth], ch1_vect[whereBoth], /double) $
         else state.colocalizationParams[4,3] = 0.
       wherePos = where(ROIArray gt 0)
       if (wherePos[0] ne -1) then state.colocalizationParams[4,4] = correlate(ch0_vect[wherePos], ch1_vect[wherePos], /double) $
         else state.colocalizationParams[4,4] = 0.
       wherePos = where(ROIArray eq 1)
       if (wherePos[0] ne -1) then state.colocalizationParams[4,5] = correlate(ch0_vect[wherePos], ch1_vect[wherePos], /double) $
         else state.colocalizationParams[4,5] = 0.
       wherePos = where(ROIArray eq 2)
       if (wherePos[0] ne -1) then state.colocalizationParams[4,6] = correlate(ch0_vect[wherePos], ch1_vect[wherePos], /double) $
         else state.colocalizationParams[4,6] = 0.
       wherePos = where(ROIArray eq 0)
       if (wherePos[0] ne -1) then state.colocalizationParams[4,7] = correlate(ch0_vect[wherePos], ch1_vect[wherePos], /double) $
         else state.colocalizationParams[4,7] = 0.

          ;'Overlap Coefficients'
       if (n_elements(ch0_vect) gt 0) then state.colocalizationParams[5,0] = total(ch0_vect * ch1_vect) / sqrt(total(ch0_vect^2) * total(ch1_vect^2) ) $
         else state.colocalizationParams[5,0] = 0.
       if (whereChannel_0[0] ne -1) then state.colocalizationParams[5,1] = total(ch0_vect[whereChannel_0] * ch1_vect[whereChannel_0]) / $
                                                                                 sqrt( total(ch0_vect[whereChannel_0]^2) * total(ch1_vect[whereChannel_0]^2) ) $
         else state.colocalizationParams[5,1] = 0.
       if (whereChannel_1[0] ne -1) then state.colocalizationParams[5,2] = total(ch0_vect[whereChannel_1] * ch1_vect[whereChannel_1]) / $
                                                                                 sqrt( total(ch0_vect[whereChannel_1]^2) * total(ch1_vect[whereChannel_1]^2) ) $
         else state.colocalizationParams[5,2] = 0.
       if (whereBoth[0] ne -1) then state.colocalizationParams[5,3] = total(ch0_vect[whereBoth] * ch1_vect[whereBoth]) / $
                                                                            sqrt( total(ch0_vect[whereBoth]^2) * total(ch1_vect[whereBoth]^2) ) $
         else state.colocalizationParams[5,3] = 0.
       wherePos = where(ROIArray gt 0)
       if (wherePos[0] ne -1) then state.colocalizationParams[5,4] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / $
                                                                           sqrt( total(ch0_vect[wherePos]^2) * total(ch1_vect[wherePos]^2) ) $
         else state.colocalizationParams[5,4] = 0.
       wherePos = where(ROIArray eq 1)
       if (wherePos[0] ne -1) then state.colocalizationParams[5,5] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / $
                                                                           sqrt( total(ch0_vect[wherePos]^2) * total(ch1_vect[wherePos]^2) ) $
         else state.colocalizationParams[5,5] = 0.
       wherePos = where(ROIArray eq 2)
       if (wherePos[0] ne -1) then state.colocalizationParams[5,6] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / $
                                                                           sqrt( total(ch0_vect[wherePos]^2) * total(ch1_vect[wherePos]^2) ) $
         else state.colocalizationParams[5,6] = 0.
       wherePos = where(ROIArray eq 0)
       if (wherePos[0] ne -1) then state.colocalizationParams[5,7] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / $
                                                                           sqrt( total(ch0_vect[wherePos]^2) * total(ch1_vect[wherePos]^2) ) $
         else state.colocalizationParams[5,7] = 0.

          ;'Overlap Coefficients k1'
       if (n_elements(ch0_vect) gt 0) then state.colocalizationParams[6,0] = total(ch0_vect * ch1_vect) / total(ch0_vect^2) $
         else state.colocalizationParams[6,0] = 0.
       if (whereChannel_0[0] ne -1) then state.colocalizationParams[6,1] = total(ch0_vect[whereChannel_0] * ch1_vect[whereChannel_0]) / total(ch0_vect[whereChannel_0]^2) $
         else state.colocalizationParams[6,1] = 0.
       if (whereChannel_1[0] ne -1) then state.colocalizationParams[6,2] = total(ch0_vect[whereChannel_1] * ch1_vect[whereChannel_1]) / total(ch0_vect[whereChannel_1]^2) $
         else state.colocalizationParams[6,2] = 0.
       if (whereBoth[0] ne -1) then state.colocalizationParams[6,3] = total(ch0_vect[whereBoth] * ch1_vect[whereBoth]) / total(ch0_vect[whereBoth]^2) $
         else state.colocalizationParams[6,3] = 0.
       wherePos = where(ROIArray gt 0)
       if (wherePos[0] ne -1) then state.colocalizationParams[6,4] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / total(ch0_vect[wherePos]^2) $
         else state.colocalizationParams[6,4] = 0.
       wherePos = where(ROIArray eq 1)
       if (wherePos[0] ne -1) then state.colocalizationParams[6,5] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / total(ch0_vect[wherePos]^2) $
         else state.colocalizationParams[6,5] = 0.
       wherePos = where(ROIArray eq 2)
       if (wherePos[0] ne -1) then state.colocalizationParams[6,6] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / total(ch0_vect[wherePos]^2) $
         else state.colocalizationParams[6,6] = 0.
       wherePos = where(ROIArray eq 0)
       if (wherePos[0] ne -1) then state.colocalizationParams[6,7] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / total(ch0_vect[wherePos]^2) $
         else state.colocalizationParams[6,7] = 0.

          ;'Overlap Coefficients k2'
       if (n_elements(ch0_vect) gt 0) then state.colocalizationParams[7,0] = total(ch0_vect * ch1_vect) /  total(ch1_vect^2) $
         else state.colocalizationParams[7,0] = 0.
       if (whereChannel_0[0] ne -1) then state.colocalizationParams[7,1] = total(ch0_vect[whereChannel_0] * ch1_vect[whereChannel_0]) /  total(ch1_vect[whereChannel_0]^2) $
         else state.colocalizationParams[7,1] = 0.
       if (whereChannel_1[0] ne -1) then state.colocalizationParams[7,2] = total(ch0_vect[whereChannel_1] * ch1_vect[whereChannel_1]) / total(ch1_vect[whereChannel_1]^2) $
         else state.colocalizationParams[7,2] = 0.
       if (whereBoth[0] ne -1) then state.colocalizationParams[7,3] = total(ch0_vect[whereBoth] * ch1_vect[whereBoth]) /  total(ch1_vect[whereBoth]^2) $
         else state.colocalizationParams[7,3] = 0.
       wherePos = where(ROIArray gt 0)
       if (wherePos[0] ne -1) then state.colocalizationParams[7,4] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / total(ch1_vect[wherePos]^2) $
         else state.colocalizationParams[7,4] = 0.
       wherePos = where(ROIArray eq 1)
       if (wherePos[0] ne -1) then state.colocalizationParams[7,5] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / total(ch1_vect[wherePos]^2) $
         else state.colocalizationParams[7,5] = 0.
       wherePos = where(ROIArray eq 2)
       if (wherePos[0] ne -1) then state.colocalizationParams[7,6] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / total(ch1_vect[wherePos]^2) $
         else state.colocalizationParams[7,6] = 0.
       wherePos = where(ROIArray eq 0)
       if (wherePos[0] ne -1) then state.colocalizationParams[7,7] = total(ch0_vect[wherePos] * ch1_vect[wherePos]) / total(ch1_vect[wherePos]^2) $
         else state.colocalizationParams[7,7] = 0.

          ;'Colocalization Coefficients m1'
       whereNS = where(ch1_vect ne 0)
       if ((n_elements(ch0_vect) gt 0) and (whereNS[0] ne -1)) then state.colocalizationParams[8,0] = total(ch0_vect[whereNS]) / ( total(ch0_vect) ) $
         else state.colocalizationParams[8,0] = 0.
       if (whereChannel_0[0] ne -1) then whereNS = where(ch1_vect[whereChannel_0] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[8,1] = total((ch0_vect[whereChannel_0])[whereNS] ) / ( total(ch0_vect[whereChannel_0]) ) $
         else state.colocalizationParams[8,1] = 0.
       if (whereChannel_1[0] ne -1) then whereNS = where(ch1_vect[whereChannel_1] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[8,2] = total((ch0_vect[whereChannel_1])[whereNS]) / ( total(ch0_vect[whereChannel_1]) ) $
         else state.colocalizationParams[8,2] = 0.
       if (whereBoth[0] ne -1) then whereNS = where(ch1_vect[whereBoth] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[8,3] = total((ch0_vect[whereBoth])[whereNS]) / total(ch0_vect[whereBoth]) $
         else state.colocalizationParams[8,3] = 0.
       wherePos = where(ROIArray gt 0)
       if (wherePos[0] ne -1) then whereNS = where(ch1_vect[wherePos] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[8,4] = total((ch0_vect[wherePos])[whereNS]) /  total(ch0_vect[wherePos]) $
         else state.colocalizationParams[8,4] = 0.
       wherePos = where(ROIArray eq 1)
       if (wherePos[0] ne -1) then whereNS = where(ch1_vect[wherePos] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[8,5] = total((ch0_vect[wherePos])[whereNS]) /  total(ch0_vect[wherePos]) $
         else state.colocalizationParams[8,5] = 0.
       wherePos = where(ROIArray eq 2)
       if (wherePos[0] ne -1) then whereNS = where(ch1_vect[wherePos] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[8,6] = total((ch0_vect[wherePos])[whereNS]) /  total(ch0_vect[wherePos]) $
         else state.colocalizationParams[8,6] = 0.
       wherePos = where(ROIArray eq 0)
       if (wherePos[0] ne -1) then whereNS = where(ch1_vect[wherePos] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[8,7] = total((ch0_vect[wherePos])[whereNS]) /  total(ch0_vect[wherePos]) $
         else state.colocalizationParams[8,7] = 0.

          ;'Colocalization Coefficients m2'
       whereNS = where(ch0_vect ne 0)
       if ((n_elements(ch1_vect) gt 0) and (whereNS[0] ne -1)) then state.colocalizationParams[9,0] = total(ch1_vect[whereNS]) / total(ch1_vect) $
         else state.colocalizationParams[9,0] = 0.
       if (whereChannel_0[0] ne -1) then whereNS = where(ch0_vect[whereChannel_0] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[9,1] = total((ch1_vect[whereChannel_0])[whereNS] ) / total(ch1_vect[whereChannel_0]) $
         else state.colocalizationParams[9,1] = 0.
       if (whereChannel_1[0] ne -1) then whereNS = where(ch0_vect[whereChannel_1] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[9,2] = total((ch1_vect[whereChannel_1])[whereNS]) / total(ch1_vect[whereChannel_1]) $
         else state.colocalizationParams[9,2] = 0.
       if (whereBoth[0] ne -1) then whereNS = where(ch0_vect[whereBoth] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[9,3] = total((ch1_vect[whereBoth])[whereNS]) / total(ch1_vect[whereBoth]) $
         else state.colocalizationParams[9,3] = 0.
       wherePos = where(ROIArray gt 0)
       if (wherePos[0] ne -1) then whereNS = where(ch0_vect[wherePos] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[9,4] = total((ch1_vect[wherePos])[whereNS]) / total(ch1_vect[wherePos]) $
         else state.colocalizationParams[9,4] = 0.
       wherePos = where(ROIArray eq 1)
       if (wherePos[0] ne -1) then whereNS = where(ch0_vect[wherePos] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[9,5] = total((ch1_vect[wherePos])[whereNS]) /  total(ch1_vect[wherePos]) $
         else state.colocalizationParams[9,5] = 0.
       wherePos = where(ROIArray eq 2)
       if (wherePos[0] ne -1) then whereNS = where(ch0_vect[wherePos] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[9,6] = total((ch1_vect[wherePos])[whereNS]) / total(ch1_vect[wherePos]) $
         else state.colocalizationParams[9,6] = 0.
       wherePos = where(ROIArray eq 0)
       if (wherePos[0] ne -1) then whereNS = where(ch0_vect[wherePos] ne 0) else whereNS = -1
       if (whereNS[0] ne -1) then state.colocalizationParams[9,7] = total((ch1_vect[wherePos])[whereNS]) / total(ch1_vect[wherePos]) $
         else state.colocalizationParams[9,7] = 0.

          ;'Colocalization Coefficients M1' the Manders as defined by Costes (intensity in ROI in respect to everything)
       whereNS = where(ROIArray eq 3)
       if ((n_elements(ch0_vect) gt 0) and (whereChannel_1[0] ne -1)) then state.colocalizationParams[10,0] = total(ch0_vect[whereChannel_1]) / ( total(ch0_vect) ) $
         else state.colocalizationParams[10,0] = 0.
       if ((whereChannel_0[0] ne -1) and (whereNS[0] ne -1)) then state.colocalizationParams[10,1] = total(ch0_vect[whereNS]) / ( total(ch0_vect[whereChannel_0]) ) $
         else state.colocalizationParams[10,1] = 0.
       if (whereBoth[0] ne -1) then state.colocalizationParams[10,3] = total(ch0_vect[whereBoth]) / total(ch0_vect[whereChannel_0]) $
         else state.colocalizationParams[10,3] = 0.
          ;... intensity in ROI-intersection in respect to intensity in ROI
       if (whereBoth[0] ne -1) then state.colocalizationParams[10,5] = total(ch0_vect[whereBoth]) / total(ch0_vect[whereChannel_0]) $
         else state.colocalizationParams[10,5] = 0.

          ;'Colocalization Coefficients M2'
       if ((n_elements(ch1_vect) gt 0) and (whereChannel_0[0] ne -1)) then state.colocalizationParams[11,0] = total(ch1_vect[whereChannel_0]) / ( total(ch1_vect) ) $
         else state.colocalizationParams[11,0] = 0.
       if ((whereChannel_1[0] ne -1) and (whereNS[0] ne -1)) then state.colocalizationParams[11,2] = total(ch1_vect[whereNS]) / ( total(ch1_vect[whereChannel_1]) ) $
         else state.colocalizationParams[11,2] = 0.
       if (whereBoth[0] ne -1) then state.colocalizationParams[11,3] = total(ch1_vect[whereBoth]) / total(ch1_vect[whereChannel_1]) $
         else state.colocalizationParams[11,3] = 0.
          ;... intensity in ROI-intersection in respect to intensity in ROI
       if (whereBoth[0] ne -1) then state.colocalizationParams[11,6] = total(ch1_vect[whereBoth]) / total(ch1_vect[whereChannel_1]) $
         else state.colocalizationParams[11,6] = 0.

       whereFinite = where(finite(state.colocalizationParams) eq 0)
       if (whereFinite[0] ne -1) then state.colocalizationParams[whereFinite] = 0.
       if (state.fRandomAndRadiusShifts gt 0) then (*state.pColocRandomParams)[state.fRandomAndRadiusShifts-1,*,*] = state.colocalizationParams
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateDataTable, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      dimI = size(state.images, /dim)
      if ((state.fViewDataTable) and (dimI[0] ge 2)) then begin
        widget_control, wTopBase, set_uValue = state, /no_copy
         s_Coloc_calcuateColocalizationValues, wTopBase
        widget_control, wTopBase, get_uValue = state, /no_copy
        paramTableuValue = {groupLeader:wTopBase,$
                            paramColumnNames:state.colocalizationColumnNames,$
                            paramRowNames:state.colocalizationRowNames,$
                            param:state.colocalizationParams,$
                            name:'Colocalization',$
                            wTopBase:state.child_DataTable_tlb}
         widget_control, wTopBase, set_uValue = state, /no_copy
          s_OPTW_Update, paramTableUValue = paramTableUValue
         widget_control, wTopBase, get_uValue = state, /no_copy
      endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateViewSurfHist, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      dimI = size(state.images, /dim)
      if ((state.fViewSurfHist) and (dimI[0] ge 2)) then begin
         widget_control, wTopBase, set_uValue = state, /no_copy
            s_Coloc_getChannelValues, wTopBase, ch0_vect = ch0_vect, ch1_vect = ch1_vect
         widget_control, wTopBase, get_uValue = state, /no_copy
         minCh0 = min(ch0_vect, max = maxCh0)
         minCh1 = min(ch1_vect, max = maxCh1)

         surf = congrid(hist_2d(ch0_vect, ch1_vect, min1 = 0, max1 = maxCh0, min2 = 0, max2 = maxCh1, bin1 = 5, bin2 = 5), (maxCh0) > 2, (maxCh1) > 2)
         itCurrent, state.idViewSurfHist
         iSurface, surf, xRange = [0, maxCh0], yRange = [0, maxCh1],$
                   view_number = 1, xTitle = 'I [Ch0]', yTitle = 'I [Ch1]',zTitle = 'frequency',$
                   identifier = state.idViewSurfHist
      endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateViewScatterHist, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      dimI = size(state.images, /dim)
      if ((state.fViewScatterHist) and (dimI[0] ge 2)) then begin
         widget_control, wTopBase, set_uValue = state, /no_copy
            s_Coloc_getChannelValues, wTopBase, ch0_vect = ch0_vect, ch1_vect = ch1_vect
         widget_control, wTopBase, get_uValue = state, /no_copy
         PCParam = correlate(ch0_vect, ch1_vect, /double)
         linFitParam = linFit(ch0_vect, ch1_vect, /double, chisq = chisq, sigma = sigma)
         minCh0 = min(ch0_vect, max = maxCh0)
         minCh1 = min(ch1_vect, max = maxCh1)

         xFit = make_array(maxCh0 - minCh0 + 3, /float, /index) + (minCh0 - 1)
         if (finite(PCParam) eq 0) then yFit = xFit*floor(mean(ch1_vect)) else yFit = float(linFitParam[0] + xFit * linFitParam[1])

         itCurrent, state.idViewScatterHist
         iPlot, ch0_vect, ch1_vect, xRange = [0,maxCh0], yRange = [0,maxCh1], view_number = 1, /scatter,$
                xTitle = 'I [Ch0] (y = a + bx: a =' + strCompress(string(linFitParam[0])) + ', b =' + strCompress(string(linFitParam[1])) + ', rAll =' + strCompress(string(PCParam)) + ')',$
                yTitle = 'I [Ch1]',$
                identifier = state.idViewScatterHist
         iPlot, xFit, yFit, color = [255, 0, 0], overplot = state.idViewScatterHist
       endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateROIImage, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      dimI = size(state.images, /dim)
      if (state.fViewROIMask and (dimI[0] ge 2)) then begin
         image = make_array(state.dimI[1], state.dimI[2], /byte)
         if state.fGlobalROI then begin
            whereI = where(state.globalROI gt 0, count)
            if (count gt 0) then image[whereI] = 1
            whereI = where(s_ObjBorder_4or8(state.globalROI) gt 0, count)
            if (count gt 0) then image[whereI] = 2
         endif
         image += state.ROIMasks[0,*,*] * 84 + state.ROIMasks[1,*,*] * 170
         live_control, image, /update, window_in = 's_ImageColoc |-> ROI-Masks'
      endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateViewFreqHist, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      dimI = size(state.images, /dim)
      if ((state.fViewFreqHist) and (dimI[0] ge 2)) then begin
         widget_control, wTopBase, set_uValue = state, /no_copy
           s_Coloc_getChannelValues, wTopBase, ch0_vect = ch0_vect, ch1_vect = ch1_vect, /getChannelValuesOnly
         widget_control, wTopBase, get_uValue = state, /no_copy
         state.thisWindow->getProperty, image_data = snapShot
         intMatrix = make_array(256,256, /long)
         for i = 0l, (n_elements(ch0_vect)-1) do intMatrix[ch0_vect[i], ch1_vect[i]] = intMatrix[ch0_vect[i], ch1_vect[i]] + 1
         if state.fBiFreqHisLog then intMatrix = alog10(intMatrix+1)
         intMatrix = byte(255. / (max(intMatrix)>1.)* intMatrix)
         image = make_array(296,296, /byte, value = 125)
         image[20:275, 20:275] = intMatrix
         live_control, image, /update, window_in = 's_ImageColoc |-> Frequency Histogram'
      endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateViewCCHist, wTopBase
  widget_control, wTopBase, get_uValue = state, /no_copy
    dimI = size(state.images, /dim)
    if (state.fViewCCHist and (dimI[0] ge 2)) then begin
       if ptr_valid(state.pViewCCFilter) then begin
          if not(obj_valid(*state.pViewCCFilter)) then *state.pViewCCFilter = obj_new('C_sImageFilter_ImageColocCostes')
       endif else state.pViewCCFilter = ptr_new(obj_new('C_sImageFilter_ImageColocCostes'), /no_copy)

       pParamStruct = (*state.pViewCCFilter)->getpParamStruct()
       (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Show Colocalization Histogram'))[0]] = 1b
       a = (*state.pViewCCFilter)->apply(image = state.images[0,*,*], imCh1 = state.images[1,*,*])
    endif
  widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateZProjection, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy

      newImages = state.Images
      dimNI = size(newImages, /dim)
      typeNI = size(newImages, /type)

      if (state.fMeanZProjection or state.fMaxZProjection) then begin
         fMaxZProjection = state.fMaxZProjection
         fMeanZProjection = state.fMeanZProjection
         stack_tlb = state.stack_tlb
         widget_control, wTopBase, set_uValue = state, /no_copy

         s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, totalZNum = totalZNum
         widget_control, stack_tlb, get_uValue = stackState, /no_copy
            imageStackInfoObject = *stackState.pImageStackInfoObject
         widget_control, stack_tlb, set_uValue = stackState, /no_copy
         newImages *= 0.

         for chPos = 0, dimNI[0]-1 do for zPos = 0, totalZNum-1 do begin
            image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
            szI = size(image, /dim)
            case 1 of
              fMaxZProjection: if ((szI[0] eq dimNI[1]) and (szI[1] eq dimNI[2])) then newImages[chPos,*,*] >= image
              fMeanZProjection: if ((szI[0] eq dimNI[1]) and (szI[1] eq dimNI[2])) then newImages[chPos,*,*] += image
            endcase
         endfor
         if fMeanZProjection then newImages /= totalZNum
         if fMeanZProjection then for chPos = 0, dimNI[0]-1 do newImages[chPos,*,*] = bytArr(szI[0],szI[1]) + bytScl(newImages[chPos,*,*])
         widget_control, wTopBase, get_uValue = state, /no_copy

          if ((dimNI[0] ne state.dimI[0]) or (dimNI[1] ne state.dimI[1]) or  (dimNI[2] ne state.dimI[2])) then begin
             for i = 0, (state.dimI[0]<dimNI[0] )-1 do state.images[i,*,*] = congrid(make_array(dimNI[1], dimNI[2], type = typeNI) + newImages[i,*,*], state.dimI[1], state.dimI[2])
          endif else state.images = newImages
      endif else state.images = newImages
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateViewColorHist, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      dimI = size(state.images, /dim)
      if (state.fViewColorHist and (dimI[0] ge 2) and (dimI[2] gt 255)) then begin
         widget_control, wTopBase, set_uValue = state, /no_copy
            s_Coloc_getChannelValues, wTopBase, ch0_vect = ch0_vect, ch1_vect = ch1_vect, /getChannelValuesOnly
         widget_control, wTopBase, get_uValue = state, /no_copy
         state.thisWindow->getProperty, image_data = snapShot
         dimSnapShot = size(snapShot, /dim)
         if ((dimSnapShot[1] ne dimI[1]) or (dimSnapShot[2] ne dimI[2] )) then snapShot = congrid(snapShot, 3, dimI[1], dimI[2], /interp)
         intMatrix = make_array(3,256,256, /byte, value = 75)
         for i = 0l, (n_elements(ch0_vect)-1) do intMatrix[*,ch0_vect[i], ch1_vect[i]] = snapShot[*,i mod dimI[1], floor(i/dimI[1])]
         image = make_array(3,296,296, /byte, value = 125)
         image[*,20:275, 20:275] = intMatrix

         intMatrix = make_array(256, /byte, /index)
         colTblArr = make_array(3, 256, /byte)

         oImage = (state.thisView->get(position = 0))->getByName('Image_Channel0')
         oImage->getProperty, data = thisData, /no_copy
         memData = thisData[*,0,0:255]
         thisData[0,0,0:255] = (state.rgbValues[0, 0,*])[intMatrix]
         thisData[1,0,0:255] = (state.rgbValues[0, 1,*])[intMatrix]
         thisData[2,0,0:255] = (state.rgbValues[0, 2,*])[intMatrix]
         case state.opacFlag[0] of
            0: thisData[3,0,0:255] = state.blendValues[0]
            1: thisData[3,0,0:255] = state.opacValues[0,*]
         endcase
         oImage->setProperty, data = thisData, /no_copy
         if state.fMergeColorTables then begin
            widget_control, wTopBase, set_uValue = state, /no_copy
               s_Coloc_mergeColorTables, {top:wTopBase}
            widget_control, wTopBase, get_uValue = state, /no_copy
         endif else state.thisWindow->draw, state.thisView
         state.thisWindow->getProperty, image_data = snapShot
         colTblArr[0,*] = snapShot[0,0,0:255]
         colTblArr[1,*] = snapShot[1,0,0:255]
         colTblArr[2,*] = snapShot[2,0,0:255]
         image[*,20:275, 9] = colTblArr
         image[*,20:275, 10] = colTblArr
         image[*,20:275, 11] = colTblArr
         image[*,20:275, 12] = colTblArr
         image[*,20:275, 13] = colTblArr
         image[*,20:275, 14] = colTblArr

         oImage->getProperty, data = thisData, /no_copy
         thisData[*,0,0:255] = memData
         oImage->setProperty, data = thisData, /no_copy

         if state.fMergeColorTables then begin
            widget_control, wTopBase, set_uValue = state, /no_copy
              s_Coloc_mergeColorTables, {top:wTopBase}
            widget_control, wTopBase, get_uValue = state, /no_copy
         endif else state.thisWindow->draw, state.thisView

         oImage = (state.thisView->get(position = 0))->getByName('Image_Channel1')
         oImage->getProperty, data = thisData, /no_copy
         memData = thisData[*,0,0:255]
         thisData[0,0,0:255] = (state.rgbValues[1, 0,*])[intMatrix]
         thisData[1,0,0:255] = (state.rgbValues[1, 1,*])[intMatrix]
         thisData[2,0,0:255] = (state.rgbValues[1, 2,*])[intMatrix]
         case state.opacFlag[1] of
           0: thisData[3,0,0:255] = state.blendValues[1]
           1: thisData[3,0,0:255] = state.opacValues[1,*]
         endcase
         oImage->setProperty, data = thisData, /no_copy
         if state.fMergeColorTables then begin
            widget_control, wTopBase, set_uValue = state, /no_copy
               s_Coloc_mergeColorTables, {top:wTopBase}
            widget_control, wTopBase, get_uValue = state, /no_copy
         endif else state.thisWindow->draw, state.thisView
         state.thisWindow->getProperty, image_data = snapShot
         colTblArr[0,*] = snapShot[0,0,0:255]
         colTblArr[1,*] = snapShot[1,0,0:255]
         colTblArr[2,*] = snapShot[2,0,0:255]
         image[*,9, 20:275] = colTblArr
         image[*,10, 20:275] = colTblArr
         image[*,11, 20:275] = colTblArr
         image[*,12, 20:275] = colTblArr
         image[*,13, 20:275] = colTblArr
         image[*,14, 20:275] = colTblArr

         oImage->getProperty, data = thisData, /no_copy
         thisData[*,0,0:255] = memData
         oImage->setProperty, data = thisData, /no_copy

         if state.fMergeColorTables then begin
            widget_control, wTopBase, set_uValue = state, /no_copy
               s_Coloc_mergeColorTables, {top:wTopBase}
            widget_control, wTopBase, get_uValue = state, /no_copy
         endif else state.thisWindow->draw, state.thisView

         live_control, image, /update, window_in = 's_ImageColoc |-> Color Histogram'
      endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateMaskSpecifier, wTopBase, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    widget_control, wTopBase, get_uValue = state, /no_copy
       for i = 0, 1 do begin
          ; upDate Time
         if (state.maskSpecifierValue[i, 0] ne 'free') then begin
          number = s_getRightNumberFromString(state.maskSpecifierValue[i, 0])
          if (number eq -1) then state.maskSpecifierValue[i, 0] = 'free' $
              else tPos[i] = number
         endif
          ; upDate Channel
         if (state.maskSpecifierValue[i, 1] ne 'free') then begin
          number = s_getRightNumberFromString(state.maskSpecifierValue[i, 1])
          if (number eq -1) then begin
              chPos[i] = i
              state.maskSpecifierValue[i, 1] = 'free'
          endif else chPos[i] = number
         endif else chPos[i] = i
          ; upDate zSlice
         if (state.maskSpecifierValue[i, 2] ne 'free') then begin
            number = s_getRightNumberFromString(state.maskSpecifierValue[i, 2])
            if (number eq -1) then state.maskSpecifierValue[i, 2] = 'free' else zPos[i] = number
         endif
          ; upDate Cluster
         if ((state.maskSpecifierValue[i, 3] ne 'free') and (n_elements(clusPos) gt 0)) then begin
            number = s_getRightNumberFromString(state.maskSpecifierValue[i, 3])
            if (number eq -1) then state.maskSpecifierValue[i, 3] = 'free' else clusPos[i] = number
         endif
         if widget_info(state.child_ROIMasks_tlb[i], /valid) then begin
            paramTableuValue = {groupLeader:wTopBase,$
                              paramRowNames:state.maskSpecifier,$
                              param:state.maskSpecifierValue[i,*],$
                              name:'ROI-Mask',$
                              uNum:i,$
                              wTopBase:state.child_ROIMasks_tlb[i]}
          widget_control, wTopBase, set_uValue = state, /no_copy
             s_OPTW_Update, paramTableUValue = paramTableUValue
          widget_control, wTopBase, get_uValue = state, /no_copy
         endif
       endfor
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_upDateROIMask, wTopBase

    widget_control, wTopBase, get_uValue = state, /no_copy
       stack_tlb = long(state.stack_tlb)
    widget_control, wTopBase, set_uValue = state, /no_copy

    if widget_info(stack_tlb, /valid) then begin
       s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
       tPos = [tPos, tPos]
       chPos = [chPos, chPos]
       zPos = [zPos, zPos]
       if (n_elements(clusPos) gt 0) then clusPos = [clusPos, clusPos]
       s_Coloc_upDateMaskSpecifier, wTopBase, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    endif
    widget_control, wTopBase, get_uValue = state, /no_copy
       for uNum = 0, 1<(state.dimI[0]-1) do begin
         case 1 of
          state.fROIMasks[uNum,0]:begin   ; Masks from Cut-Values
             state.ROIMasks[uNum,*,*] = (state.images[uNum,*,*] ge state.bottomValues[uNum]) * (state.images[uNum,*,*] le state.TopValues[uNum])
          endcase
          state.fROIMasks[uNum,1]:begin   ; Thresholds from Costes
             if ptr_valid(state.pViewCCFilter) then begin
                if not(obj_valid(*state.pViewCCFilter)) then *state.pViewCCFilter = obj_new('C_sImageFilter_ImageColocCostes')
             endif else state.pViewCCFilter = ptr_new(obj_new('C_sImageFilter_ImageColocCostes'), /no_copy)

             pParamStruct = (*state.pViewCCFilter)->getpParamStruct()
             (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Show Costes Histogram'))[0]] = state.fViewCCHist
             (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Open Colocalization Window'))[0]] = 0b

             a = (*state.pViewCCFilter)->apply(image = state.images[0,*,*], imCh1 = state.images[1,*,*])

             state.bottomValues[*] = ((*state.pViewCCFilter)->getColocValues())[0:1]
             state.ROIMasks[uNum,*,*] = (state.images[uNum,*,*] ge state.bottomValues[uNum]) * (state.images[uNum,*,*] le state.TopValues[uNum])
           endcase
           state.fROIMasks[uNum,2]:begin   ; Masks from Image-Segmentation
              cut_x = [0, state.dimI[1]-1]
              cut_y = [0, state.dimI[2]-1]

              widget_control, stack_tlb, get_uValue = stackState, /no_copy
                imageStackInfoObject = *stackState.pImageStackInfoObject
                if widget_info(stackState.child_ViewWindow_tlb, /valid) then begin
                   widget_control, stackState.child_ViewWindow_tlb, get_uValue = viewState, /no_copy
                      cut_x = [viewState.xs, viewState.xd]
                      cut_y = [viewState.ys, viewState.yd]
                   widget_control, stackState.child_ViewWindow_tlb, set_uValue = viewState, /no_copy
                endif
              widget_control, stack_tlb, set_uValue = stackState, /no_copy

              if ((cut_x[1]-cut_x[0]+1) ne state.dimI[1]) then cut_x = [0, state.dimI[1]-1]
              if ((cut_y[1]-cut_y[0]+1) ne state.dimI[2]) then cut_y = [0, state.dimI[2]-1]

              if (n_elements(clusPos) eq 0) then mask = imageStackInfoObject->applyImageSegmentation(tPos = tPos[uNum], chPos = chPos[uNum], zPos = zPos[uNum], cut_x=cut_x, cut_y=cut_y) $
                else mask = imageStackInfoObject->applyImageSegmentation(tPos = tPos[uNum], chPos = chPos[uNum], zPos = zPos[uNum], clusPos = clusPos[uNum],cut_x=cut_x, cut_y=cut_y)
              szMask = size(mask, /dim)
              case n_elements(szMask) of
              1:state.ROIMasks[uNum,*,*] = byte(round(randomu(1,state.dimI[1]*state.dimI[2])))
              2:if (szMask[0] ne state.dimI[1]) or (szMask[1] ne state.dimI[2]) then mask = congrid(mask, state.dimI[1], state.dimI[2]) else state.ROIMasks[uNum,*,*] = mask
              endcase
           endcase
          state.fROIMasks[uNum,3]:begin   ; Masks from Saved Cluster Masks
              widget_control, stack_tlb, get_uValue = stackState, /no_copy
                imageStackInfoObject = *stackState.pImageStackInfoObject
              widget_control, stack_tlb, set_uValue = stackState, /no_copy
              if (n_elements(clusPos) eq 0) then mask = ( imageStackInfoObject->getSelectedClusterMask(tPos = tPos[uNum], chPos = chPos[uNum], zPos = zPos[uNum]) gt 0) $
                else mask = ( imageStackInfoObject->getSelectedClusterMask(tPos = tPos[uNum], chPos = chPos[uNum], zPos = zPos[uNum], clusPos = clusPos[uNum]) gt 0)
                if (n_elements(mask) gt 9) then state.ROIMasks[uNum,*,*] = mask else state.ROIMasks[uNum,*,*] = byte(round(randomu(1,state.dimI[1]*state.dimI[2])))
          endcase
          state.fROIMasks[uNum,4]:begin   ; Masks from Saved ROI2D-Masks
              if (n_elements(clusPos) eq 0) then oROIGroup = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb, tPos = tPos[uNum], chPos = chPos[uNum], zPos = zPos[uNum]) $
                else oROIGroup = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb, tPos = tPos[uNum], chPos = chPos[uNum], zPos = zPos[uNum], clusPos = clusPos[uNum])
              if obj_valid(oROIGroup) then state.ROIMasks[uNum,*,*] = (oROIGroup->getGroupMask() gt 0) $
                else state.ROIMasks[uNum,*,*] = byte(round(randomu(1,state.dimI[1]*state.dimI[2])))
          endcase
          state.fROIMasks[uNum,5]:begin   ; Masks from Saved ROI3D-Masks
              if (n_elements(clusPos) eq 0) then oROIGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos[uNum], chPos = chPos[uNum]) $
                else oROIGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos[uNum], chPos = chPos[uNum], clusPos = clusPos[uNum])
              if obj_valid(oROIGroup) then state.ROIMasks[uNum,*,*] = (oROIGroup->getGroupMask(zPos = zPos[uNum]) gt 0) $
                else state.ROIMasks[uNum,*,*] = byte(round(randomu(1,state.dimI[1]*state.dimI[2])))
          endcase
          else:
         endcase
       endfor
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Coloc_backgroudCorrection, wTopBase
     widget_control, wTopBase, get_uValue = state, /no_copy
       for i = 0, state.dimI[0]-1 do state.images[i,*,*] = (fix(temporary(state.images[i,*,*])) - state.backgroudValues[i]) > 0
       newImages = state.images
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_Coloc_update, wTopBase, newImages = newImages
end


pro s_Coloc_output, ev
  widget_control, ev.top, get_uValue = state, /no_copy
    state.thisWindow->getProperty, Image_Data = snapShot
    widget_control, ev.id, get_uValue = whichFileType
    case whichFileType of
      'PNG': begin
        filename = dialog_pickfile(/write, File = 'idl.png')
        if filename ne '' then Write_PNG, filename, snapShot
      endCase
      'JPEG': begin
        filename = dialog_pickfile(/write, File = 'idl.jpg')
        if filename ne '' then Write_JPEG, filename, snapShot, true = 1
      endCase
    endCase
  widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Coloc_maskSelect, ev
  widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uNum, /no_copy

      whereVal = where(state.wROIMasksOnOff[uNum,*] eq ev.id)
      if (whereVal[0] eq 6) then begin
        whereVal2 = where(state.fROIMasks[uNum, 0:5] eq 1)
        if (whereVal2[0] gt 0) then state.fROIMasks[uNum, 6] = s_ToggleButtonOnOffState(state.wROIMasksOnOff[uNum, 6])
      endif else begin
        whereVal2 = where(state.fROIMasks[uNum, 0:5] eq 1)
        state.fROIMasks[uNum, whereVal2[0]] = s_ToggleButtonOnOffState(state.wROIMasksOnOff[uNum, whereVal2[0]])
        if (whereVal[0] ne whereVal2[0]) then $
          state.fROIMasks[uNum, whereVal[0]] = s_ToggleButtonOnOffState(state.wROIMasksOnOff[uNum, whereVal[0]])
      endelse

      whereVal = where(state.fROIMasks[uNum, 0:5] eq 1)
      if (whereVal[0] eq -1) then state.fROIMasks[uNum, 0] = s_ToggleButtonOnOffState(state.wROIMasksOnOff[uNum, 0])

      whereVal = where(state.fROIMasks[uNum, 0:5] eq 1)
      if ((state.fROIMasks[uNum, 6]) and (whereVal[0] lt 1)) then state.fROIMasks[uNum, 6] = s_ToggleButtonOnOffState(state.wROIMasksOnOff[uNum, 6])

      if state.fROIMasks[uNum, 6] then begin
         paramTableuValue = {groupLeader:ev.top,$
                              paramRowNames:state.maskSpecifier,$
                              param:state.maskSpecifierValue[uNum,*],$
                              name:'ROI-Mask',$
                              uNum:uNum,$
                              wTopBase:state.child_ROIMasks_tlb[uNum]}
         if not(widget_info(state.child_ROIMasks_tlb[uNum], /valid)) then begin
            s_ObjParamTableWidget, paramTableuValue = paramTableuValue
            state.child_ROIMasks_tlb[uNum] = paramTableuValue.wTopBase
         endif else s_OPTW_Update, paramTableUValue = paramTableUValue
      endif else begin
         if widget_info(state.child_ROIMasks_tlb[uNum], /valid) then begin
            widget_control, state.child_ROIMasks_tlb[uNum], /destroy
            state.child_ROIMasks_tlb[uNum] = -1l
         endif
      endelse

      iName = strCompress('Image_Channel' + string(uNum), /rem)
      oImage = (state.thisView->get(position = 0))->getByName(iName)
      oImage->getProperty, data = thisData, /no_copy
      widget_control, ev.id, set_uValue = uNum, /no_copy
        widget_control, ev.top, set_uValue = state, /no_copy
          s_Coloc_upDateROIMask, ev.top
        widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.id, get_uValue = uNum, /no_copy
      state.opacMasks[uNum,*,*] = state.ROIMasks[uNum,*,*]
      case state.opacFlag[uNum] of
         0: thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
         1: thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
      endcase
      oImage->setProperty, data = thisData, /no_copy
      if state.fMergeColorTables then begin
         widget_control, ev.top, set_uValue = state, /no_copy
           s_Coloc_mergeColorTables, {top:ev.top}
         widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView

    if ((total(state.fROIMasks[*,1]) eq 0) and (state.fViewCCHist eq 0)) then if ptr_valid(state.pViewCCFilter) then if obj_valid(*state.pViewCCFilter) then obj_destroy, *state.pViewCCFilter

    widget_control, ev.id, set_uValue = uNum, /no_copy
  widget_control, ev.top, set_uValue = state, /no_copy

  s_Coloc_upDateViewScatterHist, ev.top
  s_Coloc_upDateViewCCHist, ev.top
  s_Coloc_upDateViewSurfHist, ev.top
  s_Coloc_upDateDataTable, ev.top
  s_Coloc_upDateViewColorHist, ev.top
  s_Coloc_upDateViewFreqHist, ev.top
  s_Coloc_upDateROIImage, ev.top
end


pro s_Coloc_scaleEqualInt, ev
     widget_control, ev.top, get_uValue = state, /no_copy
        widget_control, ev.id, get_uValue = uNum, /no_copy

       meanVal0 = (moment(state.images[0,*,*]))[0]
       meanVal1 = (moment(state.images[1,*,*]))[0]
       case uNum of
           0:state.images[uNum,*,*] = byte( ((meanVal1/meanVal0)*state.images[uNum,*,*] ) < 255.)
           1:state.images[uNum,*,*] = byte( ((meanVal0/meanVal1)*state.images[uNum,*,*] ) < 255.)
        else:
       endcase

       iName = strCompress('Image_Channel' + string(uNum), /rem)
       oImage = (state.thisView->get(position = 0))->getByName(iName)
       oImage->getProperty, data = thisData, /no_copy
       thisData[0,*,*] = (state.rgbValues[uNum, 0,*])[state.images[uNum,*,*]]
       thisData[1,*,*] = (state.rgbValues[uNum, 1,*])[state.images[uNum,*,*]]
       thisData[2,*,*] = (state.rgbValues[uNum, 2,*])[state.images[uNum,*,*]]
       case (state.opacFlag[uNum] ) of
          0: thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
          1: thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
       endcase
       oImage->setProperty, data = thisData, /no_copy
       widget_control, ev.id, set_uValue = uNum, /no_copy
       if state.fMergeColorTables then begin
         widget_control, ev.top, set_uValue = state, /no_copy
          s_Coloc_mergeColorTables, {top:ev.top}
         widget_control, ev.top, get_uValue = state, /no_copy
       endif else state.thisWindow->draw, state.thisView
    widget_control, ev.top, set_uValue = state, /no_copy
    s_Coloc_upDateROIMask, ev.top
    s_Coloc_upDateViewScatterHist, ev.top
    s_Coloc_upDateViewCCHist, ev.top
    s_Coloc_upDateViewSurfHist, ev.top
    s_Coloc_upDateDataTable, ev.top
    s_Coloc_upDateViewColorHist, ev.top
    s_Coloc_upDateViewFreqHist, ev.top
    s_Coloc_upDateROIImage, ev.top
end


pro s_Coloc_scaleInt, ev
     widget_control, ev.top, get_uValue = state, /no_copy
        widget_control, ev.id, get_uValue = uNum, /no_copy
        minVal = min(state.images[uNum,*,*])
        state.images[uNum,*,*] = byte(( 255. / ((max(state.images[uNum,*,*]) - minVal) > 1) * (state.images[uNum,*,*]-minVal)) < 255.)

        iName = strCompress('Image_Channel' + string(uNum), /rem)
        oImage = (state.thisView->get(position = 0))->getByName(iName)
        oImage->getProperty, data = thisData, /no_copy
        thisData[0,*,*] = (state.rgbValues[uNum, 0,*])[state.images[uNum,*,*]]
        thisData[1,*,*] = (state.rgbValues[uNum, 1,*])[state.images[uNum,*,*]]
        thisData[2,*,*] = (state.rgbValues[uNum, 2,*])[state.images[uNum,*,*]]
        case state.opacFlag[uNum] of
           0: thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
           1: thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
        endcase
        oImage->setProperty, data = thisData, /no_copy
        widget_control, ev.id, set_uValue = uNum, /no_copy
        if state.fMergeColorTables then begin
           widget_control, ev.top, set_uValue = state, /no_copy
              s_Coloc_mergeColorTables, {top:ev.top}
           widget_control, ev.top, get_uValue = state, /no_copy
        endif else state.thisWindow->draw, state.thisView
    widget_control, ev.top, set_uValue = state, /no_copy
    s_Coloc_upDateROIMask, ev.top
    s_Coloc_upDateViewScatterHist, ev.top
    s_Coloc_upDateViewCCHist, ev.top
    s_Coloc_upDateViewSurfHist, ev.top
    s_Coloc_upDateDataTable, ev.top
    s_Coloc_upDateViewColorHist, ev.top
    s_Coloc_upDateViewFreqHist, ev.top
    s_Coloc_upDateROIImage, ev.top
end


pro s_Coloc_scaleIntVolume, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       widget_control, ev.id, get_uValue = uNum, /no_copy

       totVal0 = total(state.images[0,*,*])
       totVal1 = total(state.images[1,*,*])

       print, 'Total Intensity of Channel 0:', totVal0
       print, 'Total Intensity of Channel 1:', totVal1

       if (totVal0 gt totVal1) then state.images[0,*,*] = round(state.images[0,*,*] * (1.*totVal1/totVal0))
       if (totVal1 gt totVal0) then state.images[1,*,*] = round(state.images[1,*,*] * (1.*totVal0/totVal1))

       for uNum = 0,1 do begin
          iName = strCompress('Image_Channel' + string(uNum), /rem)
          oImage = (state.thisView->get(position = 0))->getByName(iName)
          oImage->getProperty, data = thisData, /no_copy
          thisData[0,*,*] = (state.rgbValues[uNum, 0,*])[state.images[uNum,*,*]]
          thisData[1,*,*] = (state.rgbValues[uNum, 1,*])[state.images[uNum,*,*]]
          thisData[2,*,*] = (state.rgbValues[uNum, 2,*])[state.images[uNum,*,*]]
          case state.opacFlag[uNum] of
             0:thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
             1:thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
          endcase
          oImage->setProperty, data = thisData, /no_copy
       endfor
       widget_control, ev.id, set_uValue = uNum, /no_copy
       if state.fMergeColorTables then begin
         widget_control, ev.top, set_uValue = state, /no_copy
          s_Coloc_mergeColorTables, {top:ev.top}
         widget_control, ev.top, get_uValue = state, /no_copy
       endif else state.thisWindow->draw, state.thisView
    widget_control, ev.top, set_uValue = state, /no_copy
    s_Coloc_upDateROIMask, ev.top
    s_Coloc_upDateViewScatterHist, ev.top
    s_Coloc_upDateViewCCHist, ev.top
    s_Coloc_upDateViewSurfHist, ev.top
    s_Coloc_upDateDataTable, ev.top
    s_Coloc_upDateViewColorHist, ev.top
    s_Coloc_upDateViewFreqHist, ev.top
    s_Coloc_upDateROIImage, ev.top
end


pro s_Coloc_colors, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.id, get_uValue = uNum, /no_copy
         if (tag_names(ev, /structure_name) eq 'XCOLORS_LOAD') then begin
            if (ev.name ne 'Unknown') then begin
               iName = strCompress('Image_Channel' + string(uNum), /rem)
               oImage = (state.thisView->get(position = 0))->getByName(iName)
               oImage->getProperty, data = thisData, /no_copy
               thisData[0,*,*] = ev.r[state.images[uNum,*,*]]
               thisData[1,*,*] = ev.g[state.images[uNum,*,*]]
               thisData[2,*,*] = ev.b[state.images[uNum,*,*]]
               case state.opacFlag[uNum] of
                  0: thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
                  1: thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
               endcase
               oImage->setProperty, data = thisData, /no_copy
               state.rgbValues[uNum, 0,*] = ev.r
               state.rgbValues[uNum, 1,*] = ev.g
               state.rgbValues[uNum, 2,*] = ev.b
               state.colTbl[uNum] = ev.index
               state.gamma[uNum] = ev.gamma
               state.stretchBotTop[uNum,*] = ev.stretch
               if state.fMergeColorTables then begin
                  widget_control, ev.top, set_uValue = state, /no_copy
                    s_Coloc_mergeColorTables, {top: ev.top}
                  widget_control, ev.top, get_uValue = state, /no_copy
               endif else state.thisWindow->draw, state.thisView
            endif
         endif else begin
         XColors, NotifyID = [ev.id, ev.top],$
                  group_leader = ev.top,$
                  title = strCompress('Color_Table_Ch_' + string(uNum)),$
                  xOffset = 100, yOffset = 100+100*uNum,$
                  colTbl = state.colTbl[uNum],$
                  rgbValues = state.rgbValues[uNum,*,*],$
                  gamma = state.gamma[uNum],$
                  stretch = state.stretchBotTop[uNum,*]
         endelse
       widget_control, ev.id, set_uValue = uNum, /no_copy
   widget_control, ev.top, set_uValue = state, /no_copy
   s_Coloc_upDateViewColorHist, ev.top
end


pro s_Coloc_vector, ev
     widget_control, ev.top, get_uValue = state, /no_copy
        widget_control, ev.id, get_uValue = uNum, /no_copy
         state.opacFlag[uNum] = 1
         if tag_names(ev, /structure_name) eq 'XCOLORS_LOAD' then begin
            iName = strCompress('Image_Channel' + string(uNum), /rem)
            oImage = (state.thisView->get(position = 0))->getByName(iName)
            oImage->getProperty, data = thisData, /no_copy
            thisData[3,*,*] = ev.r[state.images[uNum,*,*]]
            oImage->setProperty, data = thisData, /no_copy
            state.opacValues[uNum,*] = ev.r
            state.opacGamma[uNum] = ev.gamma
            state.opacStretchBotTop[uNum,*] = ev.stretch
            if state.fMergeColorTables then begin
               widget_control, ev.top, set_uValue = state, /no_copy
                 s_Coloc_mergeColorTables, {top:ev.top}
               widget_control, ev.top, get_uValue = state, /no_copy
            endif else state.thisWindow->draw, state.thisView
         endif else begin
          opacValues = state.rgbValues[uNum,*,*] * 0
          opacValues[0,0,*] = state.opacValues[uNum,*]
          opacValues[0,1,*] = state.opacValues[uNum,*]
          opacValues[0,2,*] = state.opacValues[uNum,*]
          XColors, NotifyID = [ev.id, ev.top], group_leader = ev.top,$
                    title = strCompress('Color_Blend_Ch_' + string(uNum)),$
                    xOffset = 100, yOffset = 100+100*uNum,$
                    colTbl = 0,$
                    rgbValues = opacValues,$
                    gamma = state.opacGamma[uNum],$
                    stretch = state.opacStretchBotTop[uNum,*]
         endelse
        widget_control, ev.id, set_uValue = uNum, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy
    s_Coloc_upDateViewColorHist, ev.top
end


pro s_Coloc_cleanUp, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
    if (n_elements(state) ne 0) then begin
       obj_destroy, state.thisContainer
       if state.fViewScatterHist then itDelete, state.idViewScatterHist
       if state.fViewSurfHist then itDelete, state.idViewSurfHist
       if state.fViewFreqHist then live_destroy, window_in = 's_ImageColoc |-> Frequency Histogram'
       if state.fViewColorHist then live_destroy, window_in = 's_ImageColoc |-> Color Histogram'
       if state.fViewROIMask then live_destroy, window_in = 's_ImageColoc |-> ROI-Masks'

       live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 0'
       if not((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> Channel 0" does not exist.  Operation not performed.')) then $
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 0'

       live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 1'
       if not((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> Channel 1" does not exist.  Operation not performed.')) then $
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 1'

       live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Merge Channel'
       if not((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> Merge Channel" does not exist.  Operation not performed.')) then $
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Merge Channel'

       if widget_info(long(state.groupLeader), /valid_id) then begin
       widget_control, state.groupLeader, get_uvalue = stateParent, /no_copy
       if (n_elements(stateParent) ne 0) then begin
         if ( (where(tag_names(stateParent) eq 'FCOLOCALIZATION'))[0] ne -1) then begin
           stateParent.fColocalization = s_ToggleButtonOnOffState(stateParent.wColocalizationOnOff)
           stateParent.child_Colocalization_tlb = -1
         endif
         widget_control, state.groupLeader, set_uvalue = stateParent, /no_copy
       endif

       for i = 0,n_tags(state)-1 do begin
          case size(state.(i), /tname) of
            'POINTER': ptr_free, state.(i)
            'OBJREF': obj_destroy, state.(i)
            else:
          endcase
       endfor
     endif
   endif
end


pro s_Coloc_slider, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.id, get_uValue = uNum, /no_copy
         state.opacFlag[uNum] = 0
         iName = strCompress('Image_Channel' + string(uNum), /rem)
         oImage = (state.thisView->get(position = 0))->getByName(iName)
         oImage->getProperty, data = thisData, /no_copy
         state.blendValues[uNum] = ev.value
         thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
         oImage->setProperty, data = thisData, /no_copy
      widget_control, ev.id, set_uValue = uNum, /no_copy
      if state.fMergeColorTables then begin
         widget_control, ev.top, set_uValue = state, /no_copy
          s_Coloc_mergeColorTables, {top:ev.top}
         widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
   s_Coloc_upDateROIMask, ev.top
   s_Coloc_upDateViewScatterHist, ev.top
   s_Coloc_upDateViewCCHist, ev.top
   s_Coloc_upDateViewSurfHist, ev.top
   s_Coloc_upDateDataTable, ev.top
   s_Coloc_upDateViewColorHist, ev.top
   s_Coloc_upDateViewFreqHist, ev.top
   s_Coloc_upDateROIImage, ev.top
end


pro s_Coloc_cutBottom_slider, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.id, get_uValue = uNum, /no_copy
         iName = strCompress('Image_Channel' + string(uNum), /rem)
         oImage = (state.thisView->get(position = 0))->getByName(iName)
         oImage->getProperty, data = thisData, /no_copy
         state.bottomValues[uNum] = ev.value

         widget_control, ev.id, set_uValue = uNum, /no_copy
         widget_control, ev.top, set_uValue = state, /no_copy
            s_Coloc_upDateROIMask, ev.top
         widget_control, ev.top, get_uValue = state, /no_copy
         widget_control, ev.id, get_uValue = uNum, /no_copy
         state.opacMasks[uNum,*,*] = state.ROIMasks[uNum,*,*]
         case state.opacFlag[uNum] of
            0: thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
            1: thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
         endcase
         oImage->setProperty, data = thisData, /no_copy
      widget_control, ev.id, set_uValue = uNum, /no_copy
      if state.fMergeColorTables then begin
         widget_control, ev.top, set_uValue = state, /no_copy
            s_Coloc_mergeColorTables, {top: ev.top}
         widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
   s_Coloc_upDateViewScatterHist, ev.top
   s_Coloc_upDateViewCCHist, ev.top
   s_Coloc_upDateViewSurfHist, ev.top
   s_Coloc_upDateDataTable, ev.top
   s_Coloc_upDateViewColorHist, ev.top
   s_Coloc_upDateViewFreqHist, ev.top
   s_Coloc_upDateROIImage, ev.top
end


pro s_Coloc_cutTop_slider, ev
    widget_control, ev.top, get_uValue = state, /no_copy
       widget_control, ev.id, get_uValue = uNum, /no_copy
         iName = strCompress('Image_Channel' + string(uNum), /rem)
         oImage = (state.thisView->get(position = 0))->getByName(iName)
         oImage->getProperty, data = thisData, /no_copy
         state.topValues[uNum] = ev.value

         widget_control, ev.id, set_uValue = uNum, /no_copy
         widget_control, ev.top, set_uValue = state, /no_copy
          s_Coloc_upDateROIMask, ev.top
         widget_control, ev.top, get_uValue = state, /no_copy
         widget_control, ev.id, get_uValue = uNum, /no_copy
         state.opacMasks[uNum,*,*] = state.ROIMasks[uNum,*,*]

         case state.opacFlag[uNum] of
           0: thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
           1: thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
         endcase
         oImage->setProperty, data = thisData, /no_copy
       widget_control, ev.id, set_uValue = uNum, /no_copy
       if state.fMergeColorTables then begin
         widget_control, ev.top, set_uValue = state, /no_copy
          s_Coloc_mergeColorTables, {top:ev.top}
         widget_control, ev.top, get_uValue = state, /no_copy
       endif else state.thisWindow->draw, state.thisView
    widget_control, ev.top, set_uValue = state, /no_copy
    s_Coloc_upDateViewScatterHist, ev.top
    s_Coloc_upDateViewCCHist, ev.top
    s_Coloc_upDateViewSurfHist, ev.top
    s_Coloc_upDateDataTable, ev.top
    s_Coloc_upDateViewColorHist, ev.top
    s_Coloc_upDateViewFreqHist, ev.top
    s_Coloc_upDateROIImage, ev.top
end


pro s_Coloc_expose, ev
   widget_control, ev.top, get_uValue = state, /no_copy
       if state.fMergeColorTables then begin
          widget_control, ev.top, set_uValue = state, /no_copy
             s_Coloc_mergeColorTables, {top:ev.top}
          widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Coloc_event, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.top, scr_xsize = ev.x
       state.thisWindow->setProperty, dimension = [ev.x, ev.y*state.drawScale+15]
       if state.fMergeColorTables then begin
          widget_control, ev.top, set_uValue = state, /no_copy
              s_Coloc_mergeColorTables, {top: ev.top}
          widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Coloc_mergeColorTablesOnOff, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      state.fMergeColorTables = s_ToggleButtonOnOffState(state.wMergeColorTablesOnOff)
      if (state.fMergeColorTables eq 0) then begin
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 0'
         if (error ne 'No application/applet is present for use with LIVE_INFO.') then live_destroy, window_in = 's_ImageColoc |-> Channel 0'
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 1'
         if (error ne 'No application/applet is present for use with LIVE_INFO.') then live_destroy, window_in = 's_ImageColoc |-> Channel 1'
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Merge Channel'
         if (error ne 'No application/applet is present for use with LIVE_INFO.') then live_destroy, window_in = 's_ImageColoc |-> Merge Channel'
      endif
   widget_control, ev.top, set_uValue = state, /no_copy
   s_Coloc_mergeColorTables, ev
   s_Coloc_upDateViewColorHist, ev.top
end


pro s_Coloc_ZProjectionOnOff, ev
   widget_control, ev.top, get_uValue = state, /no_copy
   widget_control, ev.id, get_uValue = uVal, /no_copy
   case uVal of
      'MAXZPROJECTION': begin
         state.fMaxZProjection = s_ToggleButtonOnOffState(state.wMaxZProjectionOnOff)
         if state.fMeanZProjection then state.fMeanZProjection = s_ToggleButtonOnOffState(state.wMeanZProjectionOnOff)
       endcase
      'MEANZPROJECTION': begin
         state.fMeanZProjection = s_ToggleButtonOnOffState(state.wMeanZProjectionOnOff)
         if state.fMaxZProjection then state.fMaxZProjection = s_ToggleButtonOnOffState(state.wMaxZProjectionOnOff)
       endcase
   endCase
   widget_control, ev.id, set_uValue = uVal, /no_copy
   widget_control, ev.top, set_uValue = state, /no_copy
   s_Coloc_update, ev.top
end


pro s_Coloc_drawEvents, ev
   widget_control, ev.top, get_uValue = state, /no_copy
   case ((['PRESS', 'RELEASE', 'MOTION', 'SCROLL'])[ev.type]) of
     'PRESS':begin
        buttonPressed = What_Button_Pressed(ev)
        if (buttonPressed eq 'RIGHT') then begin
          state.map = 1 - state.map
          widget_control, state.wFlexBase, map = state.map
        endif
      endcase
      else:
   endcase
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Coloc_update, wTopBase, newImages = newImages
   widget_control, wTopBase, get_uValue = state, /no_copy

      if (n_elements(newImages) eq 0) then newImages = state.images
;      if (size(newImages, /type) ne 1) then newImages = bytScl(newImages, min = 0, max = 4096)

      if (state.fMeanZProjection or state.fMaxZProjection) and state.fZProjectionUpdate then begin
         widget_control, wTopBase, set_uValue = state, /no_copy
            s_Coloc_upDateZProjection, wTopBase
         widget_control, wTopBase, get_uValue = state, /no_copy
      endif else state.images =  newImages

      if not(state.fShiftMasks) then begin
         widget_control, wTopBase, set_uValue = state, /no_copy
           s_Coloc_upDateROIMask, wTopBase
         widget_control, wTopBase, get_uValue = state, /no_copy
      endif

       for i = 0, state.dimI[0]-1 do begin
          iName = strCompress('Image_Channel' + string(i), /rem)
          oImage = (state.thisView->get(position = 0))->getByName(iName)
          oImage->getProperty, data = thisData, /no_copy
          thisData[0,*,*] = (bytArr(255) + state.rgbValues[i,0,*])[state.images[i,*,*]]
          thisData[1,*,*] = (bytArr(255) + state.rgbValues[i,1,*])[state.images[i,*,*]]
          thisData[2,*,*] = (bytArr(255) + state.rgbValues[i,2,*])[state.images[i,*,*]]

          state.opacMasks[i,*,*] = state.ROIMasks[i,*,*]
          case state.opacFlag[i] of
             0: thisData[3,*,*] = state.opacMasks[i,*,*] * state.blendValues[i]
             1: thisData[3,*,*] = (state.opacValues[i,*])[state.images[i,*,*]]
          endcase
         oImage->setProperty, data = thisData, /no_copy
       endfor
       if state.fMergeColorTables then begin
          widget_control, wTopBase, set_uValue = state, /no_copy
              s_Coloc_mergeColorTables, {top:wTopBase}
          widget_control, wTopBase, get_uValue = state, /no_copy
       endif else state.thisWindow->draw, state.thisView
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_Coloc_upDateViewScatterHist, wTopBase
    s_Coloc_upDateViewCCHist, wTopBase
    s_Coloc_upDateViewSurfHist, wTopBase
    s_Coloc_upDateDataTable, wTopBase
    s_Coloc_upDateViewColorHist, wTopBase
    s_Coloc_upDateViewFreqHist, wTopBase
    s_Coloc_upDateROIImage, wTopBase
end


pro s_Coloc_control, ev
  widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue, /no_copy
    case uValue of
       'VIEWCOLORHISTONOFF':begin
          dimI = size(state.images, /dim)
          if (dimI[0] ge 2) then begin
            state.fViewColorHist = s_ToggleButtonOnOffState(state.wViewColorHistOnOff)
            if state.fViewColorHist then begin
              image = make_array(3,296,296, /byte, value = 125)
              live_image, image, draw_dimension = [300,300], /no_select, reference_out = refOut, title = 's_ImageColoc |-> Color Histogram'
              live_info, refOut.vis, properties = variable, window_in = refOut.Win
              variable.color = 'Light Gray'
              live_control, refOut.vis, properties = variable, window_in = refOut.Win
              widget_control, ev.top, set_uValue = state, /no_copy
                s_Coloc_upDateViewColorHist, ev.top
              widget_control, ev.top, get_uValue = state, /no_copy
            endif else live_destroy, window_in = 's_ImageColoc |-> Color Histogram'
         endif
       endcase
       'BACKGROUNDCORRECTIONONOFF':state.fBackGroudCorrection = s_ToggleButtonOnOffState(state.wBackGroudCorrectionOnOff)
       'FREQHISTLOGONOFF':begin
          state.fBiFreqHisLog = s_ToggleButtonOnOffState(state.wViewFreqHisLogOnOff)
          if state.fViewFreqHist then begin
             widget_control, ev.top, set_uValue = state, /no_copy
                s_Coloc_upDateViewFreqHist, ev.top
             widget_control, ev.top, get_uValue = state, /no_copy
          endif
       endcase
       'ROIMASKRGYGRYONOFF':begin
          state.fViewROIMaskRGYGRY = s_ToggleButtonOnOffState(state.wViewROIMaskRGYGRYONOFF)
          if state.fViewROIMaskRGYGRY then state.RGYGRYCT = 48 else state.RGYGRYCT = 47
          if state.fViewROIMask then begin
             id = ev.id
             ev.id = state.wViewROIMaskOnOff
             widget_control, ev.top, set_uValue = state, /no_copy
                widget_control, id, set_uValue = uValue, /no_copy
                  s_Coloc_control, ev
                  s_Coloc_control, ev
                widget_control, id, get_uValue = uValue, /no_copy
             widget_control, ev.top, get_uValue = state, /no_copy
             ev.id = id
          endif
       endcase
       'VIEWROIMASKSONOFF':begin
          dimI = size(state.images, /dim)
          if (dimI[0] ge 2) then begin
             state.fViewROIMask = s_ToggleButtonOnOffState(state.wViewROIMaskOnOff)
             if state.fViewROIMask then begin
                live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> ROI-Masks'
                if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> ROI-Masks" does not exist.  Operation not performed.')) then begin $
                  image = make_array(state.dimI[1], state.dimI[2], /byte)
                  tvLCT, rr, gg, bb, /get
                  loadCT, state.RGYGRYCT
                  tvLCT, r, g, b, /get
                  r[0:2] = [255,120,0]
                  g[0:2] = [255,120,0]
                  b[0:2] = [255,120,0]
;                  r[127:191] = 20
;                  g[127:191] = 20
;                  b[127:191] = 20
                  tvLCT, rr, gg, bb
                  if (state.dimI[1] ge state.dimI[2]) then drawDim = [300.*state.dimI[1]/state.dimI[2], 300.] else drawDim = [300., 300.*state.dimI[1]/state.dimI[2]]
                  live_image, image, red = r, green = g, blue = b, draw_dimension = round(drawDim), /no_select, reference_out = refOut, title = 's_ImageColoc |-> ROI-Masks'
                  live_info, refOut.vis, properties = variable, window_in = refOut.Win
                    variable.color = 'Light Gray'
                  live_control, refOut.vis, properties = variable, window_in = refOut.Win
                endif
                widget_control, ev.top, set_uValue = state, /no_copy
                   s_Coloc_upDateROIImage, ev.top
                widget_control, ev.top, get_uValue = state, /no_copy
             endif else live_destroy, window_in = 's_ImageColoc |-> ROI-Masks'
          endif
       endcase
       'VIEWFREQHISTONOFF':begin
          dimI = size(state.images, /dim)
          if (dimI[0] ge 2) then begin
              state.fViewFreqHist = s_ToggleButtonOnOffState(state.wViewFreqHistOnOff)
              if state.fViewFreqHist then begin
                 tvLCT, rr, gg, bb, /get
                 loadCT, 0
                 tvLCT, r, g, b, /get
                 tvLCT, rr, gg, bb
                 image = make_array(296,296, /byte)
                 live_image, image, red = r, green = g, blue = b, draw_dimension = [300,300], /no_select, reference_out = refOut, title = 's_ImageColoc |-> Frequency Histogram'
                 widget_control, ev.top, set_uValue = state, /no_copy
                   s_Coloc_upDateViewFreqHist, ev.top
                 widget_control, ev.top, get_uValue = state, /no_copy
                 live_info, refOut.vis, properties = variable, window_in = refOut.Win
                   variable.color = 'Light Gray'
                 live_control, refOut.vis, properties = variable, window_in = refOut.Win
              endif else live_destroy, window_in = 's_ImageColoc |-> Frequency Histogram'
          endif
       endcase
       'VIEWCCHISTONOFF':begin
          dimI = size(state.images, /dim)
          if (dimI[0] ge 2) then begin
             state.fViewCCHist = s_ToggleButtonOnOffState(state.wViewCCHistOnOff)
             if state.fViewCCHist then begin

                if ptr_valid(state.pViewCCFilter) then begin
                   if not(obj_valid(*state.pViewCCFilter)) then *state.pViewCCFilter = obj_new('C_sImageFilter_ImageColocCostes')
                endif else state.pViewCCFilter = ptr_new(obj_new('C_sImageFilter_ImageColocCostes'), /no_copy)

                pParamStruct = (*state.pViewCCFilter)->getpParamStruct()
                (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Show Costes Histogram'))[0]] = 1b
                a = (*state.pViewCCFilter)->apply(image = state.images[0,*,*], imCh1 = state.images[1,*,*])

             endif else begin
                pParamStruct = (*state.pViewCCFilter)->getpParamStruct()
                (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Show Costes Histogram'))[0]] = 0b

                if (total(state.fROIMasks[*,1]) eq 0) then if obj_valid(*state.pViewCCFilter) then obj_destroy, *state.pViewCCFilter

                live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> CC Histogram'
                if not((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> Merge Channel" does not exist.  Operation not performed.')) then $
                   live_destroy, window_in = 's_ImageColoc |-> CC Histogram'
             endelse
          endif
       endcase
       'VIEWSCATTERHISTONOFF':begin
          dimI = size(state.images, /dim)
          if (dimI[0] ge 2) then begin
             state.fViewScatterHist = s_ToggleButtonOnOffState(state.wViewScatterHistOnOff)
             if state.fViewScatterHist then begin
                widget_control, ev.top, set_uValue = state, /no_copy
                  s_Coloc_getChannelValues, ev.top, ch0_vect = ch0_vect, ch1_vect = ch1_vect
                widget_control, ev.top, get_uValue = state, /no_copy
                PCParam = correlate(ch0_vect, ch1_vect, /double)
                linFitParam = linFit(ch0_vect, ch1_vect, /double, chisq = chisq, sigma = sigma)
                minCh0 = min(ch0_vect, max = maxCh0)
                minCh1 = min(ch1_vect, max = maxCh1)
                xFit = make_array(maxCh0 - minCh0 + 3, /float, /index) + (minCh0 - 1)
                if (finite(PCParam) eq 0) then yFit = xFit*floor(mean(ch1_vect)) else yFit = float(linFitParam[0] + xFit * linFitParam[1])
                xRange = [0, maxCh0]
                yRange = [0, maxCh1]
                iPlot, ch0_vect, ch1_vect, xRange = xRange, yRange = yRange,$
                       xTitle = 'I [Ch0] (y = a + bx: a =' + strCompress(string(linFitParam[0])) + ', b =' + strCompress(string(linFitParam[1])) + ', PC =' + strCompress(string(PCParam)) + ')',$
                       yTitle = 'I [Ch1]',$
                       title = 's_ImageColoc |-> Scatter Histogram', identifier = id, /scatter, /no_save
                iPlot, xFit, yFit, xRange = xRange, yRange = yRange,$
                       color = [255,0,0], overplot = id
                state.idViewScatterHist = id
             endif else itDelete, state.idViewScatterHist
          endif
       endcase
       'VIEWSURFHISTONOFF':begin
          dimI = size(state.images, /dim)
          if (dimI[0] ge 2) then begin
             state.fViewSurfHist = s_ToggleButtonOnOffState(state.wViewSurfHistOnOff)
             if state.fViewSurfHist then begin
                widget_control, ev.top, set_uValue = state, /no_copy
                   s_Coloc_getChannelValues, ev.top, ch0_vect = ch0_vect, ch1_vect = ch1_vect
                widget_control, ev.top, get_uValue = state, /no_copy
                minCh0 = min(ch0_vect, max = maxCh0)
                minCh1 = min(ch1_vect, max = maxCh1)
                surf = congrid(hist_2d(ch0_vect, ch1_vect, min1 = 0, max1 = maxCh0, min2 = 0, max2 = maxCh1, bin1 = 5, bin2 = 5), (maxCh0) > 2, (maxCh1) > 2)
                iSurface, surf, xRange = [0, maxCh0], yRange = [0, maxCh1],$
                          xTitle = 'I [Ch0]', yTitle = 'I [Ch1]',zTitle = 'frequency',$
                          title = 's_ImageColoc |-> Surface Histogram', identifier = id, /no_save
                state.idViewSurfHist = id
             endif else itDelete, state.idViewSurfHist
          endif
       endcase
       'BACKGROUNDDATATABLEONOFF':begin
            dimI = size(state.images, /dim)
            if (dimI[0] ge 2) then begin
               state.fBackGroudCorrection = s_ToggleButtonOnOffState(state.wBackGroudCorrectionOnOff)
               columnNames = strCompress('Image_Channel' + string(0), /rem)
               for i = 1, n_elements(state.backgroudValues) do columnNames = [columnNames, strCompress('Image_Channel' + string(i), /rem)]
               if state.fBackGroudCorrection then begin
                  paramTableuValue = {groupLeader:ev.top,$
                                          paramColumnNames:columnNames,$
                                          param:state.backgroudValues,$
                                          name:'Colocalization Background Correction',$
                                          wTopBase:state.child_DataTable_tlb}
                   s_ObjParamTableWidget, paramTableuValue = paramTableuValue
                   state.child_BackGroudCorrectionTable_tlb = paramTableuValue.wTopBase
               endif else if widget_info(state.child_BackGroudCorrectionTable_tlb, /valid) then widget_control, state.child_BackGroudCorrectionTable_tlb, /destroy
            endif
       endcase
       'SETRANDOMPARAMS':begin
            state.fRandomAndRadius = s_ToggleButtonOnOffState(state.wRandomAndRadiusOnOff)
            if state.fRandomAndRadius then begin
               paramTableuValue = {groupLeader:ev.top,$
                                       paramColumnNames:['Random Steps', 'Radius / Boxsize'],$
                                       param:[state.randomValue, state.radiusValue],$
                                       name:'Colocalization Random and Radius Values',$
                                       wTopBase:state.child_DataTable_tlb}
               s_ObjParamTableWidget, paramTableuValue = paramTableuValue
               state.child_RandomandRadiusTable_tlb = paramTableuValue.wTopBase
            endif else if widget_info(state.child_RandomandRadiusTable_tlb, /valid) then widget_control, state.child_RandomandRadiusTable_tlb, /destroy
       endcase
       'SHIFTMASKS':state.fShiftMasks = s_ToggleButtonOnOffState(state.wShiftMasksOnOff)
       'APPLYDIMXSHIFT':begin
            shiftImage = bytArr(state.dimI[1], state.dimI[2])
            xShift = floor(state.dimI[1]/2.)
            shiftImage[0:xShift-1, *] = state.images[0, (state.dimI[1] - xShift):*,*]
            shiftImage[xShift:*, *] = state.images[0, 0:(state.dimI[1] - xShift - 1),*]
            state.images[0,*,*] = shiftImage
            shiftMask = bytArr(state.dimI[1], state.dimI[2]) + state.ROIMasks[0,*,*]
            shiftMask[0:xShift-1, *] = state.ROIMasks[0, (state.dimI[1] - xShift):*,*]
            shiftMask[xShift:*, *] = state.ROIMasks[0, 0:(state.dimI[1] - xShift - 1),*]
            state.ROIMasks[0,*,*] = shiftMask
            widget_control, ev.top, set_uValue = state, /no_copy
               s_Coloc_update, ev.top
            widget_control, ev.top, get_uValue = state, /no_copy
       endcase
       'XSTEPS':begin
          if (state.fViewDataTable eq 0) then begin
              id = ev.id
              ev.id = state.wViewDataTableOnOff
              widget_control, ev.top, set_uValue = state, /no_copy
                 widget_control, id, set_uValue = uValue, /no_copy
                   s_Coloc_control, ev
                 widget_control, id, get_uValue = uValue, /no_copy
              widget_control, ev.top, get_uValue = state, /no_copy
              ev.id = id
          endif

              ; define shift-image
          shiftImage = bytArr(state.dimI[1], state.dimI[2])
              ; define shift-mask
          shiftMask = bytArr(state.dimI[1], state.dimI[2]) + state.ROIMasks[0,*,*]

          randomCount = state.randomValue < state.dimI[1]
          xShift = floor(state.dimI[1]/randomCount)
          *state.pColocRandomParams = fltArr(randomCount > 1, 12, 8)
          for i = 0, randomCount-1 do begin
             state.fRandomAndRadiusShifts = i+1
             shiftImage[0:xShift-1, *] = state.images[0, (state.dimI[1] - xShift):*,*]
             shiftImage[xShift:*, *] = state.images[0, 0:(state.dimI[1] - xShift - 1),*]
             state.images[0,*,*] = shiftImage
             shiftMask[0:xShift-1, *] = state.ROIMasks[0, (state.dimI[1] - xShift):*,*]
             shiftMask[xShift:*, *] = state.ROIMasks[0, 0:(state.dimI[1] - xShift - 1),*]
             state.ROIMasks[0,*,*] = shiftMask
             widget_control, ev.top, set_uValue = state, /no_copy
                s_Coloc_update, ev.top
             widget_control, ev.top, get_uValue = state, /no_copy
          endfor
          meanAndVar = fltArr(2, 12, 8)
          for i = 0,11 do for j = 0,7 do begin
             meanAndVar[0,i,j] = (*state.pColocRandomParams)[randomCount-1,i,j]
             meanAndVar[1,i,j] = (moment((*state.pColocRandomParams)[0:randomCount-1,i,j]))[1]
          endfor
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  colocRandomParams:*state.pColocRandomParams,$
                                  param:fltArr(12,8) + meanAndVar[0,*,*],$
                                  name:'Random Colocalization PDF-Statistics',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  param:fltArr(12,8) + sqrt(meanAndVar[1,*,*]),$
                                  name:'Random Colocalization Standard Deviation',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          state.fRandomAndRadiusShifts = 0.
          *state.pColocRandomParams = 0.

          s_ISM_getProjectInfo, stack_tlb = state.stack_tlb, zPos = zPos
          widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
             ev_struct = {top:stackState.wTopBase,$
                          id:stackState.wListzSlice,$
                          handler:stackState.wTopBase,$
                          index:zPos,$
                          clicks:1}
          widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
          widget_control, ev.top, set_uValue = state, /no_copy
             widget_control, ev.id, set_uValue = uValue, /no_copy
                s_ISM_List_Event, ev_struct
             widget_control, ev.id, get_uValue = uValue, /no_copy
          widget_control, ev.top, get_uValue = state, /no_copy
       endcase
       'RANDOMROISHIFT':begin
          if (state.fViewDataTable eq 0) then begin
             id = ev.id
             ev.id = state.wViewDataTableOnOff
             widget_control, ev.top, set_uValue = state, /no_copy
                widget_control, id, set_uValue = uValue, /no_copy
                  s_Coloc_control, ev
                widget_control, id, get_uValue = uValue, /no_copy
             widget_control, ev.top, get_uValue = state, /no_copy
             ev.id = id
          endif

              ; define shift-image
          shiftImage = bytArr(state.dimI[1], state.dimI[2]) + state.ROIMasks[0,*,*]

          whereROIs = where(shiftImage gt 0)
          shiftImage[*,*] = state.images[0,*,*]
          dimROIs = n_elements(whereROIs)
          randomCount = state.randomValue < dimROIs
          *state.pColocRandomParams = fltArr(randomCount, 12, 8)
          if (whereROIs[0] ne -1) then for i = 0, randomCount-1 do begin
              state.fRandomAndRadiusShifts = i+1
              shiftImage[whereROIs] = [shiftImage[whereROIs[dimROIs-2:dimROIs-1]], shiftImage[whereROIs[1:dimROIs-2]]]
              state.images[0,*,*] = shiftImage
              widget_control, ev.top, set_uValue = state, /no_copy
                 s_Coloc_update, ev.top
              widget_control, ev.top, get_uValue = state, /no_copy
          endfor
          meanAndVar = fltArr(2, 12, 8)
          for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = (moment((*state.pColocRandomParams)[0:randomCount-1,i,j]))[0:1]
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  colocRandomParams:*state.pColocRandomParams,$
                                  param:fltArr(12,8) + meanAndVar[0,*,*],$
                                  name:'Random Colocalization PDF-Statistics',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  param:fltArr(12,8) + sqrt(meanAndVar[1,*,*]),$
                                  name:'Random Colocalization Standard Deviation',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          state.fRandomAndRadiusShifts = 0.
          *state.pColocRandomParams = 0.

          s_ISM_getProjectInfo, stack_tlb = state.stack_tlb, zPos = zPos
          widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
             ev_struct = {top:stackState.wTopBase,$
                          id:stackState.wListzSlice,$
                          handler:stackState.wTopBase,$
                          index:zPos,$
                          clicks:1}
          widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
          widget_control, ev.top, set_uValue = state, /no_copy
             widget_control, ev.id, set_uValue = uValue, /no_copy
                s_ISM_List_Event, ev_struct
             widget_control, ev.id, get_uValue = uValue, /no_copy
          widget_control, ev.top, get_uValue = state, /no_copy
       endcase
       'RANDOMCOSTESSHIFT':begin
          if (state.fViewDataTable eq 0) then begin
             id = ev.id
             ev.id = state.wViewDataTableOnOff
             widget_control, ev.top, set_uValue = state, /no_copy
                widget_control, id, set_uValue = uValue, /no_copy
                  s_Coloc_control, ev
                widget_control, id, get_uValue = uValue, /no_copy
             widget_control, ev.top, get_uValue = state, /no_copy
             ev.id = id
          endif
          randomCount = state.randomValue
          *state.pColocRandomParams = fltArr(randomCount > 1, 12, 8)

          oScrambleImage = obj_new('C_sImageFilter_ScrambleImage')
          pParamStruct = oScrambleImage->getpParamStruct()
          (*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Scramble Box Size'))[0]] = state.radiusValue

          image = bytArr(state.dimI[1], state.dimI[2]) + state.images[0,*,*]
          mask = bytArr(state.dimI[1], state.dimI[2]) + state.ROIMasks[0,*,*]

          nDim = state.dimI[1] * state.dimI[2]
          fShiftMasks = state.fShiftMasks
          state.fShiftMasks = 1b
          for i = 0, randomCount-1 do begin
             (*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Random Var 1'))[0]] = i
             (*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Random Var 2'))[0]] = state.images[i mod nDim]
             state.fRandomAndRadiusShifts = i+1

             widget_control, ev.top, set_uValue = state, /no_copy
                s_Coloc_update, ev.top
             widget_control, ev.top, get_uValue = state, /no_copy
             randomVar = 0l
             state.images[0,*,*] = oScrambleImage->apply(image = image, randomVar = randomVar)
             state.ROIMasks[0,*,*] = oScrambleImage->apply(image = mask, randomVar = randomVar, fKeepRandomVar = 1b)
          endfor
          state.fShiftMasks = fShiftMasks
          state.images[0,*,*] = image
          state.ROIMasks[0,*,*] = mask
          obj_destroy, oScrambleImage

          meanAndVar = fltArr(2, 12, 8)
          for i = 0,11 do for j = 0,7 do begin
             meanAndVar[0,i,j] = (*state.pColocRandomParams)[0,i,j]
             meanAndVar[1,i,j] = (moment((*state.pColocRandomParams)[*,i,j]))[1]
          endfor
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  colocRandomParams:*state.pColocRandomParams,$
                                  param:fltArr(12,8) + meanAndVar[0,*,*],$
                                  name:'Random Colocalization PDF-Statistics',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  param:fltArr(12,8) + sqrt(meanAndVar[1,*,*]),$
                                  name:'Random Colocalization Standard Deviation',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          state.fRandomAndRadiusShifts = 0.
          *state.pColocRandomParams = 0.

          s_ISM_getProjectInfo, stack_tlb = state.stack_tlb, zPos = zPos
          widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
             ev_struct = {top:stackState.wTopBase,$
                          id:stackState.wListzSlice,$
                          handler:stackState.wTopBase,$
                          index:zPos,$
                          clicks:1 }
          widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
          widget_control, ev.top, set_uValue = state, /no_copy
             widget_control, ev.id, set_uValue = uValue, /no_copy
                s_ISM_List_Event, ev_struct
             widget_control, ev.id, get_uValue = uValue, /no_copy
          widget_control, ev.top, get_uValue = state, /no_copy

        endcase
       'RADSHIFT_I_CH0':begin
          if not(state.fViewDataTable) then begin
             id = ev.id
             ev.id = state.wViewDataTableOnOff
             widget_control, ev.top, set_uValue = state, /no_copy
                widget_control, id, set_uValue = uValue, /no_copy
                   s_Coloc_control, ev
                widget_control, id, get_uValue = uValue, /no_copy
             widget_control, ev.top, get_uValue = state, /no_copy
             ev.id = id
          endif

              ; define shift-image & shift-mask
          shiftImage = reform(state.images[0,*,*])
          shiftMask = reform(state.ROIMasks[0,*,*])

              ; define correlationMatrix-dimension
          correlDim = 0
          for k = -state.radiusValue,state.radiusValue do for l = -state.radiusValue,state.radiusValue do if ( (1.*(k*k+l*l)) le (state.radiusValue)^2 ) then correlDim += 1
          *state.pColocRandomParams = fltArr(correlDim > 1, 12, 8)

          fShiftMasks = state.fShiftMasks
          state.fShiftMasks = 1b
          state.fRandomAndRadiusShifts = 0.
          for k = -state.radiusValue,state.radiusValue do for l = -state.radiusValue,state.radiusValue do begin
              if ( (1.*(k*k+l*l)) le (state.radiusValue)^2 ) then begin
                 state.fRandomAndRadiusShifts += 1
                 state.images[0,*,*] = shift(shiftImage,k,l)
                 state.ROIMasks[0,*,*] = shift(shiftMask,k,l)
                 widget_control, ev.top, set_uValue = state, /no_copy
                     s_Coloc_update, ev.top
                 widget_control, ev.top, get_uValue = state, /no_copy
              endif
          endfor
          state.fShiftMasks = fShiftMasks

          state.images[0,*,*] = shiftImage
          state.ROIMasks[0,*,*] = shiftMask

          meanAndVar = fltArr(2, 12, 8)
          whereData = where((*state.pColocRandomParams)[*,0,0] ne 0, count)
          case count of
            0: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = [0., 0.]
            1: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = [(*state.pColocRandomParams)[0,i,j], 0.]
            else: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = (moment((*state.pColocRandomParams)[whereData,i,j]))[0:1]
          endcase

          meanAndVar[1,*,*] = sqrt(meanAndVar[1,*,*])
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  colocRandomParams:*state.pColocRandomParams,$
                                  param:fltArr(12,8) + meanAndVar[0,*,*],$
                                  name:'Random Colocalization Radius-Means',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  param:fltArr(12,8) + sqrt(meanAndVar[1,*,*]),$
                                  name:'Random Colocalization Standard Deviation',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          state.fRandomAndRadiusShifts = 0.
          (*state.pColocRandomParams)[*] = 0.

          s_ISM_getProjectInfo, stack_tlb = state.stack_tlb, zPos = zPos
          widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
             ev_struct = {top:stackState.wTopBase,$
                                   id:stackState.wListzSlice,$
                                   handler:stackState.wTopBase,$
                                   index:zPos,$
                                   clicks:1}
          widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
          widget_control, ev.top, set_uValue = state, /no_copy
             widget_control, ev.id, set_uValue = uValue, /no_copy
                s_ISM_List_Event, ev_struct
             widget_control, ev.id, get_uValue = uValue, /no_copy
          widget_control, ev.top, get_uValue = state, /no_copy
       endcase
       'RADSHIFT_I_GLOBAL_ROI_CH0':begin
          if state.fGlobalROI then begin
             if not(state.fViewDataTable) then begin
                id = ev.id
                ev.id = state.wViewDataTableOnOff
                widget_control, ev.top, set_uValue = state, /no_copy
                   widget_control, id, set_uValue = uValue, /no_copy
                     s_Coloc_control, ev
                   widget_control, id, get_uValue = uValue, /no_copy
                widget_control, ev.top, get_uValue = state, /no_copy
                ev.id = id
             endif

                 ; define shift-images & ROIs for xy-shifts
             oldMask = state.globalROI
             whereROIs = where(oldMask gt 0)

             shiftIxy = rotate(oldMask, 1)
             whereYROIs = where(shiftIxy gt 0)

             oldImage = state.images[0,*,*]
             shiftIxy = bytArr(state.dimI[1], state.dimI[2])
             oldMask = state.ROIMasks[0,*,*]
             shiftMxy = bytArr(state.dimI[1], state.dimI[2])

                 ; define correlationMatrix-dimension
             correlDim = 0L
             for k = -long(state.radiusValue),long(state.radiusValue) do for l = -long(state.radiusValue),long(state.radiusValue) do if ( (1.*(k*k+l*l)) le (long(state.radiusValue))^2 ) then correlDim += 1
             *state.pColocRandomParams = fltArr(correlDim > 1, 12, 8)

             fShiftMasks = state.fShiftMasks
             state.fShiftMasks = 1b
             state.fZProjectionUpdate = 0b
             state.fRandomAndRadiusShifts = 0.
             for k = -long(state.radiusValue),long(state.radiusValue) do for l = -long(state.radiusValue),long(state.radiusValue) do begin
                 if ( (1.*(k*k+l*l)) le (long(state.radiusValue))^2 ) then begin
                    state.fRandomAndRadiusShifts += 1
                    shiftIxy[whereROIs] = shift(oldImage[whereROIs], k)
                    shiftIxy = rotate(shiftIxy, 1)
                    shiftIxy[whereYROIs] = shift(shiftIxy[whereYROIs], l)
                    shiftIxy = rotate(shiftIxy, 3)

                    dummy = oldImage
                    dummy[whereROIs] = shiftIxy[whereROIs]
                    state.images[0,*,*] = dummy

                    shiftMxy[whereROIs] = shift(oldMask[whereROIs], k)
                    shiftMxy = rotate(shiftMxy, 1)
                    shiftMxy[whereYROIs] = shift(shiftMxy[whereYROIs], l)
                    shiftMxy = rotate(shiftMxy, 3)

                    dummy = oldMask
                    dummy[whereROIs] = shiftMxy[whereROIs]
                    state.ROIMasks[0,*,*] = dummy

                    widget_control, ev.top, set_uValue = state, /no_copy
                       s_Coloc_update, ev.top
                    widget_control, ev.top, get_uValue = state, /no_copy
                 endif
             endfor
             state.fZProjectionUpdate = 1b
             state.fShiftMasks = fShiftMasks

             meanAndVar = fltArr(2, 12, 8)
             whereData = where((*state.pColocRandomParams)[*,0,0] ne 0, count)
             case count of
                0: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = [0., 0.]
                1: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = [(*state.pColocRandomParams)[0,i,j], 0.]
                else: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = (moment((*state.pColocRandomParams)[whereData,i,j]))[0:1]
             endcase

             meanAndVar[1,*,*] = sqrt(meanAndVar[1,*,*])
             paramTableuValue = {groupLeader:ev.top,$
                                     paramColumnNames:state.colocalizationColumnNames,$
                                     paramRowNames:state.colocalizationRowNames,$
                                     colocRandomParams:*state.pColocRandomParams,$
                                     param:fltArr(12,8) + meanAndVar[0,*,*],$
                                     name:'Random Colocalization Radius-Means',$
                                     wTopBase:-1}
             widget_control, ev.top, set_uValue = state, /no_copy
                s_ObjParamTableWidget, paramTableuValue = paramTableuValue
             widget_control, ev.top, get_uValue = state, /no_copy
             paramTableuValue = {groupLeader:ev.top,$
                                     paramColumnNames:state.colocalizationColumnNames,$
                                     paramRowNames:state.colocalizationRowNames,$
                                     param:fltArr(12,8) + sqrt(meanAndVar[1,*,*]),$
                                     name:'Random Colocalization Standard Deviation',$
                                     wTopBase:-1}
             widget_control, ev.top, set_uValue = state, /no_copy
                s_ObjParamTableWidget, paramTableuValue = paramTableuValue
             widget_control, ev.top, get_uValue = state, /no_copy
             state.fRandomAndRadiusShifts = 0b
             (*state.pColocRandomParams)[*] = 0.

             s_ISM_getProjectInfo, stack_tlb = state.stack_tlb, zPos = zPos
             widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
                ev_struct = {top:stackState.wTopBase,$
                             id:stackState.wListzSlice,$
                             handler:stackState.wTopBase,$
                             index:zPos,$
                             clicks:1}
             widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
             widget_control, ev.top, set_uValue = state, /no_copy
                widget_control, ev.id, set_uValue = uValue, /no_copy
                   s_ISM_List_Event, ev_struct
                widget_control, ev.id, get_uValue = uValue, /no_copy
             widget_control, ev.top, get_uValue = state, /no_copy
          endif
       endcase
       'APPLYDDACOLOCALIZATIONEXTRA':begin

          imageCh1 = reform(state.images[0,*,*])
          imageCh2 = reform(state.images[1,*,*])
          imageSeg1 = reform(state.ROIMasks[0,*,*])
          imageSeg2 = reform(state.ROIMasks[1,*,*])
          if state.fGlobalROI then imageConf = state.globalROI else imageConf = bytArr(state.dimI[1], state.dimI[2]) + 1

          DDARad = state.radiusValue > 1
          DDAPlus = state.randomValue < (state.radiusValue - 1)

          dummy = s_DDA_Colocalization_old(imageCh1 = imageCh1, imageCh2 = imageCh2, imageSeg1 = imageSeg1, imageSeg2 = imageSeg2, imageConf = imageConf, DDARad = DDARad, DDAPlus = DDAPlus)
;          dummy = s_DDA_Colocalization(imageCh1 = imageCh1, imageCh2 = imageCh2, imageSeg1 = imageSeg1, imageSeg2 = imageSeg2, imageConf = imageConf, DDARad = DDARad, DDAPlus = DDAPlus)
       endcase
       'RADSHIFT_I_ROI_CH0':begin
          if (state.fViewDataTable eq 0) then begin
             id = ev.id
             ev.id = state.wViewDataTableOnOff
             widget_control, ev.top, set_uValue = state, /no_copy
                widget_control, id, set_uValue = uValue, /no_copy
                  s_Coloc_control, ev
                widget_control, id, get_uValue = uValue, /no_copy
             widget_control, ev.top, get_uValue = state, /no_copy
             ev.id = id
          endif

              ; define shift-images & ROIs for xy-shifts
          oldImage = reform(state.ROIMasks[0,*,*])
          whereROIs = where(oldImage gt 0)

          shiftIxy = rotate(oldImage, 1)
          whereYROIs = where(shiftIxy gt 0)

          oldImage[*,*] = state.images[0,*,*]
          shiftIxy = bytArr(state.dimI[1], state.dimI[2])

              ; define correlationMatrix-dimension
          correlDim = 0
          for k = -state.radiusValue,state.radiusValue do for l = -state.radiusValue,state.radiusValue do if ( (1.*(k*k+l*l)) le (state.radiusValue)^2 ) then correlDim += 1
          *state.pColocRandomParams = fltArr(correlDim > 1, 12, 8)

          state.fZProjectionUpdate = 0b
          state.fRandomAndRadiusShifts = 0.
          for k = -state.radiusValue,state.radiusValue do for l = -state.radiusValue,state.radiusValue do begin
              if ( (1.*(k*k+l*l)) le (state.radiusValue)^2 ) then begin
                 state.fRandomAndRadiusShifts += 1
                 shiftIxy[whereROIs] = shift(oldImage[whereROIs], k)
                 shiftIxy = rotate(shiftIxy, 1)
                 shiftIxy[whereYROIs] = shift(shiftIxy[whereYROIs], l)
                 shiftIxy = rotate(shiftIxy, 3)

                 dummy = oldImage
                 dummy[whereROIs] = shiftIxy[whereROIs]
                 state.images[0,*,*] = dummy
                 widget_control, ev.top, set_uValue = state, /no_copy
                    s_Coloc_update, ev.top
                 widget_control, ev.top, get_uValue = state, /no_copy
              endif
          endfor
          state.fZProjectionUpdate = 1b

          meanAndVar = fltArr(2, 12, 8)
          whereData = where((*state.pColocRandomParams)[*,0,0] ne 0, count)
          case count of
            0: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = [0., 0.]
            1: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = [(*state.pColocRandomParams)[0,i,j], 0.]
            else: for i = 0,11 do for j = 0,7 do meanAndVar[0:1, i, j] = (moment((*state.pColocRandomParams)[whereData,i,j]))[0:1]
          endcase

          meanAndVar[1,*,*] = sqrt(meanAndVar[1,*,*])
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  colocRandomParams:*state.pColocRandomParams,$
                                  param:fltArr(12,8) + meanAndVar[0,*,*],$
                                  name:'Random Colocalization Radius-Means',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          paramTableuValue = {groupLeader:ev.top,$
                                  paramColumnNames:state.colocalizationColumnNames,$
                                  paramRowNames:state.colocalizationRowNames,$
                                  param:fltArr(12,8) + sqrt(meanAndVar[1,*,*]),$
                                  name:'Random Colocalization Standard Deviation',$
                                  wTopBase:-1}
          widget_control, ev.top, set_uValue = state, /no_copy
             s_ObjParamTableWidget, paramTableuValue = paramTableuValue
          widget_control, ev.top, get_uValue = state, /no_copy
          state.fRandomAndRadiusShifts = 0.
          (*state.pColocRandomParams)[*] = 0.

          s_ISM_getProjectInfo, stack_tlb = state.stack_tlb, zPos = zPos
          widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
             ev_struct = {top:stackState.wTopBase,$
                                   id:stackState.wListzSlice,$
                                   handler:stackState.wTopBase,$
                                   index:zPos,$
                                   clicks:1}
          widget_control, state.stack_tlb, set_uValue = stackState, /no_copy
          widget_control, ev.top, set_uValue = state, /no_copy
             widget_control, ev.id, set_uValue = uValue, /no_copy
                s_ISM_List_Event, ev_struct
             widget_control, ev.id, get_uValue = uValue, /no_copy
          widget_control, ev.top, get_uValue = state, /no_copy
       endcase
       'LOADGLOBALROI':begin
          state.fGlobalROI = s_ToggleButtonOnOffState(state.wGlobalROIOnOff)
          if state.fGlobalROI then begin
            file = dialog_pickfile( /read, path = 'c:/rsi', filter = '*.tif')
            dummy = query_tiff(file[0])
            if dummy then state.globalROI = (read_tiff(file[0]) gt 0) else state.fGlobalROI = s_ToggleButtonOnOffState(state.wGlobalROIOnOff)
          endif else state.globalROI = bytArr(3,3)
       endcase
       'VIEWDATATABLEONOFF':begin
          dimI = size(state.images, /dim)
          if (dimI[0] ge 2) then begin
             state.fViewDataTable = s_ToggleButtonOnOffState(state.wViewDataTableOnOff)
             if state.fViewDataTable then begin
                widget_control, ev.top, set_uValue = state, /no_copy
                   s_Coloc_getChannelValues, ev.top, ch0_vect = ch0_vect, ch1_vect = ch1_vect
                   s_Coloc_calcuateColocalizationValues, ev.top
                widget_control, ev.top, get_uValue = state, /no_copy

;                openW,2, 'c:\RSI\Data.dat'
;                printF, 2, (state.colocalizationParams)[0:9,0:7]
;                close, 2

                paramTableuValue = {groupLeader:ev.top,$
                                     paramColumnNames:state.colocalizationColumnNames,$
                                     paramRowNames:state.colocalizationRowNames,$
                                     param:state.colocalizationParams,$
                                     name:'Colocalization',$
                                     wTopBase:state.child_DataTable_tlb}
                 s_ObjParamTableWidget, paramTableuValue = paramTableuValue
                 state.child_DataTable_tlb = paramTableuValue.wTopBase
             endif else if widget_info(state.child_DataTable_tlb, /valid) then widget_control, state.child_DataTable_tlb, /destroy
          endif
       endcase
       else:
    endcase
    widget_control, ev.id, set_uValue = uValue, /no_copy
  widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Coloc_Window, images, application_tlb = application_tlb, groupLeader = groupLeader, pPreState = pPreState, stack_tlb = stack_tlb

   if (n_elements(groupLeader) eq 0) then groupLeader = 0
   if (n_elements(stack_tlb) eq 0) then stack_tlb = groupLeader

   if (n_elements(images) eq 0) then begin
      filename = filePath(subDir = ['examples', 'data'], 'worldelv.dat')
      openR, lun, filename, /get_lun
      backgroundImage = bytArr(360,360)
      readU, lun, backgroundImage
      free_lun, lun
      filename = filePath(subDir = ['examples', 'data'], 'ctscan.dat')
      openR, lun, filename, /get_lun
      foregroundImage = bytArr(256,256)
      readU, lun, foregroundImage
      free_lun, lun
      images = bytArr(2, 360, 360)
      backgroundImage[*] = 0
      backgroundImage[0:239, 0:239] = 255
;     backgroundImage[180:269, 180:269] = 255

      images[0,*,*] = backgroundImage
      images[1,*,*] = congrid(foregroundImage,360,360)
   endif

       ; rescales non-byte images
;    if (size(images, /type) ne 1) then images = bytscl(images, min = 0, max = 4096)

    dimI = size(images, /dim)
    globalROI = bytArr(dimI[1],dimI[2])
    alpha_image = bytArr(4, dimI[1],dimI[2])
    backgroudValues = make_array(dimI[0], /int)
    ROIMasks = make_array(dimI[0]<2, dimI[1], dimI[2], /byte)
    opacMasks = make_array(dimI[0], dimI[1],dimI[2], /byte, value = 1b)
    opacFlag = make_array(dimI[0], /byte, value = 0b)
    colTbl = make_array(dimI[0], /byte)
    rgbValues = make_array(dimI[0], 3, 256, /byte)
    gamma = make_array(dimI[0], /float, value = 1.)
    opacGamma = gamma
    stretchBotTop = make_array(dimI[0], 2, /byte)
    stretchBotTop[*,1] = 255b
    opacStretchBotTop = stretchBotTop
    blendValues = make_array(dimI[0], /byte, value = 128b)
    opacValues = make_array(dimI[0], 256, /byte, value = blendValues[0])
    bottomValues = make_array(dimI[0], /byte)
    topValues = make_array(dimI[0], /byte, value = 255b)

    if ptr_valid(pPreState) then for i = 0, dimI[0]-1 do begin
       colTbl[i] = (*pPreState).colTbl[i]
       rgbValues[i,*,*] = (*pPreState).rgbValues[i,*,*]
       stretchBotTop[i] = (*pPreState).stretchBotTop[i]
       gamma[i] = (*pPreState).gamma[i]
       opacFlag[i] = (*pPreState).opacFlag[i]
       blendValues[i] = (*pPreState).blendValues[i]
       bottomValues[i] = (*pPreState).bottomValues[i]
       topValues[i] = (*pPreState).topValues[i]
       opacGamma[i] = (*pPreState).opacGamma[i]
       opacStretchBotTop[i] = (*pPreState).opacStretchBotTop[i]
       opacValues[i,*] = (*pPreState).opacValues[i,*]
    endfor

    mask = make_array(dimI[1],dimI[2], /byte, value = 1b)
    colorPalette = [41, 42, 0, 3, 1]
    thisContainer = obj_new('IDL_Container')
    thisModel = obj_new('IDLgrModel')
    thisContainer->add, thisModel
    oImage = obj_new('IDLgrImage', mask*0, dimensions = [dimI[1],dimI[2]], interleave = 0)
    thisModel->add, oImage
    thisContainer->add, oImage

    for i = 0, dimI[0]-1 do begin
       if not(ptr_valid(pPreState)) then begin
         bottomValues[i] = min(images[i,*,*])
         backgroudValues[i] = bottomValues[i]
       endif
       whereOpacMin = where(images[i,*,*] lt bottomValues[i])
       if (whereOpacMin[0] ne -1) then mask[whereOpacMin] = 0b
       opacMasks[i,*,*] = mask
       mask[*,*] = 1b
       if ptr_valid(pPreState) then begin
          loadCT, colTbl[i]
          r = bytArr(256) + rgbValues[i,0,*]
          g = bytArr(256) + rgbValues[i,1,*]
          b = bytArr(256) + rgbValues[i,2,*]
       endif else begin
          loadCT, colorPalette[i]
          colTbl[i] = colorPalette[i]
          tvLCT, r, g, b, /get
       endelse
       rgbValues[i,0,*] = r
       rgbValues[i,1,*] = g
       rgbValues[i,2,*] = b
       alpha_image[0,*,*] = r[images[i,*,*]]
;     images[i,*,*] = rotate(bytArr(dimI[1],dimI[2]) + images[i,*,*], 1)
       alpha_image[1,*,*] = g[images[i,*,*]]
;     images[i,*,*] = rotate(bytArr(dimI[1],dimI[2]) + images[i,*,*], 1)
       alpha_image[2,*,*] = b[images[i,*,*]]
       alpha_image[3,*,*] = opacMasks[i,*,*] * blendValues[i]

       iName = strCompress('Image_Channel' + string(i), /rem)
       oImage = obj_new('IDLgrImage', alpha_image, dimensions = [dimI[1],dimI[2]], interleave = 0, blend_func = [3,4], name = iName)
       thisModel->add, oImage
       thisContainer->add, oImage
    endfor

        ; Create a view.
    viewRect = [0, 0, dimI[1], dimI[2]]
    thisView = obj_new('IDLgrView', Viewplane_Rect = viewRect)
    thisView->add, thisModel
    thisContainer->add, thisView

    thisModel2 = obj_new('IDLgrModel')
    thisContainer->add, thisModel2
    oImage = obj_new('IDLgrImage', alpha_image[0:2,*,*]*0., dimensions = [dimI[1],dimI[2]], interleave = 0, name = 'mergeImage')
    thisModel2->add, oImage
    thisContainer->add, oImage
    thisView2 = obj_new('IDLgrView', ViewPlane_rect = viewRect)
    thisView2->add, thisModel2
    thisContainer->add, thisView2

        ; Create the widgets for this program.
    wTopBase = widget_base(title = 's_ImageColoc |->', tlb_size_events = 1)
    application_tlb = wTopBase
    wFlexBase = widget_base(wTopBase, map = 0, column = 1)
    wDraw = widget_draw(wTopBase, xSize = dimI[1], ySize = dimI[2], graphics_level = 2, button_events = 1, expose_events = 1, retain = 2, event_pro = 's_Coloc_drawEvents')

    wColorTable = widget_button(wFlexBase, value = 'Color Tables', /menu)
    wOpacityTable = widget_button(wFlexBase, value = 'Opacity Tables', /menu)
    for i = 0, dimI[0]-1 do begin
       wFlexBase2 = widget_base(wFlexBase, column = 1, /frame)
       iTitle = 'Color Table for Channel:' + strCompress(string(i), /rem)
       void = widget_button(wColorTable, value = iTitle, uValue = i, event_pro = 's_Coloc_colors')
       iTitle = 'Opacity Table for Channel:' + strCompress(string(i), /rem)
       void = widget_button(wOpacityTable, value = iTitle, uValue = i, event_pro = 's_Coloc_vector')
       iTitle = strCompress('Opacity_Control_Ch ' + string(i), /rem)
       void = widget_slider(wFlexBase2, min = 0, max = 255, value = 128, title = iTitle, uValue = i, event_pro = 's_Coloc_slider')
       iTitle = strCompress('Cut_Bottom_Ch ' + string(i), /rem)
       void = widget_slider(wFlexBase2, min = 0, max = 255, value = bottomValues[i], title = iTitle, uValue = i, event_pro = 's_Coloc_cutBottom_slider')
       iTitle = strCompress('Cut_Top_Ch ' + string(i), /rem)
       void = widget_slider(wFlexBase2, min = 0, max = 255, value = topValues[i], title = iTitle, uValue = i, event_pro = 's_Coloc_cutTop_slider')
    endfor
    wMergeColorTablesOnOff = widget_button(wColorTable, value = 'Merge Color Tables (off)', event_pro = 's_Coloc_mergeColorTablesOnOff', /sep)
    wMaxZProjectionOnOff = widget_button(wColorTable, value = 'Max Z-Projection (off)', uValue = 'MAXZPROJECTION', event_pro = 's_Coloc_ZProjectionOnOff', /sep)
    wMeanZProjectionOnOff = widget_button(wColorTable, value = 'Mean Z-Projection (off)', uValue = 'MEANZPROJECTION', event_pro = 's_Coloc_ZProjectionOnOff')

    if (dimI[0] lt 2) then sensitive = 0 else sensitive = 1
    wColocTable = widget_button(wFlexBase, value = 'colocalization', /menu, sensitive = sensitive)
    void = widget_button(wColocTable, value = 'Scale:', sensitive = 0)
    void = widget_button(wColocTable, value = '...|-> Apply Intensity Scale Channel:', event_pro = 's_Coloc_scaleIntVolume')
    for i = 0, 1 do begin
       iTitle = '...|-> Apply Intensity Scale Channel:' + strCompress(string(i), /rem)
       void = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_scaleInt')
    endfor
    for i = 0, 1 do begin
       iTitle = '...|-> Apply Mean Equalization Channel:' + strCompress(string(i), /rem)
       void = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_scaleEqualInt')
    endfor
    wBackGroudCorrectionOnOff = widget_button(wColocTable, value = 'Backgroud Correction (off)', uValue = 'BACKGROUNDDATATABLEONOFF', event_pro = 's_Coloc_control')

    void = widget_button(wColocTable, value = 'Randomize:', sensitive = 0, /sep)
    wRandomAndRadiusOnOff = widget_button(wColocTable, value = '...|-> Set Random Parameters (off)', uValue = 'SETRANDOMPARAMS', event_pro = 's_Coloc_control')
    wShiftMasksOnOff = widget_button(wColocTable, value = '...|-> Shift Masks (off)', uValue = 'SHIFTMASKS', event_pro = 's_Coloc_control')
    void = widget_button(wColocTable, value = '...||-> Apply x-Dim/2-Shift for Ch 0', uValue = 'APPLYDIMXSHIFT', event_pro = 's_Coloc_control')
    void = widget_button(wColocTable, value = '...||-> Apply x-Steps for Ch 0', uValue = 'XSTEPS', event_pro = 's_Coloc_control')
    void = widget_button(wColocTable, value = '...||-> Apply Random-Shifts in ROI for Ch 0', uValue = 'RANDOMROISHIFT', event_pro = 's_Coloc_control')
    void = widget_button(wColocTable, value = '...||-> Apply Costes Box Scamble for Ch 0', uValue = 'RANDOMCOSTESSHIFT', event_pro = 's_Coloc_control')
    void = widget_button(wColocTable, value = '...||-> Apply Rad-Shift for I in Ch 0', uValue = 'RADSHIFT_I_CH0', event_pro = 's_Coloc_control')
    void = widget_button(wColocTable, value = '...||-> Apply Rad-Shift for I in ROI-Ch 0', uValue = 'RADSHIFT_I_ROI_CH0', event_pro = 's_Coloc_control')
    wGlobalROIOnOff = widget_button(wColocTable, value = 'Load global ROI (off)', uValue = 'LOADGLOBALROI', event_pro = 's_Coloc_control')
    void = widget_button(wColocTable, value = '...||-> Apply Rad-Shift global ROI for Ch 0', uValue = 'RADSHIFT_I_GLOBAL_ROI_CH0', event_pro = 's_Coloc_control')
    void = widget_button(wColocTable, value = '...||-> Apply DDA_Colocalization_Extra', uValue = 'APPLYDDACOLOCALIZATIONEXTRA', event_pro = 's_Coloc_control')

    wROIMasksOnOff = make_array(2, 7, /long)
    fROIMasks = make_array(2, 7, /byte)
    fROIMasks[*,0] = [1b,1b]
    for i = 0, 1 do begin
       iTitle = '     ROI for Channel:' + strCompress(string(i), /rem)
       if (i eq 0) then void = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect', /sep, sensitive = 0) $
          else void = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect', sensitive = 0)
       iTitle = '...|-> Non-Zero Cut-Values (on )'
       wROIMasksOnOff[i,0] = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect')
       iTitle = '...|-> Use Costes Thresholds (off)'
       wROIMasksOnOff[i,1] = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect')
       iTitle = '...|-> Segmented Mask (off)'
       wROIMasksOnOff[i,2] = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect')
       iTitle = '...|-> Saved Mask (off)'
       wROIMasksOnOff[i,3] = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect')
       iTitle = '...|-> Saved ROI2D-Mask (off)'
       wROIMasksOnOff[i,4] = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect')
       iTitle = '...|-> Saved ROI3D-Mask (off)'
       wROIMasksOnOff[i,5] = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect')
       iTitle = '...||-> Set Mask Parameters (off)'
       wROIMasksOnOff[i,6] = widget_button(wColocTable, value = iTitle, uValue = i, event_pro = 's_Coloc_maskSelect')
    endfor

    void = widget_button(wColocTable, value = 'Plot Windows:', sensitive = 0, /sep)
    wViewDataTableOnOff = widget_button(wColocTable, value = '...||-> View Data Table (off)', uValue = 'VIEWDATATABLEONOFF',event_pro = 's_Coloc_control')
    wViewCCHistOnOff = widget_button(wColocTable, value = '...||-> View Costes Histogram (off)', uValue = 'VIEWCCHISTONOFF',event_pro = 's_Coloc_control')
    wViewSurfHistOnOff = widget_button(wColocTable, value = '...||-> View Surface Histogram (off)', uValue = 'VIEWSURFHISTONOFF',event_pro = 's_Coloc_control')
    wViewScatterHistOnOff = widget_button(wColocTable, value = '...||-> View Scatter Histogram (off)', uValue = 'VIEWSCATTERHISTONOFF',event_pro = 's_Coloc_control')
    wViewColorHistOnOff = widget_button(wColocTable, value = '...||-> View Color Histogram (off)', uValue = 'VIEWCOLORHISTONOFF',event_pro = 's_Coloc_control')
    wViewFreqHisLogOnOff = widget_button(wColocTable, value = '...|-> Logarithm (on )', uValue = 'FREQHISTLOGONOFF',event_pro = 's_Coloc_control')
    wViewFreqHistOnOff = widget_button(wColocTable, value = '...||-> View Frequency Histogram (off)', uValue = 'VIEWFREQHISTONOFF',event_pro = 's_Coloc_control')
    wViewROIMaskRGYGRYONOFF = widget_button(wColocTable, value = '...|-> Switch RGY->GRY CT (off)', uValue = 'ROIMASKRGYGRYONOFF',event_pro = 's_Coloc_control')
    wViewROIMaskOnOff = widget_button(wColocTable, value = '...||-> View ROI-Masks (off)', uValue = 'VIEWROIMASKSONOFF',event_pro = 's_Coloc_control')

       ; Get geometry information for resizing.
    tlbGeo = widget_info(wTopBase, /geometry)
    drawGeo = widget_info(wDraw, /geometry)
    drawScale = float(drawGeo.scr_ySize) / tlbGeo.ySize

        ; realize the widgets and get the window object.
    widget_control, wTopBase, /realize
    widget_control, wDraw, get_value = thisWindow
    thisWindow->draw, thisView
    thisContainer->add, thisWindow

    state = {thisContainer:thisContainer,$      ; The container object.
             wTopBase:wTopBase,$
             groupLeader:groupLeader,$
             stack_tlb:stack_tlb,$
             images:images,$
             dimI:dimI,$
             thisWindow:thisWindow,$           ; The window object.
             map:0,$
             wFlexBase:wFlexBase,$
             opacFlag:opacFlag,$              ; Flag defining opac-Method for each channel. 0/constant (slider) | 1/flexible (r from colorTbl)
             opacMasks:opacMasks,$            ; Masks for the opacity  values.
             ROIMasks:ROIMasks,$              ; ROI-Masks for colocalization.
             maskSpecifier:['Time', 'Channel', 'ZSlize', 'Cluster'],$
             maskSpecifierValue:make_array(2, 4, /string, value = 'free'),$
             wROIMasksOnOff:wROIMasksOnOff,$
                fROIMasks:fROIMasks,$
                child_ROIMasks_tlb:[-1l,-1l],$
             blendValues:blendValues,$
             opacValues:opacValues,$
             colTbl:colTbl,$
             wMergeColorTablesOnOff:wMergeColorTablesOnOff,$
                fMergeColorTables:0b,$
             wMaxZProjectionOnOff:wMaxZProjectionOnOff,$
                fMaxZProjection:0b,$
             wMeanZProjectionOnOff:wMeanZProjectionOnOff,$
                fMeanZProjection:0b,$
                fZProjectionUpdate:1b,$
             wBackGroudCorrectionOnOff:wBackGroudCorrectionOnOff,$
                fBackGroudCorrection:0b,$
                child_BackGroudCorrectionTable_tlb:-1l,$
                backgroudValues:backgroudValues,$
             wRandomAndRadiusOnOff:wRandomAndRadiusOnOff,$
                fRandomAndRadius:0b,$
                child_RandomandRadiusTable_tlb:-1l,$
                randomValue:20,$
                radiusValue:5,$
             wGlobalROIOnOff:wGlobalROIOnOff,$
                fGlobalROI:0b,$
                globalROI:globalROI,$
             wShiftMasksOnOff:wShiftMasksOnOff,$
                fShiftMasks:0b,$
             wViewROIMaskRGYGRYONOFF:wViewROIMaskRGYGRYONOFF,$
                fViewROIMaskRGYGRY:0b,$
                RGYGRYCT:47,$
             wViewROIMaskOnOff:wViewROIMaskOnOff,$
                fViewROIMask:0b,$
             wViewColorHistOnOff:wViewColorHistOnOff,$
                fViewColorHist:0b,$
             wViewScatterHistOnOff:wViewScatterHistOnOff,$
                fViewScatterHist:0b,$
                idViewScatterHist:'id',$
             wViewFreqHistOnOff:wViewFreqHistOnOff,$
                fViewFreqHist:0b,$
             wViewCCHistOnOff:wViewCCHistOnOff,$
                fViewCCHist:0b,$
                pViewCCFilter:ptr_new(),$
             wViewFreqHisLogOnOff:wViewFreqHisLogOnOff,$
                fBiFreqHisLog:1b,$
             wViewSurfHistOnOff:wViewSurfHistOnOff,$
                fViewSurfHist:0b,$
                idViewSurfHist:'id',$
             wViewDataTableOnOff:wViewDataTableOnOff,$
                fViewDataTable:0b,$
                child_DataTable_tlb:-1l,$
             colocalizationRowNames:['Total Area',$
                                     'ROI_0',$
                                     'ROI_1',$
                                     'ROI Intersection',$
                                     'ROI Set Union',$
                                     'ROI_0 Exclusive',$
                                     'ROI_1 Exclusive',$
                                     'None'],$
             colocalizationColumnNames:['[pixel]',$
                                        '[% total]',$
                                        '[% ROI_0]',$
                                        '[% ROI_1]',$
                                        'Pearson Coeff. [r]',$
                                        'Overlap Coeff.',$
                                        'Overlap Coeff. [k1]',$
                                        'Overlap Coeff. [k2]',$
                                        'Coloc. Coeff. [m1]',$
                                        'Coloc. Coeff. [m2]',$
                                        'Coloc. Coeff. [M1]',$
                                        'Coloc. Coeff. [M2]'],$
             colocalizationParams:fltArr(12,8),$
             pColocRandomParams:ptr_new(),$
             fRandomAndRadiusShifts:0.,$
             rgbValues:rgbValues,$
             gamma:gamma,$
             opacGamma:opacGamma,$
             stretchBotTop:stretchBotTop,$
             opacStretchBotTop:opacStretchBotTop,$
             bottomValues:bottomValues,$
             topValues:topValues,$
             drawScale:drawScale,$              ; The draw widget scaling factor.
             wDraw:wDraw,$                  ; The draw widget identifier.
             thisView:thisView,$                ; The object view.
             thisView2:thisView2}                ; The object view 2.

    state.pColocRandomParams = ptr_new(fltArr(2, 12, 8), /no_copy)
    widget_control, wTopBase, set_uValue = state, /no_copy
    XManager, 's_Coloc_Window', wTopBase, Cleanup = 's_Coloc_cleanUp', /no_block, group_leader = groupLeader
    s_Coloc_upDateROIMask, wTopBase
end