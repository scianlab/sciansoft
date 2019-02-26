;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageFRET
;
; PURPOSE:
;       - ImageFRET-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ImageFRET' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageFRET::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


pro C_sImageFilter_ImageFRET::getMatrixValues, ch0_vect = ch0_vect, ch1_vect = ch1_vect, evX = evX, evY = evY, alpha = alpha, kernelActVect = kernelActVect, kernelPasVect = kernelPasVect
   dimM = size(*(*self.pParamStruct).pCorrelMatrixCh1, /dim)
   if (n_elements(dimM) lt 3) then return
   ch0_vect = (*(*self.pParamStruct).pCorrelMatrixCh1)[*, evX>0 < (dimM[1]-1), evY>0 < (dimM[2]-1)]
   ch1_vect = (*(*self.pParamStruct).pCorrelMatrixCh2)[*, evX>0 < (dimM[1]-1), evY>0 < (dimM[2]-1)]
   kernelActVect = *(*self.pParamStruct).pKernelActVect
   kernelPasVect = *(*self.pParamStruct).pKernelPasVect
   distVect = *(*self.pParamStruct).pDistVect
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Alpha for CCRI and JMSI'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then alpha = (*(*self.pParamStruct).pValues)[whParam] else alpha = 0
end


function C_sImageFilter_ImageFRET::apply, image = image,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

   dimI = size(image, /dim)
   chPos = abs(chPos - 1)

     ; get Image Object from Container
   oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
   if obj_valid(oImage) then begin
       clusterObj = oImage->getSegContainerObj(active = clusPos)
       if obj_valid(clusterObj) then segNum = (clusterObj->count() - 1) else segNum = -1
       for i = 0, segNum do begin
          segObj = clusterObj->get(position = i)
          if obj_valid(segObj) then if obj_isa(segObj, 'C_SIMAGEFILTER_IMAGEPSFSIGNALCREATOR') then begin
             pParamStruct = segObj->getpParamStruct()
             dummy = (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]]
             (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]] = 0b
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
          endif
       endfor
   endif else image_2 = image

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Pixel Radius'))[0]
   pixRad = round((*(*self.pParamStruct).pValues)[whParam]) > 1
   pixRadSq = pixRad^2
   dimRad = 2 * pixRad + 1
   (*(*self.pParamStruct).pValues)[whParam] = pixRad

   fApplyPSFKernel = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Apply PSF-Kernel-Filter to Average Signals'))[0]]
   fRandomValues0 = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Random Values for Channel 0'))[0]]
   fRandomValues1 = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Random Values for Channel 1'))[0]]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Alpha for CCRI and JMSI'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then alpha = (*(*self.pParamStruct).pValues)[whParam] else alpha = 0

   if fRandomValues0 then begin
      image = randomn(seed, dimI[0], dimI[1])
      maxI = max(image, min = minI)
      image = round((image-minI)/(maxI-minI)*255.)
   endif
   if fRandomValues1 then begin
      image_2 = randomn(seed, dimI[0], dimI[1])
      maxI = max(image_2, min = minI)
      image_2 = round((image_2-minI)/(maxI-minI)*255.)
   endif

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Open Colocalization Window'))[0]
   fOpenColocWin = (*(*self.pParamStruct).pActive)[whParam]
   if fOpenColocWin then begin
      if (n_elements(image_2) gt 0) then begin
         images = make_array(2, dimI[0], dimI[1], type = size(image, /type))
         images[0,*,*] = image_2
         images[1,*,*] = image
      endif else begin
         images = make_array(1, dimI[0], dimI[1], type = size(image, /type))
         images[0,*,*] = image
      endelse
      dimIs = size(images, /dim)
      if widget_info(long((*(*self.pParamStruct).pValues)[whParam]), /valid_id) then begin
         if ( ((*(*self.pParamStruct).pColocWinParam)[0] ne dimIs[0]) or ((*(*self.pParamStruct).pColocWinParam)[1] ne dimIs[1]) $
            or ((*(*self.pParamStruct).pColocWinParam)[2] ne dimIs[2]) ) then begin
            if widget_info(long((*(*self.pParamStruct).pValues)[whParam]), /valid_id) then widget_control, (*(*self.pParamStruct).pValues)[whParam], /destroy
            s_Coloc_Window, images, stack_tlb = stack_tlb, application_tlb = application_tlb
            (*(*self.pParamStruct).pValues)[whParam] = application_tlb
            *(*self.pParamStruct).pColocWinParam = [dimIs[0], dimIs[1], dimIs[2]]
         endif else s_Coloc_update, (*(*self.pParamStruct).pValues)[whParam], newImages = images
      endif else begin
         s_Coloc_Window, images, stack_tlb = stack_tlb, application_tlb = application_tlb
         (*(*self.pParamStruct).pValues)[whParam] = application_tlb
         *(*self.pParamStruct).pColocWinParam = [dimIs[0], dimIs[1], dimIs[2]]
      endelse
   endif else begin
      if widget_info(long((*(*self.pParamStruct).pValues)[whParam]), /valid_id) then widget_control, (*(*self.pParamStruct).pValues)[whParam], /destroy
      (*(*self.pParamStruct).pActive)[whParam] = 0
      (*(*self.pParamStruct).pValues)[whParam] = 0
   endelse

   if (n_elements(*(*self.pParamStruct).pMethodVector) ne 7) then begin
      ptr_free, (*self.pParamStruct).pMethodVector
      self.pMethodVector = ptr_new(bytarr(7), /no_copy)
   endif

   (*(*self.pParamStruct).pMethodVector)[0] = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Pearsons Correlation (PC)'))[0]]
   (*(*self.pParamStruct).pMethodVector)[1] = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Overlap Coefficient'))[0]]
   (*(*self.pParamStruct).pMethodVector)[2] = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'B of Y = A + B * X'))[0]]
   (*(*self.pParamStruct).pMethodVector)[3] = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Corr. Coeff. of Raw Images (CCRI)'))[0]]
   (*(*self.pParamStruct).pMethodVector)[4] = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Joint Moment of Standardized Images (JMSI)'))[0]]
   (*(*self.pParamStruct).pMethodVector)[5] = (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Cross-Correlation (G[dx])'))[0]]

   if (total(*(*self.pParamStruct).pMethodVector) eq 0) then begin
      (*(*self.pParamStruct).pMethodVector)[0] = 1b
      (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Pearsons Correlation (PC)'))[0] ] = 1b
   endif

   if fApplyPSFKernel then begin
      selectedStackObject->get, pParamStruct = pParamStruct
      xPixSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] / *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
      yPixSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] / *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]

      case 1 of
       (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Active PSF: 63Xw NA1.2 ex488 em520'))[0]]: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xw NA1.2 ex488 em520')
       (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Active PSF: 63Xw NA1.2 ex543 em580'))[0]]: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xw NA1.2 ex543 em580')
       (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Active PSF: 63Xoil NA1.4 ex488 em520'))[0]]: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xoil NA1.4 ex488 em520')
       (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Active PSF: 63Xoil NA1.4 ex543 em580'))[0]]: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xoil NA1.4 ex543 em580')
       else: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xw NA1.2 ex488 em520')
      endcase
      PSFKernel = s_getPSFKernel(PSF = PSF, xPixSize = xPixSize, yPixSize = yPixSize)
      PSFKernelAct = PSFKernel/max(PSFKernel)

      case 1 of
       (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Passive PSF: 63Xw NA1.2 ex488 em520'))[0]]: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xw NA1.2 ex488 em520')
       (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Passive PSF: 63Xw NA1.2 ex543 em580'))[0]]: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xw NA1.2 ex543 em580')
       (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Passive PSF: 63Xoil NA1.4 ex488 em520'))[0]]: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xoil NA1.4 ex488 em520')
       (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Passive PSF: 63Xoil NA1.4 ex543 em580'))[0]]: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xoil NA1.4 ex543 em580')
       else: PSF = s_getPSFData(sPSF = 'Use PSF for 63Xw NA1.2 ex488 em520')
      endcase
      PSFKernel = s_getPSFKernel(PSF = PSF, xPixSize = xPixSize, yPixSize = yPixSize)
      PSFKernelPas = PSFKernel/max(PSFKernel)

       ; adjust all PSFKernels to the same sizes
      dimAct = size(PSFKernelAct, /dim)
      dimPas = size(PSFKernelPas, /dim)
      if (dimRad gt min(dimPas)) then PSFKernelPas = s_Expand_Mirror(PSFKernelPas, [(dimRad-dimPas[0])/2.,(dimRad-dimPas[1])/2.], mirrorType = 3)
      if (dimRad gt min(dimAct)) then PSFKernelAct = s_Expand_Mirror(PSFKernelAct, [(dimRad-dimAct[0])/2.,(dimRad-dimAct[1])/2.], mirrorType = 3)
      dimAct = size(PSFKernelAct, /dim)
      dimPas = size(PSFKernelPas, /dim)
      if ((dimAct[0] gt dimPas[0]) or (dimAct[1] gt dimPas[1])) then PSFKernelPas = s_Expand_Mirror(PSFKernelPas, [(dimAct[0]-dimPas[0])/2.,(dimAct[1]-dimPas[1])/2.], mirrorType = 3)
      if ((dimPas[0] gt dimAct[0]) or (dimPas[1] gt dimAct[1])) then PSFKernelAct = s_Expand_Mirror(PSFKernelAct, [(dimPas[0]-dimAct[0])/2.,(dimPas[1]-dimAct[1])/2.], mirrorType = 3)
   endif else begin
      PSFKernelAct = make_array(dimRad, dimRad, /float, value = 1.)
      PSFKernelPas = make_array(dimRad, dimRad, /float, value = 1.)
   endelse
   dimKernel = size(PSFKernelAct, /dim)
   centreKernel = floor(dimKernel/2.)

    ; define Correlation Matrices
   dimCM = 0
   for k = -pixRad,pixRad do for l = -pixRad,pixRad do if ((1.*(k*k+l*l)) le pixRadSq) then dimCM = dimCM + 1
   if not(ptr_valid((*self.pParamStruct).pCorrelMatrixCh1)) then (*self.pParamStruct).pCorrelMatrixCh1 = ptr_new(make_array(dimCM, dimI[0], dimI[1], /float), /no_copy) $
      else *(*self.pParamStruct).pCorrelMatrixCh1 = make_array(dimCM, dimI[0], dimI[1], /float)
   if not(ptr_valid((*self.pParamStruct).pCorrelMatrixCh2)) then (*self.pParamStruct).pCorrelMatrixCh2 = ptr_new(make_array(dimCM, dimI[0], dimI[1], /float), /no_copy) $
      else *(*self.pParamStruct).pCorrelMatrixCh2 = make_array(dimCM, dimI[0], dimI[1], /float)
   if not(ptr_valid((*self.pParamStruct).pKernelActVect)) then (*self.pParamStruct).pKernelActVect = ptr_new(make_array(dimCM, /float), /no_copy) $
      else *(*self.pParamStruct).pKernelActVect = make_array(dimCM, /float)
   if not(ptr_valid((*self.pParamStruct).pKernelPasVect)) then (*self.pParamStruct).pKernelPasVect = ptr_new(make_array(dimCM, /float), /no_copy) $
      else *(*self.pParamStruct).pKernelPasVect = make_array(dimCM, /float)
   if not(ptr_valid((*self.pParamStruct).pDistVect)) then (*self.pParamStruct).pDistVect = ptr_new(make_array(dimCM, /float), /no_copy) $
      else *(*self.pParamStruct).pDistVect = make_array(dimCM, /float)

    ; fill Correlation Matrices
   expImage_1 = s_Expand_Mirror(image, pixRad)
   expImage_2 = s_Expand_Mirror(image_2, pixRad)
   PSFKernelAct = PSFKernelAct/total(PSFKernelAct)
   PSFKernelPas = PSFKernelPas/total(PSFKernelPas)
   count = 0
   for k = -pixRad, pixRad do for l = -pixRad, pixRad do begin
      radDist = sqrt(k*k+l*l)
      if (radDist le pixRad) then begin
         (*(*self.pParamStruct).pDistVect)[count] = radDist
         (*(*self.pParamStruct).pKernelActVect)[count] = PSFKernelAct[centreKernel[0]+k, centreKernel[1]+l]
         (*(*self.pParamStruct).pKernelPasVect)[count] = PSFKernelPas[centreKernel[0]+k, centreKernel[1]+l]
         (*(*self.pParamStruct).pCorrelMatrixCh1)[count,*,*] = expImage_1[pixRad+k : pixRad+k+dimI[0]-1, pixRad+l : pixRad+l+dimI[1]-1]
         (*(*self.pParamStruct).pCorrelMatrixCh2)[count,*,*] = expImage_2[pixRad+k : pixRad+k+dimI[0]-1, pixRad+l : pixRad+l+dimI[1]-1]
         count = count + 1
      endif
   endfor
   *(*self.pParamStruct).pKernelActVect = *(*self.pParamStruct).pKernelActVect / total(*(*self.pParamStruct).pKernelActVect)
   *(*self.pParamStruct).pKernelPasVect = *(*self.pParamStruct).pKernelPasVect / total(*(*self.pParamStruct).pKernelPasVect)

   if (*(*self.pParamStruct).pMethodVector)[1] then begin
      *(*self.pParamStruct).pCorrelMatrixCh1 = long(*(*self.pParamStruct).pCorrelMatrixCh1)
      *(*self.pParamStruct).pCorrelMatrixCh2 = long(*(*self.pParamStruct).pCorrelMatrixCh2)
   endif
   if (total((*(*self.pParamStruct).pMethodVector)[0:2]) gt 0) then begin
      multAct = *(*self.pParamStruct).pKernelActVect / max(*(*self.pParamStruct).pKernelActVect)
      multPas = *(*self.pParamStruct).pKernelPasVect / max(*(*self.pParamStruct).pKernelPasVect)
   endif

   iCount = 0
   outI = make_array(total(*(*self.pParamStruct).pMethodVector), dimI[0], dimI[1], /float)
   dimOut = size(outI, /dim)

      ; Pearsons Correlation (PC)
   if (*(*self.pParamStruct).pMethodVector)[0] then begin
      for k = 0, dimI[0]-1 do for l = 0, dimI[1]-1 do $
         outI[iCount,k,l] = correlate( (*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l] * multAct,(*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l] * multPas, /double)
      iCount = iCount + 1
   endif

      ; Overlap Coefficient
   if (*(*self.pParamStruct).pMethodVector)[1] then begin
      for k = 0, dimI[0]-1 do for l = 0, dimI[1]-1 do begin
          actVals = (*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l] * multAct
          pasVals = (*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l] * multPas
          outI[iCount, k,l] = total(1. * actVals * pasVals) / ( sqrt( total(actVals^2) * total(pasVals^2) ) > 1.)
      endfor
      iCount = iCount + 1
   endif

      ; B of Y = A + B * X
   if (*(*self.pParamStruct).pMethodVector)[2] then begin
      for k = 0, dimI[0]-1 do for l = 0, dimI[1]-1 do $
         outI[iCount,k,l] = (linFit((*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l] * multAct,(*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l] * multPas, /double))[1]
      iCount = iCount + 1
   endif

      ; Correlation Coefficient of Raw Images (CCRI)
   if (*(*self.pParamStruct).pMethodVector)[3] then begin
      kernelMeanVect = ( (*(*self.pParamStruct).pKernelActVect) + (*(*self.pParamStruct).pKernelPasVect) ) / 2.
      for k = 0, dimI[0]-1 do for l = 0, dimI[1]-1 do begin
          ; varX = mean(x²) - mean(x)² + alpha
         varX = ( total(((*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l])^2 * (*(*self.pParamStruct).pKernelActVect) ) ) - $
                ( total((*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l] * (*(*self.pParamStruct).pKernelActVect)) )^2 + alpha
         varY = ( total(((*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l])^2 * (*(*self.pParamStruct).pKernelPasVect) ) ) - $
                ( total((*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l] * (*(*self.pParamStruct).pKernelPasVect)) )^2  +  alpha
         outI[iCount,k,l] = ( ( total(((*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l] * (*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l]) * kernelMeanVect ) )  -  $
                              ( total((*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l] * (*(*self.pParamStruct).pKernelActVect)) ) * $
                              ( total((*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l] * (*(*self.pParamStruct).pKernelPasVect)) ) ) / $
                              ( (sqrt(varX) * sqrt(varY)) > (1./dimCM) )
      endfor
      iCount = iCount + 1
   endif

      ; Joint Moment of Standardized Images (JMSI)
   if (*(*self.pParamStruct).pMethodVector)[4] then begin
      kernelMid = floor(dimCM/2.)
      for k = 0, dimI[0]-1 do for l = 0, dimI[1]-1 do begin
         ; varX = mean(x²) - mean(x)² + alpha
        varX = ( total(((*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l])^2 * (*(*self.pParamStruct).pKernelActVect) ) )  -  $
               ( total((*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l] * (*(*self.pParamStruct).pKernelActVect)) )^2  +  alpha
        varY = ( total(((*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l])^2 * (*(*self.pParamStruct).pKernelPasVect) ) )  -  $
               ( total((*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l] * (*(*self.pParamStruct).pKernelPasVect)) )^2  +  alpha
        xs = ( (*(*self.pParamStruct).pCorrelMatrixCh1)[kernelMid,k,l] - (total((*(*self.pParamStruct).pCorrelMatrixCh1)[*,k,l] * (*(*self.pParamStruct).pKernelActVect)) ) ) / (sqrt(varX) > (1./dimCM))
        ys = ( (*(*self.pParamStruct).pCorrelMatrixCh2)[kernelMid,k,l] - (total((*(*self.pParamStruct).pCorrelMatrixCh2)[*,k,l] * (*(*self.pParamStruct).pKernelPasVect)) ) ) / (sqrt(varY) > (1./dimCM))
        outI[iCount, k,l] = xs * ys
      endfor

      expImage_2 = s_Expand_Mirror(fltArr(dimI[0],dimI[1]) + outI[iCount,*,*], pixRad)
      outI[iCount,*,*] = 0.
      PSFKernelMean = PSFKernelAct + PSFKernelPas / 2.
      for k = -pixRad, pixRad do for l = -pixRad, pixRad do if ((1.*(k*k+l*l)) le (pixRad)^2) then $
         outI[iCount,*,*] = outI[iCount,*,*] + expImage_2[ pixRad+k : pixRad+k+dimI[0]-1, pixRad+l : pixRad+l+dimI[1]-1] * PSFKernelMean[centreKernel[0]+k, centreKernel[1]+l]
      expImage_2 = 0.
      iCount = iCount + 1
   endif

      ; Cross-Correlation (G[dx])
   if (*(*self.pParamStruct).pMethodVector)[5] then begin
      kernelMid = floor(dimCM/2.)
      meanCh1 = (total(*(*self.pParamStruct).pCorrelMatrixCh1, 1, /double) / dimCM) > 1e-6
      meanCh2 = (total(*(*self.pParamStruct).pCorrelMatrixCh2, 1, /double) / dimCM) > 1e-6

        ; make deviation matrices
      for i = 0, dimCM-1 do begin
         (*(*self.pParamStruct).pCorrelMatrixCh1)[i,*,*] = (*(*self.pParamStruct).pCorrelMatrixCh1)[i,*,*] - meanCh1
         (*(*self.pParamStruct).pCorrelMatrixCh2)[i,*,*] = (*(*self.pParamStruct).pCorrelMatrixCh2)[i,*,*] - meanCh2
      endFor

      gMatrix = make_array(dimCM, dimI[0], dimI[1], /float)
      gMatrix[kernelMid,*,*] = (total( *(*self.pParamStruct).pCorrelMatrixCh1 * $
                               *(*self.pParamStruct).pCorrelMatrixCh1, 1, /double ) / dimCM) $
                               / (meanCh1 * meanCh1)
      for i = 0, dimCM-2 do $
         gMatrix[(kernelMid+i+1) mod dimCM,*,*] = (total( ( *(*self.pParamStruct).pCorrelMatrixCh1 ) * $
                         [ (*(*self.pParamStruct).pCorrelMatrixCh1)[ dimCM-i-1 : dimCM-1, *,*], (*(*self.pParamStruct).pCorrelMatrixCh1)[0 : dimCM-i-2, *,*]], 1, /double) / dimCM) $
                         / (meanCh1 * meanCh1)

      xxVect = [reverse(*(*self.pParamStruct).pDistVect), -(*(*self.pParamStruct).pDistVect)[1:*]]
      for k = 0, dimI[0]-1 do for l = 0, dimI[1]-1 do begin
        GVect = [reverse(gMatrix[*,k,l]), gMatrix[1:*,k,l]]
        maxG = max(GVect)
        maxAt = 0.
        sdG = .12
        PSFGauss = gaussFit( xxVect, GVect, fitValues, estimates = [maxG, 0., sdG], nterms = 3, chisq = chisq, sigma = sigma)
        if (fitValues[0] gt 0) then outI[k,l] = 1/fitValues[0] else outI[k,l] = 0
        xVect = make_array(pixRad*10, /float, /index) / 10.
        PSFGauss = fitValues[0]* exp(-0.5* (xVect-fitValues[1])^2/fitValues[2])

        live_info, error = error, properties = prop, window_in = 'Auto-Correlation Selected Channel 1'
        if (error ne '') then begin
           style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type : 3, symbol_size : 0.01},$
                      visualization_properties = {color: 'Light Red'},$
                      xAxis_properties = {axisTitle:'[dx]', exact:1, compute_range:0},$
                      yAxis_properties = {axisTitle:'G0 [dx]', exact:1, compute_range:0},$
                      legend_properties = {hide :1})
           live_plot, GVect, independent = xxVect, /scatter, draw_dimension = [300,300], /no_select,style = style, title = 'Auto-Correlation Selected Channel 1'
           live_oPlot, PSFGauss, independent = xVect, /no_select,subType = 'LinePlot',window_in = 'Auto-Correlation Selected Channel 1'
        endif else begin
           live_control, GVect, /update, window_in = 'Auto-Correlation Selected Channel 1'
           live_control, PSFGauss, /update, window_in = 'Auto-Correlation Selected Channel 1'
        endelse
        live_info, 'XXVECT Axis', properties = variable, window_in = 'Auto-Correlation Selected Channel 1'
        variable.minRange = -.05
        maxVect = max(xxVect, min = minVect)
        variable.maxRange = maxVect + .05 * (maxVect-minVect)
        variable.axistitle = string(sigma[1])
        live_control, 'XXVECT Axis', properties = variable, window_in = 'Auto-Correlation Selected Channel 1'
        live_info, 'Y Axis', properties = variable, window_in = 'Auto-Correlation Selected Channel 1'
        maxVect = max(GVect, min = minVect)
        variable.minRange = minVect - .05 * (maxVect-minVect)
        variable.maxRange = maxVect + .05 * (maxVect-minVect)
        variable.axistitle = string(chisq)
        live_control, 'Y Axis', properties = variable, window_in = 'Auto-Correlation Selected Channel 1'
      endfor
   endif

   whereFinite = where(finite(outI) ne 1)
   if (whereFinite[0] ne -1) then outI[whereFinite] = 0

   whereROW = (where((*(*self.pParamStruct).pNames) eq 'Open Rank Order Window'))[0]
   fRankOrder = (*(*self.pParamStruct).pActive)[whereROW]
   wID = long((*(*self.pParamStruct).pValues)[whereROW])

   if fRankOrder then begin
      images = make_array(dimOut[0]+2, dimOut[1], dimOut[2], /float)
      images[0,*,*] = image
      images[1,*,*] = image_2
      for i = 0, dimOut[0]-1 do images[i+2,*,*] = outI[i,*,*]

      whereI = where((*(*self.pParamStruct).pMethodVector))
      if (whereI[0] ne -1) then iNames = ['Channel 0', 'Channel 1', (s_Image_SRO_getCorrelationNames())[whereI]] else iNames = ['Channel 0', 'Channel 1']

      if not(widget_info(wID, /valid_id)) then begin
         if 1 then begin
            if widget_info(wID, /valid_id) then widget_control, wID, /destroy
            s_Image_ShowRankOrder_Window, images, stack_tlb = stack_tlb, application_tlb = application_tlb, pSclArr = pSclArr, iNames = iNames
            (*(*self.pParamStruct).pValues)[whereROW] = application_tlb
         endif else s_ROW_update, wID, newImages = images
      endif else begin
         s_Image_SRO_update, (*(*self.pParamStruct).pValues)[whereROW], images = images
;         (*(*self.pParamStruct).pValues)[whParam] = application_tlb
;         *(*self.pParamStruct).pColocWinParam = [dimIs[0], dimIs[1], dimIs[2]]
      endelse

   endif else begin
      if widget_info(wID, /valid_id) then widget_control, wID, /destroy
      (*(*self.pParamStruct).pActive)[whParam] = 0
      (*(*self.pParamStruct).pValues)[whParam] = 0
   endelse

   return, fltArr(dimI[0],dimI[1]) + outI[0,*,*]
end


function C_sImageFilter_ImageFRET::init
   filterStruct = {Name: 'C_ImageFRET',$
                          pWidgetType: ptr_new(),$
                          pNames: ptr_new(),$   ; Pointer on filter Parameter Names.
                          pActive: ptr_new(),$  ; Pointer on filter Parameter Active Bool.
                          pMin: ptr_new(),$     ; Pointer on filter Parameter Min_Values.
                          pMax: ptr_new(),$     ; Pointer on filter Parameter Max_Values.
                          pValues: ptr_new(),$  ; Pointer on filter Parameter Values.
                          pMethodVector: ptr_new(),$    ; Pointer on Vector with MethodActiveFlags
                          pColocWinParam: ptr_new(),$   ; Pointer on Parameter Vector of Colocalization Window.
                          pKernelActVect: ptr_new(),$   ; Pointer on PSF Kernel Vector
                          pKernelPasVect: ptr_new(),$   ; Pointer on PSF Kernel Vector
                          pDistVect: ptr_new(),$        ; Pointer on Distance Vector
                          pCorrelMatrixCh1: ptr_new(),$ ; Pointer on CorrelMatrix_1.
                          pCorrelMatrixCh2: ptr_new()}  ; Pointer on CorrelMatrix_2.

    filterParamWidgetType = make_array(23, /string, value = 'widget_slider')
    filterParamNames = ['Pixel Radius',$
                        'Pearsons Correlation (PC)',$
                        'Cross-Correlation (G[dx])',$
                        'Overlap Coefficient',$
                        'B of Y = A + B * X',$
                        'Corr. Coeff. of Raw Images (CCRI)',$
                        'Joint Moment of Standardized Images (JMSI)',$
                        'Alpha for CCRI and JMSI',$
                        'Apply PSF-Kernel-Filter to Average Signals',$
                        'Active PSF: 63Xw NA1.2 ex488 em520',$
                        'Active PSF: 63Xw NA1.2 ex543 em580',$
                        'Active PSF: 63Xoil NA1.4 ex488 em520',$
                        'Active PSF: 63Xoil NA1.4 ex543 em580',$
                        'Passive PSF: 63Xw NA1.2 ex488 em520',$
                        'Passive PSF: 63Xw NA1.2 ex543 em580',$
                        'Passive PSF: 63Xoil NA1.4 ex488 em520',$
                        'Passive PSF: 63Xoil NA1.4 ex543 em580',$
                        '--',$
                        '-!-',$
                        'Random Values for Channel 0',$
                        'Random Values for Channel 1',$
                        'Open Rank Order Window',$
                        'Open Colocalization Window']

    filterParamActive = [1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0]
    filterParamMin = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1]
    filterParamMax = [50, 1, 1, 1, 1, 1, 1, 500, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    filterParamValues = [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1]

    a = [-1]
    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
    filterStruct.pMethodVector = ptr_new([1,0,0,0,0,0,0], /no_copy)
    filterStruct.pColocWinParam = ptr_new(a)
    filterStruct.pKernelActVect = ptr_new(a)
    filterStruct.pKernelPasVect = ptr_new(a)
    filterStruct.pDistVect = ptr_new(a)
    filterStruct.pCorrelMatrixCh1 = ptr_new(a)
    filterStruct.pCorrelMatrixCh2 = ptr_new(a, /no_copy)
    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageFRET__define
  tmp = {C_sImageFilter_ImageFRET, pParamStruct: ptr_new(), inherits C_sImageFilter}
end