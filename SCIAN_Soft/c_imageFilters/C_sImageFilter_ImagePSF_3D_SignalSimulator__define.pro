;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImagePSF_3D_SignalSimulator_define.pro  (c:\rsi\Steffen\s_TimeCalc\C_sImageFilter\C_sImageFilter_ImagePSF_3D_SignalSimulator_define.pro)
;
; PURPOSE:
;       - ImagePSFSignalSimulator-Filter-Class.
;
; AUTHOR:
;     FASL (2011)
;     e_mail: fsantibanez@med.uchile.cl
;     based on C_sImageFilter_ImagePSF_SignalSimulator
; CALLING SEQUENCE:
;     result = obj_new('C_sImageFilter_ImagePSF_3D_SignalSimulator' )
; METHODS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImagePSF_3D_SignalSimulator::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_ImagePSF_3D_SignalSimulator::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
end


pro C_sImageFilter_ImagePSF_3D_SignalSimulator::getValues, numberOfCoSignals = numberOfCoSignals, numberOfSignals = numberOfSignals, PSFKernel = PSFKernel,$
                                                       xPos = xPos, yPos = yPos, dimI = dimI, wNotSet = wNotSet, signalIntensity = signalIntensity

   PSFKernel = *(*self.pParamStruct).pPSFKernel
   if (n_elements(numberOfCoSignals) eq 0) then dimV = ((size(*(*self.pParamStruct).pxPos, /dim))[0]-1) $
      else dimV = ((size(*(*self.pParamStruct).pxPos, /dim))[0]-1) < (numberOfCoSignals-1)
   numberOfSignals = round((*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Signal Number [#]'))[0]]) > 0
   if (dimV lt 0) then return
   xPos = (*(*self.pParamStruct).pxPos)[0:dimV]
   yPos = (*(*self.pParamStruct).pyPos)[0:dimV]
   notSet = make_array(dimI[0], dimI[1], /byte)
   notSet[(*(*self.pParamStruct).pxPos), (*(*self.pParamStruct).pyPos)] = 1b
   wNotSet = where(notSet eq 0)
   if (n_elements((*(*self.pParamStruct).pSignalIntensity)) gt 1) then signalIntensity = (*(*self.pParamStruct).pSignalIntensity)[0:dimV] $
      else signalIntensity = make_array(dimV+1, value = *(*self.pParamStruct).pSignalIntensity)
end


function C_sImageFilter_ImagePSF_3D_SignalSimulator::apply, image = image,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y,$
                                     fKeepFilterMatrix = fKeepFilterMatrix

    file = dialog_pickfile( /read, get_path = path, filter = '*.tif')
    if (file[0] ne '') then begin
       dummy = query_tiff(file[0], tiffInfo)
       if (dummy ne 0) then begin
          kernelPSF = read_tiff(file[0])
          return, (convol(float(image), kernelPSF, /edge_wrap) < 255.)
       endif
    endif

    return, image
   
   dimI = size(image, /dim)
   image = float(image)
   selectedStackObject->get, pParamStruct = pParamStruct
   xPixSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [real]'))[0]] / $
              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
   yPixSize = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [real]'))[0]] / $
              *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]

   fPSFKernel = 1b
   case 1 of
     (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Use PSF for 63Xw NA1.2 ex488 em520'))[0]]: begin
        PSF = s_getPSFData(sPSF = 'Use PSF for 63Xw NA1.2 ex488 em520')
        PSFKernel = s_getPSFKernel(PSF = PSF, xPixSize = xPixSize, yPixSize = yPixSize)
     endcase
     (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Use PSF for 63Xw NA1.2 ex543 em580'))[0]]: begin
        PSF = s_getPSFData(sPSF = 'Use PSF for 63Xw NA1.2 ex543 em580')
        PSFKernel = s_getPSFKernel(PSF = PSF, xPixSize = xPixSize, yPixSize = yPixSize)
     endcase
     (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Use PSF for 63Xoil NA1.4 ex488 em520'))[0]]: begin
        PSF = s_getPSFData(sPSF = 'Use PSF for 63Xoil NA1.4 ex488 em520')
        PSFKernel = s_getPSFKernel(PSF = PSF, xPixSize = xPixSize, yPixSize = yPixSize)
     endcase
     (*(*self.pParamStruct).pActive)[(where((*(*self.pParamStruct).pNames) eq 'Use PSF for 63Xoil NA1.4 ex543 em580'))[0]]: begin
        PSF = s_getPSFData(sPSF = 'Use PSF for 63Xoil NA1.4 ex543 em580')
        PSFKernel = s_getPSFKernel(PSF = PSF, xPixSize = xPixSize, yPixSize = yPixSize)
     endcase
     else: begin
        PSFKernel = make_array(3,3, /float)
        PSFKernel[1,1] = 1
        fPSFKernel = 0b
     endcase
   endcase

   PSFKernel = PSFKernel / max(PSFKernel)

    file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]], get_path = path, filter = '*.tif', /multiple_files)
    if (file[0] ne '') then begin
       dummy = query_tiff(file[0], tiffInfo)
       if (dummy ne 0) then begin
          kernel2D = read_tiff(file[i])
          szKernel2D = size(image, /dim)
       endif
    endif

      ; return Convoluted Image
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Apply PSF to Image'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then return, round(convol(float(image), PSFKernel, /edge_wrap))

      ; set flag and radius [pixel] for Dendrite
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Add Dendrite rad[pixel]'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then dendRad = (*(*self.pParamStruct).pValues)[whParam] < (round(dimI[0]/2.)-1) else dendRad = 0b

      ; set Dendrite Intensity
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Dendrite Intensity'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then dendInt = (*(*self.pParamStruct).pValues)[whParam] else dendInt = 0b

      ; set flag for addition conditions
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Add PSF-Signals to Image'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then fAddToImage = 1b else fAddToImage = 0b

      ; define Signal Parameters & Distribution
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Keep Signal Intensity from Co-Channel'))[0]
   fKeepSignalIntFromCoChannel = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Mean Signal Intensity'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then signalInt = round((*(*self.pParamStruct).pValues)[whParam]) else signalInt = 126.

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Mean Signal Intensity +-'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then $
      spreadSignal = round((*(*self.pParamStruct).pValues)[whParam]) else spreadSignal = 0.

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Randomize OnOff'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then fRandomize = 1b else fRandomize = 0b

   whParamColocWin = (where((*(*self.pParamStruct).pNames) eq 'Open Colocalization Window'))[0]
   fOpenColocWin = (*(*self.pParamStruct).pActive)[whParamColocWin]

   *(*self.pParamStruct).pxPos = [-1]
   *(*self.pParamStruct).pyPos = [-1]
   *(*self.pParamStruct).pSignalIntensity = [-1.]
   if (chPos eq 0) then chPos2 = 1 else chPos2 = 0

   whParamCo = (where((*(*self.pParamStruct).pNames) eq 'Add PSF-Signals from Co-Channel'))[0]
   if ((*(*self.pParamStruct).pActive)[whParamCo]) then numberOfCoSignals = round(((*(*self.pParamStruct).pValues)[whParamCo] > 0)) else numberOfCoSignals = 0

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Signal Number [#]'))[0]
   whParamP = (where((*(*self.pParamStruct).pNames) eq 'Signal Density [%]'))[0]
   if (*(*self.pParamStruct).pActive)[whParam] then begin
      signalNumber = round((*(*self.pParamStruct).pValues)[whParam]) > 0
      signalDenstityPercent = 100. * signalNumber / (dimI[0]*dimI[1])
   endif else begin
      if ((*(*self.pParamStruct).pActive)[whParamP]) then begin
         signalDenstityPercent = (*(*self.pParamStruct).pValues)[whParamP] > 0
         signalNumber = round(dimI[0]*dimI[1]*signalDenstityPercent/100.)
      endif
   endelse
   if (signalNumber lt numberOfCoSignals) then numberOfCoSignals = signalNumber
   if (signalNumber eq 0) then return, image

      ; get PSF-signals from Co-channel
   if ((numberOfCoSignals gt 0) or fOpenColocWin) then begin
      oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos2, zPos = zPos)
      if obj_valid(oImage) then begin
         image_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                  tPos = tPos,$
                                                  chPos = chPos2,$
                                                  zPos = zPos,$
                                                  clusPos = clusPos,$
                                                  cut_x = cut_x, cut_y = cut_y)
         clusterObj = oImage->getSegContainerObj(active = clusPos)
         if obj_valid(clusterObj) then segNum = clusterObj->count()-1 else segNum = -1
         for i = 0, segNum do begin
            segObj = clusterObj->get(position = i)
            if obj_valid(segObj) then if obj_isa(segObj, 'C_sImageFilter_ImagePSF_3D_SignalSimulator') then $
               segObj->getValues, numberOfCoSignals = numberOfCoSignals, numberOfSignals = numberOfSignals, PSFKernel = PSFKernel_2,$
                                  xPos = *(*self.pParamStruct).pxPos, yPos = *(*self.pParamStruct).pyPos, dimI = dimI,$
                                  wNotSet = wNotSet, signalIntensity = *(*self.pParamStruct).pSignalIntensity
               segObj->getValues, xPos = xPosCo, yPos = yPosCo, dimI = dimI
         endfor
         if not(fKeepSignalIntFromCoChannel) then (*(*self.pParamStruct).pSignalIntensity)[*] = float(signalInt)
      endif
   endif

      ; check number of PSF-signals from Co-channel
   if ((*(*self.pParamStruct).pxPos)[0] eq -1) then numberOfCoSignals = 0
   numberOfCoSignals = numberOfCoSignals < n_elements(*(*self.pParamStruct).pxPos)

   countCoSignals = numberOfCoSignals
   while(countCoSignals ge 0) do begin
      print, 'countCoSignals', countCoSignals
      numberOfCoSignals = countCoSignals

      addNewRandomSignals = signalNumber - numberOfCoSignals
      (*(*self.pParamStruct).pValues)[whParam] = signalNumber
      (*(*self.pParamStruct).pValues)[whParamCo] = numberOfCoSignals
      (*(*self.pParamStruct).pValues)[whParamP] = signalDenstityPercent

      if fRandomize then begin
         *(*self.pParamStruct).pMemTime = long(*(*self.pParamStruct).pSysTime - sysTime(1) + memory(/current))
         randomVar = *(*self.pParamStruct).pMemTime
      endif else randomVar = *(*self.pParamStruct).pMemTime

;   move_zArr = make_array(128, /int)
;   openR, 2, 'c:/rsi/move_z.dat'
;   readf, 2, move_zArr
;   close, 2
;   move_z = min(move_zArr, whMin)
;   move_zArr[whMin] = 128
;   openW, 2, 'c:/rsi/move_z.dat'
;   printF, 2, move_zArr
;   close, 2


      if (addNewRandomSignals eq signalNumber) then begin
         if (dendRad gt 0) then *(*self.pParamStruct).pxPos = round(randomn(randomVar+chPos, addNewRandomSignals, /uni) * (2*dendRad-1)) + (round(dimI[0]/2.)-dendRad) $
            else *(*self.pParamStruct).pxPos = round(randomn(randomVar+chPos, addNewRandomSignals, /uni) * dimI[0])
         *(*self.pParamStruct).pyPos = round(randomn(randomVar+chPos2, addNewRandomSignals, /uni) * dimI[1])
         if (spreadSignal eq 0) then *(*self.pParamStruct).pSignalIntensity = float(signalInt) $
            else *(*self.pParamStruct).pSignalIntensity = 255. < (*(*self.pParamStruct).pSignalIntensity - spreadSignal + randomn(randomVar, addNewRandomSignals, /uni) * 2. * spreadSignal)
            
            
;      (*(*self.pParamStruct).pxPos)[[1]] = (*(*self.pParamStruct).pxPos)[[0]]
;      (*(*self.pParamStruct).pyPos)[[0]] = ((*(*self.pParamStruct).pyPos)[[0]] + move_z) mod 128
;      (*(*self.pParamStruct).pyPos)[[4]] = abs((*(*self.pParamStruct).pyPos)[[4]] - move_z)
      
      
      endif else begin
         if (addNewRandomSignals gt 0) then begin

            *(*self.pParamStruct).pxPos = (*(*self.pParamStruct).pxPos)[0:numberOfCoSignals-1]
            *(*self.pParamStruct).pyPos = (*(*self.pParamStruct).pyPos)[0:numberOfCoSignals-1]

            if (dendRad eq 0) then begin
               nNotSet = n_elements(wNotSet)
               case 1 of
                  (wNotSet[0] eq -1): wSet = round(randomn(randomVar+chPos, addNewRandomSignals, /uni) * (dimI[0]*dimI[1]))
                  (nNotSet ge addNewRandomSignals): wSet = wNotSet[round(randomn(randomVar+chPos, addNewRandomSignals, /uni) * nNotSet)]
                  else: wSet = [wNotSet, round(randomn(randomVar+chPos, (addNewRandomSignals - nNotSet), /uni) * (dimI[0]*dimI[1]))]
               endcase
               *(*self.pParamStruct).pxPos = [*(*self.pParamStruct).pxPos, wSet mod dimI[0]]
               *(*self.pParamStruct).pyPos = [*(*self.pParamStruct).pyPos, floor(wSet/(1.* dimI[0]))]
            endif else begin
               nNotSet = n_elements(wNotSet)
               case 1 of
                  (wNotSet[0] eq -1): wSet = round(randomn(randomVar+chPos, addNewRandomSignals, /uni) * ((2*dendRad-1)*dimI[1]))
                  (nNotSet ge addNewRandomSignals): wSet = wNotSet[round(randomn(randomVar+chPos, addNewRandomSignals, /uni) * nNotSet)]
                  else: wSet = [wNotSet, round(randomn(randomVar+chPos, (addNewRandomSignals - nNotSet), /uni) * ((2*dendRad-1)*dimI[1]))]
               endcase
               *(*self.pParamStruct).pxPos = [*(*self.pParamStruct).pxPos, (wSet mod (2*dendRad-1)) + (round(dimI[0]/2.)-dendRad)]
               *(*self.pParamStruct).pyPos = [*(*self.pParamStruct).pyPos, floor(wSet/(1.* dimI[0]))]
            endelse

            if (spreadSignal eq 0) then *(*self.pParamStruct).pSignalIntensity = [*(*self.pParamStruct).pSignalIntensity, make_array(addNewRandomSignals, value = signalInt)] $
               else *(*self.pParamStruct).pSignalIntensity = [*(*self.pParamStruct).pSignalIntensity, 255 < (*(*self.pParamStruct).pSignalIntensity - spreadSignal + randomn(randomVar + spreadSignal, signalNumber, /uni) * 2 * spreadSignal)]
         endif
      endelse

      print, '_______________________________________'
      print, 'PSF-Signal Statistics:'
      print, 'Total Number of PSF-signals added: ', signalNumber
      print, 'PSF-signals added from Co-cannel: ', numberOfCoSignals
      print, 'Random PSF-signals added: ', addNewRandomSignals
      print, 'PSF-signals in % of total pixels: ', signalDenstityPercent
      print, '_______________________________________'

      if (fAddToImage eq 0) then image[*] = 0

   ;Set dendrite with diameter 2*dendRad+1
      if (dendRad gt 0) then image[round(dimI[0]/2.)-dendRad : round(dimI[0]/2.)+dendRad, *] = dendInt

      image[*(*self.pParamStruct).pxPos, *(*self.pParamStruct).pyPos] += *(*self.pParamStruct).pSignalIntensity
      if ( (dimI[0] le (size(PSFKernel,/dim))[0]) or (dimI[1] le (size(PSFKernel,/dim))[1]) ) then return, image

      if fOpenColocWin then begin
         if ((n_elements(numberOfSignals) gt 0) and (countCoSignals ne -1)) then begin

            backImage_0 = image_2
            backImage_1 = byte(round(convol(float(image), PSFKernel, /edge_wrap)) < 255.)

            oldI_2 = image_2
            oldI = image
            oldDim = dimI

            image_2 = backImage_0
            image = backImage_1
            dimI = size(image, /dim)

            tv = fltarr(dimI[0]*3,dimI[1])
            tv[0:dimI[0]-1,*] = image_2
            tv[dimI[0]:2*dimI[0]-1,*] = image
            tv[2*dimI[0]:3*dimI[0]-1,*] = .5 * image_2 + .5 * image

            window, 10, xsize = dimI[0]*3, ysize = dimI[1]
            tvscl, tv

            path = strCompress('c:\rsi\CC\', /rem)
            ch0N = strCompress(string(numberOfSignals), /rem)
            while (strLen(ch0N) lt 3) do ch0N = strCompress('0' + ch0N, /rem)
            ch0Num = strCompress('ch0n'+ string(ch0N) + '_', /rem)

            ch1N = strCompress(string(signalNumber), /rem)
            while (strLen(ch1N) lt 3) do ch1N = strCompress('0' + ch1N, /rem)
            ch1Num = strCompress('ch1n'+ string(ch1N) + '_', /rem)

            chC = strCompress(string(countCoSignals), /rem)
            while (strLen(chC) lt 3) do chC = strCompress('0' + chC, /rem)
            chCo = strCompress('co'+ string(chC) + '_', /rem)

            write_tiff, path + ch0Num + ch1Num + chCo + 'ch0.tif', image_2, 1
            write_tiff, path + ch0Num + ch1Num + chCo + 'ch1.tif', image, 1
            print, path + ch0Num + ch1Num + chCo + 'ch0.tif'
            print, path + ch0Num + ch1Num + chCo + 'ch1.tif'

            openW, 2, path + ch0Num + ch1Num + chCo + 'ch0.dat'
            for i = 0, n_elements (xPosCo)-1 do printf, 2, [xPosCo[i], yPosCo[i]]
            close, 2

            openW, 2, path + ch0Num + ch1Num + chCo + 'ch1.dat'
            for i = 0, n_elements (*(*self.pParamStruct).pxPos)-1 do printf, 2, [(*(*self.pParamStruct).pxPos)[i], (*(*self.pParamStruct).pyPos)[i]]
            close, 2
         endif

         if (n_elements(image_2) gt 0) then begin
           images = make_array(2, dimI[0], dimI[1], type = size(image, /type))
           images[0,*,*] = image_2
           images[1,*,*] = image
         endif else begin
           images = make_array(1, dimI[0], dimI[1], type = size(image, /type))
           images[0,*,*] = image
         endelse

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

      if (n_elements(oldI) gt 0) then begin
         image_2 = oldI_2
         image = oldI
         dimI = oldDim
      endif
      countCoSignals -= 1
   endWhile

;   openW, 2, 'c:/rsi/move_z.dat'
;   printf, 2, make_array(128, /int, /index)
;   close, 2
   
;   write_tiff, 'c:/rsi/dendrite_move' + strCompress(string(move_z), /rem) + '.tif', (convol(float(image), PSFKernel, /edge_wrap) < 255.), 1
;   write_tiff, 'c:/rsi/dendrite_move_2_' + strCompress(string(move_z), /rem) + '.tif', (convol(float(image), PSFKernel, /edge_wrap)*2 < 255.), 1

   if fPSFKernel then return, (convol(float(image), PSFKernel, /edge_wrap) < 255.) else return, image
end


function C_sImageFilter_ImagePSF_3D_SignalSimulator::init
    filterStruct = {Name: 'C_sImageFilter_ImagePSF_3D_SignalSimulator',$; Filter Name.
                           pWidgetType:ptr_new(),$; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new(),$; Pointer on Filter Parameter Values.
                           pColocWinParam:ptr_new(),$; Pointer on Parameter Vector of Colocalization Window.
                           pSysTime:ptr_new(),$; Pointer on SysTime for the generation of random values
                           pMemTime:ptr_new(),$; Pointer on last random value
                           pPSFKernel:ptr_new(),$; Pointer on PSF-Kernel
                           pxPos:ptr_new(),$
                           pyPos:ptr_new(),$
                           pSignalIntensity:ptr_new()}

       ; Parameters of C_ImagePSFSignalSimulator.
    filterParamWidgetType = make_array(16, /string, value = 'widget_slider')
    filterParamNames = ['Apply PSF to Image',$
                 'Add PSF-Signals to Image',$
                 'Add Dendrite rad[pixel]',$
                 'Dendrite Intensity',$
                 'Add PSF-Signals from Co-Channel',$
                 'Keep Signal Intensity from Co-Channel',$
                 'Mean Signal Intensity',$
                 'Mean Signal Intensity +-',$
                 'Signal Number [#]',$
                 'Signal Density [%]',$
                 'Use PSF for 63Xw NA1.2 ex488 em520',$
                 'Use PSF for 63Xw NA1.2 ex543 em580',$
                 'Use PSF for 63Xoil NA1.4 ex488 em520',$
                 'Use PSF for 63Xoil NA1.4 ex543 em580',$
                 'Randomize OnOff',$
                 'Open Colocalization Window']

    filterParamActive = [0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0]
    filterParamMin = [0.,0.,5.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0]
    filterParamMax = [1.,0.,64, 255, 1000.,1,255.,100.,100000.,1000.,1.,1.,1.,1.,1.,10000]
    filterParamValues = [0.,0.,10., 1., 0.,0.,126.,0.,100.,5.,0.,0.,0.,0.,0.,0]

    a = [-1]
    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
    filterStruct.pxPos = ptr_new(a)
    filterStruct.pyPos = ptr_new(a)
    filterStruct.pPSFKernel = ptr_new(a)
    filterStruct.pColocWinParam = ptr_new(a)
    filterStruct.pSignalIntensity = ptr_new(a*1.)
    filterStruct.pMemTime = ptr_new(sysTime(1), /no_copy)
    filterStruct.pSysTime = ptr_new(sysTime(1), /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImagePSF_3D_SignalSimulator__define
  tmp = {C_sImageFilter_ImagePSF_3D_SignalSimulator, pParamStruct: ptr_new(), inherits C_sImageFilter}
end