;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_PatternKernel
;
; PURPOSE:
;       - Identity-Filter-Class. See Identity.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2011)
;     e_mail: shartel@med.uchile.cl
;     Jan Scheer (2011)
;     FASL (2011)
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_PatternKernel' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_PatternKernel::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_PatternKernel::checkParamsApplied
; compares the parameter values between the current values and the values applied in the last segmentation
   if ptr_valid(self.pParamApplied) then begin
      if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
         *self.pParamApplied = *(*self.pParamStruct).pValues
         return, 1
      endif
   endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
   return, 0
end


function C_sImageFilter_PatternKernel::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

      ; check the input image dimensions
   if (size(image, /n_dim) ne 2) or (max(image) eq 0) then return, image
   dimI = size(image, /dim)
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Pattern Folder Select'))[0]
   patternSelectionActive = (*(*self.pParamStruct).pActive)[whParam]
   (*(*self.pParamStruct).pActive)[whParam] = 0
   
   if patternSelectionActive then begin
      files = dialog_pickfile( /read, path = (*(*self.pParamStruct).pRuta)[0], get_path = path, filter = '*.tif')
      if(files[0] ne '') then begin
          (*(*self.pParamStruct).pRuta)[0] = path      
          patron = '_PK000'
          result = strPos(files[0], patron, /reverse_search)
          if (result eq -1) then return, image 
          (*(*self.pParamStruct).pRuta)[1] = strMid(files[0], 0, result)
      endif
   endif
   base = (*(*self.pParamStruct).pRuta)[1]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Calculate'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam] eq 0) then return, image
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Show_Kernel'))[0]
   fShowKernel = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Invert_Kernel'))[0]
   fInvertKernel = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Mutiple Kernel Option'))[0]
   fMultKernels = (*(*self.pParamStruct).pActive)[whParam]
   multKernels = (*(*self.pParamStruct).pValues)[whParam]
 
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Rotation_Theta'))[0]
   thetaActive = (*(*self.pParamStruct).pActive)[whParam]
   theta = round((*(*self.pParamStruct).pValues)[whParam])
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Max_Theta'))[0]
   fMaxTeta = (*(*self.pParamStruct).pActive)[whParam]
   maxTeta = (*(*self.pParamStruct).pValues)[whParam]
   
   if(fMaxTeta && maxTeta gt 0 && theta eq 0) then begin
      theta = 15
      whParam = (where((*(*self.pParamStruct).pNames) eq 'Rotation_Theta'))[0]
      (*(*self.pParamStruct).pValues)[whParam] = 15
   endif

   ; corrections for Kernel
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Zero Mean'))[0]
   fZeroMeanActive = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Normalization'))[0]
   fNormalize = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Keep Segmented Image'))[0]
   fKeepSegImage = (*(*self.pParamStruct).pActive)[whParam]

      ; if parameters/input image unchanged return  previosly segmented image
   if (fKeepSegImage && not(self->checkParamsApplied()) && ptr_valid(self.pSegImage)) then begin
       szSeg = size(*self.pSegImage, /dim)
       if (n_elements(szSeg) gt 1) then if ((dimI[0] eq szSeg[0]) && (dimI[1] eq szSeg[1])) then return, *self.pSegImage
   endif

   if (fKeepSegImage and (not(ptr_valid(self.pSegImage)))) or (not(fKeepSegImage)) then begin
      image *= 1.
      patronEstandar = '_PK'
      thereAreFile = 1
      digito = 0
      strDigito = '000'
      numImages = 0
      while thereAreFile do begin
         kernel = 1.* read_tiff(base + patronEstandar + strDigito + '.tif')
         if ((size(kernel))[0] eq 3) then kernel = reform(kernel[0,*,*])
         dimKernel = size(kernel, /dim)
         centerX = floor(dimKernel[0]/2.)
         centerY = floor(dimKernel[1]/2.)
       
         degree = 360.
         imageCov = 0
         imageCovTemp = 0
       
         ; Keep original image and kernel for convolution and rotation
         if fZeroMeanActive then kernel -= mean(kernel)
         if fNormalize then kernel /= total(abs(kernel))
         kernelOrg = kernel
       
         if((dimI[0] lt dimKernel[0]) or (dimI[1] lt dimKernel[1])) then print, 'Kernel size > ImageSize.. Select bigger area'       
         if(((dimI[0] ge dimKernel[0]) and (dimI[1] ge dimKernel[1])) and (maxTeta gt 0) && (fMaxTeta eq 1)) then begin   
              ; begin rotation
           for i = 0, degree-1, theta do begin
               kernel = rot(kernelOrg, i, 1.0, centerX, centerY, /interp)
               if fZeroMeanActive then kernel -= mean(kernel)
               if fNormalize then kernel /= total(abs(kernel))
               print, total(abs(kernel))
               ;if(i eq 0) then kernel = bytScl(kernel, min = min(kernel), max = max(kernel), top = 255, /nan)
               ;if(i eq 0) then write_tiff, strCompress('SuperKernel'+strDigito+'.tif', /rem), kernel
               
               if fShowKernel then begin
                  window, 12, xsize=200, ysize = 200
;                  tvscl, congrid(kernel, 200,200)
                  tvscl, congrid(kernel, 128,192)
                  maxK = max(kernel, min = minK)
                 ; movieNumber = 0
                 ; write_tiff, 'c:/rsi/patternmovie_t' + strcompress(string(movieNumber), /rem) + '.tif', congrid((kernel - minK) / (maxK-minK) * 255, 200,200);comentado por susana
                 ; movieNumber += 1
               endif
               
               if (i eq 0) then imageCovTemp = convol(image, kernel, /edge_trunc) else $
                   imageCovTemp >= convol(image, kernel, /edge_trunc)
           endfor
         endif else begin
            if fShowKernel then begin
               window, 12, xsize=200, ysize = 200
               tvscl, congrid(kernel, 200,200)
            endif
            imageCovTemp = convol(image, kernel, /edge_trunc) 
         endelse
         
         digito += 1
         strDigito = strCompress(string(digito), /rem)
         if (digito lt 10) then begin strDigito = '00' + strDigito
         endif else begin
            if (digito lt 100) then strDigito = '0' + strDigito
         endelse
       
         if(numImages eq 0) then begin
            imageCov = imageCovTemp
         endif else begin
           case multKernels of
                0: imageCov += imageCovTemp
                1: imageCov <= imageCovTemp
                2: imageCov >= imageCovTemp
                3: imageCov += imageCovTemp
           endcase
        endelse
      
        numImages += 1
        thereAreFile = file_test(base + patronEstandar + strDigito + '.tif')
      endwhile
      if (multKernels eq 3) then imageCov /= numImages
   endif
   
   ;Susana - guardo imagen convolucion kernel - inicio
;   kernel = imageCov
;   widget_control, stack_tlb, get_uValue = stackState, /no_copy
   ;stack_tlb = state.stack_tlb
;   tempFileName = ((*stackState.pImageStackInfoObject)->getParamAsStruct()).D
;   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   ;s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos
       
;   numberTimeDigits = 3       
;   strImageTime = strCompress(string(tPos), /rem)
;   while (strLen(strImageTime) lt numberTimeDigits) do strImageTime = strCompress('0' + strImageTime, /rem)
       
;   strImageZPos = strCompress(string(zPos), /rem)
;   while (strLen(strImageZPos) lt 3) do strImageZPos = strCompress('0' + strImageZPos, /rem)
   ;newName =  strImageName + '_t' + strImageTime
       
;   channelPos = 200
;  case clusPos of
     ;0: channelPos = 1
;     0: channelPos = 200 ;temporal por protección si FiltroPatternKernel accidentalmente cae en cluster0
;     1: channelPos = 2 ; Muertos Fam1
;     2: channelPos = 3 ; Muertos Fam2
;     3: channelPos = 4 ; Muertos Fam3
;     4: channelPos = 5 ; Muertos Fam4
;     5: channelPos = 6 ; Vivos Fam1
;     6: channelPos = 7 ; Vivos Fam2
;     7: channelPos = 8 ; Vivos Fam3
;     else: channelPos = 200
;   endcase
       
;   strImageChannel = strCompress(string(channelPos), /rem)
;   while (strLen(strImageChannel) lt (numberTimeDigits-1)) do strImageChannel = strCompress('0' + strImageChannel, /rem)       
   ;s_fileName = strCompress(tempFileName+'ConvKernel_clus'+string(clusPos)+'_t'+string(tPos)+'_ch'+string(chPos)+'_z'+string(zPos)+'.tif', /rem)
   ;s_fileName = strCompress(tempFileName+'t'+strImageTime+'_t'+strImageTime+'_ch'+string(strImageChannel)+'_z'+strImageZPos+'.tif', /rem)
   ;write_tiff, s_fileName, kernel,/LONG,/FLOAT ;HABILITAR ESTA LINEA PARA GUARDAR IMAGENES DE CONVOLUCION - SUSANA
   ;Susana - guardo imagen convolucion kernel - fin
      
   if ptr_valid(self.pSegImage) then *self.pSegImage = imageCov else self.pSegImage = ptr_new(imageCov) ; Construct Pointer with calculated content
   
   if fKeepSegImage then return, *self.pSegImage else return, imageCov 
end


function C_sImageFilter_PatternKernel::init
    filterStruct = {Name: 'C_PatternKernel',$       ;  Filter Name.
                           pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new(),$
                           pRuta:ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of Identity.
       ; Mutiple Kernel=> 0: sum, 1: min, 2 = max, 3 = mean 
    filterParamWidgetType = ['widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider','widget_slider','widget_slider','widget_slider']
    filterParamNames = ['Calculate','Pattern Folder Select','Show_Kernel','Mutiple Kernel Option','Rotation_Theta','Max_Theta','Invert_Kernel','Keep Segmented Image','Zero Mean','Normalization']
    filterParamActive = [1, 1, 1, 1, 0, 0, 0, 0, 1, 1]
    filterParamMin = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    filterParamMax = [1, 1, 1, 4, 360., 1, 1, 1, 1, 1]
    filterParamValues = [1, 1, 1, 0, 0., 0, 0, 0, 1, 1]
    filterRuta = ['C:\RSI\susana\kernel\','C:\RSI\susana\kernel\']

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
    filterStruct.pRuta = ptr_new(filterRuta, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_PatternKernel__define
  tmp = {C_sImageFilter_PatternKernel, pParamStruct:ptr_new(), pParamApplied:ptr_new(), pSegImage:ptr_new(), inherits C_sImageFilter}
end