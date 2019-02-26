;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_PatternKernelSaver
;
; PURPOSE:
;       - Identity-Filter-Class. See Identity.pro
;
; AUTHOR:
;     FASL (2011) fsantibanez@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_PatternKernel' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_PatternKernelSaver::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_PatternKernelSaver::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

      
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Limit Size'))[0]
   limitSize = (*(*self.pParamStruct).pValues)[whParam]
   limitSizeActive = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'SaveKernel'))[0]
   bSave = (*(*self.pParamStruct).pValues)[whParam]
   bSaveActive = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'dimX'))[0]
   dimX = (*(*self.pParamStruct).pValues)[whParam]
   dimXActive = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'dimY'))[0]
   dimY = (*(*self.pParamStruct).pValues)[whParam]
   dimYActive = (*(*self.pParamStruct).pActive)[whParam]
 
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Background System'))[0]
   backGSelect = (*(*self.pParamStruct).pValues)[whParam]
   backGSelectActive = (*(*self.pParamStruct).pActive)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Background Param'))[0]
   backGValue = (*(*self.pParamStruct).pValues)[whParam]
   backGValueActive = (*(*self.pParamStruct).pActive)[whParam]   
     
   ; corrections for Kernel
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Zero Mean'))[0]
   bZeroMean = (*(*self.pParamStruct).pValues)[whParam]
   bZeroMeanActive = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Normalization'))[0]
   bNormalization = (*(*self.pParamStruct).pValues)[whParam]
   bNormalizationActive = (*(*self.pParamStruct).pActive)[whParam]

   kernel = image         
      ; check the input image dimensions
   if (size(image, /n_dim) ne 2) or (max(image) eq 0) then return, image
   szI = size(image, /dim)
   
   patronEstandar = '_PK'
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Select/Create First File'))[0]
   patternSelectionActive = (*(*self.pParamStruct).pActive)[whParam]
   (*(*self.pParamStruct).pActive)[whParam] = 0
   if(patternSelectionActive)then begin
      files = dialog_pickfile( /read, path = (*(*self.pParamStruct).pRuta)[0], get_path = path, filter = '*.tif')
      if(files[0] ne '') then begin
          (*(*self.pParamStruct).pRuta)[0] = path
          (*(*self.pParamStruct).pRuta)[1] = files[0]
      endif
   endif

    ; verify if '_PK' structure is present
    result = strPos( (*(*self.pParamStruct).pRuta)[1], patronEstandar, /reverse_search)
    if (result eq -1) then begin
      ; create new image          return, image
      nameFile = strcompress((*(*self.pParamStruct).pRuta)[1] + strcompress(patronEstandar + '000' + '.tif',/rem)); name of file... only need add number('XXX') and '.tif'
    endif else begin
    ; rewrite image and increase the counter
      nameFile = (*(*self.pParamStruct).pRuta)[1]; name of file... only need add number('XXX') and '.tif'
    endelse

                        ; *state.subImage, /float
            ;    write_tiff, strCompress(path + base + '_PK' + strDigit + '.tif', /rem), tvrd(true = 1)
   if(bSaveActive) then begin
    ; options

    if(limitSize and (dimX gt 0) and (dimY gt 0)) then begin

      dimImage     = size(image, /dim)
      dimKernel    = size(image, /dim)
      dimKernel[0] = dimX
      dimKernel[1] = dimY
      minX         = dimKernel[0] < dimImage[0]
      minY         = dimKernel[1] < dimImage[1]
      centroImageX = floor(dimImage[0]/2)
      centroImageY = floor(dimImage[1]/2)
      
      initKernelX = 0
      initKernelY = 0
      
      endKernelX = dimX-1
      endKernelY = dimY-1
      
      
      offsetX      = 0
      if((dimKernel[0]  MOD 2) eq 0)then  offsetX = 1  
      initImageX        = centroImageX - floor(dimKernel[0]/2)  
      endImageX         = centroImageX + floor(dimKernel[0]/2) - offsetX

      offsetY      = 0
      if((dimKernel[1]  MOD 2) eq 0)then  offsetY = 1  
      initImageY        = centroImageY - floor(dimKernel[1]/2)  
      endImageY         = centroImageY + floor(dimKernel[1]/2) - offsetY
      
      kernel = make_array(dimKernel[0],dimKernel[1], /byte, value = 0)
      
      if(dimImage[0] lt dimKernel[0]) then begin
        offsetX      = 0
        if((dimImage[0]  MOD 2) eq 0)then  offsetX = 1  
        initImageX        = centroImageX - floor(dimImage[0]/2)  
        endImageX         = centroImageX + floor(dimImage[0]/2) - offsetX
      endif
      if(dimImage[0] lt dimKernel[0]) then begin
        offsetY      = 0
        if((dimImage[1]  MOD 2) eq 0)then  offsetY = 1  
        initImageY        = centroImageY - floor(dimImage[1]/2)  
        endImageY         = centroImageY + floor(dimImage[1]/2) - offsetY
      endif
      ;kernel[initKernelX:endKernelX,endKernelY:initKernelY] = image[initImageX:endImageX,initImageY:endImageY]
      kernel[initKernelX:endKernelX,initKernelY:endKernelY] = image[initImageX:endImageX,initImageY:endImageY]

      if(bZeroMean and bZeroMeanActive) then kernel = kernel  - mean(kernel)
      if(bNormalization and bNormalizationActive) then kernel = kernel/TOTAL(kernel)
      
      if(dimImage[0] gt dimKernel[0]) then begin
      endif
      if(dimImage[1] gt dimKernel[1]) then begin
      endif


    endif else begin
        kernel = image
        dimKernel = size(kernel, /dim)
    endelse
     kernel = bytScl(kernel, min = min(kernel), max = max(kernel), top = 255, /nan)
     write_tiff, nameFile, kernel
     (*(*self.pParamStruct).pRuta)[1] = nameFile
     ; Update next nameFile
      result = strPos( (*(*self.pParamStruct).pRuta)[1], patronEstandar, /reverse_search)
      if (result eq -1) then begin
        return, kernel
      endif else begin
        nameFile = strMid((*(*self.pParamStruct).pRuta)[1],0,result+3); name of file... only need add number('XXX') and '.tif'
        numberstring = strMid((*(*self.pParamStruct).pRuta)[1],result+3,3)
        numero = s_getRightNumberFromString(numberstring) + 1
        numberstring = strCompress(STRING(numero), /rem)
         if(numberstring lt 10) then begin numberstring = '00' + numberstring
         endif else begin
            if(numberstring lt 100) then numberstring = '0' + numberstring
         endelse        
        (*(*self.pParamStruct).pRuta)[1] = nameFile + numberstring + '.tif'
      endelse
    endif
  return, kernel 
end


function C_sImageFilter_PatternKernelSaver::init

    filterStruct = {Name: 'C_PatternKernelSaver',$       ;  Filter Name.
                           pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new(),$
                           pRuta:ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of Identity.
       ; need scale for backGround application
       ; limit size=> 0: no limits, 1: use specific dimensions, 2 =  
       ; backG sys=>  0: fill with Backg Param (BP) value, 1: Stimatation if mean and use for BP value, 2 = gaussian reduction fill
    filterParamWidgetType = ['widget_slider','widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider', 'widget_slider']
    filterParamNames = ['SaveKernel','Select/Create First File','Limit Size','dimX','dimY','Background System','Background Param','Zero Mean','Normalization']
    filterParamActive = [0,1, 1, 1, 1, 1, 1,1,1]
    filterParamMin = [0,0, 0, 1, 1, 0, -1000.0,0,0]
    filterParamMax = [1,1, 1, 2000, 2000, 2, 1000.0,1,1]
    filterParamValues = [0,1, 0, 10, 10, 0, 0.0,0,0]
    filterRuta = ['C:\RSI\','C:\RSI\']

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

pro C_sImageFilter_PatternKernelSaver__define
  tmp = {C_sImageFilter_PatternKernelSaver, pParamStruct:ptr_new(), pParamApplied:ptr_new(), pSegImage:ptr_new(), inherits C_sImageFilter}
end