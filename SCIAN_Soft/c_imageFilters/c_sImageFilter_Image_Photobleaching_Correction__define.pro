;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Image_Photobleaching_Correction
;
; PURPOSE:
;       - Photobleaching corrects
;
; AUTHOR:
;   Luis Briones (2011)
;   e_mail:lbriones@med.uchile.cl
;
; CALLING SEQUENCE:
;   result = obj_new('C_sImageFilter_Image_Photobleaching_Correction')
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_Image_Photobleaching_Correction::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

pro linfunct, X, A, F, pder
  F = A[0]*X+A[1]
  ; If procedure is called with four parameters, calculate the
  ;partial derivatives.
  if (n_params() ge 3) then pder = [[X],[replicate(1.0, n_elements(X))]] 
end

pro gfunct, X, A, F, pder
  bx = exp(-1.*X / A[1]) 
  F = A[0] * bx + A[2]
  ;If the procedure is called with four parameters, calculate the 
  ;partial derivatives. 
  if (n_params() ge 4) then pder = [[bx], [-1.*A[0] * X * bx], [replicate(1.0, n_elements(X))]] 
end

pro quadfunct, X, A, F, pder
  F = A[0]*(X*X) + A[1]*X + A[2]
  ;If the procedure is called with four parameters, calculate the 
  ;partial derivatives.
  if (n_params() ge 4) then pder = [[X*X], [X], [replicate(1.0, n_elements(X))]]
end

pro cubicfunct, X, A, F, pder
   F = A[0]*(X*X*X)+A[1]*X*X+A[2]*X+A[3]
   ;If the procedure is called with five parameters, calculate the 
   ;partial derivatives.
   if (n_params() ge 4) then pder = [[X*X*X], [X*X], [X], [replicate(1.0, n_elements(X))]]
end

function singleExp, X, A 
  return, exp(-1.*X / A[1])
end

function C_sImageFilter_Image_Photobleaching_Correction::apply, image = image,$
                        selectedStackObject = selectedStackObject,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos,$
                        chPos = chPos,$
                        zPos = zPos,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y

   whPos = (where((*(*self.pParamStruct).pNames) eq 'Plot Window'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then fPlot = 1b else fPlot = 0b
   whPos = (where((*(*self.pParamStruct).pNames) eq 'T Profile'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then fTProf = 1b else fTProf = 0b
   whPos = (where((*(*self.pParamStruct).pNames) eq 'Number of Channel'))[0]
   fNCh = (*(*self.pParamStruct).pValues)[whPos]   
   whPos = (where((*(*self.pParamStruct).pNames) eq 'Number of Cluster'))[0]
   fNCluster = (*(*self.pParamStruct).pValues)[whPos]
   ;Fitting curve 0=linear 1=exponential, 2=quadratic, 3=cubic
   whPos = (where((*(*self.pParamStruct).pNames) eq 'Type of Fitting (0->LN_1->EX_2->QD_3->CB)'))[0]
   typeFit = (*(*self.pParamStruct).pValues)[whPos]
   whPos = (where((*(*self.pParamStruct).pNames) eq 'Save Images'))[0]
   saveI = (*(*self.pParamStruct).pValues)[whPos]
   whPos = (where((*(*self.pParamStruct).pNames) eq 'Without Mask'))[0]
   wMask = (*(*self.pParamStruct).pValues)[whPos]
   
   dimI = size(image, /dim)
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum
     
   case 1 of
   fTProf: begin
      intProf = make_array(totalTNum, /float)
      a=ptrarr(totalTNum,/ALLOCATE_HEAP)
      for i = 0, totalTNum - 1 do begin
          image = (selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = zPos))
          selectedStackObject->getSelectedClusterMask, mask = intImage, tPos = i, chPos = chPos, zPos = zPos, clusPos = fNCluster
          if ((n_elements(intImage) eq 1) and (intImage[0] eq -1)) then begin
             oImage = selectedStackObject->getSelectedImageObject(tPos = i, chPos = chPos, zPos = zPos)
             intImage = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                                  tPos = i,$
                                                                  chPos = chPos,$
                                                                  zPos = zPos,$
                                                                  clusPos = fNCluster,$
                                                                  segPos = segPos - 1, $
                                                                  cut_x = cut_x, cut_y = cut_y)
          endif else intImage = intImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
                  
         *a[i] = array_indices(dimI,where(intImage eq 255),/DIMENSIONS)
         data=image[(*a[i])[0,*],(*a[i])[1,*]]
         intProf[i] = mean(data)
         ;intProf[i] = (moment(((selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]])))[0]
      endfor
              
         pParamStruct = selectedStackObject->getpParamStruct()
         whereDim = (where((*(*pParamStruct).pNames) eq 'Time Interval [s]'))[0]
         if (whereDim ne -1) then realTimeInt = *(((*pParamStruct).pValues)[whereDim]) else realTimeInt = 1
         T = float(indgen(totalTNum)) * realTimeInt
     
         case typeFit of
            0: functname='linfunct'
            1: functname='gfunct'
            2: functname='quadfunct'
            3: functname='cubicfunct'
         endcase
         
         if (typeFit eq 0) then begin
            AA=[1,1] 
            coefs = SVDFIT (T, intProf, 2, A=AA, /DOUBLE, SIGMA=SIGMA, yfit=yfit)
         endif
                  
         if (typeFit ne 0) then begin
            if ((typeFit eq 1) or (typeFit eq 2)) then begin
                ;Provide n initial guess of the function's parameters. 
                ;params0 = [(max(intProf)-min(intProf))/2, (max(T) - min(T))/2, mean(intProf)]
                ;caida rapida... 
                ;params0 = [max(intProf), (max(T) - min(T))/6, mean(intProf)]
                ;caida lenta
                params0 = [mean(intProf)/2, (max(T) - min(T))/2,(max(intProf)-min(intProf))/2]
            endif
            if (typeFit eq 3) then begin
               ; No estoy tan seguro que estos valores de parametros
               ; sean los mejores para una cubica
               params0 = [mean(intProf)/2, (max(T) - min(T))/2,(max(intProf)-min(intProf))/2, 1]
            endif
            weights = make_array(totalTNum, /float, value=1.0)
         
            ;Compute the parameters.
            yfit = curvefit(T, intProf, weights, params0, FUNCTION_NAME=functname, /Double, ITMAX = 1000, yerror = errFit)
            
            print, params0
            print, errFit
             
         endif
                            
      if fPlot then begin
        if (typeFit ne 0) then titlePlot = strcompress('Photobleaching Decay Fit (tau = ' + string(params0[1], format = '(f0.2)') + ' [s])') $
        else titlePlot = strcompress('Photobleaching Decay Fit (Linear) [s]')
        
         iPlot, T, intProf, title = 'Intensity Profile [t]', xtitle = 'time [sec]', $
              ytitle = 'mean intensity', sym_index = 6, sym_size = 0.5, sym_thick = 2, sym_color = [0, 0, 255], $
              view_title = titlePlot, linestyle = 6
              
         iPlot, T, yfit, COLOR = [255, 0, 0], THICK = 2, /overplot
        
      endif
   endcase
   fFindD: begin
   endcase
   else:
   endcase
           
    path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]
    maxValue = max(yfit)
    yfit_bin = yfit/maxValue
    yfitted = make_array(totalTNum)
    file = make_array(totalTNum, /STRING)
    
    for i = 0, totalTNum-1 do begin
      tempImage = fix((selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = zPos)))
      image1 = tempImage
      if wMask eq 0 then image1[(*a[i])[0,*],(*a[i])[1,*]] = (tempImage[(*a[i])[0,*],(*a[i])[1,*]]/yfit_bin[i]) $
      else  image1 = tempImage/yfit_bin[i]
      
      yfitted[i] = mean(image1[(*a[i])[0,*],(*a[i])[1,*]])   
      ;print, yfit_bin[i]
      
      B = (image1 gt 255) * 255
      C = (image1 le 255) * image1
      image1 = B + C
      
      if saveI then begin

          if (i lt 10) then begin
            fn = strcompress('PC_t00' + string(i)+'_z000_ch00' + string(fNCh-1) + '.tif', /rem)
          endif else begin
              if(i ge 10 and i lt 100) then begin
                fn = strcompress('PC_t0' + string(i)+'_z000_ch00' + string(fNCh-1) + '.tif', /rem)
              endif else begin
                fn = strcompress('PC_t' + string(i)+'_z000_ch00' + string(fNCh-1) + '.tif', /rem)
              endelse
          endelse
    
          write_tiff, strcompress(path +fn), image1  
          file[i] = strcompress(path +fn)
      endif
    endfor
    
    
    iPlot, T, yfit_bin, color = [255, 0, 0], thick = 2
    
    iPlot, T, yfitted, color = [0, 255, 0], thick = 2, xtitle = 'time [sec]', $
           ytitle = 'mean intensity', sym_index = 6, sym_size = 0.5, sym_thick = 2, sym_color = [0, 255, 0], $
           view_title = 'Intensity Corrected Profile [t]', linestyle = 6
    
                        
    for i=0, n_elements(a)-1 do ptr_free, a[i]
    a=1b
      
   return, image
end


function C_sImageFilter_Image_Photobleaching_Correction::init
   ImageFilterStruct = {Name:'C_Image_Photobleaching_Correction',$    ;  Filter Name.
                        pWidgetType:ptr_new(),$  ; Pointer on Filter Parameter Names.
                        pNames:ptr_new(),$    ; Pointer on Filter Parameter Names.
                        pActive:ptr_new(),$   ; Pointer on Filter Parameter Active Bool.
                        pMin:ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                        pMax:ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                        pValues:ptr_new()}    ; Pointer on Filter Parameter Values.

     ; Filer-Parameters
   filterParamWidgetType = make_array(7, /string, value = 'widget_slider')
   filterParamNames = ['T Profile', 'Plot Window','Number of Channel','Number of Cluster','Type of Fitting (0->LN_1->EX_2->QD_3->CB)','Save Images', 'Without Mask']
   filterParamActive = [1, 1, 0, 1, 1, 1, 1]
   filterParamMin = [0, 0, 0, 0, 0, 0, 0]
   filterParamMax = [1, 1, 100, 30, 3, 1, 1]
   filterParamValues = [1, 0, 1, 0, 1, 1, 0]

   ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
   return, 1
end


pro C_sImageFilter_Image_Photobleaching_Correction__define
   tmp = {C_sImageFilter_Image_Photobleaching_Correction, pParamStruct:ptr_new(), inherits C_sImageFilter}
end