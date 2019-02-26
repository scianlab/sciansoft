;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_OpticalFlowFrontTest
;
; PURPOSE:
;       - Identity-Filter-Class. See Identity.pro
;
; AUTHOR:
;     Dr. Mauricio Cerda (2001)
;     e_mail: mauriciocerda@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_OpticalFlowFrontTest' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData      ;pImageData   Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________


function C_sImageFilter_OpticalFlowFrontTest::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_OpticalFlowFrontTest::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end




function C_sImageFilter_OpticalFlowFrontTest::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y
                        
   ;represents the shift to test
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Shift_T'))[0]
   shiftt  =  (*(*self.pParamStruct).pValues)[whParam]
   
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Segmentation Cluster'))[0]
   segmentation = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Input Cluster'))[0]
   psf     = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Alpha or l2/l1 0: auto'))[0]
   alpha   =    (*(*self.pParamStruct).pValues)[whParam]
   tau     =    (*(*self.pParamStruct).pValues)[whParam]
   parameterAuto=0b
   if alpha eq 0.0 then parameterAuto=1b
   
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'OF_Iterations'))[0]
   ofiterations = (*(*self.pParamStruct).pValues)[whParam]
   
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Method 0:HS 1:CLG 2:LK'))[0]
   method  =  (*(*self.pParamStruct).pValues)[whParam]   ; 0: HS and 1: CLG and 2: LK
      
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Scales: 0:auto'))[0]
   scales  =  (*(*self.pParamStruct).pValues)[whParam]   ; >0
   
   ;first check parameters are on/off
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Show Result: 1:full 2:clean 3:clean diff'))[0]
   verbose =  (*(*self.pParamStruct).pValues)[whParam]   ; (uint) display info like elapsed time and stats
   verboseMode = verbose
   if verbose gt 0 then verbose=1b
      
   whParam =  (where((*(*self.pParamStruct).pNames) eq 'Segmentation by image diff'))[0]
   thit    =  (*(*self.pParamStruct).pValues)[whParam]   ; if active changes the segmentation
   
   ;represents max shift to test (not used)
   shiftx =  0
   shifty =  0
   
   print, 'shift x: ', shiftx
   print, 'shift y: ', shifty
   print, 'shift t: ', shiftt
   print, 'segmentation: ', segmentation
   print, 'psf: ', psf
   print, 'alpha: ', alpha 
   print, 'ofiterations: ', ofiterations
   
   filterPos=0 ;by default first filter of the cluster is applied
   
   oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)   
   segmentedImage = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                         tPos = tPos ,$
                                                         chPos = chPos ,$
                                                         zPos = zPos ,$
                                                         clusPos = fix(segmentation),$
                                                         cut_x = cut_x, cut_y = cut_y)   
                                                                                 
   psfImage = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                         tPos = tPos ,$
                                                         chPos = chPos ,$
                                                         zPos = zPos ,$
                                                         clusPos = fix(psf),$
                                                         cut_x = cut_x, cut_y = cut_y)
                                                         
   
   doShift = 0
   vectorColorTrick = 0    ; false vectors to have full range of orientations (and constant scale)
   boundaryConditions = 1  ; consider boundary conditions in Horn1981
   stopCrit = 0            ; 0 - num iterations / 1 - no improvement
   th=0.8
   
   epsilon=1e-16
     
   if shiftt gt 0 and (shiftx gt 0 or shifty gt 0) then begin
      print, 'ERROR: either shiftx/y or shiftt can be activated, not both!'
      return, 0
   end
   
   indSeg = where(segmentedImage ne 0)
   indSegNot = where(segmentedImage le 0)
   
   iterations=0
   if shiftx gt 0 then begin
      iterations=shiftx
   endif
   if shifty gt 0 then begin
      iterations=shifty
   endif
   if shiftt gt 0 then begin
      iterations=shiftt
   endif
   
   avgMagnitudeError  = dblArr(fix(iterations),1)
   avgAngularError    = dblArr(fix(iterations),1)
   avgGradient        = dblArr(fix(iterations),1)
   avgROIOFu          = dblArr(fix(iterations),1)
   avgROIOFv          = dblArr(fix(iterations),1)
   avgEPE             = dblArr(fix(iterations),1)
   
   
   gradientKernelX = 1./2*[[-1.0, 0.0, 1.0]]
   gradientKernelY = 1./2*[[-1.0], [0.0], [1.0]]
   
   dimI = size(psfImage, /dim)
   xSize = dimI[0]
   ySize = dimI[1]
      
   ;extensive testing
   ; for multiple alpha's/tau's testing
   
   alphas = (LINDGEN(25)+1.0)*5.0
   alphas=alphas*alphas/2.0 ;for CLG
   taus   = (LINDGEN(25)+0.0)/25.0
 
   ll=size(alphas, /dim)
   mytotal=1;ll[0]
   results=dblArr(mytotal,shiftt+1)
   lkindexes=dblArr(mytotal,shiftt+1)
   
   for j=1, mytotal do begin
   
   ;for automatic parameter testing
   if mytotal gt 1 then begin
      alpha=alphas[j-1]
      tau=taus[j-1]
   endif
   
   
   for i=1,iterations do begin
                              
      indSeg = where(segmentedImage ne 0)
      indSegNot = where(segmentedImage le 0)                        
                             
      if shiftx gt 0 then begin
          ppsfImage=shift(psfImage, i, 0)
      endif
      if shifty gt 0 then begin
          ppsfImage=shift(psfImage, 0, i)
      endif
      if shiftt gt 0 then begin
          pImage = selectedStackObject->getSelectedImageObject(tPos = tPos+i, chPos = chPos, zPos = zPos)
          ppsfImage = pImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                         tPos = tPos+i ,$
                                                         chPos = chPos ,$
                                                         zPos = zPos ,$
                                                         clusPos = fix(psf),$
                                                         cut_x = cut_x, cut_y = cut_y)
      endif
      
      image1 = psfImage * 1.0 ; this is necessary to ensure float (or double) data type
      image2 = ppsfImage * 1.0
  
      getDerivatives, image1, image2, ex=ex, ey=ey, et=et, alternativeFlag=1b
                  
      exmax=max(abs(ex))
      if exmax ne 0 then exn=sqrt(ex*ex)/exmax
        
      eymax=max(abs(ey))
      if eymax ne 0 then eyn=sqrt(ey*ey)/eymax
        
      etmax=max(abs(et))
      if etmax ne 0 then begin       
            etn=sqrt(et*et)/etmax
            etndisplay=et/etmax
      endif
      
      ; segmentation is done by threshold on image difference
      if thit gt 0.0 then begin
        indSeg    = where( etn ge th)
        indSegNot = where( etn lt th)
      endif            
               
      ;alpha/tau estimation
      if mytotal eq 1 and parameterAuto eq 1b then begin
        gradient=ex*ex+ey*ey
        
        ;alpha is always estimated in the difference area
        indSegA    = where( etn ge th)
        
        if method eq 0 then begin
          alpha=sqrt(mean(gradient(indSegA)))
          print, 'Estimated alpha hs=', alpha
        endif
        if method eq 1 then begin
          alpha=mean(gradient(indSegA))/2.0
          print, 'Estimated alpha clg=', alpha
        endif
        if method eq 2 then begin
          condition=LKcondition( image1, image2, epsilon)
          tau=0.0
          print, 'Estimated tau lk=', tau
        endif
        
      endif
                           
   
      if method eq 0 or method eq 1 then begin
        opticalFlowMultiscale, psfImage, ppsfImage, scales, method, un, vn, ALPHA = alpha, ITERATIONS = ofiterations, DOSHIFT = doShift, $
            verbose = verbose, stopCrit = stopCrit, boundaryConditions = boundaryConditions, energy1=energy1, energy2=energy2
        energy=energy1+alpha*energy2                                
      endif
      
      if method eq 2 then begin
               
        opticalFlowMultiscale, psfImage, ppsfImage, scales, method, un, vn,  verbose = verbose, stopCrit = stopCrit, energy1=energy1, $
           tau=tau, indComputed=indComputed, l1=l1, l2=l2
            
        energy=energy1
        lkindexes[j-1,i-1]=size(indComputed, /dim)
        
        ;intersection between where we want OF, and where is safe to compute it
        indSeg=SetIntersection(indSeg, indComputed)
        
      endif
      
      energymax=max(abs(energy))
      if energymax ne 0 then begin       
        energyn=sqrt(energy*energy)/energymax
      endif
      
      if n_elements(indSeg) ne 1 and  indSeg[0] ne -1 then begin
     
         groundtruth_x = fltArr(size(segmentedImage, /DIMENSION))
         groundtruth_y = fltArr(size(segmentedImage, /DIMENSION))
         groundtruth_x(indSeg)=i
         groundtruth_y(indSeg)=0
         
         avgMagnitudeError[i-1] = mean(sqrt((un(indSeg)-groundtruth_x(indSeg))^2+(vn(indSeg)-groundtruth_y(indSeg))^2))
         gradient=sqrt(ex*ex+ey*ey)
         
         avgROIOFu[i-1]=mean(un(indSeg))
         avgROIOFv[i-1]=mean(vn(indSeg))
         avgGradient[i-1]=mean(gradient(indSeg))
         
         avgEPE[i-1]=mean(sqrt( (un(indSeg) - groundtruth_x(indSeg))^2 + (vn(indSeg) - groundtruth_y(indSeg))^2))
         doproduct=un(indSeg)*groundtruth_x(indSeg)+vn(indSeg)*groundtruth_y(indSeg)
         magnitudeproduct=sqrt(un(indSeg)^2 + vn(indSeg)^2) * sqrt(groundtruth_x(indSeg)^2 + groundtruth_y(indSeg)^2)
         avgAngularError[i-1] = mean(acos( doproduct / magnitudeproduct))
      
         if verboseMode eq 2 or verboseMode eq 3 then begin
            un(indSegNot)=0
            vn(indSegNot)=0
         endif
         
         print, 'ROI size: ', n_elements(indSeg)
         
         if verboseMode eq 1 or  verboseMode eq 2 then begin 
            opticalFlowShowOF, un, vn, image1 = image1
            print, 'OF average: vx=', mean(un), 'vy=',mean(vn)
         endif
         if verboseMode eq 3 then begin 
            opticalFlowShowOF, un, vn, image1 = image1, image2=image2
            print, 'OF average: vx=', mean(un), 'vy=',mean(vn)
         endif
         
         if verbose eq 1 then begin
          
          pParamStruct = selectedStackObject->getpParamStruct()
          modelPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]

          slash = path_sep()
          if (FILE_TEST(strCompress(modelPath + '_of' + slash),/DIRECTORY) eq 0b) then FILE_MKDIR, strCompress(modelPath + '_of' + slash)
          
          if method eq 0.0 and scales eq 1 then folder='_of\hs\'
          if method eq 0.0 and scales gt 1 then folder='_of\hs_ms\'
          
          if method eq 1.0 and scales eq 1 then folder='_of\clg\'
          if method eq 1.0 and scales gt 1 then folder='_of\clg_ms\'
          
          if method eq 2.0 and scales eq 1 then folder='_of\lk\'
          if method eq 2.0 and scales gt 1 then folder='_of\lk_ms\'
          
          if (FILE_TEST(strCompress(modelPath + folder),/DIRECTORY) eq 0b) then FILE_MKDIR, strCompress(modelPath + folder)
          
          openw, unit, strcompress(modelPath+folder+'of_scales_'+strcompress(string(fix(scales)), /REMOVE_ALL)+'_param_'+strcompress(string(j), /REMOVE_ALL)+'_x_'+strcompress(string(tPos)+'_'+string(tpos+fix(i)), /REMOVE_ALL)+'.bin'), /get_lun
          writeu, unit, un
          free_lun, unit
      
          openw, unit, strcompress(modelPath+folder+'of_scales_'+strcompress(string(fix(scales)), /REMOVE_ALL)+'_param_'+strcompress(string(j), /REMOVE_ALL)+'_y_'+strcompress(string(tPos)+'_'+string(tpos+fix(i)), /REMOVE_ALL)+'.bin'), /get_lun
          writeu, unit, vn
          free_lun, unit
       
        endif
      endif else begin
      
         print, 'Segmentend area is empty, ROI is null'
      endelse
   endfor
   
   
   if method eq 0 or method eq 1 then begin
    results[j-1,*]=[alpha, avgEPE]
    ;print, 'AVG OF in ROI (x)', avgROIOFu
    ;print, 'AVG OF in ROI (y)', avgROIOFv
   endif
   
   if method eq 2 then begin
    results[j-1,*]=[tau, avgROIOFu]
   endif
   
   endfor
   print, 'Resumen'
   print, results
   
   if n_elements(indSeg) ne 1 then begin

      vx=un
      vy=vn
   
      ;HS,LK return float, CLG returns double (e.g. to read in matlab)
      return, sqrt(ex*ex+ey*ey) ;sqrt(vx*vx+vy*vy)
      
   endif else begin
      return, sqrt(ex*ex+ey*ey)
   endelse
end


function C_sImageFilter_OpticalFlowFrontTest::init

    filterStruct = {Name: 'C_OpticalFlowFrontTest',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new(),$         ; Pointer on Filter Parameter Values.
                          pDeltas:ptr_new()}
      
    ; Parameters of Identity.
    filterParamWidgetType = ['widget_slider','widget_slider','widget_slider','widget_slider','widget_slider',$
                             'widget_slider','widget_slider','widget_slider','widget_slider']
    filterParamNames = ['Shift_T','Segmentation Cluster', 'Input Cluster', 'Alpha or l2/l1 0: auto', 'OF_Iterations',$
                        'Method 0:HS 1:CLG 2:LK', 'Scales: 0:auto', 'Show Result: 1:full 2:clean 3:clean diff', 'Segmentation by image diff']
    filterParamActive = [1,1,1,1,1,1,1,1,0]
    filterParamMin = [0,0,0,0,0,0,0,0,0]
    filterParamMax = [100,20,20,1000,1000,10,10,3,1]
    filterParamValues = [1,0,1,0,200,0,1,0,0.0]
    filterParamDeltas = [1,$
                         1,$
                         1,$
                         1,$
                         1,$
                         1,$
                         1,$
                         1,$
                         .1]
    
    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
    filterStruct.pDeltas = ptr_new(filterParamDeltas, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_OpticalFlowFrontTest__define
  tmp = {C_sImageFilter_OpticalFlowFrontTest, pParamStruct: ptr_new(), inherits C_sImageFilter}
end