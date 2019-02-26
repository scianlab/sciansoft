;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_DriftCorrection_byIJ
;
; PURPOSE:
;       - OpticalFlow-Filter-Class.
;
; AUTHOR:
;      Steffen Härtel (2009)
;      e_mail: shartel@med.uchile.cl
;      Corrections FASL 2010-2011
;      e_mail: fsantibanez@med.uchile.cl
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_DriftCorrection_byIJ' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData  ;pImageData Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct      ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct      ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_DriftCorrection_byIJ::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end


function C_sImageFilter_DriftCorrection_byIJ::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_DriftCorrection_byIJ::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

   whCMx = (where((*(*self.pParamStruct).pNames) eq 'CMx'))[0]
   whCMy = (where((*(*self.pParamStruct).pNames) eq 'CMy'))[0]
   whCMz = (where((*(*self.pParamStruct).pNames) eq 'CMz'))[0]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Calculate on off'))[0]
   fCalc = 0b
   if ((*(*self.pParamStruct).pActive)[whParam]) then fCalc = (*(*self.pParamStruct).pValues)[whParam] < 1b
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Save Results on off'))[0]
   fSave = 0b
   if ((*(*self.pParamStruct).pActive)[whParam]) then fSave = 1b

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Extreme Saver'))[0]
   fExtSaver = 0b
   if ((*(*self.pParamStruct).pActive)[whParam]) then fExtSaver = (*(*self.pParamStruct).pValues)[whParam] < 1b

   pParamStruct = selectedStackObject->getpParamStruct()
   totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]
   totalChannels = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]]
   totalzSlices = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]

      ; get Image Stack from Container
   dimI = size(image, /dim)
   typeI = size(image, /type)
   ;zStackOrig = make_array( [dimI,totalzSlices], type = typeI)
   zStack = make_array( [dimI,totalzSlices], type = typeI)

   xCenterDif = 0
   yCenterDif = 0
   zCenterDif = 0
   
   for i = 0, totalzSlices-1 do begin
      oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = i)
      zStack[*,*,i] = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                     tPos = tPos,$
                                                     chPos = chPos,$
                                                     zPos = i,$
                                                     clusPos = clusPos,$
                                                     segPos = segPos-1,$
                                                     cut_x = cut_x, cut_y = cut_y)
   endfor

   xSum = make_array(dimI[0],type = long)
   for i = 0, dimI[0]-1 do xSum[i] = total(zStack[i,*,*])
   xCenter = total(xSum[1:*] * (make_array(dimI[0], /index))[1:*]) / total(xSum)

   ySum = make_array(dimI[1],type = long)
   for i = 0, dimI[1]-1 do ySum[i] = total(zStack[*,i,*])
   yCenter = total(ySum[1:*] * (make_array(dimI[1], /index))[1:*]) / total(ySum)

   zSum = make_array(totalzSlices,type = long)
   for i = 0, totalzSlices-1 do zSum[i] = total(zStack[*,*,i])
   zCenter = total(zSum[1:*] * (make_array(totalzSlices, /index))[1:*]) / total(zSum)

   for i = 0, totalzSlices-1 do zStack[*,*,i] = selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i)

   if fCalc then begin
      (*(*self.pParamStruct).pValues)[whCMx] = xCenter
      (*(*self.pParamStruct).pValues)[whCMy] = yCenter
      (*(*self.pParamStruct).pValues)[whCMz] = zCenter
   endif else begin

      xCenterDif = round((*(*self.pParamStruct).pValues)[whCMx] - xCenter); + round(dimI[0]/2)
      yCenterDif = round((*(*self.pParamStruct).pValues)[whCMy] - yCenter); + round(dimI[1]/2)
      zCenterDif = round((*(*self.pParamStruct).pValues)[whCMz] - zCenter); + round(totalzSlices/2)

      print, 'CMx', (*(*self.pParamStruct).pValues)[whCMx], '  xCenter', xCenter, '  xCenterDif', xCenterDif
      print, 'CMy', (*(*self.pParamStruct).pValues)[whCMy], '  yCenter', yCenter, '  yCenterDif', yCenterDif
      print, 'CMz', (*(*self.pParamStruct).pValues)[whCMz], '  zCenter', zCenter, '  zCenterDif', zCenterDif

      minZStack = min(zStack)
      if (xCenterDif ne 0) then if (xCenterDif gt 0) then zStack[(dimI[0]-xCenterDif):*,*,*] = minZStack else zStack[0:(-xCenterDif)-1,*,*] = minZStack
      if (yCenterDif ne 0) then if (yCenterDif gt 0) then zStack[*,(dimI[1]-yCenterDif):*,*] = minZStack else zStack[*,0:(-yCenterDif)-1,*] = minZStack
      if (zCenterDif ne 0) then if (zCenterDif gt 0) then zStack[*,*,(totalzSlices-zCenterDif):*] = minZStack else zStack[*,*,0:(-zCenterDif)-1] = minZStack

      zStack = shift(zStack, xCenterDif, yCenterDif, zCenterDif)
      image = reform(zStack[*, *, zPos])
   endelse

   ;Add for pseudoValid channel propagation of drift
   if fSave then for i = 0, totalzSlices-1 do selectedStackObject->saveSelectedImage, reform(zStack[*,*,i]), tPos = tPos, chPos = chPos, zPos = i
   
   if fExtSaver then begin
      if fSave then begin
         for k = 0, totalChannels-1 do begin
            if k ne chPos then begin
               for i = 0, totalzSlices-1 do begin
                  oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = k, zPos = i)
                  zStack[*,*,i] = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                           tPos = tPos,$
                                                           chPos = k,$
                                                           zPos = i,$
                                                           clusPos = clusPos,$
                                                           segPos = segPos-1,$
                                                           cut_x = cut_x, cut_y = cut_y)
               endfor
               
               zStack = shift(zStack, xCenterDif, yCenterDif, zCenterDif)
               image = reform(zStack[*, *, zPos])
               
               for i = 0, totalzSlices-1 do selectedStackObject->saveSelectedImage, reform(zStack[*,*,i]), tPos = tPos, chPos = k, zPos = i               
            endif
         endfor
      endif
   endif


   ; Magia de JAVA ,,,, ImageJ

     ; Call to Java object
    ;oIDLClassName    = 'IDLJAVAOBJECT$PROJECT_SKELETONITATIONFROMMESHCONTRACTION'
    ;oJavaClassName   = 'project.SkeletonExtrationFromMeshContraction'

    oIDLClassName    = 'IDLJAVAOBJECT$ij_IJ'
    oJavaClassName   = 'ij.ImageJ'

   ; oIDLClassName    = 'IDLJAVAOBJECT$TurboReg_'
    ;oJavaClassName   = 'TurboReg_'

    ;oImageJ = obj_new('IDLJAVAOBJECT$ij_IJ', 'ij.ImageJ',1)
    oImageJ = obj_new(oIDLClassName, oJavaClassName,1)
    


    if ~obj_valid(oImageJ) or ~strCmp(obj_class(oImageJ), oIDLClassName, /fold_case) then begin
      print, '(ERR) creating ', oJavaClassName, '. oMeshContraction =', oImageJ
      return, image
    endif
    
;    oImageJ->ImageJ
    
 ;       TurboReg_
   return, image
end


function C_sImageFilter_DriftCorrection_byIJ::init
   filterStruct = {Name: 'C_DriftCorrection_IJ',$     ;  Filter Name.
                          pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Names.
                          pNames:ptr_new(),$       ; Pointer on Filter Parameter Names.
                          pActive:ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                          pMin:ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                          pMax:ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                          pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

      ; Parameters of C_DriftCorrection.
   filterParamWidgetType = make_array(10, /string, value = 'widget_slider')
   filterParamNames = ['Center of Mass',$
                       'Optical Flow',$
                       'CMx', 'CMy', 'CMz',$
                       'OFx', 'OFy', 'OFz',$
                       'Calculate on off',$
                       'Save Results on off',$
                       'Extreme Saver']

   filterParamActive = [1,0,1,1,1,0,0,0,1,0,0,0]
   filterParamMin = [0,0,0,0,0,-1000,-1000,-1000,0,0,0,0]
   filterParamMax = [1,1,1000,1000,1000,1000,1000,1000,1,1,1,1]
   filterParamValues = [1,0,0,0,0,0,0,0,1,0,0,1]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_DriftCorrection_byIJ__define
   tmp = {C_sImageFilter_DriftCorrection_byIJ, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
