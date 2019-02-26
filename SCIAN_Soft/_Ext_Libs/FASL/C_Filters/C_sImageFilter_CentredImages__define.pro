;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_CentredImages
;
; PURPOSE:
;       - CentredImages-Filter-Class.
;
; AUTHOR:
;      FASL 2013-
;      e_mail: fsantibanez@med.uchile.cl
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_CentredImages' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData  ;pImageData Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct      ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct      ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_CentredImages::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_CentredImages::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_CentredImages::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y
                                     
   pParamStruct = selectedStackObject->getpParamStruct()
   path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]                                        

   whCMx = (where((*(*self.pParamStruct).pNames) eq 'CMx'))[0]
   whCMy = (where((*(*self.pParamStruct).pNames) eq 'CMy'))[0]
   whCMz = (where((*(*self.pParamStruct).pNames) eq 'CMz'))[0]
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Calculate on off'))[0]
   fCalc = 0b
   if ((*(*self.pParamStruct).pActive)[whParam]) then fCalc = (*(*self.pParamStruct).pValues)[whParam] < 1b
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Save Results on off'))[0]
   fSave = 0b
   if ((*(*self.pParamStruct).pActive)[whParam]) then fSave = 1b
   (*(*self.pParamStruct).pActive)[whParam] = 0

   whParam = (where((*(*self.pParamStruct).pNames) eq 'deltaAngle'))[0]
   deltaAngle = 1
   if ((*(*self.pParamStruct).pActive)[whParam]) then deltaAngle = ((*(*self.pParamStruct).pValues)[whParam] < 360)
   (*(*self.pParamStruct).pValues)[whParam] = deltaAngle

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Extreme Saver'))[0]
   fExtremeSaver = 0b
   if ((*(*self.pParamStruct).pActive)[whParam]) then fExtremeSaver = 1b
   (*(*self.pParamStruct).pValues)[whParam] = 0
   (*(*self.pParamStruct).pActive)[whParam] = 0

   whParam = (where((*(*self.pParamStruct).pNames) eq 'shiftAngle'))[0]
   shiftAngle = 1
   if ((*(*self.pParamStruct).pActive)[whParam]) then shiftAngle = ((*(*self.pParamStruct).pValues)[whParam] < 360)
   (*(*self.pParamStruct).pValues)[whParam] = shiftAngle

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
   
   chPosRef = 0;
   tPosRef = 0;
   for i = 0, totalzSlices-1 do begin
      oImage = selectedStackObject->getSelectedImageObject(tPos = tPosRef, chPos = chPosRef, zPos = i)
      zStack[*,*,i] = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                     tPos = tPosRef,$
                                                     chPos = chPosRef,$
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

   ;zSum = make_array(totalzSlices,type = long)
   ;for i = 0, totalzSlices-1 do zSum[i] = total(zStack[*,*,i])
   ;zCenter = total(zSum[1:*] * (make_array(totalzSlices, /index))[1:*]) / total(zSum)

  ; for i = 0, totalzSlices-1 do zStack[*,*,i] = selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i)
  tiempoInicial = tPos
  tiempoFinal   = tPos
  if(fExtremeSaver) then begin
      tiempoInicial = 0
      tiempoFinal   = totalTimes - 1
  endif
    for tiempo = tiempoInicial, tiempoFinal do begin
          oImage = selectedStackObject->getSelectedImageObject(tPos = tiempo, chPos = chPosRef, zPos = zPos)
          refImage = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                         tPos = tiempo,$
                                                         chPos = chPosRef,$
                                                         zPos = zPos,$
                                                         clusPos = clusPos,$
                                                         segPos = segPos-1,$
                                                         cut_x = cut_x, cut_y = cut_y)
          tempImage = refImage;
                 if fSave then begin
                   if(FILE_TEST(strCompress(path+'_PorcentajeProtusiones'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(path+'_PorcentajeProtusiones'+ path_sep())
                   subDir = strCompress(path+'_PorcentajeProtusiones'+ path_sep() +'_deltaAngle_'+ string(deltaAngle) + '_shiftAngle_'+ string(shiftAngle) + path_sep(),/rem)
                   if(FILE_TEST(subDir,/DIRECTORY) eq 0b) then  FILE_MKDIR, subDir
                      filename = strCompress(subDir +'_PorcentajeProtusiones_Time'+ string(tiempo) +'.txt',/rem)      
                      get_lun, U
                              
                      openW, U, filename
                 endif
        
           if fCalc and (deltaAngle gt 1) then begin
              ;tempImage = image;
              xCenterDif = round(round(dimI[0]/2) - xCenter); + round(dimI[0]/2)
              yCenterDif = round(round(dimI[1]/2) - yCenter); + round(dimI[1]/2)
              ;zCenterDif = round( - zCenter); + round(totalzSlices/2)
              tempImage = shift(refImage, xCenterDif, yCenterDif)
              
              for delta = 0, (360 - deltaAngle), deltaAngle do begin
                deltaShifted = delta + shiftAngle
                rotatedImage = rot(tempImage,deltaShifted,1,round(dimI[0]/2),round(dimI[1]/2))
                rotatedImage(*,0:round(dimI[1]/2)) = 0
                rotatedImage = rot(rotatedImage,deltaAngle,1,round(dimI[0]/2),round(dimI[1]/2))
                rotatedImage(*,round(dimI[1]/2):*) = 0
                 if fSave then begin
                    printf, U, 100.0*total(rotatedImage)/total(refImage)
                 endif
              endfor
              image = rotatedImage;
           endif
        
                 if fSave then begin
                    close, U 
                    free_lun, U          
                 endif
   endfor
   return, image
end


function C_sImageFilter_CentredImages::init
   filterStruct = {Name: 'C_DriftCorrection',$     ;  Filter Name.
                          pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Names.
                          pNames:ptr_new(),$       ; Pointer on Filter Parameter Names.
                          pActive:ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                          pMin:ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                          pMax:ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                          pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

      ; Parameters of C_DriftCorrection.
   filterParamWidgetType = make_array(8, /string, value = 'widget_slider')
   filterParamNames = ['CMx', 'CMy', 'CMz',$
                       'deltaAngle','shiftAngle',$
                       'Calculate on off',$
                       'Save Results on off',$
                       'Extreme Saver']

   filterParamActive = [1,1,1,1,1,0,0,0]
   filterParamMin = [-1000,-1000,-1000,-360,-360,0,0,0]
   filterParamMax = [1000,1000,1000,360,360,1,1,1]
   filterParamValues = [0,0,0,0,0,0,0,0]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_CentredImages__define
   tmp = {C_sImageFilter_CentredImages, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
