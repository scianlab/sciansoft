;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ScrambleImage
;
; PURPOSE:
;       - ScrambleImage-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2005)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ScrambleImage' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ScrambleImage::getImageFilterType
   return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_ScrambleImage::apply, image = image, randomVar = randomVar, fKeepRandomVar = fKeepRandomVar

   if (n_elements(fKeepRandomVar) eq 0) then fKeepRandomVar = 0b
   if (size(image, /n_dim) ne 2) then return, image

   dimI = size(image, /dim)
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Scramble Box Size'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then boxSize = (*(*self.pParamStruct).pValues)[whParam] < (floor(min(dimI)/2.)) else boxSize = 1.

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Random Var 1'))[0]
   randomVar1 = (*(*self.pParamStruct).pValues)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Random Var 2'))[0]
   randomVar2 = (*(*self.pParamStruct).pValues)[whParam]

   xRest = dimI[0] mod boxSize
   yRest = dimI[1] mod boxSize
   dimRed = dimI - [xRest, yRest]
   xNum = floor(dimI[0]/boxSize)
   yNum = floor(dimI[1]/boxSize)
   xyNum = xNum * yNum

   scrImage = image
   if (not(fKeepRandomVar) or (n_elements(randomVar) eq 0)) then randomVar = long((xyNum * sysTime(1) + memory(/current)) / (randomVar1 * randomVar2 > 1) mod 10e5)
   fRandomVar = randomVar

    ;randomize Vector
   posVec = make_array(xyNum, /int, /ind)
   ranVec = make_array(xyNum, /int, value = -1)
   randPos = round(randomu(randomVar, xyNum) * xyNum)
   ranVec[randPos] = posVec
   whereNotSet = where(ranVec eq -1, complement = whereSet)
   posVec[ranVec[whereSet]] = -1
   whereSet = where(posVec ne -1, count)

   if (count gt 0) then repeat begin
      randPos = round(randomu(randomVar, count) * count)
      ranVec[whereNotSet[randPos]] = posVec[whereSet]
      whereNotSet = where(ranVec eq -1, complement = whereSet)
      posVec[ranVec[whereSet]] = -1
      whereSet = where(posVec ne -1, count)
   endrep until (count eq 0)

   for i = 0, xyNum-1 do begin
     scrImage[ (i*boxSize) mod dimRed[0] :(i*boxSize) mod dimRed[0] + boxSize - 1, floor(i*boxSize/dimRed[0])*boxSize :floor(i*boxSize/dimRed[0])*boxSize + boxSize - 1] = $
     image[ (ranVec[i]*boxSize) mod dimRed[0] :(ranVec[i]*boxSize) mod dimRed[0] + boxSize - 1, floor(ranVec[i]*boxSize/dimRed[0])*boxSize :floor(ranVec[i]*boxSize/dimRed[0])*boxSize + boxSize - 1]
   endfor

   if (xRest gt 0) then begin
      for i = 0, yNum-1 do begin
        randPos = round(randomu(randomVar, 1) * (xNum-1))
        for j = 0, boxSize-1 do $
          scrImage[*, i*boxSize + j] = shift(scrImage[*, i*boxSize + j], randPos*boxSize)
      endfor
   endif

   if (yRest gt 0) then begin
      for i = 0, xNum-1 do begin
        randPos = round(randomu(randomVar, 1) * (yNum-1))
        for j = 0, boxSize-1 do $
          scrImage[i*boxSize + j,*] = shift(scrImage[i*boxSize + j,*], 0, randPos*boxSize)
      endfor
   endif

   randomVar = fRandomVar
   return, scrImage
end


function C_sImageFilter_ScrambleImage::init
   filterStruct = {Name:'C_ScrambleImage',$  ;  Filter Name.
                   pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Names.
                   pNames:ptr_new(),$        ; Pointer on Filter Parameter Names.
                   pActive:ptr_new(),$       ; Pointer on Filter Parameter Active Bool.
                   pMin:ptr_new(),$          ; Pointer on Filter Parameter Min_Values.
                   pMax:ptr_new(),$          ; Pointer on Filter Parameter Max_Values.
                   pValues:ptr_new()}        ; Pointer on Filter Parameter Values.

      ; Parameters of C_ScrambleImage.
   filterParamWidgetType = make_array(3, /string, value = 'widget_slider')
   filterParamNames = ['Scramble Box Size', 'Random Var 1','Random Var 2']
   filterParamActive = [1,1,1]
   filterParamMin = [1.,0.,0.]
   filterParamMax = [512,1.,1.]
   filterParamValues = [6,1.,2.]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_ScrambleImage__define
  tmp = {C_sImageFilter_ScrambleImage, pParamStruct:ptr_new(), inherits C_sImageFilter}
end