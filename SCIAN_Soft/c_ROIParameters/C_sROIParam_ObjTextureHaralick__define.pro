;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjTextureHaralick
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjTextureHaralick' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjTextureHaralick::apply, C_sROIGroupObj = C_sROIGroupObj, position = position

    whParam = [(where( *(*self.pParamStruct).pNames eq 'Entropy'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Contrast'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Inverse Moment Difference'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq '2nd Angular Moment'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Entropy div MaxE'))[0] ]

     ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1: if (position[0] eq -1) then return else  whParamActive[position] = 1
       else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase

    if whParamActive[4] then begin
       (*(*self.pParamStruct).pActive)[whParam[0]] = 1
       whParamActive[0] = 1
    endif

       ; check Pointers
    wherePA = where(whParamActive eq 1)
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROIGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin

         ; set Object Number Vector
       (*self.pParamStruct).pROINumberVect = ptr_new( (C_sROIGroupObj->getObjectNumberVector()) , /no_copy)

       whereTopScaleVal = (where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Top Scale Value'))[0]
       whereMaxScaleInt = (where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Maximum Scale Intensity'))[0]
       wherePixRad = (where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Pixel Radius'))[0]
       whereMedian = (where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Median Value'))[0]
       if whParamActive[0] then begin
         entrTopScaleVal = 1 > (*(*(*self.pValueStruct)[whParam[0]]).pValues)[whereTopScaleVal]
         if ((*(*(*self.pValueStruct)[whParam[0]]).pActive)[whereMaxScaleInt] eq 1) then $
          entrMaxScaleInt = 1 > (*(*(*self.pValueStruct)[whParam[0]]).pValues)[whereMaxScaleInt] else entrMaxScaleInt = -1
         entrPixRad = 1 > (*(*(*self.pValueStruct)[whParam[0]]).pValues)[wherePixRad]
         if ((*(*(*self.pValueStruct)[whParam[0]]).pActive)[whereMedian]) then $
          entrMedian = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[whereMedian] else entrMedian = 0
         entrCoocMatrix = intArr(entrTopScaleVal+1, entrTopScaleVal+1)
         *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = fltArr(nObjects)
       endif
       if whParamActive[1] then begin
         contrTopScaleVal = 1 > (*(*(*self.pValueStruct)[whParam[1]]).pValues)[whereTopScaleVal]
         contrMaxScaleInt = 1 > (*(*(*self.pValueStruct)[whParam[1]]).pValues)[whereMaxScaleInt]
         contrPixRad = 1 > (*(*(*self.pValueStruct)[whParam[1]]).pValues)[wherePixRad]
         if ((*(*(*self.pValueStruct)[whParam[1]]).pActive)[whereMedian]) then $
          contrMedian = (*(*(*self.pValueStruct)[whParam[1]]).pValues)[whereMedian] else contrMedian = 0
         contrCoocMatrix = intArr(contrTopScaleVal+1, contrTopScaleVal+1)
         *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = fltArr(nObjects)
       endif
       if whParamActive[2] then begin
         invTopScaleVal = 1 > (*(*(*self.pValueStruct)[whParam[2]]).pValues)[whereTopScaleVal]
         invMaxScaleInt = 1 > (*(*(*self.pValueStruct)[whParam[2]]).pValues)[whereMaxScaleInt]
         invPixRad = 1 > (*(*(*self.pValueStruct)[whParam[2]]).pValues)[wherePixRad]
         if ((*(*(*self.pValueStruct)[whParam[2]]).pActive)[whereMedian]) then $
          invMedian = (*(*(*self.pValueStruct)[whParam[2]]).pValues)[whereMedian] else invMedian = 0
         invCoocMatrix = intArr(invTopScaleVal+1, invTopScaleVal+1)
         *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = fltArr(nObjects)
       endif
       if whParamActive[3] then begin
         scndTopScaleVal = 1 > (*(*(*self.pValueStruct)[whParam[3]]).pValues)[whereTopScaleVal]
         scndMaxScaleInt = 1 > (*(*(*self.pValueStruct)[whParam[3]]).pValues)[whereMaxScaleInt]
         scndPixRad = 1 > (*(*(*self.pValueStruct)[whParam[3]]).pValues)[wherePixRad]
         if ((*(*(*self.pValueStruct)[whParam[3]]).pActive)[whereMedian]) then $
          scndMedian = (*(*(*self.pValueStruct)[whParam[3]]).pValues)[whereMedian] else scndMedian = 0
         scndCoocMatrix = intArr(scndTopScaleVal+1, scndTopScaleVal+1)
         *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = fltArr(nObjects)
       endif
       if whParamActive[4] then *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = fltArr(nObjects)

         ; Object-Loop
       for objNumber = 0, nObjects-1 do begin

         objArray = C_sROIGroupObj->getObjectInArray(objNumber = objNumber)
         whereMask = where(objArray.mask)
         intImage = objArray.mask - 1
         dimImage = size(intImage, /dim)

          ; Entropy
         if whParamActive[0] then begin
              ; fill coocurrence Matrix with Image-Data
          if (entrMaxScaleInt eq -1) then intImage[whereMask] = (s_scaleArray(objArray.intValues, minValue = min(objArray.intValues), maxValue = max(objArray.intValues), topValue = entrTopScaleVal))[whereMask] $
              else intImage[whereMask] = (s_scaleArray(objArray.intValues, minValue = 0, maxValue = entrMaxScaleInt, topValue = entrTopScaleVal))[whereMask]
          expandImage = s_Expand_Mirror(intImage, entrPixRad, /nonMirror)
          if (entrMedian ne 0) then expandImage = median(expandImage, round(2*entrMedian+1))
          tvscl, expandImage
          for k = -entrPixRad, entrPixRad do for l = -entrPixRad, entrPixRad do begin
              pixDist = 1.0 * (k*k + l*l)
              if (sqrt(pixDist) le entrPixRad) then begin
                 for i = 0, entrTopScaleVal do begin
                   whereI = where(intImage[whereMask] eq i)
                   if (whereI[0] ne -1) then begin
                    for j = 0, entrTopScaleVal do begin
                        whereJ = where(((expandImage[ entrPixRad+k : entrPixRad+k+dimImage[0]-1,  entrPixRad+l : entrPixRad+l+dimImage[1]-1 ])[whereMask])[whereI] eq j)
                        if (whereJ[0] ne -1) then entrCoocMatrix[i,j] = entrCoocMatrix[i,j] + n_elements(whereJ)
                    endfor
                   endif
                 endfor
              endif
          endfor
              ; normalize  coocurrence Matrix
          entrCoocMatrix = entrCoocMatrix / ( (1.*total(entrCoocMatrix)) > 1.)
          whereP = where(entrCoocMatrix gt 0.)
              ; calculate Entropy
          if whParamActive[0] then begin
              if (whereP[0] ne -1) then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[objNumber] = total(entrCoocMatrix[whereP] * (-alog(entrCoocMatrix[whereP]))) $
                 else (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[objNumber] = 0.
          endif
          if whParamActive[4] then begin
              if (whereP[0] ne -1) then (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[objNumber] = total(entrCoocMatrix[whereP] * (-alog(entrCoocMatrix[whereP]))) / (-alog( 1./((size(entrCoocMatrix, /dim))[0])^2)) $
                 else (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[objNumber] = 0.
          endif
          entrCoocMatrix = entrCoocMatrix * 0
         endif

          ; Kontrast
         if whParamActive[1] then begin
              ; fill coocurrence Matrix with Image-Data
          if (entrMaxScaleInt eq -1) then intImage[whereMask] = (s_scaleArray(objArray.intValues, minValue = min(objArray.intValues), maxValue = max(objArray.intValues), topValue = entrTopScaleVal))[whereMask] $
              else intImage[whereMask] = (s_scaleArray(objArray.intValues, minValue = 0, maxValue = entrMaxScaleInt, topValue = entrTopScaleVal))[whereMask]
          expandImage = s_Expand_Mirror(intImage, contrPixRad, /nonMirror)
          if (contrMedian ne 0) then expandImage = median(expandImage, round(2*contrMedian+1))
          for k = -contrPixRad, contrPixRad do for l = -contrPixRad, contrPixRad do begin
              pixDist = 1.0 * (k*k + l*l)
              if (sqrt(pixDist) le contrPixRad) then begin
                 for i = 0, contrTopScaleVal do begin
                   whereI = where(intImage[whereMask] eq i)
                   if (whereI[0] ne -1) then begin
                    for j = 0, contrTopScaleVal do begin
                        whereJ = where(((expandImage[ contrPixRad+k : contrPixRad+k+dimImage[0]-1,  contrPixRad+l : contrPixRad+l+dimImage[1]-1 ])[whereMask])[whereI] eq j)
                        if (whereJ[0] ne -1) then contrCoocMatrix[i,j] = contrCoocMatrix[i,j] + n_elements(whereJ)
                    endfor
                   endif
                 endfor
              endif
          endfor
              ; normalize  coocurrence Matrix
          contrCoocMatrix = contrCoocMatrix / ( (1.*total(contrCoocMatrix)) > 1.)
          sum = 0.
          for i = 0, contrTopScaleVal do for j = 0, contrTopScaleVal do sum = sum + contrCoocMatrix[i,j] *  ((i+1)*(i+1) + 2*(i+1)*(j+1) + (j+1)*(j+1))
          (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[objNumber] = sum
          contrCoocMatrix = contrCoocMatrix * 0
         endif

          ; Inverse Moment Difference
         if whParamActive[2] then begin
              ; fill coocurrence Matrix with Image-Data
          if (entrMaxScaleInt eq -1) then intImage[whereMask] = (s_scaleArray(objArray.intValues, minValue = min(objArray.intValues), maxValue = max(objArray.intValues), topValue = entrTopScaleVal))[whereMask] $
              else intImage[whereMask] = (s_scaleArray(objArray.intValues, minValue = 0, maxValue = entrMaxScaleInt, topValue = entrTopScaleVal))[whereMask]
          expandImage = s_Expand_Mirror(intImage, invPixRad, /nonMirror)
          if (invMedian ne 0) then expandImage = median(expandImage, round(2*invMedian+1))
          for k = -invPixRad, invPixRad do for l = -invPixRad, invPixRad do begin
              pixDist = 1.0 * (k*k + l*l)
              if (sqrt(pixDist) le invPixRad) then begin
                 for i = 0, invTopScaleVal do begin
                   whereI = where(intImage[whereMask] eq i)
                   if (whereI[0] ne -1) then begin
                    for j = 0, invTopScaleVal do begin
                        whereJ = where(((expandImage[ invPixRad+k : invPixRad+k+dimImage[0]-1,  invPixRad+l : invPixRad+l+dimImage[1]-1 ])[whereMask])[whereI] eq j)
                        if (whereJ[0] ne -1) then invCoocMatrix[i,j] = invCoocMatrix[i,j] + n_elements(whereJ)
                    endfor
                   endif
                 endfor
              endif
          endfor
              ; normalize  coocurrence Matrix
          invCoocMatrix = invCoocMatrix / ( (1.*total(invCoocMatrix)) > 1.)
          sum = 0.
          for i = 0, invTopScaleVal do for j = 0, invTopScaleVal do sum = sum + invCoocMatrix[i,j] /  (1. + ((i+1)*(i+1) + 2*(i+1)*(j+1) + (j+1)*(j+1)) )
          (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[objNumber] = sum
          invCoocMatrix = invCoocMatrix * 0
         endif

          ; 2nd Angular Moment
         if whParamActive[3] then begin
              ; fill coocurrence Matrix with Image-Data
          if (entrMaxScaleInt eq -1) then intImage[whereMask] = (s_scaleArray(objArray.intValues, minValue = min(objArray.intValues), maxValue = max(objArray.intValues), topValue = entrTopScaleVal))[whereMask] $
              else intImage[whereMask] = (s_scaleArray(objArray.intValues, minValue = 0, maxValue = entrMaxScaleInt, topValue = entrTopScaleVal))[whereMask]
          expandImage = s_Expand_Mirror(intImage, scndPixRad, /nonMirror)
          if (scndMedian ne 0) then expandImage = median(expandImage, round(2*scndMedian+1))
          for k = -scndPixRad, scndPixRad do for l = -scndPixRad, scndPixRad do begin
              pixDist = 1.0 * (k*k + l*l)
              if (sqrt(pixDist) le scndPixRad) then begin
                 for i = 0, scndTopScaleVal do begin
                   whereI = where(intImage[whereMask] eq i)
                   if (whereI[0] ne -1) then begin
                    for j = 0, scndTopScaleVal do begin
                        whereJ = where(((expandImage[ scndPixRad+k : scndPixRad+k+dimImage[0]-1,  scndPixRad+l : scndPixRad+l+dimImage[1]-1 ])[whereMask])[whereI] eq j)
                        if (whereJ[0] ne -1) then scndCoocMatrix[i,j] = scndCoocMatrix[i,j] + n_elements(whereJ)
                    endfor
                   endif
                 endfor
              endif
          endfor
              ; normalize  coocurrence Matrix
          scndCoocMatrix = scndCoocMatrix / ( (1.*total(scndCoocMatrix)) > 1.)
          sum = 0.
          for i = 0, scndTopScaleVal do for j = 0, scndTopScaleVal do sum = sum + scndCoocMatrix[i,j] * scndCoocMatrix[i,j]
          (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[objNumber] = sum
          scndCoocMatrix = scndCoocMatrix * 0
         endif
       endfor;     objNumber
    endelse
end


function C_sROIParam_ObjTextureHaralick::init

    ROIParamStruct = {name:'Object Haralicks Texture Features',$       ;  ROI Name.
                    type:   'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect: ptr_new()   }     ; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(5))
    ROIParamWidgetType = make_array(5, /string, value = 'widget_slider')
    ROIParamNames = ['Entropy',$
                    'Contrast',$
                    'Inverse Moment Difference',$
                    '2nd Angular Moment',$
                    'Entropy div MaxE' ]

    ROIParamActive = [1,0,0,0,0]
    ROIParamMin = [0,0,0,0,0]
    ROIParamMax = [1,1,1,1,1]
    ROIParamValues = [0,0,0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name: 'Entropy',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType =make_array(11, /string, value = 'widget_slider')
    ROIValueNames = ['Maximum Scale Intensity', 'Top Scale Value',$
                        'Pixel Radius', 'Median Value',$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [0,1,1,0,$
                        0,0,$
                        0,0,$
                        0,0,$
                        0,0 ]
    ROIValueMin = [1, 1., 1., 0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0. ]
    ROIValueMax = [4096., 100., 30., 10.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1. ]
    ROIValueValues =[255., 10., 2., 1,$
                        0.,1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]

    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Contrast',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ROIValueWidgetType]
    ROIValueNames = [ROIValueNames]
    ROIValueActive = [ROIValueActive]
    ROIValueMin = [ROIValueMin]
    ROIValueMax = [ROIValueMax]
    ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Inverse Moment Difference',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ROIValueWidgetType]
    ROIValueNames = [ROIValueNames]
    ROIValueActive = [ROIValueActive]
    ROIValueMin = [ROIValueMin]
    ROIValueMax = [ROIValueMax]
    ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: '2nd Angular Moment',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ROIValueWidgetType]
    ROIValueNames = [ROIValueNames]
    ROIValueActive = [ROIValueActive]
    ROIValueMin = [ROIValueMin]
    ROIValueMax = [ROIValueMax]
    ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Entropy div MaxE',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ROIValueWidgetType[3:*]]
    ROIValueNames = [ROIValueNames[3:*]]
    ROIValueActive = [ROIValueActive[3:*]]
    ROIValueMin = [ROIValueMin[3:*]]
    ROIValueMax = [ROIValueMax[3:*]]
    ROIValueValues =[ROIValueValues[3:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[4] = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_ObjTextureHaralick__define
   tmp = {C_sROIParam_ObjTextureHaralick, pParamStruct: ptr_new(),$
                                  pValueStruct: ptr_new(),$
                                  inherits C_sROIParam }
end