;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjSize
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjSize' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjSize::apply, C_sROIGroupObj = C_sROIGroupObj, position = position

   whParam = [(where( *(*self.pParamStruct).pNames eq 'Object Size [x²]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object Size [Pixel²]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object Size [%]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object Phase [x²]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object Phase [Pixel²]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object Phase [%]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object Phase [Mol%]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Background Phase [x²]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Background Phase [Pixel²]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Background Phase [%]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Background Phase [Mol%]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object X Real Size [x]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object Y Real Size [y]'))[0]]

       ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1:if (position[0] eq -1) then return else whParamActive[position] = 1
       else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase

       ; check Pointers
    wherePA = where(whParamActive eq 1)
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROIGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin

         ; set Object Number Vector
       *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

         ; Object-Loop for the determination of object size [pixel]
       paramVect = fltArr(nObjects)
       for i = 0, nObjects-1 do paramVect[i] = n_elements( *((C_sROIGroupObj->get(position = i))->getpWherePoints()) )

         ; get xy-Pixel Size
       pParamStruct = C_sROIGroupObj->getpParamStruct()
       if not((*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'x-Size per Pixel'))[0]]) then begin
          xPixSize = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
          (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'x-Size per Pixel'))[0]] = xPixSize
       endif else xPixSize = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'x-Size per Pixel'))[0]]
       (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'x-Size per Pixel'))[0]] = xPixSize

       if not((*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'y-Size per Pixel'))[0]]) then begin
          yPixSize = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
          (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'y-Size per Pixel'))[0]] = yPixSize
       endif else yPixSize = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'y-Size per Pixel'))[0]]
       (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'y-Size per Pixel'))[0]] = yPixSize

       pixArea =  *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]] *  *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]

         ; object size [x²]
       if whParamActive[0] then *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = paramVect * (xPixSize*yPixSize)

         ; object size [pixel²]
       if whParamActive[1] then *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = paramVect

         ; object size [%]
       if whParamActive[2] then *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = paramVect / (pixArea/100.)

         ; object phase [x²]
       if whParamActive[3] then *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = fltArr(nObjects) + total(paramVect) * (xPixSize*yPixSize)
       print, 'object phase [x²]', (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[0]

         ; object phase [pixel²]
       if whParamActive[4] then *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = fltArr(nObjects) + total(paramVect)

         ; object phase [%]
       if whParamActive[5] then *(*(*self.pValueStruct)[whParam[5]]).pROIParamVect = fltArr(nObjects) + total(paramVect) / (pixArea/100.)

         ; object phase [Mol%] or background phase [Mol%]
       if (whParamActive[6] or whParamActive[6]) then begin

            ; get lipid area
          lipidArea1 = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Area/Lipid_1 [Ang²]'))[0]]
          lipidArea2 = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Area/Lipid_2 [Ang²]'))[0]]

          numLipid1 = 1.e8 * (total(paramVect) * (xPixSize*yPixSize)) / lipidArea1
          numLipid2 = 1.e8 * ((pixArea - total(paramVect)) * (xPixSize*yPixSize)) / lipidArea2
          percLipid1 = 100. * numLipid1 / (numLipid1+numLipid2)

            ; object phase [Mol%]
          if whParamActive[6] then *(*(*self.pValueStruct)[whParam[6]]).pROIParamVect = fltArr(nObjects) + 100. * numLipid1 / (numLipid1+numLipid2)

            ; background phase [Mol%]
          if whParamActive[10] then *(*(*self.pValueStruct)[whParam[10]]).pROIParamVect = fltArr(nObjects) + 100. * numLipid2 / (numLipid1+numLipid2)

       endif

         ; background phase [x²]
       if whParamActive[7] then *(*(*self.pValueStruct)[whParam[7]]).pROIParamVect = fltArr(nObjects) + (pixArea - total(paramVect)) * (xPixSize*yPixSize)

         ; background phase [pixel²]
       if whParamActive[8] then *(*(*self.pValueStruct)[whParam[8]]).pROIParamVect = fltArr(nObjects) + pixArea - total(paramVect)

         ; background phase [%]
       if whParamActive[9] then *(*(*self.pValueStruct)[whParam[9]]).pROIParamVect = fltArr(nObjects) + 100. - total(paramVect) / (pixArea/100.)

         ; MOR - add real sizes of x and y pixels to the object size ROI parameter in order to have them be part of the ROI Stack Track - BEGIN
         ; x per pixel real size[x]
       if whParamActive[10] then *(*(*self.pValueStruct)[whParam[10]]).pROIParamVect = xPixSize

         ; y per pixel real size[y]
        if whParamActive[11] then *(*(*self.pValueStruct)[whParam[11]]).pROIParamVect = yPixSize
       ; MOR - add real sizes of x and y pixels to the object size ROI parameter in order to have them be part of the ROI Stack Track - END
   endelse
end


function C_sROIParam_ObjSize::init

    ROIParamStruct = {name:'Object Size',$   ;  ROI Name.
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

;    self.pValueStruct = ptr_new(ptrArr(11))
;    ROIParamWidgetType = make_array(11, /string, value = 'widget_slider')
;    ROIParamNames = ['Object Size [x²]',$
;                     'Object Size [Pixel²]',$
;                     'Object Size [%]',$
;                     'Object Phase [x²]',$
;                     'Object Phase [Pixel²]',$
;                     'Object Phase [%]',$
;                     'Object Phase [Mol%]',$
;                     'Background Phase [x²]',$
;                     'Background Phase [Pixel²]',$
;                     'Background Phase [%]',$
;                     'Background Phase [Mol%]']
;    ROIParamActive = [1,1,1,1,1,1,1,1,1,1,1]
;    ROIParamMin = [0,0,0,0,0,0,0,0,0,0,0]
;    ROIParamMax = [0,0,0,0,0,0,0,0,0,0,0]
;    ROIParamValues = [0,0,0,0,0,0,0,0,0,0,0]

    self.pValueStruct = ptr_new(ptrArr(13))
    ROIParamWidgetType = make_array(13, /string, value = 'widget_slider')
    ROIParamNames = ['Object Size [x²]',$
                     'Object Size [Pixel²]',$
                     'Object Size [%]',$
                     'Object Phase [x²]',$
                     'Object Phase [Pixel²]',$
                     'Object Phase [%]',$
                     'Object Phase [Mol%]',$
                     'Background Phase [x²]',$
                     'Background Phase [Pixel²]',$
                     'Background Phase [%]',$
                     'Background Phase [Mol%]',$
; MOR - adding a separate sub-parameter to be added to the stack track for the x, y real pixel size                     
                     'Object X Real Size [x]',$
                     'Object Y Real Size [y]']
    ROIParamActive = [1,1,1,1,1,1,1,1,1,1,1,1,1]
    ROIParamMin = [0,0,0,0,0,0,0,0,0,0,0,0,0]
    ROIParamMax = [0,0,0,0,0,0,0,0,0,0,0,0,0]
    ROIParamValues = [0,0,0,0,0,0,0,0,0,0,0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:'Object Size [x²]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(12, /string, value = 'widget_slider')
    ROIValueNames = ['x-Size per Pixel', 'y-Size per Pixel',$
                     'Area/Lipid_1 [Ang²]', 'Area/Lipid_2 [Ang²]',$
                     'Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [0,0,1,1, make_array(8, /byte, value = 0)]
    ROIValueMin = make_array(12, /float, value = 0)
    ROIValueMax = [1000.,1000., 500., 500., make_array(8, /float, value = 1)]
    ROIValueValues =[1.,1.,$
                     51.,84.,$
                     0.,1.,$
                     0.,1.,$
                     0.,1.,$
                     0.,1.]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Size [Pixel²]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ROIValueWidgetType[4:*]]
    ROIValueNames = [ROIValueNames[4:*]]
    ROIValueActive = [ROIValueActive[4:*]]
    ROIValueMin = [ROIValueMin[4:*]]
    ROIValueMax = [ROIValueMax[4:*]]
    ROIValueValues =[ROIValueValues[4:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Size [%]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Phase [x²]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Phase [Pixel²]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[4] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Phase [%]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[5] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Phase [Mol%]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[6] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Background Phase [x²]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[7] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Background Phase [Pixel²]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[8] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Background Phase [%]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[9] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Background Phase [Mol%]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[10] = ptr_new(ROIValueStruct, /no_copy)
    
    ; MOR - added separate sub-ROI parameters for the object x and y per pixel size - BEGIN
    ROIValueStruct = {name:'Object X Real Size [x]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[11] = ptr_new(ROIValueStruct, /no_copy)
    
        ROIValueStruct = {name:'Object Y Real Size [y]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[12] = ptr_new(ROIValueStruct, /no_copy)
; MOR - added separate sub-ROI parameters for the object x and y per pixel size - BEGIN    
    
    return, 1
end

pro C_sROIParam_ObjSize__define
   tmp = {C_sROIParam_ObjSize, pParamStruct:ptr_new(),$
                                  pValueStruct:ptr_new(),$
                                  inherits C_sROIParam}
end