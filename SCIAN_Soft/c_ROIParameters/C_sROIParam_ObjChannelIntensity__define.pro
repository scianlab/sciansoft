;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjChannelIntensity
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjChannelIntensity' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjChannelIntensity::apply, C_sROIGroupObj = C_sROIGroupObj, position = position

   whParam = [(where( *(*self.pParamStruct).pNames eq 'Object Intensity [I]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object I-Density [I-Pixel]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object I-Density [I-x²]'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object I-Variance'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object I-Skewness'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object I-Kurtosis'))[0],$
              (where( *(*self.pParamStruct).pNames eq 'Object I-Mean Absolute Deviation'))[0]]

     ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1:if (position[0] eq -1) then return else  whParamActive[position] = 1
      else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase
   whPA = where(whParamActive eq 1)

       ; check Pointers
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
    if (whPA[0] eq -1) then return
    for i = 0, n_elements(whPA)-1 do if not(ptr_valid((*(*self.pValueStruct)[whPA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[whPA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROIGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(whPA)-1 do *(*(*self.pValueStruct)[whParam[whPA[i]]]).pROIParamVect = -1
    endif else begin

         ; set Object Number Vector
       *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

       if whParamActive[2] then begin
         pParamStruct = C_sROIGroupObj->getpParamStruct()
         xSizePerPixel = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
         ySizePerPixel = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
       endif

         ; Object-Loop
       intVect = fltArr(nObjects)
       fActive = (total(whParamActive[1:*]) gt 0)
       if (fActive) then momentArray = fltArr(5, nObjects)
       for i = 0, nObjects-1 do begin
         intVect[i] = total(*((C_sROIGroupObj->get(position = i))->getpPointValues()))
         if (fActive and (n_elements(*((C_sROIGroupObj->get(position = i))->getpPointValues())) gt 2)) then begin
          momentArray[0:3,i] = (moment(*((C_sROIGroupObj->get(position = i))->getpPointValues()), mDev = mDev, /nan))[0:3]
          momentArray[4,i] = mDev
         endif
       endfor

         ; Channel Intensity [I]
       if whParamActive[0] then *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = intVect
         ; Channel I-Density [I-Voxel]
       if whParamActive[1] then *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = fltArr(nObjects) + momentArray[0,*]
         ; Channel I-Density [I-x²]
       if whParamActive[2] then *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = (fltArr(nObjects) + momentArray[0,*]) / (xSizePerPixel * ySizePerPixel)
         ; Channel I-Variance
       if whParamActive[3] then *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = fltArr(nObjects) + momentArray[1,*]
         ; Channel I-Skewness
       if whParamActive[4] then *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = fltArr(nObjects) + momentArray[2,*]
         ; Channel I-Kurtosis
       if whParamActive[5] then *(*(*self.pValueStruct)[whParam[5]]).pROIParamVect = fltArr(nObjects) + momentArray[3,*]
         ; Channel I-Mean Absolute Deviation
       if whParamActive[6] then *(*(*self.pValueStruct)[whParam[6]]).pROIParamVect = fltArr(nObjects) + momentArray[4,*]
   endelse
end


function C_sROIParam_ObjChannelIntensity::init

   ROIParamStruct = {name:'Object Channel Intensity',$     ;  ROI Name.
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                     pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

   nParams = 7
   self.pValueStruct = ptr_new(ptrArr(nParams))
   ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
   ROIParamNames = ['Object Intensity [I]',$
                    'Object I-Density [I-Pixel]',$
                    'Object I-Density [I-x²]',$
                    'Object I-Variance',$
                    'Object I-Skewness',$
                    'Object I-Kurtosis',$
                    'Object I-Mean Absolute Deviation']

   ROIParamActive = [1,0,0,0,0,0,0]
   ROIParamMin = [0,0,0,0,0,0,0]
   ROIParamMax = [1,1,1,1,1,1,1]
   ROIParamValues = [0,0,0,0,0,0,0]
   pROINumberVect = [-1]

   ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
   ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
   ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
   ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
   ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
   ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
   ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

   self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[0],$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$
                      pNames:ptr_new(),$
                      pActive:ptr_new(),$
                      pMin:ptr_new(),$
                      pMax:ptr_new(),$
                      pValues:ptr_new(),$
                      pROIParamVect:ptr_new()}

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [0,0,0,0,0,0,0,0 ]
    ROIValueMin = [0.,0.,0.,0.,0.,0.,0.,0.]
    ROIValueMax = [1.,1.,1.,1.,1.,1.,1.,1. ]
    ROIValueValues =[0.,1.,0.,1.,0.,1.,0.,1.]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    for i = 1, nParams-2 do begin
       ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[i],$
                         type:'Single ROI-Parameter-Method',$
                         pWidgetType:ptr_new(),$
                         pNames:ptr_new(),$
                         pActive:ptr_new(),$
                         pMin:ptr_new(),$
                         pMax:ptr_new(),$
                         pValues:ptr_new(),$
                         pROIParamVect:ptr_new()}

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

       (*self.pValueStruct)[i] = ptr_new(ROIValueStruct, /no_copy)
    endfor

    ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[nParams-1],$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$
                    pNames:ptr_new(),$
                    pActive:ptr_new(),$
                    pMin:ptr_new(),$
                    pMax:ptr_new(),$
                    pValues:ptr_new(),$
                    pROIParamVect:ptr_new()}

    ROIValueWidgetType = [ROIValueWidgetType]
    ROIValueNames = [ROIValueNames]
    ROIValueActive = [ROIValueActive]
    ROIValueMin = [ROIValueMin]
    ROIValueMax = [ROIValueMax]
    ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[nParams-1] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end

pro C_sROIParam_ObjChannelIntensity__define
   tmp = {C_sROIParam_ObjChannelIntensity, pParamStruct:ptr_new(),pValueStruct:ptr_new(),inherits C_sROIParam}
end