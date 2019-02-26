;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjTextureSimple
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjTextureSimple' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjTextureSimple::apply, C_sROIGroupObj = C_sROIGroupObj, position = position

    whParam = [ (where( *(*self.pParamStruct).pNames eq 'Simple Entropy'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Simple Contrast'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Simple Inverse Moment Difference'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Simple 2nd Angular Moment'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Simple Entropy div A'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Simple Contrast div A'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Simple Inverse Moment Difference div A'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Simple 2nd Angular Moment div A'))[0] ]

     ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1: if (position[0] eq -1) then return else  whParamActive[position] = 1
       else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase
    wherePA = where(whParamActive eq 1)

       ; check Pointers
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    mask = C_sROIGroupObj->getGroupMask()
    hmax = max(mask)
    if (hmax eq 0) then begin
       (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin

         ; set Object Number Vector
       (*self.pParamStruct).pROINumberVect = ptr_new( (C_sROIGroupObj->getObjectNumberVector()) , /no_copy)

       intensityMask = C_sROIGroupObj->getGroupMaskIntensity() * (mask gt 0)

       wherePixRad = (where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Pixel Radius'))[0]
       maxPixRad = 0
       for i = 0, 3 do if (whParamActive[i]) then maxPixRad = maxPixRad > (*(*(*self.pValueStruct)[whParam[i]]).pValues)[wherePixRad]

       dimImage = size(intensityMask, /dim)
       expandImage = 1. * s_Expand_Mirror(intensityMask, maxPixRad)

       if (whParamActive[0] or whParamActive[4]) then entrImage = fltArr(dimImage[0], dimImage[1])
       if (whParamActive[1] or whParamActive[5]) then contrastImage = fltArr(dimImage[0], dimImage[1])
       if (whParamActive[2] or whParamActive[6]) then invImage = fltArr(dimImage[0], dimImage[1])
       if (whParamActive[3] or whParamActive[7]) then scndImage = fltArr(dimImage[0], dimImage[1])

       for k = -maxPixRad, maxPixRad do for l = -maxPixRad, maxPixRad do begin
         pixDist = 1.0 * (k*k + l*l)
         intDist = abs( expandImage( maxPixRad+k : maxPixRad+k+dimImage[0]-1,  maxPixRad+l : maxPixRad+l+dimImage[1]-1 ) - intensityMask )
         if (sqrt(pixDist) le maxPixRad) then begin
          if (whParamActive[0] or whParamActive[4]) then if (sqrt(pixDist) le maxPixRad) then entrImage = temporary(entrImage) + intDist * aLog(intDist > 1)
          if (whParamActive[1] or whParamActive[5]) then if (sqrt(pixDist) le maxPixRad) then contrastImage = temporary(contrastImage) + intDist *  (pixDist + 2. * k * l)
          if (whParamActive[2] or whParamActive[6]) then if (sqrt(pixDist) le maxPixRad) then invImage = temporary(invImage) + intDist / (1. + pixDist + 2. * k * l)
          if (whParamActive[3] or whParamActive[7]) then if (sqrt(pixDist) le maxPixRad) then scndImage = temporary(scndImage) + intDist * intDist
         endif
       endfor
       hmin = min(mask[where(mask ne 0)])
       hist = histogram(mask, min = hmin, max = hmax, reverse_indices = revInd )
       histInt = hist * 0.
       whereHist = where(hist ne 0)
       if whParamActive[0] then begin
         for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(entrImage[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
          *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = 1.*histInt[whereHist]/hist[whereHist]
       endif
       if whParamActive[1] then begin
         for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(contrastImage[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
         *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = 1.*histInt[whereHist]/hist[whereHist]
       endif
       if whParamActive[2] then begin
         for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(invImage[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
         *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = 1.*histInt[whereHist]/hist[whereHist]
       endif
       if whParamActive[3] then begin
         for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(scndImage[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
         *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = 1.*histInt[whereHist]/hist[whereHist]
       endif
       if whParamActive[4] then begin
         for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(entrImage[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
          *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = 1.*histInt[whereHist]/(hist[whereHist]*hist[whereHist])
       endif
       if whParamActive[5] then begin
         for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(contrastImage[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
         *(*(*self.pValueStruct)[whParam[5]]).pROIParamVect = 1.*histInt[whereHist]/(hist[whereHist]*hist[whereHist])
       endif
       if whParamActive[6] then begin
         for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(invImage[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
         *(*(*self.pValueStruct)[whParam[6]]).pROIParamVect = 1.*histInt[whereHist]/(hist[whereHist]*hist[whereHist])
       endif
       if whParamActive[7] then begin
         for i = 0, n_elements(whereHist)-1 do histInt[whereHist[i]] = total(scndImage[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]])
         *(*(*self.pValueStruct)[whParam[7]]).pROIParamVect = 1.*histInt[whereHist]/(hist[whereHist]*hist[whereHist])
       endif
    endelse
end


function C_sROIParam_ObjTextureSimple::init

    ROIParamStruct = {name:'Object Simple Texture Features',$     ;  ROI Name.
                    type:   'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect: ptr_new()   }     ; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(8))
    ROIParamWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIParamNames = ['Simple Entropy',$
                    'Simple Contrast',$
                    'Simple Inverse Moment Difference',$
                    'Simple 2nd Angular Moment' ,$
                    'Simple Entropy div A',$
                    'Simple Contrast div A',$
                    'Simple Inverse Moment Difference div A',$
                    'Simple 2nd Angular Moment div A' ]

    ROIParamActive = [1,1,1,1,0,0,0,0]
    ROIParamMin = [0,0,0,0,0,0,0,0]
    ROIParamMax = [0,0,0,0,0,0,0,0]
    ROIParamValues = [0,0,0,0,0,0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name: 'Simple Entropy',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(10, /string, value = 'widget_slider')
    ROIValueNames = ['Pixel Radius', 'Intensity Intervall',$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [1,1,$
                        0,0,$
                        0,0,$
                        0,0,$
                        0,0 ]
    ROIValueMin = [1., 1.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0. ]
    ROIValueMax = [30., 100.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1. ]
    ROIValueValues =[2., 25.,$
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

    ROIValueStruct = {name: 'Simple Contrast',$
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

    ROIValueStruct = {name: 'Simple Inverse Moment Difference',$
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

    ROIValueStruct = {name: 'Simple 2nd Angular Moment',$
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

    ROIValueStruct = {name: 'Simple Entropy div A',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.


    ROIValueWidgetType = [ROIValueWidgetType[2:*]]
    ROIValueNames = [ROIValueNames[2:*]]
    ROIValueActive = [ROIValueActive[2:*]]
    ROIValueMin = [ROIValueMin[2:*]]
    ROIValueMax = [ROIValueMax[2:*]]
    ROIValueValues =[ROIValueValues[2:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[4] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Simple Contrast div A',$
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

    (*self.pValueStruct)[5] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Simple Inverse Moment Difference div A',$
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

    (*self.pValueStruct)[6] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Simple 2nd Angular Moment div A',$
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
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[7] = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_ObjTextureSimple__define
   tmp = {C_sROIParam_ObjTextureSimple, pParamStruct: ptr_new(),$
                                  pValueStruct: ptr_new(),$
                                  inherits C_sROIParam }
end