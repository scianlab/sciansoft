;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjChannelScale
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjChannelScale' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjChannelScale::apply,  C_sROIGroupObj = C_sROIGroupObj, position = position

    whParam = [ (where( *(*self.pParamStruct).pNames eq 'Linear Scale [dI]'))[0] ]

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

    nObjects = C_sROIGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin

         ; Linear Scale
       if whParamActive[0] then begin
         minMaxScales = [-1, -1]
         if ((*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Minimum Intensity'))[0]]) then $
          minMaxScales[0] = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Minimum Intensity'))[0]]
         if ((*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Maximum Intensity'))[0]]) then $
          minMaxScales[1] = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Maximum Intensity'))[0]]

         intensityMask = C_sROIGroupObj->getGroupMaskIntensity()
         C_sROIGroupObj->rescaleGroupObjIntensities, minMaxScales = minMaxScales

          ; set Object Number Vector
         *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

          ; Object-Loop
         intVect = fltArr(nObjects)
         for i = 0, nObjects-1 do $
          intVect[i] = total(  *((C_sROIGroupObj->get(position =i))->getpPointValues()) - $
                              intensityMask[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] )
       (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new(intVect, /no_copy)
       endif

    endelse
end


function C_sROIParam_ObjChannelScale::init

    ROIParamStruct = {name:'Object Channel Scale',$     ;  ROI Name.
                    type:   'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect: ptr_new()   }     ; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(1))
    ROIParamWidgetType = make_array(1, /string, value = 'widget_slider')
    ROIParamNames = [ 'Linear Scale [dI]']
    ROIParamActive = [1]
    ROIParamMin = [0]
    ROIParamMax = [0]
    ROIParamValues = [0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name: 'Linear Scale [dI]',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIThresValueWidgetType = make_array(10, /string, value = 'widget_slider')
    ROIThresValueNames = ['Minimum Intensity', 'Maximum Intensity',$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIThresValueActive = [0,0,$
                        0,0,$
                        0,0,$
                        0,0,$
                        0,0 ]
    ROIThresValueMin = [0., 0.,$
                        0,0,$
                        0.,0.,$
                        0.,0.,$
                        0.,0. ]
    ROIThresValueMax = [1., 1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1. ]
    ROIThresValueValues =[0., 1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIThresValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIThresValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIThresValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIThresValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIThresValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIThresValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end

pro C_sROIParam_ObjChannelScale__define
   tmp = {C_sROIParam_ObjChannelScale, pParamStruct: ptr_new(),$
                                                      pValueStruct: ptr_new(),$
                                                      inherits C_sROIParam }
end