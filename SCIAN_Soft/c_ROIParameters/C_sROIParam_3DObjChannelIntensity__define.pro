;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjChannelIntensity
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjChannelIntensity' )
;
; METHOHDS:
;_____________________________IOISIOI____________________


pro C_sROIParam_3DObjChannelIntensity::apply, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position

    whParam = [(where( *(*self.pParamStruct).pNames eq '3D Intensity [I]'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq '3D I-Density [I-Voxel]'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq '3D I-Density [I-dx³]'))[0]]

       ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1:if (position[0] eq -1) then return else  whParamActive[position] = 1
       else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase
    wherePA = where(whParamActive eq 1)

       ; check Pointers
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROI3DGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin

         ; set Object Number Vector
       *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()

       if whParamActive[2] then begin
         pParamStruct = C_sROI3DGroupObj->getpParamStruct()
         xSizePerPixel = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
         ySizePerPixel = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
         zSizePerPixel = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Size [pixel]'))[0]]
         (*(*(*self.pValueStruct)[whParam[2]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'x-Size per Pixel'))[0]] = xSizePerPixel
         (*(*(*self.pValueStruct)[whParam[2]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'y-Size per Pixel'))[0]] = ySizePerPixel
         (*(*(*self.pValueStruct)[whParam[2]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'z-Size per Pixel'))[0]] = zSizePerPixel
       endif

         ; Object-Loop
       pointVect = fltArr(nObjects)
       intVect = fltArr(nObjects)
       for i = 0, nObjects-1 do begin
         pointVect[i] = n_elements( *((C_sROI3DGroupObj->get(position = i))->getpWherePoints()) )
         intVect[i] = total( *((C_sROI3DGroupObj->get(position = i))->getpPointValues()) )
       endfor

         ; 3D Intensity [I]
       if whParamActive[0] then *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = intVect
         ; 3D I-Density [I-Voxel]
       if whParamActive[1] then *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = intVect / (pointVect>0.)
         ; 3D I-Density [I-x³]
       if whParamActive[2] then *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = intVect / ((pointVect*xSizePerPixel*ySizePerPixel*zSizePerPixel)>0.)
    endelse
end


function C_sROIParam_3DObjChannelIntensity::init

    ROIParamStruct = {name:'3D Object Channel Intensity',$   ;  ROI Name.
                      type:'3D ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(3))
    ROIParamWidgetType = make_array(3, /string, value = 'widget_slider')
    ROIParamNames = ['3D Intensity [I]',$
                     '3D I-Density [I-Voxel]',$
                     '3D I-Density [I-dx³]']
    ROIParamActive = [1,0,0]
    ROIParamMin = [0,0,0]
    ROIParamMax = [0,0,0]
    ROIParamValues = [0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:'3D Intensity [I]',$
                      type:'3D ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [0,0,$
                        0,0,$
                        0,0,$
                        0,0 ]
    ROIValueMin = [0.,0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0. ]
    ROIValueMax = [1.,1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1. ]
    ROIValueValues =[0.,1.,$
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
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'3D I-Density [I-Voxel]',$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

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
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'3D I-Density [I-x³]',$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ['widget_slider', 'widget_slider', 'widget_slider', ROIValueWidgetType]
    ROIValueNames = ['x-Size per Pixel', 'y-Size per Pixel',  'z-Size per Pixel',ROIValueNames]
    ROIValueActive = [1, 1, 1, ROIValueActive]
    ROIValueMin = [0., 0., 0., ROIValueMin]
    ROIValueMax = [1000., 1000., 1000., ROIValueMax]
    ROIValueValues =[1., 1., 1., ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end

pro C_sROIParam_3DObjChannelIntensity__define
   tmp = {C_sROIParam_3DObjChannelIntensity, pParamStruct:ptr_new(),$
                                                         pValueStruct:ptr_new(),$
                                                         inherits C_sROIParam}
end