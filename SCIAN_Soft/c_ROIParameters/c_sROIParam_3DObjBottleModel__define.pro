;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjBottleModel
;
; PURPOSE:
;       - Calculation of 3D Object Box Model
;
; AUTHOR:
;     Dr. Steffen Härtel (2007)
;     e_mail:shartel@med.uchile.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjBottleModel' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_3DObjBottleModel::apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position

   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do  whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

       ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1: if (position[0] eq -1) then return else  whParamActive[position] = 1
      else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase

      ; check Pointers
   wherePA = where(whParamActive eq 1)
   if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

   nObjects = C_sROI3DGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

        ; set Object Number Vector
      (*self.pParamStruct).pROINumberVect = ptr_new(C_sROI3DGroupObj->getObjectNumberVector(), /no_copy)

        ; set Object Parameter Vectors
      for i = 0, n_elements(whParam)-1 do if (whParamActive[i]) then *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float, value = -1.)

      for i = 0, nObjects-1 do begin

            ; bottle Coordinates
         evProj = (C_sROI3DGroupObj->get(position = i))->getEVSProj(fCalcBottleNeck = 1b)

         paramVect = evProj.bottleMassRatio
         for j = 0,2 do if (paramVect[j] gt 1.) then paramVect[j] = 1. / paramVect[j]  
            ; '3D Box Bottle Mass Ratio1'
         if whParamActive[0] then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = 1. - paramVect[0]

            ; '3D Box Bottle Mass Ratio2'
         if whParamActive[1] then (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[i] = 1. - paramVect[1]

            ; '3D Box Bottle Mass Ratio3'
         if whParamActive[2] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = 1. - paramVect[2]

         paramVect = evProj.bottleMassRatio_3
         for j = 0,2 do if (paramVect[j] gt 1.) then paramVect[j] = 1. / paramVect[j]  
            ; '3D Box Bottle Mass Ratio1'
         if whParamActive[3] then (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[i] = 1. - paramVect[0]

            ; '3D Box Bottle Mass Ratio2'
         if whParamActive[4] then (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[i] = 1. - paramVect[1]

            ; '3D Box Bottle Mass Ratio3'
         if whParamActive[5] then (*(*(*self.pValueStruct)[whParam[5]]).pROIParamVect)[i] = 1. - paramVect[2]
         
         paramVect = evProj.bottleAxisRatio
         for j = 0,2 do if (paramVect[j] gt 1.) then paramVect[j] = 1. / paramVect[j]
            ; '3D Box Bottle Axis Ratio1'
         if whParamActive[6] then (*(*(*self.pValueStruct)[whParam[6]]).pROIParamVect)[i] = 1. - paramVect[0]

            ; '3D Box Bottle Axis Ratio2'
         if whParamActive[7] then (*(*(*self.pValueStruct)[whParam[7]]).pROIParamVect)[i] = 1. - paramVect[1]

            ; '3D Box Bottle Axis Ratio3'
         if whParamActive[8] then (*(*(*self.pValueStruct)[whParam[8]]).pROIParamVect)[i] = 1. - paramVect[2]
      endfor
   print, "aqui", (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)
   endelse
end


function C_sROIParam_3DObjBottleModel::init

   ROIParamStruct = {name:'3D Object Bottle',$     ;  ROI Name.
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

    nParams = 9
    self.pValueStruct = ptr_new(ptrArr(nParams))
    ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
    ROIParamNames = ['3D Box Bottle Mass Ratio1',$
                     '3D Box Bottle Mass Ratio2',$
                     '3D Box Bottle Mass Ratio3',$
                     '3D Box Bottle Mass Ratio_31',$
                     '3D Box Bottle Mass Ratio_32',$
                     '3D Box Bottle Mass Ratio_33',$
                     '3D Box Bottle Axis Ratio1',$
                     '3D Box Bottle Axis Ratio2',$
                     '3D Box Bottle Axis Ratio3']

    ROIParamActive = make_array(nParams, /byte, value = 1b)
    ROIParamMin = make_array(nParams, /float, value = 0.)
    ROIParamMax = make_array(nParams, /float, value = 1.)
    ROIParamValues = make_array(nParams, /float, value = 0.)
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
                      type:'3D ROI-Parameter-Method',$
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
    ROIValueActive = [0,0,0,0,0,0,0,0]
    ROIValueMin = [0.,0.,0.,0.,0.,0.,0.,0.]
    ROIValueMax = [1.,1.,1.,1.,1.,1.,1.,1.]
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
                       type:'3D ROI-Parameter-Method',$
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
                    type:'3D ROI-Parameter-Method',$
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

pro C_sROIParam_3DObjBottleModel__define
   tmp = {C_sROIParam_3DObjBottleModel, pParamStruct:ptr_new(), pValueStruct:ptr_new(), inherits C_sROIParam}
end