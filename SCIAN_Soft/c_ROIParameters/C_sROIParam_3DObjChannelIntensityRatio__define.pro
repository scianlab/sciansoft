; ____________________ (this is art) IOISIOI (this is art) ____________________
; NAME
;     C_sROIParam_3DObjChannelIntensityRatio
;
; PURPOSE
;     Calculation of JOINT Intensity ratio between ROIS at diferent Channels
;
; AUTHOR
;     Felipe Santibáñez-Leal (2013)
;     e_mail: fsantibanezleal@ug.uchile.cl
;
; CALLING SEQUENCE
;     oParam = obj_new('C_sROIParam_3DObjChannelIntensityRatio')
;
; METHOHDS
;     init, apply, getParamNames
;
; NOTES
;     Extension to 3D from C_sROIParam_ObjChannelIntensityRatio__define.pro.
;
; ____________________ (this is art) IOISIOI (this is art) ____________________

; DEVELOPING !!!!!!!!!!!!!!! DONT TOUCH XD
function C_sROIParam_3DObjChannelIntensityRatio::getParamNames
  return, [   'Object I-Ratio Ch0 div Ch1',$
              'Object I-Ratio Ch1 div Ch0',$
              'Object I-Ratio Ch0 div Ch2',$
              'Object I-Ratio Ch2 div Ch0',$
              'Object I-Ratio Ch1 div Ch2',$
              'Object I-Ratio Ch2 div Ch1',$
              'Max I-Ratio Ch0 div Ch1',$
              'Max I-Ratio Ch1 div Ch0',$
              'Max I-Ratio Ch0 div Ch2',$
              'Max I-Ratio Ch2 div Ch0',$
              'Max I-Ratio Ch1 div Ch2',$
              'Max I-Ratio Ch2 div Ch1',$
              'Top I-Ratio Ch0 div Ch1',$
              'Top I-Ratio Ch1 div Ch0',$
              'Top I-Ratio Ch0 div Ch2',$
              'Top I-Ratio Ch2 div Ch0',$
              'Top I-Ratio Ch1 div Ch2',$
              'Top I-Ratio Ch2 div Ch1',$
              'Mean/Mean I-Ratio Ch0 div Ch1',$
              'Mean/Mean I-Ratio Ch1 div Ch0',$
              'Mean/Mean I-Ratio Ch0 div Ch2',$
              'Mean/Mean I-Ratio Ch2 div Ch0',$
              'Mean/Mean I-Ratio Ch1 div Ch2',$
              'Mean/Mean I-Ratio Ch2 div Ch1']
              
end


pro C_sROIParam_3DObjChannelIntensityRatio::apply, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position, stack_tlb = stack_tlb

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
  pParamStruct = C_sROI3DGroupObj->getpParamStruct()

   if ptr_valid((*self.pParamStruct).pROINumberVect) then ptr_free, (*self.pParamStruct).pROINumberVect

   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do  whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

     ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1:if (position[0] eq -1) then return else  whParamActive[position] = 1
      else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase
   whPA = where(whParamActive eq 1)

      ; get Parameter
   topPercentage = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Top Percentage'))[0]]

       ; check Pointers
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
    if (whPA[0] eq -1) then return
    for i = 0, n_elements(whPA)-1 do if not(ptr_valid((*(*self.pValueStruct)[whPA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[whPA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

    nObjects = C_sROI3DGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(whPA)-1 do *(*(*self.pValueStruct)[whParam[whPA[i]]]).pROIParamVect = -1
    endif else begin

         ; set Object Number Vector
       *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()

         ; set Object Parameter Vectors
       for i = 0, n_elements(whParam)-1 do if (whParamActive[i]) then *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float, value = -1.)

       ; get original intensity volumes
        C_sROI3DGroupCh0 = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = tPos, chPos = 0, clusPos = clusPos)  
        C_sROI3DGroupCh1 = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = tPos, chPos = 1, clusPos = clusPos)
        C_sROI3DGroupCh2 = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = tPos, chPos = 2, clusPos = clusPos)                    
        
        intCh0 = C_sROI3DGroupCh0->getGroupMaskIntensity()
        intCh1 = C_sROI3DGroupCh1->getGroupMaskIntensity()
        intCh2 = C_sROI3DGroupCh2->getGroupMaskIntensity()

         ; Object-Loop
       for i = 0, nObjects-1 do begin
            ;FASL
            ;0-6 bad!!! 
            ; Somebody changed IT 
            ; 0-1
            ; mean(Data1)/mean/(Data2)
            ; 2-5
            ; mean(Data1/Data2)
            ; 6-11
            ; max(Data1/Data2)
            ; 12-17
            ; upperRatio = upper "X percentage" DAta1/Data2  
            ; mean(upperRatio)
            ;
            ;
                
             ; old change from JJ for Miss Alejandra...
                        ;            ; I-Ratio Ch0/Ch1
                        ;          ;Original
                        ;          ;if whParamActive[0] then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] / (intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] > 0.01))
                        ;          ;New
                        ;          if whParamActive[0] then begin
                        ;             meanCh1 = mean(intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])
                        ;             meanCh0 = mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])
                        ;             backgroundCh0 = 25.
                        ;             backgroundCh1 = 5.
                        ;             meanCh0 -= backgroundCh0
                        ;             meanCh1 -= backgroundCh1
                        ;             if meanCh1 eq 0 then meanCh1 = -0.01
                        ;             (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = meanCh0 / meanCh1
                        ;            ;(*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] / (intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] > 0.01))
                        ;          endif
                        ;            ; I-Ratio Ch1/Ch0
                        ;          ;Original
                        ;          ;if whParamActive[1] then (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[i] = mean(intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] / (intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
                        ;          ;New
                        ;          if whParamActive[1] then begin
                        ;             meanCh1 = mean(intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])
                        ;             meanCh0 = mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])
                        ;             backgroundCh0 = 25.
                        ;             backgroundCh1 = 5.
                        ;             meanCh0 -= backgroundCh0
                        ;             meanCh1 -= backgroundCh1
                        ;             if meanCh0 eq 0 then meanCh0 = -0.01
                        ;             (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[i] = meanCh1 / meanCh0
                        ;          endif

            ; I-Ratio Ch0/Ch1
          if whParamActive[2] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] / (intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; I-Ratio Ch1/Ch0
          if whParamActive[2] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = mean(intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] / (intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; I-Ratio Ch0/Ch2
          if whParamActive[2] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = mean(intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; I-Ratio Ch2/Ch0
          if whParamActive[3] then (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[i] = mean(intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; I-Ratio Ch1/Ch2
          if whParamActive[4] then (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[i] = mean(intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; I-Ratio Ch2/Ch1
          if whParamActive[5] then (*(*(*self.pValueStruct)[whParam[5]]).pROIParamVect)[i] = mean(intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; max I-Ratio Ch0/Ch1
          if whParamActive[6] then (*(*(*self.pValueStruct)[whParam[6]]).pROIParamVect)[i] = max(intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01))
            ; max I-Ratio Ch1/Ch0
          if whParamActive[7] then (*(*(*self.pValueStruct)[whParam[7]]).pROIParamVect)[i] = max(intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; max I-Ratio Ch0/Ch2
          if whParamActive[8] then (*(*(*self.pValueStruct)[whParam[8]]).pROIParamVect)[i] = max(intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; max I-Ratio Ch2/Ch0
          if whParamActive[9] then (*(*(*self.pValueStruct)[whParam[9]]).pROIParamVect)[i] = max(intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; max I-Ratio Ch1/Ch2
          if whParamActive[10] then (*(*(*self.pValueStruct)[whParam[10]]).pROIParamVect)[i] = max(intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
            ; max I-Ratio Ch2/Ch1
          if whParamActive[11] then (*(*(*self.pValueStruct)[whParam[11]]).pROIParamVect)[i] = max(intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)) 
          
          nBottomPoints = (round(n_elements(*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())) * ((100.-topPercentage)/100.)) - 1) > 0
            ; top I-Ratio Ch0/Ch1
          if whParamActive[12] then begin
             ratioVect = intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)
             ratioVect = ratioVect[sort(ratioVect)]
             (*(*(*self.pValueStruct)[whParam[12]]).pROIParamVect)[i] = mean(ratioVect[nBottomPoints:*])
          endif
            ; top I-Ratio Ch1/Ch0
          if whParamActive[13] then begin
             ratioVect = intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)
             ratioVect = ratioVect[sort(ratioVect)]
             (*(*(*self.pValueStruct)[whParam[13]]).pROIParamVect)[i] = mean(ratioVect[nBottomPoints:*]) 
            ; top I-Ratio Ch0/Ch2
          endif
          if whParamActive[14] then begin
             ratioVect = intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)
             ratioVect = ratioVect[sort(ratioVect)]
             (*(*(*self.pValueStruct)[whParam[14]]).pROIParamVect)[i] = mean(ratioVect[nBottomPoints:*]) 
            ; top I-Ratio Ch2/Ch0
          endif
          if whParamActive[15] then begin
             ratioVect = intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh0[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)
             ratioVect = ratioVect[sort(ratioVect)]
             (*(*(*self.pValueStruct)[whParam[15]]).pROIParamVect)[i] = mean(ratioVect[nBottomPoints:*]) 
            ; top I-Ratio Ch1/Ch2
          endif
          if whParamActive[16] then begin
             ratioVect = intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)
             ratioVect = ratioVect[sort(ratioVect)]
             (*(*(*self.pValueStruct)[whParam[16]]).pROIParamVect)[i] = mean(ratioVect[nBottomPoints:*]) 
            ; top I-Ratio Ch2/Ch1
          endif
          if whParamActive[17] then begin
             ratioVect = intCh2[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] / (intCh1[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())] > 0.01)
             ratioVect = ratioVect[sort(ratioVect)]
             (*(*(*self.pValueStruct)[whParam[17]]).pROIParamVect)[i] = mean(ratioVect[nBottomPoints:*]) 
          endif
          
          
          
          ; 18-23
          ; mean(Data1)/mean(DAta2)
          ;'Mean I-Ratio Ch0 div Ch1',$
          if whParamActive[18] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = (mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())]) / mean(intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])) > 0.01
          ;'Mean I-Ratio Ch1 div Ch0',$
          if whParamActive[19] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = (mean(intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())]) / mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])) > 0.01
          ;'Mean I-Ratio Ch0 div Ch2',$
          if whParamActive[20] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = (mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())]) / mean(intCh2[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])) > 0.01
          ;'Mean I-Ratio Ch2 div Ch0',$
          if whParamActive[21] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = (mean(intCh2[*((C_sROIGroupObj->get(position = i))->getpWherePoints())]) / mean(intCh0[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])) > 0.01
          ;'Mean I-Ratio Ch1 div Ch2',$
          if whParamActive[22] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = (mean(intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())]) / mean(intCh2[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])) > 0.01
          ;'Mean I-Ratio Ch2 div Ch1']          
          if whParamActive[23] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = (mean(intCh2[*((C_sROIGroupObj->get(position = i))->getpWherePoints())]) / mean(intCh1[*((C_sROIGroupObj->get(position = i))->getpWherePoints())])) > 0.01
           
       endfor
   endelse

end


function C_sROIParam_3DObjChannelIntensityRatio::init

   ROIParamStruct = {name:'3D Object Intensity Ratio',$     ;  ROI Name.
                     type:'3D ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                     pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

   nParams = 24
   self.pValueStruct = ptr_new(ptrArr(nParams))
   ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
   ROIParamNames = self->getParamNames()

   ROIParamActive = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
   ROIParamMin =    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
   ROIParamMax =    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
   ROIParamValues = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
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

    ROIValueWidgetType = make_array(9, /string, value = 'widget_slider')
    ROIValueNames = ['Top Percentage',$
                     'Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [1,0,0,0,0,0,0,0,0 ]
    ROIValueMin = [0.,0.,0.,0.,0.,0.,0.,0.,0.]
    ROIValueMax = [100.,1.,1.,1.,1.,1.,1.,1.,1. ]
    ROIValueValues =[10.,0.,1.,0.,1.,0.,1.,0.,1.]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueNames = ROIValueNames[1:*]
    ROIValueActive = ROIValueActive[1:*]
    ROIValueMin = ROIValueMin[1:*]
    ROIValueMax = ROIValueMax[1:*]
    ROIValueValues = ROIValueValues[1:*]

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
    ROIValueValues = [ROIValueValues]

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


pro C_sROIParam_3DObjChannelIntensityRatio__define
  tmp = {C_sROIParam_3DObjChannelIntensityRatio, pParamStruct: ptr_new(),$
                                   pValueStruct: ptr_new(),$
                                   inherits C_sROIParam}
end
