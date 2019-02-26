;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_TrackParameter
;
; PURPOSE:
;       - Tracking of Objects.
;
; AUTHOR:
;     Steffen Härtel (2009)
;     e_mail: shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_TrackParameter')
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_TrackParameter::apply, stack_tlb = stack_tlb, oCurrTrackGroup = oCurrTrackGroup

   whParam = [(where(*(*self.pParamStruct).pNames eq 'Track Path'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track VCL'))[0],$                      
                  (where(*(*self.pParamStruct).pNames eq 'Track VSL'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track VAP'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track ALH'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track LIN'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track WOB'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track STR'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track BCF'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track MAD'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track Acceleration'))[0],$
                  (where(*(*self.pParamStruct).pNames eq 'Track Mean Square Distance'))[0] ]


      ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1: if (position[0] eq -1) then return else whParamActive[position] = 1
      else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase
   wherePA = where(whParamActive eq 1)

      ; check Pointers
   if not(ptr_valid((*self.pParamStruct).pROINumberVect)) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

   nObjects = C_sROI2DGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

      if whParamActive[0] then begin

          ; set ROI Number Vector with Object Number Vector
         *(*self.pParamStruct).pROINumberVect = oCurrTrackGroup->getObjectNumberVector()

               *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = dataVect


      endif else begin
         *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = make_array(nObjects, /index, /long) + 1
      endelse

   endelse
end


function C_sROIParam_TrackParameter::init

   ROIParamStruct = {name:'Object Track Parameters',$   ;  ROI Name.
                     type:'Track-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

   nParams = 12
   self.pValueStruct = ptr_new(ptrArr(nParams))
   ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
   ROIParamNames = ['Track Path',$
                    'Track VCL',$ 
                    'Track VSL',$
                    'Track VAP',$
                    'Track ALH',$   
                    'Track LIN',$
                    'Track WOB',$
                    'Track STR',$
                    'Track BCF',$
                    'Track MAD',$        
                    'Track Acceleration',$
                    'Track Mean Square Distance']
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                

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
                     type:'Track-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(14, /string, value = 'widget_slider')
   ROIValueNames = [ 'Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b',$
                     'Threshold_5a', 'Threshold_5b',$
                     'Threshold_6a', 'Threshold_6b',$
                     'Threshold_7a', 'Threshold_7b',$
                     'Threshold_8a', 'Threshold_8b',$
                     'Threshold_9a', 'Threshold_9b',$
                     'Threshold_10a', 'Threshold_10b',$
                     'Threshold_11a', 'Threshold_11b',$
                     'Threshold_12a', 'Threshold_12b',$
                     'Threshold_13a', 'Threshold_13b',$
                     'Threshold_14a', 'Threshold_14b']
   ROIValueActive =[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
   ROIValueMin = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
   ROIValueMax = [1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.]
   ROIValueValues = [0.,1.,0.,1.,0.,1.,0.,1.,0.,1.,0.,1.,0.,1.,0.,1.,0.,1.,0.,1.,0.,1.,0.,1.]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
   (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

   for i = 1, nParams-2 do begin
       ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[i],$
                         type:'Track-Parameter-Method',$
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
                     type:'Track-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect: ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

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

pro C_sROIParam_TrackParameter__define
   tmp = {C_sROIParam_TrackParameter, pParamStruct: ptr_new(), pValueStruct: ptr_new(), inherits C_sROIParam}
end