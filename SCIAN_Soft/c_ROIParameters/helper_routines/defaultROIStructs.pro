; Please test this function with custom params
; By default 
;   SAMPLE USAGE: ROIValueStruct =  s_makeCustomROIValueStruct('Object AC-Perimeter', 'Single ROI-Parameter-Method')
; You can set custom param structure for ROIValueNames, ROIValueActive,ROIValueMin, ROIValueMax or ROIValueValues
; an example was presented in C_sROIParam_ObjChannelIntensityRatio that requires
;     ROIValueNames = ['Top Percentage',$
;                     'Threshold_1a', 'Threshold_1b',$
;                     'Threshold_2a', 'Threshold_2b',$
;                     'Threshold_3a', 'Threshold_3b',$
;                     'Threshold_4a', 'Threshold_4b']
;  
function s_makeCustomROIValueStruct, paramName, paramType, ROIValueNames  = ROIValueNames,$
                                                           ROIValueActive = ROIValueActive,$
                                                           ROIValueMin    = ROIValueMin,$
                                                           ROIValueMax    = ROIValueMax,$
                                                           ROIValueValues = ROIValueValues
  ROIValueStruct = {name         : paramName,$
                    type         : paramType,$
                    pWidgetType  : ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames       : ptr_new(),$ ; Pointer on ROI-Obj Parameter Names.
                    pActive      : ptr_new(),$ ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin         : ptr_new(),$ ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax         : ptr_new(),$ ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues      : ptr_new(),$ ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()}  ; Pointer on ROI-Obj Parameter Vector.

  if (n_elements(ROIValueNames) eq 0) then begin
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
  endif
  nValues = n_elements(ROIValueNames)

  ROIValueWidgetType = make_array(nValues, /string, value = 'widget_slider')
  ROIValueActive     = n_elements(ROIValueActive) gt 0 ? ROIValueActive : make_array(nValues, /byte, value = 0)
  ROIValueMin        = n_elements(ROIValueMin) gt 0    ? ROIValueMin    : make_array(nValues, /float, value = 0)
  ROIValueMax        = n_elements(ROIValueMax) gt 0    ? ROIValueMax    : make_array(nValues, /float, value = 1)
  if n_elements(ROIValueValues) eq 0 then begin
    ROIValueValues = [0.0, 1.0]
    if (nValues gt 1) then for i=1L, nValues-1 do ROIValueValues = [ROIValueValues, 0., 1.]
  endif
  
  pROIParamVect  = [-1]

  ROIValueStruct.pWidgetType   = ptr_new(ROIValueWidgetType, /no_copy)
  ROIValueStruct.pNames        = ptr_new(ROIValueNames, /no_copy)
  ROIValueStruct.pActive       = ptr_new(ROIValueActive, /no_copy)
  ROIValueStruct.pMin          = ptr_new(ROIValueMin, /no_copy)
  ROIValueStruct.pMax          = ptr_new(ROIValueMax, /no_copy)
  ROIValueStruct.pValues       = ptr_new(ROIValueValues, /no_copy)
  ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

  return, ROIValueStruct
end


; s_makeDefaultROIValueStruct
;
; PURPOSE
;   Utility method to construct basic parameter structure por C_sROIParam derived objects 
;   (inside the pParamStruct/pValueStruct class attributes).
;   The created param struct is initialized as INactive (0), with 4x2 threshold value attributes,
;   in the range [0.0, 1.0] and with default value 0.0.
;
; PARAMETERS
;   paramName (string) 'Object AC-P²A' for example... WATCH OUT: If the ROI parameter is for 3D objects, include '3D' in the parameter names
;                      '3D Object AC-...' as example for a 3D ROI parameter object.
;   paramType (string) options used so far in SCIAN-Soft...
;                      'Single ROI-Parameter-Method',
;                      '3D ROI-Parameter-Method'
;                      'Inter ROI-Parameter-Method'
;                      'Group ROI-Parameter-Method'
;
; SAMPLE USAGE: ROIValueStruct =  s_makeDefaultROIValueStruct('Object AC-Perimeter', 'Single ROI-Parameter-Method')
function s_makeDefaultROIValueStruct, paramName, paramType
  ROIValueStruct = {name         : paramName,$
                    type         : paramType,$
                    pWidgetType  : ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames       : ptr_new(),$ ; Pointer on ROI-Obj Parameter Names.
                    pActive      : ptr_new(),$ ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin         : ptr_new(),$ ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax         : ptr_new(),$ ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues      : ptr_new(),$ ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()}  ; Pointer on ROI-Obj Parameter Vector.

  ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                   'Threshold_2a', 'Threshold_2b',$
                   'Threshold_3a', 'Threshold_3b',$
                   'Threshold_4a', 'Threshold_4b']
  ROIValueWidgetType = make_array(n_elements(ROIValueNames), /string, value = 'widget_slider')
  ROIValueActive     = make_array(n_elements(ROIValueNames), /byte, value = 0)
  ROIValueMin = make_array(n_elements(ROIValueNames), /float, value = 0)
  ROIValueMax = make_array(n_elements(ROIValueNames), /float, value = 1)
  ROIValueValues = [0., 1., 0., 1., 0., 1., 0., 1.]
  pROIParamVect  = [-1]

  ROIValueStruct.pWidgetType   = ptr_new(ROIValueWidgetType, /no_copy)
  ROIValueStruct.pNames        = ptr_new(ROIValueNames, /no_copy)
  ROIValueStruct.pActive       = ptr_new(ROIValueActive, /no_copy)
  ROIValueStruct.pMin          = ptr_new(ROIValueMin, /no_copy)
  ROIValueStruct.pMax          = ptr_new(ROIValueMax, /no_copy)
  ROIValueStruct.pValues       = ptr_new(ROIValueValues, /no_copy)
  ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

  return, ROIValueStruct
end


; s_makeDefaultROIParamStruct
; PURPOSE
;   Utility method to create  default parameter and subparameter(s) structures for a C_sROIParam-derived object
;   (such as the majority of the parameters implemented so far :). The method is intended to be called with the 
;   pointer variables pParamStruct and pValueStruct (self.p...).
;   The subparameters are created with repeated calls to s_makeDefaultROIValueStruct function
;
; ARGUMENTS
;   paramStructName: example '3D Object Volume'
;   paramStructType (string): options...
;                             'Single ROI-Parameter-Method',
;                             '3D ROI-Parameter-Method'
;                             'Inter ROI-Parameter-Method'
;                             'Group ROI-Parameter-Method'
;   ROIParamNames: string array
;   ROIParamTypes: string array whose values should be like the ones of paramStructType
;
; SAMPLE USAGE from C_sROIParam_SomeParam requires indirect assignation... see the following 3-instruction block:
; (I don't know why, but IDL seems not to be able to handle pointer attributes in method calls directly)
;
; s_makeDefaultROIParamStruct, '3D Object Volume',$
;                              '3D ROI-Parameter-Method',$
;                              ['3D Volume [Voxel]','3D Volume [x³]'],$
;                              ['3D ROI-Parameter-Method', '3D ROI-Parameter-Method'],$
;                               pParamStruct = pPS,$
;                               pValueStruct = pVS
; self.pParamStruct = pPS
; self.pValueStruct = pVS
pro s_makeDefaultROIParamStruct, paramStructName, paramStructType, ROIParamNames, ROIParamTypes, pParamStruct = pParamStruct, pValueStruct = pValueStruct
  ROIParamStruct = {name: paramStructName,$     ; Parameters class Name.
                    type: paramStructType,$
                    pWidgetType   : ptr_new(),$ ; Pointer on ROI-Obj Parameter Names.
                    pNames        : ptr_new(),$ ; Pointer on ROI-Obj Parameter Names.
                    pActive       : ptr_new(),$ ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin          : ptr_new(),$ ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax          : ptr_new(),$ ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues       : ptr_new(),$ ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect: ptr_new()}  ; Pointer on ROI-Obj Number Vector.
  nParams = n_elements(ROIParamNames)
  ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
  pValueStruct       = ptr_new(ptrArr(nParams))

  ROIParamActive = make_array(nParams, /int, value = 1)
  ROIParamMin    = make_array(nParams, /int, value = 0)
  ROIParamMax    = make_array(nParams, /int, value = 0)
  ROIParamValues = make_array(nParams, /int, value = 0)
  pROINumberVect = [-1]

  ROIParamStruct.pWidgetType    = ptr_new(ROIParamWidgetType, /no_copy)
  ROIParamStruct.pNames         = ptr_new(ROIParamNames) ; /no_copy flag intentionally ommited, in order to reuse the ROIParamNames list.
  ROIParamStruct.pActive        = ptr_new(ROIParamActive, /no_copy)
  ROIParamStruct.pMin           = ptr_new(ROIParamMin, /no_copy)
  ROIParamStruct.pMax           = ptr_new(ROIParamMax, /no_copy)
  ROIParamStruct.pValues        = ptr_new(ROIParamValues, /no_copy)
  ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

  pParamStruct = ptr_new(ROIParamStruct, /no_copy)
  pValueStruct = ptr_new(ptrArr(nParams))
  for i = 0L, nParams-1 do $
    (*pValueStruct)[i] = ptr_new(s_makeDefaultROIValueStruct(ROIParamNames[i], ROIParamTypes[i]))
end
