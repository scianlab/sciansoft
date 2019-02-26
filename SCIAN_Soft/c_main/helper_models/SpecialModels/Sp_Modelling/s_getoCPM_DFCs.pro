; FASL get Tracking Model
pro s_getoCPM_DFCs, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
     (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]


   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()

   radio = 5
   
;   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Radio Balls Model'))[0]
;   if (whParam ne -1) then begin
;      radio = fix(*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 1)
;      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = radio
;   endif
   
   oColorModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oColorModel)) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oColorModel) then fOk = 1b else fOk = 0b

   factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 2
   thickEV = 2.
   colEV = [0,0,255]

; Obtener centroide de todos los objetos en el tiempo
    clusPos  = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Cluster Position'))[0]]
    chPos    = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Channel Position'))[0]]
    tPos     = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Time Position'))[0]]
    tPosNext = tPos + 1   
   paramStruct = *(oGroupReference->getpParamStruct())
   
   masterCOM_DFCS = obj_new('C_s3D_CPM_DFCs')
   masterCOM_DFCS->Master_CPM_DFC_Creation, stack_tlb = stack_tlb, paramStruct = paramStruct, oGroupReference = oGroupReference
   
   radio = 5
   colEV = [117.,117.,117.]
   thickEV = 2.
  
  print, 'End creation ... independient interface running'
end
