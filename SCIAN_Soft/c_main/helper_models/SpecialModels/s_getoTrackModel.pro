pro s_getoTrackModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

   radio = 5
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Radio Balls Model'))[0]
   if (whParam ne -1) then begin
      radio = fix(*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 1)
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = radio
   endif
   
   oColorModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oColorModel)) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oColorModel) then fOk = 1b else fOk = 0b

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()

   factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 2
   thickEV = 6.
   colEV = [0,0,255]

   ; INFO POSICION ACTUAL
   clusPos  = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Cluster Position'))[0]]    
   chPos    = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Channel Position'))[0]]
   tPos     = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Time Position'))[0]] 
   tPosNext = tPos + 1   
   ;pParamStruct = selectedStackObject->getpParamStruct()
   ;INFO T Z CH TOTALES
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = totalTimes
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZimes
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalChNum = totalChNum
   maximoObjetos = -1
   for i = 0, totalTimes-1 do begin
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)   
        numObjetosActual = C_sROI3DGroupObjActual->count()
        if(numObjetosActual gt maximoObjetos) then maximoObjetos =  numObjetosActual
   endfor
   center = fltArr(3,totalTimes,maximoObjetos)

     for i = 0, totalTimes-1 do begin 
        ; We need apply a transform for the model at time "i" to correlate positions with last time "i-1"
        ; Initial Time is the reference ... and using two planes for centred line reference and base cell plane reference 
        transformActual = 1
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
        numObjetosActual = C_sROI3DGroupObjActual->count()
        
        for j = 0, numObjetosActual-1 do begin
          center(*,i,j) = (*((C_sROI3DGroupObjActual->get(position = j))->getpEigenSys())).centerXYZ
        endfor 
        obj_destroy, C_sROI3DGroupObjActual
   endfor
 
   maximoObjetos = oGroupReference->count()
   for j = 0, maximoObjetos-1 do begin 
        (oGroupReference->get(position = j))->getProperty, color = colEV, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = j))->getNumber()
        ;colEV=[0,100,0]
        ;color = s_hist_white(colEV,totalTimes) 
        ;color = s_hist_black(colEV,totalTimes) 
        color = s_without_degrade(colEV,totalTimes) 
     for i = 0,totalTimes-1 do begin
  
             oObjectModel->add, obj_new('C_sOrb', POS=center[*,i,j], RADIUS=radio, xyzFact = factorXYZ, color = color[*,i], thick = thickEV, xCoord_conv = xCoord_conv, $
                                                  yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_Track_SphereEnd_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
          if i ne 0 then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,i-1,j]],[center[1,i,j],center[1,i-1,j]], [center[2,i,j], center[2,i-1,j]], color = color[*,i], $
             ;oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,0,j]],[center[1,i,j],center[1,0,j]], [center[2,i,j], center[2,0,j]], color = color[*,0], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackLine_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
     endfor
   endfor
end