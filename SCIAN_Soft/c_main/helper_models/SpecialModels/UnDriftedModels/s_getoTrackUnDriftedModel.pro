pro s_getoTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

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
        obj_destroy, C_sROI3DGroupObjActual
   endfor
   center = fltArr(3,totalTimes,maximoObjetos)

   for i = 0, totalTimes-1 do begin
        ; We need apply a transformation for the model at time "i" to correlate positions with last time "i-1"
        ; Initial Time is the reference ... and using two planes for centred line reference and base cell plane reference 
        transformActual = 1
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
        numObjetosActual = C_sROI3DGroupObjActual->count()
        
        for j = 0, numObjetosActual-1 do begin
          center(*,i,j) = s_RecalculateCenters(stack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjActual, actualTime = i, actualNumber = j,chPos = chPos, clusPos = clusPos) 
        endfor 
        obj_destroy, C_sROI3DGroupObjActual
   endfor
 
   maximoObjetos = oGroupReference->count()
   for j = 0, maximoObjetos-1 do begin 
        (oGroupReference->get(position = j))->getProperty, color = colEV, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = j))->getNumber()
        colEV=[0,100,0]
        color = s_hist_white(colEV,totalTimes) 
        ;color = s_hist_black(colEV,totalTimes)
        ;color = s_without_degrade(colEV,totalTimes) 
     for i = 0, totalTimes-1 do begin
             oObjectModel->add, obj_new('C_sOrb', POS=center[*,i,j], RADIUS=radio, xyzFact = factorXYZ, color = color[*,i], thick = thickEV, xCoord_conv = xCoord_conv, $
                                                  yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_TrackUnDrifted_SphereEnd_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
          if i ne 0 then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,i-1,j]],[center[1,i,j],center[1,i-1,j]], [center[2,i,j], center[2,i-1,j]], color = color[*,i], $
             ;oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,0,j]],[center[1,i,j],center[1,0,j]], [center[2,i,j], center[2,0,j]], color = color[*,0], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackUnDriftedLine_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
     endfor
   endfor
 
end

pro s_getoGroupTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

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
   centerTime = fltArr(3,totalTimes)
   for i = 0, totalTimes-1 do begin
        ; We need apply a transformation for the model at time "i" to correlate positions with last time "i-1"
        ; Initial Time is the reference ... and using two planes for centred line reference and base cell plane reference 
        transformActual = 1
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
        numObjetosActual = C_sROI3DGroupObjActual->count()
        
        for j = 0, numObjetosActual-1 do begin
          center(*,i,j) = s_RecalculateCenters(stack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjActual, actualTime = i, actualNumber = j,chPos = chPos, clusPos = clusPos) 
        endfor 
        centerTime(*,i) = [Total(center(0,i,*)),Total(center(1,i,*)),Total(center(2,i,*))] / numObjetosActual
        obj_destroy, C_sROI3DGroupObjActual
   endfor
 
   maximoObjetos = oGroupReference->count()

        (oGroupReference->get(position = 0))->getProperty, color = colEV, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = 0))->getNumber()
        colEV=[100,0,0]
        ;color = s_hist_white(colEV,totalTimes) 
        ;color = s_hist_black(colEV,totalTimes)
        color = s_without_degrade(colEV,totalTimes)    
   radio = 4
   
   ; Temporal fix for lost code....
   ;movil mean of values
   centerTimeAnterior = fltArr(3,totalTimes)
   centerTimeActual = fltArr(3,totalTimes)
   centerTimeAnterior = centerTime
   radio = 1 
   numIteraciones = 2
   for iteraciones = 0, numIteraciones do begin
     for i = 0, totalTimes-1 do begin
        inicio  = ((i - radio) lt 0) ? 0:(i - radio)
        fin     = ((i + radio) gt (totalTimes-1))? (totalTimes-1):(i + radio)
        
        centerTimeActual[*,i] = [total(centerTimeAnterior[0,inicio:fin]),total(centerTimeAnterior[1,inicio:fin]),total(centerTimeAnterior[2,inicio:fin])]/(1+fin-inicio) 
     endfor   
     centerTimeAnterior = centerTimeActual
   end
   
   centerTime = centerTimeActual
   for i = 0, totalTimes-1 do begin

          oObjectModel->add, obj_new('C_sOrb', POS=centerTime[*,i], RADIUS=radio, xyzFact = factorXYZ, color = color[*,i], thick = thickEV, xCoord_conv = xCoord_conv, $
                                                  yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_TrackUnDrifted_SphereEnd_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
          if i ne 0 then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [centerTime[0,i],centerTime[0,i-1]],[centerTime[1,i],centerTime[1,i-1]], [centerTime[2,i], centerTime[2,i-1]], color = color[*,i], $
             ;oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,0,j]],[center[1,i,j],center[1,0,j]], [center[2,i,j], center[2,0,j]], color = color[*,0], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackUnDriftedLine_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
   endfor
 
end

pro s_getoSmoothTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

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
   centerTime = fltArr(3,totalTimes)
   for i = 0, totalTimes-1 do begin
        ; We need apply a transformation for the model at time "i" to correlate positions with last time "i-1"
        ; Initial Time is the reference ... and using two planes for centred line reference and base cell plane reference 
        transformActual = 1
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
        numObjetosActual = C_sROI3DGroupObjActual->count()
        
        for j = 0, numObjetosActual-1 do begin
          center(*,i,j) = s_RecalculateCenters(stack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjActual, actualTime = i, actualNumber = j,chPos = chPos, clusPos = clusPos) 
        endfor 
        centerTime(*,i) = [Total(center(0,i,*)),Total(center(1,i,*)),Total(center(2,i,*))] / numObjetosActual
        obj_destroy, C_sROI3DGroupObjActual
   endfor
 
   maximoObjetos = oGroupReference->count()

        (oGroupReference->get(position = 0))->getProperty, color = colEV, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = 0))->getNumber()
        colEV=[100,0,0]
        ;color = s_hist_white(colEV,totalTimes) 
        ;color = s_hist_black(colEV,totalTimes)
        color = s_without_degrade(colEV,totalTimes)    

   
   ; Temporal fix for lost code....
   ;movil mean of values
   centerByObjectTimeAnterior = fltArr(3,totalTimes,maximoObjetos)
   centerByObjectTimeActual = fltArr(3,totalTimes,maximoObjetos)
   centerByObjectTimeActual   = center
   centerByObjectTimeAnterior = center
   
   
   centerTimeAnterior = fltArr(3,totalTimes)
   centerTimeActual = fltArr(3,totalTimes)
   centerTimeAnterior = centerTime
   centerTimeActual   = centerTime
   radio = 1 
   numIteraciones = 0
   for iteraciones = 0, numIteraciones do begin
     for i = 1, totalTimes-2 do begin
        inicio  = ((i - radio) lt 0) ? 0:(i - radio)
        fin     = ((i + radio) gt (totalTimes-1))? (totalTimes-1):(i + radio)
        
        centerTimeActual[*,i] = [total(centerTimeAnterior[0,inicio:fin]),total(centerTimeAnterior[1,inicio:fin]),total(centerTimeAnterior[2,inicio:fin])]/(1+fin-inicio)
        
        for j = 0, maximoObjetos-1 do begin
          centerByObjectTimeActual[*,i,j] = [total(centerByObjectTimeAnterior[0,inicio:fin,j]),total(centerByObjectTimeAnterior[1,inicio:fin,j]),total(centerByObjectTimeAnterior[2,inicio:fin,j])]/(1+fin-inicio)        
        endfor
     endfor   
     centerTimeAnterior = centerTimeActual
     centerByObjectTimeAnterior = centerByObjectTimeActual     
   end
   
   centerTime = centerTimeActual
   for i = 0, totalTimes-1 do begin
        for j = 0, maximoObjetos-1 do begin
          oObjectModel->add, obj_new('C_sOrb', POS=centerByObjectTimeActual[*,i,j], RADIUS=radio, xyzFact = factorXYZ, color = color[*,i], thick = thickEV, xCoord_conv = xCoord_conv, $
                                                  yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_TrackUnDrifted_SphereEnd_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
          if i ne 0 then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [centerByObjectTimeActual[0,i,j],centerByObjectTimeActual[0,i-1,j]],[centerByObjectTimeActual[1,i,j],centerByObjectTimeActual[1,i-1,j]], [centerByObjectTimeActual[2,i,j], centerByObjectTimeActual[2,i-1,j]], color = color[*,i], $
             ;oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,0,j]],[center[1,i,j],center[1,0,j]], [center[2,i,j], center[2,0,j]], color = color[*,0], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackUnDriftedLine_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
        endfor
   endfor
 
end
pro testPath
  catch, theError

  if theError ne 0 then begin
    print, theError
    oPath = 'C:\RSI\SCIAN_Code\SCIAN_Soft'
    catch, /CANCEL
    goto, r
  endif

  oJava = OBJ_NEW('IDLjavaObject$Static$JAVACLASSNAME','org.eclipse.core.resources.ResourcesPlugin')
  oRoot = ((oJava->getWorkspace())->getRoot())->getProject('SCIAN_Soft')
  oPath = (oRoot->getLocation())->toOSString()
  r:print, oPath
end