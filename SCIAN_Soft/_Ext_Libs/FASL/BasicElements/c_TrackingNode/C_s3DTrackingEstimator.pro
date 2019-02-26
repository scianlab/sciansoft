
pro C_s3DTrackingEstimator::MasterTrackingCreation, vSelected = vSelected, stack_tlb = stack_tlb,oGroupReference = oGroupReference, pParamStruct = pParamStruct, limitDistance = limitDistance, vAngleCorreccion = vAngleCorreccion, vCentroCorreccion = vCentroCorreccion, vRadio = vRadio, vFlipeado = vFlipeado  

  self._stack_tlb     = stack_tlb
  self._pParamStruct   = ptr_new(pParamStruct)
  self._pGroupReference = ptr_new(oGroupReference) 
  self._clusPos       = *(((pParamStruct)).pValues)[(where(*(((pParamStruct)).pNames) eq 'Cluster Position'))[0]]
  self._chPos         = *(((pParamStruct)).pValues)[(where(*(((pParamStruct)).pNames) eq 'Channel Position'))[0]]
  self._tPos          = *(((pParamStruct)).pValues)[(where(*(((pParamStruct)).pNames) eq 'Time Position'))[0]]
  
  self._dimXYZ = (*self._pGroupReference)->getxyzDim()

  self._centroSistema = vCentroCorreccion
  self._centroSistema[1] = self._dimXYZ[1]- self._centroSistema[1]  

  self->GenerateNodesSystem, vSelected = vSelected, limitDistance = limitDistance, vAngleCorreccion = vAngleCorreccion, vRadio = vRadio, vFlipeado = vFlipeado     
end

pro C_s3DTrackingEstimator::GenerateNodesSystem, vSelected = vSelected , limitDistance = limitDistance,  vAngleCorreccion = vAngleCorreccion, vRadio = vRadio, vFlipeado = vFlipeado

   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalTNum = totalTimes
   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalZNum = totalZimes
   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalChNum = totalChNum
   
   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()
   self._factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]

   self._tiempos =  totalTimes
   
   dummy = make_array(self._tiempos,/PTR);
   self.indexByTimes = ptr_new(dummy,/no_copy) 
   dummy = make_array(self._tiempos,/PTR);
   self.numberByTimes = ptr_new(dummy,/no_copy)
   dummy = make_array(self._tiempos,/PTR);
   self.rootByTimes = ptr_new(dummy,/no_copy)
   
   self._maximoObjetos = -1

   for i = 0,  self._tiempos-1 do begin
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = self._stack_tlb,tPos = i, chPos = self._chPos, clusPos = self._clusPos)   
        numObjetosActual = C_sROI3DGroupObjActual->count()
        if(numObjetosActual gt self._maximoObjetos) then self._maximoObjetos =  numObjetosActual
        
        dummy = [-1]
        (*self.indexByTimes)[i] = ptr_new(dummy,/no_copy)
        dummy = [-1]
        (*self.numberByTimes)[i] = ptr_new(dummy,/no_copy)
        dummy = [-1]
        (*self.rootByTimes)[i] = ptr_new(dummy,/no_copy)
        
        obj_destroy,C_sROI3DGroupObjActual
   endfor
   center = fltArr(3,totalTimes,self._maximoObjetos)
   numObjetosByTime = fltArr(self._tiempos)
   posicionRelativa = fltArr(3)
   
   angleUsed = vAngleCorreccion
   radioUsed = vRadio
   
    tMatrix =make_array(3,3,/double)   
     ; First --- we need the saved matrix transformation for actual time "i" .. and for time "0"
    widget_control, self._stack_tlb, get_uValue = stackState, /no_copy
     (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, self._stack_tlb, set_uValue = stackState, /no_copy
 
    self._path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
    self._path = strCompress(self._path+strcompress('_Tracking', /rem))
    tempPATH = strCompress(self._path+strcompress(path_sep()+'transformaciones' + path_sep() + 'TransformacionImagen_', /rem)) 
    self._modelName = strcompress(path_sep() + '3D_Tracking', /rem)
   
   tempPoint = fltArr(3)
   for i = 0, self._tiempos-1 do begin
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = self._stack_tlb,tPos = i, chPos = self._chPos, clusPos = self._clusPos)
        numObjetosActual = C_sROI3DGroupObjActual->count()
        numObjetosByTime[i] = numObjetosActual
        
           filename = strCompress(tempPATH+strCompress(strCompress(i+1) + '.sav', /rem))
           vCorrectDrift = 0b
           if(FILE_TEST(filename)) then begin
                vCorrectDrift = 1b
                line = make_Array(3,/double)
                h=0
                    GET_LUN, inunit
                    openr, inunit, filename
                    while ~ EOF(inunit) do begin
                       READF, inunit, line
                       if(h lt 3) then tMatrix [*,h]=line
                       h++ 
                    endwhile    
                    FREE_LUN, inunit
           endif        
        
        for j = 0, numObjetosActual-1 do begin
          center[*,i,j] = (*((C_sROI3DGroupObjActual->get(position = j))->getpEigenSys())).centerXYZ
;         size[DIMSIZE,i,j] = (*((C_sROI3DGroupObjActual->get(position = j))->vPARAMTRO
          ; Aplicar matriz de transformacion y corregir centro
           if(vCorrectDrift eq 1b) then begin
               ; Drift Correction based on StackReg Plugin for IMAGEJ
               tempZ = center[2,i,j]
               tempPoint = center[*,i,j] 
               center[2,i,j] = 1.0
              ; tempPoint = tempPoint # invert(tMatrix) 
               tempPoint = tMatrix  # tempPoint 
               center[*,i,j] = tempPoint
               center[2,i,j] = tempZ
           endif 
             if(abs(vAngleCorreccion) gt 0.1) then begin
                posicionRelativa = center[*,i,j] - self._centroSistema
                posicionRelativa[2] = 0.0
                radio = sqrt(Total(posicionRelativa*posicionRelativa))
                posicionRelativa = posicionRelativa / radio
                betaAngle = acos(posicionRelativa[0])
                if(center[1,i,j] lt self._centroSistema[1]) then begin
                  betaAngle = 2.0*!PI  - betaAngle
                endif
                ;ctrl
                ;posicionRelativa[0] = (radio - radioUsed) * cos(betaAngle - angleUsed - 45.0) 
                ;posicionRelativa[1] = (radio - radioUsed) * sin(betaAngle - angleUsed - 45.0)
                
                ;tg
                posicionRelativa[0] = (radio) * cos(betaAngle - angleUsed)
                posicionRelativa[1] = (radio) * sin(betaAngle - angleUsed)
                
                if(vFlipeado) then posicionRelativa[0] = -posicionRelativa[0] 
                delta=[-200,0,0]
                center[0:1,i,j] = self._centroSistema[0:1] + posicionRelativa[0:1] + delta
             endif
        endfor
        obj_destroy,C_sROI3DGroupObjActual    
   endfor
 ; Tenemos todos los centros de las celulas ,. con la informacion del tiempo y el indice de ROI.. y corregido el drift si existen las matrice de correccion
 
   axis = fltArr(3,2)
   
   sysVacio = 1b
   numNodos = 0
   for i = 1, self._tiempos-1 do begin
   ; Para cada tiempo se busca el elemento del tiempo anterior mas cercano...
        
        numObjetosActual = numObjetosByTime[i]
        for j = 0, numObjetosActual-1 do begin
          indiceAnterior = -1
          distanciaMinima = 10000000L
            
          axis[*,1] = center[*,i,j]; * xyzSizePerPixel)/xyzSizePerPixel[0]
          
          numObjetosAnterior = numObjetosByTime[i-1]
                            for objetoAnterior = 0, numObjetosAnterior -1 do begin
                                resta = axis[*,1] - center[*,i-1,objetoAnterior]
                                resta[2] = resta[2] / self._factorXYZ
                                producto = resta*resta
                                suma = abs(TOTAL(producto))
                                distanciaActual = sqrt(suma)   
                                if((distanciaActual lt distanciaMinima) and (distanciaActual lt limitDistance)) then begin
                                    distanciaMinima = distanciaActual
                                                     
                                    indiceAnterior = objetoAnterior
                                    axis[*,0] = center[*,i-1,indiceAnterior]; * xyzSizePerPixel)/xyzSizePerPixel[0]
                                endif
                            endfor
 
          if(distanciaMinima lt limitDistance) then begin
          ; Si hemos llegado aqui .. hemos encontrado una relacion
          ; entre el objeto j del tiempo i .. con el objeto indiceAnterior del tiempo i-1... :D magia....
          
            ; Si se crea algo se agrega... :D sino solo se actualiza la conectividad agregada
             if(sysVacio eq 1b) then begin
             ; Crear los dos nodos....
                nodeAnterior = obj_new('C_s3DTrackingNode') ; Este sera root
                nodeActual = obj_new('C_s3DTrackingNode'); este es derivado del root
                
                nodeAnterior->setInUse, vValue = 1b
                nodeAnterior->setInView, vValue = 0b
                nodeAnterior->setCenter, vValue = axis[*,0]
                nodeAnterior->setIsRoot, vValue = 1b
                nodeAnterior->setOwnIndex, vValue = numNodos
                nodeAnterior->setOwnNumber, vValue = indiceAnterior
                nodeAnterior->setOwnCluster, vValue = self._clusPos
                nodeAnterior->setOwnTime, vValue = (i-1)
                nodeAnterior->setOwnRoot, vValue = numNodos

                nodeAnterior->AddIndexConexiones, ownIndexConexiones = 1 ; uno es el indice que tendra el nodo actual... pensad como yo...
                nodeAnterior->AddIndexHerederos, ownIndexNumberHerederos = j, ownTimeHerederos = (i)
                self.masterTracking = ptr_new(nodeAnterior,/no_copy)
                numNodos = numNodos +1;

                nodeActual->setInUse, vValue = 1b
                nodeActual->setInView, vValue = 0b
                nodeActual->setCenter, vValue = axis[*,1]
                nodeActual->setOwnIndex, vValue = numNodos
                nodeActual->setOwnNumber, vValue = j
                nodeActual->setOwnCluster, vValue = self._clusPos
                nodeActual->setOwnTime, vValue = i
                nodeActual->setOwnRoot, vValue = 0
                nodeActual->setOwnParent, vValue = 0

                self.masterTracking = ptr_new([*self.masterTracking , nodeActual],/no_copy)
                numNodos = numNodos +1;
                
                sysVacio = 0b
                
                ; Crear indice de Roots
                dummyRoot = [0]
                
                self->AddSelected, vSelected = vSelected, vTime = i, vNumber = j, vIndexRoot = 0, vIndexOwn = 1 
                self->AddSelected, vSelected = vSelected, vTime = (i-1), vNumber = indiceAnterior, vIndexRoot = 0, vIndexOwn = 0
                self.indexOfRoots  = ptr_new([*self.indexOfRoots , dummyRoot],/no_copy)                
             endif else begin
             ; Verificar la existencia de los nodos, crear el que falte... :D nada mas...
             ;Existencia del nodo anterior...
               indiceBuscadoAnterior = self->FindExistenceAd( vTime = (i-1), vNumberObject = indiceAnterior)
               if(indiceBuscadoAnterior eq -1) then begin
               ; No esta asi que a crear un nuevo root :D una nueva trayectoria comienza :D
                  nodeAnterior = obj_new('C_s3DTrackingNode') ; Este sera root

                  nodeAnterior->setInUse, vValue = 1b
                  nodeAnterior->setInView, vValue = 0b
                  nodeAnterior->setCenter, vValue = axis[*,0]
                  nodeAnterior->setIsRoot, vValue = 1b
                  nodeAnterior->setOwnIndex, vValue = numNodos
                  nodeAnterior->setOwnNumber, vValue = indiceAnterior
                  nodeAnterior->setOwnCluster, vValue = self._clusPos
                  nodeAnterior->setOwnTime, vValue = (i-1)
                  nodeAnterior->setOwnRoot, vValue = numNodos
  
                  self.masterTracking = ptr_new([*self.masterTracking , nodeAnterior],/no_copy)
                  indiceBuscadoAnterior =  numNodos                 
                  
                  self->AddSelected, vSelected = vSelected, vTime = (i-1), vNumber = indiceAnterior, vIndexRoot = numNodos, vIndexOwn = indiceBuscadoAnterior
                  
                  dummy = [indiceBuscadoAnterior]
                  self.indexOfRoots  = ptr_new([*self.indexOfRoots , dummy],/no_copy)                  
                  numNodos = numNodos +1;
               endif
             
             ;Existencia del nodo actual...
               indiceBuscadoActual = self->FindExistenceAd( vTime = i, vNumberObject = j)
               if(indiceBuscadoActual eq -1) then begin
               ; No esta asi que a crear un nuevo root :D una nueva trayectoria comienza :D
                  nodeActual = obj_new('C_s3DTrackingNode') ; Este sera root

                  nodeActual->setInUse, vValue = 1b
                  nodeActual->setInView, vValue = 0b
                  nodeActual->setCenter, vValue = axis[*,1]
                  nodeActual->setIsRoot, vValue = 0b
                  nodeActual->setOwnIndex, vValue = numNodos
                  nodeActual->setOwnNumber, vValue = j
                  nodeActual->setOwnCluster, vValue = self._clusPos
                  nodeActual->setOwnTime, vValue = i
                  nodeActual->setOwnRoot, vValue = (*self.masterTracking)[indiceBuscadoAnterior]->getOwnRoot()
                  nodeActual->setOwnParent, vValue = indiceBuscadoAnterior
  
                  indiceBuscadoActual =  numNodos                 
                  
                  self->AddSelected, vSelected = vSelected, vTime = i, vNumber = j, vIndexRoot = (*self.masterTracking)[indiceBuscadoAnterior]->getOwnRoot(), vIndexOwn = indiceBuscadoActual
                  self.masterTracking = ptr_new([*self.masterTracking , nodeActual],/no_copy)                  
                  numNodos = numNodos +1;                  
               endif
               (*self.masterTracking)[indiceBuscadoAnterior]->AddIndexConexiones, ownIndexConexiones = indiceBuscadoActual ; uno es el indice que tendra el nodo actual... pensad como yo...
               (*self.masterTracking)[(*self.masterTracking)[indiceBuscadoAnterior]->getOwnRoot()]->AddIndexHerederos, ownIndexNumberHerederos = j, ownTimeHerederos = (i) ; uno es el indice que tendra el nodo actual... pensad como yo...
             endelse
          endif
        endfor 
   endfor
   
   ; evaluar divisiones
    FOR k=0.0d,N_ELEMENTS(*self.masterTracking)-1 DO BEGIN
        self->CalcDivision, vIndice = k
    endfor
    
    
    ; Only basic analysis based on centroid components....
end

pro C_s3DTrackingEstimator::AddSelected, vSelected = vSelected, vTime = vTime, vNumber = vNumber, vIndexRoot = vIndexRoot, vIndexOwn = vIndexOwn 
 dummyOwnIndex = vIndexOwn
 dummyRootIndex = vIndexRoot
 dummyNumberIndex = vNumber 
  if(N_ELEMENTS(vSelected) gt 0) then begin
      isRootSelected = 0b
      indexTime = where(vSelected[0,*] eq vTime)
      if(indexTime[0] ne -1) then begin
        indexTimeNumber = where(vSelected[1,indexTime] eq vNumber)
        if(indexTimeNumber[0] ne -1) then begin
          isRootSelected = 1b
        endif
      endif
      if(isRootSelected eq 1b) then begin
        dummy = where(*self.indexSelected eq vIndexRoot)
        if(dummy[0] eq -1) then self.indexSelected = ptr_new([*self.indexSelected , dummyRootIndex],/no_copy)
      endif
  endif
  (*self.indexByTimes)[vTime] = ptr_new([*((*self.indexByTimes)[vTime]) , dummyOwnIndex],/no_copy)  
  (*self.numberByTimes)[vTime] = ptr_new([*((*self.numberByTimes)[vTime]) , dummyNumberIndex],/no_copy)
  
  dummyRootIndex = vIndexRoot
  (*self.rootByTimes)[vTime] = ptr_new([*((*self.rootByTimes)[vTime]) , dummyRootIndex],/no_copy)
end


function C_s3DTrackingEstimator::FindExistence, vTime = vTime, vNumberObject = vNumberObject
    FOR k=0.0d,N_ELEMENTS(*self.masterTracking)-1 DO BEGIN
      if( ( (*self.masterTracking)[k]->getOwnNumber() eq vNumberObject) and ( (*self.masterTracking)[k]->getOwnTime() eq vTime) ) then begin
        return, (*self.masterTracking)[k]->getOwnIndex()
      endif
    endfor
    return, -1
end

function C_s3DTrackingEstimator::FindExistenceAd, vTime = vTime, vNumberObject = vNumberObject
    dummy = where( (*((*self.numberByTimes)[vTime]))[*] eq vNumberObject)
    if(dummy[0] ne -1) then begin
      return, (*((*self.indexByTimes)[vTime]))[dummy[0]]
    endif
    return, dummy[0]
end

pro C_s3DTrackingEstimator::CalcDivision, vIndice = vIndice
   numConexiones = (*self.masterTracking)[vIndice]->getNumConexiones()
   conexiones = (*self.masterTracking)[vIndice]->getIndexConexiones()
   sizePropio = 0.0
   if(numConexiones gt 1) then begin
      ; inicializar variables
      puntoMedio = [0.0,0.0,0.0]
      normalDiv  = [0.0,0.0,0.0]
      normalAB  = [0.0,0.0,0.0]
      anguloDiv  = 0.0

      sizePropio = 0.1
      C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = self._stack_tlb,tPos = (*self.masterTracking)[vIndice]->getOwnTime(), chPos = self._chPos, clusPos = self._clusPos)
      sizePropio = n_elements( *((C_sROI3DGroupObjActual->get(position = (*self.masterTracking)[vIndice]->getOwnNumber()))->getpWherePoints()) )
      OBJ_DESTROY, C_sROI3DGroupObjActual
      
      sizeHijos = [0.0,0.0]
      distancia = 0.0
     for j = 0, 1 do begin
        if(conexiones[j] ne -1) then begin
            if(j eq 0) then normalAB = normalAB + (*self.masterTracking)[conexiones[j]]->getCenter()
            if(j eq 1) then normalAB = normalAB - (*self.masterTracking)[conexiones[j]]->getCenter()
        
            puntoMedio = puntoMedio + (*self.masterTracking)[conexiones[j]]->getCenter()
            
            numConexionesH = (*self.masterTracking)[conexiones[j]]->getNumConexiones()
            conexionesH = (*self.masterTracking)[conexiones[j]]->getIndexConexiones()
            if(numConexionesH gt 0) then begin
                for h = 0, numConexionesH-1 do begin
                    if(conexionesH[h] ne -1) then begin
                       C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = self._stack_tlb,tPos = (*self.masterTracking)[conexionesH[h]]->getOwnTime(), chPos = self._chPos, clusPos = self._clusPos)
                       sizeHijos[j] = sizeHijos[j] + n_elements( *((C_sROI3DGroupObjActual->get(position = (*self.masterTracking)[conexionesH[h]]->getOwnNumber()))->getpWherePoints()) )
                       OBJ_DESTROY, C_sROI3DGroupObjActual                    
                    endif
                endfor
                sizeHijos[j] = sizeHijos[j] / numConexionesH
            endif else begin
              C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = self._stack_tlb,tPos = (*self.masterTracking)[conexiones[j]]->getOwnTime(), chPos = self._chPos, clusPos = self._clusPos)
              sizeHijos[j] = sizeHijos[j] + n_elements( *((C_sROI3DGroupObjActual->get(position = (*self.masterTracking)[conexiones[j]]->getOwnNumber()))->getpWherePoints()) )
              OBJ_DESTROY, C_sROI3DGroupObjActual
            endelse
         endif
     endfor
     puntoMedio = puntoMedio / 2.0
      ; decidir si ha habido division
      distancia = sqrt(total(normalAB*normalAB))
      vDividido = 0b
      if((sizePropio gt 1.8*total(sizeHijos)) and (distancia lt 20))then begin
         vDividido = 1b
      endif
    
    
    ; Asignar valores y calcular parametros pendientes
      if((vDividido eq 1b) and (sqrt(total(normalAB*normalAB)) gt 0.0)) then begin
        ; calculos
        normalAB = normalAb / sqrt(total(normalAB*normalAB))
        normalZ  = [0.0,0.0,1.0]
        normalDiv = CROSSP(normalAB,normalZ)
        
        ; Asignacion final
        (*self.masterTracking)[vIndice]->setIsDivision,vValue = vDividido    
        (*self.masterTracking)[vIndice]->setPuntoDiv,vValue   = puntoMedio
        (*self.masterTracking)[vIndice]->setNormalDiv,vValue  = normalDiv
        
        normalY      = [0.0,1.0,0.0]
        normalDiv[2] = 0.0
        normalDiv = normalDiv / sqrt(total(normalDiv*normalDiv))
        anguloDiv = acos(total(normalY*normalDiv))
        
        (*self.masterTracking)[vIndice]->setAngleDiv,vValue   = anguloDiv
      endif
   endif
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Normalizacion de sistema de Renderizado


pro C_s3DTrackingEstimator::InitRenderArea
  s_WorldViewforModel, xdim = self._dimXYZ[0], ydim = self._dimXYZ[1] ,win =win,oClipboard = oClip, oScene = oSccene, oModel = self._model, oStaticInputModel = self._staticInputModel
  self._win = win
  self._oClipboard = oClip
  self._oScene = oSccene
 ; self._staticInputModel->remove, /all
end

pro C_s3DTrackingEstimator::UpdateRenderArea, time = time
  self->RenderCurrentState, mode = 1

  o1 = self._win
  o2 = self._oClipboard
  o3 = self._oScene
  s_SaveViewforModel,win = o1,oClipboard = o2, oScene = o3, vPath = self._path, time = time, modelName = self._modelName
  self._win = o1
  self._oClipboard = o2
  self._oScene = o3
  self._model->remove, /all  
end
pro C_s3DTrackingEstimator::RenderCurrentState,mode = mode
 (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()
  xyzSPPNorm = (*((*self._pGroupReference)->getpEigenSys())).sizePerXYZ / max((*((*self._pGroupReference)->getpEigenSys())).sizePerXYZ)


      if(mode eq 0) then begin
        self._staticInputModel->add,  obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, shading = 1, shininess = 128.,$
                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress(name,/rem), uvalue = 'ObjInOriginalPosition')
        
      endif else begin
        self._model->add,  obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, shading = 1, shininess = 128.,$
                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress(name,/rem), uvalue = 'ObjInOriginalPosition')
      endelse

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Draw functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro C_s3DTrackingEstimator::DrawDivision, vIndice = vIndice, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    vColor  = [0.0,0.0,0.0]
    vDimDiv = 10.0
    
    axis = fltArr(3,2)
    axis[*,0] = (*self.masterTracking)[vIndice]->getPuntoDiv()
    axis[*,1] = axis[*,0]
    
    axis[*,0] =+ vDimDiv * (*self.masterTracking)[vIndice]->getNormalDiv()
    axis[*,1] =- vDimDiv * (*self.masterTracking)[vIndice]->getNormalDiv()
            
    self._staticInputModel->add, obj_new('IDLgrPolyline', axis, color = vColor, thick = 2*thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                  name = strCompress('3D_TrackLine_Division_'+string(vIndice),/rem), uvalue = 'ObjInOriginalPosition')
end


pro C_s3DTrackingEstimator::DrawStaticLevels, vIndice = vIndice, vRadio = vRadio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = vRecursive, vDrawNodes = vDrawNodes, vDrawLines = vDrawLines, vColor = vColor 
   axis = fltArr(3,2)
   
   rootIndex = (*self.masterTracking)[vIndice]->getOwnRoot()
   axis[*,0] = (*self.masterTracking)[vIndice]->getCenter(); - (*self.masterTracking)[rootIndex]->getCenter() 

   fAddColor = 0b
   if(N_ELEMENTS(vColor) eq 0) then begin
      ownColor = (*self.masterTracking)[vIndice]->getActualColor()
   endif else begin
      ownColor = vColor
      fAddColor = 1b
   endelse

   if((1b eq 1b) and ((*self.masterTracking)[vIndice]->getIsDivision() eq 1b) and (vRecursive eq 0b))then begin
      ;self->DrawDivision, vIndice = vIndice, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv   
      vColorD  = [0.0,0.0,0.0]
      vDimDiv = 10.0
      
      axisD = fltArr(3,2)
      axisD[*,0] = (*self.masterTracking)[vIndice]->getPuntoDiv()
      axisD[*,1] = axisD[*,0]
      
      axisD[*,0] = axisD[*,0] + vDimDiv * (*self.masterTracking)[vIndice]->getNormalDiv()
      axisD[*,1] = axisD[*,1] - vDimDiv * (*self.masterTracking)[vIndice]->getNormalDiv()
              
      self._staticInputModel->add, obj_new('IDLgrPolyline', axisD, color = vColorD, thick = 1*thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                    name = strCompress('3D_TrackLine_Division_'+string(vIndice),/rem), uvalue = 'ObjInOriginalPosition')
   end
   
   ;ownColor = [255,0,0]
   if(vDrawNodes eq 1b) then begin
    ; self._staticInputModel->add, obj_new('C_sOrb', POS=axis[*,0], RADIUS=vRadio, xyzFact = self._factorXYZ, color = ownColor, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
     ;                           name = strCompress('3D_Track_Sphere_NodeBase_'+string(vIndice),/rem), uvalue = 'ObjInOriginalPosition')

    vDrawNumber = 1b
    if(vDrawNumber eq 1b) then begin
     ;self._staticInputModel->add, obj_new('IDLgrText', (*self.masterTracking)[vIndice]->getOwnNumber(), LOCATIONS = [axis[*,0],axis[*,1],2.0*axis[2,0]], ALIGNMENT = 0.5); 
    endif
   endif
      
   numConexiones = (*self.masterTracking)[vIndice]->getNumConexiones()
   conexiones = (*self.masterTracking)[vIndice]->getIndexConexiones()
   for j = 0, numConexiones-1 do begin
      if(conexiones[j] ne -1) then begin
        rootIndex = (*self.masterTracking)[conexiones[j]]->getOwnRoot()
        axis[*,1] = (*self.masterTracking)[conexiones[j]]->getCenter(); - (*self.masterTracking)[rootIndex]->getCenter()
        
         vView = (*self.masterTracking)[conexiones[j]]->getInView()
         if(vDrawLines eq 1b and vView eq 1b) then begin
             self._staticInputModel->add, obj_new('IDLgrPolyline', axis, color = ownColor, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                  name = strCompress('3D_TrackLine_NodeBase_'+string(conexiones[j]),/rem), uvalue = 'ObjInOriginalPosition')
         endif
                 
         if(vRecursive eq 1b) then begin
            if(fAddColor eq 1b) then begin
                if(j gt 0) then begin
                   loadct, self.indexColorTable
                   tvlct, r, g, b, /get
              
                    self.indexColorSelected = self.indexColorSelected + 255.0/self.numColorsSelected
                    vColor[0] = r[self.indexColorSelected < 255]
                    vColor[1] = g[self.indexColorSelected < 255] 
                    vColor[2] = b[self.indexColorSelected < 255]
                endif 
                (*self.masterTracking)[conexiones[j]]->setActualColor, vValue = vColor
            endif
              self->DrawStaticLevels, vIndice = conexiones[j], vRadio = vRadio, colEV = colEV, thickEV = thickEV, vRecursive = vRecursive, vDrawNodes = vDrawNodes, vDrawLines = vDrawLines, vColor = vColor
         endif
      endif
   endfor 
end

pro C_s3DTrackingEstimator::DrawDinamicLevels, vIndice = vIndice, vRadio = vRadio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = vRecursive, vDrawNodes = vDrawNodes, vDrawLines = vDrawLines, vColor = vColor, vVision = vVision 
   axis = fltArr(3,2)
   
   ownColor = (*self.masterTracking)[vIndice]->getActualColor() 
   rootIndex = (*self.masterTracking)[vIndice]->getOwnRoot()
   axis[*,0] = (*self.masterTracking)[vIndice]->getCenter() ;- (*self.masterTracking)[rootIndex]->getCenter() 
   vView = (*self.masterTracking)[vIndice]->getInView()
   
   if(vDrawNodes eq 1b and vView eq 1b) then begin
     self._model->add, obj_new('C_sOrb', POS=axis[*,0], RADIUS=vRadio, xyzFact = self._factorXYZ, color = ownColor, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                               name = strCompress('3D_Track_Sphere_NodeDin_'+string(vIndice),/rem), uvalue = 'ObjInOriginalPosition')
      vDrawNumber = 0b;
      sizeCHAR = [5.0*5.0, 10.0*5.0];
      if(vDrawNumber eq 1b) then begin
;       self._model->add, obj_new('IDLgrText', string((*self.masterTracking)[vIndice]->getOwnNumber()), xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$ 
;            LOCATIONS = [axis[0,0]- sizeCHAR[0],axis[1,0]- 0.5*sizeCHAR[1],2.0*axis[2,0]], ALIGNMENT = 0.5, CHAR_DIMENSIONS = sizeCHAR); 
       self._model->add, obj_new('IDLgrText', string(vIndice), xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$ 
            LOCATIONS = [axis[0,0]- sizeCHAR[0],axis[1,0]- 0.5*sizeCHAR[1],2.0*axis[2,0]], ALIGNMENT = 0.5, CHAR_DIMENSIONS = sizeCHAR); 
      endif
   endif

   if((vDrawLines eq 1b) and (vVision gt 0) and (vVision eq self._vision)) then begin
      axisD = fltArr(3,2)
      axisD[*,0] = axis[*,0]
      dummy = where( (*((*self.rootByTimes)[(*self.masterTracking)[vIndice]->getOwnTime()]))[*] eq vIndice)
      
      valido = (dummy[0] eq -1)?1b:0b   
      indiceActual = vIndice
      myVision = vVision
      valido = 0b
      while(valido eq 1b) do begin
        indiceSuperior = (*self.masterTracking)[indiceActual]->getOwnParent()
        axisD[*,1] = (*self.masterTracking)[indiceSuperior]->getCenter() ;- (*self.masterTracking)[rootIndex]->getCenter()

        self._model->add, obj_new('IDLgrPolyline', axisD, color = ownColor, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                              name = strCompress('3D_TrackLine_NodeDin_Parent'+string(indiceSuperior),/rem), uvalue = 'ObjInOriginalPosition')
                              
        dummy = where( (*((*self.rootByTimes)[(*self.masterTracking)[indiceSuperior]->getOwnTime()]))[*] eq indiceSuperior)
        valido = (dummy[0] eq -1)?0b:1b                              
        indiceActual = indiceSuperior
        axisD[*,0] = axisD[*,1]
        myVision = myVision - 1
        validoB = (myVision gt 0)?1b:0b
        
        valido = valido and validoB
      endwhile
   endif


   numConexiones = (*self.masterTracking)[vIndice]->getNumConexiones()
   conexiones = (*self.masterTracking)[vIndice]->getIndexConexiones()
   for j = 0, numConexiones-1 do begin
      if(conexiones[j] ne -1) then begin
        rootIndex = (*self.masterTracking)[conexiones[j]]->getOwnRoot()
        axis[*,1] = (*self.masterTracking)[conexiones[j]]->getCenter() ;- (*self.masterTracking)[rootIndex]->getCenter()

         if(vDrawLines eq 1b) then begin
             self._model->add, obj_new('IDLgrPolyline', axis, color = ownColor, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                  name = strCompress('3D_TrackLine_NodeDin_'+string(conexiones[j]),/rem), uvalue = 'ObjInOriginalPosition')
             ;if(vVision gt 0) then begin
              ;  self->DrawDinamicLevels, vIndice = conexiones[j], vRadio = vRadio, colEV = colEV, thickEV = thickEV, vRecursive = 0, vDrawNodes = 0, vDrawLines = 1, vColor = vColor, vVision = vVision -1             
             ;endif
         endif
         
         if(vRecursive eq 1b) then begin
              self->DrawDinamicLevels, vIndice = conexiones[j], vRadio = vRadio, colEV = colEV, thickEV = thickEV, vRecursive = vRecursive, vDrawNodes = vDrawNodes, vDrawLines = vDrawLines, vColor = vColor
         endif
      endif
   endfor 
end

pro C_s3DTrackingEstimator::DrawDinamicLevelsCreative, vIndice = vIndice, vRadio = vRadio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = vRecursive, vDrawNodes = vDrawNodes, vDrawLines = vDrawLines, vColor = vColor 
   axis = fltArr(3,2)
   
   ownColor = (*self.masterTracking)[vIndice]->getActualColor() 
   rootIndex = (*self.masterTracking)[vIndice]->getOwnRoot()
   axis[*,0] = (*self.masterTracking)[vIndice]->getCenter() ;- (*self.masterTracking)[rootIndex]->getCenter() 

   if(vDrawNodes eq 1b) then begin
     self._model->add, obj_new('C_sOrb', POS=axis[*,0], RADIUS=vRadio, xyzFact = self._factorXYZ, color = ownColor, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                name = strCompress('3D_Track_Sphere_NodeDin_'+string(vIndice),/rem), uvalue = 'ObjInOriginalPosition')
   endif

   if(vDrawLines eq 1b) then begin
       tempParent = (*self.masterTracking)[vIndice]->getOwnParent()
       if(tempParent ne -1) then begin
          axis[*,1] = (*self.masterTracking)[tempParent]->getCenter() 
          self._staticInputModel->add, obj_new('IDLgrPolyline', axis, color = ownColor, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                            name = strCompress('3D_TrackLine_NodeDin_'+string(vIndice),/rem), uvalue = 'ObjInOriginalPosition')
       endif
   endif
  
end


pro C_s3DTrackingEstimator::DrawAllNodesBase,win = win, oClipboard = oClipboard, oScene = oScene, oCurrTopModel = oCurrTopModel, oWorldRotModel = oWorldRotModel, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, radio = radio, colEV = colEV, thickEV = thickEV
  self.indexColorSelected = 0.0      

  for indexNode=0,N_ELEMENTS(*self.masterTracking)-1 DO BEGIN
      self->DrawStaticLevels, vIndice = indexNode, vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 0b, vDrawNodes = 1b, vDrawLines = 0b            
  endfor
  s_WorldViewforModel, xdim = self._dimXYZ[0], ydim = self._dimXYZ[1] ,win =win,oClipboard = oClip, oScene = oSccene, oModel = self._model, oStaticInputModel = self._staticInputModel
  self._win = win
  self._oClipboard = oClip
  self._oScene = oSccene
end


pro C_s3DTrackingEstimator::DrawSelectedTrackingBase, radio = radio, colEV = colEV, thickEV = thickEV
   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()

  self.indexColorSelected = 0.0 

  vColor = fltArr(3)
  tvlct, rOrigen, gOrigen, bOrigen, /get
  loadct, self.indexColorTable
  tvlct, r, g, b, /get

  for indexRoot=1, N_ELEMENTS(*self.indexOfRoots)-1 DO BEGIN
    self.indexColorSelected = self.indexColorSelected + 255.0/float(self.numColorsSelected)
    vColor[0] = r[self.indexColorSelected < 255]
    vColor[1] = g[self.indexColorSelected < 255] 
    vColor[2] = b[self.indexColorSelected < 255] 
    (*self.masterTracking)[(*self.indexOfRoots)[indexRoot]]->setActualColor, vValue = vColor
    self->DrawStaticLevels, vIndice = (*self.indexOfRoots)[indexRoot], vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 1b, vDrawNodes = 0b, vDrawLines = 0b, vColor = vColor
  endfor

  s_WorldViewforModel, xdim = self._dimXYZ[0], ydim = self._dimXYZ[1] ,win =win,oClipboard = oClip, oScene = oSccene, oModel = self._model, oStaticInputModel = self._staticInputModel
  self._win = win
  self._oClipboard = oClip
  self._oScene = oSccene

    ; maldicion... la asignacion recursiva de modelos no funciona... todo el resto de las operaciones recursivas si..
    ; el modelo se crea de forma recursiva pero no es procesable por las funciones draw....
    self._staticInputModel->remove, /all
    
;    ; Add ... Z projection image model... XD
;    s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
;                                              clusPos = clusPos, selClusName = selClusName, segPos = segPos
;    momentFilter = obj_new('C_sImageFilter_ImageZProjection')
;
;    image = MAKE_ARRAY(self._dimXYZ[0],self._dimXYZ[1],/BYTE,VALUE = 0)
;    cut_x = [0, self._dimXYZ[0]-1]
;    cut_y = [0, self._dimXYZ[1]-1]
;
;  widget_control, self._stack_tlb, get_uValue = stackState, /no_copy
;    ;(*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
;    ; why the output for filters is an referenced input too????????????????????????????????
;    ; FASL...
;    tempSelectStack = (*stackState.pImageStackInfoObject)
;  widget_control, self._stack_tlb, set_uValue = stackState, /no_copy    
;
;    image = momentFilter->apply( image = image,$
;                        selectedStackObject = tempSelectStack,$
;                        stack_tlb = self._stack_tlb,$
;                        tPos = 0,$
;                        chPos = chPos,$
;                        zPos = zPos,$
;                        clusPos = clusPos,$
;                        segPos = segPos,$
;                        cut_x = cut_x, cut_y = cut_y)
;  loadct, 3
;  tvlct, rI, gI, bI, /get
;    image = bytScl(image,/nan)
;    imageRGB = MAKE_ARRAY(3,self._dimXYZ[0],self._dimXYZ[1],/BYTE,VALUE = 0)
;    ; :_( imageRGB[*,0:self._dimXYZ[0]-1,0:self._dimXYZ[1]-1] = [rOrigen[image[0:self._dimXYZ[0]-1,0:self._dimXYZ[1]-1]],gOrigen[image[0:self._dimXYZ[0]-1,0:self._dimXYZ[1]-1]],bOrigen[image[0:self._dimXYZ[0]-1,0:self._dimXYZ[1]-1]]]
;    for vX = 0, self._dimXYZ[0]-1 do begin
;      for vY = 0, self._dimXYZ[1]-1 do begin
;        imageRGB[*,vX,vY] = [rI[image[vX,vY]],gI[image[vX,vY]],bI[image[vX,vY]]]      
;      endfor
;    endfor
;    
;    self._staticInputModel->add, obj_new('IDLgrImage',imageRGB, GREYSCALE  = 0,xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, TRANSFORM_MODE = 1, LOCATION = [0,0,-100])
;  tvlct, r, g, b
    self._staticInputModel->add, obj_new('C_sOrb', POS=self._centroSistema, RADIUS=2.0*radio, xyzFact = self._factorXYZ, color = [117,117,117], thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                name = strCompress('3D_Track_CENTER_'+string(0),/rem), uvalue = 'ObjInOriginalPosition')
  
  if(1 eq 1) then begin      
      for i=0,self._tiempos-1 DO BEGIN
          for j = 1,N_ELEMENTS(*self.indexOfRoots)-1 DO BEGIN
              dummy = where( (*((*self.rootByTimes)[i]))[*] eq (*self.indexOfRoots)[j])
              if(dummy[0] ne -1) then begin
                FOR k=0,N_ELEMENTS(dummy)-1 DO BEGIN
                     self->DrawStaticLevels, vIndice = (*((*self.indexByTimes)[i]))[dummy[k]], vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 0b, vDrawNodes = 0b, vDrawLines = 1b            
                endfor
              endif
           endfor
      endfor
  endif
  
            o1 = self._win
            o2 = self._oClipboard
            o3 = self._oScene
            s_SaveViewforModel,win = o1,oClipboard = o2, oScene = o3, vPath = self._path, time = '_Initial', modelName = self._modelName
            self._win = o1
            self._oClipboard = o2
            self._oScene = o3
   
  tvlct, rOrigen, gOrigen, bOrigen
  
  ;self._staticInputModel->remove, /all
end


pro C_s3DTrackingEstimator::DrawSelectedTrackingMovie, radio = radio, colEV = colEV, thickEV = thickEV
   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()


  tvlct, rOrigen, gOrigen, bOrigen, /get

;    ; Add ... Z projection image model... XD
;    s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
;                                              clusPos = clusPos, selClusName = selClusName, segPos = segPos
;    momentFilter = obj_new('C_sImageFilter_ImageZProjection')
;
;    image = MAKE_ARRAY(self._dimXYZ[0],self._dimXYZ[1],/BYTE,VALUE = 0)
;    cut_x = [0, self._dimXYZ[0]-1]
;    cut_y = [0, self._dimXYZ[1]-1]
;
;  widget_control, self._stack_tlb, get_uValue = stackState, /no_copy
;    ;(*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
;    ; why the output for filters is an referenced input too????????????????????????????????
;    ; FASL...
;    tempSelectStack = (*stackState.pImageStackInfoObject)
;  widget_control, self._stack_tlb, set_uValue = stackState, /no_copy    
;
;  loadct, 51
;  tvlct, rI, gI, bI, /get

  self.indexColorSelected = 0.0
  track=1
  for i=0, self._tiempos-1 do begin ;self._tiempos-1 DO BEGIN
;      image = momentFilter->apply( image = image,$
;                          selectedStackObject = tempSelectStack,$
;                          stack_tlb = self._stack_tlb,$
;                          tPos = i,$
;                          chPos = chPos,$
;                          zPos = zPos,$
;                          clusPos = clusPos,$
;                          segPos = segPos,$
;                          cut_x = cut_x, cut_y = cut_y, track = track)
;
;      image = bytScl(image,/nan)
;      imageRGB = MAKE_ARRAY(3,self._dimXYZ[0],self._dimXYZ[1],/BYTE,VALUE = 0)
;      ; :_( imageRGB(*,0:self._dimXYZ[0]-1,0:self._dimXYZ[1]-1) = [rOrigen[image(0:self._dimXYZ[0]-1,0:self._dimXYZ[1]-1)],gOrigen[image(0:self._dimXYZ[0]-1,0:self._dimXYZ[1]-1)],bOrigen[image(0:self._dimXYZ[0]-1,0:self._dimXYZ[1]-1)]]
;      for vX = 0, self._dimXYZ[0]-1 do begin
;        for vY = 0, self._dimXYZ[1]-1 do begin
;          imageRGB[*,vX,vY] = [rI[image[vX,vY]],gI[image[vX,vY]],bI[image[vX,vY]]]      
;        endfor
;      endfor
      
      ;self._model->add, obj_new('IDLgrImage',imageRGB, GREYSCALE  = 0,xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, TRANSFORM_MODE = 1, LOCATION = [0,0,-radio])

      for j = 1,N_ELEMENTS(*self.indexOfRoots)-1 DO BEGIN
          dummy = where( (*((*self.rootByTimes)[i]))[*] eq (*self.indexOfRoots)[j])
          if(dummy[0] ne -1) then begin
            FOR k=0,N_ELEMENTS(dummy)-1 DO BEGIN
                  self->DrawDinamicLevels, vIndice = (*((*self.indexByTimes)[i]))[dummy[k]], vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 0b, vDrawNodes = 1b, vDrawLines = 0b, vVision = self._vision            
            endfor
          endif
       endfor
       
        o1 = self._win
        o2 = self._oClipboard
        o3 = self._oScene
        s_SaveViewforModel,win = o1,oClipboard = o2, oScene = o3, vPath = self._path, time = i, modelName = self._modelName
        self._win = o1
        self._oClipboard = o2
        self._oScene = o3


       self._model->remove, /all
  endfor
  
  tvlct, rOrigen, gOrigen, bOrigen
end

;ESTA REPLICA NO SE DONDE SALIO????????????? NO VEO COMO SE REPLICO::: 
function C_s3DTrackingEstimator::DrawSelectedTrackingMovie_QUEESESTOQUIENPUSOESTAFUNCIONAQUI, radio = radio, colEV = colEV, thickEV = thickEV
   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()


  self.indexColorSelected = 0.0
  for i=0,self._tiempos-1 DO BEGIN
      for j = 1,N_ELEMENTS(*self.indexOfRoots)-1 DO BEGIN
          dummy = where( (*((*self.rootByTimes)[i]))[*] eq (*self.indexOfRoots)[j])
          if(dummy[0] ne -1) then begin
            FOR k=0,N_ELEMENTS(dummy)-1 DO BEGIN
                  self->DrawDinamicLevels, vIndice = (*((*self.indexByTimes)[i]))[dummy[k]], vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 0b, vDrawNodes = 1b, vDrawLines = 0b            
            endfor
          endif
       endfor
      
       self._model->remove, /all
  endfor
end


pro C_s3DTrackingEstimator::DrawFromSelectedTrackingBase, radio = radio, colEV = colEV, thickEV = thickEV
   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()

  self.indexColorSelected = 0.0  
   vColor = fltArr(3)
   loadct, self.indexColorTable
   tvlct, r, g, b, /get

   if(N_ELEMENTS(*self.indexSelected) gt 1) then begin
      for k = 1, N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
          self.indexColorSelected = self.indexColorSelected + 255.0/float(self.numColorsSelected)
          vColor[0] = r[self.indexColorSelected < 255]
          vColor[1] = g[self.indexColorSelected < 255] 
          vColor[2] = b[self.indexColorSelected < 255] 

          (*self.masterTracking)[(*self.indexSelected)[k]]->setActualColor, vValue = vColor
          self->DrawStaticLevels, vIndice = (*self.indexSelected)[k], vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 1b, vDrawNodes = 0b, vDrawLines = 0b, vColor = vColor
      endfor
    endif
    s_WorldViewforModel, xdim = self._dimXYZ[0], ydim = self._dimXYZ[1] ,win =win,oClipboard = oClip, oScene = oSccene, oModel = self._model, oStaticInputModel = self._staticInputModel
    self._win = win
    self._oClipboard = oClip
    self._oScene = oSccene
    
    ; maldicion... la asignacion recursiva de modelos no funciona... todo el resto de la soperaciones recursivas si..
    ; el modelo se crea de forma recursiva pero no es procesable por las funciones draw....
    self._staticInputModel->remove, /all
    ;dibuja el centro del sistema
;    self._staticInputModel->add, obj_new('C_sOrb', POS= self._centroSistema, RADIUS=2.0*radio, xyzFact = self._factorXYZ, color = [117,117,117], thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
;                                name = strCompress('3D_Track_CENTER_'+string(0),/rem), uvalue = 'ObjInOriginalPosition')
        
   if(N_ELEMENTS(*self.indexSelected) gt 1) then begin
      for i=0,self._tiempos-1 DO BEGIN
          for j = 1, N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
              dummy = where( (*((*self.rootByTimes)[i]))[*] eq (*self.indexSelected)[j])
              if(dummy[0] ne -1) then begin
                FOR k=0,N_ELEMENTS(dummy)-1 DO BEGIN
                    self->DrawStaticLevels, vIndice = (*((*self.indexByTimes)[i]))[dummy[k]], vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 0b, vDrawNodes = 0b, vDrawLines = 1b
                endfor
              endif            
           endfor
      endfor
    endif
    
      o1 = self._win
      o2 = self._oClipboard
      o3 = self._oScene
      s_SaveViewforModel,win = o1,oClipboard = o2, oScene = o3, vPath = self._path, time = 'Initial', modelName = self._modelName
      self._win = o1
      self._oClipboard = o2
      self._oScene = o3
    
    loadct, 3
end


pro C_s3DTrackingEstimator::DrawFromSelectedTrackingMovie, radio = radio, colEV = colEV, thickEV = thickEV
   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()

  self.indexColorSelected = 0.0
   if(N_ELEMENTS(*self.indexSelected) gt 1) then begin
      for i=0,self._tiempos-1 DO BEGIN
          for j = 1, N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
              dummy = where( (*((*self.rootByTimes)[i]))[*] eq (*self.indexSelected)[j])
              if(dummy[0] ne -1) then begin
                FOR k=0,N_ELEMENTS(dummy)-1 DO BEGIN
                    self->DrawDinamicLevels, vIndice = (*((*self.indexByTimes)[i]))[dummy[k]], vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 0b, vDrawNodes = 1b, vDrawLines = 0b
                endfor
              endif            
           endfor

          o1 = self._win
          o2 = self._oClipboard
          o3 = self._oScene
          s_SaveViewforModel,win = o1,oClipboard = o2, oScene = o3, vPath = self._path, time = i, modelName = self._modelName
          self._win = o1
          self._oClipboard = o2
          self._oScene = o3


           self._model->remove, /all
      endfor
    endif
end


pro C_s3DTrackingEstimator::DrawFromSelectedTrackingMovieCreative, radio = radio, colEV = colEV, thickEV = thickEV
   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()

  self.indexColorSelected = 0.0
    self._staticInputModel->remove, /all

   if(N_ELEMENTS(*self.indexSelected) gt 1) then begin
      for i=0,self._tiempos-1 DO BEGIN
          for j = 1, N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
              dummy = where( (*((*self.rootByTimes)[i]))[*] eq (*self.indexSelected)[j])
              if(dummy[0] ne -1) then begin
                FOR k=0,N_ELEMENTS(dummy)-1 DO BEGIN
                    self->DrawDinamicLevelsCreative, vIndice = (*((*self.indexByTimes)[i]))[dummy[k]], vRadio = radio, colEV = colEV, thickEV = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 0b, vDrawNodes = 1b, vDrawLines = 1b
                endfor
              endif            
           endfor
          
            o1 = self._win
            o2 = self._oClipboard
            o3 = self._oScene
            s_SaveViewforModel,win = o1,oClipboard = o2, oScene = o3, vPath = self._path, time = i, modelName = self._modelName
            self._win = o1
            self._oClipboard = o2
            self._oScene = o3
           
           self._model->remove, /all
      endfor
    endif
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CALC and SAVE PArams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro C_s3DTrackingEstimator::CalcIndividualParamsGroup, vCalc = vCalc, vIndice = vIndice, vDistanciaCum = vDistanciaCum, param0 = param0, param1 = param1
   ; angulo division
   if(((*self.masterTracking)[vIndice]->getIsDivision() eq 1b) and (vCalc eq 4)) then begin
      angulo = (180.0/!PI)*(*self.masterTracking)[vIndice]->getAngleDiv()
      if(angulo lt 0.0) then begin
        angulo = 360.0 + angulo
      endif   
      PRINTF, 3, angulo
   endif

   axis = fltArr(3,2)
   distancia = fltArr(3)
   
   rootIndex = (*self.masterTracking)[vIndice]->getOwnRoot()
   axis[*,0] = (*self.masterTracking)[vIndice]->getCenter() - (*self.masterTracking)[rootIndex]->getCenter() 

   numConexiones = (*self.masterTracking)[vIndice]->getNumConexiones()
   conexiones = (*self.masterTracking)[vIndice]->getIndexConexiones()
   if(  (numConexiones eq 0) and $
        ((*self.masterTracking)[vIndice]->getOwnTime() - (*self.masterTracking)[rootIndex]->getOwnTime()) gt 80) then begin
        
        if(vCalc eq 0) then begin
            for f = 1, N_ELEMENTS(param0)-1 do begin
                PRINTF, 3, param0[f]        
            endfor
         endif

        if(vCalc eq 1) then begin
            for f = 1, N_ELEMENTS(param1)-1 do begin
                PRINTF, 3, param1[f]        
            endfor
         endif

        if(vCalc eq 2) then begin
           PRINTF, 3, sqrt(TOTAL(axis[*,0]*axis[*,0]))/vDistanciaCum
        endif
        
         if(vCalc eq 3) then begin
             distancia = axis[*,0]
             distancia[2] = 0.0
          if(abs(Total(distancia*distancia)) lt 0.00001) then begin
            ;PRINTF, 3, 0
          endif else begin
            distancia = distancia /sqrt(Total(distancia*distancia))
            angulo = acos(distancia[0])*180/!PI
            if(distancia[1] lt 0.0) then begin
              angulo = 360.0 - angulo
            endif
           PRINTF, 3, angulo
          endelse
        endif       
   endif

   tempVector0 = [-1]
   tempVector1 = [-1]
   tempVector2 = 0
   for j = 0, numConexiones-1 do begin
      if(j gt 0) then self.numColorsSelected = self.numColorsSelected + 1.0
      if(conexiones[j] ne -1) then begin
        rootIndex = (*self.masterTracking)[conexiones[j]]->getOwnRoot()
        axis[*,1] = (*self.masterTracking)[conexiones[j]]->getCenter() - (*self.masterTracking)[rootIndex]->getCenter()
        
        if(vCalc eq 0) then begin
          distancia = axis[*,1] - axis[*,0]
          distanciaM = sqrt(TOTAL(distancia*distancia))
          if(j eq 0) then begin
              tempVector0 = [param0, distanciaM]
          endif else begin
              tempVector0 = [distanciaM]
          endelse
        endif
        
        if(vCalc eq 1) then begin
          distancia = axis[*,1] - axis[*,0]
          distancia[2] = 0.0
          if(abs(Total(distancia*distancia)) lt 0.00001) then begin
            tempVector1 = param1
            ;PRINTF, 3, 0
          endif else begin
            distancia = distancia /sqrt(Total(distancia*distancia))
            angulo = acos(distancia[0])*180/!PI
            if(distancia[1] lt 0.0) then begin
              angulo = 360.0 - angulo
            endif
            
            ; correccion al control
            ;angulo = angulo + 180
            ;if(angulo gt 360.0) then angulo = 360.0 - angulo 
            if(j eq 0) then begin
                tempVector1 = [param1, angulo]
            endif else begin
                tempVector1 = [angulo]
            endelse
          endelse
        endif

        if(vCalc eq 2) then begin
          distancia = axis[*,1] - axis[*,0]
          distanciaM = sqrt(TOTAL(distancia*distancia))
            if(j eq 0) then begin
                tempVector2 = vDistanciaCum + distanciaM
            endif else begin
                tempVector2 = distanciaM
            endelse
                
        endif
        
        param0Interno = tempVector0
        param1Interno = tempVector1
        vDistanciaCumInterna = tempVector2
        self->CalcIndividualParamsGroup,vCalc = vCalc, vIndice = conexiones[j], vDistanciaCum = vDistanciaCumInterna, param0 = param0Interno, param1 = param1Interno
      endif
   endfor 
end


pro C_s3DTrackingEstimator::CalcIndividualParams, vIndice = vIndice, vDistanciaCum = vDistanciaCum, param0 = param0, param1 = param1
   axis = fltArr(3,2)
   distancia = fltArr(3)
   
   rootIndex = (*self.masterTracking)[vIndice]->getOwnRoot()
   axis[*,0] = (*self.masterTracking)[vIndice]->getCenter() - (*self.masterTracking)[rootIndex]->getCenter() 

   numConexiones = (*self.masterTracking)[vIndice]->getNumConexiones()
   conexiones = (*self.masterTracking)[vIndice]->getIndexConexiones()
   if(  (numConexiones eq 0) and $
        ((*self.masterTracking)[vIndice]->getOwnTime() - (*self.masterTracking)[rootIndex]->getOwnTime()) gt 80) then begin
        name = strCompress('_SPEEDS_for_' + $
                           'INITCELL_(index_' + strCompress(rootIndex)+ $ 
                                      '_time_' + strCompress((*self.masterTracking)[rootIndex]->getOwnTime())+ $
                                      '_number_' + strCompress((*self.masterTracking)[rootIndex]->getOwnIndex())+ $
                           ')__ENDCELL_(index_' + strCompress(vIndice)+ $ 
                                      '_time_' + strCompress((*self.masterTracking)[vIndice]->getOwnTime())+ $
                                      '_number_' + strCompress((*self.masterTracking)[vIndice]->getOwnIndex())+ $
                           '.sav', /rem)
         filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep()+name, /rem))
         if(N_ELEMENTS(param0) gt 1) then begin
            openw, 3, filename
            for f = 1, N_ELEMENTS(param0)-1 do begin
                PRINTF, 3, param0[f]        
            endfor
            close, 3
         endif


        name = strCompress('_ANGLES_for_' + $
                           'INITCELL_(index_' + strCompress(rootIndex)+ $ 
                                      '_time_' + strCompress((*self.masterTracking)[rootIndex]->getOwnTime())+ $
                                      '_number_' + strCompress((*self.masterTracking)[rootIndex]->getOwnIndex())+ $
                           ')__ENDCELL_(index_' + strCompress(vIndice)+ $ 
                                      '_time_' + strCompress((*self.masterTracking)[vIndice]->getOwnTime())+ $
                                      '_number_' + strCompress((*self.masterTracking)[vIndice]->getOwnIndex())+ $
                           '.sav', /rem)
         filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep() + name, /rem))
         
         if(N_ELEMENTS(param1) gt 1) then begin
            openw, 3, filename
            for f = 1, N_ELEMENTS(param1)-1 do begin
                PRINTF, 3, param1[f]
            endfor
            close, 3
         endif


        name = strCompress('_PERSISTENCE_for_' + $
                           'INITCELL_(index_' + strCompress(rootIndex)+ $ 
                                      '_time_' + strCompress((*self.masterTracking)[rootIndex]->getOwnTime())+ $
                                      '_number_' + strCompress((*self.masterTracking)[rootIndex]->getOwnIndex())+ $
                           ')__ENDCELL_(index_' + strCompress(vIndice)+ $ 
                                      '_time_' + strCompress((*self.masterTracking)[vIndice]->getOwnTime())+ $
                                      '_number_' + strCompress((*self.masterTracking)[vIndice]->getOwnIndex())+ $
                           '.sav', /rem)
         filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep() + name, /rem))                           
         openw, 3, filename                           
           PRINTF, 3, sqrt(TOTAL(axis[*,0]*axis[*,0]))/vDistanciaCum
         close, 3                
   endif
   
   for j = 0, numConexiones-1 do begin
      if(j gt 0) then self.numColorsSelected = self.numColorsSelected + 1.0
      if(conexiones[j] ne -1) then begin
        rootIndex = (*self.masterTracking)[conexiones[j]]->getOwnRoot()
        axis[*,1] = (*self.masterTracking)[conexiones[j]]->getCenter() - (*self.masterTracking)[rootIndex]->getCenter()
        
          distancia = axis[*,1] - axis[*,0]
          distanciaM = sqrt(TOTAL(distancia*distancia))
          param0Interno = [param0, distanciaM]


          distancia = axis[*,1] - axis[*,0]
          distancia[2] = 0.0
          if(abs(Total(distancia*distancia)) lt 0.00001) then begin
            ;PRINTF, 3, 0
          endif else begin
            distancia = distancia /sqrt(Total(distancia*distancia))
            angulo = acos(distancia[0])*180/!PI 
            if(distancia[1] lt 0.0) then begin
              angulo = 360.0 - angulo
            endif

            param1Interno = [param1, angulo]
          endelse


          distancia = axis[*,1] - axis[*,0]
          distanciaM = sqrt(TOTAL(distancia*distancia))
          vDistanciaCumInterna = vDistanciaCum + distanciaM      
        
        self->CalcIndividualParams, vIndice = conexiones[j], vDistanciaCum = vDistanciaCumInterna, param0 = param0Interno, param1 = param1Interno
      endif
   endfor 
end


pro C_s3DTrackingEstimator::CalculatesParamsFromIndividualSelectedTrackingBase
; vCalc = 0 .... distances
;vcalc = 1.. angles
  self.numColorsSelected = 0.0
  
  if(FILE_TEST(strCompress(self._path+'_Tracking'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+'_Tracking'+ path_sep())
  if(FILE_TEST(strCompress(self._path+'_Tracking'+ path_sep()+'_Params'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+'_Tracking'+ path_sep()+'_Params'+ path_sep())

   if(N_ELEMENTS(*self.indexSelected) gt 1) then begin
      ;for k = 1,N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
      ;   vectorAngulos = [-1]
      ;   vectorMovs    = [-1]   
      ;   self->CalcIndividualParams, vIndice = (*self.indexSelected)[k], vDistanciaCum = 0.0, param0 = vectorMovs, param1 = vectorAngulos
      ;endfor

     vCalc = 4
     name = strCompress('___Divisions' + '.sav', /rem)
     filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep() + name, /rem))
     openw, 3, filename                
      for k = 1,N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
         vectorAngulos = [-1]
         vectorMovs    = [-1]   
         self->CalcIndividualParamsGroup, vCalc = vCalc, vIndice = (*self.indexSelected)[k], vDistanciaCum = 0.0, param0 = vectorMovs, param1 = vectorAngulos
      endfor
     close, 3

     vCalc = 3
     name = strCompress('___AngleEnd' + '.sav', /rem)
     filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep() + name, /rem))
     openw, 3, filename                
      for k = 1,N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
         vectorAngulos = [-1]
         vectorMovs    = [-1]   
         self->CalcIndividualParamsGroup, vCalc = vCalc, vIndice = (*self.indexSelected)[k], vDistanciaCum = 0.0, param0 = vectorMovs, param1 = vectorAngulos
      endfor
     close, 3
     
     vCalc = 2
     name = strCompress('___PERSISTENCE' + '.sav', /rem)
     filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep() + name, /rem))
     openw, 3, filename                
      for k = 1,N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
         vectorAngulos = [-1]
         vectorMovs    = [-1]   
         self->CalcIndividualParamsGroup, vCalc = vCalc, vIndice = (*self.indexSelected)[k], vDistanciaCum = 0.0, param0 = vectorMovs, param1 = vectorAngulos
      endfor
     close, 3

     vCalc = 1
     name = strCompress('___ANGLE' + '.sav', /rem)
     filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep() + name, /rem))
     openw, 3, filename                
      for k = 1,N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
         vectorAngulos = [-1]
         vectorMovs    = [-1]   
         self->CalcIndividualParamsGroup, vCalc = vCalc, vIndice = (*self.indexSelected)[k], vDistanciaCum = 0.0, param0 = vectorMovs, param1 = vectorAngulos
      endfor
     close, 3

     vCalc = 0
     name = strCompress('___SPPED' + '.sav', /rem)
     filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep() + name, /rem))
     openw, 3, filename                
      for k = 1,N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
         vectorAngulos = [-1]
         vectorMovs    = [-1]   
         self->CalcIndividualParamsGroup, vCalc = vCalc, vIndice = (*self.indexSelected)[k], vDistanciaCum = 0.0, param0 = vectorMovs, param1 = vectorAngulos
      endfor
     close, 3

   endif
   
   vectorAngulos = [-1]
   vectorMovs    = [-1]   
end 


pro C_s3DTrackingEstimator::CalcParams, vCalc = vCalc, vIndice = vIndice, vAngulosInternos=vAngulosInternos, $
                                        vDistTotalPerTime=vDistTotalPerTime, filename = filename, indexTemp=indexTemp, vAllParam = vAllParam

   deltaMov             = 0  ;Desplazamiento minimo valido
   distmin              = 50 ;Distancia minima recorrida por una trajectoria para ser considerada
   ;vDistanciaCumInterna = vDistanciaCum
   if vAllParam eq 0b then vAngulosCumInternos  = vAngulosInternos
   ; angulo division
   if(((*self.masterTracking)[vIndice]->getIsDivision() eq 1b) and (vCalc eq 4) and (total(vDistTotalPerTime) gt 3.0*deltaMov)) then begin
      if vAllParam eq 0 then openw, 3, filename
      angulo = (180.0/!PI)*(*self.masterTracking)[vIndice]->getAngleDiv()
      if(angulo lt 0.0) then begin
        angulo = 360.0 + angulo
      endif
      if(angulo gt 180.0) then begin
        angulo = 180.0 - angulo
      endif      
      if(angulo gt 90.0) then begin
        angulo = 180.0 - angulo
      endif
      PRINTF, 3, angulo
   endif

   axis = fltArr(3,2)
   distancia = fltArr(3)
   
   rootIndex = (*self.masterTracking)[vIndice]->getOwnRoot()
   axis[*,0] = (*self.masterTracking)[vIndice]->getCenter() - (*self.masterTracking)[rootIndex]->getCenter() 
   
   numConexiones = (*self.masterTracking)[vIndice]->getNumConexiones()
   conexiones = (*self.masterTracking)[vIndice]->getIndexConexiones()
   if((numConexiones eq 0) and (abs(total(vDistTotalPerTime)) ne 0.0)) then begin
      distanciaM = sqrt(TOTAL(axis[*,0]*axis[*,0]))
      if(distanciaM/total(vDistTotalPerTime) ne 1.0) then begin
         if((vCalc eq 2) and (total(vDistTotalPerTime) ge distmin)) then begin
            if vAllParam eq 0b then openw, 3, filename
            PRINTF, 3, sqrt(TOTAL(axis[*,0]*axis[*,0]))/total(vDistTotalPerTime)
         endif
         if((vCalc eq 3) and (distanciaM gt deltaMov) and (total(vDistTotalPerTime) gt distmin)) then begin
            distancia = axis[*,0]
            distancia[2] = 0.0
            if(abs(Total(distancia*distancia)) lt 0.001) then begin
              ;PRINTF, 3, 0
            endif else begin
              distancia = distancia /sqrt(Total(distancia*distancia))
              angulo = acos(distancia[0])*180/!PI
              if(distancia[1] lt 0.0) then begin
                angulo = 360.0 - angulo
              endif
             if vAllParam eq 0b then openw, 3, filename
             PRINTF, 3, angulo
            endelse
         endif
      endif 
   endif
   
   for j = 0L, numConexiones-1 do begin
      if(j gt 0) then self.numColorsSelected = self.numColorsSelected + 1.0
      if(conexiones[j] ne -1) then begin
        rootIndex = (*self.masterTracking)[conexiones[j]]->getOwnRoot()
        axis[*,1] = (*self.masterTracking)[conexiones[j]]->getCenter() - (*self.masterTracking)[rootIndex]->getCenter()

        distancia = axis[*,1] - axis[*,0]
        distanciaM = sqrt(TOTAL(distancia*distancia))
        if(vCalc eq 0) then begin
          if(distanciaM ge deltaMov) then begin
            if vAllParam eq 1b then PRINTF, 3, distanciaM
            if where(indexTemp eq rootIndex) eq -1 then indexTemp=[indexTemp,rootIndex,conexiones[j]] else indexTemp=[indexTemp,conexiones[j]]
            (*self.masterTracking)[conexiones[j]]->setInView, vValue = 1b
            (*self.masterTracking)[rootIndex]->setInView, vValue = 1b
          endif
        endif
        if(vCalc eq 1) then begin
          distancia[2] = 0.0
          if(abs(Total(distancia*distancia)) lt 0.00001) then begin
            ;PRINTF, 3, 0
          endif else begin
            distancia = distancia /sqrt(Total(distancia*distancia))
            angulo = acos(distancia[0])*180/!PI 
            if(distancia[1] lt 0.0) then begin
              angulo = 360.0 - angulo
            endif
            if(distanciaM gt deltaMov) then begin
              if vAllParam eq 1b then PRINTF, 3, angulo else $
              vAngulosCumInternos = [vAngulosInternos,angulo]
            endif
          endelse
        endif
        ;vDistanciaCumInterna = vDistanciaCum + distanciaM    
        vDistTotalPerTime = [vDistTotalPerTime, distanciaM ]
        self->CalcParams, vCalc = vCalc, vIndice = conexiones[j], vAngulosInternos=vAngulosCumInternos, $
                                         vDistTotalPerTime=vDistTotalPerTime, filename = filename, indexTemp=indexTemp, vAllParam = vAllParam
      endif
   endfor 
   if ((vCalc eq 0) and (total(vDistTotalPerTime) lt distmin) and (numConexiones eq 0)) then begin
      for i=1,n_elements(indexTemp)-1 do begin
          (*self.masterTracking)[indexTemp[i]]->setInView, vValue = 0b
      endfor
   endif
   if((vCalc eq 0) and (total(vDistTotalPerTime) ge distmin) and (numConexiones eq 0) and (vAllParam eq 0b)) then begin
      openw, 3, filename
      PRINTF, 3, vDistTotalPerTime[1:*], format = strcompress('(',/rem) + strcompress(string(1),/rem) + strcompress('F10.4',/rem) +strcompress(')' ,/rem)
   endif
   if((vCalc eq 1) and (total(vDistTotalPerTime) ge distmin) and (numConexiones eq 0) and (vAllParam eq 0b)) then begin
      openw, 3, filename
      PRINTF, 3, vAngulosCumInternos[1:*], format = strcompress('(',/rem) + strcompress(string(1),/rem) + strcompress('F10.4',/rem) +strcompress(')' ,/rem)
   endif
   if vAllParam eq 0b then close, 3
end


pro C_s3DTrackingEstimator::CalculatesParamsFromAllTrackingBase, vCalc = vCalc

  self.numColorsSelected = 0.0
  ; vCalc = 0 .... distances
  ;vcalc = 1.. angles
  
  if(FILE_TEST(strCompress(self._path+'_Tracking'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+'_Tracking'+ path_sep())
  if(FILE_TEST(strCompress(self._path+'_Tracking'+ path_sep()+'_Params'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+'_Tracking'+ path_sep()+'_Params'+ path_sep())
  
  for indexRoot=1, N_ELEMENTS(*self.indexOfRoots)-1 DO BEGIN
     self.numColorsSelected = self.numColorsSelected + 1.0
     if(vCalc eq 0) then begin
        name = strCompress('EstimatedTracking_SPEEDS_forTrayectory_' + strCompress(indexRoot) + '.sav', /rem)
     endif
     if(vCalc eq 1) then begin
        name = strCompress('EstimatedTracking_ANGLES_forTrayectory_' + strCompress(indexRoot) + '.sav', /rem)
     endif
     if(vCalc eq 2) then begin
        name = strCompress('EstimatedTracking_PERSISTENCE_forTrayectory_' + strCompress(indexRoot) + '.sav', /rem)
     endif     
     if(vCalc eq 4) then begin
        name = strCompress('EstimatedTracking_ANG_DIVISIONS_forTrayectory_' + strCompress(indexRoot) + '.sav', /rem)
     endif     

     filename = strCompress(self._path+strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep() + name, /rem))
     openw, 3, filename                
     self->CalcParams, vCalc = vCalc, vIndice = (*self.indexOfRoots)[indexRoot], vDistanciaCum = 0.0
     close, 3
  endfor
end 


pro C_s3DTrackingEstimator::CalculatesJointParamsFromAllTrackingBase, vCalc = vCalc
; vCalc = 0 .... distances
;vcalc = 1.. angles
  self.numColorsSelected = 0.0
  
  if(FILE_TEST(strCompress(self._path + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+ path_sep())
  if(FILE_TEST(strCompress(self._path + path_sep()+'_Params'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+ path_sep()+'_Params'+ path_sep())
  if(FILE_TEST(strCompress(self._path + path_sep()+'_Params'+ path_sep()+'_ParamsForTrajectory'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+ path_sep()+'_Params'+ path_sep()+'_ParamsForTrajectory'+ path_sep())

   if(vCalc eq 0) then begin
      name = strCompress('EstimatedTracking_SPEEDS_forALLTrayectory_' + '.dat', /rem)
   endif
   if(vCalc eq 1) then begin
      name = strCompress('EstimatedTracking_ANGLES_forALLTrayectory_' + '.dat', /rem)
   endif
   if(vCalc eq 2) then begin
      name = strCompress('EstimatedTracking_PERSISTENCE_forALLTrayectory_' + '.dat', /rem)
   endif     
   if(vCalc eq 3) then begin
      name = strCompress('EstimatedTracking_AnglesEND_forALLTrayectory_' + '.dat', /rem)
   endif     
   if(vCalc eq 4) then begin
      name = strCompress('EstimatedTracking_ANG_DIVISIONS_forALLTrayectory_' + '.dat', /rem)
   endif     
  
   filename = strCompress(self._path+strcompress(path_sep()+'_Params'+ path_sep() + name, /rem))
   openw, 3, filename  

   for indexRoot=1, N_ELEMENTS(*self.indexOfRoots)-1 DO BEGIN
                
      self->CalcParams, vCalc = vCalc, vIndice = (*self.indexOfRoots)[indexRoot], vDistTotalPerTime = 0.0, indexTemp=-1, vAllParam = 1b
    
   endfor
   close, 3
    
;    ;guarda parametros por cada trajectoria
;    for indexRoot=1, N_ELEMENTS(*self.indexOfRoots)-1 DO BEGIN
;       if(vCalc eq 0) then begin
;          name = strCompress('EstimatedTracking_SPEEDS_forALLTrayectory_'+ strcompress(indexRoot)+ '.dat', /rem)
;       endif
;       if(vCalc eq 1) then begin
;          name = strCompress('EstimatedTracking_ANGLES_forALLTrayectory_' +strcompress(indexRoot)+ '.dat', /rem)
;       endif
;       if(vCalc eq 2) then begin
;          name = strCompress('EstimatedTracking_PERSISTENCE_forALLTrayectory_' +strcompress(indexRoot)+ '.dat', /rem)
;       endif     
;       if(vCalc eq 3) then begin
;          name = strCompress('EstimatedTracking_AnglesEND_forALLTrayectory_' +strcompress(indexRoot)+ '.dat', /rem)
;       endif     
;       if(vCalc eq 4) then begin
;          name = strCompress('EstimatedTracking_ANG_DIVISIONS_forALLTrayectory_' +strcompress(indexRoot)+ '.dat', /rem)
;       endif   
;                
;       filename = strCompress(self._path+strcompress(path_sep()+'_Params'+ path_sep()+'_ParamsForTrajectory'+ path_sep() + name))
;       
;       self->CalcParams, vCalc = vCalc, vIndice = (*self.indexOfRoots)[indexRoot], vAngulosInternos=0.0, $
;                         vDistTotalPerTime=0.0, filename = filename, indexTemp=-1, vAllParam = 0b
;    endfor
  
end


pro C_s3DTrackingEstimator::CalculatesParamsFromSelectedTrackingBase, vCalc = vCalc
; vCalc = 0 .... distances
;vcalc = 1.. angles
  self.numColorsSelected = 0.0
  
  if(FILE_TEST(strCompress(self._path+'_Tracking'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+'_Tracking'+ path_sep())
  if(FILE_TEST(strCompress(self._path+'_Tracking'+ path_sep()+'_Params'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(self._path+'_Tracking'+ path_sep()+'_Params'+ path_sep())

   if(N_ELEMENTS(*self.indexSelected) gt 1) then begin
      for k = 1,N_ELEMENTS(*self.indexSelected)-1 DO BEGIN
           if(vCalc eq 0) then begin
              name = strCompress('EstimatedTracking_SPEEDS_forTrayectorie_' + strCompress(k) + '.sav', /rem)
           endif
           if(vCalc eq 1) then begin
              name = strCompress('EstimatedTracking_ANGLES_forTrayectory_' + strCompress(k) + '.sav', /rem)
           endif
           if(vCalc eq 2) then begin
              name = strCompress('EstimatedTracking_PERSISTENCE_forTrayectory_' + strCompress(k) + '.sav', /rem)
           endif     
           
           if(vCalc eq 4) then begin
              name = strCompress('EstimatedTracking_DIVISIONS_forTrayectory_' + strCompress(k) + '.sav', /rem)
           endif     
           
           filename = strCompress(self._path +strcompress('_Tracking'+ path_sep()+'_Params'+ path_sep()+ name, /rem))
           openw, 3, filename                
           self->CalcParams, vCalc = vCalc, vIndice = (*self.indexSelected)[k], vDistanciaCum = 0.0
           close, 3
      endfor
    endif
end 


function C_s3DTrackingEstimator::GetmasterTracking
    return, self.masterTracking
end


pro C_s3DTrackingEstimator::cleanup
  if(OBJ_VALID(self._staticInputModel)) then self._staticInputModel->remove, /all
  if(OBJ_VALID(self._model)) then self._model->remove, /all

  ptr_free, self.indexOfRoots
  ptr_free, self.indexSelected

   for i = 0, self._tiempos-1 do begin
      ptr_free, (*self.indexByTimes)[i]
      ptr_free, (*self.numberByTimes)[i]
      ptr_free, (*self.rootByTimes)[i]
   endfor
   ptr_free, self.indexByTimes
   ptr_free, self.numberByTimes
   ptr_free, self.rootByTimes

   for i = 0, N_ELEMENTS(*self.masterTracking) -1 do begin
      OBJ_DESTROY, (*self.masterTracking)[i]
   endfor
  ptr_free, self.masterTracking   
end


pro C_s3DTrackingEstimator::setVision, vVision = vVision
  self._vision = vVision
end

function C_s3DTrackingEstimator::init
    ; Parameters of C_s3DTrackingEstimator.
    self._staticInputModel = obj_new('IDLgrModel', uValue = 'oPhaseModel')    
    self._model = obj_new('IDLgrModel', uValue = 'oPhaseModel')
    
    self.indexColorTable = 38
    dummy = [-1]
    self.indexSelected  = ptr_new(dummy,/no_copy)
    dummy = [-1]    
    self.indexOfRoots  = ptr_new(dummy,/no_copy)
    return, 1
end

pro C_s3DTrackingEstimator__define
   temp = { C_s3DTrackingEstimator, masterTracking : ptr_new(),$
                                    indexOfRoots  :  ptr_new(),$
                                    indexByTimes  :  ptr_new(),$
                                    numberByTimes :  ptr_new(),$ ; pointer to roi indexes vector
                                    rootByTimes   :  ptr_new(),$
                                    indexSelected :  ptr_new(),$
                                    
                                    numColorsSelected  :  0.0,$
                                    indexColorSelected :  0.0,$
                                    indexColorTable    :  38,$
                                    _centroSistema     :  fltArr(3),$
                                    
                                    _staticInputModel  : obj_new(),$
                                    _model             : obj_new(),$ 
                                   

                                    _pParamStruct      : ptr_new(),$
                                    _stack_tlb         : 0L ,$
                                    _pGroupReference   : ptr_new(),$                                 
                                    _clusPos           : 0L ,$
                                    _chPos             : 0L ,$
                                    _tPos              : 0L ,$
                                    _tiempos           : 0L, $
                                                                       
                                    _factorXYZ         : 1.0d,$
                                    _maximoObjetos     : 0, $
                                    _vision            : 5, $
              
                                    _dimXYZ            : [1,1,1], $                                                           
                                    _win               : OBJ_NEW(), $
                                    _oClipboard        : OBJ_NEW(), $
                                    _oScene            : OBJ_NEW(), $
                                    _path              : 'c:\', $
                                    _modelName         : '3DTracking'}
end
