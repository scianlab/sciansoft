
pro C_s3DGraphSkelDir::MasterGraphCreation, vEdgeRoot = vEdgeRoot, vVertices = vVertices, vAristas = vAristas, $
          xyzSizePerPixel = xyzSizePerPixel, vFlip = vFlip, fAllowLoops=fAllowLoops

  self.verticesOrigen  = ptr_new(vVertices)

  tmpIndices           = dblarr(n_elements(vAristas[0,*])) * 0.0d
  self.aristasListas   = ptr_new(tmpIndices, /no_copy)
  self.aristasOrigen   = ptr_new(vAristas)
  self.xyzSizePerPixel = xyzSizePerPixel
  self.fAllowLoops     = 0b
  
  if keyword_set(fAllowLoops) then self.fAllowLoops     = fAllowLoops
  
  self->GenerateNodesSystem, vEdgeRoot = vEdgeRoot, vFlip = vFlip

  
end

pro C_s3DGraphSkelDir::GenerateNodesSystem, vEdgeRoot = vEdgeRoot, vFlip = vFlip
;verticesFinales[3,nAristas]
;aristasFinales[2,Edges] ; parseo propio para facilitar los recorridos...

    indiceParent = 0L
    levelActual = 0L
    
    if(vFlip) then begin
      indexInit = (*self.aristasOrigen)[0,vEdgeRoot]
      indexEnd  = (*self.aristasOrigen)[1,vEdgeRoot]
    endif else begin
      indexInit = (*self.aristasOrigen)[1,vEdgeRoot]
      indexEnd  = (*self.aristasOrigen)[0,vEdgeRoot]
    endelse
    
    pointInit = (*self.verticesOrigen)[*,indexInit]
    pointEnd  = (*self.verticesOrigen)[*,indexEnd]
    angulo    = 0.0
    
    normalParent = pointEnd - pointInit
    DOTvector = normalParent*normalParent
    largo     = sqrt(total(DOTvector))
    largoReal = sqrt(total(DOTvector*self.xyzSizePerPixel))  
    
    if(sqrt(total(DOTvector)) gt 0.0001) then normalParent = normalParent / sqrt(total(DOTvector))
    
    edgeBase = obj_new('C_s3GraphEdge') ; Este sera root
    edgeBase->setActualColor, vValue = [255,255,255]
    edgeBase->setIsRoot,     vValue = 1b
    edgeBase->setInitXYZ,    vValue = pointInit
    edgeBase->setEndXYZ,     vValue = pointEnd
        
    edgeBase->setAngleEdge,   vValue  = angulo
    edgeBase->setLenghtEdge,  vValue  = largo
    edgeBase->setLenghtEdgeReal, vValue = largoReal
    
    edgeBase->setOwnRoot,  vValue  = -1
    edgeBase->setOwnIndex,  vValue =  indiceParent
    edgeBase->setOwnParent, vValue = -1 
    edgeBase->setOwnLevel,  vValue =  levelActual
    
    edgeBase->setAngleMeanChildren, vValue = 0.0
    edgeBase->setLenghtMeanChildren, vValue = 0.0    
    
    edgeBase->setOwnInitVertex, vValue = indexInit
    edgeBase->setOwnEndVertex, vValue = indexEnd
    edgeBase->setOwnNormal, vValue = normalParent

    edgeBase->setWaiting, vValue = 1b
    self.masterGraphSkel = ptr_new(edgeBase,/no_copy)
    (*self.aristasListas)[vEdgeRoot] = 1.0

    vPendientes = 1b
    while vPendientes do begin
       vPendientes = 0b
       numNodosActual = N_ELEMENTS(*self.masterGraphSkel) -1
       for i = 0, numNodosActual do begin
          if((*self.masterGraphSkel)[i]->getWaiting()) then begin
            indexInit    = (*self.masterGraphSkel)[i]->getOwnInitVertex()
            indexEnd     = (*self.masterGraphSkel)[i]->getOwnEndVertex()
            levelActual  = (*self.masterGraphSkel)[i]->getOwnLevel()
            indiceParent = (*self.masterGraphSkel)[i]->getOwnIndex()
            normalParent = (*self.masterGraphSkel)[i]->getOwnNormal()
            self->CreateChildrenLimited, indexInit = indexInit, indexEnd = indexEnd, levelActual = levelActual, indiceParent = indiceParent, normalParent = normalParent, freePass = 1b
            (*self.masterGraphSkel)[i]->setWaiting, vValue = 0b
          endif
       endfor

       ;while any node is waiting 
       numNodosActual = N_ELEMENTS(*self.masterGraphSkel) -1
       for i = 0, numNodosActual do begin
          if((*self.masterGraphSkel)[i]->getWaiting()) then begin
              vPendientes = 1b
          endif
       endfor
    endwhile

    ; ponderar angulos
    numNodosActual = N_ELEMENTS(*self.masterGraphSkel) -1
    for i = 0, numNodosActual do begin
      numConexionesParent = (*self.masterGraphSkel)[i]->getNumConexiones()
      if(numConexionesParent gt 0) then begin
        cumAngulo = (*self.masterGraphSkel)[i]->getAngleMeanChildren()
        cumLargo  = (*self.masterGraphSkel)[i]->getLenghtMeanChildren()
        (*self.masterGraphSkel)[i]->setAngleMeanChildren,  vValue = cumAngulo / numConexionesParent
        (*self.masterGraphSkel)[i]->setLenghtMeanChildren, vValue = cumLargo  / numConexionesParent
      endif
    endfor
    
    print, 'End Graph creation....'   
end

function C_s3DGraphSkelDir::SelectedParameter, vSelectedParam = vSelectedParam, vActualNode = vActualNode
    actualParam = 0
    
      case vSelectedParam of
        0: begin        ; '3D GraphSkel Number Edge'
          actualParam = (*self.masterGraphSkel)[vActualNode]->getOwnIndex()
        endcase
        1: begin        ; '3D GraphSkel Bifurc Number',$      
          actualParam = (*self.masterGraphSkel)[vActualNode]->getNumConexiones()
        endcase
        2: begin        ; '3D GraphSkel Edge Lenght [Voxel]'
          actualParam = (*self.masterGraphSkel)[vActualNode]->getLenghtEdge()
        endcase
        3: begin        ; '3D GraphSkel Edge Lenght [x²]'
          actualParam = (*self.masterGraphSkel)[vActualNode]->getLenghtEdgeReal()
        endcase
        4: begin        ; '3D GraphSkel Edge Angle [Degrees]'
          actualParam = (*self.masterGraphSkel)[vActualNode]->getAngleEdge()
        endcase
        5: begin        ;'3D GraphSkel Deep Level'$
          actualParam = (*self.masterGraphSkel)[vActualNode]->getOwnLevel()
        endcase
        6: begin
          ;actualParam = (*self.masterGraphSkel)[vActualNode]->
          ;TODO
          ;real length
        endcase
        7: begin
          ;TODO
          ;volume
          ;actualParam = (*self.masterGraphSkel)[vActualNode]->
        endcase
        8: begin
          ;actualParam = (*self.masterGraphSkel)[vActualNode]->
        endcase
        else: begin
          actualParam = 0
        endcase
      endcase
    
    return, actualParam
end

function C_s3DGraphSkelDir::MINMAXforParameter, vSelectedParam = vSelectedParam
    minmaxParam = [10000000L,-1000000L]
    actualParam = 0
    
    numNodosActual = N_ELEMENTS(*self.masterGraphSkel) -1
    for i = 0, numNodosActual do begin
      case vSelectedParam of
        0: begin        ; '3D GraphSkel Number Edge'
          actualParam = (*self.masterGraphSkel)[i]->getOwnIndex()
        endcase
        1: begin        ; '3D GraphSkel Bifurc Number',$      
          actualParam = (*self.masterGraphSkel)[i]->getNumConexiones()
        endcase
        2: begin        ; '3D GraphSkel Edge Lenght [Voxel]'
          actualParam = (*self.masterGraphSkel)[i]->getLenghtEdge()
        endcase
        3: begin        ; '3D GraphSkel Edge Lenght [x²]'
          actualParam = (*self.masterGraphSkel)[i]->getLenghtEdgeReal()
        endcase
        4: begin        ; '3D GraphSkel Edge Angle [Degrees]'
          actualParam = (*self.masterGraphSkel)[i]->getAngleEdge()
        endcase
        5: begin        ;'3D GraphSkel Deep Level'$
          actualParam = (*self.masterGraphSkel)[i]->getOwnLevel()
        endcase
        6: begin
          ;actualParam = (*self.masterGraphSkel)[i]->
        endcase
        7: begin
          ;actualParam = (*self.masterGraphSkel)[i]->
        endcase
        8: begin
          ;actualParam = (*self.masterGraphSkel)[i]->
        endcase
        else: begin
          actualParam = 0
        endcase
      endcase
        minmaxParam[0] = (actualParam lt minmaxParam[0])? actualParam: minmaxParam[0]
        minmaxParam[1] = (actualParam gt minmaxParam[1])? actualParam: minmaxParam[1]
    endfor
    
    return, minmaxParam
end

pro C_s3DGraphSkelDir::CreateChildrenLimited, indexInit = indexInit, indexEnd = indexEnd, levelActual = levelActual, indiceParent = indiceParent, normalParent = normalParent, freePass = freePass
    
    if(indiceParent ne -1) then begin
      cumAngulo = (*self.masterGraphSkel)[indiceParent]->getAngleMeanChildren()
      cumLargo  = (*self.masterGraphSkel)[indiceParent]->getLenghtMeanChildren()
    endif else begin
      cumAngulo = 0.0
      cumLargo  = 0.0
    endelse
    ; Buscar edges que comparten Nodo Final ... para seguir con la pachamama
      wNodesOK = [-1]
      wNodesInit = where( ((*self.aristasOrigen)[0,*] eq indexEnd) and ((*self.aristasOrigen)[1,*] ne indexInit),/L64)
      wNodesEnd  = where( ((*self.aristasOrigen)[1,*] eq indexEnd) and ((*self.aristasOrigen)[0,*] ne indexInit),/L64)

      if(  (wNodesInit[0] ne -1) and (wNodesEnd[0] ne -1)) then begin
        wNodesOK = [wNodesInit,wNodesEnd]
      endif else begin
         if(wNodesInit[0] ne -1) then begin
            wNodesOK = [wNodesInit]
         endif
         if(wNodesEnd[0] ne -1) then begin
            wNodesOK = [wNodesEnd]
         endif
      endelse

      numEdges = 0L 
      if(wNodesOK[0] ne -1) then numEdges = N_ELEMENTS(wNodesOK)
    
    levelActualLocal = levelActual + 1L
    
    if((freePass eq 0b) and (numEdges gt 1) ) then begin
      (*self.masterGraphSkel)[indiceParent]->setWaiting, vValue = 1b 
      ;after first call only go through the same edge is a valid
    endif else begin
      ;recorre todos los arcos asociados a este nodo derecho
      for edgeIndex = 0L, numEdges-1L do begin
            if( (*self.aristasListas)[wNodesOK[edgeIndex]] ne 1b) then begin    
                edgeActual = obj_new('C_s3GraphEdge'); este es derivado del root   
      
                indexInitChild = indexEnd
                par = (*self.aristasOrigen)[*,wNodesOK[edgeIndex]]
                indexEndChild  = par[0] eq indexInitChild? par[1]: par[0]
                
                pointInit = (*self.verticesOrigen)[*,indexInitChild]
                pointEnd  = (*self.verticesOrigen)[*,indexEndChild]
      
                normalChild = pointEnd - pointInit
                DOTvector   = normalChild*normalChild
                largo       = sqrt(total(DOTvector))
                largoReal   = sqrt(total(DOTvector*self.xyzSizePerPixel))  
                
                if( largo gt 0.0001) then normalChild = normalChild / largo
      
                if((sqrt(total(normalParent*normalParent)) gt 0.0001) and (largo gt 0.0001)) then begin
                    angulo = (180.0/!PI)*acos(total(normalParent*normalChild))
                endif else begin
                    angulo = 0.0
                endelse 
                
                DOTvector = (pointEnd - pointInit)*(pointEnd - pointInit)
                largo     = sqrt(total(DOTvector))
                largoReal = sqrt(total(DOTvector*self.xyzSizePerPixel))  
      
                edgeActual->setActualColor, vValue = [255,255,255]
                edgeActual->setIsRoot, vValue = 0b
                edgeActual->setInitXYZ, vValue = pointInit
                edgeActual->setEndXYZ, vValue = pointEnd
                    
                edgeActual->setAngleEdge, vValue  = angulo
                edgeActual->setLenghtEdge, vValue = largo
                edgeActual->setLenghtEdgeReal, vValue = largoReal
                
                edgeActual->setOwnRoot,  vValue  = 0 ; irrelevante... no se porque no lo borre en su momento... sorry...
                ownIndexTemp = N_ELEMENTS(*self.masterGraphSkel)
                edgeActual->setOwnIndex,  vValue = ownIndexTemp  
                edgeActual->setOwnParent, vValue = indiceParent 
                edgeActual->setOwnLevel,  vValue = levelActualLocal
             
                cumAngulo = cumAngulo + angulo
                cumLargo  = cumLargo  + largo
      
                edgeActual->setAngleMeanChildren, vValue = 0.0
                edgeActual->setLenghtMeanChildren, vValue = 0.0    
                
                edgeActual->setOwnInitVertex, vValue = indexInitChild
                edgeActual->setOwnEndVertex, vValue = indexEndChild
                edgeActual->setOwnNormal, vValue = normalChild
            
                self.masterGraphSkel = ptr_new([*self.masterGraphSkel , edgeActual],/no_copy)
                
                if(indiceParent ne -1) then begin
                  (*self.masterGraphSkel)[indiceParent]->AddIndexConexiones, ownIndexConexiones = ownIndexTemp
                  
                  ;mcerda: indice final de este arco "indexEndChild
                  if self.fAllowLoops eq 1b then begin 
                    for lindex=0, N_ELEMENTS( (*self.masterGraphSkel) )-2 do begin
                  
                      currentInitVertex=(*self.masterGraphSkel)[lindex]->getOwnInitVertex()
                      currentEndVertex =(*self.masterGraphSkel)[lindex]->getOwnEndVertex()
                    
                      if indexEndChild eq currentInitVertex or indexEndChild eq currentEndVertex then begin 
                        (*self.masterGraphSkel)[lindex]->AddIndexConexiones, ownIndexConexiones = ownIndexTemp
                      endif
             
                    endfor
                  endif ;if fAllowLoops 
                endif
                (*self.aristasListas)[wNodesOK[edgeIndex]] = 1.0                
                
                self->CreateChildrenLimited, indexInit = indexInitChild, indexEnd = indexEndChild, $
                      levelActual = levelActualLocal, indiceParent = ownIndexTemp, normalParent = normalChild, freePass = 0b
            endif
      endfor
    endelse
    
    if(indiceParent ne -1) then begin
      (*self.masterGraphSkel)[indiceParent]->setAngleMeanChildren,  vValue = cumAngulo
      (*self.masterGraphSkel)[indiceParent]->setLenghtMeanChildren, vValue = cumLargo
    endif   
end

pro C_s3DGraphSkelDir::CreateChildren, indexInit = indexInit, indexEnd = indexEnd, levelActual = levelActual, indiceParent = indiceParent, normalParent = normalParent
    ; Buscar edges que comparten Nodo Final ... para seguir con la pachamama
      wNodesOK = [-1]
      wNodesInit = where( ((*self.aristasOrigen)[0,*] eq indexEnd) and ((*self.aristasOrigen)[1,*] ne indexInit),/L64)
      wNodesEnd  = where( ((*self.aristasOrigen)[1,*] eq indexEnd) and ((*self.aristasOrigen)[0,*] ne indexInit),/L64)

      if(  (wNodesInit[0] ne -1) and (wNodesEnd[0] ne -1)) then begin
        wNodesOK = [wNodesInit,wNodesEnd]
      endif else begin
         if(wNodesInit[0] ne -1) then begin
            wNodesOK = [wNodesInit]
         endif
         if(wNodesEnd[0] ne -1) then begin
            wNodesOK = [wNodesEnd]
         endif
      endelse

    numEdges = 0L 
    if(wNodesOK[0] ne -1) then numEdges = N_ELEMENTS(wNodesOK)
    cumAngulo = 0.0
    cumLargo  = 0.0
    
    levelActual = levelActual + 1L
    for edgeIndex = 0L, numEdges-1L do begin
          edgeActual = obj_new('C_s3GraphEdge'); este es derivado del root   

          indexInitChild = indexEnd
          indexEndChild  = (*self.aristasOrigen)[1,wNodesOK[edgeIndex]]
          
          pointInit = (*self.verticesOrigen)[*,indexInitChild]
          pointEnd  = (*self.verticesOrigen)[*,indexEndChild]

          normalChild = pointEnd - pointInit
          DOTvector   = normalChild*normalChild
          largo       = sqrt(total(DOTvector))
          largoReal   = sqrt(total(DOTvector*self.xyzSizePerPixel))  
          
          if(sqrt(total(DOTvector)) gt 0.0001) then normalChild = normalChild / sqrt(total(DOTvector))

          if((sqrt(total(normalParent)) gt 0.0001) and (sqrt(total(normalChild)) gt 0.0001)) then begin
              angulo = (180.0/!PI)*acos(total(normalParent*normalChild))
          endif else begin
              angulo = 0.0
          endelse 
          
          DOTvector = (pointEnd - pointInit)*(pointEnd - pointInit)
          largo     = sqrt(total(DOTvector))
          largoReal = sqrt(total(DOTvector*self.xyzSizePerPixel))  

          edgeActual->setActualColor, vValue = [255,255,255]
          edgeActual->setIsRoot, vValue = 0b
          edgeActual->setInitXYZ, vValue = pointInit
          edgeActual->setEndXYZ, vValue = pointEnd
              
          edgeActual->setAngleEdge, vValue  = angulo
          edgeActual->setLenghtEdge, vValue = largo
          
          edgeActual->setOwnRoot,  vValue  = 0 ; irrelevante... no se porque no lo borre en su momento... sorry...
          edgeActual->setOwnIndex,  vValue = indiceParent + 1L + edgeIndex 
          edgeActual->setOwnParent, vValue = indiceParent 
          edgeActual->setOwnLevel,  vValue = levelActual
       
          cumAngulo = cumAngulo + angulo
          cumLargo  = cumLargo  + largo

          edgeActual->setAngleMeanChildren, vValue = 0.0
          edgeActual->setLenghtMeanChildren, vValue = 0.0    
          
          self.masterGraphSkel = ptr_new([*self.masterGraphSkel , edgeActual],/no_copy)
          
          if(indiceParent ne -1) then (*self.masterGraphSkel)[indiceParent]->AddIndexConexiones, ownIndexConexiones = indiceParent + 1L + edgeIndex
          
          self->CreateChildren, indexInit = indexInitChild, indexEnd = indexEndChild, levelActual = levelActual, indiceParent = indiceParent + 1L + edgeIndex, normalParent = normalChild
    endfor
    
    numConexionesParent = (*self.masterGraphSkel)[indiceParent]->getNumConexiones()
    (*self.masterGraphSkel)[indiceParent]->setAngleMeanChildren,  vValue = cumAngulo / numConexionesParent
    (*self.masterGraphSkel)[indiceParent]->setLenghtMeanChildren, vValue = cumLargo  / numConexionesParent   
end

function C_s3DGraphSkelDir::CreateReducedSkelEdges

     edgesSkelReduced = [-1]
     currentIntermediate = [-1]
     edgesSkel=[-1]
     indiceVerticeInicio = (*self.masterGraphSkel)[0]->getOwnInitVertex()
     
     if self.fAllowLoops eq 0b then begin
          print, 'Reduced Skeleton non-recursive ', self.fAllowLoops
          self->RecursiveEndForReducedSkelEdges, actualNodeIndex = 0, edgesSkelReduced = edgesSkelReduced, initIndexVertex = indiceVerticeInicio, $
            intermediatePointList=edgesSkel, currentIntermediate=currentIntermediate
     endif else begin
          print, 'Reduced Skeleton recursive ', self.fAllowLoops
          self->RecursiveEndForReducedSkelEdgesWithLoops, actualNodeIndex = 0, edgesSkelReduced = edgesSkelReduced, initIndexVertex = indiceVerticeInicio, $
            intermediatePointList=edgesSkel, currentIntermediate=currentIntermediate 
     endelse
     
     return, edgesSkelReduced
end

function C_s3DGraphSkelDir::CreateFullSkelEdges

     edgesSkelReduced = [-1]
     edgesSkel=[-1]
     currentIntermediate = [-1]
     indiceVerticeInicio = (*self.masterGraphSkel)[0]->getOwnInitVertex()
     
     if self.fAllowLoops eq 0b then begin
          print, 'Full Skeleton non-recursive ', self.fAllowLoops
          self->RecursiveEndForReducedSkelEdges, actualNodeIndex = 0, edgesSkelReduced = edgesSkelReduced, initIndexVertex = indiceVerticeInicio, $
           intermediatePointList=edgesSkel, currentIntermediate=currentIntermediate
     endif else begin
          print, 'Full Skeleton recursive ', self.fAllowLoops
          self->RecursiveEndForReducedSkelEdgesWithLoops, actualNodeIndex = 0, edgesSkelReduced = edgesSkelReduced, initIndexVertex = indiceVerticeInicio, $
          intermediatePointList=edgesSkel, currentIntermediate=currentIntermediate
     endelse
   
     print, 'Full edge list computed'
     return, edgesSkel
end

pro C_s3DGraphSkelDir::RecursiveEndForReducedSkelEdges, actualNodeIndex = actualNodeIndex, edgesSkelReduced = edgesSkelReduced, $
     initIndexVertex = initIndexVertex, intermediatePointList=intermediatePointList, currentIntermediate=currentIntermediate

  numConexionesParent = (*self.masterGraphSkel)[actualNodeIndex]->getNumConexiones()   ;arcos que empiezan aqui
  pConexionesParent   = (*self.masterGraphSkel)[actualNodeIndex]->getIndexConexiones() ;nodos
  endVertex           = (*self.masterGraphSkel)[actualNodeIndex]->getOwnEndVertex()
  
  case numConexionesParent of
    0: begin
        ;final node
        edgesSkelReduced = (edgesSkelReduced[0] eq -1)? [2,initIndexVertex ,endVertex]:[edgesSkelReduced, 2,initIndexVertex ,endVertex]  
        ;there are no intermediate points
        if currentIntermediate[0] eq -1 then begin
            intermediatePointList = (intermediatePointList[0] eq -1 ) ? [2, initIndexVertex, endVertex]:[intermediatePointList, 2, initIndexVertex, endVertex]
        endif else begin
            nelements=(size(currentIntermediate, /dim)+2)
            intermediatePointList = (intermediatePointList[0] eq -1 ) ? [nelements, initIndexVertex, currentIntermediate, endVertex]:[intermediatePointList, nelements, initIndexVertex, currentIntermediate, endVertex]
            currentIntermediate=[-1]
        end
        
    endcase
    1: begin
    
        ;intermediate nodes
        currentIntermediate=(currentIntermediate[0] eq -1)? [endVertex]:[currentIntermediate, endVertex]
        self->RecursiveEndForReducedSkelEdges, actualNodeIndex = pConexionesParent[0], edgesSkelReduced = edgesSkelReduced, $
             initIndexVertex = initIndexVertex, currentIntermediate=currentIntermediate, intermediatePointList=intermediatePointList  
    endcase
    else: begin
    
      nextInitIndexVertex  = endVertex
      nelements=(size(currentIntermediate, /dim)+1)
      edgesSkelReduced     = (edgesSkelReduced[0] eq -1)? [2,initIndexVertex ,endVertex]:[edgesSkelReduced, 2,initIndexVertex ,endVertex]

      ;there are no intermediate points
      if currentIntermediate[0] eq -1 then begin
        intermediatePointList = (intermediatePointList[0] eq -1 ) ? [2, initIndexVertex, endVertex]:[intermediatePointList, 2, initIndexVertex, endVertex]
      endif else begin
        nelements=(size(currentIntermediate, /dim)+2)
        intermediatePointList = (intermediatePointList[0] eq -1 ) ? [2, initIndexVertex, endVertex]:[intermediatePointList, nelements, initIndexVertex, currentIntermediate, endVertex]
        currentIntermediate=[-1]
      end
      for j = 0, numConexionesParent-1 do begin
        self->RecursiveEndForReducedSkelEdges, actualNodeIndex = pConexionesParent[j], edgesSkelReduced = edgesSkelReduced, $
              initIndexVertex = nextInitIndexVertex, intermediatePointList=intermediatePointList, currentIntermediate=currentIntermediate
      endfor
    endcase
  endcase
end


pro C_s3DGraphSkelDir::RecursiveEndForReducedSkelEdgesWithLoops, actualNodeIndex = actualNodeIndex, edgesSkelReduced = edgesSkelReduced, $
  initIndexVertex = initIndexVertex, intermediatePointList=intermediatePointList, currentIntermediate=currentIntermediate

  numConexionesParent = (*self.masterGraphSkel)[actualNodeIndex]->getNumConexiones()
  pConexionesParent   = (*self.masterGraphSkel)[actualNodeIndex]->getIndexConexiones()
  endVertex           = (*self.masterGraphSkel)[actualNodeIndex]->getOwnEndVertex()
  
  case numConexionesParent of
    0: begin
        ;final node
        if initIndexVertex ne endVertex then edgesSkelReduced = (edgesSkelReduced[0] eq -1)? [2,initIndexVertex ,endVertex]:[edgesSkelReduced, 2,initIndexVertex ,endVertex]  
        ;there are no intermediate points
        if currentIntermediate[0] eq -1 then begin
            intermediatePointList = (intermediatePointList[0] eq -1 ) ? [2, initIndexVertex, endVertex]:[intermediatePointList, 2, initIndexVertex, endVertex]
        endif else begin
            nelements=(size(currentIntermediate, /dim)+2)
            intermediatePointList = (intermediatePointList[0] eq -1 ) ? [nelements, initIndexVertex, currentIntermediate, endVertex]:[intermediatePointList, nelements, initIndexVertex, currentIntermediate, endVertex]
            currentIntermediate=[-1]
        end
    endcase
    1: begin
        ;intermediate nodes
        currentIntermediate=(currentIntermediate[0] eq -1)? [endVertex]:[currentIntermediate, endVertex]
        self->RecursiveEndForReducedSkelEdges, actualNodeIndex = pConexionesParent[0], edgesSkelReduced = edgesSkelReduced, $
             initIndexVertex = initIndexVertex, currentIntermediate=currentIntermediate, intermediatePointList=intermediatePointList  
    endcase
    else: begin
    
      nextInitIndexVertex  = endVertex
      nelements=(size(currentIntermediate, /dim)+1)
      edgesSkelReduced     = (edgesSkelReduced[0] eq -1)? [2,initIndexVertex ,endVertex]:[edgesSkelReduced, 2,initIndexVertex ,endVertex]
      
      ;there are no intermediate points
      if currentIntermediate[0] eq -1 then begin
        intermediatePointList = (intermediatePointList[0] eq -1 ) ? [2, initIndexVertex, endVertex]:[intermediatePointList, 2, initIndexVertex, endVertex]
      endif else begin
        nelements=(size(currentIntermediate, /dim)+2)
        intermediatePointList = (intermediatePointList[0] eq -1 ) ? [2, initIndexVertex, endVertex]:[intermediatePointList, nelements, initIndexVertex, currentIntermediate, endVertex]
        currentIntermediate=[-1]
      end
      
      for j = 0, numConexionesParent-1 do begin
        self->RecursiveEndForReducedSkelEdgesWithLoops, actualNodeIndex = pConexionesParent[j], edgesSkelReduced = edgesSkelReduced, $
            initIndexVertex = nextInitIndexVertex, intermediatePointList=intermediatePointList, currentIntermediate=currentIntermediate
      endfor
      
    endcase
  endcase
end

function C_s3DGraphSkelDir::GetmasterGraphSkel
    return, self.masterGraphSkel
end

pro C_s3DGraphSkelDir::cleanup
  ptr_free, self.verticesOrigen
  ptr_free, self.aristasOrigen

   for i = 0, N_ELEMENTS(*self.masterGraphSkel) -1 do begin
      OBJ_DESTROY, (*self.masterGraphSkel)[i]
   endfor
  ptr_free, self.masterGraphSkel   
end


function C_s3DGraphSkelDir::init
    ; Parameters of C_s3DGraphSkelDir.
    return, 1
end

pro C_s3DGraphSkelDir__define
   InterClass = { C_s3DGraphSkelDir,      masterGraphSkel : ptr_new(),$
                                          verticesOrigen  : ptr_new(),$
                                          aristasOrigen   : ptr_new(),$
                                          aristasListas   : ptr_new(),$
                                          fAllowLoops     : 0b,$
                                          xyzSizePerPixel : [1.0,1.0,1.0]$
                }
end
