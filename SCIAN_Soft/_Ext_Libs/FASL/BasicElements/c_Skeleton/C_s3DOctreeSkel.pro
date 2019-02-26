; FASL 2013-2014 fsantibanezleal@ug.uchile.cl
; 
; Implementation of Skeletonization algorithm from Octree representation:
; Based on Paper: SkelTre - fast skeletonisation for imperfect point cloud data of
;botanic trees .... Bucksch et al. 2009
;  self._edgesOctreeGraph            [2,numEdgesOrigen]
;                     
;  self._skelTreeNodes               [numVertexsOrigen]     
;  self._vextersSkelTree             [3,numVertexsSkel]
;  self._masterEdges                 [2,numEdgesSkel]

;  self._internalIndex               [numMasterEdges]
;  self._endNodesOctree              [numEndNodesInOriginalOctree]


pro C_s3DOctreeSkel::MasterSkelCreation, MASTEROWNOCTREE = MASTEROWNOCTREE,XYZPOINTS=XYZPOINTS
  ; Create OctreeSkel for actual Octree
  
  self->CalcLastNodes, MASTEROWNOCTREE = MASTEROWNOCTREE
  self->CalcOctreeGraph, MASTEROWNOCTREE = MASTEROWNOCTREE,XYZPOINTS=XYZPOINTS

  self->GenerateNodeSystem, MASTEROWNOCTREE = MASTEROWNOCTREE,XYZPOINTS=XYZPOINTS
  
  self->GenerateSkelSystem

  ; Fix.. sort... and release outcomes
  ; loop over residual Nodes and use it as the ending vertexs.... XD
  ; create a cross reference beetwen ending vertexs and original positions (used in edgeMaster representation)
  ; at this point we have:
  ; INPUT RESUME
  ; list of end nodes from the octree representation _endNodesOctree                        [numEndNodesInOriginalOctree]  
  ; centroid of ending octree nodes  self._vertexsOctreeGraph                               [3,numVertexsOrigen]
  ; edges for the basic octree graph for this vertexs...  self._edgesOctreeGraph            [2,numEdgesOrigen]
  numFullNodes = N_ELEMENTS((*self._inUseSkelNodes))
 
  initElement = 1b
  indexNew    = 0L
  for indexNode = 0, numFullNodes - 1L do begin
    if((*self._inUseSkelNodes)[indexNode] eq 1) then begin
      newSkelVertex = (*self._skelTreeNodes)[indexNode]->getVertexPosition()
      if(initElement eq 1b) then begin
        self._vextersSkelTree = ptr_new(newSkelVertex)
        initElement = 0b
      endif else begin
        self._vextersSkelTree = ptr_new([[(*self._vextersSkelTree)],[newSkelVertex]])
      endelse
      self->JointVertex, remanent = indexNew, lost = indexNode ; optimized cross reference update
      indexNew++
    endif
  endfor
  
  ; Output
  ; skeltree nodes with vertex and relevant data... i.e index to original index in octree representation
  ; _skelTreeNodes                            [numSkelVertex]
  ; _vextersSkelTree   only with vertexs      [3,numSkelVertex]
  ; now.... to reduce deleted edges and repeated from the collapse proccess...
  indexNew    = 0L
  initElement  = 1b
  numEdges     = N_ELEMENTS((*self._masterEdges)[0,*])
  crossReferenceEdges = MAKE_ARRAY(numEdges,/INTEGER, value = -1)
  newEdge = [-1,-1]
  newMasterEdge = ptr_new([-1,-1])
  for indexEdge = 0, numEdges - 1L do begin
      newEdge = (*self._masterEdges)[*,indexEdge]
      indexFirst = self->whereRepeatedEdge( vectorEdges = *newMasterEdge, edge = newEdge)
      if(indexFirst eq -1) then begin
        if(initElement eq 1b) then begin
          newMasterEdge = ptr_new(newEdge)
          initElement = 0b
        endif else begin
          newMasterEdge = ptr_new([[*newMasterEdge],[newEdge]])
        endelse
        crossReferenceEdges[indexEdge] = indexNew
        indexNew++
      endif else begin
        crossReferenceEdges[indexEdge] = indexFirst
      endelse 
  endfor
  
  ; and .. clean skelNodes ....relese bad references...
  numFullNodes      = N_ELEMENTS((*self._inUseSkelNodes))
  newSkelNodeMaster = ptr_new([-1])
  initElement  = 1b
  for indexNode = 0, numFullNodes - 1L do begin
    if((*self._inUseSkelNodes)[indexNode] eq 1) then begin
      
      newNode = obj_new('C_sSkelNode') 
      (*self._skelTreeNodes)[indexNode]->FixEdges, crossEdgeReference = crossReferenceEdges  
      newNode->CopyEndParams, otherNode = (*self._skelTreeNodes)[indexNode] 
        if(initElement eq 1b) then begin
          newSkelNodeMaster = ptr_new(newNode,/no_copy)
        endif else begin
          newSkelNodeMaster = ptr_new([[*newSkelNodeMaster],newNode],/no_copy)
        endelse
    endif
  endfor
  
  ;Replace and release memory
   for i = 0, N_ELEMENTS(*self._skelTreeNodes) -1 do begin
      OBJ_DESTROY, (*self._skelTreeNodes)[i]
   endfor
  ptr_free, self._skelTreeNodes
  self._skelTreeNodes = ptr_new(*newSkelNodeMaster,/no_copy)  
  
  ptr_free, self._masterEdges
  self._masterEdges = ptr_new(*newMasterEdge,/no_copy)  
end
pro C_s3DOctreeSkel::CalcOctreeGraph, MASTEROWNOCTREE = MASTEROWNOCTREE,XYZPOINTS=XYZPOINTS
  first = 1
  dim = n_elements((*self._endNodesOctree))-1
  microInternalIndex = MAKE_ARRAY(dim+1,/DOUBLE, value = -1d)
  for lastNodeActual = 0, dim do begin
    tempNode = (*MASTEROWNOCTREE)[((*self._endNodesOctree))[lastNodeActual]]
    tempCenter    = tempNode->getCenterNode()
    tempSemiAxis  = tempNode->getSemiAxisXYZ()
    ; Search for candidates for connections...
    if(lastNodeActual lt dim) then begin
      for lastNodeNeighbor = lastNodeActual+1, dim do begin
        tempNeighbor = (*MASTEROWNOCTREE)[((*self._endNodesOctree))[lastNodeNeighbor]]
        tempCenterN    = tempNeighbor->getCenterNode()
        tempSemiAxisN  = tempNeighbor->getSemiAxisXYZ()
        ; verify only direct neighbors ...
        cercaniaXYZ = abs(tempCenter-tempCenterN)
        sumSemiAxis = tempSemiAxis + tempSemiAxisN

        cercaniaCartesianaXY = (cercaniaXYZ[0] lt sumSemiAxis[0]) and (cercaniaXYZ[1] lt sumSemiAxis[1]) and (cercaniaXYZ[2] le sumSemiAxis[2]) 
        cercaniaCartesianaXZ = (cercaniaXYZ[0] lt sumSemiAxis[0]) and (cercaniaXYZ[1] le sumSemiAxis[1]) and (cercaniaXYZ[2] lt sumSemiAxis[2]) 
        cercaniaCartesianaYZ = (cercaniaXYZ[0] le sumSemiAxis[0]) and (cercaniaXYZ[1] lt sumSemiAxis[1]) and (cercaniaXYZ[2] lt sumSemiAxis[2])

        if(cercaniaCartesianaXY or cercaniaCartesianaXZ or cercaniaCartesianaYZ) then begin
          ; Use variance of points distribution to estimate if connection is real
          tempCenter         = tempNode->getMassCenterXYZ()
          tempCenterN        = tempNeighbor->getMassCenterXYZ()
          normalInteraction  = tempCenter-tempCenterN
          
          meanDistanceA     = self->CalcMeanDistancePointstoPlane( points = XYZPOINTS[*,tempNode->getIndexPoints()], center = tempCenter, normal = normalInteraction)
          meanDistanceN     = self->CalcMeanDistancePointstoPlane( points = XYZPOINTS[*,tempNeighbor->getIndexPoints()], center = tempCenterN, normal = normalInteraction)
          meanDistanceJoint = self->CalcMeanDistancePointstoPlane( points = XYZPOINTS[*,[tempNode->getIndexPoints(),tempNeighbor->getIndexPoints()]], center = 0.5d*(tempCenter+tempCenterN), normal = normalInteraction)
          
          ; criteria
          ;if(((1.0d/16.d)*meanDistanceJoint) le min([meanDistanceA,meanDistanceN]))then begin
          if(((1.0d/9.d)*meanDistanceJoint) le min([meanDistanceA,meanDistanceN]))then begin
             ; save vertexs,,,
             if(first eq 1) then begin
                self._vertexsOctreeGraph = ptr_new([[tempCenter],[tempCenterN]])
                microInternalIndex[lastNodeActual]   = 0
                microInternalIndex[lastNodeNeighbor] = 1
                
                indiceAOctree = [((*self._endNodesOctree))[lastNodeActual],((*self._endNodesOctree))[lastNodeNeighbor]]                
                self._internalIndex = ptr_new(indiceAOctree)
             endif else begin
                numVertexs = n_elements((*self._vertexsOctreeGraph)[0,*])
                if(microInternalIndex[lastNodeActual] ne -1) then begin
                  newVertexs = [tempCenterN]
                  microInternalIndex[lastNodeNeighbor] = numVertexs

                  indiceAOctree = [((*self._endNodesOctree))[lastNodeNeighbor]]                
                endif else begin
                  newVertexs = [[tempCenter],[tempCenterN]]
                  microInternalIndex[lastNodeActual]   = numVertexs
                  microInternalIndex[lastNodeNeighbor] = numVertexs + 1
                  
                  indiceAOctree = [((*self._endNodesOctree))[lastNodeActual],((*self._endNodesOctree))[lastNodeNeighbor]]                  
                endelse
                self._vertexsOctreeGraph = ptr_new([[*self._vertexsOctreeGraph] , [newVertexs]])
                self._internalIndex = ptr_new([(*self._internalIndex),indiceAOctree])
             endelse          
             ; add edges
             newEdge = [microInternalIndex[lastNodeActual],microInternalIndex[lastNodeNeighbor]]
             if(first eq 1) then begin
                self._edgesOctreeGraph = ptr_new(newEdge)
                first = 0
             endif else begin
                self._edgesOctreeGraph = ptr_new([[*self._edgesOctreeGraph] , [newEdge]])             
             endelse
          endif
        endif
      endfor
    endif
  endfor
  
  ; Init End Data wit input data
 ; self._vextersSkelTree = ptr_new(*self._vertexsOctreeGraph)
  self._masterEdges     = ptr_new(*self._edgesOctreeGraph) ; start addressing *self._vertexsOctreeGraph 
end
pro C_s3DOctreeSkel::GenerateNodeSystem, MASTEROWNOCTREE = MASTEROWNOCTREE,XYZPOINTS=XYZPOINTS
  
  ;numVertexs = n_elements((*self._vextersSkelTree)[0,*])
  numVertexs = n_elements((*self._internalIndex)[*])  
  
  first = 1
  for indexVertex = 0d, numVertexs - 1 do begin
     tempNodeOctree = (*MASTEROWNOCTREE)[(*self._internalIndex)[indexVertex]]
     nodeSkel = obj_new('C_sSkelNode') 
     
     nodeSkel->setInUse, value = 1
     nodeSkel->setOwnIndex, value = indexVertex
     nodeSkel->setVertexPosition, value = tempNodeOctree->getMassCenterXYZ() 

     positionInEdges = self->FindEdges( indexVertex = indexVertex, edgeList = (*self._masterEdges))
     numEdges = 0L 
     if(positionInEdges[0] ne -1) then numEdges = N_ELEMENTS(positionInEdges)

     nodeSkel->setNumEdges, value   = numEdges
     nodeSkel->setIndexEdges, value = positionInEdges

     ; Set edge Label for edge(vertexActual,vertexNeighbor)
     edgeLabels = MAKE_ARRAY(3, numEdges, /DOUBLE) * 0.0d
     for indexEdge = 0, numEdges-1 do begin
        actualEdge = (*self._masterEdges)[*,positionInEdges[indexEdge]]
        actualIndexVertexComp = (actualEdge[0] ne indexVertex)? actualEdge[0] : actualEdge[1]
        
        tempNodeOctreeNeighbor = (*MASTEROWNOCTREE)[(*self._internalIndex)[actualIndexVertexComp]]

        tempCenterA     = tempNodeOctree->getCenterNode()
        tempSemiAxisA   = tempNodeOctree->getSemiAxisXYZ()
        tempCenterN     = tempNodeOctreeNeighbor->getCenterNode()
        tempSemiAxisN   = tempNodeOctreeNeighbor->getSemiAxisXYZ()
        distanciaXYZ     = tempCenterN-tempCenterA
        cercaniaXYZ     = abs(distanciaXYZ)
        sumSemiAxis     = tempSemiAxisA + tempSemiAxisN
        cercaniaCartesianaZ = (cercaniaXYZ[0] lt sumSemiAxis[0]) and (cercaniaXYZ[1] lt sumSemiAxis[1]) and (cercaniaXYZ[2] le sumSemiAxis[2]) 
        cercaniaCartesianaY = (cercaniaXYZ[0] lt sumSemiAxis[0]) and (cercaniaXYZ[1] le sumSemiAxis[1]) and (cercaniaXYZ[2] lt sumSemiAxis[2]) 
        cercaniaCartesianaX = (cercaniaXYZ[0] le sumSemiAxis[0]) and (cercaniaXYZ[1] lt sumSemiAxis[1]) and (cercaniaXYZ[2] lt sumSemiAxis[2])
        
        switch 1b of
          (cercaniaCartesianaX): edgeLabels[0,indexEdge] = (distanciaXYZ[0] gt 0.0d)? 1:-1
          (cercaniaCartesianaY): edgeLabels[1,indexEdge] = (distanciaXYZ[1] gt 0.0d)? 1:-1 
          (cercaniaCartesianaZ): edgeLabels[2,indexEdge] = (distanciaXYZ[2] gt 0.0d)? 1:-1 
        endswitch
     endfor 
     
     nodeSkel->setEdgeLabels, value = edgeLabels

     nodeSkel->setWCentroid, value = tempNodeOctree->getNumPoints(); tempNodeOctree->getLevelDeep()
     nodeSkel->setNumPoints, value = tempNodeOctree->getNumPoints()
     nodeSkel->setIndexOriginalPoints, value = tempNodeOctree->getIndexPoints()
     
     if(first eq 1) then begin
        self._skelTreeNodes = ptr_new(nodeSkel,/no_copy)
        first = 0
     endif else begin
        self._skelTreeNodes = ptr_new([*self._skelTreeNodes, nodeSkel],/no_copy)             
     endelse
  endfor
  
  dummy = MAKE_ARRAY(N_ELEMENTS((*self._skelTreeNodes)),/INTEGER, value = 1)
  self._inUseSkelNodes = ptr_new(dummy,/NO_COPY)
end
function C_s3DOctreeSkel::FindEdges, indexVertex = indexVertex, edgeList = edgeList
    wNodesOK = [-1]
    wNodesInit = where( (edgeList[0,*] eq indexVertex),/L64)
    wNodesEnd  = where( (edgeList[1,*] eq indexVertex),/L64)

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
  return, wNodesOK 
end
function C_s3DOctreeSkel::CalcMeanDistancePointstoPlane, points = points, center = center, normal = normal
  normal  = normal / sqrt(total(normal*normal))

  distance = 0.0d
  ;plane equation
  dPlane = 0.0d
   ; case 1b of
    ;  (abs(normal[0]*center[0]) gt 0.001d): dPlane = -((normal[1]*center[1])+(normal[2]*center[2]))/(normal[0]*center[0])  
     ; (abs(normal[1]*center[1]) gt 0.001d): dPlane = -((normal[0]*center[0])+(normal[2]*center[2]))/(normal[1]*center[1]) 
     ; (abs(normal[2]*center[2]) gt 0.001d): dPlane = -((normal[1]*center[1])+(normal[0]*center[0]))/(normal[2]*center[2])
   ; endcase  
   dPlane = -total(normal*center)

  ; distances
  distances = abs(points[0,*]*normal[0]+points[1,*]*normal[1]+points[2,*]*normal[2]+dPlane)
  return, mean(distances)
end

; FIXME FASL: We need to check the state of progress related with this last stage....

pro C_s3DOctreeSkel::GenerateSkelSystem
; Algoritm for reduction from octreeGraph to skelTree
  dim = 6
  while dim ge 1 do begin
    dim-- 
    listDim = self->getListforDim( value = dim)
    vPairs  = self->initVPairs( value = listDim)
    
    intentar = 1b
    while( intentar eq 1b) do begin
      ;dim--
      if(N_ELEMENTS(vPairs) le 1) then begin
        vPairs  = self->vPairsFromEPairs( value = listDim) ; really an Epairs List for collpases in order to create new vpairs
        if((N_ELEMENTS(vPairs) le 1)) then intentar = 0b 
      endif else begin
        ; select first unprocessed entry
        firstVPair = (*self._masterEdges)[*,vPairs[1]] ;???
        ; create Joint Vertex .... replace Vi for jointNode 
        ;and disable Vj ... replace vj for vi in edge reference
        tempSkelNode_i = ((*self._skelTreeNodes)[firstVPair[0]])
        tempSkelNode_j = ((*self._skelTreeNodes)[firstVPair[1]])
        jointNode = self->CreateJointNode(nodeSkel1 =tempSkelNode_i,nodeSkel2 =tempSkelNode_j)
        tempSkelNode_i->CopyParams, otherNode = jointNode 
        tempSkelNode_j->DisableNode
        (*self._inUseSkelNodes)[firstVPair[1]] = 0
        self->JointVertex, remanent = firstVPair[0], lost = firstVPair[1]
        OBJ_DESTROY, jointNode
        
        ; delete entry for vPair
        vPairs[0] = -1
        vPairs    = vPairs[1:*]        
      endelse
    endwhile
    ;dim-- ; NOt sure about position in For CYCLE ..xD  
  endwhile
end
pro C_s3DOctreeSkel::JointVertex, remanent = remanent, lost = lost
  wNodesInit = where( (*self._masterEdges)[0,*] eq lost,/L64)
  wNodesEnd  = where( (*self._masterEdges)[1,*] eq lost,/L64)

   if(wNodesInit[0] ne -1) then begin
      (*self._masterEdges)[0,wNodesInit] = remanent
   endif
   if(wNodesEnd[0] ne -1) then begin
      (*self._masterEdges)[1,wNodesEnd] = remanent
   endif
end


function C_s3DOctreeSkel::getListforDim, value =value
  listDim = [-1]
  wInUse =  where( (*self._inUseSkelNodes) eq 1 ,/L64) 
  if(wInUse[0] eq -1) then return, listDim
  
  numNodes = n_elements(wInUse)
  inicial = 1b
  for indexNode = 0d, numNodes - 1 do begin
     actualDim = ((*self._skelTreeNodes)[wInUse[indexNode]])->getDim()
     if(actualDim ge value) then begin
        if(inicial eq 1b) then begin
          inicial = 0b
          listDim = [indexNode]
        endif else begin
          listDim = [listDim,indexNode]
        endelse
     endif
  endfor

  return,listDim
end

function C_s3DOctreeSkel::initVPairs, value =value
  vPairs = [-1]
  numNodes = n_elements(value)  
  if(value[0] eq -1) then return, vPairs
  for indexRefNode = 0d, numNodes - 1 do begin
     tempSkelNode = (*self._skelTreeNodes)[value[indexRefNode]]
     numNeighbors = tempSkelNode->getNumEdges()
     if(numNeighbors gt 0) then begin
        indexNeighbors = tempSkelNode->getIndexEdges()
        for indexNodeN = 0d, numNeighbors - 1 do begin
          edgeRefered  = (*self._masterEdges)[*,indexNeighbors[indexNodeN]]
          indexNodeN   = (edgeRefered[0] ne value[indexRefNode])? edgeRefered[0]: edgeRefered[1]

          isVPair      = self->isVPair(indexNodeSkel1 = value[indexRefNode],indexNodeSkel2 = indexNodeN)
          if(isVPair) then begin
              firstAttemptStrong = self->verifyFreeEdgeStrong(vectorEdges =vPairs, indexEdge = indexNeighbors[indexNodeN])
              if(firstAttemptStrong) then begin
                vPairs = [vPairs,indexNeighbors[indexNodeN]]
              endif
          endif 
        endfor
     endif
  endfor

  return, vPairs
end
function C_s3DOctreeSkel::vPairsFromEPairs, value =value
  vPairs = [-1]
  numNodes = n_elements(value)  
  if(value[0] eq -1) then return, vPairs
  for indexRefNode = 0d, numNodes - 1 do begin
     tempSkelNode = (*self._skelTreeNodes)[value[indexRefNode]]
     numNeighbors = tempSkelNode->getNumEdges();getNumConexiones()
     if(numNeighbors gt 0) then begin
        indexNeighbors = tempSkelNode->getIndexEdges();getIndexConexiones()
        for indexNodeN = 0d, numNeighbors - 1 do begin
          edgeRefered  = (*self._masterEdges)[*,indexNeighbors[indexNodeN]]
          indexNodeN   = (edgeRefered[0] ne value[indexRefNode])? edgeRefered[0]: edgeRefered[1]

          isVPair      = self->isEPair(indexNodeSkel1 = value[indexRefNode],indexNodeSkel2 = indexNodeN)
          if(isVPair) then begin
              firstAttemptStrong = self->verifyFreeEdgeStrong(vectorEdges =vPairs, indexEdge = indexNeighbors[indexNodeN])
              if(firstAttemptStrong) then begin
                vPairs = [vPairs,indexNeighbors[indexNodeN]]
              endif
          endif 
        endfor
     endif
  endfor

  return, vPairs
end

function C_s3DOctreeSkel::verifyFreeEdge, vectorEdges =vectorEdges, edge = edge
  ; Sort case
  wNodesSort    = where( (vectorEdges[0,*] eq edge[0]) and (vectorEdges[1,*] eq edge[1]),/L64)
  wNodesUnSort  = where( (vectorEdges[1,*] eq edge[0]) and (vectorEdges[0,*] eq edge[1]),/L64)

  if((wNodesSort[0] ne -1) or (wNodesUnSort[0] ne -1)) then begin
    return, 0
  end 

  return, 1
end
function C_s3DOctreeSkel::whereRepeatedEdge, vectorEdges =vectorEdges, edge = edge
  ; Sort case
  wNodesSort    = where( (vectorEdges[0,*] eq edge[0]) and (vectorEdges[1,*] eq edge[1]),/L64)
  wNodesUnSort  = where( (vectorEdges[1,*] eq edge[0]) and (vectorEdges[0,*] eq edge[1]),/L64)

  if(wNodesSort[0] ne -1) then   return, wNodesSort[0]
  if(wNodesUnSort[0] ne -1) then return, wNodesUnSort[0]
    
  return, -1
end

function C_s3DOctreeSkel::verifyFreeEdgeStrong, vectorEdges = vectorEdges, indexEdge = indexEdge
  ; simple case
  wIndexEdge = where(vectorEdges eq  indexEdge,/L64)
  if(wIndexEdge[0] ne -1) then return, 0

  wValidEdge = where(vectorEdges ne -1,/L64)
  if(wValidEdge[0] eq -1) then return, 1
  indexEdgeList = vectorEdges[wValidEdge]
  
  edgesFullUsed = (*self._masterEdges)[*,indexEdgeList]
  currentEdge   = (*self._masterEdges)[*,indexEdge]
  return, self->verifyFreeEdge(vectorEdges =edgesFullUsed, edge = currentEdge)
end
function C_s3DOctreeSkel::isVPair ,indexNodeSkel1 = indexNodeSkel1, indexNodeSkel2 = indexNodeSkel2
  if(indexNodeSkel1 eq indexNodeSkel2) then  return, 0
  
  tempSkelNode1 = ((*self._skelTreeNodes)[indexNodeSkel1])
  tempSkelNode2 = ((*self._skelTreeNodes)[indexNodeSkel2])

  numEdges1     = tempSkelNode1->getNumEdges()
  indexEdges1   = tempSkelNode1->getIndexEdges()
  otherIndexN1  = [-1]
  internalIndexN1  = [-1]
  for indexEdge = 0, numEdges1-1 do begin
     actualEdge = (*self._masterEdges)[*,indexEdges1[indexEdge]]
     tempIndex  = (actualEdge[0] ne indexNodeSkel1)? actualEdge[0] : actualEdge[1]
     if(tempIndex ne indexNodeSkel2) then begin
        otherIndexN1 = (otherIndexN1[0] eq [-1])? [tempIndex]: [otherIndexN1,tempIndex]
        internalIndexN1  = (internalIndexN1[0] eq [-1])? [indexEdge]: [internalIndexN1,indexEdge]
     endif
  endfor
  
  numEdges2     = tempSkelNode2->getNumEdges()
  indexEdges2   = tempSkelNode2->getIndexEdges()
  otherIndexN2  = [-1]
  internalIndexN2  = [-1]
  for indexEdge = 0, numEdges2-1 do begin
     actualEdge = (*self._masterEdges)[*,indexEdges2[indexEdge]]
     tempIndex  = (actualEdge[0] ne indexNodeSkel2)? actualEdge[0] : actualEdge[1]
     if(tempIndex ne indexNodeSkel1) then begin
        otherIndexN2 = (otherIndexN2[0] eq [-1])? [tempIndex]: [otherIndexN2,tempIndex]
        internalIndexN2  = (internalIndexN2[0] eq [-1])? [indexEdge]: [internalIndexN2,indexEdge]
     endif
  endfor

  if((otherIndexN1[0] ne -1) and (otherIndexN2[0] ne -1)) then begin
    for indexOtherNeigborn = 0, N_ELEMENTS(otherIndexN1) - 1 do begin
      labels1 = tempSkelNode1->getEdgeLabels()
      labels2 = tempSkelNode2->getEdgeLabels()
      ; verify if this vertex is a common neigborn for v1 and v2
      wCommon = where( otherIndexN2 eq otherIndexN1[indexOtherNeigborn],/L64)
      if(wCommon[0] ne -1) then begin
        ;validate def 4.4 a ...
        comparacion = labels1[*,internalIndexN1[indexOtherNeigborn]] and labels2[*,internalIndexN2[wCommon[0]]]        
        if(total(comparacion) eq 3) then begin
          ;validate def 4.4 b ...
          jointNode = self->CreateJointNode(nodeSkel1 =tempSkelNode1,nodeSkel2 =tempSkelNode2)
          
          isVPair = jointNode->getDim() le max([nodeSkel1->getDim(),nodeSkel2->getDim()])
          OBJ_DESTROY, jointNode
          if(isVPair) then return, 1
        endif
      endif
    endfor
  endif
  return, 0
end
 function C_s3DOctreeSkel::CreateJointNode,nodeSkel1 =nodeSkel1,nodeSkel2 =nodeSkel2
   nodeSkel = obj_new('C_sSkelNode') 
   
   nodeSkel->setInUse, value = 1
   nodeSkel->setOwnIndex, value = N_ELEMENTS((*self._skelTreeNodes)) ; set out this function

   nodeSkel->setNumEdges, value   = nodeSkel1->getNumEdges() + nodeSkel2->getNumEdges()
   
   ; recovery and joint indexs and labels
   nodeSkel->setIndexEdges, value = [nodeSkel1->getIndexEdges(),nodeSkel2->getIndexEdges()]
   nodeSkel->setEdgeLabels, value = [nodeSkel1->getEdgeLabels(),nodeSkel2->getEdgeLabels()]
    
   nodeSkel->setWCentroid, value  = nodeSkel1->getWCentroid() + nodeSkel2->getWCentroid()
   nodeSkel->setNumPoints, value  = nodeSkel1->getNumPoints() + nodeSkel2->getNumPoints()
   nodeSkel->setIndexOriginalPoints, value = [nodeSkel1->getIndexOriginalPoints(),nodeSkel2->getIndexOriginalPoints()]

  return, nodeSkel
end
function C_s3DOctreeSkel::isEPair, indexNodeSkel1 = indexNodeSkel1, indexNodeSkel2 = indexNodeSkel2
  if(indexNodeSkel1 eq indexNodeSkel2) then  return, 0
  
  tempSkelNode1 = ((*self._skelTreeNodes)[indexNodeSkel1])
  tempSkelNode2 = ((*self._skelTreeNodes)[indexNodeSkel2])

  dir1 = tempSkelNode1->getDir()
  ;validate def 4.5 a ...
  if(tempSkelNode1->getDim() gt tempSkelNode2->getDim()) then return, 0

  ;validate def 4.5 c 1...
  if(total(dir1 eq [0,0,0]) eq 3) then return, 0
  ;validate def 4.5 c 2... & 4.5.d
  numEdges1     = tempSkelNode1->getNumEdges()
  indexEdges1   = tempSkelNode1->getIndexEdges()
  labels1       = tempSkelNode1->getEdgeLabels()
  
  indexEdges2   = tempSkelNode2->getIndexEdges()
  labels2       = tempSkelNode2->getEdgeLabels()
  for indexEdge = 0, numEdges1-1 do begin
     actualEdge = (*self._masterEdges)[*,indexEdges1[indexEdge]]
     tempIndex  = (actualEdge[0] ne indexNodeSkel1)? actualEdge[0] : actualEdge[1]
     if(tempIndex eq indexNodeSkel2) then begin
        ;validate def 4.5 c 2
        if(total(dir1 * labels1[indexEdge]) eq 0) then return, 0
        
        ;validate def 4.5 d ...
        labelCommon1 = labels1[*,indexEdge]
        ;FASL please verify the sense here...
        labelCommon2 = -1.0d * labels1[*,indexEdge]
        
        wOA1 = self->whereOtherAxisLabel( labelList = labels1, labelA = labelCommon1)
        ;if((wOA1[0] eq -1) or (wOA2[0] eq -1)) then return, 0
        if((wOA1[0] eq -1)) then return, 0
        
        fail45d = 1
        for indexTemp = 0, N_ELEMENTS(wOA1) - 1 do begin
          wOA2 = self->whereLabel(labelList = labels2, labelA = labels1[*,wOA1[indexTemp]])

          if(wOA2[0] ne -1) then begin 
            for indexTemp2 = 0, N_ELEMENTS(wOA2) - 1 do begin
              ; recovery both neighbors nodes
              tempEdge = (*self._masterEdges)[*,indexEdges1[wOA1[indexTemp]]]
              nViIndex = (tempEdge[0] ne indexNodeSkel1)? tempEdge[0] : tempEdge[1]
              tempEdge = (*self._masterEdges)[*,indexEdges2[wOA2[indexTemp2]]]
              nVjIndex = (tempEdge[0] ne indexNodeSkel2)? tempEdge[0] : tempEdge[1]

              nodeContactVi = ((*self._skelTreeNodes)[nViIndex])
              nodeContactVj = ((*self._skelTreeNodes)[nVjIndex])
              
              labelsNVi     = nodeContactVi->getEdgeLabels()              
              labelsNVj     = nodeContactVj->getEdgeLabels()
              
              isD1  = self->verifyLabel( labelList = labelsNVi, labelA = labelCommon1)
              isD2  = self->verifyLabel( labelList = labelsNVj, labelA = labelCommon2)
        
              if(isD1 and isD2) then fail45d = 0
            endfor  
          endif
        endfor
        
        if(fail45d) then return, 0
     endif
  endfor

  ;last condition.... validate def 4.5 b ...
  jointNode = self->CreateJointNode(nodeSkel1 =tempSkelNode1,nodeSkel2 =tempSkelNode2)
  is45b = jointNode->getDim() le max([nodeSkel1->getDim(),nodeSkel2->getDim()])
  OBJ_DESTROY, jointNode
  if(is45b) then return, 1

  return, 0
end
function C_s3DOctreeSkel::verifyLabel, labelList = labelList, labelA = labelA
  wLabel = where( (labelList[0,*] eq labelA[0]) and (labelList[1,*] eq labelA[1]) and (labelList[2,*] eq labelA[2]),/L64)
  if(wLabel[0] ne -1) then begin
    return, 1
  end 
  return, 0
end
function C_s3DOctreeSkel::whereLabel, labelList = labelList, labelA = labelA
  wLabel = where( (labelList[0,*] eq labelA[0]) and (labelList[1,*] eq labelA[1]) and (labelList[2,*] eq labelA[2]),/L64)
  return, wLabel[0]
end
function C_s3DOctreeSkel::whereOtherAxisLabel, labelList = labelList, labelA = labelA
  wLabel = where( (abs(labelList[0,*]) eq abs(labelA[0])) and (abs(labelList[1,*]) eq abs(labelA[1])) and (abs(labelList[2,*]) eq abs(labelA[2])),COMPLEMENT= wNLabel,/L64)
  return,   wNLabel
end
;octree graph data
function C_s3DOctreeSkel::GetVertexsOctreeGraph
    return, *self._vertexsOctreeGraph
end
function C_s3DOctreeSkel::GetEdgesOctreeGraph
    return, *self._edgesOctreeGraph
end
function C_s3DOctreeSkel::GetEdgesArrayOctreeGraph
    dummy = [2, (*self._edgesOctreeGraph)[*,0]]
    
    numEdges = N_ELEMENTS((*self._edgesOctreeGraph)[0,*])
    for indexNode = 1L, numEdges - 1L do begin
      dummy = [dummy, 2, (*self._edgesOctreeGraph)[*,indexNode]]
    endfor
    return, dummy
end

; skeltree data
function C_s3DOctreeSkel::GetVertexsSkelTree
    return, *self._vextersSkelTree
end
function C_s3DOctreeSkel::GetMasterEdges
    return, self._masterEdges
end
function C_s3DOctreeSkel::GetArrayMasterEdges
    dummy = [2, (*self._masterEdges)[*,0]]
    
    numEdges = N_ELEMENTS((*self._masterEdges)[0,*])
    for indexNode = 1L, numEdges - 1L do begin
      dummy = [dummy, 2, (*self._masterEdges)[*,indexNode]]
    endfor
    return, dummy
end

pro C_s3DOctreeSkel::CalcLastNodes,MASTEROWNOCTREE = MASTEROWNOCTREE
; recovery am array with the terminals nodes of Octree representation
; only use for create the initial OctreeGraph
    lastNodes = [-1]
    initialD = 0.0d
    firstBoolean = 1b
    FOR k=initialD,N_ELEMENTS(*MASTEROWNOCTREE)-1 DO BEGIN
    
      if(((*MASTEROWNOCTREE)[k]->getOffSpring() eq 0b) and ( (*MASTEROWNOCTREE)[k]->getNumPoints() gt 0) ) then begin
        if(firstBoolean) then begin
          firstBoolean = 0b
          lastNodes = [k]
        endif else     lastNodes = [lastNodes, k] 
      endif
    ENDFOR
    self._endNodesOctree = ptr_new(lastNodes, /no_copy)
end
function C_s3DOctreeSkel::GetLastNodes
    return, (*self._endNodesOctree)
end
pro C_s3DOctreeSkel::cleanup
; fIRST DESTROY OBJECTS.... 
  ptr_free, self._vertexsOctreeGraph
  ptr_free, self._edgesOctreeGraph

   for i = 0, N_ELEMENTS(*self._skelTreeNodes) -1 do begin
      OBJ_DESTROY, (*self._skelTreeNodes)[i]
   endfor
  ptr_free, self._skelTreeNodes
  ptr_free, self._vextersSkelTree
  ptr_free, self._masterEdges

  ptr_free, self._inUseSkelNodes
  ptr_free, self._internalIndex
  ptr_free, self._endNodesOctree
end
function C_s3DOctreeSkel::init
    ; Parameters of C_s3DOctreeSkel.
  self._vertexsOctreeGraph = ptr_new([-1])
  self._edgesOctreeGraph = ptr_new([-1,-1])

  self._skelTreeNodes = ptr_new([-1])
  self._vextersSkelTree = ptr_new([-1])
  self._masterEdges = ptr_new([-1,-1])

  self._inUseSkelNodes = ptr_new([-1])
  self._internalIndex = ptr_new([-1])
  self._endNodesOctree = ptr_new([-1])
    
  return, 1
end
pro C_s3DOctreeSkel__define
   temp = { C_s3DOctreeSkel, $
                    _vertexsOctreeGraph: ptr_new(),$
                    _edgesOctreeGraph : ptr_new(),$
                    
                    _skelTreeNodes     : ptr_new(),$
                    _vextersSkelTree   : ptr_new(),$
                    _masterEdges       : ptr_new(),$
                    
                    _inUseSkelNodes : ptr_new(),$
                    _internalIndex : ptr_new(),$
                    _endNodesOctree: ptr_new()}
end