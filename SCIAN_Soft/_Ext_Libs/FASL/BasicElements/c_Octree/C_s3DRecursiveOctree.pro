pro C_s3DRecursiveOctree::MasterOctreeCreation, xyzPoints = xyzPoints, vForceDiv = vForceDiv, vMinPointsOctree = vMinPointsOctree, vRateMinPointsOctree = vRateMinPointsOctree, vRateMinDistanceOctree = vRateMinDistanceOctree
; xyzPoints              : points of object
; vForceDiv              : Bool... force division for actual node if only one child contain all points
; vMinPointsOctree       : int...  min number of points for octree, if one child have > vMinPointsOctree, create level... else... stop division 
; vRateMinPointsOctree   : float.. min % (0.00 - 1.00) of Global points for divide octree.
; vRateMinDistanceOctree : float.. min % (0.00 - 1.00) of Global space for divide octree.
; The Volume for the principal Octree is a cube than inlude all points, based in the bigger axis.

  ; Create Octree for actual Point Cloud
   
   tmpIndices = dblarr(n_elements(xyzPoints[0,*])) * 0.0d
   self.indexPoints2MasterNode = ptr_new(tmpIndices, /no_copy)
   
   estimatedMinMaxXYZ = dblarr(2,3)
   estimatedMinMaxXYZ[0,0] = min(xyzPoints, max = dummy)
   estimatedMinMaxXYZ[1,0] = dummy
   estimatedMinMaxXYZ[0,1] = min(xyzPoints, max = dummy)
   estimatedMinMaxXYZ[1,1] = dummy
   estimatedMinMaxXYZ[0,2] = min(xyzPoints, max = dummy)
   estimatedMinMaxXYZ[1,2] = dummy   
   
   self->GenerateNode, ownParent = -1, actualIndex = 0.0d, xyzPoints = xyzPoints, vForceDiv = vForceDiv, vMinPointsOctree = vMinPointsOctree, vRateMinPointsOctree = vRateMinPointsOctree, vRateMinDistanceOctree = vRateMinDistanceOctree, estimatedMinMaxXYZ = estimatedMinMaxXYZ, currentLevel = 0
end

pro C_s3DRecursiveOctree::GenerateNode,ownParent = ownParent, actualIndex = actualIndex, xyzPoints = xyzPoints, vForceDiv = vForceDiv, vMinPointsOctree = vMinPointsOctree, vRateMinPointsOctree = vRateMinPointsOctree, vRateMinDistanceOctree = vRateMinDistanceOctree, estimatedMinMaxXYZ = estimatedMinMaxXYZ, currentLevel = currentLevel  
   localNode = obj_new('C_sOctree')
   localNode->setLevelDeep, levelDeep = currentLevel
   ; if actualIndex eq 0 => actualNode is root 
   ; Obtain values of Node Variables
   ; Indexs related with actual node
   whereIndexActual = where((*self.indexPoints2MasterNode) eq actualIndex,/L64)
   ; Dimensional Values
      localNode->setDimensions, xyzPoints = xyzPoints, indexPoints = whereIndexActual, minXYZ = estimatedMinMaxXYZ[0,*], maxXYZ = estimatedMinMaxXYZ[1,*]
   ; Link Values
   if (actualIndex eq 0) then begin
      localNode->setActiveRoot
      semiAxisUp = localNode->getMaxSemiAxisXYZ()
   endif else begin
      semiAxisUp = ((*self.masterNode)[0])->getMaxSemiAxisXYZ()
   endelse
   localNode->setParent, ownParent = ownParent
   
   ; Subdivision criteria values
   localNode->setIsOffSpring, ownOffSpring = 0b
   minPoints                = (localNode->getNumPoints() ge vMinPointsOctree)? 1b : 0b
   minRateMinPointsOctree   = (((1.0d*localNode->getNumPoints())/(1.0d*N_ELEMENTS(xyzPoints[0,*]))) ge vRateMinPointsOctree)? 1b : 0b
   minRateMinDistanceOctree = ((max(localNode->getMaxSemiAxisXYZ())/max(semiAxisUp)) ge vRateMinDistanceOctree)? 1b : 0b
   if(minPoints and minRateMinPointsOctree and minRateMinDistanceOctree) then begin
      localNode->setIsOffSpring, ownOffSpring = 1b
   endif

   ; Verify is this Node is Root, and set the Octree Nodes Array
   if(actualIndex eq 0) then begin
      self.masterNode = ptr_new(localNode)
   endif else begin
      self.masterNode = ptr_new([*self.masterNode , localNode])
   endelse

   ; Evaluate offSpring and create this
   if(localNode->getOffSpring())then begin
      self->DevelopeOffSpring, ownParent = actualIndex, whereParent = whereIndexActual, xyzPoints = xyzPoints, vForceDiv = vForceDiv, vMinPointsOctree = vMinPointsOctree, vRateMinPointsOctree = vRateMinPointsOctree, vRateMinDistanceOctree = vRateMinDistanceOctree, currentLevel = currentLevel
   endif
end

pro C_s3DRecursiveOctree::DevelopeOffSpring, ownParent = ownParent, whereParent = whereParent, xyzPoints = xyzPoints, vForceDiv = vForceDiv, vMinPointsOctree = vMinPointsOctree, vRateMinPointsOctree = vRateMinPointsOctree, vRateMinDistanceOctree = vRateMinDistanceOctree, currentLevel = currentLevel
   ; Assign the indexs for 8 tempIndex
     ; Points to relocate are in whereParent whereIndexActual
     tempCenterParent  = (*self.masterNode)[ownParent]->getCenterNode()
     tempSemiAxisParent  = (*self.masterNode)[ownParent]->getSemiAxisXYZ()
     tempWhereParentMinusX = where(xyzPoints[0,whereParent] le tempCenterParent[0], COMPLEMENT=tempWhereParentMoreX,/L64) 
     tempWhereParentMinusY = where(xyzPoints[1,whereParent] le tempCenterParent[1], COMPLEMENT=tempWhereParentMoreY,/L64)
     tempWhereParentMinusZ = where(xyzPoints[2,whereParent] le tempCenterParent[2], COMPLEMENT=tempWhereParentMoreZ,/L64)

     whereParentMinusX = -1
     whereParentMinusY = -1
     whereParentMinusZ = -1

     whereParentMoreX = -1
     whereParentMoreY = -1
     whereParentMoreZ = -1
     
     if(tempWhereParentMinusX[0] ne -1) then whereParentMinusX = whereParent(tempWhereParentMinusX)
     if(tempWhereParentMinusY[0] ne -1) then whereParentMinusY = whereParent(tempWhereParentMinusY)
     if(tempWhereParentMinusZ[0] ne -1) then whereParentMinusZ = whereParent(tempWhereParentMinusZ)

     if(tempWhereParentMoreX[0] ne -1) then whereParentMoreX = whereParent(tempWhereParentMoreX)
     if(tempWhereParentMoreY[0] ne -1) then whereParentMoreY = whereParent(tempWhereParentMoreY)
     if(tempWhereParentMoreZ[0] ne -1) then whereParentMoreZ = whereParent(tempWhereParentMoreZ)

     tempWhereParentMinusX = -1
     tempWhereParentMinusY = -1
     tempWhereParentMinusZ = -1

     tempWhereParentMoreX = -1
     tempWhereParentMoreY = -1
     tempWhereParentMoreZ = -1
   tempArrX = intarr(N_ELEMENTS(xyzPoints[0,*])) * 0
   tempArrY = intarr(N_ELEMENTS(xyzPoints[0,*])) * 0
   tempArrZ = intarr(N_ELEMENTS(xyzPoints[0,*])) * 0

   for f = 0, 7 do begin 
      ; get ActualPoints
      greaterX = ((f eq 1) or (f eq 3) or (f eq 5) or (f eq 7))? 1b : 0b
      greaterY = ((f eq 2) or (f eq 3) or (f eq 6) or (f eq 7))? 1b : 0b
      greaterZ = (f ge 4)? 1b : 0b

      ; Define selected limits... for balanced octrees
       estimatedMinMaxXYZ = dblarr(2,3)
       estimatedMinMaxXYZ[0,0] = greaterX ? (tempCenterParent[0]) : (tempCenterParent[0]-tempSemiAxisParent[0])
       estimatedMinMaxXYZ[1,0] = greaterX ? (tempCenterParent[0]+tempSemiAxisParent[0]) : (tempCenterParent[0])
       estimatedMinMaxXYZ[0,1] = greaterY ? (tempCenterParent[1]) : (tempCenterParent[1]-tempSemiAxisParent[1])
       estimatedMinMaxXYZ[1,1] = greaterY ? (tempCenterParent[1]+tempSemiAxisParent[1]) : (tempCenterParent[1])
       estimatedMinMaxXYZ[0,2] = greaterZ ? (tempCenterParent[2]) : (tempCenterParent[2]-tempSemiAxisParent[2])
       estimatedMinMaxXYZ[1,2] = greaterZ ? (tempCenterParent[2]+tempSemiAxisParent[2]) : (tempCenterParent[2])  

      ; recovery selected point
      tempActualXset = greaterX? whereParentMoreX : whereParentMinusX
      tempActualYset = greaterY? whereParentMoreY : whereParentMinusY
      tempActualZset = greaterZ? whereParentMoreZ : whereParentMinusZ

      tempArrX = tempArrX * 0
      tempArrY = tempArrY * 0
      tempArrZ = tempArrZ * 0
      
      if (tempActualXset[0] ne -1) then tempArrX[tempActualXset] = 1
      if (tempActualYset[0] ne -1) then tempArrY[tempActualYset] = 1
      if (tempActualZset[0] ne -1) then tempArrZ[tempActualZset] = 1
      
      tempIndex = double(N_ELEMENTS(*self.masterNode))
      whereTempIndex = where((tempArrX*tempArrY*tempArrZ) eq 1,/L64) 
      if (whereTempIndex[0] ne -1) then begin
          (*self.indeXPoints2MasterNode)[whereTempIndex] = tempIndex
      endif

      (*self.masterNode)[ownParent]->setChild, ownChild = tempIndex, ownIndexChild = f
      self->GenerateNode, ownParent = ownParent, actualIndex = tempIndex, xyzPoints = xyzPoints, vForceDiv = vForceDiv, vMinPointsOctree = vMinPointsOctree, vRateMinPointsOctree = vRateMinPointsOctree, vRateMinDistanceOctree = vRateMinDistanceOctree, estimatedMinMaxXYZ = estimatedMinMaxXYZ, currentLevel = currentLevel+1.0d

   endfor
   tempArrX = -1
   tempArrY = -1
   tempArrZ = -1
   whereParentMinusX = -1
   whereParentMinusY = -1
   whereParentMinusZ = -1
   whereParentMoreX = -1
   whereParentMoreY = -1
   whereParentMoreZ = -1
  tempActualXset = -1
  tempActualYset = -1
  tempActualZset = -1
end

function C_s3DRecursiveOctree::GetMasterNode
    return, self.masterNode
end

function C_s3DRecursiveOctree::GetIndexPoints2MasterNode
    return, self.indexPoints2MasterNode
end

function C_s3DRecursiveOctree::DrawSystem, thick = thick,  colV = colV, name = name , uvalue = uvalue, double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    modelsAdd = [self->drawNode(indexNode = 0, thick = thick,  colV = colV, name = name , uvalue = uvalue, double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)]
    FOR k=1.0d,N_ELEMENTS(*self.masterNode)-1 DO BEGIN
       modelsAdd = [modelsAdd , self->drawNode(indexNode = k, thick = thick,  colV = colV, name = name , uvalue = uvalue, double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)]
    ENDFOR
    return, modelsAdd
end

function C_s3DRecursiveOctree::DrawEndSystem, thick = thick,  colV = colV, name = name , uvalue = uvalue, double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    dummy = make_array(N_ELEMENTS(*self.masterNode), /BYTE , value = 0b)   
    FOR k=0.0d,N_ELEMENTS(*self.masterNode)-1 DO BEGIN
      dummy[k] = (*self.masterNode)[k]->getOffSpring()
    endfor

    whereTerminalNodes = where(dummy[*] eq 0b, icount,/L64)
    
    modelsAdd = [self->drawNode(indexNode = whereTerminalNodes[0], thick = thick,  colV = colV, name = name , uvalue = uvalue, double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)]    
         IF icount GT 1 THEN BEGIN
            FOR k=0.0d,icount-1 DO BEGIN
                if((*self.masterNode)[whereTerminalNodes[k]]->getNumPoints() gt 0) then begin
                   modelsAdd = [modelsAdd , self->drawNode(indexNode = whereTerminalNodes[k], thick = thick,  colV = colV, name = name , uvalue = uvalue, double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)]              
                endif
            ENDFOR
         ENDIF
    return, modelsAdd
end

function C_s3DRecursiveOctree::drawNode,indexNode = indexNode, thick = thick,  colV = colV, name = name , uvalue = uvalue, double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    
    minimos = (*self.masterNode)[indexNode]->getMinXYZ()
    maximos = (*self.masterNode)[indexNode]->getMaxXYZ()
    
    xVector = [minimos[0]]
    yVector = [minimos[1]]
    zVector = [minimos[2]]

    xVector = [xVector, minimos[0]]
    yVector = [yVector, maximos[1]]
    zVector = [zVector, minimos[2]]
    
    xVector = [xVector, maximos[0]]
    yVector = [yVector, maximos[1]]
    zVector = [zVector, minimos[2]]    

    xVector = [xVector, maximos[0]]
    yVector = [yVector, minimos[1]]
    zVector = [zVector, minimos[2]]  

    xVector = [xVector, minimos[0]]
    yVector = [yVector, minimos[1]]
    zVector = [zVector, minimos[2]]

    xVector = [xVector, minimos[0]]
    yVector = [yVector, minimos[1]]
    zVector = [zVector, maximos[2]]

    xVector = [xVector, minimos[0]]
    yVector = [yVector, maximos[1]]
    zVector = [zVector, maximos[2]]

    xVector = [xVector, minimos[0]]
    yVector = [yVector, maximos[1]]
    zVector = [zVector, minimos[2]]

    xVector = [xVector, maximos[0]]
    yVector = [yVector, maximos[1]]
    zVector = [zVector, minimos[2]]

    xVector = [xVector, maximos[0]]
    yVector = [yVector, maximos[1]]
    zVector = [zVector, maximos[2]]

    xVector = [xVector, minimos[0]]
    yVector = [yVector, maximos[1]]
    zVector = [zVector, maximos[2]]

    xVector = [xVector, minimos[0]]
    yVector = [yVector, minimos[1]]
    zVector = [zVector, maximos[2]]

    xVector = [xVector, maximos[0]]
    yVector = [yVector, minimos[1]]
    zVector = [zVector, maximos[2]]

    xVector = [xVector, maximos[0]]
    yVector = [yVector, maximos[1]]
    zVector = [zVector, maximos[2]]

    xVector = [xVector, maximos[0]]
    yVector = [yVector, minimos[1]]
    zVector = [zVector, maximos[2]]

    xVector = [xVector, maximos[0]]
    yVector = [yVector, minimos[1]]
    zVector = [zVector, minimos[2]]
;,  VERT_COLORS = colV
    return, obj_new('IDLgrPolyline',xVector,yVector,zVector , thick = thick, COLOR = colV, name = name, uvalue = uvalue,$
                                             double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
end

pro C_s3DRecursiveOctree::cleanup
  ptr_free, self.masterNode
  ptr_free, self.indexPoints2MasterNode
end

function C_s3DRecursiveOctree::init
    ; Parameters of C_s3DRecursiveOctree.

    return, 1
end

pro C_s3DRecursiveOctree__define
   temp = { C_s3DRecursiveOctree, masterNode : ptr_new(), indexPoints2MasterNode: ptr_new()}
end