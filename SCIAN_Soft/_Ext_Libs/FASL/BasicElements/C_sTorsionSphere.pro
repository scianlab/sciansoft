;_ = -____________________________IOISIOI____________________
; NAME:
;      C_sTorsionSphere
;
; PURPOSE:
;       - TorsionSphere-Basic-Class.
;
; AUTHOR:
;     FASL 2012
;     fsantibanez@med.uchile.cl
; CALLING SEQUENCE:
;       result = obj_new('C_sTorsionSphere' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sTorsionSphere::applyTorsion, radialActual = radialActual, radialAnterior = radialAnterior, factDecay = factDecay
    radialActual   = radialActual / sqrt(total(radialActual*radialActual))
    radialAnterior = radialAnterior / sqrt(total(radialAnterior*radialAnterior))
    
    actualIndex   =  self->getIndexIntersection(direccion = radialActual)
    anteriorIndex =  self->getIndexIntersection(direccion = radialAnterior)
    deltaAngles   =  self->getDeltaAngles(direccion1 = radialActual,direccion2 = radialAnterior)
    
    if((actualIndex ne anteriorIndex) and (actualIndex ge 0) and (anteriorIndex ge 0)) then begin
      if(anteriorIndex lt (self.numPoints-1)) then begin 
         indicesHV = self->getHorVer(indexRef = anteriorIndex)
         self->setHorTorsion, row = indicesHV[0],deltaAngle = deltaAngles[1]
         self->setVerTorsion, meridian = indicesHV[1],deltaAngle = deltaAngles[0]
         
         ;self->propagateTorsionH, row = indicesHV[0]-1,deltaAngle = deltaAngles[1], direction = -1, factor = factDecay
         ;self->propagateTorsionH, row = indicesHV[0]+1,deltaAngle = deltaAngles[1], direction = 1, factor = factDecay
         ;self->propagateTorsionV, meridian = indicesHV[1]-1,deltaAngle = deltaAngles[0], direction = -1, factor = factDecay, numIter = 1
         ;self->propagateTorsionV, meridian = indicesHV[1]+1,deltaAngle = deltaAngles[0], direction = 1, factor = factDecay, numIter = 1
      endif
    endif
end

pro  C_sTorsionSphere::propagateTorsionH, row = row,deltaAngle = deltaAngle, direction = direction, factor = factor
  if(factor gt 0.1d) then begin
    if((row ge 0) and (row lt self.numParallels))then begin
      self->setHorTorsion, row = row, deltaAngle = deltaAngle * factor
      
      self->propagateTorsionH, row = row + direction,deltaAngle = deltaAngle, direction = direction, factor = factor*factor
    endif
  endif
end
pro  C_sTorsionSphere::propagateTorsionV, meridian = meridian,deltaAngle = deltaAngle, direction = direction, factor = factor, numIter = numIter
  if(meridian eq -1)               then meridian = self.numMeridians-1 
  if(meridian eq self.numMeridians)then meridian = 0
  
  limit = (direction lt 0)? floor((self.numMeridians-1.0d)/2.0d):ceil((self.numMeridians-1.0d)/2.0d)
  if((numIter le limit) and (factor gt 0.1d)) then begin  
    self->setVerTorsion, meridian = meridian, deltaAngle = deltaAngle * factor
    
    self->propagateTorsionV,  meridian = meridian + direction,deltaAngle = deltaAngle, direction = direction, factor = factor*factor, numIter = numIter+1      
  endif
end

function C_sTorsionSphere::getHorVer, indexRef = indexRef
  ;paralelo   = 0 ... self.numParallels-1
  ;meridiando = 0 ... self.numMeridians-1

  paralelo  = floor(indexRef / self.numMeridians)
  meridiano = (indexRef mod self.numMeridians) - 1
  if(meridiano eq -1) then begin
      meridiano = self.numMeridians-1
      paralelo  = paralelo - 1
  endif
  return, [paralelo,meridiano]
end

pro C_sTorsionSphere::setHorTorsion, row = row,deltaAngle = deltaAngle
  indiceBase = self.numMeridians*row + 1 
  for i = 0, self.numMeridians-1 do begin
     indiceActual = indiceBase + i
         temp = (*(self.sphereVertexs))[*,indiceActual] 
         sphericCoords = self->getSpehericCoords( cartesianPoint = (*(self.sphereVertexs))[*,indiceActual])
         sphericCoords = sphericCoords + [0.0d, deltaAngle]
         correctedCoords = self->getSphericValuesinIntervals( spehericCoords = sphericCoords)
         distancia = sqrt(total((correctedCoords - temp)*(correctedCoords - temp)))
         (*(self.sphereVertexs))[*,indiceActual] = self->getCartesianCoords(sphericPoint = correctedCoords)
  endfor
end

function C_sTorsionSphere::getSphericValuesinIntervals, spehericCoords = spehericCoords
  ; map to right intervals
  ; tetha 0 ... pi .. open interval
  correctedCoords = spehericCoords
  if(spehericCoords[0] ne !PI) then begin
    count = floor(spehericCoords[0]/!PI)
    correctedCoords[0] = (spehericCoords[0] - count*!PI)
    case 1b of
      (spehericCoords[0] eq 0.0d): correctedCoords[0] = 0.0001d
      (spehericCoords[0] eq !PI): correctedCoords[0] = !PI - 0.0001d
      else: correctedCoords[0] = correctedCoords[0]
    endcase
  endif else correctedCoords[0] = !PI - 0.0001d
  
  if(spehericCoords[1] ne 2.0d*!PI) then begin  
    count = floor(spehericCoords[1]/(2.0d*!PI))
    correctedCoords[1] = (spehericCoords[1] - count*(2.0d*!PI))
    case 1b of
      (spehericCoords[1] eq (2.0d*!PI)): correctedCoords[1] = (2.0d*!PI) - 0.0001d
      else: correctedCoords[1] = correctedCoords[1]
    endcase
  endif else correctedCoords[0] = (2.0d*!PI) - 0.0001d
  return, correctedCoords
end

pro C_sTorsionSphere::setVerTorsion, meridian = meridian,deltaAngle = deltaAngle
  indiceBase = meridian + 1
  for i = 0, self.numParallels-1 do begin
    indiceActual = indiceBase + self.numMeridians*i
         temp = (*(self.sphereVertexs))[*,indiceActual]
         sphericCoords = self->getSpehericCoords( cartesianPoint = (*(self.sphereVertexs))[*,indiceActual])
         sphericCoords = sphericCoords + [deltaAngle,0.0d]
         correctedCoords = self->getSphericValuesinIntervals( spehericCoords = sphericCoords)
         (*(self.sphereVertexs))[*,indiceActual] = self->getCartesianCoords(sphericPoint = correctedCoords)
         distancia = sqrt(total((correctedCoords - temp)*(correctedCoords - temp)))
         if(distancia lt 0.5d * self.sphereRadious) then begin
            (*(self.sphereVertexs))[*,indiceActual] = self->getCartesianCoords(sphericPoint = correctedCoords)
         endif
  endfor
end

function C_sTorsionSphere::getCartesianCoords, sphericPoint = sphericPoint
  directConv = [self.sphereRadious * sin(sphericPoint[0]) * cos(sphericPoint[1]),self.sphereRadious * sin(sphericPoint[0]) * sin(sphericPoint[1]),self.sphereRadious * cos(sphericPoint[0])]
  return, self.centerXYZ + directConv[2]*self.upDirection + directConv[0]*self.ortDir1 + directConv[1]*self.ortDir2 
end

function C_sTorsionSphere::getSpehericCoords, cartesianPoint = cartesianPoint
  ; restaurar efectos locales
  A = [[self.upDirection[0],self.ortDir1[0],self.ortDir2[0]],$
       [self.upDirection[1],self.ortDir1[1],self.ortDir2[1]],$
       [self.upDirection[2],self.ortDir1[2],self.ortDir2[2]]]
  b = cartesianPoint-self.centerXYZ 
  dSolved = LA_INVERT(A,/double)## b
  cartesianPoint = [dSolved[1],dSolved[2],dSolved[0]]
  ;tetha =
    case 1b of
      (cartesianPoint[2] gt 0.0d): begin
        tetha = atan(sqrt(total(cartesianPoint[0:1]*cartesianPoint[0:1]))/cartesianPoint[2])
        break
      end
      (cartesianPoint[2] eq 0.0d): begin 
        tetha = !PI
        break
      end
      else: tetha = !PI + atan(sqrt(total(cartesianPoint[0:1]*cartesianPoint[0:1]))/cartesianPoint[2])
    endcase  

  ;phi   =
    case 1b of
      ((cartesianPoint[0] gt 0.0d) and (cartesianPoint[1] ge 0.0d)): begin
          phi = atan(cartesianPoint[1] /cartesianPoint[0])
        break
      end
      ((cartesianPoint[0] gt 0.0d) and (cartesianPoint[1] lt 0.0d)): begin
        phi = 2.0d*!PI + atan(cartesianPoint[1] /cartesianPoint[0])
        break
      end
      
      (cartesianPoint[0]  eq 0.0d): begin 
        phi = (cartesianPoint[1] ge 0.0d)?(!PI/2.0d):(-!PI/2.0d)
        break
      end
      else: phi = !PI + atan(cartesianPoint[1]/cartesianPoint[0])
    endcase  

  return, [tetha,phi]
end

function C_sTorsionSphere::getDeltaAngles, direccion1 = direccion1, direccion2 = direccion2 
  direccion1   = direccion1 / sqrt(total(direccion1*direccion1))  
  direccion2   = direccion2 / sqrt(total(direccion2*direccion2))
  proyeccion1  = self.centerXYZ + self.sphereRadious * direccion1
  proyeccion2  = self.centerXYZ + self.sphereRadious * direccion2  
  
  spehericCoords1 = self->getSpehericCoords(cartesianPoint = proyeccion1) 
  spehericCoords2 = self->getSpehericCoords(cartesianPoint = proyeccion2)
  
  return, spehericCoords2-spehericCoords1
end
function C_sTorsionSphere::getIndexIntersection, direccion = direccion
  direccion   = direccion / sqrt(total(direccion*direccion))  
  
  proyeccion  = self.centerXYZ + self.sphereRadious * direccion
  distancias = dblarr(self.numPoints) * 0.0d
  for i = 0, self.numPoints-1 do begin
    distancias[i] = sqrt(total(((*(self.sphereVertexs))[*,i] - proyeccion)*((*(self.sphereVertexs))[*,i] - proyeccion)))
  endfor
  dummy = min(distancias,indexMin)
  
  return, indexMin
end

function C_sTorsionSphere::getpVertexs
  return, self.sphereVertexs
end

function C_sTorsionSphere::getPMesh
  return, self.sphereMesh
end

function C_sTorsionSphere::getCenter
  return, self.centerXYZ
end

function C_sTorsionSphere::init, numParalelos = numParalelos, numMeridianos =  numMeridianos , centroXYZ = centroXYZ, upDir = upDir, radious = radious
    ; Parameters of C_sTorsionSphere.
    self.numParallels = numParalelos
    self.numMeridians = numMeridianos
    
    
    self.sphereRadious = radious
    self.centerXYZ     = centroXYZ
    self.upDirection   = upDir

    self.numPoints     = 2.0d + (self.numParallels * self.numMeridians)
    self.numTriangles  = 2.0d*self.numMeridians
    self.numSquares    = self.numMeridians*(self.numParallels-1.0d)
    
    ; Create mesh Structure
    dummy               = dblarr(4*self.numTriangles+5*self.numSquares) * 0.0d
    self.sphereMesh     = ptr_new(dummy,/no_copy)
    
    posBase = 0 
    for f = 0, self.numMeridians-1 do begin
      posInit = posBase + 4*f 
      posFin  = posInit + 3
      
      indexInit = f+1
      vectorIndices = [3,0,indexInit, (f lt (self.numMeridians-1))? (indexInit+1) : 1]
      (*self.sphereMesh)[posInit:posFin] = vectorIndices
    endfor 

    posBase = 4*(0.5d * self.numTriangles)
    selectedRow = 0
    selectedMeridian = 0
    indexInitLine    = 1
    for f = 0, self.numSquares-1 do begin
      
      posInit = posBase + 5*f
      posFin  = posInit + 4
      
      indexInitSquareActual = indexInitLine + selectedMeridian
      indexInitSquareNext   = indexInitSquareActual + self.numMeridians
      
      bEnd = (selectedMeridian eq (self.numMeridians-1))? 1b : 0b
      
      ;vectorIndices = [4,indexInitSquareActual,bEnd? indexInitLine : (indexInitSquareActual+1),indexInitSquareNext,bEnd? (indexInitLine+ self.numMeridians) : (indexInitSquareNext+1)] 
      vectorIndices = [4,indexInitSquareActual,indexInitSquareNext,bEnd? (indexInitLine+ self.numMeridians) : (indexInitSquareNext+1),bEnd? indexInitLine : (indexInitSquareActual+1)]
      (*self.sphereMesh)[posInit:posFin] = vectorIndices
      
      selectedMeridian++
      if(selectedMeridian eq self.numMeridians) then begin
        selectedMeridian = 0
        selectedRow++
        
        indexInitLine     = 1 + selectedRow*self.numMeridians
      endif
    endfor 

    posBase = 4*(0.5d * self.numTriangles) + 5*self.numSquares
    indexLast = 1 + (self.numMeridians*(self.numParallels -1))
    for f = 0, (self.numMeridians)-1 do begin
      posInit = posBase + 4*f
      posFin  = posInit + 3
      indexInit = indexLast + f
      vectorIndices = [3,indexInit, (f lt (self.numMeridians-1))? (indexInit+1) : indexLast,(self.numPoints-1)]
      (*self.sphereMesh)[posInit:posFin] = vectorIndices
    endfor 
    
    
   ; Create Vertexs 
    dummy               = dblarr(3,self.numPoints)  * 0.0d
    self.sphereVertexs = ptr_new(dummy,/no_copy)
    
    self.upDirection = upDir / sqrt(total(upDir*upDir))
    tempNormal = self.upDirection
    
    dummy = max(tempNormal,indexMax)
    case 1b of
      (abs(self.upDirection[0]) eq 0.0d): tempNormal[0] = 1.0d
      (abs(self.upDirection[1]) eq 0.0d): tempNormal[1] = 1.0d
      (abs(self.upDirection[2]) eq 0.0d): tempNormal[2] = 1.0d
      else: tempNormal[indexMax] = 0.0d
    endcase
    tempNormal = tempNormal / sqrt(total(tempNormal*tempNormal))
      
    vOrt1 = CROSSP(self.upDirection,tempNormal)
    vOrt1 = vOrt1 / sqrt(total(vOrt1*vOrt1))
    
    vOrt2 = CROSSP(self.upDirection,vOrt1)
    vOrt2 = vOrt2 / sqrt(total(vOrt2*vOrt2))
     
    self.ortDir1 = vOrt1
    self.ortDir2 = vOrt2
    
    (*self.sphereVertexs)[*,0] = self.centerXYZ + (self.sphereRadious*self.upDirection)
    actualIndex = 1  
    stepTetha = !PI / (self.numParallels + 1.0d)
    stepPhi   = 2.0d*!PI / (self.numMeridians)
    for tetha = stepTetha, !PI-stepTetha+(0.1d*stepTetha), stepTetha do begin
      for phi = 0.0d, ((2.0d*!PI)-stepPhi+(0.1d*stepPhi)), stepPhi do begin
        xAxis = self.sphereRadious * sin(tetha) * cos(phi)
        yAxis = self.sphereRadious * sin(tetha) * sin(phi)
        zAxis = self.sphereRadious * cos(tetha)
        
        (*self.sphereVertexs)[*,actualIndex] = self.centerXYZ + zAxis*self.upDirection + xAxis*vOrt1 + yAxis*vOrt2
        actualIndex++
      endfor
    endfor
    (*self.sphereVertexs)[*,self.numPoints-1] = self.centerXYZ - (self.sphereRadious*self.upDirection)
    return, 1
end

pro C_sTorsionSphere__define
   interClass = { C_sTorsionSphere, $ 
                  numParallels     : 0.0d, $          ;                  
                  numMeridians     : 0.0d, $          ;
                  numTriangles     : 0.0d, $          ;
                  numSquares       : 0.0d, $          ;
                  numPoints        : 0.0d, $          ;
                  
                  sphereRadious    : 0.0d, $          ;
                  
                  centerXYZ        : dblarr(3), $  ;
                  upDirection      : dblarr(3), $  ;
                  ortDir1          : dblarr(3), $  ;
                  ortDir2          : dblarr(3), $  ;
                  
                  sphereMesh       : ptr_new(), $  ;
                  sphereVertexs    : ptr_new()  $  ;
                }
end