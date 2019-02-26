; FASL 2012
; fsantibanez@med.uchile.cl

function s_getAlfaBetRectPoints, pointOrigen, pointDestino, direccion
   if (direccion[0] ne 0.0) then return, (pointDestino[0] - pointOrigen[0])/direccion[0]
   if (direccion[1] ne 0.0) then return, (pointDestino[1] - pointOrigen[1])/direccion[1]
   if (direccion[2] ne 0.0) then return, (pointDestino[2] - pointOrigen[2])/direccion[2]
   return, 0.0
end

; RENDER PLANES ... Verify conditions and optimization .. FASL
function s_getPointsIntersectionPlanes, dimensionesBox, bUsePlane1, normalPlane1, dParam1, bUsePlane2, oROI3DGroup, normalPlane2, dParam2
   
    principalIntersections = make_array(2,3,4);
    principalIntersections[*,*,*] = -100000.0   

    actualIndex1 = 0
    actualIndex2 = 0

      ; Render Lines of Intersection Cube-Planes
      ; For the 8 faces of Cube
          ; Face 1
              ; For 4 rays in face
                  ; ray 1
    orRay   = [dimensionesBox[0,0], dimensionesBox[1,0], dimensionesBox[2,0]]
    dsRay   = [dimensionesBox[0,0], dimensionesBox[1,1], dimensionesBox[2,0]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif

    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
              if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif
    ; ray 2
    orRay   = [dimensionesBox[0,0], dimensionesBox[1,0], dimensionesBox[2,0]]
    dsRay   = [dimensionesBox[0,1], dimensionesBox[1,0], dimensionesBox[2,0]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif

    ; ray 3
    orRay   = [dimensionesBox[0,1], dimensionesBox[1,1], dimensionesBox[2,0]]
    dsRay   = [dimensionesBox[0,1], dimensionesBox[1,0], dimensionesBox[2,0]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
      if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif

    ; ray 4
    orRay   = [dimensionesBox[0,1], dimensionesBox[1,1], dimensionesBox[2,0]]
    dsRay   = [dimensionesBox[0,0], dimensionesBox[1,1], dimensionesBox[2,0]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif
    
    ;
    ; For the opposite plane
    ; ray 5
    orRay   = [dimensionesBox[0,0], dimensionesBox[1,0], dimensionesBox[2,1]]
    dsRay   = [dimensionesBox[0,0], dimensionesBox[1,1], dimensionesBox[2,1]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif

    ; ray 6
    orRay   = [dimensionesBox[0,0], dimensionesBox[1,0], dimensionesBox[2,1]]
    dsRay   = [dimensionesBox[0,1], dimensionesBox[1,0], dimensionesBox[2,1]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif

    ; ray 7
    orRay   = [dimensionesBox[0,1], dimensionesBox[1,1], dimensionesBox[2,1]]
    dsRay   = [dimensionesBox[0,1], dimensionesBox[1,0], dimensionesBox[2,1]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
      if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif
    ; ray 8
    orRay   = [dimensionesBox[0,1], dimensionesBox[1,1], dimensionesBox[2,1]]
    dsRay   = [dimensionesBox[0,0], dimensionesBox[1,1], dimensionesBox[2,1]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif
    ;
    ; Residual rays
    ; ray 9
    orRay   = [dimensionesBox[0,0], dimensionesBox[1,1], dimensionesBox[2,0]]
    dsRay   = [dimensionesBox[0,0], dimensionesBox[1,1], dimensionesBox[2,1]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif
    ; ray 10
    orRay   = [dimensionesBox[0,0], dimensionesBox[1,0], dimensionesBox[2,0]]
    dsRay   = [dimensionesBox[0,0], dimensionesBox[1,0], dimensionesBox[2,1]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay*dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif
    ; Opposite Residual Rays
    ; Residual rays
    ; ray 11
    orRay   = [dimensionesBox[0,1], dimensionesBox[1,1], dimensionesBox[2,0]]
    dsRay   = [dimensionesBox[0,1], dimensionesBox[1,1], dimensionesBox[2,1]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif
    ; ray 12
    orRay   = [dimensionesBox[0,1], dimensionesBox[1,0], dimensionesBox[2,0]]
    dsRay   = [dimensionesBox[0,1], dimensionesBox[1,0], dimensionesBox[2,1]]
    dirRay  = dsRay - orRay
    moduloD = sqrt(total(dirRay * dirRay))
    dirRay  = dirRay / moduloD
    alfaMax = s_getAlfaBetRectPoints(orRay,dsRay,dirRay)

    if ((bUsePlane1) and (total(dirRay*normalPlane1) ne 0.0) and (actualIndex1 lt 4)) then begin
       alfa = - (total(orRay*normalPlane1) + dParam1) / total(dirRay*normalPlane1)
       if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
          intersection = orRay + alfa * dirRay
          if (total(intersection - principalIntersections[0,*,actualIndex1]) ne 0.0) then begin
            principalIntersections[0,*,actualIndex1] = intersection
            actualIndex1++
          endif
       endif
    endif
    if (bUsePlane2) then begin
      if ((total(dirRay*normalPlane2) ne 0.0) and (actualIndex2 lt 4)) then begin
         alfa = - (total(orRay*normalPlane2) + dParam2) / total(dirRay*normalPlane2)
         if ((alfa ge 0.0) and (alfa le alfaMax)) then begin
            intersection = orRay + alfa * dirRay
            if (total(intersection - principalIntersections[1,*,actualIndex2]) ne 0.0) then begin
              principalIntersections[1,*,actualIndex2] = intersection
              actualIndex2++
            endif
         endif
      endif
    endif
   return, principalIntersections
end

function s_getOrderPlane, principalIntersections,dimension
  ; TODO type of the array elements in make_array... default?
  principalIntersectionsTemp = make_array(3,4)
  for coord = 0, 2 do begin ; for x then for y then fo z 
     for indexElement = 0, dimension -1 do begin
       pPos = where(principalIntersections[coord,*] eq principalIntersections[coord,indexElement],count)
       if (count gt 1) then begin
         principalIntersectionsTemp[*,0] = principalIntersections[*,pPos[0]]
         principalIntersectionsTemp[*,1] = principalIntersections[*,pPos[1]]

         vectorRef = principalIntersectionsTemp[*,0] - principalIntersectionsTemp[*,1]
         indexNearest = -1
         angleMax = 0.0
         for rev = 0, dimension -1 do begin
         ; obtain neasrest vertex to second info
           if (principalIntersections[coord,rev] ne principalIntersections[coord,indexElement]) then begin

              vectorAct = principalIntersections[*,rev] - principalIntersectionsTemp[*,0]

              dotProduct = total(vectorRef * vectorAct)
              normVoNormLo = sqrt(total(vectorRef * vectorRef) * total(vectorAct * vectorAct))

              cosAngle = dotProduct / normVoNormLo
              angulo   = abs(acos(cosAngle))

              if (angulo ge angleMax) then begin
                angleMax = angulo
                indexNearest = rev
              endif
            endif
          endfor
          
          if (indexNearest ne -1) then begin
            principalIntersectionsTemp[*,2] = principalIntersections[*,indexNearest]

            if (dimension gt 3) then begin
              dummy = [0,1,2,3]
              pPos = where(dummy ne pPos[0] and $
                           dummy ne pPos[1] and $
                           dummy ne indexNearest,count)
              principalIntersectionsTemp[*,3] = principalIntersections[*,pPos[0]]
            endif
            if (principalIntersectionsTemp[0,3] eq -100000.0) then principalIntersectionsTemp[*,3] = principalIntersectionsTemp[*,0]
            return, principalIntersectionsTemp
          endif
        endif
      endfor
   endfor
   return, principalIntersections 
end

; PLanos arbitrarios
;TODO anglesRespectArbitraryOrthogonalPlane
function s_anglesRespectArbitraryOrthogonalPlane, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference
    ; funcion que calcula el angulo respecto a un plano arbitrario, se necesitan los vertices del plano
    ; se calcula luego los angulos de los ejes principales de cada objeto respecto al plano.
     p = make_array(4,4,/DOUBLE)
     new_verts = make_array(4,4,/DOUBLE)
     ;recojo la matriz de transformacion de los objetos
     s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
     widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
       oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface UnDrifted Model')
        if not(obj_valid(oMeshModel)) then oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
            if not(obj_valid(oMeshModel)) then oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
    widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
     
     ctm_plano_pol=(oObjectModel->get(position = 0))->getctm()
     ctm_mesh_pol=(oMeshModel->get(position = 0))->getctm()
     
     (oObjectModel->get(position = 0))->getProperty, DATA = verts
     (oObjectModel->get(position = 1))->getProperty, DATA = verts2
     p[0:2,*]=verts
     p[3,*]=1
     for i=0,3 do new_verts[*,i] = p[*,i]#ctm_plano_pol
     verts = new_verts[0:2,*]
     ;plano ortogonal
     p[0:2,*]=verts2
     p[3,*]=1
     for i=0,3 do new_verts[*,i] = p[*,i]#ctm_plano_pol
     verts2 = new_verts[0:2,*]
     conn=[4,3,2,1,0]
     scaleAxis=1
     normal=COMPUTE_MESH_NORMALS(verts,conn)
     normal2=COMPUTE_MESH_NORMALS(verts2,conn)
     
     xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
     axis_first = fltArr(3,2)
     alpha = make_array(oGroupReference->count(),/DOUBLE)
     alpha2 = make_array(oGroupReference->count(),/DOUBLE)
     eigenVect = make_array(3, 3, oGroupReference->count(), /float)
     
     ;usando ASIN con vector director de cada objeto
     axis_actual=make_array(4,2,/DOUBLE)
     signos = s_distanceRespectArbitraryPlane( oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference) 
     for i = 0, (oGroupReference->count())-1 do begin
         pEigenSys = (oGroupReference->get(position = i))->calcEigenSys()
         pEigenSys = (oGroupReference->get(position = i))->getpEigenSys()
         eigenVect[*,*,i] = (*pEigenSys).eigenVect
         
         scaleEV = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals) / max(((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)) * scaleAxis
         scaleEV = [scaleAxis, scaleAxis, scaleAxis]
         
         axis_first[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis_first[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis_actual[0:2,*]=axis_first[0:2,*]
         axis_actual[3,*]=1
         axis_actual[*,0]=axis_actual[*,0]#ctm_mesh_pol
         axis_actual[*,1]=axis_actual[*,1]#ctm_mesh_pol
         dir=axis_actual[0:2,1] - axis_actual[0:2,0]
         alpha[i] = abs((asin(abs((normal[0,0] * dir[0] + normal[1,0] * dir[1] + normal[2,0] * dir[2]))/(sqrt(total(normal[*,0]^2))*sqrt(total(dir^2)))) * !radeg))
;         if (signos[i] le 0) then alpha[i] += 90
      endfor
            
     return,alpha
end

function s_anglesRespectArbitraryPlane, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference
   ; funcion que calcula el angulo respecto a un plano arbitrario, se necesitan los vertices del plano
   ; se calcula luego los angulos de los ejes principales de cada objeto respecto al plano.
     p = make_array(4,4,/DOUBLE)
     new_verts = make_array(4,4,/DOUBLE)
     ;recojo la matriz de transformacion de los objetos
     s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
     widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
        oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface UnDrifted Model')
        if not(obj_valid(oMeshModel)) then oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
            if not(obj_valid(oMeshModel)) then oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
     widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
     
     ctm_plano_pol=(oObjectModel->get(position = 0))->getctm()
     ctm_mesh_pol=(oMeshModel->get(position = 0))->getctm()
     
     (oObjectModel->get(position = 0))->getProperty, DATA = verts
     p[0:2,*]=verts
     p[3,*]=1
     for i=0,3 do new_verts[*,i] = p[*,i]#ctm_plano_pol
     verts = new_verts[0:2,*]
     conn=[4,3,2,1,0]
     scaleAxis=1
     normal=COMPUTE_MESH_NORMALS(verts,conn)
     xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
     axis_first = fltArr(3,2)
     alpha = make_array(oGroupReference->count(),/DOUBLE)
     
     ;usando ASIN con vector director de cada objeto
     axis_actual=make_array(4,2,/DOUBLE)
     signos = s_distanceRespectArbitraryPlane( oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference)
     for i = 0, (oGroupReference->count())-1 do begin
         scaleEV = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals) / max(((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)) * scaleAxis
         scaleEV = [scaleAxis, scaleAxis, scaleAxis]
         
         axis_first[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis_first[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis_actual[0:2,*]=axis_first[0:2,*]
         axis_actual[3,*]=1
         axis_actual[*,0]=axis_actual[*,0]#ctm_mesh_pol
         axis_actual[*,1]=axis_actual[*,1]#ctm_mesh_pol
         dir=axis_actual[0:2,1] - axis_actual[0:2,0]
         alpha[i] = (asin(abs((normal[0,0] * dir[0] + normal[1,0] * dir[1] + normal[2,0] * dir[2]))/(sqrt(total(normal[*,0]^2))*sqrt(total(dir^2)))) * !radeg)
         if (signos[i] le 0) and (alpha[i] gt 0) then alpha[i] = -1.0*alpha[i]
      endfor
            
     return,alpha
end

function s_distanceRespectArbitraryPlane, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference
;funcion que calcula la distancia del centro de masa de cada objeto al plano
; como este plano esta orientado por la normal, las distancias pueden tener valor negativo
     
     pp = make_array(4,4,/DOUBLE)
     new_verts = make_array(4,4,/DOUBLE)
     nObjects = oGroupReference->count()
     xyzCenter = make_array(3,nObjects,/DOUBLE)
     distancia = make_array(nObjects,/DOUBLE)
     pc=make_array(4,oGroupReference->count())
     new_xyzCenter=make_array(4,oGroupReference->count(),/DOUBLE)
     conn=[4,3,2,1,0]
     
     s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
      widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
            ; First try to extract planes undrifted
            oPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D UnDrifted Plane Model Base')
            if not(obj_valid(oPlaneModel)) then oPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Plane Model Base')
            
            oOrthogonalPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D UnDrifted Plane Model Orthogonal')
            if not(obj_valid(oOrthogonalPlaneModel)) then oOrthogonalPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Plane Model Orthogonal')
            
            if not(obj_valid(oOrthogonalPlaneModel)) then oOrthogonalPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D UnDrifted Plane Model Complementary')
            if not(obj_valid(oOrthogonalPlaneModel)) then oOrthogonalPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Plane Model Complementary')
       widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
       if(obj_valid(oPlaneModel)) then (oObjectModel->get(position = 0))->getProperty, DATA = verts
       if(obj_valid(oOrthogonalPlaneModel)) then (oObjectModel->get(position = 1))->getProperty, DATA = verts
       
     ;recojo la matriz de transformacion de los objetos
     s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
     widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
        oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface UnDrifted Model')
        if not(obj_valid(oMeshModel)) then oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
            if not(obj_valid(oMeshModel)) then oMeshModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
      widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
     
     ctm_plano_pol=(oObjectModel->get(position = 0))->getctm()
     ctm_mesh_pol=(oMeshModel->get(position = 0))->getctm()
     
     ;pp = plane points
     pp[0:2,*]=verts
     pp[3,*]=1
     
     for i=0, nObjects-1 do begin
        xyzCenter[*,i] = (oGroupReference->get(position = i))->getCenterXYZ()
     endfor
     ;pc = points of Centroids
     pc[0:2,*]=xyzCenter
     pc[3,*]=1
     
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_plano_pol)
      for i=0,nObjects-1 do new_xyzCenter[*,i] = (pc[*,i]#ctm_mesh_pol)
      verts = new_verts[0:2,*]
      xyzCenter=new_xyzCenter[0:2,*]
     
     normal=COMPUTE_MESH_NORMALS(verts,conn)
     for i=0, nObjects-1 do begin
        ; calculo las distancias reales desde el plano a los centros de masa
        distancia[i] = total(normal(*,0)*(xyzCenter[*,i]-verts(*,0)))
     endfor
     
     return, distancia    
end
