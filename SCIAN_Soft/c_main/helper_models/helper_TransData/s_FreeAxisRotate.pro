 ; Transform centers by correction between times transformation matrixs
 ; based on representations by http://inside.mines.edu/~gmurray/ArbitraryAxisRotation/
 
 ; vRef  = [a,b,c]
 ; vDir  = [u,v,w]
 ; theta = vAngle
function s_FreeAxisRotateMatrix, vRef = vRef, vDir = vDir, vAngle = vAngle

  freeRotateMatrix = MAKE_ARRAY(4,4,/Double, value = 0.0)

  freeRotateMatrix[0,0] = vDir[0]*vDir[0] + (vDir[1]*vDir[1] + vDir[2]*vDir[2])*cos(vAngle)
  freeRotateMatrix[1,0] = vDir[0]*vDir[1]*(1.0-cos(vAngle))  - vDir[2]*sin(vAngle)
  freeRotateMatrix[2,0] = vDir[0]*vDir[2]*(1.0-cos(vAngle))  + vDir[1]*sin(vAngle)  
  freeRotateMatrix[3,0] = (vRef[0]*(vDir[1]*vDir[1] + vDir[2]*vDir[2]) - vDir[0]*(vRef[1]*vDir[1]+vRef[2]*vDir[2]))*(1.0-cos(vAngle))   +   (vRef[1]*vDir[2] - vRef[2]*vDir[1])*sin(vAngle)

  freeRotateMatrix[0,1] = vDir[0]*vDir[1]*(1.0-cos(vAngle))  + vDir[2]*sin(vAngle)
  freeRotateMatrix[1,1] = vDir[1]*vDir[1] + (vDir[0]*vDir[0] + vDir[2]*vDir[2])*cos(vAngle)
  freeRotateMatrix[2,1] = vDir[1]*vDir[2]*(1.0-cos(vAngle))  - vDir[1]*sin(vAngle)
  freeRotateMatrix[3,1] = (vRef[1]*(vDir[0]*vDir[0] + vDir[2]*vDir[2]) - vDir[1]*(vRef[0]*vDir[0]+vRef[2]*vDir[2]))*(1.0-cos(vAngle))   +   (vRef[2]*vDir[0] - vRef[0]*vDir[2])*sin(vAngle)

  freeRotateMatrix[0,2] = vDir[0]*vDir[2]*(1.0-cos(vAngle))  - vDir[1]*sin(vAngle)
  freeRotateMatrix[1,2] = vDir[1]*vDir[2]*(1.0-cos(vAngle))  + vDir[1]*sin(vAngle)
  freeRotateMatrix[2,2] = vDir[2]*vDir[2] + (vDir[0]*vDir[0] + vDir[1]*vDir[1])*cos(vAngle)  
  freeRotateMatrix[3,2] = (vRef[2]*(vDir[0]*vDir[0] + vDir[1]*vDir[1]) - vDir[2]*(vRef[0]*vDir[0]+vRef[1]*vDir[1]))*(1.0-cos(vAngle))   +   (vRef[0]*vDir[1] - vRef[1]*vDir[0])*sin(vAngle)

  freeRotateMatrix[0,3] = 0.0 
  freeRotateMatrix[1,3] = 0.0
  freeRotateMatrix[2,3] = 0.0  
  freeRotateMatrix[3,3] = 1.0
                 
  return, freeRotateMatrix 
end
 
function s_FreeAxisRotate, vPoint = vPoint, vRef = vRef, vDir = vDir, vAngle = vAngle
  newPoint = vPoint  

  newPoint[0] = (vRef[0]*(vDir[1]*vDir[1]+vDir[2]*vDir[2]) $
                  - vDir[0]*( Total(vRef[1:2]*vDir[1:2]) $
                            - Total(vDir*vPoint)))*(1.0 - cos(vAngle)) + $
                vPoint[0]*cos(vAngle) + $
                ( -vRef[2]*vDir[1]+vRef[1]*vDir[2] - $
                   vDir[2]*vPoint[1] + vDir[1]*vPoint[2])*sin(vAngle)
                 
  newPoint[1] = (vRef[1]*(vDir[0]*vDir[0]+vDir[2]*vDir[2]) $
                  - vDir[1]*( (vRef[0]*vDir[0] + vRef[2]*vDir[2]) $
                            - Total(vDir*vPoint)))*(1.0 - cos(vAngle)) + $
                vPoint[1]*cos(vAngle) + $
                (  vRef[2]*vDir[0] - vRef[0]*vDir[2] + $
                   vDir[2]*vPoint[0] - vDir[0]*vPoint[2])*sin(vAngle)

  newPoint[2] = (vRef[2]*(vDir[0]*vDir[0]+vDir[1]*vDir[1]) $
                  - vDir[2]*( Total(vRef[0:1]*vDir[0:1]) $
                            - Total(vDir*vPoint)))*(1.0 - cos(vAngle)) + $
                vPoint[2]*cos(vAngle) + $
                ( -vRef[1]*vDir[0] + vRef[0]*vDir[1] - $
                   vDir[1]*vPoint[0] + vDir[0]*vPoint[1])*sin(vAngle)
  
return, newPoint 
end

function s_RecalculateCenters, stack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = actual3DGroupObject, actualTime = actualTime, actualNumber = actualNumber, chPos = chPos, clusPos = clusPos
  nObjects = actual3DGroupObject->count()
  xyzCenter = make_array(3,/DOUBLE)

  xyzCenter = (actual3DGroupObject->get(position = actualNumber))->getCenterXYZ()

   driftMatrix    = MAKE_ARRAY(4,4,/Double, value = 0.0)
   tempMatrixRot1 = MAKE_ARRAY(4,4,/Double, value = 0.0)
   tempMatrixRot2 = MAKE_ARRAY(4,4,/Double, value = 0.0)
   tempMatrixTras = MAKE_ARRAY(4,4,/Double, value = 0.0)

   tempMatrixTras[0,0] = 1.0
   tempMatrixTras[1,1] = 1.0
   tempMatrixTras[2,2] = 1.0
   tempMatrixTras[3,3] = 1.0
         
   driftMatrix[0,0] = 1.0
   driftMatrix[1,1] = 1.0
   driftMatrix[2,2] = 1.0
   driftMatrix[3,3] = 1.0

   tempMatrixRot1[0,0] = 1.0
   tempMatrixRot1[1,1] = 1.0
   tempMatrixRot1[2,2] = 1.0
   tempMatrixRot1[3,3] = 1.0

   tempMatrixRot2[0,0] = 1.0
   tempMatrixRot2[1,1] = 1.0
   tempMatrixRot2[2,2] = 1.0
   tempMatrixRot2[3,3] = 1.0
         
    ctm_mesh_tLast=make_array(4,4,/double, value = 0.0)
    ctm_plane_tLast=make_array(4,4,/double, value = 0.0)
    ctm_orthogonalplane_tLast=make_array(4,4,/double, value = 0.0)
    ctm_complementaryplane_tLast=make_array(4,4,/double, value = 0.0)

    ctm_mesh_tActual=make_array(4,4,/double, value = 0.0)
    ctm_plane_tActual=make_array(4,4,/double, value = 0.0)
    ctm_orthogonalplane_tActual=make_array(4,4,/double, value = 0.0)
    ctm_complementaryplane_tActual=make_array(4,4,/double, value = 0.0)
    
     ; First --- we need the saved matrix for actual time "i" .. and for time "0" (reference for system...)
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
     (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

     name = strCompress('GraphicState_Time_' + strCompress(0) + '.sav', /rem)
     filename = strCompress(path + strcompress(name, /rem))
     flag = FILE_TEST(filename)
      line = make_Array(4,/double)
      i=0
      if ~flag then return, xyzCenter
      if flag then begin
          GET_LUN, inunit
          openr, inunit, filename
          while ~ EOF(inunit) do begin
             READF, inunit, line 
             if( i le 3) then ctm_mesh_tLast[*,i]=line
             if((i gt 3) and (i le 7) ) then ctm_plane_tLast[*,i-4]=line
             if((i gt 7) and (i le 11) ) then ctm_orthogonalplane_tLast[*,i-8]=line
             if((i gt 11) ) then ctm_complementaryplane_tLast[*,i-12]=line
             i++ 
          endwhile    
          FREE_LUN, inunit
     endif
   
     name = strCompress('GraphicState_Time_' + strCompress(actualTime) + '.sav', /rem)
     filename = strCompress(path + strcompress(name, /rem))
     flag = FILE_TEST(filename)
      line = make_Array(4,/double)
      i=0
      if ~flag then return, xyzCenter
      if flag then begin
          GET_LUN, inunit
          openr, inunit, filename
          while ~ EOF(inunit) do begin
             READF, inunit, line 
             if( i le 3) then ctm_mesh_tActual[*,i]=line
             if((i gt 3) and (i le 7) ) then ctm_plane_tActual[*,i-4]=line
             if((i gt 7) and (i le 11) ) then ctm_orthogonalplane_tActual[*,i-8]=line
             if((i gt 11) ) then ctm_complementaryplane_tActual[*,i-12] = line
             i++ 
          endwhile    
          FREE_LUN, inunit
     endif

     if( (total(abs(ctm_plane_tLast)) eq 0.0) or $
         (total(abs(ctm_orthogonalplane_tLast)) eq 0.0) or $
         (total(abs(ctm_complementaryplane_tLast)) eq 0.0) or $
         
         (total(abs(ctm_plane_tActual)) eq 0.0) or $
         (total(abs(ctm_orthogonalplane_tActual)) eq 0.0) or $
         (total(abs(ctm_complementaryplane_tActual)) eq 0.0) ) then return, xyzCenter
; Estructuras requeridas

     pp = make_array(4,4,/DOUBLE)
     new_verts = make_array(4,4,/DOUBLE)
 
     vertsPlaneB_Origen = make_array(3,4,/DOUBLE)
     vertsPlaneO_Origen = make_array(3,4,/DOUBLE)
     vertsPlaneC_Origen = make_array(3,4,/DOUBLE)
     vertsPlaneB_Actual = make_array(3,4,/DOUBLE)
     vertsPlaneO_Actual = make_array(3,4,/DOUBLE)
     vertsPlaneC_Actual = make_array(3,4,/DOUBLE)
     
     conn=[4,3,2,1,0]

; Obtener posicion de los planos en base a las matrices de transformacion

    c = actual3DGroupObject->getGroupCenterXYZ()
    center=[total(c[0,*])/actual3DGroupObject->count(),total(c[1,*])/actual3DGroupObject->count(),total(c[2,*])/actual3DGroupObject->count()]
    verts_Actual = fltarr(3,4)
    verts_Actual[0:2,0]=[(center[0] - max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]
    verts_Actual[0:2,1]=[(center[0] - max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts_Actual[0:2,2]=[(center[0] + max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts_Actual[0:2,3]=[(center[0] + max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]

    C_sROI3DGroupObjOrigen = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = 0, chPos = chPos, clusPos = clusPos)  
    c = C_sROI3DGroupObjOrigen->getGroupCenterXYZ() 
    center=[total(c[0,*])/C_sROI3DGroupObjOrigen->count(),total(c[1,*])/C_sROI3DGroupObjOrigen->count(),total(c[2,*])/C_sROI3DGroupObjOrigen->count()]
    
    verts_Last = fltarr(3,4)
    verts_Last[0:2,0]=[(center[0] - max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]
    verts_Last[0:2,1]=[(center[0] - max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts_Last[0:2,2]=[(center[0] + max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts_Last[0:2,3]=[(center[0] + max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]

     ; Apply normalized values .... :(
     ; Las transformaciones estan realizadas sobre los valores normalizados.... asi que primero se deben convertir las referencias de datos a adatos normalizados
     ; para poder aplicar estas trasnformaciones
     actual3DGroupObject->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv     
     verts_Actual[0:2,0] = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Actual[0:2,0]  * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Actual[0:2,1] = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Actual[0:2,1]  * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Actual[0:2,2] = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Actual[0:2,2]  * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Actual[0:2,3] = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Actual[0:2,3]  * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]          
     
     C_sROI3DGroupObjOrigen->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
     verts_Last[0:2,0]   = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Last[0:2,0]    * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]] 
     verts_Last[0:2,1]   = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Last[0:2,1]    * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Last[0:2,2]   = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Last[0:2,2]    * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Last[0:2,3]   = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Last[0:2,3]    * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]          

     ;pp = plane points
      pp[0:2,*]=verts_Last
      pp[3,*]=1
        
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_plane_tLast)
      vertsPlaneB_Origen = new_verts[0:2,*]
      
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_orthogonalplane_tLast)
      vertsPlaneO_Origen = new_verts[0:2,*]

      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_complementaryplane_tLast)
      vertsPlaneC_Origen = new_verts[0:2,*]

     ;pp = plane points
      pp[0:2,*]=verts_Actual
      pp[3,*]=1
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_plane_tActual)
      vertsPlaneB_Actual = new_verts[0:2,*]
      
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_orthogonalplane_tActual)
      vertsPlaneO_Actual = new_verts[0:2,*]

      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_complementaryplane_tActual)
      vertsPlaneC_Actual = new_verts[0:2,*]
      
     ; Una vez que las transformaciones extraidas de los modelos han  sido aplicadas es necesario DEnormalizar los valores..
     ; asi las transformaciones obtenidas
     ; Fix Z relation troubles...
     actual3DGroupObject->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv     
    for i=0,3 do begin
      vertsPlaneB_Actual[*,i] = (vertsPlaneB_Actual[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
      vertsPlaneO_Actual[*,i] = (vertsPlaneO_Actual[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
      vertsPlaneC_Actual[*,i] = (vertsPlaneC_Actual[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
    endfor

     C_sROI3DGroupObjOrigen->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

    for i=0,3 do begin
      vertsPlaneB_Origen[*,i] = (vertsPlaneB_Origen[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
      vertsPlaneO_Origen[*,i] = (vertsPlaneO_Origen[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
      vertsPlaneC_Origen[*,i] = (vertsPlaneC_Origen[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
    endfor
    
     N1Actual   = (COMPUTE_MESH_NORMALS(vertsPlaneB_Actual,conn))[*,0]
     N1Origen   = (COMPUTE_MESH_NORMALS(vertsPlaneB_Origen,conn))[*,0]
     N2Actual   = (COMPUTE_MESH_NORMALS(vertsPlaneO_Actual,conn))[*,0]
     N2Origen   = (COMPUTE_MESH_NORMALS(vertsPlaneO_Origen,conn))[*,0]
     N3Actual   = (COMPUTE_MESH_NORMALS(vertsPlaneC_Actual,conn))[*,0]
     N3Origen   = (COMPUTE_MESH_NORMALS(vertsPlaneC_Origen,conn))[*,0]
     
     pRefOrigen = s_3PlanesIntersection( vertsPlaneB_Origen[*,0], N1Origen, vertsPlaneO_Origen[*,0], N2Origen, vertsPlaneC_Origen[*,0], N3Origen) ;[0,0,0]
     pRefActual = s_3PlanesIntersection( vertsPlaneB_Actual[*,0], N1Actual, vertsPlaneO_Actual[*,0], N2Actual, vertsPlaneC_Actual[*,0], N3Actual);[0,0,0]
     
     crossN1    = CROSSP(N1Origen,N1Actual)
       tempN = sqrt(total(crossN1*crossN1))
       if(tempN gt 0.0) then crossN1 = crossN1 / tempN
     crossN2    = CROSSP(N2Origen,N2Actual)
       tempN = sqrt(total(crossN2*crossN2))
       if(tempN gt 0.0) then crossN2 = crossN2 / tempN
     
     angleN1    = acos(Total(N1Actual*N1Origen))
     angleN2    = acos(Total(N2Actual*N2Origen))
     
     tempPos = fltarr(3)
     tempPosNormal = fltarr(3)
     
     if(1 eq 1) then begin
        ; Matriz Traslacion a referencia compartida
        tempMatrixTras[3,0:2] = (pRefOrigen - pRefActual)  
        ; Rotacion primer plano actual y original
            ; aplicar rotacion primer plano 
            if(1 eq 1 and sqrt(total(crossN1*crossN1)) gt 0.0) then begin
               tempMatrixRot1 = s_FreeAxisRotateMatrix( vRef = pRefOrigen, vDir = crossN1, vAngle = angleN1)
               
               ;; Evaluar el valor de la normal ortogonal del segundo objeto pero dentro de universo de representacion del primero
                tempPosNormal = pRefOrigen + N2Actual     
                tempPos       = s_FreeAxisRotate(vPoint = tempPosNormal, vRef = pRefOrigen,vDir = crossN1, vAngle =angleN1)
                
                tempPosNormal = tempPos - pRefOrigen
                   tempN = sqrt(total(tempPosNormal*tempPosNormal))
                   if(tempN gt 0.0) then tempPosNormal = tempPosNormal / tempN
                crossN2       = CROSSP(N2Origen,tempPosNormal)  
                   tempN = sqrt(total(crossN2*crossN2))
                   if(tempN gt 0.0) then crossN2 = crossN2 / tempN                  
                angleN2       = acos(Total(N2Actual*tempPosNormal))
            endif        
        ;rot
        ; Teniendo el punto de referencia magica solo queda la rotacion de los segundos planos
          ; aplicar rotacion segundos planos
             if(1 eq  1 and sqrt(total(crossN2*crossN2)) gt 0.0) then begin
               tempMatrixRot2 = s_FreeAxisRotateMatrix( vRef = pRefOrigen, vDir = crossN2, vAngle = angleN2)
             endif
     endif
     
     driftMatrix = driftMatrix # ((tempMatrixTras # tempMatrixRot1) # tempMatrixRot2) 
     ;driftMatrix = driftMatrix # ((tempMatrixRot1 # tempMatrixRot2) # tempMatrixTras)

     tempPoint = fltarr(4)
     tempPoint = [xyzCenter,1] # driftMatrix
     obj_destroy,C_sROI3DGroupObjOrigen          
     return, tempPoint[0:2]
end

function s_Driftmatrix, vDraworData = vDraworData, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = actual3DGroupObject, actualTime = actualTime, chPos = chPos, clusPos = clusPos, pRefActual = pRefActual, pRefOrigen = pRefOrigen

   driftMatrix    = MAKE_ARRAY(4,4,/Double, value = 0.0)
   tempMatrixRot1 = MAKE_ARRAY(4,4,/Double, value = 0.0)
   tempMatrixRot2 = MAKE_ARRAY(4,4,/Double, value = 0.0)
   tempMatrixTras = MAKE_ARRAY(4,4,/Double, value = 0.0)

   tempMatrixTras[0,0] = 1.0
   tempMatrixTras[1,1] = 1.0
   tempMatrixTras[2,2] = 1.0
   tempMatrixTras[3,3] = 1.0
         
   driftMatrix[0,0] = 1.0
   driftMatrix[1,1] = 1.0
   driftMatrix[2,2] = 1.0
   driftMatrix[3,3] = 1.0

   tempMatrixRot1[0,0] = 1.0
   tempMatrixRot1[1,1] = 1.0
   tempMatrixRot1[2,2] = 1.0
   tempMatrixRot1[3,3] = 1.0

   tempMatrixRot2[0,0] = 1.0
   tempMatrixRot2[1,1] = 1.0
   tempMatrixRot2[2,2] = 1.0
   tempMatrixRot2[3,3] = 1.0
         
   nObjects = actual3DGroupObject->count()

    ctm_mesh_tLast=make_array(4,4,/double, value = 0.0)
    ctm_plane_tLast=make_array(4,4,/double, value = 0.0)
    ctm_orthogonalplane_tLast=make_array(4,4,/double, value = 0.0)
    ctm_complementaryplane_tLast=make_array(4,4,/double, value = 0.0)

    ctm_mesh_tActual=make_array(4,4,/double, value = 0.0)
    ctm_plane_tActual=make_array(4,4,/double, value = 0.0)
    ctm_orthogonalplane_tActual=make_array(4,4,/double, value = 0.0)
    ctm_complementaryplane_tActual=make_array(4,4,/double, value = 0.0)
    
     ; First --- we need the saved matrix for actual time "i" .. and for time "0" (reference for system...)
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
     (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

     name = strCompress('GraphicState_Time_' + strCompress(0) + '.sav', /rem)
     filename = strCompress(path + strcompress(name, /rem))
     flag = FILE_TEST(filename)
      line = make_Array(4,/double)
      i=0
      if ~flag then return, driftMatrix
      if flag then begin
          GET_LUN, inunit
          openr, inunit, filename
          while ~ EOF(inunit) do begin
             READF, inunit, line 
             if( i le 3) then ctm_mesh_tLast[*,i]=line
             if((i gt 3) and (i le 7) ) then ctm_plane_tLast[*,i-4]=line
             if((i gt 7) and (i le 11) ) then ctm_orthogonalplane_tLast[*,i-8]=line
             if((i gt 11) ) then ctm_complementaryplane_tLast[*,i-12]=line
             i++ 
          endwhile    
          FREE_LUN, inunit
     endif
   
     name = strCompress('GraphicState_Time_' + strCompress(actualTime) + '.sav', /rem)
     filename = strCompress(path + strcompress(name, /rem))
     flag = FILE_TEST(filename)
      line = make_Array(4,/double)
      i=0
      if ~flag then return, driftMatrix
      if flag then begin
          GET_LUN, inunit
          openr, inunit, filename
          while ~ EOF(inunit) do begin
             READF, inunit, line 
             if( i le 3) then ctm_mesh_tActual[*,i]=line
             if((i gt 3) and (i le 7) ) then ctm_plane_tActual[*,i-4]=line
             if((i gt 7) and (i le 11) ) then ctm_orthogonalplane_tActual[*,i-8]=line
             if((i gt 11) ) then ctm_complementaryplane_tActual[*,i-12] = line
             i++ 
          endwhile    
          FREE_LUN, inunit
     endif

     if( (total(abs(ctm_plane_tLast)) eq 0.0) or $
         (total(abs(ctm_orthogonalplane_tLast)) eq 0.0) or $
         (total(abs(ctm_complementaryplane_tLast)) eq 0.0) or $
         
         (total(abs(ctm_plane_tActual)) eq 0.0) or $
         (total(abs(ctm_orthogonalplane_tActual)) eq 0.0) or $
         (total(abs(ctm_complementaryplane_tActual)) eq 0.0) ) then return, driftMatrix
; Estructuras requeridas

     pp = make_array(4,4,/DOUBLE)
     new_verts = make_array(4,4,/DOUBLE)
 
     vertsPlaneB_Origen = make_array(3,4,/DOUBLE)
     vertsPlaneO_Origen = make_array(3,4,/DOUBLE)
     vertsPlaneC_Origen = make_array(3,4,/DOUBLE)
     vertsPlaneB_Actual = make_array(3,4,/DOUBLE)
     vertsPlaneO_Actual = make_array(3,4,/DOUBLE)
     vertsPlaneC_Actual = make_array(3,4,/DOUBLE)
     
     conn=[4,3,2,1,0]

; Obtener posicion de los planos en base a las matrices de transformacion

    c = actual3DGroupObject->getGroupCenterXYZ()
    center=[total(c[0,*])/actual3DGroupObject->count(),total(c[1,*])/actual3DGroupObject->count(),total(c[2,*])/actual3DGroupObject->count()]
    verts_Actual = fltarr(3,4)
    verts_Actual[0:2,0]=[(center[0] - max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]
    verts_Actual[0:2,1]=[(center[0] - max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts_Actual[0:2,2]=[(center[0] + max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts_Actual[0:2,3]=[(center[0] + max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]

    C_sROI3DGroupObjOrigen = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = 0, chPos = chPos, clusPos = clusPos)  
    c = C_sROI3DGroupObjOrigen->getGroupCenterXYZ() 
    center=[total(c[0,*])/C_sROI3DGroupObjOrigen->count(),total(c[1,*])/C_sROI3DGroupObjOrigen->count(),total(c[2,*])/C_sROI3DGroupObjOrigen->count()]
    
    verts_Last = fltarr(3,4)
    verts_Last[0:2,0]=[(center[0] - max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]
    verts_Last[0:2,1]=[(center[0] - max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts_Last[0:2,2]=[(center[0] + max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts_Last[0:2,3]=[(center[0] + max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]

     ; Apply normalized values .... :(
     ; Las transformaciones estan realizadas sobre los valores normalizados.... asi que primero se deben convertir las referencias de datos a adatos normalizados
     ; para poder aplicar estas trasnformaciones
     actual3DGroupObject->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv     
     verts_Actual[0:2,0] = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Actual[0:2,0]  * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Actual[0:2,1] = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Actual[0:2,1]  * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Actual[0:2,2] = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Actual[0:2,2]  * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Actual[0:2,3] = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Actual[0:2,3]  * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]          
     
     C_sROI3DGroupObjOrigen->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
     verts_Last[0:2,0]   = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Last[0:2,0]    * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]] 
     verts_Last[0:2,1]   = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Last[0:2,1]    * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Last[0:2,2]   = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Last[0:2,2]    * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
     verts_Last[0:2,3]   = [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]] + verts_Last[0:2,3]    * [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]          

     ;pp = plane points
      pp[0:2,*]=verts_Last
      pp[3,*]=1
        
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_plane_tLast)
      vertsPlaneB_Origen = new_verts[0:2,*]
      
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_orthogonalplane_tLast)
      vertsPlaneO_Origen = new_verts[0:2,*]

      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_complementaryplane_tLast)
      vertsPlaneC_Origen = new_verts[0:2,*]

     ;pp = plane points
      pp[0:2,*]=verts_Actual
      pp[3,*]=1
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_plane_tActual)
      vertsPlaneB_Actual = new_verts[0:2,*]
      
      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_orthogonalplane_tActual)
      vertsPlaneO_Actual = new_verts[0:2,*]

      for i=0,3 do new_verts[*,i] = (pp[*,i]#ctm_complementaryplane_tActual)
      vertsPlaneC_Actual = new_verts[0:2,*]
    
    if(vDraworData eq 1b) then begin
     ; Una vez que las transformaciones extraidas de los modelos han  sido aplicadas es necesario DEnormalizar los valores..
     ; asi las transformaciones obtenidas estan en el dominio de los datos.. para extraer la matriz de transformacion para datos y no graficos
     ; Fix Z relation troubles...

         actual3DGroupObject->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv     
        for i=0,3 do begin
          vertsPlaneB_Actual[*,i] = (vertsPlaneB_Actual[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
          vertsPlaneO_Actual[*,i] = (vertsPlaneO_Actual[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
          vertsPlaneC_Actual[*,i] = (vertsPlaneC_Actual[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
        endfor
    
         C_sROI3DGroupObjOrigen->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    
        for i=0,3 do begin
          vertsPlaneB_Origen[*,i] = (vertsPlaneB_Origen[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
          vertsPlaneO_Origen[*,i] = (vertsPlaneO_Origen[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
          vertsPlaneC_Origen[*,i] = (vertsPlaneC_Origen[*,i] - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]]) /  [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]
        endfor

    
    endif
    
    
     N1Actual   = (COMPUTE_MESH_NORMALS(vertsPlaneB_Actual,conn))[*,0]
     N1Origen   = (COMPUTE_MESH_NORMALS(vertsPlaneB_Origen,conn))[*,0]
     N2Actual   = (COMPUTE_MESH_NORMALS(vertsPlaneO_Actual,conn))[*,0]
     N2Origen   = (COMPUTE_MESH_NORMALS(vertsPlaneO_Origen,conn))[*,0]
     N3Actual   = (COMPUTE_MESH_NORMALS(vertsPlaneC_Actual,conn))[*,0]
     N3Origen   = (COMPUTE_MESH_NORMALS(vertsPlaneC_Origen,conn))[*,0]

     pRefOrigen = s_3PlanesIntersection( vertsPlaneB_Origen[*,0], N1Origen, vertsPlaneO_Origen[*,0], N2Origen, vertsPlaneC_Origen[*,0], N3Origen) ;[0,0,0]
     pRefActual = s_3PlanesIntersection( vertsPlaneB_Actual[*,0], N1Actual, vertsPlaneO_Actual[*,0], N2Actual, vertsPlaneC_Actual[*,0], N3Actual);[0,0,0]
     
     crossN1    = CROSSP(N1Origen,N1Actual)
       tempN = sqrt(total(crossN1*crossN1))
       if(tempN gt 0.0) then crossN1 = crossN1 / tempN
     crossN2    = CROSSP(N2Origen,N2Actual)
       tempN = sqrt(total(crossN2*crossN2))
       if(tempN gt 0.0) then crossN2 = crossN2 / tempN
     
     angleN1    = acos(Total(N1Actual*N1Origen))
     angleN2    = acos(Total(N2Actual*N2Origen))
     
     tempPos = fltarr(3)
     tempPosNormal = fltarr(3)
     
     if(1 eq 1) then begin
        ; Matriz Traslacion a referencia compartida
        tempMatrixTras[3,0:2] = (pRefOrigen - pRefActual)  
        ; Rotacion primer plano actual y original
            ; aplicar rotacion primer plano 
            if(1 eq 1 and sqrt(total(crossN1*crossN1)) gt 0.0) then begin
               tempMatrixRot1 = s_FreeAxisRotateMatrix( vRef = pRefOrigen, vDir = crossN1, vAngle = angleN1)
               
               ;; Evaluar el valor de la normal ortogonal del segundo objeto pero dentro de universo de representacion del primero
                tempPosNormal = pRefOrigen + N2Actual     
                tempPos       = s_FreeAxisRotate(vPoint = tempPosNormal, vRef = pRefOrigen,vDir = crossN1, vAngle =angleN1)
                
                tempPosNormal = tempPos - pRefOrigen
                   tempN = sqrt(total(tempPosNormal*tempPosNormal))
                   if(tempN gt 0.0) then tempPosNormal = tempPosNormal / tempN
                crossN2       = CROSSP(N2Origen,tempPosNormal)  
                   tempN = sqrt(total(crossN2*crossN2))
                   if(tempN gt 0.0) then crossN2 = crossN2 / tempN                  
                angleN2       = acos(Total(N2Actual*tempPosNormal))
            endif        
        ;rot
        ; Teniendo el punto de referencia magica solo queda la rotacion de los segundos planos
          ; aplicar rotacion segundos planos
             if(1 eq  1 and sqrt(total(crossN2*crossN2)) gt 0.0) then begin
               tempMatrixRot2 = s_FreeAxisRotateMatrix( vRef = pRefOrigen, vDir = crossN2, vAngle = angleN2)
             endif
     endif
     
     driftMatrix = driftMatrix # ((tempMatrixTras # tempMatrixRot1) # tempMatrixRot2) 
     ;driftMatrix = driftMatrix # ((tempMatrixRot1 # tempMatrixRot2) # tempMatrixTras)
     if(vDraworData eq 1b) then begin
         scaleMatrix = MAKE_ARRAY(4,4,/Double, value = 0.0)
         scaleMatrix[0,0] = xCoord_conv[1]
         scaleMatrix[1,1] = yCoord_conv[1]
         scaleMatrix[2,2] = zCoord_conv[1]
         scaleMatrix[3,3] = 1.0         
         driftMatrix = driftMatrix # scaleMatrix
     end

     ; DEnormalized for render Data in post process
     actual3DGroupObject->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv     
     pRefActual = (pRefActual - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]])/ [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]] 


     C_sROI3DGroupObjOrigen->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
     pRefOrigen = (pRefOrigen - [xCoord_conv[0],yCoord_conv[0],zCoord_conv[0]])/ [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]] 

     obj_destroy,C_sROI3DGroupObjOrigen          
     return, driftMatrix
end

function s_DataDomainUnDriftmatrix, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = actual3DGroupObject, actualTime = actualTime, chPos = chPos, clusPos = clusPos

   unDriftMatrix    = MAKE_ARRAY(4,4,/Double, value = 0.0)
   tempMatrixRot1 = MAKE_ARRAY(4,4,/Double, value = 0.0)

   tempMatrixRot1[0,0] = 1.0
   tempMatrixRot1[1,1] = 1.0
   tempMatrixRot1[2,2] = 1.0
   tempMatrixRot1[3,3] = 1.0
         
   nObjects = actual3DGroupObject->count()

     ; First --- we need the saved matrix for actual time "i" .. and for time "0" (reference for system...)
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
     (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

     name = strCompress('UnDriftDataMatrix_Time_' + strCompress(actualTime) + '.sav', /rem)
     filename = strCompress(path + strcompress(name, /rem))
     flag = FILE_TEST(filename)
      line = make_Array(4,/double)
      i=0
      if ~flag then return, tempMatrixRot1
      if flag then begin
          GET_LUN, inunit
          openr, inunit, filename
          while ~ EOF(inunit) do begin
             READF, inunit, line 
             if( i le 3) then unDriftMatrix[*,i]=line
             i++ 
          endwhile    
          FREE_LUN, inunit
     endif
     if( total(abs(unDriftMatrix) ) eq 0.0 ) then begin
        return, tempMatrixRot1
     endif
     
     return, unDriftMatrix     
end
