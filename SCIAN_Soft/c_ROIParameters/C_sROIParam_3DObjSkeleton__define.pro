;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjTracking
;
; PURPOSE:
;       - Calculation of Parameter Skeleton
;
; AUTHOR:
;     Luis Briones (2012)
;     e_mail:
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjSkeleton')
;
; METHOHDS:
;_____________________________IOISIOI____________________

Pro C_sROIParam_3DObjSkeleton::apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position

   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

       ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1:if (position[0] eq -1) then return else  whParamActive[position] = 1
      else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase

      ; check Pointers
   wherePA = where(whParamActive eq 1)
   if ~ptr_valid((*self.pParamStruct).pROINumberVect) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do $
     if ~ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

    ;obtengo el Skeleton
    oObjectModel = obj_new('IDLgrModel', uValue = '3D Skeleton From Mesh File - Test', name = '3D Skeleton From Mesh File - Test')
    a=self->s_getoSkeletonParam(oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = C_sROI3DGroupObj)
    vertex=*a[0]
    edge=*a[1]
    xyzSizePerPixel = C_sROI3DGroupObj->getxyzSizePerPixel()
    
    ;;;;;;;;COMIENZO GUARDAR NODOS;;;;;;;;;;;  
    flag1=0
    flag2=0
    nhijos=0
    volumen=0.
    
    edgeC=make_array(2,round(size(edge,/DIM)/2)-round(size(edge,/DIM)/2)/3,/int)
    dis=make_array(round(size(edge,/DIM)/2)-round(size(edge,/DIM)/2)/3)
    j=1
    ;corrige formato de las aristas
    for i=0,fix((round(size(edge,/DIM)/2)-round(size(edge,/DIM)/2)/3)[0])-1 do begin
        edgeC[0,i]=edge[j]
        edgeC[1,i]=edge[j+1]
        j+=3
    end
  
    dim_edge=size(edgeC,/DIM)
    dim_vertex=size(vertex,/DIM)
    dis=make_array(dim_edge[1])
    hijohermano=0
    
    if (dim_edge[1] lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin
    
    ; set Object Number Vector
    (*self.pParamStruct).pROINumberVect = ptr_new(indgen(dim_edge[1])+1, /no_copy)
    
    ; set Object Parameter Vectors
    for i = 0, n_elements(whParam)-1 do *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(dim_edge[1], /float, value = -1.)
      
    ;calculo distancias entre los distintas aristas
    for i=0, dim_edge[1]-1 do begin
        dis[i]=sqrt((vertex[0,edgeC[0,i]]-vertex[0,edgeC[1,i]])^2 + (vertex[1,edgeC[0,i]]-vertex[1,edgeC[1,i]])^2 + (vertex[2,edgeC[0,i]]-vertex[2,edgeC[1,i]])^2)
    end
    distmaxNodo=max(dis,index)
    mainNodo=edgeC[0,index]
    
    ;cuenta numero de hijos
    if n_elements(where(edgeC[0,*] eq mainNodo)) eq 1 then begin
        if where(edgeC[0,*] eq mainNodo) eq -1 then flag1=-1 else iNodo1=edgeC[*,where(edgeC[0,*] eq mainNodo)]
    endif else begin
        iNodo1=edgeC[*,where(edgeC[0,*] eq mainNodo)]
    endelse    
    if n_elements(where(edgeC[1,*] eq mainNodo)) eq 1 then begin
        if where(edgeC[1,*] eq mainNodo) eq -1 then flag2=-1 else iNodos2=edgeC[*,where(edgeC[1,*] eq mainNodo)] 
    endif else begin
        iNodos2=edgeC[*,where(edgeC[1,*] eq mainNodo)]
    endelse
    if flag1 eq 0 then nhijos+=n_elements(where(edgeC[0,*] eq mainNodo))
    if flag2 eq 0 then nhijos+=n_elements(where(edgeC[1,*] eq mainNodo))
    
    
     MyData=ptrarr(7)
     MyData[0] = ptr_new(mainNodo)              ; Valor
     MyData[1] = ptr_new(0)                    ; Distancia Padre - hijo
     MyData[2] = ptr_new(-1)                    ; Padre
     MyData[3] = ptr_new(vertex[*,edgeC[0,index]])  ; Vertices nodo (hijo)
     MyData[4] = ptr_new(-1)                    ; Vertices nodo (padre)
     MyData[5] = ptr_new(nhijos)                ; Numero Hijos
     MyData[6] = ptr_new(hijohermano)           ; Hijo o Hermano
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Tree_Insert, DataTree, Padre,Hijo o Hermano, ValorNodo, MyData
  allData=ptrarr(dim_edge[1])
  allData[0] = ptr_new(MyData)
  noEnco=ptrarr(1)
  
  SkeletonStructTree_new, DataTree
  ;inserto el nodo RAIZ
  SkeletonStructTree_Insert, DataTree, allData[0]
  ;;GUARDO PARAMETROS
   ; '3D Distance Net [Voxel]'
   if whParamActive[1] then  (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[0] = 0
   ; 3D Distance Net [x³]
   if whParamActive[2] then  (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[0] = 0
       
  cache=make_array(dim_edge[1],dim_edge[1], value=-1)
  index_total=make_array(dim_edge[1],dim_edge[1], value=-1)
  nhijos=make_Array(dim_edge[1],/integer)
  total_nodos=make_array(dim_edge[1])
  total_nodos_padres=make_array(dim_edge[1])
  total_nodos[0]=mainNodo

  fin=0
  cont=1
  ii=0
  k=0
  kk=0
  for i=0, dim_edge[1]-1 do begin
 
    repeat begin 
     f1=0
     f2=0
     ;indices de los hijos de mainnodo
     in1=where(edgeC[0,*] eq mainNodo)
     in2=where(edgeC[1,*] eq mainNodo) 
     x=where(in1 eq index,complement=resto)
     
     ;ordeno los indices para agregarlos correctamente
     if x ne -1 then begin
        if n_elements(in1) gt 1 then begin
          if in2[0] ne -1 then inaux=[in1[x],in1[resto],[in2]] else  inaux=[in1[x],in1[resto]]
        endif else begin
          if in2[0] ne -1 then inaux=[in1[x],[in2]] else inaux=[in1[x]]
        endelse
        f1=1
     endif else begin
        x=where(in2 eq index,complement=resto)
        if n_elements(in2) gt 1 then begin
          if in1[0] ne -1 then inaux=[in2[x],in2[resto],[in1]] else inaux=[in2[x],in2[resto]] 
         endif else begin
          if in1[0] ne -1 then inaux=[in2[x],[in1]] else inaux=[in2[x]]
         endelse
     endelse
     ;largo del for, N° elementos a agregar
     if in2[0] ne -1 and in1[0] ne -1 then begin
        dim=n_elements(in1)+n_elements(in2)-1
     endif else begin
        if in1[0] eq -1 then dim=n_elements(in2)-1 else dim=n_elements(in1)-1
     endelse
     jj=0
     for j=0, dim do begin
      if f1 eq 1 then begin 
       if j lt n_elements(in1) then begin  
         nodo=edgeC[1,inaux[j]] 
          
         MyData[0] = ptr_new(nodo)            ; Valor
         MyData[2] = ptr_new(edgeC[0,inaux[j]])            ; Padre
         MyData[3] = ptr_new(vertex[*,edgeC[1,inaux[j]]])  ; Vertices nodo (Hijo)
         MyData[4] = ptr_new(vertex[*,edgeC[0,inaux[j]]])  ; Vertices nodo (Padre)
         MyData[5] = ptr_new(nhijos[i])             ; Numero Hijos
         MyData[6] = ptr_new(hijohermano)           ; Hijo o Hermano
         MyData[1] = ptr_new(sqrt(((*MyData[4])[0]-(*MyData[3])[0])^2 + ((*MyData[4])[1]-(*MyData[3])[1])^2 + ((*MyData[4])[2]-(*MyData[3])[2])^2)) ; Distancia Padre - Hijo
       endif else begin
         nodo=edgeC[0,inaux[j]] 
          
         MyData[0] = ptr_new(nodo)            ; Valor
         MyData[2] = ptr_new(edgeC[1,inaux[j]])            ; Padre
         MyData[3] = ptr_new(vertex[*,edgeC[0,inaux[j]]])  ; Vertices nodo (Hijo)
         MyData[4] = ptr_new(vertex[*,edgeC[1,inaux[j]]])  ; Vertices nodo (Padre)
         MyData[5] = ptr_new(nhijos[i])             ; Numero Hijos
         MyData[6] = ptr_new(hijohermano)           ; Hijo o Hermano
         MyData[1] = ptr_new(sqrt(((*MyData[4])[0]-(*MyData[3])[0])^2 + ((*MyData[4])[1]-(*MyData[3])[1])^2 + ((*MyData[4])[2]-(*MyData[3])[2])^2)) ; Distancia Padre - Hijo
       endelse
      endif else begin
         if j lt n_elements(in2) then begin  
           nodo=edgeC[0,inaux[j]] 
            
           MyData[0] = ptr_new(nodo)            ; Valor
           MyData[2] = ptr_new(edgeC[1,inaux[j]])            ; Padre
           MyData[3] = ptr_new(vertex[*,edgeC[0,inaux[j]]])  ; Vertices nodo (Hijo)
           MyData[4] = ptr_new(vertex[*,edgeC[1,inaux[j]]])  ; Vertices nodo (Padre)
           MyData[5] = ptr_new(nhijos[i])             ; Numero Hijos
           MyData[6] = ptr_new(hijohermano)           ; Hijo o Hermano
           MyData[1] = ptr_new(sqrt(((*MyData[4])[0]-(*MyData[3])[0])^2 + ((*MyData[4])[1]-(*MyData[3])[1])^2 + ((*MyData[4])[2]-(*MyData[3])[2])^2)) ; Distancia Padre - Hijo
         endif else begin
           nodo=edgeC[1,inaux[j]] 
            
           MyData[0] = ptr_new(nodo)            ; Valor
           MyData[2] = ptr_new(edgeC[0,inaux[j]])            ; Padre
           MyData[3] = ptr_new(vertex[*,edgeC[1,inaux[j]]])  ; Vertices nodo (Hijo)
           MyData[4] = ptr_new(vertex[*,edgeC[0,inaux[j]]])  ; Vertices nodo (Padre)
           MyData[5] = ptr_new(nhijos[i])             ; Numero Hijos
           MyData[6] = ptr_new(hijohermano)           ; Hijo o Hermano
           MyData[1] = ptr_new(sqrt(((*MyData[4])[0]-(*MyData[3])[0])^2 + ((*MyData[4])[1]-(*MyData[3])[1])^2 + ((*MyData[4])[2]-(*MyData[3])[2])^2)) ; Distancia Padre - Hijo
       endelse
      endelse
        allData[i] = ptr_new(MyData)
        newnodo=self->existe(*mydata[0],total_nodos)
        if newnodo eq -1 then begin 
        ;;GUARDO PARAMETROS
         ; '3D Distance [Voxel]'
         if whParamActive[1] then (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[cont] = *MyData[1]
         ; 3D Distance [x²]
         if whParamActive[2] then  (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[cont] = *MyData[1] * (xyzSizePerPixel[0]*xyzSizePerPixel[1]*xyzSizePerPixel[2])
                  
          SkeletonStructTree_Insert, DataTree, allData[i] 
          total_nodos[cont]=*(*allData[i])[0]
          total_nodos_padres[cont]=*(*allData[i])[2]
          cache[jj,kk]=nodo
          index_total[jj,kk]=inaux[j]
          cont++
        endif else begin
          jj=j-1
        endelse    
        jj++
     endfor
       
      mainNodo=cache[k,ii]
      index=index_total[k,ii]
      k++
      if newnodo eq -1 then kk++
    endrep until cache[k-1,i] eq -1
      ii++
      k=0
      if cache[0,kk-1] eq -1 then kk--
      mainNodo=cache[k,ii]
      index=index_total[k,ii]
      k++
      
      fin+=n_elements(in1)+n_elements(in2)-1
      if fin eq dim_edge[1]-1 or mainNodo eq -1 then break,1
  endfor
      
      ;Cuenta bifurcaciones 
      fork=self->contFork(total_nodos_padres)
      
      ; 3D N° Total Node
      if whParamActive[0] then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[*] = cont   
      ; 3D Numero de bifurcaciones
      if whParamActive[3] then  (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[*] = fork
      ; 3D Nodes Selected
      if whParamActive[4] then  (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[*] = total_nodos
              
   endelse
   
   ptr_free,MyData
   ptr_free,allData
End


Function C_sROIParam_3DObjSkeleton::existe, data, total_nodos
  dim=size(total_nodos, /dim)
  res=data eq total_nodos
  for i=0, dim[0]-1 do begin
    if res[i] eq 1 then return, 1
  endfor
  return, -1
end


Function C_sROIParam_3DObjSkeleton::contFork, total_nodos_padres 
    fork=make_array(n_elements(total_nodos_padres))
    i=0
    
    while i lt n_elements(total_nodos_padres) do begin
      if n_elements(where(total_nodos_padres[i] eq total_nodos_padres)) gt 1 and total_nodos_padres[i] ne 0 then begin
          fork[i:i+n_elements(where(total_nodos_padres[i] eq total_nodos_padres))-1] = fork[i-1] + n_elements(where(total_nodos_padres[i] eq total_nodos_padres))
          i+=n_elements(where(total_nodos_padres[i] eq total_nodos_padres))
      endif else begin
          if i eq 0 then fork[i] = 0 else fork[i] = fork[i-1]
          i++
      endelse      
    endwhile
    
    return, fork
End


Function C_sROIParam_3DObjSkeleton::s_getoSkeletonParam, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference
   
   lastSkelStep = 2 ; step 1: contract, step2: collapse, step3: refinement
   i=0
   
   ; Call to Java object
   tsn = systime(1)
   oIDLClassName = "IDLJAVAOBJECT$PROJECT_SKELETONITATIONFROMMESHCONTRACTION"
   oJavaClassName = "project.SkeletonitationFromMeshContraction"
   oMeshContraction = obj_new(oIDLClassName, oJavaClassName)
      
   if (obj_class(oMeshContraction) ne oIDLClassName) then begin
      prin0t, '(ERR) creating ', oJavaClassName, '. oMeshContraction =', oMeshContraction
   endif else begin
         
   tn = systime(1) - tsn
   
   ;lectura obj reticulo_supder_fixed.obj
   oIDLClassNametest = "IDLJAVAOBJECT$PROJECT_READOBJFILE"
   oJavaClassNametest = "project.files.ReadObjFile"
   oMeshContractiontest = obj_new(oIDLClassNametest, oJavaClassNametest, "C:\RSI\skeletontest\fertilty_cgal")
   oMeshContractiontest->setPath, "C:\RSI\skeletontest\fertilty_cgal"
   oMeshContractiontest->read
   
   verticesx = oMeshContractiontest->getX() ;X
   verticesy = oMeshContractiontest->getY() ;y
   verticesz = oMeshContractiontest->getZ() ;Z
   polygon = oMeshContractiontest->getTriangles() ;triangles index
   
   if(lastSkelStep ge 1) then begin
      ts2 = systime(1)
      oMeshContraction->geometryContraction, verticesx,verticesy, verticesz, polygon
      t2 = systime(1) - ts2
   endif                                               
   if(lastSkelStep ge 2) then begin
      ts3 = systime(1)         
      oMeshContraction->connectivitySurgery
      t3 = systime(1) - ts3
   endif
   if(lastSkelStep ge 3) then begin         
      ts4 = systime(1)         
      oMeshContraction->embeddingRefinement
      t4 = systime(1) - ts4 
   endif
      
   if(lastSkelStep eq 1) then begin
      verticesFinales = oMeshContraction->getMeshVerticesCoordsIDL()
      poligonosFinales = oMeshContraction->getMeshTrianglesForIDL()
   endif
   if(lastSkelStep eq 2 || lastSkelStep eq 3) then begin
      verticesFinales = oMeshContraction->getVerticesCoordsIDL()
      aristasFinales = oMeshContraction->getEdgesForIDL()
   endif
   
    if (lastSkelStep ge 2) then begin
        (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = i))->getNumber()
        xyzPixel = oGroupReference->getxyzSizePerPixel()
        zxFactor = xyzPixel[2]/xyzPixel[0]
    
        linesnr=SIZE(aristasFinales)
        ;for each segment of the skeleton
        for j=0, linesnr[1]-1, 3 do begin 
          x1=verticesFinales[0,aristasFinales[j+1]]
          y1=verticesFinales[1,aristasFinales[j+1]]
          z1=verticesFinales[2,aristasFinales[j+1]]/zxFactor
          
          x2=verticesFinales[0,aristasFinales[j+2]]    
          y2=verticesFinales[1,aristasFinales[j+2]]
          z2=verticesFinales[2,aristasFinales[j+2]]/zxFactor
        endfor
     endif
    endelse ;end valid java object
    a=ptrarr(2)
    a[0]=ptr_new(verticesFinales)
    a[1]=ptr_new(aristasFinales)
    obj_destroy, oMeshContraction
    obj_destroy, oMeshContractiontest
    return,a
End


Function C_sROIParam_3DObjSkeleton::init

  ROIParamStruct = {name:'3D Skeleton',$     ;  ROI Name.
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

  ROIParamNames = ['3D Skeleton Number Nodes',$
                   '3D Skeleton Distance [Voxel]',$
                   '3D Skeleton Distance [x²]',$
                   '3D Skeleton Number Fork',$
                   '3D Skeleton Nodes Selected']

  nParams = n_elements(ROIParamNames)
  self.pValueStruct = ptr_new(ptrArr(nParams))
  ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')

  ROIParamActive = make_array(nParams, /byte, value = 1b)
  ROIParamMin = make_array(nParams, /float, value = 0.)
  ROIParamMax = make_array(nParams, /float, value = 1.)
  ROIParamValues = make_array(nParams, /float, value = 0.)
  pROINumberVect = [-1]

  ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
  ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
  ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
  ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
  ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
  ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
  ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
  self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

  ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[0],$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$
                    pNames:ptr_new(),$
                    pActive:ptr_new(),$
                    pMin:ptr_new(),$
                    pMax:ptr_new(),$
                    pValues:ptr_new(),$
                    pROIParamVect:ptr_new()}

  ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
  ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                   'Threshold_2a', 'Threshold_2b',$
                   'Threshold_3a', 'Threshold_3b',$
                   'Threshold_4a', 'Threshold_4b']
  ROIValueActive = [0,0,$
                    0,0,$
                    0,0,$
                    0,0]
  ROIValueMin = [0.,0.,$
                 0.,0.,$
                 0.,0.,$
                 0.,0.]
  ROIValueMax = [1.,1.,$
                 1.,1.,$
                 1.,1.,$
                 1.,1.]
  ROIValueValues =[0.,1.,$
                   0.,1.,$
                   0.,1.,$
                   0.,1.]

  pROIParamVect = [-1]
  ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
  ROIValueStruct.pNames = ptr_new(ROIValueNames)
  ROIValueStruct.pActive = ptr_new(ROIValueActive)
  ROIValueStruct.pMin = ptr_new(ROIValueMin)
  ROIValueStruct.pMax = ptr_new(ROIValueMax)
  ROIValueStruct.pValues = ptr_new(ROIValueValues)
  ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
  (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

  for i = 1, nParams-2 do begin
     ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[i],$
                     type:'3D ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$
                     pNames:ptr_new(),$
                     pActive:ptr_new(),$
                     pMin:ptr_new(),$
                     pMax:ptr_new(),$
                     pValues:ptr_new(),$
                     pROIParamVect:ptr_new()}

     ROIValueWidgetType = [ROIValueWidgetType]
     ROIValueNames = [ROIValueNames]
     ROIValueActive = [ROIValueActive]
     ROIValueMin = [ROIValueMin]
     ROIValueMax = [ROIValueMax]
     ROIValueValues =[ROIValueValues]

     pROIParamVect = [-1]
     ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
     ROIValueStruct.pNames = ptr_new(ROIValueNames)
     ROIValueStruct.pActive = ptr_new(ROIValueActive)
     ROIValueStruct.pMin = ptr_new(ROIValueMin)
     ROIValueStruct.pMax = ptr_new(ROIValueMax)
     ROIValueStruct.pValues = ptr_new(ROIValueValues)
     ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

     (*self.pValueStruct)[i] = ptr_new(ROIValueStruct, /no_copy)
  endfor

  ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[nParams-1],$
                  type:'3D ROI-Parameter-Method',$
                  pWidgetType:ptr_new(),$
                  pNames:ptr_new(),$
                  pActive:ptr_new(),$
                  pMin:ptr_new(),$
                  pMax:ptr_new(),$
                  pValues:ptr_new(),$
                  pROIParamVect:ptr_new()}

  ROIValueWidgetType = [ROIValueWidgetType]
  ROIValueNames = [ROIValueNames]
  ROIValueActive = [ROIValueActive]
  ROIValueMin = [ROIValueMin]
  ROIValueMax = [ROIValueMax]
  ROIValueValues =[ROIValueValues]

  pROIParamVect = [-1]
  ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
  ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
  ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
  ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
  ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
  ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
  ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

  (*self.pValueStruct)[nParams-1] = ptr_new(ROIValueStruct, /no_copy)
  return, 1
End


Pro C_sROIParam_3DObjSkeleton__define
   tmp = {C_sROIParam_3DObjSkeleton, pParamStruct:ptr_new(), pValueStruct:ptr_new(), inherits C_sROIParam}
End
