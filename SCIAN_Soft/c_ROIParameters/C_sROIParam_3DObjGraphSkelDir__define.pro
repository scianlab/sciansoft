;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjGraphSkelDir
;
; PURPOSE:
;       - Calculation of Parameter Skeleton
;
; AUTHORS:
;     Felipe Santibáñez Leal (2012)
;     e_mail: fsantibanez@med.uchile.cl
;     Mauricio Cerda (2012)
;     e_mail: mauriciocerda@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjGraphSkelDir' )
;
; METHODS:
;_____________________________IOISIOI____________________


Pro C_sROIParam_3DObjGraphSkelDir::apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position
   ;xyzSizePerPixel = C_sROI3DGroupObj->getxyzSizePerPixel() ; es equivalente
   pParamStruct    = C_sROI3DGroupObj->getpParamStruct()
   xSizePerPixel   = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
   ySizePerPixel   = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
   zSizePerPixel   = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Size [pixel]'))[0]]
   xyzSizePerPixel = [xSizePerPixel,ySizePerPixel,zSizePerPixel]

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos

  widget_control, stateObj_tlb, get_uValue = state, /no_copy
    currROI3DGroupFileName = state.currROI3DGroupFileName
  widget_control, stateObj_tlb, set_uValue = state, /no_copy

  slash = path_sep()
  posLastSlash = strPos(currROI3DGroupFileName, slash, /reverse_search)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, strPos(currROI3DGroupFileName, slash, /reverse_search)+1)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, 0, strPos(currROI3DGroupFileName, '.sav'))

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  modelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]])
  

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
   if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

   ; Load Skeleton, Reduced Skeleton and Original Mesh
   ; mcerda
   fAllowLoops=0b
   
   fFile = [0]
   nObj = C_sROI3DGroupObj->count()
      
   ; from the original skeleton
   C_sROI3DGroupObj->restoreSavedSkeletonModels, stack_tlb = stack_tlb, modelName = '_Skeleton_', pVertices = pVertices, pEdges = pEdges, $
     fFile = fFile, silent = 1, loadFromObjFile=0, pSkeletonMeshAssociation=pSkeletonMeshAssociation
   self->restoreMasterGraphCreation, pVertices, pEdges, xyzSizePerPixel, skelGraphMaster=skelGraphMasterSkeleton, $
    verticesFinales=verticesFinales, vAristas=vAristas, fAllowLoops=fAllowLoops, vEdgeRoot=-1
   aristasFull    = skelGraphMasterSkeleton->CreateFullSkelEdges()
   
   ; from the reduced skeleton
   C_sROI3DGroupObj->restoreSavedSkeletonModels, stack_tlb = stack_tlb, modelName = '_SkeletonReduced_', pVertices = pVerticesReduced, $
     pEdges = pEdgesReduced, fFile = fFile, silent = 1, loadFromObjFile=0
   self->restoreMasterGraphCreation, pVerticesReduced, pEdgesReduced, xyzSizePerPixel, skelGraphMaster=skelGraphMasterReducedSkeleton, $
    fAllowLoops=fAllowLoops, vEdgeRoot=-1
    
   pGraph = skelGraphMasterReducedSkeleton->GetmasterGraphSkel()
   dim_edge = N_ELEMENTS(*pGraph)
   
   modelPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]
   
   openW, unit, strcompress(modelPath+'SkeletonParams.txt'), /get_lun, WIDTH=dim_edge*1000
      
   ; from original mesh data
   s_getSavedModels, stack_tlb, C_sROI3DGroupObj, '_MeshFix_', pVertices = pVerticesMesh, pPolygons = pPolygonsMesh, fOK = fOK
   
   ; set Object Number Vector
   (*self.pParamStruct).pROINumberVect = ptr_new(indgen(dim_edge,/long)+1L, /no_copy)
   
   ; set Default Object Parameter Vectors
   for i = 0, n_elements(whParam)-1 do *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(dim_edge, /long, value = -1.)
   
   ; '3D GraphSkel Number Edge'
   if whParamActive[0] then begin
     vec = make_array(dim_edge, /long, value = -1.0)
     for edgeIndex = 0, dim_edge-1 do vec[edgeIndex] = (*pGraph)[edgeIndex]->getOwnIndex()
     *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = vec
     print, '3D GraphSkel Number Edge ', vec
     printf, unit, '3D GraphSkel Number Edge, '
     theFormat = '(' + StrTrim(dim_edge,2) + '(F, :, ","))'
     printf, unit, vec, Format=theFormat
   endif
   
   ; '3D GraphSkel Bifurc Number',$
   if whParamActive[1] then begin
     vec = make_array(dim_edge, /long, value = -1.0)
     for edgeIndex = 0, dim_edge-1 do vec[edgeIndex] = (*pGraph)[edgeIndex]->getNumConexiones()
     *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = vec
     print, '3D GraphSkel Bifurc Number ', vec
     printf, unit, '3D GraphSkel Bifurc Number, '
     theFormat = '(' + StrTrim(dim_edge,2) + '(F, :, ","))'
     printf, unit, vec, Format=theFormat
   endif
   
   
   ; '3D GraphSkel Edge Lenght [Voxel]'
   if whParamActive[2] then begin
     vec = make_array(dim_edge, /float, value = -1.0)
     for edgeIndex = 0, dim_edge-1 do vec[edgeIndex] = (*pGraph)[edgeIndex]->getLenghtEdge()
     *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = vec
     print, '3D GraphSkel Edge Lenght [Voxel] ', vec
     printf, unit, '3D GraphSkel Edge Lenght [Voxel], '
     theFormat = '(' + StrTrim(dim_edge,2) + '(F, :, ","))'
     printf, unit, vec, Format=theFormat
   endif
   
   ; '3D GraphSkel Edge Lenght [x]'
   if whParamActive[3] then begin
     vec = make_array(dim_edge, /float, value = -1.0)
     for edgeIndex = 0, dim_edge-1 do vec[edgeIndex] = (*pGraph)[edgeIndex]->getLenghtEdgeReal()
     *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = vec
     print, '3D GraphSkel Edge Lenght [x] ', vec
     printf, unit, '3D GraphSkel Edge Lenght [x], '
     theFormat = '(' + StrTrim(dim_edge,2) + '(F, :, ","))'
     printf, unit, vec, Format=theFormat
   endif
   
   ; '3D GraphSkel Edge Angle [Degrees]'
   if whParamActive[4] then begin
     vec = make_array(dim_edge, /float, value = -1.0)
     for edgeIndex = 0, dim_edge-1 do vec[edgeIndex] = (*pGraph)[edgeIndex]->getAngleEdge()
     *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = vec
     print, '3D GraphSkel Edge Angle [Degrees] ', vec
     printf, unit, '3D GraphSkel Edge Angle [Degrees]'
     theFormat = '(' + StrTrim(dim_edge,2) + '(F, :, ","))'
     printf, unit, vec, Format=theFormat
   endif
   
   ;'3D GraphSkel Deep Level'$
   if whParamActive[5] then begin
     vec = make_array(dim_edge, /long, value = -1.0)
     for edgeIndex = 0, dim_edge-1 do vec[edgeIndex] =  (*pGraph)[edgeIndex]->getOwnLevel()
     *(*(*self.pValueStruct)[whParam[5]]).pROIParamVect = vec
     print, '3D GraphSkel Deep Level ', vec
     printf, unit, '3D GraphSkel Deep Level'
     theFormat = '(' + StrTrim(dim_edge,2) + '(F, :, ","))'
     printf, unit, vec, Format=theFormat
   endif
   
   ; mcerda
   ; '3D GraphSkel Bifurc Number Recursive',$
   if whParamActive[6] then begin
     maxnodes=N_ELEMENTS(verticesFinales)/3
     nodesvec = [-1]
     for nodeIndex = 0, maxnodes-1 do begin
       index=where(vAristas eq nodeIndex)
       if index[0] ne -1 then begin
         if nodesvec[0] eq -1 then begin
           nodesvec= [N_ELEMENTS(index)]
         endif else begin
           nodesvec=[nodesvec,N_ELEMENTS(index) ]
         endelse
       endif
     endfor
     print, '3D GraphSkel Bifurc Number Recursive', nodesvec
     printf, unit, '3D GraphSkel Bifurc Number Recursive, '
     dim_nodesvec=SIZE(nodesvec, /DIM)
     theFormat = '(' + StrTrim(dim_nodesvec[0],2) + '(F, :, ","))'
     printf, unit, nodesvec, Format=theFormat
   endif
   
   ; mcerda
   ; '3D GraphSkel Edge length Recursive',
   if whParamActive[7] then begin
     nedges=N_ELEMENTS(*pEdgesReduced[0])/3
     edgeslength= make_array(nedges, /float, value = 0.0)
     for lindex = 0, nedges-1 do begin
       left =vAristas[0, lindex]
       right=vAristas[1, lindex]
       edgeslength[lindex]+=(verticesFinales[0, left]-verticesFinales[0, right])^2
       edgeslength[lindex]+=(verticesFinales[1, left]-verticesFinales[1, right])^2
       edgeslength[lindex]+=(verticesFinales[2, left]-verticesFinales[2, right])^2
       edgeslength[lindex] = sqrt(edgeslength[lindex])
     endfor
     print, '3D GraphSkel Edge length Recursive  [Voxel]', edgeslength
     printf, unit, '3D GraphSkel Edge length Recursive  [Voxel], '
     dim_edgeslength=SIZE(edgeslength, /DIM)
     theFormat = '(' + StrTrim(dim_edgeslength[0],2) + '(F, :, ","))'
     printf, unit, edgeslength, Format=theFormat
   endif
   
   ; '3D GraphSkel Internal Angle Recursive',
   if whParamActive[7] then begin
   nedges=dim_edge
   anglesrecursive= [-1]
   vAristasReduced=*pEdgesReduced[0]
   vVerticesReduced=*pVerticesReduced[0]
   for lindex = 0L, nedges-1L do begin
   
       left =vAristasReduced[lindex*3L+1L]
       right=vAristasReduced[lindex*3L+2L]
       
       ;first case
       index=where(vAristas[0,lindex:*] eq left)
       if index[0] ne -1 then begin
       
         for i=0, N_ELEMENTS(index)-1 do begin
          localLeft =vAristas[0,lindex+index[i]]
          localRight=vAristas[1,lindex+index[i]]
          
          magLeft=sqrt(total( (vVerticesReduced[*,right]-vVerticesReduced[*,left]) ^2))
          magRight=sqrt(total( (vVerticesReduced[*,localRight]-vVerticesReduced[*,localLeft])^2 ))
          
          vLeft =(vVerticesReduced[*,right]-vVerticesReduced[*,left])/magLeft
          vRight=(vVerticesReduced[*,localRight]-vVerticesReduced[*,localLeft])/magRight
          
          angle=0.0
          if magLeft gt 0.001 and magRight gt 0.001 then angle=acos(total(vLeft*vRight))
          
          if anglesrecursive[0] eq -1 then begin
           anglesrecursive= [angle]
          endif else begin
           anglesrecursive=[anglesrecursive,angle ]
          endelse
          
         endfor
         
       endif
       
       ;second case
       index=where(vAristas[1,lindex:*] eq left)
       if index[0] ne -1 then begin
       
         for i=0, N_ELEMENTS(index)-1 do begin
          localLeft =vAristas[0,lindex+index[i]]
          localRight=vAristas[1,lindex+index[i]]
          
          magLeft=sqrt(total( (vVerticesReduced[*,right]-vVerticesReduced[*,left]) ^2))
          magRight=sqrt(total( (vVerticesReduced[*,localRight]-vVerticesReduced[*,localLeft])^2 ))
          
          vLeft=  (vVerticesReduced[*,right]-vVerticesReduced[*,left])/magLeft
          vRight=-(vVerticesReduced[*,localRight]-vVerticesReduced[*,localLeft])/magRight
          
          angle=0.0
          if magLeft gt 0.001 and magRight gt 0.001 then angle=acos(total(vLeft*vRight))
          
          if anglesrecursive[0] eq -1 then begin
           anglesrecursive= [angle]
          endif else begin
           anglesrecursive=[anglesrecursive,angle ]
          endelse
          
         endfor
         
       endif
       
       ;;;
       ;third case
       index=where(vAristas[1,lindex:*] eq right)
       if index[0] ne -1 then begin
       
         for i=0, N_ELEMENTS(index)-1 do begin
          localLeft =vAristas[0,lindex+index[i]]
          localRight=vAristas[1,lindex+index[i]]
          
          magLeft=sqrt(total( (vVerticesReduced[*,right]-vVerticesReduced[*,left]) ^2))
          magRight=sqrt(total( (vVerticesReduced[*,localRight]-vVerticesReduced[*,localLeft])^2 ))
          
          vLeft= -(vVerticesReduced[*,right]-vVerticesReduced[*,left])/magLeft
          vRight=-(vVerticesReduced[*,localRight]-vVerticesReduced[*,localLeft])/magRight
          
          angle=0.0
          if magLeft gt 0.001 and magRight gt 0.001 then angle=acos(total(vLeft*vRight))
          
          if anglesrecursive[0] eq -1 then begin
           anglesrecursive= [angle]
          endif else begin
           anglesrecursive=[anglesrecursive,angle ]
          endelse
          
         endfor
         
       endif
       
       ;fourth case
       index=where(vAristas[0,lindex:*] eq right)
       if index[0] ne -1 then begin
       
         for i=0, N_ELEMENTS(index)-1 do begin
          localLeft =vAristas[0,lindex+index[i]]
          localRight=vAristas[1,lindex+index[i]]
          
          magLeft=sqrt(total( (vVerticesReduced[*,right]-vVerticesReduced[*,left]) ^2))
          magRight=sqrt(total( (vVerticesReduced[*,localRight]-vVerticesReduced[*,localLeft])^2 ))
          
          vLeft= -(vVerticesReduced[*,right]-vVerticesReduced[*,left])/magLeft
          vRight= (vVerticesReduced[*,localRight]-vVerticesReduced[*,localLeft])/magRight
          
          angle=0.0
          if magLeft gt 0.001 and magRight gt 0.001 then angle=acos(total(vLeft*vRight))
          
          if anglesrecursive[0] eq -1 then begin
           anglesrecursive= [angle]
          endif else begin
           anglesrecursive=[anglesrecursive,angle ]
          endelse
          
         endfor
         
       endif
       
     endfor
     
     print, '3D GraphSkel Internal Angle Recursive  [Degrees]', 180.0*anglesrecursive/!PI
     printf, unit, '3D GraphSkel Internal Angle Recursive  [Degrees], '
     dim_anglesrecursive=size(anglesrecursive,/DIM)
     theFormat = '(' + StrTrim(dim_anglesrecursive[0],2) + '(F, :, ","))'
     printf, unit, 180.0*anglesrecursive/!PI, Format=theFormat
   endif
   
   ; mcerda
   ; '3D GraphSkel Curved Edge Length [Voxel]'
   if whParamActive[8] then begin
     vec = make_array(dim_edge, /float, value = 0.0)
     nedges=N_ELEMENTS(aristasFull)
     edgeslength= [-1]
     lindex=0L
     i=0L
     while lindex lt nedges do begin
     
       first=lindex+1L
       last =lindex+aristasFull[lindex]
       indexes=aristasFull[first:last]
       length=self->pathLength( verticesFinales, indexes, [1.0, 1.0, 1.0])
       edgeslength = (edgeslength[0] eq -1)? [length]:[edgeslength, length]
       vec[i]=length
       i+=1L
       lindex=lindex+aristasFull[lindex]+1L
       
     endwhile
     *(*(*self.pValueStruct)[whParam[8]]).pROIParamVect = vec
     ;print, '3D GraphSkel Curved Edge Length ', vec
     print, '3D GraphSkel Curved Edge Length  [Voxel]', edgeslength
     printf, unit, '3D GraphSkel Curved Edge Length  [Voxel], '
     dim_edgeslength=size(edgeslength,/DIM)
     theFormat = '(' + StrTrim(dim_edgeslength[0],2) + '(F, :, ","))'
     printf, unit, edgeslength, Format=theFormat
   endif
   
   ; '3D GraphSkel Curved Edge Length [x]'
   if whParamActive[9] then begin
     vec = make_array(dim_edge, /float, value = 0.0)
     nedges=N_ELEMENTS(aristasFull)
     edgeslength= [-1]
     lindex=0L
     i=0L
     while lindex lt nedges do begin
     
       first=lindex+1L
       last =lindex+aristasFull[lindex]
       indexes=aristasFull[first:last]
       length=self->pathLength( verticesFinales, indexes, xyzSizePerPixel )
       edgeslength = (edgeslength[0] eq -1)? [length]:[edgeslength, length]
       vec[i]=length
       i+=1L
       lindex=lindex+aristasFull[lindex]+1L
       
     endwhile
     *(*(*self.pValueStruct)[whParam[9]]).pROIParamVect = vec
     ;print, '3D GraphSkel Curved Edge Length ', vec
     print, '3D GraphSkel Curved Edge Length  [x]', edgeslength
     printf, unit, '3D GraphSkel Curved Edge Length  [x], '
     dim_edgeslength=size(edgeslength,/DIM)
     theFormat = '(' + StrTrim(dim_edgeslength[0],2) + '(F, :, ","))'
     printf, unit, edgeslength, Format=theFormat
   endif
      
   ; '3D GraphSkel Segment Volume [Voxel^3]'
   if whParamActive[10] then begin
     vec = make_array(dim_edge, /float, value = 0.0)
     nedges=N_ELEMENTS(aristasFull)
     ;TODO ROI number := 0!!!
     roiNumber=0L
     ss=SIZE(verticesFinales, /DIM)
     nodesMesh=self->getNodeVerticesStructure( *(pSkeletonMeshAssociation[roiNumber]), ss[1] )
     verticesMesh= *(pVerticesMesh[roiNumber])
     connectivityMesh= *(pPolygonsMesh[roiNumber])
     
     ;TODO nodesMesh has a different number os vertices than verticesMesh, more parameters?
     lindex=0L
     i=0L
     
     while lindex lt nedges do begin
     
       first=lindex+1L
       last =lindex+aristasFull[lindex]
       indexes=aristasFull[first:last]
       vol=self->segmentVolume( verticesMesh, connectivityMesh, indexes,  nodesMesh, [1.0, 1.0, 1.0])
       vec[i]=vol
       i+=1L
       lindex=lindex+aristasFull[lindex]+1L
       
     endwhile
     *(*(*self.pValueStruct)[whParam[10]]).pROIParamVect = vec
     print, '3D GraphSkel Segment Volume  [Voxel^3]', vec
     printf, unit, '3D GraphSkel Segment Volume  [Voxel^3], '
     dim_vec=size(vec,/DIM)
     theFormat = '(' + StrTrim(dim_vec[0],2) + '(F, :, ","))'
     printf, unit, vec, Format=theFormat
   endif

; '3D GraphSkel Segment Volume [Voxel^3]'
   if whParamActive[11] then begin
     vec = make_array(dim_edge, /float, value = 0.0)
     nedges=N_ELEMENTS(aristasFull)
     ;TODO ROI number := 0!!!
     roiNumber=0L
     ss=SIZE(verticesFinales, /DIM)
     nodesMesh=self->getNodeVerticesStructure( *(pSkeletonMeshAssociation[roiNumber]), ss[1] )
     verticesMesh= *(pVerticesMesh[roiNumber])
     connectivityMesh= *(pPolygonsMesh[roiNumber])
     
     ;TODO nodesMesh has a different number os vertices than verticesMesh, more parameters?
     lindex=0L
     i=0L
     
     while lindex lt nedges do begin
     
       first=lindex+1L
       last =lindex+aristasFull[lindex]
       indexes=aristasFull[first:last]
       vol=self->segmentVolume( verticesMesh, connectivityMesh, indexes,  nodesMesh, xyzSizePerPixel)
       vec[i]=vol
       i+=1L
       lindex=lindex+aristasFull[lindex]+1L
       
     endwhile
     *(*(*self.pValueStruct)[whParam[11]]).pROIParamVect = vec
     print, '3D GraphSkel Segment Volume  [x^3]', vec
     printf, unit, '3D GraphSkel Segment Volume  [x^3], '
     dim_vec=size(vec,/DIM)
     theFormat = '(' + StrTrim(dim_vec[0],2) + '(F, :, ","))'
     printf, unit, vec, Format=theFormat
   endif

   close, unit
   
   ;DEBUG skeleton for Sergio
   filename='C:\RSI\skelEstimation.skel'
   s_getoSkeletonFromMeshRepair_writeoff, *pVerticesReduced[0], *pEdgesReduced[0], filename, elementSize=3
      
end

pro C_sROIParam_3DObjGraphSkelDir::restoreMasterGraphCreation, pVertices, pEdges, xyzSizePerPixel, skelGraphMaster=skelGraphMaster, $
   verticesFinales=verticesFinales, vAristas=vAristas, fAllowLoops=fAllowLoops, vEdgeRoot=vEdgeRoot

   verticesFinales = *pVertices[0] ; Simply load
   dim_edge      = N_ELEMENTS(*pEdges[0])/3
   vAristas      = MAKE_ARRAY(2,dim_edge, /long)
   vAristas[0,*] = (*pEdges[0])[1:(3*dim_edge)-1:3]
   vAristas[1,*] = (*pEdges[0])[2:(3*dim_edge)-1:3]

   ; For now use the biggest edge with external node as root edge... OR.... set manual...
   if vEdgeRoot eq -1 then begin 
    distanciaBase = 0.0
    vFlip = 0b
    for index = 0, dim_edge-1 do begin
      pointInit = verticesFinales[*,vAristas[0,index]]
      pointEnd  = verticesFinales[*,vAristas[1,index]]
      delta = pointEnd - pointInit
      distancia = sqrt(total(delta*delta))
      if(distancia gt distanciaBase) then begin
         wLeft  = where((vAristas[0,*] eq vAristas[0,index]) or (vAristas[1,*] eq vAristas[0,index]),/L64)
         wRight = where((vAristas[0,*] eq vAristas[1,index]) or (vAristas[1,*] eq vAristas[1,index]),/L64)
         
         if( ((wLeft[0] ne -1) and (N_ELEMENTS(wLeft) eq 1)) and (N_ELEMENTS(wRight) gt 1)) then begin
             distanciaBase = distancia
             vEdgeRoot     = index
             vFlip         = 1b
         endif
         if( ((wRight[0] ne -1) and (N_ELEMENTS(wRight) eq 1)) and (N_ELEMENTS(wLeft) gt 1)) then begin
             distanciaBase = distancia
             vEdgeRoot     = index
             vFlip         = 0b
         endif
      endif
    endfor
   endif


   skelGraphMaster = obj_new('C_s3DGraphSkelDir')
   skelGraphMaster->MasterGraphCreation, vEdgeRoot = vEdgeRoot, vVertices = verticesFinales, vAristas = vAristas, $
     xyzSizePerPixel = xyzSizePerPixel, vFlip = vFlip, fAllowLoops=fAllowLoops
   
end

function C_sROIParam_3DObjGraphSkelDir::pathLength, vertices, indexes, xyzSizePerPixel

    path=0.0
    n=N_ELEMENTS(indexes)-2
    
    for i=0, n do begin
      dx=(vertices[0, indexes[i] ]-vertices[0, indexes[i+1] ])^2
      dy=(vertices[1, indexes[i] ]-vertices[1, indexes[i+1] ])^2
      dz=(vertices[2, indexes[i] ]-vertices[2, indexes[i+1] ])^2
      path+=sqrt((xyzSizePerPixel[0]^2)*dx+(xyzSizePerPixel[1]^2)*dy+(xyzSizePerPixel[2]^2)*dz)
    endfor

    return, path
end


function C_sROIParam_3DObjGraphSkelDir::segmentVolume, verticesMesh, connectivityMesh, nodesSegment, nodesMesh, xyzSizePerPixel

    volume=0.0
    meshSegmentVertices=[-1]
    
    ;get vertices set from the original mesh associated to this skeleton segment
    for i=0L, N_ELEMENTS(nodesSegment)-1 do begin
      p=nodesSegment[i]
      val=*(nodesMesh[p])
      if val[0] ne -1 then meshSegmentVertices=meshSegmentVertices[0] eq -1 ? val : [ meshSegmentVertices, val ]
    endfor
    
    ;get triangles set from the original mesh associated to this skeleton segment (SPEED?)
    nVerticesMeshSegment=max(meshSegmentVertices) ;some vertices may be deleted by the skeleton
    meshSegmentConnectivity=[-1]
    for i=0L, N_ELEMENTS(connectivityMesh)/4L - 1L do begin
      a=where(connectivityMesh[i*4L+1L] eq  meshSegmentVertices)
      b=where(connectivityMesh[i*4L+2L] eq  meshSegmentVertices)
      c=where(connectivityMesh[i*4L+3L] eq  meshSegmentVertices)
      if  a[0] ne -1 and N_ELEMENTS(a) eq 1 and $
          b[0] ne -1 and N_ELEMENTS(b) eq 1 and $
          c[0] ne -1 and N_ELEMENTS(c) eq 1 then $
         meshSegmentConnectivity=meshSegmentConnectivity[0] eq -1 ? connectivityMesh[i*4L:(i+1L)*4L-1] : [ meshSegmentConnectivity, connectivityMesh[i*4L:(i+1L)*4L-1] ]
         
    endfor
    meshSegmentVerticesCoordinates=fltarr(3, nVerticesMeshSegment+1)
    for i=0, N_ELEMENTS(meshSegmentVertices) -1 do begin
      meshSegmentVerticesCoordinates[*,meshSegmentVertices[i]]=verticesMesh[*,meshSegmentVertices[i]]
    endfor
    
    if meshSegmentConnectivity[0] eq -1 then return, 0.0
    
    ;repair the mesh piece {meshSegmentVertices, meshSegmentConnectivity} associated to this skeleton segment
    callMeshFixDLL, meshSegmentVerticesCoordinates, meshSegmentConnectivity, repairedVertices, repairedPolygons
    nverticesRep = size(repairedVertices, /dim)
    
    if repairedVertices[0] eq -1 then return, 0.0
    
    ;get volume using signed thetahedra volume (it must be repaired first)
    repairedVertices[0,*]=repairedVertices[0,*]*xyzSizePerPixel[0]
    repairedVertices[1,*]=repairedVertices[1,*]*xyzSizePerPixel[1]
    repairedVertices[2,*]=repairedVertices[2,*]*xyzSizePerPixel[2]
    
    volume=mesh_volume(repairedVertices, repairedPolygons)
   
   return, volume
end

; nodeVertices list has a structure N_s1 s1_{0} ... s1_{N_s1-1} N_s2 ... N_sk.
function C_sROIParam_3DObjGraphSkelDir::getNodeVerticesStructure, nodeVertices, nNodes
  list=ptrarr(nNodes)
  jump=0L
  
  for i=0L, nNodes-1L do begin
    left=jump+1L
    right=jump+nodeVertices[jump]
    
    ;TODO some skeleton nodes have 0! associated vertices
    if nodeVertices[jump] ne 0 then begin
      list[i]=Ptr_New(nodeVertices[ left:right ])
    endif else begin
      list[i]=Ptr_New( [-1] )
    endelse
    jump   =jump+nodeVertices[jump]+2L
  endfor

  return, list
end

function C_sROIParam_3DObjGraphSkelDir::init

    ROIParamStruct = {name:'3D Graph Skeleton',$     ;  ROI Name.
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

    ROIParamNames = ['3D GraphSkel Number Edge',$
                     '3D GraphSkel Bifurc Number',$
                     '3D GraphSkel Edge Lenght [Voxel]',$
                     '3D GraphSkel Edge Lenght [x²]',$
                     '3D GraphSkel Edge Angle [Degrees]',$
                     '3D GraphSkel Deep Level',$
                     '3D GraphSkel Bifurc Number Recursive',$
                     '3D GraphSkel Edge length Recursive [Voxel]',$
                     '3D GraphSkel Curved Edge Length [Voxel]',$
                     '3D GraphSkel Curved Edge Length [x]',$
                     '3D GraphSkel Edge Volume [Voxel^3]',$
                     '3D GraphSkel Edge Volume [x^3]'$
                     ]

    nParams = n_elements(ROIParamNames)
    self.pValueStruct = ptr_new(ptrArr(nParams))
    ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')

    ROIParamActive = make_array(nParams, /byte, value = 1b)
    ROIParamMin = make_array(nParams, /float, value = -1000.)
    ROIParamMax = make_array(nParams, /float, value = 1000.)
    ROIParamValues = make_array(nParams, /float, value = 0.)
    ROIParamActive[10]=0b
    ROIParamActive[11]=0b
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

    for i = 1, nParams-1 do begin
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


Pro C_sROIParam_3DObjGraphSkelDir__define
   tmp = {C_sROIParam_3DObjGraphSkelDir, pParamStruct:ptr_new(), pValueStruct:ptr_new(), inherits C_sROIParam}
End