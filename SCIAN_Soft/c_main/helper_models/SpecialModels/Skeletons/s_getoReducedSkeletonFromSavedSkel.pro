pro s_getoReducedSkeletonFromSavedSkel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, pVertices = pVertices, pEdges = pEdges, silent = silent, fAddGraphicModel = fAddGraphicModel

  oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

  ;xyzSizePerPixel = oGroupReference->getxyzSizePerPixel() ; es equivalente
  pParamStruct    = oGroupReference->getpParamStruct()
  xSizePerPixel   = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
  ySizePerPixel   = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
  zSizePerPixel   = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Size [pixel]'))[0]]
  xyzSizePerPixel = [xSizePerPixel,ySizePerPixel,zSizePerPixel]
  fAddGraphicModel = keyword_set(fAddGraphicModel)
  zxRatio = xyzSizePerPixel[2] / xyzSizePerPixel[0]
  
    ; option to load saved models 
  fUseSavedModels = 1b
  fSaveModels = 1b
    ; TODO names familiy for skeleton models... 1st and 2nd
  savedSkeletonModelName = '_Skeleton_'
  modelName = '_SkeletonReduced_'

  nObj = oGroupReference->count()
  pEdges    = ptrArr(nObj)
  pVertices = ptrArr(nObj)
  fFile = make_array(nObj, /byte, value = 0b)
  ; At the moment, we will assume that all the skeletons are saved
  usedSkel = 0; ninguno
  loadFromObjFile = 0
  if (fUseSavedModels eq 1) then begin
    oGroupReference->restoreSavedSkeletonModels, stack_tlb = stack_tlb, modelName = modelName, pVertices = pVerticesFinales, pEdges = pAristasFinales, fFile = fFile, loadFromObjFile = loadFromObjFile
    if(total(fFile) ne 0) then usedSkel = 1
  endif

  if((fUseSavedModels eq 0) or (usedSkel eq 0) ) then begin
    oGroupReference->restoreSavedSkeletonModels, stack_tlb = stack_tlb, modelName = savedSkeletonModelName, pVertices = pVerticesFinales, pEdges = pAristasFinales, fFile = fFile, loadFromObjFile = loadFromObjFile
    if(total(fFile) ne 0) then usedSkel = 2
  endif

  if (usedSkel ne 0) then begin
    for i = 0, nObj-1 do begin
      objNumber = (oGroupReference->get(position = i))->getNumber()
      ; LOAD EACH SKEL
      distanciaBase = 0.0
      if(usedSkel eq 1)then begin
        verticesReducidos = *pVerticesFinales[i]
        aristasReducidas  = *pAristasFinales[i]
      endif else begin
        verticesReducidos = *pVerticesFinales[i]; TODO complete vertices/aristas... but for now .... i keep unused data
        dim_edge      = N_ELEMENTS(*pAristasFinales[i])/3 ;IDL format for segments

        vAristas      = MAKE_ARRAY(2,dim_edge, /long)
        vAristas[0,*] = (*pAristasFinales[i])[1:(3*dim_edge)-1:3]
        vAristas[1,*] = (*pAristasFinales[i])[2:(3*dim_edge)-1:3]

          ; For now use the biggest edge with external node as root edge... OR.... set manual...
        distanciaBase = 0.0
        vEdgeRoot     = -1
        vFlip         = 0b
        
        ;to find the root
        for index = 0, dim_edge-1 do begin
          pointInit = verticesReducidos[*,vAristas[0,index]]
          pointEnd  = verticesReducidos[*,vAristas[1,index]]
          delta = pointEnd - pointInit
          distancia = sqrt(total(delta*delta))

          if (distancia gt distanciaBase) then begin
            wLeft  = where((vAristas[0,*] eq vAristas[0,index]) or (vAristas[1,*] eq vAristas[0,index]),/L64)
            wRight = where((vAristas[0,*] eq vAristas[1,index]) or (vAristas[1,*] eq vAristas[1,index]),/L64)

            if (((wLeft[0] ne -1) and (N_ELEMENTS(wLeft) eq 1)) and (N_ELEMENTS(wRight) gt 1)) then begin
              distanciaBase = distancia
              vEdgeRoot     = index
              vFlip         = 1b
            endif
            if (((wRight[0] ne -1) and (N_ELEMENTS(wRight) eq 1)) and (N_ELEMENTS(wLeft) gt 1)) then begin
               distanciaBase = distancia
               vEdgeRoot     = index
               vFlip         = 0b
            endif
          endif
        endfor
        
		;manual root selection
        ;vEdgeRoot=1

        skelGraphMaster = obj_new('C_s3DGraphSkelDir')
        ;mcerda
        fAllowLoops=0b
        skelGraphMaster->MasterGraphCreation, vEdgeRoot = vEdgeRoot, vVertices = verticesReducidos, $
              vAristas = vAristas, xyzSizePerPixel = xyzSizePerPixel, vFlip = vFlip, fAllowLoops=fAllowLoops
        aristasReducidas = skelGraphMaster->CreateReducedSkelEdges()
        aristasFull = skelGraphMaster->CreateFullSkelEdges()
;;        mcerda: caso simple
;        factor=100.0
;        fAllowLoops=0b
;        verticesReducidos = [[0,0,0], [factor,0,0], [2.*factor,0,0], [3.*factor,0,0], [4.*factor,0,0], [2.*factor,factor,0], [2.*factor,2.*factor,0]]
;        vAristas = [[0,1], [1,2], [2,3], [3,4], [3,5], [5,6], [5,1]]
;        xyzSizePerPixel=[0.00997567,   0.00995146,      1.00000]
;        
;        skelGraphMaster->MasterGraphCreation, vEdgeRoot = 0, vVertices = verticesReducidos, $
;               vAristas = vAristas, xyzSizePerPixel = xyzSizePerPixel, vFlip = 1b, fAllowLoops=fAllowLoops
;               
;        aristasReducidas = skelGraphMaster->CreateFullSkelEdges()
;        print, aristasReducidas
        
        OBJ_DESTROY, skelGraphMaster
      endelse
          
      if (fSaveModels eq 1) then begin
    
          ; TODO factorize this and the piece from C_sROI3DGroupObj::getoACModelWithSaveRestore, and the other...
        s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos

        widget_control, stateObj_tlb, get_uValue = state, /no_copy
          currROI3DGroupFileName = state.currROI3DGroupFileName
        widget_control, stateObj_tlb, set_uValue = state, /no_copy

        widget_control, stack_tlb, get_uValue = stackState, /no_copy
          (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
        widget_control, stack_tlb, set_uValue = stackState, /no_copy

        slash = path_sep()
        posLastSlash = strPos(currROI3DGroupFileName, slash, /reverse_search)
        currROI3DGroupFileName = strMid(currROI3DGroupFileName, strPos(currROI3DGroupFileName, slash, /reverse_search)+1)
        currROI3DGroupFileName = strMid(currROI3DGroupFileName, 0, strPos(currROI3DGroupFileName, '.sav'))
        modelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]] + strcompress(modelName + slash, /rem))
        if (file_test(strCompress(modelPath),/DIRECTORY) eq 0b) then file_mkDir, strCompress(modelPath)

        roiModelFileName = strCompress(modelPath + strcompress(currROI3DGroupFileName + modelName + string(objNumber) + '.sav', /rem))
        vertices         = verticesReducidos
        edges            = aristasReducidas
        save, vertices, edges, fileName = roiModelFileName
      endif

      (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha

      dim_edge       = N_ELEMENTS(aristasReducidas)/3
      vAristas       = MAKE_ARRAY(2,dim_edge, /long)
      vAristas[0,*]  = aristasReducidas[1 : (3*dim_edge)-1 : 3]
      vAristas[1,*]  = aristasReducidas[2 : (3*dim_edge)-1 : 3]
      vEdgeRoot      = 0
      vFlip          = 1
      skelGraphMaster = obj_new('C_s3DGraphSkelDir')
      skelGraphMaster->MasterGraphCreation, vEdgeRoot = vEdgeRoot, vVertices = verticesReducidos, vAristas = vAristas, xyzSizePerPixel = xyzSizePerPixel, vFlip = vFlip
      pVertices[i] = ptr_new(verticesReducidos)
      pEdges[i] = ptr_new(vAristas)
      pGraph = *(skelGraphMaster->GetmasterGraphSkel())
      dim_edge = N_ELEMENTS(pGraph)
      ;0: begin        ; '3D GraphSkel Number Edge'
      ;1: begin        ; '3D GraphSkel Bifurc Number',$      
      ;2: begin        ; '3D GraphSkel Edge Lenght [Voxel]'
      ;3: begin        ; '3D GraphSkel Edge Lenght [x²]'
      ;4: begin        ; '3D GraphSkel Edge Angle [Degrees]'
      ;5: begin        ;'3D GraphSkel Deep Level'$
      parametroDeseado = sGetSavedSelectedParam(stack_tlb = stack_tlb)
      parametroDeseado >= 0
      print, 'Restoring skeleton param ', parametroDeseado

      limitesMinMax = skelGraphMaster->MINMAXforParameter(vSelectedParam = parametroDeseado)
      deltaParam   = limitesMinMax[1]- limitesMinMax[0]
                
      print, 'skeleton param scale ', limitesMinMax
      mySymbol = OBJ_NEW('IDLgrSymbol', 5, COLOR=[255,0,0])
      
      if fAddGraphicModel then $
        for nodeIndex = 0, dim_edge-1 do begin
          initVertex   = pGraph[nodeIndex]->getInitXYZ()
          endVertex    = pGraph[nodeIndex]->getEndXYZ()
          actualParam  = skelGraphMaster->SelectedParameter(vSelectedParam = parametroDeseado, vActualNode = nodeIndex)
          pVolState    = oGroupReference->getpVolState()
          colorTable0 = reform((*pvolState).rgbValues[0,*,*])
          scaledColor  = (abs(deltaParam) lt 0.0001) ? 0 : (actualParam - limitesMinMax[0])*(250.0 - 50.0) / deltaParam
          segmentColor = colorTable0[*, scaledColor]
          ;mcerda z-scale
          ;initVertex[2]/=zxRatio
          ;endVertex[2]/=zxRatio
          
          oObjectModel->add, obj_new('IDLgrPolyline', [[initVertex], [endVertex]], color = segmentColor, thick = 3,$
                                                      xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                      name = strCompress('3DReducedSkeletonMeshModel:' + string(objNumber) + '_' + string(nodeIndex), /rem), $
                                                      uvalue = 'ObjInOriginalPosition', symbol=mySymbol)
        endfor
        obj_destroy, skelGraphMaster
    endfor
  endif
end
