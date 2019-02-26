; s_getoSkeletonFromMeshModel
;
;
; PARAMETERS
;
;   stack_tlb              The current stack_tlb.
;   oObjectModel           The object model that serves as parent for the graphic skeleton models.
;   o3DroiGroup            Reference to the 3D ROI group (C_sROI3DGroupObject) used for the skeleton computations.
;   useFixedMeshes         Indicates wheter to use fixed meshes or not.                 
;                          DEFAULT VALUE: 1 (true). Standard INT type must be used (not BYTE), to avoid weird string parsing.
;   fSaveSkeletonInSavFile Indicates if the skeletons must be saved as a .sav file.
;                          DEFAULT VALUE: 1 (true), since computations are time consuming for large ROIs.
;   fUseSavedSkeletons     Indicates that the skeletons must be loaded from a .sav file (if exists).
;                          DEFAULT VALUE: 1 (true), since computations are time consuming for large ROIs.
;                          If the 'use' and 'save' flags were both true and the skeleton files do exist, no files will be saved.
;   fSaveAsObj             Indicates that the skeletons must be saved as .obj files.
;                          DEFAULT VALUE: false (keyword not set).
;   fUseSavedObj           Indicates that the skeletons must be loaded from .obj files.
;                          DEFAULT VALUE: false (keyword not set).
;   lastSkelStep           Indicates the skeleton computation stages to perform.
;                          1-> mesh contraction ; 2 -> mesh contraction + edge collapse ; 3 -> mesh contraction + edge collapse + embedded refinement.
;                          DEFAULT VALUE: 3.
;   loadLevel              When loading skeletons from files...
;                          0 -> restore only saved skeletons, and missing files will be skipped
;                          1 -> restore from existing skeleton files and compute for the missing files
;                          2 -> forces the computation for all the skeletons if there are some missing files
;                          DEFAULT VALUE: 0.
;   saveLevel              When saving skeletons to files...
;                          1 -> save only for the missing skeleton files
;                          2 -> save all the skeleton files overwriting the existing ones
;                          DEFAULT VALUE: 2.
; NOTES
;   The procedure follows this scheme
;     i check if saved skeletons exist for computation, according to the specified parameters
;     i.a if they exist, restore them and done!
;     i.b if not, proceed to check and compute fixed meshes and then compute the corresponding skeletons
;     ii check if previously computed skeletons exist
;
; HISTORY
;     2012.04 First release. M Cerda, J Jara, F Santibanez.
;
pro s_getoSkeletonFromMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, o3DroiGroup = o3DroiGroup,$
                                 lastSkelStep = lastSkelStep,$
                                 fSaveSkeletonInSavFile = fSaveSkeletonInSavFile, fSaveAsObj = fSaveAsObj,$
                                 fUseSavedSkeletons = fUseSavedSkeletons, fUseSavedObj = fUseSavedObj,$
                                 loadLevel = loadLevel, saveLevel = saveLevel, useFixedMeshes = useFixedMeshes, useAllFlagsInLoad = useAllFlagsInLoad

  if ~obj_valid(o3DroiGroup) then begin
    print, 'No valid ROI group given, returning...'
    return
  end

  nObj = o3DroiGroup->count()
  if (nObj lt 1) then begin
    print, '0 ROI objects found in this o3DroiGroup, returning...'
    return
  endif

  o3DroiGroup->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  xyzPixel = o3DroiGroup->getxyzSizePerPixel()
  zxRatio = xyzPixel[2] / xyzPixel[0]
  ; Color settings
  ;rgb_table0 = bytArr(256,3) + transpose((*o3DroiGroup.pVolState).rgbValues[0,*,*])
  ;color = 

  lastSkelStep = keyword_set(lastSkelStep) ? lastSkelStep : 3           ; 1 -> contract, 2 -> collapse, 3 -> refinement
  skeletonType = (lastSkelStep le 1)       ? 0            : 1           ; 0 -> mesh    , 1 -> polyline
 ;maxnvertices = keyword_set(maxNumVertices) ? maxNumVertices : 10000.0 ; max number of vertices in the mesh (10^5 takes ~2 days for skeleton computation)
 ;minarea      = 0.0                                                    ; area to consider a triangle "degenerated" using exact arithmetic (wired in the DLL)

    ; Debug flags
  debugLevelSkeletonization = 2L
  fContractionDebugLevel    = debugLevelSkeletonization
  fConnSurgeryDebugLevel    = debugLevelSkeletonization
  fRefinementDebugLevel     = debugLevelSkeletonization

    ; Parameters for the embedded refinement step.
  fRefinementCheckRepresentativeness = 2
  refinementN                        = 3L

  debug                  = 1 ;save mesh file (.off) at each step for debug
  if ~keyword_set(useFixedMeshes)         then useFixedMeshes         = 1
  if ~keyword_set(fSaveSkeletonInSavFile) then fSaveSkeletonInSavFile = 1b
  if ~keyword_set(loadLevel)              then loadLevel              = 1
  fUseSavedObj           = keyword_set(fUseSavedObj)
  fSaveAsObj             = keyword_set(fSaveAsObj)
  fUseSavedSkeletons     = keyword_set(fUseSavedSkeletons)
  useAllFlagsInLoad      = keyword_set(useAllFlagsInLoad)

    ; TODO names familiy for skeleton models... this is the first, also, the suffix handling could be factorized to a utility method for models other than the skeleton.
  skeletonModelName      = '_Skeleton_'
  modelName              = skeletonModelName

  if keyword_set(useAllFlagsInLoad) then begin
    skeletonParamsSuffix = '_meshFix'            + string(useFixedMeshes) $
                         + '_step'               + string(lastSkelStep) $
                         + '_refinementCheckRep' + string(fRefinementCheckRepresentativeness) $
                         + '_refinementN'        + string(refinementN)
    skeletonParamsSuffix = strCompress(skeletonParamsSuffix, /remove_all)
    modelName            = skeletonModelName + skeletonParamsSuffix
  endif

  skeletonToComputeIndexes = make_array(nObj, /byte, value = ~fUseSavedSkeletons)

  if (fUseSavedSkeletons eq 1) then begin
    o3DroiGroup->restoreSavedSkeletonModels, stack_tlb = stack_tlb, modelName = modelName, pVertices = pVertices, pEdges = pEdges, $
        fFile = fFile, loadFromObjFile = fUseSavedObj, fSkeletonTypes = fSkeletonTypes, pSkeletonMeshAssociation=pSkeletonMeshAssociation
        
    whType = where(fSkeletonTypes ne lastSkelStep, neCount)
    if (neCount gt 0) then fFile[whType] = 0b
    restoredSkeletonsCount = total(fFile)
    print, 'Restored ', restoredSkeletonsCount, ' skeletons (total obj. count is ', nObj, ')'

    if (restoredSkeletonsCount eq nObj) then begin
      skeletonTypes = fSkeletonTypes ge 1
      for i = 0L, nObj-1 do begin
        objNumber = (o3DroiGroup->get(position = i))->getNumber()
        (o3DroiGroup->get(position = i))->getProperty, color = color, alpha_channel = alpha
        s_addGraphicalSkeletonStruct, oObjectModel = oObjectModel, vertices = *pVertices[i], connectivity = *pEdges[i], skeletonType = skeletonTypes[i],$
                                      xcc = xCoord_conv, ycc = yCoord_conv, zcc = zCoord_conv,ZXRATIO = zxRatio,$
                                      alpha = alpha, color = color, name = strCompress('3DSkeletonMeshModel:' + string(objNumber), /remove_all)
      endfor
      print, 'All skeletons restored from file'
      return
    endif else begin
        ; loadLevel
        ; 0 -> restore only saved skeletons, and missing files will be skipped
        ; 1 -> restore from existing skeleton files and compute for the missing files
        ; 2 -> forces the computation for all the skeletons if there are some missing files
      case loadLevel of
        0: begin
           for i = 0L, nObj-1 do $
             if (fFile[i] eq 1) then begin
               objNumber = (o3DroiGroup->get(position = i))->getNumber()
               (o3DroiGroup->get(position = i))->getProperty, color = color, alpha_channel = alpha
               s_addGraphicalSkeletonStruct, oObjectModel = oObjectModel, vertices = *pVertices[i], connectivity = *pEdges[i], skeletonType = fSkeletonTypes[i],$
                                             xcc = xCoord_conv, ycc = yCoord_conv, zcc = zCoord_conv, ZXRATIO = zxRatio,$
                                             alpha = alpha, color = color, name = strCompress('3DSkeletonMeshModel:' + string(objNumber), /remove_all)
             endif
           return
           endcase
        1: skeletonToComputeIndexes[where(fFile eq 0)] = 1b
        2: skeletonToComputeIndexes[*]                 = 1b
        else: begin
              f = dialog_message('Unrecognized load option (loadLevel = ' + loadLevel + '). Call a programmer...')
              stop
              end
      endcase
    endelse
  endif ; if (fUseSavedSkeletons eq 1)

    ; TODO Get+check the proper indices to avoid inconsistencies... "position = i" vs. objNumber
  roiNumVec = make_array(nObj, /INTEGER, value = 0)
  for i = 0L, nObj-1 do $
    roiNumVec[i] = (o3DroiGroup->get(position = i))->getNumber()
  roiPosVec = sort(roiNumVec)
  ; and then do something like this...
  ;for i = 0, nObj-1 do begin
  ;  objPos = roiPosVec[i]
  ;  obj = (oGroupReference->get(position = objPos))->makePixelObjectInVoxel(/all)
  ;  ...
  ;endfor

    ; Get ROI surface meshes, repair and compute skeleton
  pVertices = ptrArr(nObj)
  pPolygons = ptrArr(nObj)
  tryMeshModelNames = ['_MeshFix_']
  whereObj = where(skeletonToComputeIndexes eq 1)
  s_getSavedModels, stack_tlb, o3DroiGroup, tryMeshModelNames, pVertices = pVerticesAux, pPolygons = pPolygonsAux, fOK = fOK, whereObj = whereObj
  
  ;mesh is already computed
  if (fOK eq 1) then begin
    for i = 0L, n_elements(whereObj)-1 do begin
      pVertices[whereObj[i]] = pVerticesAux[i]
      pPolygons[whereObj[i]] = pPolygonsAux[i]
    endfor
  ;mesh needs to be computead first 
  endif else begin
    for i = 0L, nObj-1 do begin
      if (skeletonToComputeIndexes[i] eq 1) then begin
        objPos = roiPosVec[i]
        obj = (o3DroiGroup->get(position = objPos))->makePixelObjectInVoxel(/all)
        shade_volume, obj.obj, 0, vertices, polygons, /low
        vertices[0,*] += (obj.minX - obj.pixelFrame)
        vertices[1,*] += (obj.minY - obj.pixelFrame)
        vertices[2,*] += (obj.minZ - obj.pixelFrame)
          ; Call to mesh_decimate in order to ensure a triangles-only mesh
        numtri = mesh_decimate(vertices, polygons, triList, vertices = triVertices)
        print, n_elements(triVertices)/3, ' vertices - ', n_elements(triList)/4, ' edges'
        repairSurfaceMesh, triVertices, triList, repairedVertices = repairedVertices, repairedPolygons = repairedPolygons,$
                                                 skipCheckTriangles = skipCheckTriangles, maxNumVertices = maxNumVertices,$
                                                 smoothingIterations = smoothingIterations, roiJoinDistance = roiJoinDistance,$
                                                 debug = debug
        pVertices[i] = ptr_new(repairedVertices, /no_copy)
        pPolygons[i] = ptr_new(repairedPolygons, /no_copy)
      endif
      vertices = -1
      polygons = -1
    endfor
  endelse

    ; Main loop for skeleton computation.
  for i = 0L, nObj-1 do begin
    objNumber = (o3DroiGroup->get(position = i))->getNumber()
   (o3DroiGroup->get(position = i))->getProperty, color = color, alpha_channel = alpha

    if (skeletonToComputeIndexes[i] eq 0) and (fFile[i] eq 1) then begin
      s_addGraphicalSkeletonStruct, oObjectModel = oObjectModel, VERTICES = *pVertices[i], CONNECTIVITY = *pEdges[i], skeletonType = fSkeletonTypes[i],$
                                    color = color, alpha = alpha, xcc = xCoord_conv, ycc = yCoord_conv, zcc = zCoord_conv, ZXRATIO = zxRatio,$
                                    name = strCompress('3DSkeletonMeshModel:' + string(i), /remove_all)
      continue
    endif

    if (skeletonToComputeIndexes[i] eq 1) then begin
      vertices  = double(*pVertices[i])
      nVertices = size(vertices, /dim)
      tsn       = sysTime(1)

        ; Call to Java object
      ;oIDLClassName    = 'IDLJAVAOBJECT$PROJECT_SKELETONITATIONFROMMESHCONTRACTION'
      ;oJavaClassName   = 'project.SkeletonitationFromMeshContraction'
      oIDLClassName    = 'IDLJAVAOBJECT$PROJECT_SkeletonExtrationFromMeshContraction'
      oJavaClassName   = 'main.SkeletonExtractionFromMeshContraction'
      oMeshContraction = obj_new(oIDLClassName, oJavaClassName, 1L)

      if ~obj_valid(oMeshContraction) or ~strCmp(obj_class(oMeshContraction), oIDLClassName, /fold_case) then begin
        print, '(ERR) creating ', oJavaClassName, '. oMeshContraction =', oMeshContraction
        continue
      endif

      kernelpath = s_getPathForProjectGeneric() + '_cu\kernels\'
      oMeshContraction->setup, transpose(vertices[0,*]),$
                               transpose(vertices[1,*]),$
                               transpose(vertices[2,*]),$
                               *pPolygons[i]
                               
        ; Perform skeletonization
      if (lastSkelStep ge 1) then begin
        ts2 = sysTime(1)
        oMeshContraction->geometryContraction, fContractionDebugLevel,$
                                               1L,$ ;flagCuda
                                               kernelpath
        t2 = sysTime(1) - ts2
      endif
      if (lastSkelStep ge 2) then begin
        ts3 = sysTime(1)
        oMeshContraction->connectivitySurgery, fConnSurgeryDebugLevel
        t3 = sysTime(1) - ts3
      endif
      if (lastSkelStep ge 3) then begin
        ts4 = sysTime(1)
        oMeshContraction->embeddingRefinement, fRefinementCheckRepresentativeness, refinementN, fRefinementDebugLevel
        t4 = sysTime(1) - ts4
      endif

      case lastSkelStep of
      1: begin
         vertices     = oMeshContraction->getVerticesCoordsIDL()
         connectivity = oMeshContraction->getTrianglesForIDL()
         skeletonMeshAssociation=[-1]
         endcase
      2: begin
         vertices     = oMeshContraction->getVerticesCoordsIDL()
         connectivity = oMeshContraction->getEdgesForIDL()
         skeletonMeshAssociation= oMeshContraction->getSkeletonNodesWithVerticesForIDL() 
         endcase
      3: begin
         vertices     = oMeshContraction->getNodesCoordsIDL()
         connectivity = oMeshContraction->getSegmentsForIDL()
         skeletonMeshAssociation= oMeshContraction->getSkeletonNodesWithVerticesForIDL()
         endcase
      else: begin
         print, 'Unrecognized skeletonization step... Ouch!'
         stop
         endcase
      endcase

      print, 'Skeleton computed for object ', objNumber
      s_addGraphicalSkeletonStruct, oObjectModel = oObjectModel, vertices = vertices, connectivity = connectivity, skeletonType = skeletonType,$
                                    xcc = yCoord_conv, ycc = yCoord_conv, zcc = zCoord_conv,$
                                    alpha = alpha, color = color, thick = thick, zxRatio = zxRatio,$
                                    name = strCompress('3DSkeletonMeshModel:' + string(objNumber), /remove_all)

      if (fSaveSkeletonInSavFile eq 1) then begin

          ; TODO factorize this and the piece from C_sROI3DGroupObj::getoACModelWithSaveRestore
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
        if (file_test(strCompress(modelPath), /DIRECTORY) eq 0b) then file_mkDir, strCompress(modelPath)

        roiModelFileName = strCompress(modelPath + strcompress(currROI3DGroupFileName + modelName + string(objNumber) + '.sav', /rem))
          ; TODO Variable renaming for save skeleton file...change "edges" for "connectivity"
        edges        = connectivity
        connectivity = -1
          ; Also include parameters of the skeleton computation
        savedLastSkelStep          = lastSkelStep
        savedContractionDebugLevel = fContractionDebugLevel
        savedConnSurgeryDebugLevel = fConnSurgeryDebugLevel
        savedRefinementDebugLevel  = fRefinementDebugLevel
        print, 'saving ', roiModelFileName
        ;mcerda: now saving skeleton - mesh association
        save, vertices, edges, savedLastSkelStep, savedContractionDebugLevel, savedConnSurgeryDebugLevel, $
          savedRefinementDebugLevel, skeletonMeshAssociation, fileName = roiModelFileName
        vertices = -1
        edges    = -1
      endif
      if obj_valid(oMeshContraction) then obj_destroy, oMeshContraction
    endif
  endfor
end


; zxFactor = xyzPixel[2] / xyzPixel[0]
pro s_addGraphicalSkeletonStruct, oObjectModel = oObjectModel, vertices = vertices, connectivity = connectivity, skeletonType = skeletonType,$
                                  name = name, alpha = alpha, xcc = xcc, ycc = ycc, zcc = zcc, color = color, thick = thick, zxRatio = zxRatio

  mySymbol = OBJ_NEW('IDLgrSymbol', 5, COLOR=[255,0,0])

  if ~keyword_set(zxRatio) then zxRatio = 1
  if ~keyword_set(thick)   then thick   = 1
  case skeletonType of
    0: oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = connectivity,$
                                                  ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                  xCoord_conv = xcc, yCoord_conv = ycc, zCoord_conv = zcc,$
                                                  name = name, uValue = 'ObjInOriginalPosition', symbol=mySymbol)
    1: begin
       connSize = size(connectivity)
         ; Add each segment of the skeleton individually
       for j = 0L, connSize[1]-1, 3 do begin 
         x1 = vertices[0, connectivity[j+1]]
         y1 = vertices[1, connectivity[j+1]]
         z1 = vertices[2, connectivity[j+1]] ;/ zxRatio
         x2 = vertices[0, connectivity[j+2]]
         y2 = vertices[1, connectivity[j+2]]
         z2 = vertices[2, connectivity[j+2]] ;/ zxRatio
         oObjectModel->add, obj_new('IDLgrPolyline', [[x1, y1, z1], [x2, y2, z2]], color = color, alpha_channel = alpha, thick = thick,$
                                                     xCoord_conv = xcc, yCoord_conv = ycc, zCoord_conv = zcc,$
                                                     name = strCompress(name + '_' + string(j), /remove_all), $
                                                     symbol=mySymbol, uValue = 'ObjInOriginalPosition')
       endfor
       endcase
    else: print, 'Unrecognized skeleton type. Expecting 0 or 1. Given: ', skeletonType, '. No model was added.'
  endcase
end


; getSavedModels
;
; Retrieves saved models models from .sav files (or the interactive window, someday ;), given a list of model names in order of preference.
;
; o3DroiGroup   Input ROI-group for whose ROI-objects the saved files will be loaded.
; tryModelNames Input list, ordered by priority, specifiying which models will be tried.
;               Example: ['_AC3D_Obj_', '_MeshFix'_]
; whereObj      An optional input vector containing specific ROI indices (positions) for file loading.
;               If not set, files will be loaded for all the ROIs in the input ROIgroup.
; fOK           If set, used as an output OK status flag (1-> success, 0 otherwise)
; fSilent       If set, impedes dialog windows from appearing.
;
; HISTORY
;  2012.05.29 - [JJ] Alpha version.
;  2013.06.05 - [JJ] release version. GRAPHIC MODELS LOOKUP NOT YET INCLUDED.
pro s_getSavedModels, stack_tlb, o3DroiGroup, tryModelNames, pVertices = pVertices, pPolygons = pPolygons, whereObj = whereObj, fOK = fOK, fSilent = fSilent

  nObj     = o3DroiGroup->count()
  whereObj = n_elements(whereObj) gt 0 ? (n_elements(whereObj) eq 1 ? [whereObj] : whereObj) : indGen(nObj)
  nMesh    = n_elements(whereObj)
  if (n_elements(tryModelNames) eq 1) then tryModelNames = [tryModelNames] ; Ensure array type

;  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
;  fFix    = 0b
;  fSmooth = 0b

;  widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
;    if obj_valid(stateObjWindow) then begin
;      case 1 of
;        obj_valid(*(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface AC Model')):$
;          dummy = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
;        obj_valid(*(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh-Fix Model')):$
;        begin
;          dummy = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh-Fix Model')
;          fFix  = 1b
;        end
;        obj_valid(*(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')):$
;        begin
;          dummy   = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
;          fFix    = 1b
;          fSmooth = 1b
;        end
;      endcase
;    endif
;  widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy

  t = 0
  while (t lt n_elements(tryModelNames)) do begin

    pVertices = ptrArr(nMesh)
    pPolygons = ptrArr(nMesh)

    ;print, 'Trying ' + tryModelNames[t]
    loadedRoisCount = 0
    ; First, try with graphic objects.
    ; TODO [JJ] Not yet... just go to the next attempt with saved objects.
    if (loadedRoisCount eq nMesh) then break

    ; Next, try with saved objects.
    loadedRoisCount = 0
    for i = 0L, nMesh-1 do begin
      roiNumber = (o3DroiGroup->get(position = whereObj[i]))->getNumber()
      o3DroiGroup->restoreSavedSurfaceModel, stack_tlb = stack_tlb, modelName = tryModelNames[t], roiNumber = roiNumber, pVertices = pVertAux, pPolygons = pPolyAux, fSilent = fSilent
      if ptr_valid(pVertAux) and ptr_valid(pPolyAux) then begin
        loadedRoisCount += 1
        pVertices[i] = pVertAux
        pPolygons[i] = pPolyAux
      endif
    endfor
    if (loadedRoisCount eq nMesh) then break

    t += 1
  endwhile

  fOK = loadedRoisCount eq nMesh

  ; If not OK, do the cleanup and return
  if (fOK ne 1) then begin
    for i = 0L, nMesh-1 do begin
      if ptr_valid(pVertices[i]) then ptr_free, pVertices[i]
      if ptr_valid(pPolygons[i]) then ptr_free, pPolygons[i]
    endfor
    print, 'Failed to recover existing/saved objects.'
  endif
end


pro s_getoSkeletonFromMeshRepair, oObjectModel = oObjectModel, stack_tlb = stack_tlb, o3DroiGroup = o3DroiGroup

  maxnvertices = 10000.0; max number of vertices in the mesh (10^5 is about 2 days for the skeleton)
  lastSkelStep = 3 ; step 1: contract, step2: collapse, step3: refinement
  minarea      = 0.0 ;area to consider a triangle "degenerated" using exact arithmetic (wired in the DLL).
  debug        = 0 ;save mesh file (.off) at each step for debug
  fUseSkeletonFromRepairedMesh = 1b
  fSaveSkeletonInSavFile = 1b

  o3DroiGroup->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

    ;compute mesh, repair and compute skeleton
  nObj = o3DroiGroup->count()
  nObj = 1
  for i = 0L, nObj-1 do begin

    print, 'Processing ROI=' + string(i)
    print, 'Shade Volume Call...'
    startTime = sysTime(1)

    obj = (o3DroiGroup->get(position = i))->makePixelObjectInVoxel(/all)
    shade_volume, obj.obj, 0, vertices, polygons, /low

    endTime = sysTime(1)
    print, 'Shade Volume time: ' + string(endTime - startTime) + 's'

    vertices[0,*] += (obj.minX - obj.pixelFrame)
    vertices[1,*] += (obj.minY - obj.pixelFrame)
    vertices[2,*] += (obj.minZ - obj.pixelFrame)

    vertices  = double(vertices)
    nvertices = size(vertices, /dim)

      ; Triangles-only mesh
    numtri = mesh_decimate(vertices, polygons, trilist, VERTICES = relvertices)
    print, 'Decimated mesh (to have triangles only)' + string(n_elements(polygons) - numtri)
    vertices = relvertices
    polygons = trilist

      ; Checking step to ensure that all the polygons are triangles.
    print, 'Verification of triangles-ONLY...'
    startTime = sysTime(1)
    verifyOnlyTriangles, vertices, polygons, verticesOUT, polygonsOUT
    vertices = verticesOUT
    polygons = polygonsOUT
    endTime = sysTime(1)
    print, 'Verification of triangles-ONLY time: ' + string(endTime - startTime) + 's'

    if debug eq 1.0 then begin
        ;write OFF (DEBUG)
      path = 'C:\RSI\'
      name = strCompress('Mesh' + string(i) + '.off', /rem)
      filename = strCompress(path + strcompress(name, /rem))
      s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename
    endif

;     print, 'Repairing mesh (FIRST CALL)...'
;     callMeshFixDLL, vertices, polygons, verticesOUT, polygonsOUT
;     vertices=verticesOUT
;     polygons=polygonsOUT

    if debug eq 1.0 then begin
        ;write OFF to manually check the repaired mesh (DEBUG)
      path = 'C:\RSI\'
      name = strCompress('Mesh' + string(i) + '_fixed.off', /rem)
      filename = strCompress(path + strcompress(name, /rem))
      s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename
    endif

    nverticesRep = size(vertices, /dim)

      ;Decimate mesh to speed-up computation (use with precaution!!!!)
    if (nverticesRep[1] gt maxnvertices) then begin

      percent = 1.0 + fix(100.0 * maxnvertices / nverticesRep[1]) ;1% is the min number of vertices
      print, 'keeping ' + string(fix(percent)) + ' % of vertices'
      numtri = mesh_decimate(vertices, polygons, trilist, PERCENT_VERTICES = percent) 
      ;vertices=relvertices
      polygons = trilist
      numtri = mesh_validate(vertices, polygons, /PACK_VERTICES)

      if debug eq 1.0 then begin
        ;write OFF to manually check the repaired mesh (DEBUG)
        path = 'C:\RSI\'
        name = strCompress('Mesh' + string(i) + '_fixed_decimated.off', /rem)
        filename = strCompress(path + strcompress(name, /rem))
        s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename
      endif

;        print, 'Repairing mesh (SECOND CALL)...'
;        callMeshFixDLL, vertices, polygons, verticesOUT, polygonsOUT
;        vertices=verticesOUT
;        polygons=polygonsOUT

      if debug eq 1.0 then begin
          ;write OFF to manually check the repaired mesh (DEBUG)
        path = 'C:\RSI\'
        name = strCompress('Mesh' + string(i) + '_fixed_decimated_fixed.off', /rem)
        filename = strCompress(path + strcompress(name, /rem))
        s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename
      endif
    endif

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;vertices, polygons= decimated and fixed, mesh

    ;TODO: compute skeleton
    ; Call to Java object
    tsn = sysTime(1)
    oIDLClassName    = "IDLJAVAOBJECT$PROJECT_SKELETONITATIONFROMMESHCONTRACTION"
    oJavaClassName   = "project.SkeletonitationFromMeshContraction"
    oMeshContraction = obj_new(oIDLClassName, oJavaClassName)

    if (obj_class(oMeshContraction) ne oIDLClassName) then begin
      print, '(ERR) creating ', oJavaClassName, '. oMeshContraction =', oMeshContraction
    endif else begin

      tn = sysTime(1) - tsn

        ;lectura obj reticulo_supder_fixed.obj
      oIDLClassNametest    = "IDLJAVAOBJECT$PROJECT_READOBJFILE"
      oJavaClassNametest   = "project.files.ReadObjFile"
      oMeshContractiontest = obj_new(oIDLClassNametest, oJavaClassNametest, "C:\Mesh14_totest")
      oMeshContractiontest->setPath, "C:\Mesh14_totest"
      oMeshContractiontest->read

      verticesx = oMeshContractiontest->getX() ;X
      verticesy = oMeshContractiontest->getY() ;y
      verticesz = oMeshContractiontest->getZ() ;Z
      polygon   = oMeshContractiontest->getTriangles() ;triangles index

      if(lastSkelStep ge 1) then begin
        ts2 = sysTime(1)
        oMeshContraction->geometryContraction, verticesx, verticesy, verticesz, polygon, 2L
        t2 = sysTime(1) - ts2
      endif
      if(lastSkelStep ge 2) then begin
        ts3 = sysTime(1)
        oMeshContraction->connectivitySurgery, 2L
        t3 = sysTime(1) - ts3
      endif
      if(lastSkelStep ge 3) then begin
        ts4 = sysTime(1)
        oMeshContraction->embeddingRefinement, 2, 3L, 2L
        t4 = sysTime(1) - ts4
      endif
      if(lastSkelStep eq 1) then begin
        verticesFinales  = oMeshContraction->getVerticesCoordsIDL()
        poligonosFinales = oMeshContraction->getTrianglesForIDL()
      endif
      if(lastSkelStep eq 2) then begin
        verticesFinales = oMeshContraction->getVerticesCoordsIDL()
        aristasFinales  = oMeshContraction->getEdgesForIDL()
      endif
      if(lastSkelStep eq 3) then begin
        verticesFinales = oMeshContraction->getNodesCoordsIDL()
        aristasFinales  = oMeshContraction->getSegmentsForIDL()
      endif

      a = oMeshContraction->skeletonNodesWithVerticesForIDL()
      print, 'Skeleton Computed'

      (o3DroiGroup->get(position = i))->getProperty, color = color, alpha_channel = alpha
      objNumber = (o3DroiGroup->get(position = i))->getNumber()
      ;xyzPixel = o3DroiGroup->getxyzSizePerPixel()
      ;zxRatio = xyzPixel[2] / xyzPixel[0]
      s_addGraphicalSkeletonStruct, oObjectModel = oObjectModel, vertices = *pVertices[i], connectivity = *pEdges[i], skeletonType = skeletonType,$
                                    color = color, alpha = alpha, xcc = xCoord_conv, ycc = yCoord_conv, zcc = zCoord_conv, $;ZXRATIO = zxRatio,$
                                    name = strCompress('3DSkeletonMeshModel:' + string(objNumber), /remove_all)

      if (lastSkelStep ge 2) then begin
        if (fSaveSkeletonInSavFile eq 1) then begin
            ; TODO names familiy for skeleton models... this is the first
          modelName = '_Skeleton_'

            ; TODO factorize this and the piece from C_sROI3DGroupObj::getoACModelWithSaveRestore,
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
            ; rename variable renaming
          vertices = verticesFinales
          edges = aristasFinales
          save, vertices, edges, fileName = roiModelFileName
          vertices = -1
          edges = -1
        endif
      endif
    endelse ;end valid java object
    obj_destroy, oMeshContraction
    obj_destroy, oMeshContractiontest

  endfor ;end foreach object in the
end


pro s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename, elementSize=elementSize

  if n_elements(elementSize) eq 0 then elementSize = 4
  
  sVertices = size(vertices, /dim)
  sPolygons = size(polygons, /dim)

  ;save mesh
  ;path = 'C:\RSI\'
  ;name = strCompress('Mesh'+string(i) + '.off', /rem)
  ;filename = strCompress(path + name, /rem)

  ;write input
  ;print, filename
  openW, unit, filename, /get_lun

  if elementSize eq 4 then begin
    printf, unit, 'OFF'
    printf, unit, strCompress(string(sVertices[1]) + ' ' + string(sPolygons[0]/elementSize) + ' 0')
    printf, unit, strTrim(vertices)
    printf, unit, strTrim(reform(polygons, elementSize, sPolygons[0]/elementSize))
    close, unit
  endif
  
  if elementSize eq 3 then begin
    printf, unit, 'SKEL'
    printf, unit, string(sVertices[1]) + ' ' + string(sPolygons[0]/elementSize)
    printf, unit, strcompress(string(vertices), /REMOVE_ALL)
    bloque=reform(polygons, elementSize, sPolygons[0]/elementSize)
    bloque[1:*,*]=bloque[1:*,*]-1L
    printf, unit, strcompress(string(bloque), /REMOVE_ALL) 
    close, unit
  endif

  free_lun, unit
end


pro s_getoSkeletonFromMeshRepair_readoff, filename, vertices, polygons

  ;name = strCompress('Mesh' + string(i) + '_fixed.off', /rem)
  ;filename = strCompress(path + name, /rem)

  numVertices  = 0.0d
  numFaces     = 0.0d
  ignoredLines = 1

  A = ''
  openR, 1, filename
  ;.off generated by meshfix have two empty lines
  for j = 0, ignoredLines-1 do readf, 1, A ;ignore line

  readf, 1, A
  print, A

  tempString  = STRSPLIT(A, /EXTRACT)
  numVertices = double(tempString[0])
  numFaces    = double(tempString[1])
  vertices    = MAKE_ARRAY(3, numVertices, /float, value = 0)

  for j = 0L, numVertices-1L do begin 
    ;Read a line of text: 
    readf, 1, A
    tempString = strSplit(A, /EXTRACT )
    vertices[0,j] = double(tempString[0])
    vertices[1,j] = double(tempString[1])
    vertices[2,j] = double(tempString[2])
  endfor

  polygons = MAKE_ARRAY(4L*numFaces,/long, Value = 0)
  for j = 0L, numFaces-1L do begin 
    ;Read a line of text: 
    readf, 1, A
    tempString = strSplit(A, /EXTRACT)                             
    polygons[j*4l + 0L] = double(tempString[0])
    polygons[j*4l + 1L] = double(tempString[1])
    polygons[j*4l + 2L] = double(tempString[2])
    polygons[j*4l + 3L] = double(tempString[3])
  endfor
  close, 1
  free_lun, 1
end


; Loads a SCIAN-Soft skeleton model from an .obj file
pro s_getoSkeletonFromMeshRepair_readSkeletonFromObj, filename, vertices, edges, debugPrint = debugPrint, makeSav = makeSav, nameSav = nameSav
  vertCount = 0.0d
  edgeCount = 0.0d
  vertices  = make_array(3, 1, /double, value = -1d)
  edges     = [-1L]
  line      = ''
  lineCount = 0L
  skipCount = 0L
  fPrint    = keyword_set(debugPrint)
  openR, 1, filename
  while ~eof(1) do begin
    readF, 1, line
    parts = strSplit(line, /extract)
    case 1 of
      strCmp(parts[0], 'v', /FOLD_CASE) eq 1: begin
        vertCount += 1
        newV = make_array(3, 1, /double, value = 0d)
        newV[0] = double(parts[1])
        newV[1] = double(parts[2])
        newV[2] = double(parts[3])
        if (fPrint eq 1) then print, 'adding vertex ', newV
        vertices = [[vertices], [newV]]
      endcase
      strCmp(parts[0], 'l', /FOLD_CASE) eq 1: begin
        edgeCount += 1
        newE = [2L, parts[1:2] - 1L]
        if (fPrint eq 1) then print, 'adding edge ', newE
        edges = [edges, newE]
      endcase
      else: skipCount += 1
    endcase
    lineCount += 1
  endwhile
  edges    = edges[1:*]
  vertices = vertices[*,1:*]
  if (fPrint eq 1) then print, lineCount, ' lines, ', vertCount, ' vertices, ', edgeCount, ' edges'
  if keyword_set(makeSav) then save, vertices, edges, fileName = nameSav
  close, 1
  free_lun, 1
end
