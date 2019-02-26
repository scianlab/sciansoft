; FASL get Balls Model
pro s_getoACSurfaceUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

  oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos

  xyzDim      = oGroupReference->getxyzDim()
  nObj        = oGroupReference->count()
  acModelName = '_undriftedAC3D_Obj_'

    ; Option to load saved 3D active contour models (if they exist). Be careful with saved models computed with who-knows? parameter values :)
  fUseSavedModels = 1b 
    ; Option to save computed 3D active contour models. If you want to record parameter values for later then...
  fSaveACModels = 1b
  fSaveACModels = fSaveACModels and ~fUseSavedModels ; avoid accidental overwriting of saved models, if fUseSavedModels is set to 1

  fFile = make_array(nObj, /byte, value = 0b)
  if (fUseSavedModels eq 1) then $
    oGroupReference->restoreSavedSurfaceModels, stack_tlb = stack_tlb, modelName = acModelName, pPolygons = pPolygons, pVertices = pVertices, fFile = fFile $
  else begin
    pVertices = ptrArr(nObj)
    pPolygons = ptrArr(nObj)
  endelse

  if (fsaveACModels eq 1) then begin
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
    acModelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]] + strcompress(acModelName + slash, /rem))
    if (file_test(strCompress(acModelPath),/DIRECTORY) eq 0b) then file_mkDir, strCompress(acModelPath)
  endif

    ; Check if a C_sActiveContour3D instance is required... the use of || skips unnecessary evaluations
  if (~fUseSavedModels || (max(fFile) eq 0)) then begin
    print, 'Creating C_sUnDriftedActiveContour3D instance'
    acIterations       = 5
    acAlpha            = 0.05
    acBeta             = 0.2
    acKappa            = 0.5
    acGamma            = 1.0
    acVFiterations     = 5
      ; vector field specific parameters (GVF/GGVF, EPGGVF)
    acGVFmu            = 0.05
    acEPGGVFstep       = 1
    acEPGGVFerror      = 1
    acEPGGVFnoiseCut   = 1
    acEPGGVFnoiseRange = 1
  
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
  
    volData = make_array(xyzDim, /byte, value = 0)
  
      ; An option to consider only the image intensities at the voxels of the initial ROI models, instead of the entire 3D image.
    fUseROIintOnly = 0b
    case fUseROIintOnly of
      1:    for i = 0, nObj-1      do volData[*((oGroupReference->get(position = i))->getpWherePoints())] = *((oGroupReference->get(position = i))->getpPointValues())
      else: for z = 0, xyzDim[2]-1 do volData[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
    endcase
  
    oAC3D = obj_new('C_sActiveContour3D', volData, iterations = acIterations, alpha = acAlpha, beta = acBeta, kappa = acKappa, gamma = acGamma, gvf_iterations = acVFiterations, mu = acGVFmu)
    volData = -1
      ; Select one of the multiple choices for vector field computation
    fVFmethod = 0
    case fVFmethod of
     0: oAC3D->calcGVF;, mu = acGVFmu
     1: oAC3D->calcGGVF;, mu = acGVFmu
     2: oAC3D->calcEPGGVF, noiseCut = acEPGGVFnoiseCut, noiseRange = acEPGGVFnoiseRange, error = acEPGGVFerror, step = acEPGGVFstep
     else: oAC3D->calcGVF, mu = acGVFmu
    end
  endif else print, '3DAC models loaded. No C_sActiveContour3D computation will be performed this time'

  for i = 0, nObj-1 do begin

     objNumber = (oGroupReference->get(position = i))->getNumber()

    if (fUseSavedModels and fFile[i]) then begin

      pRoiVertices = pVertices[i]
      pRoiPolygons = pPolygons[i]
      ; TODO include vert_color, opacVect

    endif else begin

      obj = (oGroupReference->get(position = i))->makePixelObjectInVoxel(/all)
      shade_volume, obj.obj, 0, vertices, polygons, /low
      vertices[0,*] += (obj.minX - obj.pixelFrame)
      vertices[1,*] += (obj.minY - obj.pixelFrame)
      vertices[2,*] += (obj.minZ - obj.pixelFrame)

      oAC3D->setContour, transpose(vertices[0,*]), transpose(vertices[1,*]), transpose(vertices[2,*]), polygons
      fAdjustOK = oAC3D->adjustContour()

      if (fAdjustOK eq 1) then begin
        pRoiVertices  = ptr_new(oAC3D->getCoords())
        pRoiPolygons  = ptr_new(oAC3D->getPolygonList())
        colorVect     = oAC3D->getCurv4()

        maxCV = max(colorVect, min = minCV)
        ;hist = histogram(colorVect, max = maxCV, min = minCV, nbins = 100, locations = curvValues)
        ;xhist = ((make_array(n_elements(hist), /float, /index)) / 100.) * (maxCV-minCV) + minCV
        ;live_plot, hist, independent = xhist
        ;print, 'radius r - calc r - sd (calc r) - curvature k - calc k - sd(calc k) - surface s - calc s'
        print, tPos + 1,      (moment(1./colorVect))[0], sqrt(((1./colorVect))[1]),$
               1./(tPos + 1), (moment(colorVect))[0],    sqrt((moment(colorVect))[1]),$
               4. * !pi * (tPos + 1)^2, (oGroupReference->get(position = i))->getObjectSurfaceFromPolygons(polygons = polygons, vertices = vertices)

        if (maxCV gt minCV) then colorVect -= minCV

        vert_color = make_array(4, (size(vertices, /dim))[1], /byte)
        rgb_table0 = bytArr(256,3) + transpose((*(oGroupReference->getpVolState())).rgbValues[0,*,*])
        opacVect = oGroupReference->getOpacVect('1st Volume Opacity')

        vert_color[0,*] = (rgb_table0[*,0])[colorVect]
        vert_color[1,*] = (rgb_table0[*,1])[colorVect]
        vert_color[2,*] = (rgb_table0[*,2])[colorVect]
        vert_color[3,*] = opacVect[colorVect]

      endif else print, 'Error adjusting contour for object number ', objNumber

      if (fSaveACModels eq 1) and (fAdjustOK) then begin
        roiACfileName = strCompress(acModelPath + strcompress(currROI3DGroupFileName + acModelName + string(objNumber) + '.sav', /rem))
        vertices = *pRoiVertices
        polygons = *pRoiPolygons
        save, vertices, polygons, fileName = roiACfileName
        ;save, vertices, polygons, opacVect, vert_color, fileName = roiACfileName
      endif
    endelse

    (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha
    oObjectModel->add, obj_new('IDLgrPolygon', data = *pRoiVertices, poly = *pRoiPolygons,$
                                               ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                               name = strCompress('3DundriftedACModel:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition')
  endfor
  if obj_valid(oAC3D) then obj_destroy, oAC3D
      
      oObjectModel->getproperty, TRANSFORM = matrixModel
      matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
      ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
      oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix

end
