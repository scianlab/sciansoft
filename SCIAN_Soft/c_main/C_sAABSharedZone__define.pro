; C_sAABSharedZone
;
; Aims to encapsulate image and shape elements involved in a "shared" boundary
; zone, for segmenting multiple objects that can touch or be located very close
; to each other.
;
; History
;   2013.08.05  Prototype version. JJara.

function C_sAABSharedZone::getVerboseMode
  return, self.fVerbose
end


pro C_sAABSharedZone::setVerboseMode, verboseMode
  self.fVerbose = verboseMode ne 0
end


function C_sAABSharedZone::getAdjustedBoundary
  out = -1
  return, out
end


; THIS IS AN UNTESTED METHOD!
function C_sAABSharedZone::computeImageFilter, filterName,$
                                               paramNames  = paramNames,$
                                               paramValues = paramValues,$
                                               paramActive = paramActive,$
                                               stack_tlb   = stack_tlb,$
                                               nDim        = nDim,$
                                               fGetSliceFrom3Dregion = fGetSliceFrom3Dregion,$
                                               zSlicePos   = zSlicePos
  nDim = keyword_set(nDim) ? nDim : 2

  if (nDim eq 2) and keyword_set(fGetSliceFrom3Dregion) then begin
    if self.fVerbose then print, 'Getting 2D slice from 3D region... NOT YET'
    stop
  endif

  imageAtt = self->getAttributeByName('image', position = imageAttPos)
  if (imageAttPos eq -1) then begin
    print, 'ERROR: image attribute not found'
    stop
    return, -1
  endif
  subImage = *(imageAtt.pData)

  oFilter            = obj_new(filterName)
  filterpParamStruct = oFilter->getpParamStruct()
  nParamsFilter      = n_elements(*((*filterpParamStruct).pNames))

  filterClassName = 'C_sImageFilter'
  nParams         = n_elements(paramNames)
  paramPositions  = nParams gt 0 ? intArr(nParams) : indGen(nParamsFilter)

  if (nParams gt 0) then begin

    for i = 0, nParams-1 do $
      paramPositions[i] = (where((*((*filterpParamStruct).pNames)) eq paramNames[i]))[0]

    nLimit = nParams < nParamsFilter ; paranoid checking
    if (nParams gt nParamsFilter) then begin
      print, 'number of input parameters (', nParams, ') greater than number of filter parameters (', nParamsFilter, ') Ouch!'
      stop
    endif

    for i = 0, nLimit-1 do begin
      paramVal = paramValues[i]
      paramVal >= (*((*filterpParamStruct).pMin))[paramPositions[i]]
      paramVal <= (*((*filterpParamStruct).pMax))[paramPositions[i]]
      if self.fVerbose then $
        print, 'Setting parameter ', (*((*filterpParamStruct).pNames))[paramPositions[i]], ' with value ', paramVal, '(original : ', paramValues[i], ')'
      (*((*filterpParamStruct).pValues))[paramPositions[i]] = paramVal
    endfor
  endif

  filterType = oFilter->getImageFilterType()
  case filterType of
    'Single_Image_Filter_Method'  : filteredSubImage = oFilter->apply(image = subImage)
    'Multiple_Image_Filter_Method': filteredSubImage = oFilter->apply(image = subImage,$
                                                                      selectedStackObject = selectedStackObject,$
                                                                      stack_tlb           = stack_tlb,$
                                                                      tPos    = tPos,$
                                                                      chPos   = chPos,$
                                                                      zPos    = zPos,$
                                                                      clusPos = clusPos,$
                                                                      segPos  = segPos)
    else: begin
      print, 'Unrecognized image filter type: ', filterType, '. Using original image...'
      filteredSubImage = image
      stop
    endcase
  endcase

end


function C_sAABSharedZone::getDimension
  nAtt = self.nAttrib
  if (nAtt lt 1) then begin
    if (self.fVerbose ne 0) then print, 'C_sAABSharedZone::getDimension - No attributes to get a dimension'
    return, -1
  endif
  nDim = -1
  for i = 0, nAtt-1 do begin
    curDim = 2
    if ~ptr_valid(((*(self.pAttrib)[0])[i])) $
    then print, 'WARNING: trying to get type from a NULL attribute at position ', i $
    else if (self.fVerbose ne 0) then $
      print, 'attribute ', (*((*(self.pAttrib)[0])[i])).name, ' has type ', (*((*(self.pAttrib)[0])[i])).type
    if ~ptr_valid(((*(self.pAttrib)[0])[i])) then return, -1
    case (*((*(self.pAttrib)[0])[i])).type of
      '2D': curDim = 2
      '3D': curDim = 3
      else: curDim = 2
    endcase
    nDim >= curDim
  endfor
  return, nDim
end


pro C_sAABSharedZone::mergeVertexListsForPair

  roiIndexVectorStruct = self->getAttributeByName('roi_index_vector')
  roiIndexVector       = *(roiIndexVectorStruct.pData[0])
  nRois = n_elements(*roiIndexVectorStruct.pData[0])
  if (nRois gt 2) then begin
    print, 'Not yet for ', nRois, ' rois'
    stop
  endif

  roiVertexIndicesStruct = self->getAttributeByName('roi_vertex_indices')
  roiVertexStruct = self->getAttributeByName('roi_vertex_list')
  pairedaabTypeStruct = self->getAttributeByName('paired_aab_type_list')
  aabPairID = 0
  nMergedAABs = n_elements(*pairedaabTypeStruct.pData[aabPairID])

  ;pSortedVertexIndexList = ptrArr(nRois)
  ;for i = 0L, nRois-1 do $
  ;  pSortedVertexIndexList[i] = ptr_new([-1])
  mergeStructName = 'roi_merged_vertex_indices'
  structType = '2D'
  aabType = 'merge'
  roiMergeVertexIndicesStruct = self->getAttributeByName(mergeStructName)
  fMergedVertices = (size(roiMergeVertexIndicesStruct, /type) eq 8)
  case fMergedVertices of
  0: begin ; case for 2 rois which have two vertex lists to be merged
      for i = 0L, nMergedAABs-2 do begin
        roi1index = (*roiIndexVectorStruct.pData[aabPairID])[0]
        roi2index = (*roiIndexVectorStruct.pData[aabPairID])[1]
        roi1indicesA = *roiVertexIndicesStruct.pData[2*i]
        roi2indicesA = *roiVertexIndicesStruct.pData[2*i+1]
        roi1indicesB = *roiVertexIndicesStruct.pData[2*(i+1)]
        roi2indicesB = *roiVertexIndicesStruct.pData[2*(i+1)+1]
        ;check if the vertex index lists are consecutive
        roi1mergedIndices = [roi1indicesA, roi1indicesB]
        roi1mergedIndices = roi1mergedIndices[uniq(roi1mergedIndices, sort(roi1mergedIndices))]
        roi2mergedIndices = [roi2indicesA, roi2indicesB]
        roi2mergedIndices = roi2mergedIndices[uniq(roi2mergedIndices, sort(roi2mergedIndices))]
        roi1runs = makeRunFromCorrelativeValues(roi1mergedIndices)
        roi2runs = makeRunFromCorrelativeValues(roi2mergedIndices)
        nRunsRoi1 = n_elements(roi1runs)/2
        nRunsRoi2 = n_elements(roi2runs)/2

        if (nRunsRoi1 eq 1) and (nRunsRoi2 eq 1) then begin
          pData = ptrArr(2)
          pData[0] = ptr_new(roi1runs)
          pData[1] = ptr_new(roi2runs)
          roiIndexVector = [roi1index, roi2index]
        ;TODO special case when ((nRunsRoi1 eq 1) and (nRunsRoi2 ne 1)) or ((nRunsRoi1 ne 1) and (nRunsRoi2 eq 1))
        endif else begin
          pData = ptrArr(4)
          pData[0] = ptr_new(roi1indicesA)
          pData[1] = ptr_new(roi2indicesA)
          pData[2] = ptr_new(roi1indicesB)
          pData[3] = ptr_new(roi2indicesB)
          roiIndexVector = [roi1index, roi2index, roi1index, roi2index]
        endelse
        att = self->createAttributeStruct(mergeStructName, structType, aabType, pData, roiIndices = roiIndexVector)
        self->appendAttributeStruct, att
      endfor
     endcase
  else: begin
     stop
     endcase
  endcase
  stop
end


; METHOD UNDER CONSTRUCTION
function C_sAABSharedZone::computeAABregion, fGetSliceFrom3Dregion = fGetSliceFrom3Dregion, zSlicePos = zSlicePos, cornerPoints = cornerPoints

  ;fPreconditionsOK = self->checkPreconditions()
  fPreconditionsOK = 0
  if (fPreconditionsOK ne 0) then begin
    if self.fVerbose then print, 'ERROR checking pre-conditions. AAB adjustment will not be performed'
    return, -1
  endif
  nDim = self->getDimension()
  case nDim of
    2: return, self->computeAABregion_impl2D(cornerPoints = cornerPoints)
    3: return, keyword_set(fGetSliceFrom3Dregion) ? self->computeAABregion_impl2Dfrom3D(zSlicePos = zSlicePos) $ 
                                                  : self->computeAABregion_impl3D()
    else: begin
       print, 'Unrecognized number of dimensions (', nDim, ')'
       return, -1
       endcase
  endcase
end


; TODO check (& update if needed) against current attribute settings
function C_sAABSharedZone::checkPreconditions

  fStatus = 1

  roiList  = self->getAttributeByName('ROI_list')
  listType = size(roiList, /TYPE)

  if (listType ne 8) then begin ; 8 stands for IDL's STRUCT type
    print, 'Precondition error: No ROIs found for AAB region computation.'
    fStatus = 0
  endif

  nRois = n_elements(*(roiList.pData))
  if (nRois lt 2) then begin
    print, 'Precondition error: not enough ROIs in the ROI_list attribute structure (bad initialization?)'
    stop ; Not critical. It can be by-passed by now.
    fStatus = 0
  endif

  roiRuns  = self->getAttributeByName('ROI_vertexRuns')
  runsType = size(roiRuns, /TYPE)

  if (runsType ne 8) then begin ; 8 stands for IDL's STRUCT type
    print, 'Precondition error: no ROI runs found for AAB region computation.'
    fStatus = 0
  endif

  nRuns = n_elements(*(roiRuns.pData))
  if (nRuns lt 2) then begin
    print, 'Precondition error: not enough ROI-runs in the ROI_vertexRuns attribute structure (bad initialization?)'
    fStatus = 0
  endif

  return, fStatus
end


; TODO JJW Check4Delete
; Determines the number of paired AAB objects contained as attributes
function C_sAABSharedZone::getNumberOfPairedAABs, cornerAABindices = cornerAABindices
  if (self.nAttrib eq 0) then return, 0
  nPairedAABs = 0
  if (n_elements(cornerAABindices) eq 0) then begin
    cornerAABindices = [-1]
    for i = 0, self.nAttrib-1 do begin
      attStruct = (*(self.pAttrib)[0])[i]
      print, 'C_sAABSharedZone::getNumberOfPairedAABs... at postition ', i, ', type is :', (*attStruct).aabType
      ; careful with nulls... in development
      if ((*attStruct).aabType eq 'corner') then begin
        cornerAABindices = [cornerAABindices, i]
      endif
    endfor
  endif
  stop
  nCornerAABs = n_elements(cornerAABindices)
  if (nCornerAABs eq 1) then return, 0
  for i = 1, nCornerAABs-1 do $
    nPairedAABs += n_elements(*(*(((*(self.pAttrib)[0])[cornerAABindices[i]]->getAttributeByName('paired_AABs')).pData)[0]))
  return, nPairedAABs
end


function C_sAABSharedZone::getNumberOfUniqueRois, roiIndices = roiIndices

  if (self.nAttrib eq 0) then return, 0
  roiIndexVectorAttrib = self->getAttributeByName('roi_index_vector')
  roiIndexVector = **roiIndexVectorAttrib.pData[0]

;  Some nasty lookup imcomplete method... and probably not needed :P
;  roiIndices = [-1]
;  for i = 0, self.nAttrib-1 do begin
;    attStruct = *((*(self.pAttrib)[0])[i])
;    if (attStruct.name eq 'roi_index_vector') $
;    then roiIndexvector = **attStruct.pData[0] $
;    else begin
;      if ptr_valid(*attStruct.pRoiIndices) $
;      then roiIndexVector = *(*attStruct.pRoiIndices) $
;      else begin
;        stop
;      endelse
;    endelse
;    stop
;    roiIndices = [roiIndices, roiIndexVector]
;  endfor
;  roiIndices = roiIndices[uniq(roiIndices, sort(roiIndices))]

  roiIndices = roiIndexVector[uniq(roiIndexVector, sort(roiIndexVector))]
  nRois      = n_elements(roiIndices)
  return, nRois

end


pro computeSeparationSVM, p1x, p1y, p2x, p2y, borderX = borderX, borderY = borderY, dataOut = dataOut, fDebug = fDebug

  ;http://svmlight.joachims.org/
  kernelType = 1
  kernelPolyDegree = 5 ;9
  kernelRBFgamma   = 1.0d
  kernelCoefLin    = 1.0d
  kernelCoefConst  = 1.0d
  ;SVM param C...-> recommended  default... [avg. x*x]^-1
  param_SVM_C = ((kernelType eq 0) or ((kernelType eq 1) and (kernelPolyDegree eq 1))) ? 0.0d : 2.0d
  ;param_SVM_C = 1 / min([mean(p1x*p1x), mean(p2x*p2x), mean(p1y*p1y), mean(p2y*p2y)])

  s_SVMnormalizeXYpoints, p1x, p1y, p2x, p2y, p1xOut = p1x_, p1yOut = p1y_, p2xOut = p2x_, p2yOut = p2y_, /fKeepRatio
  nDivisions = max(n_elements(p1x)) > max(n_elements(p2x)) ;256 ; size of test image nDivisions * nDivisions
  pDomain    = make2DsvmTestDomain(p1x_, p1y_, p2x_, p2y_, nDivisions = nDivisions)
  pDomainOri = make2DsvmTestDomain(p1x, p1y, p2x, p2y, nDivisions = nDivisions)

  f3 = computeSVMseparationFor2Dpoints(p1x_, p1y_, p2x_, p2y_, $                       ; input ROI elements
                                       kernelType, suppVectorsOut, suppVectorsIndexOut, signedAlphas, bOut,$ ; input separation calc. parameters
                                       krnlPolyDegree = kernelPolyDegree, $
                                       krnlCoefConst  = kernelCoefConst, $
                                       krnlRBFgamma   = kernelRBFgamma, $
                                       krnlCoefLin    = kernelCoefLin, $
                                       param_SVM_C    = param_SVM_C, $
                                       pDomainX       = pDomain[0], pDomainY = pDomain[1], $ ; input x/y coords. for evaluating separation
                                       dataOut        = dataOut, $                           ; output separation values for x/y coords
                                       fUseLibSVM     = fUseLibSVM)
  if keyword_set(fDebug) then print, 'result from this call:', f3

  contour, dataOut, path_xy = borderOut, levels = [0.0], closed = 0, /PATH_DATA_COORDS
  if n_elements(borderOut) eq 0 then stop
  borderX = interpolate(*pDomainOri[0], borderOut[0,*], borderOut[1,*], cubic = -0.5)
  borderY = interpolate(*pDomainOri[1], borderOut[0,*], borderOut[1,*], cubic = -0.5)
end


function C_sAABSharedZone::adjustCorner_impl2D_new, fDebug = fDebug

  ; 1. Pre-conditions check
  attPairedAABs = self->getAttributeByName('corner_aab_list')
  pairedAABs    = *(*(attPairedAABs.pData[0]))
  nPairedAABs   = n_elements(pairedAABs)
  if (nPairedAABs lt 2) then stop ; Something did not go as expected... call jjara.

  ; 2. Process distance map
  distMapAtt = self->getAttributeByName('corner_roi_distance_map_patch')
  distMap  = *(*(distMapAtt.pData[0]))
  distMapSize = size(distMap, /dim)

  minDistSize = 10 ; Minimum size required for the watershed function.
  if (distMapSize[0] lt minDistSize) or (distMapSize[1] lt minDistSize) then begin
    cornerXYcoords = [distMapSize[0]/2, distMapSize[1]/2]
    countJunctions = 1
  endif else begin
    boundMap = watershed(distMap, connectivity = 8, nRegions = nRoisWatershed, /long)
    wh0 = where(boundMap eq 0, count0, complement = whNot0, ncomplement = countNot0)
    boundMap[wh0]    = 1
    boundMap[whNot0] = 0

    minMapSize = 5 ; As per IDL thinning requirements, check and expand boundary map.
    boundMapSize = size(boundMap, /dim)
    deltaMapSize = min(boundMapSize - minMapSize)
    if (deltaMapSize lt 0) then begin
      boundMapExpanded = s_expand_mirror(boundMap, -deltaMapSize)
      junctMap = thin(boundMapExpanded, /neighbor_count) ; Collect pints with value >=4
      junctMap = junctMap[-deltaMapSize : boundMapSize[0]-deltaMapSize-1, -deltaMapSize : boundMapSize[1]-deltaMapSize-1]
    endif else junctMap = thin(boundMap, /neighbor_count) ; Collect pints with value >=4

    whJunctions = where(junctMap gt 3, countJunctions)
    if (whJunctions[0] eq -1) then begin
      print, 'No junction points were found in distance map (number of watershed ROIs = ', nRoisWatershed,'). Using box center...'
      mapSize = size(boundMap, /dimensions)
      cornerXYcoords = mapSize / 2.0
      countJunctions = 1
    endif else cornerXYcoords = array_indices(junctMap, whJunctions)

  endelse

  case countJunctions of
    0: stop
    1: goto, f
    else: begin
      print, countJunctions, ' candidate corner points found'
      stop
    endcase
  endcase

  ; Convert to full image coordinates...
f:attBoundingBox = self->getAttributeByName('corner_bounding_box')
  boundingBox = *(*(attBoundingBox.pData[0]))
  cornerXYcoords[0] += floor(boundingBox[0])
  cornerXYcoords[1] += floor(boundingBox[1])

  return, cornerXYcoords

  nRois = self->getNumberOfUniqueRois(roiIndices = roiIndicesUniq)
  pairedRoiIndices   = uIntArr(2, nPairedAABs)
  pairedRoiIndicesPr = uIntArr(2)
  roiPairMatchVector = uIntArr(nPairedAABs)
  roiOutMatchVector  = uIntArr(nRois)

  pLinesArray = ptrArr(1)
  ; save x|y coords

  cornerX = -1
  cornerY = -1
  cornerBoxXMin = cornerX - separationDist
  cornerBoxXMax = cornerX + separationDist
  cornerBoxYMin = cornerY - separationDist
  cornerBoxYMax = cornerY + separationDist
  cornerBoundingBox = [cornerBoxXMin, cornerBoxYMin, cornerBoxXMax, cornerBoxYMax]
  cornerBoxX = [cornerBoxXMin, cornerBoxXMax, cornerBoxXMax, cornerBoxXMin]
  cornerBoxY = [cornerBoxYMin, cornerBoxYMin, cornerBoxYMax, cornerBoxYMax]

  attDistMap = self->getAttributeByName('corner_roi_distance_map_patch')
  distMapPatch  = *(attDistMap.pData[0])
  boundMapPatch = watershed(distMapPatch, connectivity = watershedConn)
  wh0 = where(boundMapPatch eq 0, count0, complement = whNot0, ncomplement = countNot0)
  boundMapPatch[wh0]    = 1
  boundMapPatch[whNot0] = 0
  junctMapPatch = thin(boundMapPatch, /neighbor_count) ; Collect pints with value >=4

end


; Corner adjustment method.
function C_sAABSharedZone::adjustCorner_impl2D, separationDist = separationDist, fDebug = fDebug
stop
  attPairedAABs = self->getAttributeByName('paired_AABs')
  pairedAABs    = *(*(attPairedAABs.pData[0]))
  nPairedAABs   = n_elements(pairedAABs)
  if (nPairedAABs lt 2) then stop ; Something did not go as expected... call me.
  nRois = self->getNumberOfUniqueRois(roiIndices = roiIndicesUniq)
  pairedRoiIndices   = uIntArr(2, nPairedAABs)
  pairedRoiIndicesPr = uIntArr(2)
  roiPairMatchVector = uIntArr(nPairedAABs)
  roiOutMatchVector  = uIntArr(nRois)

  currPairNum = 1
  for i = 0, nPairedAABs-1 do begin
    attRoiIndexVec = pairedAABs[i]->getAttributeByName('roi_index_vector')
    pairedRoiIndices[*, i] = **(attRoiIndexVec.pData[0])
    if (i eq 0) then begin
      roiPairMatchVector[i] = 0
      whRoi1 = where(roiIndicesUniq eq pairedRoiIndices[0, i])
      whRoi2 = where(roiIndicesUniq eq pairedRoiIndices[1, i])
      roiOutMatchVector[whRoi1] = 0
      roiOutMatchVector[whRoi2] = 0
      pairedRoiIndicesPr = pairedRoiIndices[*, i]
    endif else begin
      pairNum  = currPairNum
      fOK      = 0
      j        = 0
      fNewPair = 1
      while ~fOK do begin
        if ((pairedRoiIndices[0,j] eq pairedRoiIndices[0,i]) and (pairedRoiIndices[1,j] eq pairedRoiIndices[1,i])) $
        or ((pairedRoiIndices[0,j] eq pairedRoiIndices[1,i]) and (pairedRoiIndices[1,j] eq pairedRoiIndices[0,i])) $
        then begin
          pairNum  = j
          fOK      = 1
          fNewPair = 0
        endif else begin
          j += 1
          if (j eq i) then fOK = 1
        endelse
        if (j eq i) then begin
          pairNum = currPairNum
          currPairNum += 1
        endif
      endwhile
      if fNewPair then pairedRoiIndicesPr = [pairedRoiIndicesPr, pairedRoiIndices[*,i]]
      roiPairMatchVector[i] = pairNum
      whRoi1 = where(roiIndicesUniq eq pairedRoiIndices[0, i])
      whRoi2 = where(roiIndicesUniq eq pairedRoiIndices[1, i])
      roiOutMatchVector[whRoi1] = pairNum
      roiOutMatchVector[whRoi2] = pairNum
    endelse
  endfor
  roiPairCountVector = histogram(roiPairMatchVector, min = 0, max = currPairNum-1)

  self->setOutputsNum, 2*currPairNum + 1 ; One output for the corner point, the rest for the separation lines.
  fPlotFigure  = 1
  fDebug       = 1
  attImage     = self->getAttributeByName('image')
  imagePatch   = *(*(attImage.pData[0]))
  ;attImageFull = self->getAttributeByName('image_feature')
  ;imageFull    = *(*(attImage.pData[0]))
  ;imgageFullSz = size(imageFull, /dim)
  pBordersOut  = ptrArr(currPairNum+1)
  pRoiVertexIndices = ptrArr(currPairNum * 2)

  if (fPlotFigure gt 0) then begin
    winIdCorner    = 10
    sizeImagePatch = size(imagePatch, /dim)
    winFactor      = 4
    window, winIdCorner, xSize = winFactor * sizeImagePatch[0], ySize = winFactor * sizeImagePatch[1]
    tvscl, congrid(imagePatch, winFactor * sizeImagePatch[0], winFactor * sizeImagePatch[1])
  endif

  boundingBoxOutR = [0.0, 0.0, 0.0, 0.0]

  for i = 0ul, currPairNum-1 do begin
    whPairI    = where(roiPairMatchVector eq i)
    roiIndices = pairedRoiIndicesPr[2*i:2*i+1]

    if (roiPairCountVector[i] eq 1) then begin
      roiIndexVecAtt = pairedAABs[whPairI[0]]->getAttributeByName('roi_index_vector')
      roiIndicesAAB  = *(*(roiIndexVecAtt.pData)[0])
      fFlipIndices   = roiIndicesAAB[0] ne roiIndices[0]
      pos1 = fFlipIndices ? 2 : 0
      pos2 = fFlipIndices ? 0 : 2
      roiVerticesAtt = pairedAABs[whPairI[0]]->getAttributeByName('roi_vertex_list') ; 2 x n_vertices
      p1x = (*((*(roiVerticesAtt.pData))[pos1])) & p1y = (*((*(roiVerticesAtt.pData))[pos1+1]))
      p2x = (*((*(roiVerticesAtt.pData))[pos2])) & p2y = (*((*(roiVerticesAtt.pData))[pos2+1]))
        ; Alternate route. VC did a quick try for interior + boundary points... didn't work :P
        ;xMax = 398 & yMax = 398
        ;pp1 = getPolyBoundaryAndInteriorPixelPoints( *((*((*(roiVerticesAtt.pData))[0]))[0]), *((*((*(roiVerticesAtt.pData))[0]))[1]), xMax, yMax, winId = 5)
        ;p1x = pp1[*,0] & p1y = pp1[*,1]
        ;pp2 = getPolyBoundaryAndInteriorPixelPoints( *((*((*(roiVerticesAtt.pData))[1]))[0]), *((*((*(roiVerticesAtt.pData))[1]))[1]), xMax, yMax, winId = 5)
        ;p2x = pp2[*,0] & p2y = pp2[*,1]
      roiVertIdxAtt = pairedAABs[whPairI[0]]->getAttributeByName('roi_vertex_indices')
      pRoiVertexIndices[2*i]   = ptr_new((*((*(roiVertIdxAtt.pData))[0])))
      pRoiVertexIndices[2*i+1] = ptr_new((*((*(roiVertIdxAtt.pData))[1])))
    endif else begin

      p1x = [0] & p1y = [0]
      p2x = [0] & p2y = [0]
      v1  = [0] & v2  = [0]
      for j = 0, roiPairCountVector[i]-1 do begin

        roiIndexVecAtt = pairedAABs[whPairI[j]]->getAttributeByName('roi_index_vector')
        roiIndicesAAB  = *(*(roiIndexVecAtt.pData)[0])
        fFlipIndices   = roiIndicesAAB[0] ne roiIndices[0]
        roiVerticesAtt = pairedAABs[whPairI[j]]->getAttributeByName('roi_vertex_list') ; 2 x n_vertices
        pos1 = fFlipIndices ? 2 : 0
        pos2 = fFlipIndices ? 0 : 2
        x1 = (*((*(roiVerticesAtt.pData))[pos1])) & y1 = (*((*(roiVerticesAtt.pData))[pos1+1]))
        x2 = (*((*(roiVerticesAtt.pData))[pos2])) & y2 = (*((*(roiVerticesAtt.pData))[pos2+1]))
        p1x = [p1x, x1] & p1y = [p1y, y2]
        p2x = [p2x, x2] & p2y = [p2y, y2]

        roiVertIdxAtt = pairedAABs[whPairI[j]]->getAttributeByName('roi_vertex_indices')
        if fFlipIndices then begin
          v1 = [v1, (*((*(roiVertIdxAtt.pData))[1]))]
          v2 = [v2, (*((*(roiVertIdxAtt.pData))[0]))]
        endif else begin
          v1 = [v1, (*((*(roiVertIdxAtt.pData))[0]))]
          v2 = [v2, (*((*(roiVertIdxAtt.pData))[1]))]
        endelse
      endfor

      p1x = p1x[1:*] & p1y = p1y[1:*]
      p2x = p2x[1:*] & p2y = p2y[1:*]
      v1  = v1[1:*]  & v2  = v2[1:*]
      u1  = uniq(v1, sort(v1))
      u2  = uniq(v2, sort(v2))
      pRoiVertexIndices[2*i]   = ptr_new(v1[u1])
      pRoiVertexIndices[2*i+1] = ptr_new(v2[u2])
      p1x = p1x[u1] & p1y = p1y[u1]
      p2x = p2x[u2] & p2y = p2y[u2]
    endelse

    if (n_elements(p1x) lt 2) or (n_elements(p2x) lt 2) then begin
      stop
    endif else begin ; p1 or p2 with 2 or more points

    computeSeparationSVM, p1x, p1y, p2x, p2y, borderX = borderX, borderY = borderY, dataOut = dataOut

    pBordersOut[i+1] = ptr_new([borderX, borderY]) ; Save at position i+1 because 0 is reserved for the corner point.

    minXbox = min(borderX, max = maxXbox)
    minYbox = min(borderY, max = maxYbox)
    if (i eq 0) then begin
      boundingBoxOutR = [minXbox, minYbox, maxXbox, maxYbox]
    endif else begin
      if (minXbox lt boundingBoxOutR[0]) then boundingBoxOutR[0] = minXbox
      if (minYbox lt boundingBoxOutR[1]) then boundingBoxOutR[1] = minYbox
      if (maxXbox gt boundingBoxOutR[2]) then boundingBoxOutR[2] = maxXbox
      if (maxYbox gt boundingBoxOutR[3]) then boundingBoxOutR[3] = maxYbox
    endelse

    if (fPlotFigure gt 0) then begin
      wSet, winIdCorner
      if (i eq 0) $
      then plot, borderX, borderY, xRange = [0, sizeImagePatch[0]-1], yRange = [0, sizeImagePatch[1]-1], xMargin = [0, 0], yMargin = [0, 0], xTicks = 1, yTicks = 1, xStyle = 1, yStyle = 1, /noErase $
      else oPlot, borderX, borderY
    endif
stop
    if (fDebug gt 0) and (fPlotFigure gt 0) then begin
      minxOri = min(p1x, max = max1xOri) < min(p2x, max = max2xOri) & maxxOri = max1xOri > max2xOri
      minyOri = min(p1y, max = max1yOri) < min(p2y, max = max2yOri) & maxyOri = max1yOri > max2yOri
      makeSeparationPlot, p1x, p1y, p2x, p2y, dataOut,$
                          minx = minxOri-0.5, maxx = maxxOri+0.5,$
                          miny = minyOri-0.5, maxy = maxyOri+0.5,$
                          window_title = 'AAB line adjustment: SVM Separation Plot for ROIs ' + string(roiIndicesAAB[0]) + '-' + string(roiIndicesAAB[1])
      oPlot, borderX, borderY
    endif
endelse ; p1 or p2 with 2 or more points
  endfor

  pBordersOut[0] = ptr_new([-1.0, -1.0]) ; TODO add corner point
  self->addOutputStruct, 0, 'corner', '2D', 'corner', pBordersOut[0]

  for i = 1u, currPairNum do begin
    pairPos = 2 * (i - 1)
    self->addOutputStruct, i, 'separation_line', '2D', 'paired_AAB', pBordersOut[i],$
                           roiIndices = pairedRoiIndicesPr[pairPos:pairPos+1]
    self->addOutputStruct, i + currPairNum, 'separation_line_roi_vertex_indices', '2D', 'paired_AAB', pRoiVertexIndices[pairPos:pairPos+1],$
                           roiIndices = pairedRoiIndicesPr[pairPos:pairPos+1]
  endfor
;  if (keyword_set(fDebug) and fPlotFigure) then begin
;    imSize = size(imagePatch, /DIMENSIONS)
;    aspectRatio = 1.0 * imSize[0] / imSize[1]
;    baseSize    = 720
;    xSz = baseSize * aspectRatio
;    ySz = baseSize
;    window, title = 'AAB-corner detection for ROIs ' + string(roiIndexVectorUnique), xSize = xSz, ySize = ySz
;    tvscl, congrid(imagePatch, xSz, ySz)
;    for i = 1L, nPairedAABs-1 do begin
;      attVertexCoords = aabArr[i]->getAttributeByName('ToDo')
;    endfor
;  endif
  stop
  ; Post-conditions...
  ; Bounding box/polygon
  pBoundingBox = ptr_new(boundingBoxOutR)
  self->appendAttributeStruct, self->createAttributeStruct('axis_bounding_box_absolute', '2D', 'corner', pBoundingBox)
  if (fDebug gt 0) then print, 'adjustCorner_impl2D - Corner adjustment done: ', currPairNum, ' ROI pairs'
  return, currPairNum
end


; Entry point for AAB zone adjustment
function C_sAABSharedZone::computeAABregion_impl2D, separationDist = separationDist, fMockAdjustment = fMockAdjustment, fDebug = fDebug, cornerPoints = cornerPoints

  attType = self->getAttributeByPosition(0) ; Get the type from the first attribute (at the moment it should be the same among the others)
  aabType = attType.aabType

  ; Check which type of AAB zone adjustment perform, and call the proper method.
  case aabType of
    'corner': return, self->adjustCorner_impl2D(separationDist = separationDist, fDebug = fDebug)
    else:     return, self->adjustAABLine_impl2D(separationDist = separationDist, fMockAdjustment = fMockAdjustment, fDebug = fDebug, cornerPoints = cornerPoints)
  endcase
  return, -1
end


; TODO Ivan/Victor
; Pre-condition: merge (when multiple sections of the same ROI are present) has been performed, so 
; there is no ambiguity/duplicity in the roi index vector and vertex lists
function C_sAABSharedZone::adjustAABLine_impl2D, separationDist = separationDist, fMockAdjustment = fMockAdjustment, fDebug = fDebug, cornerPoints = cornerPoints, pointSamplingDistance = pointSamplingDistance

  roiIndexVectorAtt = self->getAttributeByName('roi_index_vector')
  roiIndexVector    = *(*roiIndexVectorAtt.pData[0])
  nRois = n_elements(roiIndexVector)
  if (nRois ne 2) then stop ; Ouch! What happened? Call the programmer! This method is for 2 ROIs only...
  if keyword_set(fDebug) then print, nRois, ' rois'

  pointSamplingDistanceMin = 1.0
  pointSamplingDistance = keyword_set(pointSamplingDistance) ? pointSamplingDistance > pointSamplingDistanceMin : pointSamplingDistanceMin 
fDebug = 1
fSnakeRelaxation = 0b
fComputeMetrics     = 1
fComputeMetricsPrnt = 1
printLogPath = 'D:\tmp\similarityMetrics_z032_separationLine_D1_'

  pairTypeListAtt = self->getAttributeByName('paired_aab_type_list')
  pairTypeList    = **(pairTypeListAtt.pData[0])
  nPairs = n_elements(pairTypeList)
  if (nPairs ne (nRois/2)) then stop ; Maybe one day it will be useful to handle many pairs for a single ROI-pair...
  if (nPairs eq 0) then begin
    print, 'No paired AABs found to adjust. Returning...'
    return, 0
  endif
  if keyword_set(fDebug) then print, 'nPairs = n_elements(**(pairedaabTypeStruct.pData[0])) = ', nPairs

  self->setOutputsNum, nPairs * 2 ; Save separation line coordinates plus vertex indices

  separationType = 'mid' ; TODO JJ make flexible...
  printLogPath += separationType
  roiVertexAtt   = self->getAttributeByName('roi_vertex_list')

  for i = 0L, nPairs-1 do begin
    if keyword_set(fDebug) then print, 'Adjusting pair ', i
    p1x = (*roiVertexAtt.pData)[0,2*i]
    p1y = (*roiVertexAtt.pData)[1,2*i]
    p2x = (*roiVertexAtt.pData)[0,2*i+1]
    p2y = (*roiVertexAtt.pData)[1,2*i+1]
    npts1 = n_elements(*p1x)
    npts2 = n_elements(*p2x)

    case separationType of
    'mid': begin ; Compute center line for the two polylines

           ; separation calculation - closest point point midline
           ;nptsMidline = centerLineForTwoPolylines_sorted(*p1x, *p1y, reverse(*p2x), reverse(*p2y), outX = borderX, outY = borderY, fConn2 = fConn2, fReverse = fReverse, corr12 = corr12)
           ;whConn2 = where(fConn2 ne 0, nConn2, complement = whConn2comp, nComplement = nConn2comp)
             ;makePolygonsPlot, *p1x, *p1y, *p2x, *p2y, sizeFactorWin = 25, winTitle = 'AAB line adjustment: midline[point] separation plot. ROIs ' + string(roiIndexVector[0]) + '-' + string(roiIndexVector[1])
             ;for n = 0, n_elements(corr12)-1 do oPlot, [(*p1x)[n], (reverse(*p2x))[corr12[n]]], [(*p1y)[n], (reverse(*p2y))[corr12[n]]]

           ; separation calculation - normalized-length vertex correspondence
           centerlineNormLength, *p1x, *p1y, reverse(*p2x), reverse(*p2y), xOut = borderX, yOut = borderY, fOplot = 0;keyword_set(fDebug)

           ; resample separation
           borderLen = 0.0
           for s = 0u, n_elements(borderX)-2 do borderLen += sqrt((borderX[s+1] - borderX[s])^2 + (borderY[s+1] - borderX[s])^2)
           nPtsArcSample = round(borderLen / pointSamplingDistance)
           polygonArcSample, borderX, borderX, xCoordsInt, yCoordsInt, points = nPtsArcSample, /fClose

           if keyword_set(fDebug) then begin
             ;print, nConn2, nConn2comp
             imgPatchAtt = self->getAttributeByName('image')
             imgPatch = *(*imgPatchAtt.pData[0])
             ;makePolygonsPlot, *p1x, *p1y, *p2x, *p2y, sizeFactorWin = 25, winTitle = 'AAB line adjustment: midline[point] separation plot. ROIs ' + string(roiIndexVector[0]) + '-' + string(roiIndexVector[1]), backgroundImage = 255-imgPatch, /FCROPIMAGE
             makePolygonsPlot, *p1x, *p1y, *p2x, *p2y, sizeFactorWin = 25, winTitle = 'AAB line adjustment: midline[point] separation plot. ROIs ' + string(roiIndexVector[0]) + '-' + string(roiIndexVector[1])
             for n = 0, n_elements(corr12)-1 do oPlot, [(*p1x)[n], (reverse(*p2x))[corr12[n]]], [(*p1y)[n], (reverse(*p2y))[corr12[n]]]
             oPlot, borderX, borderY, pSym = 1, SymSize = 0.3
             stop
           endif
           endcase

    'svm': begin
           if (npts1 lt 2) or (npts2 lt 2) then begin
             print, 'Warning: adjusting separation line with few input points (', npts1, '-', npts2, ' for ROIs ', roiIndexVector, ')'
             avgX = npts1 eq 1 ? mean(*p2x) : mean(*p1x)
             avgY = npts1 eq 1 ? mean(*p2y) : mean(*p1y)
             ptX = npts1 eq 1 ? (*p1x)[0] : (*p2x)[0]
             ptY = npts1 eq 1 ? (*p1y)[0] : (*p2y)[0]
             borderX = [(avgX + ptX) / 2.0]
             borderY = [(avgY + ptY) / 2.0]
             if keyword_set(fDebug) then stop
           endif else begin
             computeSeparationSVM, *p1x, *p1y, *p2x, *p2y, borderX = borderX, borderY = borderY, dataOut = dataOut, fDebug = fDebug
             if keyword_set(fDebug) then begin
               minxOri = min(*p1x, max = max1xOri) < min(*p2x, max = max2xOri)
               maxxOri = max1xOri > max2xOri
               minyOri = min(*p1y, max = max1yOri) < min(*p2y, max = max2yOri)
               maxyOri = max1yOri > max2yOri
               makeSeparationPlot, *p1x, *p1y, *p2x, *p2y, dataOut,$
                                    minx = minxOri - 0.5, maxx = maxxOri + 0.5, miny = minyOri - 0.5, maxy = maxyOri + 0.5, $
                                    window_title = 'AAB line adjustment: SVM separation plot. ROIs ' + string(roiIndexVector[0]) + '-' + string(roiIndexVector[1])
               if (n_elements(borderX) gt 1) then oPlot, borderX, borderY else oPlot, borderX, borderY, psym = 7
               stop
             endif
           endelse
           endcase

    'hog': begin
           imagePatchStruct = self->getAttributeByName('image')
           imagePatch = **imagePatchStruct.pData[0]
           hog = s_histogramOfOrientedGradients(imagePatch, nBins = 9, nWin_x = 1, nWin_y = 1, angle_min = -!pi/2, $
                                                angle_max = !pi/2, hog_correspondences = hog_correspondences, $
                                                vec_x = hog_x, vec_y = hog_y, grad_xr = gradX, grad_yu = gradY, hog_angles = hog_angles)
           ; TODO Victor 90deg vector rotation gives membrane direction
           ;vx = grad_x
           ;vy = ...
           ;velovect, vx, vy, xMargin = [0, 0], yMargin = [0, 0], /noerase
           theOg = s_getEdgeQuality_2(imagePatch, 5, nBins = 16, centerIm = centerIm, /fPlot)
           borderX = [-1]
           borderY = [-1]
           stop
          endcase
    else: begin
          endcase
    endcase

    case 1 of
      fSnakeRelaxation eq 1: begin
        imagePatchStruct = self->getAttributeByName('image')
        imagePatch = **imagePatchStruct.pData[0]
        snakeAlpha = 0.0
        snakeBeta  = 0.0
        snakeGamma = 0.0
        snakeKappa = 0.0
        snakeIterations   = 0.0
        snakeVFiterations = 0.0
        snakeVFmu = 0.0
        oBorderAC = obj_new('C_sActiveContour', imagePatch, borderX, borderY, $
                                                alpha = snakeAlpha, beta = snakeBeta, gamma = snakeGamma, kappa = snakeKappa, $
                                                mu = snakeVFmu, gvf_iterations = snakeVFiterations, iterations = snakeIterations)
        fAdjust  = oBorderAC->adjustContour()
        snakeX = oBorderAC->getXcoords()
        snakeY = oBorderAC->getYcoords()
        obj_destroy, oBorderAC
        stop
      endcase
      else:
    endcase

    if keyword_set(fComputeMetrics) then begin
      ;msg = 'simD;simL;simKc;simKd;simS'
      ;fileLogger, msg, printLogPath + '.csv'
      computeMetricsForSharedLine, borderX, borderY, *p1x, *p1y, reverse(*p2x), reverse(*p2y), fPrint = fComputeMetricsPrnt, printLogPath = printLogPath
    endif

    nCornerPoints = n_elements(cornerPoints) / 2
    if (nCornerPoints gt 0) then begin

      nptsBorder = n_elements(borderX)
      d00sqr = (cornerPoints[0] - borderX[0])^2 + (cornerPoints[1] - borderY[0])^2
      d01sqr = (cornerPoints[0] - borderX[nptsBorder-1])^2 + (cornerPoints[1] - borderY[nptsBorder-1])^2
      closestTo0 = d00sqr le d01sqr ? 0 : 1

      ; Append corner points to the AAB line. Look for the nearest neighbors.
      case nCornerPoints of
      1: begin
        borderXm = closestTo0 eq 0 ? [cornerPoints[0], borderX] : [borderX, cornerPoints[0]]
        borderYm = closestTo0 eq 0 ? [cornerPoints[1], borderY] : [borderY, cornerPoints[1]]
      endcase
      2: begin
        d10sqr = (cornerPoints[2] - borderX[0])^2 + (cornerPoints[3] - borderY[0])^2
        d11sqr = (cornerPoints[2] - borderX[nptsBorder-1])^2 + (cornerPoints[3] - borderY[nptsBorder-1])^2
        closestTo1 = d10sqr le d11sqr ? 0 : 1
        case 1 of
          (closestTo0 eq 0) and (closestTo1 eq 1): begin
            borderXm = [cornerPoints[0], borderX, cornerPoints[2]]
            borderYm = [cornerPoints[1], borderY, cornerPoints[3]]
          endcase
          (closestTo0 eq 1) and (closestTo1 eq 0): begin
            borderXm = [cornerPoints[2], borderX, cornerPoints[0]]
            borderYm = [cornerPoints[3], borderY, cornerPoints[1]]
          endcase
          (closestTo0 eq 0) and (closestTo1 eq 0): begin
            if (d10sqr lt d11sqr) then begin
              borderXm = [cornerPoints[2], borderX, cornerPoints[0]]
              borderYm = [cornerPoints[3], borderY, cornerPoints[1]]
            endif else begin
              borderXm = [cornerPoints[0], borderX, cornerPoints[2]]
              borderYm = [cornerPoints[1], borderY, cornerPoints[3]]
            endelse
          endcase
          (closestTo0 eq 1) and (closestTo1 eq 1): begin
            if (d00sqr lt d01sqr) then begin
              borderXm = [cornerPoints[0], borderX, cornerPoints[2]]
              borderYm = [cornerPoints[1], borderY, cornerPoints[3]]
            endif else begin
              borderXm = [cornerPoints[2], borderX, cornerPoints[0]]
              borderYm = [cornerPoints[3], borderY, cornerPoints[1]]
            endelse
          endcase
          else: stop ; Unreachable since the 00/01/10/11 cases have been covered above.
        endcase
      endcase
      else: stop ; Should not happen. Call the programmer.
      endcase

      ; TODO JJ Some minimal interpolation before returning. Should(?) be called using the active contour parameter for contour resolution.
      ;sepLinePerimeter = polygonPerimeter(borderXm, borderYm)
      ;polygonArcSample, borderXm, borderYm, borderXn, borderYn, nPoints = sepLinePerimeter, /FPOLYLINE
      polygonLineSample, borderXm, borderYm, borderXn, borderYn, nPointsPerPix = 1, /FPOLYLINE
      pSeparatingCoords = ptr_new([transpose(borderXn), transpose(borderYn)])
    endif else begin
      ;NOT YET TESTED ;cropPolyLineMaxMinMax, borderX, borderY, *p1x, *p1y, cropPolyX2 = *p2x, cropPolyY2 = *p2y, xCoordsOut = borderXcropped, yCoordsOut = borderYcropped
      pSeparatingCoords = ptr_new([transpose(borderX), transpose(borderY)])
    endelse

    self->addOutputStruct, i, 'separation_line', '2D', pairTypeList[i], pSeparatingCoords, roiIndices = roiIndexVector
    roiVertexIndexAtt = self->getAttributeByName('roi_vertex_indices')
    self->addOutputStruct, i + nPairs, 'separation_line_roi_vertex_indices', '2D', pairTypeList[i], *roiVertexIndexAtt.pData[0], roiIndices = roiIndexVector
  endfor

  return, nPairs
end


; V0
function C_sAABSharedZone::computeAABregion_impl2DsvmPairSeparator, p1x, p1y, p2x, p2y, separationFunctionType, fDebug = fDebug

  nDiv    = 64
  suppVectors = 1
  pDomain = make2DsvmTestDomain(p1x, p1y, p2x, p2y, distanceThreshold = distanceThreshold, nDivisions = nDiv)
  nSupportVectors = computeSVMseparationFor2Dpoints(p1x, p1y, p2x, p2y, separationFunctionType, suppVectors, suppVectorIndices, signedAlphas, b,$
                                                    pDomainX = pDomain[0], pDomainY = pDomain[1], dataOut = distMap)
  nBoundaryPoints = getBorderPolylinesFrom2DistanceMap(distMap, distanceThreshold, firstBoundary, secondBoundary, fPlot = fDebug)
  if keyword_set(fDebug) then begin
    print, 'Debug print of the two boundaries...'
    print, '1st: ', firstBoundary
    print, '2nd: ', secondBoundary
    makeSeparationPlot, p1x, p1y, p2x, p2y, dataOut, window_title = 'SVM Separation Plot'
  endif
  print, 'TODO'
end


; V0
; computeAABregion_impl2D
; METHOD UNDER CONSTRUCTION
function C_sAABSharedZone::computeAABregion_impl2D_v0, separationDist = separationDist, fMockAdjust2rois = fMockAdjust2rois, fDebug = fDebug
;return, -1;stop
  roiList = self->getAttributeByName('ROI_list')
  nRois   = n_elements(*(roiList.pData))
  roiRuns = self->getAttributeByName('ROI_vertexRuns')
  nRuns   = n_elements(*(roiRuns.pData))

  aabType = roiList.aabType

  ; By now only the two first ROIs/runs will be used, even when more can be added to the data structures.
  if self.fVerbose then print, 'Going to adjust AAB for ', nRois, ' ROI zones - Type: ', aabType

  ; Setup separation (SVM) variables *(*(roiList.pData))[0], *(*(roiList.pData))[1]
  pxCoords = *(self->getAttributeByName('ROI_xCoords')).pData
  pyCoords = *(self->getAttributeByName('ROI_yCoords')).pData

  p1x = *pxCoords[0]
  p2x = *pxCoords[1]
  p1y = *pyCoords[0]
  p2y = *pyCoords[1]
  separationFunctionType = 1 ; TODO make this an input parameter
  kernelPolyDegree = 1

  if keyword_set(fMockAdjust2rois) then begin
    self->setOutputsNum, 1
    pMockCoords = ptrArr(2)
    pMockCoords[0] = ptr_new(p1x, /no_copy)
    pMockCoords[1] = ptr_new(p1y, /no_copy)
    self->addOutputStruct, 0, 'mock_2roi_adjustment', '2D', aabType, pMockCoords
    return, 1
  endif

  ; TODO JJ: First version of adjustment for ROI pairs only.
  nDiv    = 64
  ;pDomain = make2DsvmTestDomain(p1x, p1y, p2x, p2y, distanceThreshold = distanceThreshold, nDivisions = nDiv)
  pDomain = make2DsvmTestDomain(p1y, p1x, p2y, p2x, distanceThreshold = distanceThreshold, nDivisions = nDiv)
  ;nSupportVectors = computeSVMseparationFor2Dpoints(p1x, p1y, p2x, p2y, separationFunctionType, suppVectors, suppVectorIndices, signedAlphas, b,$
  nSupportVectors = computeSVMseparationFor2Dpoints(p1y, p1x, p2y, p2x, separationFunctionType, suppVectors, suppVectorIndices, signedAlphas, b,$
                                                    krnlPolyDegree = kernelPolyDegree, $
                                                    pDomainX = pDomain[0], pDomainY = pDomain[1], dataOut = distMap)
  nBoundaryPoints = getBorderPolylinesFrom2DistanceMap(distMap, separationDist, firstBoundary, secondBoundary, fPlot = fDebug)

  if keyword_set(fDebug) then begin
    print, 'C_sAABSharedZone separator...'
    print, '1st boundary has ', nBoundaryPoints[0], ' elements - 2nd boundary has ', nBoundaryPoints[0], ' elements'
    print, 'firstBoundary [px]     : ', firstBoundary
    print, 'firstBoundary [domain] : ', *(pDomain[0])[firstBoundary, 0]
    print, 'secondBoundary [px]    : ', secondBoundary
    print, 'secondBoundary [domain]: ', *(pDomain[0])[secondBoundary, 0]
    window, title = 'C_sAABSharedZone Separation Plot', xSize = 700, ySize = 700
    tvscl, distMap
    plot , firstBoundary, xMargin = [0,0], yMargin = [0,0]
    oPlot, secondBoundary
  endif
  stop
  nDomains = n_elements(pDomain)
  if (nDomains gt 0) then for i = 0, nDomains-1 do ptr_free, pDomain[i]

  pAdjustedBoundaries = ptrArr(nRois)
  ; TODO hard-coded case for two boundaries
  pAdjustedBoundaries[0] = ptr_new(firstBoundary, /no_copy)
  pAdjustedBoundaries[1] = ptr_new(secondBoundary, /no_copy)

  case aabType of

  'overlap': begin
    self->setOutputsNum, nRois
    correctedPoints = intArr(nRois)-1

    for i = 0, nRois-1 do begin
      if self.fVerbose then print, 'Setting output number ', i, ' of ', nRois-1
      self->addOutputStruct, i, (*(roiList.pNames))[i], 'overlap_adjusted_boundary', aabType, pAdjustedBoundaries[i]
    endfor
  endcase

  'proximity': begin
    self->setOutputsNum, nRois
    correctedPoints = [-1]
    for i = 0, nRois-1 do begin
      if self.fVerbose then print, 'Setting output number ', i, ' of ', nRois-1
      self->addOutputStruct, i, (*(roiList.pNames))[i], 'proximity_adjusted_boundary', aabType, pAdjustedBoundaries[i]
    endfor
  endcase

  'touch': begin
    print, 'Not yet implemented this AAB type: ', aabType
    stop
  endcase

  else: begin
    print, 'ERROR: no adjustment is defined for AAB type ', aabType
    return, -1
  endcase

  endcase
  return, 1
end


function C_sAABSharedZone::computeAABregion_impl2Dfrom3D, zPos
  print, 'Not yet...'
  return, -1
end


function C_sAABSharedZone::computeAABregion_impl3D
  print, 'Not yet...'
  return, -1
end


; createAttributeStruct
;
; Utility function to define custom attributes for AABM structure.
; By now, an attribute is thought as a scalar data type or as an array of scalar data types.
;
; Parameters:
;   structName  attribute name.
;   structType  attribute type.
;   aabType     indicates the type of AAB: 'overlap', 'proximity', 'touch' (boundary superposition). 
;   pData       pointer to the ROI boundary, image data or another relevant feature for the attribute definition.
;   dataNames   optional string array of name for the data pointers... Could be useful for instance in specifying file name of images or ROIgroups.
;
function C_sAABSharedZone::createAttributeStruct, structName, structType, aabType, pData, dataNames  = dataNames,$
                                                                                          active     = active,$
                                                                                          weights    = weights,$
                                                                                          minWeights = minWeights,$
                                                                                          maxWeights = maxWeights,$
                                                                                          roiIndices = roiIndices,$
                                                                                          roiVertIdx = roiVertIdx,$
                                                                                          fMakeEmptyStruct = fMakeEmptyStruct
  fMakeEmptyStruct = keyword_set(fMakeEmptyStruct)

  if (n_elements(pData) eq 0) and ~fMakeEmptyStruct then begin
    print, 'ERROR in C_sAABSharedZone::createAttributeStruct. No valid data pointer was given to define this structure.'
    return, -1
  endif

  if fMakeEmptyStruct then begin
    nNames = n_elements(dataNames)
    nElem  = nNames gt 0 ? nNames : 1
  endif else nElem = n_elements(pData)

  if (nElem eq 0) then stop
  fPtrPData = 0b
  if (size(pData[0], /TYPE) eq 10) then $
    if (min(ptr_valid(pData)) eq 0) then stop $
    else fPtrPData = 1

  attStruct = {name    : structName,$
               type    : structType,$ ; '2D', '3D', '2D+t', '3D+t', ...
               aabType : aabType,$    ; 'overlap', 'proximity', 'corner'
               roiIndices: keyword_set(roiIndices) ? [roiIndices] : [-1],$
               roiVertIdx: keyword_set(roiVertIdx) ? [roiVertIdx] : [-1],$
               pNames  : ptr_new(keyword_set(dataNames) ? dataNames      : make_array(nElem, /string, value = 'not-set')),$ ; Pointer on attribute feature names.
               pData   : (fMakeEmptyStruct eq 0) $
                 ? (n_elements(pData) eq 1 ? [fPtrPData ? ptr_new(pData) : pData] : fPtrPData ? ptr_new(pData) : pData) $
                 : ptr_new(ptrArr(nElem)),$
               pActive : keyword_set(active)     ? ptr_new(active)     : ptr_new(make_array(nElem, /byte, value = 1b)),$   ; Pointer to feature active flags.
               pWeight : keyword_set(weights)    ? ptr_new(weights)    : ptr_new(make_array(nElem, /float, value = 1.0)),$ ; Pointer to feature weights.
               pMin    : keyword_set(minWeights) ? ptr_new(minWeights) : ptr_new(make_array(nElem, /float, value = 0.0)),$ ; Pointer to minimum feature weights.
               pMax    : keyword_set(maxWeights) ? ptr_new(maxWeights) : ptr_new(make_array(nElem, /float, value = 1.0)),$ ; Pointer to maximum feature weights.
               pROIParamVect: ptr_new() }     ; Pointer to things not yet used.
  return, attStruct
end


; printAttributeStruct
;
; TODO juanedo terminar de implementar la funcion que imprime el contenido de la estructura.
pro C_sAABSharedZone::printAttributeStruct, structPos = structPos, attributeStruct = attributeStruct, fPrintAll = fPrintAll
  if n_elements(structPos) gt 0 then begin
    attributeStruct = self->getAttributeByPosition(structPos)
    print, 'Printing attributeStruct content at position :', structPos
  endif else print, 'Printing attributeStruct content...' 
  if (size(attributeStruct, /type) eq 2) then begin
    print, 'Invalid attribute structure (position: ', structPos, ')'
    return
  endif
  print, '> name: ', attributeStruct.name
  print, '> type: ', attributeStruct.type
  print, '> aabType: ', attributeStruct.aabtype
  print, '> pNames: ', *(attributeStruct.pNames)
  print, '> pData: ', (attributeStruct.pData) ; TODO be smart and print content whithin the pointer array ;)
  for i = 0L, n_elements(attributeStruct.pData)-1 do begin
    isImageAtt = stRegEx(attributeStruct.name, 'image') gt -1
    if ~isImageAtt $
    then print, '-> pData[', i,']', ptr_valid((attributeStruct.pData)[i]) ? *((attributeStruct.pData)[i]) : 'Null pointer' $
    else print, '-> pData[', i,']', keyword_set(fPrintAll) ? *((attributeStruct.pData)[i]) : 'Image attribute (not displayed)'
  endfor

  if keyword_set(fPrintAll) then begin
    print, '> pActive: ', *(attributeStruct.pActive)
    print, '> pWeight: ', *(attributeStruct.pWeight)
    print, '> pMin: ', *(attributeStruct.pMin)
    print, '> pMax: ', *(attributeStruct.pMax)
    print, '> pRoiIndices: TODO'
    print, '> pROIParamVect: TODO'
  endif
  print, 'Printing done'
end


pro C_sAABSharedZone::printContent, fVerbose = fVerbose
  nAttrib = self.nAttrib
  if (nAttrib eq 0) then print, 'No attributes to print...'
  for i=0,nAttrib-1 do self->printAttributeStruct, structPos = i, fPrintAll = fVerbose
end


pro C_sAABSharedZone::addAttributeStruct, position, structName, structType, aabType, pData, roiIndices = roiIndices,$
                                                                                            dataNames  = dataNames,$
                                                                                            active     = active,$
                                                                                            weights    = weights,$
                                                                                            minWeights = minWeights,$
                                                                                            maxWeights = maxWeights,$
                                                                                            fMakeEmptyStruct = fMakeEmptyStruct
  att = self->createAttributeStruct(structName, structType, aabType, pData,$
                                    roiIndices = roiIndices,$
                                    dataNames  = dataNames,$
                                    active     = active,$
                                    weights    = weights,$
                                    minWeights = minWeights,$
                                    maxWeights = maxWeights,$
                                    fMakeEmptyStruct = fMakeEmptyStruct)
  (*(self.pAttrib)[0])[position] = ptr_new(att, /no_copy)
end


pro C_sAABSharedZone::appendAttributeStruct, att, no_copy = no_copy
  pAttListOld = *self.pAttrib
  nAtt = n_elements(pAttListOld)
  pAttListNew = ptrArr(nAtt + 1)
  pAttListNew[0:nAtt-1] = pAttListOld
  pAttListNew[nAtt] = keyword_set(no_copy) ? ptr_new(att, /NO_COPY) : ptr_new(att)
  self.pAttrib = ptr_new(pAttListNew)
  self.nAttrib += 1
end


pro C_sAABSharedZone::setAttributesNum, numAttribtes
  numAttribtes >= 0u
  if (numAttribtes eq 0) or (self.nAttrib gt 0) then self->freeAttributes
  if (numAttribtes gt 0) then self.pAttrib = ptr_new(ptrArr(numAttribtes))
  self.nAttrib = numAttribtes
end


function C_sAABSharedZone::getAttributeByPosition, position

  if (self.nAttrib lt 1) then return, -1
  if (position ge self.nAttrib) or (position lt 0) then begin
    print, 'ERROR in C_sAABSharedZone::getAttributesNum: No attribute struct for requested position (', position, ')'
    return, -1
  endif
  return, ptr_valid(((*(self.pAttrib)[0])[position])) ? *((*(self.pAttrib)[0])[position]): -1 ; TODO temporary workaround for null pointers, since we are intentionally lefting some things unassigned
end



function C_sAABSharedZone::getAttributeByName, name, position = position, fGetAll = fGetAll

  if (self.nAttrib lt 1) then return, -1

  fFirst = 1
  for i = 0L, self.nAttrib-1 do begin
    if ~ptr_valid((*(self.pAttrib)[0])[i]) then begin
      print, 'WARNING: invalid pointed attribute (position ', i, ')
      continue
    endif
    theStruct = (*(self.pAttrib)[0])[i]
    sType = size(*theStruct, /type)
    if (sType ne 8) then begin
      print, 'ERROR: incorrect attribute type (', sType, ', expected 8).'
      stop
    endif
    if ((*theStruct).name eq name) then begin
      if ~keyword_set(fGetAll) then begin
        if arg_present(position) then position = i
        return, *theStruct
      endif else begin
        if fFirst then begin
          position = [i]
          out      = [*theStruct]
        endif else begin
          position = [position, i]
          out      = [out, *theStruct]
        endelse
      endelse
    endif 
  endfor
  if n_elements(out) eq 0 then stop
  return, out
end


pro C_sAABSharedZone::freeAttributeStruct, position

  if (position ge self.nAttrib) or (position lt 0) then return
  if ~ptr_valid(((*self.pAttrib)[position])) then return
  att = *((*self.pAttrib)[position])
  if (size(att, /TYPE) ne 8) then begin
    print, 'C_sAABSharedZone::freeAttributeStruct WARNING. Attribute at position ', position, ' is not a structure: ', att
    return
  endif

  nData = n_elements(att.pNames)
  if (nData gt 0) then begin
    for i = 0u, nData-1 do begin
      if ~ptr_valid((att.pData)[i]) then continue
      typeI = size(*((att.pData)[i]), /TYPE)
      case typeI of
        10: ptr_free, ((att.pData)[i])
        11: obj_destroy, ((att.pData)[i]) ;TODO check for the case of pointer array
        else: ;stop
      endcase
      ptr_free, (att.pData)[i]
    endfor
  endif
  ptr_free, (*(self.pAttrib)[0])[position]
end


; freeAttributes
; Performs pointer deallocation operations for the existing attribute structures.
; Used by the cleanup method, but it could be used to "recycle" an existing C_sAABSharedZone object.
pro C_sAABSharedZone::freeAttributes
  if ~ptr_valid(self.pAttrib) then return
  nAttrib = n_elements(*self.pAttrib)
  for i = 0u, nAttrib-1 do $
    self->freeAttributeStruct, i
  self.nAttrib = 0u
  ptr_free, self.pAttrib
end


function C_sAABSharedZone::createOutputStruct, outputName, outputType, aabType, pData, dataNames  = dataNames,$
                                                                                       active     = active,$
                                                                                       weights    = weights,$
                                                                                       minWeights = minWeights,$
                                                                                       maxWeights = maxWeights,$
                                                                                       fMakeEmptyStruct = fMakeEmptyStruct
  fMakeEmptyStruct = keyword_set(fMakeEmptyStruct)
  if (n_elements(pData) eq 0) and ~fMakeEmptyStruct then begin
    print, 'ERROR in C_sAABSharedZone::createOutputStruct. No valid data pointer was given to define this structure.'
    return, -1
  endif

  if fMakeEmptyStruct then begin
    nNames = n_elements(dataNames)
    nElem  = nNames gt 0 ? nNames : 1
  endif else nElem = n_elements(pData)

  attStruct = {name    : outputName,$
               type    : outputType,$ ; '2D', '3D', '2D+t', '3D+t', ...
               aabType : aabType,$    ; 'overlap', 'proximity', 'touch', ...
               pNames  : ptr_new(keyword_set(dataNames) ? dataNames  : make_array(nElem, /string, value = 'not-set')),$  ; Pointer on attribute feature names.
               pData   : (fMakeEmptyStruct eq 0) $
                 ? (n_elements(pData) eq 1 ? [pData] : pData) $
                 : ptr_new(ptrArr(nElem)),$
               pActive : ptr_new(keyword_set(active)    ? active     : make_array(nElem, /byte, value = 1b)),$   ; Pointer to feature active flags.
               pWeight : ptr_new(keyword_set(weights)   ? weights    : make_array(nElem, /float, value = 1.0)),$ ; Pointer to feature weights.
               pMin    : ptr_new(keyword_set(minWeights)? minWeights : make_array(nElem, /float, value = 0.0)),$ ; Pointer to minimum feature weights.
               pMax    : ptr_new(keyword_set(maxWeights)? maxWeights : make_array(nElem, /float, value = 1.0)),$ ; Pointer to maximum feature weights.
               pROIParamVect:ptr_new() }     ; Pointer to things not yet used.
  return, attStruct
end


pro C_sAABSharedZone::addOutputStruct, position, name, type, aabType, pData, dataNames  = dataNames,$
                                                                             active     = active,$
                                                                             weights    = weights,$
                                                                             minWeights = minWeights,$
                                                                             maxWeights = maxWeights,$
                                                                             roiIndices = roiIndices,$
                                                                             roiVertIdx = roiVertIdx,$
                                                                             fMakeEmptyStruct = fMakeEmptyStruct
  out = self->createAttributeStruct(name, type, aabType, pData,$
                                    dataNames  = dataNames,$
                                    active     = active,$
                                    weights    = weights,$
                                    minWeights = minWeights,$
                                    maxWeights = maxWeights,$
                                    roiIndices = roiIndices,$
                                    roiVertIdx = roiVertIdx,$
                                    fMakeEmptyStruct = fMakeEmptyStruct)
  (*self.pOutput)[position] = ptr_new(out, /no_copy)
end


pro C_sAABSharedZone::setOutputsNum, numOutputs
  numOutputs >= 0u
  if (numOutputs eq 0) or (self.nOutputs gt 0) then self->freeOutputs
  if (numOutputs gt 0) then self.pOutput = ptr_new(ptrArr(numOutputs))
  self.nOutputs = numOutputs
end


function C_sAABSharedZone::getOutputsCount
  return, self.nOutputs
end


function C_sAABSharedZone::getOutputByPosition, position

  if (self.nOutputs lt 1) then return, -1
  if (position ge self.nOutputs) or (position lt 0) then begin
    print, 'ERROR in C_sAABSharedZone::getAttributesNum: No attribute struct for requested position (', position, ')'
    return, -1
  endif else return, *((*self.pOutput)[position])

end


function C_sAABSharedZone::getOutputByName, name, position = position, fGetAll = fGetAll

  if (self.nOutputs lt 1) then return, -1

  out    = -1
  fFirst = 1
  for i = 0L, self.nOutputs-1 do begin
    theStruct = (*self.pOutput)[i]
    if ((*theStruct).name eq name) then begin
      if fFirst and ~keyword_set(fGetAll) then begin
        position = i
        return, *theStruct
      endif
      if fFirst then position = [i]     else position = [position, i]
      if fFirst then out = [*theStruct] else out = [out, *theStruct]
      fFirst = 0
    endif
  endfor

  return, out
end


pro C_sAABSharedZone::freeOutputStruct, position

  if (position ge self.nOutputs) or (position lt 0) then return
  if ~ptr_valid(((*self.pOutput[0])[position])) then return
  out = *((*self.pOutput[0])[position])

  nData = n_elements(out.pNames)
  if (nData gt 0) then begin
    for i = 0u, nData-1 do begin
      typeI = size(*out.pData[i], /TYPE)
      case typeI of
        10: ptr_free, *out.pData[i]
        11: obj_destroy, *out.pData[i]
        else: ;stop
      endcase
      ptr_free, (out.pData)[i]
    endfor
    ptr_free, out.pData
  endif
  ptr_free, (*self.pOutput)[position]
end


pro C_sAABSharedZone::freeOutputs
  if ~ptr_valid(self.pOutput) then return
  nOut = n_elements(*self.pOutput[0])
  for i = 0u, nOut-1 do $
    self->freeOutputStruct, i
  self.nOutputs = 0u
  ptr_free, self.pOutput
end


; cleanup
; Class destructor
pro C_sAABSharedZone::cleanup
  self->freeAttributes
  self->freeOutputs
end


; init
; Class constructor
function C_sAABSharedZone::init, fVerbose = fVerbose, pStructsArray = pStructsArray, fNoCopyStructs = fNoCopyStructs

  self.fVerbose = keyword_set(fVerbose)      ? 1b : 0b
  nStructs      = keyword_set(pStructsArray) ? n_elements(pStructsArray) : 0
  if (nStructs gt 0) then $
    self.pAttrib = keyword_set(fNoCopyStructs) ? ptr_new(pStructsArray, /NO_COPY) $
                                               : ptr_new(pStructsArray)
  return, 1
end


; Class definition
pro C_sAABSharedZone__define
  tmp = { $
    C_sAABSharedZone,$
    nAttrib : 0u,$        ; Number of attributes.
    pAttrib : ptr_new(),$ ; Pointer to (pointer array of) attributes.
    nOutputs: 0u,$        ; Number of outputs (maybe only 1 is sufficient, but who knows...).
    pOutput : ptr_new(),$ ; Pointer to output data structure(s).
    fVerbose: 0b $        ; Flag to indicate verbose mode (0: off, 1: on)
  }
end


; C_sAABSharedZone_test
;
; Test function for the C_sAABSharedZone object class.
; Can be used as an example to create and manipulate C_sAABSharedZone objects.
pro C_sAABSharedZone_test

  print, 'C_sAABSharedZone_test Creating object'
  oAAB1 = obj_new('C_sAABSharedZone', fVerbose = 1b)
  aabType1 = 'overlap'

  print, 'C_sAABSharedZone_test Set attributes number'
  oAAB1->setAttributesNum, 5

  ; Define two rectangles that touch each other by the side... when interpolated, they become oval shapes that overlap.
  factor = 10 ; 1000
  p1x = [0.0, 2.0, 2.0, 0.0, 0.0] * factor
  p1y = [0.0, 0.0, 2.0, 2.0, 0.0] * factor
  polygonArcSample, p1x, p1y, p1xInt, p1yInt, NPOINTS = 60;, /fCloseOutput
  oRoi1 = obj_new('IDLanROI', p1xInt, p1yInt)

  p2x = [2.0, 4.0, 4.0, 2.0, 2.0] * factor
  p2y = [1.0, 1.0, 2.0, 2.0, 1.0] * factor
  polygonArcSample, p2x, p2y, p2xInt, p2yInt, NPOINTS = 60;, /fCloseOutput
  oRoi2 = obj_new('IDLanROI', p2xInt, p2yInt)

  containedIn1 = oRoi1->containsPoints(p2xInt, p2yInt)
  whereIn1 = where(containedIn1 eq 1, nToSeparate1)

  containedIn2 = oRoi2->containsPoints(p1xInt, p1yInt)
  whereIn2 = where(containedIn2 eq 1, nToSeparate2)

  p1x_sep = p1xInt[whereIn2]
  p1y_sep = p1yInt[whereIn2]

  p2x_sep = p2xInt[whereIn1]
  p2y_sep = p2yInt[whereIn1]

;  pROIarr    = ptrArr(2)
;  pROIarr[0] = ptr_new(oRoi1, /NO_COPY)
;  pROIarr[1] = ptr_new(oRoi2, /NO_COPY)
;  roiNames = ['ROI-A', 'ROI-B']
;  oAAB1->addAttributeStruct, 0, 'ROI_names', '2D', aabType1, pRoiArr, dataNames = roiNames

  roiIndexVector = [0,1]
  oAAB1->addAttributeStruct, 0, 'roi_index_vector', '2D', aabType1, ptr_new(roiIndexVector), dataNames = strCompress(string(roiIndexVector), /remove_all)
  pROIvertices = ptrArr(2, 2)
  pROIvertices[0,0] = ptr_new(p1x_sep)
  pROIvertices[0,1] = ptr_new(p1y_sep)
  pROIvertices[1,0] = ptr_new(p2x_sep)
  pROIvertices[1,1] = ptr_new(p2y_sep)
  print, p1x_sep
  print, p1y_sep
  print, p2x_sep
  print, p2y_sep
  oAAB1->addAttributeStruct, 1, 'roi_vertex_list', '2D', aabType1, pROIvertices, dataNames = roiNames
  oAAB1->addAttributeStruct, 2, 'paired_aab_type_list', '2D', aabType1, ptr_new(['test']), dataNames = roiNames
  result = oAAB1->adjustAABLine_impl2D(separationDist=0.0, /fDebug)
;  pImgArr    = ptrArr(2)
;  image      = bytArr(512, 512)
;  pImgArr[0] = ptr_new(image)
;  pImgArr[1] = ptr_new(canny(image)) ; default sigma = 0.6, default high = .8, default low = 0.6
;  oAAB1->addAttributeStruct, 1, 'Image_features', '2D', aabType1, pImgArr, dataNames = ['image', 'CannyEdgemap']

  ; V0
;  pRoiRuns    = ptrArr(2)
;  pRoiRuns[0] = ptr_new(whereIn2)
;  pRoiRuns[1] = ptr_new(whereIn1)
;  oAAB1->addAttributeStruct, 2, 'ROI_vertexRuns', '2D', aabType1, pRoiRuns, dataNames = roiNames

;  pRoiXcoords = ptrArr(2)
;  pRoiXcoords[0] = ptr_new(p1x_sep)
;  pRoiXcoords[1] = ptr_new(p2x_sep)
;  oAAB1->addAttributeStruct, 3, 'ROI_xCoords', '2D', aabType1, pRoiXcoords, dataNames = roiNames

;  pRoiYcoords = ptrArr(2)
;  pRoiYcoords[0] = ptr_new(p1y_sep)
;  pRoiYcoords[1] = ptr_new(p2y_sep)
;  oAAB1->addAttributeStruct, 4, 'ROI_yCoords', '2D', aabType1, pRoiYcoords, dataNames = roiNames

;  oAAB1->printAttributeStruct, structPos = 0
;  structNameROIlist = oAAB1->getAttributeByName('ROI_list', position = getPosROIlist)
  ;print, getPosROIlist
  ;print, structNameROIlist
;  oAAB1->printAttributeStruct, attributeStruct = structNameROIlist


  ;print, 'Going for computeAABregion_impl2D()...'
  ;fCompute = oAAB1->computeAABregion_impl2D(separationDist = 1)
  ;oAAB1->printAttributeStruct, structPos=
  print, 'C_sAABSharedZone_test Destroying object...'
  stop
  obj_destroy, oAAB1
  print, 'C_sAABSharedZone_test Done'
end


; compareAABsByVertexPos
;   Comparator function for two given AAB objects, using a specified ROI index for reference.
;   The objects are compared according to the reference ROI indices.
;   (since the order can change according to this ROI index)
;
; Return value:
;   +1 aab2 is 'greater than' aab1, i.e. greatest ROI vertex index from aab2 >= greatest vertex index from aab1.
;    0 aab1 'equals' aab2, i.e. no diference from the max. ROI index among them.
;   -1 aab1 is 'greater than' aab2, i.e. greatest ROI vertex index from aab1 >= greatest vertex index from aab2.
;  SPECIAL case. AAB object that contains the indices of the other.
function compareAABsByVertexPos, aab1, aab2, roiIndex

  roiIndexVector1att = *((aab1->getAttributeByName('roi_index_vector')).pData)[0]
  roiIndexVector2att = *((aab2->getAttributeByName('roi_index_vector')).pData)[0]
  roiPos1 = (where(*roiIndexVector1att eq roiIndex))[0]
  roiPos2 = (where(*roiIndexVector2att eq roiIndex))[0]

  vertexIndexList1att = (aab1->getAttributeByName('roi_vertex_indices')).pData[0]
  vertexIndexList2att = (aab2->getAttributeByName('roi_vertex_indices')).pData[0]

  vertexIndexList1 = *((*vertexIndexList1att)[roiPos1])
  vertexIndexList2 = *((*vertexIndexList2att)[roiPos2])
  minVertexIndex1 = min(vertexIndexList1, max = maxVertexIndex1)
  minVertexIndex2 = min(vertexIndexList2, max = maxVertexIndex2)

  case 1 of
    ;aab2 contains the indices of aab1.
    (minVertexIndex2 le minVertexIndex1) and (maxVertexIndex2 ge maxVertexIndex1): return, -1
    ;aab1 contains the indices of aab2.
    (minVertexIndex1 le minVertexIndex2) and (maxVertexIndex1 ge maxVertexIndex2): return, +1
    maxVertexIndex1 gt maxVertexIndex2: return, -1
    maxVertexIndex2 gt maxVertexIndex1: return, +1
    else: begin
      if minVertexIndex1 gt minVertexIndex2 then return, -1
      if minVertexIndex2 gt minVertexIndex1 then return, +1
      return, 0
    endcase
  endcase
end


pro printListRoiVertexIndices, aabList, roiIndex
  print, 'printListRoiVertexIndices: roiIndex ', roiIndex
  nAABs = n_elements(aabList)
  if (nAABs lt 1) then return
  for i = 0u, nAABs-1 do begin
    roiIndexVectorAtt = *((aabList[i]->getAttributeByName('roi_index_vector')).pData)[0]
    roiPos = where(*roiIndexVectorAtt eq roiIndex)
    vertexIndexListAtt = (*((aabList[i]->getAttributeByName('roi_vertex_indices')).pData))[roiPos]
    vertexIndexList    = *vertexIndexListAtt[0]
    wh0 = where(vertexIndexList eq 0, count0)
    if (count0 eq 1) and (wh0[0] gt 0) then begin
      minVertexIndex = min(vertexIndexList[0:wh0[0]-1])
      maxVertexIndex = wh0[0] lt (n_elements(vertexIndexList)-1) ? max(vertexIndexList[1+wh0[0]:*]) : vertexIndexList[wh0[0]]
      print, 'Pos. ', i, ': ', minVertexIndex, '-', maxVertexIndex, '(vertex list goes trough index 0)'
    endif else begin
      minVertexIndex = min(vertexIndexList, max = maxVertexIndex)
      print, 'Pos. ', i, ': ', minVertexIndex, '-', maxVertexIndex
    endelse
  endfor
end


; sortAABlistByVertexPos_implInsertionSort
;
; Sorts a given list of AAB objects by using the "insertion sort" algorithm.
; If a list of bounding boxes/polygons is given, it will too be sorted, following the AAB list ordering.
; Bounding box list array format for each element: [xmin, ymin, xmax, ymax]
; Bounding polygon list is a pointer array with each element being a pointer to a list of n x 2 elements (x-y vertices)
pro sortAABlistByVertexPos_implInsertionSort, aabList, roiIndex, boundingBoxList = boundingBoxList, boundingPolyList = boundingPolyList, fPrintSortedList = fPrintSortedList

  nAABs = n_elements(aabList)
  if (nAABs lt 1) then return
  fBBox  = n_elements(boundingBoxList) gt 0
  fBPoly = n_elements(boundingPolyList) gt 0
  for i = 1L, nAABs-1 do begin
    x = aabList[i]
    if (fBBox ne 0)  then xbbox  = boundingBoxList[4*i:4*i+3]
    if (fBPoly ne 0) then xbpoly = boundingPolyList[i]
    j = i - 1
    while ((j ge 0) and (compareAABsByVertexPos(x, aabList[j], roiIndex)) gt 0) do begin
      aabList[j+1] = aabList[j]
      if (fBBox ne 0) then boundingBoxList[4*j+4:4*j+7] = boundingBoxList[4*j:4*j+3]
      if (fBPoly ne 0) then boundingPolyList[j+1] = boundingPolyList[j]
      j -= 1
      if (j eq -1) then break ; Looks stupidly redundant, but IDL seems to be ignoring the WHILE constraint today... ARG!
    endwhile
    aabList[j+1] = x
    if (fBBox ne 0)  then boundingBoxList[4*j+4:4*j+7] = xbbox
    if (fBPoly ne 0) then boundingPolyList[j+1]        = xbpoly
  endfor

  if keyword_set(fPrintSortedList) then printListRoiVertexIndices, aabList, roiIndex
end


pro cropSeparationLineWithBoundaryPolygon, sepX, sepY, polyX, polyY, sepXout = sepXout, sepYout = sepYout

  nPtsSep = n_elements(sepX)
  if (nPtsSep lt 2) then return

  nPtsPoly = n_elements(polyX)
  if (nPtsPoly lt 2) then return

  oRoi = obj_new('IDLanROI', polyX, polyY)
  flagSep = oRoi->ContainsPoints(sepX, sepY)
stop ; not yet!
end
