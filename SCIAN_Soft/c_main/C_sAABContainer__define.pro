; (this is art) _____________________________ IOISIOI _____________________________ (this is art)
;
; NAME
;
; PURPOSE
;
; HISTORY
;
;   Author: Jorge Jara-Wilde.
;
; NOTES
;
;   UNDER CONSTRUCTION.
;
; DEPENDENCIES
;   geometryFunctions.pro  Helper functions for polygon calculations.
;
; (this is art) _____________________________ IOISIOI _____________________________ (this is art)

pro C_sAABContainer::setSpatialIndexNames, indexNames
  if n_elements(indexNames) gt 0 then self.spaceIndexTypes = ptr_new(indexNames)
end


function C_sAABContainer::getSpatialIndexNames
  return, self.pSpaceIndexNames
end


pro C_sAABContainer::setVerboseMode, verboseMode
  self.fVerbose = verboseMode > 0
end


function C_sAABContainer::getVerboseMode
  return, self.fVerbose
end


pro C_sAABContainer::setParameter, $
  alphaVal = alphaVal,$
  betaVal  = betaVal,$
  gammaVal = gammaVal,$
  kappaVal = kappaVal,$
  proximityDist        = proximityDist,$
  separationDist       = separationDist,$
  vfMaxIterations      = vfMaxIterations,$
  contourMaxIterations = contourMaxIterations,$
  contourSamplingDist  = contourSamplingDist,$
  minContourPointCount = minContourPointCount
  if keyword_set(alphaVal)       then self.alphaVal = alphaVal > 0.00001
  if keyword_set(betaVal)        then self.betaVal  = betaVal  > 0.00001
  if keyword_set(gammaVal)       then self.gammaVal = gammaVal > 0.00001
  if (n_elements(kappaVal) eq 1) then self.kappaVal = abs(kappaVal)
  if (n_elements(proximityDist)  eq 1) then self.proximityDist  = abs(proximityDist)
  if (n_elements(separationDist) eq 1) then self.separationDist = abs(separationDist)
  if (n_elements(contourSamplingDist)  eq 1) then self.samplingDist = abs(contourSamplingDist > 0.5)
  if (n_elements(vfIterationsMax)      eq 1) then self.vfIterationsMax      = uInt(round(vfIterationsMax > 0))
  if (n_elements(contourIterationsMax) eq 1) then self.contourIterationsMax = uInt(round(contourIterationsMax > 0))
  if (n_elements(minContourPointCount) eq 1) then self.minContourPointCount = uInt(round(minContourPointCount > 16u))
end


function C_sAABContainer::getImagePatch, xMinRoi, yMinRoi, xMaxRoi, yMaxRoi, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, xMargin = xMargin, yMargin = yMargin, image = image
  xMargin = n_elements(xMargin) eq 0 ? 0 : (xMargin > 0) ;TODO these could be parameter from outside...
  yMargin = n_elements(yMargin) eq 0 ? 0 : (yMargin > 0)
  imgSize = size(*(self.pImage), /DIMENSIONS)
  ; Add some padding
  xMin = round(xMinROI - xMargin) > 0
  yMin = round(yMinROI - yMargin) > 0
  xMax = round(xMaxROI + xMargin) < (imgSize[0]-1)
  yMax = round(yMaxROI + yMargin) < (imgSize[1]-1)
  return, n_elements(image) gt 0 ? image[xMin : xMax, yMin : yMax] : (*(self.pImage))[xMin : xMax, yMin : yMax]
end


function C_sAABContainer::getContours, nContours = nContours, pAABMvertexColors = pAABMvertexColors

  nContours = ptr_valid(self.pXcoords) ? n_elements(*(self.pXcoords)[0]) : 0
  if (nContours eq 0) then return, -1

  if arg_present(pAABMvertexColors) and ptr_valid(self.pAABvertexType) then pAABMvertexColors = *(self.pAABvertexType)[0]

  return, {px: *(self.pXcoords)[0],$
           py: *(self.pYcoords)[0],$
           pz: ptr_valid(self.pZcoords) ? *(self.pZcoords)[0] : ptr_new()}
end


function C_sAABContainer::getSpatialIndexPosition, indexName
  nIndices = n_elements(self.pSpaceIndices)
  if (self.fVerbose gt 0) then print, 'getSpatialIndexPosition: looking for ', indexName, '...'
  if (nIndices lt 1) then begin
    if (self.fVerbose gt 0) then print, '...No indices exist!'
    return, -1
  endif
  for i = 0, nIndices-1 do begin
    if (self.fVerbose gt 0) then print, '...at position', i, ', : ', (*self.pSpaceIndexNames)[i]
    if (*self.pSpaceIndexNames)[i] eq indexName then return, i
  endfor
  if (self.fVerbose gt 0) then print, '...Index not found'
  return, -1
end


;+
; setContours
;
; numROIs may be redundant, but for now I think it can be useful :)
;
; PARAMETERS
;   contours        Array of input contour polygons.
;   numROIs         Number of contours to be set.
;   spaceIndexNames String array specifiying which types of indexes (if any) should be constructed for the contours.
;   fNoCopy         Optional flag that specifies if the input contours array should be destroyed after initializing the AABM
;                   contour pointers.
;-
function C_sAABContainer::setContours, pContours, numROIs, spaceIndexNames = spaceIndexNames, fNoCopy = fNoCopy, fNoInterpolate = fNoInterpolate, fVoronoiPolygons = fVoronoiPolygons

    ; 0 - Prior checkings
  failStatus = 1
  ;numROIs = n_elements(pContours)
  if (numROIs lt 1) then begin
    print, 'Error, number of input ROI/contours to set is lower than 1 (', numROIs, ')'
    return, failStatus
  endif

    ; 1 - Setup
  nSpaceIndex = n_elements(spaceIndexNames)
  fMakeSpatialIndices = nSpaceIndex gt 0

    ; Clean and rebuild (if necessary) pointers to spatial indexes
  if (nSpaceIndex ne 0) then begin

    if (nSpaceIndex eq 1) then spaceIndexNames = [spaceIndexNames] ; ensure array form
    ptr_free, self.pSpaceIndexNames
    self.pSpaceIndexNames = ptr_new(spaceIndexNames)

    nSpaceIndexOld = n_elements(self.pSpaceIndices)
    if nSpaceIndexOld gt 0 then $
      for idx = 0, nSpaceIndexOld-1 do ptr_free, (self.pSpaceIndices)[i]

    self.pSpaceIndices = ptrArr(nSpaceIndex)
  endif

  fContourPreSetupOK = self->preSetNumContours(numROIs)
  if (fContourPreSetupOK ne 1) then begin
    print, 'Something went wrong during contours pre-setup (fContourPreSetupOK is ', fContourPreSetupOK, '). Returning...'
    return, fContourPreSetupOK
  endif

  if (self.fVerbose) then print, 'Avoid initial contour interpolation? ', keyword_set(fNoInterpolate) ? 'Yes' : 'No'
  nDim = size((*pContours[0])[0,*], /N_DIMENSIONS)
  if (self.fVerbose gt 0) then print, 'n_dim for contour 0: ', nDim
  pContoursSet = pContours

  if keyword_set(fVoronoiPolygons) then begin
    xMax = 0
    yMax = 0
    stop ; Need to get xMax & yMax values! Do it and replace this line ;)
    xCentroids = fltArr(numROIs)
    yCentroids = fltArr(numROIs)
    for i = 0u, numROIs-1 do begin
      centroid = polygonCentroid((*pContours[i])[*,0], (*pContours[i])[*,1], xMaxCoord = xMax, yMaxCoord = yMax)
      xCentroids[i] = centroid[0]
      yCentroids[i] = centroid[1]
    endfor
    pContoursSet = voronoiPolygons(xCentroids, yCentroids)
    ; TODO Victor / JJ perform line interpolation with the resulting Voronoi polygons.
    fNoInterpolate = 1
    fNoCopy = 1
  endif

    ; 2 - Finally, set contours
  case nDim of

    2: begin
       for i = 0u, numROIs-1 do begin
         fAdd = self->setContour((*pContoursSet[i])[*,0], (*pContoursSet[i])[*,1], i, fNoInterpolate = fNoInterpolate)
         if (fAdd eq 1) and keyword_set(fNoCopy) then ptr_free, pContoursSet[i]
         if (fMakeSpatialIndices eq 1) then for idx = 0, nSpaceIndex-1 do begin
           print, '(Not yet) adding contour to index... ', idx
         endfor
       endfor
       endcase

    3: begin
       for i = 0u, numROIs-1 do begin
         fAdd = self->setContour((*pContoursSet[i])[*,0], (*pContoursSet[i])[*,1], i, zCoords = (*pContoursSet[i])[*,2], fNoInterpolate = fNoInterpolate)
         if (fAdd eq 1) and keyword_set(fNoCopy) then ptr_free, pContoursSet[i]
         if (fMakeSpatialIndices eq 1) then for idx = 0, nSpaceIndex-1 do begin
           print, '(Not yet) adding contour to index... ', idx
         endfor
       endfor
       endcase

    else: begin
          print, 'Incorrect contour dimensions (', nDim, ') Required 2 or 3'
          return, -1
          endcase
  endcase
  self.numRois = numRois

    ; 3 - Post-execution tasks

  return, 0
end


; preSetNumContours
;
; Ugly but necessary setup method. Initialize pointer arrays for a given number of contours. 
; Must be called before individually adding contours.
function C_sAABContainer::preSetNumContours, numContours, numRois = numRois, fUseZcoord = fUseZcoord

  self.numContours = numContours > 0
  self.numRois = keyword_set(numRois) ? numRois > self.numContours : self.numContours
  if (self.numContours eq 0) then return, 0b

  self.pXcoords    = ptr_new(ptrArr(numContours))
  self.pXcoordsBox = ptr_new(ptrArr(numContours))
  self.pYcoords    = ptr_new(ptrArr(numContours))
  self.pYcoordsBox = ptr_new(ptrArr(numContours))

  if keyword_set(fUseZcoord) then begin
    self.pZcoords    = ptr_new(ptrArr(numContours))
    self.pZcoordsBox = ptr_new(ptrArr(numContours))
  endif

  return, 1b
end


; setContour
;
; Adds a new 2D contour to the object.
;
; Notes:
;   - An x-y axis aligned bounding box is computed and stored for the new contour.
;   - Avoid calling this directly! Use preSetNumContours first!
;
; fDestroyCoords  If set, the input xCoords|yCoords variables will be set to -1 (memory saving flag ;)
function C_sAABContainer::setContour, xCoords, yCoords, pos, zCoords = zCoords, fDestroyCoords = fDestroyCoords, fNoInterpolate = fNoInterpolate

  testAndCorrectCWOrientation, xCoords, yCoords, xOut = xCoords_, yOut = yCoords_
  if (n_elements(pos) eq 0) or (pos ge self.numContours) then begin
    print, 'Error setting contour. Be sure to make a proper call to preSetNumContours first and/or specify correct position.'
    return, -1
  endif

  imgSize = size(*self.pImage, /dim)
  ;repairedPoly = checkAndRepairPolygon(xCoords, yCoords, [0, imgSize[0]-1], [0, imgSize[1]-1], winId = 14)
  ;testAndCorrectCWOrientation, repairedPoly[0,*], repairedPoly[1,*], xOut = xCoordsCW, yOut = yCoordsCW
  ; 2015.11.30 perimeter = polygonPerimeter(xCoordsCW, yCoordsCW)
  perimeter = polygonPerimeter(xCoords_, yCoords_)
  npts      = round(perimeter / self.samplingDist) > self.minContourPointCount

  if ~keyword_set(fNoInterpolate) then begin
    if (self.fVerbose gt 0) then $
      print, 'Contour perimeter is ', perimeter, '. Interpolating from ', n_elements(xCoords_), ' to ', npts, ' points'
    ; 2015.11.30 polygonArcSample, xCoordsCW, yCoordsCW, xCoordsInt, yCoordsInt, nPoints = npts
    polygonArcSample, xCoords_, yCoords_, xCoordsInt, yCoordsInt, nPoints = npts
  endif else begin
    if (self.fVerbose gt 0) then $
      print, 'Contour perimeter is ', perimeter, '. Interpolation avoided by caller function.'
    ; 2015.11.30 xCoordsInt = double(xCoordsCW)
    ; 2015.11.30 yCoordsInt = double(yCoordsCW)
    xCoordsInt = double(xCoords_)
    yCoordsInt = double(yCoords_)
  endelse
  xCoordsCW = -1
  yCoordsCW = -1

  ovrPolys  = calcPolygonIntersectionWithPolyIndices(p1x = xCoordsInt, p1y = yCoordsInt, p2x = xCoordsInt, p2y = yCoordsInt, polyAreas = polyAreas)
  nOvrPolys = ovrPolys[0]

  if (nOvrPolys gt 1) then begin
    print, 'WARNING: self-intersecting polygon at position ', pos, '. Sub-polygon vertex counts are...'
    ovrPolyStart = 1
    polySizes    = lonArr(nOvrPolys)
    polyStartPos = lonArr(nOvrPolys)
    for i = 0, nOvrPolys-1 do begin
      polyStartPos[i] = ovrPolyStart
      polySizes[i] = round(ovrPolys[ovrPolyStart])
      ovrPolyStart += 2 * ovrPolys[ovrPolyStart] + 1
    endfor
    print, polySizes
    maxSize = max(polySizes, min = minSize)
    whMax   = where(polySizes eq maxSize, countMax)
    if (countMax gt 1) then stop ; TODO select which polygon keep
    polyStart  = polyStartPos[whMax]
    polyEnd    = polyStartPos[whMax] + ovrPolys[polyStart] * 2
    xCoordsInt = ovrPolys[polyStart + 1: polyStartPos[whMax] + ovrPolys[polyStart]]
    yCoordsInt = ovrPolys[polyStartPos[whMax] + ovrPolys[polyStart] + 1 : polyStartPos[whMax] + ovrPolys[polyStart]*2]
  endif

  xMin = min(xCoordsInt, max = xMax) - self.proximityDist
  xMax += self.proximityDist
  yMin = min(yCoordsInt, max = yMax) - self.proximityDist
  yMax += self.proximityDist
  zMin = keyword_set(zCoords) ? min(zCoords, max = zMax) : -1

  if (self.fVerbose gt 0) then $
    print, 'Contour bounding box x: ', xMin, '-', xMax, ', y: ', yMin, '-', yMax, ', z?: ', zMin, '-', zMin ne -1 ? 'zMax' : 'n/d'

  nPts = n_elements(xCoordsInt)
  if (xCoordsInt[0] eq xCoordsInt[nPts-1]) and (yCoordsInt[0] eq yCoordsInt[nPts-1]) then begin
    xCoordsInt = xCoordsInt[0:nPts-2]
    yCoordsInt = yCoordsInt[0:nPts-2]
  endif
  (*(self.pXcoords)[0])[pos]    = ptr_new(xCoordsInt, /no_copy)
  (*(self.pYcoords)[0])[pos]    = ptr_new(yCoordsInt, /no_copy)
  (*(self.pXcoordsBox)[0])[pos] = ptr_new([xMin, xMax])
  (*(self.pYcoordsBox)[0])[pos] = ptr_new([yMin, yMax])
  if keyword_set(zCoords) then begin
    (*(self.pZcoords)[0])[pos]    = ptr_new(zCoords)
    (*(self.pZcoordsBox)[0])[pos] = ptr_new([zMin, zMax])
  endif

  if (self.fVerbose gt 0) then print, 'Contour added! In position ', pos, '(', pos+1, ') of ', self.numContours

  if keyword_set(fDestroyCoords) then begin
    xCoords = -1
    yCoords = -1
  endif

  return, 1
end


;; updateContourZonesForRoi
;;
;; TODO JJ! Make subcontour arrays for the ROIs and their corresponding bounding volumes
;pro C_sAABContainer::updateContourZonesForRoi, roiIndex, pFreeContourZones, pSharedContourZones
;
;  ; First, find free vertex runs (contour zones).
;  verticesRoi       = *(*(self.pAABvertexType))[roiIndex]
;  runsFreeVertices  = extractValueRunsFromArray(verticesRoi, 0)
;  nRunsFreeVertices = n_elements(runsFreeVertices) / 2
;
;  if (nRunsFreeVertices gt 0) then begin
;    pFreeContourZones = ptrArr(nRunsFreeVertices)
;    for i = 0u, nRunsFreeVertices-1 do begin
;      runPos = 2*i
;      xCoordsSub = (*((*(self.pXcoords)[0])[roiIndex]))[runsFreeVertices[runPos]:runsFreeVertices[runPos+1]]
;      yCoordsSub = (*((*(self.pYcoords)[0])[roiIndex]))[runsFreeVertices[runPos]:runsFreeVertices[runPos+1]]
;      pFreeContourZones[i] = ptr_new([[xCoordsSub], [yCoordsSub]])
;    endfor
;    if (self.fVerbose gt 0) then print, 'ROI ', roiIndex, ': ', nRunsFreeVertices, ' free contour zone(s)'
;  endif else pFreeContourZones = ptr_new()
;
;  ; Second, find the remaining vertex runs (AAB or "shared" zones).
;  whNot0 = where(verticesRoi ne 0, countNot0)
;
;  if (countNot0 gt 0) then begin
;    maxVertexIndex   = n_elements(verticesRoi)-1
;    runsAABvertices  = makeRunFromCorrelativeValues(whNot0, nMax = maxVertexIndex)
;    nRunsAABvertices = n_elements(runsAABvertices) / 2
;
;    pSharedContourZones = ptrArr(nRunsAABvertices)
;    for i = 0u, nRunsAABvertices-1 do begin
;      runPos = 2*i
;      fThru0 = runsAABvertices[runPos] gt runsAABvertices[runPos+1]
;      if ~fThru0 then begin
;        xCoordsSub = (*((*(self.pXcoords)[0])[roiIndex]))[runsAABvertices[runPos]:runsAABvertices[runPos+1]]
;        yCoordsSub = (*((*(self.pYcoords)[0])[roiIndex]))[runsAABvertices[runPos]:runsAABvertices[runPos+1]]
;      endif else begin
;        xCoordsSub = (*((*(self.pXcoords)[0])[roiIndex]))[runsAABvertices[runPos]:maxVertexIndex]
;        yCoordsSub = (*((*(self.pYcoords)[0])[roiIndex]))[runsAABvertices[runPos]:maxVertexIndex]
;        xCoordsSub = [xCoordsSub, (*((*(self.pXcoords)[0])[roiIndex]))[0:runsAABvertices[runPos+1]]]
;        yCoordsSub = [yCoordsSub, (*((*(self.pYcoords)[0])[roiIndex]))[0:runsAABvertices[runPos+1]]]
;      endelse
;      pSharedContourZones[i] = ptr_new([[xCoordsSub], [yCoordsSub]])
;    endfor
;    if (self.fVerbose gt 0) then print, 'ROI ', roiIndex, ': ', nRunsAABVertices, ' shared contour zone(s)'
;  endif else pSharedContourZones = ptr_new()
;
;end
;
;
;; updateContourZonesForAllRois
;pro C_sAABContainer::updateContourZonesForAllRois
;
;  nRois = ptr_valid(self.pXcoords) ? n_elements(*self.pXcoords) : 0
;  if (nRois lt 1) then return
;
;  ; Check for eventual cleanup previous to setting the new values
;  if ptr_valid(self.pFreeContourZones) then begin
;    nContourZones = n_elements(*self.pFreeContourZones)
;    if nContourZones gt 0 then for i = 0u, nContourZones-1 do ptr_free, (*self.pFreeContourZones)[i]
;  endif
;  if ptr_valid(self.pSharedContourZones) then begin
;    nContourZones = n_elements(*self.pSharedContourZones)
;    if nContourZones gt 0 then for i = 0u, nContourZones-1 do ptr_free, (*self.pSharedContourZones)[i]
;  endif
;
;  pFreeContourZonesArr   = ptrArr(nRois)
;  pSharedContourZonesArr = ptrArr(nRois)
;  for i = 0u, nRois-1 do begin
;    self->updateContourZonesForRoi, i, pFree, pShared
;    if ptr_valid(pFree[0])   then pFreeContourZonesArr[i]   = ptr_new(pFree, /no_copy)
;    if ptr_valid(pShared[0]) then pSharedContourZonesArr[i] = ptr_new(pShared, /no_copy)
;  endfor
;
;  self.pFreeContourZones = ptr_new(pFreeContourZonesArr, /no_copy)
;  self.pSharedContourZones = ptr_new(pSharedContourZonesArr, /no_copy)
;end


; Not yet
function C_sAABContainer::makeQuadTree
  fOK = 0b

  if (numContours lt 1) then goto, r

  self.pSpaceIndices = ptr_new(-1)
  for i = 0L, numContours-1 do begin
    print, 'nothing here yet, contour ', i
  endfor
  fOK = 1b

r:return, fOK
end


function C_sAABContainer::computeErrorIndicatorForVertexList, x0, y0, x1, y1, errType = errType
  if (self.fVerbose gt 0) then print, 'C_sAABContainer::computeErrorIndicatorForVertexList'
  err = -1.0
  if (n_elements(errType) eq 0) then errType = 'hausdorff'
  case errType of
    'hausdorff': err = s_HausdorffDistanceFor2Dpoints(x0, y0, x1, y1)
    'dice'     : err = calcDiceCoefficientForPolygons(x0, y0, x1, y1)
    'jaccard'  : err = calcJaccardSimCoefficientForPolygons(x0, y0, x1, y1)
    'nsd'      : err = calcNSDfromPolygonToPolygon(x0, y0, x1, y1)
    else: print, 'Unrecognized error indicator (', errType, '). Setting output to -1...'
  endcase
  if (self.fVerbose gt 0) then print, 'Computed error indicator: ', err
  return, err
end


;+
; computeErrorIndicator
;
; PARAMETERS
;   refX, refY : pointer arrays to the x and y coordinates of the reference polygons.
;
;   norm
;      - Hausdorff distance
;      - DICE
;      - "false positive"
;      - "false negative"
;      - to keep track of the displacement of each individual node a different approach is required
;-
function C_sAABContainer::computeErrorIndicatorAllContours, refX, refY, errType = errType

  if (self.numContours lt 1) then begin
    print, 'Cannot compute error indicator, no contours to calculate'
    return, -1
  endif

  nrX = n_elements(refX)
  nrY = n_elements(refY)
  if (nrX ne self.numContours) or (nrY ne self.numContours) then begin
    print, 'Cannot compute error indicator. AABM contour number (', self.numContours, ') does not match reference contours (',nrX,'-',nrY,')'
    return, -1
  endif

  errVector = dblArr(self.numContours)

  case errType of
    'hausdorff': $
      for i = 0L, self.numContours-1 do $
        errVector[i] = s_HausdorffDistance2Dpoints([*(refX[i]), *(refY[i])], [(*(self.pXcoords)[0])[i], (*(self.pYcoords)[0])[i]])
    'dice'     : $
      for i = 0L, self.numContours-1 do $
        errVector[i] = calcDiceCoefficientForPolygons(*(refX[i]), *(refY[i]), (*(self.pXcoords)[0])[i], (*(self.pYcoords)[0])[i])
    'jaccard'  : $
      for i = 0L, self.numContours-1 do $
        errVector[i] = calcJaccardSimCoefficientForPolygons(*(refX[i]), *(refY[i]), (*(self.pXcoords)[0])[i], (*(self.pYcoords)[0])[i])
    'nsd'      : $
      for i = 0L, self.numContours-1 do $
        errVector[i] = calcNSDfromPolygonToPolygon(*(refX[i]), *(refY[i]), (*(self.pXcoords)[0])[i], (*(self.pYcoords)[0])[i])
    else: begin
      print, 'Unrecognized error indicator type (', errType, '). Setting output to -1...'
      errVector[*] = -1.0
      endcase
  endcase
  return, errVector
end


pro C_sAABContainer::updateRunCorrespondence, roiCorrespondenceRuns, runPos, runStack, runStatus, runHead, runTail, runCutHead, runCutTail, runCutStatus = runCutStatus

    case 1 of
      (runCutHead eq runHead) and (runCutTail eq runTail): begin
        if (self.fVerbose gt 0) then print, '(runCutHead eq runHead) and (runCutTail eq runTail)'
        runCutStatus = 0ul
        roiCorrespondenceRuns = [roiCorrespondenceRuns, runPos, runCutHead, runCutTail, runCutStatus]
        runStatus[runPos] = 2 ; marked as ready with no cuts needed
        runStack = runStack[2:*]
      endcase

      (runCutHead eq runHead) and (runCutTail lt runTail): begin
        if (self.fVerbose gt 0) then print, '(runCutHead eq runHead) and (runCutTail lt runTail)'
        runCutStatus = 1ul
        roiCorrespondenceRuns = [roiCorrespondenceRuns, runPos, runCutHead, runCutTail, runCutStatus]
        runStatus[runPos]   = 1 ; marked as cutted
        runStack = [runCutTail+1, runStack[1:*]] ; replace original head by a "new" head after cutting
      endcase        

      (runCutHead gt runHead) and (runCutTail eq runTail): begin
        if (self.fVerbose gt 0) then print, '(runCutHead gt runHead) and (runCutTail eq runTail)'
        runCutStatus = 2ul
        roiCorrespondenceRuns = [roiCorrespondenceRuns, runPos, runCutHead, runCutTail, runCutStatus]
        runStatus[runPos] = 1 ; marked as cutted
        runCutBefore = [runHead, runCutHead-1]
        runStack     = [runCutBefore, runStack[2:*]] ; replace by the remainder of the cut (before)
      endcase

      (runCutHead gt runHead) and (runCutTailI lt runTail): begin
        if (self.fVerbose gt 0) then print, '(runCutHead gt runHead) and (runCutTailI lt runTail)'
        runCutStatus = 3ul
        roiCorrespondenceRuns = [roiCorrespondenceRuns, runPos, runCutHead, runCutTail, runCutStatus]
        runStatus[runPos] = 1 ; marked as cutted
        runCutBefore = [runHead, runCutHead-1]
        runCutAfter  = [runCutTail+1, runTail]
        runStack     = [runCutBefore, runCutAfter, runStack[2:*]] ; remove original and put the two cut remainders (before and after)
      endcase

      else: begin
        print, 'Run panic!'
        stop
      endcase
    endcase

end


function C_sAABContainer::findRunCorrespondences, roiIndex1, roiIndex2, runsRoiItype0, runsRoiItype1, runsRoiItype2, runsRoiJtype0, runsRoiJtype1, runsRoiJtype2, pointsFrom1 = pointsFrom1, pointsFrom2 = pointsFrom2

  fStatus = 0b
  fParanoidCheck = 1b

  xi = *((*(self.pXcoords)[0])[roiIndex1])
  yi = *((*(self.pYcoords)[0])[roiIndex1])
; TODO; check .. Optimize selection of elements used in analysis... 
;  if keyword_set(pointsFrom1) then begin
;    xi = xi[pointsFrom1]
;    yi = yi[pointsFrom1]
;  endif
  ni = n_elements(xi)

  xj = *((*(self.pXcoords)[0])[roiIndex2])
  yj = *((*(self.pYcoords)[0])[roiIndex2])
;  if keyword_set(pointsFrom2) then begin
;    xj = xj[pointsFrom2]
;    yj = yj[pointsFrom2]
;  endif
  nj = n_elements(xj)

  ijDistanceMatrix = dblArr(ni, nj)
  ;distFromPolyI = calcDistanceFromPointsToPolygon(xi, yi, xj, yj) ; Remainder: by now this is more precise than the distance function used before.
  ;distFromPolyJ = calcDistanceFromPointsToPolygon(xj, yj, xi, yi)
  for j = 0L, nj-1 do begin
    ijDistanceMatrix[*,j] = (xi - xj[j])^2 + (yi - yj[j])^2  
  endfor

  nRunHeadsI = n_elements(runsRoiItype1)/2
  nRunHeadsJ = n_elements(runsRoiJtype1)/2

  ; [original_run_index, run_start, run_end, relationship_with_original_run]
  ; relationship_with_original_run: 0-> equal to original run
  ;                                 1->from original start to somewhere in the middle
  ;                                 2->from somewhere in the middle to original end
  ;                                 3->from somewhere in the middle to somewhere in the middle
  roiIcorrespondenceRuns = [0ul] ; Intends to be a 4-tuple array of elements
  roiJcorrespondenceRuns = [0ul]
  numCorrespondences     = 0ul ; counts how many correspondences hav been found

  countRunsReadyI = nRunHeadsI ; starts as the total number of runs in I... must go down to 0
  countRunsReadyJ = nRunHeadsJ ; starts as the total number of runs in J... must go down to 0
  runsStatusI = bytArr(nRunHeadsI) ; 0-> not processed, 1->being_cutted, 2->ready_no_cut_needed, 3->ready_with_cuts
  runsStatusJ = bytArr(nRunHeadsJ)

  runIpos = 0 ; indicator of the run-index in roi-I that is being processed
  runJpos = 0 ; indicator of the run-index in roi-J that is being processed

  runStackI = [runsRoiItype1, -1]
  runStackJ = [runsRoiJtype1, -1]
  remainingI = n_elements(runStackI)
  remainingJ = n_elements(runStackJ)

  repeat begin

    ; find run start(head) in run-J
    runHeadJ     = runStackJ[0]
    runTailJ     = runStackJ[1]
    ; for this head, find closest point (candidate) of roi-I
    minDistanceHeadI = min(ijDistanceMatrix[*,runHeadJ], runCutHeadI)
    ; for this candidate, find its corresponding run index and head (in list for roi-I)
    runHeadI         = findIndexInRunEndsList(runStackJ[0:remainingJ-2], runCutHeadI, /fReverseSearch, runNum = runIPosStack) ; Reverse order in the second ROI Poly...
    runTailI         = runStackI[2*runIPosStack+1]
    runIpos = (where(runsRoiItype1 eq runHeadI))[0]
    runJpos = (where(runsRoiJtype1 eq runHeadJ))[0] ; This is rdundant... check or comment
    cutStatusI = -1
    cutStatusJ = -1

    ; Now let's see what to do with the found runs
    runJlength = runTailJ - runHeadJ
    runIlength = runTailI - runHeadI
    ; select the smallest run to do the chacking for matching in the distance matrix
    isRunJtheShorter = runJlength le runIlength ; TODO maybe this can be optimized by renaming the largest point-set to roiI, before distance matrix construction 

    if (self.fVerbose gt 0) then print, 'isRunJtheShorter? (1->yes, 0->no): ', isRunJtheShorter
    if isRunJtheShorter then begin
      rowToCheck     = ijDistanceMatrix[*,runTailJ]
      minDist        = min(rowToCheck[runHeadI:runTailI], minSubscript)
      runCutTailI    = minSubscript + runHeadI
      isRunCutRunEndI = runCutTailI eq runTailI

      if (self.fVerbose gt 0) then print, 'check if the run I is fully covered returns ', isRunCutRunEndI
      ;if the run is entirely contained (both are), add both runs to the correspondence arrays and remove them from the stack
    endif else begin

    endelse

    self->updateRunCorrespondence, roiIcorrespondenceRuns, runIPos, runStackI, runsStatusI, runHeadI, runTailI, runCutHeadI, runCutTailI, runCutStatus = cutStatusI
    self->updateRunCorrespondence, roiJcorrespondenceRuns, runJPos, runStackJ, runsStatusJ, runHeadJ, runTailJ, runCutHeadJ, runCutTailJ, runCutStatus = cutStatusJ

    if self.fVerbose then begin
      print, 'roiIcorrespondenceRuns ', roiIcorrespondenceRuns
      print, 'roiJcorrespondenceRuns ', roiJcorrespondenceRuns
    endif

    ; Finally, check if the run stacks are empty (i.e. they containt only a '-1' value)
    remainingI = n_elements(runStackI)
    remainingJ = n_elements(runStackJ)
    if self.fVerbose then begin
      print, 'runsStatusI ', runsStatusI, ' - remaining i ', remainingI
      print, 'runsStatusJ ', runsStatusJ, ' - remaining j ', remainingJ
    endif

  endrep until (remainingI eq 1) and (remainingJ eq 1)

  while ((ountRunsReadyI + countRunsReadyJ) gt 0) do begin

    ; find run start(head) in run-J
    currentHeadJ     = runsRoiItype1[2*runJpos]
    ; for this head, find closest point (candidate) of roi-I
    minDistanceHeadI = min(ijDistanceMatrix[*,currentHeadJ], candidateHeadI)
    ; for this candidate, find its corresponding run index and head (in list for roi-I)
    runHeadI         = findIndexInRunEndsList(runsRoiJtype1, candidateHeadI, /fReverseSearch, runNum = runIPos) ; Reverse order in the second ROI Poly...

    ; Now let's see what to do with the found runs
    runJlength = runsRoiJtype1[2*runJpos +1] - currentHeadJ
    runIlength = runsRoiItype1[runHeadI +1]  - runHeadI

    if self.fVerbose then begin
      print, 'runIPos: ', runIPos, ' runHeadI: ', runHeadI, 'runIlength: ', runIlength
      print, 'runJpos: ', runJpos, ' currentHeadJ: ', currentHeadJ, ' runJlength: ', runJlength
    endif

    case 1 of
      (runJlength eq runJlength): begin ; possible perfect 1-1 match between points of rois I-J
        if fParanoidCheck then $
          hausDist = s_HausdorffDistance2Dpoints([transpose(xi[runHeadI:runHeadi+runIlength]),$
                                                  transpose(yi[runHeadI:runHeadi+runIlength])],$
                                                 [transpose(xj[currentHeadJ:currentHeadJ+runJlength]),$
                                                  transpose(yj[currentHeadJ:currentHeadJ+runJlength])])
        if self.fVerbose then print, 'Paranoid checking for runs... Hausdorff distance is ', hausDist
        fOverMargin = hausDist gt self.proximityDist
        if self.fVerbose then print, 'Is over proximity threshold (', self.proximityDist, ')?', fOverMargin
        ; maybe allow some slack to this...
        if fOverMargin then begin
          print, 'nothing done here yet!'
        endif else begin
          subRunCode = 0
          roiIcorrespondenceRuns = [roiIcorrespondenceRuns, runIPos, runHeadI, runHeadI+runIlength, subRunCode]
          roiJcorrespondenceRuns = [roiIcorrespondenceRuns, runJpos, currentHeadJ, currentHeadJ+runJlength, subRunCode]
          runsStatusI[runIPos] = 2
          runsStatusJ[runJPos] = 2
        endelse
      endcase
      (runJlength gt runJlength): begin
      endcase
      else: begin ;(runJlength eq runJlength): begin
      endcase
    endcase

    whereRunsIReady = where(runsStatusI lt 2, countRunsReadyI) ; gt 2 indicates readyu, according to our code
    whereRunsJReady = where(runsStatusJ lt 2, countRunsReadyJ)
  endwhile

  for j = 0L, nRunHeadsJ-1 do begin
    currentHeadJ     = runsRoiItype1[2*j]
    minDistanceHeadI = min(ijDistanceMatrix[*,currentHeadJ], candidateHeadI)
    runHeadI         = findIndexInRunEndsList( runsRoiJtype1, candidateHeadI, /fReverseSearch) ; Reverse order in the second ROI Poly...

    if self.fVerbose then  print, 'currentHeadJ ', currentHeadJ, ', runHeadI ', runHeadI

    runJlength = runsRoiJtype1[2*j +1] - currentHeadJ
    runIlength = runsRoiItype1[runHeadI +1] - runHeadI
    if self.fVerbose then begin
      print, 'runJlength: ', runJlength
      print, 'runIlength: ', runIlength
    endif

  endfor

  return, {roiIcorrespondenceRuns: roiIcorrespondenceRuns,$
           roiJcorrespondenceRuns: roiJcorrespondenceRuns}
end


function C_sAABContainer::makeRunsFrom2ndPolygon, roiIndex1, roiIndex2, type0run_out, type1run_out, type2run_out,$
                                                  countInside = countInside, countTouch = countTouch, countInRange = countInRange, $
                                                  aabTypes = aabTypes, wh0 = wh0, wh1 = wh1, wh2 = wh2, whAdj = whAdj
  fStatus = 0b
  xi = *((*(self.pXcoords)[0])[roiIndex1])
  yi = *((*(self.pYcoords)[0])[roiIndex1])
  oROII = obj_new('IDLanROI', xi, yi)
  xj = *((*(self.pXcoords)[0])[roiIndex2])
  yj = *((*(self.pYcoords)[0])[roiIndex2])
  contained  = oRoiI->ContainsPoints([transpose(xj), transpose(yj)]) ; watch out with transpose...
  ; NOTE: touching points are considered as "in range"
  whereIn    = where(contained eq 1, countInside, complement = whereOut, nComplement = countOut)
;  whereOut   = where(contained eq 0, countOut)
;  whereTouch = where((contained eq 2) or (contained eq 3), countTouch) ; 2-> on edge, 3->on vertex

  if (countOut gt 0) then begin
    ;distToISqr2 = fltArr(countOut)
    distToISqr = calcDistanceFromPointsToPolygon(xi, yi, xj[whereOut], yj[whereOut])
    ;for k = 0L, countOut-1 do $
    ;  distToISqr2[k] = min((xi - xj[whereOut[k]])^2 + (yi - yj[whereOut[k]])^2)
    ;whereInRangeFromOut = where(distToISqr le self.proximityDist^2, countInRange)
    whereInRangeFromOut = where(distToISqr le self.proximityDist, countInRange)
    if self.fVerbose then print, countInRange, ' points within threshold distance (', self.proximityDist, ')'
    whereInRange = countInRange gt 0 ? whereOut[whereInRangeFromOut] : [-1]
  endif

  if (countInRange gt 0) then begin
    contained[whereInRange] = 1
    countOut -= countInRange
  endif
  if (countInside gt 0) then contained[whereIn] = 2
  fStatus = (countInside + countInRange) gt 0; + countTouch) gt 0

  if arg_present(wh0) then wh0 = whereOut
  if arg_present(wh1) then wh1 = whereInRange
  if arg_present(wh2) then wh2 = whereIn

  aabTypes = bytArr(n_elements(xj))
  if (countInRange gt 0) then aabTypes[whereInRange] = 1b
  if (countInside  gt 0) then aabTypes[whereIn]      = 2b

  runs = makeRunList(contained)
  maxRunIndex = n_elements(contained)-1
  runElem = n_elements(runs.valueList)
  if self.fVerbose then print, 'now making output runs'

  type0run_out = [-1]
  if (countOut gt 0) then begin
    wh = where(runs.valueList eq 0, whCount)
    for k = 0L, whCount-1 do begin
      runStart = (runs.indexList)[wh[k]]
      runEnd   = runStart eq maxRunIndex ? maxRunIndex $
                                         : wh[k]+1 eq runElem ? maxRunIndex $
                                                              : (runs.indexList)[wh[k]+1] - 1
      type0run_out = [type0run_out, runStart, runEnd]
    endfor
    type0run_out = type0run_out[1:*]
  endif
  if self.fVerbose then print, 'run type 0: ', type0run_out

  type1run_out = [-1]
  if (countInRange gt 0) then begin
    wh = where(runs.valueList eq 1, whCount)
    for k = 0L, whCount-1 do begin
      runStart = (runs.indexList)[wh[k]]
      runEnd   = runStart eq maxRunIndex ? maxRunIndex $
                                         : wh[k]+1 eq runElem ? maxRunIndex $
                                                              : (runs.indexList)[wh[k]+1] - 1
      type1run_out = [type1run_out, runStart, runEnd]
    endfor
    type1run_out = type1run_out[1:*]
  endif
  if self.fVerbose then print, 'run type 1: ', type1run_out

  type2run_out = [-1]
  if (countInside) gt 0 then begin
    wh = where(runs.valueList eq 2, whCount)
    for k = 0L, whCount-1 do begin
      runStart = (runs.indexList)[wh[k]]
      runEnd   = runStart eq maxRunIndex ? maxRunIndex $
                                         : wh[k]+1 eq runElem ? maxRunIndex $
                                                              : (runs.indexList)[wh[k]+1] - 1
      type2run_out = [type2run_out, runStart, runEnd]
    endfor
    type2run_out = type2run_out[1:*]
  endif
  if self.fVerbose then print, 'run type 2: ', type2run_out

  if arg_present(whAdj) then begin
    whAdj = makeRunListWithExclusion(aabTypes, 0)
  endif

  obj_destroy, oROII
  if self.fVerbose then print, "makeRunsFrom2ndPolygon: finished with status ", fStatus
  return, fStatus
end


; containmentTest
; Performs containment and proximity tests for two given ROIs.
; The tests are performed using the first given ROI as subject, so the
; computed containment/proximity results are given for the vertex positions
; of the second ROI.
;
; ARGUMENTS
;   roiIndex1/roiIndex2: The indices for the ROIs to be tested.
;   Output arguments...
;    whereInside1       : Stores the vertex-positions of the 
;                         second ROI that lie inside the first ROI.
;    whereInRange1      : The vertex-positions of the second ROI that are not inside
;                         the first ROI, but within the proximity threshold distance.
;    whereNotInside1    : The vertex-positions of the second ROI that are outside
;                         the first ROI, and are not in range.
;    countInside1       : The number of elements of whereInside1.
;    countInRange1      : The number of elements of whereInRange1.
;    countNotInRange1   : The number of elements of whereNotInside1.
;
pro C_sAABContainer::containmentTest, roiIndex1, roiIndex2, whereInside1, whereInRange1, whereNotInside1, countInside1, countInRange1, countNotInRange1

  fUseIDLComputation = 1b
  x1 = *((*(self.pXcoords)[0])[roiIndex1])
  y1 = *((*(self.pYcoords)[0])[roiIndex1])
  ; Create an IDL ROI Object to perform the containment test
  oRoi1 = obj_new('IDLanROI', x1, y1)
  x2 = *((*(self.pXcoords)[0])[roiIndex2])
  y2 = *((*(self.pYcoords)[0])[roiIndex2])

  ; Points from ROI 2 against points in ROI 1
  containedIn1 = oRoi1->containsPoints(x2, y2)
  obj_destroy, oRoi1
  whereInside1 = where(containedIn1 eq 1, countInside1, complement = whereNotInside1, ncomplement = countNotInside1)
  countInRange1 = 0

  if (countNotInside1 gt 0) then begin
    ;distToISqr2 = fltArr(countNotInsideI)
    if (fUseIDLComputation eq 0) then begin
      distTo1 = calcDistanceFromPointsToPolygon(x1, y1, x2[whereNotInside1], y2[whereNotInside1]) ; calculate only with outside points
    endif else begin
      n1 = n_elements(x1)
      n2notin1 = n_elements(whereNotInside1)
      distTo1 = dblArr(n2notin1)
      for y = 0, n2notin1-1 do begin
        distTo1[y] = distanceFromPointToSegment(x2[whereNotInside1[y]], y2[whereNotInside1[y]], x1[n1-1], y1[n1-1], x1[0], y1[0])
        for z = 0, n1-2 do $
          distTo1[y] <= distanceFromPointToSegment(x2[whereNotInside1[y]], y2[whereNotInside1[y]], x1[z], y1[z], x1[z+1], y1[z+1])
      endfor
      ;distTo1 = distTo1[whereNotInside1]
    endelse
    ;for k = 0L, countNotInside1-1 do $
    ;  distTo1Sqr[k] = min((x1 - x2[whereNotInside1[k]])^2 + (y1 - y2[whereNotInside1[k]])^2)
    ;whereInRangeFromOutside1 = where(distTo1Sqr le self.proximityDist^2, countInRange1)
    whereInRangeFromOutside1 = where(distTo1 le self.proximityDist, countInRange1, complement=whereNotInRangeFromOutside1, ncomplement=countNotInRange1)
    if (self.fVerbose gt 0) then print, countInRange1, ' points within threshold distance (', self.proximityDist, ')'
    whereInRange1 = countInRange1 gt 0 ? whereNotInside1[whereInRangeFromOutside1] : [-1] ; get indices for the whole polygon array
    whereNotInside1 = (countNotInRange1 gt 0) ? whereNotInRangeFromOutside1 : [-1]
  endif

  if (countInside1 gt 0) then begin
    containedIn1[whereInside1] = 2
  endif

  if (countInRange1 gt 0) then begin
    containedIn1[whereInRange1] = 1
    countNotInRange1 = countNotInside1 - countInRange1
  endif

end


; extractContainedVerticesFromOvrPolygon
;
;
; PARAMETERS
;  xi|yi             : the input polygon x|y coordinates.
;  ovrPolyX|ovrPoly  : the overlap polygon x|y coordinates.
;  whereInOvrPoly    : .
;  whereComplementOvr: .
;  nVerticesFoundComplement    : .
;
function C_sAABContainer::extractContainedVerticesFromOvrPolygon, xi, yi, ovrPolyX, ovrPolyY, whereInOvrPoly, whereInOvrPolyComplement, nVerticesFoundComplement

  if self.fVerbose then print, 'C_sAABContainer::extractContainedVerticesFromOvrPolygon'
  nVerticesOvrPoly = n_elements(ovrPolyX)
  nComplementOvr   = 0
  verticesIndexListFromI   = [-1L]
  whereInOvrPoly           = [-1L]
  whereInOvrPolyComplement = [-1L]

  for i = 0L, nVerticesOvrPoly-1 do begin
    whereVertexI = where((xi eq ovrPolyX[i]) and (yi eq ovrPolyY[i]), countWhereInPoly)
    if self.fVerbose then print, countWhereInPoly, ' vertices from ROI found matching overlap polygon vertex [', i, ']'
    if (countWhereInPoly gt 0) then begin
      if countWhereInPoly eq 1 then stop ; TODO is this value is greater than 1 there is something wrong... probably the polygon is not well formed.
      verticesIndexListFromI = [verticesIndexListFromI, whereVertexI]
      whereInOvrPoly         = [whereInOvrPoly, i]
    endif else whereInOvrPolyComplement = [whereInOvrPolyComplement, i]
  endfor

  nVerticesFound = n_elements(verticesIndexListFromI) - 1
  nVerticesFoundComplement = n_elements(whereInOvrPolyComplement) - 1

  if self.fVerbose then $
    print, nVerticesFound, ' vertices from ROI found in the overlap polygon (', $
           nVerticesFoundComplement, ' vertices in in the complement)'

  if (nVerticesFoundComplement gt 0) then whereInOvrPolyComplement = whereInOvrPolyComplement[1:*]
  if (nVerticesFound eq 0) then return, [-1]

  whereInOvrPoly = whereInOvrPoly[1:*]
  return, verticesIndexListFromI[1:*]

end


; Stub. Not usable for now.
function C_sAABContainer::sortVertexListsByConsecutiveIndices, proxRuns, ovrVertexList, nVertices

  if (nVertices eq 0) then return, -1

  nProxRuns = n_elements(proxRuns)
  if proxRuns[0] eq -1 then begin
    nProxRuns = 0
    proxRuns  = [-1, -1]
  endif

  nOvrVert = n_elements(ovrVertices)
  if ovrVertices[0] eq -1 then begin
    nOvrVert    = 0
    ovrVertices = [-1, -1]
  endif

  if self.fVerbose then print, 'nProxRuns = ', nProxRuns, ', nOvrVert = ', nOvrVert

  pLists = ptrArr(1)
  nLists = 0
  ovrCount  = 0
  proxCount = 0
  totalCount = 0

  ; Perform mergesort-like lists merging
  repeat begin
    fAddRun     = 0b
    fUseProxRun = 0b
    fUseOvrRun  = 0b
    auxRun = [-1]
    proxRunPos  = 2 * proxCount
    currentProxRun = [proxRuns[proxRunPos], proxRuns[proxRunPos+1]]
    currentOvrRun  = ovrVertexList[-1,-1]
    prevVertProxRun = currentProxRun[0] eq 0 ? nVertices-1 : currentProxRun[0]-1
    proxVertProxRun = currentProxRun[1] eq nVertices-1 ? 0 : currentProxRun[0]+1

      ; no runs intercalation is assumed
    case 1 of

      currentOvrRun[1] eq prevVertProxRun: begin
        auxRun = [auxRun, -singleRunToIndices(currentOvrRun[0], currentOvrRun[1], nVertices),$
                           singleRunToIndices(currentProxRun[0], currentProxRun[1], nVertices)]
      endcase

      currentOvrRun[0] eq proxVertProxRun: begin
        auxRun = [auxRun, singleRunToIndices(currentProxRun[0], currentProxRun[1], nVertices),$
                         -singleRunToIndices(currentOvrRun[0], currentOvrRun[1], nVertices)]
      endcase

      currentOvrRun[1] lt prevVertProxRun: begin
      endcase

      currentOvrRun[0] gt proxVertProxRun: begin
      endcase

      else: begin ; This case corresponds to an invalid [-1, -1] run
      endcase
    endcase

    if (fAddRun eq 1) then begin
      pLists[nLists] = ptr_new(vertexList)
      nLists += 1
      pLists = [pLists, ptr_new()]
      if (fUseProxRun eq 1) then proxCount += 1
      if (fUseProxRun eq 1) then ovrCount += 1
    endif

  endrep until (ovrCount eq nOvrVert) and (proxCount eq nProxRuns)

end


; Update vertex color code arrays (display purposes)
pro C_sAABContainer::updateVertexCodeVector, roiIndex, vertexIndexVector, vertexCode, updateType = updateType
  if (n_elements(vertexIndexVector) eq 0) then return
  if (vertexIndexVector[0] eq -1) then return
  vertexTypes = *(*(self.pAABvertexType))[roiIndex]
  if ~keyword_set(updateType) then vertexTypes[vertexIndexVector] >= vertexCode
  (*(self.pAABvertexType))[roiIndex] = ptr_new(vertexTypes, /no_copy)
end


function C_sAABContainer::getVertexListsByType, roiIndex, vertexType, vertexListArrayComplement = vertexListArrayComplement

  if (roiIndex ge self.numContours) then return, -1
  vertexCodes  = (*(self.pAABvertexType))[roiIndex]
  nVertices    = n_elements(*vertexCodes)
  fMakeComplement = arg_present(vertexListArrayComplement)
  if fMakeComplement $
  then whVertexCode = where(*vertexCodes eq vertexType, vertexCount, complement = whVertexCodeCompl, nComplement = vertexCountCompl) $
  else whVertexCode = where(*vertexCodes eq vertexType, vertexCount)

  if (vertexCount eq 0) then begin
    if fMakeComplement then $
      vertexListArrayComplement = [ptr_new(uIndGen(nVertices))]
    return, [ptr_new()]
  endif

  vertexListArray = ptrArr(1)
  vertexList = [-1]
  if fMakeComplement then vertexListArrayComplement = ptrArr(1)

  for i = 0u, vertexCount-1 do begin

    if (i eq (vertexCount-1)) then goto, t

    if ((whVertexCode[i+1] - whVertexCode[i]) eq 1) $
    then vertexList = [vertexList, whVertexCode[i]] $
    else begin
t:    vertexList      = [vertexList, whVertexCode[i]]
      vertexListArray = [vertexListArray, ptr_new(vertexList[1:*])]
      vertexList      = [-1]
    endelse

  endfor

  vertexListArray = vertexListArray[1:*]
  nRuns = n_elements(vertexListArray)

  if (nRuns gt 1) then begin

    nElemLastRun = n_elements(*vertexListArray[nRuns-1])
    if (((*vertexListArray[0])[0] eq 0) $
    and ((*vertexListArray[nRuns-1])[nElemLastRun-1] eq (nVertices-1))) then begin

      tmp = [*vertexListArray[nRuns-1], *vertexListArray[0]]
      vertexListArray[0] = ptr_new(tmp, /no_copy)
      ptr_free, vertexListArray[nRuns-1]
      vertexListArray = vertexListArray[0:nRuns-2]

    endif
  endif

  return, vertexListArray
end


; TODO document
; makeAABforPairedROIvertices
;
; roiIndexVector: vector containing the ROI indices involved in the AAB zone to construct (for now they can repeat).
; type          : specifies the type of the AAB 'overlap', 'proximity', ... 'mixed?'
; pVertexIndices: pointer array of "nRois" elements, each of them containing the vertex indices involved in the AAB.
; aabInput polys: list of the polygons/polylines involved (e.g. the overlap polygon between two ROIs, or the vertex lists.
;                 for proximity).
function C_sAABContainer::makeAABforPairedROIvertices, roiIndexVector = roiIndexVector, pVertexIndices = pVertexIndices, type = type, roiNames = roiNames, fPlot = fPlot, $
                                                       aabInputPolys = aabInputPolys, correlativeIndex = correlativeIndex, correlIndexType = correlIndexType, $
                                                       pRoiBoundingBoxes = pRoiBoundingBoxes, minMaxCoordsXY = minMaxCoordsXY, boundingBoxOut = boundingBoxOut, boundingPolyXout = boundingPolyXout, boundingPolyYout = boundingPolyYout
  nRois = n_elements(roiIndexVector)
  if (nRois lt 2) then begin
    print, 'Not enough input ROIs where given. At least two are required for a shared boundary.'
    stop
    return, -1
  endif

  ; TODO JJ workaround for vertex index next to the 0 (nth-1 position)
  for i = 0, nRois-1 do begin
    vertexIndices = *pvertexIndices[i]
    whNeg = where(vertexIndices lt 0, countNeg)
    if (countNeg gt 0) then begin
      if (countNeg gt 1) then stop
      nPtsI = n_elements(*((*(self.pXcoords)[0])[roiIndexVector[i]]))
      for j = 0, countNeg-1 do $
        vertexIndices[whNeg[j]] = nPtsI + vertexIndices[whNeg[j]]
      pvertexIndices[i] = ptr_new(vertexIndices, /no_copy)
    endif
  endfor
  attPosRoiIndexVector = 0
  attPosImage          = 1
  attPosImgFeatures    = 2
  attPosROIvertexIndices  = 3
  attPosROIvertices       = 4
  attPosBoundingBoxAbs    = 5
  attPosBoundingBoxRel    = 6
  attPosType              = 7
  attPosCorrIndex         = 8
  attPosCorrIndexType     = 9
  attPosCorrVertexIndices = 10
  attPosRoiPolygons       = 11
  attPosDistMapPatch      = 12
  nAttribAAB              = 13
  fAddInputPolys = n_elements(aabInputPolys) gt 0
  if fAddInputPolys then begin
    attPosPolys = nAttribAAB
    nAttribAAB += 1
  endif

  oAABsharedZone = obj_new('C_sAABSharedZone', fVerbose = self.fVerbose)
  oAABsharedZone->setAttributesNum, nAttribAAB
  fBoundingBoxes = n_elements(pRoiBoundingBoxes) eq nRois
  fBoundingBoxesRel = 0b
  case 1 of
    n_elements(minMaxCoordsXY) gt 0: begin
      xMin = minMaxCoordsXY[0]
      xMax = minMaxCoordsXY[1]
      yMin = minMaxCoordsXY[2]
      yMax = minMaxCoordsXY[3]
    endcase

    fBoundingBoxes eq 1: begin ; Get bounding box from the ROI-bounding box list
      xMinROI = (*pRoiBoundingBoxes[0])[0]
      xMaxROI = (*pRoiBoundingBoxes[0])[1]
      yMinROI = (*pRoiBoundingBoxes[0])[2]
      yMaxROI = (*pRoiBoundingBoxes[0])[3]
      for i = 1, nRois-1 do begin
        xMinROI <= (*pRoiBoundingBoxes[i])[0]
        xMaxROI >= (*pRoiBoundingBoxes[i])[1]
        yMinROI <= (*pRoiBoundingBoxes[i])[2]
        yMaxROI >= (*pRoiBoundingBoxes[i])[3]
      endfor
    endcase

    else: begin
      ; Compute bounding boxes
      pRoiBoundingBoxesRel = ptrArr(nRois)
      xMinROI = min((*((*(self.pXcoords)[0])[roiIndexVector[0]]))[*pVertexIndices[0]], max = xMaxROI)
      yMinROI = min((*((*(self.pYcoords)[0])[roiIndexVector[0]]))[*pVertexIndices[0]], max = yMaxROI)
      pRoiBoundingBoxesRel[0] = ptr_new([xMinROI, xMaxROI, yMinROI, yMaxROI])
      for i = 1, nRois-1 do begin
        xMinTmp = min((*((*(self.pXcoords)[0])[roiIndexVector[i]]))[*pVertexIndices[i]], max = xMaxTmp)
        yMinTmp = min((*((*(self.pYcoords)[0])[roiIndexVector[i]]))[*pVertexIndices[i]], max = yMaxTmp)
        xMinROI <= xMinTmp
        yMinROI <= yMinTmp
        xMaxROI >= xMaxTmp
        yMaxROI >= yMaxTmp
        if (yMaxTmp lt 0) or (xMaxTmp lt 0) then stop ; Should not happen, unless in the case of bad polygons.
        pRoiBoundingBoxesRel[i] = ptr_new([xMinTmp, xMaxTmp, yMinTmp, yMaxTmp])
      endfor
;      for i = 0, nRois-1 do begin
;        (*pRoiBoundingBoxesRel[i])[0] -= xMinROI
;        (*pRoiBoundingBoxesRel[i])[1] -= xMinROI
;        (*pRoiBoundingBoxesRel[i])[2] -= yMinROI
;        (*pRoiBoundingBoxesRel[i])[3] -= yMinROI
;      endfor
      fBoundingBoxesRel = 1b
    endcase
  endcase

  ; Select a bounding area to get the image from, based on the bounding box of the ROI set
  imagePatch   = self->getImagePatch(xMinROI, yMinROI, xMaxROI, yMaxROI, xMin = xMinPatch, xMax = xMaxPatch, yMin = yMinPatch, yMax = yMaxPatch)
  distMap      = self->makeDistanceMaskFromRois(roiIndexVector = roiIndexVector)
  distMapPatch = self->getImagePatch(xMinROI, yMinROI, xMaxROI, yMaxROI, image = distMap)
  ; Make the AAB zone bounding box
  aabBoundingBoxAbs = [xMinRoi, xMaxRoi, yMinRoi, yMaxRoi] ; Form [[[xMinRoi, xMaxRoi], [yMinRoi, yMaxRoi]] means row 0: xmin, xmax / row 1: ymin, ymax
  aabBoundingBoxRel = [xMinRoi-xMinPatch, xMaxRoi-xMinPatch, yMinRoi-yMinPatch, yMaxRoi-yMinPatch]
  boundingBoxOut    = [xMinROI, yMinROI, xMaxROI, yMaxROI]

  imagePatch = (*(self.pImage))[xMinPatch : xMaxPatch, yMinPatch : yMaxPatch]

  if (self.fVerbose gt 0) and keyword_set(fPlot) then begin ;and (nRois eq 2) then $
    makePolygonsPlot, (*((*(self.pXcoords)[0])[roiIndexVector[0]]))[*pVertexIndices[0]] - xMinPatch,$
                      (*((*(self.pYcoords)[0])[roiIndexVector[0]]))[*pVertexIndices[0]] - yMinPatch,$
                      (*((*(self.pXcoords)[0])[roiIndexVector[1]]))[*pVertexIndices[1]] - xMinPatch,$
                      (*((*(self.pYcoords)[0])[roiIndexVector[1]]))[*pVertexIndices[1]] - yMinPatch,$
                      backgroundImage = imagePatch, /fUseImageScale, winTitle = 'AAB Zone Assembly Plot (' + type + ')', sizeFactorWin = 20, sizeFactorOut = sizeFactorOut ; TODO add new available flags such as /fNoAxes
    if fBoundingBoxesRel eq 1 then begin
      oPlot, sizeFactorOut * ([(*pRoiBoundingBoxesRel[0])[0], (*pRoiBoundingBoxesRel[0])[1], (*pRoiBoundingBoxesRel[0])[1], (*pRoiBoundingBoxesRel[0])[0], (*pRoiBoundingBoxesRel[0])[0]]-xMinPatch), $
             sizeFactorOut * ([(*pRoiBoundingBoxesRel[0])[2], (*pRoiBoundingBoxesRel[0])[2], (*pRoiBoundingBoxesRel[0])[3], (*pRoiBoundingBoxesRel[0])[3], (*pRoiBoundingBoxesRel[0])[2]]-yMinPatch), $
             lineStyle = 0, color = '0000FF'x
      oPlot, sizeFactorOut * ([(*pRoiBoundingBoxesRel[1])[0], (*pRoiBoundingBoxesRel[1])[1], (*pRoiBoundingBoxesRel[1])[1], (*pRoiBoundingBoxesRel[1])[0], (*pRoiBoundingBoxesRel[1])[0]]-xMinPatch), $
             sizeFactorOut * ([(*pRoiBoundingBoxesRel[1])[2], (*pRoiBoundingBoxesRel[1])[2], (*pRoiBoundingBoxesRel[1])[3], (*pRoiBoundingBoxesRel[1])[3], (*pRoiBoundingBoxesRel[1])[2]]-yMinPatch), $
             lineStyle = 0, color = '00FFFF'x
    endif

    if (nRois gt 2) then for i = 2, nRois-1 do begin
      oPlot, sizeFactorOut * ((*((*(self.pXcoords)[0])[roiIndexVector[i]]))[*pVertexIndices[i]] - xMinPatch), sizeFactorOut * ((*((*(self.pYcoords)[0])[roiIndexVector[i]]))[*pVertexIndices[i]] - yMinPatch)
      if fBoundingBoxes eq 1 then $
      oPlot, sizeFactorOut * ([(*pRoiBoundingBoxes[i])[0], (*pRoiBoundingBoxes[i])[1], (*pRoiBoundingBoxes[i])[1], (*pRoiBoundingBoxes[i])[0], (*pRoiBoundingBoxes[i])[0]] - xMinPatch), $
             sizeFactorOut * ([(*pRoiBoundingBoxes[i])[2], (*pRoiBoundingBoxes[i])[2], (*pRoiBoundingBoxes[i])[3], (*pRoiBoundingBoxes[i])[3], (*pRoiBoundingBoxes[i])[2]] - yMinPatch), $
             lineStyle = 0, color = 'FF0000'x
      stop
    endfor
  endif

  oAABsharedZone->addAttributeStruct, attPosRoiIndexVector, 'roi_index_vector', '2D', type, ptr_new(roiIndexVector), dataNames = roiNames
  oAABsharedZone->addAttributeStruct, attPosImage, 'image', '2D', type, ptr_new(*self.pImage), dataNames = ['image']
  oAABsharedZone->addAttributeStruct, attPosImgFeatures, 'image_feature', '2D', type, ptr_new(imagePatch, /no_copy), dataNames = ['image_feature']
  oAABsharedZone->addAttributeStruct, attPosDistMapPatch, 'paired_roi_distance_map_patch', '2D', type, ptr_new(distMapPatch, /no_copy), dataNames = ['roi_feature']

  if (self.fVerbose gt 0) and keyword_set(fPlot) then $
    oPlot, sizeFactorOut*([xMinRoi, xMaxRoi, xMaxRoi, xMinRoi, xMinRoi]-xMinPatch), $
           sizeFactorOut*([yMinRoi, yMinRoi, yMaxRoi, yMaxRoi, yMinRoi]-yMinPatch), color = '00FF00'x

  pROIvertices = ptrArr(nRois, 2) ; number of ROIs x number of dimensions
  pCorrelativeRoiVertexIndices = ptrArr(nRois)
  pVertexIndices2 = ptrArr(2)
  for i=0, nRois-1 do begin
    pROIvertices[0,i] = ptr_new((*((*(self.pXcoords)[0])[roiIndexVector[i]]))[*pVertexIndices[i]]); - xMinPatch)
    pROIvertices[1,i] = ptr_new((*((*(self.pYcoords)[0])[roiIndexVector[i]]))[*pVertexIndices[i]]); - yMinPatch)
    pCorrelativeRoiVertexIndices[i] = ptr_new(*pVertexIndices[i])
    pVertexIndices2[i] = ptr_new(*pVertexIndices[i])
  endfor
  nPtsConvexHull = convexHullFromXYpointsArray(pROIvertices, convexHullX = boundingPolyXout, convexHullY = boundingPolyYout)

  oAABsharedZone->addAttributeStruct, attPosROIvertices, 'roi_vertex_list', '2D', type, pROIvertices, dataNames = roiNames
  oAABsharedZone->addAttributeStruct, attPosROIvertexIndices, 'roi_vertex_indices', '2D', type, pVertexIndices2, dataNames = roiNames
  oAABsharedZone->addAttributeStruct, attPosType, 'paired_aab_type_list', '2D', type, ptr_new([type]), dataNames = roiNames
  oAABsharedZone->addAttributeStruct, attPosBoundingBoxAbs, 'axis_bounding_box_absolute', '2D', type, ptr_new(aabBoundingBoxAbs), dataNames = ['axis_bounding_box_absolute']
  if (self.fVerbose gt 0) then print, 'boundingBoxOut (absolute xmin,ymin,xmax,ymax): ', boundingBoxOut
  oAABsharedZone->addAttributeStruct, attPosBoundingBoxRel, 'axis_bounding_box_relative', '2D', type, ptr_new(aabBoundingBoxRel), dataNames = ['axis_bounding_box_relative']
  oAABsharedZone->addAttributeStruct, attPosCorrIndex, 'correlative_AAB_index', '2D', type, ptr_new([correlativeIndex]), dataNames = ['correlative_AAB_index']
  oAABsharedZone->addAttributeStruct, attPosCorrIndexType, 'correlative_AAB_index_type', '2D', type, ptr_new([correlIndexType]), dataNames = ['correlative_AAB_index_type']
  oAABsharedZone->addAttributeStruct, attPosCorrVertexIndices, 'correlative_roi_vertex_indices', '2D', type, pCorrelativeRoiVertexIndices, dataNames = roiNames
  pRoiPolygons = ptrArr(nRois)
  for i=0, nRois-1 do begin
    pRoiPolygonVertices    = ptrArr(2)
    pRoiPolygonVertices[0] = ptr_new((*((*(self.pXcoords)[0])[roiIndexVector[i]])))
    pRoiPolygonVertices[1] = ptr_new((*((*(self.pYcoords)[0])[roiIndexVector[i]])))
    pRoiPolygons[i] = ptr_new(pRoiPolygonVertices)
  endfor
  oAABsharedZone->addAttributeStruct, attPosRoiPolygons, 'roi_polygons', '2D', type, pRoiPolygons, dataNames = roiNames
  if fAddInputPolys then $
    oAABsharedZone->addAttributeStruct, attPosPolys, type+'_poly', '2D', type, aabInputPolys, dataNames = roiNames

;  if (self.fVerbose gt 0) then $
;    for sp = attPosROIvertices, nAttribAAB-1 do oAABsharedZone->printAttributeStruct, structPos = sp

  return, oAABsharedZone
end


; fNonStrict  Enables merging of vertex runs (and thus the input AABs) when 
;             there is a gap that can be filled according to the following parameters...
; i.  maxGapInd   Maximum gap size allowed in vertex indices. Default value: 2 (1 vertex missing between vertex runs).
; ii. maxGapDist  Maximum gap size allowed in euclidean distance. Default value: taken from self.proximityDist.
;                 maxGapDist NOT YET IMPLEMENTED!!!
;
; Pre-Conditions
;  - same roi index pairs for both aab1 and aab2
;  - both vertex runs are consecutive
;  - roi index 1 > roi index 2, for both aab1 and aab2
function C_sAABContainer::tryAABpairMerge, aab1, aab2, maxIndex1 = maxIndex1, maxIndex2 = maxIndex2, boundingBox1 = boundingBox1, boundingBox2 = boundingBox2, boundingBoxOut = boundingBoxOut, $
                                                       pBoundingPoly1 = pBoundingPoly1, pBoundingPoly2 = pBoundingPoly2, pBoundingPolyOut = pBoundingPolyOut, fNonStrict = fNonStrict, maxGapInd = maxGapInd, maxGapDist = maxGapDist
  deltaIndex = keyword_set(fNonStrict) ? (maxGapInd > 0) + 1 : 0
  if keyword_set(maxIndex1) and keyword_set(maxIndex2) then begin

    vertexIndexListAtt1 = aab1->getAttributeByName('roi_vertex_indices')
    vertexIndexList1_s  = *(*(vertexIndexListAtt1.pData))[0]
    vertexIndexList2_e  = *(*(vertexIndexListAtt1.pData))[1]
    vertexIndexListAtt2 = aab2->getAttributeByName('roi_vertex_indices')
    vertexIndexList1_e  = *(*(vertexIndexListAtt2.pData))[0]
    vertexIndexList2_s  = *(*(vertexIndexListAtt2.pData))[1]

    roi1indices = [vertexIndexList1_e, vertexIndexList1_s]
    roi2indices = [vertexIndexList2_s, vertexIndexList2_e]
    roi2indices = roi2indices[uniq(roi2indices, sort(roi2indices))] ; there may be duplicates here
    runs1  = makeRunFromCorrelativeValues(roi1indices, nMax = maxIndex1, delta = deltaIndex)
    runs2  = makeRunFromCorrelativeValues(roi2indices, nMax = maxIndex2, delta = deltaIndex)
    nRuns1 = n_elements(runs1) / 2
    nRuns2 = n_elements(runs2) / 2

  endif else begin

    roiIndexVector = *(*((aab1->getAttributeByName('roi_index_vector')).pData[0]))
    maxIndex1 = n_elements((*((*(self.pXcoords)[0])[roiIndexVector[0]]))) - 1
    maxIndex2 = n_elements((*((*(self.pXcoords)[0])[roiIndexVector[1]]))) - 1

    roiVertexIndicesStructA = aab1->getAttributeByName('roi_vertex_indices')
    roiVertexIndicesStructB = aab2->getAttributeByName('roi_vertex_indices')
    roi1indicesA = *((*roiVertexIndicesStructA.pData)[0]) & roi2indicesA = *((*roiVertexIndicesStructA.pData)[1])
    roi1indicesB = *((*roiVertexIndicesStructB.pData)[0]) & roi2indicesB = *((*roiVertexIndicesStructB.pData)[1])
    roi1indices  = [roi1indicesA, roi1indicesB] & roi1indices = roi1indices[uniq(roi1indices, sort(roi1indices))]
    roi2indices  = [roi2indicesA, roi2indicesB] & roi2indices = roi2indices[uniq(roi2indices, sort(roi2indices))]
    if roi1indices[0] eq 0 then begin
      lastPos1 = n_elements(roi1indices) - 1
      if roi1indices[lastPos1] ge (maxIndex1-deltaIndex) then $
        roi1indices = [roi1indices[1:*], 0]
    endif
    if roi2indices[0] eq 0 then begin
      lastPos2 = n_elements(roi2indices) - 1
      if roi2indices[lastPos2] ge (maxIndex2-deltaIndex) then $
        roi2indices = [roi2indices[1:*], 0]
    endif
    runs1 = makeRunFromCorrelativeValues(roi1indices, nMax = maxIndex1, delta = deltaIndex)
    runs2 = makeRunFromCorrelativeValues(roi2indices, nMax = maxIndex2, delta = deltaIndex)
    nRuns1 = n_elements(runs1) / 2
    nRuns2 = n_elements(runs2) / 2

  endelse

  nMerge = (nRuns1 eq 1) and (nRuns2 eq 1) ? 1 : 2 ; 2 is a default value, indicating one vertex index "missing" between both run extremes.

  ; Check for non-strict merging: small distance/set of points between vertex runs.
  if (nMerge eq 2) then return, [aab1, aab2]
  ;if ((nMerge eq 2) and ~keyword_set(fNonStrict)) then return, [aab1, aab2]
  roi1indices = runsToIndices(runs1, maxIndex1 + 1)
  roi2indices = runsToIndices(runs2, maxIndex2 + 1)
  ;wh0inList1 = where(roi1indices eq 0, count0inList1) ; JuanEdo code cleanup
  ;if (count0inList1 gt 0) and (nRuns1 eq 1) and (runs1[0] ne 0) and ~keyword_set(maxIndex1) then $
  ;  if (runs1[0] ne roi1indicesA[0]) then roi1indices = [roi1indicesB, roi1indicesA]
  ;wh0inList2 = where(roi2indices eq 0, count0inList2)
  ;if (count0inList2 gt 0) and (nRuns2 eq 1) and (runs2[0] ne 0) and ~keyword_set(maxIndex2)  then $
  ;  if (runs2[0] ne roi2indicesA[0]) then roi2indices = [roi2indicesB, roi2indicesA]

;fNonStrict = 0
  if (nRuns1 eq 2) or (nRuns2 eq 2) then if (nMerge eq 2) and keyword_set(fNonStrict) then begin
    maxGapDef = 2
    maxGapInd = keyword_set(maxGapInd) ? maxGapInd > maxGapDef : maxGapDef
    fLookupGaps1  = nRuns1 gt 1
    fOKMergeGaps1 = ~fLookupGaps1
    fLookupGaps2  = nRuns2 gt 1
    fOKMergeGaps2 = ~fLookupGaps2

    roiIndexVec1 = *(*((aab1->getAttributeByName('roi_index_vector')).pData[0]))
    nVert1 = n_elements(*((*(self.pXcoords)[0])[roiIndexVec1[0]]))
    nVert2 = n_elements(*((*(self.pXcoords)[0])[roiIndexVec1[1]]))

    if fLookupGaps1 then begin
      gapVec1 = [0]
      for r = 0, nRuns1-2 do begin
        runPosEnd = 2*r + 1
        ind1a = runs1[runPosEnd+1]
        ind1b = runs1[runPosEnd]
        gap = abs(ind1a - ind1b)
        if (ind1a eq 0) and (ind1b eq (nVert1-2)) then gap = 2 ; TODO correct gap for 0-crossing runs
        gapVec1 = [gapVec1, gap]
      endfor
      gapVec1 = gapVec1[1:*]
      if max(gapVec1) le maxGapInd then begin
        if self.fVerbose then print, 'Non-strict merge for runs in 1st AAB: ', runs1
        roi1indices = runsToIndices([runs1[0], runs1[3]], nVert1)
        fOKMergeGaps1 = 1
      endif
    endif

    if fLookupGaps2 then begin
      gapVec2 = [0]
      for r = 0, nRuns2-2 do begin
        runPosEnd = 2*r + 1
        ind2a = runs2[runPosEnd+1]
        ind2b = runs2[runPosEnd]
        gap = abs(ind2a - ind2b)
        if (ind2a eq 0) and (ind2b eq (nVert2-2)) then gap = 2 ; TODO correct gap for 0-crossing runs
        gapVec2 = [gapVec2, gap]
      endfor
      gapVec2 = gapVec2[1:*]
      if max(gapVec2) le maxGapInd then begin
        if self.fVerbose then print, 'Non-strict merge for runs in 2nd AAB: ', runs2
        roi2indices = runsToIndices([runs2[0], runs2[3]], nVert2) ; TODO JJW maybe expand to more than 2 runs? (also with runs1)
        fOKMergeGaps2 = 1
      endif
    endif
    if (~fOKMergeGaps1 or ~fOKMergeGaps2) then return, [aab1, aab2]
  endif

  if keyword_set(pBoundingPoly1) then begin
    unionPoly = calcPolygonUnion(p1x = (*pBoundingPoly1)[*,0], p1y = (*pBoundingPoly1)[*,1], p2x = (*pBoundingPoly2)[*,0], p2y = (*pBoundingPoly2)[*,1])
  endif else begin
    p1x = [boundingBox1[0], boundingBox1[2], boundingBox1[2], boundingBox1[0], boundingBox1[0]]
    p1y = [boundingBox1[1], boundingBox1[1], boundingBox1[3], boundingBox1[3], boundingBox1[1]]
    p2x = [boundingBox2[0], boundingBox2[2], boundingBox2[2], boundingBox2[0], boundingBox2[0]]
    p2y = [boundingBox2[1], boundingBox2[1], boundingBox2[3], boundingBox2[3], boundingBox2[1]]
    unionPoly = calcPolygonUnion(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y)
  endelse

  nUnionPolys = unionPoly[0]
  ;if (self.fVerbose gt 0) then print, unionPoly[0], ' union polygons, first has ', nVert, ' vertices'

  ; Handling of special cases of zero polygons, or two or more polygons connected by a single vertex.
  case 1 of
    (nUnionPolys eq 0): begin
      pux = [p1x, p2x]
      puy = [p1y, p2y]
      convexHullIndices = convexHullPolygon(pux, puy)
      unionPolyX = pux[convexHullIndices]
      unionPolyY = puy[convexHullIndices]
    endcase
    (nUnionPolys eq 1): begin
      nVert = unionPoly[1]
      unionPolyX = unionPoly[2:1+nVert]
      unionPolyY = unionPoly[2+nVert:1+2*nVert]
      polygonCheckAndRemoveCollinearSegments, unionPolyX, unionPolyY, pxOut = upxCorrected, pyOut = upyCorrected
      unionPolyX = upxCorrected
      unionPolyY = upyCorrected
    endcase
    else: begin
      ; If two or more polygons result from the union, get the convex hull as the enclosing polyon for all of them
      if self.fVerbose then print, 'Warning: ', nUnionPolys, ' were obtained while trying to merge bounding polygons. Replacing Union with Convex Hull...'
      nVert = unionPoly[1]
      xUnionPolyAcc = unionPoly[2:1+nVert]
      yUnionPolyAcc = unionPoly[2+nVert:1+2*nVert]
      startUpoly = 2+2*nVert
      for u = 1, nUnionPolys-1 do begin
        nVert = unionPoly[startUpoly]
        xUnionPolyAcc = [xUnionPolyAcc, unionPoly[startUpoly+1:startUpoly+nVert]]
        yUnionPolyAcc = [yUnionPolyAcc, unionPoly[startUpoly+nVert+1:startUpoly+2*nVert]]
        startUpoly += 2*nVert+1
      endfor
      convexHullIndices = convexHullPolygon(xUnionPolyAcc, yUnionPolyAcc)
      unionPolyX = xUnionPolyAcc[convexHullIndices]
      unionPolyY = yUnionPolyAcc[convexHullIndices]
    endcase
  endcase
  minX = min(unionPolyX, max = maxX)
  minY = min(unionPolyY, max = maxY)
  boundingBoxOut   = [minX, minY, maxX, maxY]
  pBoundingPolyOut = ptr_new([[unionPolyX], [unionPolyY]])
  if (self.fVerbose gt 0) then print, 'boundingBoxOut (xmin,ymin,xmax,ymax): ', boundingBoxOut

  attPosRoiIndexVector = 0
  attPosImage          = 1
  attPosImgFeatures    = 2
  attPosROIvertIndices = 3
  attPosROIvertices    = 4
  attPosBoundingBoxAbs = 5
  attPosBoundingBoxRel = 6
  attPosType           = 7
  attPosCorrIndex      = 8
  attPosCorrIndexType  = 9
  attPosCorrVertIndices = 10
  nAttribAAB = 11
  type   = 'merge'
  oMerge = obj_new('C_sAABSharedZone', fVerbose = self.fVerbose)
  oMerge->setAttributesNum, nAttribAAB

  ; Select a bounding area to get the image from, based on the bounding box of the ROI
  imagePatch = self->getImagePatch(minX, minY, maxX, maxY, xMin = xMinPatch, xMax = xMaxPatch, yMin = yMinPatch, yMax = yMaxPatch)

  ; Make the bounding boxes
  aabBoundingBoxAbs = [minX, maxX, minY, maxY] ; Form [[[xMinRoi, xMaxRoi], [yMinRoi, yMaxRoi]] means row 0: xmin, xmax / row 1: ymin, ymax
  aabBoundingBoxRel = [minX-xMinPatch, maxX-xMinPatch, minY-yMinPatch, maxY-yMinPatch]

  roiIndexVector = *(*((aab1->getAttributeByName('roi_index_vector')).pData[0]))
  roiNames = ['ROI ' + num2strTrim(roiIndexVector[0]), 'ROI ' + num2strTrim(roiIndexVector[1])]
  oMerge->addAttributeStruct, attPosRoiIndexVector, 'roi_index_vector', '2D', type, ptr_new(roiIndexVector), dataNames = roiNames
  oMerge->addAttributeStruct, attPosImage, 'image', '2D', type, ptr_new(*self.pImage), dataNames = ['image']
  oMerge->addAttributeStruct, attPosImgFeatures, 'image_feature', '2D', type, ptr_new(imagePatch, /no_copy), dataNames = ['image_feature']

  pROIvertices = ptrArr(2, 2) ; number of ROIs x number of dimensions
  pROIvertices[0,0] = ptr_new((*((*(self.pXcoords)[0])[roiIndexVector[0]]))[roi1indices]); - xMinPatch)
  pROIvertices[1,0] = ptr_new((*((*(self.pYcoords)[0])[roiIndexVector[0]]))[roi1indices]); - yMinPatch)
  pROIvertices[0,1] = ptr_new((*((*(self.pXcoords)[0])[roiIndexVector[1]]))[roi2indices]); - xMinPatch)
  pROIvertices[1,1] = ptr_new((*((*(self.pYcoords)[0])[roiIndexVector[1]]))[roi2indices]); - yMinPatch)
  oMerge->addAttributeStruct, attPosROIvertices, 'roi_vertex_list', '2D', type, pROIvertices, dataNames = roiNames

  pVertexIndices = ptrArr(2)
  pVertexIndices[0] = ptr_new(roi1indices)
  pVertexIndices[1] = ptr_new(roi2indices)
  pCorrelativeRoiVertexIndices = ptrArr(2)
  pCorrelativeRoiVertexIndices[0] = ptr_new(roi1indices, /NO_COPY)
  pCorrelativeRoiVertexIndices[1] = ptr_new(roi2indices, /NO_COPY)
  oMerge->addAttributeStruct, attPosROIvertIndices, 'roi_vertex_indices', '2D', type, pVertexIndices, dataNames = roiNames
  oMerge->addAttributeStruct, attPosCorrVertIndices, 'correlative_roi_vertex_indices', '2D', type, pCorrelativeRoiVertexIndices, dataNames = roiNames
  oMerge->addAttributeStruct, attPosBoundingBoxAbs, 'axis_bounding_box_absolute', '2D', type, ptr_new(aabBoundingBoxAbs), dataNames = ['axis_bounding_box_absolute']
  oMerge->addAttributeStruct, attPosBoundingBoxRel, 'axis_bounding_box_relative', '2D', type, ptr_new(aabBoundingBoxRel), dataNames = ['axis_bounding_box_relative']
  oMerge->addAttributeStruct, attPosType, 'paired_aab_type_list', '2D', type, ptr_new([type]), dataNames = roiNames
  ; TODO -1 JJW in the correlative order indicates "not set" => update by calling the AAB sorting method
  oMerge->addAttributeStruct, attPosCorrIndex, 'correlative_AAB_index', '2D', type, ptr_new([-1]), dataNames = ['correlative_AAB_index']
  oMerge->addAttributeStruct, attPosCorrIndexType, 'correlative_AAB_index_type', '2D', type, ptr_new(['merge']), dataNames = ['correlative_AAB_index_type']

;  if (self.fVerbose gt 0) then $
;    for sp = attPosROIvertices, nAttribAAB-1 do oMerge->printAttributeStruct, structPos = sp

  return, [oMerge]
end


function C_sAABContainer::splitPairedAABForCorner
  return, 0
end


pro C_sAABContainer::mergeBoundingGeometries, boundingBoxList = boundingBoxList, boundingBoxOut = boundingBoxOut, $
                                              pBoundingPolyIn = pBoundingPolyIn, pBoundingPolyOut = pBoundingPolyOut
  nBoxes  = n_elements(boundingBoxList) / 4
  fBBoxes = nBoxes gt 0
  nPolys  = n_elements(pBoundingPolyIn)
  fBPolys = nPolys gt 0
  nObj    = fbBoxes ? nBoxes : nPolys

  if (nObj eq 0) then return

  fStoreUnionPolygon = arg_present(pBoundingPolyOut)
  fBBoxesOK = 0

  if (fBPolys gt 0) then begin
    unionPolyX = (*pBoundingPolyIn[0])[*,0]
    unionPolyY = (*pBoundingPolyIn[0])[*,1]
    outXmin    = min(unionPolyX, max = outXmax)
    outYmin    = min(unionPolyY, max = outYmax)
    fBBoxesOK  = 1

    for i = 1L, nObj-1 do begin
      maxupx = max((*pBoundingPolyIn[i])[*,0], min = minupx)
      maxupy = max((*pBoundingPolyIn[i])[*,1], min = minupy)
      dx = maxupx - minupx
      dy = maxupy - minupy

      outXmin <= minupx
      outXmax >= maxupx
      outYmin <= minupy
      outYmax >= maxupy

      if (dx eq 0) or (dy eq 0) then begin
        print, 'A polygon to perform union with is a line!: ', (*pBoundingPolyIn[i])[*,0], '-', (*pBoundingPolyIn[i])[*,1]
        stop
        goto, chk
      endif
      unionPoly = calcPolygonUnion(p1x = unionPolyX, p1y = unionPolyY, p2x = (*pBoundingPolyIn[i])[*,0], p2y = (*pBoundingPolyIn[i])[*,1])
      nVert     = unionPoly[1]
      ;if (self.fVerbose gt 0) then print, unionPoly[0], ' union polygons, first has ', nVert, ' vertices'
      unionPolyX = unionPoly[2:1+nVert]
      unionPolyY = unionPoly[2+nVert:1+2*nVert]
chk:  polygonCheckAndRemoveCollinearSegments, unionPolyX, unionPolyY, pxOut = upxCorrected, pyOut = upyCorrected
      unionPolyX = upxCorrected
      unionPolyY = upyCorrected
    endfor

    boundingBoxOut = [outXmin, outYmin, outXmax, outYmax]
    if fStoreUnionPolygon then pBoundingPolyOut = ptr_new([[unionPolyX], [unionPolyY]])
  endif

  if (fBBoxes and ~fBBoxesOK) then begin

    outXmin = boundingBoxList[0]
    outYmin = boundingBoxList[1]
    outXmax = boundingBoxList[2]
    outYmax = boundingBoxList[3]

    if (fCalcUnionPolygon ne 0) then begin
      unionPolyX = [outXmin, outXmax, outXmax, outXmin]
      unionPolyY = [outYmin, outYmin, outYmax, outYmax]
    endif

    for i = 1L, nObj-1 do begin

      bbxmin = boundingBoxList[4*i]
      bbymin = boundingBoxList[4*i+1]
      bbxmax = boundingBoxList[4*i+2]
      bbymax = boundingBoxList[4*i+3]
      outXmin <= bbxmin
      outYmin <= bbymin
      outXmax >= bbxmax
      outYmax >= bbymax

      if fStoreUnionPolygon then begin
        curbbX = [bbxmin, bbxmax, bbxmax, bbxmin]
        curbbY = [bbymin, bbymin, bbymax, bbymax]
        unionPoly = calcPolygonUnion(p1x = unionPolyX, p1y = unionPolyY, p2x = curbbX, p2y = curbbY)
        nVert = unionPoly[1]
        ;if (self.fVerbose gt 0) then print, unionPoly[0], ' union polygons, first has ', nVert, ' union vertices'
        unionPolyX = unionPoly[2:1+nVert]
        unionPolyY = unionPoly[2+nVert:1+2*nVert]
      endif
    endfor
    boundingBoxOut = [outXmin, outYmin, outXmax, outYmax]
    if fStoreUnionPolygon then pBoundingPolyOut = ptr_new([[unionPolyX], [unionPolyY]])
  endif

end


; Not currently in use
function C_sAABContainer::getVertexRunsByRois, aabList, roiIndexVectorUnique = roiIndexVectorUnique, pRunsDistance = pRunsDistance
  nAABs = n_elements(aabList)
  if (nAABs lt 1) then return, [-1]
  if n_elements(roiIndexVectorUnique) eq 0 $
  then nRois = getNumberOfUniqueROIsFromAABlist(roiIndexVectorUnique = roiIndexVectorUnique)
  pRunLists  = ptrArr(nRois)
  for i = 0, nAABs-1 do begin
    roiIndexVectorStruct     = aabList[i]->getAttribureByName('roi_index_vector')
    roiIndexVector           = *(roiIndexVectorStruct.pData[0])
    roiVertexIndexListStruct = aabList[i]->getAttribureByName('roi_vertex_indices')
    roiVertexIndexList       = *(roiVertexIndexListStruct.pData[0])
    for j = 0, n_elements(roiIndexVector)-1 do begin
      whereIndexJ  = where(roiIndexVectorUnique eq roiIndexVector[j], countJ)
      if (j eq 0) then continue
      roiIndexPos  = whereIndexJ[0]
      fNewPtr      = ~ptr_valid(pRunLists[roiIndexPos])
      runList      = getRunNew
      pRunLists[j] = ptr_new(fNewPtr ? runList : [getRunOld, runList])
    endfor
  endfor

  ; If required, compute the "index gap" distance between runs as an indirect proximity measurement
  if arg_present(pRunsDistance) then begin
    pRunLists = ptrArr(nRois)
    for i = 0, nRois-1 do begin
      nRuns = n_elements(*pRunLists[i]) / 2
      if (nRuns le 1) then continue
      distArr = intArr(nRuns-1)
      for j = 0, nRuns-2 do begin
        runPos = 2*j
        fBack  = 0
        distArr[j] = 0
      endfor
      pRunsDistance[i] = ptr_new(distArr, /NO_COPY)
    endfor
  endif

  return, pRunLists
end


; Pre-conditions
; number of AABs greater than 2
; mergeIter   Used for debug plots... it could be deleted after I'm done with it.
; fMergeOut   Indicates that the input AABs were effectively merged (caller method would need to update object references or something like that ;).
function C_sAABContainer::mergeAABsForCorner, aabArr, boundingBoxList = boundingBoxList, boundingBoxOut   = boundingBoxOut,$
                                              pBoundingPolyIn         = pBoundingPolyIn, pBoundingPolyOut = pBoundingPolyOut, fMergeOut = fMergeOut, $
                                              roiIndexVectorUnique    = roiIndexVectorUnique, pairedAABsIdVector = pairedAABsIdVector, mergeIter = mergeIter, $
                                              cornerBoundingBoxOut    = cornerBoundingBoxOut
  nAABs = n_elements(aabArr)
  if (nAABs le 1) then return, -1
  fMergeOut = 0
  if (nAABs gt 2) then stop ; Has not been tested... careful.
  if (nAABs eq 2) then begin
    if (aabArr[0] eq aabArr[1]) then begin
      print, 'mergeAABsForCorner: input AABs are the same object!. Returning a single instance...'
      pBoundingPolyOut = [pBoundingPolyOut[0]]
      boundingBoxOut   = boundingBoxOut[0:3]
      stop
      return, aabArr[0]
    endif
    if (((aabArr[0]->getAttributeByPosition(0)).aabType eq 'corner') $
    and ((aabArr[1]->getAttributeByPosition(0)).aabType eq 'corner')) then begin
      corners0 = **((aabArr[0]->getAttributeByName('paired_AABs')).pData[0])
      corners1 = **((aabArr[1]->getAttributeByName('paired_AABs')).pData[0])
      nc0 = n_elements(corners0)
      nc1 = n_elements(corners1)
      if (nc0 eq nc1) and (nc0 gt 0) then begin
        fAllFrom0in1 = 1b
        matchCount   = 0u
        for i = 0u, nc0-1 do begin
          whIn1 = whereObjInObjArray(corners0[i], corners1)
          if (whIn1 eq -1) then fAllFrom0in1 = 0b else begin
            matchCount += 1
            if self.fVerbose then print, 'mergeAABsForCorner: paired AAB ', i, ' from corner 0 found in corner 1'
          endelse
        endfor
        if fAllFrom0in1 then begin
          print, 'mergeAABsForCorner: different object instances but same content (pairedAABs). Returning the first AAB...'
          fMergeOut = 1
          pBoundingPolyOut = [pBoundingPolyOut[0]]
          boundingBoxOut   = boundingBoxOut[0:3]
          return, aabArr[0]
        endif
      endif
    endif
  endif
;  fPlotFigure = 1
;  if (self.fVerbose and fPlotFigure) then begin
;    window, title = 'AAB-corner detection for ROIs ' + string(roiIndexVectorUnique), xSize = xSz, ySize = ySz
;    tvscl, congrid(imagePatch, xSz, ySz)
;    for i = 1L, nAABs-1 do begin
;      attVertexCoords = aabArr[i]->getAttributeByName('ToDo')
;    endfor
;  endif

  ; Look for an intersection between bounding polygons. If there is any then 
  ; make a corner bounding box from it. Also, if there is a "corner type" AAB 
  ; as input, there should be a corner bounding box to try.
  ; TODO JJ note that the output change if only bounding polygons are used, instead of bboxes which make up for a larger corner neighborhood 
  if ((aabArr[0]->getAttributeByPosition(0)).aabType eq 'corner') then begin
    attCornerBBox = (aabArr[0]->getAttributeByName('corner_bounding_box'))
    stop
    bboxCorner = *attCornerBBox.pData[0]
    polyIntX = [bboxCorner[0], bboxCorner[2], bboxCorner[2], bboxCorner[0]]
    polyIntY = [bboxCorner[1], bboxCorner[1], bboxCorner[3], bboxCorner[3]]
  endif else begin
    polyIntX = (*pBoundingPolyIn[0])[*,0]
    polyIntY = (*pBoundingPolyIn[0])[*,1]
  endelse
  for i = 1u, nAABs-1 do begin
    if ((aabArr[i]->getAttributeByPosition(0)).aabType eq 'corner') then begin
      attCornerBBox = (aabArr[i]->getAttributeByName('corner_bounding_box'))
      stop
      bboxCorner = *attCornerBBox.pData[0]
      pAABx = [-1]
      pAABx = [-1]
    endif else begin
      pAABx = (*pBoundingPolyIn[i])[*,0]
      pAABy = (*pBoundingPolyIn[i])[*,1]
    endelse
    outInters = calcPolygonIntersection(p1x = polyIntX, p1y = polyIntY, p2x = pAABx, p2y = pAABy) 
     ; bounding polygons are convex, so no multiple intersections are expected.
    if (outInters[0] eq 0) then begin
      boundingBoxOut   = boundingBoxList
      pBoundingPolyOut = pBoundingPolyIn
      return, aabArr
    endif
    nptsInt  = outInters[1]
    polyIntX = outInters[2:2+nptsInt-1]
    polyIntY = outInters[2+nptsInt:2+2*nptsInt-1]
  endfor
  maxX = max(polyIntX, min = minX)
  maxY = max(polyIntY, min = minY)
  cornerBoundingBoxOut = [minX, minY, maxX, maxY]

  cornerAABindices    = [-1]
  nonCornerAABindices = [-1]
  ; Retrieve ROI indices for keeping them in the merged object list
  roiIndexVectorAll   = [-1]

  for i = 0L, nAABs-1 do begin
    aabType = (aabArr[i]->getAttributeByPosition(0)).aabType
    if (aabType eq 'corner') $
    then cornerAABindices    = [cornerAABindices, i] $
    else nonCornerAABindices = [nonCornerAABindices, i]
    attRoiIndexVector        = aabArr[i]->getAttributeByName('roi_index_vector')
    outType = size(attRoiIndexVector, /TYPE)
    if (outType eq 2) or (aabType eq 'corner') $ ; If this is a corner object, try with the corresponding attribute
    then attRoiIndexVector = aabArr[i]->getAttributeByName('roiIndexVectorUnique')
    roiIndexVectorAll      = [roiIndexVectorAll, **(attRoiIndexVector.pData[0])]
  endfor

  nCornerAABs    = n_elements(cornerAABindices)-1
  nNonCornerAABs = n_elements(nonCornerAABindices)-1
  nAABsToMerge   = nCornerAABs + nNonCornerAABs
  if (nCornerAABs gt 0) then cornerAABindices = cornerAABindices[1:*]

  fAllCorners = nCornerAABs eq nAABs
  if fAllCorners then begin
    corneredAABlist = objArr(1)
    for i = 0u, nAABs-1 do $
      corneredAABlist = [corneredAABlist, **((aabArr[i]->getAttributeByName('paired_AABs')).pData[0])]
    uniqObjVector   = removeDuplicatesFromObjArray(corneredAABlist[1:*])
    corneredAABlist = corneredAABlist[1 + uniqObjVector]
    stop; return
  endif

  roiIndexVectorAll    = roiIndexVectorAll[1:*]
  if n_elements(roiIndexVectorUnique) eq 0 then $
  roiIndexVectorUnique = roiIndexVectorAll[uniq(roiIndexVectorAll, sort(roiIndexVectorAll))]
  nUniqueRois          = n_elements(roiIndexVectorUnique)
  roiNames             = strArr(nUniqueRois)
  for i = 0u, nUniqueRois-1 do roiNames[i] = 'ROI' + strCompress(string(roiIndexVectorUnique[i]), /REMOVE_ALL)

  ; Retrieve/Store vertex-index runs...
  pVertexRuns = ptrArr(nUniqueRois)

  ; ...first from the corner AABs...
  for i = 0L, nCornerAABs-1 do begin

    attPvertexRunsI = aabArr[cornerAABindices[i]]->getAttributeByName('p_vertex_runs')
    roiIndicesUniqI = **((aabArr[cornerAABindices[i]]->getAttributeByName('roiIndexVectorUnique')).pData[0])
    pVertexRunsI    = *attPvertexRunsI.pData[0]

    for j = 0u, n_elements(pVertexRunsI)-1 do begin
      indexRoiIndexVectorUnique = (where(roiIndexVectorUnique eq roiIndicesUniqI[j]))[0]
      rivVec = runsToIndices(*pVertexRunsI[j], n_elements(*(*self.pXcoords[0])[roiIndicesUniqI[j]])-1)
      if ptr_valid(pVertexRuns[indexRoiIndexVectorUnique]) then begin
        tmp = *pVertexRuns[indexRoiIndexVectorUnique]
        ptr_free, pVertexRuns[indexRoiIndexVectorUnique]
        pVertexRuns[indexRoiIndexVectorUnique] = ptr_new([rivVec, tmp])
      endif else pVertexRuns[indexRoiIndexVectorUnique] = ptr_new(rivVec)
    endfor
  endfor

  ; ...and then from the non-corner AABs...
  for i = 0L, nNonCornerAABs-1 do begin

    attRoiVertexIndices = aabArr[i]->getAttributeByName('roi_vertex_indices')
    nRoisI      = n_elements(*(attRoiVertexIndices.pData[0]))
    roiIndicesI = **((aabArr[i]->getAttributeByName('roi_index_vector')).pData[0])
    roiIndexIJ  = uLonArr(nRoisI)
    for j = 0u, nRoisI-1 do $
      roiIndexIJ[j]  = (where(roiIndexVectorUnique eq roiIndicesI[j]))[0]
    for j = 0u, nRoisI-1 do begin
      vertexIndicesJ = *(*attRoiVertexIndices.pData)[j]
      if ptr_valid(pVertexRuns[roiIndexIJ[j]]) then begin
        tmp = *pVertexRuns[roiIndexIJ[j]]
        ptr_free, pVertexRuns[roiIndexIJ[j]]
        pVertexRuns[roiIndexIJ[j]] = ptr_new([tmp, vertexIndicesJ])
      endif else $
        pVertexRuns[roiIndexIJ[j]] = ptr_new(vertexIndicesJ)
    endfor
  endfor
;stop
  ; ...to finally make a merged list of vertex runs.
  for i = 0u, nUniqueRois-1 do begin
    vertexIndicesI = *pVertexRuns[i]
    sortedIndicesI = vertexIndicesI[uniq(vertexIndicesI, sort(vertexIndicesI))]
    ptr_free, pVertexRuns[i]
    runs = makeRunFromCorrelativeValues(sortedIndicesI, nMax = n_elements(*(*self.pXcoords[0])[roiIndexVectorUnique[i]])-1)
    pVertexRuns[i] = ptr_new(runs, /no_copy)
  endfor

  ; Gather the non-corner-merged AABs
  if (nNonCornerAABs ge 1) then begin
    nonCornerAABlist = objArr(nNonCornerAABs)
    for i = 0u, nNonCornerAABs-1 do $
      nonCornerAABlist[i] = aabArr[nonCornerAABindices[i+1]]
  endif

  ; Extract the paired AABs from the each corner-merge attribute
  if (~fAllCorners and (nCornerAABs ge 1)) then begin
;stop
    corneredAABlist = objArr(1)
    for i = 0u, nCornerAABs-1 do begin
      corneredAABlist = [corneredAABlist, *(*((aabArr[cornerAABindices[i]]->getAttributeByName('paired_AABs')).pData)[0])]
      ;pairedAABsIdVecI = *(*((aabArr[cornerAABindices[i]]->getAttributeByName('paired_AABs_id_vector'))
      ; TODO clean? this
      ;obj_destroy, aabArr[cornerAABindices[i]], /fKeepInternalObjects
    endfor
    corneredAABlist = corneredAABlist[1:*]
  endif

  case 1 of
    (nCornerAABs ge 1) and (nNonCornerAABs ge 1): pairedList = [corneredAABlist, nonCornerAABlist]
    (nCornerAABs ge 1) and (nNonCornerAABs lt 1): pairedList = [corneredAABlist]
    (nCornerAABs lt 1) and (nNonCornerAABs ge 1): pairedList = [nonCornerAABlist]
    else: stop ; Should not happen!
  endcase

  ; Remove duplicated AABs from the paired list
  nPairedAABs          = n_elements(pairedList)
  uniqPairedAABindices = removeDuplicatesFromObjArray(pairedList)
  if (n_elements(uniqPairedAABindices) lt nPairedAABs) then pairedList = pairedList[uniqPairedAABindices]
  nRois = n_elements(roiIndexVectorUnique)

  if (nRois eq 0) then nRois = getNumberOfUniqueROIsFromAABlist(aabArr, roiIndexVectorUnique = roiIndexVectorUnique)
  if (nRois le 2) then stop ; This should not occur!
  if (self.fVerbose gt 0) then print, 'roiIndexVectorUnique: ', roiIndexVectorUnique
  pCompleteRoiPolygons = ptrArr(nUniqueRois)
  for j = 0u, nUniqueRois-1 do $
    pCompleteRoiPolygons[j] = ptr_new([[*((*self.pXcoords[0])[roiIndexVectorUnique[j]])], [*((*self.pYcoords[0])[roiIndexVectorUnique[j]])]])

  self->mergeBoundingGeometries, boundingBoxList = boundingBoxList, boundingBoxOut   = boundingBoxOut,$
                                 pBoundingPolyIn = pBoundingPolyIn, pBoundingPolyOut = pBoundingPolyOut
  boundingBoxRel = [0.0, 0.0, boundingBoxOut[2]-boundingBoxOut[0], boundingBoxOut[3]-boundingBoxOut[1]]

  ; Get the image patch for the (large) intersecting zone containing the corner.
  ; From getImagePatch, it shouldn't be necessary to get the corrected x|y min/max when merging (already done before)
  imagePatch = self->getImagePatch(boundingBoxOut[0], boundingBoxOut[1], boundingBoxOut[2], boundingBoxOut[3])

  ; Optional section for saving image patches... FiDeO material ;)
  fSaveImagePatch = 1b
  if fSaveImagePatch then begin
    self->getFolderFileNamePrefix, prefix, baseFileName, folderPath
    pathSep         = path_sep()
    posLastSep      = strPos(self.roiGroupFilePath, pathSep, /reverse_search)
    folderPath      = strMid(self.roiGroupFilePath, 0, posLastSep) + pathSep + '_AAB_' + pathSep + '_cornerImagePatches_' + pathSep
    if ~file_test(folderPath, /DIRECTORY) then file_mkDir, folderPath
    aabsAABIdSuffix = ''
    for j = 0u, nAABs-2 do aabsAABIdSuffix += (num2strTrim(pairedAABsIdVector[j]) + '-')
    aabsAABIdSuffix += num2strTrim(pairedAABsIdVector[nAABs-1])
    aabsRoiIdSuffix  = strCompress(strJoin(string(roiIndexVectorUnique), '-'))
    imagePatchFileNameBase = (keyword_set(mergeIter) ? 'mergeIter' + num2strTrim(mergeIter) : '') + prefix + baseFileName + 'cornerForROIs' + aabsRoiIdSuffix + '_cornerForAABs_' + aabsAABIdSuffix + '.png'
    write_png, folderPath + imagePatchFileNameBase, imagePatch
  endif
;stop
  ; Create merge object, fill attributes
  attPosRoiIdVector       = 0
  attPosRoiIdVectorUnique = 1
  attPosImage             = 2
  attPosPAABs             = 3
  attPosCompleteRoiPolys  = 4
  attPosPvertexRuns       = 5
  attPosPairedAABsIdVec   = 6
  attPosCornerBBox        = 7
  nAttrib = 8
  aabType = 'corner'

  oCornerAAB = obj_new('C_sAABSharedZone', fVerbose = self.fVerbose)
  oCornerAAB->setAttributesNum, nAttrib
  oCornerAAB->addAttributeStruct, attPosRoiIdVector, 'roi_index_vector', '2D', aabType, ptr_new(roiIndexVectorAll, /no_copy), dataNames = roiNames
  oCornerAAB->addAttributeStruct, attPosRoiIdVectorUnique, 'roiIndexVectorUnique', '2D', aabType, ptr_new(roiIndexVectorUnique, /no_copy), dataNames = roiNames
  oCornerAAB->addAttributeStruct, attPosImage, 'image', '2D', aabType, ptr_new(imagePatch, /no_copy), dataNames = ['image_patch']
  oCornerAAB->addAttributeStruct, attPosPAABs, 'paired_AABs', '2D', aabType, ptr_new(pairedList), dataNames = roiNames
  oCornerAAB->addAttributeStruct, attPosCompleteRoiPolys, 'completeRoiPolygons', '2D', aabType, ptr_new(pCompleteRoiPolygons), dataNames = roiNames
  oCornerAAB->addAttributeStruct, attPosPvertexRuns, 'p_vertex_runs', '2D', aabType, pVertexRuns, dataNames = roiNames
  oCornerAAB->addAttributeStruct, attPosPairedAABsIdVec, 'paired_AABs_id_vector', '2D', aabType, pairedAABsIdVector, dataNames = roiNames
  oCornerAAB->addAttributeStruct, attPosCornerBBox, 'corner_bounding_box', '2D', aabType, pairedAABsIdVector, dataNames = roiNames
  if (fAllCorners or (nAABs eq 2)) then begin
    fMergeOut = 1
    aabArr[0] = oCornerAAB
    aabArr[1] = oCornerAAB
    boundingBoxOut   = [boundingBoxOut, boundingBoxOut]
    pBoundingPolyOut = [pBoundingPolyOut, pBoundingPolyOut]
  endif
  return, oCornerAAB

end


function C_sAABContainer::computeAABcontourVertexTypes
  if (self.numContours lt 1) then return, -1

  self.pAABvertexType = ptr_new(ptrArr(self.numContours)) ; contains the color code for the ROI vertices
  for i = 0L, self.numContours-1 do begin
    nPts = n_elements(*((*(self.pXcoords)[0])[i]))
    if (self.fVerbose gt 0) then print, 'Initializing pAABvertexType for contour ', i, ' with ', nPts, ' points.'
      ; Make array of aabm vertex type 0 (no adjacent contours at the beginning).
    (*(self.pAABvertexType)[0])[i] = ptr_new(bytArr(nPts))
  endfor

  return, self->checkAABsForAllRois()
end


function C_sAABContainer::checkAABsForROIpair, roiIndex1, pxCoords1, pyCoords1, roiIndex2, pxCoords2, pyCoords2, AABlist = AABlist, $
                                               aabzoneBoundingBoxes = aabzoneBoundingBoxes, aabBoundingPolys = aabBoundingPolys, $
                                               fFillZoneGaps = fFillZoneGaps, FillZoneGapIndex = FillZoneGapIndex, FillZoneGapDist = FillZoneGapDist

  if (self.fVerbose gt 0) then print, 'C_sAABContainer::checkAABsForROIpair Starting...'

  ; 1. Intersection computation
  ; 1.1 Get polygons from the intersection (if any) between rois I and J
  ovrPolys = calcPolygonIntersectionWithPolyIndices(p1x = *pxCoords1, p1y = *pyCoords1, p2x = *pxCoords2, p2y = *pyCoords2,$
                                                    pVertexIndices = pVertexIndices, endpoint1code = ep1code, endpoint2code = ep2code,$
                                                    boundingBoxX = boundingBoxXovr, boundingBoxY = boundingBoxYovr, polyAreas = polyAreas,$
                                                    fVerbose = 0, backImage = *self.pImage, /fForcePointsForUniqueIntersection)
  ; Intersection polygon total count
  nOvrPolys = round(ovrPolys[0])
  if (self.fVerbose gt 0) then print, 'Found ', nOvrPolys, ' intersection polygons'
  ; Get the vertex count from rois I and J
  ni = n_elements(*pxCoords1)
  nj = n_elements(*pxCoords2)

  ; 1.2a Get vertices from roi I that are not-inside roi J.
  containedInJ = [0]
  containedInI = [0]
  if (nOvrPolys gt 0) then begin
    ovrVerticesI = [-1]
    oRoiJ = obj_new('IDLanROI', *pxCoords2, *pyCoords2)
    containedInJ    = oRoiJ->containsPoints(*pxCoords1, *pyCoords1)
    whereNotInsideJ = where(containedInJ ne 1, nNotInsideJ, COMPLEMENT = whereInsideJ, NCOMPLEMENT = nInsideJ)
    if (nInsideJ gt 0) then ovrVerticesI = whereInsideJ
    sortedIndicesI  = sort(OvrVerticesI)
    obj_destroy, oRoiJ

    ; 1.2b Get vertices from roi J that are not-inside roi I.
    OvrVerticesJ = [-1]
    oRoiI = obj_new('IDLanROI', *pxCoords1, *pyCoords1)
    containedInI    = oRoiI->containsPoints(*pxCoords2, *pyCoords2)
    whereNotInsideI = where(containedInI ne 1, nNotInsideI, COMPLEMENT = whereInsideI, NCOMPLEMENT = nInsideI)
    if (nInsideI gt 0) then ovrVerticesJ = whereInsideI
    sortedIndicesJ  = sort(OvrVerticesJ)
    obj_destroy, oRoiI

    ; Check if one of the ROIs is enclosed by the other
    fIinJ = (nInsideJ gt 0) and (nNotInsideJ eq 0)
    fJinI = (nInsideI gt 0) and (nNotInsideI eq 0)
    if (fIinJ or fJinI) then begin
      print, 'ROI ', fIinJ ? roiIndex1 : roiIndex2, ' contained within ', fIinJ ? roiIndex2 : roiIndex1, '... no AABs made'
      return, 0
    endif

    ; 1.Color
    self->updateVertexCodeVector, roiIndex1, ovrVerticesI, 2b
    self->updateVertexCodeVector, roiIndex2, ovrVerticesJ, 2b

  endif else begin ; No intersection means that all of the points are not inside (they still may touch the boundary)
    whereNotInsideI = indGen(nj)
    whereNotInsideJ = indGen(ni)
    nNotInsideI = nj
    nNotInsideJ = ni
  endelse

  ; 2 Proximity Computations

  ; 2.1a Get vertices from roi I that are not-inside roi J but lie in proximity range.
  nRunsIJ = 0
  nSegmentsInRangeIJ = 0
  if (nNotInsideJ gt 0) then begin
    distIJ = distanceBetweenSegments(*pxCoords1, *pyCoords1, *pxCoords2, *pyCoords2,$
                                     distanceThreshold = self.proximityDist, whereInThreshold = whereInRangeIJ,$
                                     proximityCorrespondences = proxArrayIJ, crossingList12 = crossingListIJ, /fCircular, vectorIn = containedInJ)

    nSegmentsInRangeIJ = whereInRangeIJ[0] eq -1 ? 0 : n_elements(whereInRangeIJ)
    ;nSegmentsInRangeIJ = findNeighborSegmentsInPairOfPolygons(*pxCoords1, *pyCoords1,$
    ;  *pxCoords2, *pyCoords2, self.proximityDist, runsIJ_1, runsIJ_2, distIJ = distIJ, fMakePlot = self.fVerbose)
    if (self.fVerbose gt 0) then print, nSegmentsInRangeIJ, ' segments from ', roiIndex1, ' to ', roiIndex2, ' within threshold distance (', self.proximityDist, ')'
    if (nSegmentsInRangeIJ gt 0) then begin

      distJI = distanceBetweenSegments(*pxCoords2, *pyCoords2, *pxCoords1, *pyCoords1,$
                                       distanceThreshold = self.proximityDist, whereInThreshold = whereInRangeJI,$
                                       proximityCorrespondences = proxArrayJI, crossingList12 = crossingListJI, /fCircular, vectorIn = cii)
      nRunsIJ = findRunsFromCorrespondences(proxArrayIJ, runListIprox, runListJprox, maxIndexA = ni-1, maxindexB = nj-1)

      if (self.fVerbose gt 0) then print, nRunsIJ, ' proximity runs for ROIs', roiIndex1, '-', roiIndex2,' (threshold distance =', self.proximityDist, ')'
      ; TMP, select a bounding area to get a display image from, based on the vertex positions 
      ;xMinROI = min(*pxCoords1, max = xMaxROI)
      ;yMinROI = min(*pyCoords1, max = yMaxROI)
      ;xMinROI <= min(*pxCoords2, max = xMaxTmp)
      ;yMinROI <= min(*pyCoords2, max = yMaxTmp)
      ;xMaxROI >= xMaxTmp
      ;yMaxROI >= yMaxTmp
      ;; Get image cut and cut-coordinates (with some extra padding)
      ;imagePatch = self->getImagePatch(xMinRoi, yMinRoi, xMaxRoi, yMaxRoi, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax)
      ;makePolygonsPlot, *pxCoords1, *pyCoords1, *pxCoords2, *pyCoords2, backgroundImage = *self.pImage, $
      ;                  winID = 22, winTitle = 'Proximity Plot: ROIs' + string(roiIndex1) + '-' + string(roiIndex2), /fCropImage
    endif
  endif
; Flag for the quick & dirty (sort of) overlap handling.
fSkipOvrPruning = 1
  if (nSegmentsInRangeIJ gt 0) then begin
     ; Not used... only if segments indices are given
    ;proximityVerticesIJ = [-1]
    ;for s = 0, n_elements(whereInRangeIJ)-1 do $
    ;  proximityVerticesIJ = [proximityVerticesIJ, whereInRangeIJ[s], ((whereInRangeIJ[s]+1) mod ni)]
    ;proximityVerticesIJ   = proximityVerticesIJ[uniq(proximityVerticesIJ)]
    ;if (nOvrPolys gt 0) then proximityVerticesIJ = removeFromArray(proximityVerticesIJ[1:*], ovrVerticesI)
    if ~fSkipOvrPruning and (nOvrPolys gt 0) $
    then proximityVerticesIJ = removeFromArray(whereInRangeIJ, ovrVerticesI) $
    else proximityVerticesIJ = whereInRangeIJ
    if (proximityVerticesIJ[0] eq -1) then stop ;proximityVerticesIJ = proximityVerticesIJ[1:*]
    self->updateVertexCodeVector, roiIndex1, proximityVerticesIJ, 1b
  endif

  if (nNotInsideI gt 0) and (nSegmentsInRangeIJ gt 0) then begin
    nSegmentsInRangeJI = whereInRangeJI[0] eq -1 ? 0 : n_elements(whereInRangeJI)
    if (self.fVerbose gt 0) then print, nSegmentsInRangeJI, ' segments from ', roiIndex2, ' to ', roiIndex1, ' within threshold distance (', self.proximityDist, ')'
  endif else begin
    nSegmentsInRangeJI = 0
  endelse
  if (nSegmentsInRangeJI gt 0) then begin
     ; Not used... only if segments indices are given
;    proximityVerticesJI = [-1]
;    for s = 0, n_elements(whereInRangeJI)-1 do $
;      proximityVerticesJI = [proximityVerticesJI, whereInRangeJI[s], ((whereInRangeJI[s]+1) mod nj)]
;    proximityVerticesJI   = proximityVerticesJI[uniq(proximityVerticesJI)]
;    if (nOvrPolys gt 0) then proximityVerticesJI = removeFromArray(proximityVerticesJI[1:*], ovrVerticesJ)
    if ~fSkipOvrPruning and (nOvrPolys gt 0) $
    then proximityVerticesJI = removeFromArray(whereInRangeJI, ovrVerticesJ) $
    else proximityVerticesJI = whereInRangeJI
    if (proximityVerticesJI[0] eq -1) then stop ;proximityVerticesJI = proximityVerticesJI[1:*]
    self->updateVertexCodeVector, roiIndex2, proximityVerticesJI, 1b
  endif

  if (self.fVerbose gt 0) then begin
    print, 'ROI ', roiIndex1
    print, 'ovrVerticesI ', (nOvrPolys gt 0) ? ovrVerticesI : 'none'
    print, 'proximityVerticesIJ ', (nSegmentsInRangeIJ gt 0) ? proximityVerticesIJ : 'none'
    print, 'ROI ', roiIndex2
    print, 'ovrVerticesJ ', (nOvrPolys gt 0) ? ovrVerticesJ : 'none'
    print, 'proximityVerticesJI ', (nSegmentsInRangeJI gt 0) ? proximityVerticesJI : 'none'
  endif

  ; 3. Assemble AAB objects
h:aabCountOvr   = nOvrPolys
  aabCountProx  = nRunsIJ
  aabCountTotal = aabCountOvr + aabCountProx ; Preliminary estimation only!

  if (aabCountTotal eq 0) then begin
    print, 'C_sAABContainer::checkAABsForROIpair finished - No adjacency found for ROIs ', roiIndex1, '-', roiIndex2
    return, aabCountTotal
  endif

  AABlist              = objArr(1)
  aabzoneBoundingBoxes = [-1]
  aabBoundingPolys     = ptrArr(1)
  roiNames = ['ROI ' + num2strTrim(roiIndex1), 'ROI ' + num2strTrim(roiIndex2)]

  ; TODO JJW Plot AAB pairs/merge. Variables here.
  fPlot      = 1 and self.fVerbose
  fSaveImg   = 1b ; For writing image files to drive.
  aabbbID    = 11
  aabbbTitle = 'AAB-Zone Bounding Boxes (Before Pairwise Merge) : ROIs ' + num2strTrim(roiIndex1) + '-' + num2strTrim(roiIndex2)
  imgFolderName = '_img_'

  ; Process overlapping segments to create AAB zones.
  if fSkipOvrPruning then begin;aabCountOvr = 0
    ; TODO JJW The quick & dirty way... it works when no ROI vertices are found in overlap polygons, but within threshold distance.
    if (self.fVerbose gt 0) and fPlot then begin
      makePolygonsPlot, *pxCoords1, *pyCoords1, *pxCoords2, *pyCoords2, /fNoAxes, $
                        backgroundImage = *self.pImage, winID = aabbbID, winTitle = aabbbTitle, /fCropImage, xPos = xPos, $
                        xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, sizeFactorOut = plotSizeFactor, sizeFactorWin = 50
    endif

    if (aabCountOvr gt 0) then begin
      oAABlistOvr  = objArr(aabCountOvr)
      ovrPolyStart = 2
      p = 0
      repeat begin
        pVertices = ptrArr(2)
        pVertices[0] = pVertexIndices[0, p]
        pVertices[1] = pVertexIndices[2, p]
        nv0 = n_elements(*pVertices[0])
        nv1 = n_elements(*pVertices[1])
        ovrPolyVertexCount = ovrPolys[ovrPolyStart-1]
        if (nv0 eq 1) or (nv1 eq 1) then if ([*pvertices[0]] eq -1) or ([*pvertices[1]] eq -1) then goto, ovry

        pX = ovrPolys[ovrPolyStart                      : ovrPolyStart +   ovrPolyVertexCount - 1]
        pY = ovrPolys[ovrPolyStart + ovrPolyVertexCount : ovrPolyStart + 2*ovrPolyVertexCount - 1]
        nPoints = n_elements(pX)
        oAABlistOvr[p] = self->makeAABforPairedROIvertices(roiIndexVector = [roiIndex1, roiIndex2], type = 'overlap', roiNames = roiNames, pVertexIndices = pVertices, $
                                                           correlativeIndex = p, correlIndexType = 'ovr', aabInputPolys = [pX, pY], boundingBoxOut = currentAABzoneBoundingBox)
        bbx = [currentAABzoneBoundingBox[0], currentAABzoneBoundingBox[2], currentAABzoneBoundingBox[2], currentAABzoneBoundingBox[0], currentAABzoneBoundingBox[0]]
        bby = [currentAABzoneBoundingBox[1], currentAABzoneBoundingBox[1], currentAABzoneBoundingBox[3], currentAABzoneBoundingBox[3], currentAABzoneBoundingBox[1]]
        if (self.fVerbose gt 0) and fPlot then begin
          print, bbx, '-', bby
          wSet, aabbbID
          plot, bbx, bby, color = '0000FF'x, lineStyle = 0, xrange = [xMin,xMax], yrange = [yMin,yMax], xmargin = [0,0], ymargin = [0,0], xTickInterval = 50, yTickInterval = 50, xStyle = 5, yStyle = 5, /noErase
        endif
        aabzoneBoundingBoxes = [aabzoneBoundingBoxes, currentAABzoneBoundingBox]
        aabBoundingPolys     = [aabBoundingPolys, ptr_new([[bbx], [bby]])]

  ovry: ovrPolyStart += 2*ovrPolyVertexCount + 1
        if ~obj_valid(oAABlistOvr[p])$
        then print, 'Skipped invalid polygon for overlap AAB (p = ', p, ')'
        p += 1
      endrep until (p eq aabCountOvr)
      AABlist = oAABlistOvr[where(obj_valid(oAABlistOvr) eq 1)]
      aabCountOvr = n_elements(AABlist)
    endif
  endif

  if ~fSkipOvrPruning and (aabCountOvr gt 0) then begin

    oAABlistOvr = objArr(aabCountOvr)

    if (self.fVerbose gt 0) and fPlot then begin
      makePolygonsPlot, *pxCoords1, *pyCoords1, *pxCoords2, *pyCoords2, /fNoAxes, $
                        backgroundImage = *self.pImage, winID = aabbbID, winTitle = aabbbTitle, /fCropImage, xPos = xPos, $
                        xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, sizeFactorOut = plotSizeFactor, sizeFactorWin = 50
    endif
    ovrPolyStart = 2
    ;TODO JuenEdo comment this section
    for p = 0, aabCountOvr-1 do begin
      ovrPolyVertexCount = ovrPolys[ovrPolyStart-1]
      pX = ovrPolys[ovrPolyStart                      : ovrPolyStart +   ovrPolyVertexCount - 1]
      pY = ovrPolys[ovrPolyStart + ovrPolyVertexCount : ovrPolyStart + 2*ovrPolyVertexCount - 1]
      nPoints = n_elements(pX)

      nCrossingsI = 0
      nCrossingsJ = 0
      pVertices = ptrArr(2)

      if (((*pVertexIndices[0,p])[0] eq -1) or ((*pVertexIndices[2,p])[0] eq -1)) then begin
        crossingListI = [-1]
        crossingListJ = [-1]
        whereCrossings = where(ptr_valid(crossingListIJ) eq 1, nWhereCrossings)
        arithDistThreshold = 0.001
        if (nWhereCrossings gt 0) then begin ;TODO redundant IF; check if can be omitted
          for s = 0, nWhereCrossings-1 do begin
            s1x = (*pxCoords1)[whereCrossings[s]]
            s2x = (*pxCoords1)[(whereCrossings[s]+1) mod ni]
            s1y = (*pyCoords1)[whereCrossings[s]]
            s2y = (*pyCoords1)[(whereCrossings[s]+1) mod ni]

            for v = 0, nPoints-2 do begin
              distA = distanceFromPointToSegment(pX[v], pY[v], s1x, s1y, s2x, s2y, endpointCode = endpointCodeA, /FSQUAREDIST)
              distB = distanceFromPointToSegment(pX[v+1], pY[v+1], s1x, s1y, s2x, s2y, endpointCode = endpointCodeB, /FSQUAREDIST)
              if ((distA lt arithDistThreshold) or (distB lt arithDistThreshold)) then crossingListI = [crossingListI, whereCrossings[s]]
              if (self.fVerbose gt 0) then begin
                print, pX[v], pY[v], s1x, s1y, s2x, s2y, ' - distance: ', distA
                print, pX[v+1], pY[v+1], s1x, s1y, s2x, s2y, ' - distance: ', distB
              endif
            endfor
          endfor
          ; TODO JuanEdo 1st priority
          ; there should be at least 1 segment found
          if n_elements(crossingListI) eq 1 then goto, ovrx
          crossingListI = crossingListI[uniq(crossingListI)]
          crossingListI = crossingListI[1:*]
          nCrossingsI = n_elements(crossingListI)
        endif

        whereCrossings = where(ptr_valid(crossingListJI) eq 1, nWhereCrossings)
        if (nWhereCrossings gt 0) then begin ;TODO redundant IF; check if can be omitted
          for s = 0, nWhereCrossings-1 do begin
            s1x = (*pxCoords2)[whereCrossings[s]]
            s2x = (*pxCoords2)[(whereCrossings[s]+1) mod nj]
            s1y = (*pyCoords2)[whereCrossings[s]]
            s2y = (*pyCoords2)[(whereCrossings[s]+1) mod nj]

            for v = 0, nPoints-2 do begin
              dista = distanceFromPointToSegment(pX[v], pY[v],s1x, s1y, s2x, s2y, endpointCode = endpointCodeA, /FSQUAREDIST)
              distb = distanceFromPointToSegment(pX[v+1], pY[v+1],s1x, s1y, s2x, s2y, endpointCode = endpointCodeB, /FSQUAREDIST)
              if ((distA lt arithDistThreshold) or (distB lt arithDistThreshold)) then crossingListJ = [crossingListJ, whereCrossings[s]]
            endfor
          endfor
          ; there should be at least 1 segment found
          if n_elements(crossingListJ) eq 1 then goto, ovrx
          crossingListJ = crossingListJ[1:*]
          crossingListJ = crossingListJ[uniq(crossingListJ)]
          nCrossingsJ   = n_elements(crossingListJ)
        endif

        crossingVerticesI = [-1]
        crossingVerticesJ = [-1]
        for c = 0, nCrossingsI-1 do $
          crossingVerticesI = [crossingVerticesI, crossingListI[c], ((crossingListI[c]+1) mod ni)]
        if (*pVertexIndices[0,p])[0] ne -1 $
        then crossingVerticesI = [crossingVerticesI[1:*], *pVertexIndices[0,p]] $
        else crossingVerticesI = crossingVerticesI[1:*]
        crossingVerticesI = crossingVerticesI[sort(crossingVerticesI)]
        crossingVerticesI = crossingVerticesI[uniq(crossingVerticesI)]

        for c = 0, nCrossingsJ-1 do $
          crossingVerticesJ = [crossingVerticesJ, crossingListJ[c], ((crossingListJ[c]+1) mod nj)]
        if (*pVertexIndices[2,p])[0] ne -1 $
        then crossingVerticesJ = [crossingVerticesJ[1:*], *pVertexIndices[2,p]] $
        else crossingVerticesJ = crossingVerticesJ[1:*]
        crossingVerticesJ = crossingVerticesJ[sort(crossingVerticesJ)]
        crossingVerticesJ = crossingVerticesJ[uniq(crossingVerticesJ)]

        pVertices[0] = ptr_new(crossingVerticesI, /no_copy)
        pVertices[1] = ptr_new(crossingVerticesJ, /no_copy)
      endif else begin
        pVertices[0] = pVertexIndices[0, p]
        pVertices[1] = pVertexIndices[2, p]
      endelse

      oAABlistOvr[p] = self->makeAABforPairedROIvertices(roiIndexVector = [roiIndex1, roiIndex2], type = 'overlap', roiNames = roiNames, pVertexIndices = pVertices, $
                                                         correlativeIndex = p, correlIndexType = 'ovr', aabInputPolys = [pX, pY], boundingBoxOut = currentAABzoneBoundingBox)
      bbx = [currentAABzoneBoundingBox[0], currentAABzoneBoundingBox[2], currentAABzoneBoundingBox[2], currentAABzoneBoundingBox[0], currentAABzoneBoundingBox[0]]
      bby = [currentAABzoneBoundingBox[1], currentAABzoneBoundingBox[1], currentAABzoneBoundingBox[3], currentAABzoneBoundingBox[3], currentAABzoneBoundingBox[1]]
      if (self.fVerbose gt 0) and fPlot then begin
        print, bbx, '-', bby
        wSet, aabbbID
        plot, bbx, bby, color = '0000FF'x, lineStyle = 0, xrange = [xMin,xMax], yrange = [yMin,yMax], xmargin = [0,0], ymargin = [0,0], xTickInterval = 50, yTickInterval = 50, xStyle = 5, yStyle = 5, /noErase
      endif
      aabzoneBoundingBoxes = [aabzoneBoundingBoxes, currentAABzoneBoundingBox]
      aabBoundingPolys     = [aabBoundingPolys, ptr_new([[bbx], [bby]])]

ovrx: ovrPolyStart += 2*ovrPolyVertexCount + 1
      if ~obj_valid(oAABlistOvr[p])$
      then print, 'Skipped invalid polygon for overlap AAB (p = ', p, ')'
      ;stop
    endfor

    AABlist = oAABlistOvr[where(obj_valid(oAABlistOvr) eq 1)]
    aabCountOvr = n_elements(AABlist)

  endif
  ; TODO JJ comment this section. Runs consistency checking after overlap pruning.
  oAABlistProx = objArr(1)
  runsIprox = [-1L]
  runsJprox = [-1L]
  if ((aabCountProx) gt 0) then for z = 0, aabCountProx-1 do begin
    fSomethingToAdd = 1b
    runPos = 2*z
    runListIcurr = [-1]
    runListJcurr = [-1]
    if ~fSkipOvrPruning and (nOvrPolys gt 0) then begin
      runListIcurr = extractFromRuns(runListIprox[runPos:runPos+1], ovrVerticesI[SortedIndicesI]) ; TODO JuanEdo 4th prority 2015.08.03 it does not work properly in _idl_condicion t054, rois 11-22, SORTEDINDICESI undefined
      runListJcurr = extractFromRuns(runListJprox[runPos:runPos+1], ovrVerticesJ[SortedIndicesJ])
    endif else begin
      runListIcurr = runListIprox[runPos:runPos+1]
      runListJcurr = runListJprox[runPos:runPos+1]
    endelse
    nRunsJcurr = n_elements(runListJcurr)/2
    if (nRunsJcurr gt 1) then begin
      tmp = [-1] ; invert run ordering in roi J
      for j = 2*nRunsJcurr, 2, -2 do tmp = [tmp, runListJcurr[j-2:j-1]]
      runListJcurr = tmp[1:*]
    endif
    if ((runListIcurr[0] eq -1) and (runListJcurr[0] eq -1)) then begin
      fSomethingToAdd = 0
      runsIprox = [runsIprox, runListIprox[runPos:runPos+1]]
      runsJprox = [runsJprox, runListJprox[runPos:runPos+1]]
    endif
    if (fSomethingToAdd ne 1) then continue
    runsIprox = [runsIprox, runListIcurr]
    runsJprox = [runsJprox, runListJcurr]
  endfor

  if (n_elements(runsIprox) gt 1) then begin
    runsIprox = runsIprox[1:*]
    runsJprox = runsJprox[1:*]
  endif

  nRunsI = n_elements(runsIprox) / 2
  nRunsJ = n_elements(runsJprox) / 2
  p1x = *pxCoords1
  p1y = *pyCoords1
  p2x = *pxCoords2
  p2y = *pyCoords2
  arrVerticesIprox = ptrArr(1)
  arrVerticesJprox = ptrArr(1)
  multiplicityCountI = uIntArr(nRunsI)

  for runPosI = 0, nRunsI-1 do begin
    ;makePolygonsPlot, *pxCoords1, *pyCoords1, *pxCoords2, *pyCoords2, winID = 22, sizeFactorWin=30
    runEndI = (runsIprox[2*runPosI+1]+1) mod ni
    if (runEndI eq runsIprox[2*runPosI]) then runEndI = runsIprox[2*runPosI+1] mod ni ; Boundary condition
    verticesIprox = singleRunToIndices(runsIprox[2*runPosI], runEndI, ni)
    ;oPlot, p1x[verticesIprox], p1y[verticesIprox], pSym = 2, symsize = 0.1, color='0000FF'x

    for runPosJ = 0, nRunsJ-1 do begin
      runEndJ = (runsJprox[2*runPosJ+1]+1) mod nj
      if (runEndJ eq runsJprox[2*runPosJ]) then runEndJ = runsJprox[2*runPosJ+1] mod nj ; Boundary condition
      verticesJprox = singleRunToIndices(runsJprox[2*runPosJ], runEndJ, nj)
      ;oPlot, p2x[verticesJprox], p2y[verticesJprox], pSym = 2, symsize = 0.1, color='FF0000'x
      tmp = distanceBetweenSegments(p1x[verticesIprox], p1y[verticesIprox], p2x[verticesJprox], p2y[verticesJprox],$
                                    distanceThreshold = self.proximityDist, proximityCorrespondences = proxArray, vectorIn = containedInJ[verticesIprox])

      if (total(ptr_valid(proxArray)) gt 0) then begin
        multiplicityCountI[runPosI] += 1
        tmp2 = findRunsFromCorrespondences(proxArray, verticesIRunPos, verticesJRunPos)
        nvertIprox = n_elements(verticesIprox)
        limRunI = verticesIRunPos[1]+1 ge nvertIprox ? nvertIprox-1 : verticesIRunPos[1]+1
        verticesIproxNew = verticesIprox[singleRunToIndices(verticesIRunPos[0], limRunI, nvertIprox)]
        nvertJprox = n_elements(verticesJprox)
        limRunJ = verticesJRunPos[1]+1 ge nvertJprox ? nvertJprox-1 : verticesJRunPos[1]+1
        verticesJproxNew = verticesJprox[singleRunToIndices(verticesJRunPos[0], limRunJ, nvertJprox)]
        ;oPlot, p1x[verticesIprox], p1y[verticesIprox], pSym = 2, symsize = 0.1, color='0000FF'x
        ;oPlot, p2x[verticesJprox], p2y[verticesJprox], pSym = 2, symsize = 0.1, color='FF0000'x
        arrVerticesIprox = [arrVerticesIprox, ptr_new(verticesIproxNew)]
        arrVerticesJprox = [arrVerticesJprox, ptr_new(verticesJproxNew)]
      endif
    endfor

    if (multiplicityCountI[runPosI] eq 0) then begin
    stop ; why am i here?!
      tmp = distanceBetweenSegments(p1x[verticesIprox], p1y[verticesIprox], p2x, p2y,$
                                    distanceThreshold = self.proximityDist, proximityCorrespondences = proxArray, vectorIn = containedInJ[verticesIprox])
      *proxArray[0] = (*proxArray[0])[where(tmp[*proxArray[0]] eq min(tmp[*proxArray[0]]))]
      tmp2 = findRunsFromCorrespondences(proxArray, verticesIRunPos, verticesJRunPos)
      verticesIproxNew = verticesIprox[singleRunToIndices(verticesIRunPos[0],verticesIRunPos[1]+1,n_elements(verticesIprox))]
      verticesJproxNew = singleRunToIndices(verticesJRunPos[0],verticesJRunPos[1]+1,nj)
      arrVerticesIprox = [arrVerticesIprox, ptr_new(verticesIproxNew)]
      arrVerticesJprox = [arrVerticesJprox, ptr_new(verticesJproxNew)]
      multiplicityCountI[runPosI] += 1
    endif
  endfor
  nProxRuns = total(multiplicityCountI)
  arrVerticesIprox = arrVerticesIprox[1:*]
  arrVerticesJprox = arrVerticesJprox[1:*]
  aabCountProx     = 0u
  cntVerticesIprox = 0

  aabCountProxTotal = -1
  for z = 0, nRunsI-1 do $
    if (multiplicityCountI[z] gt 0) then $
      for y = 0, multiplicityCountI[z]-1 do aabCountProxTotal += 1
  if (self.fVerbose gt 0) then print, 'aabCountProxTotal', aabCountProxTotal
  if (self.fVerbose gt 0) and fPlot and (aabCountOvr eq 0) and (nProxRuns gt 0) then $
      makePolygonsPlot, *pxCoords1, *pyCoords1, *pxCoords2, *pyCoords2, $
                        backgroundImage = *self.pImage, winID = aabbbID, winTitle = aabbbTitle, /fCropImage, /fNoAxes, xPos = xPos, $
                        xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, sizeFactorOut = plotSizeFactor, sizeFactorWin = 50

  oAABlistProx = objArr(aabCountProxTotal + 1)
  for z = 0, nRunsI-1 do begin
    if (multiplicityCountI[z] gt 0) then begin
      subRunList = ptrArr(2, 1)
      for y = 0, multiplicityCountI[z]-1 do begin
        subRunList = [[subRunList], [[arrVerticesIprox[cntVerticesIprox], arrVerticesJprox[cntVerticesIprox]]]]
        cntVerticesIprox += 1
        if (n_elements(AABList) eq 1) and ~obj_valid(AABList[0]) $ ; Check if this will be the first AAB of the list.
        then AABlist = [self->makeAABforPairedROIvertices(roiIndexVector = [roiIndex1, roiIndex2], type = 'proximity', roiNames = roiNames, pVertexIndices = subRunList[*,y+1],$
                                                          boundingBoxOut = currentAABzoneBoundingBox, correlativeIndex = aabCountProxTotal - aabCountProx, correlIndexType = 'prox',$
                                                          boundingPolyXout = bpx, boundingPolyYout = bpy)] $
        else AABlist = [AABlist, self->makeAABforPairedROIvertices(roiIndexVector = [roiIndex1, roiIndex2], type = 'proximity', roiNames = roiNames, pVertexIndices = subRunList[*,y+1],$
                                                                   boundingBoxOut = currentAABzoneBoundingBox, correlativeIndex = aabCountProxTotal - aabCountProx, correlIndexType = 'prox',$
                                                                   boundingPolyXout = bpx, boundingPolyYout = bpy)]
        oAABlistProx[aabCountProx] = AABList[n_elements(AABList)-1]
        bbx = [currentAABzoneBoundingBox[0], currentAABzoneBoundingBox[2], currentAABzoneBoundingBox[2], currentAABzoneBoundingBox[0], currentAABzoneBoundingBox[0]]
        bby = [currentAABzoneBoundingBox[1], currentAABzoneBoundingBox[1], currentAABzoneBoundingBox[3], currentAABzoneBoundingBox[3], currentAABzoneBoundingBox[1]]
        if (self.fVerbose gt 0) and fPlot then begin
          print, 'Bounding box (xmin, xmax, ymin, xmax): ', bbx, '-', bby
          wSet, aabbbID
          plot, bpx, bpy, color = '00FF00'x, lineStyle = 0, xrange = [xMin, xMax], yrange = [yMin, yMax], xmargin = [0, 0], ymargin = [0, 0], xTickInterval = 50, yTickInterval = 50, xStyle = 5, yStyle = 5, /noErase
        endif
        aabzoneBoundingBoxes = [aabzoneBoundingBoxes, currentAABzoneBoundingBox]
        aabBoundingPolys     = [aabBoundingPolys, ptr_new([[bpx], [bpy]], /no_copy)]
        aabCountProx        += 1
      endfor
    endif else begin
      print, 'WTF?!'
      stop
    endelse
  endfor
  aabCountTotal = aabCountOvr + aabCountProx

  ; Merge adjacent and consecutive AAB zones. Seems redundant to first make them separately, then to merge them. Time/combinations will tell...
  if (aabCountTotal gt 0) then begin

    if (fPlot and fSaveImg) then begin
      self->getFolderFileNamePrefix, prefix, baseFileName, folderPath
      if file_test(folderPath + imgFolderName, /DIRECTORY) eq 0 then file_mkDir, folderPath + imgFolderName
      scrFileNameSuffix = 'aabZoneBPs_pairwiseMerge_ROIs_' + num2strTrim(roiIndex1) + '-' + num2strTrim(roiIndex2) + '_before'
      write_png, folderPath + imgFolderName + path_sep() + prefix + baseFileName + scrFileNameSuffix + '.png', tvrd()

      ; raw image patch
      imgFileNameSuffix = 'aabZoneBPs_pairwiseMerge_ROIs_' + num2strTrim(roiIndex1) + '-' + num2strTrim(roiIndex2) + '_image'
      imgPatch = (*self.pImage)[xMin : xMax, yMin : yMax]
      write_png, folderPath + imgFolderName + path_sep() + prefix + baseFileName + imgFileNameSuffix + '.png', imgPatch
    endif

    aabzoneBoundingBoxes = aabzoneBoundingBoxes[1:*]
    aabBoundingPolys     = aabBoundingPolys[1:*]
    sortAABlistByVertexPos_implInsertionSort, aabList, roiIndex1, boundingBoxList = aabzoneBoundingBoxes, boundingPolyList = aabBoundingPolys, fPrintSortedList = 0
    self->mergeAABsForRoiPair, aabList, roiIndex1, roiIndex2, boundingBoxList = aabzoneBoundingBoxes, boundingPolyList = aabBoundingPolys,$
                               fFillZoneGaps = fFillZoneGaps, FillZoneGapIndex = FillZoneGapIndex, FillZoneGapDist = FillZoneGapDist
    aabCountTotal = n_elements(aabList)

    ; Boundary case for merge: try merging if the list extremes vertex indices go trough 0 in ROI1
    if (aabCountTotal gt 1) then begin
      nVert1 = n_elements(*pxCoords1)
      nVert2 = n_elements(*pxCoords2)
      nElemBBoxes = n_elements(aabzoneBoundingBoxes)
      aabMerge = self->tryAABpairMerge(aabList[0], aabList[aabCountTotal-1], maxIndex1 = nVert1-1, maxIndex2 = nVert2-1, $
                                       boundingBox1 = aabzoneBoundingBoxes[0:3], boundingBox2 = aabzoneBoundingBoxes[nElemBBoxes-3:nElemBBoxes-1],$
                                       pBoundingPoly1 = aabBoundingPolys[0], pBoundingPoly2 = aabBoundingPolys[aabCountTotal-1], $
                                       pBoundingPolyOut = pbpOut, boundingBoxOut = bbOut, $
                                       fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
      if n_elements(aabMerge) eq 1 then begin
        aabCountTotal -= 1
        obj_destroy, aabList[0]
        obj_destroy, aabList[aabCountTotal]
        aabList[0]                = aabMerge
        aabList                   = aabList[0:aabCountTotal-1]
        aabBoundingPolys[0]       = pbpOut
        ptr_free, aabBoundingPolys[aabCountTotal]
        aabBoundingPolys          = aabBoundingPolys[0:aabCountTotal-1]
        aabzoneBoundingBoxes[0:3] = bbOut
        aabzoneBoundingBoxes      = aabzoneBoundingBoxes[0:nElemBBoxes-5]
      endif
    endif

    if (self.fVerbose gt 0) then begin
      aabbbID = 11
      aabbbTitle = 'AAB-Zone Bounding Boxes (Pairwise Merged) : ROIs ' + num2strTrim(roiIndex1) + '-' + num2strTrim(roiIndex2)
      makePolygonsPlot, *(*(self.pXcoords)[0])[roiIndex1], *(*(self.pYcoords)[0])[roiIndex1], *(*(self.pXcoords)[0])[roiIndex2], *(*(self.pYcoords)[0])[roiIndex2], $
                         backgroundImage = *self.pImage, /fCropImage, /fNoAxes, xPos = xPos, winID = aabbbID, winTitle = aabbbTitle, $
                         xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, sizeFactorOut = plotSizeFactor, sizeFactorWin = 50
      for i = 0, n_elements(aabBoundingPolys)-1 do begin ;n_elements(aabzoneBoundingBoxes)/4-1 do begin
        ;bboxPos = 4*i
        ;bbx = [aabzoneBoundingBoxes[bboxPos], aabzoneBoundingBoxes[bboxPos+2], aabzoneBoundingBoxes[bboxPos+2], aabzoneBoundingBoxes[bboxPos], aabzoneBoundingBoxes[bboxPos]]
        ;bby = [aabzoneBoundingBoxes[bboxPos+1], aabzoneBoundingBoxes[bboxPos+1], aabzoneBoundingBoxes[bboxPos+3], aabzoneBoundingBoxes[bboxPos+3], aabzoneBoundingBoxes[bboxPos+1]]
        plot, (*aabBoundingPolys[i])[*,0], (*aabBoundingPolys[i])[*,1], color = '00FFFF'x, lineStyle = 0, xrange = [xMin,xMax], yrange = [yMin,yMax], xmargin = [0,0], ymargin = [0,0], xTickInterval = 50, yTickInterval = 50, xStyle = 5, yStyle = 5, /noErase
      endfor
;oplot, (*aabBoundingPolys[0])[*,0], (*aabBoundingPolys[0])[*,1], color = '00FFFF'x, lineStyle = 0

      if (fPlot and fSaveImg) then begin
        self->getFolderFileNamePrefix, prefix, baseFileName, folderPath
        scrFileNameSuffix = 'aabZoneBPs_pairwiseMerge_ROIs_' + num2strTrim(roiIndex1) + '-' + num2strTrim(roiIndex2) + '_after'
        write_png, folderPath + imgFolderName + path_sep() + prefix + baseFileName + scrFileNameSuffix + '.png', tvrd()
      endif

    endif
    ; ...and finally, update list of neighboring ROIs
    if ptr_valid((*self.pAdjacentROIindexVector)[roiIndex1]) then begin
      oldList = *(*self.pAdjacentROIindexVector)[roiIndex1]
      (*self.pAdjacentROIindexVector)[roiIndex1] = ptr_new([oldList, roiIndex2], /no_copy)
    endif else (*self.pAdjacentROIindexVector)[roiIndex1] = ptr_new([roiIndex2])

    if ptr_valid((*self.pAdjacentROIindexVector)[roiIndex2]) then begin
      oldList = *(*self.pAdjacentROIindexVector)[roiIndex2]
      (*self.pAdjacentROIindexVector)[roiIndex2] = ptr_new([oldList, roiIndex1], /no_copy)
    endif else (*self.pAdjacentROIindexVector)[roiIndex2] = ptr_new([roiIndex1])
  endif

  if (self.fVerbose gt 0) then begin
    print, 'Found ', aabCountOvr, ' overlap and ', aabCountProx, ' proximity AAB zones, merged into ', aabCountTotal
    print, 'C_sAABContainer::checkAABsForROIpair Finished!'
  endif

  return, aabCountTotal
end


; Pointer array of nRois with each element containing a list of the ROI index of the neighbors (if any)
function C_sAABContainer::getListOfNeighborROIs
  return, *self.pAdjacentROIindexVector
end


pro C_sAABContainer::mergeAABsForRoiPair, aabList, roiIndex1, roiIndex2, boundingBoxList = boundingBoxList, boundingPolyList = boundingPolyList, $
                                          fFillZoneGaps = fFillZoneGaps, FillZoneGapIndex = FillZoneGapIndex, FillZoneGapDist = FillZoneGapDist

  if (n_elements(aabList) lt 2) then return
  fBBoxes  = n_elements(boundingBoxList) gt 0
  fBPolys  = n_elements(boundingPolyList) gt 0
  fMergeOK = 0
  mergeIt  = 0u

  while ~fMergeOK do begin

    nAABs = n_elements(aabList)
    aabListMerge  = objArr(1)
    aabBBoxMerge  = [-1.0]
    aabBPolyMerge = ptrArr(1)
    fTailAppended = 0b
    i = 0u
    while (i lt (nAABs-1)) do begin
      case 1 of
        (fBBoxes eq 1) and (fBPolys eq 1): mergeArr = self->tryAABpairMerge(aabList[i], aabList[i+1], boundingBox1 = boundingBoxList[(4*i):(4*i+3)], boundingBox2 = boundingBoxList[(4*i+4):(4*i+7)], boundingBoxOut = bbOut, pBoundingPoly1 = boundingPolyList[i], pBoundingPoly2 = boundingPolyList[i+1], pBoundingPolyOut = pbpOut,$
                                                      fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
        (fBBoxes eq 1) and (fBPolys eq 0): mergeArr = self->tryAABpairMerge(aabList[i], aabList[i+1], boundingBox1 = boundingBoxList[(4*i):(4*i+3)], boundingBox2 = boundingBoxList[(4*i+4):(4*i+7)], boundingBoxOut = bbOut, pBoundingPolyOut = pbpOut,$
                                                      fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
        (fBBoxes eq 0) and (fBPolys eq 1): mergeArr = self->tryAABpairMerge(aabList[i], aabList[i+1], boundingBoxOut = bbOut, pBoundingPoly1 = boundingPolyList[i], pBoundingPoly2 = boundingPolyList[i+1], pBoundingPolyOut = pbpOut,$
                                                      fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
        else: mergeArr = self->tryAABpairMerge(aabList[i], aabList[i+1], fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
      endcase
      nMergeOut = n_elements(mergeArr)
      case 1 of
        (nMergeOut eq 1): begin
          aabListMerge = [aabListMerge, mergeArr]
          if (fBBoxes eq 1) then aabBBoxMerge  = [aabBBoxMerge , bbOut]
          if (fBPolys eq 1) then aabBPolyMerge = [aabBPolyMerge, pbpOut]
          if self.fVerbose then begin
            print, 'Merged AABs with ROI indices:'
            printListRoiVertexIndices, aabList[i:i+1], roiIndex1
            printListRoiVertexIndices, aabList[i:i+1], roiIndex2
          endif
        endcase
        else: begin
          aabListMerge = [aabListMerge, mergeArr[0]]
          if (fBBoxes eq 1) then aabBBoxMerge  = [aabBBoxMerge , boundingBoxList[4*i:4*i+3]]
          if (fBPolys eq 1) then aabBPolyMerge = [aabBPolyMerge, boundingPolyList[i]]
        endcase
      endcase
      fTailAppended = (i eq nAABs-2) and (nMergeOut eq 1)
      if (nMergeOut eq 1) then begin
        ptr_free, boundingPolyList[i], boundingPolyList[i+1]
        obj_destroy, aabList[i]
        obj_destroy, aabList[i+1]
      endif
      i += (nMergeOut eq 1) ? 2 : 1
    endwhile
    if ~fTailAppended then begin ; check and add (if necessary) the last element
      aabListMerge = [aabListMerge, aabList[nAABs-1]]
      if (fBBoxes eq 1) then aabBBoxMerge  = [aabBBoxMerge , boundingBoxList[4*nAABs-4:4*nAABs-1]]
      if (fBPolys eq 1) then aabBPolyMerge = [aabBPolyMerge, boundingPolyList[nAABs-1]]
    endif

    if (fBBoxes eq 1) then boundingBoxList  = aabBBoxMerge[1:*]
    if (fBPolys eq 1) then boundingPolyList = aabBPolyMerge[1:*]
    aabList  = aabListMerge[1:*]
    fMergeOK = nAABs eq n_elements(aabList)
    if fMergeOK and (nAABs ge 2) then begin ; boundary case: merge tail with head
      case 1 of
        (fBBoxes eq 1) and (fBPolys eq 1): oTH = self->tryAABpairMerge(aabList[nAABs-1], aabList[0], boundingBox1 = boundingBoxList[(4*(nAABs-1)):(4*(nAABs-1)+3)], boundingBox2 = boundingBoxList[0:3], boundingBoxOut = bbOut, pBoundingPoly1 = boundingPolyList[i], pBoundingPoly2 = boundingPolyList[0], pBoundingPolyOut = pbpOut,$
                                                      fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
        (fBBoxes eq 1) and (fBPolys eq 0): oTH = self->tryAABpairMerge(aabList[nAABs-1], aabList[0], boundingBox1 = boundingBoxList[(4*(nAABs-1)):(4*(nAABs-1)+3)], boundingBox2 = boundingBoxList[0:3], boundingBoxOut = bbOut, pBoundingPolyOut = pbpOut,$
                                                      fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
        (fBBoxes eq 0) and (fBPolys eq 1): oTH = self->tryAABpairMerge(aabList[nAABs-1], aabList[0], boundingBoxOut = bbOut, pBoundingPoly1 = boundingPolyList[nAABs], pBoundingPoly2 = boundingPolyList[0], pBoundingPolyOut = pbpOut,$
                                                      fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
        else: oTH = self->tryAABpairMerge(aabList[nAABs-1], aabList[0], fNonStrict = fFillZoneGaps, maxGapInd = FillZoneGapIndex, maxGapDist = FillZoneGapDist)
      endcase
      mergeOK = n_elements(oTH) eq 1
      if mergeOK then begin
        aabList = nAABs eq 2 ? [oTH] : [oTH, aabList[1:nAABs-2]]
        if (fBBoxes eq 1) then boundingBoxList  = nAABs eq 2 ? [bbOut]: [bbOut, aabBBoxMerge[4:4*(nAABs-2)+3]]
        if (fBPolys eq 1) then boundingPolyList = nAABs eq 2 ? [pbpOut] : [pbpOut, aabBPolyMerge[1:nAABs-2]]
        mergeOK = 1
      endif
    endif
    mergeIt += 1
  endwhile
  if self.fVerbose then print, 'Merge for ROIs ', roiIndex1, '-', roiIndex2, ' completed in ', mergeIt-1, ' iterations'
end


pro C_sAABContainer::updateContourBoxes, roiVector = roiVector
  ;if (self.numRois lt 1) or (self.numContours lt 1) then begin
  ;  print, 'C_sAABContainer::updateContourBoxes: Not enough ROIs/contours to update (', self.numRois, '/', self.numContours, ')' 
  ;  return
  ;endif
  if n_elements(roiVector) eq 0 then roiVector = uIndGen(self.numRois)
  nRois = n_elements(roiVector)
  for i = 0L, nRois-1 do begin
    iRoi = roiVector[i]
    numContourParts = n_elements((*(self.pXcoords)[0])[iRoi])
    if (self.fVerbose gt 0) then print, 'Updating ROI ', iRoi, ' with ', numContourParts, ' contour parts'
    xMin = dblArr(numContourParts)
    xMax = dblArr(numContourParts)
    yMin = dblArr(numContourParts)
    yMax = dblArr(numContourParts)
    for j = 0L, numContourParts-1 do begin
      xMin[j] = min(*(((*(self.pXcoords)[0])[iRoi])[j]), max = maxX)
      xMax[j] = maxX
      yMin[j] = min(*(((*(self.pYcoords)[0])[iRoi])[j]), max = maxY)
      yMax[j] = maxY
    endfor
    imgSize = size(*self.pImage, /DIM)
    xMin -= self.proximityDist
    xMin >= 0
    xMax += self.proximityDist
    xMax <= imgSize[0] - 1
    yMin -= self.proximityDist
    yMin >= 0
    yMax += self.proximityDist
    yMax <= imgSize[1] - 1
    *((*(self.pXcoordsBox)[0])[iRoi]) = [min(xMin), max(xMax)]
    *((*(self.pYcoordsBox)[0])[iRoi]) = [min(yMin), max(yMax)]
    if (self.fVerbose gt 0) then begin
      print, 'pXcoordsBox for ROI ', iRoi, ': ', *((*(self.pXcoordsBox)[0])[iRoi])
      print, 'pYcoordsBox for ROI ', iRoi, ': ', *((*(self.pYcoordsBox)[0])[iRoi])
    endif
  endfor
end


function getNumberOfUniqueROIsFromAABlist, aabList, roiIndexVectorUnique = roiIndexVectorUnique

  nAABs = n_elements(aabList)
  if (nAABs lt 1) then return, 0

  roiIndexVectorAll = [-1]
  for i = 0u, nAABs-1 do begin
    if ~obj_valid(aabList[i]) then begin ; 2016.01.14 this can happen with already merged (and then deleted) corner objects
      print, 'Skipping AAB object which no longer exists in the list...'
      continue
    endif
    roiIndexVectorStruct = aabList[i]->getAttributeByName('roi_index_vector')
    outType = size(roiIndexVectorStruct, /TYPE)
    if (outType eq 2) $ ; If this is a corner object, try with the corresponding attribute
    then roiIndexVectorStruct = aabList[i]->getAttributeByName('roiIndexVectorUnique')
    roiIndexVectorAll = [roiIndexVectorAll, **(roiIndexVectorStruct.pData[0])]
  endfor

  roiIndexVectorAll    = roiIndexVectorAll[1:*]
  roiIndexVectorUnique = roiIndexVectorAll[uniq(roiIndexVectorAll, sort(roiIndexVectorAll))]
  return, n_elements(roiIndexVectorUnique)

end


; Pre-conditions:
;  pair-wise ROI AAB objects are given => each AAB object contains info from only two ROIs.
function getVertexRunsFromPairedAABs, aabList, roiIndexVector = roiIndexVector, nRunsVector = nRunsVector, nRoisVector = nRoisVector, fSkipRuns = fSkipRuns

  nAABs = n_elements(aabList)
  if (nAABs lt 1) then stop
  roiIndexVector = [0u]
  nRoisVector    = [0u]

  for i = 0u, nAABs-1 do $
    roiIndexVector  = [roiIndexVector, *((aabList[i]->getAttributeByName('roi_index_vector')).pData[0])]
  roiIndexVector = roiIndexVector[1:*]
  roiIndexVector = roiIndexVector[uniq(roiIndexVector, sort(roiIndexVector))]
  nRoisVector = [nRoisVector, n_elements(roiIndexVector)]

  if keyword_set(fSkipRuns) then return, 0

  vertexIndexList = ptrArr(nRois)

  if nAABs eq 2 then begin ; TODO JJ old code. delete when finished.
    pRoiVertexIndices1 = (aab1->getAttributeByName('roi_vertex_indices')).pData
    pRoiVertexIndices2 = (aab2->getAttributeByName('roi_vertex_indices')).pData

    for i = 0u, nAABs-1 do begin
      whRoi = roiIndexVector1[i]
      vertexIndicesAAB1i = *pRoiVertexIndices1[*,i]
      if ptr_valid(vertexIndexList[whRoi]) $
      then vertexIndices = [vertexIndicesAAB1i, *vertexIndexList[whRoi]] $
      else vertexIndices = [vertexIndicesAAB1i]
      vertexIndexList[whRoi] = ptr_new(vertexIndices)
    endfor
    for i = 0u, nAABs-1 do begin
      whRoi = roiIndexVector2[i]
      vertexIndicesAAB2i = *pRoiVertexIndices2[*,i]
      if ptr_valid(vertexIndexList[whRoi]) $
      then vertexIndices = [vertexIndicesAAB2i, *vertexIndexList[whRoi]] $
      else vertexIndices = [vertexIndicesAAB2i]
      vertexIndexList[whRoi] = ptr_new(vertexIndices)
    endfor
  endif

  vertexRunList = ptrArr(nRois)
  nRunsVector   = uIntArr(nRois)
  for i = 0u, nRois-1 do begin
    sortedVertices   = *vertexIndexList[i]
    runs = makeRunFromCorrelativeValues(sortedVertices, /getnMaxFromZeroPos, outNmax = nMax)
    nRunsVector[i]   = n_elements(runs/2)
    vertexRunList[i] = ptr_new(runs, /no_copy)
  endfor
  return, vertexRunList

end


; Check for conditions for proposing a corner from two input AAB objects.
function tryMergeAABsForCorner140, aab1, aab2, boundingBox1, boundingBox2, pBoundingPoly1, pBoundingPoly2

  ; 1. Check if there is at least one single run that can be formed from the input AABs.
  ; This would mean that a single ROI boundary zone spans across two AABs (3-ROI junction).
  ; This would ne a '+'
  runs12   = getVertexRunsFromPairedAABs([aab1, aab2], nRunsVector = nRunsVector, roiIndexVector = roiIndexVector)
  minNruns = min(nRunsVector, max = maxNruns)

  ; 2. If minNruns eq 1 rhis would also mean that there are 3 ROIs for 2 AABs, otherwise it must be checked.
  ; Having 3 ROIs would be a '+'.
  nRois = n_elements(roiIndexVector)

  ; needed: AAB indices (absolute, from the whole list, which also need to be used to point to bounding geometries)
  ; bounding geometry for the corner?
  ; resulting corner struct has to be put into a list (and indexed?)
  ; store roiIndexVector, runs12
  return, oCorner
end


; Creates a corner AAB object referencing the non-corner AAB objects in the input list.
; If a corner is in the input list, only its referenced (non-corner) AABs are added.
; "Extra" info (as of now) must be consolidated in the caller method. Looking at you, bounding boxes!
; Note: so far used with lists of two elements, but it should also work with 3+.
function C_sAABContainer::makeCornerObjectFromAABList, aabList, aabIndexVector, roiIndexVectorUniq = roiIndexVectorUniq, $
                                                       cornerBoundingBox = cornerBoundingBox, cornerImagePatch = cornerImagePatch, boundingPolys = boundingPolys
  nAABs = n_elements(aabList)
  if nAABs lt 2 then return, -1
  if n_elements(aabIndexVector) eq 0 then stop ; They should be the same. Debug and resume/start over!

  roiIndexVector = [0u]
  nRoisVector    = [0u]
  nRoisCountOld  = 1
  pairedAABList  = objArr(1)

  for i = 0u, nAABs-1 do begin
    att = aabList[i]->getAttributeByName('roi_index_vector')
    roiIndexVector = [roiIndexVector, **att.pData[0]];**((aabList[i]->getAttributeByName('roi_index_vector')).pData[0])]
    nRoisCountNew  = n_elements(roiIndexVector)
    nRoisVector    = [nRoisVector, nRoisCountNew - nRoisCountOld]
    nRoisCountOld  = nRoisCountNew
    if (att.aabType eq 'corner') $
    then pairedAABList = [pairedAABList, **((aabList[i]->getAttributeByName('corner_aab_list')).pData[0])] $
    else pairedAABList = [pairedAABList, aabList[i]]
  endfor

  pairedAABList      = pairedAABList[1:*]
  roiIndexVector     = roiIndexVector[1:*]
  roiIndexVectorUniq = roiIndexVector[uniq(roiIndexVector, sort(roiIndexVector))]
  nRoisUniq = n_elements(roiIndexVectorUniq)

  ; If no bounding box is provided, it will have to be computed... not implemented here yet :P
  if n_elements(cornerBoundingBox) ne 4 then begin
    if n_elements(boundingPolys) ne 2 then stop ;!? Not intended for more than 2 ROIs (but it's do-able). Call jjara...
    intBoundingPolys = calcPolygonIntersection(p1x = (*boundingPolys[0])[*,0], p1y = (*boundingPolys[0])[*,1], p2x = (*boundingPolys[1])[*,0], p2y = (*boundingPolys[1])[*,1])
    if intBoundingPolys[0] lt 1 then stop ;!? Call jjara
    npts = intBoundingPolys[1]
    xMax = max(intBoundingPolys[2:2+npts-1], min = xMin)
    yMax = max(intBoundingPolys[2+npts:2+npts+npts-1], min = yMin)
    cornerBoundingBox = [xMin, yMin, xMax, yMax]
  endif

  if n_elements(cornerImagePatch) eq 0 then $
    cornerImagePatch = self->getImagePatch(cornerBoundingBox[0], cornerBoundingBox[1], cornerBoundingBox[2], cornerBoundingBox[3])

  attPosBBox          = 0
  attPosRoiIdxVectorU = 1
  attPosAABidxVector  = 2
  attPosAABlist       = 3
  attPosImgPatch      = 4
  nAttrib = 5
  cType = 'corner'
  sType = '2D'

  oCorner = obj_new('C_sAABSharedZone')
  oCorner->setAttributesNum, nAttrib
  oCorner->addAttributeStruct, attPosBBox, 'corner_bounding_box', sType, cType, ptr_new(cornerBoundingBox)
  oCorner->addAttributeStruct, attPosRoiIdxVectorU, 'roi_index_vector', sType, cType, ptr_new(roiIndexVectorUniq) ; 'corner_roi_index_vector_unique' ?
  oCorner->addAttributeStruct, attPosAABidxVector, 'corner_aab_index_vector', sType, cType, ptr_new(aabIndexVector)
  oCorner->addAttributeStruct, attPosAABlist, 'corner_aab_list', sType, cType, ptr_new(pairedAABList)
  oCorner->addAttributeStruct, attPosImgPatch, 'corner_image_patch', sType, cType, ptr_new(cornerImagePatch)
  return, oCorner
end


; Checking for intersections and (if any) merge corner objects (they start with 2xAAB objects, meaning 3 ROIs).
; Intersecting corner objects (by checking their bounding boxes) are merged by calling a sub-method (see method above).
; Return value: the number of resulting corners.
; Three main structures constructed: corner list, its corresponding AAB-index list, and bounding boxes
function C_sAABContainer::checkCornerAABsForAllRois2, aabList, cornerList = cornerList, pCornerAABindices = pCornerAABindices, cornerBoundingBoxes = cornerBoundingBoxes, pCornerBoundingPolys = pCornerBoundingPolys

  nCorners = n_elements(aabList)
  if (nCorners eq 1) then return, 0
;  cornerPairs     = lonArr(nCorners*(nCorners-1))
;  cornerPairCount = -1L
;  libLoc = getDLLfilename('SweepAndPrune2D', /GetFileNameAndPath)
;  result = call_external(libLoc, 'find_intersections2D',$
;                         fix(cornerBoundingBoxes, type = 4), fix(nCorners, type = 2), fix(2, type = 2),$
;                         cornerPairs, cornerPairCount, /unload)
  cornerPairs     = intersectingPolygonPairs(pCornerBoundingPolys);, boundingBoxesOut = cornerBoundingBoxes) ; corner bounding boxes created here
  cornerPairCount = n_elements(cornerPairs) / 2
  if (cornerPairCount eq 0) then return, 0
  ;if (cornerPairCount gt 0) then cornerPairs = cornerPairs[0:2*cornerPairCount-1]
  if self.fVerbose then print, cornerPairCount, ' AABs (bounding polygons) pairs overlap'

;  pCornerAABindices = ptrArr(cornerPairCount)
;  for i = 0u, cornerPairCount-1 do $
;    pCornerAABindices[i] = ptr_new(cornerPairs[2*i:2*i + 1])
  pCornerAABindices = ptrArr(nCorners)
  for i = 0u, nCorners-1 do $
    pCornerAABindices[i] = ptr_new([i])
  cornerBoundingBoxes = fltArr(8*cornerPairCount) - 1.0 ; not-set valued bounding box list
  mergeList       = aabList
  mergeBBlist     = cornerBoundingBoxes
  mergeAABindices = pCornerAABindices
  mergeBPlist     = pCornerBoundingPolys
  mergeIt = 0
  mergeOK = 0
  BBoxMock = [-1.0]
  while ~mergeOK do begin
    mergeIt += 1
    for i = 0u, cornerPairCount-1 do begin
      pairPos     = 2*i
      cornerI1Pos = cornerPairs[pairPos]
      cornerI2Pos = cornerPairs[pairPos+1]
      aabIndexVectorPair = [*mergeAABindices[pairPos], *mergeAABindices[pairPos+1]]
      aabIndexVectorPair = aabIndexVectorPair[uniq(aabIndexVectorPair, sort(aabIndexVectorPair))]
      if mergeIt eq 1 then begin
        bboxI12 = bboxMock
        oCorner = self->makeCornerObjectFromAABList(mergeList[pairPos:pairPos+1], aabIndexVectorPair, cornerBoundingBox = bboxI12, boundingPolys = [pCornerBoundingPolys[cornerI1Pos], pCornerBoundingPolys[cornerI2Pos]]);, roiIndexVectorUniq = rivuq)
      endif else begin
        bboxI1 = mergeBBlist[cornerI1Pos*4 : cornerI1Pos*4+3]
        bboxI2 = mergeBBlist[cornerI2Pos*4 : cornerI2Pos*4+3]
        bboxI12 = [bboxI1[0] < bboxI2[0], bboxI1[1] < bboxI2[1], bboxI1[2] > bboxI2[2], bboxI1[3] > bboxI2[3]]
        bboxImagePatch = self->getImagePatch(bboxI12[0], bboxI12[1], bboxI12[2], bboxI12[3])
        oCorner = self->makeCornerObjectFromAABList(mergeList[pairPos:pairPos+1], aabIndexVectorPair, $
                                                    cornerBoundingBox = bboxI12, cornerImagePatch = bboxImagePatch);, roiIndexVectorUniq = rivuq)
      endelse
      ; After merging, the corner list needs to be rebuilt to account for the new corner.
      ; For now, doing it the inefficient but safe way...
      whCorner = where((cornerPairs eq cornerI1Pos) or (cornerPairs eq cornerI2Pos), countI12, complement = whNotI12, nComplement = countNotI12)
      if (countI12 gt 0) and (countNotI12 gt 0) then begin
        rbBBoxes = [-1d]
        cpi = cornerPairs[whNotI12] ; remove duplicate entries
        cpi = cpi[uniq(cpi, sort(cpi))]
        for z = 0u, n_elements(cpi)-1 do begin
          rbBBoxPos = cpi[z] * 4
          rbBBoxes  = [rbBBoxes, mergeBBlist[rbBBoxPos:rbBBoxPos+3]]
        endfor
        ;mergeBPlist     = [ptr_new(), mergeBPlist[whNotI12]] ; Mock line: not used nor implemented
        mergeAABindices = [ptr_new(aabIndexVectorPair, /no_copy), mergeAABindices[cpi]]
        mergeBBlist     = [bboxI12, rbBBoxes[1:*]]
        mergeList       = [oCorner, mergeList[cpi]]
        nCornersMerge   = n_elements(mergeList)
        mergeOK = 0b
        goto, cm
      endif
      if (countI12 eq 2) and (countNotI12 eq 0) then begin
        mergeList       = [oCorner]
        mergeBBlist     = [bboxI12]
        ;mergeBPlist     = ptrArr(1) ; Mock line: not used nor implemented
        mergeAABindices = [ptr_new(aabIndexVectorPair, /no_copy)]
        nCornersMerge   = 1
        mergeOK = 1b
        goto, cm
      endif
      mergeList       = [mergeList, oCorner]
      mergeBBlist     = [mergeBBlist, bboxI12]
      ; mergeBPlist = ptrArr(1) ; Mock line: not used nor implemented
      mergeAABindices = [mergeAABindices, ptr_new(aabIndexVectorPair, /no_copy)]
    endfor

    nCornersMerge = n_elements(mergeList)-1
    if (nCornersMerge ge 1) then begin
      mergeList       = mergeList[1:*]
      mergeBBlist     = mergeBBlist[1:*]
      mergeAABindices = mergeAABindices[1:*]
      ; mergeBPlist = ptrArr(1) ; Mock line: not used nor implemented
    endif

cm: if (nCornersMerge ge 2) then begin
      cornerPairs     = lonArr(nCornersMerge*(nCornersMerge -1))
      cornerPairCount = -1L
      libLoc = getDLLfilename('SweepAndPrune2D', /GetFileNameAndPath)
      result = call_external(libLoc, 'find_intersections2D',$
                             fix(mergeBBlist, type = 4), fix(nCornersMerge, type = 2), fix(2, type = 2),$
                             cornerPairs, cornerPairCount, /unload)
      mergeOK = (cornerPairCount eq 0)
      if (cornerPairCount gt 0) then cornerPairs = cornerPairs[0:2*cornerPairCount-1]
    endif else mergeOK = 1
  endwhile

  nCornersMerge = n_elements(mergeList);-1
  if self.fVerbose then print, 'corner merge completed in ', mergeIt, ' iterations: ', nCornersMerge, ' corners (', nCorners, ' as input)'
  if (nCornersMerge ge 1) then begin
    cornerList = mergeList;[1:*]
    cornerBoundingBoxes = mergeBBlist;[1:*]
    nAABindices = n_elements(pCornerAABindices)
    for j = nAABindices-1, 0, -1 do ptr_free, pCornerAABindices[j]
    pCornerAABindices = mergeAABindices;[1:*]
    ;for j = nAABindices-1, 0, -1 do ptr_free, pCornerBoundingPolys[j] & stop
    pCornerBoundingPolys = mergeBPlist;[1:*] ; not used... re-setting the same
    return, nCornersMerge
;    cornerList = mergeList[1:*]
;    cornerBoundingBoxes = cornerBBlistOut[1:*]
;    nAABindices = n_elements(pCornerAABindices)
;    if n_elements(pCornerAABindices) gt 0 then $
;    for j = nAABindices-1, 0, -1 do ptr_free, pCornerAABindices[j]
;    pCornerAABindices = cornerAABindexVectorOut[1:*]
;    if n_elements(pCornerBoundingPolys) gt 0 then $
;    for j = nAABindices-1, 0, -1 do ptr_free, pCornerBoundingPolys[j]
;    pCornerBoundingPolys = cornerAABindexVectorOut[1:*]
;    return, nCornersMerge
  endif else return, 0
end


; Pruning method used during AAB corner detection, with the purpose of removing
; ROI vertices from paired AABs when there are other ROIs that overlap with them.
function C_sAABContainer::prunePairedAABs, aabPairedList

  ; Check basic conditions: there are more than two ROIs, and at least two paired AABs
  if self.numRois le 2 then return, 0

  nPairedAABs = n_elements(aabPairedList)
  if nPairedAABs lt 2 then return, 0

  intersectingROIAABindexPairs = [0u]
  for i = 0u, self.numRois-1 do begin
  endfor

  nIntersectingPairs = n_elements(intersectingROIAABindexPairs)-1 ; Discard first, mockup element.
  return, nIntersectingPairs eq 0 ? [-1] : intersectingROIAABindexPairs[1:*]

end


function C_sAABContainer::makeDistanceMaskFromRois, distMapSize = distMapSize, roiIndexVector = roiIndexVector

  if (self.numRois lt 1) then begin
    print, 'No ROIs to compute the distance mask from.'
    return, -1
  endif
  fAutoSize = 0
  case n_elements(distMapSize) of
    0: fAutoSize = 1
    1: distMapSize = [distMapSize, distMapSize]
    2: distMapSize = distMapSize
    else: fAutoSize = 1
  endcase

  if (fAutoSize eq 1) then distMapSize = size(*self.pImage, /dim)

  winId = 26
  window, winId, xsize = distMapSize[0], ysize = distMapSize[1], retain = 2

  if (n_elements(roiIndexVector) eq 0) then roiIndexVector = uIndGen(self.numRois)
  for i = 0u, n_elements(roiIndexVector)-1 do begin
    px = *(*(self.pXcoords)[0])[roiIndexVector[i]]
    py = *(*(self.pYcoords)[0])[roiIndexVector[i]]
    plot, [px, px[0]], [py, py[0]], xrange = [0,distMapSize[0]-1], yrange = [0,distMapSize[1]-1], xstyle = 1, ystyle = 1, xmargin = [0,0], ymargin = [0,0], xticks = 1, yticks = 1, /noErase
    polyFill, px, py
  endfor
  img = tvrd()
  wDelete, winId
  distMap = morph_distance(img, neighbor_sampling = 3, /background)
  return, distMap
end


function C_sAABContainer::checkAABcorners, aabList, cornerList = cornerList, pCornerAABindices = pCornerAABindices, cornerBoxes = cornerBoxes, pCornerBoundingPolys = pCornerBoundingPolys, pCornerROIindices = pCornerROIindices

  nAABs = n_elements(aabList)
  if (nAABs lt 2) then return, 0
  ; TODO JJW corner box padding parameter (as a function of the proximityDistance attribute?)
  cornerPairs     = intersectingPolygonPairs(pCornerBoundingPolys, boundingBoxesOut = cornerBoxes, boxPad = self.proximityDist * 0.25) ; corner bounding boxes created here
  cornerPairCount = n_elements(cornerPairs) / 2
  if (cornerPairCount eq 0) then return, 0
  ;if (cornerPairCount gt 0) then cornerPairs = cornerPairs[0:2*cornerPairCount-1]
  if self.fVerbose then print, cornerPairCount, ' AABs (bounding polygons) pairs overlap'

  pCornerAABindices = ptrArr(cornerPairCount)
  pCornerROIindices = ptrArr(cornerPairCount)
  for i = 0u, cornerPairCount-1 do begin
    pCornerAABindices[i] = ptr_new(cornerPairs[2*i:2*i + 1])
    roiIndices = [**((aabList[cornerPairs[2*i]]->getAttributeByName('roi_index_vector')).pData[0]),$
                  **((aabList[cornerPairs[2*i+1]]->getAttributeByName('roi_index_vector')).pData[0])]
    roiIndices = roiIndices[uniq(roiIndices, sort(roiIndices))]
    pCornerROIindices[i] = ptr_new(roiIndices)
  endfor

  cornerBoxes   = mergeCornerBoxes(cornerBoxes, pCornerAABIndices, pCornerROIindices, roiIndicesOut = cornerMergeROIindices, aabIndicesOut = cornerMergeAABindices)
  nCornersMerge = n_elements(cornerBoxes) / 4
  if (nCornersMerge eq 0) then for i = n_elements(pCornerAABIndices)-1, 0, -1 do ptr_free, pCornerAABIndices[i], pCornerROIindices[i]
;stop
  if (nCornersMerge ge 1) then begin
    pCornerAABIndices   = cornerMergeAABindices
    attPosBBox          = 0
    attPosRoiIdxVectorU = 1
    attPosAABidxVector  = 2
    attPosAABlist       = 3
    attPosImgPatch      = 4
    attPosImgPatchXYPos = 5
    attPosDistMapROIs   = 6
    nAttrib = 7
    cType = 'corner'
    sType = '2D'
    cornerList = objArr(nCornersMerge)
    distMap = self->makeDistanceMaskFromRois()

    for i = 0u, nCornersMerge-1 do begin
      oCorner = obj_new('C_sAABSharedZone')
      oCorner->setAttributesNum, nAttrib
      cornerBoundingBox = [cornerBoxes[4*i:4*i+3]]
      oCorner->addAttributeStruct, attPosBBox, 'corner_bounding_box', sType, cType, ptr_new(cornerBoundingBox)
      oCorner->addAttributeStruct, attPosRoiIdxVectorU, 'roi_index_vector', sType, cType, ptr_new(*cornerMergeROIindices[i]) ; 'corner_roi_index_vector_unique' ?
      oCorner->addAttributeStruct, attPosAABidxVector, 'corner_aab_index_vector', sType, cType, ptr_new(*cornerMergeAABindices[i])
      oCorner->addAttributeStruct, attPosAABlist, 'corner_aab_list', sType, cType, ptr_new(aabList[*cornerMergeAABindices[i]])
      xPos = cornerBoundingBox[0]
      yPos = cornerBoundingBox[1]
      cornerImagePatch = self->getImagePatch(cornerBoundingBox[0], cornerBoundingBox[1], cornerBoundingBox[2], cornerBoundingBox[3])
      distMapPatch     = self->getImagePatch(cornerBoundingBox[0], cornerBoundingBox[1], cornerBoundingBox[2], cornerBoundingBox[3], image = distMap)
      oCorner->addAttributeStruct, attPosImgPatch, 'corner_image_patch', sType, cType, ptr_new(cornerImagePatch)
      oCorner->addAttributeStruct, attPosImgPatchXYPos, 'corner_image_patch_xy_position', sType, cType, ptr_new([xPos, yPos])
      oCorner->addAttributeStruct, attPosDistMapROIs, 'corner_roi_distance_map_patch', sType, cType, ptr_new(distMapPatch)
      cornerList[i] = oCorner
    endfor
  endif
  return, nCornersMerge
end


pro C_sAABContainer::checkCornerAABsForAllRois, aabPairedList, aabBoundingBoxes, paabBoundingPolys, $
  aabCornerListOut = aabCornerListOut, aabBoundingBoxesOut = aabBoundingBoxesOut, paabBoundingPolysOut = paabBoundingPolysOut,$
  aabCornerBoundingBoxesOut = aabCornerBoundingBoxesOut, paabCornerBoundingPolysOut = paabCornerBoundingPolysOut

  nPairedAABs = n_elements(aabPairedList)
  if (nPairedAABs lt 2) then return

  aabPairs     = lonArr(nPairedAABs*(nPairedAABs-1))
  aabPairCount = -1L
  libLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
  result = call_external(libLoc, 'find_intersections2D',$
                         fix(aabBoundingBoxes, type = 4),$
                         fix(nPairedAABs, type = 2),$
                         fix(2, type = 2),$
                         aabPairs,$
                         aabPairCount, /unload)
  if (aabPairCount gt 0) then aabPairs = aabPairs[0:2*aabPairCount-1]

  if self.fVerbose then begin
    print, aabPairCount, ' AAB zone pairs overlap'
    if (aabPairCount gt 0) and (aabPairCount le 10) then print, aabPairs
  endif
  if (aabPairCount) eq 0 then return

  aabPairsPruned      = [-1L]
  aabPairsPrunedCount = 0u
  fDoPrune = 0 ; TODO JJ reserved for more aggresive|paranoid checkings
  if fDoPrune then begin
    for i = 0, aabPairCount-1 do begin
      pairPos   = 2*i
      aabIndex1 = aabPairs[pairPos]
      aabIndex2 = aabPairs[pairPos+1]
      polyDist  = calcDistanceBetweenPolylines((*paabBoundingPolys[aabIndex1])[*,0], (*paabBoundingPolys[aabIndex1])[*,1],$
                                               (*paabBoundingPolys[aabIndex2])[*,0], (*paabBoundingPolys[aabIndex2])[*,1])
      if (polyDist le self.proximityDist) then begin
        aabPairsPrunedCount += 1
        aabPairsPruned = [aabPairsPruned, aabIndex1, aabIndex2]
      endif
    endfor

    if (aabPairsPrunedCount gt 0) then begin
      aabPairCount = aabPairsPrunedCount
      aabPairs     = aabPairsPruned[1:*]
    endif else return
  endif

  aabBoundingBoxesMerge  = aabBoundingBoxes
  paabBoundingPolysMerge = paabBoundingPolys
  aabCornerList = aabPairedList
  mergeOK       = aabPairCount eq 0
  mergeIter     = 0
  nCornerAABs   = 0
  cornerBoundingBoxes = [-1.0]
  while ~mergeOK do begin
stop
    mergeIter += 1
    aabCornerBoundingBoxesMerge  = [-1d]
    pAABcornerBoundingPolysMerge = ptrArr(1)
    aabCornerListMerge           = objArr(1)
    for i = 0u, aabPairCount-1 do begin
      pairPos    = 2*i
      nRoisInt12 = getNumberOfUniqueROIsFromAABlist(aabCornerList[aabPairs[pairPos:pairPos+1]], roiIndexVectorUnique = roiIndexVectorUnique)
      if (nRoisInt12 le 2) then continue ; Maybe interesting to check someday?
      aabIndex1  = aabPairs[pairPos]
      aabIndex2  = aabPairs[pairPos+1]
      bBoxes     = [aabBoundingBoxesMerge[4*aabIndex1:4*aabIndex1+3], aabBoundingBoxesMerge[4*aabIndex2:4*aabIndex2+3]]
      pBPolys    = [paabBoundingPolysMerge[aabIndex1], paabBoundingPolysMerge[aabIndex2]] ; get/set the bounding boxes and polygons...
      oCornerAAB = self->mergeAABsForCorner(aabCornerList[aabPairs[pairPos:pairPos+1]], boundingBoxList = bBoxes, pBoundingPolyIn = pBPolys, fMergeOut = fMergeOut, cornerBoundingBoxOut = cornerBB, $
                                            boundingBoxOut = bbOut, pBoundingPolyOut = pBPOut, roiIndexVectorUnique = roiIndexVectorUnique, pairedAABsIdVector = [pairPos, pairPos+1])
      if fMergeOut then begin ; If there is an AAB duplicated within the corner objects, leave only one.
        whAAB12 = where((aabPairs eq aabIndex1) and (aabPairs eq aabIndex2), count12, complement = whNotAAB12, nComplement = countNot12)
        if (count12 gt 0) then begin
          rbBBoxes = [-1d]
          for z = 0, countNot12-1 do begin
            rbBBoxPos = whNotAAB12[z]*4
            rbBBoxes  = aabCornerBoundingBoxesMerge[rbBBoxPos:rbBBoxPos+3]
          endfor
          aabCornerBoundingBoxesMerge  = [bbOut, rbBBoxes[1:*]]
          pAABcornerBoundingPolysMerge = [pBPOut, paabBoundingPolysMerge[whNotAAB12]]
          aabCornerListMerge           = [oCornerAAB, aabCornerList[whNotAAB12]]
          mergeOK = 0b
          goto, mok
        endif
      endif
      if n_elements(oCornerAAB) eq 1 then begin
        aabCornerListMerge           = [aabCornerListMerge, oCornerAAB]
        aabCornerBoundingBoxesMerge  = [aabCornerBoundingBoxesMerge, bbOut]
        pAABcornerBoundingPolysMerge = [pAABcornerBoundingPolysMerge, pBPOut]
      endif else begin
        stop
      endelse
    endfor
    nCornerAABsMerge = n_elements(aabCornerListMerge)-1
    mergeOK = (nCornerAABs eq nCornerAABsMerge) or (nCornerAABsMerge eq 1)
    ; TODO JJ check if it becomes necessary to clean the objects from aabCornerList before rewriting
    ;if (mergeIter gt 1) then for j = 0, n_elements(aabCornerList)-1 do obj_destroy, aabCornerList[j]
    aabCornerList          = aabCornerListMerge[1:*]
    nCornerAABs            = n_elements(aabCornerList)
    aabBoundingBoxesMerge  = aabCornerBoundingBoxesMerge[1:*]
    paabBoundingPolysMerge = pAABcornerBoundingPolysMerge[1:*]

mok:  if ~mergeOK then begin
      aabPairs = lonArr(nCornerAABsMerge*(nCornerAABsMerge-1))
      result   = call_external(libLoc, 'find_intersections2D',$
                               fix(aabBoundingBoxesMerge, type = 4),$
                               fix(nCornerAABs, type = 2),$
                               fix(2, type = 2),$
                               aabPairs,$
                               aabPairCount, /unload)
      mergeOK = aabPairCount eq 0
    endif
  endwhile
stop
  nNonCornerAABsOut    = 0L
  nCornerAABsOut       = 0L
  aabPairedListOut     = objArr(1)
  aabCornerListOut     = objArr(1)
  aabBoundingBoxesOut  = [-1]
  paabBoundingPolysOut = ptrArr(1)
  aabCornerBoundingBoxesOut  = [-1]
  paabCornerBoundingPolysOut = ptrArr(1)

  for i = 0L, nCornerAABs-1 do begin
    aabBoundingBoxPos = 4*i
    att0 = (aabCornerList[i])->getAttributeByPosition(0)
    if (att0.aabType eq 'corner') then begin
      nCornerAABsOut  += 1
      aabCornerListOut = [aabCornerListOut, aabCornerList[i]]
      aabCornerBoundingBoxesOut  = [aabCornerBoundingBoxesOut, aabBoundingBoxesMerge[aabBoundingBoxPos:aabBoundingBoxPos+3]]
      paabCornerBoundingPolysOut = [paabCornerBoundingPolysOut, paabBoundingPolysMerge[i]]
    endif else begin ; In case some AABs were not found suitable for corner
      nNonCornerAABsOut += 1
      aabPairedListOut     = [aabPairedListOut, aabCornerList[i]]
      aabBoundingBoxesOut  = [aabBoundingBoxesOut, aabBoundingBoxesMerge[aabBoundingBoxPos:aabBoundingBoxPos+3]]
      paabBoundingPolysOut = [paabBoundingPolysOut, paabBoundingPolysMerge[i]]
    endelse
  endfor

  if (self.fVerbose) then print, nCornerAABsOut, ' corner, ', nNonCornerAABsOut, ' non-corner AABs found'
  if (nCornerAABsOut ge 1) then begin
    aabCornerListOut = aabCornerListOut[1:*]
    aabCornerBoundingBoxesOut  = aabCornerBoundingBoxesOut[1:*]
    paabCornerBoundingPolysOut = paabCornerBoundingPolysOut[1:*]
stop
    fNonCornerVec = bytArr(nPairedAABs) + 1
    for i = 0L, nPairedAABs-1 do begin
      fInCorner = 0b
      for j = 0L, nCornerAABsOut-1 do begin
        attPairedAABsFromCorner = aabCornerListOut[j]->getAttributeByName('paired_AABs')
        if (whereObjInObjArray(aabPairedList[i], **attPairedAABsFromCorner.pData[0]) ge 0) $
        then fInCorner = 1b
      endfor
      fNonCornerVec[i] = fInCorner eq 0
    endfor
    if (total(fNonCornerVec) gt 0) then begin
      whereNC              = where(fNonCornerVec gt 0) + 1 ; shift by the empty list-head
      aabPairedList        = aabPairedList[whereNC]
      aabBoundingBoxesOut  = aabBoundingBoxesOut[whereNC]
      paabBoundingPolysOut = paabBoundingPolysOut[whereNC]
    endif
  endif else begin
    aabBoundingBoxesOut  = aabBoundingBoxes
    paabBoundingPolysOut = paabBoundingPolys
  endelse

  fSkipJunctionSetting = 1
  if (nCornerAABsOut gt 0) and (~fSkipJunctionSetting) then begin
    pJunctionCandidateROIs = ptrArr(self.numRois)
    pJunctionCandidateAABs = ptrArr(self.numRois)
    for i = 0, nCornerAABsOut-1 do begin
      nRoisInt12 = getNumberOfUniqueROIsFromAABlist(aabCornerListOut[i], roiIndexVectorUnique = roiIndexVectorUnique)
      if (nRoisInt12 le 2) then stop ; Should not happen

      ; The ROI with the lower index gets to register the other ROIs (with greater indices) as neighbors.
      minRoiIndex = min(roiIndexVectorUnique)
      roiIndices  = roiIndexVectorUnique[where(roiIndexVectorUnique ne minRoiIndex)]
      if ~ptr_valid(pJunctionCandidateROIs[minRoiIndex]) then begin
        pArrNew = [ptr_new(roiIndices)]
      endif else begin
        pArrNew = *pJunctionCandidateROIs[minRoiIndex] ; it's a pointer array
        pArrNew = [pArrNew, ptr_new(roiIndices)]
      endelse
      pJunctionCandidateROIs[minRoiIndex] = ptr_new(pArrNew)

      if ~ptr_valid(pJunctionCandidateAABs[minRoiIndex]) then begin
        oArrNew = [aabCornerListOut[i]]
      endif else begin
        oArrNew = *pJunctionCandidateAABs[minRoiIndex] ; it's a pointer array
        oArrNew = [oArrNew, aabCornerListOut[i]]
      endelse
      pJunctionCandidateAABs[minRoiIndex] = ptr_new(oArrNew)
    endfor
  endif
;  stop
end


; checkCornerAABsForAllRois
;
; Find and merge (if any) AABs that contain a corner point for 3 or more ROIs.
;
; Preconditions:
;  - pairwise AABs for all ROIs have been computed and stored in aabPairedList.
;
; Arguments:
;  aabPairedList      Input array containing the ROI-paired AABs to look for corners.
;  aabBoundingBoxes   Input array containing the bounding box coordinates of the input ROI-paired AABs.
;  paabBoundingPolys  Input array containing the bounding polygon pointers of mathe input ROI-paired AABs.
;  aabCornerList      Output array containing the corner AABs.
pro C_sAABContainer::checkCornerAABsForAllRois_dev0, aabPairedList, aabBoundingBoxes, paabBoundingPolys, aabCornerList = aabCornerList

  aabCountTotal   = n_elements(aabPairedList)
  if (aabCountTotal lt 2) then return

  fAABMergeOK     = 0b
  mergeIterations = 0
  nCornerAABs     = n_elements(aabCornerList)
  if (nCornerAABs gt 0) then for i = 0u, nCornerAABs-1 do obj_destroy, aabCornerList[i]
  aabCornerList   = objArr(1)
  nCornerAABs     = 0u
  fLog = 1

  nBBoxes = n_elements(aabzoneBoundingBoxes) / 4
  if (self.fVerbose and fLog) then begin
    msg = 'Merge Iteration 0 - ' + num2strTrim(nBBoxes) + ' Bounding Boxes'
    print, msg
    ;fileLogger, msg, logFileLoc
  endif

  aabPairs     = lonArr(aabCountTotal*(aabCountTotal-1))
  aabPairCount = -1L
  libLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
  result = call_external(libLoc, 'find_intersections2D',$
                         fix(aabBoundingBoxes, type = 4),$
                         fix(aabCountTotal, type = 2),$
                         fix(2, type = 2),$
                         aabPairs,$
                         aabPairCount, /unload)

  if ((self.fVerbose gt 0) and (aabPairCount le 10)) $
  then print, aabPairCount, ' AAB zone pairs overlap: ', (aabPairCount gt 0 ? aabPairs[0: aabPairCount*2-1] : '.')

  ; No broad-band intersections for bounding boxes => no corners => return
  if (aabPairCount eq 0) then return

  nIntersectingPairsUniq = 0u
  nIntersectingPairs = 0u
  pIntersectingPairs = ptrArr(1)
  intersectingPairs  = [-1L]
  aabPairsPruned     = [-1L]

  ; If bounding boxes intersect, check bounding polygon intersections
  for i = 0u, aabPairCount-1 do begin

    pairPos    = 2*i
    nRoisInt12 = getNumberOfUniqueROIsFromAABlist(aabPairedList[aabPairs[pairPos:pairPos+1]])
    if (nRoisInt12 le 2) then continue ; Maybe interesting to check someday?

    pBP1     = paabBoundingPolys[aabPairs[pairPos]]
    pBP2     = paabBoundingPolys[aabPairs[pairPos+1]]
    int12    = calcPolygonIntersectionAlt(p1x = (*pBP1)[*,0], p1y = (*pBP1)[*,1], p2x = (*pBP2)[*,0], p2y = (*pBP2)[*,1])
    int12num = int12[0]
    ; if the bounding polygons (tigher than the boxes) do not intersect, skip merging
    if (int12num lt 1) then continue

    ; Keep count of the number of "unique" AAB pairs (one for each pair of AABs)
    nIntersectingPairsUniq += 1
    ; Make list of "non-unique" AAB pairs (according to the bounding polygons, but it a better criterion could be used)
    nIntersectingPairs += int12num
    aabPairsPruned = [aabPairsPruned, aabPairs[pairPos:pairPos+1]]
    pair12indices  = aabPairs[pairPos:pairPos+1]
    p12intPolys    = ptrArr(int12num)
    curPolyStart   = 1
    for j = 0u, int12num-1 do begin
      intersectingPairs = [intersectingPairs, pair12indices]
      nPolyVertices     = int12[curPolyStart]
      polyXcoords       = int12[curPolyStart+1:curPolyStart+nPolyVertices]
      polyYcoords       = int12[curPolyStart+1+nPolyVertices:curPolyStart+1+2*nPolyVertices]
      p12intPolys[j]    = ptr_new([[polyXcoords], [polyYcoords]])
      curPolyStart     += 2*nPolyVertices + 1
    endfor
    pIntersectingPairs = [pIntersectingPairs, p12intPolys]

  endfor

  ; No intersections for bounding polygons => no corners => return
  if (nIntersectingPairsUniq lt 1) then return
  intersectingPairs  = intersectingPairs[1:*]
  pIntersectingPairs = pIntersectingPairs[1:*] ; TODO JJ keep or delete these pointers?
  aabCornerList      = objArr(nIntersectingPairs)

  for i = 0u, nIntersectingPairsUniq-1 do begin
    pairPos       = 2*i
    pairVectorPos = aabPairsPruned[pairPos:pairPos+1]
    pairBBoxPos   = [uIndGen(4)+ 4*aabPairsPruned[pairPos], uIndGen(4) + 4*aabPairsPruned[pairPos+1]]
    aabCornerList[i] = self->mergeAABsForCorner(aabPairedList[pairVectorPos],$
                                                boundingBoxList = aabBoundingBoxes[pairBBoxPos],$
                                                boundingBoxOut  = bbOut, pBoundingPolyOut = pbpOut,$
                                                pBoundingPolyIn = paabBoundingPolys[pairVectorPos])
  endfor

  fAABMergeOK = aabPairCount eq 0
  aabIndices  = aabPairs[uniq(aabPairs, sort(aabPairs))]
  nAABs = n_elements(aabIndices)
;  aabIndices00 = bytArr(nAABs)
;  aabIndices10 = bytArr(nAABs)
;  aabIndices20 = bytArr(nAABs)
;  aabIndices30 = bytArr(nAABs)
;  aabIndices40 = bytArr(nAABs)
;  aabIndices50 = bytArr(nAABs)

  while ~fAABMergeOK do begin
    mergeIterations += 1
    msg = 'Merge Iteration ' + num2strTrim(mergeIterations) + ' - ' + num2strTrim(nBBoxes) + ' Bounding Boxes'
    print, msg
    fileLogger, msg, logFileLoc
    fileLogger, string(aabzoneBoundingBoxes), logFileLoc
    msg = num2strTrim(aabPairCount) + ' AAB zone pairs overlap' + (aabPairCount lt 10 ? aabPairs[0:aabPairCount*2-1] : '.')
    if (self.fVerbose gt 0) then print, msg
    fAABMergeOK = 1
  endwhile
end


pro C_sAABContainer::checkCornerAABsForAllRois_dev1, aabPairedList, aabBoundingBoxes, paabBoundingPolys, aabCornerList = aabCornerList

  fAABMergeOK     = 0b
  mergeIterations = 0
  aabCountTotalPreviousIteration = aabCountTotal
  if (fLog eq 1) then begin
    fileLogger, 'Merge Iteration 0 - Bounding Boxes: ', logFileLoc
    fileLogger, string(aabzoneBoundingBoxes), logFileLoc
  endif

  while ~fAABMergeOK do begin

    aabPairs     = lonArr(aabCountTotal*(aabCountTotal-1))
    aabPairCount = -1L

    if (mergeIterations eq 0) then begin
      libLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
      result = call_external(libLoc, 'find_intersections2D',$
                             fix(aabzoneBoundingBoxes, type = 4),$
                             fix(aabCountTotal, type = 2),$
                             fix(2, type = 2),$
                             aabPairs,$
                             aabPairCount, /unload)

      if ((self.fVerbose gt 0) and (aabPairCount le 10)) $
      then print, aabPairCount, ' AAB zone pairs overlap', (aabPairCount gt 0 ? aabPairs[0: aabPairCount*2-1] : '.')
    endif else begin
      aabPairsBB     = lonArr(aabCountTotal*(aabCountTotal-1))
      aabPairCountBB = -1L
      libLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
      result = call_external(libLoc,$
                             'find_intersections2D',$
                             fix(aabzoneBoundingBoxes, type = 4),$
                             fix(aabCountTotal, type = 2),$
                             fix(2, type = 2),$
                             aabPairsBB,$
                             aabPairCountBB, /unload)
      aabPairsBB   = (aabPairCountBB gt 0) ? aabPairsBB[0:2*aabPairCountBB-1] : [-1]
      aabPairCount = 0L
      aabPairs     = [-1L]
      if (aabPairCountBB gt 0) then begin
        for ibb = 0L, aabPairCountBB-1 do begin
          aabPairPosBB  = 2*ibb
          boundingPolyI = AABListBoundingPolys[aabPairsBB[aabPairPosBB]]
          boundingPolyJ = AABListBoundingPolys[aabPairsBB[aabPairPosBB+1]]
          intersectIJ = calcPolygonIntersectionAlt(p1x = (*boundingPolyI)[*,0], p1y = (*boundingPolyI)[*,1], p2x = (*boundingPolyJ)[*,0], p2y = (*boundingPolyJ)[*,1])
          ;if (intersectIJ[0] gt 1) then begin
          ;  print, 'Found ', intersectIJ[0], ' intersection polygons'
          ;  stop
          ;endif
          if (intersectIJ[0] gt 0) then begin
            aabPairCount += 1
            aabPairs = [aabPairs, aabPairsBB[aabPairPosBB], aabPairsBB[aabPairPosBB+1]]
          endif
        endfor
      endif
      if (aabPairCount gt 0) then aabPairs = aabPairs[1:*]
    endelse

    if (aabPairCount eq 0) then break
    mergeIterations += 1
    if (self.fVerbose gt 0) then print, 'Merge iteration ', mergeIterations

    aabPairs = aabPairs[0: aabPairCount*2-1]
    for i = 0L, aabPairCount-1 do begin ;TODO this is a loop to sort pairs -> change DLL code to return them sorted
      aabPairPos = 2*i
      if (aabPairs[aabPairPos] gt aabPairs[aabPairPos+1]) then begin
        tmp = aabPairs[aabPairPos]
        aabPairs[aabPairPos]   = aabPairs[aabPairPos+1]
        aabPairs[aabPairPos+1] = tmp
      endif
    endfor

    aabFreeList = objArr(1)
    aabFreeBBox = [-1]
    pAabFreeBPoly = ptrArr(1)
    for i = 0L, aabCountTotal-1 do begin
      whI = where(aabPairs eq i, countI)
      if (countI eq 0) then begin
        aabFreeList   = [aabFreeList, aabList[i]]
        aabFreeBBox   = [aabFreeBBox, aabzoneBoundingBoxes[4*i: 4*i+3]]
        pAabFreeBPoly = [pAabFreeBPoly, AABListBoundingPolys[i]]
      endif
    endfor
    nFreeAABs = n_elements(aabFreeList)-1

    aabIntersection = ptrArr(aabCountTotal)
    for i = 0L, aabPairCount-1 do begin
      aabPairPos = 2*i
      ;if (aabPairs[aabPairPos] eq 1691) then stop
      if (ptr_valid(aabIntersection[aabPairs[aabPairPos]])) then begin
        newArray = *aabIntersection[aabPairs[aabPairPos]]
        aabIntersection[aabPairs[aabPairPos]] = ptr_new([newArray, aabPairs[aabPairPos+1]])
      endif else begin
        aabIntersection[aabPairs[aabPairPos]] = ptr_new([aabPairs[aabPairPos+1]])
      endelse
    endfor

    ;if (self.fVerbose gt 0) then begin
    ;  print, 'Intersections before pruning:'
    ;  ;for i = 0L, aabCountTotal-1 do print, i, ':', ptr_valid(aabIntersection[i]) ? *aabIntersection[i] : '-'
    ;  for i = 0L, aabCountTotal-1 do if ptr_valid(aabIntersection[i]) then print, i, ':', *aabIntersection[i]
    ;endif
    if (fLog eq 1) then begin
      fileLogger, 'Merge Iteration ' + string(mergeIterations) + ' - Intersections before pruning:', logFileLoc
      for i = 0L, aabCountTotal-1 do begin
        if ptr_valid(aabIntersection[i]) then begin
          fileLogger, strCompress(string(i), /remove_all) + ':', logFileLoc
          fileLogger, string(*aabIntersection[i]), logFileLoc
        endif
      endfor
    endif

    AABListOrphan = objArr(1)
    AABListOrphanBoundingPolys = ptrArr(1)
    aabzoneOrphanBoundingBoxes = [-1.0]
    nOrphanAABs = 0L

    for i = 0L, aabCountTotal-2 do begin

      if (ptr_valid(aabIntersection[i])) then begin
        intersectionsFromI  = *aabIntersection[i]
        nIntersectionsFromI = n_elements(intersectionsFromI)
        for j = i+1, aabCountTotal-1 do if (ptr_valid(aabIntersection[j])) then begin
          prunedList = [-1]
          intersectionsFromJ  = *aabIntersection[j]
          nIntersectionsFromJ = n_elements(intersectionsFromJ)
          for k = 0L, nIntersectionsFromJ-1 do begin
            whIntI = where(intersectionsFromI eq intersectionsFromJ[k], countExists)
            if (countExists eq 0) then prunedList = [prunedList, intersectionsFromJ[k]]
          endfor
          nPrunedList = n_elements(prunedList)-1
          if (nPrunedList gt 0) $
          then aabIntersection[j] = ptr_new(prunedList[1:*]) $
          else begin
            nOrphanAABs += 1
            aabIntersection[j] = ptr_new()
            AABListOrphan      = [AABListOrphan, AABlist[j]]
            aabzoneOrphanBoundingBoxes = [aabzoneOrphanBoundingBoxes, aabzoneBoundingBoxes[4*j: 4*j+3]]
            AABListOrphanBoundingPolys = [AABListOrphanBoundingPolys, AABListBoundingPolys[j]]
          endelse
        endif
      endif
    endfor
    ;if (self.fVerbose gt 0) then begin
    ;  print, 'Intersections after pruning:'
    ;  ;for i = 0L, aabCountTotal-1 do print, i, ':', ptr_valid(aabIntersection[i]) ? *aabIntersection[i] : '-'
    ;  for i = 0L, aabCountTotal-1 do if ptr_valid(aabIntersection[i]) then print, i, ':', *aabIntersection[i]
    ;endif

    if (fLog eq 1) then begin
      fileLogger, 'Merge Iteration ' + string(mergeIterations) + ' - Intersections after pruning:', logFileLoc
      for i = 0L, aabCountTotal-1 do $
        if ptr_valid(aabIntersection[i]) then begin
          fileLogger, strCompress(string(i), /remove_all) + ':', logFileLoc
          fileLogger, string(*aabIntersection[i]), logFileLoc
        endif
    endif

    AABListMerge = objArr(1)
    AABListMergeBoundingPolys = ptrArr(1)
    aabzoneMergeBoundingBoxes = [-1.0]
     ; Set flags to keep track of AABs which have merge lists and also are listed in other merge list(s), in order to avoid duplicates
    fAABsInMergeList  = bytArr(aabCountTotal)
    for i = 0L, aabCountTotal-1 do if (ptr_valid(aabIntersection[i])) $
    then fAABsInMergeList[*aabIntersection[i]] = 1
    fAABsWithAndInMergeLists = ptr_valid(aabIntersection) and fAABsInMergeList

    for i = 0L, aabCountTotal-1 do if (ptr_valid(aabIntersection[i])) then begin
      ;if (self.fVerbose gt 0) then print, 'Merge AABs ', i, ':', *aabIntersection[i]
      bbox   = aabzoneBoundingBoxes[4*i:4*i+3]
      pbpoly = [AABListBoundingPolys[i]]
      for j = 0L, n_elements(*aabIntersection[i])-1 do begin
        bbox   = [bbox, aabzoneBoundingBoxes[4*(*aabIntersection[i])[j]:4*(*aabIntersection[i])[j]+3]]
        pbpoly = [pbpoly, AABListBoundingPolys[(*aabIntersection[i])[j]]]
      endfor

      nIntersectingAABrois = getNumberOfUniqueROIsFromAABlist(AABlist[[i,*aabIntersection[i]]], roiIndexVectorUnique = roiIndexVectorUnique)
      if (nIntersectingAABrois gt 2) then begin
        calcPolygonIntersectionMulitpleWithPlot, pbPoly, pIntersections = pIntersections
        fNonEmptyIntersection = ptr_valid(pIntersections[0])
      endif else $
        fNonEmptyIntersection = 0
; Here! avoid duplicities by checking fAABsWithAndInMergeLists
      fPerformMerge = fNonEmptyIntersection and (nIntersectingAABrois gt 2); and other things, if need be
      if (fPerformMerge ne 0) then begin
        AABListMerge = [AABListMerge, self->mergeAABsForCorner(AABlist[[i,*aabIntersection[i]]], roiIndexVectorUnique = roiIndexVectorUnique, boundingBoxList = bbox, boundingBoxOut = bboxOut, pBoundingPolyIn = pbPoly, pBoundingPolyOut = pbpOut)]
        AABListMergeBoundingPolys = [AABListMergeBoundingPolys, pbpOut]
        aabzoneMergeBoundingBoxes = [aabzoneMergeBoundingBoxes, bboxOut]
      endif else begin
        AABListMerge = [AABListMerge, AABlist[[i,*aabIntersection[i]]]]
        AABListMergeBoundingPolys = [AABListMergeBoundingPolys, pbPoly]
        aabzoneMergeBoundingBoxes = [aabzoneMergeBoundingBoxes, bbox]
      endelse
    endif

    ; TODO JJ here it would be necessary to delete unused bounding polygon pointers
    aabList = AABListMerge[1:*]
    aabzoneBoundingBoxes = aabzoneMergeBoundingBoxes[1:*]
    AABListBoundingPolys = AABListMergeBoundingPolys[1:*]

    if (nFreeAABs gt 0) then begin
      aabList = [aabList, aabFreeList[1:*]]
      aabzoneBoundingBoxes = [aabzoneBoundingBoxes, aabFreeBBox[1:*]]
      AABListBoundingPolys = [AABListBoundingPolys, pAabFreeBPoly[1:*]]
    endif

    if (nOrphanAABs gt 0) then begin
      aabList = [aabList, AABListOrphan[1:*]]
      aabzoneBoundingBoxes = [aabzoneBoundingBoxes, aabzoneOrphanBoundingBoxes[1:*]]
      AABListBoundingPolys = [AABListBoundingPolys, AABListOrphanBoundingPolys[1:*]]
    endif

    aabCountTotal = n_elements(AABList)
    fAABMergeOK   = (aabCountTotal eq 1) or (aabCountTotal eq aabCountTotalPreviousIteration)
    if (self.fVerbose gt 0) then begin;and (fFileLoaded eq 1) then begin
      szImg = size(*self.pImage, /dim)
      window, 16, xSize = szImg[0], ySize = szImg[1], title = 'Merge iteration ' + num2strTrim(mergeIterations)
      ;tvscl, *self.pImage
      ;colors = ['0000FF'x, '00FF00'x, 'FF0000'x, '00FFFF'x, 'FFFF00'x, 'FF00FF'x, '000088'x, '008800'x, '880000'x, 'FFFFFF'x]
      plot, (*AABListBoundingPolys[0])[*,0], (*AABListBoundingPolys[0])[*,1], lineStyle = 0, xrange = [0,szImg[0]], yrange = [0,szImg[1]], xmargin = [0,0], ymargin = [0,0]; color = colors[0], /NOERASE
      for i = 1L, aabCountTotal-1 do begin
        ;colorIndex = (aabCountTotal / 100) mod 10
        oplot, (*AABListBoundingPolys[i])[*,0], (*AABListBoundingPolys[i])[*,1], lineStyle = 0;, color = colors[colorIndex]
        ;xyouts, (*AABListBoundingPolys[i])[0,0], (*AABListBoundingPolys[i])[0,1], strCompress(string(i mod 100)), charsize = 0.5;, color = colors[colorIndex]
      endfor
    endif

    if (fLog eq 1) then begin
      fileLogger, 'Merge Iteration ' + string(mergeIterations) + ' - Bounding Polygons:', logFileLoc
      for i = 0L, aabCountTotal-1 do begin
        fileLogger, strCompress(string(i), /remove_all) + ': ', logFileLoc
        fileLogger, 'x', logFileLoc
        fileLogger, string((*AABListBoundingPolys[i])[*,0]), logFileLoc
        fileLogger, 'y', logFileLoc
        fileLogger, string((*AABListBoundingPolys[i])[*,1]), logFileLoc
      endfor
      fileLogger, 'Merge Iteration ' + string(mergeIterations) + ' - Completed at ' + string(sysTime()), logFileLoc
      fileLogger, '', logFileLoc
    endif
  endwhile
end


pro C_sAABContainer::getFolderFileNamePrefix, prefix, baseFileName, folderPath

  pathSep      = path_sep()
  posLastSep   = strPos(self.roiGroupFilePath, pathSep, /reverse_search)
  baseFileName = strMid(self.roiGroupFilePath, posLastSep+1)
  distPrefix   = strCompress('proxDist' + string(self.proximityDist), /remove_all)
  prefix       = distPrefix

  ; TODO JJW turn these into input/class parameters
  fFillZoneGaps    = 1
  FillZoneGapIndex = 3
  FillZoneGapDist  = self.proximityDist

  if fFillZoneGaps then $
    prefix += '_FillZoneGapsIndex' + strCompress(string(FillZoneGapIndex), /REMOVE_ALL) $
           + '-Dist' + strCompress(string(FillZoneGapDist), /REMOVE_ALL)
  folderPath  = strMid(self.roiGroupFilePath, 0, posLastSep) + pathSep + '_AAB_' + pathSep
end


;+
; checkAABsForAllRois
;
; KEYWORDS
;  checkingType: 0 or not set -> exhaustive verification between ROI boundaries
;                1            -> use bounding volumes (not yet)
;                2            -> use space partition (not yet)
;                3            -> something else
; fUseExternal: flag to activate external library computations instead of the IDL built-ins (usually slower).
;-
function C_sAABContainer::checkAABsForAllRois, roiIndexVector = roiIndexVector, contourIndexVector = contourIndexVector, checkingType = checkingType, $
                                               fUseExternal = fUseExternal, fSkipCornerCheckings = fSkipCornerCheckings, fTryFileLoad = fTryFileLoad, $
                                               AABlistNames = AABlistNames
  fUseExternal = keyword_set(fUseExternal)

  case n_elements(roiIndexVector) of
    0: begin
       if (self.fVerbose gt 0) then print, 'Using default roiIndexVector/contourIndexVector'
       roiIndexVector = uIndGen(self.numContours)
       contourIndexVector = roiIndexVector
       endcase
    1: begin
       if (self.fVerbose gt 0) then print, 'Using filtered roiIndexVector/contourIndexVector (single element)'
       roiIndexVector = [roiIndexVector]
       contourIndexVector = [contourIndexVector]
       endcase
    else: if (self.fVerbose gt 0) then print, 'Using filtered roiIndexVector/contourIndexVector'
  endcase
  if (self.fVerbose gt 0) then begin
    print, 'roiIndexVector: '    , roiIndexVector
    print, 'contourIndexVector: ', contourIndexVector
  endif
  nContours = n_elements(roiIndexVector)

  if (nContours lt 1) or ~(ptr_valid(self.pXcoords)) then begin
    print, 'No contours set. Returning. (numContours = ', nContours, '), ptr_valid(self.pXcoords) = ', ptr_valid(self.pXcoords)
    return, 0
  end

  if (nContours eq 1) then begin
    print, 'Proximity/intersection checking not necessary for only one contour.'
    return, 0
  endif

  fUseSavFile              = 1b or keyword_set(fTryFileLoad)
  fSaveAndOverwriteSavFile = 1b ; For now it requires fUseSavFile = 0 to act as overwriting flag
  fLog                     = 0b
  self->getFolderFileNamePrefix, prefix, baseFileName, folderPath

  oAABListFileNameNM     = prefix + baseFileName + '_oAABListROIPairsNonMerged.sav'
  oAABBBFileNameNM       = prefix + baseFileName + '_oAABBBListROIPairsNonMerged.sav'
  oAABBPFileNameNM       = prefix + baseFileName + '_oAABBPListROIPairsNonMerged.sav'
  oAABListFileName       = prefix + baseFileName + '_oAABListROIPairsMerged.sav'
  oAABBBFileName         = prefix + baseFileName + '_oAABBBListROIPairsMerged.sav'
  oAABBPFileName         = prefix + baseFileName + '_oAABBPListROIPairsMerged.sav'
  oAABvertexTypeFileName = prefix + baseFileName + '_oAABvertexType.sav'

  logFileLoc  = folderPath + prefix + baseFileName + '.txt'
  fFileLoaded = 0b
  fFileLoadedNonMerge = 0b
  if (fLog eq 1) then fileLogger, 'Log started at ' + string(sysTime()), logFileLoc
  if (fUseSavFile and (file_test(folderPath, /DIRECTORY) eq 1)) then begin
    if (file_test(folderPath + oAABListFileName) and file_test(folderPath + oAABBBFileName) and file_test(folderPath + oAABvertexTypeFileName)) then begin
      print, 'Loading saved files: ', oAABListFileName, ',', oAABBBFileName, ', ', oAABvertexTypeFileName; & stop
      restore, folderPath + oAABListFileName
      restore, folderPath + oAABBBFileName
      restore, folderPath + oAABBPFileName
      restore, folderPath + oAABvertexTypeFileName
      self.pAABvertexType = ptr_new(vertexType, /NO_COPY)
      aabCountTotal = obj_valid(aabList[0]) ? n_elements(aabList) : 0
      fFileLoaded = 1b
      goto, l
    endif ;else $
;    if (file_test(folderPath + oAABListFileNameNM) and file_test(folderPath + oAABBBFileNameNM)) then begin
;      print, 'Loading saved files: ', oAABListFileNameNM, ',', oAABBBFileNameNM
;      restore, folderPath + oAABListFileNameNM
;      restore, folderPath + oAABBBFileNameNM
;      restore, folderPath + oAABBPFileNameNM
;      self.pAABvertexType = ptr_new(vertexType, /NO_COPY)
;      aabCountTotal = obj_valid(aabList[0]) ? n_elements(aabList) : 0
;      fFileLoadedNonMerge = 1b
;      goto, l
;    endelse
  endif

  ; Count of how many AAB zones there are for each contour, regardless of the type (for now).
  ;aabCountList  = uIntArr(self.numContours)
  aabCountTotal = 0ul
  aabList       = objArr(1)
  self.pAdjacentROIindexVector = ptr_new(ptrArr(self.numRois))

  checkingType = 0 ; TODO hard-coded by now... 0-> exhaustive, 1-> sweep and prune
  if (self.fVerbose gt 0) then print, 'Going for overlap/proximity computations. Checking type is ', checkingType

  ;numContoursBack = self.numContours
  ;self.numContours = 2
  aabzoneBoundingBoxes = [-1.0]
  AABListBoundingPolys = ptrArr(1)
  case checkingType of

  ; All ROI pairs checking.
  0: begin
    for i = 0L, nContours-2 do begin

      iXBox = *(*(self.pXcoordsBox)[0])[contourIndexVector[i]]
      iYBox = *(*(self.pYcoordsBox)[0])[contourIndexVector[i]]
      ;iZBox = *(*(self.pZcoordsbox)[0])[contourIndexVector[i]]

      for j = i+1, nContours-1 do begin

        jXBox = *(*(self.pXcoordsBox)[0])[contourIndexVector[j]]
        jYBox = *(*(self.pYcoordsBox)[0])[contourIndexVector[j]]

        fInX = (((jXBox[1] ge iXBox[0]) and (jXBox[0] le iXBox[0])) $
             or ((jXBox[0] le iXBox[1]) and (jXBox[1] ge iXBox[0])) $
             or ((jXBox[0] ge iXBox[0]) and (jXBox[1] le iXBox[1])))
        fInY = (((jYBox[1] ge iYBox[0]) and (jYBox[0] le iYBox[0])) $
             or ((jYBox[0] le iYBox[1]) and (jYBox[1] ge iYBox[0])) $
             or ((jYBox[0] ge iYBox[0]) and (jYBox[1] le iYBox[1])))

        ; Bounding box overlap requires both x and y ranges intersect
        if ~(fInX and fInY) then continue
;if ((i eq 0) and (j eq 8)) then stop
        if (self.fVerbose) then begin
          strRois = 'i-j ' + num2strTrim(i) + '-' + num2strTrim(j) + $
                    ' / ROIs ' + num2strTrim(roiIndexVector[i]) + '-' + num2strTrim(roiIndexVector[j]) + $
                    ' / contour indices ' + num2strTrim(contourIndexVector[i]) + '-' + num2strTrim(contourIndexVector[j])
          print, 'Going for detailed check: ', strRois
;          print, 'box[',i,']: [',iXBox[0],',',iXBox[1],']x[',iYBox[0],',',iYBox[1],']'
;          print, 'box[',j,']: [',jXBox[0],',',jXBox[1],']x[',jYBox[0],',',jYBox[1],']'
        endif
        contourIndexI = contourIndexVector[i]
        contourIndexJ = contourIndexVector[j]
        aabCountIJ = self->checkAABsForROIpair(roiIndexVector[i], (*(self.pXcoords)[0])[contourIndexI], (*(self.pYcoords)[0])[contourIndexI],$
                                               roiIndexVector[j], (*(self.pXcoords)[0])[contourIndexJ], (*(self.pYcoords)[0])[contourIndexJ],$
                                               AABlist = AABlistIJ, aabzoneBoundingBoxes = aabzoneBoundingBoxesIJ, aabBoundingPolys = AABListBoundingPolysIJ,$
                                               fFillZoneGaps = fFillZoneGaps, FillZoneGapIndex = FillZoneGapIndex, FillZoneGapDist = FillZoneGapDist)
        if (aabCountIJ gt 0) then begin
          AABlist = [AABlist, AABlistIJ]
          aabzoneBoundingBoxes = [aabzoneBoundingBoxes, aabzoneBoundingBoxesIJ]
          AABListBoundingPolys = [AABListBoundingPolys, AABListBoundingPolysIJ]
          aabCountTotal += aabCountIJ
        endif
      endfor
    endfor
  endcase

  ; "Sweep & Prune" search algorithm (sorts the bounding boxes and then scans for contact)
  1: begin
    ; array for coordinates in order, necessary for SAP algorithm
    boxesCoords = fltArr(nContours*4)
    pairsArray  = lonArr(nContours*(nContours-1))
    pairsCount  = -1

    for i = 0L, nContours-1 do begin
      iXBox = *(*(self.pXcoordsBox)[0])[contourIndexVector[i]]
      iYBox = *(*(self.pYcoordsBox)[0])[contourIndexVector[i]]
      boxesCoords[4*i]   = iXBox[0]
      boxesCoords[4*i+1] = iYBox[0]
      boxesCoords[4*i+2] = iXBox[1]
      boxesCoords[4*i+3] = iYBox[1]
    endfor
    if (self.fVerbose gt 0) then print, 'AABBs (boxes) coords = ', boxesCoords
    ;tSapS = sysTime(1)
    libLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
    result = call_external(libLoc,$
                           'find_intersections2D',$
                           fix(boxesCoords, type = 4),$
                           fix(nContours, type = 2),$
                           fix(2, type = 2),$
                           pairsArray,$
                           pairsCount, /unload)
    ;tSapE = sysTime(1) - tSapS
    for i = 0L, pairsCount-1 do $
      pos1 = (pairsArray[i])[0]
      pos2 = (pairsArray[i])[1]
      contourIndexI = contourIndexVector[pos1]
      contourIndexJ = contourIndexVector[pos2]
      aabCountIJ = self->checkAABsForROIpair(roiIndexVector[pos1], (*(self.pXcoords)[0])[contourIndexI], (*(self.pYcoords)[0])[contourIndexI],$
                                             roiIndexVector[pos2], (*(self.pXcoords)[0])[contourIndexJ], (*(self.pYcoords)[0])[contourIndexJ],$
                                             AABlist = AABlistIJ, aabzoneBoundingBoxes = aabzoneBoundingBoxesIJ, aabBoundingPolys = AABListBoundingPolysIJ, $
                                             fFillZoneGaps = fFillZoneGaps, FillZoneGapIndex = FillZoneGapIndex, FillZoneGapDist = FillZoneGapDist)
    if (aabCountIJ gt 0) then begin
      AABlist = [AABlist, AABlistIJ]
      aabzoneBoundingBoxes = [aabzoneBoundingBoxes, aabzoneBoundingBoxesIJ]
      AABListBoundingPolys = [AABListBoundingPolys, AABListBoundingPolysIJ]
      aabCountTotal += aabCountIJ
    endif
  endcase
  else: stop
  endcase

  if n_elements(aabList) gt 1 then begin
    aabList = aabList[1:*]
    aabzoneBoundingBoxes = aabzoneBoundingBoxes[1:*]
    AABListBoundingPolys = AABListBoundingPolys[1:*]
  endif

  if (fFileLoaded eq 0) and (fSaveAndOverwriteSavFile eq 1) then begin
    if (file_test(folderPath, /DIRECTORY) eq 0) then file_mkDir, folderPath
    vertexType = *self.pAABvertexType
    save, vertexType,           fileName = folderPath + oAABvertexTypeFileName
    save, aabList,              fileName = folderPath + oAABListFileName
    save, aabzoneBoundingBoxes, fileName = folderPath + oAABBBFileName
    save, AABListBoundingPolys, fileName = folderPath + oAABBPFileName
  endif

l:if (self.fVerbose gt 0) then print, 'Found ', aabCountTotal, ' AAB shared zones in total'
  if (aabCountTotal gt 0) then begin
    nCorners = 0
    minAABCountForCorner = 2 ; Could be 3...
    if (aabCountTotal gt 1) and (self.numRois gt minAABCountForCorner) and ~keyword_set(fSkipCornerCheckings) then begin
      t0 = sysTime(1)
      ;self->checkCornerAABsForAllRois, aabList, aabzoneBoundingBoxes, aabListBoundingPolys, aabCornerListOut = aabCorners,$
      ;                                 aabBoundingBoxesOut = aabBoundingBoxesOut, paabBoundingPolysOut = paabBoundingPolysOut,$
      ;                                 aabCornerBoundingBoxesOut = aabCornerBoundingBoxes, paabCornerBoundingPolysOut = paabCornerBoundingPolys
      ;nCorners = self->checkCornerAABsForAllRois2(aabList, cornerList = aabCornerList, pCornerAABindices = pCornerAABindices, $
      ;                                            pCornerBoundingPolys = aabListBoundingPolys, cornerBoundingBoxes = aabCornerBoundingBoxes)
      nCorners = self->checkAABcorners(aabList, cornerList = aabCornerList, pCornerBoundingPolys = aabListBoundingPolys, $
                                       cornerBoxes = aabCornerBoundingBoxes, pCornerAABindices = pCornerAABindices, pCornerROIindices = pCornerROIindices)
      deltaT = sysTime(1) - t0
      if self.fVerbose then print, 'Ellapsed time for checking corner AABs: ', deltaT, 's'
;stop
      ;aabzoneBoundingBoxes = aabBoundingBoxesOut
      ;for p = 0, n_elements(aabListBoundingPolys)-1 do ptr_free, aabListBoundingPolys[i]
      ;aabListBoundingPolys = paabBoundingPolysOut
      aabCountTotal = n_elements(AABlist) ; It may seem redundant... it's not
    endif
;    Plot for merged AABs bounding boxes, which can be too gross as an estimation
;    if (self.fVerbose gt 0) then begin;and (fFileLoaded eq 1) then begin
;      szImg = size(*self.pImage, /dim)
;      window, 27, xSize = szImg[0], ySize = szImg[1]
;      tvscl, *self.pImage
;      plot, (*AABListMergeBoundingPolys[0])[*,0], (*AABListMergeBoundingPolys[0])[*,1], color = '00FF00'x, lineStyle = 0, xrange = [0,szImg[0]], yrange = [0,szImg[1]], xmargin = [0,0], ymargin = [0,0], /NOERASE
;      for i = 0L, aabCountTotal-1 do begin
;        bbx = [aabzoneBoundingBoxes[4*i], aabzoneBoundingBoxes[4*i+2], aabzoneBoundingBoxes[4*i+2], aabzoneBoundingBoxes[4*i], aabzoneBoundingBoxes[4*i]]
;        bby = [aabzoneBoundingBoxes[4*i+1], aabzoneBoundingBoxes[4*i+1], aabzoneBoundingBoxes[4*i+3], aabzoneBoundingBoxes[4*i+3], aabzoneBoundingBoxes[4*i+1]]
;        ;plot, bbx, bby, color = '0000FF'x, lineStyle = 0, xrange = [0,szImg[0]], yrange = [0,szImg[1]], xmargin = [0,0], ymargin = [0,0], xTickInterval = 128, yTickInterval = 128, /NOERASE
;        ;oPlot, bbx, bby, linestyle = 0, symsize = 1, color='FFFF00'x
;        if ptr_valid(AABListMergeBoundingPolys[i]) then oplot, (*AABListMergeBoundingPolys[i])[*,0], (*AABListMergeBoundingPolys[i])[*,1], color = '00FF00'x, lineStyle = 0
;      endfor
;    endif
    ;for aabCount = 0L, aabCountTotal-1 do begin
    ;  AABList[aabCount]->mergeVertexLists
    ;endfor
    if (fLog eq 1) then fileLogger, 'Log finished at ' + string(sysTime()), logFileLoc
    ;stop
    pAAB = ptrArr(7)
    pAAB[0] = ptr_new(AABlist, /no_copy)
    pAAB[1] = ptr_new(aabzoneBoundingBoxes, /no_copy)
    pAAB[2] = ptr_new(aabListBoundingPolys, /no_copy)
    if (nCorners ge 1) then begin
      pAAB[3] = ptr_new(aabCornerList, /no_copy)
      pAAB[4] = ptr_new(aabCornerBoundingBoxes, /no_copy)
      pAAB[5] = ptr_new(pCornerAABindices, /no_copy)
      pAAB[6] = ptr_new(pCornerROIindices, /no_copy)
    endif
    self.pAABlist = ptr_new(pAAB)
    AABlistNames  = ['aabList', 'aabzoneBoundingBoxes', 'aabListBoundingPolys', 'aabCornerList', 'aabCornerBoundingBoxes', 'pCornerAABindices']
    return, aabCountTotal
  endif

  self->updateVertexCodeVector
  return, 0

end


;+
; adjustSingleContour
;
; Performs the basic snakes algorithm for a given polyline.
;
; NOTES: xCoords/yCoords are overwritten.
;-
pro C_sAABContainer::adjustSingleContour, xCoords, yCoords, iterations    = iterations,$
                                                            errorThresh   = errorThresh,$
                                                            errorMetric   = errorMetric,$
                                                            fFixEndpoints = fFixEndpoints
  npts = n_elements(xCoords)
  if (npts ne n_elements(yCoords)) then begin
    print, 'ERROR in C_sAABContainer::adjustSingleContour : number of elements in x- and y-coords does not match.'
    return
  endif
  nptsMin = keyword_set(fFixEndpoints) ? 3 : 2
  if (npts le nptsMin) then begin
    print, 'ERROR in C_sAABContainer::adjustSingleContour : Too few points to adjust (', npts, ').'
    return
  endif

  iterations  = keyword_set(iterations)  ? iterations > 1 : 140
  errorThresh = keyword_set(errorThresh) ? errorThresh : 0.05
  errorMetric = keyword_set(errorMetric) ? errorMetric : 'haussdorf'

  numType = size(xCoords, /TYPE)
  if (numType ne 4) and (numType ne 5) then stop ; Shouldn't happen, but...

  if (numType eq 4) then begin
    alpha = make_array(npts, value = self.alphaVal, /FLOAT)
    beta  = make_array(npts, value = self.betaVal, /FLOAT)
  endif

  if (numType eq 5) then begin
    alpha = make_array(npts, value = self.alphaVal, /DOUBLE)
    beta  = make_array(npts, value = self.betaVal, /DOUBLE)
  endif

  a = beta
  b = -alpha - 4*beta
  c = 2*alpha + 6*beta

  invArray = invert(diag_matrix(a[0:npts-3], 2) + diag_matrix(a[npts-2:npts-1], -(npts-2)) $
           + diag_matrix(b[0:npts-2],  1) + diag_matrix(b[npts-1], -(npts-1)) $
           + diag_matrix(c + self.gammaVal) $
           + diag_matrix(b[0:npts-2], -1) + diag_matrix(b[npts-1], (npts-1)) $
           + diag_matrix(a[0:npts-3], -2) + diag_matrix(a[npts-2:npts-1], (npts-2)))

  currentError = errorThresh + 1
  iterCount    = 0

  if keyword_set(fFixEndpoints) then begin
    x0 = xCoords[0]
    y0 = yCoords[0]
    xn = xCoords[npts-1]
    yn = yCoords[npts-1]
  endif

  while (iterCount le iterations) and (currentError gt errorThresh) do begin

    iterCount += 1

    vfx = (self.kappaVal gt 0) and (ptr_valid(self.pU)) ? interpolate(*self.pU, xCoords, yCoords, cubic = -0.5) : dblArr(n_elements(xCoords))
    vfy = (self.kappaVal gt 0) and (ptr_valid(self.pV)) ? interpolate(*self.pV, xCoords, yCoords, cubic = -0.5) : dblArr(n_elements(yCoords))

    ; deform the contour
    xCoords = invArray ## (self.gammaVal * xCoords + ((self.kappaVal gt 0) ? self.kappaVal * vfx : 0))
    yCoords = invArray ## (self.gammaVal * yCoords + ((self.kappaVal gt 0) ? self.kappaVal * vfy : 0))

    if keyword_set(fFixEndpoints) then begin
      xCoords[0] = x0
      yCoords[0] = y0
      xCoords[npts-1] = xn
      yCoords[npts-1] = yn
    endif

     ; re-interpolate the contour points
    polygonArcSample, xCoords, yCoords, xOut, yOut, nPoints = npts, /fPolyLine
    if keyword_set(fFixEndpoints) then begin
      xOut[0] = x0
      yOut[0] = y0
      xCoords[npts-1] = xn
      yCoords[npts-1] = yn
    endif

    if (self.fVerbose gt 5) then $
      case iterCount of
         1:          oPlot, [xOut, xOut[0]], [yOut, yOut[0]], color = 255, linestyle = 1, thick = 3
         iterations: oPlot, [xOut, xOut[0]], [yOut, yOut[0]], color = 255, thick = 3
         else:       oPlot, [xOut, xOut[0]], [yOut, yOut[0]], color = (255 - (iterations - j) * 30) > 100
      endcase

    case errorMetric of     ; TODO add the different error criteria
      0:    currentError = s_HausdorffDistanceFor2Dpoints(xCoords, yCoords, xOut, yOut)
      else: currentError = s_HausdorffDistanceFor2Dpoints(xCoords, yCoords, xOut, yOut)
    endcase

    xCoords = xOut
    yCoords = yOut

  endwhile

end


; sortForRoi
;
; Sorts and merges the shared vertex-index lists for a given ROI.
function sortForRoi, pSharedZonesList, nMax, complementRuns = complementRuns

  nSharedZones = n_elements(pSharedZonesList)
  if (nSharedZones lt 1) then return, [-1]

  sharedZoneVertexIndexRuns = uIntArr(nSharedZones * 2)

  if (nSharedZones eq 1) then begin
    sharedZoneVertexIndexRuns = [(*pSharedZonesList[0])[0], (*pSharedZonesList[0])[n_elements(*pSharedZonesList[0])-1]]
  endif else $
    for i = 0u, nSharedZones-1 do begin
      posRun = 2*i
      nElem  = n_elements(*pSharedZonesList[i])
      sharedZoneVertexIndexRuns[posRun:posRun+1] = [(*pSharedZonesList[i])[0], (*pSharedZonesList[i])[nElem-1]]
      ;if (sharedZoneVertexIndexRuns[posRun] ge sharedZoneVertexIndexRuns[posRun+1]) then begin
        ;stop ; TODO The AAB assembly method should send the vertex runs sorted...
      ;  tmpVal = sharedZoneVertexIndexRuns[posRun]
      ;  sharedZoneVertexIndexRuns[posRun]   = sharedZoneVertexIndexRuns[posRun+1]
      ;  sharedZoneVertexIndexRuns[posRun+1] = tmpVal
      ;end
    endfor
;runOri = sharedZoneVertexIndexRuns & runTmp = runOri
;sortRunList_insertionSort, runTmp, circularListMaxIndex = nMax, /fRemoveDuplicatesAndContainedRuns
;print, 'before... ', runOri ; ToDo delete runOri when fully confident in the output ;)
;print, 'after... ', runTmp
;complementRuns = getComplementRuns(uIndGen(nMax+1), runTmp)
  sortRunList_insertionSort, sharedZoneVertexIndexRuns, circularListMaxIndex = nMax, /fRemoveDuplicatesAndContainedRuns
  complementRuns = getComplementRuns(uIndGen(nMax+1), sharedZoneVertexIndexRuns)
  return, sharedZoneVertexIndexRuns
end


function C_sAABContainer::adjustContoursC, iterations = iterations, errorThresh = errorThresh, errorMetric = errorMetric, $
                                           outCoordsX = outCoordsX, outCoordsY = outCoordsY, fDebug = fDebug
  if (self.numContours lt 1) then return, -1

  ; 0. Get parameter values
  iterations     = keyword_set(iterations)  ? iterations > 1 : self.contourIterationsMax
  errorThresh    = keyword_set(errorThresh) ? errorThresh    : self.contourErrorThresh
  iterationError = make_array(self.numContours, /double)
  errorMetric    = n_elements(errorMetric) gt 0 ? errorMetric : self.contourErrorThreshNormType

  fPlotAllRois   = self.fVerbose and 0b
;stop
  ; 1. Perform isolated ROI contour adjustment
  print, 'Adjusting individual ROI contours.'
  outCoordsX = ptrArr(self.numRois)
  outCoordsY = ptrArr(self.numRois)

  for i = 0L, self.numContours-1 do begin
    xCoords = *((*(self.pXcoords)[0])[i])
    yCoords = *((*(self.pYcoords)[0])[i])
    ;self->adjustSingleContour, xCoords, yCoords, iterations = iterations, errorMetric = errorMetric, errorThresh = errorThresh
    ;self->adjustSingleContour, xCoords, yCoords, iterations = 1
    ((*(self.pXcoords)[0])[i]) = ptr_new(xCoords)
    ((*(self.pYcoords)[0])[i]) = ptr_new(yCoords)
    outCoordsX[i] = ptr_new(xCoords)
    outCoordsY[i] = ptr_new(yCoords)
  endfor

  fLogStats = 1
  self->getFolderFileNamePrefix, prefix, baseFileName, folderPath
  logMorphoStatsFilePath = folderPath + prefix + baseFileName + '_morphoStats_initialContours.csv'
  morphoStatsSizeFact = 0.166
  if fLogStats then begin
    fileLogger, 'Initial ROI Stats;' + num2strTrim(self.numRois) + ' ROIs;', logMorphoStatsFilePath
    fileLogger, 'ROI;Area;Perimeter', logMorphoStatsFilePath
    areaVec  = fltArr(self.numRois)
    perimVec = fltArr(self.numRois)

    for i = 0u, self.numRois-1 do begin
      xRoiCoords  = *outCoordsX[i] * morphoStatsSizeFact
      yRoiCoords  = *outCoordsY[i] * morphoStatsSizeFact
      areaVec[i]  = poly_area([xRoiCoords, xRoiCoords[0]], [yRoiCoords, yRoiCoords[0]])
      perimVec[i] = polygonPerimeter([xRoiCoords, xRoiCoords[0]], [yRoiCoords, yRoiCoords[0]])
      fileLogger, num2strTrim(i)+';'+num2strTrim(areaVec[i]) + ';' + num2strTrim(perimVec[i]) + ';', logMorphoStatsFilePath
    endfor
  endif

  ; 2. Check neighboring ROIs

  ; 2.0 Initialize color code vector pointers
  self.pAABvertexType = ptr_new(ptrArr(self.numContours)) ; contains the color code for the ROI vertices
  for i = 0L, self.numContours-1 do begin
    nPts = n_elements(*((*(self.pXcoords)[0])[i]))
    if (self.fVerbose gt 0) then print, 'Initializing pAABvertexType for contour ', i, ' with ', nPts, ' points.'
      ; Make array of aabm vertex type 0 (no adjacent contours at the beginning).
    (*(self.pAABvertexType)[0])[i] = ptr_new(bytArr(nPts))
  endfor

  ; 2.1 
  aabmCandidatesNum = self->checkAABsForAllRois(aabListNames = aabListNames);/fTryFileLoad)
   if (aabmCandidatesNum eq 0) then begin
     print, 'No AAB zones were found for the input contours. Returning...'
     return, 1
  endif
  nCorners = ptr_valid((*self.pAABlist)[3]) ? n_elements(*(*self.pAABlist)[3]) : 0

  ; O1. Plot
  adjustmentAllWinID = 12
  imgSize = size(*self.pImage, /dim)
  roiPlotThick = 3
  fInvertImage = 1
  plotImage = fInvertImage ? 255-*self.pImage : *self.pImage
  fPlotAABs = 1
  fPlotCorners = 0
  fPlotBoundingPolys = 0

  if fPlotAllRois then begin
    makePolygonsPlot, *((*(self.pXcoords)[0])[0]), *((*(self.pYcoords)[0])[0]), *((*(self.pXcoords)[0])[1]), *((*(self.pYcoords)[0])[1]), $
                      backgroundImage = plotImage, winID = adjustmentAllWinID, winTitle = 'AAB Adjustment Plot', xPos = xPos, thick = roiPlotThick, $
                      xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, sizeFactorWin = 2, /fNoAxes, /fUseImageScale, /fNoScreenSizeAutoAdjust
    if (self.numContours gt 2) then for roiCount = 0, self.numContours-1 do $
      oPlot, *((*(self.pXcoords)[0])[roiCount]), *((*(self.pYcoords)[0])[roiCount]), color = 'FF0000'x;, thick = roiPlotThick

    if fPlotAABs and (aabmCandidatesNum ge 1) then begin
      aabList = *(*self.pAABlist)[0]
      ;aabBBoxes = *(*self.pAABlist)[1]
      aabListBoundingPolys = *(*self.pAABlist)[2]
;stop
      if fPlotBoundingPolys then for i = 0u, aabmCandidatesNum-1 do begin
        oPlot, (*aabListBoundingPolys[i])[*,0], (*aabListBoundingPolys[i])[*,1], color = '0000FF'x, lineStyle = 0
;        bbx = [aabzoneBoundingBoxes[4*i], aabzoneBoundingBoxes[4*i+2], aabzoneBoundingBoxes[4*i+2], aabzoneBoundingBoxes[4*i], aabzoneBoundingBoxes[4*i]]
;        bby = [aabzoneBoundingBoxes[4*i+1], aabzoneBoundingBoxes[4*i+1], aabzoneBoundingBoxes[4*i+3], aabzoneBoundingBoxes[4*i+3], aabzoneBoundingBoxes[4*i+1]]
;        ;plot, bbx, bby, color = '0000FF'x, lineStyle = 0, xrange = [0,szImg[0]], yrange = [0,szImg[1]], xmargin = [0,0], ymargin = [0,0], xTickInterval = 128, yTickInterval = 128, /NOERASE
;        ;oPlot, bbx, bby, linestyle = 0, symsize = 1, color='FFFF00'x
;        if ptr_valid(AABListMergeBoundingPolys[i]) then oplot, (*AABListMergeBoundingPolys[i])[*,0], (*AABListMergeBoundingPolys[i])[*,1], color = '00FF00'x, lineStyle = 0
      endfor

      fSaveROIaabIndices = 1b
      if fSaveROIaabIndices then begin
        self->getFolderFileNamePrefix, prefix, baseFileName, folderPath
        imgFolderName = '_img_'
        if file_test(folderPath + imgFolderName, /DIRECTORY) eq 0 then file_mkDir, folderPath + imgFolderName
;stop
        for i = 0u, self.numRois-1 do begin
          logFileNameTxt = folderPath + imgFolderName + path_sep() + baseFileName + 'ROI' + num2strTrim(i) + '_xyCoords' + '.txt'
          px = ((*(self.pXcoords)[0])[i])
          py = ((*(self.pYcoords)[0])[i])
          npts = n_elements(*px)
          fileLogger, 'ROI ' + num2strTrim(i) + ': ' + num2strTrim(npts) + ' vertices', logFileNameTxt
          fileLogger, transpose(num2strTrim(*px)), logFileNameTxt
          fileLogger, transpose(num2strTrim(*py)), logFileNameTxt
        endfor

        for i = 0u, aabmCandidatesNum-1 do begin
          oAAB             = (*((*self.pAABlist)[0]))[i]
          roiIndexVectorAtt = oAAB->getAttributeByName('roi_index_vector')
          roiIndexVector    = *(*roiIndexVectorAtt.pData[0])
          logFileNameTxt    = folderPath + imgFolderName + path_sep() + baseFileName + 'ROIs' + num2strTrim(roiIndexVector[0]) + '-' + num2strTrim(roiIndexVector[1]) + '_aabROIindices' + '.txt'
          roiVertexAtt     = oAAB->getAttributeByName('roi_vertex_indices')
          pairTypeListAtt  = oAAB->getAttributeByName('paired_aab_type_list')
          ;roiVertCoordsAtt = oAAB->getAttributeByName('roi_vertex_list')
          pairTypeList     = **(pairTypeListAtt.pData[0])
          nPairs = n_elements(pairTypeList)
          for ii = 0, nPairs-1 do begin
            ;p1x = (*roiVertCoordsAtt.pData)[0,2*ii]
            ;p1y = (*roiVertCoordsAtt.pData)[1,2*ii]
            ;p2x = (*roiVertCoordsAtt.pData)[0,2*ii+1]
            ;p2y = (*roiVertCoordsAtt.pData)[1,2*ii+1]
            ;npts1 = n_elements(*p1x)
            ;npts2 = n_elements(*p2x)
            p1v = (*roiVertexAtt.pData)[0]
            p2v = (*roiVertexAtt.pData)[1]
            npts1 = n_elements(*p1v)
            npts2 = n_elements(*p2v)
            fileLogger, 'ROI ' + num2strTrim(roiIndexVector[0]) + ': ' + num2strTrim(npts1) + ' vertices', logFileNameTxt 
            fileLogger, transpose(num2strTrim(*p1v)), logFileNameTxt
            ;fileLogger, transpose(num2strTrim(*p1y)), logFileNameTxt
            fileLogger, 'ROI ' + num2strTrim(roiIndexVector[1]) + ': ' + num2strTrim(npts2) + ' vertices', logFileNameTxt 
            fileLogger, transpose(num2strTrim(*p2v)), logFileNameTxt
            ;fileLogger, transpose(num2strTrim(*p2y)), logFileNameTxt
          endfor
        endfor
      endif
stop

      if (fPlotCorners and (nCorners gt 0)) then begin
        aabCornerListBoxes = *(*self.pAABlist)[4]
        for i = 0u, nCorners-1 do begin
          bboxI = aabCornerListBoxes[4*i:4*i+3]
          bbx = [bboxI[0], bboxI[2], bboxI[2], bboxI[0], bboxI[0]]
          bby = [bboxI[1], bboxI[1], bboxI[3], bboxI[3], bboxI[1]]
          oPlot, bbx, bby, linestyle = 0, symsize = 1, color='FFFF00'x
        endfor
        stop
      endif
    endif
  endif
;stop
  ; 3. AAB zone adjustment.
  pROIsharedZoneVertIndices = ptrArr(self.numRois)
  pROIsharedZoneCoords      = ptrArr(self.numRois) ; TODO Maybe redundant, but for now a copy of the separation coords is made for each ROI contour.

  ; 3.1 Check corner AABs
  if (nCorners gt 0) then begin
    cornerPts = fltArr(3, nCorners)
    aabCornerList = *(*self.pAABlist)[3]
    ;aabCornerListBoxes = *(*self.pAABlist)[4]
    aabCornerListAABindices = *(*self.pAABlist)[5]
    aabCornerListROIindices = *(*self.pAABlist)[6]
    pAABCornerIDfromAAB = ptrArr(aabmCandidatesNum)
    pAABCornerIDfromROI = ptrArr(self.numRois)

    ; Map corners with ROIs and AABs
    for c = 0u, nCorners-1 do begin

      aabIndicesCorner = *aabCornerListAABindices[c]
      nAABsCorner = n_elements(aabIndicesCorner)
      for i = 0u, nAABsCorner-1 do begin
        aabID = aabIndicesCorner[i]
        if ptr_valid(pAABCornerIDfromAAB[aabID]) then begin
          tmp = *pAABCornerIDfromAAB[aabID]
          pAABCornerIDfromAAB[aabID] = ptr_new([tmp, c])
        endif else pAABCornerIDfromAAB[aabID] = ptr_new([c])
      endfor

      roiIndicesCorner = *aabCornerListROIindices[c]
      nRoisCorner = n_elements(roiIndicesCorner)
      for i = 0u, nRoisCorner-1 do begin
        roiID = roiIndicesCorner[i]
        if ptr_valid(pAABCornerIDfromROI[roiID]) then begin
          tmp = *pAABCornerIDfromROI[roiID]
          pAABCornerIDfromROI[roiID] = ptr_new([tmp, c])
        endif else pAABCornerIDfromROI[roiID] = ptr_new([c])
      endfor
    endfor

    ; Adjust corners (points)
    cornersXYcoords = fltArr(2, nCorners)
    for c = 0u, nCorners-1 do begin
      oCorner = aabCornerList[c]
      xyCorner = oCorner->adjustCorner_impl2D_new()
      cornersXYcoords[*,c] = xyCorner
    endfor
    ;cp = intersectPairedPolylinesVichoCrap(pRoiXYpoints)
  endif

  ; 3.2 Adjust AAB paired zones
  for aabmCounter = 0L, aabmCandidatesNum-1 do begin
    oAAB = (*((*self.pAABlist)[0]))[aabmCounter]
    print, 'Going for AAB adjustment of zone ', aabmCounter

    if (self.fVerbose gt 0) then begin
      roiIndexVectorStruct = oAAB->getAttributeByName('roi_index_vector')
      roiIndexVector       = *(*(roiIndexVectorStruct.pData[0]))
      nRois = n_elements(roiIndexVector)
      print, 'AAB zone with ', nRois, ' ROIs: ', roiIndexVector
    endif

    if (nCorners gt 0) then begin
      nCornersCurrentAAB = ptr_valid(pAABCornerIDfromAAB[aabmCounter]) ? n_elements(pAABCornerIDfromAAB[aabmCounter]) : 0
      if (nCornersCurrentAAB gt 0) then begin
        cornerIDs = *pAABCornerIDfromAAB[aabmCounter]
        cornerPts = fltArr(2, nCornersCurrentAAB)
        for c = 0u, nCornersCurrentAAB-1 do cornerPts[*,c] = cornersXYcoords[*,cornerIDs[c]]
      endif
    endif else cornerPts = [-1]
fSkipCorners = 1b
    nRoiPairedLines = fSkipCorners ? oAAB->computeAABregion() : oAAB->computeAABregion(cornerPoints = cornerPts)
    if (nRoiPairedLines lt 1) then stop
    nOutputStructs  = oAAB->getOutputsCount()
    if (self.fVerbose gt 0) then print, 'Retrieving ', nRoiPairedLines, ' separation lines - ', nOutputStructs, ' output structs'

    bbAbsStruct = oAAB->getAttributeByName('axis_bounding_box_absolute')
    bbAbs       = **(bbAbsStruct.pData[0])

    ; Pre-condition/Assumption: the output attributes are sorted with respect to each other, so we don't need to look for the matching pairs.
    separationLines            = oAAB->getOutputByName('separation_line', /fGetAll)
    separationRoiVertexIndices = oAAB->getOutputByName('separation_line_roi_vertex_indices', /fGetAll)
    nSeparations = n_elements(separationLines)
    for ns = 0, nSeparations-1 do begin

      ; Extract the adjusted x,y separation coordinates
      pCurrentSepLineCoords  = *(separationLines[ns].pData[0])

      for ri = 0, n_elements(separationRoiVertexIndices[ns].roiIndices)-1 do begin
        roiPos         = (separationRoiVertexIndices[ns].roiIndices)[ri]
        roiVertIdxSep  = *(*separationRoiVertexIndices[ns].pData)[ri]
        pRoiVertIdxSep = ptr_new(roiVertIdxSep);, /no_copy)

        ; Get the ROI indices to map the adjusted separation to each ROI-contour vertex list.
        if ptr_valid(pROIsharedZoneVertIndices[roiPos]) then begin
          pCurrentZones = *pROIsharedZoneVertIndices[roiPos]
          pROIsharedZoneVertIndices[roiPos] = ptr_new([pCurrentZones, pRoiVertIdxSep])
        endif else begin
          pROIsharedZoneVertIndices[roiPos] = ptr_new([pRoiVertIdxSep])
        endelse

        ; Now get the x,y coords and store them while matching the order from the pROIsharedZoneVertIndices.
        if ptr_valid(pROIsharedZoneCoords[roiPos]) then begin
          pCurrentCoords = *pROIsharedZoneCoords[roiPos]
          pROIsharedZoneCoords[roiPos] = ptr_new([pCurrentCoords, pCurrentSepLineCoords])
        endif else begin
          pROIsharedZoneCoords[roiPos] = ptr_new([pCurrentSepLineCoords])
        endelse

      endfor
    endfor

    if fPlotAllRois then begin
      wSet, adjustmentAllWinID
      for pl = 0, nSeparations-1 do plot, (**(separationLines[pl].pData)[0])[0,*], (**(separationLines[pl].pData)[0])[1,*], color='00FFFF'x, thick=2.5, xRange=[0,imgSize[0]-1], yRange=[0,imgSize[1]-1], xMargin=[0,0], yMargin=[0,0], xStyle=1, yStyle=1, xticks=1, yticks=1, /noErase
      ;for sl = 0, nSeparations-1 do oPlot, (**(separationLines[sl].pData)[0])[0,*], (**(separationLines[sl].pData)[0])[0,*], color = '00FFFF'x, thick = 2.5
    endif
  endfor ; End 3.

  pROIfreeZoneVertIndices = ptrArr(self.numRois)
  for i = 0u, self.numRois-1 do begin
    nVerticesRoiI = n_elements(*((*(self.pXcoords)[0])[i]))
    nSharedZonesI = ptr_valid(pROIsharedZoneVertIndices[i]) ? n_elements(*pROIsharedZoneVertIndices[i]) : 0
    if (self.fVerbose gt 0) then print, 'ROI ', i, ': ', nSharedZonesI, ' shared contour zones in ', nVerticesRoiI, ' vertices'
    if (nSharedZonesI eq 0) then begin
      pROIfreeZoneVertIndices[i] = ptr_new([ptr_new(uIndGen(nVerticesRoiI))])
      continue
    endif

    pSharedZonesRoiI          = *pROIsharedZoneVertIndices[i]
    ;sharedZoneVertexIndexRuns = sortForRoi(pSharedZonesRoiI, n_elements(*(*(self.pAABvertexType))[i]) - 1, complementRuns = complementRuns)
    nMax = n_elements(*(*(self.pAABvertexType))[i]) - 1
    nSharedZones = n_elements(pSharedZonesRoiI)
    if (nSharedZones gt 1) then begin
      sharedZoneVertexIndexRuns = uIntArr(nSharedZones * 2)
      if (nSharedZones eq 1) then begin
        sharedZoneVertexIndexRuns = [(*pSharedZonesRoiI[0])[0], (*pSharedZonesRoiI[0])[n_elements(*pSharedZonesRoiI[0])-1]]
      endif else $
        for j = 0u, nSharedZones-1 do begin
          posRun = 2*j
          nElem  = n_elements(*pSharedZonesRoiI[j])
          sharedZoneVertexIndexRuns[posRun:posRun+1] = [(*pSharedZonesRoiI[j])[0], (*pSharedZonesRoiI[j])[nElem-1]]
        endfor

      complementRuns   = getComplementRuns(uIndGen(nMax+1), sharedZoneVertexIndexRuns)
      nSharedZoneRunsI = n_elements(sharedZoneVertexIndexRuns) / 2
      nElemRunsI       = n_elements(complementRuns)
      nFreeZoneRunsI   = nElemRunsI / 2
      pSharedZonesRoiExtended = ptrArr(nSharedZoneRunsI)
      for j = 0u, nSharedZoneRunsI-1 do begin
        runPos = 2*j
        vertIndicesRun = runsToIndices(sharedZoneVertexIndexRuns[runPos:runPos+1], nVerticesRoiI)
        pSharedZonesRoiExtended[j] = ptr_new(vertIndicesRun, /no_copy)
      endfor
      pROIsharedZoneVertIndices[i] = ptr_new(pSharedZonesRoiExtended)

    endif else begin
      nFreeZoneRunsI = 1
      complementRuns = [0, nMax]
    endelse

    ; Check if there is a circular run (which goes through index 0) and "absorb" it ;)
    if (nFreeZoneRunsI gt 1) then $
    if (complementRuns[0] eq 0) and (complementRuns[nElemRunsI-1] eq (nVerticesRoiI-1)) then begin
      complementRuns  = [complementRuns[2:nElemRunsI-2], complementRuns[1]]
      nFreeZoneRunsI -= 1
    endif
    if (self.fVerbose gt 0) then print, 'ROI ', i, ': ', nFreeZoneRunsI, ' free contour zones'


    pFreeZonesRoiIsorted = ptrArr(nFreeZoneRunsI)
    for j = 0u, nFreeZoneRunsI-1 do begin
      runPos = 2*j
      vertIndicesRun = runsToIndices(complementRuns[runPos:runPos+1], nVerticesRoiI)
      pFreeZonesRoiIsorted[j] = ptr_new(vertIndicesRun, /no_copy) ; For now this run is converted to list here
    endfor

    pROIfreeZoneVertIndices[i] = ptr_new(pFreeZonesRoiIsorted)
  endfor
;stop
  self.pAdjacentROIindexVector = ptr_new(pROIsharedZoneVertIndices)
  self.pSharedContourZones     = ptr_new(pROIsharedZoneCoords)
  self.pFreeContourZones       = ptr_new(pROIfreeZoneVertIndices)

  ; 4. Recover/separate free and shared boundary zones for each ROI.

  ;freeVertexCoordsPreAdjust = ptrArr(self.numRois)

  ; Differentiate between free&shared zones -> recover shared zones and intercalate them with the non-free
  ; zones in order to get the full ordered sequence for each roi
  pVertCodes = ptrArr(self.numRois)
  for i = 0u, self.numRois-1 do begin
;if (i eq 6) then stop
    self->assembleRoiContourForRoi, i, xCoordsOut = xCo, yCoordsOut = yCo, outCodes = outCodes
    checkAndFixConsistencyForRoiContour, xCo, yCo, outCodes, xCoordsOut = xCoFix, yCoordsOut = yCoFix, outCodes = outCodesFix, fFixOut = fFixOut
    if ~fFixOut $
    then self->relaxFreeContourZones, i, xCoFix, yCoFix, outCodesFix, xCoordsOut = xCoordsR, yCoordsOut = yCoordsR, contourCodesOut = outCodesR $
    else begin
      print, 'Contour polygon for ROI ', i, ' is erronoeus/inconsistent. No further adjustments will be made to it'
      xCoordsR = xCo
      yCoordsR = yCo
      outCodesR = outCodes
    endelse
    nPtsClosed = n_elements(xCoordsR)
    outCoordsX[i] = ptr_new([xCoordsR[0:nPtsClosed-1]])
    outCoordsY[i] = ptr_new([yCoordsR[0:nPtsClosed-1]])
    pVertCodes[i] = ptr_new(outCodesR, /no_copy)
;LINESAMPLE
    ;polygonLineSample, xco, yco, xCoordsInt, yCoordsInt, nPointsPerPix = aabmPointSamplingDistance, /fCloseOutput, flagVector = outCodes, interpFlagVector = outCodesInt, /FFORCEZEROFLAGENDS
; ARCSAMPLE
    ;polyLen = 0.0
    ;for s = 0u, n_elements(xco)-2 do polyLen += sqrt((xco[s+1] - xco[s])^2 + (yco[s+1] - yco[s])^2)
    ;polyLen += sqrt((xco[s] - xco[0])^2 + (yco[s] - yco[0])^2)
    ;nPtsArcSample = round(polyLen / aabmPointSamplingDistance)
    ;polygonArcSample, xco, yco, xCoordsInt, yCoordsInt, nPoints = nPtsArcSample, /fClose
    ;nPtsClosed = n_elements(xCoordsInt)
    ;outCoordsX[i] = ptr_new(xCoordsInt[0:nPtsClosed-1])
    ;outCoordsY[i] = ptr_new(yCoordsInt[0:nPtsClosed-1])
    ;pVertCodes[i] = ptr_new(outCodesInt, /no_copy)
  endfor
  fPlotFinals = 0b
  if fPlotFinals then begin
    makePolygonsPlot, *outCoordsX[0], *outCoordsY[0], *outCoordsX[1], *outCoordsY[1], $
                      backgroundImage = plotImage, winID = 23, winTitle = 'AAB Adjustment Plot', xPos = xPos, thick = roiPlotThick, $
                      xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, sizeFactorWin = 8, /fNoAxes, /fUseImageScale, /fNoScreenSizeAutoAdjust
    for i = 2u, self.numRois-1 do oPlot, *outCoordsX[i], *outCoordsY[i], pSym = 3, color = 0
    fOverPlotCornrers = 1b
    if fOverPlotCornrers then begin
      aabCornerListBoxes = *(*self.pAABlist)[4]
      for i = 0u, nCorners-1 do begin
        bboxI = aabCornerListBoxes[4*i:4*i+3]
        bbx = [bboxI[0], bboxI[2], bboxI[2], bboxI[0], bboxI[0]]
        bby = [bboxI[1], bboxI[1], bboxI[3], bboxI[3], bboxI[1]]
        oPlot, bbx, bby, linestyle = 0, symsize = 1, color='FFFF00'x
      endfor
    endif
    i1 = tvrd() & write_png, 'D:\tmp\z032_adjustesLines-withCornerBBoxes.png', i1 & stop
    makePolygonsPlot, *outCoordsX[0], *outCoordsY[0], *outCoordsX[1], *outCoordsY[1], $
                      backgroundImage = 255-(plotImage)*0, winID = 24, winTitle = 'AAB Adjustment Plot (no image)', xPos = xPos, thick = roiPlotThick, $
                      xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, sizeFactorWin = 8, /fNoAxes, /fUseImageScale, /fNoScreenSizeAutoAdjust
    for i = 2u, self.numRois-1 do oPlot, *outCoordsX[i], *outCoordsY[i], pSym = 3, color = 255
    i2 = tvrd () & stop
  endif
  ; Set outCoordsX/Y as the new coordinates
  for i = 0u, self.numRois-1 do begin
    self.pXcoords = ptr_new(outCoordsX)
    self.pXcoords = ptr_new(outCoordsY)
  endfor
  self.pAABvertexType = ptr_new(pVertCodes)
  print, 'AABM adjustment finished!'

  if fLogStats then begin
    self->getFolderFileNamePrefix, prefix, baseFileName, folderPath
    logMorphoStatsFilePath = folderPath + prefix + baseFileName + '_morphoStats_finalContours.csv'
    fileLogger, 'Adjusted ROI Stats;' + num2strTrim(self.numRois) + ' ROIs;', logMorphoStatsFilePath
    fileLogger, 'ROI;Area;Perimeter', logMorphoStatsFilePath
    areaVec  = fltArr(self.numRois)
    perimVec = fltArr(self.numRois)

    for i = 0u, self.numRois-1 do begin
      xRoiCoords  = *outCoordsX[i] * MORPHOSTATSSIZEFACT
      yRoiCoords  = *outCoordsY[i] * MORPHOSTATSSIZEFACT
      areaVec[i]  = poly_area([xRoiCoords, xRoiCoords[0]], [yRoiCoords, yRoiCoords[0]])
      perimVec[i] = polygonPerimeter([xRoiCoords, xRoiCoords[0]], [yRoiCoords, yRoiCoords[0]])
      fileLogger, num2strTrim(i)+';'+num2strTrim(areaVec[i]) + ';' + num2strTrim(perimVec[i]) + ';', logMorphoStatsFilePath
    endfor
;    stop
  endif
end


; assembleRoiContourForRoi
;
; Returns the continous contour xy-point list for a given ROI, given that its AAB zones have been computed.
pro C_sAABContainer::assembleRoiContourForRoi, roiIndex, xCoordsOut = xCoordsOut, yCoordsOut = yCoordsOut, outCodes = outCodes

  if (roiIndex ge self.numRois) or (roiIndex lt 0) then return

  xCoordsOut  = *((*(self.pXcoords)[0])[roiIndex])
  yCoordsOut  = *((*(self.pYcoords)[0])[roiIndex])
  nptsROI     = n_elements(xCoordsOut)
  outCodes    = bytArr(nptsROI)
  aabVertCode = 3 ; code for shared contour sections.

  adjustedROIvertexIndices = lIndGen(nptsROI)
  pROIsharedZonesRoiIdxVec = (*self.pAdjacentROIindexVector)[roiIndex]
  pROIsharedZonesCoords    = (*self.pSharedContourZones)[roiIndex]
  pFreeContourZonesRoi     = (*self.pFreeContourZones)[roiIndex]
  nRoiFreeBoundZones       = ptr_valid(pFreeContourZonesRoi) ? n_elements(*pFreeContourZonesRoi) : 0
  nRoiSharedBoundZones     = ptr_valid(pROIsharedZonesRoiIdxVec) ? n_elements(*pROIsharedZonesRoiIdxVec) : 0
  if self.fVerbose then $
    print, 'ROI ', roiIndex, ' contour re-assembly: ', nRoiSharedBoundZones, ' shared boundary zones'
  if (nRoiSharedBoundZones eq 0) then return

  deltaOutCodesSize  = 0L
  if self.fVerbose then begin
    print, 'Number of vertices for adjacent / shared contour zones:'
    for u = 0,nRoiSharedBoundZones-1 do print, n_elements(*(*pROIsharedZonesRoiIdxVec)[u]), n_elements(*(*pROIsharedZonesCoords)[u])/2
  endif

  for i = 0u, nRoiSharedBoundZones-1 do begin
    vertexIndices = *(*pROIsharedZonesRoiIdxVec)[i]
    vertexCoords  = *(*pROIsharedZonesCoords)[i]
    nptsSh    = n_elements(vertexIndices)
    shIni     = adjustedROIvertexIndices[vertexIndices[0]]; lookup for possible displacements from previous iterations
    nvertSh   = n_elements(vertexCoords) / 2
    deltaNPts = nptsSh - nVertSh   ; how many points in this vertex/coords array
    deltaOutCodesSize -= deltaNPts ; accumulated difference w.r.t the original ROI

    if (nptsSh eq nptsROI) then begin
      if self.fVerbose then print, 'ROI index ', roiIndex, ' has an entirely shared boundary'
      outCodes = replicate(aabVertCode, nvertSh)
      xCoordsOut = vertexCoords[0,*]
      yCoordsOut = vertexCoords[1,*]
      return
    endif

    case 1 of

      deltaNPts lt 0: begin ; insert points

        if ((shIni+nptsSh) lt nptsROI) then adjustedROIvertexIndices[shIni+nptsSh:*] -= deltaNPts
        carEnd = shIni - 1
        cdrIni = shIni + nptsSh
        case 1 of
        (carEnd lt 0) and (cdrIni le (nptsRoi-1)): begin ; only tail
          carEnd += nptsROI
          cdrC = [outCodes[cdrIni:*]]
          outCodes = [replicate(aabVertCode, nvertSh), cdrC]
          cdrX = [xCoordsOut[cdrIni:*]]
          cdrY = [yCoordsOut[cdrIni:*]]
          x0 = xCoordsOut[carEnd] & xN1 = xCoordsOut[cdrIni]
          y0 = yCoordsOut[carEnd] & yN1 = yCoordsOut[cdrIni]
          xc0 = (vertexCoords[0,*])[0] & xc1 = (vertexCoords[0,*])[nvertSh-1]
          yc0 = (vertexCoords[1,*])[0] & yc1 = (vertexCoords[1,*])[nvertSh-1]
          matchPair = getClosestMatch(xc0, yc0, xc1, yc1, x0, y0, xn1, yn1, fEquidistant = fEquidistant)
          if matchPair[0] eq matchPair[1] then stop ; Should not happen. Check fEquidistant...
          fReverseCoords = matchPair[0] ne 0
          xCoordsOut = [fReverseCoords ? reverse(transpose(vertexCoords[0,*])) : transpose(vertexCoords[0,*]), cdrX]
          yCoordsOut = [fReverseCoords ? reverse(transpose(vertexCoords[1,*])) : transpose(vertexCoords[1,*]), cdrY]
        endcase

        (carEnd ge 0) and (cdrIni gt (nptsRoi-1)): begin ; head only
          midIni = (cdrIni mod nptsRoi) ; TODO fix adjustedRoiVertexIndices[] += midIni... ?
          midEnd = carEnd
        if (midEnd-midIni+1+nVertSh) eq nptsRoi then midIni-=1 ; TODO JJ boundary condition
          adjustedRoiVertexIndices[midIni:*] -= midIni
          outCodes = [outCodes[midIni:midEnd], replicate(aabVertCode, nvertSh)]
          midX = xCoordsOut[midIni:midEnd]
          midY = yCoordsOut[midIni:midEnd]
          x0  = xCoordsOut[midIni] & xN1 = xCoordsOut[midEnd]
          y0  = yCoordsOut[midIni] & yN1 = yCoordsOut[midEnd]
          xc0 = (vertexCoords[0,*])[0] & xc1 = (vertexCoords[0,*])[nvertSh-1]
          yc0 = (vertexCoords[1,*])[0] & yc1 = (vertexCoords[1,*])[nvertSh-1]
          matchPair = getClosestMatch(xc0, yc0, xc1, yc1, x0, y0, xn1, yn1, fEquidistant = fEquidistant)
          if matchPair[0] eq matchPair[1] then stop ; Should not happen. Check fEquidistant...
          fReverseCoords = matchPair[0] ne 0
          xCoordsOut = [midX, fReverseCoords ? transpose(vertexCoords[0,*]) : reverse(transpose(vertexCoords[0,*]))]
          yCoordsOut = [midY, fReverseCoords ? transpose(vertexCoords[1,*]) : reverse(transpose(vertexCoords[1,*]))]
        endcase

        else: begin
          adjustedRoiVertexIndices[cdrIni:*] -= deltaNpts
          carC = [outCodes[0:carEnd]]
          cdrC = [outCodes[cdrIni:*]]
          outCodes = [carC, replicate(aabVertCode, nvertSh), cdrC]
          carX = [xCoordsOut[0:carEnd]]
          cdrX = [xCoordsOut[cdrIni:*]]
          carY = [yCoordsOut[0:carEnd]]
          cdrY = [yCoordsOut[cdrIni:*]]
          x0 = xCoordsOut[carEnd] & xN1 = xCoordsOut[cdrIni]
          y0 = yCoordsOut[carEnd] & yN1 = yCoordsOut[cdrIni]
          xc0 = (vertexCoords[0,*])[0] & xc1 = (vertexCoords[0,*])[nvertSh-1]
          yc0 = (vertexCoords[1,*])[0] & yc1 = (vertexCoords[1,*])[nvertSh-1]
          matchPair = getClosestMatch(xc0, yc0, xc1, yc1, x0, y0, xn1, yn1, fEquidistant = fEquidistant)
          if matchPair[0] eq matchPair[1] then stop ; Should not happen. Check fEquidistant...
          fReverseCoords = matchPair[0] ne 0
          xCoordsOut = [carX, fReverseCoords ? reverse(transpose(vertexCoords[0,*])) : transpose(vertexCoords[0,*]), cdrX]
          yCoordsOut = [carY, fReverseCoords ? reverse(transpose(vertexCoords[1,*])) : transpose(vertexCoords[1,*]), cdrY]
        endcase
        endcase
      endcase

      deltaNPts gt 0: begin ; remove points
        if ((shIni+nptsSh) lt nptsROI) then adjustedROIvertexIndices[shIni+nptsSh:*] -= deltaNPts
        carEnd = shIni - 1
        cdrIni = shIni + nptsSh;nVertSh
        case 1 of
        (carEnd ge 0) and (cdrIni gt (nptsRoi-1)): begin ; head only
          outCodes   = [outCodes[0:carEnd]]
          xCoordsOut = [xCoordsOut[0:carEnd]]
          yCoordsOut = [yCoordsOut[0:carEnd]]
        endcase
        (carEnd lt 0) and (cdrIni le (nptsRoi-1)): begin ; tail only
          outCodes   = [outCodes[cdrIni:*]]
          xCoordsOut = [xCoordsOut[cdrIni:*]]
          yCoordsOut = [yCoordsOut[cdrIni:*]]
        endcase
        else: begin
          carC = [outCodes[0:carEnd]]
          cdrC = [outCodes[cdrIni:*]]
          outCodes = [carC, replicate(aabVertCode,nVertSh), cdrC]
          carX = [xCoordsOut[0:carEnd]]
          cdrX = [xCoordsOut[cdrIni:*]]
          carY = [yCoordsOut[0:carEnd]]
          cdrY = [yCoordsOut[cdrIni:*]]
          x0 = xCoordsOut[carEnd] & xN1 = xCoordsOut[cdrIni]
          y0 = yCoordsOut[carEnd] & yN1 = yCoordsOut[cdrIni]
          xc0 = (vertexCoords[0,*])[0] & xc1 = (vertexCoords[0,*])[nvertSh-1]
          yc0 = (vertexCoords[1,*])[0] & yc1 = (vertexCoords[1,*])[nvertSh-1]
          matchPair = getClosestMatch(xc0, yc0, xc1, yc1, x0, y0, xn1, yn1, fEquidistant = fEquidistant)
          if matchPair[0] eq matchPair[1] then stop ; Should not happen. Check fEquidistant...
          fReverseCoords = matchPair[0] ne 0
          xCoordsOut = [carX, fReverseCoords ? reverse(transpose(vertexCoords[0,*])) : transpose(vertexCoords[0,*]), cdrX]
          yCoordsOut = [carY, fReverseCoords ? reverse(transpose(vertexCoords[1,*])) : transpose(vertexCoords[1,*]), cdrY]
        endcase
        endcase
      endcase

      else: begin ; Don't modify point count, but check before replacing.
        shEnd = shIni + nVertSh - 1
        if (shEnd ge nptsROI) then begin
          shEnd2 = shEnd mod nptsROI
          x0 = xCoordsOut[shIni] & xN1 = xCoordsOut[shEnd2]
          y0 = yCoordsOut[shIni] & yN1 = yCoordsOut[shEnd2]
          xc0 = (vertexCoords[0,*])[0] & xc1 = (vertexCoords[0,*])[nvertSh-1]
          yc0 = (vertexCoords[1,*])[0] & yc1 = (vertexCoords[1,*])[nvertSh-1]
          matchPair = getClosestMatch(xc0, yc0, xc1, yc1, x0, y0, xn1, yn1, fEquidistant = fEquidistant)
          if matchPair[0] eq matchPair[1] then stop ; Should not happen. Check fEquidistant...
          fReverseCoords = matchPair[0] ne 0
          xCoordsPatch = fReverseCoords ? reverse(reform(vertexCoords[0,*])) : reform(vertexCoords[0,*])
          yCoordsPatch = fReverseCoords ? reverse(reform(vertexCoords[1,*])) : reform(vertexCoords[1,*])
          lenTail = nptsRoi - shIni ;+ 1 ; in + 1
          lenHead = shEnd - nptsRoi  ; out +1
          outCodes[shIni : *]   = replicate(aabVertCode, lenTail)
          xCoordsOut[shIni : *] = xCoordsPatch[0:lenTail-1]
          yCoordsOut[shIni : *] = yCoordsPatch[0:lenTail-1]
          outCodes[0 : lenHead]   = replicate(aabVertCode, lenHead+1)
          xCoordsOut[0 : lenHead] = xCoordsPatch[lenTail:*]
          yCoordsOut[0 : lenHead] = yCoordsPatch[lenTail:*]
        endif else begin
          outCodes[shIni : shEnd] = replicate(aabVertCode, nVertSh)
          x0 = xCoordsOut[shIni] & xN1 = xCoordsOut[shEnd]
          y0 = yCoordsOut[shIni] & yN1 = yCoordsOut[shEnd]
          xc0 = (vertexCoords[0,*])[0] & xc1 = (vertexCoords[0,*])[nvertSh-1]
          yc0 = (vertexCoords[1,*])[0] & yc1 = (vertexCoords[1,*])[nvertSh-1]
          matchPair = getClosestMatch(xc0, yc0, xc1, yc1, x0, y0, xn1, yn1, fEquidistant = fEquidistant)
          if matchPair[0] eq matchPair[1] then stop ; Should not happen. Check fEquidistant...
          fReverseCoords = matchPair[0] ne 0
          xCoordsOut[shIni : shEnd] = fReverseCoords ? reverse(transpose(vertexCoords[0,*])) : transpose(vertexCoords[0,*])
          yCoordsOut[shIni : shEnd] = fReverseCoords ? reverse(transpose(vertexCoords[1,*])) : transpose(vertexCoords[1,*]); TODO watch out...
        endelse
      endcase
    endcase
    nptsROI -= deltaNpts
  endfor
  fDebugPlot = 1
  if fDebugPlot then begin
    makePolygonsPlot, xCoordsOut, yCoordsOut, pSym = 3, winId = 21, winTitle = 'ROI' + string(roiIndex), sizeFactorWin = 9
    wh0 = where(outCodes eq 0, count0)
    if (count0 gt 0) then oplot, xCoordsOut[wh0], yCoordsOut[wh0], pSym = 4
    ;stop
  endif
end


function C_sAABContainer::getAABlistForRoiIndex, roiIndexVector = roiIndexVector

  roiAABlist = objArr(1)
  nAABs      = n_elements(self.pAABlist[0])
  if (nAABs lt 1) then return, roiAABlist

  for i = 0u, nAABs-1 do begin
    if (whRoi ge 0) then begin
      oAABs = (*(self.pAABList[0]))[i]
      stop
      roiAABlist = [roiAABlist, curAAB]
    endif
  endfor

  return, roiAABlist
end


; WARNING: Calling this method with any order of free zones produce requires that the shared zones have at least 2 points (wich is the case).
pro C_sAABContainer::relaxFreeContourZones, roiIndex, xCoords, yCoords, contourCodes, xCoordsOut = xCoordsOut, yCoordsOut = yCoordsOut, contourCodesOut = contourCodesOut

  npts   = n_elements(xCoords)
  whFree = where(contourCodes eq 0, countFree, complement = whShared, nComplement = countShared)

  if (countShared eq 0) or (countFree eq 0) then begin
    if (self.fVerbose gt 0) then print, 'No free contour zones to relax for ROI index ', roiIndex
    xCoordsOut = xCoords
    yCoordsOut = yCoords
    contourCodesOut = contourCodes
    return
  endif

  codeRuns   = makeRunList(contourCodes, /fCircularList)
  nRuns      = n_elements(codeRuns.valueList)
  whFreeRuns = where(codeRuns.valueList eq 0, countFreeRuns)
  fLastRunIsFree = contourCodes[npts-1] eq 0

  for c = 0, countFreeRuns-1 do begin
    whFreeC       = whFreeRuns[c]
    runFirstIndex = codeRuns.indexList[whFreeC]
    runLastIndex  = (whFreeC eq (nRuns-1)) ? (codeRuns.indexList[0] eq 0 ? npts-1: codeRuns.indexList[0]-1) : (codeRuns.indexList)[whFreeC+1]-1

    case 1 of
      (c eq 0) and fLastRunIsFree: begin
        ;vertexIndices1a = uIndGen(runLastIndex+1)
        delta1         = npts - runFirstIndex
        vertexIndices1 = runFirstIndex + uIndGen(delta1)
        vertexIndices2 = setDifference(uIndGen(npts), [vertexIndices1])
      endcase
      else: begin
        delta1         = runLastIndex - runFirstIndex + 1
        vertexIndices1 = runFirstIndex + uIndGen(delta1)
        vertexIndices2 = setDifference(uIndGen(npts), vertexIndices1)
      endcase
    endcase
;stop
    ; Make snake with a blank image and relax
    blankImage = bytArr(size(*self.pImage, /dim))
    snakeA  = 0.03 ; 0.001 ; self.alphaVal
    snakeB  = 0.1; self.betaVal
    snakeG  = 1.2 ; self.gammaVal
    snakeK  = 0.0
    snakeIt = 100;000;self.contourIterationsMax
    oSnake  = obj_new('C_sActiveContour', blankImage, xCoords, yCoords, alpha = snakeA, beta = snakeB, gamma = snakeG, kappa = snakeK, iterations = snakeIt)
    ;oSnake->setContour, xCoords, yCoords
    oSnake->setParams, iterations = 100
    outSnake = oSnake->adjustContour(perimeterFactor = 1.0 / self.samplingDist, fixPointIndices = vertexIndices2); fixPointCount = 2)
    xCoordsOut = outSnake.x
    yCoordsOut = outSnake.y
    contourCodesOut = contourCodes
    obj_destroy, oSnake
  endfor
  ;wset, 21 & oplot, xCoordsOut, yCoordsOut & stop
end


;+
; PARAMETERS
;   type  Specifies the type of the vector field to compute.
;         Available options:
;         'GVF' Xu & Prince 1998
;         'GGVF' Xu & Prince 1999
;         'EP-GVF' Li et al. 2005. Felipe Olmos 2009.
;-
function C_sAABContainer::calcVectorField, type = type, iterations = iterations, convergenceLimit = convergenceLimit, step = step,$
                                           mu = mu, sigmaSq = sigmaSq, epgvfNoiseCut = epgvfNoiseCut, epgvfCutRange = epgvfCutRange

  type             = keyword_set(type)                 ? type       : 'GVF'
  iterations       = keyword_set(iterations)           ? iterations : 200
  convergenceLimit = n_elements(convergenceLimit) eq 0 ? 0.01d : convergenceLimit
  step             = n_elements(step)             eq 0 ? 0.01d : step
  epgvfNoiseCut    = n_elements(epgvfNoiseCut)    eq 0 ? 0.10d : epgvfNoiseCut
  epgvfCutRange    = n_elements(epgvfCutRange)    eq 0 ? 0.05d : epgvfCutRange
  sigmaSq          = n_elements(sigmaSq)          eq 0 ? 0.0   : sigmaSq
  mu               = keyword_set(mu)                   ? mu    : 0.05

  nDim = size(*self.pImage, /N_DIMENSIONS)
  if (self.fVerbose gt 0) then print, type, ' vector field computation for image of dimension ', nDim

  if (nDim eq 2) then $
  case type of
  'GVF'   : calcGVF, *self.pImage, pU, pV, iterations = iterations, mu = mu;, edgemapSigmaSq = sigmaSq
  'GGVF'  : calcGGVF, *self.pImage, pU, pV, iterations = iterations, mu = mu;, edgemapSigmaSq = sigmaSq
  'EP-GVF': calcEPGVF, *self.pImage, pU, pV, iterations = iterations, convergenceLimit = convergenceLimit, step = step,$
                                             noiseCut   = epgvfNoiseCut,$
                                             noiseRange = epgvfNoiseRange,$
                                             edgemapSigmaSq = sigmaSq
  else: begin
        print, 'Unrecognized option for vector field computation ("', type, '")'
        return, -1
        endcase
  endcase else print, 'Nothing done for image of ', nDim, ' dimensions'
  ; Currently, IDL doesn't handle direct passing of a "self.pointer" variable to the VF calc. function...
  self.pU = pU
  self.pV = pV
  return, 1
end


pro C_sAABContainer::consistencyCheck
  print, 'Consistency checking will be here...'
end


;+
; init
;
; Class constructor.
;-
function C_sAABContainer::init, image,$
                                alphaVal = alphaVal,$
                                betaVal  = betaVal,$
                                gammaVal = gammaVal,$
                                kappaVal = kappaVal,$
                                proximityDist           = proximityDist,$
                                separationDist          = separationDist,$
                                contourMaxIterations    = contourMaxIterations,$
                                vfMaxIterations         = vfMaxIterations,$
                                contourSamplingDistance = contourSamplingDistance,$
                                minContourPointCount    = minContourPointCount,$
                                fVerbose                = fVerbose,$
                                roiGroupFilePath        = roiGroupFilePath
  self.pImage   = ptr_new(image)
  self.alphaVal = keyword_set(alphaVal) ? abs(alphaVal) : 1.0
  self.betaVal  = keyword_set(betaVal)  ? abs(betaVal)  : 1.0
  self.gammaVal = keyword_set(gammaVal) ? abs(gammaVal) : 1.0
  self.kappaVal = n_elements(kappaVal) gt 0 ? abs(kappaVal) : 1.0
  self.contourIterationsMax = keyword_set(contourMaxIterations)    ? contourMaxIterations : 50
  self.vfIterationsMax      = keyword_set(vfIterationsMax)         ? vfIterationsMax      : 200
  self.samplingDist         = keyword_set(contourSamplingDistance) ? contourSamplingDistance > 0.01 : 1
  minContourPointCountDefault = 16u
  self.minContourPointCount = keyword_set(minContourPointCount)    ? minContourPointCount > minContourPointCountDefault : minContourPointCountDefault
  self.proximityDist        = keyword_set(proximityDist)           ? proximityDist  > 0.0 : 0.0
  self.separationDist       = keyword_set(separationDist)          ? separationDist > 0.0 : 0.0
  self.fVerbose             = keyword_set(fVerbose)
  if (n_elements(roiGroupFilePath) gt 0) $
  then self.roiGroupFilePath = roiGroupFilePath $
  else begin
    self.roiGroupFilePath = ProgramRootDir(/TWOUP)
    folderPath = self.roiGroupFilePath + '\_AAB_'
    fDir = file_test(folderPath, /DIRECTORY)
    if ~fDir then file_mkDir, folderPath
  endelse
  return, 1
end


pro C_sAABContainer::cleanupAABList

  nAAB = ptr_valid(self.pAABlist) ? n_elements(*((self.pAABlist))) : 0
  if (nAAB eq 0) then return

  for i = nAAB-1, 0, -1 do begin
    if ~ptr_valid((*(self.pAABlist))[i]) and ~obj_valid((*(self.pAABlist))[i]) then begin
      print, 'Warning: attempt to delete invalid pointer/object (not assigned?)'
      stop
      continue
    endif
    typeElem = size(((*(self.pAABlist))[i]), /TYPE)
    case typeElem of
      10  : ptr_free, ((*(self.pAABlist))[i])
      11  : obj_destroy, ((*(self.pAABlist))[i])
      else:
    endcase
  endfor
  ptr_free, self.pAABlist

end


;+
; cleanup
;
; Class destructor.
;-
pro C_sAABContainer::cleanup

  ptr_free, self.pImage, self.pU, self.pV, self.pW

  nPtr = ptr_valid(self.pXcoords) ? n_elements(*(self.pXcoords[0])) : 0
  for i = 0L, nPtr-1 do $
    ptr_free, (*(self.pXcoords[0]))[i], (*(self.pYcoords[0]))[i],$
              (*(self.pXcoordsBox[0]))[i], (*(self.pYcoordsBox[0]))[i]

  nPtr = ptr_valid(self.pAABvertexType) ? n_elements(*(self.pAABvertexType[0])) : 0
  for i = 0L, nPtr-1 do ptr_free, (*(self.pAABvertexType[0]))[i]

  nPtr = ptr_valid(self.pZcoords) ? n_elements(*(self.pZcoords[0])) : 0
  for i = 0L, nPtr-1 do ptr_free, (*(self.pZcoords[0]))[i], (*(self.pZcoordsbox[0]))[i]

  ptr_free, self.pXcoords, self.pYcoords, self.pZcoords, self.pXcoordsBox, self.pYcoordsBox, self.pZcoordsBox

  nPtr = ptr_valid(self.pBoundingShapeSub) ? n_elements(*(self.pBoundingShapeSub)[0]) : 0
  for i = 0L, nPtr-1 do ptr_free, (*(self.pBoundingShapeSub[0]))[i]

  nPtr = ptr_valid(self.pSpaceIndices) ? n_elements(*(self.pSpaceIndices)[0]) : 0
  for i = 0L, nPtr-1 do ptr_free, (*(self.pSpaceIndices[0]))[i]
  ptr_free, self.pSpaceIndices
  ptr_free, self.pSpaceIndexNames

  nPtr = ptr_valid(self.pFreeContourZones) ? n_elements(*self.pFreeContourZones) : 0
  for i = 0L, nPtr-1 do ptr_free, (*self.pFreeContourZones)[i]
  ptr_free, self.pFreeContourZones

  nPtr = ptr_valid(self.pSharedContourZones) ? n_elements(*self.pSharedContourZones) : 0
  for i = 0L, nPtr-1 do ptr_free, (*self.pSharedContourZones)[i]
  ptr_free, self.pSharedContourZones

  nPtr = ptr_valid(self.pAdjacentROIindexVector) ? n_elements(*self.pAdjacentROIindexVector) : 0
  for i = 0L, nPtr-1 do ptr_free, (*self.pAdjacentROIindexVector)[i]
  ptr_free, self.pAdjacentROIindexVector

  nPtr = ptr_valid(self.pAABcornerlist) ? n_elements(*self.pAABcornerlist) : 0
  for i = 0L, nPtr-1 do ptr_free, (*self.pAABcornerlist)[i]
  ptr_free, self.pAABcornerlist

  self->cleanupAABList

end


;+
;
; NAME:    C_sAABContainer__Define.
;
; PURPOSE: C_sAABContainer object's structure class definition code.
;
;-
pro C_sAABContainer__Define
  tmp = { $
    C_sAABContainer,$
    pImage        : ptr_new(),$ ; 2D image for which the contours are adjusted.
    numRois       : 0L,$        ; A single ROI can be formed of one or more contour parts.
    numContours   : 0L,$
    pXcoords      : ptr_new(),$ ; pointer array to X-coordinates of each contour.
    pYcoords      : ptr_new(),$ ; pointer array to Y-coordinates of each contour.
    pZcoords      : ptr_new(),$ ; pointer array to Z-coordinates of each contour.
    pXcoordsBox   : ptr_new(),$ ; pointer array to X box coordinates of the contours, in the form [xMin, xMax].
    pYcoordsBox   : ptr_new(),$ ; pointer array to Y box coordinates of the contours, in the form [yMin, yMax].
    pZcoordsBox   : ptr_new(),$ ; pointer array to Z box coordinates of the contours, in the form [zMin, zMax].
    pXcoordsSub   : ptr_new(),$
    pYcoordsSub   : ptr_new(),$
    pZcoordsSub   : ptr_new(),$
    pBoundingShapeSub: ptr_new(),$
    pBoundingShapeSubName: ptr_new(),$
    ;experimental... start
    pFreeContourZones   : ptr_new(),$ ; for storing free/shared contour zones separately
    pSharedContourZones : ptr_new(),$ ; for storing free/shared contour zones separately
    pAdjacentROIindexVector: ptr_new(),$ ; for each ROI, the indices its neighboring ROIs (if any) are stored
    pAABcornerlist: ptr_new(),$
    pSpaceIndices : ptr_new(),$
    pSpaceIndexNames : ptr_new(),$
    ;experimental... end
    pAABlist      : ptr_new(),$ ; pointer array to vertex index of adjacent contour points of each contour (from pX-pY).
    pAABvertexType: ptr_new(),$ ; pointer array specifying a code for representing the AAB type of a given point.
                                ; 0: point is considered isolated from other ROI boundaries (non-AAB point),
                                ; 1: point lies within distance threshold from another ROI contour,
                                ; 2: point is contained within another ROI contour.
    pU            : ptr_new(),$ ; pointer to vector field X-components.
    pV            : ptr_new(),$ ; pointer to vector field Y-components.
    pW            : ptr_new(),$ ; pointer to vector field Z-components.
    proximityDist : 0.0d,$      ; Threshold distance for proximity/overlap/adjacency detection. Non-negative values will be used.
    separationDist: 0.0d,$      ; Separation distance for adjusted adjacenct boundaries. Non-negative values will be used.
    samplingDist  : 1.0d,$      ; Sampling distance reference value, used to control the number of contour points.
    alphaVal      : 0.0,$       ; Elasticity/contraction force weight (snake's tension).
    betaVal       : 0.0,$       ; Rigidity/bending force weight (snake's rigidity).
    gammaVal      : 0.0,$       ; Viscosity force weight.
    kappaVal      : 0.0,$       ; External force weight.
    minContourPointCount      : 16u,$   ; Minimum number of points for any contour.
    contourIterationsMax      : 0u,$    ; Max. number of iterations for the contours to converge.
    vfIterationsMax           : 0u,$    ; Max. number of iterations for image vector field to converge.
    contourErrorThresh        : 0.1d,$  ; Threshold error value for contour convergence evaluation.
    vfErrorThresh             : 0.1d,$  ; Threshold error value for image vector field convergence evaluation.
    contourErrorThreshNormType: 'hausdorff',$ ; Type of norm used to compute contour convergence. Default: 'inf'. Other values: '1', '2'.
    contourErrorVFNormType    : 'inf',$ ; Type of norm used to compute vector field convergence. Default: 'inf'. Other values: '1', '2'.
    fVerbose         : 1b,$ ; toggle verbose mode on/off.
    roiGroupFilePath : '',$ ; file path.
    inherits IDLanROI }
end


;+
; Begin TMP development functions. Delete when finished.
;-


; makePolygonsFromOverlap
;
; windingOutput  If set, specifies the output polygon winding (+1: CW, -1: CCW)
;
; ASSUMPTIONS - REQUIREMENTS
; - It is required that both polygons have the same winding. Don't be lazy and perform the checkings and corrections before ;)
; - The "overlap polygon" should be effectively a polygon resulting from an overlap computation algorithm, i.e. should not have
;   more than one "crossing" with the input polygon (polyX, polyY).
;
; HISTORY
;   First version, JJW (2013).
function makePolygonsFromOverlap, polyX, polyY, ovrPolyX, ovrPolyY, windingOutput = windingOutput, fVerbose = fVerbose

  fVerbose = keyword_set(fVerbose)
  oRoi = obj_new('IDLanROI', polyX, polyY)
  pointsInROI = oROI1->containsPoints(ovrPolyX, ovrPolyY)
  winding     = calcPolygonWinding(polyX, polyY)

  insideToOutside   = locatePair(pointsInROI, 0, 1, /searchForInvertedPair, isInverted = fIsInvertedIO)
  if (insideToOutside gt -1) and (fIsInvertedIO eq 1) then begin
    outsideToInside = insideToOutside - 1 ; Get the position of the '1' in the list
    insideToOutside = locatePair(pointsInROI, 1, 0)
  endif else outsideToInside = locatePair(pointsInROI, 1, 0)

  insideToEdge   = locatePair(pointsInROI, 0, 2, /searchForInvertedPair, isInverted = fIsInvertedIE)
  if (insideToEdge gt -1) and (fIsInvertedIE eq 1) then begin
    edgeToInside = insideToEdge - 1 ; Get the position of the '2' in the list
    insideToEdge = locatePair(pointsInROI, 2, 0)
  endif else edgeToInside = locatePair(pointsInROI, 2, 0)

  insideToVertex   = locatePair(pointsInROI, 0, 3, /searchForInvertedPair, isInverted = fIsInvertedIV)
  if (insideToVertex gt -1) and (fIsInvertedIV eq 1) then begin
    vertexToInside = insideToVertex - 1 ; Get the position of the '3' in the list
    insideToVertex = locatePair(pointsInROI, 3, 0)
  endif else vertexToInside = locatePair(pointsInROI, 3, 0)

  whereOut    = where(pointsInROI eq 0, countOut) ; ASSN countOut = 0 given how ovr-polygon is computed
  whereInside = where(pointsInROI eq 1, countInside)
  whereOnEdge = where(pointsInROI eq 2, countOnEdge)
  whereVertex = where(pointsInROI eq 3, countVertex)

  if fVerbose then begin
    print, 'countOut: '   , countOut
    print, 'countInside: ', countInside
    print, 'countOnEdge: ', countOnEdge
    print, 'countVertex: ', countVertex
  endif

  ; Re-label in order to ease handling
  pointsInROI[whereInside] = 0
  pointsInROI[whereOnEdge] = 1
  pointsInROI[whereVertex] = 2
  pointsInROI[whereOut]    = 3

  runListInside = makeRunList(whereInside)
  if fVerbose then print, 'runListInside: ', runListInside
  runListOnEdge = makeRunList(whereOnEdge)
  if fVerbose then print, 'runListOnEdge: ', runListOnEdge
  runListVertex = makeRunList(whereVertex)
  if fVerbose then print, 'runListVertex: ', runListVertex

  ; Since the input is an overlap computation polygon, the IDLanROI->ContainsPoints method output should look like this
  ; [* 1 1 1 1 *] or [1 1 * 1 1 1]
  ; with '*' being a series with any number of non-1 elements
  ; i.e. an array containing a single sequence of 1's, possibly shifted by an arbitrary quantity.
  pos1stPointInside    = 0
  pos1stPointNotInside = 1
  pos2ndPointInside    = 1
  pos2ndPointNotInside = 2

  halfPolyInsideX = ovrPolyX[pos1stPointInside : pos2ndPointInside]
  halfPolyInsideY = ovrPolyY[pos1stPointInside : pos2ndPointInside]

  halfPolyBoundX = ovrPolyX[pos1stPointNotInside : pos2ndPointNotInside]
  halfPolyBoundY = ovrPolyY[pos1stPointNotInside : pos2ndPointNotInside]

  case countVertex of
    2: begin
       endcase
    1: begin
       endcase
    0: begin
       endcase
    else: begin
          endcase
  endcase

  return, {innerPolyLine: inner, outerPolyLine: outer}
end

;+
; End TMP development functions. Delete when finished.
;-

function num2strTrim, num
  return, strCompress(string(num), /remove_all)
end
