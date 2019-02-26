; ____________________ (this is art) IOISIOI (this is art) ____________________
;
;     Utility functions for polygon computations.
;
; AUTHOR:
;     Jorge Jara (2011-2019) jjara@dcc.uchile.cl
;
; NOTES:
;     Some methods are coupled to dynamic link libraries (DLLs) made with geometric functions.
;
; ____________________ (this is art) IOISIOI (this is art) ____________________


; dotProduct
;
; Inner product function, intended for euclidean vectors.
function dotProduct, a, b
  na = n_elements(a)
  nb = n_elements(b)
  if (na ne nb) then begin
    print, 'dotProduct input argument error: array sizes do not match: ', na, nb
    stop
  endif
  dotP = a[0] * 0 ; Get numeric data type from the first vector.
  for i = 0L, na-1  do dotP += a[i]*b[i]
  return, dotP
end


; distanceBetween2DSegments
;
; Distance between two segments in the plane:
;  - segment one is (x11, y11) to (x12, y12)
;  - segment two is (x21, y21) to (x22, y22)
; The resulting type will be depending on the type of the input parameters (double/float)
; This function requires an external dynamic link library (DLL).
function distanceBetween2DSegments, x11, x12, y11, y12, x21, x22, y21, y22, endpointCode12 = endpointCode12, endpointCode21 = endpointCode21, fSqrDist = fSqrDist

;  if segments_intersect(x11, y11, x12, y12, x21, y21, y22, y22) then return, 0
;
;  ; try each of the 4 vertices w/the other segment
;  distances = [$
;    point_segment_distance(x11, y11, x21, y21, x22, y22), $
;    point_segment_distance(x12, y12, x21, y21, x22, y22), $
;    point_segment_distance(x21, y21, x11, y11, x12, y12), $
;    point_segment_distance(x22, y22, x11, y11, x12, y12) ]
;  return, min(distances)
  numType = size(x11, /TYPE) > size(x21, /TYPE)
  endpointCode12 = -1
  endpointCode21 = -1
  dllLocation = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
  d = call_external(dllLocation, 'distanceFromSegmentToSegment',$
                    fix(x11, type = 5), fix(y11, type = 5), fix(x12, type = 5), fix(y12, type = 5), $
                    fix(x21, type = 5), fix(y21, type = 5), fix(x22, type = 5), fix(y22, type = 5), $
                    endpointCode12, endpointCode21, $
                    /unload, RETURN_TYPE = 5)
  if keyword_set(fSqrDist) $
  then return,      numType eq 5 ? d : float(d)$
  else return, sqrt(numType eq 5 ? d : float(d))

end


pro distanceBetween2DSegments_test
  print, distanceBetween2DSegments(0, 1, 0, 0, 0, 1, 1, 1, ENDPOINTCODE12=e12, ENDPOINTCODE21=e21)
  print, distanceBetween2DSegments(0, 1, 2, 1, 2, 2, 2, 1, ENDPOINTCODE12=e12, ENDPOINTCODE21=e21)
  print, e12, e21
  print, distanceBetween2DSegments(0, 1, 2, 1, 2, 2, 2, 0, ENDPOINTCODE12=e12, ENDPOINTCODE21=e21)
  print, e12, e21
  print, distanceBetween2DSegments(1, 0, 1, 2, 2, 2, 2, 1, ENDPOINTCODE12=e12, ENDPOINTCODE21=e21)
  print, e12, e21

  print, 'the two following should yield the same result'
  print, distanceBetween2DSegments(0, 1, 0, 0, 0, 1, 1, 2)
  print, distanceBetween2DSegments(0, 0, 1, 0, 0, 1, 2, 1)

  print, distanceBetween2DSegments(0, 1, 0, 1, 0, 1, 2, 3)
  print, distanceBetween2DSegments(1, 0, 0, 1, 1, 0, 2, 3)

  print, distanceBetween2DSegments(0, 1, 0, 1, 1, 2, 2, 3)
  print, distanceBetween2DSegments(1, 0, 1, 0, 2, 1, 3, 2)

  print, distanceBetween2DSegments(0, 1, 0, 0, 0.5, 0.5, 0, 2)
  print, distanceBetween2DSegments(0, 1, 0, 0, 0.5, 0.5, 0.1, 2)
  print, distanceBetween2DSegments(1.0, 2.0, 0.5, 1.0, 0.0, 3.0, 0.6, 5.0)

end


;distanceBetweenSegments
;
; Calcula la distancia entre los segmentos de dos poligonos (ROI1 - ROI2), y regresa un arreglo
; donde la primera columna corresponde a los segmentos del ROI1 (sentido anti-horario), la segunda
; columna, el segmento mas cercano del ROI2, y en la ultima columna un indice que indica que punto
; del segmento del ROI2 es el mas cercano, esto es 0: primer punto del segmento
;                                                  1: segundo punto del segmento
;                                                  2: punto intermedio del segmento
;
function distanceBetweenSegments, p1x, p1y, p2x, p2y, $
                                  proximityCorrespondences = proximityCorrespondences, $
                                  proximityEndpointCodes   = proximityEndpointCodes, $
                                  crossingList12           = crossingList12, $
                                  distanceThreshold        = distanceThreshold, $
                                  whereInThreshold         = whereInThreshold, $
                                  fVerbose = fVerbose, fCloseInput = fCloseInput, fSqrDist = fSqrDist, fCircular = fCircular, vectorIn = vectorIn
  ;tStart  = sysTime(1)
  roiType = size(p1x, /TYPE)
  if ((roiType ne 4) and (roiType ne 5)) then begin
    p1x = double(p1x)
    p1y = double(p1y)
    p2x = double(p2x)
    p2y = double(p2y)    
  endif

  if keyword_set(fCloseInput) then begin
    polyClose, p1x = p1x, p1y = p1y
    polyClose, p1x = p2x, p1y = p2y
  endif

  n1 = n_elements(p1x)
  n2 = n_elements(p2x)
  if (n1 lt 2) or (n2 lt 2) then return, -1

  listLimit = keyword_set(fCircular) ? 0 : 1
  dist12    = roiType eq 4 ? fltArr(n1 - listLimit, n2 - listLimit) : dblArr(n1 - listLimit, n2 - listLimit) ; Distance matrix for ROI1-to-ROI2 segment distances
  minDist12 = roiType eq 4 ? fltArr(n1-listLimit) : dblArr(n1-listLimit)
  indices12 = uIntArr(n1-listLimit)
  endpointCodes = uIntArr(n1-listLimit)
  proximityCorrespondences = ptrArr(n1-listLimit)
  proximityEndpointCodes   = ptrArr(n1-listLimit)
  whereInThreshold = [-1]
fUseVectorIn = n_elements(vectorIn) eq n1
  fSaveCrossings = arg_present(crossingList12)
  if fSaveCrossings then $ 
    crossingList12 = ptrArr(n1-listLimit)

  if keyword_set(distanceThreshold) then begin
    sqrDistThreshold = distanceThreshold * distanceThreshold
    machineArithmetic = machAr()
    if (sqrDistThreshold lt machineArithmetic.EPS) $
    then print, 'WARNING: input threshold distance is less than the supported arithmetic precision (', machineArithmetic.EPS, ')'
    sqrDistThreshold >= machineArithmetic.EPS
  endif

  for i = 0u, n1-1-listLimit do begin ; Loop over each segment from the 1st polygon...
    fIinThreshold = 0b
    iNext = i eq (n1-1) ? 0 : i+1
    for j = 0u, n2-1-listLimit do begin ; comparing it against each segment from the 2nd polygon
      jNext = j eq (n2-1) ? 0 : j+1
      dist12[i,j] = distanceBetween2DSegments(p1x[i], p1x[iNext], p1y[i], p1y[iNext], p2x[j], p2x[jNext], p2y[jNext], p2y[jNext], $
                                              endpointCode12 = ec12, endpointCode21 = ec21, /fSqrDist) ;TODO JJ ec12/ec21 could be used for crossings list
      if ((dist12[i,j] eq 0) and (fSaveCrossings eq 1)) then begin
        if ptr_valid(crossingList12[i]) then begin
          curCrossings = *crossingList12[i]
          crossingList12[i] = ptr_new([curCrossings, j, jNext])
        endif else crossingList12[i] = ptr_new([j, jNext])
      endif

      if keyword_set(distanceThreshold) then begin
        if (dist12[i, j] le sqrDistThreshold) then begin
          fIinThreshold = 1b
          if ptr_valid(proximityCorrespondences[i]) then begin
            curCorresp = *proximityCorrespondences[i]
            curCorrEnd = *proximityEndpointCodes[i]
            ptr_free, proximityCorrespondences[i]
            ptr_free, proximityEndpointCodes[i]
            proximityCorrespondences[i] = ptr_new([curCorresp, j, jNext])
            proximityEndpointCodes[i]   = ptr_new([curCorrEnd, ec12])
          endif else begin
            proximityCorrespondences[i] = ptr_new([j, jNext])
            proximityEndpointCodes[i]   = ptr_new([ec12])
          endelse
        endif
      endif else begin
        if (j eq 0) then begin
          minDist12[i] = dist12[i, j]
          indices12[i] = j
          endpointCodes[i] = ec12
        endif
        if (dist12[i, j] lt minDist12[i]) then begin
          minDist12[i] = dist12[i, j]
          indices12[i] = j
          endpointCodes[i] = ec12
        endif
      endelse
    endfor
    fIn = fUseVectorIn ? vectorIn[i] : 0 ; if a vector of points from ROI 2 inside ROI 1 is provided, consider it
    if (fIinThreshold eq 1) or fIn then whereInThreshold = [whereInThreshold, i, iNext]
  endfor
  if n_elements(whereInThreshold) gt 1 then begin
    whereInThreshold = whereInThreshold[1:*]
    whereInThreshold = whereInThreshold[uniq(whereInThreshold, sort(whereInThreshold))]
  endif

  if ~keyword_set(fSqrDist) then dist12 = sqrt(temporary(dist12))
  if keyword_set(fVerbose) then begin
    print, dist12
    print, minDist12
    print, indices12
  endif

  if ~keyword_set(distanceThreshold) and arg_present(proximityCorrespondences) then begin
    proximityCorrespondences = ptr_new(indices12, /no_copy)
    proximityCorrespondences = ptr_new(endpointCodes, /no_copy)
    ;x = ptr_new([indices12], [minDist12], [endpointCodes]], /no_copy)
  endif

  if keyword_set(fVerbose) then print, endpointCodes
  ;print, 'Elapsed time: ', sysTime(1) - tStart
  return, dist12
end


pro distanceBetweenSegments_test

  ; example with Float
  p1x = [1.0,2.0,2.0,1.0]
  p1y = [1.0,1.0,2.0,2.0]
  p2x = [3.0,4.0,4.0,3.0]
  p2y = [1.0,1.0,3.0,3.0]
  print, distanceBetweenSegments(p1x, p1y, p2x, p2y)

  ; using Int, which will be converted to Double
  p1x = [1,2,2,1]
  p1y = [1,1,2,2]
  p2x = [3,4,4,3]
  p2y = [1,1,3,3]
  print, distanceBetweenSegments(p1x, p1y, p2x, p2y)

  p1x = [1.0,3.0,3.0,1.0]
  p1y = [1.0,1.0,3.0,3.0]
  p2x = [5.0,6.0,6.0,4.0]
  p2y = [1.0,2.0,4.0,2.0]
  print, distanceBetweenSegments(p1x, p1y, p2x, p2y)

  p1x = [1.0,3.0,2.0,1.0]
  p1y = [0.0,2.0,2.0,1.0]
  p2x = [6.0,4.0,2.0,4.0,5.0,4.0,4.0]
  p2y = [2.0,4.0,3.0,3.0,2.0,1.0,0.0]
  print, distanceBetweenSegments(p1x, p1y, p2x, p2y)
end


; distanceBetween2SegmentsIn3D
; IT DOESN'T WORK CORRECTLY YET!
;
; s1start | s1end | s2start | s2end  3-element arrays containing the coordinate of the endpoints for 
; input segments 1 and 2.
;
; Example:
; s1start  = [0, 0, 0]
; ...
function distanceBetween2SegmentsIn3D, s1start, s1end, s2start, s2end

  numType = size(x1a, /TYPE) > size(x2a, /TYPE)
  machineArithmetic = numType eq 5 ? machAr(/DOUBLE) : machAr()

  dif1 = s1end - s1start
  dif2 = s2end - s2start
  lengthS1 = sqrt(dotProduct(dif1, dif1))
  lengthS2 = sqrt(dotProduct(dif2, dif2))

  if (lengthS1 le machineArithmetic.EPS) or (lengthS2 le machineArithmetic.EPS) then begin

    if (lengthS1 le machineArithmetic.EPS) and (lengthS2 le machineArithmetic.EPS) then begin
      return, sqrt(dotProduct(s1start, s2start))
    endif

    if (lengthS1 le machineArithmetic.EPS) then begin
      print, 'not yet implemented'
      stop
    endif

    if (lengthS2 le machineArithmetic.EPS) then begin
      print, 'not yet implemented'
      stop
    endif

  endif

  u = double(s1end - s1start)
  v = double(s2end - s2start)
  w = double(s1start - s2start)

  a = dotProduct(u, u) ; >= 0
  b = dotProduct(u, v)
  c = dotProduct(v, v) ; >= 0
  d = dotProduct(u, w)
  e = dotProduct(v, w)
  D = a*c - b*b ; >= 0
  sD = D
  tD = D

    ; compute the line parameters of the two closest points
  if (D lt machineArithmetic.EPS) then begin ; the lines are almost parallel
    sN = 0.0                            ; force using point P0 on segment S1
    sD = 1.0                            ; to prevent possible division by 0.0 later
    tN = e
    tD = c
  endif else begin                      ; get the closest points on the infinite lines
    sN = (b*e - c*d)
    tN = (a*e - b*d)
    if (sN lt 0.0) then begin           ; sc < 0 => the s=0 edge is visible       
      sN = 0.0
      tN = e
      tD = c
    endif else if (sN gt sD) then begin ; sc > 1 => the s=1 edge is visible
      sN = sD
      tN = e + b
      tD = c
    endif
  endelse

  if (tN lt 0.0) then begin             ; tc < 0 => the t=0 edge is visible
    tN = 0.0
    ; recompute sc for this edge
    if (-d lt 0.0) then $
      sN = 0.0 $
    else if (-d gt a) then $
      sN = sD $
    else begin
      sN = -d
      sD = a
    endelse
  endif else if (tN gt tD) then begin   ; tc > 1 => the t=1 edge is visible
    tN = tD
    ; recompute sc for this edge
    if ((-d + b) lt 0.0) then $
      sN = 0.0 $
    else if ((-d + b) gt a) then $
      sN = sD $
    else begin
      sN = (-d + b)
      sD = a
    endelse
  endif

    ; finally do the division to get sc and tc
  sc = (abs(sN) lt machineArithmetic.EPS) ? 0.0 : (sN / sD)
  tc = (abs(tN) lt machineArithmetic.EPS) ? 0.0 : (tN / tD)

    ; get the difference of the two closest points
  dP = w + (sc * u) - (tc * v);  = S1(sc) - S2(tc)

    ; distance = norm(dP)
  print, 'distanceBetween2SegmentsIn3D: DOES NOT WORK CORRECTLY YET!'
  return, sqrt(dotProduct(dP, dP))
end


function distanceBetweenSegments_wrapFor3D, x11, x12, y11, y12, x21, x22, y21, y22
  s1a = [x11, y11, 0]
  s1b = [x12, y12, 0]
  s2a = [x21, y21, 0]
  s2b = [x22, y22, 0]
  return, distanceBetween2SegmentsIn3D(s1a, s1b, s2a, s2b)
end


pro distanceBetween2SegmentsIn3D_Test
  print, 'First, some 2D tests...'
  print, distanceBetweenSegments_wrapFor3D(0, 1, 0, 0, 0, 1, 1, 1)

  print, 'the two following should yield the same result'
  print, distanceBetweenSegments_wrapFor3D(0, 1, 0, 0, 0, 1, 1, 2)
  print, distanceBetweenSegments_wrapFor3D(0, 0, 1, 0, 0, 1, 2, 1)

  print, distanceBetweenSegments_wrapFor3D(0, 1, 0, 1, 0, 1, 2, 3)
  print, distanceBetweenSegments_wrapFor3D(1, 0, 0, 1, 1, 0, 2, 3)

  print, distanceBetweenSegments_wrapFor3D(0, 1, 0, 1, 1, 2, 2, 3)
  print, distanceBetweenSegments_wrapFor3D(1, 0, 1, 0, 2, 1, 3, 2)

  print, distanceBetweenSegments_wrapFor3D(1.0, 2.0, 0.5, 1.0, 0.0, 3.0, 0.6, 5.0)

  print, 'Now, some 3D tests...'
  print, 'mmm... not yet... bye!'
end


pro makeSeparationPlot, p1x, p1y, p2x, p2y, dataOut, minx = minx, maxx = maxx, miny = miny, maxy = maxy, window_title = window_title, overplot = overplot

  if ~keyword_set(window_title) then window_title = 'Separation Plot'

  minx12 = min(p1x, max = max1x) < min(p2x, max = max2x)
  maxx12 = max1x > max2x
  miny12 = min(p1y, max = max1y) < min(p2y, max = max2y)
  maxy12 = max1y > max2y

  minxx = n_elements(minx) gt 0 ? minx12 < minx : minx12
  minyy = n_elements(miny) gt 0 ? miny12 < miny : miny12
  maxxx = n_elements(maxx) gt 0 ? maxx12 > maxx : maxx12
  maxyy = n_elements(maxy) gt 0 ? maxy12 > maxy : maxy12

  print, 'xrange = ', minxx, ',', maxxx
  print, 'yrange = ', minyy, ',', maxyy

  xSz = 700
  ySz = 700

  dataCode = dataOut
  whNeg    = where(dataOut le 0, countNeg, complement = whPos, nComplement = countPos)
  if (countNeg gt 0) then dataCode[whNeg] = 0
  if (countPos gt 0) then dataCode[whPos] = 255

  if ~keyword_set(overplot) then begin
    window, 25, xsize = xSz, ysize = ySz, title = window_title
    tvscl, congrid(dataCode, xsz, ysz), channel = 3
    plot, p1x, p1y, xstyle = 5, ystyle = 5, xrange = [minxx, maxxx], yrange = [minyy, maxyy], xmargin = [0,0], ymargin = [0,0], /noerase, psym = 6
  endif else begin
    oplot, p1x, p1y, psym = 6
  endelse
  oplot, p2x, p2y, psym = 5

end


; getBorderPolylinesFrom2DistanceMap
;
; distanceMap    : Input image with the distance matrix from SVM classification
;                  Note: Input image has to be maxDistance wider and higher because 
;                  out border will cut maxDistance from each side.
; separationDist : Input parameter with separation distance between edges
; firstBorderOut : Output polygon (2xN1) of first border (no warranty of being upper).
; secondBorderOut: Output polygon (2xN2) of second border (no warranty of being lower).
; return         : [N1, N2].
function getBorderPolylinesFrom2DistanceMap, distanceMap, separationDist, firstBorderOut, secondBorderOut, fPlot = fPlot

  imgExpRad = 1 > (separationDist/2)
  distMapEx = s_expand_mirror(distanceMap, imgExpRad, mirrorType = 4)
  mapSize   = size(distanceMap, /dim)
  whNeg     = where(distMapEx le 0, countNeg, complement = whPos, nComplement = countPos)
  if (countNeg gt 0) then distMapEx[whNeg] = 0
  if (countPos gt 0) then distMapEx[whPos] = 1

  distanceDataOutPos = morph_distance(distMapEx, NEIGHBOR_SAMPLING = 3)
  whDistance         = where(distanceDataOutPos le separationDist, nDistance) ; LT, LE...
  if (nDistance gt 0) then distMapEx[whDistance] = 0

  contourLevels = [1.0]
  contourClosed = 0
  contour, distMapEx[1:mapSize[0], 1:mapSize[1]], path_xy = firstBorderOut, levels = contourLevels, closed = contourClosed, /PATH_DATA_COORDS

  levelMapEx1 = distanceDataOutPos

  if (countNeg gt 0) then distMapEx[whNeg] = 1
  if (countPos gt 0) then distMapEx[whPos] = 0

  distanceDataOutPos = morph_distance(distMapEx, NEIGHBOR_SAMPLING = 3)
  whDistance         = where(distanceDataOutPos le separationDist, nDistance)
  if (nDistance gt 0) then distMapEx[whDistance] = 0

  contour, distMapEx[1:mapSize[0], 1:mapSize[1]], path_xy = secondBorderOut, levels = contourLevels, closed = contourClosed, /PATH_DATA_COORDS

  levelMapEx1 += distanceDataOutPos
  ;mx = max(levelMapEx1, min = mn)
  ;levelMapEx1Neg = (-1*levelMapEx1) + mx + mn
  ;levelMapEx1[0,*] = -1
  ;levelMapEx1[*,0] = -1
  ;levelMapEx1[mapSize[0]+1,*] = -1
  ;levelMapEx1[*,mapSize[1]+1] = -1
  levelMap = levelMapEx1[imgExpRad:imgExpRad+(mapSize[0]-1), imgExpRad:imgExpRad+(mapSize[1]-1)]
  ;watershedImg = watershed(levelMapEx1Neg, connectivity = 8, nregions = regionCount, /long)
  return, [n_elements(firstBorderOut)/2, n_elements(secondBorderOut)/2]

end


; TODO [JJ] delete this after ensuring all of the ideas have been used
function getSeparationPolylineFrom2Dpolylines, p1x, p1y, ps2, p2y, data_x, data_y, thresholdDistance = thresholdDistance, fPlot = fPlot, np1 = np1, np2 = np2

  thresholdDistance = n_elements(thresholdDistance) gt 0 ? (thresholdDistance > 0) : 0

  ;borderPolygon = makeSVMseparator(data_out)
  nBorders = getBorderPolylinesFrom2DistanceMap(data_out, thresholdDistance, upperBorder, lowerBorder)
  print, 'Threshold distance: ', thresholdDistance
  print, 'Hausdorff distance for input point sets [input units]: ', s_HausdorffDistance2Dpoints([transpose(p1x), transpose(p1y)], [transpose(p2x), transpose(p2y)])
  print, 'Hausdorff distance for adjusted point sets [pixel units]: ', s_HausdorffDistance2Dpoints(upperBorder, lowerBorder)
  ; old version (to be deleted)
  ;nBorders = makeBorderFromSVMSeparator2Dpoints(data_out, thresholdDistance, upperBorder, lowerBorder)

  ; TODO replace with the actual keyword parameter values
  if ~keyword_set(np1) then nPtsInterp1 = 10 * n_elements(p1x)
  if ~keyword_set(np2) then nPtsInterp2 = 10 * n_elements(p2x)
  ;polygonArcSample, upperBorder[0,*], upperBorder[1,*], upperBorder_x_out, upperBorder_y_out, nPoints = nPtsInterp1, FCLOSEOUTPUT=0
  ;polygonArcSample, lowerBorder[0,*], lowerBorder[1,*], lowerBorder_x_out, lowerBorder_y_out, nPoints = nPtsInterp2, FCLOSEOUTPUT=0

  dpoly_1x = data_x[upperBorder[0,*], upperBorder[1,*]]
  dpoly_1y = data_y[upperBorder[0,*], upperBorder[1,*]]
  dpoly_2x = data_x[lowerBorder[0,*], lowerBorder[1,*]]
  dpoly_2y = data_y[lowerBorder[0,*], lowerBorder[1,*]]
  polygonArcSample, dpoly_1x, dpoly_1y, dpoly_1x_out, dpoly_1y_out, points = nPtsInterp1, /FPOLYLINE
  polygonArcSample, dpoly_2x, dpoly_2y, dpoly_2x_out, dpoly_2y_out, points = nPtsInterp2, /FPOLYLINE
  hDistPts       = s_HausdorffDistanceFor2Dpoints(DPOLY_1X,     DPOLY_1Y,     DPOLY_2X,     DPOLY_2Y)
  hDistPtsInterp = s_HausdorffDistanceFor2Dpoints(DPOLY_1X_OUT, DPOLY_1Y_OUT, DPOLY_2X_OUT, DPOLY_2Y_OUT)

  print, 'Hausdorff distance for adjusted point sets [input units]: ', hDistPts
  print, 'Hausdorff distance for interpolated adjusted point sets [input units]: ', hDistPtsInterp
  if keyword_set(fPlot) then begin
    min_datax = min(data_x, max = max_datax)
    min_datay = min(data_y, max = max_datay)
    makeSeparationPlot, dpoly_1x_out, dpoly_1y_out, dpoly_2x_out, dpoly_2y_out, data_out,$
                        minx = min_datax, maxx = max_datax, miny = min_datay, maxy = max_datay,$
                        window_title = 'Separation Plot With Boundary distance (', thresholdDistance, ' units)'
    ;oplot, dpoly_1x, dpoly_1y
    ;oplot, dpoly_2x, dpoly_2y
  endif
end


; makeObjectBorderChainCode
;                                             |3  2  1|
;   Builds chainCodeVector for voxelObject:   |4  X  0|  ; [Xpixel, YPixel, chaincode ...]
;                                             |5  6  7|
function makeObjectBorderChainCode, voxelObject; voxelObject: 2D/3D-Object = 1 in Voxel (PixelFrame + dim_xyz)

  dimVO = size(voxelObject, /dim)
  chainCodeVector = make_array(2*n_elements(voxelObject), /integer, value = -1)

  s = [0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5]
  x = [1, 1, 0,-1,-1,-1, 0, 1]
  y = [0, 1, 1, 1, 0,-1,-1,-1]
  r = [6, 6, 0, 0, 2, 2, 4, 4]

  a = (where(voxelObject ne 0, countWhNe0))[0]
    ; If for some reason there is no object to make the chain code for, return a default "invalid" array.
  if (countWhNe0 eq 0) then return, [-1]

  chainCodeVector[0:1] = [a mod dimVO[0], floor(1.0*a/dimVO[0])] ; Set first Object-Pixel
  x_run = chainCodeVector[0]
  y_run = chainCodeVector[1]

  for j = 3, 0, -1 do begin
    if voxelObject[x_run+x[j], y_run+y[j]] then begin
      chainCodeVector[2] = j
      GoDirection = j
      GoRight = r[j]
    endif
  endfor

  if (chainCodeVector[2] eq -1) then GoDirection = -1

  j = 2
  while (chainCodeVector[0] ne x_run) or $
        (chainCodeVector[1] ne y_run) or $
        (chainCodeVector[j>3] ne GoDirection) do begin
    x_run += x[chainCodeVector[j]]
    y_run += y[chainCodeVector[j]]
    a = GoRight + 1
    j += 1
    while (a lt (GoRight+8)) do begin
      if voxelObject[x_run + x[s[a]], y_run + y[s[a]]] then begin
        chainCodeVector[j] = s[a]
        GoRight = r[s[a]]
        a = 20
      endif
      a += 1
    endwhile
  endwhile

  return, [chainCodeVector[where(chainCodeVector ne -1)], -1]
end


; makePolygonFromChainCodeVector
;
;   Returns closedPolygonxyzPixel
;
; KEYWORDS
;   noClose                 Indicates whether to close or not the generated polygon.
;   reducedChainCodeVector  Optional chaincode generated for polygon.
;   xRange|yRange           Optional keywords to retrieve the min. and max. polygon positions in x|y.
;
; USAGE EXAMPLE:
;
; image = ...
; borderChainCode = makeObjectBorderChainCode(image)
; borderPolygon   = makePolygonFromChainCodeVector(borderChainCode)
; nPts = ...
; polygonArcSample, x, y, xInterpolated, yInterpolated, nPoints = nPts
function makePolygonFromChainCodeVector, chainCodeVector, noClose = noClose, xRange = xRange, yRange = yRange,$
                                         reducedChainCodeVector   = reducedChainCodeVector

  if (n_elements(chainCodeVector) le 1) then begin
    print, 'ERROR: chainCodeVector invalid (', chainCodeVector, ')'
    stop
    return, -1
  endif

  if ((size(chainCodeVector, /dim))[0] le 3) $
  then return, [transpose([chainCodeVector[0], chainCodeVector[0]]),$
                transpose([chainCodeVector[1], chainCodeVector[1]])]

  xRange = [chainCodeVector[0], chainCodeVector[0]]
  yRange = [chainCodeVector[1], chainCodeVector[1]]
  closedPolygonxPixel = chainCodeVector[0]
  closedPolygonyPixel = chainCodeVector[1]
  xyzStep             = [chainCodeVector[0], chainCodeVector[1]]
  stepVector          = [[1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0], [-1, -1], [0, -1], [1, -1]]
  reducedChainCodeVector = 0

  for i = 2, n_elements(chainCodeVector)-3 do begin
    repeat begin
      xyzStep += stepVector[*, chainCodeVector[i]]
      i += 1
    endrep until (chainCodeVector[i-1] ne chainCodeVector[i])
    i -= 1
    xRange[0] <= xyzStep[0]
    xRange[1] >= xyzStep[0]
    yRange[0] <= xyzStep[1]
    yRange[1] >= xyzStep[1]
    closedPolygonxPixel    = [closedPolygonxPixel, xyzStep[0]]
    closedPolygonyPixel    = [closedPolygonyPixel, xyzStep[1]]
    reducedChainCodeVector = [reducedChainCodeVector, chainCodeVector[i]]
  endfor

  reducedChainCodeVector = reducedChainCodeVector[1:*]
  if keyword_set(noClose) then begin
    ; Make polygon vector that does not close at last position
    polygon = [transpose(closedPolygonxPixel), transpose(closedPolygonyPixel)]
    return, polygon[*, 0: (size(polygon, /dim))[1]-2]
  endif else return, [transpose(closedPolygonxPixel), transpose(closedPolygonyPixel)]
end


; checkAndRepairPolygon
;
; Repair ill-formed polygons by removing self-intersections.
; The function makes a raster image and fills the polygon interior, in order to
; compute a chain code of the filled area. This chain code is the basis for the
; corrected output.
;
; fUseSmallestArea  Set this keyword to get the polygon with the smallest area between the ROI and the background.
function checkAndRepairPolygon, px, py, xrange, yrange, winId = winId, fUseSmallestArea = fUseSmallestArea
  winId = n_elements(winId) eq 0 ? 15 : winId
  window, winId, xsize = xrange[1]+1, ysize = yrange[1]+1, retain = 2
  plot, px, py, xrange = xrange, yrange = yrange, xstyle = 1, ystyle = 1, xmargin = [0,0], ymargin = [0,0], xticks = 1, yticks = 1
  polyFill, px, py
  img = tvrd()
  wdelete, winId
  if keyword_set(fUseSmallestArea) then begin
    wh0 = where(img eq 0, count0, complement = whNot0, nComplement = countNot0)
    if (count0 lt countNot0) then img = ~img
  endif
  img[*,0] = 0 & img[*,yrange[1]] = 0
  img[0,*] = 0 & img[xrange[1],*] = 0
  chainCode = makeObjectBorderChainCode(img)
  return, makePolygonFromChainCodeVector(chainCode, xrange = xrange, yrange = yrange)
end


; makePolygonsPlot
;
; p1x/p2x, p1y/p2y  Input array of x and y coordinates for 1 or 2 objects plot
;
; Optional parameters
; winTitle          String for window caption.
; sizeFactor        Parameter for plot scaling (it affects the window size as well).
; backgroundImage   Background image for the plot.
; psym              Specifies a plot symbol (values from 0 to 10, default: 2, 0 means connected lines, see "Graphics Keywords" in IDL help).
; linestyle         Indicates the line style of the lines used to connect the data point (0 means solid, see "Graphics Keywords" in IDL help).
;                   If set to 0, no custom symbols (psym) will be applied.
;
; An example to use with active contour would be something like this:
; 
;   acWinId = 31
;   makePolygonsPlot, [0], [0], backgroundImage = intImage, sizeFactor = 1.0, winTitle = 'Sample active contour plot', winId = acWinId, psym = 0, linestyle = 0
;   wSet, acWinId
;   ...
;   for i = 0, nObjects-1 do begin
;     ...
;     xCoords = ...
;     yCoords = ...
;     ...
;     oPlot, xCoords, yCoords, color = (pointCurvD-min(pointCurvD))/((max(pointCurvD)-min(pointCurvD))/255.)
;     ...
;   endfor
pro makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = winTitle, winId = winId, backgroundImage = backgroundImage, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, fNoScreenSizeAutoAdjust = fNoScreenSizeAutoAdjust,$
                      fCropImage = fCropImage, xPos = xPos, sizeFactorWin = sizeFactorWin, sizeFactorOut = sizeFactorOut, psym = psym, linestyle = linestyle, fUseImageScale = fUseImageScale, fNoAxes = fNoAxes, thick = thick

  xPos = keyword_set(xPos) ? (xPos > 0) : 0
  if keyword_set(backgroundImage) then imgSize = size(backgroundImage, /DIMENSIONS)

  if (n_elements(p1x) gt 1) then begin

    if (keyword_set(fUseImageScale) and keyword_set(backgroundImage)) then begin
      xMin = 0
      xMax = imgSize[0]-1
      yMin = 0
      yMax = imgSize[1]-1
    endif else begin
      xMin = min(p1x, max = xMax)
      yMin = min(p1y, max = yMax)
      if (n_elements(p2x) gt 0) then begin
        xMax >= max(p2x, min = minx2)
        xMin <= minx2
        yMax >= max(p2y, min = miny2)
        yMin <= miny2
      endif

      minMod50 = xMin mod 50
      if (minMod50 gt 0) then xMin -= minMod50
      minMod50 = yMin mod 50
      if (minMod50 gt 0) then yMin -= minMod50
      maxMod50 = xMax mod 50
      if (maxMod50 gt 0) then xMax += (50-maxMod50)
      maxMod50 = yMax mod 50
      if (maxMod50 gt 0) then yMax += (50-maxMod50)
      diff = (xMax-xMin) - (yMax-yMin)

      if (diff gt 0) and (yMin gt 0) then yMin -= diff
      if (diff lt 0) and (xMin gt 0) then xMin -= (-diff)
      if (yMin lt 0) then begin
        yMin = 0
        yMax += 50
      endif 
      if (xMin lt 0) then begin
        xMin = 0
        xMax += 50
      endif
    endelse
  endif else begin
    xMin = 0
    yMin = 0
    xMax = 1
    yMax = 1
    if (n_elements(backgroundImage) gt 0) then if (size(backgroundImage, /N_DIMENSIONS) eq 2) then begin
      xMax = imgSize[0] - 1
      yMax = imgSize[1] - 1
    endif
    if ~keyword_set(fUseImageScale) then begin
      maxMod50 = xMax mod 50
      if (maxMod50 gt 0) then xMax += (50-maxMod50)
      maxMod50 = yMax mod 50
      if (maxMod50 gt 0) then yMax += (50-maxMod50)
    endif
  endelse
  aspectRatioOrig = 1.0 * (xMax-xMin) / (yMax-yMin)

  sizeFactorOut = keyword_set(sizeFactorWin) ? sizeFactorWin : 3.3

  xSz = round((xMax-xMin) * sizeFactorOut)
  ySz = round((yMax-yMin) * sizeFactorOut)
  if ~keyword_set(fNoScreenSizeAutoAdjust) then begin
    ;device, get_screen_size = screenSize
    screenSize = [1072, 1920]
    ;xPos -= 1080
    if (screenSize[0] lt xSz) then xSz = screenSize[0]
    if (screenSize[1] lt ySz) then ySz = screenSize[1];*0.95
    aspectRatioZoom = 1.0 * xSz / ySz
    case 1 of
      aspectRatioZoom gt aspectRatioOrig: xSz = round(ySz * aspectRatioOrig)
      aspectRatioOrig gt aspectRatioZoom: ySz = round(xSz / aspectRatioOrig)
      else:
    endcase
  endif

  if ~keyword_set(winTitle) then winTitle = 'Util plot'

  if (n_elements(winId) gt 0) $
  then window, winId, xSize = xSz, ySize = ySz, title = winTitle, xPos = xPos $
  else window, xSize = xSz, ySize = ySz, title = winTitle, xPos = xPos

  if (n_elements(p1x) gt 1) and (n_elements(backgroundImage) gt 0) and (~keyword_set(fUseImageScale)) then begin
    xMax <= (imgSize[0] - 1)
    yMax <= (imgSize[1] - 1)
    if keyword_set(fCropImage) $
    then tvscl, congrid(backgroundImage[xMin:xMax,yMin:yMax], xSz, ySz) $
    else tvscl, congrid(backgroundImage, xSz, ySz)
  endif else begin
    if (n_elements(backgroundImage) gt 0) then $
      tvscl, keyword_set(sizeFactorOut) ? congrid(backgroundImage, xSz, ySz) : backgroundImage
  endelse

  axisStyle = 1 + (keyword_set(fNoAxes)  ? 4 : 0)
  linestyle = n_elements(linestyle) gt 0 ? linestyle : 1
  thick     = keyword_set(thick)         ? thick     : 1
  symSize   = (linestyle gt 0)           ? thick     : 1
  pSym      = n_elements(pSym)      gt 0 ? pSym      : 3
  pSym1 = pSym;4
  pSym2 = pSym;5
  pColor = 0;'000000'x
  if (keyword_set(fUseImageScale) and keyword_set(sizeFactorOut)) then begin
    ;xMin = 0 & xMax = xSz-1 & yMin = 0 & yMax = ySz-1
    plot, p1x, p1y, linestyle = linestyle, pSym = pSym1, color = pColor, thick = thick, symSize = symSize, $;, color = 'FFFF00'xs,$
          xRange = [0,xMax], yRange = [0,yMax], xMargin = [0,0], yMargin = [0,0], xStyle = axisStyle, yStyle = axisStyle, xticks = 1, yticks = 1, /noErase

    if (n_elements(p2x) gt 0) and (n_elements(p2x) gt 0) then $
      oPlot, p2x, p2y, linestyle = linestyle, pSym = pSym2, symSize = symSize, color = pColor, thick = thick;, color = 'FF8822'x
  endif else begin
    plot, p1x, p1y, linestyle = linestyle, pSym = pSym1, symSize = symSize, thick = thick, $;, color = 'FFFF00'xs,$
          xRange = [xMin, xMax], yRange = [yMin, yMax], xMargin = [0,0], yMargin = [0,0], xStyle = axisStyle, yStyle = axisStyle, xticks = 1, yticks = 1, /noErase

    if (n_elements(p2x) gt 0) and (n_elements(p2x) gt 0) then $
      oPlot, p2x, p2y, linestyle = linestyle, pSym = pSym2, symSize = symSize, thick = thick;, color = 'FF8822'x
  endelse

end


pro makePolygonsPlotWithVertices, p1x, p1y, p2x, p2y, winTitle = winTitle, winId = winId, backgroundImage = backgroundImage, $
                                  vertIndices1 = vertIndices1, vertIndices2 = vertIndices2, runsI = runsI, runsJ = runsJ, $
                                  xPos = xPos, fCropImage = fCropImage, fNoAxes = fNoAxes

  makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = winTitle, winId = winId, backgroundImage = backgroundImage, fCropImage = fCropImage, xPos = xPos, fNoAxes = fNoAxes

  n1 = n_elements(p1x)
  n2 = n_elements(p2x)
  if (n_elements(vertIndices1) gt 0) or (n_elements(vertIndices2) gt 0) then begin
    if (n_elements(vertIndices1) gt 0) then oPlot, [p1x[vertIndices1]], [p1y[vertIndices1]], psym = 6;, color = '00FF00'x 
    if (n_elements(vertIndices2) gt 0) then oPlot, [p2x[vertIndices2]], [p2y[vertIndices2]], psym = 6;, color = '00FF00'x
  endif else begin
    nRunsI = n_elements(runsI)/2
    nRunsJ = n_elements(runsJ)/2
    for i = 0L, nRunsI-1 do begin
      runPos = 2*i
      indices = runToIndexes(runsI[runPos], runsI[runPos+1], n1)
      oPlot, p1x[indices], p1y[indices], psym = 6, color = '00FF00'x
    endfor
    for i = 0L, nRunsJ-1 do begin
      runPos = 2*i
      indices = runToIndexes(runsJ[runPos], runsJ[runPos+1], n2)
      oPlot, p2x[indices], p2y[indices], psym = 6, color = '00FF00'x
    endfor
  endelse
end


; calcPolygonWinding
;   Determines if a given polygon (an ordered list of [x,y] vertices) is oriented CW or CCW.
;
; ARGUMENTS
;  xCoords The list of x coordinates of the polygon vertices.
;  yCoords The list of y coordinates of the polygon vertices.
;
; RETURN VALUE
;   1 if the polygon orientation is CW, -1 if is CCW
function calcPolygonWinding, xCoords, yCoords
  area = 0.0
  for i = 0L, size(xCoords, /n_elements)-2 do $
    area = area + xCoords[i]*yCoords[i+1] - xCoords[i+1]*yCoords[i]
  return, (area lt 0) ? 1 : -1
end


; testAndCorrectCWOrientation
;   Checks and correct (if necessary) the orientation of a given polygon (represented as an ordered list of consecutive [x,y] vertices).
;
; ARGUMENTS
;   px, py       Input lists containing the polygon [x,y] vertices.
;   pxOut, pyOut Output lists containing the polygon vertices in the required orientation (CW is the default).
;   fCCW         Set this keyword to a non-zero value in order to orientate the polygon CCW. CW is be used as default.
pro testAndCorrectCWOrientation, px, py, xOut = xOut, yOut = yOut, fCCW = fCCW
  windingPoly = calcPolygonWinding(px, py)
  fCCW        = keyword_set(fCCW)
  windingRef  = fCCW eq 1 ? -1 : 1
  xOut = windingPoly eq windingRef ? px : reverse(px)
  yOut = windingPoly eq windingRef ? py : reverse(py)
end


; polygonPerimeter
;
; Computes the perimeter for a given polygon (list of 2D points).
;
; The polygon is not required to be closed.
;
; Parameters:
;   x         The input polygon x-coordinate array.
;   y         The input polygon y-coordinate array.
;   xyFactor  (optional input) Scaling factor in x and y (array of 2 elements).
function polygonPerimeter, x, y, xyFactor = xyFactor

  if n_elements(x) eq 0 then return, -1.0
  if n_elements(x) eq 1 then return,  0.0

  if (n_elements(xyFactor) eq 0) then xyFactor = [1., 1.]
  if (n_elements(xyFactor) eq 1) then xyFactor = [xyFactor, xyFactor]

  return, total(sqrt(((shift(x,-1) - x) * xyFactor[0])^2$
                    +((shift(y,-1) - y) * xyFactor[1])^2))
end


; An awfully simple first test XD
pro polygonPerimeterTest
  x = [0, 1, 1, 0]
  y = [0, 0, 1, 1]
  print, polygonPerimeter(x, y)
end


; polyArea
;
;  Computes and return the area of a given polygon.
;
; ARGUMENTS
;  method = 0 or undefined -> IDL computation
;  method = 1 -> C++ DLL computation
;  if roiObj is specified, then xCoords and yCoords are ignored
;  if xCoords/yCoords are specified, they must not have their initial/end point as the same
;
; RETURN VALUE
;  The computed polygon area.
;
; NOTES
;  For ROI objects (C_sROIObject), holes are not considered in the area computation... yet.
function polyArea, xCoords = xCoords, yCoords = yCoords, roiObj = roiObj, method = method

  if (n_elements(method) eq 0) then method = 0

  if keyword_set(roiObj) then begin

    pBorderPolygon = roiObj->getpObjectBorderPolygonList()
    _pX = transpose((*pBorderPolygon[0])[0,*])
    _pY = transpose((*pBorderPolygon[0])[1,*])
    _pX = [_pX, _pX[0]]
    _pY = [_pY, _pY[0]]

    nPoly = n_elements(pBorderPolygon)
    for i = 0L, nPoly-1 do ptr_free, pBorderPolygon[i]
    pBorderPolygon = -1
  endif else begin
    _pX = [xCoords, xCoords[0]]
    _pY = [yCoords, yCoords[0]]
  endelse
  testAndCorrectCWOrientation, _pX, _pY, XOUT = pX, YOUT = pY, fCCW = (method eq 0)
  area = 0d

  case method of
    0: begin
       ; CCW-polygon area computation
       ; area = 0.5 * sum(i=0.. n-1) x[i] * y[i+1] - x[i+1] * y[i]
       ; n-1 equiv to 0 (closed polygon)
       for i = 0L, n_elements(pX)-2 do $
         area += (pX[i] * pY[i+1]) - (pX[i+1] * pY[i])
       area *= 0.5
    endcase
    1: begin
       print, 'DLL method for area computation'
       dllLocation = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
       area = call_external(dllLocation, 'polygonArea',$
                            pX, pY, fix(n_elements(pX), type = 2),$
                            RETURN_TYPE = 4, /unload)
     endcase
    else: begin
      print, 'no match for specified method for area computation (', method, ')'
      return, -1
    endcase
  endcase
  return, area
end


; trimPolygonsArray
;
; Remove non-used elements of a multi-polygon array, as it occurs with current DLL polygon function calls.
function trimPolygonsArray, polyArr
  if (n_elements(polyArr) lt 3) then return, polyArr
  nPolys = polyArr[0]
  polyStartIndex = 1L
  if (nPolys gt 1) then for i = 1L, nPolys do polyStartIndex += (2*polyArr[polyStartIndex]) + 1
  delta = (nPolys gt 1) ? -1 : 0
  return, polyArr[0:polyStartIndex + 2*polyArr[polyStartIndex] + delta]
end


; trimPolygonsArray2
;
; Remove non-used elements of a multi-polygon array, as it occurs with current DLL polygon function calls.
; [mcerda] Now each vertex has dimension 4 (x,y,c1,c2) for the index in the intersecting polygons.
function trimPolygonsArray2, polyArr
  if (n_elements(polyArr) lt 3) then return, polyArr
  nPolys = polyArr[0]
  polyStartIndex = 1L
  if (nPolys gt 1) then for i = 1L, nPolys do polyStartIndex += (4*polyArr[polyStartIndex]) + 1
  delta = (nPolys gt 1) ? -1 : 0
  return, polyArr[0:polyStartIndex + 4*polyArr[polyStartIndex] + delta]
end


pro polyClose, p1x = p1x, p1y = p1y, double = double
  n1 = n_elements(p1x)
  if (p1x[0] ne p1x[n1-1]) or (p1y[0] ne p1y[n1-1]) then begin
    p1x = [p1x, p1x[0]]
    p1y = [p1y, p1y[0]]
  endif
  ; Type check for double precision arrays (mandatory, for some DLL 
  ; functions that can make IDL crash).
  if keyword_set(double) then begin
    if (size(p1x, /TYPE) ne 5) then p1x = double(p1x)
    if (size(p1y, /TYPE) ne 5) then p1y = double(p1y)
  endif
end


; TODO JuanEdo Este es el que funciona!!
; polygonCheckAndRemoveCollinearSegments
; This function requires an external dynamic link library (DLL).
pro polygonCheckAndRemoveCollinearSegments, px, py, pxOut = pxOut, pyOut = pyOut, distThresh = distThresh
  nPts = n_elements(px)
  nSegments = px[0] eq px[nPts-1] ? nPts-1 : nPts
  collinearSegmentPairs = [-1]
  collinearSegmentCount = 0L
  defaultEpsilon = 0.001
  distTresh = keyword_set(distTresh) ? (abs(distTresh) > defaultEpsilon) : defaultEpsilon
  libLoc = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
  for i = 0L, nSegments-2 do begin
    endpointCode1 = -1
    endpointCode2 = -1
    zeroLenFlag = -1
    d1 = call_external(libLoc, 'distanceFromPointToSegment',$
                       fix(px[i+2], type = 5), fix(py[i+2], type = 5), fix(px[i], type = 5), fix(py[i], type = 5), fix(px[i+1], type = 5), fix(py[i+1], type = 5), $
                       endpointCode1, zeroLenFlag, RETURN_TYPE = 5, /unload)
    d2 = call_external(libLoc, 'distanceFromPointToSegment',$
                       fix(px[i], type = 5), fix(py[i], type = 5), fix(px[i+2], type = 5), fix(py[i+2], type = 5), fix(px[i+1], type = 5), fix(py[i+1], type = 5), $
                       endpointCode2, zeroLenFlag, RETURN_TYPE = 5, /unload)
    if (d1 le distTresh) or (d2 le distTresh) then begin
      print, 'At segment ', i, ': endpointCode1 ', endpointCode1, ' - endpointCode2 ', endpointCode2, ' - d1 ', d1, ' - d2 ', d2
      collinearSegmentCount += 1
      collinearSegmentPairs = [collinearSegmentPairs, i]
    endif
  endfor
  pxOut = px
  pyOut = py
  ; REPAIR CONNECTIVITY BY REMOVING THE MIDDLE POINT OF THE TWO SEGMENTS
  if (collinearSegmentCount gt 0) then begin
    collinearSegmentPairs = collinearSegmentPairs[1:*]
    for i = 0L, collinearSegmentCount-1 do begin
      fZeroLenSegment = (abs(pxOut[collinearSegmentPairs[i]] - pxOut[collinearSegmentPairs[i]+2]) le distTresh) $
                    and (abs(pyOut[collinearSegmentPairs[i]] - pyOut[collinearSegmentPairs[i]+2]) le distTresh)
      if ~fZeroLenSegment then begin
        pxOut = [pxOut[0:collinearSegmentPairs[i]], pxOut[collinearSegmentPairs[i]+2:*]]
        pyOut = [pyOut[0:collinearSegmentPairs[i]], pyOut[collinearSegmentPairs[i]+2:*]]
        if (i lt collinearSegmentCount-1) then collinearSegmentPairs[i+1:*] -= 1
      endif else begin
        stop  ;In this case we should also remove points at collinearSegmentPairs[i] and collinearSegmentPairs[i]+2
      endelse
    endfor
  endif
;  ; PARANOID VERSION IMPLIES COMPARING THE POINT COORDINATES
;  if (collinearSegmentCount gt 0) then begin
;    collinearSegmentPairs = collinearSegmentPairs[1:*]
;    for i = 0L, collinearSegmentCount-1 do begin
;      pairPos = 2*i
;      pix = px[collinearSegmentPairs[pairPos]]
;      piy = py[collinearSegmentPairs[pairPos]]
;      pi1x = px[collinearSegmentPairs[pairPos]+1]
;      pi1y = py[collinearSegmentPairs[pairPos]+1]
;      pi2x = px[collinearSegmentPairs[pairPos]+2]
;      pi2y = py[collinearSegmentPairs[pairPos]+2]
;      fMatchI = ((pix eq pi1x) and (piy eq pi1y)) or ((pix eq pi2x) and (piy eq pi2y))
;      stop
;    endfor
;  endif
end


; calcPolygonIntersection
;
; Output is a 1D array with x,y coords plus some extra information...
; output[0] count of the resulting intersection polygons
; output[1] number of vertices of the first polygon p1, namely n1
; output[2, ... , 2 + n1 -1] x coords of the n1 vertices of p1
; output[2 + n1, ... , 2  + n1 + n1 - 1] y coords of the n1 vertices of p1
; output[2*n1+2] number of vertices of the second polygon p2, namely n2
; The polygons must be defined in cw order, and the last point must be the same as the first
;
; DEPENDENCIES
; - DLL for the imlpemented polygon utility functions.
function calcPolygonIntersection, p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y
  dllLocation = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
  testAndCorrectCWOrientation, p1x, p1y, XOUT = p1x_, YOUT = p1y_
  testAndCorrectCWOrientation, p2x, p2y, XOUT = p2x_, YOUT = p2y_
  ; XXX problem with dynamic memory in the DLL... making some workarounds here
  errdeltaoutputsize = 0
  factor = 0.5
  output = [-1.0d]

  polyClose, p1x = p1x_, p1y = p1y_, /double
  polyClose, p1x = p2x_, p1y = p2y_, /double
  n1 = n_elements(p1x_)
  n2 = n_elements(p2x_)
  repeat begin
    factor += 1.
    estimated_output_size = ceil(((n1 + n2) * 2) * factor)
    print, 'trying with output size ', estimated_output_size
    output = dblArr(estimated_output_size)
    nIntersections = call_external(dllLocation, 'intersection',$
                                   p1x_, p1y_, fix(n1, type = 2), p2x_, p2y_, fix(n2, type = 2),$
                                   output, estimated_output_size, errdeltaoutputsize, /unload)
  endrep until (errdeltaoutputsize le 0)
  print, 'found ', nIntersections, ' intersection(s) for input polygons'
  return, trimPolygonsArray(output)
end


; calcPolygonIntersectionAlt
; Alternative (under testing) version of the polygon intersection function.
; Implementation in external C++ library uses the Clipper library.
; http://sourceforge.net/projects/polyclipping/
;
; intFactor Integer value (>0) used to scale polygon coordinates, since the Clipper library
; uses only integer values (internal type long long int).
;
; This function requires an external dynamic link library (DLL).
function calcPolygonIntersectionAlt, p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, intFactor = intFactor
  dllLocation = getDLLfilename('polygon_intersection_alt', /GETFILENAMEANDPATH)
  testAndCorrectCWOrientation, p1x, p1y, XOUT = p1x_, YOUT = p1y_
  testAndCorrectCWOrientation, p2x, p2y, XOUT = p2x_, YOUT = p2y_
  ; XXX problem with dynamic memory in the DLL... making some workarounds here
  errdeltaoutputsize = 0
  factor = 2.5
  output = [-1.0d]

  polyClose, p1x = p1x_, p1y = p1y_, /double
  polyClose, p1x = p2x_, p1y = p2y_, /double
  n1 = n_elements(p1x_)
  n2 = n_elements(p2x_)
  intFactor = keyword_set(intFactor) ? round(abs(intFactor)) : 1000
  extra_pad = 0u
  repeat begin
    estimated_output_size = ceil(((n1 + n2) * 2) * factor) + errdeltaoutputsize + extra_pad
    ;print, 'trying with output size ', estimated_output_size
    output = dblArr(estimated_output_size)
    nIntersections = call_external(dllLocation, 'intersection',$
                                   p1x_, p1y_, n1, p2x_, p2y_, n2,$
                                   output, estimated_output_size, errdeltaoutputsize, fix(intFactor, type = 2), /unload)
    extra_pad += 1
  endrep until (errdeltaoutputsize le 0)
  return, trimPolygonsArray(output)
end


;calcPolygonIntersectionWithPolyIndices
;
; TODO JuanEdo Comentar esta funcion
; 
; ARGUMENTS
; 
;
; endpoint1code/endpoint2code 3-element vector that tndicates...
; [sharedStatus, indexInPolygon1, indexInPolygon2] Output variable. 
; sharedStatus = 0 this point is not present in any of the two input polygons
;              = 1 this point is present in the first polygon, at position given in indexInPolygon1
;              = 2 this point is present in the second polygon, at position given in indexInPolygon2
;              = 3 this point is present in both polygons, at positions given in indexInPolygon1/indexInPolygon2
; RETURN VALUE
;
;   intersectionsArray = [numInter, numVertexP1, xp1_1,...,xp1_numVertexP1,yp1_1,...,yp1_numVertexP1,numVertexP2, xp2_1,...,xp2_numVertexP1,yp2_1,...,yp2_numVertexP2,...]
;   Un arreglo donde el primer elemento es el número de intersecciones encontradas, seguido del numero de vertices del poligono, más los puntos X e Y
;   que lo conforman, esto para cada intersección encontrada.
; NOTES
function calcPolygonIntersectionWithPolyIndices, p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, intFactor = intFactor, fForcePointsForUniqueIntersection = fForcePointsForUniqueIntersection, $
                                                 indexCodes = indexCodes, endpoint1code = endpoint1code, endpoint2code = endpoint2code,$
                                                 pVertexIndices = pVertexIndices, fPlot = fPlot, fVerbose = fVerbose, fNoClose = fNoClose,$
                                                 backImage = backImage, boundingBoxX = boundingBoxX, boundingBoxY = boundingBoxY, polyAreas = polyAreas
  dllLocation = getDLLfilename('polygon_intersection_alt', /GETFILENAMEANDPATH)
  testAndCorrectCWOrientation, p1x, p1y, XOUT = p1x_, YOUT = p1y_
  testAndCorrectCWOrientation, p2x, p2y, XOUT = p2x_, YOUT = p2y_
  ; XXX problem with dynamic memory in the DLL... making some workarounds here
  errdeltaoutputsize = 0L
  factor = 1.0
  output = [-1.0d]

  if ~keyword_set(fNoClose) then begin
    polyClose, p1x = p1x_, p1y = p1y_, /double
    polyClose, p1x = p2x_, p1y = p2y_, /double
  endif
  n1 = n_elements(p1x_)
  n2 = n_elements(p2x_)
  intFactor = keyword_set(intFactor) ? round(abs(intFactor)) : 1000 ; TODO intFactor default value depending on...
  extra_pad = 0u
  repeat begin
    estimated_output_size = ceil(((n1 + n2) * 2) * factor) + errdeltaoutputsize + extra_pad
    estimated_codes_size  = ceil( (n1 + n2)      * factor) + errdeltaoutputsize + extra_pad
    if keyword_set(fVerbose) then print, 'trying with output size ', estimated_output_size
    output       = dblArr(estimated_output_size)
    indexCodes   = lonArr(estimated_output_size)
    boundingBoxX = dblArr(estimated_codes_size)
    boundingBoxY = dblArr(estimated_codes_size)
    polyAreas    = dblArr(estimated_codes_size)
    sharedEndpoint1code = -1L
    sharedEndpoint1indexPoly1 = -1L
    sharedEndpoint1indexPoly2 = -1L
    sharedEndpoint2code = -1L
    sharedEndpoint2indexPoly1 = -1L
    sharedEndpoint2indexPoly2 = -1L
    nIntersections = call_external(dllLocation, 'intersectionWithIndices',$
                                   p1x_, p1y_, n1, p2x_, p2y_, n2,$
                                   output, estimated_output_size, errdeltaoutputsize, intFactor, indexCodes,$
                                   sharedEndpoint1code, sharedEndpoint1indexPoly1, sharedEndpoint1indexPoly2,$
                                   sharedEndpoint2code, sharedEndpoint2indexPoly1, sharedEndpoint2indexPoly2);,$
                                   ;boundingBoxX, boundingBoxY, polyAreas, /unload)
    extra_pad += 1
  endrep until (errdeltaoutputsize le 0)
  if (nIntersections eq 0) then return, [0]

  intersectionsArray = trimPolygonsArray(output)
  if keyword_set(fPlot) then plotPolyAndIntersections, p1x, p1y, p2x, p2y, intersectionsArray, backImage = backImage, xPos = 650

  endpoint1code = [sharedEndpoint1code, sharedEndpoint1indexPoly1, sharedEndpoint1indexPoly2]
  endpoint2code = [sharedEndpoint2code, sharedEndpoint2indexPoly1, sharedEndpoint2indexPoly2]

  ovrPolyStart = 2 ; position counter for reading the output array
  fKeepVertexIndices = arg_present(pVertexIndices)
  pVertexIndices = ptrArr(4, nIntersections)

  for p = 0L, nIntersections-1 do begin
;    ovrPolyVertexCount = intersectionsArray[ovrPolyStart-1]
    ; Toggle comment if you want to get the last point (which is the same as the first with the current implementation).
;    ovrPolyX = intersectionsArray[ovrPolyStart                      : ovrPolyStart +   ovrPolyVertexCount - 1]
;    ovrPolyY = intersectionsArray[ovrPolyStart + ovrPolyVertexCount : ovrPolyStart + 2*ovrPolyVertexCount - 1]
    ; Get n-1 points, since the polygons come with the last point equal to the first
    ;ovrPolyX = intersectionsArray[ovrPolyStart                      : ovrPolyStart +   ovrPolyVertexCount - 2]
    ;ovrPolyY = intersectionsArray[ovrPolyStart + ovrPolyVertexCount : ovrPolyStart + 2*ovrPolyVertexCount - 2]
;    ovrPolyStart += 2*ovrPolyVertexCount + 1

    nVertices  = indexCodes[ovrPolyStart-1]
    curIndices = indexCodes[ovrPolyStart : ovrPolyStart+nVertices-1]
    wherePoly1indices = where(curIndices lt 0, nIndices1); look for the indices belonging to the 1st oply
    if keyword_set(fVerbose) then begin
      print, 'Processing intersection polygon ', p, ' with ', nVertices, ' vertices'
      print, 'curIndices ', curIndices
      print, 'nIndices1 ' , nIndices1
      print, 'polygon bounding box x: ', boundingBoxX[2*p:2*p+1]
      print, 'polygon bounding box y: ', boundingBoxY[2*p:2*p+1]
      print, 'polygon area ' , polyAreas[p]
    endif

    if (nIndices1 gt 0) then begin
      poly1indices = reverse((-curIndices[wherePoly1indices]) - 1)
       ; If the point found is the last of the polygon, set its index to 0.
      wh0 = where(poly1indices eq (n1-1), count0)
      if (count0 gt 0) then poly1indices[wh0] = 0
      if keyword_set(fVerbose) then print, 'retrieved indices from polygon 1: ', poly1indices
    endif else poly1indices = [-1]

    if keyword_set(fForcePointsForUniqueIntersection) then begin
      runs  = makeRunFromCorrelativeValues(poly1indices, nMax = (n1-1))
      nRuns = n_elements(runs) / 2
      if (nRuns gt 1) then begin
        print, 'Warning: polygon intersection returned non-consecutive vertices within polygon 1 and flag to force consecutive vertices is SET.'
        ; TODO JJW code vector handling
        poly1indices = runsToIndices([runs[0],runs[2*nRuns-1]], n1)
      endif
    endif

    pVertexIndices[0,p] = ptr_new(poly1indices, /NO_COPY)

    fAddEndpoints = 1b
    case 1 of
      (endpoint1code[0] eq 1) and (endpoint2code[0] eq 1): tmp = [endpoint1code[1], endpoint2code[1]]
      (endpoint1code[0] eq 1) and (endpoint2code[0] ne 1): tmp = [endpoint1code[1]]
      (endpoint1code[0] ne 1) and (endpoint2code[0] eq 1): tmp = [endpoint2code[1]]
      else: fAddEndpoints = 0b
    endcase

    if (fAddEndpoints eq 1) then begin
      if keyword_set(fVerbose) then print, 'adding endpoints from polygon 1: ', tmp
      pVertexIndices[1,p] = ptr_new(tmp)
    endif

    wherePoly2indices = where(curIndices gt 0, nIndices2)
    if keyword_set(fVerbose) then print, 'nIndices2 ', nIndices2
    ;nIndices2 += (endpoint1code[0] eq 2) + (endpoint1code[1] eq 2) ; Ensure that endpoints are considered if needed
    if (nIndices2 gt 0) then begin
      poly2indices = reverse(curIndices[wherePoly2indices] - 1)
       ; If the point found is the last of the polygon, set its index to 0.
      wh0 = where(poly2indices eq (n2-1), count0)
      if (count0 gt 0) then poly2indices[wh0] = 0
      if keyword_set(fVerbose) then print, 'retrieved indices from polygon 2: ', poly2indices
    endif else poly2indices = [-1]

    if keyword_set(fForcePointsForUniqueIntersection) then begin
      runs  = makeRunFromCorrelativeValues(poly2indices, nMax = (n2-1))
      nRuns = n_elements(runs) / 2
      if (nRuns gt 1) then begin
        print, 'Warning: polygon intersection returned non-consecutive vertices within polygon 2 and flag to force consecutive vertices is SET.'
        ; TODO JJW code vector handling
        poly2indices = runsToIndices([runs[0],runs[2*nRuns-1]], n2)
      endif
    endif

    pVertexIndices[2,p] = ptr_new(poly2indices, /NO_COPY)

    fAddEndpoints = 1b
    case 1 of
      (endpoint1code[0] eq 2) and (endpoint2code[0] eq 2): tmp = [endpoint1code[2], endpoint2code[2]]
      (endpoint1code[0] eq 2) and (endpoint2code[0] ne 2): tmp = [endpoint1code[2]]
      (endpoint1code[0] ne 2) and (endpoint2code[0] eq 2): tmp = [endpoint2code[2]]
      else: fAddEndpoints = 0b
    endcase

    if (fAddEndpoints eq 1) then begin
      if keyword_set(fVerbose) then print, 'adding endpoints from polygon 2: ', tmp
      pVertexIndices[3,p] = ptr_new(tmp)
    endif

    ovrPolyStart += nVertices+1
  endfor

  if (fKeepVertexIndices eq 0) then $
  for i = 0, nIntersections-1 do ptr_free, pVertexIndices[0,i], pVertexIndices[1,i], pVertexIndices[2,i], pVertexIndices[3,i]

  return, intersectionsArray
end


pro calcPolygonIntersectionWithPolyIndices_TestPolylines
  p1x = [1.0, 2.0, 3.0, 4.0]
  p1y = [1.0, 1.5, 1.5, 1.0]
  p2x = [1.0, 2.0, 3.0, 4.0]
  p2y = [2.0, 1.4, 1.4, 2.0]
  o = calcPolygonIntersectionWithPolyIndices(p1x = reverse(p1x), p1y = reverse(p1y), p2x = reverse(p2x), p2y = reverse(p2y), intFactor = 10)
  print, o
end


; TODO jjara 2015.05.20 check if this function can be deleted, together with is C++ counterpart
; mcerda, la funcion retorna una lista con lo siguiente
; [n_poligonos n_vertices_p1 p1_x1 p1x2 p1y1 p1y2 p1c1 p1c2 ...]
; donde p1c1 es el indice en el poligono 1 de ese punto.
; Si no coincide con ningun vertices, entonces es -1
function calcPolygonIntersectionWithPolyIndices2, p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, intFactor = intFactor,$
                                                  fPlot = fPlot, fVerbose = fVerbose, backImage = backImage, fUseAltIndexCalc = fUseAltIndexCalc

  dllLocation = getDLLfilename('polygon_intersection_alt', /GETFILENAMEANDPATH)
  funcName = keyword_set(fUseAltIndexCalc) ? 'intersectionWithIndices' : 'intersectionWithIndices2'
  funcName = 'intersectionWithIndices2' ; TODO JJ delete this line after testing is complete
  testAndCorrectCWOrientation, p1x, p1y, XOUT = p1x_, YOUT = p1y_
  testAndCorrectCWOrientation, p2x, p2y, XOUT = p2x_, YOUT = p2y_
  ; XXX problem with dynamic memory in the DLL... making some workarounds here

  errdeltaoutputsize = 0
  factor = 1.0
  output = [-1.0d]

  polyClose, p1x = p1x_, p1y = p1y_, /double
  polyClose, p1x = p2x_, p1y = p2y_, /double
  n1 = n_elements(p1x_)
  n2 = n_elements(p2x_)
  intFactor = keyword_set(intFactor) ? round(abs(intFactor)) : 1000
  extra_pad = 0
  repeat begin
    ;mcerda now is factor 4
    estimated_output_size = ceil(((n1 + n2) * 4) * factor) + errdeltaoutputsize + extra_pad

    if keyword_set(fVerbose) then print, 'trying with output size ', estimated_output_size
    output = dblArr(estimated_output_size)

    nIntersections = call_external(dllLocation, funcName,$
                                   p1x_, p1y_, n1, p2x_, p2y_, n2,$
                                   output, estimated_output_size, errdeltaoutputsize, intFactor, /unload)
    extra_pad += 1
  endrep until (errdeltaoutputsize le 0)

  if (nIntersections eq 0) then return, [0]

  intersectionsArray = trimPolygonsArray2(output)
  if keyword_set(fPlot) then plotPolyAndIntersections, p1x, p1y, p2x, p2y, intersectionsArray, backImage = backImage, xPos = 650

  ovrPolyStart = 2
  fKeepVertexIndices = arg_present(pVertexIndices)
  pVertexIndices = ptrArr(4, nIntersections)

  if (fKeepVertexIndices eq 0) then $
  for i = 0, nIntersections-1 do ptr_free, pVertexIndices[0,i], pVertexIndices[1,i], pVertexIndices[2,i], pVertexIndices[3,i]

  return, intersectionsArray
end


pro plotPolyAndIntersections, p1x, p1y, p2x, p2y, intersectionsArray, backImage = backImage, xPos = xPos

  makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = 'Intersection Plot', winId = 31, backgroundImage = backImage, /fCropImage, xPos = xPos
  nOvrPolys = intersectionsArray[0]
  ovrPolyStart = 2
  ; Iterate over the overlap polygons list (there could be more than one)
  for p = 0L, nOvrPolys-1 do begin
    ovrPolyVertexCount = intersectionsArray[ovrPolyStart-1]
    ; Toggle comment if you want to get the last point (which is the same as the first with the current implementation).
    ovrPolyX = intersectionsArray[ovrPolyStart                      : ovrPolyStart +   ovrPolyVertexCount - 1]
    ovrPolyY = intersectionsArray[ovrPolyStart + ovrPolyVertexCount : ovrPolyStart + 2*ovrPolyVertexCount - 1]
    ; Get n-1 points, since the polygons come with the last point equal to the first
    ;ovrPolyX = intersectionsArray[ovrPolyStart                      : ovrPolyStart +   ovrPolyVertexCount - 2]
    ;ovrPolyY = intersectionsArray[ovrPolyStart + ovrPolyVertexCount : ovrPolyStart + 2*ovrPolyVertexCount - 2]
    oplot, ovrPolyX, ovrPolyY, psym = 2, symsize = 0.7, color = '0000FF'x, linestyle = 1
    ovrPolyStart += 2*ovrPolyVertexCount + 1
  endfor
end


; calcPolygonIntersectionMulitple
;
; Computes the intersection for two or more polygons.
;
; Arguments:
;
;   pPolyCoords  Pointer array of i polygons, with each element pointing to a list
;                of [n_i x 2] vertices (x/y coordinates).
function calcPolygonIntersectionMulitple, pPolyCoords

  nPolys = n_elements(pPolyCoords)
  if (nPolys eq 0) then return, [-1]
  if (nPolys eq 1) then return, ptr_new(*pPolyCoords[0]) ; Return a copy of the content to avoid eventual deallocations

  poly0 = pPolyCoords[0]
  poly1 = pPolyCoords[1]

  intersection   = calcPolygonIntersectionAlt(p1x = (*poly0)[*,0], p1y = (*poly0)[*,1], p2x = (*poly1)[*,0], p2y = (*poly1)[*,1])
  nIntersections = intersection[0]

  if (nIntersections gt 0) then begin

    if (nPolys eq 2) then begin

      ; Base case: intersection between two polygons
      pIntPolys       = ptrArr(nIntersections)
      curPolygonStart = 1
      for j = 0u, nIntersections-1 do begin
        nVerticesJ   = intersection[curPolygonStart]
        xCoordsJ     = intersection[curPolygonStart+1:curPolygonStart+nVerticesJ]
        yCoordsJ     = intersection[curPolygonStart+1+nVerticesJ:curPolygonStart+nVerticesJ*2]
        pIntPolys[j] = ptr_new([[xCoordsJ], [yCoordsJ]])
        curPolygonStart += (2*nVerticesJ + 1)
      endfor
      return, pIntPolys

    endif else begin

      ; Recursion case: for each resulting intersection polygon, make the 
      ; recursive call with the remaining polygons from the input list
      pIntPolysJK      = ptrArr(1)
      curPolygonStartJ = 1
      for j = 1, nIntersections do begin
        nVerticesJ = intersection[curPolygonStartJ]
        xCoordsJ   = intersection[curPolygonStartJ+1:curPolygonStartJ+nVerticesJ]
        yCoordsJ   = intersection[curPolygonStartJ+1+nVerticesJ:curPolygonStartJ+nVerticesJ*2]
        pIntPolysK = calcPolygonIntersectionMulitple(pPolyCoords[2:*])
        whIntValid = where(ptr_valid(pIntPolysK) eq 1, countIntValid)
        if (countIntValid eq 0) $
        then return, ptrArr(1) $
        else begin
          for k = 0, countIntValid-1 do begin
            intersectionJK = calcPolygonIntersectionAlt(p1x = xCoordsJ, p1y = yCoordsJ, p2x = (*pIntPolysK[whIntValid[k]])[*,0], p2y = (*pIntPolysK[whIntValid[k]])[*,1])
            ptr_free, pIntPolysK[whIntValid[k]]
            nIntersectionsJK = intersectionJK[0]
            if (nIntersectionsJK gt 0) then begin
              startPolyJK = 1
              for jk = 1, nIntersectionsJK do begin
                nVerticesJK  = intersectionJK[startPolyJK]
                xCoordsJK    = intersectionJK[startPolyJK+1:startPolyJK+nVerticesJK]
                yCoordsJK    = intersectionJK[startPolyJK+1+nVerticesJK:startPolyJK+nVerticesJK*2]
                pIntPolysJK  = [pIntPolysJK, ptr_new([[xCoordsJK], [yCoordsJK]])]
                startPolyJK += (2*nVerticesJK + 1)
              endfor
            endif
          endfor
        endelse
        curPolygonStartJ += (2*nVerticesJ + 1)
      endfor
      return, n_elements(pIntPolysJK) gt 1 ? pIntPolysJK[1:*] : pIntPolysJK
    endelse
  endif
  return, ptrArr(1)
end


; calcPolygonIntersectionMulitpleWithPlot
;
; Wrapper method for calcPolygonIntersection with a plotting function to ease visualization, if needed.
pro calcPolygonIntersectionMulitpleWithPlot, pPolys, pIntersections = pIntersections
  nPolys = n_elements(pPolys)
  if (nPolys lt 2) then return
  xMin = min((*pPolys[0])[*,0], max = xMax)
  yMin = min((*pPolys[0])[*,1], max = yMax)
  for i = 0u, nPolys-1 do begin
    xMin <= min((*pPolys[i])[*,0], max = xMaxTmp)
    yMin <= min((*pPolys[i])[*,1], max = yMaxTmp)
    xMax >= xMaxTmp
    yMax >= yMaxTmp
  endfor
  dx = xMax - xMin
  dy = yMax - yMin
  aspectRatio = 1.0 * dx / dy
  xSz = aspectRatio lt 1.0 ? 700 : 700 * aspectRatio
  ySz = aspectRatio lt 1.0 ? 700 * aspectRatio : 700
  margin = 1.0
  window, xSize = xSz, ySize = ySz
  plot, (*pPolys[0])[*,0], (*pPolys[0])[*,1], xRange = [xMin - margin, xMax + margin], yRange = [yMin - margin, yMax + margin]
  for i = 1u, nPolys-1 do oplot, (*pPolys[i])[*,0], (*pPolys[i])[*,1]
  pIntersections = calcPolygonIntersectionMulitple(pPolys)
  if ~ptr_valid(pIntersections[0]) then return
  for i = 0u, n_elements(pIntersections)-1 do oplot, (*pIntersections[i])[*,0], (*pIntersections[i])[*,1], color = '0000ff'x
end


pro calcPolygonIntersectionMulitple_test
  tri1x = [0.0, 3.0, 0.0, 0.0]
  tri1y = [0.0, 0.0, 2.0, 0.0] + 1.0
  tri2x = [1.5, 2.0, 2.5, 1.5]
  tri2y = [5.0, -1.0, 5.0, 5.0] + 1.0
  tri3x = [2.0, 5.0, 2.0, 2.0]
  tri3y = [0.0, 0.0, 2.0, 0.0] + 1.0
;  window, xsize = 600, ysize = 600
;  plot, tri1x, tri1y, xrange = [-1.0, 5.0], yrange = [0.0, 7.0]
;  oplot, tri2x, tri2y
;  oplot, tri3x, tri3y
  pTris = ptrArr(3)
  pTris[0] = ptr_new([[tri1x], [tri1y]])
  pTris[1] = ptr_new([[tri2x], [tri2y]])
  pTris[2] = ptr_new([[tri3x], [tri3y]])
  calcPolygonIntersectionMulitpleWithPlot, pTris, pIntersections = pm
  for i = 0, n_elements(pm)-1 do ptr_free, pm[i]
end


; TODO JuanEdo / JJ este metodo no funciona!! > ver DLL
; output is a 1D array with x,y coords plus some extra information...
; output[0] count of the resulting intersection polygons
; output[1] number of vertices of the first polygon p1, namely n1
; output[2, 4, ... , 2 + n1 -1] x coords of the n1 vertices of p1
; output[2*n1+2] number of vertices of the second polygon p2, namely n2
; The polygons must be defined in cw order, and the last point must be the same as the first.
;
; DEPENDENCIES
; - DLL for the imlpemented polygon utility functions.
;
function calcPolygonUnion, p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, fVerbose = fVerbose, fCheckCollinearSegments = fCheckCollinearSegments

  dllLoc  = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
  ;dllLoc = 'C:\Users\Juan Edo\IDLWorkspace71\SCIAN_Soft\_dll\x64\polygon_utils_tmp.dll'
  funName = keyword_set(fCheckCollinearSegments) ? 'calcUnionWithIntegrityChecking' : 'calc_union'
  if keyword_set(fCheckCollinearSegments) then begin
    polygonCheckAndRemoveCollinearSegments, p1x, p1y, pxOut = p1x_, pyOut = p1y_
    polygonCheckAndRemoveCollinearSegments, p2x, p2y, pxOut = p2x_, pyOut = p2y_
    p1x = p1x_ & p1y = p1y_
    p2x = p2x_ & p2y = p2y_
  endif
  testAndCorrectCWOrientation, p1x, p1y, XOUT = p1x_, YOUT = p1y_;, /FCCW
  testAndCorrectCWOrientation, p2x, p2y, XOUT = p2x_, YOUT = p2y_;, /FCCW
  ; XXX problem with dynamic memory in the DLL... making some workarounds here
  deltaoutputsize = 0
  factor = 1.5
  output = [-1.0]
  polyClose, p1x = p1x_, p1y = p1y_, /double
  polyClose, p1x = p2x_, p1y = p2y_, /double
  n1 = n_elements(p1x_)
  n2 = n_elements(p2x_)
  repeat begin
    factor += 1.
    estimated_output_size = ceil(((n1 + n2) * 2) * factor)
    if keyword_set(fVerbose) then print, 'trying with output size ', estimated_output_size
    output  = dblArr(estimated_output_size)
    nUnions = call_external(dllLoc, funName, p1x_, p1y_, fix(n1, type = 2), p2x_, p2y_, fix(n2, type = 2),$
                            output, estimated_output_size, deltaoutputsize, /unload)
  endrep until (deltaoutputsize le 0)
  if keyword_set(fVerbose) then print, 'found ', nUnions, ' intersection(s) for input polygons'
  return, trimPolygonsArray(output)
end


; TODO JuanEdo / JJ este no funciona!! > ver DLL
pro calcPolygonUnion_testCheckAndRemoveCollinearSegments
  p1x = [0.0, 0.5, 0.5, 0.5, 1.0, 1.0, 0.0, 0.0]
  p1y = [0.0, 0.0, 0.5, 0.0, 0.0, 1.0, 1.0, 0.0]
  p2x = [1.0, 1.5, 1.5, 1.0, 1.0]
  p2y = [0.5, 0.5, 1.5, 1.5, 0.5]
  stop
  uChecked = calcPolygonUnion(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FCHECKCOLLINEARSEGMENTS)
  print, uChecked
  stop
end


; Note that the order of the vertices for the DLL library (CW) is the reverse of the IDL order (CCW).
; type of the arrays must be double
pro testdllpolyunion
  p1x = [0.0, 0.0, 1.0, 1.0, 0.0]
  p1y = [0.0, 1.0, 1.0, 0.0, 0.0]
  p2x = [0.5, 0.5, 1.5, 1.5, 0.5]
  p2y = [0.5, 1.5, 1.5, 0.5, 0.5]
  groundTruthPolyCount = 1
  groundTruthPolyNvertices = [5]
  groundTruthVerticesX = [0.5, 1.0, 1.0, 0.5, 0.5]
  groundTruthVerticesX = [1.0, 1.0, 0.5, 0.5, 1.0]
  o = calcPolygonUnion(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FVERBOSE)
  print, 'test output ', o
end


; Note that the order of the vertices for the DLL library (CW) is the reverse of the IDL order (CCW).
; type of the arrays must be double
pro testdllPolyInt, fAlt = fAlt, fGetIndices = fGetIndices
  p1x = [0.0, 0.0, 1.0, 1.0, 0.0]
  p1y = [0.0, 1.0, 1.0, 0.0, 0.0]
  p2x = [0.5, 0.5, 1.5, 1.5, 0.5]
  p2y = [0.5, 1.5, 1.5, 0.5, 0.5]
  groundTruthPolyCount = 1
  groundTruthPolyNvertices = [5]
  groundTruthVerticesX = [0.5, 1.0, 1.0, 0.5, 0.5]
  groundTruthVerticesY = [1.0, 1.0, 0.5, 0.5, 1.0]
  print, 'winding p1: ', calcPolygonWinding(p1x, p1y)
  print, 'winding p2: ', calcPolygonWinding(p2x, p2y)
  if ~keyword_set(fGetIndices) $
  then o = keyword_set(fAlt) ? calcPolygonIntersectionAlt(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y) $
                             : calcPolygonIntersection(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y) $
  else o = calcPolygonIntersectionWithPolyIndices(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o
end


pro testdllPolyInt2, fAlt = fAlt, fGetIndices = fGetIndices
  p1x = [0.0, 0.0, 1.0, 1.0, 0.0]
  p1y = [0.0, 1.0, 1.0, 0.0, 0.0]
  p2x = [0.0, 0.0, 0.8, 0.8, 0.7, 0.7, 0.3, 0.3, 0.0]
  p2y = [0.0, 2.0, 2.0, 0.0, 0.0, 1.2, 1.2, 0.0, 0.0]
  print, 'double... ', double(p2x), double(p2y)
  print, 'winding p1: ', calcPolygonWinding(p1x, p1y)
  print, 'winding p2: ', calcPolygonWinding(p2x, p2y)
  if ~keyword_set(fGetIndices) $
  then o = keyword_set(fAlt) ? calcPolygonIntersectionAlt(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y) $
                             : calcPolygonIntersection(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y) $
  else o = calcPolygonIntersectionWithPolyIndices(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o
end


; Given these polygons, the intersection is a line.
; When using boost 1.54.0 or Clipper 5.1.6 (or later) this returns no intersections.
pro testdllPolyIntTricky, fAlt = fAlt
  p1x = [0.0, 3.0, 3.0, 0.0, 0.0]
  p1y = [4.0, 4.0, 5.0, 5.0, 4.0]
  p2x = [0.0, 3.0, 3.0, 1.0, 1.0, 0.0, 0.0]
  p2y = [0.0, 0.0, 4.0, 4.0, 3.0, 3.0, 0.0]
  print, 'winding p1: ', calcPolygonWinding(p1x, p1y)
  print, 'winding p2: ', calcPolygonWinding(p2x, p2y)
  o = keyword_set(fAlt) ? calcPolygonIntersectionAlt(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y) $
                        : calcPolygonIntersection(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o
end


; Note that the order of the vertices for the DLL library (CW) is the reverse of the IDL order (CCW).
; type of the arrays must be double
; mcerda
pro testdllPolyInt3, fAlt = fAlt, fGetIndices = fGetIndices

  ; ex. 0
  p1x = [-1.0,  0.5, -1.0]
  p1y = [-1.0,  0.0,  1.0]
  p2x = [ 0.0,  1.0,  0.0]
  p2y = [-1.0,  0.0,  1.0]

  print, 'winding p1: ', calcPolygonWinding(p1x, p1y)
  print, 'winding p2: ', calcPolygonWinding(p2x, p2y)

  fUseAltIndexCalc = 1 ;0 boost - 1 clipper
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  ; ex. A
  p1x = [2.0, 3.0, 4.0]
  p1y = [1.0, 5.0, 3.0]
  p2x = [0.0, 0.0, 4.0, 4.0]
  p2y = [0.0, 4.0, 0.0, 4.0]
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  ; ex. B
  p1x = [2.0, 5.0, 2.0, 5.0]
  p1y = [2.0, 2.0, 5.0, 5.0]
  p2x = [3.0, 3.0, 8.0, 8.0]
  p2y = [3.0, 6.0, 3.0, 6.0]
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  ; ex. C
  p1x = [0.0, 0.0, 4.0]
  p1y = [0.0, 3.0, 3.0]
  p2x = [2.0, 3.0, 4.0]
  p2y = [0.0, 4.0, 1.0]
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  ; ex. D
  p1x = [1.0, 3.0, 1.0]
  p1y = [1.0, 1.0, 4.0]
  p2x = [2.0, 2.0, 4.0, 4.0]
  p2y = [2.0, 4.0, 2.0, 4.0]
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  ; ex. E
  p1x = [1, 0, 3, 5]
  p1y = [1, 2, 3, 2]
  p2x = [3, 2, 5]
  p2y = [1, 5, 4]
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  ; ex. F1
;  p1x = []
;  p1y = []
;  p2x = []
;  p2y = []
;  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
;  print, 'found ', o[0], ' intersection(s) for input polygons'
;  print, 'test output ', o

  ; ex. F2
;  p1x = []
;  p1y = []
;  p2x = []
;  p2y = []
;  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
;  print, 'found ', o[0], ' intersection(s) for input polygons'
;  print, 'test output ', o

  ; ex. G
  p1x = [1, 1.5, 3, 2, 4]
  p1y = [3, 1.5, 1, 4, 2]
  p2x = [3, 4, 6]
  p2y = [2, 5, 3]
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  ; ex. H
  p1x = [1, 1, 4, 2, 4]
  p1y = [1, 3, 2, 2, 1]
  p2x = [3, 3, 4]
  p2y = [0.5, 5, 5]
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  ; ex. I
  p1x = [2, 2, 5, 3, 5]
  p1y = [2, 5, 3, 3, 2]
  p2x = [1.5, 4.5, 5.5]
  p2y = [1, 1, 3]
  o = calcPolygonIntersectionWithPolyIndices2(p1x = p1x, p1y = p1y, p2x = p2x, p2y = p2y, /FPLOT, /FVERBOSE, fUseAltIndexCalc = fUseAltIndexCalc)
  print, 'found ', o[0], ' intersection(s) for input polygons'
  print, 'test output ', o

  print, 'testdllPolyInt3: Done'
end


; TODO Victor CORNER - intersect polylines
function intersectPairedPolylines, polyLines

  nPolylines = n_elements(polyLines)
  if (nPolylines lt 2) then return, [-1.0, -1.0]

  nIntersections = nPolylines * (nPolylines-1) / 2
  intersectionsX = fltArr(nIntersections)
  intersectionsY = fltArr(nIntersections)
  counter = 0

  for i = 0, nPolylines-2 do begin

    xIntersect = 0
    yIntersect = 0
    nIntersect = 0
    nSegmentsPolyLineI = n_elements(*polyLines[i]) / 2

    for j = i+1, nPolylines-1 do begin

      nSegmentsPolyLineJ = n_elements(*polyLines[j]) / 2
; x...      (*polyLines[i]) [0,*]
      for k = 0, nSegmentsPolyLineI-2 do begin
        xI1 = (*polyLines[i]) [0, k]
        xI2 = (*polyLines[i]) [0, k+1]
        yI1 = (*polyLines[i]) [1, k]
        yI2 = (*polyLines[i]) [1, k+1]
        for l = 0, nSegmentsPolyLineJ-2 do begin
          xJ1 = (*polyLines[j]) [0, l]
          xJ2 = (*polyLines[j]) [0, l+1]
          yJ1 = (*polyLines[j]) [1, l]
          yJ2 = (*polyLines[j]) [1, l+1]

          s1_x = xI2 - xI1
          s1_y = yI2 - yI1
          s2_x = xJ2 - xJ1
          s2_y = yJ2 - yJ1

          s = (-s1_y * (xI1 - xJ1) + s1_x *(yI1 - yJ1)) / (-s2_x * s1_y + s1_x * s2_y)
          t = ( s2_x * (yI1 - yJ1) - s2_y *(xI1 - xJ1)) / (-s2_x * s1_y + s1_x * s2_y)

          if (s gt 0) and (s lt 1) and (t gt 0) and (t lt 1) then begin
            xIntersect = xIntersect + xI1 + t * s1_x
            yIntersect = yIntersect + yI1 + t * s1_y
            nIntersect += 1
          endif
        endfor
      endfor

      intersectionsX[counter] = xIntersect / nIntersect
      intersectionsY[counter] = yIntersect / nIntersect
      counter += 1
     ;d = distanceFromPointToSegment()
     ;d = distanceBetween2DSegments()
    endfor
  endfor

  finalIntersectionX = 0.0
  finalIntersectionY = 0.0
  finalIntersectionN = 0

  for i = 0u, nIntersections-1 do begin
    if (intersectionsX[i] ne 0) and (intersectionsY[i] ne 0) then begin
      finalIntersectionX = finalIntersectionX + intersectionsX[i]
      finalIntersectionY = finalIntersectionY + intersectionsY[i]
      finalIntersectionN += 1
    endif
  endfor

  finalIntersectionX /= finalIntersectionN
  finalIntersectionY /= finalIntersectionN

  return, [finalIntersectionX, finalIntersectionY]
end


; TODO JJW method under construction... AAB corners.
function intersectPairedPolylinesAABwrapper, pAABpolyLines

  npLines = n_elements(pAABpolyLines)
  if (npLines eq 0) then return, -1

  nLines = npLines * 2
  polyLines = ptrArr(nLines)
  for i = 0u, npLines-1 do begin

    polyLinePair = *pAABpolyLines[i]
    line1Xcoords = polyLinePair[0,0]
    line1Ycoords = polyLinePair[1,0]
    line2Xcoords = polyLinePair[0,1]
    line2Ycoords = polyLinePair[1,1]

    n1 = n_elements(*line1Xcoords)
    line1xy = dblArr(n1 * 2)
    for j = 0u, n1-1 do begin
      line1xy[2*j] = (*line1Xcoords)[j]
      line1xy[2*j+1] = (*line1Ycoords)[j]
    endfor
    polyLines[2*i] = ptr_new(line1xy)

    n2 = n_elements(*line2Xcoords)
    line2xy = dblArr(n2 * 2)
    for j = 0u, n2-1 do begin
      line2xy[2*j] = (*line2Xcoords)[j]
      line2xy[2*j+1] = (*line2Ycoords)[j]
    endfor
    polyLines[2*i + 1] = ptr_new(line2xy)

  endfor

  return, intersectPairedPolylines(polyLines)
end


function intersectPairedPolylinesVichoCrap, pAABpolyLines
  npLines = n_elements(pAABpolyLines)
  if (npLines eq 0) then return, -1

  nLines = npLines * 2
  polyLines = ptrArr(nLines)
  boxXmin = 0
  boxXmax = 0
  boxYmin = 0
  boxYmax = 0
  for i = 0u, npLines-1 do begin
    polyLinePair = *pAABpolyLines[i]
    line1Xcoords = polyLinePair[0,0]
    line1Ycoords = polyLinePair[1,0]
    line2Xcoords = polyLinePair[0,1]
    line2Ycoords = polyLinePair[1,1]
    maxX1 = max(*line1Xcoords, min = minX1)
    maxY1 = max(*line1Ycoords, min = minY1)
    maxX2 = max(*line2Xcoords, min = minX2)
    maxY2 = max(*line2Ycoords, min = minY2)
    boxXmax = (i eq 0) ? maxX1 > maxX2 : boxXmax > (maxX1 > maxX2)
    boxYmax = (i eq 0) ? maxY1 > maxY2 : boxYmax > (maxY1 > maxY2)
    boxXmin = (i eq 0) ? minX1 < minX2 : boxXmin < (minX1 < minX2)
    boxYmin = (i eq 0) ? minY1 < minY2 : boxYmin < (minY1 < minY2)
  endfor
  boxXmin -= 1
  boxXmax += 1
  boxYmin -= 1
  boxYmax += 1
  boxXsize = ceil(boxXmax) - floor(boxXmin)
  boxYsize = ceil(boxYmax) - floor(boxYmin)
  boxImg   = bytArr(boxXsize, boxYsize)

  for i = 0u, npLines-1 do begin
    polyLinePair = *pAABpolyLines[i]
    line1Xcoords = round(*polyLinePair[0,0]) - boxXmin
    line1Ycoords = round(*polyLinePair[1,0]) - boxYmin
    n1 = n_elements(line1Xcoords)
    for j = 0u, n1-1 do boxImg[line1Xcoords[j], line1Ycoords[j]] = 255
    line2Xcoords = round(*polyLinePair[0,1]) - boxXmin
    line2Ycoords = round(*polyLinePair[1,1]) - boxYmin
    n2 = n_elements(line2Xcoords)
    for j = 0u, n2-1 do boxImg[line2Xcoords[j], line2Ycoords[j]] = 255
  endfor
  whLines = where(boxImg eq 255)
  distMap = morph_distance(boxImg, NEIGHBOR_SAMPLING = 3, /background)
  distMap[whLines] = -1
  distMapCrop = distMap[1:boxXsize-2,1:boxYsize-2]
  stop; intX = -1 & intY = -1
  return, [intX, intY]
end


;pro intersectPairedPolylines_test
;  p1x = []
;  p1y = []
;  p2x = []
;  p2y = []
;  p3x = []
;  p3y = []
;  p4x = []
;  p4y = []
;  p5x = []
;  p5y = []
;  p = ptrArr(5)
;  p[0] = ptr_new([[p1x], [p1y]])
;  p[1] = ptr_new([[p2x], [p2y]])
;  p[2] = ptr_new([[p3x], [p3y]])
;  p[3] = ptr_new([[p4x], [p4y]])
;  p[4] = ptr_new([[p5x], [p5y]])
;end


;distanceFromPointToSegment
; endpointCode: 0->point lies within the segment
;               1->the closest point in the segment is the first
;               2->the closest point in the segment is the second
;               3->other (not used currently).
; The resulting type will be depending on the type of the input parameters (double/float).
;
; fSquareDist Flag to compute square distances (avoiding square roots computations saves time in long runs).
; fZeroLen    Output flag indicating whether the input segment has length equal to zero. 
function distanceFromPointToSegment, px_, py_, s1x_, s1y_, s2x_, s2y_, fSquareDist = fSquareDist, endpointCode = endpointCode, fZeroLen = fZeroLen, closestEndpoint = closestEndpoint

  roiType = size(px_,/TYPE)
  machineArithmetic = roiType eq 5 ? machAr(/DOUBLE) : machAr()

  if ((roiType ne 4) and (roiType ne 5)) then begin
    px = double(px_)
    py = double(py_)
    s1x = double(s1x_)
    s1y = double(s1y_)
    s2x = double(s2x_)
    s2y = double(s2y_)
  endif else begin
    px = px_
    py = py_
    s1x = s1x_
    s1y = s1y_
    s2x = s2x_
    s2y = s2y_
  endelse

  xDelta = s2x - s1x
  yDelta = s2y - s1y

  if ((xDelta eq 0) and (yDelta eq 0)) then begin
    ;print, "Error: segment points are the same. Distance from point to point will be computed..."
    fZeroLen = 1b
    dSqr = (px - s1x)*(px - s1x) + (py - s1y)*(py - s1y)
    endpointCode = (dSqr eq 0.0) ? 0 : 3
    closestEndpoint = 0
    dSqr = roiType eq 5 ? dSqr : float(dSqr)
    return, keyword_set(fSquareDist) ? dSqr : sqrt(dSqr)
  endif else fZeroLen = 0b

  lineMagSqr = (s2x - s1x)*(s2x - s1x) + (s2y - s1y)*(s2y - s1y)
  u = (((px - s1x) * (s2x - s1x)) + ((py - s1y) * (s2y - s1y))) / lineMagSqr

  ;Closest point does not fall within the segment. Distance from point to the closest segment endpoint will be returned.
  if (u lt 0.0) or (u gt 1.0) then begin
    endpointCode = 3
    dps1Sq = (px - s1x)*(px - s1x) + (py - s1y)*(py - s1y)
    dps2Sq = (px - s2x)*(px - s2x) + (py - s2y)*(py - s2y)
    if (dps1Sq lt dps2Sq) $
    then begin
      closestEndpoint = 1
      dps1Sq = roiType eq 5 ? dps1Sq : float(dps1Sq)
      return, keyword_set(fSquareDist) ? dps1Sq : sqrt(dps1Sq)
    endif else begin
      closestEndpoint = 2
      dps2Sq = roiType eq 5 ? dps2Sq : float(dps2Sq)
      return, keyword_set(fSquareDist) ? dps2Sq : sqrt(dps2Sq)
    endelse
  endif

  ;Closest point does fall within the segment. Compute the segment's point that is closest.
  intersection_x = s1x + u * (s2x - s1x)
  intersection_y = s1y + u * (s2y - s1y)

  dps1Sq = (px - intersection_x)*(px - intersection_x) + (py - intersection_y)*(py - intersection_y)
  if (dps1Sq eq 0) then begin
    case 1 of
      (px eq s1x) and (py eq s1y): endpointCode = 1
      (px eq s2x) and (py eq s2y): endpointCode = 2
      else: endpointCode = 0
    endcase
  endif else endpointCode = 3

  if arg_present(closestEndpoint) then begin
    dps1Sq = (px - s1x)*(px - s1x) + (py - s1y)*(py - s1y)
    dps2Sq = (px - s2x)*(px - s2x) + (py - s2y)*(py - s2y)
    closestEndpoint = dps1Sq eq dps2Sq ? 0 : (dps1Sq lt dps2Sq) ? 1 : 2
  endif
  dps1Sq = roiType eq 5 ? dps1Sq : float(dps1Sq)
  return, keyword_set(fSquareDist) ? dps1Sq : sqrt(dps1Sq)
end


pro distanceFromSegmentToSegment_Test
  print, 'Not yet'
end


pro distanceFromPointToSegment_Test

  px = 120.0d
  py = 12.0d

  s1x = 100.0d
  s1y = 12.0d
  s2x = 140.0d
  s2y = 12.0d
  idx = -1
  dist = distanceFromPointToSegment(px, py, s1x, s1y, s2x, s2y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = distanceFromPointToSegment(px, py, s2x, s2y, s1x, s1y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  ; A couple tests for special cases
  ;print, 'Test for a segment of 0-length with the same endpoints, but different from the input point'
  dist = distanceFromPointToSegment(px, py, s1x, s1y, s1x, s1y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  ;print, 'Test for a segment of 0-length with the same endpoints as the input point'
  dist = distanceFromPointToSegment(px, py, px, py, px, py, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  ;print, 'Test for very small and close segment'
  dist = distanceFromPointToSegment(px, py, px+ 1e-12, py+ 1e-12, px-1e-12, py- 1e-12, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 100.0d
  s1y = 12.0d
  s2x = 100.0d
  s2y = 20.0d
  idx = -1
  dist = distanceFromPointToSegment(px, py, s1x, s1y, s2x, s2y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = distanceFromPointToSegment(px, py, s2x, s2y, s1x, s1y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 100.0d
  s1y = 10.0d
  s2x = 120.0d
  s2y = 12.0d
  idx = -1
  dist = distanceFromPointToSegment(px, py, s1x, s1y, s2x, s2y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = distanceFromPointToSegment(px, py, s2x, s2y, s1x, s1y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 100.0d
  s1y = 10.0d
  s2x = 140.0d
  s2y = 10.0d
  idx = -1
  dist = distanceFromPointToSegment(px, py, s1x, s1y, s2x, s2y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = distanceFromPointToSegment(px, py, s2x, s2y, s1x, s1y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 0.0d
  s1y = 10.0d
  s2x = 100.0d
  s2y = 10.0d
  dist = distanceFromPointToSegment(px, py, s1x, s1y, s2x, s2y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = distanceFromPointToSegment(px, py, s2x, s2y, s1x, s1y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 1.0d
  s1y = 1.0d
  s2x = 10.0d
  s2y = 10.0d
  dist = distanceFromPointToSegment(px, py, s1x, s1y, s2x, s2y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = distanceFromPointToSegment(px, py, s2x, s2y, s1x, s1y, endpointCode = idx, /FSQUAREDIST)
  ;print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

end


pro distanceFromPointToSegmentDLL_Test

  dllLoc = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
  px = 120.0d
  py = 12.0d

  s1x = 100.0d
  s1y = 12.0d
  s2x = 140.0d
  s2y = 12.0d
  idx = -1
  zeroLenStatus = -1
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s1x, s1y, s2x, s2y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s2x, s2y, s1x, s1y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  ; A couple tests for special cases
;  print, 'Test for a segment of 0-length with the same endpoints, but different from the input point'
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s1x, s1y, s1x, s1y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx, ' - zeroLenStatus is ', zeroLenStatus
;  print, 'Test for a segment of 0-length with the same endpoints as the input point'
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, px, py, px, py, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx, ' - zeroLenStatus is ', zeroLenStatus
;  print, 'Test for very small and close segment'
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, px+ 1e-12, py+ 1e-12, px-1e-12, py- 1e-12, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx, ' - zeroLenStatus is ', zeroLenStatus

  s1x = 100.0d
  s1y = 12.0d
  s2x = 100.0d
  s2y = 20.0d
  idx = -1
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s1x, s1y, s2x, s2y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s2x, s2y, s1x, s1y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 100.0d
  s1y = 10.0d
  s2x = 120.0d
  s2y = 12.0d
  idx = -1
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s1x, s1y, s2x, s2y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s2x, s2y, s1x, s1y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 100.0d
  s1y = 10.0d
  s2x = 140.0d
  s2y = 10.0d
  idx = -1
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s1x, s1y, s2x, s2y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s2x, s2y, s1x, s1y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 0.0d
  s1y = 10.0d
  s2x = 100.0d
  s2y = 10.0d
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s1x, s1y, s2x, s2y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s2x, s2y, s1x, s1y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

  s1x = 1.0d
  s1y = 1.0d
  s2x = 10.0d
  s2y = 10.0d
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s1x, s1y, s2x, s2y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx
  dist = call_external(dllLoc, 'distanceFromPointToSegment', px, py, s2x, s2y, s1x, s1y, idx, zeroLenStatus, RETURN_TYPE=5)
;  print, 'Computed distance is ', dist, ' - segment vertex index is ', idx

end


pro speedTestDistancePointToSegment, nRuns = nRuns

  nRuns = keyword_set(nRuns) ? nRuns > 1 : 10000
  t1    = sysTime(1)
  for i = 1L, nRuns do distanceFromPointToSegmentDLL_Test
  tDll  = sysTime(1) - t1

  t1    = sysTime(1)
  for i = 1L, nRuns do distanceFromPointToSegment_Test
  tIDL  = sysTime(1) - t1
  print, 'speedTestDistancePointToSegment: ', nRuns, ' test runs. DLL time : ', tDll, ' - IDL time : ', tIDL, ' - DLL/IDL time ratio: ', string(1.0 * tDll/tIDL)
end


function isVertexInProximityRun, pDistanceMatrix, vertexIndex, RunStart, RunEnd, thresholdDist
  nSegments = size(*pDistanceMatrix, /DIMENSIONS)
  if (runStart gt runEnd) $
  then nSegmentsToCheck = nSegments[1] - runStart + runend + 1 $
  else nSegmentsToCheck = runEnd - runStart + 1
  testPos = runStart
  for i = 0, nSegmentsToCheck-1 do begin
    if (*pDistanceMatrix)[vertexIndex, testPos] le thresholdDist then return, 1b
    testPos += 1
    if (testPos ge nSegments[1]) then testPos = 0
  endfor
  return, 0b 
end


; Requires closed polygons
function findRunsInDistMatrix2a, p1x, p1y, p2x, p2y, pDistance12, start1, start2, thresholdDist, runList1, runList2, fVerbose = fVerbose, $
                                 fReturnFirst = fReturnFirst, fBackwards = fBackwards, end1 = end1, end2 = end2, pEndpoints12 = pEndpoints12
  lookupPos1 = long(start1)
  lookupPos2 = long(start2)
  runList1 = [-1]
  runList2 = [-1]
  nRuns  = 0u

  n1 = n_elements(p1x) - 1 ; since polygons are closed...
  n2 = n_elements(p2x) - 1

  if (n_elements(end1) eq 1) then n1a = end1 - start1 + ((end1 ge start1) ? 0 : n1) else n1a = n1
  if (n_elements(end2) eq 1) then n2a = end2 - start2 + ((end2 ge start2) ? 0 : n2) else n2a = n2
  if keyword_set(fVerbose) $
  then print, 'n1 ', n1, ' n2 ', n2, ' start1 ', start1, ((n_elements(end1) eq 1) ? (' end1 ' + string(end1)) : ''), $
                                     ' start2 ', start2, ((n_elements(end2) eq 1) ? (' end2 ' + string(end2)) : ''), ' n1a ', n1a, ' n2a ', n2a
  checkCount1 = 1u
  checkCount2 = 1u
  fNewRun = 1b

  while (checkCount1 le n1a) do begin

    fMatch = ((*pDistance12)[lookupPos1,lookupPos2] le thresholdDist)

    if (fMatch eq 1) and (checkCount2 le n2a) then begin

      if (fNewRun eq 1) then begin
        fNewRun  = 0b
        run1curr = [lookupPos1, lookupPos1]
        run2curr = [lookupPos2, lookupPos2]
      endif

      while (checkCount2 le n2a) and (fMatch eq 1) do begin

        if keyword_set(fBackwards) $
        then lookupPos2 = lookupPos2 ge n2-1 ? 0 : lookupPos2+1 $
        else lookupPos2 = lookupPos2 gt 0 ? lookupPos2-1 : n2-1
        checkCount2 += 1

        fMatch = ((*pDistance12)[lookupPos1,lookupPos2] le thresholdDist)
        if (fMatch eq 1) and ~fNewRun then run2curr[0] = lookupPos2
      endwhile

      if keyword_set(fBackwards) $
      then lookupPos1 = lookupPos1 gt 0 ? lookupPos1-1 : n1-1 $
      else lookupPos1 = lookupPos1 ge n1-1 ? 0 : lookupPos1+1
      checkCount1 += 1
      if (checkCount1 gt n1a) then begin
        runList1 = [runList1, run1curr]
        runList2 = [runList2, run2curr]
        fNewRun = 1b
      endif else begin
        fMatch = ((*pDistance12)[lookupPos1,lookupPos2] le thresholdDist)
        if (fMatch eq 1) then begin
          run1curr[1] = lookupPos1
          run2curr[0] = lookupPos2
          if keyword_set(fVerbose) then print, 'run1curr', run1curr
          if keyword_set(fVerbose) then print, 'run2curr', run2curr
        endif else begin
          fMatch = isVertexInProximityRun(pDistance12, lookupPos1, run2curr[0], run2curr[1], thresholdDist)
          if (fMatch eq 1) $
          then run1curr[1] = lookupPos1 $
          else begin
            fNewRun = 1b
            runList1 = [runList1, run1curr]
            runList2 = [runList2, run2curr]
            if keyword_set(fReturnFirst) then goto, x
          endelse
        endelse
      endelse

    endif else begin

      if ~fNewRun then begin
        runList1 = [runList1, run1curr]
        runList2 = [runList2, run2curr]
        fNewRun = 1b
      endif
      if keyword_set(fBackwards) $
      then lookupPos2 = lookupPos2 ge n2-1 ? 0 : lookupPos2+1 $
      else lookupPos2 = lookupPos2 gt 0 ? lookupPos2-1 : n2-1
      checkCount2 += 1
      if (checkCount2 ge n2a) then begin
        if keyword_set(fBackwards) $
        then lookupPos1 = lookupPos1 gt 0 ? lookupPos1-1 : n1-1 $
        else lookupPos1 = lookupPos1 ge n1-1 ? 0 : lookupPos1+1
        checkCount1 += 1
        checkCount2 = 0
        lookupPos2 = start2
      endif
    endelse
  endwhile
x:nRuns = n_elements(runList1) / 2
  if nRuns gt 0 then begin
    runList1 = runList1[1:*]
    runList2 = runList2[1:*]
  endif
;  runList1a = runList1
;  runList1b = runList1
;  print, runList1
;  if (n_elements(runList1) gt 2) then begin
;  nVer = (n_elements(runList1)/2)-2
;  cont = 0
;    for i=0, nVer do begin
;      if (runList1[2*cont + 1] eq (runList1[2*cont + 2] - 1)) then begin
;        runList1 = [runList1[0:2*cont], runList1[2*cont + 3: *]]
;      endif else begin
;        cont += 1
;      endelse
;    endfor
;  endif
;  print, runList1
;  indicesList1 = runsToIndices(runList1, n1)
;  indicesList1 = indicesList1[sort(indicesList1)]
;  indicesList1 = indicesList1[uniq(indicesList1)]
;  runList1b = makeRunFromCorrelativeValues(indicesList1)
;  nRuns = n_elements(runList1) / 2
;  if ((nRuns gt 1) and (runList1[2*nRuns-1] eq (n1-1)) and (runList1[0] eq 0)) then $
;    runList1 = [runList1[2:2*nRuns-2], runList1[1]]
;  print, runList1
  runList1 = fixRun(runList1, n1, /FVERBOSE)
  runList2 = fixRun(runList2, n2, /FVERBOSE)
  
  nRuns = n_elements(runList1) / 2
  return, nRuns
end


pro findRunsInDistMatrix2a_testPlot, p1x, p1y, p2x, p2y, runList1, runList2, winID = winID, testID = testID
  winTitle = 'C_sAABContainer_Test: Test' + (n_elements(testID) gt 0 ? string(testID) : '')
  makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = winTitle, winId = winID, sizeFactorWin = 20, linestyle = 0, psym = 0
  nRuns = n_elements(runList1) / 2
  n1 = n_elements(p1x)
  n2 = n_elements(p2x)
  for runPos = 0, nRuns-1 do begin
    vertices1 = singleRunToIndices(runList1[2*runPos], (runList1[2*runPos+1]+1) mod n1, n1)
    vertices2 = singleRunToIndices(runList2[2*runPos], (runList2[2*runPos+1]+1) mod n2, n2)
    oPlot, p1x[vertices1], p1y[vertices1], pSym = 4, color='FF0000'x
    oPlot, p1x[vertices1], p1y[vertices1], pSym = 0, color='FF0000'x, linestyle = 0
    oPlot, p2x[vertices2], p2y[vertices2], pSym = 4, color='0000FF'x
    oPlot, p2x[vertices2], p2y[vertices2], pSym = 0, color='0000FF'x, linestyle = 0
    stop
  endfor
end


pro findRunsInDistMatrix2a_test
  p1x_ = double([0, 0, 0, 0, -1, 0])
  p1y_ = double([0, 1, 2, 3, 1.5, 0])
  p2x_ = double([1, 1, 1, 1, 1, 2, 1])
  p2y_ = double([0, 1, 2, 3, 4, 2, 0])

  testAndCorrectCWOrientation, p1x_, p1y_, xOut = p1x, yOut = p1y, fCCW = 1
  testAndCorrectCWOrientation, p2x_, p2y_, xOut = p2x, yOut = p2y, fCCW = 1
  print, p1x
  print, p1y
  print, p2x
  print, p2y
  thresholdDist = 1.0
  distIJ = distanceBetweenSegments(p1x, p1y, p2x, p2y, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  print, distIJ
  out = findRunsInDistMatrix2a(p1x, p1y, p2x, p2y, ptr_new(distIJ), 0, 0, thresholdDist, runList1, runList2, /fVerbose)

  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2

  f1 = 40
  f2 = 60
  offset1 = 60
  p1x = [0.0, 2.0, 2.0, 2.0, 0.0, 0.0]*f1 + offset1 
  p1y = [0.0, 0.0, 2.0, 4.0, 4.0, 0.0]*f1
  p2x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p2y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
  thresholdDist = 15
  nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1xInt = p1x
    p1yInt = p1y
    p2xInt = p2x
    p2yInt = p2y
  endif else begin
    polygonArcSample, p1x, p1y, p1xInt, p1yInt, POINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2xInt, p2yInt, POINTS=nPts, /FCLOSEOUTPUT
  endelse  

  distIJ = distanceBetweenSegments(p1xint, p1yint, p2xint, p2yint, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  ;print, distIJ
  out = findRunsInDistMatrix2a(p1xint, p1yint, p2xint, p2yint, ptr_new(distIJ), 0, 0, thresholdDist, runList1, runList2, /fVerbose)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1xint, p1yint, p2xint, p2yint, runList1, runList2, winID = 10, testID = 0

  p1x = [0.0, 2.0, 1.5, 3.2, 0.0, 0.0]*f1 + offset1
  p1y = [0.0, 0.0, 2.0, 1.9, 4.0, 0.0]*f1
  p2x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p2y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
  nPts = keyword_set(nPts) ? nPts > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1xInt = p1x
    p1yInt = p1y
    p2xInt = p2x
    p2yInt = p2y
  endif else begin
    polygonArcSample, p1x, p1y, p1xInt, p1yInt, POINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2xInt, p2yInt, POINTS=nPts, /FCLOSEOUTPUT
  endelse
  thresholdDist = 20
  distIJ = distanceBetweenSegments(p1xint, p1yint, p2xint, p2yint, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  ;print, distIJ
  out = findRunsInDistMatrix2a(p1xint, p1yint, p2xint, p2yint, ptr_new(distIJ), 0, 0, thresholdDist, runList1, runList2, /fVerbose)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1xint, p1yint, p2xint, p2yint, runList1, runList2, winID = 11, testID = 1

  offset = 5
  p1x_ = [0.0, 2.0, 1.0, 2.0, 0.0, 0.0]+offset
  p1y_ = [1.0, 2.1, 2.5, 2.9, 4.0, 1.0]
  p2x_ = [3.0,3.0,3.0,3.0,4.0,5.0,6.0,6.0,6.0,6.0,5.0,4.0,3.0]+offset
  p2y_ = [1.0,2.0,3.0,4.0,4.0,4.0,4.0,3.0,2.0,1.0,1.0,1.0,1.0]
  testAndCorrectCWOrientation, p1x_, p1y_, xOut = p1x, yOut = p1y, fCCW = 1
  testAndCorrectCWOrientation, p2x_, p2y_, xOut = p2x, yOut = p2y, fCCW = 1
  thresholdDist = 1.0
  distIJ = distanceBetweenSegments(p1x, p1y, p2x, p2y, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  print, distIJ
  out = findRunsInDistMatrix2a(p1x, p1y, p2x, p2y, ptr_new(distIJ), 0, 0, thresholdDist, runList1, runList2, /fVerbose)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1x, p1y, p2x, p2y, runList1, runList2, winID = 12, testID = 2

  offset = 5
  p1x_ = [1.0, 6.0, 1.0, 1.0]+offset
  p1y_ = [2.0, 3.5, 5.0, 2.0]
  p2x_ = [5.0,8.0,8.0,5.0,5.0]+offset
  p2y_ = [2.0,2.0,5.0,5.0,2.0]
  testAndCorrectCWOrientation, p1x_, p1y_, xOut = p1x, yOut = p1y, fCCW = 1
  testAndCorrectCWOrientation, p2x_, p2y_, xOut = p2x, yOut = p2y, fCCW = 1
  thresholdDist = 1.0
  distIJ = distanceBetweenSegments(p1x, p1y, p2x, p2y, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  print, distIJ
  out = findRunsInDistMatrix2a(p1x, p1y, p2x, p2y, ptr_new(distIJ), 0, 0, thresholdDist, runList1, runList2, /fVerbose)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1x, p1y, p2x, p2y, runList1, runList2, winID = 13, testID = 3

end


function findRunsFromCorrespondences, proxArrayIJ, runListA, runListB, maxIndexA = maxIndexA, maxIndexB = maxIndexB
  indicesListA = [-1]
  indicesListB = [-1]
  runListA = [-1]
  runListB = [-1]

  nElemProxArray = n_elements(proxArrayIJ)
  for i = 0, nElemProxArray-1 do begin
    if (ptr_valid(proxArrayIJ[i])) then begin
      indicesListA = [indicesListA, i]
      ;if keyword_set(maxIndexA) then if (i lt (nElemProxArray-1)) then indicesListA = [indicesListA, i]
      if (i lt (nElemProxArray-1)) then indicesListA = [indicesListA, i+1]
      indicesListB = [indicesListB, *proxArrayIJ[i]]
    endif else begin
      if (n_elements(indicesListA) gt 1) then begin ; check if there is a run to make
        indicesListA = indicesListA[1:*]
        indicesListA = indicesListA[sort(indicesListA)]
        indicesListA = indicesListA[uniq(indicesListA)]
        indicesListB = indicesListB[1:*]
        indicesListB = indicesListB[sort(indicesListB)]
        indicesListB = indicesListB[uniq(indicesListB)]
        runB   = makeRunFromCorrelativeValues(indicesListB)
        nElemB = n_elements(runB)
        nRunsB = nElemB / 2
        if (nRunsB gt 1) then begin
          runA = [-1]
          tmp  = [-1]
          runAiter = makeRunFromCorrelativeValues(indicesListA)
          for j = nElemB, 2, -2 do begin
            runA = [runA, runAiter]
            tmp  = [tmp, runB[j-2:j-1]]
          endfor
          runA = runA[1:*]
          runB = tmp[1:*]
        endif else begin
          runA = makeRunFromCorrelativeValues(indicesListA)
        endelse
        runListA = [runListA, runA]
        runListB = [runListB, runB]
        indicesListA = [-1]
        indicesListB = [-1]
      endif
    endelse

    if (i eq (nElemProxArray-1)) then begin
      if (n_elements(indicesListA) gt 1) then begin
        indicesListA = indicesListA[1:*]
        indicesListA = indicesListA[sort(indicesListA)]
        indicesListA = indicesListA[uniq(indicesListA)]
        indicesListB = indicesListB[1:*]
        indicesListB = indicesListB[sort(indicesListB)]
        indicesListB = indicesListB[uniq(indicesListB)]
        runB   = makeRunFromCorrelativeValues(indicesListB)
        nElemB = n_elements(runB)
        nRunsB = nElemB / 2
        if (nRunsB gt 1) then begin
          runA = [-1]
          tmp  = [-1]
          runAiter = makeRunFromCorrelativeValues(indicesListA)
          for j = nElemB, 2, -2 do begin
            runA = [runA, runAiter]
            tmp  = [tmp, runB[j-2:j-1]]
          endfor
          runA = runA[1:*]
          runB = tmp[1:*]
        endif else begin
          runA = makeRunFromCorrelativeValues(indicesListA)
        endelse
        runListA = [runListA, runA]
        runListB = [runListB, runB]
        indicesListA = [-1]
        indicesListB = [-1]
      endif
    endif
  endfor
  runListA = runListA[1:*]
  runListB = runListB[1:*]
  nElemA = n_elements(runListA)
  nRunsA = nElemA / 2

  ; Check circular runs.
  if n_elements(maxIndexA) gt 0 then $
    if (nRunsA gt 1) and (runListA[0] eq 0) and (runListA[nElemA-1] eq maxIndexA) then begin
      nElemB   = n_elements(runListB)
      subRunB1 = runListB[0:1]
      subRunB2 = runListB[nElemB-2:nElemB-1]
      fMergeB = (subRunB1[1] ge subRunB2[0]) and (subRunB1[0] le subRunB2[1])
      if fMergeB then begin
        minB = subRunB1[0] < subRunB2[0]
        maxB = subRunB1[1] > subRunB2[1]
        runListB = nElemB eq 4 ? [minB, maxB] : [minB, maxB, runListB[2:nElemB-3]]
        runListA = [runListA[nElemA-2], runListA[1:nElemA-3]]
        nRunsA = n_elements(runListA)/2
      endif
    endif

  return, nRunsA
end


pro findRunsFromCorrespondences_test 
;====== 1er Ejemplo =====
  p1x_ = double([1,4,4,2,2,4,4,1,1])
  p1y_ = double([2,2,3,3,6,6,7,7,2])
  p2x_ = double([5,8,8,5,5,7,7,5,5,7,7,5,5])
  p2y_ = double([1,1,7,7,6,6,4,4,3,3,2,2,1])
  testAndCorrectCWOrientation, p1x_, p1y_, xOut = p1x, yOut = p1y, fCCW = 1
  testAndCorrectCWOrientation, p2x_, p2y_, xOut = p2x, yOut = p2y, fCCW = 1
  print, p1x
  print, p1y
  print, p2x
  print, p2y
  thresholdDist = 1.0
  distIJ = distanceBetweenSegments(p1x, p1y, p2x, p2y, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  print, distIJ
  print, proxArrayIJ
  out = findRunsFromCorrespondences(proxArrayIJ, runList1, runList2)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1x, p1y, p2x, p2y, runList1, runList2, winID = 11, testID = 1
;====== 1er Ejemplo - Invertido =====
  p1x_ = double([5,8,8,5,5,7,7,5,5,7,7,5,5])
  p1y_ = double([1,1,7,7,6,6,4,4,3,3,2,2,1])
  p2x_ = double([1,4,4,2,2,4,4,1,1])
  p2y_ = double([2,2,3,3,6,6,7,7,2])
  testAndCorrectCWOrientation, p1x_, p1y_, xOut = p1x, yOut = p1y, fCCW = 1
  testAndCorrectCWOrientation, p2x_, p2y_, xOut = p2x, yOut = p2y, fCCW = 1
  print, p1x
  print, p1y
  print, p2x
  print, p2y
  thresholdDist = 1.0
  distIJ = distanceBetweenSegments(p1x, p1y, p2x, p2y, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  print, distIJ
  print, proxArrayIJ
  out = findRunsFromCorrespondences(proxArrayIJ, runList1, runList2)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1x, p1y, p2x, p2y, runList1, runList2, winID = 21, testID = 1


;====== 2do Ejemplo =====
  p1x_ = double([1,5,5,2,2,5,5,2,2,5,5,2,2,5,5,1,1])
  p1y_ = double([2,2,3,3,6,6,7,7,9,9,10,10,11,11,12,12,2])
  p2x_ = double([6,11,11,6 ,6 ,10,10,6,6,10,10,6,6,10,10,6,6])
  p2y_ = double([1,1 ,11,11,10,10,7 ,7,6,6 ,4 ,4,3,3 ,2 ,2,1])
  testAndCorrectCWOrientation, p1x_, p1y_, xOut = p1x, yOut = p1y, fCCW = 1
  testAndCorrectCWOrientation, p2x_, p2y_, xOut = p2x, yOut = p2y, fCCW = 1
  print, p1x
  print, p1y
  print, p2x
  print, p2y
  thresholdDist = 1.0
  distIJ = distanceBetweenSegments(p1x, p1y, p2x, p2y, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  print, distIJ
  print, proxArrayIJ
  out = findRunsFromCorrespondences(proxArrayIJ, runList1, runList2)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1x, p1y, p2x, p2y, runList1, runList2, winID = 12, testID = 2
 ;====== 2do Ejemplo - Invertido=====
  p1x_ = double([6,11,11,6 ,6 ,10,10,6,6,10,10,6,6,10,10,6,6])
  p1y_ = double([1,1 ,11,11,10,10,7 ,7,6,6 ,4 ,4,3,3 ,2 ,2,1])
  p2x_ = double([1,5,5,2,2,5,5,2,2,5,5,2,2,5,5,1,1])
  p2y_ = double([2,2,3,3,6,6,7,7,9,9,10,10,11,11,12,12,2])
  testAndCorrectCWOrientation, p1x_, p1y_, xOut = p1x, yOut = p1y, fCCW = 1
  testAndCorrectCWOrientation, p2x_, p2y_, xOut = p2x, yOut = p2y, fCCW = 1
  print, p1x
  print, p1y
  print, p2x
  print, p2y
  thresholdDist = 1.0
  distIJ = distanceBetweenSegments(p1x, p1y, p2x, p2y, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  print, distIJ
  print, proxArrayIJ
  out = findRunsFromCorrespondences(proxArrayIJ, runList1, runList2)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1x, p1y, p2x, p2y, runList1, runList2, winID = 22, testID = 2
  
;====== 3er Ejemplo =====
  f1 = 40
  f2 = 60
  offset1 = 60
  p1x = [0.0, 2.0, 2.0, 2.0, 0.0, 0.0]*f1 + offset1 
  p1y = [0.0, 0.0, 2.0, 4.0, 4.0, 0.0]*f1
  p2x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p2y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
  thresholdDist = 15
  nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1xInt = p1x
    p1yInt = p1y
    p2xInt = p2x
    p2yInt = p2y
  endif else begin
    polygonArcSample, p1x, p1y, p1xInt, p1yInt, POINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2xInt, p2yInt, POINTS=nPts, /FCLOSEOUTPUT
  endelse
  distIJ = distanceBetweenSegments(p1xint, p1yint, p2xint, p2yint, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  out = findRunsFromCorrespondences(proxArrayIJ, runList1, runList2)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1xint, p1yint, p2xint, p2yint, runList1, runList2, winID = 13, testID = 3
;====== 3er Ejemplo - Invertido =====
  f1 = 40
  f2 = 60
  offset1 = 60
  p1x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p1y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
  p2x = [0.0, 2.0, 2.0, 2.0, 0.0, 0.0]*f1 + offset1 
  p2y = [0.0, 0.0, 2.0, 4.0, 4.0, 0.0]*f1
  thresholdDist = 15
  nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1xInt = p1x
    p1yInt = p1y
    p2xInt = p2x
    p2yInt = p2y
  endif else begin
    polygonArcSample, p1x, p1y, p1xInt, p1yInt, POINTS=nPts, /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2xInt, p2yInt, POINTS=round(nPts*2.5), /FCLOSEOUTPUT
  endelse
  distIJ = distanceBetweenSegments(p1xint, p1yint, p2xint, p2yint, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  out = findRunsFromCorrespondences(proxArrayIJ, runList1, runList2)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1xint, p1yint, p2xint, p2yint, runList1, runList2, winID = 23, testID = 3
  
;====== 4to Ejemplo =====
  p1x = [0.0, 2.0, 1.5, 3.2, 0.0, 0.0]*f1 + offset1
  p1y = [0.0, 0.0, 2.0, 1.9, 4.0, 0.0]*f1
  p2x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p2y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
  nPts = keyword_set(nPts) ? nPts > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1xInt = p1x
    p1yInt = p1y
    p2xInt = p2x
    p2yInt = p2y
  endif else begin
    polygonArcSample, p1x, p1y, p1xInt, p1yInt, POINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2xInt, p2yInt, POINTS=nPts, /FCLOSEOUTPUT
  endelse
  thresholdDist = 20
  distIJ = distanceBetweenSegments(p1xint, p1yint, p2xint, p2yint, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  out = findRunsFromCorrespondences(proxArrayIJ, runList1, runList2)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1xint, p1yint, p2xint, p2yint, runList1, runList2, winID = 14, testID = 4
;====== 4to Ejemplo - Invertido =====
  p1x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p1y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
  p2x = [0.0, 2.0, 1.5, 3.2, 0.0, 0.0]*f1 + offset1
  p2y = [0.0, 0.0, 2.0, 1.9, 4.0, 0.0]*f1
  nPts = keyword_set(nPts) ? nPts > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1xInt = p1x
    p1yInt = p1y
    p2xInt = p2x
    p2yInt = p2y
  endif else begin
    polygonArcSample, p1x, p1y, p1xInt, p1yInt, POINTS=nPts, /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2xInt, p2yInt, POINTS=round(nPts*2.5), /FCLOSEOUTPUT
  endelse
  thresholdDist = 20
  distIJ = distanceBetweenSegments(p1xint, p1yint, p2xint, p2yint, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points
  out = findRunsFromCorrespondences(proxArrayIJ, runList1, runList2)
  print, out
  print, 'runList1', runList1
  print, 'runList2', runList2
  if out gt 0 then findRunsInDistMatrix2a_testPlot, p1xint, p1yint, p2xint, p2yint, runList1, runList2, winID = 24, testID = 4
      
end


; findRunsInDistMatrix2
; It is assumed that the values from startI(endI)/startJ(endJ) conform to their polygon index range [0..n-1].
function findRunsInDistMatrix2, p1x, p1y, p2x, p2y, pDistance1to2, pEndpoints, startI, startJ, thresholdSqDist, runList1, runList2, fReturnFirst = fReturnFirst, fBackwards = fBackwards, endI = endI, endJ = endJ 

  verticesChecked1 = 1u
  verticesChecked2 = 0u

  lookupPos1 = long(startI)
  lookupPos2 = long(startJ)

  runList1 = [-1]
  runList2 = [-1]
  nRuns  = 0u

  n1 = n_elements(p1x)
  n2 = n_elements(p2x)

  if (n_elements(endI) eq 1) then n1a = endI - startI + ((endI ge startI) ? 0 : n1) else n1a = n1
  if (n_elements(endJ) eq 1) then n2a = endJ - startJ + ((endJ ge startJ) ? 0 : n2) else n2a = n2
  print, 'n1 ', n1, ' n2 ', n2, ' startI ', startI, ((n_elements(endI) eq 1) ? (' endI ' + string(endI)) : ''), ' startJ ', startJ, ((n_elements(endJ) eq 1) ? ('endJ ' + string(endJ)) : ''), ' n1a ', n1a, ' n2a ', n2a

  while (verticesChecked1 lt n1a-1) do begin
    fMatch1 = ((*pDistance1to2)[lookupPos2,lookupPos1] le thresholdSqDist)
    ;print, lookupPos1,lookupPos2, fMatch1
x:  if (fMatch1 eq 1) then begin
      ;print, 'Found a run start'
      nRuns = 1
      runLen = 0
      runList1 = [lookupPos1, -1]
      runList2 = [(*pEndpoints)[lookupPos2, lookupPos1], -1]
      runIn2FromI = [lookupPos2, lookupPos2]
      while (fMatch1 eq 1) do begin
        verticesChecked2 = 1u
        lastOKpos2 = lookupPos2
        ; Add the run starting points
        repeat begin
          runLen += 1
          if keyword_set(fBackwards) $
          then lookupPos2 = lookupPos2 ge n2-2 ? 0 : lookupPos2+1 $
          else lookupPos2 = lookupPos2 gt 0 ? lookupPos2-1 : n2-2
          verticesChecked2 += 1

          fMatch = (*pDistance1to2)[lookupPos2, lookupPos1] le thresholdSqDist
          ;print, lookupPos1,lookupPos2, fMatch
          if (fMatch eq 1) then begin
            runList1[1] = lookupPos1
            runList2[1] = lookupPos2
            lastOKpos2 = lookupPos2
          endif

        endrep until (fMatch eq 0) or (verticesChecked2 eq n2a)
        runIn2FromI[1] = lastOKpos2

        if keyword_set(fBackwards) $
        then lookupPos1 = lookupPos1 gt 0 ? lookupPos1-1 : n1-1 $
        else lookupPos1 = lookupPos1 ge n1-1 ? 0 : lookupPos1+1
        fMatch1 = ((*pDistance1to2)[lookupPos2,lookupPos1] le thresholdSqDist)
        ;print, lookupPos1,lookupPos2, fMatch1

        if (fMatch1 eq 0) then begin
          nElem = runIn2FromI[0] - runIn2FromI[1] + 1
          if (nElem lt 0) then nElem += (n2-1)
          ;nElem = runIn2FromI[1] - runIn2FromI[0] + 1 + ((runIn2FromI[1] ge runIn2FromI[0]) ? 0 : n2) ; ONLY forward case by now...
          ;print, 'nElem ', nElem
          for i = 0L, nElem-1 do begin
            ;print, 'looking in the past for ', lookupPos1
            unadjustedPos = runIn2FromI[1]+i
            fMatch1 = ((*pDistance1to2)[(unadjustedPos ge n2-1)? (unadjustedPos-n2+1) : unadjustedPos,lookupPos1] le thresholdSqDist)
            ;print, 'history for ', nElem, 'elements: ', lookupPos1,(unadjustedPos ge n2-1)? (unadjustedPos-n2+1) : unadjustedPos, fMatch1
            if (fMatch1 eq 1) then begin
              lookupPos2 = lastOKpos2
              if keyword_set(fBackwards) $
              then lookupPos1 = lookupPos1 gt 0 ? lookupPos1-1 : n1-1 $
              else lookupPos1 = lookupPos1 ge n1-1 ? 0 : lookupPos1+1
            endif
            if (fMatch1 eq 1) then break
          endfor
        endif
        if (fMatch1 eq 1) then runList1[1] = lookupPos1
      endwhile
      return, nRuns
    endif else begin
      for verticesChecked2 = 0L, n2a-1 do begin
        if keyword_set(fBackwards) $
        then lookupPos2 = lookupPos2 ge n2-2 ? 0 : lookupPos2+1 $
        else lookupPos2 = lookupPos2 gt 0 ? lookupPos2-1 : n2-2
        fMatch1 = ((*pDistance1to2)[lookupPos2,lookupPos1] le thresholdSqDist)
        if (fMatch1 eq 1) then goto, x
      endfor
    endelse

    if keyword_set(fBackwards) $
    then lookupPos1 = lookupPos1 gt 0 ? lookupPos1-1 : n1-1 $
    else lookupPos1 = lookupPos1 ge n1-1 ? 0 : lookupPos1+1
    verticesChecked1 += 1

  endwhile
  return, nRuns
end


; findRunsInDistMatrix
; It is assumed that the values from startI(endI)/startJ(endJ) conform to their polygon index range [0..n-1].
function findRunsInDistMatrix, p1x, p1y, p2x, p2y, pDistance1to2, pEndpoints, startI, startJ, thresholdDist, runList1, runList2, fReturnFirst = fReturnFirst, fBackwards = fBackwards, endI = endI, endJ = endJ

  verticesChecked1 = 0u
  verticesChecked2 = 0u

  lookupPos1 = startI
  lookupPos2 = startJ
  lastVertexPos2 = lookupPos2

  runList1 = [-1]
  runList2 = [-1]
  nRuns  = 0u

  n1 = n_elements(p1x)
  n2 = n_elements(p2x)

  if (n_elements(endI) eq 1) then n1 = endI - startI + 1 + (endI ge startI) ? 0 : n1
  if (n_elements(endJ) eq 1) then n2 = endJ - startJ + 1 + (endJ ge startJ) ? 0 : n2

  while (verticesChecked1 lt n1) and (verticesChecked2 lt n2-1) do begin

    verticesChecked1 += 1
    verticesChecked2 += 1

    if ((*pDistance1to2)[lookupPos2,lookupPos1] le thresholdDist) then begin
      nRuns += 1
      runLen = 0u
      ; Add the run starting points
      runList1 = [runList1, lookupPos1]
      runList2 = [runList2, (*pEndpoints)[lookupPos2, lookupPos1]]
      if keyword_set(fBackwards) $
      then lookupPos2 = lookupPos2 ge n2-2 ? 0 : lookupPos2+1 $
      else lookupPos2 = lookupPos2 gt 0 ? lookupPos2-1 : n2-2
      ; Now look for the endpoints
      repeat begin
        runLen += 1u
        fMatch = (*pDistance1to2)[lookupPos2, lookupPos1] le thresholdDist
        if (fMatch eq 1) then begin
          if keyword_set(fBackwards) $
          then lookupPos2 = lookupPos2 ge n2-2 ? 0 : lookupPos2+1 $
          else lookupPos2 = lookupPos2 gt 0 ? lookupPos2-1 : n2-2
          verticesChecked2 += 1

          if keyword_set(fBackwards) $
          then lookupPos1 = lookupPos1 gt 0 ? lookupPos1-1 : n1-1 $
          else lookupPos1 = lookupPos1 ge n1-1 ? 0 : lookupPos1+1
          verticesChecked1 += 1

          fMatch = (*pDistance1to2)[lookupPos2, lookupPos1] le thresholdDist
        endif else begin
          if keyword_set(fBackwards) $
          then lookupPos1 = lookupPos1 gt 0 ? lookupPos1-1 : n1-1 $
          else lookupPos1 = lookupPos1 ge n1-1 ? 0 : lookupPos1+1

          verticesChecked2 -= (lookupPos2 - lastVertexPos2)
          lookupPos2 = lastVertexPos2
          verticesChecked1 += 1
          fMatch = (*pDistance1to2)[lookupPos2, lookupPos1] le thresholdDist
        endelse
      endrep until (fMatch eq 0)
      runList1 = [runList1, lookupPos1]
      runList2 = [runList2, (*pEndpoints)[lookupPos2, lookupPos1]]
      if keyword_set(fReturnFirst) then goto, a
    endif

    ; Checkings needed before advancing to the next segment.
    ; It is evident that vertices in the second polygon need to be navigated backwards ;)
    if keyword_set(fBackwards) then begin
      lookupPos2 = lookupPos2 ge n2-2 ? 0 : lookupPos2+1
      lookupPos1 = lookupPos1 gt 0 ? lookupPos1-1 : n1-1
    endif else begin
      lookupPos2 = lookupPos2 gt 0 ? lookupPos2-1 : n2-2
      lookupPos1 = lookupPos1 ge n1-1 ? 0 : lookupPos1+1
    endelse

  endwhile
  print, 'Checked ', verticesChecked1, ' elements from 1st list, ', verticesChecked2, ' from 2nd list'

a:if (nRuns gt 0) then begin
    runList1 = runList1[1:*]
    runList2 = runList2[1:*]
  endif
  return, nRuns

end


; TODO document and remove fMakePlot
;
; verticesToInclude : vector of boolean
function findNeighborSegmentsInPairOfPolygons, p1x, p1y, p2x, p2y, thresholdDist, runsIJ_1, runsIJ_2, distIJ = distIJ, fMakePlot = fMakePlot, verticesToInclude = verticesToInclude
  n1 = n_elements(p1x)
  n2 = n_elements(p2x)
  if (n1 lt 1) or (n2 lt 1) then begin
    print, 'findNearestNeighborsInPolygonPair error: not enough points to look for proximity runs'
    return, -1
  endif

  distIJ = distanceBetweenSegments(p1x, p1y, p2x, p2y, $
    distanceThreshold = thresholdDist, proximityCorrespondences = proxArrayIJ, proximityEndpointCodes = endpointCodesIJ, whereInThreshold = whereInThresholdIJ) ; calculate only with non-inside points

  whereInRangeIJ = where(ptr_valid(proxArrayIJ) eq 1, nInRangeIJ)
  if (nInRangeIJ eq 0) then return, 0

  if (nInRangeIJ gt 0) and keyword_set(fMakePlot) then $
    for i = 0u, nInRangeIJ-1 do $
      oPlot, [p1x[whereInRangeIJ[i]], p1x[whereInRangeIJ[i]+1]], [p1y[whereInRangeIJ[i]], p1y[whereInRangeIJ[i]+1]], pSym = 6, color = '00FF00'x

  if (nInRangeIJ gt 0) then begin
    runsIJ_1 = makeRunFromCorrelativeValues(whereInRangeIJ)
    nRunsIJ = n_elements(runsIJ_1) / 2
    if (nRunsIJ gt 1) and ((runsIJ_1[2*nRunsIJ-1] eq (n1-1)) and (runsIJ_1[0] eq 0)) then runsIJ_1 = [runsIJ_1[2:2*nRunsIJ-2], runsIJ_1[1]]
    runsIJ_2 = ptrArr(1)
    runsIJ_2_pos = 0
    for i = 0u, nRunsIJ-1 do begin
      currentNeighbors = [-1]
      runPos = 2*i
      nElementsCurRun = (runsIJ_1[runPos+1] ge runsIJ_1[runPos]) ? (runsIJ_1[runPos+1] - runsIJ_1[runPos] + 1) : (runsIJ_1[runPos+1] + 1 + (n1 - runsIJ_1[runPos])) 
      for j = 0, nElementsCurRun-1 do begin
        curSegment = (runsIJ_1[runPos] + j) mod n1
        currentNeighbors = [currentNeighbors, *proxArrayIJ[curSegment]]
      endfor
      currentNeighbors = currentNeighbors[1:*]
      currentNeighbors = currentNeighbors[sort(currentNeighbors)]
      currentNeighborRuns = makeRunFromCorrelativeValues(currentNeighbors[uniq(currentNeighbors)])
      runsIJ_2[runsIJ_2_pos] = ptr_new(currentNeighborRuns, /no_copy)
      runsIJ_2 = [runsIJ_2, ptr_new()]
      runsIJ_2_pos += 1
    endfor
    runsIJ_2 = runsIJ_2[0:nRunsIJ-1]
    if keyword_set(fMakePlot) then for j = 0, n_elements(runsIJ_2)-1 do print, 'Runs found [', j, ']:', *runsIJ_2[j]
  endif
  return, nInRangeIJ
end


; findNeighborsInPairOfPolygons
;
; Input arguments
;  p1x/p2x, p1y/p2y  2D polygon vertex lists (in the form of consecutive line segments).
;  thresholdDist     A threshold distance (squared) to use to determine neighboring polygon elements.
;
; Output arguments
;  runs1out
;  runs2out
;
; Optional arguments
;  fMakePlot         If set to non-zero value, indicates to make a sample/debug plot.
;  backImage         Background image to be used with the (optional) plot.
function findNeighborsInPairOfPolygons2, p1x, p1y, p2x, p2y, thresholdDist, runs1out, runs2out, fMakePlot = fMakePlot, backImage = backImage

  n1 = n_elements(p1x)
  n2 = n_elements(p2x)
  if (n1 lt 1) or (n2 lt 1) then begin
    print, 'findNearestNeighborsInPolygonPair error: not enough points to look for proximity runs'
    return, -1
  endif

  if keyword_set(fMakePlot) then makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = 'PROXIMITY PLOT', winId = 30, backgroundImage = backImage, /fCropImage

  defaultMinDist = 1344d * 1024d
  min1to2_d = defaultMinDist
  min1to2_i = -1
  min1to2_j = -1
  distance_1to2 = dblArr(n2-1, n1)
  closestEndPoints1to2 = ulonArr(n2-1, n1)

  for i = 0L, n1-1 do begin ; vertex counter for polygon 1
    for j = 0L, n2-2 do begin ; segment counter for polygon 2
      d_ji = distanceFromPointToSegment(p1x[i], p1y[i], p2x[j], p2y[j], p2x[j+1], p2y[j+1], closestEndpoint = closestEndpointCode, /FSQUAREDIST)
      case closestEndpointCode of
        0: closestEndPoints1to2[j,i] = j+1 ; arbitrary selection, both segments overlap
        1: closestEndPoints1to2[j,i] = j
        2: closestEndPoints1to2[j,i] = j+1
        3: closestEndPoints1to2[j,i] = j+1 ; arbitrary selection, segments are parallel and aligned
      endcase
      if (d_ji le min1to2_d) then begin ; "lt" vs "le"...
        min1to2_d = d_ji
        min1to2_i = i
        min1to2_j = j
      endif
      distance_1to2[j,i] = d_ji
    endfor
  endfor
  if keyword_set(fMakePlot) and (n_elements(distance_1to2) lt 400) then print, distance_1to2

  p1to2 = ptr_new(distance_1to2, /NO_COPY)
  pEndpoints1to2 = ptr_new(closestEndPoints1to2, /NO_COPY)
  endI = min1to2_i eq 0 ? n1-1 : min1to2_i-1
  endJ = min1to2_j eq 0 ? n2-2 : min1to2_j-1
  nRuns1bwd = findRunsInDistMatrix(p1x, p1y, p2x, p2y, p1to2, pEndpoints1to2, min1to2_i, min1to2_j, thresholdDist, run1b, run2b, endI = endI, endJ = endJ, /fReturnFirst, /FBACKWARDS)
  nRuns1fwd = findRunsInDistMatrix(p1x, p1y, p2x, p2y, p1to2, pEndpoints1to2, min1to2_i, min1to2_j, thresholdDist, run1f, run2f, endI = endI, endJ = endJ, /fReturnFirst)

  ; Assemble the first run found (merge backward and forward runs), before looking for others.
  if (nRuns1fwd gt 0) or (nRuns1bwd gt 0) then begin
    case 1 of
      (nRuns1bwd gt 0) and (nRuns1fwd eq 0): run1 = run1b
      (nRuns1bwd eq 0) and (nRuns1fwd gt 0): run1 = run1f
      else: run1 = [run1b[1], run1f[1]]
    endcase
    run1idx = runToIndexes(run1[0], run1[1], n1)

    if keyword_set(fMakePlot) then oplot, [p1x[run1idx]], [p1y[run1idx]], psym=2, symsize = 0.7;, color = 'FFFF00'x, linestyle = 1
    nRuns2bwd = n_elements(run2b)
    nRuns2fwd = n_elements(run2f)
    case 1 of
      (nRuns2bwd gt 0) and (nRuns2fwd eq 0): run2 = run2b
      (nRuns2bwd eq 0) and (nRuns2fwd gt 0): run2 = run2f
      else: run2 = [run2f[1], run2b[1]]
    endcase
    run2idx = runToIndexes(run2[0], run2[1], n2)
    if keyword_set(fMakePlot) then oplot, [p2x[run2idx]], [p2y[run2idx]], psym=2, symsize = 0.7;, color = 'FFFF00'x, linestyle = 1
  endif else begin
    runs1out = [-1]
    runs2out = [-1]
    print, 'No runs found for given distance threshold ', thresholdDist
    goto, z
  endelse

  runLength1 = 1 + ((run1[1] gt run1[0]) ? run1[1]-run1[0] : run1[1]+(n1-run1[0])-1)
  print, 'First run / 1st roi found, length =', runLength1, ' , from ', run1[0], ' to ', run1[1]
  runLength2 = 1 + ((run2[1] gt run2[0]) ? run2[1]-run2[0] : run2[1]+(n2-run2[0])-1)
  print, 'First run / 2nd roi found, length =', runLength2, ' , from ', run2[0], ' to ', run2[1]

  ; If there are remaining zones to search for runs...
  if (runLength1 lt n1) and (runLength2 lt n2) then begin
  startI = run1[1] eq n1-1 ? 0 : run1[1] + 1
  endI   = run1[0]
  ;startJ = run2[0] eq 0 ? n2-1 : run2[0] - 1
  ;endJ   = run2[1] eq n2-2 ? 0 : run2[1] + 1
  it = 0u

  checkedStartI = [startI]

  repeat begin
    it += 1
    ; TODO JJ For now, don't mind finding duplicated AAB segments in 2nd ROI -> check and correct after
    endJ   = min1to2_j
    startJ = min1to2_j eq n2-2 ? 0 : min1to2_j+1
    nRuns1next = findRunsInDistMatrix2(p1x, p1y, p2x, p2y, p1to2, pEndpoints1to2, startI, startJ, thresholdDist, run1fNext, run2fNext, endI = endI, /FRETURNFIRST)
    print, 'Found ', nRuns1next, ' runs in try ', it, ', 1st ROI: ', run1fNext
    print, 'Found ', nRuns1next, ' runs in try ', it, ', 2nd ROI: ', run2fNext
    if (nRuns1next gt 0) then begin
      run2fNextFwd = [-1]
      for i = 0L, nRuns1next-1 do begin
        runPos = i*2
        if (run1fNext[runPos] eq -1) or (run1fNext[runPos+1] eq -1) then stop
        run1fNextidx = runToIndexes(run1fNext[runPos], run1fNext[runPos+1], n1)
        if keyword_set(fMakePlot) then oplot, p1x[run1fNextidx], p1y[run1fNextidx], psym=2, symsize = 0.7, color = 'FFFF00'x, linestyle = 1
        run2fNextidx = runToIndexes(run2fNext[runPos+1], run2fNext[runPos], n2)
        if keyword_set(fMakePlot) then oplot, p2x[run2fNextidx], p2y[run2fNextidx], psym=2, symsize = 0.7, color = 'FFFF00'x, linestyle = 1
        run2fNextFwd = [run2fNextFwd, run2fNext[runPos+1], run2fNext[runPos]]
      endfor
      runs1out = [run1, run1fNext]
      runs2out = [run2, run2fNextFwd[1:*]]

      startI = run1fNext[1] eq n1-1 ? 0 : run1fNext[1] + 1
      whChecked = where(startI eq checkedStartI);
      if (whChecked[0] ne -1) $
      then break $
      else checkedStartI = [checkedStartI, startI]
    ; Implicit that endI = run1[0]
    endif else begin
      runs1out = run1
      runs2out = run2
    endelse
  endrep until (nRuns1next eq 0)
  endif

;  min2to1_d = defaultMinDist
;  min2to1_i = -1
;  min2to1_j = -1
;  distance_2to1 = dblArr(n1-1, n2)
;
;  for i = 0L, n2-1 do begin
;    for j = 0L, n1-2 do begin
;      d_ji = distanceFromPointToSegment(p2x[i], p2y[i], p1x[j], p1y[j], p1x[j+1], p1y[j+1], /FSQUAREDIST)
;      if (d_ji lt min2to1_d) then begin
;        min2to1_d = d_ji
;        min2to1_i = i
;        min2to1_j = j
;      endif
;      distance_2to1[j,i] = d_ji
;    endfor
;  endfor
  ;print, distance_2to1
;  p2to1 = ptr_new(distance_1to2, /NO_COPY)
;  ptr_free, p2to1, pEndpoints2to1
  ; TODO set distance for interior vertices to 0 or negative, before this...
z:ptr_free, p1to2, pEndpoints1to2
  ;stop
  return, n_elements(runs1out)/2
end


pro findNeighborSegmentsInPairOfPolygons_test, fNoInterp = fNoInterp
  f1 = 40
  f2 = 60
  offset1 = 60
  p1x = [0.0, 2.0, 2.0, 2.0, 0.0, 0.0]*f1 + offset1 
  p1y = [0.0, 0.0, 2.0, 4.0, 4.0, 0.0]*f1
  p2x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p2y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
  nPts = keyword_set(nPts) ? nPts > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1xInt = p1x
    p1yInt = p1y
    p2xInt = p2x
    p2yInt = p2y
  endif else begin
    polygonArcSample, p1x, p1y, p1xInt, p1yInt, POINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2xInt, p2yInt, POINTS=nPts, /FCLOSEOUTPUT
  endelse

;  dllLocation = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
;  n1 = n_elements(p1xint)
;  n2 = n_elements(p2xint)
;  distMatrix = dblArr((n1-1) * (n2-1))
;  closest1 = 0u
;  closest2 = 0u
;  fCall = call_external(dllLocation, 'distanceMatrixFromPolygonToPolygon', p1xint, p1yint, p2xint, p2yint, n1, n2, distMatrix, closest1, closest2, RETURN_TYPE=5, /UNLOAD)

  makePolygonsPlot, p1xint, p1yint, p2xint, p2yint, winTitle = 'findNeighborsInPairOfPolygons_test';, winId = 30, backgroundImage = backImage, /fCropImage
  thresholdDistance = 0.75 * f1<f2
  neighborsIJ = findNeighborSegmentsInPairOfPolygons(p1xint, p1yint, p2xint, p2yint, thresholdDistance, /FMAKEPLOT)
  neighborsJI = findNeighborSegmentsInPairOfPolygons(p2xint, p2yint, p1xint, p1yint, thresholdDistance, /FMAKEPLOT)
  stop
  p1x = [0.0, 2.0, 1.5, 3.2, 0.0, 0.0]*f1 + offset1
  p1y = [0.0, 0.0, 2.0, 1.9, 4.0, 0.0]*f1
  p2x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p2y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
  nPts = keyword_set(nPts) ? nPts > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1xInt = p1x
    p1yInt = p1y
    p2xInt = p2x
    p2yInt = p2y
  endif else begin
    polygonArcSample, p1x, p1y, p1xInt, p1yInt, POINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2xInt, p2yInt, POINTS=nPts, /FCLOSEOUTPUT
  endelse

  makePolygonsPlot, p1xint, p1yint, p2xint, p2yint, winTitle = 'findNeighborsInPairOfPolygons_test';, winId = 30, backgroundImage = backImage, /fCropImage
  thresholdDistance = 1.75 * 10
  neighborsIJ = findNeighborSegmentsInPairOfPolygons(p1xint, p1yint, p2xint, p2yint, thresholdDistance, /FMAKEPLOT)
  neighborsJI = findNeighborSegmentsInPairOfPolygons(p2xint, p2yint, p1xint, p1yint, thresholdDistance, /FMAKEPLOT)

;  p1x = [0.0, 0.0, 1.0, 1.0, 0.0]
;  p1y = [0.0, 1.0, 1.0, 0.0, 0.0]
;  p2x = [0.5, 0.5, 1.5, 1.5, 0.5]
;  p2y = [0.5, 1.5, 1.5, 0.5, 0.5]
;  distMatrix = calcDistanceMatrixforPolygonVertices(p1x, p1y, p2x, p2y, /fSquare)
;  print, 'square distances', distMatrix
;  distMatrix = calcDistanceMatrixforPolygonVertices(p1x, p1y, p2x, p2y)
;  print, 'distances', distMatrix
end


function distanceBetweenSegments_notYet, x1a, x1b, y1a, y1b, x2a, x2b, y2a, y2b, distanceThreshold = distanceThreshold

  numType = size(x1a, /TYPE) > size(x2a, /TYPE)
  machineArithmetic = numType eq 5 ? machAr(/DOUBLE) : machAr()
  if (n_elements(distanceThreshold) eq 0) $
  then distanceThreshold = machineArithmetic.EPS $
  else distanceThreshold >= machineArithmetic.EPS
  return, distanceBetween2SegmentsIn3D([x1a, y1a, 0], [x1b, y1b, 0], [x2a, y2a, 0], [x2b, y2b, 0])
end


; TODO IT DOESN'T WORK YET!
; JJW. 2013.03.24
function calcDistanceBetweenPolylines, p1x, p1y, p2x, p2y

  n_segments1 = n_elements(p1x) - 1
  n_segments2 = n_elements(p2x) - 1
  numType1 = size(p1x, /TYPE)
  numType2 = size(p2x, /TYPE)
  numTypeD = numType1 > numType2

  if (n_segments1 lt 1) or (n_segments2 lt 1) then begin
    print, 'ERROR in calcDistanceBetweenPolylines: insufficient number of elements (', n_segments1, ', ', n_segments2,')'
    return, -1
  endif

  case numTypeD of
    4 : begin
      d1to2 = fltArr(n_segments1)
      d2to1 = fltArr(n_segments2)
    endcase
    5 : begin
      d1to2 = dblArr(n_segments1)
      d2to1 = dblArr(n_segments2)
    endcase
    else: return, -1
  endcase

  d1to2Index = ulonArr(n_segments1)
  d2to1Index = ulonArr(n_segments2)
  d = -1.0d
  ; TODO Insert claver strategy by HMA here ;)
  for i = 0L, n_segments1-1 do begin
    ns1 = 2*1
    for j = 0L, n_segments2-1 do begin
      ns2 = 2*j
      stop ; TODO this function has changed... modify the calling accordingly
      dij = distanceBetweenSegments(p1x[ns1], p1x[ns1+1], p1y[ns1], p1y[ns1+1], p2x[ns2], p2x[ns2+1], p2y[ns2], p2y[ns2])
    endfor
  endfor

  return, numTypeD eq 4 ? float(d) : d
end


; TODO JJW check/delete
function calcDistanceFromPointsToPolygonDbl, polyX, polyY, pointsX, pointsY
  nPoints     = n_elements(pointsX)
  nPointsPoly = n_elements(polyX)
  if (nPoints lt 1) or (nPointsPoly lt 1) then return, -1.0
  if (nPoints eq 1) then begin
    pointsX = [pointsX]
    pointsY = [pointsY]
  endif
  distanceArr = dblArr(nPoints)
  stop ; Not computed yet.
  return, distanceArr
end


function calcDistanceFromPointsToPolygon, polyX, polyY, pointsX, pointsY

  nPoints = n_elements(pointsX)
  nPointsPoly = n_elements(polyX)
  if (nPoints lt 1) or (nPointsPoly lt 1) then return, -1.0
  if (nPoints eq 1) then begin
    pointsX = [pointsX]
    pointsY = [pointsY]
  endif
  distanceArr = dblArr(nPoints)
  testAndCorrectCWOrientation, polyX, polyY, XOUT = polyX_, YOUT = polyY_

  dllLocation = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
  fResult = call_external(dllLocation, 'distanceFromPointsToPolygon',$
                          pointsX, pointsY, fix(nPoints, type = 2),$
                          polyX_, polyY_, fix(nPointsPoly, type = 2),$
                          distanceArr, /unload) ; TODO add parameter to ask for comparable distance (e.g. sqr. distance which is faster)
  return, distanceArr
end


pro calcDistanceFromPointsToPolygonTest
  polyX = [0.0, 0.0, 1.0, 1.0, 0.0]
  polyY = [0.0, 1.0, 1.0, 0.0, 0.0]
  pointsX = [0.0, 0.0, 1.0, 1.0, 0.0, 2.0, -1.0, 3.141592]
  pointsY = [0.0, 1.0, 1.0, 0.0, 0.0, 2.0, -1.0, 2.78]
  print, calcDistanceFromPointsToPolygon(polyX, polyY, pointsX, pointsY)
end


pro testIDLanROIcontainsPoints
  p1x = [0.0, 0.0, 1.0, 1.0, 0.0]
  p1y = [0.0, 1.0, 1.0, 0.0, 0.0]
  p2x = [0.5, 0.5, 1.5, 1.5, 0.5]
  p2y = [0.5, 1.5, 1.5, 0.5, 0.5]
  roi1 = obj_new('IDLanROI', p1x, p1y, type = 0)
  roi2 = obj_new('IDLanROI', p2x, p2y, type = 0)
  does1contain2 = roi1->ContainsPoints(p2x, p2y)
  print, 'does1contain2: ', does1contain2
  does2contain1 = roi1->ContainsPoints(p1x, p1y)
  print, 'does2contain1 :', does1contain2
end


; ASSN: border rows and columns of the image have background color
pro scanlinefill, image, fillValue
  sz = size(image, /dim)
  t0 = systime(1)
  for i = 0, sz[1]-1 do begin
    count = 0u
    fOut = 1b
    for j = 0, sz[0]-1 do begin
      case 1 of
        (fOut eq 1) and (image[j,i] ne fillValue): image[j,i] = fillValue
        (fOut eq 1) and (image[j,i] eq fillValue): begin
          count += 1
          fOut = 0b
        end
        (fOut eq 0) and (image[j,i] eq fillValue): begin
          if (j+1 lt sz[0]) then $
          if (image[j+1,i] ne fillValue) then begin
            fOut = 1b
            count += 1
          endif
        end
        else: ;(fOut eq 0) and (image[j,i] ne fillValue)... do nothing
      endcase
    endfor
  endfor
  t1 = systime(1)
  print, 'elapsed time (s): ', t1-t0
end


; DEPENDENCIES:
; - rasterCircle
pro testfloodfillDLL
  dllLoc = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
  sz = [200, 200]
  img = make_array(sz, /int, value = 0b)
  xCenter = floor(sz[0] * 0.5)
  yCenter = floor(sz[1] * 0.5)
  radius = floor((xCenter < yCenter) / 2)
  rasterCircle, image = img, radius = radius, x0 = xCenter, y0 = yCenter, value = 255, fill = 0
  fillValue = 140
  f = call_external(dllLoc, 'fill', fix(0, type = 2), fix(0, type = 2), fix(img, type = 2), fix(sz[0], type = 2), fix(sz[1], type = 2), fix(fillValue, type = 2), /unload)
  print, f
end


; TODO this doesn't work since the functions were refactored... pending
pro testDllDistancePointPoly
  p1x = [0.0, 0.0, 1.0, 1.0, 0.0]
  p1y = [0.0, 1.0, 1.0, 0.0, 0.0]
  pointX = [10.0d]
  pointY = [10.0d]
  np = n_elements(pointX)
  distArr = dblArr(np)
  dllLocation = getDLLfilename('polygon_intersection', /GETFILENAMEANDPATH)
  d = call_external(dllLocation, 'distanceFromPointsToPolygon',$
                    pointX, pointY, np, fix(p1x, type = 5), fix(p1y, type = 5), fix(n_elements(p1x), type = 2),$
                    distArr, RETURN_TYPE=5, /unload)
  print, 'computed distance(s): ', distArr

  pointX = [0.05d]
  pointY = [0.05d]
  np = n_elements(pointX)
  distArr = dblArr(np)
  d = call_external(dllLocation, 'distanceFromPointsToPolygon',$
                    pointX, pointY, np, fix(p1x, type = 5), fix(p1y, type = 5), fix(n_elements(p1x), type = 2),$
                    distArr, RETURN_TYPE=5, /unload)
  print, 'computed distance(s): ', distArr
end


function calcDiceCoefficientForPolygons, p1x, p1y, p2x, p2y

  p1area = polyArea(XCOORDS = p1x, YCOORDS=p1y)
  p2area = polyArea(XCOORDS = p2x, YCOORDS=p2y)

  pOvrArea = 0.0
  ovr = calcPolygonIntersection(P1X=p1x, P1Y=p1y, P2X=p2x, P2Y=p2y)
  numOvrPolys = ovr[0]

  if (numOvrPolys gt 0) then begin
    polyStart = 2 ; start from 2, since ovr[1] contains the number of vertices for the 1st polygon
      ; Iterate over the resulting overlap polygon(s) for computing the total overlap area
    for c = 0L, numOvrPolys-1 do begin
      np = ovr[polyStart - 1]
      pOvrArea += polyArea(XCOORDS = reverse(ovr[polyStart      : polyStart +   np - 1]),$
                           YCOORDS = reverse(ovr[polyStart + np : polyStart + 2*np - 1]))
      ;window, 15, XSIZE = 750, YSIZE = 700
      ;plot, ovr[polyStart : polyStart+np-1], ovr[polyStart+np : polyStart+2*np-1]
      polyStart += 2*np + 1
    endfor
  endif
    ; Now calculate and return the Dice coefficient
  return, 2.0 * pOvrArea / (p1area + p2area)
end 


; Jaccard similarity / Tanimoto coefficient
function calcJaccardSimCoefficientForPolygons, p1x, p1y, p2x, p2y
  pOvrArea = 0.0
  res = calcPolygonIntersection(P1X=p1x, P1Y=p1y, P2X=p2x, P2Y=p2y)
  numOvrPolys = res[0]

  if (numOvrPolys gt 0) then begin
    polyStart = 2 ; start from 2, since ovr[1] contains the number of vertices for the 1st polygon
      ; Iterate over the resulting overlap polygon(s) for computing the total overlap area
    for c = 0L, numOvrPolys-1 do begin
      np = res[polyStart - 1]
      pOvrArea += polyArea(XCOORDS = reverse(res[polyStart      : polyStart +   np - 1]),$
                           YCOORDS = reverse(res[polyStart + np : polyStart + 2*np - 1]))
      ;window, 15, XSIZE = 750, YSIZE = 700
      ;plot, ovr[polyStart : polyStart+np-1], ovr[polyStart+np : polyStart+2*np-1]
      polyStart += 2*np + 1
    endfor
  endif

  pUnionArea = 0.0
  res = calcPolygonUnion(P1X=p1x, P1Y=p1y, P2X=p2x, P2Y=p2y)
  numUnionPolys = res[0]

  if (numUnionPolys gt 0) then begin
    polyStart = 2 ; start from 2, since ovr[1] contains the number of vertices for the 1st polygon
      ; Iterate over the resulting overlap polygon(s) for computing the total overlap area
    for c = 0L, numUnionPolys-1 do begin
      np = res[polyStart - 1]
      pUnionArea += polyArea(XCOORDS = reverse(res[polyStart      : polyStart +   np - 1]),$
                             YCOORDS = reverse(res[polyStart + np : polyStart + 2*np - 1]))
      ;window, 15, XSIZE = 750, YSIZE = 700
      ;plot, ovr[polyStart : polyStart+np-1], ovr[polyStart+np : polyStart+2*np-1]
      polyStart += 2*np + 1
    endfor
  endif

  return, pOvrArea / pUnionArea
end


; calcNSDfromPolygonToPolygon
;  NSD (normalized sum of square distances) computation function.
;
; NOTE:
;  This is not a metric, as it is not symmetric nor it does satisfies the
;  triangle inequality.
;
; REFERENCE:
;  Coelho L, Shariff A, Murphy R (2009) Nuclear segmentation in microscope cell
;  images: A hand-segmented dataset and comparison of algorithms.
;  In Proceedings of the 6th IEEE International Symposium on Biomedical Imaging
;  (ISBI 2009): From Nano to Macro, pages 518-521.
function calcNSDfromPolygonToPolygon, p1x, p1y, p2x, p2y, fPlotWindow = fPlotWindow, winCaption = winCaption

  n1 = n_elements(p1x)
  n2 = n_elements(p2x)
  if (n1 eq 0) or (n1 ne n_elements(p1y)) or (n2 eq 0) or (n2 ne n_elements(p2y)) $
  then return, -1.0

  nsd = 0.0

  ; Polygon drawing window.
  window, 14
  ; Set x|y range
  offset = 30
  p1xMin = min(p1x) - offset
  p1xMax = max(p1x) + offset
  ;p1yMin = min(p1y) le 10 ? 0 : min(p1y)-10
  p1yMin = min(p1y) - offset
  p1yMax = max(p1y) + offset
  p2xMin = min(p2x) - offset
  p2xMax = max(p2x) + offset
  p2yMin = min(p2y) - offset
  p2yMax = max(p2y) + offset
  xMin = min([p1xMin, p2xMin])
  xMax = max([p1xMax, p2xMax])
  yMin = min([p1yMin, p2yMin])
  yMax = max([p1yMax, p2yMax])

  ; Plot polygon
  plot, p1x, p1y, xrange = [xMin, xMax], yrange = [yMin, yMax], xstyle =1, ystyle = 1, xmargin = [0,0], ymargin = [0,0], xticks = 1, yticks = 1
  ;"Clean" style plot... xrange = xrange, yrange = yrange, xstyle = 1, ystyle =1, xmargin = [0,0], ymargin = [0,0], xticks = 1, yticks = 1

  ; Read image.
  roiR = tvrd()
  ; Get image size(var, /DIM)->
  tmp = size(roiR, /DIMENSIONS)
  nCol = min(tmp)
  nRow = max(tmp)

  roiR[0,*] = 0 ; Set left margin to 0 
  roiR[*,0] = 0 ; Set bottom margin to 0

  ; Compute distances from reference ROI R
  rDist = morph_distance(255 - roiR, neighbor_sampling = 3)
  ;window
  ;tvscl, rDist

  ;;;; Get the pixels from each region
  window, 14
  plot, p1x, p1y, xrange = [xMin,xMax], yrange = [yMin,yMax], xstyle = 1, ystyle =1, xmargin = [0,0], ymargin = [0,0], xticks = 1, yticks = 1
  polyfill, p1x, p1y
  regionR = tvrd()
  regionR[0,*] = 0
  regionR[*,0] = 0
  plot, p2x, p2y, xrange = [xMin,xMax], yrange = [yMin,yMax], xstyle = 1, ystyle =1, xmargin = [0,0], ymargin = [0,0], xticks = 1, yticks = 1;, /NOERASE
  polyfill, p2x, p2y
  regionS = tvrd()
  regionS[0,*] = 0
  regionS[*,0] = 0

  whR = where(regionR gt 0)
  whS = where(regionS gt 0)

  ;;;; Union
  roiUnion = bytArr(nRow, nCol)
  roiUnion[whS] = 255
  roiUnion[whR] = 255

  ;;;; Intersection
  roiInter = bytArr(nRow,nCol)
  roiInter = regionS and regionR

  ;; Union minus Intersection distances
  roiUniInter = roiUnion-roiInter
  whUniInter = where(roiUniInter gt 0)
  if whUniInter[0] ne -1 then begin
    distUniInter = bytArr(nRow,nCol)
    distUniInter[whUniInter] = rDist[whUniInter]
    sumUniInter = total(distUniInter)
  endif else begin
    sumUniInter = 0
  endelse

  ;; Union distances
  whUnion   = where(roiUnion gt 0)
  distUnion = bytArr(nRow, nCol)
  distUnion[whUnion] = rDist[whUnion]
  sumUnion = total(distUnion)

  ;;;; NSD
  nsd = float(sumUniInter) / float(sumUnion)

  if keyword_set(fPlotWindow) then begin
    winTitle   = 'Polygon overlay'
    if (n_elements(winCaption) gt 0) then winTitle += ' - ' + winCaption
    xPolyRange = xMax - xMin
    yPolyRange = yMax - yMin
    maxRange   = xPolyRange > yPolyRange
    if ~keyword_set(winFactor) then begin
      case 1 of
        maxRange lt 50 : winFactor = 10.0
        maxRange lt 100: winFactor = 7.0
        maxRange lt 200: winFactor = 3.5
        maxRange lt 300: winFactor = 2.3
        else: maxRange = 1.0
      endcase
    endif else winFactor >= 1.0
    xWinSize = xPolyRange * winFactor
    yWinSize = yPolyRange * winFactor
    window, 14, title = winTitle, xSize = xWinSize, ySize = yWinSize
    plot, p1x, p1y, xrange = [xMin, xMax], yrange = [yMin, yMax], xstyle = 1, ystyle = 1, xmargin = [0,0], ymargin = [0,0], xticks = 1, yticks = 1, linestyle = 1
    oPlot, p2x, p2y, linestyle = 0
    stop
  endif

  return, nsd
end


pro calcNSDfromPolygonToPolygon_test
  print, 'Initializing Test for NSD...'
  f1 = 1
  f2 = 1
  offset1 = 150
  ; roi 1
  p1x = [3.0, 5.0, 8.0, 10.0, 10.0, 9.0, 9.0, 4.0, 3.0, 3.0]*f1 ;+ offset1 
  p1y = [4.0, 2.0, 2.0, 4.0, 7.0, 8.0, 9.0, 9.0, 8.0, 4.0]*f1 ;+ 30
  ; roi 2
  p2x = [1.0, 3.0, 6.0, 7.0, 7.0, 5.0, 2.0, 1.0, 1.0]*f2
  p2y = [4.0, 2.0, 2.0, 3.0, 7.0, 9.0, 9.0, 8.0, 4.0]*f2 ;+ 30
  NSD = calcNSDfromPolygonToPolygon(p1x,p1y, p2x,p2y)
  print, 'NSD :', NSD
  
  ; roi 1 = roi 2 -> NSD = 0
  p1x = [3.0, 5.0, 8.0, 10.0, 10.0, 9.0, 9.0, 4.0, 3.0, 3.0]*f1 ;+ offset1 
  p1y = [4.0, 2.0, 2.0, 4.0, 7.0, 8.0, 9.0, 9.0, 8.0, 4.0]*f1 ;+ 30
  ; roi 2
  p2x = p1x
  p2y = p1y
  NSD = calcNSDfromPolygonToPolygon(p1x,p1y, p2x,p2y)
  print, 'NSD :', NSD
  
  ; rois que no intersectan -> NSD = 1
  p1x = [3.0, 5.0, 8.0, 10.0, 10.0, 9.0, 9.0, 4.0, 3.0, 3.0]*f1 ;+ offset1 
  p1y = [4.0, 2.0, 2.0, 4.0, 7.0, 8.0, 9.0, 9.0, 8.0, 4.0]*f1 ;+ 30
  ; roi 2
  p2x = [3.0, 5.0, 8.0, 10.0, 10.0, 9.0, 9.0, 4.0, 3.0, 3.0]*f1+10
  p2y = [4.0, 2.0, 2.0, 4.0, 7.0, 8.0, 9.0, 9.0, 8.0, 4.0]*f1
  NSD = calcNSDfromPolygonToPolygon(p1x,p1y, p2x,p2y)
  print, 'NSD :', NSD
  
  
end


; calcPolyOverlapInROIgroup
;
; ARGUMENTS
;   oRoiGroup : a C_sROI3DGroupObject or C_sROIGroupObject instance.
;
; HISTORY
;   First version. J. Jara (08.2012).
;
; NOTES
;   C_sROI3DObject not tested yet.
;
pro calcPolyOverlapInROIgroup, oRoiGroup, intersectionRegionCount = intersectionRegionCount,$
                                          intersectionObjectCount = intersectionObjectCount,$
                                          intersectionSize = intersectionSize,$
                                          intersectionFraction = intersectionFraction
  nObj = oRoiGroup->count()
  intersectionRegionCount = make_array(nObj, /float)
  intersectionObjectCount = make_array(nObj, /float)
  intersectionSize        = make_array(nObj, /float)
  intersectionFraction    = make_array(nObj, /float)

  if (nObj le 1) then return
  roiSize                 = make_array(nObj, /float)

  xyBoxes = fltArr(nObj, 4) ; [xmin, xmax, ymin, ymax] for each object
  xCoords = ptrArr(nObj)
  yCoords = ptrArr(nObj)

  for i = 0L, nObj-1 do begin
    oRoi           = oRoiGroup->get(position = i)
    pBorderPolygon = oROI->getpObjectBorderPolygonList()
    j = 0 ; TODO polygons without holes only
    ;nPolygons      = (size(pBorderPolygon, /dim))[0]
    ;for j = 0, nPolygons-1 do begin
      xCoords[i] = ptr_new(reform((*pBorderPolygon[j])[0,*]))
      yCoords[i] = ptr_new(reform((*pBorderPolygon[j])[1,*]))
      roiSize[i] = polyArea(xCoords = *xCoords[i], yCoords = *yCoords[i])
      xMax = max(*xCoords[i], min = xMin)
      yMax = max(*yCoords[i], min = yMin)
      xyBoxes[i,0] = xMin
      xyBoxes[i,1] = xMax
      xyBoxes[i,2] = yMin
      xyBoxes[i,3] = yMax
    ;endfor
  endfor

  t1 = sysTime(1)

  ; Now check and "correct" overlaps
  if (nObj gt 1) then $
    for i = 0L, nObj - 2 do begin
      xLo = xyBoxes[i,0]
      xHi = xyBoxes[i,1]
      yLo = xyBoxes[i,2]
      yHi = xyBoxes[i,3]

      for j = i+1, nObj-1 do begin
        ; Check with bounding boxes to discard some exhaustive distance checkings
        fInX = ((xyBoxes[j,1] ge xLo) and (xyBoxes[j,0] le xLo)) $
          or ((xyBoxes[j,0] le xHi) and (xyBoxes[j,1] ge xLo))
        fInY = ((xyBoxes[j,3] ge yLo) and (xyBoxes[j,2] le yLo)) $
          or ((xyBoxes[j,2] le yHi) and (xyBoxes[j,3] ge yLo))

        fInXY = fInX and fInY

        if (fInXY eq 1) then begin
          print, 'cheking points of possibly overlapping ROIs ', i, '-', j

          res = calcPolygonIntersection(p1x = *xCoords[i], p1y = *yCoords[i], p2x = *xCoords[j], p2y = *yCoords[j])
          numIntPolys = res[0]

          if (numIntPolys ge 1) then begin
            intersectionRegionCount[i] += numIntPolys
            intersectionObjectCount[i] += 1
            intersectionRegionCount[j] += numIntPolys
            intersectionObjectCount[j] += 1
            intArea = 0.0
            polyStart = 2 ; start from 2, since ...[1] contains the number of vertices for the 1st polygon
            ; Iterate over the resulting overlap polygon(s) for computing the total overlap area
            for c = 0L, numIntPolys-1 do begin
              np = res[polyStart - 1]
              intArea += polyArea(xCoords = reverse(res[polyStart      : polyStart +   np - 1]), yCoords = reverse(res[polyStart + np : polyStart + 2*np - 1]))
              ;window, 15, XSIZE = 750, YSIZE = 700
              ;plot, ovr[polyStart : polyStart+np-1], ovr[polyStart+np : polyStart+2*np-1]
              polyStart += 2*np + 1
            endfor

            intersectionSize[i] += intArea
            intersectionSize[j] += intArea
          endif
        endif
      endfor

      if (intersectionSize[i] gt 0) then intersectionFraction[i] = intersectionSize[i] / roiSize[i]
    endfor

    if (intersectionSize[nObj-1] gt 0) then intersectionFraction[nObj-1] = intersectionSize[nObj-1] / roiSize[nObj-1]
    ; free memory of the bboxes by assigning a scalar value to the variable
    xyBoxes = -1
    t2 = systime(1)
end


; calcROIoverlapInROIgroup
;
; ARGUMENTS
;   oRoiGroup : a C_sROI3DGroupObject or C_sROIGroupObject instance.
;
; HISTORY
;   First version. J. Jara (08.2012).
;
; NOTES
;   C_sROI3DObject not tested yet.
;
pro calcROIoverlapInROIgroup, oRoiGroup, intersectionRegionCount = intersectionRegionCount,$
                                         intersectionObjectCount = intersectionObjectCount,$
                                         intersectionSize        = intersectionSize,$
                                         intersectionFraction    = intersectionFraction
  nObj = oRoiGroup->count()
  if (nObj lt 1) then return

  ; if there are more than two objects, check for overlaps and...
  ; optional: flag for compute and use of axis aligned bounding boxes
  pBP      = ptrArr(nObj)
  ;pBPoly   = ptrArr(nObj)

  roiSize  = uintArr(nObj, /NOZERO)
  intersectionRegionCount = make_array(nObj, /float)
  intersectionObjectCount = make_array(nObj, /float)
  intersectionSize        = make_array(nObj, /float)
  intersectionFraction    = make_array(nObj, /float)

  for i = 0L, nObj-1 do begin
    oRoi = oRoiGroup->get(position = i)
    case 1 of
     obj_isa(oRoi, 'C_sROI3DObject'): begin
       obj        = oRoi->makePixelObjectInVoxel(/all)
       xyzPoints  = oRoi->getXyzPoints()
       roiSize[i] = n_elements(xyzPoints[0,*])
       pBP[i]     = ptr_new(xyzPoints, /no_copy)
       ; ALT: use surface mesh model
       ;shade_volume, obj.obj, 0, vertices, polygons, /low
       ;vertices[0,*] += (obj.minX - obj.pixelFrame)
       ;vertices[1,*] += (obj.minY - obj.pixelFrame)
       ;vertices[2,*] += (obj.minZ - obj.pixelFrame)
       ;pBP[i]         = ptr_new(vertices, /no_copy)
       endcase
     obj_isa(oRoi, 'C_sROIObject'): begin
       xyzPoints      = oRoi->getXyzPoints()
       roiSize[i]     = n_elements(xyzPoints[0,*])
       ;pParamStruct   = oRoiGroup->getpParamStruct()
       ;xyzPoints[2,*] = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Slice Position'))[0]]
       pBP[i]         = ptr_new(xyzPoints, /no_copy)
     endcase
     endcase
  endfor

  ; Compute bounding boxes in xyz for each object, in order to avoid comparisons between "coarsely" distant objects
  fUseBBoxes = nObj gt 1
  if (fUseBBoxes eq 1) then begin
    ; TODO JJ check/change array structure to improve access times
    xyzBoxes = fltArr(6, nObj) ; [xmin, ymin, zmin, xmax, ymax, zmax] for each object
    for i = 0L, nObj-1 do begin
      ;roiI = (self->get(position = i))
      (oRoiGroup->get(position = i))->getProperty, ROI_XRANGE = xr, ROI_YRANGE = yr, ROI_ZRANGE = zr
      xyzBoxes[0,i] = xr[0] ; min and max for x coords
      xyzBoxes[3,i] = xr[1]
      xyzBoxes[1,i] = yr[0] ; min and max for y coords
      xyzBoxes[4,i] = yr[1]
      xyzBoxes[2,i] = zr[0] ; min and max for z coords
      xyzBoxes[5,i] = zr[1]
    endfor
  endif

  t1 = sysTime(1)
  ; Now check and "correct" overlaps
  if (nObj gt 1) then $
  for i = 0L, nObj - 2 do begin

    xLo = xyzBoxes[0,i]
    xHi = xyzBoxes[3,i]
    yLo = xyzBoxes[1,i]
    yHi = xyzBoxes[4,i]
    zLo = xyzBoxes[2,i]
    zHi = xyzBoxes[5,i]
    for j = i+1, nObj-1 do begin
      fInXYZ = 1
      ; Check with bounding boxes to discard some exhaustive distance checkings
      fInX = ((xyzBoxes[3,j] ge xLo) and (xyzBoxes[0,j] le xLo)) $
          or ((xyzBoxes[0,j] le xHi) and (xyzBoxes[3,j] ge xLo))
      fInY = ((xyzBoxes[4,j] ge yLo) and (xyzBoxes[1,j] le yLo)) $
          or ((xyzBoxes[1,j] le yHi) and (xyzBoxes[4,j] ge yLo))
      fInZ = ((xyzBoxes[5,j] ge zLo) and (xyzBoxes[2,j] le zLo)) $
          or ((xyzBoxes[2,j] le zHi) and (xyzBoxes[5,j] ge zLo))
      fInXYZ = fInX and fInY and fInZ
      if (fInXYZ eq 1) then begin
        print, 'checking points of possibly overlapping ROIs ', i, '-', j
        points = (oRoiGroup->get(position = j))->containsPoints(*pBP[i])
        ; comparison returns 0 for points in the exterior, 1 in the interior and 2 in the edge
        pointsInside = where(points eq 1, countInside)
        pointsOnEdge = where((points eq 2) or (points eq 3), countOnEdge)
        totalIntCount = countInside + countOnEdge
        if (totalIntCount ge 1) then begin
          intersectionRegionCount[i] = -1 ;  TODO JJ implement these two with -1
          intersectionRegionCount[j] = -1
          intersectionObjectCount[i] += 1
          intersectionSize[i]        += totalIntCount
          intersectionObjectCount[j] += 1
          intersectionSize[j]        += totalIntCount
        endif
      endif
    endfor
    if (intersectionSize[i] gt 0) then intersectionFraction[i] = intersectionSize[i] / (size(*pBP[i], /dim))[1]
  endfor
  if (intersectionSize[nObj-1] gt 0) then intersectionFraction[nObj-1] = intersectionSize[nObj-1] / (size(*pBP[nObj-1], /dim))[1]
  ; free memory of the bboxes by assigning a scalar value to the variable
  if (fUseBBoxes eq 1) then xyzBoxes = -1
  t2 = systime(1)
  for i = 0L, nObj-1 do begin
    ptr_free, pBP[i]
  ;  ptr_free, pBPoly[i]
  endfor
end


; TODO Hector modifica aqui 03/10/2013
pro calcROIoverlapBetweenROIgroups, oRoiGroup1, oRoiGroup2,$
                                    intersectionRegionCount = intersectionRegionCount,$
                                    intersectionObjectCount = intersectionObjectCount,$
                                    intersectionSize        = intersectionSize,$
                                    intersectionFraction    = intersectionFraction,$
                                    fUseLib = fUseLib
  nObj1 = oRoiGroup1->count()
  if (nObj1 lt 1) then return
  nObj2 = oRoiGroup2->count()
  if (nObj2 lt 1) then return
  print, 'calcROIoverlapBetweenROIgroups - Checking ', nObj1, ' vs. ', nObj2, ' objects'
  ; if there are more than two objects, check for overlaps and...
  ; optional: flag for compute and use of axis aligned bounding boxes
  pBP1      = ptrArr(nObj1)
  pBP2      = ptrArr(nObj2)

  roiSize1  = uintArr(nObj1, /NOZERO)
  roiSize2  = uintArr(nObj2, /NOZERO)
  intersectionRegionCount = make_array(nObj1, /float)
  intersectionObjectCount = make_array(nObj1, /float)
  intersectionSize        = make_array(nObj1, /float)
  intersectionFraction    = make_array(nObj1, /float)

  for i = 0L, nObj1-1 do begin
    oRoi = oRoiGroup1->get(position = i)
    case 1 of
     obj_isa(oRoi, 'C_sROI3DObject'): begin
       obj         = oRoi->makePixelObjectInVoxel(/all)
       xyzPoints   = oRoi->getXyzPoints()
       roiSize1[i] = n_elements(xyzPoints[0,*])
       pBP1[i]     = ptr_new(xyzPoints, /no_copy)
       ; ALT: use surface mesh model
       ;shade_volume, obj.obj, 0, vertices, polygons, /low
       ;vertices[0,*] += (obj.minX - obj.pixelFrame)
       ;vertices[1,*] += (obj.minY - obj.pixelFrame)
       ;vertices[2,*] += (obj.minZ - obj.pixelFrame)
       ;pBP1[i]         = ptr_new(vertices, /no_copy)
       endcase
     obj_isa(oRoi, 'C_sROIObject'): begin
       xyzPoints       = oRoi->getXyzPoints()
       roiSize1[i]     = n_elements(xyzPoints[0,*])
       ;pParamStruct   = oRoiGroup->getpParamStruct()
       ;xyzPoints1[2,*] = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Slice Position'))[0]]
       pBP1[i]         = ptr_new(xyzPoints, /no_copy)
      endcase
    endcase
  endfor

  for i = 0L, nObj2-1 do begin
    oRoi = oRoiGroup2->get(position = i)
    case 1 of
     obj_isa(oRoi, 'C_sROI3DObject'): begin
       obj         = oRoi->makePixelObjectInVoxel(/all)
       xyzPoints   = oRoi->getXyzPoints()
       roiSize2[i] = n_elements(xyzPoints[0,*])
       pBP2[i]     = ptr_new(xyzPoints, /no_copy)
       ; ALT: use surface mesh model
       ;shade_volume, obj.obj, 0, vertices, polygons, /low
       ;vertices[0,*] += (obj.minX - obj.pixelFrame)
       ;vertices[1,*] += (obj.minY - obj.pixelFrame)
       ;vertices[2,*] += (obj.minZ - obj.pixelFrame)
       ;pBP2[i]         = ptr_new(vertices, /no_copy)
       endcase
     obj_isa(oRoi, 'C_sROIObject'): begin
       xyzPoints       = oRoi->getXyzPoints()
       roiSize2[i]     = n_elements(xyzPoints[0,*])
       ;pParamStruct   = oRoiGroup->getpParamStruct()
       ;xyzPoints[2,*] = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Slice Position'))[0]]
       pBP2[i]         = ptr_new(xyzPoints, /no_copy)
     endcase
     endcase
  endfor

  ; Compute bounding boxes in xyz for each object, in order to avoid comparisons between "coarsely" distant objects
  fUseBBoxes = (nObj1 ge 1) and (nObj2 ge 1) ;[JJ] This is arbitrary... yet
  if (fUseBBoxes eq 1) then begin
    ; TODO JJ check/change array structure to improve access times
    xyzBoxes1 = fltArr(6, nObj1) ; [xmin, ymin, zmin, xmax, ymax, zmax] for each object
    for i = 0L, nObj1-1 do begin
      ;roiI = (self->get(position = i))
      (oRoiGroup1->get(position = i))->getProperty, ROI_XRANGE = xr, ROI_YRANGE = yr, ROI_ZRANGE = zr
      xyzBoxes1[0,i] = xr[0] ; min and max for x coords
      xyzBoxes1[3,i] = xr[1]
      xyzBoxes1[1,i] = yr[0] ; min and max for y coords
      xyzBoxes1[4,i] = yr[1]
      xyzBoxes1[2,i] = zr[0] ; min and max for z coords
      xyzBoxes1[5,i] = zr[1]
      ;print, 'In Group 1 [', i, '] xr ', xr, ' yr ', yr, ' zr ', zr
    endfor
    xyzBoxes2 = fltArr(6, nObj2) ; [xmin, ymin, zmin, xmax, ymax, zmax] for each object
    for i = 0L, nObj2-1 do begin
      ;roiI = (self->get(position = i))
      (oRoiGroup2->get(position = i))->getProperty, ROI_XRANGE = xr, ROI_YRANGE = yr, ROI_ZRANGE = zr
      xyzBoxes2[0,i] = xr[0] ; min and max for x coords
      xyzBoxes2[3,i] = xr[1]
      xyzBoxes2[1,i] = yr[0] ; min and max for y coords
      xyzBoxes2[4,i] = yr[1]
      xyzBoxes2[2,i] = zr[0] ; min and max for z coords
      xyzBoxes2[5,i] = zr[1]
      ;print, 'In Group 2 [', i, '] xr ', xr, ' yr ', yr, ' zr ', zr
    endfor
  endif

  ;t1 = sysTime(1)
  ; Now check and "correct" overlaps
  ;if (fUseBBoxes eq 1) then $
  ;TODO HECTOR, aqui!!!!!!
  if ~keyword_set(fUseLib) then begin
    for i = 0L, nObj1 - 1 do begin

      xLo = xyzBoxes1[0,i]
      xHi = xyzBoxes1[3,i]
      yLo = xyzBoxes1[1,i]
      yHi = xyzBoxes1[4,i]
      zLo = xyzBoxes1[2,i]
      zHi = xyzBoxes1[5,i]
      oRoiI = (oRoiGroup1->get(position = i))

      for j = 0, nObj2-1 do begin
        fInXYZ = 1
        ; Check with bounding boxes to discard some exhaustive distance checkings
        fInX = (xLo le (xyzBoxes2[0,j]) and (xyzBoxes2[0,j] le xHi)) $
            or ((xyzBoxes2[0,j] le xLo) and (xyzBoxes2[3,j] ge xLo)) $
            or ((xLo le xyzBoxes2[0,j]) and (xyzBoxes2[3,j] le xHi))
        fInY = (yLo le (xyzBoxes2[1,j]) and (xyzBoxes2[1,j] le yHi)) $
            or ((xyzBoxes2[1,j] le yLo) and (xyzBoxes2[4,j] ge yLo)) $
            or ((yLo le xyzBoxes2[1,j]) and (xyzBoxes2[4,j] le yHi))
        fInZ = (zLo le (xyzBoxes2[2,j]) and (xyzBoxes2[2,j] le zHi)) $
            or ((xyzBoxes2[2,j] le zLo) and (xyzBoxes2[5,j] ge zLo)) $
            or ((zLo le xyzBoxes2[2,j]) and (xyzBoxes2[5,j] le zHi))
        fInXYZ = fInX and fInY and fInZ

        if (fInXYZ eq 1) then begin
          ;print, 'Checking points of possibly overlapping ROIs ', i, ' (group 1) - ', j, ' (group 2)'
          fInt = intersectionXYZpoints(*pBP1[i], *pBP2[j])
          ;points = oRoiI->containsPoints(*pBP2[j])
          ;; comparison returns 0 for points in the exterior, 1 in the interior and 2 in the edge
          ;pointsInside = where(points eq 1, countInside)
          ;pointsOnEdge = where((points eq 2) or (points eq 3), countOnEdge)
          ;totalIntCount = countInside + countOnEdge
          ;if (totalIntCount ge 1) then begin
          if (fInt ge 1) then begin
            print, 'Intersection found for ROIs ', i, ' (group 1) - ', j, ' (group 2)'
            ;intersectionRegionCount[i] = -1 ;  TODO quantify size i/j
            ;intersectionRegionCount[j] = -1
            intersectionObjectCount[i] += 1
            ;intersectionSize[i]        += totalIntCount ; TODO discard duplicated voxels... multiple intersections for a given voxel
            ;intersectionObjectCount[j] += 1
            ;intersectionSize[j]        += totalIntCount
          endif
        endif
      endfor
    endfor
  endif else begin
    ; metodo realizado por hector solo compara un grupo entre si, en este caso hay que elegir cual de los grupos tiene menos elementos y sobre ese iterar
    ; comparandolo con el otro grupo
    if (nObj1 lt nObj2) then begin
      dim = 3
      nPairs = -1
      pairList = lonarr(nObj1*nObj2/2)
      szBoxes2 = size(xyzBoxes2, /dim)
      cajas = fltArr(szBoxes2 + [0, 1])
      cajas[*, 0:nObj2-1] = xyzBoxes2

      for i = 0L, nObj1 - 1 do begin
        cajas[0, nObj2] = xyzBoxes1[0,i]
        cajas[3, nObj2] = xyzBoxes1[3,i]
        cajas[1, nObj2] = xyzBoxes1[1,i]
        cajas[4, nObj2] = xyzBoxes1[4,i]
        cajas[2, nObj2] = xyzBoxes1[2,i]
        cajas[5, nObj2] = xyzBoxes1[5,i]
        oRoiI = (oRoiGroup1->get(position = i))

        dllLoc = getDLLfilename('SweepAndPrune3D', /GETFILENAMEANDPATH)
        print, dllLoc
        result = call_external(dllLoc, 'find_intersections3D', fix(cajas, type=4), fix(szBoxes, type=2), 3, pairList, nPairs, /unload)
      endfor
    endif else begin
      dim = 3
      nPairs = -1
      pairList = lonarr(nObj1*nObj2/2)

      for i = 0L, nObj2 - 1 do begin
        szBoxes1 = size(xyzBoxes1, /dim)
        cajas = fltArr(szBoxes1 + [0, 1])
        cajas[*, 0:nObj1-1] = xyzBoxes1
        cajas[0, nObj1] = xyzBoxes2[0,i]
        cajas[3, nObj1] = xyzBoxes2[3,i]
        cajas[1, nObj1] = xyzBoxes2[1,i]
        cajas[4, nObj1] = xyzBoxes2[4,i]
        cajas[2, nObj1] = xyzBoxes2[2,i]
        cajas[5, nObj1] = xyzBoxes2[5,i]
        oRoiI = (oRoiGroup1->get(position = i))

        dllLoc = getDLLfilename('SweepAndPrune3D', /GETFILENAMEANDPATH)
        print, dllLoc
        result = call_external(dllLoc, 'find_intersections3D', fix(cajas, type=4), fix(nBoxes, type=2), 3, pairList, nPairs, /unload)
      endfor
    endelse
  endelse

  ;if (intersectionSize[i] gt 0) then intersectionFraction[i] = intersectionSize[i] / (size(*pBP1[i], /dim))[1]
  ;endfor

  if (intersectionSize[nObj1-1] gt 0) then intersectionFraction[nObj1-1] = intersectionSize[nObj1-1] / (size(*pBP[nObj1-1], /dim))[1]
  ; free memory of the bboxes by assigning a scalar value to the variable
  if (fUseBBoxes eq 1) then xyzBoxes = -1
  t2 = systime(1)
  for i = 0L, nObj1-1 do begin
    ptr_free, pBP1[i]
  ;  ptr_free, pBPoly1[i]
  endfor
  for i = 0L, nObj2-1 do begin
    ptr_free, pBP2[i]
  ;  ptr_free, pBPoly2[i]
  endfor
  print, 'calcROIoverlapBetweenROIgroups - Finished'
end


; calcPolyOverlap
;
; ARGUMENTS
;   oRoiGroup : a C_sROI3DGroupObject or C_sROIGroupObject instance.
;
; HISTORY
;   First version. J. Jara (08.2012).
;
; NOTES
;   C_sROI3DObject not tested yet.
;
pro calcPolyOverlap, pXcoordsArr, pYcoordsArr, tPos, intersectionRegionCount = intersectionRegionCount,$
                                                     intersectionObjectCount = intersectionObjectCount,$
                                                     intersectionSize        = intersectionSize,$
                                                     intersectionFraction    = intersectionFraction
  nObj = n_elements(polyArr)
  if (nObj le 1) then return

  xyBoxes = fltArr(nObj, 4) ; [xmin, xmax, ymin, ymax] for each object

  roiSize                 = make_array(nObj, /float)
  intersectionRegionCount = make_array(nObj, /float)
  intersectionObjectCount = make_array(nObj, /float)
  intersectionSize        = make_array(nObj, /float)
  intersectionFraction    = make_array(nObj, /float)

  for i = 0L, nObj-1 do begin
    ; for j... TODO
    j = 0
    xCoords = (*(pXcoordsArr[tPos,i]))[j]
    yCoords = (*(pYcoordsArr[tPos,i]))[j]
    roiSize[i] = polyArea(xCoords = xCoords, yCoords = yCoords)
    xMax = max(xCoords, min = xMin)
    yMax = max(yCoords, min = yMin)
    xyBoxes[i,0] = xMin
    xyBoxes[i,1] = xMax
    xyBoxes[i,2] = yMin
    xyBoxes[i,3] = yMax
    ;endfor
  endfor
  t1 = sysTime(1)
  ; Now check and "correct" overlaps
  if (nObj gt 1) then $
  for i = 0L, nObj - 2 do begin

    xLo = xyBoxes[i,0]
    xHi = xyBoxes[i,1]
    yLo = xyBoxes[i,2]
    yHi = xyBoxes[i,3]
    for j = i+1, nObj-1 do begin
      ; Check with bounding boxes to discard some exhaustive distance checkings
      fInX = ((xyBoxes[j,1] ge xLo) and (xyBoxes[j,0] le xLo)) $
          or ((xyBoxes[j,0] le xHi) and (xyBoxes[j,1] ge xLo))
      fInY = ((xyBoxes[j,3] ge yLo) and (xyBoxes[j,2] le yLo)) $
          or ((xyBoxes[j,2] le yHi) and (xyBoxes[j,3] ge yLo))
      fInXY = fInX and fInY
      if (fInXY eq 1) then begin
        print, 'cheking points of possibly overlapping ROIs ', i, '-', j
        res = calcPolygonIntersection(p1x = (*(pXcoordsArr[tPos,i]))[0], p1y = (*(pYcoordsArr[tPos,i]))[0],$
                                      p2x = (*(pXcoordsArr[tPos,j]))[0], p2y = (*(pXcoordsArr[tPos,j]))[0])
        numIntPolys = res[0]
        if (numIntPolys ge 1) then begin
          intersectionRegionCount[i] += numIntPolys
          intersectionObjectCount[i] += 1
          intersectionRegionCount[j] += numIntPolys
          intersectionObjectCount[j] += 1
          intArea = 0.0
          polyStart = 2 ; start from 2, since ...[1] contains the number of vertices for the 1st polygon
            ; Iterate over the resulting overlap polygon(s) for computing the total overlap area
          for c = 0L, numIntPolys-1 do begin
            np = res[polyStart - 1]
            intArea += polyArea(xCoords = reverse(res[polyStart      : polyStart +   np - 1]),$
                                yCoords = reverse(res[polyStart + np : polyStart + 2*np - 1]))
            ;window, 15, XSIZE = 750, YSIZE = 700
            ;plot, ovr[polyStart : polyStart+np-1], ovr[polyStart+np : polyStart+2*np-1]
            polyStart += 2*np + 1
          endfor
          intersectionSize[i] += intArea
          intersectionSize[j] += intArea
        endif
      endif
    endfor
    if (intersectionSize[i] gt 0) then intersectionFraction[i] = intersectionSize[i] / roiSize[i]
  endfor
  if (intersectionSize[nObj-1] gt 0) then intersectionFraction[nObj-1] = intersectionSize[nObj-1] / roiSize[nObj-1]
  ; free memory of the bboxes by assigning a scalar value to the variable
  xyBoxes = -1
  t2 = systime(1)
end


; polygonArcSample
;
; Takes a closed 2D curve (polygon) and re-samples it in equal arc lengths.
;
; Parameters:
;   x             Input x-coordinates array.
;   y             Input x-coordinates array. It must have the same number of elements as x.
;   x_out         Output variable to store the interpolated x-coordinates.
;   y_out         Output variable to store the interpolated y-coordinates.
;   nPoints       Optional number of points in the output curve. Default value:50.
;   fCloseOutput  Optional keyword to specify that the output list of poitns must be closed, 
;                 i.e. set the last point with the same coordinates as the first.
; History
;   2013. SCIAN-Soft version. JJara.
;   Original version by David Fanning (www.idlcoyote.com). Active Contour IDL demo.

pro polygonArcSample, x, y, x_out, y_out, points = points, fCloseOutput = fCloseOutput, fPolyline = fPolyline

     ; Check parameters.
   if n_elements(points) eq 0 then points = 50

   if size(x, /n_dimensions) eq 2 then begin
     x_in = reform(x)
     y_in = reform(y)
   endif else begin
     x_in = x
     y_in = y
   endelse

   npts = n_elements(x_in)

     ; Make sure the curve is closed (first point same as last point).
   if keyword_set(fClose) then begin
     if (x_in[0] ne x_in[npts-1]) or (y_in[0] ne y_in[npts-1]) then begin
        x_in = [x_in, x_in[0]]
        y_in = [y_in, y_in[0]]
        ;print, 'Active contour interpolation warning: adding 1 point to close the contour, according to the specified input flag.'
        npts += 1
     endif
   endif else points -= 1

      ; Interpolate very finely.
   nc = (npts - 1) * 100
   t  = dIndGen(npts)
   t1 = dIndGen(nc + 1) / 100
   x1 = spl_interp(t, x_in, spl_init(t, x_in), t1)
   y1 = spl_interp(t, y_in, spl_init(t, y_in), t1)

   if keyword_set(fClose) then begin
     avgSlopeX = (x1[1]-x1[0] + x1[nc]-x1[nc-1]) / (t1[1]-t1[0]) * 0.5
     avgSlopeY = (y1[1]-y1[0] + y1[nc]-y1[nc-1]) / (t1[1]-t1[0]) * 0.5
     dx1 = spl_init(t, x_in, yp0 = avgSlopeX, ypn_1 = avgSlopeX)
     dy1 = spl_init(t, y_in, yp0 = avgSlopeY, ypn_1 = avgSlopeY)
   endif else begin
     avgSlopeX0 = (x1[1] - x1[0])    / (t1[1]-t1[0])
     avgSlopeX1 = (x1[nc]- x1[nc-1]) / (t1[nc]-t1[nc-1])
     avgSlopeY0 = (y1[1] - y1[0])    / (t1[1]-t1[0])
     avgSlopeY1 = (y1[nc]- y1[nc-1]) / (t1[nc]-t1[nc-1])
     dx1 = spl_init(t, x_in, yp0 = avgSlopeX0, ypn_1 = avgSlopeX1)
     dy1 = spl_init(t, y_in, yp0 = avgSlopeY0, ypn_1 = avgSlopeY1)
   endelse

   x1 = spl_interp(t, x_in, dx1, t1)
   y1 = spl_interp(t, y_in, dy1, t1)

      ; Compute cumulative path length.
   ds = sqrt((x1[1:*]-x1)^2 + (y1[1:*]-y1)^2)
   ss = [0d, total(ds, /cum)]

      ; Invert this curve, solve for TX, which should be evenly sampled in the arc length space.
   sx = dIndGen(points) * (ss[nc]/points)
   tx = spl_interp(ss, t1, spl_init(ss, t1), sx)

      ; Reinterpolate the original points using the new values of TX and optionally close the contour.
   if  keyword_set(fClose) then begin
     x_out = spl_interp(t, x_in, dx1, tx)
     y_out = spl_interp(t, y_in, dy1, tx)
     x_out = [x_out, x_out[0]]
     y_out = [y_out, y_out[0]]
   endif else begin
     x_out = [spl_interp(t, x_in, dx1, tx), x_in[npts-1]]
     y_out = [spl_interp(t, y_in, dy1, tx), y_in[npts-1]]
   endelse

end


; polygonLineSample
; Performs straight-line segment interpolation for a given input polygon or polyline.
; Optional vector with flag values associated to the input vertices is also interpolated.
pro polygonLineSample, x, y, x_out, y_out, nPointsPerPix = nPointsPerPix, fCloseOutput = fCloseOutput, fPolyline = fPolyline, $
                       flagVector = flagVector, interpFlagVector = interpFlagVector, fForceZeroFlagEnds = fForceZeroFlagEnds

  if (size(x, /n_dimensions) eq 2) then begin
    x_in = reform(x)
    y_in = reform(y)
  endif else begin
    x_in = x
    y_in = y
  endelse

  nSegments = n_elements(x)
  if (nSegments le 1) then return

  fFlagVector = n_elements(flagVector) eq n_elements(x)
  if fFlagVector then flag_in = flagVector

  segLimit = 2
  if keyword_set(fCloseOutput) then begin
    segLimit = 1
    x_in = [x_in, x_in[0]]
    y_in = [y_in, y_in[0]]
    if fFlagVector then flag_in = [flagVector, flagVector[0]]
  endif

  if fFlagVector then interpFlagVector = [flag_in[0]]
  x_out = [x_in[0]]
  y_out = [y_in[0]]
  if ~keyword_set(nPointsPerPix) then nPointsPerPix = 1
  for i = 0u, nSegments-segLimit do begin
    deltaX    = x_in[i+1] - x_in[i]
    deltaY    = y_in[i+1] - y_in[i]
    segLen    = sqrt(deltaX^2 + deltaY^2)
    nPtsSeg   = round(nPointsPerPix * segLen) + 1
    deltaXseg = deltaX / (nPtsSeg-1)
    deltaYseg = deltaY / (nPtsSeg-1)
    for j = 1, nPtsSeg-1 do begin
      x_out = [x_out, x_in[i] + j*deltaXseg]
      y_out = [y_out, y_in[i] + j*deltaYseg]
      if fFlagVector then begin
        flagVal = j eq (nPtsSeg-1) ? flag_in[i+1] : flag_in[i]
        if keyword_set(fForceZeroFlagEnds) then $
          if ((flag_in[i] eq 0) or (flag_in[i+1] eq 0)) then flagVal = 0
        interpFlagVector = [interpFlagVector, flagVal]
      endif
    endfor
  endfor
end


pro save1DArrayInASCIIFile, array, fileName
  openW, unit, fileName, /get_lun
  for i = 0L, n_elements(array)-1 do $
    printf, unit, strTrim(string(array[i]), 2)

  close, unit
  free_lun, unit
  print, 'Array saved in file ', fileName
end


; Sample usage: save1DArrayInASCIIFileTest, 'C:\RSI\save1DArrayInASCIIFileTest.txt'
pro save1DArrayInASCIIFileTest, fileName
  tmp = fIndGen(140)
  save1DArrayInASCIIFile, tmp, fileName
end


; testSAP2D_1
;
; boxes 4xn array containing object bounding boxes, in the format [xmin, ymin, xmax, ymax].
; dim   Number of spatial dimensions. Default: 2.
; listaParesOut Output list, containing the intersecting object pair indices.
pro testSAP2D_1, boxes = boxes, dim = dim, listaParesOut, cantParesOut
  if n_elements(boxes) eq 0 then begin
    boxes = [0, 0, 3.0, 1.0, 0, 0, 2.0, 2.0]
  endif
  if n_elements(dim) eq 0 then dim = 2
  nBoxes = n_elements(boxes) / (2*dim)
  if ~arg_present(cantParesOut) then cantParesOut = 0u
  if n_elements(listaParesOut) eq 0 then listaParesOut = uIntArr(nBoxes*nBoxes)
  listaParesOut = fix(listaParesOut, type=3)
  cantParesOut  = fix(cantParesOut, type=2)
  help,  boxes
  print, boxes 
  help, nBoxes
  help, dim

  dllLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections2D', fix(boxes, type = 4), $
                         fix(nBoxes, type = 2), fix(dim, type = 2), listaParesOut, cantParesOut, /unload)
end

;3 triangulos que se intersectan entre si
pro testSAP2D_2, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
 listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  if n_elements(listaPuntos) eq 0 then begin
    listaPuntos = [0, 2, -2, 0, 1, -2, $
                   0, -2, -1, 0, -2, -1, $
                   -1, -2, 2, -4, 1, 0]  
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 6, 12]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [3, 3, 3]
  endif
  
  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse 
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin 
    cantParesOut  = fix(cantParesOut, type=2)
  endelse
  
  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices
  
  nPuntos = n_elements(listaPuntos)/2
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections2D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut

end

; pentagono irregular y rombo cuyos vertices NO intersectan pero si sus cajas
pro testSAP2D_3, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  if n_elements(listaPuntos) eq 0 then begin
  	objA = [-2,-1, 1,-1, 0.5,0, -1,1, -2,0]
  	objB = [1,0, 2,1, 1,2, 0,1]
    listaPuntos = [objA, objB]  
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 10]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [5, 4]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices
  
  nPuntos = n_elements(listaPuntos)/2
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections2D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut
end

;2 cuadrilateros irregulares cuyas cajas NO intersectan
pro testSAP2D_4, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  if n_elements(listaPuntos) eq 0 then begin
    listaPuntos = [2, 1, 3, 2, 2, 3, 1, 2, $
                    0.5, 0, 0, 1, -2, 0, 0, -1]  
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 8]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [4, 4]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices
  
  nPuntos = n_elements(listaPuntos)/2
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections2D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut
end

;triangulo y pentagono irregular cuyas cajas tienen UNA ARISTA en comun
pro testSAP2D_5, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  if n_elements(listaPuntos) eq 0 then begin
    listaPuntos = [-3, -1, -1, 0, -2, 1, $
                    1, -2, 2, 0, 1, 3, -1, 1, 0, 1]  
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 6]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [3, 5]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices

  print, listaPuntos
  print, listaIndices
  print, cantidadVertices
  
  nPuntos = n_elements(listaPuntos)/2
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections2D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut
end

;triangulo y hexagono irregular con UN VERTICE en comun
pro testSAP2D_6, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  if n_elements(listaPuntos) eq 0 then begin
    listaPuntos = [1, 1, -1, 0, 0, -1, $
                    1, 1, 3, 2, 3, 3, 2, 4, 1, 3, 2, 3]  
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 6]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [3, 6]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices
  
  nPuntos = n_elements(listaPuntos)/2
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections2D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut
end

;prisma y tetraedro 3D que se intersectan -  Hector Moraga
pro testSAP3D_1, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  
  if n_elements(listaPuntos) eq 0 then begin
    objA = [1,1,1, 1,3,1, 3,3,1, 3,1,1, 1,1,5, 3,1,5, 3,3,4, 1,3,4]   ;prisma
    objB = [0,2,6, 2,2,4, 0,4,4, -2,4,4, 0,0,4, 0,2,2] ;tetraedro
    listaPuntos = [ objA, objB]  
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 24]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [8, 6]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices

  print, listaPuntos
  print, listaIndices
  print, cantidadVertices
  
  nPuntos = n_elements(listaPuntos)/3
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune3D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections3D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut

end

; tres objetos 3D que no se intersectan, pero si 2 de sus cajas - Hector Moraga
pro testSAP3D_2, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  
  if n_elements(listaPuntos) eq 0 then begin
  ;objeto A; objeto B; objeto C uno por linea
    objA = [0,0,0, 0,0,1, 0.9,0,0, 0.9,0,1, 2.9,2.1,0, 2.9,2.1,1, 3.9,2.1,0, 3.9,2.1,1, 3.9,3.1,0, 3.9,3.1,1, 5,3.1,0, 5,3.1,1, 0,4,0, 0,4,1, 5,4,0, 5,4,1]
    objB = [1,0,0, 1,0,1, 7,0,0, 7,0,1, 3,2,0, 3,2,1, 5,2,0, 5,2,1]
    objC = [4,3,0, 4,3,1, 8,3,0, 8,3,1, 4,2.1,0, 4,2.1,1, 5.1,2.1,0, 5.1,2.1,1, 7.1,0,0, 7.1,0,1, 8,0,0, 8,0,1]
    listaPuntos = [objA, objB, objC]
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 48, 72]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [16, 8, 12]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices

  nPuntos = n_elements(listaPuntos)/3
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune3D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections3D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut

end

; dos objetos 3D que solo tienen un vertice en comun - Hector Moraga
pro testSAP3D_3, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  
  if n_elements(listaPuntos) eq 0 then begin
  ;objeto A; objeto B, uno por linea
    objA = [1,2,1, 3,2,1, 3,4,1, 1,4,1, 1,3,3, 3,3,3, 3,4,3, 1,4,3]
    objB = [1,5,3, 3,5,3, 3,6,3, 1,6,3, 1,4,2, 3,4,2, 3,5,2, 1,5,2, 1,5,1, 3,5,1, 3,6,1, 1,6,1]
    listaPuntos = [ objA, objB]
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 24]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [8, 12]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices

  nPuntos = n_elements(listaPuntos)/3
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune3D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections3D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut

end

; dos objetos 3D el mismo caso del test 4 solo que el segundo objeto esta desplazado un pequeño delta
; por lo que ambas cajas no intersectan
pro testSAP3D_4, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  
  if n_elements(listaPuntos) eq 0 then begin
    objA = [2.1,1,1, 3.1,1,1, 3.1,2,1, 2.1,2,1, 2.1,2,2, 3.1,2,2, 3.1,3,2, 2.1,3,2, 2.1,1,3, 3.1,1,3, 3.1,2,3, 2.1,2,3]
    objB = [1,4,1, 2,4,1, 2,5,1, 1,5,1, 1,3,2, 2,3,2, 2,4,2, 1,4,2, 1,4,3, 2,4,3, 2,5,3, 1,5,3]
    listaPuntos = [objA, objB]
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 36]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [12, 12]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices

  nPuntos = n_elements(listaPuntos)/3
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune3D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections3D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut

end

; dos objetos 3D que solo tienen un punto en comun - Hector Moraga
pro testSAP3D_5, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 
  
  if n_elements(listaPuntos) eq 0 then begin
  ;objeto A; objeto B, uno por linea
    objA =[2,1,1, 3,1,1, 3,2,1, 2,2,1, 2,2,2, 3,2,2, 3,3,2, 2,3,2, 2,1,3, 3,1,3, 3,2,3, 2,2,3]
    objB =[1,4,1, 2,4,1, 2,5,1, 1,5,1, 1,3,2, 2,3,2, 2,4,2, 1,4,2, 1,4,3, 2,4,3, 2,5,3, 1,5,3]
    listaPuntos = [objA, objB]
  endif
  
  if n_elements(listaIndices) eq 0 then begin
    listaIndices = [0, 36]
  endif
  
  if n_elements(cantidadVertices) eq 0 then begin
    cantidadVertices = [12, 12]
  endif

  if n_elements(listaParesOut) eq 0 then begin
    listaParesOut = make_array(n_elements(cantidadVertices)*n_elements(cantidadVertices),1,VALUE=-1)
  endif else begin
    listaParesOut = fix(listaParesOut, type=2)
  endelse
  
  if n_elements(cantParesOut) eq 0 then begin
    cantParesOut = 0
  endif else begin
    cantParesOut  = fix(cantParesOut, type=2)
  endelse

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices

  nPuntos = n_elements(listaPuntos)/3
  nCantVertices = n_elements(cantidadVertices)
  
  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('SweepAndPrune3D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_intersections3D', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)

  print, cantParesOut
  print, listaParesOut
end


; Two intersecting parallelograms - Hector Moraga
pro testAABB2D_1, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 

  if n_elements(listaPuntos) eq 0 then begin
    objA = [0,0, 2,2, 5,2, 3,0]
    objB = [3,-1, 3,3, 5,1, 5,-3]
    listaPuntos = [objA, objB]  
  endif

  if n_elements(listaIndices) eq 0 then listaIndices = [0, 8]
  if n_elements(cantidadVertices) eq 0 then cantidadVertices = [4, 4]
  npts = n_elements(listaPuntos)
  listaParesOut = n_elements(listaParesOut) eq 0 ? intArr(npts*npts/8, 1)-1 : fix(listaParesOut, type=2)
  cantParesOut = n_elements(cantParesOut) eq 0 ? 0 : fix(cantParesOut, type=2)

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices

  nPuntos = npts/2
  nCantVertices = n_elements(cantidadVertices)

  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('AABBTreeCollision2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_Edge2D_intersections', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)
  print, cantParesOut
  print, listaParesOut
end


; Two NON-intersecting triangles - Hector Moraga
pro testAABB2D_2, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 

  if n_elements(listaPuntos) eq 0 then begin
    objA = [0,0, 1,3, -1,3]
    objB = [-2,-1, 0,-1, -1,1]
    listaPuntos = [objA, objB]  
  endif
  
  if n_elements(listaIndices) eq 0 then listaIndices = [0, 6]
  if n_elements(cantidadVertices) eq 0 then cantidadVertices = [3, 3]
  npts = n_elements(listaPuntos)
  listaParesOut = n_elements(listaParesOut) eq 0 ? intArr(npts*npts/8, 1)-1 : fix(listaParesOut, type=2)
  cantParesOut = n_elements(cantParesOut) eq 0 ? 0 : fix(cantParesOut, type=2)

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices

  nPuntos = npts/2
  nCantVertices = n_elements(cantidadVertices)

  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('AABBTreeCollision2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_Edge2D_intersections', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)
  print, cantParesOut
  print, listaParesOut
end


; Two polygons with at least ona common edge - Hector Moraga
; 10 intersections are returned, since a vertex is part of two edges (in closed polygons).
pro testAABB2D_3, listaPuntos = listaPuntos, listaIndices = listaIndices, cantidadVertices = cantidadVertices, $
  listaParesOut = listaParesOut, cantParesOut = cantParesOut 

  if n_elements(listaPuntos) eq 0 then begin
    objA = [0,0, 5,5, 2,5, 0,3]
    objB = [0,3, 2,5, 5,5, 1,7, -2,7]
    ;objC = [0,0, 0,3, -2,7, -3,3]
    listaPuntos = [objA, objB]  
  endif

  if n_elements(listaIndices) eq 0 then listaIndices = [0, 8]
  if n_elements(cantidadVertices) eq 0 then cantidadVertices = [4, 5]
  npts = n_elements(listaPuntos)
  listaParesOut = n_elements(listaParesOut) eq 0 ? intArr(npts*npts/8, 1)-1 : fix(listaParesOut, type=2)
  cantParesOut = n_elements(cantParesOut) eq 0 ? 0 : fix(cantParesOut, type=2)

  help, listaPuntos
  help, listaIndices
  help, cantidadVertices
  print, listaPuntos
  print, listaIndices
  print, cantidadVertices

  nPuntos = npts/2
  nCantVertices = n_elements(cantidadVertices)

  help, nPuntos
  help, nCantVertices

  dllLoc = getDLLfilename('AABBTreeCollision2D', /GETFILENAMEANDPATH)
  result = call_external(dllLoc, 'find_Edge2D_intersections', fix(listaPuntos, type=4), $
                         fix(nPuntos, type=2), fix(listaIndices, type=2), $
                         fix(cantidadVertices, type=2), fix(nCantVertices, type=2), $
                         listaParesOut, cantParesOut, /unload)
  print, cantParesOut
  print, listaParesOut
end


pro testPointInPolygon3D
  p1x = 1.0d
  p1y = 1.0d
  p1z = 1.0d
  poly1x = double([0.0, 0.0, 2.0])
  poly1y = double([0.0, 1.0, 2.0])
  poly1z = double([0.0, 1.0, 2.0])
  roi1 = obj_new('IDLanROI', poly1x, poly1y, poly1z, type = 2)
  isP1inPoly1 = roi1->containsPoints(p1x, p1y, p1z)
  print, 'Is p1 contained in polygon 1? ', (isP1inPoly1 ne 0) ? 'YES' : 'NO', '(', isP1inPoly1, ')'
end


function pathLength, vertices, indices, xyzSizePerPixel = xyzSizePerPixel

  if ~keyword_set(xyzSizePerPixel) then xyzSizePerPixel = [1.0, 1.0, 1.0]
  pathLength = 0.0
  n = n_elements(indices)-2

  for i = 0L, n do begin
    dx = (vertices[0, indices[i]] - vertices[0, indices[i+1]])^2
    dy = (vertices[1, indices[i]] - vertices[1, indices[i+1]])^2
    dz = (vertices[2, indices[i]] - vertices[2, indices[i+1]])^2
    pathLength += sqrt((xyzSizePerPixel[0]^2)*dx + (xyzSizePerPixel[1]^2)*dy + (xyzSizePerPixel[2]^2)*dz)
  endfor
  return, pathLength
end


; First an untested version (derived from the active contour code).
;
; http://mathworld.wolfram.com/Curvature.html
function getCurvaturesFromPoly, pXcoords, pYcoords, xyRes = xyRes
  if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
  if ~ptr_valid(pXcoords) then return, -1
  npts = n_elements(*pXcoords)
  gx  = xyRes[0] * shift((*pXcoords)[0:npts-1], -1) - (*pXcoords)[0:npts-1]
  gy  = xyRes[1] * shift((*pYcoords)[0:npts-1], -1) - (*pYcoords)[0:npts-1]
  gxx = xyRes[0] * shift((*pXcoords)[0:npts-1], 1) - 2 * (*pXcoords)[0:npts-1] + shift((*pXcoords)[0:npts-1], -1)
  gyy = xyRes[1] * shift((*pYcoords)[0:npts-1], 1) - 2 * (*pYcoords)[0:npts-1] + shift((*pYcoords)[0:npts-1], -1)
  curv = ((gx * gyy - gy * gxx) / (gx^2 + gy^2)^1.5) * 2 / sqrt(2)
  ;curv = ((gx * gyy - gy * gxx) / (gx^2 + gy^2)^1.5) ; http://mathworld.wolfram.com/Curvature.html
  whereNAN = where(finite(curv) ne 1)
  if (whereNAN[0] ne -1) then curv[whereNAN] = 0.
  return, curv
end


; First an untested version (derived from the active contour code).
function getInflexPointsFromPoly, pXcoords, pYcoords, curvArray = curvArray

  if ~ptr_valid(pXcoords) then return, -1

  if (n_elements(curvArray) eq 0) then curvArray = getCurvaturesFromPoly(pXcoords, pYcoords)

  szC = n_elements(curvArray)
  inflexArray = make_array(szC, /byte)
  flag  = 0
  count = 0
  for i = 0, szC-1 do begin
    case 1 of
      (curvArray[i] gt 0): begin
        if (flag eq -1) then begin
          inflexArray[i-(count/2)] = 1
          count = 0
        endif
        flag = 1
      endcase
      (curvArray[i] lt 0): begin
        if (flag eq 1) then begin
          inflexArray[i-(count/2)] = 2
          count = 0
        endif
        flag = -1
      endcase
      else: count += 1
    endcase
  endfor

  whereNe0 = where(inflexArray ne 0, countWhereNe0)
  if (countWhereNe0 gt 0) then begin
    case 1 of
      (inflexArray(whereNe0[0]) eq 1) and (flag eq  1):inflexArray[(i+whereNe0[0]-(count/2)) mod (szC-1)] = 1
      (inflexArray(whereNe0[0]) eq 2) and (flag eq -1):inflexArray[(i+whereNe0[0]-(count/2)) mod (szC-1)] = 2
    else:
    endcase
  endif

  return, inflexArray
end


function areaOfTriangle, x0, y0, x1, y1, x2, y2
  return, 0.5 * (((x1 - x0) * (y2 - y0)) - ((x2 - x0) * (y1 - y0)))
end


; voronoiPolygons
;
; Given a list of 2D points (x- and y-coordinates), computes and returns their 
; corresponding Voronoi polygons.
;
; xMaxCoord|xMaxCoord give the upper coordinate limit used to compute the Voronoi polygons.
; It is assumed that the lower coordinate limit is [0,0].
;
; areaSortedIndices If present, the polygon areas will be computed and the 
;                   sorted index list will be stored here.
; polyAreas         If present, the surface area will be computed for each polygon and stored here.
; fInterpolateLines If set, a straight line interpolation will be performed for each polygon segment.
; 
function voronoiPolygons, xPointCoords, yPointCoords, xMaxCoord = xMaxCoord, yMaxCoord = yMaxCoord, clipPolygonX = clipPolygonX, clipPolygonY = clipPolygonY, $
                          areaSortedIndices = areaSortedIndices, polyAreas = polyAreas, fInterpolateLines = fInterpolateLines, fClipConvexHull = fClipConvexHull,$
                          xPolyRasterPoints = xPolyRasterPoints, yPolyRasterPoints = yPolyRasterPoints
  nPoints = n_elements(xPointCoords)
  if (nPoints le 1) then return, -1
  vPolys  = ptrArr(nPoints)

  if (keyword_set(xMaxCoord) and keyword_set(yMaxCoord)) $
  then voroBox = [0, 0, xMaxCoord > max(xPointCoords), yMaxCoord > max(yPointCoords)]
  if keyword_set(fClipConvexHull) then begin
    cHullVertexIndices = convexHullPolygon(xPointCoords, yPointCoords)
    clipPolygonX = xPointCoords[cHullVertexIndices]
    clipPolygonY = yPointCoords[cHullVertexIndices]
  endif

  triangulate, xPointCoords, yPointCoords, tr, conn = c
  for i = 0ul, nPoints-1 do begin
    voronoi, xPointCoords, yPointCoords, i, c, xPoly, yPoly, voroBox

    ; There is a bug with IDL's built-in implmentation of the Voronoi/Delaunay 
    ; functions, so a clip rectangle is used to correct the output.
    fCorrectBoundaries = (max(xPoly, min = minX) gt xMaxCoord) or (max(yPoly, min = minY) gt yMaxCoord)
    fCorrectBoundaries = fCorrectBoundaries or (minX lt 0) or (minY lt 0)
    if (fCorrectBoundaries) then begin
      fixPoly = checkAndRepairPolygon(xPoly, yPoly, [0, xMaxCoord], [0, yMaxCoord], /fUseSmallestArea)
      xPoly = transpose(fixPoly[0,*])
      yPoly = transpose(fixPoly[1,*])
    endif
    if keyword_set(clipPolygonX) then begin
      clippedPoly = calcPolygonIntersection(p1x = xPoly, p1y = yPoly, p2x = clipPolygonX, p2y = clipPolygonY)
      if (clippedPoly[0] gt 1) then stop ; Check what happened... it should be one polygon... unless IDL's Voronoi function is even more crappy :S
      nVertices = clippedPoly[1]
      if (nVertices gt 0) then begin ; 2016.09 TODO JJ Unhandled (non-critical) inconsistency... resume with the original polygon
        xPoly = clippedPoly[2:2+nVertices-1]
        yPoly = clippedPoly[2+nVertices:2*nVertices+1]
      endif
    endif
    if keyword_set(fInterpolateLines) then begin
      polygonLineSample, xPoly, yPoly, xInt, yInt, /fCloseOutput
      vPolys[i] = ptr_new([[xInt], [yInt]], /no_copy)
    endif else vPolys[i] = ptr_new([[xPoly], [yPoly]], /no_copy)
  endfor

  if arg_present(polyAreas) or arg_present(areaSortedIndices) then begin
    polyAreas = fltArr(nPoints)
    for i = 0ul, nPoints-1 do polyAreas[i] = poly_area((*vPolys[i])[*,0], (*vPolys[i])[*,1])
    areaSortedIndices = sort(polyAreas)
  endif
  return, vPolys
end


function polygonCentroid, px, py
  npts = n_elements(px)
  if (npts lt 3) then return, [-1.0, -1.0]
  fPolyIsClosed = (px[0] eq px[npts-1]) and (py[0] eq py[npts-1])
  polygonArea   = polyArea(xCoords = px, yCoords = py)
  cx = 0.0
  cy = 0.0
  pLimit = fPolyIsClosed ? npts-3 : npts-2
  for i = 0u, pLimit do begin
    cx += ((px[i] + px[i+1]) * ((px[i]*py[i+1]) - (px[i+1]*py[i])))
    cy += ((py[i] + py[i+1]) * ((px[i]*py[i+1]) - (px[i+1]*py[i])))
  endfor
  if ~fPolyIsClosed then begin
    cx += ((px[i] + px[0]) * ((px[pLimit]*py[0]) - (px[0]*py[i])))
    cy += ((py[i] + py[0]) * ((px[pLimit]*py[0]) - (px[0]*py[i])))
  endif
  return, [cx, cy] / (6 * polygonArea)
end


; Returns the sorted indices for a given list of polygon segments [2 x n]
function sortPolygonSegments, segments
  nSegments = n_elements(segments[0,*])
  if (nSegments lt 1) then return, [-1]
  if (nSegments eq 1) then return, [0]
  sortedIndices = [0]
  whCurr = 0
  for i = 1, nSegments-1 do begin
    whNext = (where(segments[0, *] eq segments[1, whCurr]))[0]
    sortedIndices = [sortedIndices, whNext]
    whCurr = whNext
  endfor
  return, sortedIndices
end


; Computes and returns the index vector for the vertices that form the convex hull
; of a given 2D input polygon.
function convexHullPolygon, x, y
  nPts = n_elements(x)
  if (nPts lt 3) or (nPts ne n_elements(y)) then return, [-1]
  qHull, x, y, hullSegments
  nHullSegments = n_elements(hullSegments[0,*])
  sortedQhullIndices = sortPolygonSegments(hullSegments)
  hullSorted = hullSegments[0, 0]
  for i = 1u, nHullSegments-1 do $
    hullSorted = [hullSorted, hullSegments[0, sortedQhullIndices[i]]]
  return, [hullSorted, hullSegments[1, sortedQhullIndices[i-1]]]
end


; NOT A CROP IN THE COMMON SENSE...! Not yet finished.
pro cropPolyLineMaxMinMax, lineXcoords, lineYcoords, cropPolyX, cropPolyY, $ ;cropPolyX2 = cropPolyX2, cropPolyY2 = cropPolyY2, $
                           xCoordsOut = xCoordsOut, yCoordsOut = yCoordsOut

  nptsLine = n_elements(lineXcoords)
  if (nptsLine lt 1) then return

  maxX = max(cropPolyX, min = minX)
  maxY = max(cropPolyY, min = minY)
;  if n_elements(cropPolyX2) gt 0 then begin
;    maxX <= max(cropPolyX2, min = minX2) ; ...because of this!
;    minX >= minX2
;  endif
;  if n_elements(cropPolyY2) gt 0 then begin ; ...and because of this!
;    maxY <= max(cropPolyY2, min = minY2)
;    minY >= minY2
;  endif

  fRemoveIndexVec = bytArr(nptsLine)
  for i = 0u, nptsLine-1 do $
    if ((lineXcoords[i] lt minX) or (lineXcoords[i] gt maxX)) $
    or ((lineYcoords[i] lt minY) or (lineYcoords[i] gt maxY)) then fRemoveIndexVec[i] = 1

  whRemove = where(fRemoveIndexVec gt 0, countRemove)
  case 1 of
    (countRemove eq 0) or (countRemove eq nptsLine): begin
      xCoordsOut = lineXcoords
      yCoordsOut = lineYcoords
    endcase
    else: begin
    stop
    endcase
  endcase

end


; getPolyBoundaryAndInteriorPixelPoints
;
; Given an input polygon, computes and returns the integer coordinates of the
; pixels that the polygon encloses, by rastering the polygon with IDL built-in
; in functions (plot, polyfill, tvrd).
function getPolyBoundaryAndInteriorPixelPoints, px, py, xMax, yMax, winId = winId

  ; Stop in the case of incorrect input polygons (out of boundaries).
  if (min(px, max = maxpx) lt 1) or (min(py, max = maxpy) lt 1) then stop
  if (maxpx ge xMax) or (maxpy ge yMax) then stop

  if ~arg_present(winId) then winId = 15
  window, winId, xSize = xMax+1, ySize = yMax+1
  plot, px, py, xrange = [0,xMax], yrange = [0,yMax], xstyle = 1, ystyle = 1, xmargin = [0,0], ymargin = [0,0], xticks = 1, yticks = 1
  polyfill, px, py
  img = tvrd()
  wDelete, winId
  img[*,0]  = 0 & img[*,yMax] = 0
  img[0,*]  = 0 & img[xMax,*] = 0
  whPoints  = where(img ne 0, cnt)
  whPointsX = whPoints mod (xMax+1)
  whPointsY = whPoints / (xMax+1)
  return, [[whPointsX], [whPointsY]]
end


; TODO jjara 2018.09.14 Version 0.1 Untested!!!
pro srTesseler_calcConvexHullPolysAndAreasFromTXTs
  polyFilenames = dialog_pickFile(/MULTIPLE_FILES)
  nFiles = n_elements(polyFileNames)
  if (nFiles lt 1) and (nFiles ne '') then return
  ;polyAreas = fltArr(nFiles)
  ;polyCHulls = ptrArr(nFiles)
  logFileName = 'd:\tmp\convexHullAreas.csv'
  print, 'srTesseler_calcPolyAreaFromFile - ', nFiles, ' files to process...'
  fileLogger, 'File;ConvexHullArea;NumberOfPoints', logFileName
  for i = 0u, nFiles-1 do begin
    openr, lunID, polyFilenames[i], /GET_LUN
    tmp = ''
    readf, lunID, tmp ; skip the first line
    readf, lunID, tmp
    nVertices = long(tmp)
    print, 'Reading file ', polyFilenames[i], ' with ', nVertices, ' points'
    ptsX = lonArr(nVertices)
    ptsY = lonArr(nVertices)
    for j = 0, nVertices-1 do begin
      readf, lunID, tmp
      tmpSpl = strsplit(tmp, /extract)
      ptsX[j] = long(tmpSpl[0])
      ptsY[j] = long(tmpSpl[1])
    endfor
    ; calc convex hull & its area
    ; write convex hull area & number of points
    chArea = polyArea(xCoords = polyCHX, yCoords = polyCHY)
    logLine = polyFilenames[i] + ';' + string(chArea, format = '(F20)') + string(nVertices, format = '(F20)')
    fileLogger, logLine, logFileName
    close, lunID
    free_lun, lunID
  endfor
  
end


pro srTesseler_calcPolyAreaFromFile, polyAreas = polyAreas
  polyFilenames = dialog_pickFile(/MULTIPLE_FILES)
  nFiles = n_elements(polyFileNames)
  if (nFiles lt 1) and (nFiles ne '') then return
  polyAreas = fltArr(nFiles)
  print, 'srTesseler_calcPolyAreaFromFile - ', nFiles, ' files to process...'
  for i = 0u, nFiles-1 do begin
    openr, lunID, polyFilenames[i], /GET_LUN
    tmp = ''
    readf, lunID, tmp ; skip the first line
    readf, lunID, tmp
    nVertices = long(tmp)
    print, 'Reading file ', polyFilenames[i], ' with ', nVertices, ' points'
    polyX = lonArr(nVertices)
    polyY = lonArr(nVertices)
    for j = 0, nVertices-1 do begin
      readf, lunID, tmp
      tmpSpl = strsplit(tmp, /extract)
      polyX[j] = long(tmpSpl[0])
      polyY[j] = long(tmpSpl[1])
    endfor
    ;convexPolyIndices = convexHullPolygon(polyX, polyY)
    ;convexX = polyX[convexPolyIndices]
    ;convexY = polyY[convexPolyIndices]
    polyAreas[i] = polyArea(xCoords = polyX, yCoords = polyY)
    close, lunID
    free_lun, lunID
  endfor
  print, 'srTesseler_calcPolyAreaFromFile - Done. Polygon areas:'
  for i = 0u, nFiles-1 do print, polyFilenames[i], ' - ', string(polyAreas[i], format = '(F20)')
end


; Object/ROI IDs need to be sorted.
; 1. read points from CSV input file, grouping by "object number" field
; 2. compute convex hull for each group/object
; 3. read points from TXT input file, and write in the TXT output file only those that lie outside the convex hull polygons.
pro srTesseler_filterLocPointsWithRoisFromFiles

  inputLocationsFull = dialog_pickFile(title = 'Select TXT file with all the locations', filter = '*.txt')
  if (n_elements(inputLocationsFull) eq 0) or (inputLocationsFull eq '') then return
  inputLocationsROIs = dialog_pickFile(title = 'Select file with the points to define ROIs of exclusion', filter = '*.csv')
  if (n_elements(inputLocationsROIs) eq 0) or (inputLocationsROIs eq '') then return
  splitChar = ';'
  ;txtFilePrefix = 'SR_Alexa_647_'
  extPos = strPos(inputLocationsROIs, '.csv')
  distThreshold = 10.0
  distThresholdSuffix = distThreshold gt 0 ? ('_d' + strCompress(string(distThreshold), /REMOVE_ALL)) : ''
  outputLocations = strMid(inputLocationsROIs, 0, extPos) + distThresholdSuffix + '.txt'
  outputLocationsConvexHull = strMid(inputLocationsROIs, 0, extPos) + '_convexHull.txt'
  filterRoiArrX = [ptr_new()]
  filterRoiArrY = [ptr_new()]
  eofReached   = 0
  currentRoiId = 0L ; 0 means "no object"
  openR, lunID, inputLocationsROIs, /GET_LUN
  aux1 = ''
  readf, lunID, aux1
  aux2 = strSplit(aux1, ';', /EXTRACT)
  posRoiId = (where(aux2 eq 'Object index'))[0]
  posX = (where(aux2 eq 'x'))[0]
  posy = (where(aux2 eq 'y'))[0]
  xArr = [-1.0]
  yArr = [-1.0]
  while not eof(lunID) do begin
    readf, lunID, aux1
    aux2  = strSplit(aux1, splitChar, /EXTRACT)
    roiId = long(aux2[posRoiId])
    if (roiId eq 0) then continue
    ptX   = float(aux2[posX])
    ptY   = float(aux2[posY])
    if (roiId gt currentRoiId) then begin ; New ROI. Allocate memory
      currentRoiId += 1
      if (currentRoiId gt 1) then begin
        pRoiPointsX = currentRoiId eq 2 ? [ptr_new(xArr, /no_copy)] : [pRoiPointsX, ptr_new(xArr, /no_copy)]
        pRoiPointsY = currentRoiId eq 2 ? [ptr_new(yArr, /no_copy)] : [pRoiPointsY, ptr_new(yArr, /no_copy)]
      endif
      xArr = [ptX]
      yArr = [ptY]
    endif else if (currentRoiId eq roiId) then begin ; Just to be sure, but roiId field should be sorted in input file.
      xArr = [xArr, ptX]
      yArr = [yArr, ptY]
    endif
  endwhile
  if (currentRoiId ge 0) then begin
    pRoiPointsX = currentRoiId eq 2 ? [ptr_new(xArr, /no_copy)] : [pRoiPointsX, ptr_new(xArr, /no_copy)]
    pRoiPointsY = currentRoiId eq 2 ? [ptr_new(yArr, /no_copy)] : [pRoiPointsY, ptr_new(yArr, /no_copy)]
  endif
;stop
  close, lunID
  free_lun, lunID

  nRois = n_elements(pRoiPointsX)
  if (nRois eq 0) then begin ; Nothing to do!?
    stop
  endif else begin
    ;for i = 0u, nRois-1 do print, 'ROI ', i, ' has ', n_elements(*pRoiPointsX[i]), '/', n_elements(*pRoiPointsY[i]), ' x/y points'
    ;ptCount = 0ul & for i = 0u, nRois-1 do ptCount += n_elements(*pRoiPointsX[i])
    ;print, 'Total point count = ', ptCount
    pConvexHullX = ptrArr(nRois)
    pConvexHullY = ptrArr(nRois)
    convexRois = objArr(nRois)
    roiAreaSum = 0.0
    for i = 0u, nRois-1 do begin
      convexPolyIndices = convexHullPolygon(*pRoiPointsX[i], *pRoiPointsY[i])
      convexX = (*pRoiPointsX[i])[convexPolyIndices]
      convexY = (*pRoiPointsY[i])[convexPolyIndices]
      convexRois[i] = obj_new('IDLanROI', convexX, convexY)
      roiAreaSum += polyArea(xCoords = convexX, yCoords = convexY)
      nptsConvexHull = n_elements(convexX)
      fileLogger, strCompress(string(i), /rem) + ' ' + strCompress(string(nptsConvexHull), /rem), outputLocationsConvexHull
      for j = 0u, nptsConvexHull-1 do $
        fileLogger, strCompress(string(convexX[j]), /rem) + ' ' + strCompress(string(convexY[j]), /rem), outputLocationsConvexHull
      pConvexHullX[i] = ptr_new(convexX, /NO_COPY)
      pConvexHullY[i] = ptr_new(convexY, /NO_COPY)
    endfor
    for i = nRois-1, 0, -1 do ptr_free, pRoiPointsX[i], pRoiPointsY[i] 
  endelse
stop
  openR, lunID, inputLocationsFull, /GET_LUN
  openW, lunIDout, outputLocations, /GET_LUN
  printf, lunIDout, 'x,y,intensity,frame' ; print first line
  ptCount = -1
  aux3 = ''
  readf, lunID, aux3 ; first line not used
  posX = 0
  posY = 1
  countOutPts = 0u
  countInPts  = 0u
  scaleFactor = 10.0
  while not eof(lunID) do begin
    readf, lunID, aux3
    aux4 = strSplit(aux3, /EXTRACT)
    ptX = float(aux4[posX])
    ptY = float(aux4[posY])
    fPtOutCum = 1b
    for i = 0u, nRois-1 do begin
      oRoi = convexRois[i]
      fPtOut = (oRoi->containsPoints([ptX], [ptY])) eq 0
      if (fPtOut eq 1) and (distThreshold gt 0) then begin
        distToCH = calcDistanceFromPointsToPolygon(*pConvexHullX[i]/scaleFactor, *pConvexHullY[i]/scaleFactor, [ptX/scaleFactor], [ptY/scaleFactor])
        if (distToCH * scaleFactor le distThreshold) then fPtOut = 0
      endif
      fPtOutCum = fPtOutCum and fPtOut
      if ~fPtOutCum then break
    endfor
    if fPtOutCum eq 0 then countInPts += 1 else countOutPts += 1
    if (fPtOutCum eq 1) then printf, lunIDout, aux3
  endwhile
  close, lunID, lunIDout
  free_lun, lunID, lunIDout
  for i = 0u, nRois-1 do obj_destroy, convexRois[i]
  print, 'Finished! Number of points in/out/total : ', countInPts, '/', countOutPts, '/', countInPts+countOutPts

end


function convexHullFromXYpointsArray, pPolygonArray, convexHullX = convexHullX, convexHullY = convexHullY
  nPolys = n_elements(pPolygonArray) / 2
  if (nPolys eq 0) then return, -1
  xPts = [-1.0]
  yPts = [-1.0]
  for i = 0u, nPolys-1 do begin
    xPts = [xPts, *(pPolygonArray[0,i])]
    yPts = [yPts, *(pPolygonArray[1,i])]
  endfor
  convexHullIndices = convexHullPolygon(xPts[1:*], yPts[1:*])
  convexHullX = xPts[convexHullIndices+1]
  convexHullY = yPts[convexHullIndices+1]
  return, n_elements(convexHullX)
end


function intersectingPolygonPairs, pPolygons, pIntersectionsOut = pIntersectionsOut, boundingBoxesIn = boundingBoxesIn, boundingBoxesOut = boundingBoxesOut, boxPad = boxPad

  nPolygons = n_elements(pPolygons)
  if nPolygons le 1 then return, [-1]

  fSaveIntersections = arg_present(pIntersectionsOut)
  if fSaveIntersections then pIntersectionsOut = ptrArr(1)
  fBoundingBoxesIn  = n_elements(boundingBoxesIn) eq (nPolygons * 4)
  fBoundingBoxesOut = arg_present(boundingBoxesOut)
  if fBoundingBoxesOut then boundingBoxesOut = [-1.0]
  boxPad = keyword_set(boxPad) ? boxPad > 0 : 0
  pairs  = [0u]

  for i = 0u, nPolygons-2 do begin
    pxi = (*pPolygons[i])[*,0]
    pyi = (*pPolygons[i])[*,1]
    for j = i+1, nPolygons-1 do begin
      pxj = (*pPolygons[j])[*,0]
      pyj = (*pPolygons[j])[*,1]
      fCalc = 1
      if fBoundingBoxesIn then begin
        bboxPosI = 4*i
        xmini = boundingBoxesIn[bboxPosI]   & xmaxi = boundingBoxesIn[bboxPosI+2]
        ymini = boundingBoxesIn[bboxPosI+1] & ymaxi = boundingBoxesIn[bboxPosI+3]
        bboxPosJ = 4*j
        xminj = boundingBoxesIn[bboxPosJ]   & xmaxj = boundingBoxesIn[bboxPosJ+2]
        yminj = boundingBoxesIn[bboxPosJ+1] & ymaxj = boundingBoxesIn[bboxPosJ+3]
        fCalc = ((xmaxj gt xmini) and (xminj le xmaxi)) and ((ymaxj gt ymini) and (yminj le ymaxi))
      endif
      if (fCalc eq 0) then continue
      intIJ = calcPolygonIntersection(p1x = pxi, p1y = pyi, p2x = pxj, p2y = pyj)
      if intIJ[0] gt 0 then begin
        pairs = [pairs, i, j]
        if fBoundingBoxesOut then begin
          npts = intIJ[1]
          maxIntX = max(intIJ[2:2+npts-1], min = minIntX)
          maxIntY = max(intIJ[2+npts:2+npts+npts-1], min = minIntY)
          boundingBoxesOut = [boundingBoxesOut, minIntX - boxPad, minIntY - boxPad, maxIntX + boxPad, maxIntY + boxPad]
        endif
        if fSaveIntersections then pIntersectionsOut = [pIntersectionsOut, ptr_new(intIJ, /no_copy)]
      endif
    endfor
  endfor
  nPairs = (n_elements(pairs)-1) / 2
  if (nPairs ge 1) then begin
    if fSaveIntersections then pIntersectionsOut = pIntersectionsOut[1:*]
    if fBoundingBoxesOut then boundingBoxesOut = boundingBoxesOut[1:*]
    return, pairs[1:*]
  endif else return, [-1] 
end


; getClosestMatch
; Given a two pairs of points (a,b) and (p,q), returns the closest pair of points coded as [0,1] or [1,0] when...
; fEquidistant is a flag that, if provided, indicates if the points are equidistant from pair to pair. 
function getClosestMatch, ax, ay, bx, by, px, py, qx, qy, fEquidistant = fEquidistant
  dap = (ax-px)^2 + (ay-py)^2
  daq = (ax-qx)^2 + (ay-qy)^2
  dbp = (bx-px)^2 + (by-py)^2
  dbq = (bx-qx)^2 + (by-qy)^2
  fEquidistant = (dap eq dbp) and (daq eq dbq)
  if (dap le daq) and (dbq le daq) then return, [0,1]
  if (dap lt daq) and (dap gt dbp) and (dbq le daq) then return, [0,1]
  if (dap lt daq) and (dap gt dbp) and (dbq gt daq) then return, [1,0]
  if (daq le dap) and (dbp le dap) then return, [1,0]
  if (daq le dap) and (dbp le dbq) then return, [1,0]
  if (daq le dap) and (dbq lt daq) then return, [0,1]
  if (dap le dbp) then if (dbp lt dbq) then return, [1,0] else return, [0,1]
  stop ; Should not happen. Call jjara!
end


pro getClosestMatch_test
  print, getClosestMatch(0., 0., 1., 1., -2., -12, 2., 2.)
  print, getClosestMatch(0., 0., 1., 1., 0.1, 0.1, 2., 2.)
  print, getClosestMatch(0., 0., 1., 1., 10.1, 0.1, 2., 2.)
  print, getClosestMatch(0., 0., 0., 1., 10.1, 0.1, 2., 2.)
  print, getClosestMatch(0., 0., 0., 1., 10.0, 0.5, .5, .5, fEquidistant = feq)
  print, feq ; 1
  print, getClosestMatch(0., 0., 1., 1., -2., -2, 2., 2.)
  print, getClosestMatch(0., 0., 1., 1., -1., -1., 2., 2., fEquidistant = feq)
  print, feq ; 0
  print, getClosestMatch(0., 0., 1., 1., 0.1, 10.1, 2., 2.)
  print, getClosestMatch(0., 0., 1., 1., 0.1, 10.1, .5, .5)
  print, getClosestMatch(0., 0., 1., 1., 10.1, 10.1, .5, .5)
  print, getClosestMatch(0., 0., 1., 1., -10.1, -10.1, .5, .5)
end

; TODO JJ this is slow... optimize with sorted arrays, histogram...
function nEqualInArrays, arr1, arr2, mergedIndices = mergedIndices
  n1 = n_elements(arr1)
  n2 = n_elements(arr2)
  nMatches = 0u
  mergedIndices = [0u]
  if (n1 eq 0) or (n2 eq 0) then return, 0
  if (n1 ge n2) then begin
    for i = 0u, n1-1 do begin
      wh1i = where(arr2 eq arr1[i], count1i)
      if count1i ge 1 then begin
        nMatches += 1
        mergedIndices = [mergedIndices, arr1[i]]
      endif
    endfor
  endif else begin
    for i = 0u, n2-1 do begin
      wh2i = where(arr1 eq arr2[i], count2i)
      if count2i ge 1 then begin
        nMatches += 1
        mergedIndices = [mergedIndices, arr2[i]]
      endif
    endfor
  endelse
  if nMatches ge 1 then mergedIndices = mergedIndices[1:*] else mergedIndices = [-1]
  return, nMatches
end


function mergeCornerBoxes_impl, cornerBoxes, cornerAABindices, cornerROIindices, aabIndicesOut = aabIndicesOut, minNindices = minNindices, roiIndicesOut = roiIndicesOut, fRecall = fRecall
  minNindices = keyword_set(minNindices) ? minNindices > 1 : 2
  nCornerBoxes = n_elements(cornerBoxes) / 4
  if (nCornerBoxes lt 2) then begin
    roiIndicesOut = cornerROIindices
    aabIndicesOut = cornerAABindices
    fRecall = 0b
    return, cornerBoxes
  endif
  ; Find intersections
  nPairs = 0u
  pairs  = uLonArr(nCornerBoxes * (nCornerBoxes-1))
  libLoc = getDLLfilename('SweepAndPrune2D', /GetFileNameAndPath)
  result = call_external(libLoc, 'find_intersections2D',$
                         fix(cornerBoxes, type = 4), fix(nCornerBoxes, type = 2), fix(2, type = 2),$
                         pairs, nPairs, /unload)
  if (nPairs eq 0) then begin
    roiIndicesOut = cornerROIindices
    aabIndicesOut = cornerAABindices
    fRecall = 0b
    return, cornerBoxes
  endif
  pairs          = pairs[0:2*nPairs-1]
  cornerBoxesOut = [-1.0]
  fNotMerged     = bytArr(nCornerBoxes)
  fMerged        = bytArr(nCornerBoxes)
  cornerIndicesOut    = ptrArr(1)
  cornerIndicesAABOut = ptrArr(1)

  for p = 0u, nPairs-1 do begin
    cornerPosI  = pairs[2*p]
    cornerBoxI  = cornerBoxes[4*cornerPosI : 4*cornerPosI+3]
    cornerPosJ  = pairs[2*p+1]
    cornerBoxJ  = cornerBoxes[4*cornerPosJ : 4*cornerPosJ+3]
    indicesIaab = *cornerAABindices[cornerPosI]
    indicesJaab = *cornerAABindices[cornerPosJ]
    nEqualAABs  = nEqualInArrays(indicesIaab, indicesJaab, mergedIndices = commonIndicesAABs)
    indicesI    = *cornerROIindices[cornerPosI]
    indicesJ    = *cornerROIindices[cornerPosJ]
    nEquals     = nEqualInArrays(indicesI, indicesJ, mergedIndices = commonIndices)
    if (nEquals ge minNindices) then begin ; if criteria to merge is OK, then merge...
      mergeROIIndices = [indicesI, indicesJ]
      mergeROIIndices = mergeROIIndices[uniq(mergeROIIndices, sort(mergeROIIndices))]
      mergeAABindices = [indicesIaab, indicesJaab]
      mergeAABindices = mergeAABindices[uniq(mergeAABindices, sort(mergeAABindices))]
      fMerged[cornerPosI] = 1
      fMerged[cornerPosJ] = 1
      cornerBoxIJ = [cornerBoxI[0] < cornerBoxJ[0], cornerBoxI[1] < cornerBoxJ[1], cornerBoxI[2] > cornerBoxJ[2], cornerBoxI[3] > cornerBoxJ[3]]

      if (p lt nPairs-1) then begin ; Check if there are duplicities to be considered
        pairsCDR = pairs[2*p+2:*]
        whIJ     = where((pairsCDR eq cornerPosI) or (pairsCDR eq cornerPosJ), countIJ, complement = whNotIJ, nComplement = countNotIJ)
        if (countIJ eq 0) then begin ; If not, add the merged corner and continue
          cornerBoxesOut      = p eq 0 ? cornerBoxIJ : [cornerBoxesOut, cornerBoxIJ]
          cornerIndicesOut    = p eq 0 ? [ptr_new(mergeROIindices)] : [cornerIndicesOut, ptr_new(mergeROIindices)]
          cornerIndicesAABOut = p eq 0 ? [ptr_new(mergeAABindices)] : [cornerIndicesAABOut, ptr_new(mergeAABindices)]
          continue
        endif
        if (countNotIJ ge 1) then begin ; If yes, return merged+unprocessed corners and ask for a re-call
          cornerBoxesOut = p eq 0 ? cornerBoxIJ : [cornerBoxesOut[1:*], cornerBoxIJ]
          cornersNotIJ = pairsCDR[whNotIJ]
          cornersNotIJ = cornersNotIJ[uniq(cornersNotIJ, sort(cornersNotIJ))]
          whNotMerged  = where(fNotMerged eq 1, countNotMerged)
          if countNotMerged gt 0 $ ; Remove non-merged corners that were already added to the output
            then cornersNotIJ = setDifference(cornersNotIJ, whNotMerged)
          countNotIJ = cornersNotIJ[0] eq -1 ? 0 : n_elements(cornersNotIJ)
          if (countNotIJ gt 0) then begin
            for i = 0u, countNotIJ-1 do begin
              boxPos         = cornersNotIJ[i] * 4
              cornerBoxesOut = [cornerBoxIJ, cornerBoxes[boxPos:boxPos+3]]
            endfor
            cornerIndicesOut = p eq 0 $
              ? [ptr_new(mergeROIindices), cornerROIindices[cornersNotIJ]] $
              : [cornerIndicesOut, ptr_new(mergeROIindices), cornerROIindices[cornersNotIJ]]
            cornerIndicesAABOut = p eq 0 $
              ? [ptr_new(mergeAABindices), cornerAABindices[cornersNotIJ]] $
              : [cornerIndicesAABOut, ptr_new(mergeAABindices), cornerAABindices[cornersNotIJ]]
          endif
          fRecall = 1b
          roiIndicesOut = cornerIndicesOut
          aabIndicesOut = cornerIndicesAABOut
          return, cornerBoxesOut
        endif
      endif else begin
        cornerBoxesOut   = p eq 0 ? cornerBoxIJ : [cornerBoxesOut, cornerBoxIJ]
        cornerIndicesOut = p eq 0 ? [ptr_new(mergeROIindices)] : [cornerIndicesOut, ptr_new(mergeROIindices)]
        cornerIndicesAABOut = p eq 0 ? [ptr_new(mergeAABindices)] : [cornerIndicesAABOut, ptr_new(mergeAABindices)]
        fRecall = 0b
        roiIndicesOut = cornerIndicesOut
        aabIndicesOut = cornerIndicesAABOut
        return, cornerBoxesOut
      endelse

    endif else begin ; If criteria not met, add the non-merged corner boxes from the input to the eoutput
      if (fNotMerged[cornerPosI] eq 0) and (fMerged[cornerPosI] eq 0) then begin
        cornerBoxesOut = [cornerBoxesOut, cornerBoxI]
        cornerIndicesOut = [cornerIndicesOut, cornerROIindices[cornerPosI]]
        cornerIndicesAABOut = [cornerIndicesAABOut, cornerAABindices[cornerPosI]]
        fNotMerged[cornerPosI] = 1
      endif else stop ;???
      if (fNotMerged[cornerPosJ] eq 0) and (fMerged[cornerPosJ] eq 0) then begin
        cornerBoxesOut = [cornerBoxesOut, cornerBoxJ]
        cornerIndicesOut = [cornerIndicesOut, cornerROIindices[cornerPosJ]]
        cornerIndicesAABOut = [cornerIndicesAABOut, cornerAABindices[cornerPosJ]]
        fNotMerged[cornerPosJ] = 1
      endif else stop ;???
    endelse
  endfor
end


function mergeCornerBoxes, cornerBoxes, cornerAABindices, cornerROIindices, minNindices = minNindices, roiIndicesOut = roiIndicesOut, aabIndicesOut = aabIndicesOut
  fRecall = 1b
  cBoxes = cornerBoxes
  cAABindices = cornerAABindices
  cROIindices = cornerROIindices
  it = 0
  while fRecall do begin
    out = mergeCornerBoxes_impl(cBoxes, cAABindices, cROIindices, minNindices = minNindices, roiIndicesOut = roiIndicesOut, aabIndicesOut = aabIndicesOut, fRecall = fRecall)
    if fRecall then begin
;      if (it gt 0) then $
;        for i = n_elements(cAABindices)-1, 0, -1 do ptr_free, cAABindices[i], cAABindices[i]
      cBoxes = out
      cAABindices = aabIndicesOut
      cROIindices = roiIndicesOut
    endif
    it += 1
  endwhile
  return, out
end


pro srTesseler_loadFiles
  fileNamePath = dialog_pickfile(filter='*.CSV')
  if fileNamePath eq ' ' then return
  openR, lunID, fileNamePath, /get_lun
  tmp = ''
  readf, lunID, tmp
  tmpSplit = strSplit(tmp, ';', /extract)
  nRois = long(tmpSplit[1])
  if (nRois lt 1) then stop ; No ROIs... ???
  print, 'Going for ', nRois, ' ROIs'
  pROIsX = ptrArr(nRois)
  pROIsY = ptrArr(nRois)
  xMin = 0
  yMin = 0
  xMax = 0
  yMax = 0
  for i = 0u, nROIs-1 do begin
    readf, lunID, tmp ; ROI number, it can be skipped
    readf, lunID, tmp ; Vertex Count for the ROI
    tmpSplit = strSplit(tmp, ';', /extract)
    nVertices = tmpSplit[1]
    print, 'ROI ', i, ' with ', nVertices, ' vertices'
    xCoords = fltArr(nVertices)
    yCoords = fltArr(nVertices)
    for j = 0u, nVertices-1 do begin
      readf, lunID, tmp ; Vertex count for the ROI.
      tmpSplit = strSplit(tmp, ';', /extract)
      xCoords[j] = float(tmpSplit[0])
      yCoords[j] = float(tmpSplit[1])
    endfor
    if (i eq 0) then begin
      xMax = max(xCoords)
      yMax = max(yCoords)
    endif else begin
      xMax >= max(xCoords)
      yMax >= max(yCoords)
    endelse

    pROIsX[i] = ptr_new(xCoords, /no_copy)
    pROIsY[i] = ptr_new(yCoords, /no_copy)
  endfor
  close, lunID
  free_lun, lunID
  window, xsize = 1050, ysize = 1050  
  plot, *pROIsX[0], *pROIsY[0], xRange = [0, xMax], yRange =[0, yMax]
  for i = 1u, nROIs-1 do begin
    oPlot, *pROIsX[i], *pROIsY[i]
  endfor
  totalArea = 0d
  for i = 0u, nROIs-1 do begin
    polyAreaI = polyArea(xCoords = *pROIsX[i], yCoords = *pROIsY[i])
    totalArea += polyAreaI
    print, polyAreaI
  endfor
  print, 'Total Area = ', totalArea
  stop
  for i = nROIs-1, 0, -1 do ptr_free, pROIsX[i], pROIsY[i]
end


function centerLineForTwoPolylines_sorted, p1x, p1y, p2x, p2y, outX = outX, outY = outY, fConn2 = fConn2, fReverse = fReverse, corr12 = corr12
  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  if (np1 lt 1) or (np2 lt 1) then return, -1
  fReverse = np1 lt np2
  if fReverse $
  then return, centerLineForTwoPolylines_sorted2(p2x, p2y, p1x, p1y, outX = outX, outY = outY, fConn2 = fConn2, corr12 = corr12) $
  else return, centerLineForTwoPolylines_sorted2(p1x, p1y, p2x, p2y, outX = outX, outY = outY, fConn2 = fConn2, corr12 = corr12)
end


function centerLineForTwoPolylines_sorted2, p1x, p1y, p2x, p2y, outX = outX, outY = outY, fConn2 = fConn2, corr12 = corr12

  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  if (np1 lt 1) or (np2 lt 1) then return, -1
  outXini = (p1x[0] + p2x[0])/2
  outYini = (p1y[0] + p2y[0])/2
  outXend = (p1x[np1-1] + p2x[np2-1])/2
  outYend = (p1y[np1-1] + p2y[np2-1])/2

  ; Always connect the first points from both lists.
  outX = [outXini]
  outY = [outYini]

  fConn2 = bytArr(np2)
  fConn2[0] = 1
  ; Outer loop through polyline 1, chacking for nearest segments in polyline 2
  distIJprevSq = (p1x[0] - p2x[0])^2 + (p1y[0] - p2y[0])^2

  j = 0u
  corr12 = uIntArr(np1)
  for i = 0u, np1-1 do begin
    closestToI = j
    distMinIJ  = (p1x[i] - p2x[j])^2 + (p1y[i] - p2y[j])^2
    jTmp = j + 1
    fOK  = (jTmp ge (np2-1)) ? 1b : 0b ; Watch out for ties! Some day...
    while ~fOK do begin
      distIJSqCur = (p1x[i] - p2x[jTmp])^2 + (p1y[i] - p2y[jTmp])^2
      if (distIJSqCur lt distMinIJ) then begin
        closestToI = jTmp
        distMinIJ  = distIJSqCur
        jTmp += 1
      endif else fOK = 1
      if (jTmp ge np2-1) then fOK = 1
    endwhile

    j = closestToI
    midIJx = (p1x[i] + p2x[j])/2
    midIJy = (p1y[i] + p2y[j])/2
    outX   = [outX, midIJx]
    outY   = [outY, midIJy]
    fConn2[j] = 1
    corr12[i] = j
  endfor

  ; Append tail point only if it has not been previously set.
  if (fConn2[np2-1] eq 0) then begin
    outX = [outX, outXend]
    outY = [outY, outYend]
  endif
  fConn2[np2-1] = 1
  return, n_elements(outX)
end


pro centerLineForTwoPolylines_sorted_Test1, np = np
  npts12 = n_elements(np) gt 0 ? np > 2 : 100
  freq12 = .05
  p1t = indgen(npts12)
  p1x = p1t
  p1y = sin(freq12 * p1t)
  offset = 1./3
  p2t = indgen(npts12)
  p2x = p2t
  ;p2y = sin(freq12 * p1t+!pi*offset)
  p2y = cos(freq12 * p1t)
  window, 0, xSize = 2000
  plot, p1x, p1y, pSym = 5
  oPlot, p2x, p2y, pSym = 6
  nptsMidline = centerLineForTwoPolylines_sorted(p1x, p1y, p2x, p2y, outX = midLineX, outY = midLineY, fConn2 = fConn2, fReverse = fReverse)
  whConn2 = where(fConn2 ne 0, nConn2, complement = whConn2comp, nComplement = nConn2comp)
  print, nConn2, nConn2comp
  oPlot, midLineX, midLineY, pSym = 4
  if (nConn2comp gt 0) then if ~fReverse then oPlot, p2x[whConn2comp], p2y[whConn2comp] else oPlot, p1x[whConn2comp], p1y[whConn2comp]
end


;
; Comparison metrics for a separation line between two polylines.
;

function distanceBetweenPointlists_pointToPoint, p1x, p1y, p2x, p2y
  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  distSqMin = fltArr(np1)
  for i = 0u, np1-1 do $
    distSqMin[i] = min((p2x-p1x[i])^2 + (p2y-p1y[i])^2)
  return, distSqMin
end


function distanceBetweenPointlists_pointToSegment, p1x, p1y, p2x, p2y
  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  distSqMin = fltArr(np1)
  for i = 0u, np1-1 do begin
    dist1i2 = fltArr(np2-1)
    for j = 0, np2-2 do $
      dist1i2[j] = distanceFromPointToSegment(p1x[i], p1y[i], p2x[j], p2y[j], p2x[j+1], p2y[j+1])
    distSqMin[i] = min(dist1i2)
  endfor
  return, distSqMin
end


function computeMetric_equiArea, sx, sy, p1x, p1y, p2x, p2y, zeroDiff = zeroDiff

  nps = n_elements(sx)
  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  if (nps lt 1) or (np1 lt 1) or (np2 lt 1) then return, -1

  sIndices = uIndGen(nps)
  distS12x = [sIndices, reverse(sIndices), 0]

  distS1 = distanceBetweenPointlists_pointToSegment(sx, sy, p1x, p1y)
  distS1y = [replicate(0, nps), reverse(distS1), 0]
  areaS1 = polyArea(xCoords = distS12x, yCoords = distS1y)

  distS2 = distanceBetweenPointlists_pointToSegment(sx, sy, p2x, p2y)
  distS2y = [replicate(0, nps), reverse(distS2), 0]
  areaS2 = polyArea(xCoords = distS12x, yCoords = distS2y)

  ;print, areaS1, areaS2
  areaSum = areaS1 + areaS2
  zeroDiff = areaSum eq 0
  return, zeroDiff ? 1.0 : 1.0 - abs(areaS1-areaS2) / areaSum

end


pro computeMetric_equiArea_Test
 np1 = 10
 np2 = 10
 nps = 10
 p1x = uIndGen(np1)
 p2x = p1x
 p1y = replicate(3, np1)
 p2y = replicate(0, np1)
 sx = p1x
 sy = replicate(2, nps)
 sy[2 * uIndGen(nps/2)] = 1
 print, computeMetric_equidistance(sx, sy, p1x, p1y, p2x, p2y)
 print, computeMetric_equidistance(sx, sy-0.5, p1x, p1y, p2x, p2y)
end


function curvatureForPolyline_CircleApprox, px, py, xyRes = xyRes
  if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
  np = n_elements(px)
  if (np lt 3) then return, -1

  cirArray = dblArr(np)
  for i = 1, np-2 do begin
    i1 = i-1
    i2 = i+1
    s_cir_3pnt, px[[i,i1,i2]], py[[i,i1,i2]], r, xCenter, yCenter, xyRes = xyRes
    cirArray[i] = r
    xCenter /= xyRes[0]
    yCenter /= xyRes[1]
;    plots, .5 + [px[i1], xCenter], .5 + [py[i1], yCenter], color = 125
;    plots, .5 + [px[i] , px[i]],   .5 + [py[i],  py[i]] , linestyle = 1
;    plots, .5 + [px[i1], px[i1]],  .5 + [py[i1], py[i1]], linestyle = 1
;    plots, .5 + [px[i2], px[i2]],  .5 + [py[i2], py[i2]], linestyle = 1
  endfor
  cirArray[0]    = cirArray[1]
  cirArray[np-1] = cirArray[np-2]
  return, cirArray
end


pro curvatureForPolyline_CircleApprox_Test1
  p1x = [0, 1, 1, 0]
  p1y = [0, 0, 1, 1]
  print, curvatureForPolyline_CircleApprox(p1x, p1y)
  p2x = [0, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9]
  p2y = [0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0]
  print, curvatureForPolyline_CircleApprox(p2x, p2y)
end


; http://mathworld.wolfram.com/Curvature.html
function curvatureForPolyline_DerivativeApprox, px, py, xyRes = xyRes

  if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
  np = n_elements(px)
  if (np lt 3) then return, -1

  gx   = xyRes[0] * shift(px[0:np-1],-1) - px[0:np-1]
  gy   = xyRes[1] * shift(py[0:np-1],-1) - py[0:np-1]
  gxx  = xyRes[0] * shift(px[0:np-1], 1) - 2 * px[0:np-1] + shift(px[0:np-1], -1)
  gyy  = xyRes[1] * shift(py[0:np-1], 1) - 2 * py[0:np-1] + shift(py[0:np-1], -1)
  curvArr = ((gx * gyy - gy * gxx) / (gx^2 + gy^2)^1.5) * 2 / sqrt(2)
;  curv = ((gx * gyy - gy * gxx) / (gx^2 + gy^2)^1.5) ; http://mathworld.wolfram.com/Curvature.html
  whereNAN = where(finite(curvArr) ne 1, countNAN)
  if (countNAN gt 0) then curvArr[whereNAN] = 0.
  curvArr[0]    = curvArr[1]
  curvArr[np-1] = curvArr[np-2]
  return, curvArr
end


pro curvatureForPolyline_DerivativeApproxTest1
  p1x = [0, 1, 1, 0]
  p1y = [0, 0, 1, 1]
  print, curvatureForPolyline_DerivativeApprox(p1x, p1y)
  p2x = [0, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9]
  p2y = [0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0]
  print, curvatureForPolyline_DerivativeApprox(p2x, p2y)
end


function normalizedLengthsForPolylineVertices, px, py
  np = n_elements(px)
  if (np lt 2) then return, -1
  ns = np-1
  lenSqArr = dblArr(ns)
  lenSqTotal = 0d
  for i = 0u, ns-1 do begin
    lenSqArr[i] = (px[i]-px[i+1])^2 + (py[i]-py[i+1])^2
    lenSqTotal += lenSqArr[i]
  endfor
  lenSqArr /= lenSqTotal
  tArr = dblArr(np)
  for i = 1u, ns-1 do tArr[i] = tArr[i-1] + lenSqArr[i-1]
  tArr[np-1] = 1
  return, tArr
end


pro normalizedLengthsForPolylineVertices_Test1
  np1 = 50
  p1x = uIndGen(np1)
  p1y = sin(p1x)
  print, normalizedLengthsForPolylineVertices(p1x, p1y)
end


; findCorrespondenceBetweenIndices
; Given two arrays, finds the index of the closest values from array1 to array2 (for each element of array 1). 
; This function is intended to operate on sorted arrays!
function findCorrespondenceBetweenIndices, arr1, arr2

  n1 = n_elements(arr1)
  n2 = n_elements(arr2)
  if (n1 lt 1) or (n2 lt 1) then return, -1

  correspondenceArr = uIntArr(n1)
  j = 0u
  pos2 = j
  for i = 0u, n1-1 do begin
    d12min = abs(arr1[i] - arr2[pos2])
    j = pos2
    if j lt (n2-1) then begin
      k = j+1
      fOut = 0b
      repeat begin
        d12k = abs(arr1[i] - arr2[k])
        if (d12k le d12min) then begin
          pos2 = k
          d12min = d12k
          k +=1
        endif else begin
          fOut = 1b
        endelse
      endrep until (k ge (n2-1)) or fOut
    endif
    correspondenceArr[i] = pos2
  endfor
  return, correspondenceArr
end


pro centerlineNormLength_impl, p1x, p1y, p2x, p2y, xOut = xOut, yOut = yOut, corr12 = corr12
  np1 = n_elements(p1x)
  normPts1indices = normalizedLengthsForPolylineVertices(p1x, p1y)
  normPts2indices = normalizedLengthsForPolylineVertices(p2x, p2y)
  corr12 = findCorrespondenceBetweenIndices(normPts1indices, normPts2indices)
  xOut = fltArr(np1)
  yOut = fltArr(np1)
  for i = 0u, np1-1 do begin
    p2pos = corr12[i]
    xOut[i] = (p1x[i] + p2x[p2pos]) / 2.0
    yOut[i] = (p1y[i] + p2y[p2pos]) / 2.0
  endfor
end


pro overPlotSegments, p1x, p1y, p2x, p2y, corr12 = corr12, winId = winId
  np1 = n_elements(p1x)
  if n_elements(winId) gt 0 then wSet, winId
  for i = 0u, np1-1 do oPlot, [p1x[i], p2x[corr12[i]]], [p1y[i], p2y[corr12[i]]]
end


pro centerlineNormLength, p1x, p1y, p2x, p2y, xOut = xOut, yOut = yOut, fOplot = fOplot, winId = winId
  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  if np1 ge np2 $
  then begin
    centerlineNormLength_impl, p1x, p1y, p2x, p2y, xOut = xOut, yOut = yOut, corr12 = corr12
    if keyword_set(fOplot) then overPlotSegments, p1x, p1y, p2x, p2y, corr12 = corr12, winId = winId
  endif else begin
    centerlineNormLength_impl, p2x, p2y, p1x, p1y, xOut = xOut, yOut = yOut, corr12 = corr21
    if keyword_set(fOplot) then overPlotSegments, p2x, p2y, p1x, p1y, corr12 = corr21, winId = winId
  endelse 
end


function diffCurvatureBetweenPolylines, p1x, p1y, p2x, p2y

  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  if (np1 lt 1) or (np2 lt 1) then return, -1

  t1 = normalizedLengthsForPolylineVertices(p1x, p1y)
  t2 = normalizedLengthsForPolylineVertices(p2x, p2y)
  c1 = curvatureForPolyline_DerivativeApprox(p1x, p1y)
  c2 = curvatureForPolyline_DerivativeApprox(p2x, p2y)

  corresp12 = findCorrespondenceBetweenIndices(t1, t2)
  diffArr = dblArr(np1)
  for i = 0u, np1-1 do begin
    stop
  endfor

  return, diffArr
end


function computeMetric_curvatureSimilarity, sx, sy, p1x, p1y, p2x, p2y, zeroDiff = zeroDiff, fDerivative = fDerivative

  nps = n_elements(sx)
  np1 = n_elements(p1x)
  np2 = n_elements(p2x)

  if (nps lt 1) or (np1 lt 1) or (np2 lt 1) then return, -1
  if keyword_set(fDerivative) then begin
    curv1 = curvatureForPolyline_DerivativeApprox(p1x, p1y)
    curv2 = curvatureForPolyline_DerivativeApprox(p2x, p2y)
    curvS = curvatureForPolyline_DerivativeApprox(sx, sy)
  endif else begin
    curv1 = curvatureForPolyline_CircleApprox(p1x, p1y)
    curv2 = curvatureForPolyline_CircleApprox(p2x, p2y)
    curvS = curvatureForPolyline_CircleApprox(sx, sy)
  endelse

  paramLen1 = normalizedLengthsForPolylineVertices(p1x, p1y)
  paramLen2 = normalizedLengthsForPolylineVertices(p2x, p2y)
  paramLenS = normalizedLengthsForPolylineVertices(sx, sy)

  if (np1 gt nps) then begin
    corrs1  = findCorrespondenceBetweenIndices(paramLen1, paramLenS)
    curvS1  = curvS[corrs1]
    curvDiff = abs(curvS1-curv1)
    sPolyX1 = [paramLen1, reverse(paramLen1), 0]
    area1   = polyArea(xCoords = sPolyX1, yCoords = [replicate(0, np1), curvDiff, 0])
  endif else begin
    corrsS  = findCorrespondenceBetweenIndices(paramLenS, paramLen1)
    curv1S  = curv1[corrsS]
    curvDiff = abs(curv1S-curvS)
    sPolyX1 = [paramLenS, reverse(paramLenS), 0]
    area1   = polyArea(xCoords = sPolyX1, yCoords = [replicate(0, nps), curvDiff, 0])
  endelse

  if (np2 gt nps) then begin
    corrs2  = findCorrespondenceBetweenIndices(paramLen2, paramLenS)
    curvS2  = curvS[corrs2]
    curvDiff = abs(curvS2-curv2)
    sPolyX2 = [paramLen2, reverse(paramLen2), 0]
    area2   = polyArea(xCoords = sPolyX2, yCoords = [replicate(0, np2), curvDiff, 0])
  endif else begin
    corrsS  = findCorrespondenceBetweenIndices(paramLenS, paramLen2)
    curv2S  = curv2[corrsS]
    curvDiff = abs(curv2S-curvS)
    sPolyX2 = [paramLenS, reverse(paramLenS), 0]
    area2   = polyArea(xCoords = sPolyX2, yCoords = [replicate(0, nps), curvDiff, 0])
  endelse

  areaSum  = area1 + area2
  zeroDiff = areaSum eq 0
  return, zeroDiff ? 1.0 : 1.0 - abs(area1-area2) / areaSum
end


pro computeMetric_curvatureSimilarity_Test
  stop
end


function pointSamplingOfPolyline, px, py, perimeterFactor = perimeterFactor
  if ~keyword_set(perimeterFactor) then perimeterFactor = 1.0
  np = n_elements(px)
  if (np le 1) then return, 0
  pxs = shift(px, -1)
  pys = shift(py, -1)
  dSqrt  = sqrt((pxs - px)^2 + (pys - py)^2)
  distxy = total(dSqrt[0:np-2])
  return, (distXY / (np-1)) / perimeterFactor
end


function computeMetric_equiLength, sx, sy, p1x, p1y, p2x, p2y
  ns    = n_elements(sx)
  ssx   = shift(sx, -1)
  ssy   = shift(sy, -1)
  dSqrt = sqrt((ssx - sx)^2 + (ssy - sy)^2)
  ls = total(dSqrt[0:ns-2])

  n1    = n_elements(p1x)
  s1x   = shift(p1x, -1)
  s1y   = shift(p1y, -1)
  dSqrt = sqrt((s1x - p1x)^2 + (s1y - p1y)^2)
  l1 = total(dSqrt[0:n1-2])

  n2    = n_elements(p2x)
  s2x   = shift(p2x, -1)
  s2y   = shift(p2y, -1)
  dSqrt = sqrt((s2x - p2x)^2 + (s2y - p2y)^2)
  l2  = total(dSqrt[0:n2-2])
  lr1 = ls / l1
  lr2 = ls / l2
  return, ((lr1 < lr2) / (lr1 > lr2))
end


function computeMetric_equiLength2, sx, sy, p1x, p1y, p2x, p2y
  ns    = n_elements(sx)
  ssx   = shift(sx, -1)
  ssy   = shift(sy, -1)
  dSqrt = sqrt((ssx - sx)^2 + (ssy - sy)^2)
  ls = total(dSqrt[0:ns-2])

  n1    = n_elements(p1x)
  s1x   = shift(p1x, -1)
  s1y   = shift(p1y, -1)
  dSqrt = sqrt((s1x - p1x)^2 + (s1y - p1y)^2)
  l1 = total(dSqrt[0:n1-2])

  n2    = n_elements(p2x)
  s2x   = shift(p2x, -1)
  s2y   = shift(p2y, -1)
  dSqrt = sqrt((s2x - p2x)^2 + (s2y - p2y)^2)
  l2  = total(dSqrt[0:n2-2])

  lr1 = ls / l1
  lr2 = ls / l2

  return, 1 - abs(lr1-lr2) / (l1+l2)
end


function computeMetric_pointSampling1, sx, sy, p1x, p1y
  pss = pointSamplingOfPolyline(sx, sy)
  ps1 = pointSamplingOfPolyline(p1x, p1y)
  return, (pss < ps1) / (pss >ps1)
end


function computeMetric_pointSampling2, sx, sy, p1x, p1y, p2x, p2y
  pss = pointSamplingOfPolyline(sx, sy)
  ps1 = pointSamplingOfPolyline(p1x, p1y)
  ps2 = pointSamplingOfPolyline(p2x, p2y)
  sr1 = pss/ps1
  sr2 = pss/ps2
  return, ((sr1 < sr2) / (sr1 > sr2))
end


pro pointSamplingOfPolyline_Test
  np1 = 10
  p1x = uIndGen(np1)
  p1y = replicate(3, np1)
  print, pointSamplingOfPolyline(p1x, p1y, perimeterFactor = 1.0) 
  print, pointSamplingOfPolyline(p1x, sin(p1x), perimeterFactor = 1.0)
end


function computeMetric_averageSegmentLength, sx, sy, p1x, p1y, p2x, p2y
  aslS = pointSamplingOfPolyline(sx, sy)
  asl1 = pointSamplingOfPolyline(p1x, p1y)
  asl2 = pointSamplingOfPolyline(p2x, p2y)
  return, 1.0 - abs((aslS / asl1) - (aslS/asl2)) / ((aslS / asl1) + (aslS/asl2))
end


function computeMetric_averageSegmentLength2, sx, sy, p1x, p1y, p2x, p2y
  aslS = pointSamplingOfPolyline(sx, sy)
  asl1 = pointSamplingOfPolyline(p1x, p1y)
  asl2 = pointSamplingOfPolyline(p2x, p2y)
  return, ((asl1 + asl2)/2 < aslS) / ((asl1 + asl2)/2 > aslS)
end


function computeMetric_containment, sx, sy, p1x, p1y, p2x, p2y
  oRoi1 = obj_new('IDLanROI', p1x, p1y)
  oRoi2 = obj_new('IDLanROI', p2x, p2y)
  ns  = n_elements(sx)
  c1  = oRoi1->containsPoints(sx, sy)
  c2  = oRoi2->containsPoints(sx, sy)
  wh1 = where(c1 ne 0, count1)
  wh2 = where(c2 ne 0, count2)
  obj_destroy, oRoi1
  obj_destroy, oRoi2
  fracs1 = count1 / ns
  fracs2 = count2 / ns
  if (fracs1 eq 0) and (fracs2) eq 0 $
  then return, 1.0 $
  else return, (fracs1 < fracs2) / (fracs2 > fracs2)
end


function correspondingVerticesDistances_impl, p1x, p1y, p2x, p2y, fSquareDist = fSquareDist
  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  normPtslindices = normalizedLengthsForPolylineVertices(p1x, p1y)
  normPts2indices = normalizedLengthsForPolylineVertices(p2x, p2y)
  corr12 = findCorrespondenceBetweenIndices(normPtslindices, normPts2indices)
  distVec = fltArr(np1)
  for i = 0u, np1-1 do begin
    p2pos   = corr12[i]
    distIsq = (p1x[i] - p2x[p2pos])^2 + (p1y[i] - p2y[p2pos])^2
    distVec[i] = keyword_set(fSquareDist) ? distIsq : sqrt(distIsq)
  endfor
  return, distVec
end


function correspondingVerticesByDistance, p1x, p1y, p2x, p2y, fReverse = fReverse
  np1 = n_elements(p1x)
  np2 = n_elements(p2x)
  fReverse = np2 gt np1
  if ~fReverse $
  then return, correspondingVerticesDistances_impl(p1x, p1y, p2x, p2y) $
  else return, correspondingVerticesDistances_impl(p2x, p2y, p1x, p1y)
end


pro computeMetricsForSharedLine, sx, sy, p1x, p1y, p2x, p2y, fPrint = fPrint, printLogPath = printLogPath
  ;simC  = computeMetric_containment(sx, sy, p1x, p1y, p2x, p2y)
  simKc = computeMetric_curvatureSimilarity(sx, sy, p1x, p1y, p2x, p2y)
  simKd = computeMetric_curvatureSimilarity(sx, sy, p1x, p1y, p2x, p2y, /fDerivative)
  simD  = computeMetric_equiArea(sx, sy, p1x, p1y, p2x, p2y)
  simL  = computeMetric_equiLength2(sx, sy, p1x, p1y, p2x, p2y)
  simS  = computeMetric_pointSampling1(sx, sy, p1x, p1y)
  simASL = computeMetric_averageSegmentLength(sx, sy, p1x, p1y, p2x, p2y)
  simASL2 = computeMetric_averageSegmentLength2(sx, sy, p1x, p1y, p2x, p2y)
  if keyword_set(fPrint) then begin
    ;print, 'simC' , simC
    print, 'simKc', simKc
    print, 'simKd', simKd
    print, 'simD' , simD
    print, 'simL' , simL
    print, 'simASL2', simASL2
    print, 'simASL', simASL
    print, 'simS' , simS
  endif
  if (n_elements(printLogPath) gt 0) then begin
    sep = ';'
    msg = string(simD) + sep + string(simL) + sep + string(simKd) + sep + string(simASL2) + sep + string(simASL)
    ;msg = string(simD) + sep + string(simL) + sep + string(simKc) + sep + string(simKd) + sep + string(simS) + string(simASL)
    fileLogger, msg, printLogPath + '.csv'
  endif
end


; checkAndFixConsistencyForRoiContour
;
; Detects and removes auto-intersections of the input polygon. If an associated vector of labels is given,
; the method will process it accordingly (NOT FULLY FINISHED).
pro checkAndFixConsistencyForRoiContour, xCoords, yCoords, codes, xCoordsOut = xCoordsOut, yCoordsOut = yCoordsOut, outCodes = outCodes, fFixOut = fFixOut
  npts = n_elements(xCoords)
  if (npts lt 3) then return

  ; Check self-intersections
  ovrPolys  = calcPolygonIntersectionWithPolyIndices(p1x = xCoords, p1y = yCoords, p2x = xCoords, p2y = yCoords, polyAreas = polyAreas, $
                                                     indexCodes = indexCodes, pVertexIndices = pVertexIndices)
  nOvrPolys = ovrPolys[0]
  ; Keep the largest polygon
  if (nOvrPolys gt 1) then begin
    fFixOut = 1
    print, 'Self-intersecting polygon found with ', nOvrPolys, 'polygons.'
    ovrPolyStart = 1
    polySizes    = lonArr(nOvrPolys)
    polyStartPos = lonArr(nOvrPolys)
    for i = 0, nOvrPolys-1 do begin
      polyStartPos[i] = ovrPolyStart
      polySizes[i]    = round(ovrPolys[ovrPolyStart])
      ovrPolyStart   += 2 * ovrPolys[ovrPolyStart] + 1
    endfor
    print, polySizes
    maxSize = max(polySizes, min = minSize)
    whMax   = where(polySizes eq maxSize, countMax)
    if (countMax gt 1) then stop ; TODO select which polygon is to keep.
    polyStart  = polyStartPos[whMax]
    polyEnd    = polyStartPos[whMax] + ovrPolys[polyStart] * 2
    xCoordsOut = ovrPolys[polyStart + 1: polyStartPos[whMax] + ovrPolys[polyStart]]
    yCoordsOut = ovrPolys[polyStartPos[whMax] + ovrPolys[polyStart] + 1 : polyStartPos[whMax] + ovrPolys[polyStart]*2]
    nptsOut    = n_elements(xCoordsOut) - 1
    xCoordsOut = xCoordsOut[0:nptsOut-1]
    yCoordsOut = yCoordsOut[0:nptsOut-1]
    if n_elements(codes) gt 0 then begin
      whCodesToRemove = [-1L]
      for i = 0u, nOvrPolys-1 do $
        if (i ne whMax) then whCodesToRemove = [whCodesToRemove, (*(pVertexIndices[0, i])[0])]
      nCodesToRemove = n_elements(whCodesToRemove)-1
      if (nCodesToRemove ge 1) then whCodesToRemove = whCodesToRemove[1:*]
      whOutCodes = setDifference(ulIndGen(npts), whCodesToRemove)
      nCodes     = n_elements(outCodes)
      if (nCodes ne nptsOut) then begin
        stop ; NOT PROPERLY IMLPEMENTED YET!!! Vertex codes should be inserted/interpolated or removed according to...
        if nCodes lt nptsOut then begin ; add codes
          outCodes = [codes, replicate(0, nptsOut-nCodes)] ; incorrect but matches the output size
        endif
        if nptsOut lt nCodes then begin ; remove codes
          outCodes2 = intArr(nptsOut)
          stop
          ;outCodes2[] = 1
          ;outCodes2[] = 2
          ;outCodes = outCodes2
          outCodes = codes[0:nptsOut-1] ; incorrect but matches the output size
        endif
      endif else outCodes = codes[whOutCodes]
    endif
  endif else begin
    xCoordsOut = xCoords
    yCoordsOut = yCoords
    if arg_present(outCodes) and keyword_set(codes) then outCodes = codes
    fFixOut = 0
  endelse

end


; Object/ROI IDs need to be sorted.
; 1. read points from CSV input file, grouping by "object number" field
; 2. compute convex hull for each group/object
; 3. read points from TXT input file, and write in the TXT output file only those that lie outside the convex hull polygons.
pro srTesseler_filterLocPointsWithRoisFromFiles
  inputLocationsFull = dialog_pickFile(title = 'Select TXT file with all the locations', filter = '*.txt')
  if (n_elements(inputLocationsFull) eq 0) or (inputLocationsFull eq '') then return
  inputLocationsROIs = dialog_pickFile(title = 'Select file with the points to define ROIs of exclusion', filter = '*.csv')
  if (n_elements(inputLocationsROIs) eq 0) or (inputLocationsROIs eq '') then return
  splitChar = ';'
  ;txtFilePrefix = 'SR_Alexa_647_'
  extPos = strPos(inputLocationsROIs, '.csv')
  distThreshold = 10.0
  distThresholdSuffix = distThreshold gt 0 ? ('_d' + strCompress(string(distThreshold), /remove_all)) : ''
  outputLocations = strMid(inputLocationsROIs, 0, extPos) + distThresholdSuffix + '.txt'
  filterRoiArrX = [ptr_new()]
  filterRoiArrY = [ptr_new()]
  eofReached   = 0
  currentRoiId = 0L ; 0 means "no object"
  openR, lunID, inputLocationsROIs, /GET_LUN
  aux1 = ''
  readf, lunID, aux1
  aux2 = strSplit(aux1, ';', /EXTRACT)
  posRoiId = (where(aux2 eq 'Object index'))[0]
  posX = (where(aux2 eq 'x'))[0]
  posy = (where(aux2 eq 'y'))[0]
  xArr = [-1.0]
  yArr = [-1.0]
  while not eof(lunID) do begin
    readf, lunID, aux1
    aux2  = strSplit(aux1, splitChar, /EXTRACT)
    roiId = long(aux2[posRoiId])
    if (roiId eq 0) then continue
    ptX   = float(aux2[posX])
    ptY   = float(aux2[posY])
    if (roiId gt currentRoiId) then begin ; New ROI. Allocate memory
      currentRoiId += 1
      if (currentRoiId gt 1) then begin
        pRoiPointsX = currentRoiId eq 2 ? [ptr_new(xArr, /no_copy)] : [pRoiPointsX, ptr_new(xArr, /no_copy)]
        pRoiPointsY = currentRoiId eq 2 ? [ptr_new(yArr, /no_copy)] : [pRoiPointsY, ptr_new(yArr, /no_copy)]
      endif
      xArr = [ptX]
      yArr = [ptY]
    endif else if (currentRoiId eq roiId) then begin ; Just to be sure, but roiId field should be sorted in input file.
      xArr = [xArr, ptX]
      yArr = [yArr, ptY]
    endif
  endwhile
  if (currentRoiId ge 0) then begin
    pRoiPointsX = currentRoiId eq 2 ? [ptr_new(xArr, /no_copy)] : [pRoiPointsX, ptr_new(xArr, /no_copy)]
    pRoiPointsY = currentRoiId eq 2 ? [ptr_new(yArr, /no_copy)] : [pRoiPointsY, ptr_new(yArr, /no_copy)]
  endif
;stop
  close, lunID
  free_lun, lunID

  nRois = n_elements(pRoiPointsX)
  if (nRois eq 0) then begin ; Nothing to do!?
    stop
  endif else begin
    ;for i = 0u, nRois-1 do print, 'ROI ', i, ' has ', n_elements(*pRoiPointsX[i]), '/', n_elements(*pRoiPointsY[i]), ' x/y points'
    ;ptCount = 0ul & for i = 0u, nRois-1 do ptCount += n_elements(*pRoiPointsX[i])
    ;print, 'Total point count = ', ptCount
    pConvexHullX = ptrArr(nRois)
    pConvexHullY = ptrArr(nRois)
    convexRois = objArr(nRois)
    roiAreaSum = 0.0
    for i = 0u, nRois-1 do begin
      convexPolyIndices = convexHullPolygon(*pRoiPointsX[i], *pRoiPointsY[i])
      convexX = (*pRoiPointsX[i])[convexPolyIndices]
      convexY = (*pRoiPointsY[i])[convexPolyIndices]
      convexRois[i] = obj_new('IDLanROI', convexX, convexY)
      roiAreaSum += polyArea(xCoords = convexX, yCoords = convexY)
      pConvexHullX[i] = ptr_new(convexX, /NO_COPY)
      pConvexHullY[i] = ptr_new(convexY, /NO_COPY)
    endfor
    for i = nRois-1, 0, -1 do ptr_free, pRoiPointsX[i], pRoiPointsY[i] 
  endelse
;stop
  openR, lunID, inputLocationsFull, /GET_LUN
  openW, lunIDout, outputLocations, /GET_LUN
  printf, lunIDout, 'x,y,intensity,frame' ; print first line
  ptCount = -1
  aux3 = ''
  readf, lunID, aux3 ; first line not used
  posX = 0
  posY = 1
  countOutPts = 0u
  countInPts = 0u
  while not eof(lunID) do begin
    readf, lunID, aux3
    aux4 = strSplit(aux3, /EXTRACT)
    ptX = float(aux4[posX])
    ptY = float(aux4[posY])
    fPtOutCum = 1b
    for i = 0u, nRois-1 do begin
      oRoi = convexRois[i]
      fPtOut = (oRoi->containsPoints([ptX], [ptY])) eq 0
      if (fPtOut eq 1) and (distThreshold gt 0) then begin
        scaleFactor = 100.0
        distToCH = calcDistanceFromPointsToPolygon(*pConvexHullX[i]/scaleFactor, *pConvexHullY[i]/scaleFactor, [ptX/scaleFactor], [ptY/scaleFactor])
        if (distToCH le distThreshold/scaleFactor) then fPtOut = 0
      endif
      fPtOutCum = fPtOutCum and fPtOut
      if ~fPtOutCum then break
    endfor
    if fPtOutCum eq 0 then countInPts += 1 else countOutPts += 1
    if (fPtOutCum eq 1) then printf, lunIDout, aux3
  endwhile
  close, lunID, lunIDout
  free_lun, lunID, lunIDout
  for i = 0u, nRois-1 do obj_destroy, convexRois[i]
  print, 'Finished! Number of points in/out/total : ', countInPts, '/', countOutPts, '/', countInPts+countOutPts

end
