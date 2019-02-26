; computeSVMseparationFor2Dpoints
;
; separationFnType...
;    0: LINEAR
;    1: POLY    (set the kernelPolyDegree keyword to specify the degree)
;    2: RBF     not yet
;    3: SIGMOID not yet
function computeSVMseparationFor2Dpoints, p1x, p1y, p2x, p2y, separationFnType, suppVectorsOut, suppVectorsIndexOut, signedAlphasOut, bOut,$
                                          krnlPolyDegree = krnlPolyDegree,$
                                          krnlRBFgamma   = krnlRBFgamma,$
                                          krnlCoefLin    = krnlCoefLin,$
                                          krnlCoefConst  = krnlCoefConst,$
                                          param_SVM_C    = param_SVM_C,$
                                          pDomainX = pDomainX, pDomainY = pDomainY, dataOut = dataOut, fUseLibSVM = fUseLibSVM
  np1 = size(p1x, /n_elements)
  np2 = size(p2x, /n_elements)
  if (np1 lt 1) or (np2 lt 1) then begin
    print, 'computeSVMseparationFor2Dpoints - error: not enough points to compute separation'
    return, -1
  endif

  numType = keyword_set(fUseLibSVM) ? 5 : 4 ; force float (4) (5-double not supported in current implementation)

  separationFnType = (separationFnType > 0) < 3
  krnlPolyDegree = separationFnType gt 0 ? (keyword_set(krnlPolyDegree) ? long(krnlPolyDegree) : 3L) : 1L
  if (krnlPolyDegree eq 1) then separationFnType = 0
  krnlRBFgamma   = keyword_set(krnlRBFgamma)  ? double(krnlRBFgamma)  : 1.0d ; krnlRBFgamma should be float when fUseLibSVM is set
  krnlCoefLin    = keyword_set(krnlCoefLin)   ? double(krnlCoefLin)   : 1.0d
  krnlCoefConst  = keyword_set(krnlCoefConst) ? double(krnlCoefConst) : 1.0d
  param_SVM_C    = keyword_set(param_SVM_C)   ? double(param_SVM_C)   : 0.0d

  minPointCount = np1 < np2
  ; TODO Now forcing polynomial degree adjustment depending on the input size.
  case 1 of
  (separationFnType eq 1) and (krnlPolyDegree gt 2) and (minPointCount lt 2): separationFnType = 0
  separationFnType eq 1: begin
    if (minPointCount gt  3) then krnlPolyDegree = 4
    if (minPointCount gt 10) then krnlPolyDegree = 5
  endcase
  else:
  endcase
  if (separationFnType eq 1) then print, 'krnlPolyDegree = ', krnlPolyDegree

  ; Setup output variables...  not-nice-but-doable form to pre-set output arrays before calling the external function.
  nDim = 2 ; for now, 2D case only but it is possible to make a generic case
  baseFactor = 20 ; TODO JJ/FASL: Ugly but necessary since external functions (C/C++) doen't work with variable length outputs yet.
  suppVectorsMaxElemCount = baseFactor * nDim * (separationFnType + krnlPolyDegree) ; arbitrary but enough for now... ?
  suppVectorsOut      = dblArr(suppVectorsMaxElemCount * nDim * 2)
  signedAlphasOut     = dblArr(suppVectorsMaxElemCount)
  suppVectorsIndexOut = lonArr(suppVectorsMaxElemCount)
  bOut = -1.0d
  suppVectorsCountOut = 0L ; Aux. variable, used to trim output arrays.

  libName  = keyword_set(fUseLibSVM) ? 'svm_lib' : 'svm_light'
  funNameA = keyword_set(fUseLibSVM) ? 'find_separationFor2Dpoints2' : 'find_separationFor2Dpoints'
  funNameB = keyword_set(fUseLibSVM) ? 'find_separationFor2DpointsWithData2' : 'find_separationFor2DpointsWithData'
  fUseDefaultKernelValues = 0b
  dllLocation = getDLLfilename(libName, /GETFILENAMEANDPATH)
  nDom = keyword_set(pDomainX) ? n_elements(*pDomainX) : 0
  if (nDom eq 0) then begin
    out = keyword_set(fUseLibSVM) $
        ? call_external(dllLocation, funNameA,$
                        fix(p1x, type = numType), fix(p1y, type = numType), np1,$
                        fix(p2x, type = numType), fix(p2y, type = numType), np2,$
                        param_SVM_C,$
                        fix(fUseDefaultKernelValues, type = 2), separationFnType,$
                        krnlPolyDegree, krnlRBFgamma, krnlCoefLin, krnlCoefConst,$
                        suppVectorsCountOut, suppVectorsIndexOut,$
                        return_type = 14, /unload) $
        : call_external(dllLocation, funNameA,$
                        fix(p1x, type = numType), fix(p1y, type = numType), np1,$
                        fix(p2x, type = numType), fix(p2y, type = numType), np2,$
                        param_SVM_C,$
                        separationFnType, krnlPolyDegree, krnlRBFgamma, krnlCoefLin, krnlCoefConst,$
                        suppVectorsCountOut, suppVectorsIndexOut,$
                        suppVectorsOut, signedAlphasOut, bOut,$
                        /unload)
  endif else begin
    dataOut = dblArr(size(*pDomainX, /DIMENSIONS))
    out = keyword_set(fUseLibSVM) $
        ? call_external(dllLocation, funNameB,$
                        fix(p1x, type = numType), fix(p1y, type = numType), np1,$
                        fix(p2x, type = numType), fix(p2y, type = numType), np2,$
                        *pDomainX, *pDomainY, nDom,$
                        param_SVM_C,$
                        fix(fUseDefaultKernelValues, type = 2), separationFnType,$
                        krnlPolyDegree, krnlRBFgamma, krnlCoefLin, krnlCoefConst,$
                        suppVectorsCountOut, suppVectorsIndexOut,$
                        suppVectorsOut, signedAlphasOut, bOut, dataOut,$
                        return_type = 14, /unload) $
        : call_external(dllLocation, funNameB,$
                        fix(p1x, type = numType), fix(p1y, type = numType), np1,$
                        fix(p2x, type = numType), fix(p2y, type = numType), np2,$
                        *pDomainX, *pDomainY, nDom,$
                        param_SVM_C,$
                        separationFnType, krnlPolyDegree, krnlRBFgamma, krnlCoefLin, krnlCoefConst,$
                        suppVectorsCountOut, suppVectorsIndexOut,$
                        suppVectorsOut, signedAlphasOut, bOut, dataOut,$
                        /unload)
  endelse
  if (out lt 1) then stop
  if suppVectorsCountOut ge (n_elements(suppVectorsIndexOut)-1) then begin
    stop ; TODO VC SOS. Why does it happen?.
  endif
  suppVectorsIndexOut = suppVectorsIndexOut[0:   (suppVectorsCountOut>1)-1]
  signedAlphasOut     = signedAlphasOut    [0:   (suppVectorsCountOut>1)-1]
  suppVectorsOut      = suppVectorsOut     [0: 2*(suppVectorsCountOut>1)-1]

  ; The output is not necessarily sorted (Aug. 2013), so we sort...
  sortedAlphas = sort(signedAlphasOut)
  signedAlphasOut     = signedAlphasOut[sortedAlphas]
  suppVectorsIndexOut = suppVectorsIndexOut[sortedAlphas]
  suppVectorsOutTmp   = [-1]

  for i = 0L, n_elements(sortedAlphas)-1 do begin
    pos = sortedAlphas[i] * 2
    suppVectorsOutTmp = [suppVectorsOutTmp, suppVectorsOut[pos:pos+1]]
  endfor
  suppVectorsOut = suppVectorsOutTmp[1:*]

  return, suppVectorsCountOut
end


; Not yet ready...
pro computeSVMseparationFor2Dpoints_polygonExample

  ; 1st test, two rectangles that touch each other by the side... when interpolated, they become oval shapes that overlap
  p1x = [0.0, 2.0, 2.0, 0.0, 0.0]
  p1y = [0.0, 0.0, 2.0, 2.0, 0.0]
  polygonArcSample, p1x, p1y, p1xInt, p1yInt, NPOINTS = 20
  oRoi1 = obj_new('IDLanROI', p1xInt, p1yInt)

  p2x = [2.0, 4.0, 4.0, 2.0, 2.0]
  p2y = [1.0, 1.0, 2.0, 2.0, 1.0]
  polygonArcSample, p2x, p2y, p2xInt, p2yInt, NPOINTS = 20
  oRoi2 = obj_new('IDLanROI', p2xInt, p2yInt)

  containedIn1 = oRoi1->containsPoints(p2xInt, p2yInt)
  whereIn1 = where(containedIn1 eq 1, nToSeparate1)

  containedIn2 = oRoi2->containsPoints(p1xInt, p1yInt)
  whereIn2 = where(containedIn2 eq 1, nToSeparate2)

  if (nToSeparate1 gt 0) and (nToSeparate2 gt 0) then begin
    p1x_sep = p1xInt[whereIn2]
    p1y_sep = p1yInt[whereIn2]

    p2x_sep = p2xInt[whereIn1]
    p2y_sep = p2yInt[whereIn1]

    separationFnType = 0 ; 0->linear, 1->polynomial
    polyDegree = 3
    nVectors = computeSVMseparationFor2Dpoints(p1x_sep, p1y_sep, p2x_sep, p2y_sep, separationFnType,$
                                               suppVectorsOut, suppVectorsIndexOut, signedAlphasOut, bOut,$
                                               krnlPolyDegree = polyDegree)
  endif
  print, 'suppVectorsIndexOut ', suppVectorsIndexOut
  print, 'suppVectorsOut '     , suppVectorsOut
  print, 'signedAlphasOut '    , signedAlphasOut
  print, 'bOut ', bOut

  print, 'Test 2D separation for points. Done!'
end


; make2DsvmTestDomain
;
;  Given two sets of 2D points, returns a 2D grid of points to be evaluated against a support vector machine.
;  The constructed grid does include the coordinate range of the input point sets, plus an additional margin
;  or distance threshold. The size of the grid is given by an input value
;
; ARGUMENTS
;  p1x|p1y|p2x|p2y   The input point sets, given by their x|y coordinates.
;  distanceThreshold Optional margin for constructing the evaluation domain.
;  nDivisions        The number of domain divisions in the x|y axes, that yields a grid of
;                    (nDivisions+1)*(nDivisions+1) elements.
;                    Default value: 64.
function make2DsvmTestDomain, p1x, p1y, p2x, p2y, distanceThreshold = distanceThreshold, nDivisions = nDivisions

  minx = min(p1x, max = max1x) < min(p2x, max = max2x)
  maxx = max1x > max2x

  miny = min(p1y, max = max1y) < min(p2y, max = max2y)
  maxy = max1y > max2y

  ; threshold distance for the separation
  if (n_elements(distanceThreshold) eq 0) then distanceThreshold = 0.0

  ; Params for plot and grid used on SVM-hyperplane distance map
  if ~keyword_set(nDivisions) then nDivisions = 64 ; size of test image nDivisions * nDivisions

  minx -= distanceThreshold
  maxx += distanceThreshold
  miny -= (distanceThreshold); + 1)
  maxy += (distanceThreshold); + 1)

  xrange = maxx - minx
  yrange = maxy - miny

  stepSizeX = xrange / nDivisions
  stepSizeY = yrange / nDivisions
;  stepSizeX = 0.008
;  stepSizeY = 0.008

  pDomX = ptr_new(fltArr(nDivisions+1, nDivisions+1))
  pDomY = ptr_new(fltArr(nDivisions+1, nDivisions+1))

  for i = 0, nDivisions do begin
    (*pDomX)[*,i] = minx + fIndGen(nDivisions+1) * stepSizeX
    (*pDomY)[*,i] = miny + replicate(i*stepSizeY, nDivisions+1)
  endfor
  return, [pDomX, pDomY]
end


pro s_SVMnormalizeXYpoints, p1x, p1y, p2x, p2y, p1xOut = p1xOut, p1yOut = p1yOut, p2xOut = p2xOut, p2yOut = p2yOut, fKeepRatio = fKeepRatio

  minxOri = min(p1x, max = max1xOri) < min(p2x, max = max2xOri)
  maxxOri = max1xOri > max2xOri
  minyOri = min(p1y, max = max1yOri) < min(p2y, max = max2yOri)
  maxyOri = max1yOri > max2yOri
  p1xOut = p1x
  p1yOut = p1y
  p2xOut = p2x
  p2yOut = p2y

  if (minxOri ne 0) then begin
    p1xOut -= minxOri
    p2xOut -= minxOri
    minx = 0
  endif

  if (minyOri ne 0) then begin
    p1yOut -= minyOri
    p2yOut -= minyOri
    miny = 0
  endif

  maxx = maxxOri - minxOri
  maxy = maxyOri - minyOri

  if keyword_set(fKeepRatio) then begin
    maxxy = maxx > maxy
    p1xOut /= maxxy
    p1yOut /= maxxy
    p2xOut /= maxxy
    p2yOut /= maxxy
  endif else begin
    if (maxx ne 0) then begin
      p1xOut /= maxx
      p2xOut /= maxx
    endif
    if (maxy ne 0) then begin
      p1yOut /= maxy
      p2yOut /= maxy
    endif

  endelse
end


pro s_SVMcallTest, thDist = thDist, fUseLibSVM = fUseLibSVM

  dllLocation = getDLLfilename('svm_light', /GETFILENAMEANDPATH)

;  f1 = call_external(dllLocation, 'test', 1, /unload)
;  print, 'result from this call:', f1
;
;  p1x = [2.0, 3.0]
;  p1y = [1.0, 2.0]
;
;  p2x = [2.0, 3.0]
;  p2y = [3.0, 4.0]
;
;  kernelType = 0 ; 0->linear
;
;  ;f2 = computeSVMseparationFor2Dpoints(p1x, p1y, p2x, p2y, kernelType, suppVectorsOut, suppVectorsIndexOut, signedAlphasOut, bOut)
;  print, 'result from this call (number of support vectors):', f2
;  print, 'kernelType ', kernelType
;  print, 'suppVectorsIndexOut ', suppVectorsIndexOut
;  print, 'suppVectorsOut ', suppVectorsOut
;  print, 'signedAlphasOut ', signedAlphasOut
;  print, 'bOut ', bOut
;
;  p1x_ = [2.0, 3.0, 4.0]
;  p1y_ = [0.0, 0.0, 0.0]
;
;  p2x_ = [2.0, 3.0, 4.0, 3.0]
;  p2y_ = [2.0, 2.0, 2.0, 3.0]
;
;  kernelType = 0 ; 0->linear
;
;  ;f3 = computeSVMseparationFor2Dpoints(p1x_, p1y_, p2x_, p2y_, kernelType, suppVectorsOut, suppVectorsIndexOut, signedAlphasOut, bOut)
;  print, 'result from this call (number of support vectors):', f3
;  print, 'kernelType ', kernelType
;  print, 'suppVectorsIndexOut ', suppVectorsIndexOut
;  print, 'suppVectorsOut '     , suppVectorsOut
;  print, 'signedAlphasOut '    , signedAlphasOut
;  print, 'bOut ', bOut

  p1x_ = [0, 1, 2, 3, 4, 5, 8]
  p1y_ = [3, 5, 6, 4, 3, 3, -10]
  p2x_ = [3, 4  , 5, 7, 8, 9, 10]
  p2y_ = [4, 5.5, 7, 10, 10, 4, 4]
  ;polygonArcSample, p1x, p1y, p1x_, p1y_, NPOINTS = 10, FCLOSEOUTPUT=0
  ;polygonArcSample, p2x, p2y, p2x_, p2y_, NPOINTS = 10, FCLOSEOUTPUT=0

;  p1x = [72.77];670]
;  p1y = [38.53];419]
;  p2x = [73.19];033]
;  p2y = [38.50];917]
  ;polygonArcSample, p1x_, p1y_, p1x, p1y, NPOINTS = 20
  ;polygonArcSample, p2x_, p2y_, p2x, p2y, NPOINTS = 20
;  p1x_ = [0.0, 1.0, 2.0, 3.0, 5.0]
;  p1y_ = [2.1, 2.1, 2.1, 2.1, 2.1]
;  p2x_ = [0.0, 1.0, 2.0, 3.0, 4.0]
;  p2y_ = [2.0, 2.0, 2.0, 2.0, 2.0]

  ; threshold distance for the separation
  if (n_elements(thDist) eq 0) then thDist = 0.5
  s_SVMnormalizeXYpoints, p1x_, p1y_, p2x_, p2y_, P1XOUT=p1x, P1YOUT=p1y, P2XOUT=p2x, P2YOUT=p2y
  ; Params for plot and grid used on SVM-hyperplane distance map 
  nDivisions = 640;256 ; size of test image nDivisions * nDivisions
  pDomain = make2DsvmTestDomain(p1x, p1y, p2x, p2y, nDivisions = nDivisions)
  pDomainOri = make2DsvmTestDomain(p1x_, p1y_, p2x_, p2y_, nDivisions = nDivisions)

  minxOri = min(p1x, max = max1xOri) < min(p2x, max = max2xOri)
  maxxOri = max1xOri > max2xOri
  minyOri = min(p1y, max = max1yOri) < min(p2y, max = max2yOri)
  maxyOri = max1yOri > max2yOri

  kernelType = 1
  kernelPolyDegree = 5;9
  kernelRBFgamma   = 1.0d
  kernelCoefLin    = 1.0d
  kernelCoefConst  = 1.0d

  ;http://svmlight.joachims.org/
  ;SVM param C...-> recommended  default... [avg. x*x]^-1
  param_SVM_C = ((kernelType eq 0) or ((kernelType eq 1) and (kernelPolyDegree eq 1))) ? 0.0d : 2.0d
  ;param_SVM_C = 1/ min([mean(p1x*p1x), mean(p2x*p2x), mean(p1y*p1y) , mean(p2y*p2y)])

  f3 = computeSVMseparationFor2Dpoints(p1x, p1y, p2x, p2y, $                                                 ; input ROI elements
                                       kernelType, suppVectorsOut, suppVectorsIndexOut, signedAlphas, bOut,$ ; input separation calc. parameters
                                       krnlPolyDegree = kernelPolyDegree, $
                                       krnlCoefConst  = kernelCoefConst, $
                                       krnlRBFgamma   = kernelRBFgamma, $
                                       krnlCoefLin    = kernelCoefLin, $
                                       param_SVM_C    = param_SVM_C, $
                                       ;pDomainX = pDataX, pDomainY = pDataY, $                               ; input x/y coords. for evaluating separation
                                       pDomainX = pDomain[0], pDomainY = pDomain[1], $                       ; input x/y coords. for evaluating separation
                                       dataOut = dataOut, $                                                 ; output separation values for x/y coords
                                       fUseLibSVM     = fUseLibSVM)
  print, 'result from this call:', f3
  contour, dataOut, path_xy = borderOut, levels = [0.0], closed = 0, /PATH_DATA_COORDS
  marginDist = 0.5
  makeSeparationPlot, p1x_, p1y_, p2x_, p2y_, dataOut,$
                      minx = minxOri-0.5, maxx = maxxOri+0.5,$
                      miny = minyOri-0.5, maxy = maxyOri+0.5,$
                      window_title = 'SVM Separation Plot'
  borderX = interpolate(*pDomainOri[0], borderOut[0,*], borderOut[1,*], cubic=-0.5)
  borderY = interpolate(*pDomainOri[1], borderOut[0,*], borderOut[1,*], cubic=-0.5)
  oplot, borderX, borderY
  ;print, dataout
  ;outBoundaries = getBorderPolylinesFrom2DistanceMap(dataOut, thDist, upperBorder, lowerBorder)
  print, '2D tests done!'
  return

  p1x = [2.0, 3.0, 3.0]
  p1y = [1.0, 2.0, 2.0]
  p1z = [0.0, 0.0, 1.0]
  np1 = n_elements(p1x)

  p2x = [2.0, 3.0, 3.0]
  p2y = [3.0, 4.0, 4.0]
  p2z = [0.0, 0.0, 1.0]
  np2 = n_elements(p2x)

  kernelType    = 0 ; 0->linear
  modelFileType = 0 ; 0->defaultFile, 1->other
  numType       = 4 ; 4->float, 5->double

  pOut  = [-1]
  npOut = n_elements(pOut)
  outputType = 140
  f3 = call_external(dllLocation, 'find_separationFor3Dpoints',$
                                   fix(p1x, type = numType), fix(p1y, type = numType), fix(p1z, type = numType), np1,$
                                   fix(p2x, type = numType), fix(p2y, type = numType), fix(p2z, type = numType), np2,$
                                   kernelType, pOut, npOut, outputType,$
                                   modelFileType, /unload)
  print, 'result from this call:', f3
;  np1 = 10
;  np2 = 14
;  p1x = dblArr(np1)
;  p1y = dblArr(np1)
;  p1z = dblArr(np1)
;  p2x = dblArr(np2)
;  p2y = dblArr(np2)
;  p2z = dblArr(np2)
;  kernelType = 0
;  outputType = 0
;  npOut = 1
  print, 'Test done!'
end


; 2013.08.16
; Using IDL 7.1 and MSVC 9.0 (Visual Studio 2008) this kind of function call still makes IDL crash
; (trying to get IDL_VPTR from C code).
pro s_SVMcallTestVPTR
   dllLocation = getDLLfilename('svm_light', /GETFILENAMEANDPATH)
   print, 'WARNING: this function will make IDL crash'
   stop
   x = call_external(dllLocation, 'idl_vptr_testArray1D', 3, 4)
   print, x
end
