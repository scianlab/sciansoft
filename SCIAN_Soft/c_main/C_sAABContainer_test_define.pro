; (this is art) _____________________________ IOISIOI _____________________________ (this is art)
;
; NAME
;
; PURPOSE
;
; HISTORY
;
;   First version by Juan Edo. Rodríguez (2015).
;
; NOTES
;
;   EVERYTHING IS UNDER CONSTRUCTION YET!!! (Except for me, I'm under destruction :S).
;
; DEPENDENCIES
;   geometryFunctions.pro  Helper functions for polygon calculations.
;
; (this is art) _____________________________ IOISIOI _____________________________ (this is art)


;*******************************************************************************
; Test functions.
;*******************************************************************************

;+
; Test functions.
;-
pro C_sAABContainer_Test, fNoInterp = fNoInterp, nPtsMin = nPtsMin, testVector = testVector
  image = bytArr(255,255)
  oAABM = obj_new('C_sAABContainer',$
    image,$
    alphaVal = 1.0,$
    betaVal  = 1.0,$
    gammaVal = 1.0,$
    kappaVal = 1.0,$
    proximityDist           = 1.0,$
    separationDist          = 0.0,$
    contourMaxIterations    = 1000,$
    vfMaxIterations         = 1000,$
    contourSamplingDistance = 1.0,$
    minContourPointCount    = nPtsMin,$
    fVerbose                = 1b)
;*******************************************************************************
  wh1 = n_elements(testVector) gt 0 ? where(testVector eq 1) : -1
  if (wh1 ge 0) then begin

    f1 = 40
    f2 = 60
    offset1 = 150
    p1x = [0.0, 2.0, 2.0, 2.0, 0.0, 0.0]*f1 + offset1
    p1y = [0.0, 0.0, 2.0, 4.0, 4.0, 0.0]*f1
    p2x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
    p2y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2
    proximityDist = 15.0

    nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
    if keyword_set(fNoInterp) then begin
      p1xInt = p1x
      p1yInt = p1y
      p2xInt = p2x
      p2yInt = p2y
    endif else begin
      polygonArcSample, p1x, p1y, p1xInt, p1yInt, NPOINTS=round(nPts*2.5), /FCLOSEOUTPUT
      polygonArcSample, p2x, p2y, p2xInt, p2yInt, NPOINTS=nPts, /FCLOSEOUTPUT
    endelse

    makePolygonsPlot, p1xint, p1yint, p2xint, p2yint, winTitle = 'C_sAABContainer_Test: Test 01', winId = 30

    oAABM->SetParameter, proximityDist = proximityDist

    fSet = oAABM->preSetNumContours(2)
    fContour0 = oAABM->setContour(p1x,p1y, 0, FNOINTERPOLATE = 1b)
    fContour1 = oAABM->setContour(p2x,p2y, 1, FNOINTERPOLATE = 1b)
    fAABMsuccess = oAABM->adjustContours()

  endif
;*******************************************************************************
  wh2 = n_elements(testVector) gt 0 ? where(testVector eq 2) : -1
  if (wh2 ge 0) then begin
    p1x = [1.0,2.0,2.0,6.0,2.0,2.0,1.0,1.0]
    p1y = [0.0,0.0,0.5,1.0,1.5,2.0,2.0,0.0]+10
    p2x = [4.0,5.0,4.5,4.0]
    p2y = [0.0,0.0,2.0,0.0]+10
    proximityDist = 0.1

    makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = 'C_sAABContainer_Test: Test 02', winId = 31, SIZEFACTORWIN=100, psym = 0, linestyle = 0

;    dist12 = distanceBetweenSegments(p1x,p1y,p2x,p2y,proximityCorrespondences = proximityCorrespondences,proximityEndpointCodes = proximityEndpointCodes,$
;                                     crossingList12 = crossingList12,distanceThreshold = proximityDist)
    oAABM->SetParameter, proximityDist = proximityDist

    fSet = oAABM->preSetNumContours(2)
    fContour0 = oAABM->setContour(p1x,p1y, 0, FNOINTERPOLATE = 1b)
    fContour1 = oAABM->setContour(p2x,p2y, 1, FNOINTERPOLATE = 1b)
    fAABMsuccess = oAABM->adjustContours()
    if (fAABMsuccess ne 1) then begin
      print, 'Something went wrong while adjusting the contours. Nothing to show :('
      stop
    endif else begin
      aabmOut = oAABM->getContours(nContours = nObjOut, pAABMvertexColors = pContourColors)
      stop
    endelse
    obj_destroy, oAABM
  endif
  ;*******************************************************************************
  wh3 = n_elements(testVector) gt 0 ? where(testVector eq 3) : -1
  if (wh3 ge 0) then begin
    p1x = [1.0,2.0,3.0,3.0,3.0,4.5,6.0,7.5,9.0,7.5,6.0,4.5,3.0,3.0,3.0,2.0,1.0,1.0,1.0]
    p1y = [1.0,1.0,1.0,2.0,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,8.0,9.0,9.0,9.0,5.0,1.0]+10
    p2x = [7.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0]
    p2y = [1.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]+10
    proximityDist = 0.1

    makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = 'C_sAABContainer_Test: Test 02', winId = 3, SIZEFACTORWIN=100, psym = 0, linestyle = 0
    oplot, p1x,p1y,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x,p2y,psym=2,symsize=0.5,color='FF0000'x
    oAABM->SetParameter, proximityDist = proximityDist

    fSet = oAABM->preSetNumContours(2)
    fContour0 = oAABM->setContour(p1x,p1y, 0, FNOINTERPOLATE = 1b)
    fContour1 = oAABM->setContour(p2x,p2y, 1, FNOINTERPOLATE = 1b)
    fAABMsuccess = oAABM->adjustContours()
  endif
  ;*******************************************************************************
  wh4 = n_elements(testVector) gt 0 ? where(testVector eq 4) : -1
  if (wh4 ge 0) then begin
    p1x = [1.0,2.0,3.0,3.0,3.0,4.5,6.0,7.5,9.0,7.5,6.0,4.5,3.0,3.0,3.0,2.0,1.0,1.0,1.0]
    p1y = [1.0,1.0,1.0,2.0,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,8.0,9.0,9.0,9.0,5.0,1.0]+10
    p2x = [5.9,7.6,7.6,7.6,7.6,7.6,7.6,7.6,7.6,7.6,5.9,5.9,5.9,5.9,5.9,5.9,5.9,5.9,5.9]
    p2y = [1.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]+10
    proximityDist = 0.1

    makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = 'C_sAABContainer_Test: Test 02', winId = 34, SIZEFACTORWIN=100, psym = 0, linestyle = 0
    oAABM->SetParameter, proximityDist = proximityDist

    fSet = oAABM->preSetNumContours(2)
    fContour0 = oAABM->setContour(p1x,p1y, 1, FNOINTERPOLATE = 1b)
    fContour1 = oAABM->setContour(p2x,p2y, 0, FNOINTERPOLATE = 1b)
    fAABMsuccess = oAABM->adjustContours()
  endif
end


pro Test, testVector = testVector
  image = bytArr(255,255)
  oAABM = obj_new('C_sAABContainer',$
    image,$
    alphaVal = 1.0,$
    betaVal  = 1.0,$
    gammaVal = 1.0,$
    kappaVal = 1.0,$
    proximityDist           = 1.0,$
    separationDist          = 0.0,$
    contourMaxIterations    = 1000,$
    vfMaxIterations         = 1000,$
    contourSamplingDistance = 1.0,$
    minContourPointCount    = nPtsMin,$
    fVerbose                = 1b)
  
  if (n_elements(testVector) eq 1) then begin  
    case testVector of
      1: begin
        test1, p1x_ = p1x, p1y_ = p1y, p2x_ = p2x, p2y_ = p2y
        proximityDist = 2.0;15.0
      end
      2: begin
        test2, p1x_ = p1x, p1y_ = p1y, p2x_ = p2x, p2y_ = p2y, fNoInterp = 1b
        proximityDist = 0.1
      end
      3: begin
        test3, p1x_ = p1x, p1y_ = p1y, p2x_ = p2x, p2y_ = p2y, fNoInterp = 1b
        proximityDist = 0.1
      end
      4: begin
        test4, p1x_ = p1x, p1y_ = p1y, p2x_ = p2x, p2y_ = p2y
        proximityDist = 0.1
      end
      5: begin
        test5, p1x_ = p1x, p1y_ = p1y, p2x_ = p2x, p2y_ = p2y
        proximityDist = 1
      end
      else: begin
        print, 'Error: This example has no been created.'
        return
      end
    endcase
  endif else begin
    return
  endelse
  
  oAABM->SetParameter, proximityDist = proximityDist
  fSet = oAABM->preSetNumContours(2)
  testAndCorrectCWOrientation, p1x, p1y, xOut = p1xCW, yOut = p1yCW
  testAndCorrectCWOrientation, p2x, p2y, xOut = p2xCW, yOut = p2yCW
  fContour0 = oAABM->setContour(p1xCW,p1yCW, 0, FNOINTERPOLATE = 1b)
  fContour1 = oAABM->setContour(p2xCW,p2yCW, 1, FNOINTERPOLATE = 1b)
  fAABMsuccess = oAABM->adjustContoursB()
  
  return
;TODO pending to remove 
    if (fAABMsuccess ne 1) then begin
      print, 'Something went wrong while adjusting the contours. Nothing to show :('
      stop
    endif else begin
      aabmOut = oAABM->getContours(nContours = nObjOut, pAABMvertexColors = pContourColors)
      stop
    endelse
    obj_destroy, oAABM
  ;*******************************************************************************
  
end

;*******************************************************************************
pro test1, p1x_ = p1x_, p1y_ = p1y_, p2x_ = p2x_, p2y_ = p2y_, fNoInterp = fNoInterp, nPtsMin = nPtsMin
  print, 'Initializing Test 1...'
  f1 = 40
  f2 = 60
  offset1 = 150
  p1x = [0.0, 2.0, 2.0, 2.0, 0.0, 0.0]*f1 + offset1 
  p1y = [0.0, 0.0, 2.0, 4.0, 4.0, 0.0]*f1 + 30
  p2x = [2.8, 4.0, 4.0, 2.8, 2.8]*f2
  p2y = [0.0, 0.0, 1.0, 1.0, 0.0]*f2 + 30

  
  nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1x_ = p1x
    p1y_ = p1y
    p2x_ = p2x
    p2y_ = p2y
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 01', winId = 1, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
  endif else begin
    polygonArcSample, p1x, p1y, p1x_, p1y_, NPOINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2x_, p2y_, NPOINTS=nPts, /FCLOSEOUTPUT
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 01', winId = 1, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
    oplot, p1x,p1y,psym=4,symsize=1.0,color='00FFFF'x
    oplot, p2x,p2y,psym=4,symsize=1.0,color='FFFF00'x
  endelse
end

;*******************************************************************************
pro test2, p1x_ = p1x_, p1y_ = p1y_, p2x_ = p2x_, p2y_ = p2y_, fNoInterp = fNoInterp, nPtsMin = nPtsMin
  print, 'Initializing Test 2...'
  f1 = 30
  f2 = 30
  p1x = [1.0,2.0,2.0,6.0,2.0,2.0,1.0,1.0]*f1
  p1y = [0.0,0.0,0.5,1.0,1.5,2.0,2.0,0.0]*f1
  p2x = [4.0,5.0,4.5,4.0]*f2
  p2y = [0.0,0.0,2.0,0.0]*f2
  
  nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1x_ = p1x
    p1y_ = p1y
    p2x_ = p2x
    p2y_ = p2y
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 02', winId = 2, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
  endif else begin
    polygonArcSample, p1x, p1y, p1x_, p1y_, NPOINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2x_, p2y_, NPOINTS=nPts, /FCLOSEOUTPUT
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 02', winId = 2, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
    oplot, p1x,p1y,psym=4,symsize=1.0,color='00FFFF'x
    oplot, p2x,p2y,psym=4,symsize=1.0,color='FFFF00'x
  endelse
end

;*******************************************************************************
pro test3, p1x_ = p1x_, p1y_ = p1y_, p2x_ = p2x_, p2y_ = p2y_, fNoInterp = fNoInterp, nPtsMin = nPtsMin
  print, 'Initializing Test 3...'
  f1 = 10
  f2 = 1
  p1x = [1.0,2.0,3.0,3.0,3.0,4.5,6.0,7.5,9.0,7.5,6.0,4.5,3.0,3.0,3.0,2.0,1.0,1.0,1.0]*f1
  p1y = [1.0,1.0,1.0,2.0,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,8.0,9.0,9.0,9.0,5.0,1.0]*f1
  p2x = [7.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,8.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0,7.0]*f1
  p2y = [1.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]*f1
  
  nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1x_ = p1x
    p1y_ = p1y
    p2x_ = p2x
    p2y_ = p2y
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 03', winId = 3, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
  endif else begin
    polygonArcSample, p1x, p1y, p1x_, p1y_, NPOINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2x_, p2y_, NPOINTS=nPts, /FCLOSEOUTPUT
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 03', winId = 3, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
    oplot, p1x,p1y,psym=4,symsize=1.0,color='00FFFF'x
    oplot, p2x,p2y,psym=4,symsize=1.0,color='FFFF00'x
  endelse
end

;*******************************************************************************
pro test4, p1x_ = p1x_, p1y_ = p1y_, p2x_ = p2x_, p2y_ = p2y_, fNoInterp = fNoInterp, nPtsMin = nPtsMin
  print, 'Initializing Test 4...'
  f1 = 30
  f2 = 30
  p1x = [1.0,2.0,3.0,3.0,3.0,4.5,6.0,7.5,9.0,7.5,6.0,4.5,3.0,3.0,3.0,2.0,1.0,1.0,1.0]*f1
  p1y = [1.0,1.0,1.0,2.0,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,8.0,9.0,9.0,9.0,5.0,1.0]*f1
  p2x = [5.9,7.6,7.6,7.6,7.6,7.6,7.6,7.6,7.6,7.6,5.9,5.9,5.9,5.9,5.9,5.9,5.9,5.9,5.9]*f2
  p2y = [1.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]*f2
  
  nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1x_ = p1x
    p1y_ = p1y
    p2x_ = p2x
    p2y_ = p2y
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 04', winId = 4, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
  endif else begin
    polygonArcSample, p1x, p1y, p1x_, p1y_, NPOINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2x_, p2y_, NPOINTS=nPts, /FCLOSEOUTPUT
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 04', winId = 4, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
    oplot, p1x,p1y,psym=4,symsize=1.0,color='00FFFF'x
    oplot, p2x,p2y,psym=4,symsize=1.0,color='FFFF00'x
  endelse
end

;*******************************************************************************
pro test5, p1x_ = p1x_, p1y_ = p1y_, p2x_ = p2x_, p2y_ = p2y_, fNoInterp = fNoInterp, nPtsMin = nPtsMin
  print, 'Initializing Test 5...'
  f1 = 30
  f2 = 30
  p1x = [1.0,2.0,4.0,5.0,7.0,8.0,8.0,7.0,5.0,4.0,2.0,1.0,1.0]*f1
  p1y = [4.0,3.0,2.0,2.0,3.0,4.0,5.0,6.0,7.0,7.0,6.0,5.0,4.0]*f1
  p2x = [6.0,7.0,8.0,9.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,9.0,8.0,7.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0]*f2
  p2y = ([1.0,1.0,1.0,1.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,8.0,8.0,8.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]+1)*f2
  
  nPts = keyword_set(nPtsMin) ? nPtsMin > 10 : 30
  if keyword_set(fNoInterp) then begin
    p1x_ = p1x
    p1y_ = p1y
    p2x_ = p2x
    p2y_ = p2y
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 05', winId = 5, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
  endif else begin
    polygonArcSample, p1x, p1y, p1x_, p1y_, NPOINTS=round(nPts*2.5), /FCLOSEOUTPUT
    polygonArcSample, p2x, p2y, p2x_, p2y_, NPOINTS=nPts, /FCLOSEOUTPUT
    makePolygonsPlot, p1x_, p1y_, p2x_, p2y_, winTitle = 'C_sAABContainer_Test: Test 05', winId = 5, SIZEFACTORWIN=1, psym = 0, linestyle = 0
    oplot, p1x_,p1y_,psym=2,symsize=0.5,color='0000FF'x
    oplot, p2x_,p2y_,psym=2,symsize=0.5,color='FF0000'x
    oplot, p1x,p1y,psym=4,symsize=1.0,color='00FFFF'x
    oplot, p2x,p2y,psym=4,symsize=1.0,color='FFFF00'x
  endelse
end
