; WARNING: Not fully implemented. Look for polygonLineSample method which contains line interpolation.
pro lineBresenhamDecimalCalc, x0, y0, x1, y1, outX, outY, npts = npts

  deltax = x1 - x0
  deltay = y1 - y0
  npts = keyword_set(npts) ? npts : round(abs(deltax))
  if (npts eq 0) then npts = round(abs(deltay))
  if (npts eq 0) then return

  npts += 1
  outX  = intArr(npts)
  outY  = intArr(npts)
  error = 0d
  if (deltax eq 0) then begin
    outX = replicate(x0, npts)
    outY = deltay / (npts-1) * indGen(npts) + y0
    return
  endif
  deltaerr = abs(1.0d * deltay / deltax) ; note that this division needs to be done in a way that preserves the fractional part
  y   = y0
  pos = 0
  incx = (x1-x0) / (npts-1)
  for x = x0, x1, incx do begin
    outX[pos] = x
    outY[pos] = y
    error += deltaerr
    if (error ge 0.5) then begin
       y     += 1
       error -= 1.0
    endif
    pos += 1
  endfor
end


pro lineBresenhamDecimalCalc_test
  lineBresenhamDecimalCalc, 0, 10, 0, 10, x, y, npts = 10
  print, y
  lineBresenhamDecimalCalc, 0, 10, 10, 10, x, y, npts = 10
  print, y
  lineBresenhamDecimalCalc, 10, 10, 0, 10, x, y, npts = 10
  print, y
end


;The problem with the approach above is that computers operate relatively slowly
;on fractional numbers like error and deltaerr; moreover, errors can accumulate 
;over many floating-point additions. Working with integers will be both faster 
;and more accurate. The trick we use is to multiply all the fractional numbers 
;(including the constant 0.5) in the code above by deltax, which enables us to 
;express them as integers. This results in a divide inside the main loop, 
;however. To deal with this we modify how error is initialized and used so that 
;rather than starting at zero and counting up towards 0.5, it starts at 0.5 and 
;counts down to zero. The new program looks like this:
pro lineBresenhamIntegerCalc, x0_, y0_, x1_, y1_, outX, outY, npts = npts, fPlot = fPlot, fSwappedOutputXY = fSwappedOutputXY, fReverse = fReverse

  ;npts = keyword_set(npts) ? npts : round(abs(x1_ - x0_))
  ;nTyp = size(x0_, /TYPE)
  x0 = x0_
  y0 = y0_
  x1 = x1_
  y1 = y1_

  fSteep = abs(y1 - y0) gt abs(x1 - x0)
  if fSteep then begin
    tmp = x0
    x0  = y0
    y0  = tmp
    tmp = x1
    x1  = y1
    y1  = tmp
  endif
  fSwappedOutputXY = fSteep

  fReverse = 0
  if (x0 gt x1) then begin
    tmp = x0
    x0  = x1
    x1  = tmp
    tmp = y0
    y0  = y1
    y1  = tmp
    fReverse = 1
   endif

   deltax = x1 - x0
   deltay = abs(y1 - y0)
   error  = deltax / 2
   y      = y0
   ystep  = (y0 lt y1) ? 1 : -1
   pos    = 0
   outX = intArr(round(deltax)+1)
   outY = intArr(round(deltax)+1)
   for x = x0, x1 do begin
     if fSteep then begin
       if keyword_set(fPlot) then plot, y, x
       outX[pos] = y
       outY[pos] = x
     endif else begin
       if keyword_set(fPlot) then plot, x, y
       outX[pos] = x
       outY[pos] = y
     endelse
     error -= deltay
     if (error lt 0) then begin
       y     += ystep
       error += deltax
     endif
     pos += 1
   endfor
   if fSteep then begin
     tmp  = outY
     outY = outX
     outY = tmp
   endif
   if fReverse then begin
     outX = reverse(outX)
     outY = reverse(outY)
   endif
end


; polygonInterpolateSegments
;
; Given an input polygon, defined by an ordered set of x|y integer vertices, 
; this method performs an interpolation to generate a polygon with points at 
; each integer coordinate point that the input polygon covers. The interpolation
; is performed by applying a line raster algorithm to each polygon segment, so 
; that the output polygon has segments defined by adjacent integer points 
; (or pixels).
pro polygonInterpolateSegments, xCoords, yCoords, xOut = xOut, yOut = yOut, interpMethod = interpMethod
  nPts = n_elements(xCoords)
  if (nPts lt 3) then return
  polyClosed = (xCoords[0] eq xCoords[nPts-1]) and (yCoords[0] eq yCoords[nPts-1])
  nSegments  = n_elements(xCoords)
  if polyClosed then nSegments -= 1
  xOut = xCoords[0]
  yOut = yCoords[0]
  if (n_elements(interpMethod) eq 0) then interpMethod = 0
  for i = 0u, nSegments-1 do begin
    if ((i eq nPts-1) and ~polyClosed) then break
    case interpMethod of
    0: lineBresenhamIntegerCalc, xCoords[i], yCoords[i], xCoords[i+1], yCoords[i+1], xOutSeg, yOutSeg, $
                                 fSwappedOutputXY = fsxy, fReverse = fReverse
    ; TODO JJ lineBresenhamDecimalCalc is not ready, but polygonLineSample is.... check if lineBresenhamDecimalCalc can be deleted
    1: stop; lineBresenhamDecimalCalc is not ready... ; lineBresenhamDecimalCalc, xCoords[i], yCoords[i], xCoords[i+1], yCoords[i+1], xOutSeg, yOutSeg
    endcase
    xOut = [xOut, xOutSeg[1:*]]
    yOut = [yOut, yOutSeg[1:*]]
  endfor
  if ~polyClosed then begin
    case interpMethod of
    0: lineBresenhamIntegerCalc, xCoords[nPts-1], yCoords[nPts-1], xCoords[0], yCoords[0], xOutSeg, yOutSeg, $
                                 fSwappedOutputXY = fsxy, fReverse = fReverse
    1: lineBresenhamDecimalCalc, xCoords[nPts-1], yCoords[nPts-1], xCoords[0], yCoords[0], xOutSeg, yOutSeg
    endcase
    xOut = [xOut, xOutSeg[1:*]]
    yOut = [yOut, yOutSeg[1:*]]
  endif
end
