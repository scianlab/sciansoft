;_____________________________IOISIOI (ESTO ES ARTE)_____________________________
; NAME:            C_sActiveContours
;
; PURPOSE:         ActiveContours Class.
;
; CALLING SEQUENCE:result = obj_new('C_sActiveContours')
;
; METHODS:
;     setParams
;     getParams
;     getCoords
;     getXCoords
;     getYCoords
;     getCurvD
;     getDistance
;     getGradients
;     getInflexPoints
;     getNpts
;     getPerimeter
;     arcSample
;     adjustContour
;     calcGVF
;     calcGGVF
;     gradient
;     laplacian
;     setSnakeCoords
;     cleanup
;     init
;
;_____________________________IOISIOI (ESTO ES ARTE)_____________________________
pro C_sActiveContour::setParams, alpha = alpha, beta = beta, gamma = gamma, kappa = kappa, mu = mu,$
                                 iterations = iterations, gvf_iterations = gvf_iterations
   if n_elements(alpha) ne 0 then self.alpha = alpha > 0.001
   if n_elements(beta) ne 0 then self.beta = beta > 0.001
   if n_elements(gamma) ne 0 then self.gamma = gamma > 0.1
   if n_elements(kappa) ne 0 then self.kappa = kappa > 0
   if n_elements(mu) ne 0 then self.mu = mu > 0.001
   if n_elements(iterations) ne 0 then self.iterations = iterations > 1
   if n_elements(gvf_iterations) ne 0 then self.gvf_iterations = gvf_iterations > 1
end


function C_sActiveContour::getParam, paramName = paramName
   case paramName of
      'alpha':return, self.alpha
      'beta':return, self.beta
      'gamma':return, self.gamma
      'kappa':return, self.kappa
      'mu':return, self.mu
      'iterations':return, self.iterations
      'gvf_iterations':return, self.gvf_iterations
      'npts':return, self.npts
      else:return, -1
   endcase
end


function C_sActiveContour::getCoords, xyRes = xyRes
   if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
   return, ptr_valid(self.pX) ? [[xyRes[0] * *self.pX], [xyRes[1] * *self.pY]] : -1
end


function C_sActiveContour::getXcoords
   return, ptr_valid(self.pX) ? *self.pX : -1
end


function C_sActiveContour::getYcoords
   return, ptr_valid(self.pY) ? *self.pY : -1
end


function C_sActiveContour::getpGGVF
   return, ptr_valid(self.pU) ? {pU:self.pU, pV:self.pV} : -1
end


function C_sActiveContour::getCir3, xyRes = xyRes

   if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
   if ~(ptr_valid(self.pX)) then return, -1

   cirArray = make_array(self.npts, /double)
   modN = self.npts
   for i = 0, modN-1 do begin
      i1 = (i+1) mod modN
      i2 = (i+2) mod modN
      s_cir_3pnt, (*self.pX)[[i,i1,i2]],$
                  (*self.pY)[[i,i1,i2]],$
                  r, xCenter, yCenter, xyRes = xyRes
      cirArray[i1] = r
      xCenter /= xyRes[0]
      yCenter /= xyRes[1]
;      plots, .5+[(*self.pX)[i1],xCenter], .5+[(*self.pY)[i1],yCenter], color = 125
;      plots, .5+[(*self.pX)[i],(*self.pX)[i]], .5+[(*self.pY)[i],(*self.pY)[i]], linestyle = 1
;      plots, .5+[(*self.pX)[i1],(*self.pX)[i1]], .5+[(*self.pY)[i1],(*self.pY)[i1]], linestyle = 1
;      plots, .5+[(*self.pX)[i2],(*self.pX)[i2]], .5+[(*self.pY)[i2],(*self.pY)[i2]], linestyle = 1
   endfor
   return, cirArray
end


; http://mathworld.wolfram.com/Curvature.html
function C_sActiveContour::getCurvD, xyRes = xyRes
   if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
   if ~(ptr_valid(self.pX)) then return, -1
   gx   = xyRes[0] * shift((*self.pX)[0:self.npts-1], -1) - (*self.pX)[0:self.npts-1]
   gy   = xyRes[1] * shift((*self.pY)[0:self.npts-1], -1) - (*self.pY)[0:self.npts-1]
   gxx  = xyRes[0] * shift((*self.pX)[0:self.npts-1], 1) - 2 * (*self.pX)[0:self.npts-1] + shift((*self.pX)[0:self.npts-1], -1)
   gyy  = xyRes[1] * shift((*self.pY)[0:self.npts-1], 1) - 2 * (*self.pY)[0:self.npts-1] + shift((*self.pY)[0:self.npts-1], -1)
   curv = ((gx * gyy - gy * gxx) / (gx^2 + gy^2)^1.5) * 2 / sqrt(2)
;   curv = ((gx * gyy - gy * gxx) / (gx^2 + gy^2)^1.5) ; http://mathworld.wolfram.com/Curvature.html
   whereNAN = where(finite(curv) ne 1)
   if (whereNAN[0] ne -1) then curv[whereNAN] = 0.
   return, curv
end


function C_sActiveContour::getDistance, xyRes = xyRes
   if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
   if ptr_valid(self.pX) $
   then return, sqrt(((shift((*self.pX), -1) - (*self.pX)) * xyRes[0])^2 + ((shift((*self.pY), -1) - (*self.pY)) * xyRes[1])^2) $
   else return, -1
end


function C_sActiveContour::getGradients, xyRes = xyRes
   if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
   if ptr_valid(self.pX) $
   then return, [[shift((*self.pX)[0:self.npts-2], 1) - (*self.pX)[0:self.npts-2]]*xyRes[0],$
                 [shift((*self.pY)[0:self.npts-2], 1) - (*self.pY)[0:self.npts-2]]*xyRes[1]] $
   else return, -1
end


function C_sActiveContour::getInflexPoints, curvArray = curvArray

   if ~(ptr_valid(self.pX)) then return, -1

   if (n_elements(curvArray) eq 0) then curvArray = self->getCurvD()

   szC = n_elements(curvArray)
   inflexArray = bytArr(szC)
   flag  = 0
   count = 0
   for i = 0, szC-1 do begin
      case 1 of
         (curvArray[i] gt 0):begin
            if (flag eq -1) then begin
               inflexArray[i-(count/2)] = 1
               count = 0
            endif
            flag = 1
         endcase
         (curvArray[i] lt 0):begin
            if (flag eq 1) then begin
               inflexArray[i-(count/2)] = 2
               count = 0
            endif
            flag = -1
         endcase
         else:count += 1
      endcase
   endfor

   wherene0 = where(inflexArray ne 0)
   if (wherene0[0] ne -1) then begin
      case 1 of
        (inflexArray(wherene0[0]) eq 1) and (flag eq  1): inflexArray[(i+wherene0[0]-(count/2)) mod (szC-1)] = 1
        (inflexArray(wherene0[0]) eq 2) and (flag eq -1): inflexArray[(i+wherene0[0]-(count/2)) mod (szC-1)] = 2
      else:
      endcase
   endif

   return, inflexArray

end


function C_sActiveContour::getPerimeter, xyRes = xyRes
   if (n_elements(xyRes) eq 0) then xyRes = [1., 1.]
   if ptr_valid(self.pX) $
   then return, total(self->getDistance(xyRes = xyRes)) $
   else return, -1
end


;*****************************************************************************************************
;+
; NAME:    arcSample
;
; PURPOSE: Takes a closed curve and re-samples it in equal arc lengths.
;
; SYNTAX:  theObject->arcSample [POINTS = points]
;
; KEYWORDS:
;
;           POINTS : The number of points in the output vectors.
;                    Default:50.
;           fClose : Set this keyword to specify close the contour curve.
;-
;*****************************************************************************************************
pro C_sActiveContour::arcSample, points = points, fClose = fClose

     ; Check parameters.
   if n_elements(points) eq 0 then points = 50

   if size(*self.pX, /n_dimensions) eq 2 then begin
     x_in = reform(*self.pX)
     y_in = reform(*self.pY)
   endif else begin
     x_in = *self.pX
     y_in = *self.pY
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

   x1  = spl_interp(t, x_in, dx1, t1)
   y1  = spl_interp(t, y_in, dy1, t1)

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
     *self.pX = [x_out, x_out[0]]
     *self.pY = [y_out, y_out[0]]
   endif else begin
     *self.pX = [spl_interp(t, x_in, dx1, tx), x_in[npts-1]]
     *self.pY = [spl_interp(t, y_in, dy1, tx), y_in[npts-1]]
   endelse
   self.npts = n_elements(*self.pX)
end


;*****************************************************************************************************
;+
; NAME:  plotGVF
;
; PURPOSE: Plots the GVF Field
;
; SYNTAX: 
;
; RETURN_VALUE:
;
;-
;**************************************************************************************************
pro C_sActiveContour::plotGVF, plotContour = plotContour

   if (n_elements(plotContour) eq 0) then return

   if (plotContour gt 0) then begin
     window, plotContour, xpos = 10, ypos = 10, xsize=850, ysize=800
     velovect, *self.pU, *self.pV
   endif
end


;***************************************************************************************************
;+
; NAME:         adjustContour
;
; PURPOSE:      Runs the GVF Active Contour code to completion.
;
; SYNTAX:       roiInfo = theObject-adjustContour()
;
; RETURN_VALUE: The {x,y} contour point list.
;
; 2018.12 Added perimeterFactor keyword to dynamically adjust the contour point count, depending on
;         its estimated perimeter at each iteration. It takes precedence over the number of points
;         that is set on initialization.
;-
;***************************************************************************************************
function C_sActiveContour::adjustContour, plotContour = plotContour, perimeterFactor = perimeterFactor, fClose = fClose, fixPointCount = fixPointCount, fixPointIndices = fixPointIndices, fKeepPointCount = fKeepPointCount

  if (n_elements(plotContour) eq 0) then plotContour = 0b

  compile_opt idl2, hidden
  if (~ptr_valid(self.pX) or ~ptr_valid(self.pY)) then return, -1

  fixPointCount = n_elements(fixPointIndices) gt 0 $
                ? n_elements(fixPointIndices) : $
                  n_elements(fixPointCount) gt 0 ? $
                    fixPointCount > 0 $
                    : 0

  nptsIter = (keyword_set(perimeterFactor) and (fixPointCount eq 0)) $
    ? round(polygonPerimeter(*self.pX, *self.pY) * (perimeterFactor > 0.1)) > 5 $
    : self.npts
  if (nptsIter ne self.npts) and ~keyword_set(fKeepPointCount) then self->arcSample, points = nptsIter, fClose = fClose

  alpha = dblArr(nptsIter) + self.alpha
  beta  = dblArr(nptsIter) + self.beta
  a = beta
  b =  -alpha - 4*beta
  c = 2*alpha + 6*beta
  vfx = 0.0
  vfy = 0.0

  abcMatrix = diag_matrix(a[0:nptsIter-3],  2) + diag_matrix(a[nptsIter-2:nptsIter-1], -(nptsIter-2)) $
            + diag_matrix(b[0:nptsIter-2],  1) + diag_matrix(b[nptsIter-1], -(nptsIter-1)) $
            + diag_matrix(c + self.gamma) $
            + diag_matrix(b[0:nptsIter-2], -1) + diag_matrix(b[nptsIter-1],  (nptsIter-1)) $
            + diag_matrix(a[0:nptsIter-3], -2) + diag_matrix(a[nptsIter-2:nptsIter-1],  (nptsIter-2))

;  if ~keyword_set(fClose) then begin
;    abcMatrix[*,0] = 0.
;    abcMatrix[*,nptsIter-1] = 0.
;    abcMatrix[0,0] = 2*self.alpha + 4*self.beta
;    abcMatrix[nptsIter-1,nptsIter-1] = 2*self.alpha + 4*self.beta
;  endif

  invArray = invert(abcMatrix, invStatus)

  if (self.iterations ge 1) then for j = 1L, self.iterations do begin

    if (self.kappa gt 0) then begin
      vfx = interpolate(*self.pU, *self.pX, *self.pY, cubic = -0.5)
      vfy = interpolate(*self.pV, *self.pX, *self.pY, cubic = -0.5)
    endif

    nElemInvArray = (size(invArray, /dim))[0]
    nElemContour  = n_elements(*self.pX)
    if (nElemInvArray ne nElemContour) then begin
      nptsIter = nElemContour
      alpha = dblArr(nptsIter) + self.alpha
      beta  = dblArr(nptsIter) + self.beta
      a = beta
      b =  -alpha - 4*beta
      c = 2*alpha + 6*beta
      abcMatrix = diag_matrix(a[0:nptsIter-3],  2) + diag_matrix(a[nptsIter-2:nptsIter-1], -(nptsIter-2)) $
                + diag_matrix(b[0:nptsIter-2],  1) + diag_matrix(b[nptsIter-1], -(nptsIter-1)) $
                + diag_matrix(c + self.gamma) $
                + diag_matrix(b[0:nptsIter-2], -1) + diag_matrix(b[nptsIter-1],  (nptsIter-1)) $
                + diag_matrix(a[0:nptsIter-3], -2) + diag_matrix(a[nptsIter-2:nptsIter-1],  (nptsIter-2))
;      if ~keyword_set(fClose) then begin
;        abcMatrix[*,0] = 0.
;        abcMatrix[0,0] = 1.0
;        abcMatrix[*,nptsIter-1] = 2*self.alpha + 4*self.beta
;        abcMatrix[nptsIter-1,nptsIter-1] = 2*self.alpha + 4*self.beta
;      endif
      invArray = invert(abcMatrix, invStatus)
    endif
;stop
      ; Deform the snake.
    if (fixPointCount gt 0) and ~keyword_set(fClose) then begin ; With fixed points at the beginning and end (only with open contours).

      xTmp = invArray ## (self.gamma * (*self.pX) + self.kappa * vfx)
      yTmp = invArray ## (self.gamma * (*self.pY) + self.kappa * vfy)

      if keyword_set(fixPointIndices) then begin

        xTmp[fixPointIndices] = (*self.pX)[fixPointIndices]
        yTmp[fixPointIndices] = (*self.pY)[fixPointIndices]

          ; Re-interpolate the snake points.
        if keyword_set(perimeterFactor) $
        then begin
          polyLineLength = 0.0
          for k = 0, n_elements(*self.pX)-2 do polyLineLength += sqrt(((*self.pX)[k+1] - (*self.pX)[k])^2 + ((*self.pY)[k+1] - (*self.pY)[k])^2)
          nptsIter = (round(polyLineLength) * (perimeterFactor > 0.1)) > 5
        endif
        if ~keyword_set(fKeepPointCount) then self->arcSample, points = nptsIter

        ptr_free, self.pX & self.pX = ptr_new(xTmp, /no_copy)
        ptr_free, self.pY & self.pY = ptr_new(yTmp, /no_copy)

      endif else begin

        xFixVec1 = (*self.pX)[0 : fixPointCount-1]
        xFixVec2 = (*self.pX)[nptsIter-fixPointCount+1 : *]
        yFixVec1 = (*self.pY)[0 : fixPointCount-1]
        yFixVec2 = (*self.pY)[nptsIter-fixPointCount+1 : *]

        xTmp2 = [xFixVec1, xTmp[1:nElemContour-fixPointCount], xFixVec2]
        yTmp2 = [yFixVec1, yTmp[1:nElemContour-fixPointCount], yFixVec2]

          ; Re-interpolate the snake points.
        if keyword_set(perimeterFactor) $
        then begin
          polyLineLength = 0.0
          for k = 0, n_elements(*self.pX)-2 do polyLineLength += sqrt(((*self.pX)[k+1] - (*self.pX)[k])^2 + ((*self.pY)[k+1] - (*self.pY)[k])^2)
          nptsIter = (round(polyLineLength) * (perimeterFactor > 0.1)) > 5
        endif
        if keyword_set(fKeepPointCount) then self->arcSample, points = nptsIter

        ; Put back the fixed points
        (*self.pX)[0 : fixPointCount-1]          = xFixVec1
        (*self.pX)[nptsIter-fixPointCount+1 : *] = xFixVec2
        (*self.pY)[0 : fixPointCount-1]          = yFixVec1
        (*self.pY)[nptsIter-fixPointCount+1 : *] = yFixVec2

        ptr_free, self.pX & self.pX = ptr_new(xTmp2, /no_copy)
        ptr_free, self.pY & self.pY = ptr_new(yTmp2, /no_copy)
      endelse

    endif else begin ; Move all the contour points.

      *self.pX = invArray ## (self.gamma * (*self.pX) + self.kappa * vfx)
      *self.pY = invArray ## (self.gamma * (*self.pY) + self.kappa * vfy)
        ; Re-interpolate the snake points.
      if keyword_set(perimeterFactor) $
      then nptsIter = round(polygonPerimeter(*self.pX, *self.pY) * (perimeterFactor > 0.1)) > 5
      self->arcSample, points = nptsIter, fClose = fClose

    endelse

    if (plotContour gt 0) then begin
      case j of
        1              : oPlot, [*self.pX, (*self.pX)[0]], [*self.pY, (*self.pY)[0]], color = 255, linestyle = 1, thick = 3
        self.iterations: oPlot, [*self.pX, (*self.pX)[0]], [*self.pY, (*self.pY)[0]], color = 255, thick = 3
        else           : oPlot, [*self.pX, (*self.pX)[0]], [*self.pY, (*self.pY)[0]], color = (255 - (self.iterations - j) * 30) > 100
      endcase
    endif
  endfor

  ; Restore exception handling.
  ;void = check_math()
  ;!Except = thisExcept
  return, {x:*self.pX, y:*self.pY}
end


;*****************************************************************************************************
;+
; NAME:   calcGVF
;
; PURPOSE:Computes the GVF field for the Active Contour.
;
; SYNTAX: theObject->calcGVF()
;
; NOTES:
;
;       GVF computation based on the MATLAB version of the Active Contour program
;       ("Snake Demo" by Xu, Prince, Tomazevic, 1998)
;-
;*****************************************************************************************************
pro C_sActiveContour::calcGVF

   compile_opt idl2, hidden

      ; Calculate gradients [fx, fy] to initialize the vector field [u, v].
   self->edgeMap

      ; original version by Xu98
   ;b = (*self.pU)^2+(*self.pV)^2
   b = abs(*self.pU) + abs(*self.pV)
   bMinus = 1. - b
   c1 = b* *self.pU
   c2 = b* *self.pV

      ; Iteratively solve for the GVF u,v.
      ; delta_x = delta_y = delta_t = 1
   for j=1, self.gvf_iterations do begin
      u_lap = self->laplacian(*self.pU)
      v_lap = self->laplacian(*self.pV)

      *self.pU = (bMinus * *self.pU) + (self.mu * 4 * u_lap) + c1
      *self.pV = (bMinus * *self.pV) + (self.mu * 4 * v_lap) + c2
   endfor
   ;window, 1, xsize=800, ysize=800, title='GVF'
   ;velovect, *self.pU, *self.pV

end


;*****************************************************************************************************
;+
; NAME:   calcGGVF
;
; PURPOSE:Computes the GGVF field for the Active Contour.
;
; SYNTAX: theObject->calcGVF()
;
; NOTES:
;       GGVF computation based on the MATLAB version of the Active Contour program
;       ("Snake Demo" by Xu, Prince, Tomazevic, 1998)
;
; REFERENCE:
;        Generalized gradient vector flow external forces for active contouring
;        Chenyang Xu, Jerry L. Prince
;        Signal processing 71 (1998) 131-139
;*****************************************************************************************************
pro C_sActiveContour::calcGGVF

      ; calculate gradients [fx, fy] to initialize the vector field [u, v].
   self->edgeMap

      ; original version for the GGVF by Xu99
   ;b = (*self.pU)^2+(*self.pV)^2
   b = abs(*self.pU) + abs(*self.pV)

      ; this pair of functions act as an "enhancer/de-enhancer" of high gradient neighbors
      ; the choice of the functions must satisfy some convergence restrictions (see reference)
   g = exp(-b / self.mu)
   c1 = *self.pU * (1-g)
   c2 = *self.pV * (1-g)

      ; solve iteratively for the GGVF [u,v].
      ; delta_x = delta_y = delta_t = 1
;   window, 11, xsize=800, ysize=800, title='GGVF'
;   velovect, (*self.pU)[175:250,125:200], (*self.pV)[175:250,125:200], length = 0.4
   for j = 1, self.gvf_iterations do begin
      u_lap = self->laplacian(*self.pU)
      v_lap = self->laplacian(*self.pV)

         ; original iteration scheme
;     *self.pU += g*u_lap - h*(*self.pU-fx)
;     *self.pV += g*v_lap - h*(*self.pV-fy)

         ; optimized iteration scheme
;window, 10
;      velovect, (*self.pU) - (g * (*self.pU + u_lap) + c1), (*self.pV) - (g * (*self.pV + v_lap) + c2), length = 0.4
;window, 11
;      PLOT, (*self.pU) - (g * (*self.pU + u_lap) + c1)

      *self.pU = g * (*self.pU + u_lap) + c1
      *self.pV = g * (*self.pV + v_lap) + c2

;window, 13
;      velovect, (*self.pU), (*self.pV), length = 0.4
;window, 14
;      PLOT, (*self.pU)
   endfor

end


;*****************************************************************************************************
;+
; NAME:    calcEPGGVF
;
; PURPOSE: Computes the EP-GGVF field for the Active Contour.
;
; SYNTAX:  theObject->calcGVF()
;
; NOTES:   EP-GGVF computation based on implementation of Felipe Olmos (SCIAN-Lab, 2009).
;
; DEPENDENCIES:
;     calcEPGVF function (s_GVFcalc.pro)
;
; REFERENCE:
;     F. Olmos (2009) Segmentacion por Contornos Activos con Adapabilidad Topologica.
;
;*****************************************************************************************************
pro C_sActiveContour::calcEPGVF, iterations = iterations,$
                                 noiseCut   = noiseCut,$
                                 noiseRange = noiseRange,$
                                 errorTol   = errorTol,$
                                 step       = step,$
                                 edgemapSigmaSq = edgemapSigmaSq

  calcEPGVF, *self.pImage, pUtmp, pVtmp,$
             iterations     = iterations,$
             noiseCut       = noiseCut,$
             noiseRange     = noiseRange,$
             convergenceLimit = errorTol,$
             step           = step,$
             edgemapSigmaSq = edgemapSigmaSq
  self.pU = pUtmp
  self.pV = pVtmp
end


;*****************************************************************************************************
;+
; NAME:   edgemap
;
; PURPOSE:Calculates the edgemap for the object image and initialize the GVF vector field.
;
; SYNTAX: theObject->edgeMap
;-
;*****************************************************************************************************
pro C_sActiveContour::edgeMap

   compile_opt idl2, hidden

     ; An edgemap is the gradient of the image.
   edgeMap = sqrt((self->gradient(double(*self.pImage), 0))^2 $
                + (self->gradient(double(*self.pImage), 1))^2)
;   edgeMap = abs(self->gradient(double(*self.pImage), 0)) $
;            +abs(self->gradient(double(*self.pImage), 1))

;window, 10
;tvscl, edgeMap

      ; Normalize in the [0, 1] interval
   maxVal = max(edgeMap, min=minVal)
   if (maxVal ne minVal) then begin
      edgeMap -= minVal
      edgeMap /= (maxVal - minVal)
   endif

      ; Calculate gradients fx and fy and initialize the vector field u, v.
   self.pU = ptr_new(self->gradient(edgeMap, 0), /no_copy)
   self.pV = ptr_new(self->gradient(edgeMap, 1), /no_copy)
end


;*****************************************************************************************************
;+
; NAME:         gradient
;
; PURPOSE:      Calculates the gradient of an image.
;
; SYNTAX:       theGradient = theObject->gradient(image, 0)
;
; RETURN_VALUE: theGradient: The gradient of the input image.
;
; ARGUMENTS:
;       image:       The input image.
;       direction:   Set this parameter to 0 (default) for gradient in the X direction (fx), or
;                      to 1 for gradient in the Y direction (fy).
;-
;*****************************************************************************************************
function C_sActiveContour::gradient, image, direction

   compile_opt idl2, hidden

   if (n_elements(direction) eq 0) then direction = 0
   szI = size(image, /dim)

   if (n_elements(szI) ne 2) then return, -1
   theGradient = make_array(szI[0], szI[1], /double)

   case direction of
      0:begin
         theGradient = (shift(image, -1, 0) - shift(image, 1, 0))*.5
         theGradient[[0,szI[0]-1], *] = theGradient[[1,szI[0]-2], *]
      end
      1:begin
         theGradient = (shift(image, 0, -1) - shift(image, 0, 1))*.5
         theGradient[*, [0,szI[1]-1]] = theGradient[*, [1,szI[1]-2]]
      end
      else:return, -1
   endcase
   return, theGradient

end


;*****************************************************************************************************
;+
; NAME:         laplacian
;
; PURPOSE:      Compute the laplacian operator for a 2D image.
;
; SYNTAX:       filteredImage = theObject->Laplacian(image)
;
; RETURN_VALUE: The Laplacian filtered image.
;
; ARGUMENTS:
;
;           image: The input image.
;-
;*****************************************************************************************************
function C_sActiveContour::laplacian, image
   compile_opt idl2, hidden

;   kernel = fltArr(3,3)
;   kernel[*,1] = 0.25
;   kernel[1,*] = 0.25
;   kernel[1,1] = -1.0

   kernel = fltArr(5,5)
   kernel[2,0] = 0.0833333
   kernel[1:3,1] = 0.0833333
   kernel[*,2] = 0.0833333
   kernel[1:3,3] = 0.0833333
   kernel[2,4] = 0.0833333
   kernel[2,2] = -1.0

   return, convol(double(image), kernel, center=1, /edge_truncate)
end



pro C_sActiveContour::defaultContour
   ptr_free, self.pX
   ptr_free, self.pY

   i = bytArr(3,3,3)
   O3 = obj_new('C_sActiveContour3D', i)
   O3->defaultContour

   self.pX = ptr_new(O3->getXcoords())
   self.pY = ptr_new(O3->getYcoords())
   SnakeZ = O3->getZcoords()

   obj_destroy, O3

   whereMin = where(SnakeZ eq min(SnakeZ))

   *self.pX = (*self.pX)[whereMin]
   *self.pY = (*self.pY)[whereMin]

   self.npts = n_elements(*self.pX)
end


;*****************************************************************************************************
;+
; NAME:   setContour
;
; PURPOSE: Set the [x, y] coordinates for the active contour.
;
; SYNTAX:  theObject->setSnakeCoords, x, y
;
; ARGUMENTS:
;          x, y   : x and y coordinate arrays of the contour to be set.
;          fNoCopy (optional): if set with non-zero value, makes the input arguments x and y undefined (sets the NO_COPY flag in ptr_new() call).
;-
;*****************************************************************************************************
pro C_sActiveContour::setContour, x, y, fNoCopy = fNoCopy

   ptr_free, self.pX
   ptr_free, self.pY
   self.pX = keyword_set(fNoCopy) ? ptr_new(x, /NO_COPY) : ptr_new(x)
   self.pY = keyword_set(fNoCopy) ? ptr_new(y, /NO_COPY) : ptr_new(y)
   self.npts = n_elements(*self.pX)

end


;*****************************************************************************************************
;+
; NAME:   cleanup
;
; PURPOSE:Object class destructor method.
;
; SYNTAX: Called automatically when the object is destroyed.
;-
;*****************************************************************************************************
pro C_sActiveContour::cleanup

   ptr_free, self.pImage
   ptr_free, self.pX
   ptr_free, self.pY
   ptr_free, self.pU
   ptr_free, self.pV

end


;*****************************************************************************************************
;+
; NAME:   init
;
; PURPOSE:Object class creator method.
;
; SYNTAX: Called automatically when the object is created.
;
; ARGUMENTS:
;
;       IMAGE:        The image for which the active contour (snake) will be applied.
;                       This argument must be 2D.
;
;       X:            The initial X points of the active contour or snake. Optional.
;                       Must be used with Y.
;
;       Y:            The initial Y points of the active contour or snake. Optional.
;                       Must be used with X.
;
; KEYWORDS:
;
;       ALPHA:        The elasticity parameter of the active contour. It reflects the contour's
;                       ability to stretch along its length. Default:0.10.
;
;       BETA:         The rigidity parameter of the active contour. It reflects the contour's
;                       ability to bend, as, for example, around corners. Default:0.25.
;
;       GAMMA:        The viscosity parameter. Larger values make it harder to deform the active
;                       contour in space. Default:1.0.
;
;       KAPPA:        The external force weight. Default:1.25.
;
;       MU:           The regularization parameter. This should be set according to the amount of
;                       noise in the image. Use a larger value for noisier images. Default:0.10.
;
;       GVF_ITERATIONS:The number of iterations for calculating the Gradient Vector Flow (GVF). Default:30.
;
;       ITERATIONS:   The number of iterations to use in calculating the snake positions. Default:120.
;
;
; MODIFICATION HISTORY:
;
;        Original version:ActiveContour.pro Written by David W. Fanning, 1 December 2003.
;        Copyright Fanning Software Consulting, Inc. All rights reserved.
;        This version:C_sActiveContour.pro Written by Jorge Jara and Steffen Härtel, 2005-2006
;        http://scian.cl
;-
;*****************************************************************************************************
function C_sActiveContour::init, image, xCoords, yCoords,$
                                 ALPHA = alpha, BETA = beta, GAMMA = gamma, KAPPA = kappa, MU = mu,$
                                 GVF_ITERATIONS = gvf_iterations, ITERATIONS = iterations
      ; Error handling.
   catch, theError
   if theError ne 0 then begin
      catch, /cancel
      return, 0
   endif

   if (n_elements(image) eq 0) then begin
      ok = dialog_message('Active Contour Error - No input image')
      return, -1
   endif

       ; Check dimensions.
   if (size(image, /n_dimensions)) ne 2 then begin
      ok = dialog_message('The image argument must be a 2D array.')
      return, -1
   endif

   self.pImage = ptr_new(image)

   if n_elements(xCoords) ne 0 then x = ptr_new(xCoords) else x = ptr_new()
   if n_elements(yCoords) ne 0 then y = ptr_new(yCoords) else y = ptr_new()
   self.pX = x
   self.pY = y
   self.npts =  ptr_valid(self.pX) ? n_elements(*self.pX) : 0L

      ; Check keyword parameters.
   if n_elements(alpha) eq 0 then alpha = 0.1
   if n_elements(beta)  eq 0 then beta  = 0.25
   if n_elements(gamma) eq 0 then gamma = 1.0
   if n_elements(kappa) eq 0 then kappa = 0.;   1.25
   if n_elements(mu)    eq 0 then mu    = 0.1 else mu = mu < .25
   if n_elements(iterations)     eq 0 then iterations     = 100
   if n_elements(gvf_iterations) eq 0 then gvf_iterations = 200

      ; Set object parameters.
   self.alpha = alpha > 0.001
   self.beta  = beta  > 0.001
   self.gamma = gamma > 0.1
   self.kappa = kappa > 0
   self.mu    = mu    > 0.001
   self.iterations     = iterations     > 1
   self.gvf_iterations = gvf_iterations > 1

   return, 1
end


;*****************************************************************************************************
;
; NAME:   C_sActiveContour class definition.
;
; PURPOSE:C_sActiveContour object's structure class definition code.
;
;*****************************************************************************************************
pro C_sActiveContour__Define
  tmp = { C_sActiveContour,$
      pImage: ptr_new(),$   ; 2D image for which the snake is being calculated.
      pX    : ptr_new(),$   ; X coordinates of the snake.
      pY    : ptr_new(),$   ; Y coordinates of the snake.
      pU    : ptr_new(),$   ; GVF X-gradient.
      pV    : ptr_new(),$   ; GVF Y-gradient.
      npts  : 0u,$          ; Number of points in the snake.
      alpha : 0.0,$         ; Elasticity coefficient (snake's tension).
      beta  : 0.0,$         ; Rigidity coefficient (snake's rigidity).
      gamma : 0.0,$         ; Viscosity coefficient.
      kappa : 0.0,$         ; External force weight coefficient.
      mu    : 0.0,$         ; Regularization coefficient. Increase for noisy images.
      iterations     : 0u,$ ; Number of iterations for the snake to converge.
      gvf_iterations : 0u } ; Number of iterations for the GVF field to converge.
end
