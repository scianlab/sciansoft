;_____________________________IOISIOI (ESTO ES ARTE)_____________________________
; NAME:     C_sActiveContour3D
;
; PURPOSE:  3D Active Contours Class.
;
; CALLING SEQUENCE:
;
;     result = obj_new('C_sActiveContour3D', image, xCoords, yCoords, zCoords, polygons
;                                            [,ALPHA = alpha] [,BETA = beta] [,GAMMA = gamma] [,KAPPA = kappa]
;                                            [,MU = mu] [,GVF_ITERATIONS = gvf_iterations] [,ITERATIONS = iterations])
; METHODS:
;     setParams
;     getParams
;     getCoords
;     getXcoords
;     getYcoords
;     getZcoords
;     getPolygonList
;     getCurv4
;     getInflexPoints
;     getNpts
;     setContour
;     calcGVF
;     calcGGVF
;     adjustSsurface
;     getAvgNeighborNodes
;     edgemap
;     gradient
;     laplacian
;     cleanup
;     init
;
; HISTORY:
;
;     2D Active Contour version by David Fanning (http://www.dfanning.com)
;     Original version: ActiveContour.pro Written by David W. Fanning, 1 December 2003.
;     Copyright Fanning Software Consulting, Inc. All rights reserved.
;
;     3D version by Jorge Jara and Steffen Härtel.
;     C_sActiveContour3D__define.pro Written by Jorge Jara and Steffen Härtel, 2005-2006.
;
;_____________________________IOISIOI (ESTO ES ARTE)_____________________________


function det, A

   zero = 1.0d-10 ;Double-precision zero.

   singular = cond(a,  /double)
   if (singular eq -1) or (singular eq 0) then return, 0.0d

   ;Make a copy of the array for its LU decomposition.
   aLUd = a

   ;Compute LU decomposition.
   luDc, aLUd, Index, double = 1, Interchanges = sign

   dims = size(a,  /dim)
   ;Are there any zeros on the main diagonal?
   ii = where(abs(aLUd[lindgen(dims[0])*(dims[0]+1)] ) le zero, cnt)

   d = 1 ;Initialize determinant.

   ;A zero on the main diagonal results in a zero determ.
   if cnt ne 0 then return, 0.0d else begin
      for k = 0, dims[0]-1 do d = d * aLUd[k,k]
      return, sign * d
   endelse
end


pro C_sActiveContour3D::setParams, alpha = alpha, beta = beta, gamma = gamma, kappa = kappa, mu = mu,$
                                   iterations = iterations, gvf_iterations = gvf_iterations

   if n_elements(alpha) ne 0 then self.alpha = alpha
   if n_elements(beta) ne 0 then self.beta = beta
   if n_elements(gamma) ne 0 then self.gamma = gamma
   if n_elements(kappa) ne 0 then self.kappa = kappa
   if n_elements(mu) ne 0 then self.mu = mu
   if n_elements(iterations) ne 0 then self.iterations = iterations > 0
   if n_elements(gvf_iterations) ne 0 then self.gvf_iterations = gvf_iterations > 1
end


function C_sActiveContour3D::getParam, paramName = paramName
   case paramName of
      'alpha': return, self.alpha
      'beta': return, self.beta
      'gamma': return, self.gamma
      'kappa': return, self.kappa
      'mu': return, self.mu
      'iterations': return, self.iterations
      'gvf_iterations': return, self.gvf_iterations
      'npts': return, self.npts
      else: return, -1
   endcase
end

function C_sActiveContour3D::getCoords
   if ptr_valid(self.pX) then return, [transpose([*self.pX]),$
                                       transpose([*self.pY]),$
                                       transpose([*self.pZ])] $
   else return, -1
end


function C_sActiveContour3D::getCurvVect
   if ptr_valid(self.pCurv) then return, *self.pCurv else return, -1
end


function C_sActiveContour3D::getXcoords
   if ptr_valid(self.pX) then return, *self.pX else return, -1
end


function C_sActiveContour3D::getYcoords
   if ptr_valid(self.pX) then return, *self.pY else return, -1
end


function C_sActiveContour3D::getZcoords
   if ptr_valid(self.pX) then return, *self.pZ else return, -1
end


function C_sActiveContour3D::getPolygonList
   if ptr_valid(self.pX) then return, *self.pPolygons else return, -1
end


function C_sActiveContour3D::getContourArea
   if not ptr_valid(self.pX) then return, -1
   pos = 0l
   triPos = [0]
   repeat begin
      case (*self.pPolygons)[pos] of
      3: begin
         triPos = [triPos, (*self.pPolygons)[pos+1:pos+3]]
         pos += 4
         endcase
      4: begin
         triPos = [triPos, (*self.pPolygons)[[pos+1, pos+3, pos+2]], (*self.pPolygons)[[pos+1,pos+4,pos+3]]]
         pos += 5
         endcase
      else: begin
         print, 'Polygon of higher order than 4'
         pos += (*self.pPolygons)[pos]
         endcase
      endcase
   endrep until (pos ge n_elements(*self.pPolygons))

   triPos = triPos[1:*]
   triTri = make_array(n_elements(triPos)/3, /index) * 3

   a = sqrt(((*self.pX)[triPos[triTri+1]]-(*self.pX)[triPos[triTri]  ])^2+((*self.pY)[triPos[triTri+1]]-(*self.pY)[triPos[triTri]  ])^2+((*self.pZ)[triPos[triTri+1]]-(*self.pZ)[triPos[triTri]  ])^2)
   b = sqrt(((*self.pX)[triPos[triTri+2]]-(*self.pX)[triPos[triTri+1]])^2+((*self.pY)[triPos[triTri+2]]-(*self.pY)[triPos[triTri+1]])^2+((*self.pZ)[triPos[triTri+2]]-(*self.pZ)[triPos[triTri+1]])^2)
   c = sqrt(((*self.pX)[triPos[triTri+2]]-(*self.pX)[triPos[triTri]  ])^2+((*self.pY)[triPos[triTri+2]]-(*self.pY)[triPos[triTri]  ])^2+((*self.pZ)[triPos[triTri+2]]-(*self.pZ)[triPos[triTri]  ])^2)

   s = (a + b + c) / 2.
   a = sqrt( s * (s-a) * (s-b) * (s-c) )
   return, total(a)
end


pro C_sActiveContour3D::computeCurv4, x, y, z, xC, yC, zC, r

   a = dblarr(3,3)

      ;Define the relationships between X, Y and Z as the linear system.
   for k = 0, 2 do begin
      a[0, k] = x[k] - x[k+1]
      a[1, k] = y[k] - y[k+1]
      a[2, k] = z[k] - z[k+1]
   endfor

       ;Define right-side of linear system.
   q = x^2 + y^2 + z^2
   c = 0.5 * (q[0:2] - q[1:3])

   if det(a) ne 0 then begin
        ;Solve the linear system Ay = c where y = (xC, yC, zC)
      luDc, a,index, double = double
          ;Solution y is stored in C
      c = luSol(a, index, c, double = double)
          ;The sphere's center coordinates.
      xC = c[0]
      yC = c[1]
      zC = c[2]
          ;The sphere's radius.
      r = sqrt(q[0] - 2*(x[0]*xC +y[0]*yC + z[0]*zC) + xC^2 + yC^2 + zC^2)
   endif else begin
      r = -1
      xC = 0
      yC = 0
      zC = 0
   endelse
end


function C_sActiveContour3D::getCurv4

   nZeros = 0
   if not(ptr_valid(self.pX)) then return, -1
   curvVec = make_array(self.npts, /float)
   for i = 0ul, self.npts-1 do begin
      p0neighbors = *(*self.pNeighbor1)[i]
      n = n_elements(p0neighbors)
      radVec = [-1]
      if n ge 3 then begin
         radVec = [-1]
         for j = 0, n-3  do begin
            for k = j+1, n-2 do begin
               for l = k+1, n-1 do begin
                  spherePoints = [p0neighbors[j], p0neighbors[k], p0neighbors[l]]
                  self->computeCurv4,[(*self.pX)[i],(*self.pX)[spherePoints[0]],(*self.pX)[spherePoints[1]],(*self.pX)[spherePoints[2]]],$
                                     [(*self.pY)[i],(*self.pY)[spherePoints[0]],(*self.pY)[spherePoints[1]],(*self.pY)[spherePoints[2]]],$
                                     [(*self.pZ)[i],(*self.pZ)[spherePoints[0]],(*self.pZ)[spherePoints[1]],(*self.pZ)[spherePoints[2]]],$
                                     xC, yC, zC, R
                  radVec = [radVec, R]
               endfor
            endfor
         endfor
      endif else print, 'node ', i, 'has no neighbors in the polygon list: ', where(*self.pPolygons eq i)

      wRad = where(radVec ne -1, count)
      if (count ge 1) then begin
         rad = total(radVec[wRad]) / count
         curv = total(1./radVec[wRad]) / (n_elements(radVec)-1)
         curvVec[i] = curv
      endif else curvVec[i] = 0
   endfor
   return, curvVec
end


function C_sActiveContour3D::getInflexPoints, curvArray = curvArray
   if not(ptr_valid(self.pX)) then return, -1
   if n_elements(curvArray) eq 0 then curvArray = self->getCurv4()
end


function C_sActiveContour3D::getNpts
   return, self.npts
end


;*****************************************************************************************************
;+
; NAME:           adjustContour
;
; PURPOSE:        Deforms the surface according to the pressure forces given by the GVF/GGVF field and the
;                 internal elastic and rigid forces.
;
; SYNTAX:         adjustResult = theObject->adjustContour()
;
; RETURN_VALUE:   1 if the process is completed succesfully, -1 otherwise
;-
;**************************************************************************************************
function C_sActiveContour3D::adjustContour

   compile_opt idl2, hidden

   if not ptr_valid(self.pX) then return, -1
   if (self.iterations eq 0) then return, 1
;   window, 31, xsize = 900, ysize = 800
;   whereZ35 = where(*self.pZ eq 35)
;   plot,  (*self.pX)[whereZ35],(*self.pY)[whereZ35], psym = 1, xrange = [25, 45], yrange = [25, 45], symsize = .5, color = 100
;   velovect, reform((*self.pU)[*,*,35]), reform((*self.pV)[*,*,35]), length = 0.4, /overplot
   for i = 1, self.iterations do begin
         ; compute the elastic and rigid forces
      neighbors = self->getAvgNeighborNodes()

         ; interpolate the force field vectors for the mesh points
;     vfx = interpolate(*self.pU, *self.pX, *self.pY, *self.pZ)
;     vfy = interpolate(*self.pV, *self.pX, *self.pY, *self.pZ)
;     vfz = interpolate(*self.pW, *self.pX, *self.pY, *self.pZ)

         ; deform the surface: alpha
;      *self.pX +=  (self.alpha / self.gamma) * (neighbors.x1 - *self.pX)
;      *self.pY +=  (self.alpha / self.gamma) * (neighbors.y1 - *self.pY)
;      *self.pZ +=  (self.alpha / self.gamma) * (neighbors.z1 - *self.pZ)

         ; deform the surface: alpha & vector fields
;      *self.pX +=  (self.alpha / self.gamma) * (neighbors.x1 - *self.pX) + (self.kappa / self.gamma) * (vfx)
;      *self.pY +=  (self.alpha / self.gamma) * (neighbors.y1 - *self.pY) + (self.kappa / self.gamma) * (vfy)
;      *self.pZ +=  (self.alpha / self.gamma) * (neighbors.z1 - *self.pZ) + (self.kappa / self.gamma) * (vfz)

         ; deform the surface: alpha & beta
      *self.pX +=  (self.alpha / self.gamma) * (neighbors.x1 - *self.pX) + (self.beta / self.gamma) * (neighbors.x2 - *self.pX)
      *self.pY +=  (self.alpha / self.gamma) * (neighbors.y1 - *self.pY) + (self.beta / self.gamma) * (neighbors.y2 - *self.pY)
      *self.pZ +=  (self.alpha / self.gamma) * (neighbors.z1 - *self.pZ) + (self.beta / self.gamma) * (neighbors.z2 - *self.pZ)

         ; deform the surface: alpha & beta & vector fields
;      *self.pX +=  (self.alpha / self.gamma) * (neighbors.x1 - *self.pX) + (self.beta / self.gamma) * (neighbors.x2 - *self.pX) + (self.kappa / self.gamma) * (vfx)
;      *self.pY +=  (self.alpha / self.gamma) * (neighbors.y1 - *self.pY) + (self.beta / self.gamma) * (neighbors.y2 - *self.pY) + (self.kappa / self.gamma) * (vfy)
;      *self.pZ +=  (self.alpha / self.gamma) * (neighbors.z1 - *self.pZ) + (self.beta / self.gamma) * (neighbors.z2 - *self.pZ) + (self.kappa / self.gamma) * (vfz)
      ;print, 'surface area ', self->getContourArea()

 ;  oPlot, (*self.pX)[whereZ35],(*self.pY)[whereZ35], psym = 1, symsize = .5, color = 100

   endfor

   self.npts = n_elements(*self.pX)
   return, 1 ;{x:*self.pX, y:*self.pY, z:*self.pZ}
end


function C_sActiveContour3D::getAvgNeighborNodes

   if not(ptr_valid(self.pX)) then return, -1

      ; get the (index position, number of nodes) pair...
   polyInd = 0
      ; ...for the first polygon
   posArray = [(*self.pPolygons)[0]]

      ; ...and iteratively for the rest
   while ((polyInd + (*self.pPolygons)[polyInd] + 1) lt n_elements(*self.pPolygons)) do begin
      polyInd += ((*self.pPolygons)[polyInd] + 1)
      posArray = [posArray, (*self.pPolygons)[polyInd]]
   endwhile

      ; make an array for each node in the mesh and their adjacent vertices at 1 and 2 steps
   for i = 0ul, self.npts-1 do (*self.pNeighbor1)[i] = ptr_new([i], /no_copy)

      ; iterate over the polygon list
   polyCount = 0
   polyPos = 1

   for polyCount = 0ul, n_elements(posArray)-1 do begin
      polyNodes = (*self.pPolygons)[polyPos : polyPos+posArray[polyCount]-1]
      polyPos += posArray[polyCount] + 1

         ; for each polygon node, add their contiguous to its neighbor array
      for nodeCount = 0, posArray[polyCount]-1 do $
         (*(*self.pNeighbor1)[polyNodes[nodeCount]]) = [(*(*self.pNeighbor1)[polyNodes[nodeCount]]),$
                                                        polyNodes[(posArray[polyCount]+nodeCount-1) mod posArray[polyCount]],$
                                                        polyNodes[(nodeCount+1) mod posArray[polyCount]]]
   endfor

   avgNeighbor1 = make_array(3, self.npts, /float)
   avgNeighbor2 = make_array(3, self.npts, /float)

      ; define & sort 1st neighbor nodes
   for i = 0ul, self.npts-1 do begin
      numNodes = n_elements(*(*self.pNeighbor1)[i])
         ; sort the array and eliminate duplicated nodes
      case 1 of
         numNodes gt 1: begin
            dummy = (*(*self.pNeighbor1)[i])[1:numNodes-1]
            *(*self.pNeighbor1)[i] = dummy[uniq(dummy, sort(dummy))]
         end
         else: begin
            *(*self.pNeighbor1)[i] = i
            print, '3D Active Contour Warning: Node ', i, ' does not appear in the surface polygon list'
         end
      endcase

         ; compute the average of the 1st neighbor nodes
      avgNeighbor1[0:2, i] = [ total((*self.pX)[*(*self.pNeighbor1)[i]]),$
                               total((*self.pY)[*(*self.pNeighbor1)[i]]),$
                               total((*self.pZ)[*(*self.pNeighbor1)[i]])] / n_elements(*(*self.pNeighbor1)[i])
   endfor

      ; define & sort 2nd neighbor nodes & compute the average of the 2nd neighbor nodes
   for i = 0ul, self.npts-1 do begin
      n2Arr = [i]
      for j = 0, n_elements(*(*self.pNeighbor1)[i])-1 do n2Arr = [n2Arr, *(*self.pNeighbor1)[(*(*self.pNeighbor1)[i])[j]] ]

         ; sort the array and eliminate duplicated nodes
      numNodes = n_elements(n2Arr)
      case 1 of
         numNodes gt 1: begin
            n2Arr = n2Arr[uniq(n2Arr, sort(n2Arr))]
            if (n_elements(*(*self.pNeighbor1)[i]) gt 1) then begin
               for j = 0, n_elements(*(*self.pNeighbor1)[i])-1 do $
                  n2Arr = n2Arr[where(n2Arr ne (*(*self.pNeighbor1)[i])[j])]
               n2Arr = n2Arr[where(n2Arr ne i) > 0]
            endif
         end
         else: begin
            n2Arr = [i]
            print, '3D Active Contour Warning: Node ', i, ' does not appear in the surface polygon list'
         end
      endcase

      avgNeighbor2[0:2, i] = [total((*self.pX)[n2Arr]), total((*self.pY)[n2Arr]), total((*self.pZ)[n2Arr]) ] / n_elements(n2Arr)
   endfor

;      ; define & sort 2nd neighbor nodes & compute the average of the 2nd neighbor nodes
;   for i = 0ul, self.npts-1 do begin
;      projXYZ = fltArr(3, n_elements(*(*self.pNeighbor1)[i]))
;
;      for j = 0, n_elements(*(*self.pNeighbor1)[i])-1 do begin
;         p2Neighbors = *(*self.pNeighbor1)[(*(*self.pNeighbor1)[i])[j]]
;
;
;         ; sort the array and eliminate duplicated nodes
;      numNodes = n_elements(n2Arr)
;      case 1 of
;         numNodes gt 1: begin
;            n2Arr = n2Arr[uniq(n2Arr, sort(n2Arr))]
;            if (n_elements(*(*self.pNeighbor1)[i]) gt 1) then begin
;               for j = 0, n_elements(*(*self.pNeighbor1)[i])-1 do $
;                  n2Arr = n2Arr[where(n2Arr ne (*(*self.pNeighbor1)[i])[j])]
;               n2Arr = n2Arr[where(n2Arr ne i)]
;            endif
;         end
;         else: begin
;            n2Arr = [i]
;            print, '3D Active Contour Warning: Node ', i, ' does not appear in the surface polygon list'
;         end
;      endcase
;
;
;
;
;         p2Neighbors = p2Neighbors[where(p2Neighbors ne i)]
;
;          for l = 0ul, n_elements(*(*self.pNeighbor1)[i])-1 do $
;             p2Neighbors = p2Neighbors[where(p2Neighbors ne (*(*self.pNeighbor1)[i])[l])]
;
;;         where1NE2 = where(p2Neighbors ne *(*self.pNeighbor1)[i], count)
;;         if (count gt 0) then p2Neighbors = p2Neighbors[where1NE2]
;;         p2Neighbors = p2Neighbors[where(p2Neighbors ne *(*self.pNeighbor1)[i])]
;
;         vect = [0.,0.,0.]
;         for k = 0, n_elements(p2Neighbors)-1 do begin
;            vect += [(*self.pX)[(*(*self.pNeighbor1)[i])[j]] - (*self.pX)[p2Neighbors[k]],$
;                     (*self.pY)[(*(*self.pNeighbor1)[i])[j]] - (*self.pY)[p2Neighbors[k]],$
;                     (*self.pZ)[(*(*self.pNeighbor1)[i])[j]] - (*self.pZ)[p2Neighbors[k]]]
;         endfor
;         projXYZ[0:2,j] = [(*self.pX)[(*(*self.pNeighbor1)[i])[j]],$
;                           (*self.pY)[(*(*self.pNeighbor1)[i])[j]],$
;                           (*self.pZ)[(*(*self.pNeighbor1)[i])[j]]] + (vect / n_elements(p2Neighbors))
;      endfor
;      avgNeighbor2[0:2, i] = [total(projXYZ[0,*]), total(projXYZ[1,*]), total(projXYZ[2,*])] / n_elements(*(*self.pNeighbor1)[i])
;   endfor


;    *self.pCurv = dblarr(self.npts)
      ; curvature-based approach
;   for i = 0ul, self.npts-1 do begin
;      sphereCount = 0
;      rSum = 0
;      xSum = 0
;      ySum = 0
;      zSum = 0
;      p0neighbors = *(*self.pNeighbor1)[i]
;      n = n_elements(p0neighbors)
;      if n ge 3 then begin
;         for j = 0, n-3  do begin
;            for k = j+1, n-2 do begin
;               for l = k+1, n-1 do begin
;                  spherePoints = [p0neighbors[j], p0neighbors[k], p0neighbors[l]]
;                  self->computeCurv4,[(*self.pX)[i],(*self.pX)[spherePoints[0]],(*self.pX)[spherePoints[1]],(*self.pX)[spherePoints[2]]],$
;                                     [(*self.pY)[i],(*self.pY)[spherePoints[0]],(*self.pY)[spherePoints[1]],(*self.pY)[spherePoints[2]]],$
;                                     [(*self.pZ)[i],(*self.pZ)[spherePoints[0]],(*self.pZ)[spherePoints[1]],(*self.pZ)[spherePoints[2]]],$
;                                     xC, yC, zC, R
;                  sphereCount += 1
;                  rSum += R
;                  xSum += xC
;                  ySum += yC
;                  zSum += zC
;               endfor
;            endfor
;         endfor
;      endif else begin
;         print, 'node ', i, 'has no neighbors in the polygon list: ', where(*self.pPolygons eq i)
;         shpereCount = 1
;         R = 0
;      endelse
;
;      avgR = rSum / sphereCount
;      avgX = xSum / sphereCount
;      avgY = ySum / sphereCount
;      avgZ = zSum / sphereCount
;      ;print, avgR, avgX, avgY, avgZ
;      angle = atan(((*self.pX)[i]-avgX) / sqrt(((*self.pY)[i]-avgY)^2 + ((*self.pZ)[i]-avgZ)^2))
;      if not finite(angle) then angle = 0
;      rX = avgR * sin(angle)
;
;      angle = atan(((*self.pY)[i]-avgY) / sqrt(((*self.pX)[i]-avgX)^2 + ((*self.pZ)[i]-avgZ)^2))
;      if not finite(angle) then angle = 0
;      rY = avgR * sin(angle)
;
;      angle = atan(((*self.pZ)[i]-avgZ) / sqrt(((*self.pX)[i]-avgX)^2 + ((*self.pY)[i]-avgY)^2))
;      if not finite(angle) then angle = 0
;      rZ = avgR * sin(angle)
;
;      avgNeighbor2X[i] = rX + avgX
;      avgNeighbor2Y[i] = rY + avgY
;      avgNeighbor2Z[i] = rZ + avgZ
;
;   endfor

   ;print, string(sysTime(/seconds)-startTime)
   return, {x1: reform(avgNeighbor1[0,*]), y1: reform(avgNeighbor1[1,*]), z1: reform(avgNeighbor1[2,*]),$
            x2: reform(avgNeighbor2[0,*]), y2: reform(avgNeighbor2[1,*]), z2: reform(avgNeighbor2[2,*])}
end


;*****************************************************************************************************
;+
; NAME:     calcGVF
;
; PURPOSE:  Computes the GVF field for the Active Contour.
;
; SYNTAX:   theObject->calcGVF()
;
; NOTES:
;           GVF computation based on the MATLAB version of the Active Contour program
;           ("Snake Demo" by Xu, Prince, Tomazevic, 1998)
;-
;*****************************************************************************************************
pro C_sActiveContour3D::calcGVF

   compile_opt idl2, hidden

   self->edgeMap

   b = (*self.pU)^2 + (*self.pV)^2 + (*self.pW)^2
   bMinus = 1. - b
   c1 = b* *self.pU
   c2 = b* *self.pV
   c3 = b* *self.pW

      ; Iteratively solve for the GVF u,v.
   for j=1, self.gvf_iterations do begin
      u_lap = self->Laplacian(*self.pU)
      v_lap = self->Laplacian(*self.pV)
      w_lap = self->Laplacian(*self.pW)
      *self.pU = (bMinus * *self.pU) + (self.mu * u_lap) + c1
      *self.pV = (bMinus * *self.pV) + (self.mu * v_lap) + c2
      *self.pW = (bMinus * *self.pW) + (self.mu * w_lap) + c3
   endfor

end


;*****************************************************************************************************
;+
; NAME:     calcGGVF
;
; PURPOSE:  Computes the GGVF field for the Active Contour.
;
; SYNTAX:   theObject->calcGGVF()
;
; NOTES:
;           Generalized gradient vector flow external forces for active contouring
;           Chenyang Xu, Jerry L. Prince
;           Signal processing 71 (1998) 131-139
;-
;*****************************************************************************************************
pro C_sActiveContour3D::calcGGVF

      ; calculate gradients [fx, fy] to initialize the vector field [u, v].
   self->edgeMap

      ; original version for the GGVF by Xu99
   ;b = (*self.pU)^2+(*self.pV)^2+(*self.pW)^2
   b = abs(*self.pU) + abs(*self.pV) + abs(*self.pW)

      ; this set of functions act as an "enhancer/de-enhancer" of high gradient neighbors
      ; the choice of the functions must satisfy some convergence restrictions (see reference)
   g = exp(-b / self.mu)
   c1 = *self.pU * (1-g)
   c2 = *self.pV * (1-g)
   c3 = *self.pW * (1-g)
;   startTime = sysTime(/seconds)

;      window, 10, xsize=800, ysize=700
;      velovect, (*self.pU)[*,*,35], (*self.pV)[*,*,35], length = 0.4
      ; solve iteratively for the GGVF [u,v,w].
      ; delta_x = delta_y = delta_t = 1
   for j = 0, self.gvf_iterations do begin
      u_lap = self->laplacian(*self.pU)
      v_lap = self->laplacian(*self.pV)
      w_lap = self->laplacian(*self.pW)

      *self.pU = g * (*self.pU + u_lap) + c1
      *self.pV = g * (*self.pV + v_lap) + c2
      *self.pW = g * (*self.pW + w_lap) + c3
;      velovect, (*self.pU)[*,*,35], (*self.pV)[*,*,35], length = 0.4
   endfor

   ; set max forces to 1.
;   mag = sqrt(*self.pu^2 + *self.pv^2 + *self.pw^2)
;   *self.pu /= (mag + 1e-10)
;   *self.pv /= (mag + 1e-10)
;   *self.pw /= (mag + 1e-10)

;   print, 'GGVF computation done - Elapsed time:' + string(sysTime(/seconds)-startTime)
end


;*****************************************************************************************************
;+
; NAME:     edgemap
;
; PURPOSE:  Calculates the edgemap for the object image
;
; SYNTAX:   theObject->edgeMap
;-
;*****************************************************************************************************
pro C_sActiveContour3D::edgeMap

   compile_opt idl2, hidden

      ; An edgemap is the gradient of the image, scaled into 0 to 1.
   edgeMap = sqrt(  (self->gradient(float(*self.pImage), 0))^2 $
                  + (self->gradient(float(*self.pImage), 1))^2 $
                  + (self->gradient(float(*self.pImage), 2))^2)

      ; Normalize the edgmap to the [0, 1] interval.
   maxVal = max(edgeMap, min=minVal)
   if (maxVal ne minVal) then begin
      edgeMap -= minVal
      edgeMap /= (maxVal - minVal)
   endif

   self.pU = ptr_new(self->gradient(edgeMap, 0))
   self.pV = ptr_new(self->gradient(edgeMap, 1))
   self.pW = ptr_new(self->gradient(edgeMap, 2))
end


;*****************************************************************************************************
;+
; NAME:           gradient
;
; PURPOSE:        Calculates the gradient of an image.
;
; SYNTAX:         theGradient = theObject->gradient(image, 0)
;
; RETURN_VALUE:   theGradient: The gradient of the input image.
;
; ARGUMENTS:
;
;     image: The unfiltered image.
;
;     direction: Set this parameter to 0 (default) for gradient in the X direction (fx),
;              to 1 for gradient in the Y direction (fy), or to 2 for gradient in Z (fz).
;
;-
;*****************************************************************************************************
function C_sActiveContour3D::gradient, image, direction

   compile_opt idl2, hidden
   if (n_elements(direction) eq 0) then direction = 0

   szI = size(image, /dim)
   if (n_elements(szI) ne 3) then return, -1

   theGradient = make_array(szI[0], szI[1], szI[2], /float)

   case direction of
      0: begin
         theGradient = (shift(image, -1, 0, 0) - shift(image, 1, 0, 0))*.5
         theGradient[[0,szI[0]-1], *, *] = theGradient[[1,szI[0]-2], *, *]
      end
      1: begin
         theGradient = (shift(image, 0, -1, 0) - shift(image, 0, 1, 0))*.5
         theGradient[*, [0,szI[1]-1], *] = theGradient[*, [1,szI[1]-2], *]
      end
      2: begin
         theGradient = (shift(image, 0, 0, -1) - shift(image, 0, 0, 1))*.5
         theGradient[*, *, [0,szI[2]-1]] = theGradient[*, *, [1,szI[2]-2]]
      end
      else: return, -1
   endcase

   return, theGradient

end


;*****************************************************************************************************
;+
; NAME:           laplacian
;
; PURPOSE:        Applies a Laplacian filter to its argument.
;
; SYNTAX:         theLaplacian = theObject->Laplacian(image)
;
; RETURN_VALUE:   The Laplacian filtered 3D image.
;
; ARGUMENTS:
;
;     image: The input 3D image..
;-
;*****************************************************************************************************
function C_sActiveContour3D::laplacian, image
   compile_opt idl2, hidden
;   kernel = fltArr(3,3,3)
;   kernel[1,1,*] = 1./6
;   kernel[1,*,1] = 1./6
;   kernel[*,1,1] = 1./6
;   kernel[1,1,1] = -1.0

   kernel = fltArr(5,5,5)
   kernel[1:3,1:3,1:3] = 1./32
   kernel[*,2,2] = 1./32
   kernel[2,*,2] = 1./32
   kernel[2,2,*] = 1./32
   kernel[2,2,2] = -1.0

   return, convol(image, kernel, center=1, /edge_truncate)
end

pro C_sActiveContour3D::setContour, x, y, z, polygons

   ptr_free, self.pX
   ptr_free, self.pY
   ptr_free, self.pZ
   ptr_free, self.pPolygons

   self.pX = ptr_new(x, /no_copy)
   self.pY = ptr_new(y, /no_copy)
   self.pZ = ptr_new(z, /no_copy)
   self.pPolygons = ptr_new(polygons)

   self.npts = n_elements(*self.pX)

   if ptr_valid(self.pNeighbor1) then begin
      for i = 0ul, n_elements(*self.pNeighbor1)-1 do if ptr_valid((*self.pNeighbor1)[i]) then ptr_free, (*self.pNeighbor1)[i]
      ptr_free, self.pNeighbor1
   endif
   self.pNeighbor1 = ptr_new(ptrArr(self.npts))
end


pro C_sActiveContour3D::defaultContour
   ptr_free, self.pX
   ptr_free, self.pY
   ptr_free, self.pZ
   ptr_free, self.pPolygons

   szI = size(*self.pImage, /dim) - 1

   self.pX = ptr_new(float([0, 1, 1, 0, 0, 1, 1, 0]))
   self.pY = ptr_new(float([0, 0, 1, 1, 0, 0, 1, 1]))
   self.pZ = ptr_new(float([0, 0, 0, 0, 1, 1, 1, 1]))
   self.pPolygons = ptr_new([4, 0, 1, 2, 3,$
                             4, 0, 1, 5, 4,$
                             4, 1, 2, 6, 5,$
                             4, 2, 3, 7, 6,$
                             4, 3, 0, 4, 7,$
                             4, 4, 5, 6, 7])
   self.npts = n_elements(*self.pX)

   if ptr_valid(self.pNeighbor1) then begin
      for i = 0, n_elements(*self.pNeighbor1)-1 do if ptr_valid((*self.pNeighbor1)[i]) then ptr_free, (*self.pNeighbor1)[i]
      ptr_free, self.pNeighbor1
   endif

   self.pNeighbor1 = ptr_new(ptrArr(self.npts))

end



pro C_sActiveContour3D::testContour
   ptr_free, self.pX
   ptr_free, self.pY
   ptr_free, self.pZ
   ptr_free, self.pPolygons

   szI = size(*self.pImage, /dim) - 1

   self.pX = ptr_new(float([0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0]))
   self.pY = ptr_new(float([0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1]))
   self.pZ = ptr_new(float([0, 0, 0, 0,.5,.5,.5,.5, 1, 1, 1, 1]))
   self.pPolygons = ptr_new([4, 0, 1, 2, 3,$
                             4, 0, 1, 5, 4,$
                             4, 1, 2, 6, 5,$
                             4, 2, 3, 7, 6,$
                             4, 3, 0, 4, 7,$
                             4, 4, 5, 9, 8,$
                             4, 5, 6,10, 9,$
                             4, 6, 7,11,10,$
                             4, 7, 4, 8,11,$
                             4, 8, 9,10,11])
   self.npts = n_elements(*self.pX)

   if ptr_valid(self.pNeighbor1) then begin
      for i = 0, n_elements(*self.pNeighbor1)-1 do if ptr_valid((*self.pNeighbor1)[i]) then ptr_free, (*self.pNeighbor1)[i]
      ptr_free, self.pNeighbor1
   endif

   self.pNeighbor1 = ptr_new(ptrArr(self.npts))

end


;*****************************************************************************************************
;+
; NAME:     cleanup
;
; PURPOSE:  Object class destructor method.
;
; SYNTAX:   Called automatically when the object is destroyed.
;-
;*****************************************************************************************************
pro C_sActiveContour3D::cleanup
   ptr_free, self.pImage
   ptr_free, self.pU
   ptr_free, self.pV
   ptr_free, self.pW
   ptr_free, self.pPolygons
   ptr_free, self.pX
   ptr_free, self.pY
   ptr_free, self.pZ
   if ptr_valid(self.pNeighbor1) then begin
      for i = 0ul, n_elements(*self.pNeighbor1)-1 do if ptr_valid((*self.pNeighbor1)[i]) then ptr_free, (*self.pNeighbor1)[i]
      ptr_free, self.pNeighbor1
   endif
end


;*****************************************************************************************************
;+
; NAME:     init
;
; PURPOSE:  Object class creator method.
;
; SYNTAX:   Called automatically when the object is created.
;
; ARGUMENTS:
;
;  image: The image for which the active surface (3D contour) will be applied. This argument must be 3D.
;  xCoords: The initial X points of the active surface. Optional. Must be used with Y and Z.
;  yCoords: The initial Y points of the active surface. Optional. Must be used with X and Z.
;  zCoords: The initial Z points of the active surface. Optional. Must be used with X and Y.
;  polygons: The initial polygon list.
;
; KEYWORDS:
;
;  ALPHA: The elasticity parameter of the active surface. It reflects the surface's
;  ability to stretch along its length. Default: 1.0
;
;  BETA: The rigidity parameter of the active surface. It reflects the surface's
;    ability to bend as, for example, around corners. Default: 1.0
;
;  GAMMA: The viscosity parameter. Larger values make it harder to deform the active
;    surface in space. Default: 1.0
;
;  KAPPA: The external force weight. Default: 1.0
;
;  MU: The regularization parameter. This should be set according to the amount of
;    noise in the image. Use a larger value for noisier images. Default: 0.10
;
;  GVF_ITERATIONS: The number of iterations for calculating the Gradient Vector Flow (GVF). Default:10.
;
;  ITERATIONS: The number of iterations to use in calculating the surface positions. Default: 3.
;-
;*****************************************************************************************************
function C_sActiveContour3D::init, image, xCoords, yCoords, zCoords, polygons,$
                                   ALPHA = alpha, BETA = beta, GAMMA = gamma, KAPPA = kappa, MU = mu,$
                                   GVF_ITERATIONS = gvf_iterations, ITERATIONS = iterations
      ; Error handling.
   catch, theError
   if theError ne 0 then begin
      catch, /cancel
      return, 0
   endif

;   if (n_elements(image) ne 0) then $
;         ; Check dimensions.
;      if (size(image, /n_dim)) ne 3 then begin
;         ok = dialog_message('IMAGE argument must be a 3D array.')
;         return, -1
;   endif

   if n_elements(xCoords) ne 0 then x = ptr_new(xCoords) else x = ptr_new()
   if n_elements(yCoords) ne 0 then y = ptr_new(yCoords) else y = ptr_new()
   if n_elements(zCoords) ne 0 then z = ptr_new(yCoords) else z = ptr_new()
   if n_elements(polygons) ne 0 then pPolygons = ptr_new(polygons) else pPolygons = ptr_new()

      ; Check keyword parameters.
   if n_elements(alpha) eq 0 then alpha = 1.
   if n_elements(beta) eq 0 then beta = 1.
   if n_elements(gamma) eq 0 then gamma = 1.
   if n_elements(kappa) eq 0 then kappa = 1.
   if n_elements(mu) eq 0 then mu = 0.1
   if n_elements(gvf_iterations) eq 0 then gvf_iterations = 15
   if n_elements(iterations) eq 0 then iterations = 0

      ; Set object parameters.
   self.alpha = alpha
   self.beta = beta
   self.gamma = gamma
   self.kappa = kappa
   self.mu = mu
   self.gvf_iterations = gvf_iterations > 1
   self.iterations = iterations > 0
   if n_elements(image) ne 0 then self.pImage = ptr_new(image, /no_copy)
   self.pX = x
   self.pY = y
   self.pZ = z
   self.pPolygons = pPolygons
   self.pNeighbor1 = ptr_new(ptrArr(2))
   for i = 0, 1 do (*self.pNeighbor1)[i]=ptr_new([-1], /no_copy)
   self.pCurv = ptr_new(dblArr(2), /no_copy)
   if ptr_valid(self.pX) then self.npts = n_elements(*self.pX) else self.npts = 0ul

   return, 1
end


;*****************************************************************************************************
;
; NAME:     C_sActiveContour3D class definition.
;
; PURPOSE:  C_sActiveContour3D object's structure class definition code.
;
;*****************************************************************************************************
pro C_sActiveContour3D__define

   class = { C_sActiveContour3D,$
      pImage: ptr_new(),$    ; 3D image for which the surface is being calculated.
      pCurv: ptr_new(),$     ; array with the curvature for each surface node.
      pX: ptr_new(),$        ; X coordinates of the surface.
      pY: ptr_new(),$        ; Y coordinates of the surface.
      pZ: ptr_new(),$        ; Z coordinates of the surface.
      pNeighbor1: ptr_new(ptrArr(2ul)),$
      pPolygons: ptr_new(),$ ; Polygon list of the surface
      pU: ptr_new(),$        ; GVF-X gradient.
      pV: ptr_new(),$        ; GVF-Y gradient.
      pW: ptr_new(),$        ; GVF-Z gradient.
      npts: 0ul,$            ; Nmber of points in the surface.
      alpha: 0.0,$           ; Elasticity parameter (surface's tension).
      beta: 0.0,$            ; Rigidity parameter (surface's rigidity).
      gamma: 0.0,$           ; Viscosity parameter.
      kappa: 0.0,$           ; External force weight.
      mu: 0.0,$              ; Regularization parameter. Increase for noisy images.
      gvf_iterations: 0u,$   ; Number of iterations for the GVF field to converge.
      iterations: 0u }       ; Number of iterations for the surface to converge.
end