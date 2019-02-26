; s_HausdorffDistance3DMeshVertices
;
; PURPOSE
;   Computes the Hausdorff distance using the euclidean norm between two given sets of 3D points.
;
; RETURN VALUE
;   The computed Hausdorff distance (see http://en.wikipedia.org/wiki/Hausdorff_distance for example+illustration :)
;
; ARGUMENTS
;   xyzPoints1  3xn array of points of the first geometry.
;   xyzPoints2  3xn array of points of the second geometry.
;
; NOTES
;   [JJ] There is a forced change of type of the input point lists in order to avoid calculation errors from unsigned data types.
;
; HISTORY
;   First version, Jorge Jara (2012).
function s_HausdorffDistance3DMeshVertices, pXyzPoints1, pXyzPoints2
  nVerts1 = n_elements(xyzPoints1[0,*])
  nVerts2 = n_elements(xyzPoints2[0,*])
  dist12 = dblArr(nVerts1, /NOZERO) ; Works faster, but it's necessary to set all the array values.
  dist21 = dblArr(nVerts2, /NOZERO)

  for i = 0L, nVerts1-1 do begin
    dist12[i] = min(sqrt((xyzPoints2[0,*] - xyzPoints1[0,i])^2 $
                       + (xyzPoints2[1,*] - xyzPoints1[1,i])^2 $
                       + (xyzPoints2[2,*] - xyzPoints1[2,i])^2))
  endfor
    ; Now do the inverse loop, since the computation results are not symmetric
  for i = 0L, nVerts2-1 do begin
    dist21[i] = min(sqrt((xyzPoints1[0,*] - xyzPoints2[0,i])^2 $
                       + (xyzPoints1[1,*] - xyzPoints2[1,i])^2 $
                       + (xyzPoints1[2,*] - xyzPoints2[2,i])^2))
  endfor
  return, max(dist12) > max(dist21)
end


; s_HausdorffDistance2Dpoints
;
; PURPOSE
;   Computes the Hausdorff distance using the euclidean norm between two given point sets,
;   defined by ordered vertex lists.
;
; RETURN VALUE
;   The computed Hausdorff distance (see http://en.wikipedia.org/wiki/Hausdorff_distance for example+illustration :)
;
; ARGUMENTS
;   x_1, y_1  The input coordinates of the first point set.
;   x_2, y_2  The input coordinates of the second point set.
;
;
; HISTORY
;   First version, Jorge Jara (2013).
function s_HausdorffDistanceFor2Dpoints, x_1, y_1, x_2, y_2
  nVerts1 = n_elements(x_1)
  nVerts2 = n_elements(x_2)
  x1 = double(x_1) ; Avoid errors due to unsigned types.
  y1 = double(y_1)
  x2 = double(x_2)
  y2 = double(y_2)
  dist12 = dblArr(nVerts1, /NOZERO) ; Works faster, but it becomes necessary to set all the array values.
  dist21 = dblArr(nVerts2, /NOZERO)
  for i = 0L, nVerts1-1 do $
    dist12[i] = min(sqrt((x2 - x1[i])^2 + (y2 - y1[i])^2))
  for i = 0L, nVerts2-1 do $
    dist21[i] = min(sqrt((x1 - x2[i])^2 + (y1 - y2[i])^2))
  return, max(dist12) > max(dist21)
end


; s_HausdorffDistance2Dpoints
;
; HISTORY
;   First version, Jorge Jara (2012).
;   Refactored version, Jorge Jara (2013).
function s_HausdorffDistance2Dpoints, xyPoints_1, xyPoints_2

  return, s_HausdorffDistanceFor2Dpoints(xyPoints_1[0,*], xyPoints_1[1,*], xyPoints_2[0,*], xyPoints_2[1,*])
;  nVerts1 = n_elements(xyPoints_1[0,*])
;  nVerts2 = n_elements(xyPoints_2[0,*])
;  t1 = size(xyPoints1, /TYPE)
;  t2 = size(xyPoints2, /TYPE)
;  xyPoints1 = double(xyPoints_1) ; Avoid nasty errors due to unsigned types.
;  xyPoints2 = double(xyPoints_2)
;  dist12 = dblArr(nVerts1, /NOZERO) ; Works faster, but it's necessary to set all the array values.
;  dist21 = dblArr(nVerts2, /NOZERO)
;  for i = 0L, nVerts1-1 do $
;    dist12[i] = min(sqrt((xyPoints2[0,*] - xyPoints1[0,i])^2 $
;                       + (xyPoints2[1,*] - xyPoints1[1,i])^2))
;  for i = 0L, nVerts2-1 do $
;    dist21[i] = min(sqrt((xyPoints1[0,*] - xyPoints2[0,i])^2 $
;                       + (xyPoints1[1,*] - xyPoints2[1,i])^2))
;  return, max(dist12) > max(dist21)
end


; s_HausdorffDistance2DpointsTest
;
; PURPOSE
;   Test method for s_HausdorffDistance2Dpoints. Quite simple for now...
;
; HISTORY
;   First version, Jorge Jara (2012).
;   Refactored version, Jorge Jara (2012).
pro s_HausdorffDistance2D_Test
  p1x = [0, 2, 2, 0]
  p1y = [0, 0, 2, 2]
  p2x = [1, 3, 3, 1]
  p2y = [0, 0, 2, 2]
  p3x = [3, 4, 4, 3]
  p3y = [3, 3, 4, 4]
  dp1p2 = 1.0d
  dp1p3 = sqrt(18.0d)
  print, 's_HausdorffDistance2Dpoints tests'
  print, 'Error in p1-p2 distance :', dp1p2 - s_HausdorffDistance2Dpoints([transpose(p1x), transpose(p1y)],$
                                                                          [transpose(p2x), transpose(p2y)])
  print, 'Error in p1-p3 distance :', dp1p3 - s_HausdorffDistance2Dpoints([transpose(p1x), transpose(p1y)],$
                                                                          [transpose(p3x), transpose(p3y)])
  print, 's_HausdorffDistanceFor2Dpoints tests'
  print, 'Error in p1-p2 distance :', dp1p2 - s_HausdorffDistanceFor2Dpoints(p1x, p1y, p2x, p2y)
  print, 'Error in p1-p3 distance :', dp1p3 - s_HausdorffDistanceFor2Dpoints(p1x, p1y, p3x, p3y)

end
