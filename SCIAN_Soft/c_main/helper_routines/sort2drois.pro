; sort2Drois
;
; Sort 2D ROI objects according to their x|y coordinates, in ascending order.
;
; ARGUMENTS
;   oROIgroup                                    ROIgroup object, containing the 2D ROI objects to be sorted.
;   xMinSorted/xMaxSorted, yMinSorted/yMaxSorted Optional output arrays for storing the corresponding ordered coordinates.
;
; NOTES
;   This function depends on IDL sort function which in turn depends on the OS's (operating system) imlpementation of sorting.
;   It has been reported that the sort output can vary depending on the OS.
;
; VERSION HISTORY
;   2013.06  Initial version (J. Jara)
function sort2Drois, oROIgroup, xMinSorted = xMinSorted, xMaxSorted = xMaxSorted, yMinSorted = yMinSorted, yMaxSorted = yMaxSorted

  nObjects = oROIgroup->count()

  if (nObjects lt 1) then return, -1

  ;xOrder = make_array(nObjects, /LONG)
  ;yOrder = make_array(nObjects, /LONG)

  xMinSorted = make_array(nObjects, /LONG)
  xMaxSorted = make_array(nObjects, /LONG)
  yMinSorted = make_array(nObjects, /LONG)
  yMaxSorted = make_array(nObjects, /LONG)
  ;xMinMaxCoords = make_array(2 * nObjects, /LONG)

  for i = 0L, nObjects-1 do begin
    (oROIgroup->get(position = i))->getProperty, roi_xrange = roi_xrange, roi_yrange = roi_yrange;, roi_zrange = roi_zrange
    xMinSorted[i] = roi_xrange[0]
    xMaxSorted[i] = roi_xrange[1]
    yMinSorted[i] = roi_yrange[0]
    yMaxSorted[i] = roi_yrange[1]
    ;xMinMaxCoords[arrPos : arrPos+1] = roi_xrange
    ;yMinMaxCoords[arrPos : arrPos+1] = roi_yrange
  endfor

  xMinSorted = sort(xMinCoords)
  xMaxSorted = sort(xMaxCoords)
  yMinSorted = sort(yMinCoords)
  yMaxSorted = sort(yMaxCoords)

  return, 1
end


; TODO JJ: experimental function... not ready for serious use
function comparePair, p1, p2
  case 1 of
   (p1[0] eq p2[0]) and (p1[0] eq p2[0]): return, 0
   (p1[0] lt p2[0]) and (p1[0] lt p2[0]): return, -1
   (p1[0] gt p2[0]) and (p1[0] gt p2[0]): return, 1
  endcase
end


; TODO JJ: experimental function... not ready for serious use
function sortPairs, pairs
  nPairs = n_elements(pairs)
  if (nPairs lt 1) then return, -1
  for i = 0L, nPairs-1 do begin
    sort
  endfor
  return, -1
end
