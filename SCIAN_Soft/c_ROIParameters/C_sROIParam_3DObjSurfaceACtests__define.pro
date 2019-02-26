;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjSurfaceACtests
;
; PURPOSE:
;       - Calculation of 3D Active Contours parameters for segmentation testing
;
; AUTHOR:
;     Jorge Jara (2011)
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjSurfaceACtests')
;
; METHOHDS:
;_____________________________IOISIOI____________________

; TODO STRONG ASSUMPTION: the number of ROIs is the same, and the ROI numbers match... improve
pro C_sROIParam_3DObjSurfaceACtests::apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position

  xyzSizePerPixel = C_sROI3DGroupObj->getxyzSizePerPixel()
  xyzDim          = C_sROI3DGroupObj->getxyzDim()
  pParamStruct    = C_sROI3DGroupObj->getpParamStruct()
  clusPos  = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Cluster Position'))[0]]
  chPos    = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Channel Position'))[0]]
  tPos     = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Time Position'))[0]]
  ; TODO make a user-definable parameter of this, and keep an eye on the total number of times
  tStep    = 1
  tPosNext = tPos + tStep

    ; get imageStackInfoObject
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = totalTNum
  if (tPosNext ge totalTNum) then begin
    print, 'parameter calculation not applicable to times ', tPos, ' and ', tPosNext, ' (out of range)'
    return
  endif

  nParams = n_elements((*(*self.pParamStruct).pNames))
  whParam = (where(*(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
  for i = 1, nParams-1 do whParam = [whParam, (where(*(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0]]

    ; check active parameters
  whParamActive = whParam * 0
  case (n_elements(position) gt 0) of
    1   : if (position[0] eq -1) then return else whParamActive[position] = 1
    else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
  endcase

    ; check pointers
  wherePA = where(whParamActive eq 1)
  if ~(ptr_valid((*self.pParamStruct).pROINumberVect)) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
  if (wherePA[0] eq -1) then return

  for i = 0, n_elements(wherePA)-1 do $
    if ~(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect)) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

  nObjects = C_sROI3DGroupObj->count()
  if (nObjects lt 1) then begin
    *(*self.pParamStruct).pROINumberVect = -1
    for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    return
  endif

    ; set Object Number Vector
  *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()

    ; set Object Parameter Vectors
  for i = 0, n_elements(whParam)-1 do $
    if whParamActive[i] then *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float, value = -1.)

  ; Create arrays for parameter calculations of the Dice coefficient (DC).
  ; DC computation is averaged for each 3D ROI from its corresponding 2D slices
  ; calculate 2D contours at time i, for all objects using 2D active contours, and compute their DCs
  dcValuesCurrentTime = make_array(nObjects, xyzDim[2], /double)
  dcValuesNextTime    = make_array(nObjects, xyzDim[2], /double)
  avgDCnextTime       = make_array(nObjects, /double)
  stdDevDCnextTime    = make_array(nObjects, /double)

  dcOvrAreas    = make_array(nObjects, xyzDim[2], /double, value = 0)
  dcManualAreas = make_array(nObjects, xyzDim[2], /double, value = 0)
  dcSnakesAreas = make_array(nObjects, xyzDim[2], /double, value = 0)
    ; Use -1 value to identify non-defined DC in... DC values are nonnegatives... yes, a little inefficient :P
  dcCoeffArray  = make_array(nObjects, xyzDim[2], /double, value = -1)

  dcCoeffWeightedACAvg   = make_array(nObjects, /double, value = 0)
  dcCoeffWeightedMaskAvg = make_array(nObjects, /double, value = 0)
  dcCoeffUnweightedAvg   = make_array(nObjects, /double, value = 0)
  sumAreasSnakes         = make_array(nObjects, /double, value = 0)
  sumAreasMasks          = make_array(nObjects, /double, value = 0)

  winFactor = (xyzDim[0] gt 1360) ? 0.8 : 1
  if (xyzDim[1] gt xyzDim[0]) then winFactor = (xyzDim[1] gt 760) ? 0.8 * winFactor : winFactor
  wsx = xyzDim[0] * winFactor
  wsy = xyzDim[1] * winFactor

  volData1 = make_array(xyzDim, /byte)
  for z = 0, xyzDim[2]-1 do $
    volData1[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)

  volData2 = make_array(xyzDim, /byte)
  for z = 0, xyzDim[2]-1 do $
    volData2[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos+tStep, chPos = chPos, zPos = z)

  ; optical flow parameters
  ofAlpha      = 80.0
  ofIterations = 200
  ofRho        = 2.0
  ofSigma      = 1.0
  ofMethod     = 0
  case ofMethod of
    0   : opticalFlowHS3D, volData1, volData2, u, v, w, alpha = ofAlpha, iterations = ofIterations
    else: print, 'Ouch! Not yet'
  endcase

    ; active contour parameters
  acAlpha = 0.1
  acBeta  = 1.0
  acGamma = 0.5
  acKappa = 1
  acMu    = 0.01
  acPerimeterFactor = 1.0
  acSnakeIterations = 10
  acGVFiterations   = 100
  acGVFmethod       = 2 ; 0 -> GVF, 1 -> GGVF, 2 -> custom EPGVF

  savedFileName = 'o3DAC_t' + tPos + '_c' + chPos + '_clus' + clusPos
  savedFileName = strCompress(savedFileName, /remove_all)
  fSavedROIs = file_test(savedFileName)
;cJ035128183US

  if (fSavedROIs eq 1) then begin
    restore, savedFileName, RESTORED_OBJECTS = oROI3DGroup, /RELAXED_STRUCTURE_ASSIGNMENT
  endif else oROI3DGroup = C_sROI3DGroupObj

  nObjects = n_elements(oROI3DGroup)
  roiPts = ptrArr(nObjects)
  roiNumVec = make_array(nObjects, /ULONG)

  for i = 0, nObjects-1 do begin
    oRoi = oROI3DGroup->get(position = i)
    roiPts[i] = ptr_new(oRoi->getXyzPoints())
    opParamStruct = oRoi->getpParamStruct()
    roiNumVec[i] = *(*opParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]] ; TODO check number getter method
  endfor

  for i = 0, nObjects-1 do obj_destroy, oROI3DGroup[i]

  C_sROI3DGroupObjNext = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPosNext, chPos = chPos, clusPos = clusPos, fileName = fileName)
  nObjectsNext = C_sROI3DGroupObjNext->count()
  roiPtsNext = ptrArr(nObjectsNext)
  roiNumVecNext = make_array(nObjectsNext, /ULONG)

  for i = 0, nObjectsNext-1 do begin
    oRoi = C_sROI3DGroupObjNext->get(position = i)
    roiPtsNext[i] = ptr_new(oRoi->getXyzPoints())
    opParamStruct = oRoi->getpParamStruct()
    roiNumVec[i] = *(*opParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]] ; TODO check number getter method
  endfor

  for i = 0, n_elements(C_sROI3DGroupObjNext)-1 do obj_destroy, C_sROI3DGroupObjNext[i]

    ; Find the ROIs whose number match between the current and the next time
  matches = match2arrays(roiNumVec, roiNumVecNext)

  for z = 0, xyzDim[2]-1 do begin
;if z eq 31 then begin
    zSliceCur  = imageStackInfoObject->getSelectedImage(tPos = tPos,     chPos = chPos, zPos = z)
    zSliceNext = imageStackInfoObject->getSelectedImage(tPos = tPosNext, chPos = chPos, zPos = z)
    ;opticalFlowHS, zSliceCur, zSliceNext, u, v, alpha = 120, iterations = 200, verbose = 0
    ;opticalFlowCLG, zSliceCur, zSliceNext, u, v, alpha = 120, iterations = 200, verbose = 0
    scaleFactor = 6
    ct1 = congrid(zSliceCur, xyzDim[0] / scaleFactor, xyzDim[1] /scaleFactor)
    ct2 = congrid(zSliceNext, xyzDim[0] / scaleFactor, xyzDim[1] / scaleFactor)
    opticalFlowCLG, ct1, ct2, u, v, alpha = 120, iterations = 200, verbose = 0
    u = congrid(u, xyzDim[0], xyzDim[1]) * scaleFactor
    v = congrid(v, xyzDim[0], xyzDim[1]) * scaleFactor
    frameSide = 4
;    frameDim = 2*frameSide + 1
;    frame = make_array(frameDim, frameDim, /double, value = 1.0 / frameDim^2)
    ;window, 1, xSize = wsx, ySize = wsy
    oAC = obj_new('C_sActiveContour3D', zSliceNext, xCoords, yCoords,$
                                        ALPHA          = acAlpha,$
                                        BETA           = acBeta,$
                                        GAMMA          = acGamma,$
                                        ITERATIONS     = acSnakeIterations,$
                                        KAPPA          = acKappa,$
                                        GVF_ITERATIONS = acGVFiterations)
      ; GVF computation
    case acGVFmethod of
      0: oAC->calcGVF, mu = acMu
      1: oAC->calcGGVF, mu = acMu
      2: oAC->calcEPGVF
    end

    for i = 0, nObjects-1 do begin
      mockImage = make_array(xyzDim[0], xyzDim[1], /byte, value = 0)
      whZ = where((*roiPtsNext[i])[2,*] eq z, nPix)
      roiValue = 1
      if (nPix gt 0) then begin
        pPolygons     = self->assemblePolygonFrom3DroiPointsInZ(*roiPts[i],     z, xyzDim[0:1])
        ; TODO use matching array here... match2arrays(roiNumVec, roiNumVecNext)
        pPolygonsNext = self->assemblePolygonFrom3DroiPointsInZ(*roiPtsNext[i], z, xyzDim[0:1])
        nContours     = keyword_set(pPolygons)     ? n_elements(pPolygons)     : -1
        nContoursNext = keyword_set(pPolygonsNext) ? n_elements(pPolygonsNext) : -1
        ; TODO not implemented: handling of different number of contours in the same plane from t to t+1
        if (nContours gt 1) or (nContoursNext gt 1) then begin
          print, '(yet) warning: object', i, ' at zSlice ', z
          print, '(yet) warning: ', nContours, ' 2D contours at current time and ', nContoursNext, ' at the next'
        endif
        if (nContours eq 1) and (nContoursNext eq 1) then begin
        ;if (nContours ge 1) then begin
          for j = 0, nContours-1 do begin
            print, 'going for contour ', j, ' of ROI ', i, ' of z ', z
            xCoords = (*pPolygons[j])[0,*]
            yCoords = (*pPolygons[j])[1,*]
;deltaXY = 10 ; TODO another magic value...
;xmin = min(xCoords, max = xmax)
;ymin = min(yCoords, max = ymax)
;xmin -= deltaXY
;xmax += deltaXY
;ymin -= deltaXY
;ymax += deltaXY
;sx = (xmax - xmin) + 1
;sy = (ymax - ymin) + 1
;xminf = xmin / scaleFactor
;xmaxf = xmax / scaleFactor
;yminf = ymin / scaleFactor
;ymaxf = ymax / scaleFactor
;opticalFlowCLG, congrid(zSliceCur[xmin : xmax, ymin : ymax], sx / 2, sy / 2),$
;                congrid(zSliceNext[xmin : xmax, ymin : ymax], sx / 2, sy / 2),$
;                u, v, alpha = 120, iterations = 200, verbose = 0
;u = congrid(u, sx, sy) * scaleFactor
;v = congrid(v, sx, sy) * scaleFactor
;mu = make_array(xyzDim[0], xyzDim[1], /float, value = 0.0)
;mv = make_array(xyzDim[0], xyzDim[1], /float, value = 0.0)
;mu[xmin:xmax,ymin:ymax] = u
;mv[xmin:xmax,ymin:ymax] = v
            for p = 0, n_elements(xCoords)-1 do begin
              xPos = round(xCoords[p])
              yPos = round(yCoords[p])
              ltx = frameSide
              gtx = frameSide
              lty = frameSide
              gty = frameSide
              ; TODO include boundary handling...
              um = mean(u[xPos-ltx:xPos+gtx,yPos-lty:yPos+gty])
              vm = mean(v[xPos-ltx:xPos+gtx,yPos-lty:yPos+gty])
              xCoords[p] += um
              yCoords[p] += vm
            endfor
polyImgPath = 'c:\RSI\aabm_tests\exp1\ovrPolys\exp1_t' + string(tPos) + '_c' + string(i) + '_z' + string(z) 
            window, 31, retain = 2, xsize = xyzDim[0], ysize = xyzDim[1]
            plots, (*pPolygons[j])[0,*], (*pPolygons[j])[1,*]
            tmp = tvrd()
            mockImg = make_array(3, xyzDim[0], xyzDim[1], /byte, value = 0b)
            mockImg[0,*,*] = tmp
            write_tiff , strCompress(polyImgPath + '_currentManual.tif', /remove_all), mockImg

            window, 31, retain = 2, xsize = xyzDim[0], ysize = xyzDim[1]
            plots, (*pPolygonsNext[j])[0,*], (*pPolygonsNext[j])[1,*]
            tmp = tvrd()
            mockImg = make_array(3, xyzDim[0], xyzDim[1], /byte, value = 0b)
            mockImg[0,*,*] = tmp
            write_tiff , strCompress(polyImgPath + '_nextManual.tif', /remove_all), mockImg
polyfill, (*pPolygonsNext[j])[0,*], (*pPolygonsNext[j])[1,*]
tmp = tvrd()
refImg = tmp
            window, 31, retain = 2, xsize = xyzDim[0], ysize = xyzDim[1]
            plots, xCoords, yCoords
            tmp = tvrd()
            mockImg = make_array(3, xyzDim[0], xyzDim[1], /byte, value = 0b)
            mockImg[1,*,*] = tmp
            write_tiff , strCompress(polyImgPath + '_nextProjected.tif', /remove_all), mockImg

              ; active contour segmentation
            ;oAC->setContour, (*pPolygons[j])[0,*], (*pPolygons[j])[1,*]
            oAC->setContour, xCoords, yCoords
            npts = round((oAC->getPerimeter()) * acPerimeterFactor) > 16
            oAC->arcSample, points = npts
            res = oAC->adjustContour()

            window, 31, retain = 2, xsize = xyzDim[0], ysize = xyzDim[1]
            polyfill, res.x, res.y
snkImg = tvrd()
            window, 31, retain = 2, xsize = xyzDim[0], ysize = xyzDim[1]
            plots, res.x, res.y
            tmp = tvrd()
            mockImg = make_array(3, xyzDim[0], xyzDim[1], /byte, value = 0b)
            mockImg[1,*,*] = tmp
            write_tiff , strCompress(polyImgPath + '_nextSnake.tif', /remove_all), mockImg
ovrImg = refImg * snkImg
whRef = where(refImg ne 0, nPixRef)
whSnk = where(snkImg ne 0, nPixSnk)
whOvr = where(ovrImg ne 0, nPixOvr)
dcOvrAreas[i,z]    += 2.0 * nPixOvr / (nPixRef + nPixSnk)
dcManualAreas[i,z] += nPixRef
sumAreasMasks[i]   += dcManualAreas[i,z]
dcSnakesAreas[i,z] += nPixSnk
sumAreasSnakes[i]  += dcSnakesAreas[i,z]
;            ovr = calcPolygonIntersection(P1X = transpose((*pPolygonsNext[j])[0,*]), P1Y = transpose((*pPolygonsNext[j])[1,*]),$
;                                     P2X = res.x, P2Y = res.y)
;            numOvrPolys = ovr[0]
;            polyStart = 2 ; go from 2, since ovr[1] contains the number of vertices for the 1st polygon
;              ; DC computation, iterate over the resulting overlap polygon(s)
;            for c = 0, numOvrPolys-1 do begin
;              np = ovr[polyStart - 1]
;              dcOvrAreas[i,z] += polyArea(XCOORDS = reverse(ovr[polyStart      : polyStart +   np - 1]),$
;                                          YCOORDS = reverse(ovr[polyStart + np : polyStart + 2*np - 1]))
;              ;window, 15, XSIZE = 750, YSIZE = 700
;              ;plot, ovr[polyStart : polyStart+np-1], ovr[polyStart+np : polyStart+2*np-1]
;              polyStart += 2*np + 1
;            endfor
;            dcManualAreas[i,z] += polyArea(xCoords = reverse(transpose((*pPolygonsNext[j])[0,*])),$
;                                           yCoords = reverse(transpose((*pPolygonsNext[j])[1,*])))
;            sumAreasMasks[i]   += dcManualAreas[i,z]
;            dcSnakesAreas[i,z] += polyArea(xCoords = reverse(res.x), yCoords = reverse(res.y))
;            sumAreasSnakes[i]  += dcSnakesAreas[i,z]
          endfor
            ; Once processed all the contours for the current object in the current slice, compute and
            ; store the corresponding Dice coefficient.
          dcCoeffArray[i,z] = 2 * dcOvrAreas[i,z] / (dcManualAreas[i,z] + dcSnakesAreas[i,z])
          strm = 'Dice coef for roi in object ' + strCompress(string(i), /remove_all)$
               + ' at zSlice ' + strCompress(z, /remove_all)$
               + ' from time ' + strCompress(string(tPos), /remove_all)$
               + ' to ' + strCompress(string(tPosNext), /remove_all)$
               + ' is ' + strCompress(string(dcCoeffArray[i,z]), /remove_all)
          print, strm
        endif else begin
          print, 'Ouch! No contours were recovered for object ', i, ' at zPos ', z
        endelse
      endif
    endfor
    obj_destroy, oAC
;endif ; z eq 31, debug
  endfor
;  return
    ; consolidating results
  for i = 0, nObjects-1 do begin
    for z = 0, xyzDim[2]-1 do begin
      ; use only values actually computed (-1 used as marker for z-slices with no ROI data)
      if (dcCoeffArray[i,z] ne -1) then begin
        dcCoeffWeightedACAvg[i] += (dcCoeffArray[i,z] * dcSnakesAreas[i,z])
        dcCoeffWeightedMaskAvg[i] += (dcCoeffArray[i,z] * dcManualAreas[i,z])
        dcCoeffUnweightedAvg[i] += dcCoeffArray[i,z]
      endif
    endfor
    whZne0 = where(dcCoeffArray[i,*] ne -1, count)
    dcCoeffUnweightedAvg[i] /= count
    dcCoeffWeightedACAvg[i] /= sumAreasSnakes[i]
    dcCoeffWeightedMaskAvg[i] /= sumAreasMasks[i]
  endfor
    ; std. dev. for the unweighted DC
  sdDC = make_array(nObjects, /float, value = 0.0)
  for i = 0, nObjects-1 do begin
    whZne0 = where(dcCoeffArray[i,*] ne -1, count)
    if (count gt 0) then begin
     tmp = (dcCoeffArray[i,*])[whZne0] - dcCoeffUnweightedAvg[i]
     varDC = sqrt(total(tmp) / count)
    endif else sdDC[i] = -1.0 ; indicate that something went wrong here
  endfor
  print, 'dcCoeffWeightedMaskAvg ', dcCoeffWeightedMaskAvg
  print, 'dcCoeffWeightedACAvg ', dcCoeffWeightedACAvg
  print, 'dcCoeffUnweightedAvg ', dcCoeffUnweightedAvg
  print, 'dcCoeffUnweightedStdDev ', sdDC
  ; delete ROI-points lists
  for i = 0, nObjects-1 do ptr_free, roiPts[i]

  *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()

  if whParamActive[0] then $
    *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = dcCoeffUnweightedAvg

  if whParamActive[1] then $
    *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = dcCoeffWeightedACAvg

  if whParamActive[2] then $
    *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = dcCoeffWeightedMaskAvg

  if whParamActive[3] then $
    *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = sdDC
end


; Separated method for development purposes.
; WARNING: it is assumed that the ROIs match from ont time to another given the number attribute
function C_sROIParam_3DObjSurfaceACtests::doHausdorffDistanceCalc, C_sROI3DGroupObj1 = C_sROI3DGroupObj1, C_sROI3DGroupObj2 = C_sROI3DGroupObj2
  nObj1 = C_sROI3DGroupObj1->count()
  nObj2 = C_sROI3DGroupObj2->count()
  hdValues = make_array(nObj1, /double, value = 0.0)
  for i = 0, nObj1-1 do begin
    roiPts1 = (C_sROI3DGroupObj1->get(position = i))->getxyzpoints()
    roiPts2 = (C_sROI3DGroupObj2->get(position = i))->getxyzpoints()
    hdValues[i] = s_HausdorffDistanceForROIVertices(roiPts1, roiPts2)
  endfor
  return, hdValues
end


pro C_sROIParam_3DObjSurfaceACtests::projectWithOpticalFlow3D, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, tStep = tStep
  ; get 3d image, compute of, move ROI...
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  xyzDim = C_sROI3DGroupObj->getxyzDim()
  pParamStruct = C_sROI3DGroupObj->getpParamStruct()
  clusPos = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Cluster Position'))[0]]
  chPos   = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Channel Position'))[0]]
  tPos    = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Time Position'))[0]]

  volData1 = make_array(xyzDim, /byte)
  for z = 0, xyzDim[2]-1 do $
    volData1[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)

  volData2 = make_array(xyzDim, /byte)
  for z = 0, xyzDim[2]-1 do $
    volData2[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos+tStep, chPos = chPos, zPos = z)

  ; TODO allow parametrized form of 3D-OF computation
  ofAlpha = 80.0
  ofIterations = 200
  ofRho = 2.0
  ofSigma = 1.0
  ofMethod = 0
  opticalFlowHS3D, volData1, volData2, u, v, w, alpha = ofAlpha, iterations = ofIterations
  print, 'now what?'
end


; xyzPoints 3xn array of the 3D ROI points
; zCoord    the z value corresponding to the polygon(s) plane
; xyDim     x and y dimensions of a 2D frame used to construct the polygon (not really necessary, but allows the use of simpler methods)
function C_sROIParam_3DObjSurfaceACtests::assemblePolygonFrom3DroiPointsInZ, xyzPoints, zCoord, xyDim

  whZ = where(xyzPoints[2,*] eq zCoord, nPix)
  if (nPix eq 0) then return, ptr_new()

  roiValue = 1
  xCoords = xyzPoints[0, whZ]
  yCoords = xyzPoints[1, whZ]

  mockImage = make_array(xyDim, /byte, value = 0)
  mockImage[xCoords, yCoords] = 1
  labeledImage = label_region(mockImage, /all_neighbors)
  h = histogram(labeledImage)
  nRois = n_elements(h)-1
  pPoints = ptrArr(nRois)
  ; a somewhat inefficient loop but with simple logic ;)
  for i = 1, nRois do begin
    mockImage[*] = 0
    mockImage[where(labeledImage eq i)] = roiValue
    contour, mockImage, level = roiValue, path_info = pathInfo, path_xy = pathXY, /PATH_DATA_COORDS
    pPoints[i-1] = ptr_new(pathXY, /no_copy)
  endfor
  return, pPoints
end


; match2arrays
;
; Given two input arrays (possibly unsorted), the function find (if any) and returns an array of pairs, 
; each of them containing the positions of the matching elements in the input arrays.
;
; PARAMETERS:
; - arr1      The first input array.
; - arr2      The second input array.
; - nMatches  Optional argument, stores the number of matches found.
;
; RETURN VALUE:
; - An array of 2xn pairs (with n the number of matches found) is returned; for a given pair, its first 
;   and second element contain the index for the corresponding elements in the first and second array,
;   respectively. 
; - If no match is found, -1 is returned.
;
; NOTES:
; - No checking are performed for arrays with repeated elements.
; - Careful with large arrays, since the sorting is not being made in place and thus comparisons could 
;   require access to non-consecutive array portions (this could lead to several page faults).
function match2arrays, arr1, arr2, nMatches = nMatches

  sz1 = n_elements(arr1)
  sz2 = n_elements(arr2)
  if (sz1 eq 0) or (sz2 eq 0) then begin
    if arg_present(nMatches) then nMatches = 0
    return, -1
  endif

  matches = make_array(2, sz1 < sz2, /ULONG)
  s1 = sort(arr1)
  s2 = sort(arr2)

  pos1 = 0
  pos2 = 0
  matchesCount = 0

  for pos1 = 0, sz1-1 do begin
    if (arr1[s1[pos1]] gt arr2[s2[pos2]]) then $
      ; Note the use && instead of "and" or "&" to avoid double "if"
      while ((pos2 lt sz2) && (arr1[s1[pos1]] gt arr2[s2[pos2]])) do pos2 += 1

    if (pos2 eq sz2) then goto, r

    if (arr1[s1[pos1]] eq arr2[s2[pos2]]) then begin
      matches[0,matchesCount] = s1[pos1]
      matches[1,matchesCount] = s2[pos2]
      pos2 += 1
      matchesCount += 1
    endif

    if (pos2 eq sz2) then goto, r

  endfor

r: if arg_present(nMatches) then nMatches = matchesCount
   return, matchesCount gt 0 ? matches[*, 0:matchesCount-1] : -1
end


pro match2arraysTest
  s = [1, 4, 2, 5, 6, 7]
  p = [4, 2, 66, 5, 3, 22, 1, 10]
  m = match2arrays(s, p)
  print, m

  s = [1, 4, 2, 5, 6, 7]
  p = [140]
  m = match2arrays(s, p)
  print, m

  s = [1, 4, 2, 5, 6, 7]
  p = [140]
  m = match2arrays(p, s)
  print, m

  s = [1]
  p = [140]
  m = match2arrays(p, s)
  print, m

  s = [140]
  p = [140, 140]
  m = match2arrays(p, s)
  print, m

end


pro testSeqMaskReadingROImaking
  image = read_tiff('c:\rsi\sciansoft_tests\2rois.tif')
  ; Get blob indices:
  b = label_region(image) 
  ; Get population and members of each blob: 
  h = histogram(b, REVERSE_INDICES=r) 
  ; Each region
  for i = 0, n_elements(h)-1 do begin
    ;Find subscripts of members of region i.
    p = r[r[i]:r[i+1]-1]
    ; Pixels of region i
    q = image[p]
    print, 'Region ', i, ', Population = ', h[i], ', Min = ', min(q, max = maxax), ', Max = ', maxax
    ;print, 'Region ', i, ', Population = ', h[i], ', Standard Deviation = ', stDev(q, mean), 'Mean = ', mean
    contour, b, LEVEL = i, PATH_INFO = pathInfo, PATH_XY = pathXY, /PATH_DATA_COORDS
    line = [LindGen(PathInfo(I).N), 0]
    oROI = obj_new('IDLanROI', (pathXY(*, pathInfo(I).OFFSET + line))[0, *], (pathXY(*, pathInfo(I).OFFSET + line))[1, *])
  endfor
  xs = 700
  ys = 700
  window, 31, xsize = xs, ysize = ys
  tvscl, congrid(b, xs, ys)
end


function C_sROIParam_3DObjSurfaceACtests::init

  ROIParamStruct = {name: '3D Object Surface AC test',$ ; 3D-ROI-Obj Parameter Name.
                    type: '3D ROI-Parameter-Method',$   ; 3D-ROI-Obj Parameter Type.
                    pWidgetType   : ptr_new(),$ ; Pointer on ROI-Obj Parameter Names.
                    pNames        : ptr_new(),$ ; Pointer on ROI-Obj Parameter Names.
                    pActive       : ptr_new(),$ ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin          : ptr_new(),$ ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax          : ptr_new(),$ ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues       : ptr_new(),$ ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect: ptr_new()}  ; Pointer on ROI-Obj Number Vector

  ROIParamNames = ['dcCoeffUnweightedAvg',$
                   'dcCoeffWeightedACAvg',$
                   'dcCoeffWeightedMaskAvg',$
                   'dcCoeffUnweightedStdDev']

  nParams = n_elements(ROIParamNames)
  ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
  self.pValueStruct = ptr_new(ptrArr(nParams))

  ROIParamActive = make_array(nParams, /byte, value = 1b)
  ROIParamMin    = make_array(nParams, /float, value = 0.)
  ROIParamMax    = make_array(nParams, /float, value = 1.)
  ROIParamValues = make_array(nParams, /float, value = 0.)
  pROINumberVect = [-1]

  ROIParamStruct.pWidgetType    = ptr_new(ROIParamWidgetType, /no_copy)
  ROIParamStruct.pNames         = ptr_new(ROIParamNames, /no_copy)
  ROIParamStruct.pActive        = ptr_new(ROIParamActive, /no_copy)
  ROIParamStruct.pMin           = ptr_new(ROIParamMin, /no_copy)
  ROIParamStruct.pMax           = ptr_new(ROIParamMax, /no_copy)
  ROIParamStruct.pValues        = ptr_new(ROIParamValues, /no_copy)
  ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
  self.pParamStruct             = ptr_new(ROIParamStruct, /no_copy)

  ROIValueStruct = {name         : (*(*self.pParamStruct).pNames)[0],$
                    type         : '3D ROI-Parameter-Method',$
                    pWidgetType  : ptr_new(),$
                    pNames       : ptr_new(),$
                    pActive      : ptr_new(),$
                    pMin         : ptr_new(),$
                    pMax         : ptr_new(),$
                    pValues      : ptr_new(),$
                    pROIParamVect: ptr_new()}

  ROIValueNames  = ['Threshold_1a','Threshold_1b',$
                    'Threshold_2a','Threshold_2b',$
                    'Threshold_3a','Threshold_3b',$
                    'Threshold_4a','Threshold_4b']

  ROIValueWidgetType = make_array(n_elements(ROIValueNames), /string, value = 'widget_slider')
  ROIValueActive = [0,0,0,0,0,0,0,0]
  ROIValueMin    = [0.,0.,0.,0.,0.,0.,0.,0.]
  ROIValueMax    = [1.,1.,1.,1.,1.,1.,1.,1.]
  ROIValueValues = [0.,1.,0.,1.,0.,1.,0.,1.]

  pROIParamVect = [-1]
  ROIValueStruct.pWidgetType   = ptr_new(ROIValueWidgetType)
  ROIValueStruct.pNames        = ptr_new(ROIValueNames)
  ROIValueStruct.pActive       = ptr_new(ROIValueActive)
  ROIValueStruct.pMin          = ptr_new(ROIValueMin)
  ROIValueStruct.pMax          = ptr_new(ROIValueMax)
  ROIValueStruct.pValues       = ptr_new(ROIValueValues)
  ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
  (*self.pValueStruct)[0]      = ptr_new(ROIValueStruct, /no_copy)

  for i = 1, nParams-1 do begin
    ROIValueStruct = {name         : (*(*self.pParamStruct).pNames)[i],$
                      type         : '3D ROI-Parameter-Method',$
                      pWidgetType  : ptr_new(),$
                      pNames       : ptr_new(),$
                      pActive      : ptr_new(),$
                      pMin         : ptr_new(),$
                      pMax         : ptr_new(),$
                      pValues      : ptr_new(),$
                      pROIParamVect: ptr_new()}

    ROIValueWidgetType = [ROIValueWidgetType]
    ROIValueNames = [ROIValueNames]
    ROIValueActive = [ROIValueActive]
    ROIValueMin = [ROIValueMin]
    ROIValueMax = [ROIValueMax]
    ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType   = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames        = ptr_new(ROIValueNames)
    ROIValueStruct.pActive       = ptr_new(ROIValueActive)
    ROIValueStruct.pMin          = ptr_new(ROIValueMin)
    ROIValueStruct.pMax          = ptr_new(ROIValueMax)
    ROIValueStruct.pValues       = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[i] = ptr_new(ROIValueStruct, /no_copy)
  endfor
  return, 1
end


pro C_sROIParam_3DObjSurfaceACtests__define
  tmp = {C_sROIParam_3DObjSurfaceACtests,$
         pParamStruct: ptr_new(),$
         pValueStruct: ptr_new(),$
         inherits C_sROIParam}
end
