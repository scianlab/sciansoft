; s_histogramOfOrientedGradients
;
; Image descriptor based on Histogram of Orientated Gradients for gray-level images.
;
; ARGUMENTS
;
;   image  2D array of CxL elements containing the input image, in the form of 
;          pixel gray-scale intensities.
;   nBins  .
;   nWin_x .
;   nWin_y .
;
; HISTORY
;
;  Original MATLAB code: O. Ludwig, D. Delgado, V. Goncalves, and U. Nunes,
;  developed for the work
;  Trainable Classifier-Fusion Schemes: An Application To Pedestrian Detection,
;  In: 12th International IEEE Conference On Intelligent Transportation Systems, 
;  2009, St. Louis, 2009. V. 1. P. 432-437.
;  In case of publication with this code, please cite the paper above.
;
;  IDL code by Victor Castañeda, Jorge Jara (2013) http://www.scian.cl
;  TODO JJ-VC untested version!
function s_histogramOfOrientedGradients, image, nBins = nBins, nWin_x = nWin_x, nWin_y = nWin_y, angle_min = angle_min, angle_max = angle_max,$
                                         hog_correspondences = hog_correspondences, vec_x = vec_x, vec_y = vec_y, grad_xr = grad_xr, grad_yu = grad_yu,$
                                         gradientThresholdMag = gradientThresholdMag, hog_angles = hog_angles

    ; Set the number of HOG windows per bound box in x and y
  nWin_x = keyword_set(nWin_x) ? (nWin_x > 1) : 3
  nWin_y = keyword_set(nWin_y) ? (nWin_y > 1) : 3
  gradientThresholdMag = keyword_set(gradientThresholdMag) ? gradientThresholdMag : 0

    ; Set the number of histogram bins
    ; 4 bins => 45 degrees each, 9 bins => 20 degrees each
  B = keyword_set(nBins) ? nBins > 4 : 9
  imageSize = size(image, /DIMENSIONS)
  if (imageSize[0] eq 0) then return, 0
  h = fltArr(nwin_x * nwin_y * B); column vector with zeros
  m = sqrt(imageSize[1]/2)
  im     = double(image)
  step_x = floor(imageSize[0] / nwin_x)
  step_y = floor(imageSize[1] / nwin_y)

  gradientKernel = [-1, 0, 1]
  grad_xr  = convol(im, gradientKernel, /edge_truncate)
  grad_x = abs(grad_xr) ; TODO Victor / JJ be smart about this!!!
  grad_yu = convol(im, transpose(gradientKernel), /edge_truncate)
  angles  = atan(grad_yu, grad_x) ; IDL atan function requires two input arguments to ensure that the result lies in [-pi, pi].
  magnit  = sqrt((grad_yu^2) + (grad_x^2)) ; TODO handle angle normalization for input range
  angle_min = n_elements(angle_min) gt 0 ? angle_min : 0.0
  angle_max = n_elements(angle_max) gt 0 ? angle_max : !pi
  angle_inc = 1.0 * (angle_max - angle_min) / B
  if (angle_min gt angle_max) then angle_inc = -angle_inc
  fSaveCorrespondendes = 0b
  if arg_present(hog_correspondences) or keyword_set(hog_correspondences) then begin
    fSaveCorrespondendes = 1b
    hog_correspondences = uintArr(imageSize)
  endif
  fMakeVectors = 0b
  if arg_present(vec_x) or keyword_set(vec_x) then begin
    fMakeVectors = 1b
    vec_x = fltArr(imageSize)
    vec_y = fltArr(imageSize)
  endif
  hog_angles = (fIndGen(nBins) * angle_inc) + angle_min + angle_inc/2

  cont = 0
  for n = 0, nWin_y-1 do begin
    for m = 0, nWin_x-1 do begin
      cont += 1
      v_angles = angles[m*step_x : (m+1)*step_x -1, n*step_y : (n+1)*step_y -1] 
      v_magnit = magnit[m*step_x : (m+1)*step_x -1, n*step_y : (n+1)*step_y -1]
      bin = 0u
      h2  = fltArr(B)
      for ang_lim = angle_min, angle_max, angle_inc do begin
        if (fMakeVectors ne 0) then begin
          vx = cos(ang_lim + angle_inc/2.0)
          vy = sin(ang_lim + angle_inc/2.0)
        endif
        for k = 0, n_elements(v_angles)-1 do begin
          if (v_angles[k] le ang_lim+angle_inc) then begin
            v_angles[k] = 100
            if (fSaveCorrespondendes ne 0) then hog_correspondences[m*step_x + k] = bin
            if (fMakeVectors ne 0) and (v_magnit[k] gt gradientThresholdMag) then begin
              vec_x[m*step_x + k] = vx
              vec_y[m*step_x + k] = vy
            endif
            h2[bin] += v_magnit[k]
          endif
        endfor
        bin += 1
      endfor
      sum_h2 = total(h2)
      h[(cont-1)*B : cont*B-1] = h2 / ((sum_h2 gt 0) ? sum_h2 : 1.0)  
    endfor
  endfor
  return, h
end


pro s_hogPlot, image, hog, hog_angles, hog_correspondences = hog_correspondences, $
               winTitle = winTitle, winId = winId, xSz = xSz, ySz = ySz, xPos = xPos, sizeFactor = sizeFactor

  nCorresp = n_elements(hog_correspondences)
  if ((n_elements(image) eq 0) and (nCorresp eq 0)) then return

  nBins = n_elements(hog)
  if (nBins eq 0) and (nCorresp eq 0) then return

  if n_elements(hog_correspondences) eq 0 then begin
    imgSize = size(image, /DIMENSIONS)
    xSz = imgSize[0]
    ySz = imgSize[1]
    freqMax = max(hog)
    topPercent = keyword_set(topPercent) ? topPercent : 0
    angle_min = min(hog_angles, max = angle_max)
    angle_inc = 1.0 * (angle_max - angle_min) / nBins
    angle_lim = angle_min
    print, 'nothing to do yet'
    stop
    for i = 0, nBins-1 do begin
      ;whAngle = where(angles 
      angle_lim += angle_inc
    endfor
    imDisp = image
  endif else begin
    imDisp = hog_correspondences
    imgSize = size(hog_correspondences, /DIMENSIONS)
    xSz = imgSize[0]
    ySz = imgSize[1]
  endelse

  if keyword_set(xSz) then begin
    ySz = keyword_set(ySz) ? ySz : imgSize[1]
    imDisp = congrid(imDisp, xSz, ySz)
  endif
  if keyword_set(sizeFactor) then begin
    xSz = imgSize[0] * sizeFactor
    ySz = imgSize[1] * sizeFactor
    imDisp = congrid(imDisp, xSz, ySz)
  endif

  if ~keyword_set(winTitle) then winTitle = 'Util plot'
  if (n_elements(winId) gt 0) $
  then window, winId, xSize = xSz, ySize = ySz, title = winTitle, xPos = xPos $
  else window, xSize = xSz, ySize = ySz, title = winTitle, xPos = xPos

  tvscl, imDisp 
end


; TODO JJ, VC 2014 old code under scrutiny ;)
; hog_sector
;
; Purpose:
;   Determines which sector a given angle falls into.  Example of angle 
;   grouping (in degrees):
;   -180   - -157.5 : sector 0
;   -157.5 - -112.5 : sector 1
;   -112.5 -  -67.5 : sector 2
;    -67.5 -  -22.5 : sector 3
;    -22.5 -   22.5 : sector 0
;     22.5 -   67.5 : sector 1
;     67.5 -  112.5 : sector 2
;    112.5 -  157.5 : sector 3
;    157.5 -  180   : sector 0
;
; Parameters:
;   THETA - an array of angles (-pi : pi)
;
; Keywords:
;   BINANGLEDEG - not fully implemented yet.
;   NBINS       - number of bins to classify the angles. Default is 4.
;
function hog_sector, theta, binAngle = binAngleDeg, nBins = nBins

  binAngle = n_elements(binAngleDeg) eq 0 ? !dtor * 45.0/2.0 $
                                          : !dtor * binAngleDeg
  if keyword_set(nBins) then begin 
    binAngle    = !pi / (nBins*2)
    binAngleDeg = binAngle / !dtor
  endif

  iLimit = (360.0/binAngleDeg) - 1
  sector = make_array(size=size(theta), type=1)
  hist   = histogram(theta, min=-!pi, max=!pi, binsize=binAngle, reverse_indices=rev)

  for i = 0, iLimit do $
    if (hist[i] ne 0) then sector[rev[rev[i]:rev[i+1]-1]] = (i+1)/2 mod (180.0/(binAngleDeg*2))

  return, sector

end


function hog_onlySector, image, cellSize, fNoSmooth = fNoSmooth, nBins = nBins, gx = gx, gy = gy, mag = mag

  if ~keyword_set(nBins) then nBins = 4
  sz = size(image,/dimensions)
  gxKern = [[-1, 0, 1],[-2, 0, 2],[-1, 0, 1]]
  gyKern = [[-1,-2,-1],[ 0, 0, 0],[ 1, 2, 1]]
  gx = convol(image, double(gxKern), total(gxKern))
  gy = convol(image, double(gyKern), total(gyKern))

  minSizeCell = cellSize*cellSize

  oFilter = obj_new('C_sImageFilter_GaussianKernel')
  pParamStruct = oFilter->getpParamStruct()
  whParam = (where( *(*pParamStruct).pNames eq 'Gradient_Kernel'))[0]
  (*(*pParamStruct).pActive )[whParam] = 1
  (*(*pParamStruct).pValues )[whParam] = 1

  whParam = (where( *(*pParamStruct).pNames eq 'Filter_Size'))[0]
  (*(*pParamStruct).pValues )[whParam] = cellSize

  oFilter->set, pParamStruct = pParamStruct
  gy = oFilter->apply(image = image, stack_tlb = 0,$
                                     selectedStackObject = 0 ,$
                                     tPos = 0 ,$
                                     chPos = 0 ,$
                                     zPos = 0 ,$
                                     clusPos = 0,$
                                     segPos = 0)

   whParam = (where( *(*pParamStruct).pNames eq 'Rotation_Theta'))[0]
   (*(*pParamStruct).pActive )[whParam] = 1
   (*(*pParamStruct).pValues )[whParam] = 90

   oFilter->set, pParamStruct = pParamStruct
   gx = oFilter->apply(image = image,$
                       stack_tlb = 0,$
                       selectedStackObject = 0,$
                       zPos = 0,$
                       tPos = 0,$
                       chPos = 0,$
                       segPos = 0,$
                       clusPos = 0)

  ; step 3 : find edge direction and magnitude
  mag = sqrt(gx^2 + gy^2)

  ; zero the 3-pixel edges left over from boundary conditions from
  ; the smoothing and the convolution

  ; left
  mag[0:2, 0:sz[1]-1] = 0
  ; right
  mag[sz[0]-3:sz[0]-1, 0:sz[1]-1] = 0
  ; bottom
  mag[0:sz[0]-1, 0:2] = 0
  ; top
  mag[0:sz[0]-1, sz[1]-3:sz[1]-1] = 0
  ; calculate angles of gradient
  theta = atan(gy, gx)
  ; get sector of angle : 0-3
  xAngle = 20
  sector = hog_sector(theta, nBins = nBins)
  ;stop
  return, sector
end


function hog, image, cellSize, nBins = nBins, gx = gx, gy = gy, fPlot = fPlot, fNoSmooth = fNoSmooth

  sector2 = hog_OnlySector(image, cellSize, nBins = nBins, gx = gx, gy = gy, mag = magG, fNoSmooth = fNoSmooth)
  sector  = epGVF_getHandG(image, g, h)
  minSizeCell = cellSize * cellSize
  dimSector = size(sector, /DIMENSIONS)
  oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [dimSector, 1], ZSliceInterval = 1)
  labels = uLonArr(dimSector)
  roiIndex = 1ul

  for i = 0ul, nBins do begin
     mask = sector eq i
     labels_tmp = label_region(sector*mask, /ALL_NEIGHBORS, /ULONG)
     max_label  = max(labels_tmp)
     for j = 1ul, max_label do begin
        dummyMask = sector * 0
        ;labels     = labels + labels_tmp + mask * roiIndex
        whLabel = where(labels_tmp eq j, countI)
        dummyMask[whLabel] = roiIndex
        labels     += dummyMask 
        oROI2DGroup->addROIObjectsFromSegImage, maskImage = dummyMask, intImage = magG, objNumberVector = roiIndex
       ; oROI2DGroup->addROIObjectsFromSegImage, maskImage = dummyMask, intImage = sector, objNumberVector = roiIndex
        roiIndex += 1
     endfor
  endfor

  counter = oROI2DGroup->getObjectNumberVector()

  if keyword_set(fPlot) then begin
    ;stop
    xSz = 700
    ySz = 700
    window, 29, xsize = xSz, ysize = ySz
    tvscl, congrid(labels, xsz, ysz), channel = 3   
  endif
  
  ; What the HELLL... DEPRECATED apop function... 2Dmoments ... killed all my night...
  paramMoments  = obj_new('C_sROIParam_ObjMomentsOfMorphology')
  paramMoments->apply, C_sROIGroupObj = oROI2DGroup 
  pParamStruct  = paramMoments->getpParamStruct()
  whParam       = (where( *(*pParamStruct).pNames eq 'Excentricity [µ]'))[0]
  pROIParamVect = paramMoments->getpValueStruct(POSITION = whParam[0])
  paramRequired = *(*pROIParamVect).pROIParamVect

  centerIm = sector * 0
  minSize  = 3; min number of points to include...

  for i=0u, n_elements(counter)-1 do begin
     oROI = oROI2DGroup->get(position = i)
     dummyCenter = (oROI->getCenterXYZ())[0:1]
     dummyValMean = mean(*(oROI->getpPointValues()))
     ;centerIm[dummyCenter[0],dummyCenter[1]] = i+1

     whereP = where(labels eq counter[i], count)
     if(count lt minSize) then paramRequired[i] = 1.0

     if (dummyValMean gt 5) then centerIm[dummyCenter[0], dummyCenter[1]] = i+1
     ; FASL..Trouble with rois with only one point
     ; Last commentary is a lie... deprecated function provided my bad data... :S
  endfor

  ; we have now the values for eccentricity for each HOG-ROI at paramRequired...
  ; now???? can we use a threshold for eccentricity 

  if keyword_set(fPlot) then begin
    window, 30, xsize = xSz, ysize = ySz
    tvscl, congrid(image, xsz, ysz), channel = 2
    tvscl, congrid(centerIm, xsz, ysz), channel = 3  
   endif

;  OrientationBin = make_array()
;  
;  if ~keyword_set(fNoSmooth) then begin
;    
;  endif
  return, 1.0 * n_elements(centerIm)/(xSz*ySz/minSizeCell)
end


pro test_hog_OnlySector, cellsize = cellsize, nBins = nBins

  if ~keyword_set(cellsize) then cellsize = 3

  image = [[0, 1, 1, 0, 0, 0, 0, 0, 0, 0], $
           [0, 0, 1, 1, 0, 0, 0, 0, 0, 0], $
           [0, 0, 0, 1, 1, 0, 0, 0, 0, 0], $
           [0, 0, 0, 0, 1, 1, 0, 0, 0, 0], $
           [0, 0, 0, 0, 0, 1, 1, 0, 0, 0], $
           [0, 0, 0, 0, 0, 0, 1, 1, 0, 0], $
           [0, 0, 1, 0, 0, 0, 0, 1, 1, 0], $
           [0, 0, 1, 0, 0, 0, 0, 0, 1, 1], $
           [0, 0, 0, 1, 0, 0, 0, 0, 0, 0], $
           [0, 0, 1, 1, 0, 0, 0, 0, 0, 0] ]

  sector = hog_OnlySector(image, cellsize, fNoSmooth = fNoSmooth, nBins = nBins, gx = gx, gy = gy)

  xSz = 700
  ySz = 700

  window, 27, xsize = xSz, ysize = ySz
  tvscl, congrid(image, xsz, ysz), channel = 3

  window, 28, xsize = xSz, ysize = ySz
  tvscl, congrid(sector, xsz, ysz), channel = 3
end


pro test_hog, cellsize = cellsize, nBins = nBins

  if ~keyword_set(cellsize) then cellsize = 3

  image = [[0, 1, 1, 0, 0, 0, 0, 0, 0, 0], $
           [0, 0, 1, 1, 0, 0, 0, 0, 0, 0], $
           [0, 0, 0, 1, 1, 0, 0, 0, 0, 0], $
           [0, 0, 0, 0, 1, 1, 0, 0, 0, 0], $
           [0, 0, 0, 0, 0, 1, 1, 0, 0, 0], $
           [0, 0, 0, 0, 0, 0, 1, 1, 0, 0], $
           [0, 0, 1, 0, 0, 0, 0, 1, 1, 0], $
           [0, 0, 1, 0, 0, 0, 0, 0, 1, 1], $
           [0, 0, 0, 1, 0, 0, 0, 0, 0, 0], $
           [0, 0, 1, 1, 0, 0, 0, 0, 0, 0] ]
  out = hog(image, cellsize, fNoSmooth = fNoSmooth, nBins = nBins, gx = gx, gy = gy)
end
