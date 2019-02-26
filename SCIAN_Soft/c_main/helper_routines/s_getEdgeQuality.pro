;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_getEdgeQuality
;
; PURPOSE:
;       Calculates and returns an indicator or edge quality 
;       0 if there is no visible edge
;       1 if edge is strong and visible
;
; AUTHOR:
;   Dr. Victor Castañeda (2013)
;   e_mail: vcastane@gmail.com
;
; CALLING SEQUENCE:
;      value = s_getEdgeQuality(image,cellsize,binsize)
;
; REQUIRED INPUTS:
;      image:     Portion of image where the edge is evaluated. 
;      cellsize:  Size of agrupation of gradient
;      binsize:   Size of discretization in angles (10 degrees for example)
;
; REFERENCES:
;*  - Dalal, N., Triggs, B.: Histograms of Oriented Gradients for Human detection. In: CVPR 2005 (2005)
;*  - D. Lowe. Distinctive Image Features from Scale-Invariant Keypoints. IJCV, 60(2):91110, 2004.
;_____________________________IOISIOI____________________


pro test_get_edge
     image = [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
              [0, 0, 1, 0, 0, 0, 0, 0, 0, 0], $
              [0, 0, 0, 1, 0, 0, 0, 0, 0, 0], $
              [0, 0, 0, 0, 1, 0, 0, 0, 0, 0], $
              [0, 0, 0, 0, 0, 1, 0, 0, 0, 0], $
              [0, 0, 0, 0, 0, 0, 1, 0, 0, 0], $
              [0, 0, 0, 0, 0, 0, 0, 1, 0, 0], $
              [0, 0, 0, 0, 0, 0, 0, 0, 1, 0], $
              [0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
              [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ]
     print, s_getEdgeQuality(image, 3, nBins = 20)
end


pro test_get_edge_with_image, image, p1x, p1y, p2x, p2y

  imgSize = size(image, /DIMENSIONS)
  minImgSize = min(imgSize, max = maxImgSize)
  case 1 of
    minImgSize lt 100: factor = 5.0
    minImgSize lt 500: factor = 2.0
    else: factor = 1.0
  endcase
  maxWinSize = maxImgSize * factor
  if (maxWinSize ge 1600) then factor = 1600/maxImgSize

  maxx1 = max(p1x, min = minx1)
  maxx2 = max(p2x, min = minx2)
  maxy1 = max(p1y, min = miny1)
  maxy2 = max(p2y, min = miny2)

  rangeMin = (MINX1<MINX2) < (MINY1<MINY2)
  rangeMax = (MAXX1>MAXX2) > (MAXY1>MAXY2)
  window, 30, XSIZE=imgSize[0]*factor, YSIZE=imgSize[1]*factor
  minMod50 = rangeMin mod 50
  maxMod50 = rangeMax mod 50
  if (minMod50 gt 0) then rangeMin -= minMod50
  if (maxMod50 gt 0) then rangeMax += (50-maxMod50)
  print, rangemin, rangemax
  ;tvscl, congrid(image[rangeMin:rangeMax, rangeMin:rangeMax], imgSize[0]*factor, imgSize[1]*factor)
  ;plot, p1x, p1y, xrange=[rangeMin, rangeMax], yrange=[rangeMin, rangeMax], xmargin = [0,0], ymargin=[0,0], psym = 5, /NOERASE, color = '00FF00'x, THICK=1.0
  makePolygonsPlot, p1x, p1y, p2x, p2y, winTitle = 'HOG', winId = 30, backgroundImage = image, fCropImage = 1.0
  ;oplot, p2x, p2y, psym = 5, color = '00FFFF'x, THICK=1.0

  window, 31, XSIZE=imgSize[0]*factor, YSIZE=imgSize[1]*factor
  theOg = s_getEdgeQuality(image[rangeMin:rangeMax, rangeMin:rangeMax], 20, nBins = 20, centerIm=centerIm)
  ;velovect, theGy, theGx, length=5.0
  stop
end


function epGVF_getHandG, image, g, fx = fx, fy = fy, nBins = nBins, edgemapSigmaSq = edgemapSigmaSq

  edgemapSigmaSq = n_elements(edgemapSigmaSq) eq 0 ? 3.0 : edgemapSigmaSq
  eMap = imageEdgemapGaussian(image, p_ix, p_iy, sigmaSq = edgemapSigmaSq)

    ; Now compute final edgemap as the squared norm of the smoothed intensity gradient normalized to [0,1]
  f = double(*p_ix^2 + *p_iy^2)

    ; Compute the partial derivatives of the edge map (spatial step = 1)
  fx = convol(f, [-1, 0, 1], 2, /edge_truncate)
  fy = convol(f, transpose([-1, 0, 1]), 2, /edge_truncate)
  
  theta = atan(fx,fy)
  if ~keyword_set(nBins) then nBins = 10
  sector = og_sector(theta, nBins = nBins)
  g = sqrt(fx*fx+fy*fy)

  return, sector
end


function magnitudeGradient, image, cellSize, gx = gx, gy = gy

  if ~keyword_set(nBins) then nBins = 4
  sz = size(image,/dimensions)
  gxKern = [[-1, 0, 1],[-2, 0, 2],[-1, 0, 1]]
  gyKern = [[-1,-2,-1],[ 0, 0, 0],[ 1, 2, 1]]
  gx = convol(image, double(gxKern), total(gxKern))
  gy = convol(image, double(gyKern), total(gyKern))
  
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
   gx = oFilter->apply(image = image, stack_tlb = 0,$
                                     selectedStackObject = 0 ,$
                                     tPos = 0 ,$
                                     chPos = 0 ,$
                                     zPos = 0 ,$
                                     clusPos = 0,$
                                     segPos = 0)
  obj_destroy, oFilter
  ; step 3 : find edge direction and magnitude
  mag = sqrt(gx^2 + gy^2)

;  ; zero the 3-pixel edges left over from boundary conditions from
;  ; the smoothing and the convolution
;
;  ; left
;  mag[0:2, 0:sz[1]-1] = 0
;  ; right
;  mag[sz[0]-3:sz[0]-1, 0:sz[1]-1] = 0
;  ; bottom
;  mag[0:sz[0]-1, 0:2] = 0
;  ; top
;  mag[0:sz[0]-1, sz[1]-3:sz[1]-1] = 0
  
  return, mag
 end
 

; s_getEdgeQuality
; TODO JJ, JuanEdo, VC
; - Gradientes se calculan de forma distinta (filtro con rotación, máscara, máscara con normalización)
; - Se hace cálculo de MoMs, pero no se utilizan (excentricidad)
; - Valor umbral fijo para seleccionar ROIs cuyos gradientes orientados se consideran significativos (dummyValMean = mean(*(oROI->getpPointValues())))
; - cellSize es más bien gradientMaskSize
; - cuadrantes están en 0-180... ¿por qué?
function s_getEdgeQuality_2, image, cellSize, nBins = nBins, centerIm = centerIm, edgemapSigmaSq = edgemapSigmaSq, fPlot = fPlot

  magG = magnitudeGradient(image, cellSize)
  sector = epGVF_getHandG(image, g, fx = fx, fy = fy, nBins = nBins, edgemapSigmaSq = edgemapSigmaSq)
  minSizeCell = cellSize*cellSize

  dimSector = size(sector, /DIMENSIONS)
  oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [dimSector, 1], ZSliceInterval = 1)

  labels = uLonArr(dimSector)
  idx_Region = 1ul
  centerIm = sector * 0
  for i = 0ul, nBins do begin
     mask = sector eq i
     if i eq 0 then begin
        iplus = i+1
        iminus = nBins -1
     endif else begin
        if i eq nBins-1 then begin
           iplus = 0
           iminus = i-1
        endif else begin
           iplus = i+1
           iminus = i-1
        endelse
     endelse
     mask_plus = sector eq iplus
     mask_minus = sector eq iminus
     wimage = mask*magG
     wimage_plus = mask_plus*magG
     wimage_minus = mask_minus*magG
     
     wimage = wimage+(nBins-3.0)/nBins*wimage_plus+(nBins-3.0)/nBins*wimage_minus
     
     kernel =  [ [1,1,1],[1,1,1],[1,1,1] ]
     centerIm = centerIm + mask*CONVOL(wimage, kernel,1/9.0)
  endfor
 if keyword_set(fPlot) then begin
    ;stop
    xSz = 700
    ySz = 700
    window, 29, xsize = xSz, ysize = ySz
    tvscl, congrid(centerIm, xsz, ysz), channel = 3
    window, 31, xsize = xSz, ysize = ySz   
    tvscl, congrid(sector, xsz, ysz), channel = 3
  endif
count = 0
  return, 1.0 * count
end


function s_getEdgeQuality, image, cellSize, nBins = nBins, centerIm = centerIm, edgemapSigmaSq = edgemapSigmaSq, fPlot = fPlot

  magG = magnitudeGradient(image, cellSize)
  sector = epGVF_getHandG(image, g, fx = fx, fy = fy, nBins = nBins, edgemapSigmaSq = edgemapSigmaSq)
  minSizeCell = cellSize*cellSize

  dimSector = size(sector, /DIMENSIONS)
  oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [dimSector, 1], ZSliceInterval = 1)

  labels = uLonArr(dimSector)
  idx_Region = 1ul
  for i = 0ul, nBins do begin
     mask = sector eq i
     labels_tmp = label_region(sector*mask, /ALL_NEIGHBORS, /ULONG)
     max_label  = max(labels_tmp)
     for j = 1ul, max_label do begin
        dummyMask = sector * 0
        ;labels     += (labels_tmp + mask * idx_Region)
        whLabel = where(labels_tmp eq j, countI)
        dummyMask[whLabel] = idx_Region
        labels     += dummyMask 
        oROI2DGroup->addROIObjectsFromSegImage, maskImage = dummyMask, intImage = magG, objNumberVector = idx_Region
        idx_Region += 1
     endfor
  endfor

  labelVector = oROI2DGroup->getObjectNumberVector()

  if keyword_set(fPlot) then begin
    ;stop
    xSz = 700
    ySz = 700
    window, 29, xsize = xSz, ysize = ySz
    tvscl, congrid(labels, xsz, ysz), channel = 3
    window, 31, xsize = xSz, ysize = ySz   
    tvscl, congrid(sector, xsz, ysz), channel = 3
  endif

  ; Counting of oriented gradients (observing orientation tendency)
  paramMoments = obj_new('C_sROIParam_ObjMomentsOfMorphology')
  paramMoments->apply, C_sROIGroupObj = oROI2DGroup
  pParamStruct = paramMoments->getpParamStruct()
  whParam = (where( *(*pParamStruct).pNames eq 'Excentricity [µ]'))[0]

  pROIParamVect   = paramMoments->getpValueStruct(POSITION = whParam[0])
  paramRequired = *(*pROIParamVect).pROIParamVect 

  centerIm = sector * 0
  minSize  = 3; min number of points to include...
  for i=0, n_elements(labelVector)-1 do begin
     oROI = oROI2DGroup->get(position = i)
     dummyCenter = (oROI->getCenterXYZ())[0:1]
     dummyValMean = mean(*(oROI->getpPointValues()))
     whereP = where(labels eq labelVector[i], count)
     if(count lt minSize) then paramRequired[i] = 1.0

     if( (dummyValMean gt 5) ) then centerIm[dummyCenter[0],dummyCenter[1]] = i+1
  endfor
 
  ; we have now the values for excentricity for each HOG-ROI at paramRequired...
  ; now???? can we use a threshold for excentricity 

  if keyword_set(fPlot) then begin
    window, 30, xsize = xSz, ysize = ySz
    tvscl, congrid(image, xsz, ysz), channel = 2
    tvscl, congrid(centerIm, xsz, ysz), channel = 3  
   endif
  stop
  whUseful = where(centerIm ne 0, count)
  return, 1.0 * count
end

function og_sector, theta, binAngle = binAngleDeg, nBins = nBins

  binAngle = n_elements(binAngleDeg) eq 0 ? !dtor * 45.0/2.0 $
                                          : !dtor * binAngleDeg
  if keyword_set(nBins) then begin 
    binAngle    = !pi / (nBins*2)
    binAngleDeg = binAngle / !dtor
  endif

  iLimit = (360.0/binAngleDeg) - 1
  sector = bytArr(size(theta, /dimensions))
  hist   = histogram(theta, min=-!pi, max=!pi, binsize=binAngle, reverse_indices=rev)

  for i = 0,iLimit do $
    if (hist[i] ne 0) then sector[rev[rev[i]:rev[i+1]-1]] = (i+1)/2 mod (180.0/(binAngleDeg*2))

  return, sector

end

