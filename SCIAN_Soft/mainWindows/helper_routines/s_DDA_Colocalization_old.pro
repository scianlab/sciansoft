function s_DDA_Colocalization_old, imageCh1 = imageCh1, imageCh2 = imageCh2, imageSeg1 = imageSeg1, imageSeg2 = imageSeg2, imageConf = imageConf, DDARad = DDARad, DDAPlus = DDAPlus

   device, decomposed = 1
   dimI = size(imageCh1, /dim)

      ; set colour table
   tvlct, r_old, g_old, b_old, /get
   r = make_array(256, /byte)
   g = make_array(256, /byte)
   b = make_array(256, /byte)
   r[1:4] = [150,255,0,  255]
   g[1:4] = [150,0,  255,255]
   b[1:4] = [150,0,  0,  0]
   tvlct, r, g, b

      ; define confined compartments
   whereROIs = where(imageConf gt 0)
   whereYROIs = where(rotate(imageConf,1) gt 0)

      ; define confined compartments
   oldImage = imageCh2
   shiftIxy = bytArr(dimI[0], dimI[1])
   oldMask = imageSeg2
   shiftMxy = bytArr(dimI[0], dimI[1])

      ; define k-,l-,dist-, & value- vectors
   correlDim = 0
   kVect = [0]
   lVect = [0]
   distVect = [0]
   for k = -DDARad,DDARad do for l = -DDARad,DDARad do begin
      if ((1.*(k*k+l*l)) le DDARad^2) then begin
         correlDim += 1
         kVect = [kVect, k]
         lVect = [lVect, l]
         distVect = [distVect, sqrt(1.*(k*k+l*l))]
      endif
   endfor
   kVect = kVect[1:*]
   lVect = lVect[1:*]
   distVect = distVect[1:*]
   DDAVals = fltArr(correlDim > 1, 2)

      ; sort vectors
   sortDV = sort(distVect)
   distVect = distVect[sortDV]
   kVect = kVect[sortDV]
   lVect = lVect[sortDV]

   bottom = 140
   border = 10

   window, 10, xsize = dimI[0] * 3 + 4 * border, ysize = dimI[1] * 2 + 3 * border
   rgbImage = bytarr(3, dimI[0] * 3 + 4 * border, dimI[1] * 2 + 3 * border)
   rgbImage[0,*,*] = 255
   rgbImage[1,*,*] = 175
   rgbImage[2,*,*] = 75
   tv, rgbImage, true = 1

   rgbImage = make_array(3, dimI[0], dimI[1])
   rgbImage[0,*,*] = (make_array(256, /index))[imageCh1]
   tv, rgbImage, border,  dimI[1] + 2 * border, true = 1
   rgbImage[0,*,*] = 0
   rgbImage[1,*,*] = (make_array(256, /index))[imageCh2]
   tv, rgbImage, dimI[0] + 2 * border, dimI[1] + 2 * border, true = 1
   rgbImage[0,*,*] = (make_array(256, /index))[imageCh1]
   tv, rgbImage, dimI[0] * 2 + 3 * border,  dimI[1] + 2 * border, true = 1
;   mpegID = MPEG_OPEN([dimI[0]*5, dimI[1]])
   for i = 0, correlDim-1 do begin

      shiftIxy[whereROIs] = shift(oldImage[whereROIs], kVect[i])
      shiftIxy = rotate(shiftIxy, 1)
      shiftIxy[whereYROIs] = shift(shiftIxy[whereYROIs], lVect[i])
      shiftIxy = rotate(shiftIxy, 3)

      dummy = oldImage
      dummy[whereROIs] = shiftIxy[whereROIs]
      shiftImage = dummy

      shiftMxy[whereROIs] = shift(oldMask[whereROIs], kVect[i])
      shiftMxy = rotate(shiftMxy, 1)
      shiftMxy[whereYROIs] = shift(shiftMxy[whereYROIs], lVect[i])
      shiftMxy = rotate(shiftMxy, 3)

      dummy = oldMask
      dummy[whereROIs] = shiftMxy[whereROIs]
      shiftMask = dummy

         ; calculate Manders Coefficients
         ; get channel-values
      ch1_vect = make_array(dimI[0] * dimI[1], /long) + imageCh1[*]
      ch2_vect = make_array(dimI[0] * dimI[1], /long) + shiftImage[*]

         ; make ROI
      ROIArray = make_array(dimI[0], dimI[1], /byte) + imageConf
      whereChannel_1 = where(imageSeg1)
      whereChannel_2 = where(shiftMask)
      ROIArray[whereChannel_1] = 2
      ROIArray[whereChannel_2] = 3
      whereBoth = where((shiftMask+imageSeg1) eq 2)
      ROIArray[whereBoth] = 4

      if ((whereChannel_1[0] ne -1) and (whereBoth[0] ne -1)) then DDAVals[i,0] = total(ch1_vect[whereBoth]) / total(ch1_vect[whereChannel_1]) $
         else DDAVals[i,0] = 0.
      if ((whereChannel_2[0] ne -1) and (whereBoth[0] ne -1)) then DDAVals[i,1] = total(ch2_vect[whereBoth]) / total(ch2_vect[whereChannel_2]) $
         else DDAVals[i,1] = 0.

      device, decomposed = 0
      if (i eq 0) then tv, ROIArray, border,  border else tv, ROIArray, dimI[0] + 2 * border,  border
      device, decomposed = 1

      rgbImage[0,*,*] = (make_array(256, /index))[imageCh1]
      rgbImage[1,*,*] = (make_array(256, /index))[shiftImage]
      tv, rgbImage, dimI[0] * 2 + 3 * border, border, true = 1
;      outimage = tvrd(true = 1)
;      write_tiff, strCompress('C:\Users\steffen\Desktop\projectos_rodrigo_SCIAN\DDA\movie'+string(i)+'.tif', /rem), outimage
;      mpeg_put, mpegID, image = outimage, frame = i
   endfor
;   mpeg_save, mpegID, filename = 'C:\Users\steffen\Desktop\projectos_rodrigo_SCIAN\DDA\outMovie.mpg'
;   mpeg_close, mpegID

   outimage = tvrd(true = 1)
   dimO = size(outimage, /dim)
   sclale550 = 550./dimO[1]
   newDim = [floor(dimO[1]*sclale550), floor(dimO[2]*sclale550)]

   rgbImage = make_array(3, newDim[0], newDim[1] + bottom)
   rgbImage[0,*,*] = 255
   rgbImage[1,*,*] = 175
   rgbImage[2,*,*] = 75

   outimage = congrid(outimage, 3, newDim[0], newDim[1], /center, /interp)
   window, 10, xsize = newDim[0], ysize = newDim[1] + bottom
   tv, rgbImage, true = 1
   tv, outimage, true = 1, 0, bottom

   whereRandom = where(distVect ge DDAPlus, nRandom)
   if (whereRandom[0] ne -1) then begin
      M1Random = moment(DDAVals[whereRandom,0])
      M2Random = moment(DDAVals[whereRandom,1])
   endif

   device, decomposed = 0
   xyOuts, 15, 120, 'Manders Coefficient M1:', charSize = 1, color = 0, /device
   xyOuts, 15, 100, 'M1[d=0] = ' + strCompress(string(DDAVals[0,0]),/rem), charsize = 1, color = 0, /device
   xyOuts, 15, 80, 'Mean Random M1 +- SD:', charSize = 1, color = 0, /device
   xyOuts, 15, 60, 'M1[' + strCompress(string(DDAPlus),/rem) + '-' + strCompress(string(DDARad),/rem) + '] = ' $
      + strCompress(string(M1Random[0]),/rem) + ' +- '  + strCompress(string(sqrt(M1Random[1])),/rem), charSize = 1, color = 0, /device

   xyOuts, 300, 120, 'Manders Coefficient M2:', charSize = 1, color = 0, /device
   xyOuts, 300, 100, 'M2[d=0] = ' + strCompress(string(DDAVals[0,1]),/rem), charsize = 1, color = 0, /device
   xyOuts, 300, 80, 'Mean Random M2 +- SD:', charSize = 1, color = 0, /device
   xyOuts, 300, 60, 'M2[' + strCompress(string(DDAPlus),/rem) + '-' + strCompress(string(DDARad),/rem) + '] = ' $
      + strCompress(string(M2Random[0]),/rem) + ' +- '  + strCompress(string(sqrt(M2Random[1])),/rem), charSize = 1, color = 0, /device
   device, decomposed = 1

   rgbImage = congrid(rgbImage, 3, 250, 250, /center, /interp)
   rgbImage[1,*,*] = 200
   rgbImage[2,*,*] = 100
   tvlct, r_old, g_old, b_old
   window, 11, xsize = 250, ysize = 250
   tv, rgbImage, true = 1
   plot, distVect, DDAVals[*,0], psym = 1, symsize = 0.7, xtitle = 'radial distance d [pixel]', ytitle = 'M1', xMargin = [6,2], xRange = [-0.5, DDARad + 0.5], yRange = [0, 1.05], xStyle = 1, yStyle = 1, color = 0, /noerase
   outM1DDA = tvrd(true = 1)
   wDelete, 11

   window, 12, xsize = 250, ysize = 250
   tv, rgbImage, true = 1
   plot, distVect, DDAVals[*,1], psym = 1, symsize = 0.7, xtitle = 'radial distance d [pixel]', ytitle = 'M2', xMargin = [6,2], xRange = [-0.5, DDARad + 0.5], yRange = [0, 1.05], xStyle = 1, yStyle = 1, color = 0, /noerase
   outM2DDA = tvrd(true = 1)
   wDelete, 12

   window, 13, xsize = 250, ysize = 250
   tv, rgbImage, true = 1
   minM1R = min(DDAVals[whereRandom,0], max = maxM1R)
   nbins = 20
   histM1 = histogram(DDAVals[whereRandom,0], min = minM1R, max = maxM1R, nbins = nbins)
   sortDDA = (DDAVals[whereRandom,0])[sort(DDAVals[whereRandom,0])]
   histM1 /= total(histM1)
   histM1 = [0, histM1,0]
   xAxis = make_array(nbins, /ind) / (nbins-1.) * (maxM1R - minM1R) + minM1R
   xAxis = [xAxis[0] - (xAxis[1]-xAxis[0]), xAxis, xAxis[nbins-1] + (xAxis[1]-xAxis[0])]
   xRange = [(minM1R < DDAVals[0,0] - 0.07), (maxM1R > DDAVals[0,0]) + 0.07]
   plot, xAxis, histM1, psym = 10, xtitle = 'M1', ytitle = 'PDF', xMargin = [8,2], xRange = xRange, yRange = [0, max(histM1) + max(histM1) * 0.2], xStyle = 1, yStyle = 1, color = 0, /noerase
   oPlot, [DDAVals[0,0],DDAVals[0,0]], [max(histM1) * 1.05, 0], color = [255]
   oPlot, [DDAVals[0,0],-1], [max(histM1) * 1.05, 0], pSym = 6, color = [255], symsize = 1.5

   pCut = floor(nRandom * 0.05)
   oPlot, [sortDDA[pCut],sortDDA[pCut]], [max(histM1) * 0.6, 0], color = [255]
   oPlot, [sortDDA[pCut],-1], [max(histM1) * 0.6, 0], pSym = 6, color = [255], symsize = 1
   oPlot, [sortDDA[nRandom-pCut],sortDDA[nRandom-pCut]], [max(histM1) * 0.6, 0], color = [255]
   oPlot, [sortDDA[nRandom-pCut],-1], [max(histM1) * 0.6, 0], pSym = 6, color = [255], symsize = 1
   outM1pCuts = [sortDDA[pCut], sortDDA[nRandom-pCut]]
   outM1PDF = tvrd(true = 1)
   wDelete, 13

   window, 14, xsize = 250, ysize = 250
   tv, rgbImage, true = 1
   minM2R = min(DDAVals[whereRandom,1], max = maxM2R)
   nbins = 20
   histM2 = histogram(DDAVals[whereRandom,1], min = minM2R, max = maxM2R, nbins = nbins)
   sortDDA = (DDAVals[whereRandom,1])[sort(DDAVals[whereRandom,1])]
   histM2 /= total(histM2)
   histM2 = [0, histM2,0]
   xAxis = make_array(nbins, /ind) / (nbins-1.) * (maxM2R - minM2R) + minM2R
   xAxis = [xAxis[0] - (xAxis[1]-xAxis[0]), xAxis, xAxis[nbins-1] + (xAxis[1]-xAxis[0])]
   xRange = [(minM2R < DDAVals[0,1] - 0.07), (maxM2R > DDAVals[0,1]) + 0.07]
   plot, xAxis, histM2, psym = 10, xtitle = 'M2', ytitle = 'PDF', xMargin = [8,2], xRange = xRange, yRange = [0, max(histM2) + max(histM2) * 0.2], xStyle = 1, yStyle = 1, color = 0, /noerase
   oPlot, [DDAVals[0,1],DDAVals[0,1]], [max(histM2) * 1.05, 0], color = [0]
   oPlot, [DDAVals[0,1],-1], [max(histM2) * 1.05, 0], pSym = 5, color = [0], symsize = 1.5

   pCut = floor(nRandom * 0.05)
   oPlot, [sortDDA[pCut],sortDDA[pCut]], [max(histM2) * 0.6, 0], color = [0]
   oPlot, [sortDDA[pCut],-1], [max(histM2) * 0.6, 0], pSym = 5, color = [0], symsize = 1
   oPlot, [sortDDA[nRandom-pCut],sortDDA[nRandom-pCut]], [max(histM2) * 0.6, 0], color = [0]
   oPlot, [sortDDA[nRandom-pCut],-1], [max(histM2) * 0.6, 0], pSym = 5, color = [0], symsize = 1
   outM2pCuts = [sortDDA[pCut], sortDDA[nRandom-pCut]]
   outM2PDF = tvrd(true = 1)
   wDelete, 14

   window, 15, xsize = 250, ysize = 250
   tv, rgbImage, true = 1
   colocAxis = make_array(101, /ind)
   M1eff = colocAxis * ((.99 - M1Random[0]) / 100.)
   M2eff = colocAxis * ((.99 - M2Random[0]) / 100.)
   plot, colocAxis, M2eff, xtitle = 'colocalization [%]', ytitle = 'Meff = M[0]-M[' + strCompress(string(DDAPlus),/rem) + '-' + strCompress(string(DDARad),/rem) + ']', $
         xMargin = [8,2], xRange = [-5,105], yRange = [0, 1], xStyle = 1, yStyle = 1, color = 0, /noerase
   oPlot, colocAxis, M1eff, color = 255
   c1 = (DDAVals[0,0] - M1Random[0]) / ((1. - M1Random[0]) / 100.)
   c2 = (DDAVals[0,1] - M2Random[0]) / ((1. - M2Random[0]) / 100.)
   oPlot, [c1, c1], [DDAVals[0,0] - M1Random[0], DDAVals[0,0] - M1Random[0]], pSym = 6, color = 255, symsize = 1
   oPlot, [c2, c2], [DDAVals[0,1] - M2Random[0], DDAVals[0,1] - M2Random[0]], pSym = 5, color = 0, symsize = 1
   outMLin = tvrd(true = 1)
   wDelete, 15

   wSet, 10
   xyOuts, 15, 40, 'p < 0.05 values: ' + strCompress(string(outM1pCuts[0]),/rem) + ' | ' + strCompress(string(outM1pCuts[1]),/rem), charSize = 1, color = 0, /device
   xyOuts, 300, 40, 'p < 0.05 values: ' + strCompress(string(outM1pCuts[0]),/rem) + ' | ' + strCompress(string(outM1pCuts[1]),/rem), charSize = 1, color = 0, /device
   if (DDAVals[0,0] le outM1pCuts[0]) or (DDAVals[0,0] ge outM1pCuts[1]) then $
      xyOuts, 15, 20, 'M1[d=0] - value is significant !', charSize = 1, color = 0, charThick = 1.5, /device else $
      xyOuts, 15, 20, 'M1[d=0] - value is NOT significant !', charSize = 1, color = 0, charThick = 1.5, /device

   if (DDAVals[0,1] le outM2pCuts[0]) or (DDAVals[0,1] ge outM2pCuts[1]) then $
      xyOuts, 300, 20, 'M2[d=0] - value is significant !', charSize = 1, color = 0, charThick = 1.5, /device else $
      xyOuts, 300, 20, 'M2[d=0] - value is NOT significant !', charSize = 1, color = 0, charThick = 1.5, /device
   outimage = tvrd(true = 1)

   rgbImage = bytarr(3, newDim[0], newDim[1] + bottom + 760)
   rgbImage[0,*,*] = 255
   rgbImage[1,*,*] = 175
   rgbImage[2,*,*] = 75
   window, 10, xsize = newDim[0], ysize = newDim[1] + bottom + 760
   tv, rgbImage, true = 1
   tv, outimage, 0, 760, true = 1
   tv, outM1PDF, 10, 270, true = 1
   tv, outM2PDF, 280, 270, true = 1
   tv, outM1DDA, 10, 510, true = 1
   tv, outM2DDA, 280, 510, true = 1
   tv, outMLin, 10, 10, true = 1

   xyOuts, 300, 230, 'Colocalization [%]:', charSize = 1, color = 0, /device
   xyOuts, 300, 200, 'C [%] = Meff / ', charSize = 1, color = 0, /device
   xyOuts, 300, 180, '        (M[100] - M[random]) / 100', charSize = 1, color = 0, /device
   xyOuts, 300, 140, 'CM1 [%] = ' + strCompress(string(c1),/rem), charSize = 1, color = 0, /device
   xyOuts, 300, 120, 'CM2 [%] = ' + strCompress(string(c2),/rem), charSize = 1, color = 0, /device
   outImage = tvrd(true = 1)

   window, 11, xsize = floor(newDim[0]/1.5), ysize = floor((newDim[1] + 750)/1.5)
   tv, congrid(outimage, 3, floor(newDim[0]/1.5), floor((newDim[1] + 750)/1.5)), true = 1

   struct = {outImage:outImage, DDAVals:DDAVals, distVect:distVect}

   openW,2, strCompress('c:\rsi\DDASpecial_M1[0]_M1[R]_M1eff_M2[0]_M2[R]_M2eff_.dat', /rem)
   printF, 2, [DDAVals[0,0], M1Random[0], DDAVals[0,0]-M1Random[0], DDAVals[0,1], M2Random[0], DDAVals[0,1]-M2Random[0]]
   close, 2

   openW,2, strCompress('c:\rsi\DDASpecial_r_M1_M2.dat', /rem)
   printF, 2, [transpose(distVect), transpose(DDAVals[*,0]), transpose(DDAVals[*,1])]
   close, 2

   return, struct
end