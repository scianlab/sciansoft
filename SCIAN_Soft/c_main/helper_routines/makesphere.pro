pro makeSphere

   maxRad = 63
   sideLen = 2*maxRad + 1
   if ((sideLen mod 2) eq 1) then sideLen +=1
   xyzDim = [sideLen, sideLen, sideLen]

   volData1 = bytArr(xyzDim[0],xyzDim[1],xyzDim[2])
   xyzCentre = xyzDim/2
   slash = path_sep()
   subDirName = strCompress(string(xyzDim[0]) + 'x' + string(xyzDim[1]) + 'x' + string(xyzDim[2]), /rem)
   dirName = 'D:' + slash + 'RSI' + slash + 'SCIANSoft_tests' + slash + 'sphereRad' + slash + subDirName + slash
   fDir = file_test(dirName, /DIRECTORY)
   if (fDir eq 0) then file_mkDir, dirName

   for rad = 1, maxRad do begin

     strImageTStack = strCompress(string(rad-1), /rem)
     while (strLen(strImageTStack) lt 3) do strImageTStack = strCompress('0' + strImageTStack, /rem)
     strImageTStack = strCompress('_t' + strImageTStack, /rem)

     for i = 0, xyzDim[0]-1 do for j = 0, xyzDim[1]-1 do for k = 0, xyzDim[2]-1 do begin
      if (((i - xyzCentre[0])^2 + (j - xyzCentre[1])^2 + (k - xyzCentre[2])^2) le rad^2) then volData1[i,j,k] = 255
     endfor
;     volData1[20:80, 20:80,20:80] = 255
;   save, FILENAME = 'C:\rsi\makeSphere.sav', volData1

     for zStep = 0, xyzDim[2]-1 do begin
       strImageZStack = strCompress(string(zStep), /rem)
       while (strLen(strImageZStack) lt 3) do strImageZStack = strCompress('0' + strImageZStack, /rem)
       strImageZStack = strCompress('_z' + strImageZStack, /rem)

       write_tiff, dirName + 'sphereRad_' + subDirName + strImageTStack + '_ch_000' + strImageZStack + '.tif', bytArr(xyzDim[0],xyzDim[1]) + volData1[*,*,zStep]
     endfor

     volData1 = volData1 * 0
   endfor
end


pro makeTubes

  xyzDim = [45,200,100]*1.
  xCentre = round(xyzDim[0]/2.)*1.
  yCentre = [4,10,18,30,45,64,90,120,160]
  zCentre = round(xyzDim[2]/2.)*1.

  volData1 = bytArr(xyzDim[0],xyzDim[1],xyzDim[2])

  strImageTStack = strCompress(string(0), /rem)
  while (strLen(strImageTStack) lt 3) do strImageTStack = strCompress('0' + strImageTStack, /rem)
  strImageTStack = strCompress('_t' + strImageTStack, /rem)

  for i = 0,xyzDim[0]-1 do for j = 0,xyzDim[1]-1 do for k = 10, xyzDim[2]-10 do begin
    if (((i - xCentre[0])^2 + (j - yCentre[0])^2) le 1.^2) then volData1[i,j,k] = 255
    if (((i - xCentre[0])^2 + (j - yCentre[1])^2) le 2.^2) then volData1[i,j,k] = 255
    if (((i - xCentre[0])^2 + (j - yCentre[2])^2) le 3.^2) then volData1[i,j,k] = 255
    if (((i - xCentre[0])^2 + (j - yCentre[3])^2) le 4.^2) then volData1[i,j,k] = 255
    if (((i - xCentre[0])^2 + (j - yCentre[4])^2) le 5.^2) then volData1[i,j,k] = 255
    if (((i - xCentre[0])^2 + (j - yCentre[5])^2) le 7.^2) then volData1[i,j,k] = 255
    if (((i - xCentre[0])^2 + (j - yCentre[6])^2) le 10.^2) then volData1[i,j,k] = 255
    if (((i - xCentre[0])^2 + (j - yCentre[7])^2) le 15.^2) then volData1[i,j,k] = 255
    if (((i - xCentre[0])^2 + (j - yCentre[8])^2) le 20.^2) then volData1[i,j,k] = 255
  endfor

  slash      = path_sep()
  baseDir    = 'D:\RSI'
  subDirName = strCompress(string(xyzDim[0]) + 'x' + string(xyzDim[1]) + 'x' + string(xyzDim[2]), /rem)
  dirName    = baseDir + slash + 'SCIANSoft_tests' + slash + 'tubesRad' + slash + subDirName + slash
  fDir       = file_test(dirName, /DIRECTORY)
  if (fDir eq 0) then file_mkDir, dirName

  for zStep = 0, xyzDim[2]-1 do begin
    strImageZStack = strCompress(string(zStep), /rem)
    while (strLen(strImageZStack) lt 3) do strImageZStack = strCompress('0' + strImageZStack, /rem)
    strImageZStack = strCompress('_z' + strImageZStack, /rem)
    write_tiff, dirName + 'tubeRad_' + subDirName + strImageTStack + '_ch_000' + strImageZStack + '.tif', bytArr(xyzDim[0],xyzDim[1]) + volData1[*,*,zStep]
  endfor
  print, 'Files written as ' + dirName + 'tubeRad_' + subDirName + strImageTStack + '_ch_000' + '_z*.tif'
end


pro drawCube, volData, xsize = xsize, ysize = ysize, zsize = zsize, cubeside = cubeside, xmin = xmin, ymin = ymin, zmin = zmin
  if n_elements(xmin) eq 0 then xmin = (xsize - cubeside) /2 > 0
  if n_elements(ymin) eq 0 then ymin = (ysize - cubeside) /2 > 0
  if n_elements(zmin) eq 0 then zmin = (zsize - cubeside) /2 > 0
  ;nVoxels = xsize * ysize * zsize
  ;if n_elements(volData) ne nVoxels then begin
  ;  print, 'Incorrect image size!' & stop
  ;  volData = bytArr(xsize, ysize, zsize)
  ;endif
  volData[xmin:xmin+cubeside,ymin:ymin+cubeside,zmin:zmin+cubeside] = 255
end


pro makeAdjacentCubeCells

  imSizeX = 512
  imSizeY = 512
  imSizeZ = 512

  cellSizeSide1 = 140
  cellSizeSide2 = 140
  nCells = 2

  ; Two cells
  originCell1x = 120
  originCell1y = 120
  originCell1z = 120
  strOrig1 = 'ori1_' + strCompress(string(originCell1x), /remove_all) + '-' + strCompress(string(originCell1y), /remove_all) + '-' + strCompress(string(originCell1z), /remove_all)

  originCell2x = 265
  originCell2y = 120
  originCell2z = 120
  strOrig2 = 'ori2_' + strCompress(string(originCell2x), /remove_all) + '-' + strCompress(string(originCell2z), /remove_all) + '-' + strCompress(string(originCell2z), /remove_all)

  volData = bytArr(imSizeX, imSizeY, imSizeZ)
  drawCube, volData, cubeSide = cellSizeSide1, xmin = originCell1x, ymin = originCell1y, zmin = originCell1z
  drawCube, volData, cubeSide = cellSizeSide2, xmin = originCell2x, ymin = originCell2y, zmin = originCell2z

  slash      = path_sep()
  baseDir    = 'D:\RSI'
  subDirName = strCompress(string(imSizeX) + 'x' + string(imSizeY) + 'x' + string(imSizeZ), /rem) + strOrig1 + '-' + strOrig2
  dirName    = baseDir + slash + 'SCIANSoft_tests' + slash + 'cubolebias_adjacent' + strCompress(string(nCells), /rem) + slash + subDirName + slash
  fDir       = file_test(dirName, /DIRECTORY)
  strImageName   = 'cubolebiasAdj_'
  strImageTStack = '_t000'
  if (fDir eq 0) then file_mkDir, dirName

  for i = 0u, imSizeZ-1 do begin
    strImageZStack = strCompress(string(i), /rem)
    while (strLen(strImageZStack) lt 3) do strImageZStack = strCompress('0' + strImageZStack, /rem)
    write_tiff, dirName + strImageName + subDirName + strImageTStack + '_ch_000' + strImageZStack + '.tif', bytArr(imSizeX, imSizeY) + volData[*,*,i]
  endfor

  print, 'Files written as ' + dirName + strImageName + subDirName + strImageTStack + '_ch_000' + '_z*.tif'

end
