; countConsecutiveDigitsAfterPrefix
;
; This function returns the digit count in a the input string containing a given prefix for consecutive digits.
;
; ARGUMENTS:
;           theString: input string
;           prefix   : the prefix after which the consecutive digit sequence requires to be counted.
;
; RETURN VALUE:
;           - the digit count for the consecutive digits after the prefix; for example, in the string '_t140'
;             with '_t' given as prefix, the function will return 3;
;           - when no digits are found inmediately after the prefix, the return value is -1; example, '_t_140'
;             with '_t' given as prefix.
; Note: a string including additional regular expression patterns as prefix can be used to further benefit of this ;)
; TODO JJ, method under construction, this is a temporary placement.
function countConsecutiveDigitsAfterPrefix, theString, prefix
  theRegExp = prefix + '[0|1|2|3|4|5|6|7|8|9]*'
  matchRegExPos = stRegEx(theString, theRegExp)
  if (matchRegExPos eq -1) then return, -1
  recoveredString = stRegEx(theString, theRegExp, /extract)
  countDigits = strLen(recoveredString) - strLen(prefix)
  return, countDigits
end


; Utility procedure to make a given set of SCIAN-Soft mask folders consistent in terms of the number of z-slices
;
; STRONG REQUIREMENT 1 : the size of the all the images must be the same (nevertheless, is easy to make it flexible, I really mean it ;).
; STRONG REQUIREMENT 2 : the file name convention for the masks must be the same for all the folders.
pro fillMissingMasksInFolders, nLevels, mtMaskValue = mtMaskValue
  fileSuffix = '.tif'
  file = dialog_pickfile(/read, get_path = path, filter = '*' + fileSuffix)
  slash = path_sep()
  if ~keyword_set(mtMaskValue) then mtMaskValue = 255

  ; get the list of all the subfolders of 'folder' that correspond to the roi mask folders name pattern
  roiMasksFoldersList = file_search(path, '*_fill_c[0|1|2|3|4|5|6|7|8|9]*', /test_directory, count = fCount)

  ; 1st, iterate to found the largest count of z-slices
  if (fCount ge 1) then begin
    maxZcount = 0
    minZcount = (2ul^32 - 1) ; initialize min to max u-long value
    zCountList = make_array(fCount, /uint)
    roiSampleNames = make_array(fCount, /string)
    for i = 0, fCount-1 do begin
      roiZslicesList = file_search(roiMasksFoldersList[i], '*fill_c*_z*.tif', count = zCount)
      roiSampleNames[i] = roiZslicesList[0]
      zCountList[i] = zCount
      maxZcount >= zCount
      minZcount <= zCount ; not strictly necessary to use the minZcount, just for user resaons :)
    endfor
    if (maxZcount ne minZcount) then begin
      print, 'going to complete masks to have ', maxZcount, ' z-slices'
      sz = size(read_tiff(roiZslicesList[0]), /dim)
      mockImage = make_array(sz, /byte, Value = 255)
      for i = 0, fCount-1 do begin
        if (zCountList[i] lt maxZcount) then begin
          diff = maxZcount - zCountList[i]
          if (diff gt 0) then begin
            print, 'adding ', diff, ' slices in ', roiMasksFoldersList[i], '(', zCountList[i], '-', zCountList[i]+diff-1, ')'
            digitsToRemove = countConsecutiveDigitsAfterPrefix(roiSampleNames[i], '_z')
            baseName = strMid(roiSampleNames[i], 0, strLen(roiSampleNames[i]) - digitsToRemove - strLen(fileSuffix))
            for j = 0, diff-1 do begin
              tifName = baseName + strCompress(zCountList[i]+j, /remove_all) + fileSuffix
              print, 'writing ', tifName
              write_tiff, tifName, mockImage
            endfor
          endif
        endif
      endfor
    endif
  endif
end


pro congridFromImagesInFolderMulti, factor
  basePath = 'C:\RSI\aabm_tests\ylemus\'
  ;imgFoldersInBase  = dialog_pickfile(title = 'Select the INPUT image folder', path = basePath, /DIRECTORY, /READ)
  ;imgFoldersOutBase = dialog_pickfile(title = 'Select the OUTPUT image folder', path = basePath, /DIRECTORY, /READ)
  ;imgFoldersInBase  = ['C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascaras\t14_fill\t14_fill_c']
  ;imgFoldersOutBase = ['C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\serie2\021210_TL_FLH_DORSAL\488nm_halfSize\fill\t13\t33_fill_c']
  imgFoldersInBase  = ['C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascaras\t40_fill\',$
                       'C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascaras\t46_fill\',$
                       'C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascaras\t52_fill\',$
                       'C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascaras\t64_fill\']
  imgFoldersOutBase = ['C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascarasCongrid0.5\t40_fill\',$
                       'C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascarasCongrid0.5\t46_fill\',$
                       'C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascarasCongrid0.5\t52_fill\',$
                       'C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\mascarasCongrid0.5\t64_fill\']
  startCellNum = 1
  endCellNum = 22
  for i = 0L, n_elements(imgFoldersInBase)-1 do $
    congridFromImagesInFolder, factor, imgFoldersInBase[i], imgFoldersOutBase[i], startCellNum, endCellNum
end


; The size of the first image in the list will be used to determine the output size (according to the input factor)
pro congridFromImagesInFolder, factor, imgFolderInBase, imgFolderOutBase, startCellNum, endCellNum

  pathSep = path_sep()

  for cellNum = startCellNum, endCellNum do begin

    if ((imgFolderInBase ne '') and (imgFolderOutBase ne '')) then begin
      folder_1 = strMid(imgFolderInBase, 0, strLen(imgFolderInBase)-1)
      flPrefix = strMid(folder_1, strPos(folder_1, pathSep, /REVERSE_SEARCH)+1)
      cellNumFolder = flPrefix + '_c' + strCompress(string(cellNum), /remove_all) + pathSep
      curFolderIn  = imgFolderInBase  + cellNumFolder
      curFolderOut = imgFolderOutBase + cellNumFolder
      if (file_test(curFolderOut, /DIRECTORY) eq 0) then file_mkDir, curFolderOut
      file = file_search(curFolderIn + '*.tif', count = nFile)
      if (nFile gt 0) then begin
        im0 = read_tiff(file[0])
        szI = size(im0, /DIMENSIONS)
        im0 = -1
        szO = round(szI * factor)
        imgNamePrefix = 'congrid_'
        for i = 0L, nFile-1 do begin
          img = read_tiff(file[i])
          imgName = strMid(file[i], 1+strPos(file[i], pathSep, /REVERSE_SEARCH))
          congrioName = strCompress(imgNamePrefix + imgName ,/remove_all)
          szImg = size(img, /DIMENSIONS)
          congrio = (szImg[0] eq szO[0]) and (szImg[1] eq szO[1]) ? img : congrid(img, szO[0], szO[1])
          print, 'writing to ' + curFolderOut + congrioName
          write_tiff, curFolderOut + congrioName, congrio
        endfor
      endif
    endif
  endfor
  print, 'congridFromImagesInFolder done with factor, ', factor
end


pro congridImagesInFolder, factor
  inputDir  = 'C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\serie3\021210_TL_FLH_DORSAL\488nm\'
  outputDir = 'C:\RSI\aabm_tests\ylemus\flh_TL\021210_flh_TL_export\serie3\021210_TL_FLH_DORSAL\488nm_halfSize\'
  pathSep = path_sep()
  fileList = file_search(inputDir + '*.tif', count = nFile)
  if (nFile gt 0) then begin
    im0 = read_tiff(fileList[0])
    szI = size(im0, /DIMENSIONS)
    im0 = -1
    szO = round(szI * factor)
    imgNamePrefix = 'congrid_'
    for i = 0L, nFile-1 do begin
      img = read_tiff(fileList[i])
      imgName = strMid(fileList[i], 1+strPos(fileList[i], pathSep, /REVERSE_SEARCH))
      congrioName = strCompress(imgNamePrefix + imgName ,/remove_all)
      szImg = size(img, /DIMENSIONS)
      congrio = (szImg[0] eq szO[0]) and (szImg[1] eq szO[1]) ? img : congrid(img, szO[0], szO[1])
      print, 'writing to ' + outputDir + congrioName
      write_tiff, outputDir + congrioName, congrio
    endfor
  endif
  print, 'congridImagesInFolder done'
end


; fillMissingZslicesInFolders
;
; preliminary version
pro fillMissingZslicesInFolders, nLevels, mtMaskValue = mtMaskValue
  fileSuffix = '.tif'
  filePrefix = '26-flh_z'
  nZdigits = 3
  zSlicesList = dialog_pickfile(/read, get_path = path, filter = '*' + fileSuffix)
  ;zSlicesList = file_search(path, '*' + fileSuffix, count = zCount)
  sampleSlice = read_tiff(zSlicesList)
  dummy = make_array(size(sampleSlice, /dim), /byte, value = 0)
  zmin = 45
  zmax = 72
  for z = zmin, zmax do begin
    fname = strCompress(filePrefix + '0' + string(z) + fileSuffix, /remove_all)
    print, 'to write... ', path + fname
    ;write_tiff, fname, dummy
  endfor
end


function intToStringWithZeros, num, nDigits
  auxString = strCompress(string(num), /remove_all)
  auxLen = strLen(auxString)
  deltaChar = nDigits - auxLen
  if (deltaChar lt 0) then begin
    print, 'Input number too large. Increase the total number of digits and try again'
    return, -1
  endif
  zeros = ''
  for i = 0L, deltaChar-1 do zeros += '0'
  return, zeros + auxString
end

; TODO Pablo Liddle
; Sample usage:
; shiftImagesNameCounter, ['c','z'], [-1,-1], 'test'
pro shiftImagesNameCounter, startStrings, shiftCounts, outFolderName, previewOnly = previewOnly
  fileList  = dialog_pickfile(FILTER = '*.tif', /MULTIPLE_FILES, GET_PATH = startPath, DISPLAY_NAME = 'Select all of the images you want to process')
  if (n_elements(fileList) eq 1) and (fileList[0] eq '') then return
  fullOutPath = startPath + outFolderName
  file_mkDir, fullOutPath

  nCount = n_elements(shiftCounts)
  if (nCount gt 0) then begin
    outFileList = fileList
    for i = 0, nCount-1 do $
      outFileList = shiftImagesNameCounterSpec(outFileList, startStrings[i], shiftCounts[i])
  endif

  print, 'Write to output to ', fullOutPath
  for i = 0L, n_elements(fileList)-1 do begin
    print, 'Replace ' + fileList[i] + ' by ' + outFileList[i]
    if ~keyword_set(previewOnly) then begin
      print, 'actually writing'
      img = read_tiff(fileList[i])
      write_tiff, fullOutPath + path_sep() + outFileList[i], img
    endif
  endfor

end


; Works only if the input file names can be splitted in two with the separator (given by the startString).
function shiftImagesNameCounterSpec, fileList, startString, shiftCount

  nFiles = n_elements(fileList)

  if (nFiles lt 1) then begin
    print, 'No files to process. Returning...'
    return, -1
  endif

  outFileList = make_array(nFiles, /string)
  numSeparator = '[0|1|2|3|4|5|6|7|8|9]*'
  separator = '_' + startString + numSeparator

  for i = 0L, n_elements(fileList)-1 do begin
    fileList[i] = strMid(fileList[i], 0)
    aux = strSplit(fileList[i], path_sep(), /extract)
    aux = aux[n_elements(aux)-1]
    sepStrPos = stregex(aux, separator, LENGTH = sepStrLen)
    sepStr = strMid(aux, sepStrPos, sepStrLen)
    sepStrNum = (strSplit(sepStr, '_' + startString, /extract))[0]
    numDigits = strLen(sepStrNum)
    numIn  = sepStrNum + 0
    numOut = numIn + shiftCount
    numStrOut = '_' + startString + intToStringWithZeros(numOut, numDigits)
    outFileList[i] = aux
    aux2 = outFileList[i]
    strPut, aux2, numStrOut, sepStrPos
    outFileList[i] = aux2
  endfor
  return, outFileList

end


pro shiftImages, d
  fileList  = dialog_pickfile(FILTER='*.tif', /MULTIPLE_FILES)
  theChar   = 's'
  separator = '_' + theChar + '[0|1|2|3|4|5|6|7|8|9]*_'
  factor    = 8
  iter      = 34
  for i = 0L, n_elements(fileList)-1 do begin
    img    = read_tiff(fileList[i])
    sepPos = stregex(fileList[i], separator)
    sepStr = stregex(fileList[i], separator, /extract)
    ;sepStrNum = stregex(sepStr, '[0|1|2|3|4|5|6|7|8|9]+', /EXTRACT)
    endPos    = strPos(sepStr, '_', /REVERSE_SEARCH)
    startPos  = strPos(sepStr, theChar)
    sepStrNum = strMid(sepStr, startPos+1, endPos-startPos-1)
    outNames  = strSplit(fileList[i], sepStr, /EXTRACT, /REGEX)
    outNum = uint(sepStrNum) + 1
    amt = 0
    for it = 1, iter do begin
      amt += (factor * d)
      dimg = shift(img, amt, 0)
      outName = strCompress(outNames[0] + '_' + theChar + string(outNum) + '_' + outNames[1], /REMOVE_ALL)
      print, 'Writing from ', fileList[i], ' to ', outName
      write_tiff, outName, dimg
      outNum += 1
    endfor
  endfor
end


pro shiftImagesOvr, d
  fileList  = dialog_pickfile(FILTER='*.tif', /MULTIPLE_FILES)
  separator = '_s[0|1|2|3|4|5|6|7|8|9]*_'
  factor = 8

  for i = 0L, n_elements(fileList)-1 do begin
    img  = read_tiff(fileList[i])
    amt  = (factor * d)
    dimg = shift(img, amt, 0)
    print, 'Overwriting ', fileList[i]
    write_tiff, fileList[i], dimg
  endfor
end


; extractPrefixFromTIFFiles
; Utility function to remove prefixes from TIFF images in a given folder.
; Intended to be used from the IDL command prompt
; 
; AUTHOR: J. Jara (2011)
pro extractPrefixFromTIFFiles
  prefix  = '_Clus0_'
  lprefix = strLen(prefix)
  file    = dialog_pickfile(/read, path = 'C:\RSI', get_path = path, filter = '*.tif', /multiple_files)
  if (file[0] ne '') then begin
    nFiles = n_elements(file)
    for i = 0, nFiles - 1 do begin
      strImageName = strMid(file[i], (strPos(file[i], path_sep(), /reverse_s))+1,$
                                     (strPos(file[i], '.', /reverse_s) - strPos(file[i], path_sep(), /reverse_s)-1))
      resLength = strLen(strImageName) - lprefix
      strImageNewName = path + strMid(strImageName, lprefix, resLength) + '.tif'
      image = read_tiff(file[i])
      write_tiff, strImageNewName, image
      print, 'saving ', strImageName, ' as ', strImageNewName
    endfor
    print, 'Saved ', nFiles, ' files'
  endif
end


pro makeTiffStackWithIncreasingValues, xSize, ySize, zSize, minValue = minValue, maxValue = maxValue, fReverse = fReverse, fShiftOdd = fShiftOdd, fileNamePrefix = fileNamePrefix, valueVect = valueVect

  if n_elements(valueVect) gt 0 then begin
    f16bit = 0b
    fileNameBaseDir = dialog_pickfile(/Directory)
    fileNamePrefix  = arg_present(fileNamePrefix) ? string(fileNamePrefix) : 'greyscaleSlice'
    fileNameBase    = fileNameBaseDir + fileNamePrefix + '_'
    curValuePos     = 0
    nValues = n_elements(valueVect)
    if (nValues gt zSize) then begin
      deltaValuePos = nValues / zSize
    endif else begin
      deltaValuePos = nVa
    endelse
    for z = 0, zSize-1 do begin
      fileNameSuffix = 'z'+ strCompress(string(z, format = '(I4.4)'), /remove_all) + '.tif'
      write_tiff, fileNameBase + fileNameSuffix, f16bit ? (uLonArr(xSize, ySize) + valueVect[curValuePos]) : (bytArr(xSize, ySize) + valueVect[curValuePos])
      print, fileNameBase + fileNameSuffix, ' - : value ', curVal 
      curValuePos += deltaValuePos
    endfor
    print, 'Done. ', zSize, ' z-slices of ', xSize, 'x', ySize, ' pixels with values from ', startVal, ' to ', endVal
    return
  endif

  if keyword_set(maxValue) then begin
    minVal   = arg_present(minValue) ? (minValue > 0ul) : 0ul
    f16bit   = (minVal gt 255) or (maxValue gt 255)
    if f16bit then begin
      print, '16-bit format not supported by IDL. Use continue to proceed with 8-bit...'
      stop
      maxVal = 255b
      minVal = byte(minVal < 254b)
      f16bit = 0
    endif
    maxScale = f16bit ? (2ul^16)-1 : 255
    maxVal   = (maxValue < maxScale) > (minVal+1)
  endif else begin
    maxVal = 255
    minVal = 0
    f16bit = 0
  endelse

  valueRange = (maxVal - minVal) + 1
  fReverse   = keyword_set(fReverse)
  deltaVal   = fReverse ? 1 : -1
  startVal   = fReverse ? maxVal : minVal
  endVal     = fReverse ? minVal : maxVal
  inc        = valueRange / zSize
  if ((valueRange mod zSize) eq 0) and keyword_set(fShiftOdd) then begin
    if fReverse $
    then startVal -= (inc-1) $
    else startVal += (inc-1)
    print, 'Shift in scale... starting from ', startVal
  endif
  if fReverse then inc = -inc

  fileNameBaseDir = dialog_pickfile(/Directory)
  fileNamePrefix  = arg_present(fileNamePrefix) ? string(fileNamePrefix) : 'greyscaleSlice'
  fileNameBase    = fileNameBaseDir + fileNamePrefix + '_'
  curVal = startVal
  for z = 0, zSize-1 do begin
    fileNameSuffix = 'z'+ strCompress(string(z, format = '(I4.4)'), /remove_all) + '.tif'
    write_tiff, fileNameBase + fileNameSuffix, f16bit ? (uLonArr(xSize, ySize) + curVal) : (bytArr(xSize, ySize) + curVal)
    print, fileNameBase + fileNameSuffix, ' - : value ', curVal 
    curVal += inc
  endfor
  print, 'Done. ', zSize, ' z-slices of ', xSize, 'x', ySize, ' pixels with values from ', startVal, ' to ', endVal
end
