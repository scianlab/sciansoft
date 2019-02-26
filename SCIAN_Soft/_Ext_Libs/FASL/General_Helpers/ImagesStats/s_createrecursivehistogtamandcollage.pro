
pro s_CreateRecursiveHistogtamAndCollage
  sFolder = DIALOG_PICKFILE(PATH='c:\', /DIRECTORY, $ 
            TITLE="Choose Directory containing Images for Collage.")

  if(strLen(sFolder) lt 1) then begin
    print, 'Wrong Selection...  Please.. Try again'
            endif else begin
              ; First level of directories...
              ; continue... at each subFolder
              pattern = 'grey*';

              ssFolder = FILE_SEARCH(sFolder + '*', /TEST_DIRECTORY)

              groupHist = MAKE_ARRAY(256,/BYTE, value = 0)
              metaHist  = MAKE_ARRAY(256,/BYTE, value = 0)
              
              numSubFolders     = N_ELEMENTS(ssFolder)
              flagSubFolder     = MAKE_ARRAY(numSubFolders, /LONG, value = 0)
              numImagesY        = MAKE_ARRAY(numSubFolders, /LONG, value = 0)
              numImagesX        = MAKE_ARRAY(numSubFolders, /LONG, value = 0)
              numImagesToPlot   = MAKE_ARRAY(numSubFolders, /LONG, value = 0)
              xSizeCollage      = MAKE_ARRAY(numSubFolders, /LONG, value = 0)
              ySizeCollage      = MAKE_ARRAY(numSubFolders, /LONG, value = 0)
              numImagesBase     = 0L
              
              for index = 0L, N_ELEMENTS(ssFolder)-1L do begin
                  ; Second level of directories...
                  ; We need to find the images here..!!!!
                  result_L1 = FILE_SEARCH(ssFolder[index], pattern)
                  numImages = N_ELEMENTS(result_L1)
                  filenameIN = result_L1[0]
                  flag = FILE_TEST(filenameIn)

                  if(flag) then begin
                    fOK = query_tiff(filenameIN, info)
                    if fOK then begin
                      image = read_tiff(filenameIN)
                      szIBase = size(image, /dim)
                    endif else flag = 0
                  endif
 
                  if(flag) then begin
                    ; Try to find a good size for collage
                    ; Aspect Ratio 3:4
                    ; 3*numImagesToPlot = 3*numImagesToPlot  
                    ; So...
                    ; 5*4+
                    numImagesBase           = numImagesBase + numImages 
                    dummy                   = sqrt(numImages)
                    numImagesY[index]       = ceil(dummy);                   
                    numImagesX[index]       = ceil(numImagesY[index] * 3.0/4.0)
                    xSizeCollage[index]     = numImagesX[index] * szIBase(0)
                    ySizeCollage[index]     = numImagesY[index] * szIBase(1)
                    numImagesToPlot[index]  = numImagesX[index] * numImagesY[index] 

                  endif
                  flagSubFolder[index] = flag
              endfor

              if(flag) then begin
                ; Try to find a good size for Big collage
                ; Aspect Ratio 3:4
                ; 3*numImagesToPlot = 3*numImagesToPlot  
                ; So...
                dummy                   = sqrt(numImagesBase)
                numImagesYBig           = ceil(dummy);                   
                numImagesXBig           = ceil(numImagesYBig * 3.0/4.0)
                numImagesToPlotBig      = numImagesX * numImagesY
                xSizeCollageBig         = numImagesXBig * szIBase(0)
                ySizeCollageBig         = numImagesYBig * szIBase(1)
                numImagesToPlotBig      = numImagesXBig * numImagesYBig 
              endif

              imCollageGlobal = MAKE_ARRAY(xSizeCollageBig, ySizeCollageBig, /Byte, Value = 0)
              offsetXBig  = 0L
              offsetyBig  = 0L
              imUsedsBig  = 0L
              
              for index = 0L, N_ELEMENTS(ssFolder)-1L do begin
                  metaHist = metaHist * 0L 
                  ; Second level of directories...
                  ; We need to find the images here..!!!!
                  result_L1 = FILE_SEARCH(ssFolder[index], pattern)
                  numImages = N_ELEMENTS(result_L1)

                  flag = flagSubFolder[index]
                  if(flag) then begin
                    metaHist  = MAKE_ARRAY(256,/BYTE, value = 0)

                    imUseds = 0L
                    imCollageSubFolder = MAKE_ARRAY(xSizeCollage[index], ySizeCollage[index], /Byte, Value = 0)
                    
                    offsetX = 0L
                    offsetY = 0L
                      for idx_L1 = 0L, numImages - 1L do begin
                        filenameIN = result_L1[idx_L1]
                        flag = FILE_TEST(filenameIn)
                        
                        fOK = query_tiff(filenameIN, info)
                        if fOK then begin
                          image = read_tiff(filenameIN)
                          szI = size(image, /dim)
                          ;tvscl, image, XSIZE= szI[0],YSIZE= szI[1]
                          tvscl, image
                          
                          if(imUseds lt numImagesToPlot[index]) then begin
                            imCollageSubFolder(offsetX:offsetX+szIBase(0)-1,offsetY:offsetY+szIBase(1)-1) = image
                            offsetX = offsetX + szIBase(0)
                            if((offsetX + szIBase(0))gt xSizeCollage[index]) then begin  
                              offsetY = offsetY + szIBase(1)
                              offsetX = 0
                            endif 
                            imUseds++;
                          endif
                          
                          ; Calc Hist 
                          metaHist = metaHist + histogram(image)
                          
                          
                          ; Add image to BigCollage
                          if(imUsedsBig lt numImagesToPlotBig) then begin
                            imCollageGlobal(offsetXBig:offsetXBig+szIBase(0)-1,offsetYBig:offsetYBig+szIBase(1)-1) = image
                            offsetXBig = offsetXBig + szIBase(0)
                            if((offsetXBig + szIBase(0))gt xSizeCollageBig) then begin  
                              offsetYBig = offsetYBig + szIBase(1)
                              offsetXBig = 0
                            endif 
                            imUsedsBig++;
                          endif

                          
                                                      
                          
                        endif
                      endfor
                      ; Calc global hist
                      groupHist = groupHist + metaHist;
                      ; Save multifolder hist and collage   
                      ; save metaHist
                      
                      write_tiff, strCompress(ssFolder[index] + 'Collage' + string(index) +'.tif'), imCollageSubFolder
                      tvscl, imCollageSubFolder
                           
                  endif
              endfor 
              ; Save bigCollage and global hist
              write_tiff, strCompress(sFolder + 'CollageGlobal' +'.tif'), imCollageGlobal
              tvscl, imCollageGlobal
              
            endelse
end

