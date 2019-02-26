pro CEDAI_combineImages_motilMovVariables, background, imageLeft, imageRight, imageRight2, textRight, filename,winIndex

DEVICE,DECOMPOSED=0
;
;            Background = 'C:\RSI\Phil\VCL_Background.tif'
;            ImageLeft = 'C:\RSI\Phil\VCL_Sperms.tiff'
;            ImageRight = 'C:\RSI\Phil\VCL_Histo.tiff'
;            TextRight = 'C:\RSI\Phil\VCL_Text.tiff'
;            Filename = 'C:\RSI\Phil\VCL.tiff'
            ; MOR - add another histogram to the output image 
;            background = 'C:\RSI\mia\CEDAI_TemplateResults\Esperm_Motilidad.bmp'
;            imageLeft = 'C:\RSI\mia\VCL_trajectory.tiff'
;            imageRight = 'C:\RSI\mia\VCL_Histo.tiff'
;            imageRight2 = 'C:\RSI\mia\VCL_Histo2.tiff'
;            textRight = 'C:\RSI\mia\VCL_Text.tiff'
;            filename = 'C:\RSI\mia\VCL_TestMotil.tiff'

  ;image_background = reverse(read_image(background),3)
  image_background = read_image(background)
  
    ; MOR - 10Feb2011 - get the name of the test running and output to report 
   pos = STRSPLIT(filename, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
   testName = strMid(filename,pos[bslachs-2],lbSlashs[bslachs-2])
   
;;   fn =  strcompress(strMid(filename,pos[0],total(lbSlashs[0:bslachs-2],/integer)-1) + testName + '\' + testName + '.tiff', /rem)
    ;fn =  strcompress(strMid(fileName,pos[0],pos[bslachs-2]) + testName + '\' + testName + '.tiff', /rem); aqui tedría que incluir el zslice para que aparezca en informe
    ;testName2 = testName+'_zslice'+strcompress(STRING(zPos),/rem); Susana - incluye zslice en informe
    fn =  strcompress(strMid(fileName,pos[0],pos[bslachs-2]) + testName + '\' + testName + '.tiff', /rem); aqui tedría que incluir el zslice para que aparezca en informe
    ;fn =  strcompress(strMid(fileName,pos[0],pos[bslachs-2]) + testName + '\' + testName2 + '.tiff', /rem); Susana -  testName2 incluye el zslice para que aparezca en informe

    ;Text Base
   oBuffer = obj_new('IDLgrBuffer', dimensions = [250,60])
   myFont = obj_new('IDLgrFont', 'hershey*bold', SIZE = 20)
   ; MOR - dummy to get range - BEGIN
   ; bluish background color
   dummy = obj_new('IDLgrPlot',  xRange = [0,25], yRange = [0,25], dataY = [0, 0], color = [74,126,187])
   ; black background color
   ;dummy = obj_new('IDLgrPlot',  xRange = [0,25], yRange = [0,25], dataY = [0, 0], color = [0,0,0])
   ; yellowish background color
   ;dummy = obj_new('IDLgrPlot',  xRange = [0,25], yRange = [0,25], dataY = [0, 0], color = [255,204,0])
   position = [0.1, 0.1, 0.95, 0.95]
   dummy->GetProperty, XRange=xrange
   dummy->GetProperty, YRange=yrange

   ; Set up the scaling so that the axes for the plot and the
   ; plot data extends from 0->1 in the X and Y directions.
   xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
   ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])
    
   oText_0 = obj_new('IDLgrText', location = [2.0, 2.0], color = [255,255,255], XCoord_Conv=xs1, YCoord_Conv=ys1, testName, font = myFont, /enable_formatting)
   ;oText_0 = obj_new('IDLgrText', location = [2.0, 2.0], color = [255,255,255], XCoord_Conv=xs1, YCoord_Conv=ys1, testName2, font = myFont, /enable_formatting); Susana - testName2 incluye el zslice para que aparezca en informe
 ;;  oText_0 = obj_new('IDLgrText', location = [2.0, 4.0], color = [0,0,0], XCoord_Conv=xs1, YCoord_Conv=ys1, testName, font = myFont, /enable_formatting)
   oModel = obj_new('IDLgrModel')
   oModel->remove, /all
   oModel->add, oText_0
   ; bluish background color
   oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 1.05, 1.05], color = [74, 126, 187])
   ; black background color
   ;oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 1.05, 1.05], color = [0, 0, 0])
   ; yellowish background color
   ;oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 1.05, 1.05], color = [255, 204, 0])
   oView->remove, /all
   oView->add, oModel
   oBuffer->draw, oView
   oOImage = oBuffer->Read()
    
   oOImage->GetProperty, data = outImage_1
   outImage_1 = reverse(outimage_1, 3)
   write_tiff, fn, outImage_1
   ; MOR - destroy objects from memory
   obj_destroy, [oBuffer, oView]
  
  ;image_left_before = congrid(read_image(imageLeft),3,500,450)
  image_left_before = congrid(read_image(imageLeft),3,537.42,413.4)
  ;image_left = image_left_before[*, 0:688, 10:520]
  image_left = image_left_before
  image_left = image_left_before[*,0:520,*]

  image_right_before = read_image(imageRight)
  siz = size(image_right_before)
  image_right = congrid(image_right_before, 3,325 ,235)
  image_right = image_right[*,15:310,15:195]

  image_right_before2 = read_image(imageRight2)
  image_right2 = congrid(image_right_before2, 3,325,235)
  image_right2 = image_right2[*,15:310,15:195]
  
  image_text_before = reverse(read_image(textRight),3)
  ; MOR - cut some of the top off
  size_text = size(image_text_before, /dim)
  image_text = image_text_before[*,0:size_text[1]*0.95,0:size_text[2]*0.80]
  
   ;load testName
  image_name = reverse(read_image(fn),3)
  size_name = size(image_name, /dim)
  image_name2 = image_name

  size_left = size(image_left, /dim)
  x1 = size_left[1]
  y1 = size_left[2]

  size_right = size(image_right, /dim)
  x2 = size_right[1]
  y2 = size_right[2]

  size_right2 = size(image_right2, /dim)
  x22 = size_right2[1]
  y22 = size_right2[2]

  size_text = size(image_text, /dim)
  x3 = size_text[1]
  y3 = size_text[2]
  
  size_image2 = size(image_name2, /dim)
  x4 = size_image2[1]
  y4 = size_image2[2]
  

  ; Define Starting Point Left
  ;start_left_X = 30
  start_left_X = 13
  ;start_left_Y = 110
  start_left_Y = 148

  ; Define Starting Point Right
  start_right_X = 580
  start_right_Y = 380

  ; Define Starting Point Right 2
  start_right_X2 = 580
  start_right_Y2 = 195

  ; Define Starting Point Text
  start_text_X = 550
  start_text_Y = 50
  
   ; Define Starting Point TestName
  start_text_X2 = 400
  start_text_Y2 = 625

  image_background[*,start_left_X:start_left_X+X1-1,start_left_Y:start_left_Y+Y1-1] = image_left
  image_background[*,start_right_X:start_right_X+X2-1,start_right_Y:start_right_Y+Y2-1] = image_right
  image_background[*,start_right_X2:start_right_X2+X22-1,start_right_Y2:start_right_Y2+Y22-1] = image_right2
  image_background[*,start_text_X:start_text_X+X3-1,start_text_Y:start_text_Y+Y3-1] = image_text
  image_background[*,start_text_X2:start_text_X2+X4-1, start_text_Y2:start_text_Y2+Y4-1] = image_name2

   ; MOR - make window dependent on the size of the background  - BEGIN
  siz = size(image_background, /dim)
  window, winIndex, xsize = siz[1], ysize = siz[2], title = filename ;Susana - incluyo winIndex para no sobreescribir informe original
  ;window, 0, xsize = 1170, ysize = 827, title = filename ; MOR - comment out original code
  ; MOR - make window dependent on the size of the background  - BEGIN
  tvscl, image_background, /true

  ; write Image
  image = reverse(image_background, 3)
   ;image = image_background
  write_tiff, filename, image
end