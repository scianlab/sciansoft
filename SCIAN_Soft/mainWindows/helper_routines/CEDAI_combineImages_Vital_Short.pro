pro CEDAI_combineImages_Vital_Short, background, imageLeft, fileName, textLeft

DEVICE,DECOMPOSED=0
; PARA GENERAR UN INFORME POR CADA IMAGEN CON ESPERMATOZOIDES MARCADOS
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
  
  vSube = -250
  
  ; MOR - 10Feb2011 - get the name of the test running and output to report 
   pos = STRSPLIT(fileName, count = bslachs, '\', ESCAPE=':', LENGTH = lbSlashs)   ;posicion de substrings
   
   ; testName = last folder name
   testName = strMid(fileName,pos[bslachs-2],lbSlashs[bslachs-2]); testName = last folder name
;  fn =  strcompress(strMid(fileName,pos[0],pos[bslachs-1) + testName + '\' + testName + '.tiff', /rem)
   fn =  strcompress(strMid(fileName,pos[0],pos[bslachs-2]) + testName + '\' + testName + '.tiff', /rem)
  
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
    
    ; text color - black
    ;include the last folder name
   oText_0 = obj_new('IDLgrText', location = [2.0, 2.0], color = [255,255,255], XCoord_Conv=xs1, YCoord_Conv=ys1, testName, font = myFont, /enable_formatting)
   ; text color - white
   ;oText_0 = obj_new('IDLgrText', location = [2.0, 4.0], color = [0,0,0], XCoord_Conv=xs1, YCoord_Conv=ys1, testName, font = myFont, /enable_formatting)
   
   oModel = obj_new('IDLgrModel')
   oModel->remove, /all
   oModel->add, oText_0
   ; bluish background color
   oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 1.05, 1.05], color = [74, 126, 187])
   ; black background color
   ;oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 1.05, 1.05], color = [0, 0, 0])
   ; yellowish background color
;   oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 1.05, 1.05], color = [255, 204, 0])
   oView->remove, /all
   oView->add, oModel
   oBuffer->draw, oView
   oOImage = oBuffer->Read()
    
   oOImage->GetProperty, data = outImage_1
   outImage_1 = reverse(outimage_1, 3)
   write_tiff, fn, outImage_1
   ; MOR - destroy objects from memory
   obj_destroy, [oBuffer, oView, oOImage]
  
    ; trajectories on image
  image_left_before = congrid(read_image(imageLeft),3,537.42,413.4); Motil
  ;image_left_before = congrid(read_image(imageLeft),3,573.42,449.4);Susana achica imagen de trayectorias
  
  ;image_left = image_left_before[*, 0:688, 10:520]
  image_left = image_left_before
  ;image_left = image_left_before[*,0:520,*]; Susana comenta esto
  image_left = image_left_before[*,*,*]; Susana

  ; motility statistics 
;  image_text_before = reverse(read_image(textRight),3)
  ; MOR - cut some of the top off
;  size_text = size(image_text_before, /dim)
;  image_text = image_text_before[*,*,0:size_text[2]*.95]
  
  ; MOR - 12Feb2011 - concentration statistics
  image_text2_before = reverse(read_image(textLeft),3)
  ; MOR - cut some of the top off
  ;image_text2 = congrid(image_text2_before, 3,100,175)
  size_text2 = size(image_text2_before, /dim)
  ;image_text2 = image_text2_before[*,*,25:size_text2[2]*0.87]
  image_text2 = image_text2_before[*,*,25:size_text2[2]*0.95]
  
  ;load testName for displaying on ppt
  image_name = reverse(read_image(fn),3)
  ;image_name2 = congrid(image_name, 3,75,75)
  ;size_name = size(image_name2, /dim)
  size_name = size(image_name, /dim)
  ;image_name2 = image_name2[*,*,0:size_name[2]*0.60]
  ;image_name2 = image_name[*,*,0:size_name[2]*0.60]
  image_name2 = image_name

  size_left = size(image_left, /dim)
  x1 = size_left[1]
  y1 = size_left[2]

 ; size_text = size(image_text, /dim)
 ; x2 = size_text[1]
 ; y2 = size_text[2]
  
  size_text2 = size(image_text2, /dim)
  x3 = size_text2[1]
  y3 = size_text2[2]
  
  size_image2 = size(image_name2, /dim)
  x4 = size_image2[1]
  y4 = size_image2[2]
  
  ; Define Starting Point Left
  start_left_X = 13
  start_left_Y = 148

  ; Define Starting Point Right
  start_right_X = 580
  start_right_Y = 425-vSube

  ; Define Starting Point Right 2
  start_right_X2 = 580
  start_right_Y2 = 220-vSube

 
 ; Define Starting Point TestName
  start_text_X2 = 400
  start_text_Y2 = 625-vSube
  
  image_background[*,start_left_X:start_left_X+X1-1,start_left_Y:start_left_Y+Y1-1] = image_left            ; trajectory
  ;image_background[*,start_right_X:start_right_X+X2-1,start_right_Y:start_right_Y+Y2-1] = image_text       ; motility stats
  image_background[*,start_right_X2:start_right_X2+X3-1,start_right_Y2:start_right_Y2+Y3-1] = image_text2   ; concentration stats
  ;image_background[*,start_text_X2:start_text_X2+X4-1, start_text_Y2:start_text_Y2+Y4-1] = image_name2      ; experimental label
  
  ; MOR - make window dependent on the size of the background  - BEGIN
  siz = size(image_background, /dim)
  window, 0, xsize = siz[1], ysize = siz[2], title = fileName
  ;window, 0, xsize = 1170, ysize = 827, title = fileName ; MOR - comment out original code
  ; MOR - make window dependent on the size of the background  - BEGIN
  tvscl, image_background, /true

  ; write Image
  image = reverse(image_background, 3)
  ;image = image_background
  write_tiff, fileName, image
end