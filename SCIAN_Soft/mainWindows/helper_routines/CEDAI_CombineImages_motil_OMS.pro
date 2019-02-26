pro CEDAI_combineImages_motil_OMS, background, imageLeft, imageRight, imageRight2, textRight, filename, textLeft,textRight2,v_guardaPNG
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
   
;   fn =  strcompress(strMid(filename,pos[0],total(lbSlashs[0:bslachs-2],/integer)-1) + testName + '\' + testName + '.tiff', /rem)
fn =  strcompress(strMid(fileName,pos[0],pos[bslachs-2]) + testName + '\' + testName + '.tiff', /rem) ; MOR - 16Mar2011
  
   ;Text Base
   oBuffer = obj_new('IDLgrBuffer', dimensions = [250,60])
   myFont = obj_new('IDLgrFont', 'hershey*bold', SIZE = 20)
   ; MOR - dummy to get range - BEGIN
   ; bluish background color
   dummy = obj_new('IDLgrPlot',  xRange = [0,25], yRange = [0,25], dataY = [0, 0], color = [74,126,187]) ; EN uso Marzo2013
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
   ;oText_0 = obj_new('IDLgrText', location = [2.0, 4.0], color = [0,0,0], XCoord_Conv=xs1, YCoord_Conv=ys1, testName, font = myFont, /enable_formatting)
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
  ;;image_right = congrid(image_right_before, 3,325 ,235)
  ;image_right = congrid(image_right_before, 3,325 ,230);Susana probando ver barras en histograma VCL en informe 
  image_right = congrid(image_right_before, 3,325 ,218);Susana probando ver barras en histograma VCL en informe
  image_right = image_right[*,15:310,15:195]

 image_right_before2 = read_image(imageRight2)
  ;;image_right2 = congrid(image_right_before2, 3,325,235)
  ;image_right2 = congrid(image_right_before2, 3,325,230);Susana probando ver barras en histograma VSL en informe 
  image_right2 = congrid(image_right_before2, 3,325,218);Susana probando ver barras en histograma VSL en informe
  image_right2 = image_right2[*,15:310,15:195]
  
  image_text_before = reverse(read_image(textRight),3)
  ; MOR - cut some of the top off
  size_text = size(image_text_before, /dim)
  ;image_text = image_text_before[*,*,0:size_text[2]*0.80];default
  image_text = image_text_before[*,*,size_text[1]*0.1:size_text[2]*0.8]
  
  ;Susana - Concentration - START
  image_text_beforeConc = reverse(read_image(textRight2),3)
  ; MOR - cut some of the top off
  size_textConc = size(image_text_beforeConc, /dim)
  image_textRight2 = image_text_beforeConc[*,*,0:size_textConc[2]*0.80];default
  ;image_textRight2 = image_text_before[*,*,size_textConc[1]*0.1:size_textConc[2]*0.8]
  ;Susana - Concentration - END
  
;  ; MOR - 12Feb2011 - concentration statistics
;  image_text2_before = reverse(read_image(textLeft),3)
;  ; MOR - cut some of the top off
;  image_text2 = congrid(image_text2_before, 3,100,175)
;  size_text2 = size(image_text2_before, /dim)
;  image_text2 = image_text2_before[*,*,75:size_text2[2]*0.87]
;  


    ;load testName
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

  size_right = size(image_right, /dim)
  x2 = size_right[1]
  y2 = size_right[2]

  size_right2 = size(image_right2, /dim)
  x22 = size_right2[1]
  y22 = size_right2[2]

  size_text = size(image_text, /dim)
  x3 = size_text[1]
  y3 = size_text[2];default alto texto?
  ;y3 = size_text[2]
  
  ;Susana - Concentration - START
  ;image_textRight2
  size_textConc = size(image_textRight2, /dim)
  x3Conc = size_textConc[1]
  y3Conc = size_textConc[2];default alto texto?
  ;Susana - Concentration - END
  
  size_image2 = size(image_name2, /dim)
  x4 = size_image2[1]
  y4 = size_image2[2]
  
;  size_text2 = size(image_text2, /dim)
;  x5 = size_text2[1]
;  y5 = size_text2[2]

  ; Define Starting Point Left
  ;;start_left_X = 30; image_text = image_text_before[*,*,size_text[1]*0.1:size_text[2]*0.8]
  start_left_X = 13 ;image_text = image_text_before[*,*,size_text[1]*0.1:size_text[2]*0.8]
  
  ;;start_left_Y = 110;image_text = image_text_before[*,*,size_text[1]*0.1:size_text[2]*0.8]
  start_left_Y = 148;image_text = image_text_before[*,*,size_text[1]*0.1:size_text[2]*0.8]
  
  ; Define Starting Point Right
  start_right_X = 580
  start_right_Y = 375

  ; Define Starting Point Right 2
  start_right_X2 = 580
  start_right_Y2 = 190

  ; Define Starting Point Text
  start_text_X = 560 ;  default
  ;start_text_Y = 50;default
  start_text_Y = 90; susana sube el rotulo %s Motilidad, abro espacio para Concentration
  
  ;Susana - Concentration - START
  ; Define Starting Point Text
  start_text_XConc = 560 ;
  start_text_YConc = 40;Concentracion
  sizeBackground = size(image_background, /dim)
  ;Susana - Concentration - END
  
 ; Define Starting Point TestName
  start_text_X2 = 400
  start_text_Y2 = 625
  
;  ; Define Starting Point Left - Text Concentration
;  start_text_X5 = 20
;  start_text_Y5 = 0

  image_background[*,start_left_X:start_left_X+X1-1,start_left_Y:start_left_Y+Y1-1] = image_left
  image_background[*,start_right_X:start_right_X+X2-1,start_right_Y:start_right_Y+Y2-1] = image_right
  image_background[*,start_right_X2:start_right_X2+X22-1,start_right_Y2:start_right_Y2+Y22-1] = image_right2
  image_background[*,start_text_X:start_text_X+X3-1,start_text_Y:start_text_Y+Y3-1] = image_text;default
  ;image_background[*,start_text_XConc:sizeBackground[1]-1,start_text_YConc:sizeBackground[2]-1] = image_textRight2;default  textRight2
  image_background[*,start_text_XConc:start_text_XConc+X3Conc-1,start_text_YConc:start_text_YConc+Y3Conc-1] = image_textRight2;default  textRight2
  
  ;image_background[*,start_text_X2:start_text_X2+X4-1, start_text_Y2:start_text_Y2+Y4-1] = image_name2; Susana - En uso Marzo2013 Nombre/Rótulo con fondo azul al centro del informe, puede ser la carpeta contenedora
;;  image_background[*,start_text_X5:start_text_X5+X5-1, start_text_Y5:start_text_Y5+Y5-1] = image_text2
  
   ; MOR - make window dependent on the size of the background  - BEGIN
  siz = size(image_background, /dim)
  window, 0, xsize = siz[1], ysize = siz[2], title = filename
  ;window, 0, xsize = 1170, ysize = 827, title = filename ; MOR - comment out original code
  ; MOR - make window dependent on the size of the background  - BEGIN
  tvscl, image_background, /true

  ; write Image
  image = reverse(image_background, 3)
   ;image = image_background
  write_tiff, filename, image
  
  ; Susana - guardo png para adjuntar a cedai.cl - INICIO
  if(v_guardaPNG eq 1) then begin
    filename = strMid(filename,0,strlen(filename)-4)+'png'
    write_png, filename, image,/ORDER ; rotar verticalmente
  endif
  ; Susana - guardo png para adjuntar a cedai.cl - FIN
end