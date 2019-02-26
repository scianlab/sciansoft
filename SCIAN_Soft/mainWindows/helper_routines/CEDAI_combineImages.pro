pro CEDAI_combineImages, background, imageLeft, imageRight, textRight, filename

;            Background = 'C:\RSI\Phil\VCL_Background.tif'
;            ImageLeft = 'C:\RSI\Phil\VCL_Sperms.tiff'
;            ImageRight = 'C:\RSI\Phil\VCL_Histo.tiff'
;            TextRight = 'C:\RSI\Phil\VCL_Text.tiff'
;            Filename = 'C:\RSI\Phil\VCL.tiff'
;            background = 'C:\RSI\mia\CEDAI_TemplateResults\Spermiogram_VCL_Background.bmp'
;            imageLeft = 'C:\RSI\mia\M1_1_1seg_sinROIs\VCL_Trajectory.tiff'
;            imageRight = 'C:\RSI\mia\M1_1_1seg_sinROIs\VCL_Histo.tiff'
;            textRight = 'C:\RSI\mia\M1_1_1seg_sinROIs\VCL_Text.tiff'
;            filename = 'C:\RSI\mia\M1_1_1seg_sinROIs\VCL.tiff'

   ; MOR - undo reverse - generated new background image with text in english
  ;image_background = reverse(read_image(background),3)
  image_background = read_image(background)
  
  ; MOR - modify the sizing of the left image - BEGIN
  ;image_left_before = congrid(read_image(imageLeft),3,689,530) ; MOR - commented out orig. code
  ;image_left_before = congrid(read_image(imageLeft),3,525,500) ;susana comenta este y activa linea siguiente
  
  image_left_before = congrid(read_image(imageLeft),3,537.42,413.4);Susana
  
  size_left = size(image_left_before, /dim) 
  ;image_left = image_left_before[*, 0:688, 10:520] ; MOR - commented out orig. code
  ;image_left = image_left_before[*,0:floor(size_left[1]*0.95), floor(0+size_left[2]*0.02):floor(size_left[2]*0.95)]  comentada por susana, sustituida por linea siguiente
  image_left = image_left_before[*,0:floor(size_left[1]*0.95), floor(0+size_left[2]*0.000001):10+floor(size_left[2]*0.95)]
  ; MOR - modify the sizing of the left image - END

  image_right_before = read_image(imageRight)
  size_right = size(image_right_before, /dim)
  
  ;image_right = image_right_before[*,5:445,50:300]
  ;image_right = congrid(image_right_before, 3, 440,250)
  image_right = image_right_before[*,floor(0+size_right[1]*0.06):floor(size_right[1]*0.85), floor(0+size_right[2]*0.07):floor(size_right[2]*0.85)]
  
  image_text_before = reverse(read_image(textRight),3)
;  image_text = image_text_before[*,0:340,0:130] ; MOR - commented out original code
  size_text = size(image_text_before, /dim)
  image_text = image_text_before[*,*,5:floor(size_text[2]*0.85)]

  size_left = size(image_left, /dim)
  x1 = size_left[1]
  y1 = size_left[2]

  size_right = size(image_right, /dim)
  x2 = size_right[1]
  y2 = size_right[2]

  size_text = size(image_text, /dim)
  x3 = size_text[1]
  y3 = size_text[2]

  ; Define Starting Point Left
  start_left_X = 25
  ;start_left_Y = 125  ; MOR - commented out original code
  start_left_Y = 90

  ; Define Starting Point Right
  ;start_right_X = 705  ; MOR - commented out original code
  start_right_X = 545
  ;start_right_Y = 385  ; MOR - commented out original code
  start_right_Y = 270

  ; Define Starting Point Text
  ;start_text_X = 740  ; MOR - commented out original code
  start_text_X = 560
  ;start_text_Y = 195  ; MOR - commented out original code
  start_text_Y = 120  ; MOR - move text up a bit 

  image_background[*,start_left_X:start_left_X+X1-1,start_left_Y:start_left_Y+Y1-1] = image_left
  image_background[*,start_right_X:start_right_X+X2-1,start_right_Y:start_right_Y+Y2-1] = image_right
  image_background[*,start_text_X:start_text_X+X3-1,start_text_Y:start_text_Y+Y3-1] = image_text

  ;window, 0, xsize = 1170, ysize = 827, title = filename  ; MOR - commented out original code
  ; MOR - size the window depending on the dimensions of the background image - BEGIN
  siz = size(image_background, /dim)
  window, 0, xsize = siz[1], ysize = siz[2], title = filename
  ; MOR - size the window depending on the dimensions of the background image - BEGIN
  tvscl, image_background, /true

  ; write Image
  image = reverse(image_background, 3)
  write_tiff, filename, image
end