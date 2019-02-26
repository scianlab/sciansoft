pro CEDAI_textPlot, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11,string_21,string_23,nom

; for Debugging activate the following:
;        string_0 = 'Typo A:'
;        string_1 = 'Typo B:'
;        string_2 = 'Typo C:'
;        string_3 = 'Typo D:'
;        string_4 = '0.85 %'
;        string_5 = '11.25 %'
;        string_6 = '92.84 %'
;        string_7 = '0.01 %'
;        string_8 = '(0 espermios)'
;        string_9 = '(11 espermios)'
;        string_10 = '(89 espermios)'
;        string_11 = '(5 espermios)'
;        Filename = 'C:\RSI\Mia\M1_1_1seg_sinROIs\Text.tiff'

      ;Text Base
   oBuffer = obj_new('IDLgrBuffer', dimensions = [340, 130])
   myFont = obj_new('IDLgrFont', 'arial', SIZE = 15)
   ; MOR - dummy to get range - BEGIN
   dummy = obj_new('IDLgrPlot',  xRange = [0,25], yRange = [0,25], dataY = [0, 0], color = [0, 0,0])
   ; MOR - dummy to get range - BEGIN
   
    if n_elements(string_0) eq 0 then string_0 = ''
    if n_elements(string_1) eq 0 then string_1 = ''
    if n_elements(string_2) eq 0 then string_2 = ''
    if n_elements(string_3) eq 0 then string_3 = ''
    if n_elements(string_4) eq 0 then string_4 = ''
    if n_elements(string_5) eq 0 then string_5 = ''
    if n_elements(string_6) eq 0 then string_6 = ''
    if n_elements(string_7) eq 0 then string_7 = ''
    if n_elements(string_8) eq 0 then string_8 = ''
    if n_elements(string_9) eq 0 then string_9 = ''
    if n_elements(string_10) eq 0 then string_10 = ''
    if n_elements(string_11) eq 0 then string_11 = ''

; MOR - add boxes with associated color for the text - BEGIN
   ; blue - type D
   boxB = bytarr(3, 2, 2)
   boxB[0,*,*] = 0
   boxB[1, *,*] = 0
   boxB[2,*,*] = 255
   ; green - type C
   boxG = bytarr(3, 2, 2)
   boxG[0,*,*] = 0
   boxG[1, *,*] = 255
   boxG[2,*,*] = 0
   ; yellow - type B
   boxY = bytarr(3, 2, 2)
   boxY[0,*,*] = 255
   boxY[1, *,*] = 255
   boxY[2,*,*] = 0  
   ; red - type A
   boxR = bytarr(3, 2, 2)
   boxR[0,*,*] = 255
   boxR[1, *,*] = 0
   boxR[2,*,*] = 0
   
   ; Susana - Coloco un box temporalmente del color de los NC (No Clasificados para Movilidad) - INICIO
   ;; NC - type NC
   ;boxNC = bytarr(3, 2, 2)
   ;boxNC[0,*,*] = 0
   ;boxNC[1, *,*] = 255
   ;boxNC[2,*,*] = 255
   ; Susana - Coloco un box temporalmente del color de los NC (No Clasificados para Movilidad) - FIN
   
          ; Because we may not be using exact axis ranging, the axes
    ; may extend further than the xrange and yrange. Get the
    ; actual axis range so that the plot, etc. can be scaled
    ; appropriately.
    position = [0.1, 0.1, 0.95, 0.95]
    dummy->GetProperty, XRange=xrange
    dummy->GetProperty, YRange=yrange

    ; Set up the scaling so that the axes for the plot and the
    ; plot data extends from 0->1 in the X and Y directions.

    xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
    ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])

    ; Scale the plot data and axes into 0->1.

   ;oImage_0 = obj_new('IDLgrImage',boxR,location = [0.1, 16.0], XCoord_Conv=xs1, YCoord_Conv=ys1)
   ;oImage_0 = obj_new('IDLgrImage',boxR,location = [-0.8, 16.0], XCoord_Conv=xs1, YCoord_Conv=ys1) ;susana mueve a la izquierda
   oImage_0 = obj_new('IDLgrImage',boxR,location = [-0.8, 17.0], XCoord_Conv=xs1, YCoord_Conv=ys1) ;susana mueve a la izquierda
   oImage_1 = obj_new('IDLgrImage',boxY,location = [-0.8, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1)
   oImage_2 = obj_new('IDLgrImage',boxG,location = [-0.8, 7.0], XCoord_Conv=xs1, YCoord_Conv=ys1)
   oImage_3 = obj_new('IDLgrImage',boxB,location = [-0.8, 2.0], XCoord_Conv=xs1, YCoord_Conv=ys1)
   ; Susana - Coloco un box temporalmente del color de los NC (No Clasificados para Movilidad) - INICIO
   ;oImage_60 = obj_new('IDLgrImage',boxNC,location = [-0.8, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1)
   ; Susana - Coloco un box temporalmente del color de los NC (No Clasificados para Movilidad) - FIN
   
   ;;oText_0 = obj_new('IDLgrText', location = [3.0, 16.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_0, font = myfont, /enable_formatting)
   ;oText_0 = obj_new('IDLgrText', location = [2.0, 16.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_0, font = myfont, /enable_formatting)
   oText_0 = obj_new('IDLgrText', location = [2.0, 17.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_0, font = myfont, /enable_formatting)
   oText_1 = obj_new('IDLgrText', location = [2.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_1, font = myfont, /enable_formatting)
   oText_2 = obj_new('IDLgrText', location = [2.0, 7.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_2, font = myfont, /enable_formatting)
   oText_3 = obj_new('IDLgrText', location = [2.0, 2.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_3, font = myfont, /enable_formatting)
   ;;oText_4 = obj_new('IDLgrText', location = [15.0, 16.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_4, font = myfont, alignment = 1, /enable_formatting)
   ;oText_4 = obj_new('IDLgrText', location = [13.0, 16.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_4, font = myfont, alignment = 1, /enable_formatting)
   oText_4 = obj_new('IDLgrText', location = [13.0, 17.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_4, font = myfont, alignment = 1, /enable_formatting)
   oText_5 = obj_new('IDLgrText', location = [13.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_5, font = myfont, alignment = 1, /enable_formatting)
   oText_6 = obj_new('IDLgrText', location = [13.0, 7.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_6, font = myfont, alignment = 1, /enable_formatting)
   oText_7 = obj_new('IDLgrText', location = [13.0, 2.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_7, font = myfont, alignment = 1, /enable_formatting)
   
   ;;oText_8 = obj_new('IDLgrText', location = [28.0, 16.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8, font = myfont, alignment = 1, /enable_formatting)
   ;oText_8 = obj_new('IDLgrText', location = [22.0, 16.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8, font = myfont, alignment = 1, /enable_formatting)
   oText_8 = obj_new('IDLgrText', location = [22.0, 17.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8, font = myfont, alignment = 1, /enable_formatting)
   oText_9 = obj_new('IDLgrText', location = [22.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_9, font = myfont, alignment = 1, /enable_formatting)
   oText_10 = obj_new('IDLgrText', location = [22.0, 7.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_10, font = myfont, alignment = 1, /enable_formatting)
   oText_11 = obj_new('IDLgrText', location = [22.0, 2.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11, font = myfont, alignment = 1, /enable_formatting)
   
   ;NUEVO SUSANA INCLUYO TRACKOBJECTS NO CLASIFICADOS POR POCOS PUNTOS SEGUIDOS
   ;oText_21 = obj_new('IDLgrText', location = [22.0, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_21, font = myfont, alignment = 1, /enable_formatting)
   ;oText_21 = obj_new('IDLgrText', location = [22.0, -2.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_21, font = myfont, alignment = 1, /enable_formatting)
   ;;oText_23 = obj_new('IDLgrText', location = [2.0, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_23, font = myfont, /enable_formatting)
   ;oText_23 = obj_new('IDLgrText', location = [-3.0, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_23, font = myfont, /enable_formatting)
   ;oText_23 = obj_new('IDLgrText', location = [-3.0, -2.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_23, font = myfont, /enable_formatting)
   ;oText_27 = obj_new('IDLgrText', location = [14.0, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_27, font = myfont, alignment = 1, /enable_formatting)
   
   
   ;oText_filename=obj_new('IDLgrText', location = [-23.0, 19.5], XCoord_Conv=xs1, YCoord_Conv=ys1, filename, font = myfont, /enable_formatting)
;   oText_filename=obj_new('IDLgrText', location = [22.0, 19.5], XCoord_Conv=xs1, YCoord_Conv=ys1, nom, font = myfont, /enable_formatting)
   ;oText_filename=obj_new('IDLgrText', location = [2.0, -3.0], XCoord_Conv=xs1, YCoord_Conv=ys1, nom, font = myfont, /enable_formatting)
; MOR - add boxes with associated color for the text - END


      ;Text
;   oText_0 = obj_new('IDLgrText', location = [0.5, 5.5], string_0, font = myfont, /enable_formatting)
;   oText_1 = obj_new('IDLgrText', location = [0.5, 4.5], string_1, font = myfont, /enable_formatting)
;   oText_2 = obj_new('IDLgrText', location = [0.5, 3.5], string_2, font = myfont, /enable_formatting)
;   oText_3 = obj_new('IDLgrText', location = [0.5, 2.5], string_3, font = myfont, /enable_formatting)
;   oText_4 = obj_new('IDLgrText', location = [3.0, 5.5], string_4, font = myfont, alignment = 1, /enable_formatting)
;   oText_5 = obj_new('IDLgrText', location = [3.0, 4.5], string_5, font = myfont, alignment = 1, /enable_formatting)
;   oText_6 = obj_new('IDLgrText', location = [3.0, 3.5], string_6, font = myfont, alignment = 1, /enable_formatting)
;   oText_7 = obj_new('IDLgrText', location = [3.0, 2.5], string_7, font = myfont, alignment = 1, /enable_formatting)
;   oText_8 = obj_new('IDLgrText', location = [6.5, 5.5], string_8, font = myfont, alignment = 1, /enable_formatting)
;   oText_9 = obj_new('IDLgrText', location = [6.5, 4.5], string_9, font = myfont, alignment = 1, /enable_formatting)
;   oText_10 = obj_new('IDLgrText', location = [6.5, 3.5], string_10, font = myfont, alignment = 1, /enable_formatting)
;   oText_11 = obj_new('IDLgrText', location = [6.5, 2.5], string_11, font = myfont, alignment = 1, /enable_formatting)

   oModel = obj_new('IDLgrModel')
   oModel->remove, /all
   oModel->add, oText_0
   oModel->add, oText_1
   oModel->add, oText_2
   oModel->add, oText_3
   oModel->add, oText_4
   oModel->add, oText_5
   oModel->add, oText_6
   oModel->add, oText_7
   oModel->add, oText_8
   oModel->add, oText_9
   oModel->add, oText_10
   oModel->add, oText_11
   ;NUEVO SUSANA INCLUYO TRACKOBJECTS NO CLASIFICADOS POR POCOS PUNTOS SEGUIDOS
   ;oModel->add, oText_21
   ;oModel->add, oText_23
   ;oModel->add, oText_27
   ; MOR - include testname below the classification
   ;oModel->add, oText_filename
   
   ; MOR - add images of boxes to the model - BEGIN
   oModel->Add, oImage_0
   oModel->Add, oImage_1
   oModel->Add, oImage_2
   oModel->Add, oImage_3
   ; Susana - Coloco un box temporalmente del color de los NC (No Clasificados para Movilidad) - INICIO
   ;oModel->Add, oImage_60
   ; Susana - Coloco un box temporalmente del color de los NC (No Clasificados para Movilidad) - FIN
   ; MOR - add images of boxes to the model - END

   ;oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 8, 9], color = [255, 255, 255])
    oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 1.05, 1.05], color = [255, 255, 255])
   oView->remove, /all
   oView->add, oModel
   oBuffer->draw, oView

   oOImage = oBuffer->Read()
   oOImage->GetProperty, data = outImage_1

   outImage_1 = reverse(outimage_1, 3)
   write_tiff, filename, outImage_1
   ; MOR - destroy objects from memory
   obj_destroy, [oBuffer, oView]
   
end
