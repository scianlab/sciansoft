pro CEDAI_textPlotOMS_Conc, filename, string_3, string_7, string_11,nom

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
   ;oBuffer = obj_new('IDLgrBuffer', dimensions = [340, 50]) ;default
   oBuffer = obj_new('IDLgrBuffer', dimensions = [340, 35]) 
   myFont = obj_new('IDLgrFont', 'arial', SIZE = 15)
   ; MOR - dummy to get range - BEGIN
   dummy = obj_new('IDLgrPlot',  xRange = [0,25], yRange = [0,25], dataY = [0, 0], color = [0, 0, 0]); default
   ;dummy = obj_new('IDLgrPlot',  xRange = [0,25], yRange = [0,25], dataY = [0, 0], color = [0, 0, 0]); xRange e yRange son espaciados
   
   ; MOR - dummy to get range - BEGIN
   
    if n_elements(string_3) eq 0 then string_3 = ''
    if n_elements(string_7) eq 0 then string_7 = ''
    if n_elements(string_11) eq 0 then string_11 = ''
   
          ; Because we may not be using exact axis ranging, the axes
    ; may extend further than the xrange and yrange. Get the
    ; actual axis range so that the plot, etc. can be scaled
    ; appropriately.
    position = [0.1, 0.1, 0.95, 0.95]
    ;position = [0.1, 0.05, 0.95, 0.95]
    dummy->GetProperty, XRange=xrange
    dummy->GetProperty, YRange=yrange

    ; Set up the scaling so that the axes for the plot and the
    ; plot data extends from 0->1 in the X and Y directions.

    xs1 = FSC_Normalize(xrange, Position = [position[0], position[2]])
    ys1 = FSC_Normalize(yrange, Position = [position[1], position[3]])

    ; Scale the plot data and axes into 0->1.

   ;oText_3 = obj_new('IDLgrText', location = [6.0, 7.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_3, font = myFont, alignment = 1, /enable_formatting)
   oText_3 = obj_new('IDLgrText', location = [6.0, 9.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_3, font = myFont, alignment = 1, /enable_formatting)
   oText_7 = obj_new('IDLgrText', location = [13.0, 9.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_7, font = myFont, alignment = 1, /enable_formatting)
   oText_11 = obj_new('IDLgrText', location = [22.0, 9.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11, font = myFont, alignment = 1, /enable_formatting); concentration hacia la izquierda
   
; MOR - add boxes with associated color for the text - END

   oModel = obj_new('IDLgrModel')
   oModel->remove, /all
;   oModel->add, oText_0
;   oModel->add, oText_1
;   oModel->add, oText_2
   oModel->add, oText_3
;   oModel->add, oText_4
;   oModel->add, oText_5
;   oModel->add, oText_6
   oModel->add, oText_7
;   oModel->add, oText_8
;   oModel->add, oText_9
;   oModel->add, oText_10
   oModel->add, oText_11
   ;NUEVO SUSANA INCLUYO TRACKOBJECTS NO CLASIFICADOS POR POCOS PUNTOS SEGUIDOS
   ;oModel->add, oText_21
   ;oModel->add, oText_23
   ;oModel->add, oText_27
   ; MOR - include testname below the classification
   ;oModel->add, oText_filename
   
;   ; MOR - add images of boxes to the model - BEGIN
;   oModel->Add, oImage_0
;   oModel->Add, oImage_1
;   oModel->Add, oImage_2
;   ;oModel->Add, oImage_3
   
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
