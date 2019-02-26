pro CEDAI_textPlotMotilOMS, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11, string_21, string_23, string_27, string_28, string_29, string_30,string_8_1,string_8_2,string_8_3,string_8_4,string_9_1,string_9_2,string_9_3,string_9_4,string_10_1,string_10_2,string_10_3,string_10_4,string_11_1,string_11_2,string_11_3,string_11_4,string_31,string_32

;SUSANA - Basada en CEDAI_textPlot  Incluye Movement Variables:
;      VAP promedio por grupo de motilidad ...
;      


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

  ;string_21 rotulo meanVAP
  ;string_23 rotulo meanVSL
  ;string_27 rotulo meanVCL
  ;string_28 LIN
  ;string_29 STR
  ;string_30 WOB
 
      ;Text Base
   oBuffer = obj_new('IDLgrBuffer', dimensions = [340, 130])
   myFontHeader = obj_new('IDLgrFont', 'arial', SIZE = 14)
   myFont = obj_new('IDLgrFont', 'arial', SIZE = 12)
   myFontUnits = obj_new('IDLgrFont', 'arial', SIZE = 9)
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
   ; blue - type IM
   boxB = bytarr(3, 2, 2)
   boxB[0,*,*] = 0
   boxB[1, *,*] = 0
   boxB[2,*,*] = 255
   ; green - type NP
   boxG = bytarr(3, 2, 2)
   boxG[0,*,*] = 0
   boxG[1, *,*] = 255
   boxG[2,*,*] = 0
   ; yellow - type B
   ;boxY = bytarr(3, 2, 2)
   ;boxY[0,*,*] = 255
   ;boxY[1, *,*] = 255
   ;boxY[2,*,*] = 0  
   ; red - type PR
   boxR = bytarr(3, 2, 2)
   boxR[0,*,*] = 255
   boxR[1, *,*] = 0
   boxR[2,*,*] = 0
     
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

   oImage_0 = obj_new('IDLgrImage',boxR,location = [-4.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1) ;susana mueve a la izquierda
   oImage_1 = obj_new('IDLgrImage',boxG,location = [-4.0, 7.3], XCoord_Conv=xs1, YCoord_Conv=ys1); ex boxY
   oImage_2 = obj_new('IDLgrImage',boxB,location = [-4.0, 2.6], XCoord_Conv=xs1, YCoord_Conv=ys1); ex boxG
   ;oImage_3 = obj_new('IDLgrImage',boxB,location = [-4.0, -2.1], XCoord_Conv=xs1, YCoord_Conv=ys1);ex boxB
   ; Susana - Coloco un box temporalmente del color de los NC (No Clasificados para Movilidad) - INICIO
   ;oImage_60 = obj_new('IDLgrImage',boxNC,location = [-0.8, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1)
   ; Susana - Coloco un box temporalmente del color de los NC (No Clasificados para Movilidad) - FIN
   ;string_32 %
   
   ;SUSANA - Types - INICIO
   ;oText_0 = obj_new('IDLgrText', location = [2.0, 14.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_0, font = myfont, /enable_formatting)
   oText_0 = obj_new('IDLgrText', location = [-1.8, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_0, font = myfont, /enable_formatting)
   oText_1 = obj_new('IDLgrText', location = [-1.8, 7.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_1, font = myfont, /enable_formatting)
   oText_2 = obj_new('IDLgrText', location = [-1.8, 2.6], XCoord_Conv=xs1, YCoord_Conv=ys1, string_2, font = myfont, /enable_formatting)
   oText_3 = obj_new('IDLgrText', location = [-1.8, -2.1], XCoord_Conv=xs1, YCoord_Conv=ys1, string_3, font = myfont, /enable_formatting)
   ;SUSANA - Types - FIN
   
   ;sRotulo = string_21+'* '+string_23+' '+string_27+' '+string_28+' '+string_29+' '+string_30;+STRING(13b)+'dd'+' '+string_31+' '+string_31+' '+string_31+' '+string_31+' '+string_31
   ;oText_VAP = obj_new('IDLgrText', location = [6.0, 18.0], XCoord_Conv=xs1, YCoord_Conv=ys1, sRotulo , font = myfont, /enable_formatting)
;   oText_VAP = obj_new('IDLgrText', location = [3.5, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_21 , font = myfontHeader, /enable_formatting)
;   oText_VCL = obj_new('IDLgrText', location = [8.0, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_27 , font = myfontHeader, /enable_formatting)
;   oText_VSL = obj_new('IDLgrText', location = [12.5, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_23 , font = myfontHeader, /enable_formatting)
;   oText_LIN = obj_new('IDLgrText', location = [17.0, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_28 , font = myfontHeader, /enable_formatting)
;   oText_STR = obj_new('IDLgrText', location = [20.5, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_29 , font = myfontHeader, /enable_formatting)
;   ;oText_WOB = obj_new('IDLgrText', location = [24.8, 18.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_30 , font = myfont, /enable_formatting)
;   oText_WOB = obj_new('IDLgrText', location = [25, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_30 , font = myfontHeader, /enable_formatting)
   oText_VAP = obj_new('IDLgrText', location = [6.0, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_21 , font = myfontHeader, alignment = 1,/enable_formatting)
   oText_VCL = obj_new('IDLgrText', location = [10.0, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_27 , font = myfontHeader, alignment = 1, /enable_formatting)
   oText_VSL = obj_new('IDLgrText', location = [14.0, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_23 , font = myfontHeader, alignment = 1, /enable_formatting)
   oText_LIN = obj_new('IDLgrText', location = [18.0, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_28 , font = myfontHeader, alignment = 1, /enable_formatting)
   oText_STR = obj_new('IDLgrText', location = [22, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_29 , font = myfontHeader, alignment = 1, /enable_formatting)
   ;oText_WOB = obj_new('IDLgrText', location = [24.8, 18.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_30 , font = myfont, /enable_formatting)
   oText_WOB = obj_new('IDLgrText', location = [26, 18.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_30 , font = myfontHeader, alignment = 1, /enable_formatting)
   
;   oText_VAPunits = obj_new('IDLgrText', location = [3.7, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_31 , font = myFontUnits,/enable_formatting)
;   oText_VCLunits = obj_new('IDLgrText', location = [7.6, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_31 , font = myFontUnits ,  /enable_formatting)
;   oText_VSLunits = obj_new('IDLgrText', location = [11.5, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_31 , font = myFontUnits ,/enable_formatting)
;   oText_LINunits = obj_new('IDLgrText', location = [15.4, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_32 , font = myFontUnits ,  /enable_formatting)
;   oText_STRunits = obj_new('IDLgrText', location = [19.3, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_32 , font = myFontUnits ,  /enable_formatting)
;   oText_WOBunits = obj_new('IDLgrText', location = [23.2, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_32 , font = myFontUnits ,  /enable_formatting)
   oText_VAPunits = obj_new('IDLgrText', location = [5.9, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_31 , font = myFontUnits, alignment = 1,/enable_formatting)
   oText_VCLunits = obj_new('IDLgrText', location = [9.9, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_31 , font = myFontUnits, alignment = 1,  /enable_formatting)
   oText_VSLunits = obj_new('IDLgrText', location = [13.9, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_31 , font = myFontUnits, alignment = 1 ,/enable_formatting)
   oText_LINunits = obj_new('IDLgrText', location = [18.1, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_32 , font = myFontUnits, alignment = 1 ,  /enable_formatting)
   oText_STRunits = obj_new('IDLgrText', location = [22.1, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_32 , font = myFontUnits, alignment = 1 ,  /enable_formatting)
   oText_WOBunits = obj_new('IDLgrText', location = [26.1, 15.5], XCoord_Conv=xs1, YCoord_Conv=ys1, string_32 , font = myFontUnits, alignment = 1 ,  /enable_formatting)
   
   
   ;SUSANA - VAP values - INICIO
   ;oText_4 = obj_new('IDLgrText', location = [7.8, 14.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_4, font = myfont, alignment = 1, /enable_formatting)
   oText_4 = obj_new('IDLgrText', location = [6.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_4, font = myfont, alignment = 1, /enable_formatting)
   oText_5 = obj_new('IDLgrText', location = [6.0, 7.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_5, font = myfont, alignment = 1, /enable_formatting)
   oText_6 = obj_new('IDLgrText', location = [6.0, 2.6], XCoord_Conv=xs1, YCoord_Conv=ys1, string_6, font = myfont, alignment = 1, /enable_formatting)
   oText_7 = obj_new('IDLgrText', location = [6.0, -2.1], XCoord_Conv=xs1, YCoord_Conv=ys1, string_7, font = myfont, alignment = 1, /enable_formatting)
   
   ;VCL
   ;oText_4_2 = obj_new('IDLgrText', location = [12.4, 14.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8_3, font = myfont, alignment = 1, /enable_formatting)
   oText_4_2 = obj_new('IDLgrText', location = [10.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8_3, font = myfont, alignment = 1, /enable_formatting)
   oText_5_2 = obj_new('IDLgrText', location = [10.0, 7.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_9_3, font = myfont, alignment = 1, /enable_formatting)
   oText_6_2 = obj_new('IDLgrText', location = [10.0, 2.6], XCoord_Conv=xs1, YCoord_Conv=ys1, string_10_3, font = myfont, alignment = 1, /enable_formatting)
   oText_7_2 = obj_new('IDLgrText', location = [10.0, -2.1], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11_3, font = myfont, alignment = 1, /enable_formatting)
   
   ;VSL
   ;oText_4_3 = obj_new('IDLgrText', location = [17.0, 14.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8_4, font = myfont, alignment = 1, /enable_formatting)
   oText_4_3 = obj_new('IDLgrText', location = [14.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8_4, font = myfont, alignment = 1, /enable_formatting)
   oText_5_3 = obj_new('IDLgrText', location = [14., 7.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_9_4, font = myfont, alignment = 1, /enable_formatting)
   oText_6_3 = obj_new('IDLgrText', location = [14.0, 2.6], XCoord_Conv=xs1, YCoord_Conv=ys1, string_10_4, font = myfont, alignment = 1, /enable_formatting)
   oText_7_3 = obj_new('IDLgrText', location = [14.0, -2.1], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11_4, font = myfont, alignment = 1, /enable_formatting)
   
   ;LIN
   ;oText_4_4 = obj_new('IDLgrText', location = [21.6, 14.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8, font = myfont, alignment = 1, /enable_formatting)
   oText_4_4 = obj_new('IDLgrText', location = [18.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8, font = myfont, alignment = 1, /enable_formatting)
   oText_5_4 = obj_new('IDLgrText', location = [18.0, 7.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_9, font = myfont, alignment = 1, /enable_formatting)
   oText_6_4 = obj_new('IDLgrText', location = [18.0, 2.6], XCoord_Conv=xs1, YCoord_Conv=ys1, string_10, font = myfont, alignment = 1, /enable_formatting)
   oText_7_4 = obj_new('IDLgrText', location = [18.0, -2.1], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11, font = myfont, alignment = 1, /enable_formatting)
   
   ;STR
   ;oText_4_5 = obj_new('IDLgrText', location = [26.2, 14.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8_1, font = myfont, alignment = 1, /enable_formatting)
   oText_4_5 = obj_new('IDLgrText', location = [22.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8_1, font = myfont, alignment = 1, /enable_formatting)
   oText_5_5 = obj_new('IDLgrText', location = [22.0, 7.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_9_1, font = myfont, alignment = 1, /enable_formatting)
   oText_6_5 = obj_new('IDLgrText', location = [22.0, 2.6], XCoord_Conv=xs1, YCoord_Conv=ys1, string_10_1, font = myfont, alignment = 1, /enable_formatting)
   oText_7_5 = obj_new('IDLgrText', location = [22.0, -2.1], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11_1, font = myfont, alignment = 1, /enable_formatting)
   
   ;WOB
   ;oText_4_6 = obj_new('IDLgrText', location = [30.8, 14.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8_2, font = myfont, alignment = 1, /enable_formatting)
   oText_4_6 = obj_new('IDLgrText', location = [26.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8_2, font = myfont, alignment = 1, /enable_formatting)
   oText_5_6 = obj_new('IDLgrText', location = [26.0, 7.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_9_2, font = myfont, alignment = 1, /enable_formatting)
   oText_6_6 = obj_new('IDLgrText', location = [26.0, 2.6], XCoord_Conv=xs1, YCoord_Conv=ys1, string_10_2, font = myfont, alignment = 1, /enable_formatting)
   ;oText_7_6 = obj_new('IDLgrText', location = [28.0, -1.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11_2, font = myfont, alignment = 1, /enable_formatting)
   oText_7_6 = obj_new('IDLgrText', location = [26.0, -2.1], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11_2, font = myfont, alignment = 1, /enable_formatting)
   
   ;SUSANA - VAP values - FIN
   
   
   ;oText_8 = obj_new('IDLgrText', location = [20.0, 14.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_8, font = myfont, alignment = 1, /enable_formatting)
   ;oText_9 = obj_new('IDLgrText', location = [20.0, 9.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_9, font = myfont, alignment = 1, /enable_formatting)
   ;oText_10 = obj_new('IDLgrText', location = [20.0, 4.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_10, font = myfont, alignment = 1, /enable_formatting)
   ;oText_11 = obj_new('IDLgrText', location = [20.0, -1.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_11, font = myfont, alignment = 1, /enable_formatting)
   
   ;;NUEVO SUSANA INCLUYO TRACKOBJECTS NO CLASIFICADOS POR POCOS PUNTOS SEGUIDOS
   ;oText_21 = obj_new('IDLgrText', location = [22.0, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_21, font = myfont, alignment = 1, /enable_formatting)
   ;oText_23 = obj_new('IDLgrText', location = [2.0, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_23, font = myfont, /enable_formatting)
   ;;oText_27 = obj_new('IDLgrText', location = [14.0, -3.3], XCoord_Conv=xs1, YCoord_Conv=ys1, string_27, font = myfont, alignment = 1, /enable_formatting)
   
; MOR - add boxes with associated color for the text - END

   oModel = obj_new('IDLgrModel')
   oModel->remove, /all
   oModel->add, oText_VAP
   oModel->add, oText_VSL
   oModel->add, oText_VCL
   oModel->add, oText_LIN
   oModel->add, oText_STR
   oModel->add, oText_WOB
   
   oModel->add, oText_VAPunits
   oModel->add, oText_VSLunits
   oModel->add, oText_VCLunits
   oModel->add, oText_LINunits
   oModel->add, oText_STRunits
   oModel->add, oText_WOBunits
   
   oModel->add, oText_0
   oModel->add, oText_1
   oModel->add, oText_2
   oModel->add, oText_3
   
   oModel->add, oText_4
   oModel->add, oText_5
   oModel->add, oText_6
   oModel->add, oText_7
   oModel->add, oText_4_2
   oModel->add, oText_5_2
   oModel->add, oText_6_2
   oModel->add, oText_7_2
   oModel->add, oText_4_3
   oModel->add, oText_5_3
   oModel->add, oText_6_3
   oModel->add, oText_7_3
   oModel->add, oText_4_4
   oModel->add, oText_5_4
   oModel->add, oText_6_4
   oModel->add, oText_7_4
   oModel->add, oText_4_5
   oModel->add, oText_5_5
   oModel->add, oText_6_5
   oModel->add, oText_7_5
   oModel->add, oText_4_6
   oModel->add, oText_5_6
   oModel->add, oText_6_6
   oModel->add, oText_7_6   
   
   
   ;oModel->add, oText_8
   ;oModel->add, oText_9
   ;oModel->add, oText_10
   ;oModel->add, oText_11
   ;;NUEVO SUSANA INCLUYO TRACKOBJECTS NO CLASIFICADOS POR POCOS PUNTOS SEGUIDOS
   ;oModel->add, oText_21
   ;oModel->add, oText_23
   ;;oModel->add, oText_27
   
   
   ; MOR - add images of boxes to the model - BEGIN
   oModel->Add, oImage_0
   oModel->Add, oImage_1
   oModel->Add, oImage_2
   ;oModel->Add, oImage_3
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
   obj_destroy, [oBuffer, oView,oModel,oOImage]
   
end
