pro CEDAI_textPlot_Count_short, filename, string_0, string_1, string_2, string_3, string_4, string_5

DEVICE,DECOMPOSED=0

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
   oBuffer = obj_new('IDLgrBuffer', dimensions = [300, 100])
   myFontHeader = obj_new('IDLgrFont', 'arial', SIZE = 15)
   myFont = obj_new('IDLgrFont', 'arial', SIZE = 16)
   ; MOR - dummy to get range - BEGIN
   dummy = obj_new('IDLgrPlot',  xRange = [0,25], yRange = [0,25], dataY = [0, 0], color = [0, 0,0])
   ; MOR - dummy to get range - END
   
    if n_elements(string_0) eq 0 then string_0 = ''
    if n_elements(string_1) eq 0 then string_1 = ''
    if n_elements(string_2) eq 0 then string_2 = ''
    if n_elements(string_3) eq 0 then string_3 = ''
    if n_elements(string_4) eq 0 then string_4 = ''
    if n_elements(string_5) eq 0 then string_5 = ''
  
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
    ; title 
 ;  oText_0 = obj_new('IDLgrText', location = [0.0, 22.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_0, font = myfontHeader, alignment = 0, /enable_formatting)
   ; stats name
   oText_1 = obj_new('IDLgrText', location = [0.0, 19.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_1, font = myfont, alignment = 0, /enable_formatting)
   oText_2 = obj_new('IDLgrText', location = [0.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_2, font = myfont, alignment = 0, /enable_formatting)
   ; stats
   oText_3 = obj_new('IDLgrText', location = [15.0, 19.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_3, font = myfont, alignment = 1, /enable_formatting)
   oText_4 = obj_new('IDLgrText', location = [15.0, 12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_4, font = myfont, alignment = 1, /enable_formatting)
   
   oText_5 = obj_new('IDLgrText', location = [26.0,19.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_5, font = myfont, alignment = 1, /enable_formatting)
   oText_6 = obj_new('IDLgrText', location = [26.0,12.0], XCoord_Conv=xs1, YCoord_Conv=ys1, string_5, font = myfont, alignment = 1, /enable_formatting)
   

   oModel = obj_new('IDLgrModel')
   oModel->remove, /all
  ; oModel->add, oText_0
   oModel->add, oText_1
   oModel->add, oText_2
   oModel->add, oText_3
   oModel->add, oText_4
   oModel->add, oText_5
   oModel->add, oText_6


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