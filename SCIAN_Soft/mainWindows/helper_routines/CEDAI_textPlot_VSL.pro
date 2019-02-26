pro CEDAI_textPlotVSL, filename, string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10, string_11,string_12, string_13

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
;        Filename = 'C:\RSI\Phil\Text.tiff'

      ;Text Base
   oBuffer = obj_new('IDLgrBuffer', dimensions = [410, 180])
   ;oBuffer = obj_new('IDLgrBuffer', dimensions = [600, 600])
   myFont = obj_new('IDLgrFont', 'arial', SIZE = 15)

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
    
    if n_elements(string_11) eq 0 then string_12 = ''
    if n_elements(string_11) eq 0 then string_13 = ''

      ;Text
   oText_0 = obj_new('IDLgrText', location = [0.5, 5.5], string_0, font = myfont, /enable_formatting)
   oText_1 = obj_new('IDLgrText', location = [0.5, 4.5], string_1, font = myfont, /enable_formatting)
   oText_2 = obj_new('IDLgrText', location = [0.5, 3.5], string_2, font = myfont, /enable_formatting)
   oText_3 = obj_new('IDLgrText', location = [0.5, 2.5], string_3, font = myfont, /enable_formatting)
   oText_4 = obj_new('IDLgrText', location = [3.0, 5.5], string_4, font = myfont, alignment = 1, /enable_formatting)
   oText_5 = obj_new('IDLgrText', location = [3.0, 4.5], string_5, font = myfont, alignment = 1, /enable_formatting)
   oText_6 = obj_new('IDLgrText', location = [3.0, 3.5], string_6, font = myfont, alignment = 1, /enable_formatting)
   oText_7 = obj_new('IDLgrText', location = [3.0, 2.5], string_7, font = myfont, alignment = 1, /enable_formatting)
   oText_8 = obj_new('IDLgrText', location = [6.5, 5.5], string_8, font = myfont, alignment = 1, /enable_formatting)
   oText_9 = obj_new('IDLgrText', location = [6.5, 4.5], string_9, font = myfont, alignment = 1, /enable_formatting)
   oText_10 = obj_new('IDLgrText', location = [6.5, 3.5], string_10, font = myfont, alignment = 1, /enable_formatting)
   oText_11 = obj_new('IDLgrText', location = [6.5, 2.5], string_11, font = myfont, alignment = 1, /enable_formatting)
   
   oText_12 = obj_new('IDLgrText', location = [2.4, 7.5], string_12, font = myfont, alignment = 1, /enable_formatting)
   oText_13 = obj_new('IDLgrText', location = [3.5, 7.5], string_13, font = myfont, alignment = 1, /enable_formatting)

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
   
   oModel->add, oText_12
   oModel->add, oText_13

   oView = obj_new('IDLgrView', viewplane_rect = [0, 0, 8, 9], color = [255, 255, 255])
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