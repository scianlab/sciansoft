;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ShiftXY
;
; PURPOSE:
;       - ShiftXY-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ShiftXY' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________


function C_sImageFilter_ShiftXY::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_ShiftXY::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    dimI = size(image, /dim)
    whParam = (where((*(*self.pParamStruct).pNames) eq 'x_Shift'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
         x_Shift = (((*(*self.pParamStruct).pValues)[whParam]  < (dimI[0]-2)) >  (-dimI[0]-2)) else x_Shift = 0
       (*(*self.pParamStruct).pValues)[whParam] = x_Shift
    whParam = (where((*(*self.pParamStruct).pNames) eq 'y_Shift'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
         y_Shift = (((*(*self.pParamStruct).pValues)[whParam]  < (dimI[1]-2)) >  (-dimI[1]-2)) else y_Shift = 0
       (*(*self.pParamStruct).pValues)[whParam] = y_Shift

    setBorder = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Set_Border_To_MinValue'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then setBorder = min(image)
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Set_Border_To_MaxValue'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then setBorder = max(image)
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Set_Border_To'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
       setBorder = (*(*self.pParamStruct).pValues)[whParam]

    if (x_Shift gt 0) then begin
       xIndizesOld = [0, dimI[0]-x_Shift-1]
       xIndizesNew = [x_Shift, dimI[0]-1]
    endif else begin
       xIndizesOld = [-x_Shift, dimI[0]-1]
       xIndizesNew = [0, dimI[0]+x_Shift-1]
    endelse

    if (y_Shift gt 0) then begin
       yIndizesOld = [0, dimI[1]-y_Shift-1]
       yIndizesNew = [y_Shift, dimI[1]-1]
    endif else begin
       yIndizesOld = [-y_Shift, dimI[1]-1]
       yIndizesNew = [0, dimI[1]+y_Shift-1]
    endelse

    returnImage = make_array(dimI[0], dimI[1], type = size(image, /type)) + setBorder
    returnImage[xIndizesNew[0]:xIndizesNew[1], yIndizesNew[0]:yIndizesNew[1]] = image[xIndizesOld[0]:xIndizesOld[1], yIndizesOld[0]:yIndizesOld[1]]
    return, returnImage
end


function C_sImageFilter_ShiftXY::init

    filterStruct = {Name: 'C_ShiftXY',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pImageFilterDataIn: ptr_new(),$     ; Pointer on original Data.
                           pImageFilterDataOut:ptr_new(),$       ; Pointer on filtered Data.
                           pNames: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$       ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

       ; Parameters of sMeanImage.
    filterParamWidgetType = make_array(5, /string, value = 'widget_slider')

    filterParamNames = ['x_Shift',$       ;     Pixel position of the X ShiftXY position.
                        'y_Shift',$
                        'Set_Border_To_MaxValue',$
                        'Set_Border_To_MinValue',$
                        'Set_Border_To' ]          ;  Pixel position of the Y ShiftXY position.

    filterParamActive = [  1,$
                           1,$
                           0,$
                           1,$
                           1]

    filterParamMin = [ -1000,$
                           -1000,$
                           0,$
                           0,$
                           0]

    filterParamMax = [      10000,$
                              10000,$
                              10000,$
                              10000,$
                              10000]

    filterParamValues = [ 0,$
                              0,$
                              0,$
                              0,$
                             0.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ShiftXY__define
  tmp = {C_sImageFilter_ShiftXY, pParamStruct: ptr_new(), inherits C_sImageFilter}
end