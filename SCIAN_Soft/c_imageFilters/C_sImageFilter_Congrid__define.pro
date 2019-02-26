;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Congrid
;
; PURPOSE:
;       - Congrid-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Congrid' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_Congrid::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Congrid::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    dimI = size (image, /dim)
    newXSize = dimI[0]
    newYSize = dimI[1]

    whParam = (where((*(*self.pParamStruct).pNames) eq 'New_XDim'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then $
       newXSize = 2 > (*(*self.pParamStruct).pValues)[whParam]
    whParam = (where((*(*self.pParamStruct).pNames) eq 'New_YDim'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then $
       newYSize = 2 > (*(*self.pParamStruct).pValues)[whParam]

    whParam = (where((*(*self.pParamStruct).pNames) eq 'XY_Factor'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then begin
       factor = (*(*self.pParamStruct).pValues)[whParam]
       newXSize = round(dimI[0] * factor) > 2
       newYSize = round(dimI[1] * factor) > 2
    endif

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Nearest_Neighbor'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then $
       return, congrid(image, newXSize, newYSize , /minus_one)

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Linear_Interpolation'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then $
       return, congrid(image, newXSize, newYSize, /interp , /minus_one)

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Cubic_Interpolation'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then begin
       cubic = 0. < (*(*self.pParamStruct).pValues)[whParam]
       return, congrid(image, newXSize, newYSize, cubic = cubic , /minus_one)
    endif

    return, congrid(image, newXSize, newYSize , /minus_one)
end


function C_sImageFilter_Congrid::init

    filterStruct = {Name: 'C_Congrid',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pImagefilterParamType: ptr_new(),$      ; Pointer on Filter Parameter Type (Bool, Byte, Int, Long, Float, Double).
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$       ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$   ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}     ; Pointer on Filter Parameter Values.

       ; Parameters of sMeanImage.
    filterParamWidgetType = make_array(6, /string, value = 'widget_slider')

    filterParamNames = ['New_XDim',$   ;   New X-Dimension
                    'New_YDim',$      ;   New Y-Dimension
                    'XY_Factor',$       ;  New Y-Dimension
                    'Nearest_Neighbor',$     ;    Forces  nearest-neighbor sampling (DEFAULT, 0 OFF/1 ON).
                    'Linear_Interpolation',$     ;    Forces linear interpolation when resizing a 1- or 2-dimensional array (0 OFF/1 ON).
                    'Cubic_Interpolation' ]     ;  Forces cubic convolution interpolation parameter [-1,0] (Default = -0.5).

    filterParamType = ['Long',$
                        'Long',$
                        'Long',$
                        'Bool',$
                        'Bool',$
                        'Float' ]

    filterParamActive = [   0,$
                              0,$
                              1,$
                              1,$
                              0,$
                              0]

    filterParamMin = [       1.,$
                              1.,$
                              0.,$
                              0.,$
                              0.,$
                              -1.]

    filterParamMax = [      10000,$
                              10000,$
                              10,$
                              1,$
                              1,$
                              0]

    filterParamValues = [  50.,$
                              50.,$
                              1.5,    $
                              0.,$
                              0.,$
                              -.5]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pImagefilterParamType = ptr_new(filterParamType, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Congrid__define
  tmp = {C_sImageFilter_Congrid, pParamStruct: ptr_new(), inherits C_sImageFilter}
end