;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_sMeanImage
;
; PURPOSE:
;       - sMeanImage-Filter-Class. See sMeanImage.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_sMeanImage' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_sMeanImage::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_sMeanImage::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   image *= 1.
   pix_rad = fix(round((*(*self.pParamStruct).pValues)[0]))

   return, sMeanImage(image, pix_rad,$
                      Expand_Mirror = (*(*self.pParamStruct).pActive)[1],$
                      Expand_Congrid_Method = (*(*self.pParamStruct).pActive)[2],$
                      Expand_Congrid_Near = (*(*self.pParamStruct).pActive)[3],$
                      Expand_Congrid_Interp = (*(*self.pParamStruct).pActive)[4],$
                      Expand_Congrid_Cubic = (*(*self.pParamStruct).pValues)[5])
end


function C_sImageFilter_sMeanImage::init

    filterStruct = {Name: 'C_sMeanImage',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of sMeanImage.
    filterParamWidgetType = make_array(6, /string, value = 'widget_slider')

    filterParamNames = ['Pixel_Radius',$            ;    Radius ( PixelSize ) for which mean values are calculated.
                    'Expand_Congrid_Cubic',$      ;   Forces cubic convolution interpolation parameter [-1,0] (Default = -0.5).
                    'Expand_Mirror',$             ;   Set this keyword to  expand 2D-Image Data by CONGRID routine (DEFAULT, 0 OFF/1 ON).
                    'Expand_Congrid',$               ;  Set this keyword to  expand 2D-Image Data by CONGRID routine (0 OFF/1 ON).
                    'Expand_Congrid_Near',$       ;        Forces  nearest-neighbor sampling (DEFAULT, 0 OFF/1 ON).
                    'Expand_Congrid_Interp']       ;        Forces linear interpolation when resizing a 1- or 2-dimensional array (0 OFF/1 ON).

    filterParamActive = [   1,$    ;  Pixel_Radius (always active)
                              0,$    ; Expand_Congrid_Cubic
                              0,$    ; Expand_Congrid
                              1,$    ; Expand_Congrid_Near (default acitve)
                              0,$    ; Expand_Congrid_Interp
                              1]   ;  Expand_Mirror (default acitve)

    filterParamMin = [         1.,$  ;  Pixel_Radius
                              -1.,$  ; Expand_Congrid_Cubic
                              0.,$   ; Expand_Congrid
                              0.,$   ; Expand_Congrid_Near (default acitve)
                              0.,$   ; Expand_Congrid_Interp
                              0.] ; Expand_Mirror

    filterParamMax = [    100.,$   ;  Pixel_Radius
                             0.,$    ; Expand_Congrid_Cubic
                             1.,$    ; Expand_Congrid
                             1.,$    ; Expand_Congrid_Near (default acitve)
                             1.,$    ; Expand_Congrid_Interp
                             1.]   ; Expand_Mirror

    filterParamValues = [     1.,$    ;  Pixel_Radius
                           -0.5,$   ; Expand_Congrid_Cubic
                             0.,$    ; Expand_Congrid
                             0.,$    ; Expand_Congrid_Near (default acitve)
                             0.,$    ; Expand_Congrid_Interp
                             0.]   ;  Expand_Mirror

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_sMeanImage__define
  tmp = {C_sImageFilter_sMeanImage, pParamStruct: ptr_new(), inherits C_sImageFilter}
end