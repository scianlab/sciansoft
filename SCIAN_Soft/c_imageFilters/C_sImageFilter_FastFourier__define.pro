;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_FastFourier
;
; PURPOSE:
;       - sFastFourier-Filter-Class. See sMeanImage.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;    modified by FASL 2011
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_FastFourier' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_FastFourier::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

; XXX FASL 2012 ... some tropuble in this function... Now.. i dont have time to check...
; FASL 2011, function needs an image non an *PImage
;Parameters for sMeanImage havent an Expand_Congrid component.... assume Expand_Congrid_Method
function C_sImageFilter_FastFourier::apply, image = image

       ;(0) 'Pixel_Radius'
    pix_rad = (*(*self.pParamStruct).pValues)[0]

    ; rare structure... first Parameters are the Active State, but the last is the Value State.... ?
    return, sMeanImage(image, pix_rad,$
                            Expand_Mirror =         (*(*self.pParamStruct).pActive)[1] * (*(*self.pParamStruct).pValues)[1],$
                            Expand_Congrid_Method = (*(*self.pParamStruct).pActive)[2] * (*(*self.pParamStruct).pValues)[2],$
                            Expand_Congrid_Near =   (*(*self.pParamStruct).pActive)[3] * (*(*self.pParamStruct).pValues)[3],$
                            Expand_Congrid_Interp = (*(*self.pParamStruct).pActive)[4] * (*(*self.pParamStruct).pValues)[4],$
                            Expand_Congrid_Cubic =  (*(*self.pParamStruct).pActive)[5] * (*(*self.pParamStruct).pValues)[5] $
                            )
end


function C_sImageFilter_FastFourier::init

    filterStruct = {Name: 'C_FastFourier',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$       ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new(),$     ; Pointer on Filter Parameter Values.
                           pImageFilterInputData: ptr_new(),$     ; Pointer on Filter Input Data(Image).
                           pImageFilterOutputData: ptr_new(),$       ; Pointer on Filter Output Data(Image).
                           pImageFilterPowerSpectrum: ptr_new()}  ; Pointer on Filter Filter Power-Spectrum.

       ; Parameters of sMeanImage.
    filterParamWidgetType = ['widget_slider']

    filterParamNames = ['F1_Filter_Function',$         ;    FFT_Filter_Function_List
                    'F1_Butterworth',$           ;   Butterworth-Filter
                    'F1_Exponential',$           ;   Exponential-Filter
                    'F1_Ideal',$             ;     Ideal-Filter
                    'F2_Order',$     ;      Order of Filter-function
                    'F3_CutOff_Type',$     ;      FFT_Filter CutOffType
                    'F3_Low_Pass',$       ;        Filters High Frequency Noise
                    'F3_High_Pass',$    ;    Filters Low Frequency Noise
                    'F3_Band_Width_Pass',$       ;    Filters  Outer-Band Noise
                    'F3_Band_Width_Reject',$
                    'F3_Band_Pass_Pass',$          ;     Filters  Exactly-Outer-Band Noise
                    'F3_Band_Pass_Reject',$
                    'F4_Expand_Image',$       ;        Expands/Extrapotates Image 10 % to evoid border effects
                    'S1_1st_CutOff_Frequency',$        ;   Selects 1st_CutOff_Frequency
                    'S2_2nd_CutOff_Frequency',$        ;  Selects 2nd_CutOff_Frequency
                    'S3_Band_Frequency' ]         ;  Selects Band_Frequency

    filterParamActive = [     1,    $  ;  F1_Filter_Function (always active)
                           1,    $  ; F1_Butterworth (default active)
                           0,    $  ; F1_Exponential
                           0,    $  ; F1_Ideal
                           1,    $  ; F2_Order (always active)
                           1,    $  ; F3_CutOff_Type (always active)
                           0,    $  ; F3_Low_Pass
                           0,    $  ; F3_High_Pass
                           0,    $  ; F3_Band_Width_Pass
                           0,    $  ; F3_Band_Width_Reject
                           1,    $  ; F3_Band_Pass_Pass  (default active)
                           0,    $  ; F3_Band_Pass_Reject
                           0,    $  ; F4_Expand_Image (default passive)
                           1,    $  ; S1_1st_CutOff_Frequency
                           1,    $  ; S2_2nd_CutOff_Frequency
                           1]       ; S3_Band_Frequency

    filterParamMin = [     0,$   ;  F1_Filter_Function (always)
                           0,    $  ; F1_Butterworth (always)
                           0,    $  ; F1_Exponential (always)
                           0,    $  ; F1_Ideal  (always)
                           1,    $  ; F2_Order
                           0,    $  ; F3_CutOff_Type (always)
                           0,    $  ; F3_Low_Pass (always)
                           0,    $  ; F3_High_Pass (always)
                           0,    $  ; F3_Band_Width_Pass (always)
                           0,    $  ; F3_Band_Width_Reject (always)
                           0,    $  ; F3_Band_Pass_Pass (always)
                           0,    $  ; F3_Band_Pass_Reject (always)
                           0,    $  ; F4_Expand_Image (always)
                           1,    $  ; S1_1st_CutOff_Frequency
                           1,    $  ; S2_2nd_CutOff_Frequency
                           1]       ; S3_Band_Frequency

    filterParamMax = [     1,$   ;  F1_Filter_Function (always) ; J.Scheer changed to 1
                           0,    $  ; F1_Butterworth (always)
                           0,    $  ; F1_Exponential (always)
                           0,    $  ; F1_Ideal  (always)
                           30,   $ ; F2_Order
                           0,    $  ; F3_CutOff_Type (always)
                           0,    $  ; F3_Low_Pass (always)
                           0,    $  ; F3_High_Pass (always)
                           0,    $  ; F3_Band_Width_Pass (always)
                           0,    $  ; F3_Band_Width_Reject (always)
                           0,    $  ; F3_Band_Pass_Pass (always)
                           0,    $  ; F3_Band_Pass_Reject (always)
                           0,    $  ; F4_Expand_Image (always)
                           1,    $  ; S1_1st_CutOff_Frequency (must be derived from image dimensions)
                           1,    $  ; S2_2nd_CutOff_Frequency (must be derived from image dimensions)
                           1]       ; S3_Band_Frequency (must be derived from image dimensions)

    filterParamValues = [   0,$    ;  F1_Filter_Function (always)
                           0,    $  ; F1_Butterworth (always)
                           0,    $  ; F1_Exponential (always)
                           0,    $  ; F1_Ideal  (always)
                           5,    $  ; F2_Order
                           0,    $  ; F3_CutOff_Type (always)
                           0,    $  ; F3_Low_Pass (always)
                           0,    $  ; F3_High_Pass (always)
                           0,    $  ; F3_Band_Width_Pass (always)
                           0,    $  ; F3_Band_Width_Reject (always)
                           0,    $  ; F3_Band_Pass_Pass (always)
                           0,    $  ; F3_Band_Pass_Reject (always)
                           0,    $  ; F4_Expand_Image (always)
                           2,    $  ; S1_1st_CutOff_Frequency
                           2,    $  ; S2_2nd_CutOff_Frequency
                           2]       ; S3_Band_Frequency


    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_FastFourier__define
  tmp = {C_sImageFilter_FastFourier, pParamStruct: ptr_new(), inherits C_sImageFilter}
end