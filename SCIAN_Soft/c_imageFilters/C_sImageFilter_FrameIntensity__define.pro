;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_FrameIntensity
;
; PURPOSE:
;       - BorderDistance-Filter-Class. See BorderDistance.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_FrameIntensity' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_FrameIntensity::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_FrameIntensity::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Frame Intensity'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then frameInt = (*(*self.pParamStruct).pValues)[whParam] else frameInt = 0b

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Frame Size [pixel]'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then frameSize = (*(*self.pParamStruct).pValues)[whParam] else frameSize = 0b

   if (frameSize eq 0) then return, image

   dimImage = size(image, /dim)

   frameSize = frameSize < ( dimImage[0] < dimImage[1])
   (*(*self.pParamStruct).pValues)[whParam] = frameSize

   image[0:(frameSize-1), 0:(dimImage[1]-1)] = frameInt
   image[(dimImage[0]-(frameSize-1))-1:dimImage[0]-1, 0:(dimImage[1]-1)] = frameInt

   image[0:(dimImage[0]-1), 0:(frameSize-1)] = frameInt
   image[0:(dimImage[0]-1), (dimImage[1]-(frameSize-1))-1:dimImage[1]-1] = frameInt

   return, image
end


function C_sImageFilter_FrameIntensity::init

    filterStruct = {Name: 'C_FrameIntensity',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of BorderDistance.
    filterParamWidgetType = make_array(2, /string, value = 'widget_slider')
    filterParamNames = ['Frame Size [pixel]', 'Frame Intensity']
    filterParamActive = [1,1]
    filterParamMin = [0,0]
    filterParamMax = [100,255]
    filterParamValues = [1,0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_FrameIntensity__define
  tmp = {C_sImageFilter_FrameIntensity, pParamStruct: ptr_new(), inherits C_sImageFilter}
end