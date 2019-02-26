;_____________________________IOISIOI_____________________________
; NAME:
;  C_sImageFilter_IsoAlpha
;
; PURPOSE:
;  IsoAlpha-Filter-Class.
;
; AUTHOR:
;  loyarzo (2003), e_mail: loyarzo@gmx.net
;  jjaraw (2017) - Modified to allow larger pixel-radius values within image size bounds.
;
; CALLING SEQUENCE:
;  result = obj_new('C_sImageFilter_IsoAlpha')
;
; METHODS:
;_____________________________IOISIOI_____________________________

function C_sImageFilter_IsoAlpha::getImageFilterType
  return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_IsoAlpha::apply, image = image

  dimI   = size(image, /dim)
  maxRad = (dimI[0] > dimI[1]); / 2 ; Limit value for pixRad1, pixRad2

  whParam = (where((*(*self.pParamStruct).pNames) eq 'PixRad1'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) $
  then pixRad1 = (round((*(*self.pParamStruct).pValues)[whParam]) > 0) < maxRad $
  else pixRad1 = 0

  whParam = (where((*(*self.pParamStruct).pNames) eq 'PixRad2'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) $
  then pixRad2 = round((*(*self.pParamStruct).pValues)[whParam]) > (pixRad1+1) $
  else pixRad2 = 1 > (pixRad1+1)

  whParam = (where((*(*self.pParamStruct).pNames) eq 'IntensityRad1'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) $
  then IntensityRad1 = round((*(*self.pParamStruct).pValues)[whParam]) > 0 $
  else IntensityRad1 = 0

  whParam = (where((*(*self.pParamStruct).pNames) eq 'IntensityRad2'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) $
  then IntensityRad2 = round((*(*self.pParamStruct).pValues)[whParam]) > IntensityRad1 $
  else IntensityRad2 = 0 > IntensityRad1

  normI    = 1. * image
  distance = fltArr(dimI[0], dimI[1]) ; Distance Matrix.
  alphan1  = distance + 1.            ; Matrix for rad1.
  alphan2  = alphan1                  ; Matrix for rad2.

  expImage = s_Expand_Mirror(image, pixRad2)
  r1Max = sqrt(pixRad1^2 + IntensityRad1^2)
  r2Max = sqrt(pixRad2^2 + IntensityRad2^2)

  for k = -pixRad2, pixRad2 do $
  for l = -pixRad2, pixRad2 do begin
    pixDist = 1.0 * (k*k + l*l)
    if (pixDist le (pixRad2)^2 ) and (pixDist ne 0) then begin
      distance = sqrt(((expImage[pixRad2+k : pixRad2+k+dimI[0]-1, pixRad2+l : pixRad2+l+dimI[1]-1] - normI)^2) + pixDist)
;      intensityAbs = (expImage[pixRad2+k : pixRad2+k+dimOri[0]-1, pixRad2+l : pixRad2+l+dimOri[1]-1] - normOri)^2
;      distance = sqrt(intensityAbs + pixelAbs)
      alphan1 = temporary(alphan1) + (distance le r1Max)
      alphan2 = temporary(alphan2) + (distance le r2Max)
    endif
  endfor
  return, alog(alphan2/alphan1) / alog((r2Max>2)/((r1Max>1)))
end


function C_sImageFilter_IsoAlpha::init

  imageFilterStruct = {Name       :'C_IsoAlpha',$ ; Filter Name.
                       pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                       pNames     : ptr_new(),$ ; Pointer on Filter Parameter Names.
                       pActive    : ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
                       pMin       : ptr_new(),$ ; Pointer on Filter Parameter Min Values.
                       pMax       : ptr_new(),$ ; Pointer on Filter Parameter Max Values.
                       pValues    : ptr_new()}  ; Pointer on Filter Parameter Values.

  filterParamWidgetType = make_array(4, /string, value = 'widget_slider')
  filterParamNames = ['PixRad1',$       ; Pixel radius 1.
                      'PixRad2',$       ; Pixel radius 2.
                      'IntensityRad1',$ ; Color in Units radius 1.
                      'IntensityRad2']  ; Color in Units radius 2.

  filterParamActive = [1, 1,  0, 0]
  filterParamMin    = [0, 1., 0, 1.]
  filterParamMax    = [499.,500.,99.,100.]
  filterParamValues = [1.,2., 0, 0]

  imageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  imageFilterStruct.pNames  = ptr_new(filterParamNames, /no_copy)
  imageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
  imageFilterStruct.pMin    = ptr_new(filterParamMin, /no_copy)
  imageFilterStruct.pMax    = ptr_new(filterParamMax, /no_copy)
  imageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct = ptr_new(imageFilterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_IsoAlpha__define
  tmp = {C_sImageFilter_IsoAlpha, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
