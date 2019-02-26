;_____________________________IOISIOI____________________
; NAME:
;   C_sImageFilter_FrangiVessel
;
; PURPOSE:
;   FrangiVessel-Filter-Class.
;
; AUTHOR:
;   Hector Moraga (2011)
;   e_mail: hmoraga@med.uchile.cl
;
; CALLING SEQUENCE:
;   result = obj_new('C_sImageFilter_FrangiVessel')
;
; METHODS:
;
; HISTORY:
;
; Matlab version written by Marc Schrijver, 2/11/2001.
; Re-Written by D.Kroon University of Twente (May 2009)
; IDL version written by Hector Moraga, 2011.
;_____________________________IOISIOI____________________

function C_sImageFilter_FrangiVessel::getImageFilterType
  return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_FrangiVessel::getImageFilterDevelopmentalState
  ;return, 'Release_Image_Filter_Method'
  ;return, 'Alpha_Debugging_Filter_Method'
  return, 'Beta_Release_Filter_Method'
end


; Parameters of C_sImageFilter_FrangiVessel
;
;    Scale Range Min : The range of sigmas used, default 1.
;    Scale Range Max : The range of sigmas used, default 8.
;    Scale Ratio     : Step size between sigmas, default 2.
;    Beta            : Frangi correction constant, default 0.5.
;    C               : Frangi correction constant, default 15.
;    Black/White     : Detect black ridges (default) set to true,
;                      for white ridges set to false.
;    verbose         : Show debug information, default true.
;
;    defaultoptions = struct('FrangiScaleRange', [1 20], 'FrangiScaleRatio', 2, 'FrangiAlpha', 0.5, 'FrangiBeta', 15, 'verbose', true, 'BlackWhite', true);
function C_sImageFilter_FrangiVessel::apply, image = image

  if (size(image, /n_dim) ne 2) then return, image

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Calculate'))[0]
  self.calcular =(*(*self.pParamStruct).pValues)[whParam] 

  whParam  = (where((*(*self.pParamStruct).pNames) eq 'Scale Range Min'))[0]
  rangeMin = (*(*self.pParamStruct).pValues)[whParam]
  whParam  = (where((*(*self.pParamStruct).pNames) eq 'Scale Range Max'))[0]
  rangeMax = (*(*self.pParamStruct).pValues)[whParam]
  self.scaleRangeMin = rangeMin < rangeMax
  self.scaleRangeMax = rangeMin > rangeMax

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Scale Ratio'))[0]
  self.scaleRatio =(*(*self.pParamStruct).pValues)[whParam]

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Beta'))[0]
  self.betaUno =(*(*self.pParamStruct).pValues)[whParam]

  whParam = (where((*(*self.pParamStruct).pNames) eq 'C'))[0]
  self.c =(*(*self.pParamStruct).pValues)[whParam]

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Gamma'))[0]
  self.gammaUno =(*(*self.pParamStruct).pValues)[whParam]

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Black/White'))[0]
  self.bw =(*(*self.pParamStruct).pValues)[whParam]

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Verbose'))[0]
  self.verbose =(*(*self.pParamStruct).pValues)[whParam]

  if (self.calcular eq 0) then return, image

  self->calculate, image, outIm = outIm, whatScale = whatScale, Direction = Direction
  ; Rescale output to [0, 255].
  minI = min(outIm, max = maxI)
  if (minI ne maxI) then outIm = (outIm-minI) * 255d / (maxI-minI)
  return, outIm

end


pro C_sImageFilter_FrangiVessel::calculate, image, outIm = outIm, whatScale = whatScale, Direction = Direction

  dimI   = size(image, /dim)
  sigmas = s_dynamicRangeArray(min = self.scaleRangeMin, step = self.scaleRatio, max = self.scaleRangeMax, dType = 2)
  beta   = self.betaUno
  c      = self.c
  gamma  = self.gammaUno

  ; Make matrices to store all filtered images.
  ALLfiltered = dblArr(dimI[0], dimI[1], n_elements(sigmas))
  ALLangles = dblArr(dimI[0], dimI[1], n_elements(sigmas))
  Ifiltered = dblArr(dimI[0], dimI[1])

  ; Frangi filter for all sigmas.
  for i = 0, n_elements(sigmas)-1 do begin
    ; Show progress
    if (self.verbose) then print, ('Current Frangi Filter Sigma: ' + strTrim(string(sigmas[i]), 2))

    ; Make 2D hessian.
    s_hessian2D, image = image, desv = sigmas[i], Dxx = Dxx, Dxy = Dxy, Dyy = Dyy

    ; Correct for scale.
    Dxx *= (double(sigmas[i])^gamma)
    Dxy *= (double(sigmas[i])^gamma)
    Dyy *= (double(sigmas[i])^gamma)

    ; Get EPS value.
    res = machAr()

    ; Calculate (abs sorted) eigenvalues and vectors.
    s_eig2image, Dxx = Dxx, Dxy = Dxy, Dyy = Dyy, Lambda1 = Lambda1, Lambda2 = Lambda2, Ix = Ix, Iy = Iy

    ;idx=where(abs(Ix) lt res.xmin, count)
    ;if (count ne 0) then Ix[idx]=0
    ;idx=where(abs(Iy) lt res.xmin, count)
    ;if (count ne 0) then Iy[idx]=0
    angles = atan(Iy, Ix)

    Rb = dblArr(size(Lambda1, /dim))
    S  = dblArr(size(Lambda1, /dim))

    ; Compute some similarity measures.
    idx = where(Lambda1 eq 0, count)
    if (count ne 0) then Lambda1[idx] = res.xmin
    Rb = Lambda1 / Lambda2
    S  = sqrt(Lambda1^2 + Lambda2^2)

    ; Compute the output image.
    Iones = fltArr(size(image, /DIMENSIONS)) + 1.0
    Ifiltered = exp(-(Rb^2/(2*beta^2))) * (Iones-exp(-(S^2/(2*c^2))))

    if (self.bw eq 1) then begin
      idx = where(Lambda2 lt 0, count)
      if (count gt 0) then Ifiltered[idx] = 0
    endif else begin
      idx = where(Lambda2 gt 0, count)
      if (count gt 0) then Ifiltered[idx] = 0
    endelse
    ; Store the results in 3D matrices.
    ALLfiltered[*, *,i] = Ifiltered
    ALLangles[*, *, i]  = angles
  end

  ; Return for every pixel the value of the scale (sigma) with the maximum
  ; output pixel value.
  if (n_elements(sigmas) gt 1) then begin

    outIm = max(Allfiltered, DIMENSION = 3, location)
    ind   = array_indices(Allfiltered, location)
    outIm = reform(outIm, size(image, /DIMENSIONS))

    if keyword_set(whatScale) then begin
      whatScale = dblArr(size(outIm, /DIMENSIONS))
      whatScale = reform(whatScale, size(image))
    end
    if keyword_set(Direction) then begin
      Direction = dblArr(size(outIm, /DIMENSIONS))
      Direction = reform(transpose(ALLangles[0:n_elements(image)-1])+(whatScale-1)*n_elements(image),size(image,/DIMENSIONS))
    end
  endif else begin
    outIm = reform(ALLfiltered, size(image, /DIMENSIONS))
    if keyword_set(whatScale) then whatScale = make_array(size(image, /DIMENSIONS))
    if keyword_set(Direction) then Direction = reform(ALLangles,size(image, /DIMENSIONS))
  endelse 
end


function C_sImageFilter_FrangiVessel::init

  filterStruct = { $
    Name: 'C_FrangiVessel',$ ;  Filter Name.
    pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
    pNames     : ptr_new(),$ ; Pointer on Filter Parameter Names.
    pActive    : ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
    pMin       : ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
    pMax       : ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
    pValues    : ptr_new()}  ; Pointer on Filter Parameter Values.

  filterParamWidgetType = make_array(9, /string, value = 'widget_slider')
  filterParamNames  = ['Calculate', 'Scale Range Min', 'Scale Range Max', 'Scale Ratio', 'Beta', 'C', 'Gamma', 'Black/White', 'Verbose']
  filterParamActive = [0,1,1,1,1,1,0,0,0]
  filterParamMin    = [0,1,1,1,0,0,0,0,0]
  filterParamMax    = [1,20,20,5,1,50,10,1,1]
  filterParamValues = [0,1,10,2,0.5,15,2,1,1]

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
  filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct = ptr_new(filterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_FrangiVessel__define
  tmp = { C_sImageFilter_FrangiVessel, $
; WARNING. The author defined object attributes here which is problematic to
; update in IDL; pParamStruct/pValueStruct should be used instead. The code was
; left "as is" in order to avoid issues, but the filter attributes should not be modified after.
    calcular     : 0, $
    scaleRangeMin: 1, $
    scaleRangeMax: 8, $
    scaleRatio   : 1, $
    betaUno      : 0.5, $
    c            : 15, $
    gammaUno     : 2, $
    bw           : 1, $
    verbose      : 1, $
    pParamStruct : ptr_new(), $
    inherits C_sImageFilter}
end
