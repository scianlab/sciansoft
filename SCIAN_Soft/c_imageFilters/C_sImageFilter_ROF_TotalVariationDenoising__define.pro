;_____________________________IOISIOI (THIS IS ART)_____________________________
; Rudin-Osher-Fatemi (ROF) Total Variation Denoising Model Filter Class
;
; BIOBLIOGRAPHY:
;    Leonid I. Rudin, Stanley Osher, Emad Fatem
;    Physica D: Nonlinear Phenomena, 60(1-4): 259, 1992
;
; Implementation uses gradient descent.
;
; AUTHOR: Jorge Jara (2011)
;
;_____________________________IOISIOI (THIS IS ART)_____________________________

function C_sImageFilter_ROF_TotalVariationDenoising::getImageFilterType
  return, 'Single_Image_Filter_Method'
end


; Computes the normalized gradients of a given image, using epsilon as a threshold for avoiding divisions by zero
pro C_sImageFilter_ROF_TotalVariationDenoising::normalizedEpsilonGradient, image, epsilon, ix = ix, iy = iy
  szI = size(image, /dim)
  ; using an arbitrary minimum value... it could be done with the IDL 'machar' method or something more clever ;)
  epsilon = epsilon > 10^(-6)
  ix = shift(image, -1, 0) - image
  ix[0, *] = 0
  ix[szI[0]-1, *] = 0

  iy = shift(image, 0, -1) - image
  iy[*, 0] = 0
  iy[*, szI[1]-1] = 0

  mag = sqrt(ix^2 + iy^2 + epsilon)
  ix /= mag
  iy /= mag

;  window, 28, title = 'Image gradient Ix', xsize = 560, ysize = 600
;  surface, ix
;  window, 29, title = 'Image gradient Iy', xsize = 560, ysize = 600
;  surface, iy
end


; Computes the divergence of a given image I, given its gradient vector field (Ix, Iy) as inputs
; Note that while the gradients must be computed using forward finite differences, the divergence 
; must be computed with backward finite differences, in order to satisfy the Gauss theorem with the 
; discrete scheme of the gradient descent method.
function C_sImageFilter_ROF_TotalVariationDenoising::divergence2D, ix = ix, iy = iy
  sz = size(ix, /dim)

  dx = ix - shift(ix, 1, 0)
  dx[0, *] = ix[1, *]
  dx[sz[0]-1, *] = -ix[sz[0]-2, *]

  dy = iy - shift(iy, 0, 1)
  dy[*, 0] = iy[*, 1]
  dy[*, sz[1]-1] = -iy[*, sz[1]-2]

  div = dx + dy
;  window, 30, title = 'Divergence 2D', xsize = 550, ysize = 600
;  surface, div
  return, div
end


; Entry point fot the filter application
function C_sImageFilter_ROF_TotalVariationDenoising::apply, image = image
  if (size(image, /n_dimensions) ne 2) then return, image

  whParam   = (where(*(*self.pParamStruct).pNames eq 'iterations'))[0]
  iterCount = (*(*self.pParamStruct).pValues)[whParam]
  whParam   = (where(*(*self.pParamStruct).pNames eq 'epsilon'))[0]
  epsilon   = (*(*self.pParamStruct).pValues)[whParam]
  whParam   = (where(*(*self.pParamStruct).pNames eq 'tau'))[0]
  tau       = (*(*self.pParamStruct).pValues)[whParam]
  whParam   = (where(*(*self.pParamStruct).pNames eq 'lambda'))[0]

  fLambda   = (*(*self.pParamStruct).pActive)[whParam]
  if (fLambda eq 1) then lambda1 = 1. / (*(*self.pParamStruct).pValues)[whParam]
  whParam   = (where(*(*self.pParamStruct).pNames eq 'display_intermediate'))[0]
  fDisplay  = (*(*self.pParamStruct).pActive)[whParam]

  szI = size(image, /dim)
  ix = make_array(szI, /double)
  iy = make_array(szI, /double)
  filtered = double(image)
  displayIterStep = iterCount / 20
  xWinSz = 700
  yWinSz = 600
  winNum = 31 ; an arbitrary number between 0 and 31

  if (fLambda eq 0) then print, 'ROFTotalVariationDenoising filter: using only divergence (regularity term)'
  if (fLambda eq 1) then print, 'ROFTotalVariationDenoising filter: using divergence (regularity) and image similarity terms'

  case fLambda of

   0: for i = 1L, iterCount do begin
        self->normalizedEpsilonGradient, filtered, epsilon, ix = ix, iy = iy
        filtered += tau * self->divergence2D(ix = ix, iy = iy)
        ; clamp the result to 0-1, according to the "recipe"...
        filtered >= 0
        filtered <= 1
        if (fDisplay eq 1) and ((i mod displayIterStep) eq 0) then begin
          window, winNum, title = 'iteration ' + strCompress(i, /remove_all), xSize = xWinSz, ySize = yWinSz
          surface, filtered, zRange = [0,1]
        endif
      endfor

   1: for i = 1L, iterCount do begin
        self->normalizedEpsilonGradient, filtered, epsilon, ix = ix, iy = iy
        filtered += tau * (self->divergence2D(ix = ix, iy = iy) - lambda1 * (filtered - image))
        ; clamp the result to 0-1, according to the "recipe"...
        filtered >= 0
        filtered <= 1
        if (fDisplay eq 1) and ((i mod displayIterStep) eq 0) then begin
          window, winNum, title = 'iteration ' + strCompress(i, /remove_all), xSize = xWinSz, ySize = yWinSz
          surface, filtered, zRange = [0,1]
        endif
      endfor

   else: begin
      print, 'ROFTotalVariationDenoising filter: error while determining the iterative method, returning input image...'
      return, image
    end
  endcase

  if (fDisplay eq 1) then begin
    window, winNum, title = 'iteration ' + strCompress(i-1, /remove_all), xSize = xWinSz, ySize = yWinSz
    surface, filtered, zRange = [0,1]
  endif

  return, filtered
end


function C_sImageFilter_ROF_TotalVariationDenoising::init

  filterStruct = {Name       : 'C_ROF_TotalVariationDenoising',$ ;Filter Name.
                  pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pNames     : ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pActive    : ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
                  pMin       : ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
                  pMax       : ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
                  pValues    : ptr_new()}  ; Pointer on Filter Parameter Values.

     ; Filter parameters.
  filterParamNames  = ['epsilon', 'iterations', 'tau', 'lambda', 'display_intermediate']
  filterParamActive = [1,         1,            1,      1,       0]
  filterParamMin    = [0,         1,            0.00001,0.0001,  0]
  filterParamMax    = [1,         5000,         10,     1000,    1]
  filterParamValues = [0.001,     100,          0.001,  0.3,     0]
  filterParamWidgetType = make_array(n_elements(filterParamNames), /string, value = 'widget_slider')

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames      = ptr_new(filterParamNames, /no_copy)
  filterStruct.pActive     = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin        = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax        = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues     = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct = ptr_new(filterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_ROF_TotalVariationDenoising__define
  tmp = {C_sImageFilter_ROF_TotalVariationDenoising, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
