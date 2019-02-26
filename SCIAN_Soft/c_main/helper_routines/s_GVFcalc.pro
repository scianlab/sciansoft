;_____________________________IOISIOI (THIS IS ART)_____________________________
;
; s_GVFcalc.pro     Image gradient vector flow field calculation routines.
;
; AUTHOR
;                   Felipe Olmos, Jorge Jara (2010-2011)
;
;_____________________________IOISIOI (THIS IS ART)_____________________________

;*****************************************************************************************************
;+
; NAME:         gradient
;
; PURPOSE:      Calculates the gradient of an image.
;
; SYNTAX:       theGradient = theObject->gradient(image, 0)
;
; RETURN_VALUE: theGradient: The gradient of the input image.
;
; ARGUMENTS:
;       image:     The input image.
;       direction: Set this parameter to 0 (default) for gradient in the X direction (fx), or
;                  to 1 for gradient in the Y direction (fy).
;-
;*****************************************************************************************************
function imageGradient, image, direction

  compile_opt idl2, hidden
  if (n_elements(direction) eq 0) then direction = 0
  szI = size(image, /dim)

  if (n_elements(szI) ne 2) then return, -1
  theGradient = make_array(szI[0], szI[1], /double)

  case direction of
    0: begin
       theGradient = (shift(image, -1, 0) - shift(image, 1, 0)) * .5
       theGradient[[0, szI[0]-1], *] = theGradient[[1, szI[0]-2], *]
    end
    1: begin
       theGradient = (shift(image, 0, -1) - shift(image, 0, 1)) * .5
       theGradient[*, [0, szI[1]-1]] = theGradient[*, [1, szI[1]-2]]
    end
    else: return, -1
  endcase
  return, theGradient
end


;*****************************************************************************************************
;+
; NAME:           gradient
;
; PURPOSE:        Calculates the gradient of an image.
;
; SYNTAX:         theGradient = theObject->gradient(image, direction)
;
; RETURN_VALUE:   theGradient: The gradient of the input image in the specified direction.
;
; ARGUMENTS:
;
;     image: The unfiltered image.
;
;     direction: Set this parameter to 0 (default) for gradient in the X direction (fx),
;              to 1 for gradient in the Y direction (fy), or to 2 for gradient in Z (fz).
;-
;*****************************************************************************************************
; TODO JJ: add(?) gaussian option for edgemap, as in the 2D EPGGVF
function imageGradient3D, image, direction

   compile_opt idl2, hidden
   if (n_elements(direction) eq 0) then direction = 0

   szI = size(image, /dim)
   if (n_elements(szI) ne 3) then return, -1

   theGradient = make_array(szI, /float)

   case direction of
      0: begin
         theGradient = (shift(image, -1, 0, 0) - shift(image, 1, 0, 0))*.5
         theGradient[[0,szI[0]-1], *, *] = theGradient[[1,szI[0]-2], *, *]
      end
      1: begin
         theGradient = (shift(image, 0, -1, 0) - shift(image, 0, 1, 0))*.5
         theGradient[*, [0,szI[1]-1], *] = theGradient[*, [1,szI[1]-2], *]
      end
      2: begin
         theGradient = (shift(image, 0, 0, -1) - shift(image, 0, 0, 1))*.5
         theGradient[*, *, [0,szI[2]-1]] = theGradient[*, *, [1,szI[2]-2]]
      end
      else: return, -1
   endcase

   return, theGradient

end


; image      the input image
; normalize  a flag that specificies if the edgemap shoould be normalized. Default is 0 (not).
function imageEdgemap, image, ex, ey, normalize = normalize

  igx = imageGradient(double(image), 0)
  igy = imageGradient(double(image), 1)

  edgeMap = sqrt(igx^2 + igy^2)
  ;edgeMap = abs(igx) + abs(igy)

  ex = ptr_new(igx, /no_copy)
  ey = ptr_new(igy, /no_copy)
  ;window, 10
  ;tvscl, edgeMap

  ; Normalize in the [0, 1] interval
  if keyword_set(normalize) then begin
    maxVal = max(edgeMap, min = minVal)
    if (maxVal ne minVal) then begin
      edgeMap -= minVal
      edgeMap /= (maxVal - minVal)
    endif
  endif
  return, edgeMap
end


; Isotropic gaussian kernel for 3D.
; TODO Development method for 3D GVF testing... make generic for n-dimensions?
; kernelRadius: size of each tail of the kernel, the size in each direction is 2*kernelRadius + 1
; totalSum: if set stores the sum of the kernel coefficients, approximating the integral of the function
; (remember that the analytical integral of a gaussian function over the whole domain is equal to 1).
function gaussianKernel3D, sigma, kernelRadius, totalSum = totalSum
  kernel = make_array(replicate(2*kernelRadius + 1, 3), /double)
  const = 1.0d / (sigma * sqrt(2 * !PI))^3
  constExp = -0.5d / sigma^2
  
  kernel[kernelRadius, kernelRadius, kernelRadius] = const
  ; yup... some overlap in the calculations
  for k = 0, kernelRadius do begin
    dz = kernelRadius + k
    for j = 0, kernelRadius do begin
      dy = kernelRadius + j
      for i = 0, kernelRadius do begin
        dx = kernelRadius + i
        kernel[dx, dy, dz] = const * exp(constExp * (i^2 + j^2 + k^2))
        kernel[kernelRadius-i, dy,             dz] = kernel[dx, dy, dz]
        kernel[dx,             kernelRadius-j, dz] = kernel[dx, dy, dz]
        kernel[kernelRadius-i, kernelRadius-j, dz] = kernel[dx, dy, dz]
        kernel[dx,             dy, kernelRadius-k] = kernel[dx, dy, dz]
        kernel[kernelRadius-i, dy, kernelRadius-k] = kernel[dx, dy, dz]
        kernel[dx, kernelRadius-j, kernelRadius-k] = kernel[dx, dy, dz]
        kernel[kernelRadius-i, kernelRadius-j, kernelRadius-k] = kernel[dx, dy, dz]
      endfor
    endfor
  endfor
  totalSum = total(kernel)
  return, kernel
end


; PURPOSE:  image gradient computation method
;
; PARAMETERS:
;
; image     input image from which the gradient will be computed.
; ex, ey    output variables to store the corresponding x- and y-gradient components.
; sigmaSq   Std. deviation parameter for applying an optional gaussian smoothing to the gradient.
;           By making sigmaSq = 0 no gaussian smoothing is applied.
; normalize set this keyword to normalize the output to the [0,1] interval.
function imageEdgemapGaussian, image, ex, ey, sigmaSq = sigmaSq, normalize = normalize

  compile_opt idl2, hidden

  if keyword_set(sigmasq) and (sigmasq gt 0) then begin
    sz = size(image, /dim)
    sz = sz[0] < sz[1]
    kernelSize = 21 < sz
    if ((kernelSize mod 2) eq 0) then kernelSize -= 1

    ; The smooth edgemap is the squared - module of the gradient of the image convoluted with a Gaussian.
    x = (make_array(kernelSize, kernelSize, /index) mod kernelSize - 10) * exp(1)/5
    y = transpose(x)

    ; Partial derivatives of the Gaussian
    gaussx = exp(-(x^2 + y^2) / (2 * sigmasq)) * (-x) / sigmasq
    gaussy = exp(-(x^2 + y^2) / (2 * sigmasq)) * (-y) / sigmasq

    ; In a convolution the derivatives can be kicked to the kernel
    igx = ptr_new(convol(double(image), gaussx, 255, /edge_truncate), /no_copy)
    igy = ptr_new(convol(double(image), gaussy, 255, /edge_truncate), /no_copy)

  endif else begin
    dervx = [[0, 0,0], [-1,0,1], [0,0,0]]
    dervy = [[0,-1,0], [ 0,0,0], [0,1,0]]

    igx = ptr_new(convol(double(image), dervx, /edge_truncate), /no_copy)
    igy = ptr_new(convol(double(image), dervy, /edge_truncate), /no_copy)
  endelse

  edgeMap = sqrt(*igx^2 + *igy^2)
  ;edgeMap = abs(*igx) + abs(*igy)

  ; Normalize in the [0, 1] interval
  if keyword_set(normalize) then begin
    maxVal = max(edgeMap, min = minVal)
    if (maxVal ne minVal) then begin
      edgeMap -= minVal
      edgeMap /= (maxVal - minVal)
    endif
  endif

  ex = igx
  ey = igy
  return, edgeMap
end


function imageEdgemapGaussian3D, image, ex, ey, ez, sigmasq = sigmasq, normalize = normalize

  compile_opt idl2, hidden

  sz = size(image, /dim)
  if keyword_set(sigmasq) and (sigmasq gt 0) then begin
    kernelSize = 21 < min(sz) ; an arbitrary reduction in the size... Why 21, LFOM?
    if ((kernelSize mod 2) eq 0) then kernelSize -= 1

    ; The smooth edgemap is the squared - module of the gradient of the image convoluted with a Gaussian.

    ; Partial derivatives of the Gaussian
    gaussx = exp(-(x^2 + y^2 + z^2) / (2 * sigmasq)) * (-x) / sigmasq
    gaussy = exp(-(x^2 + y^2 + z^2) / (2 * sigmasq)) * (-y) / sigmasq
    gaussz = exp(-(x^2 + y^2 + z^2) / (2 * sigmasq)) * (-z) / sigmasq

    ; In a convolution the derivatives can be kicked in to the kernel
    igx = ptr_new(convol(double(image), gaussx, 255, /edge_truncate), /no_copy)
    igy = ptr_new(convol(double(image), gaussy, 255, /edge_truncate), /no_copy)
    igz = ptr_new(convol(double(image), gaussz, 255, /edge_truncate), /no_copy)

  endif else begin
      ; TODO convert to 3D
;    dervx = [[0, 0,0], [-1,0,1], [0,0,0]]
;    dervy = [[0,-1,0], [ 0,0,0], [0,1,0]]
    dervx = make_array(3,3,3, /double, value = 0)
    dervx[0,1,1] = -1
    dervx[2,1,1] =  1
    dervy = make_array(3,3,3, /double, value = 0)
    dervy[1,0,1] = -1
    dervy[1,2,1] =  1
    dervz = make_array(3,3,3, /double, value = 0)
    dervz[1,1,0] = -1
    dervz[1,1,2] =  1

    igx = ptr_new(convol(double(image), dervx, /edge_truncate), /no_copy)
    igy = ptr_new(convol(double(image), dervy, /edge_truncate), /no_copy)
    igz = ptr_new(convol(double(image), dervz, /edge_truncate), /no_copy)
  endelse

  edgeMap = sqrt(*igx^2 + *igy^2 + *igz^2)
  ;TODO JJ edgemap computation... better? faster? stronger?
  ;edgeMap = abs(*igx) + abs(*igy) + abs(*igz)

  ; Normalize in the [0, 1] interval
  if keyword_set(normalize) then begin
    maxVal = max(edgeMap, min = minVal)
    if (maxVal ne minVal) then begin
      edgeMap -= minVal
      edgeMap /= (maxVal - minVal)
    endif
  endif

  ex = igx
  ey = igy
  ez = igz
  return, edgeMap
end


function imageLaplacian, image, kernel_size = kernel_size
  compile_opt idl2, hidden

  if ~(keyword_set(kernel_size)) then kernel_size = 3

  case kernel_size of
  3: begin
    kernel = fltArr(3,3)
    kernel[*,1] = 0.25
    kernel[1,*] = 0.25
    kernel[1,1] = -1.0
   end
  5: begin
    kernel = fltArr(5,5)
    kernel[2,  0] = 0.0833333
    kernel[1:3,1] = 0.0833333
    kernel[*,  2] = 0.0833333
    kernel[1:3,3] = 0.0833333
    kernel[2,  4] = 0.0833333
    kernel[2,  2] = -1.0
  end
  else: begin
    print, 'unrecognized kernel size (use 3 or 5)'
    return, -1
  end
  end
  return, convol(double(image), kernel, center = 1, /edge_truncate)
end


;*****************************************************************************************************
;+
; NAME:   calcGVF
;
; PURPOSE:Computes the GVF field for the Active Contour.
;
; SYNTAX: calcGVF(image, u, v, iterations = iterations)
;
; PARAMETERS:
;       iterations  default value is 100
;
; NOTES:
;
;       GVF computation based on the MATLAB version of the Active Contour program
;       ("Snake Demo" by Xu, Prince, Tomazevic, 1998)
;-
;*****************************************************************************************************
pro calcGVF, image, pU, pV, iterations = iterations, mu = mu

  compile_opt idl2, hidden

  if ~(keyword_set(iterations)) then iterations = 100
  if ~(keyword_set(mu)) then mu = 0.01

      ; Calculate gradients [fx, fy] to initialize the vector field [u, v].
  e = imageEdgemap(image, p_ex, p_ey)
  ex = *p_ex
  ey = *p_ey
      ; original version by Xu98
   ;b = (ey)^2+(ey)^2
  b = abs(ex) + abs(ey)
  bMinus = 1. - b
  c1 = b * ex
  c2 = b * ey

  ; Iteratively solve for the GVF u,v.
  ; delta_x = delta_y = delta_t = 1
  for j = 1, iterations do begin
    u_lap = imageLaplacian(ex)
    v_lap = imageLaplacian(ey)
    ex = (bMinus * ex) + (mu * 4 * u_lap) + c1
    ey = (bMinus * ey) + (mu * 4 * v_lap) + c2
  endfor
  window, 1, xsize=800, ysize=800, title='GVF'
  velovect, ex, ey
  quo = sqrt(ex^2 + ey^2) + double(ex^2 + ey^2 eq 0)
  pU = ptr_new(ex/quo, /no_copy)
  pV = ptr_new(ey/quo, /no_copy)
  ptr_free, p_ex, p_ey
  print, 'GVF computation done'
end


pro calcGGVF, image, pU, pV, iterations = iterations, mu = mu

  if ~(keyword_set(iterations)) then iterations = 100
  if ~(keyword_set(mu)) then mu = 0.01

  ; calculate gradients [fx, fy] to initialize the vector field [u, v].
  e = imageEdgemap(image, p_ex, p_ey)

  ; original version for the GGVF by Xu99
  ;b = (ex)^2+(ey)^2
  b = abs(*p_ex) + abs(*p_ey)

  ; this pair of functions act as an "enhancer/de-enhancer" of high gradient neighbors
  ; the choice of the functions must satisfy some convergence restrictions (see reference)
  g = exp(-b / mu)
  c1 = *p_ex * (1 - g)
  c2 = *p_ey * (1 - g)

   ; solve iteratively for the GGVF [u,v].
   ; delta_x = delta_y = delta_t = 1
;   window, 11, xsize=800, ysize=800, title='GGVF'
;   velovect, ex[175:250,125:200], ey[175:250,125:200], length = 0.4
  for j = 1, iterations do begin
    u_lap = imageLaplacian(*p_ex)
    v_lap = imageLaplacian(*p_ey)

    ; original iteration scheme
    ; *p_ex += g * u_lap - h * (*p_ex - fx)
    ; *p_ey += g * v_lap - h * (*p_ey - fy)

;   window, 10
;     velovect, *p_ex - (g * (*p_ex + u_lap) + c1), *p_ey - (g * (*p_ey + v_lap) + c2), length = 0.4
;   window, 11
;     PLOT, *p_ey - (g * (*p_ey + u_lap) + c1)

    ; optimized iteration scheme
    *p_ex = g * (*p_ex + u_lap) + c1
    *p_ey = g * (*p_ey + v_lap) + c2

;    window, 13
;      velovect, *p_ex, *p_ey, length = 0.4
;    window, 14
;      PLOT, (ex)
  endfor
  quo      = sqrt(*p_ex^2 + *p_ey^2)
  thresh   = 1.0^(-9)
  whereLET = where(abs(quo) le thresh, countLET)
  if (countLET gt 0) then quo[whereLET] = 1.0
  pU = p_ex
  pV = p_ey
  *pU /= quo
  *pV /= quo
  print, 'GGVF computation done'
end


pro calcEPGVF3D, image, pU, pV, pW, iterations = iterations, noiseCut = noiseCut, noiseRange = noiseRange, error = error, step = step
  ; Check keyword parameters.
  step       = n_elements(step)       eq 0 ? 0.01D : step
  error      = n_elements(error)      eq 0 ? 0.01D : error
  maxIt      = n_elements(maxIt)      eq 0 ? 1000L : maxIt
  noiseCut   = n_elements(noiseCut)   eq 0 ? 0.10D : noiseCut
  cutRange   = n_elements(cutRange)   eq 0 ? 0.05D : cutRange
  iterations = n_elements(iterations) eq 0 ? 1000  : (iterations > 1)
  xyzSize = size(image, /dim)
  ; first, compute image gradients
  fx = abs(imageGradient3d(double(image), 0))
  fy = abs(imageGradient3d(double(image), 1))
  fz = abs(imageGradient3d(double(image), 2))
  ; compute edgemap image as the squared norm of the smoothed intensity gradient... optional normalize to [0,1]
  f = fx^2 + fy^2 + fz^2
  ; now compute the gradients from the edgemap
  fx = imageGradient3d(f, 0)
  mx = max(fx, min=nx)
  fx -= nx
  fx /= (mx-nx)
  fy = imageGradient3d(f, 1)
  my = max(fy, min=ny)
  fy -= ny
  fy /= (my-ny)
  fz = imageGradient3d(f, 2)
  mz = max(fz, min=nz)
  fz -= nz
  fz /= (mz-nz)

  tau = noiseCut
  delta = cutRange

  ; Calculate the weight functions g and h
  f -= tau
  whereLT = where(f lt -delta, countLT)
  if (countLT gt 0) then f[whereLT] = -delta
  ;f = f * double(f ge -delta) + double(f lt -delta) * (-delta)

  whereGT = where(f gt delta, countGT)
  if (countGT gt 0) then f[whereGT] = delta
  ;f = f * double(f le  delta) + double(f gt  delta) * ( delta)

  h  = -(f)^3 / (4.0 * delta^3) + 3.0 * f / (4.0 * delta) + 0.5
  g = 1 - h
    ; free some memory
  f = -1

  u = make_array(xyzSize, /double)
  v = make_array(xyzSize, /double)
  w = make_array(xyzSize, /double)
  ; TODO JJ note: trouble with step size varying even slightly above or below 0.01 (default)
  calc_GVF_wrapper3D, iterations, xyzSize[0], xyzSize[1], xyzSize[2], step, u, v, w, fx, fy, fz, g, h, error

  ; The EP-GVF is the normalized field
  ; Strange 0 avoiding...
  quo = sqrt(u^2 + v^2 + w^2)
  wh0 = where(quo eq 0, count0)
  if (count0 gt 0) then quo[wh0] = 1.0

  pU = ptr_new(u / quo, /no_copy)
  pV = ptr_new(v / quo, /no_copy)
  pW = ptr_new(w / quo, /no_copy)
  print, 'EPGVF computation done'

end


pro calcEPGVF, image, pU, pV, iterations = iterations,$
                              noiseCut   = noiseCut,$
                              noiseRange = noiseRange,$
                              step       = step,$
                              convergenceLimit = convergenceLimit,$
                              edgemapSigmaSq = edgemapSigmaSq
    ; Check keyword parameters.
  noiseCut         = n_elements(noiseCut)   eq 0 ? 0.10D : noiseCut
  cutRange         = n_elements(cutRange)   eq 0 ? 0.05D : cutRange
  step             = n_elements(step)       eq 0 ? 0.01D : step
  iterations       = n_elements(iterations) eq 0 ? 200   : (iterations > 1)
  convergenceLimit = n_elements(convergenceLimit) eq 0 ? 0.01D : convergenceLimit

  edgemapSigmaSq = n_elements(edgemapSigmaSq) eq 0 ? 3.0 : edgemapSigmaSq
  eMap = imageEdgemapGaussian(image, p_ix, p_iy, sigmaSq = edgemapSigmaSq)

    ; Now compute final edgemap as the squared norm of the smoothed intensity gradient normalized to [0,1]
  f = double(*p_ix^2 + *p_iy^2)

    ; Compute the partial derivatives of the edge map (spatial step = 1)
  fx = convol(f, [-1, 0, 1], 2, /edge_truncate)
  fy = convol(f, transpose([-1, 0, 1]), 2, /edge_truncate)

  ; ivector, fx, fy, title='Edgemap gradient', auto_color=2, RGB_TABLE=39, SCALE_ISOTROPIC=1, data_location=0, /no_saveprompt
  ; ivector, p1, p2, title='EPGVF norm', auto_color=2, RGB_TABLE=39, SCALE_ISOTROPIC=1, data_location=0, /no_saveprompt

    ; Get the dimensions of the image, they are useful :D
  sz = size(f, /dim)
  nx = fix(sz[0])
  ny = fix(sz[1])

  tau = noiseCut
  delta = cutRange

   ; Calculate the weight functions g and h
  ftemp = f - tau
  whereLT = where(ftemp lt -delta, countLT)
  if (countLT gt 0) then ftemp[whereLT] = -delta
    ; old
  ;ftemp = ftemp * double(ftemp ge -delta) + double(ftemp lt -delta) * (-delta)

  whereGT = where(ftemp gt delta, countGT)
  if (countGT gt 0) then ftemp[whereGT] = delta
    ; old
  ;ftemp = ftemp * double(ftemp le  delta) + double(ftemp gt  delta) * ( delta)

  h = -(ftemp)^3 / (4.0 * delta^3) + 3.0 * ftemp / (4.0 * delta) + 0.5
  g = 1 - h

  u = make_array(nx, ny, /double)
  v = make_array(nx, ny, /double)

  calc_GVF_wrapper, iterations, nx, ny, step, u, v, fx, fy, g, h, convergenceLimit

  ; The EP-GVF is the normalized field
  quo = sqrt(u^2 + v^2) + double(u^2 + v^2 eq 0) ; Strange 0 avoiding...
  pU = ptr_new(u / quo, /no_copy)
  pV = ptr_new(v / quo, /no_copy)
  print, 'EPGVF computation done'
  ;ivector, *pU, *pV, auto_color=2, length_scale=0.1, SCALE_ISOTROPIC=1, data_location=0, RGB_TABLE=39, /no_saveprompt
end

function getGGVFDLLdir
  return, 'C:\Users\JetGo\workspaceVS\IDL_3DGGVF\x64\Release'
end


function getGGVFDLLname
  return, 'IDL_3DGGVF.dll'
end


pro testGVFDLL
  dllDir = getGGVFDLLdir()
  dllName = getGGVFDLLname()
  fResult = call_external(dllDir + path_sep() + dllName, 'test', fix(1, type=2), /unload)
  print, fResult
end


pro test3Ddata
  mock = make_array(3, 3, 2, /double)
  mockSz = n_elements(mock)
  for i = 0, mockSz-1 do mock[i] = i * 1.0d
  dllDir = getGGVFDLLdir()
  dllName = getGGVFDLLname()
  fResult = call_external(dllDir + path_sep() + dllName, 'testPrint3dData', fix(mock, type = 5), fix(mockSz, type = 2), /unload)
  print, fResult
end
