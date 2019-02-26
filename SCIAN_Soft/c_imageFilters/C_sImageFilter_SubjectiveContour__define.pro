; This code performs a segmentation of a 2D membrane using the 
; Subjective Surfaces method as suggested in the paper
; "Cells Segmentation from 3-D Confocal Images of Early Zebrafish
; Embryogenesis", authored by C.Zanella, M.Campana, B.Rizzi, 
; C.Melani, G.Sanguinetti, P. Bourgine, K. Mikula, N. Peyriéras, A. Sarti
;
; Jorge Jara (2011)
;

; getImageFilterType
;
; Image-filter-type getter method
function C_sImageFilter_SubjectiveContour::getImageFilterType
  ;return, 'Multiple_Image_Filter_Method'
  return, 'Alpha_Debugging_Filter_Method'
end


; Edge indicator function
;
; g = g(I, dt, iter, beta)                   
;
; PARAMETERS:
; 
; - dt    time step of heat equation
; - iter  scale steps of heat equation
; - beta  parameter to modify the contrast
function C_sImageFilter_SubjectiveContour::g, image = image, dt = dt, iter = iter, beta = beta
  iLap = image * 1.0
  for i = 1L, iter do iLap += dt * laplacian(iLap, KERNEL_SIZE = 3, /EDGE_WRAP)
  return, beta / (1. + iLap * iLap)
end


; Geodesic mean curvature filtering: 
; geocurv
;
; I_t = g.* H.* |grad(I)| + grad(I) .* grad(g)
;
; PARAMETERS:
;
; I0:    original image 
; dt:    time step of filtering equation
; nIter: scale steps of filtering equation
; dtg:   time step of heat equation for edge indicator
; beta:  parameter to modify the contrast in the edge indicator
; iter:  scale steps of heat equation for edge indicator
function C_sImageFilter_SubjectiveContour::geocurv, I0 = I0, d_t = d_t, nIter = nIter, dtg = dtg, beta = beta, iter = iter
  I = I0
  a = 1
  for j = 1L, nIter do begin
    edge_indicator = self->g(image = I, dt = dtg, iter = iter, beta = beta)
    I += d_t * (edge_indicator * self->Hg(M = I, a = a) - self->gUp(I = I, Fx = -self->Dx(edge_indicator), Fy = -self->Dy(edge_indicator)))
  endfor
  ; Rescaling to [0,1]
  minI = min(I, max = maxI)
  I -= minI
  I /= (maxI - minI)
  return, I
end


; Centered difference in x
function C_sImageFilter_SubjectiveContour::Dx, M
  szM = size(M, /dim)
  Dx = make_array(szM, /float, value = 0)
  Dx[1:szM[0]-2,*] = M[2:szM[0]-1,*] - M[0:szM[0]-3,*]
  Dx[  szM[0]-1,*] = M[  szM[0]-1,*] - M[  szM[0]-2,*]
  Dx[0,*] = M[1,*] - M[0,*]
  return, Dx / 2.0
end


; Second derivative (finite differences) in x
function C_sImageFilter_SubjectiveContour::Dxx, M
  szM = size(M, /dim)
  Dxx = -2.0 * M
  Dxx[1:szM[0]-2,*] += M[2:szM[0]-1,*] + M[0:szM[0]-3,*]
  Dxx[  szM[0]-1,*] += M[  szM[0]-1,*] + M[szM[0]-2,*]
  Dxx[0,*] += M[1,*] + M[0,*]
  return, Dxx
end

; Forward difference in x
function C_sImageFilter_SubjectiveContour::Dp_x, M
  szM = size(M, /dim)
  Dp_x = -M
  Dp_x[0:szM[0]-2,*] += M[1:szM[0]-1,*]
  Dp_x[szM[0]-1,*] = 0
  return, Dp_x
end


; Backward difference in x
function C_sImageFilter_SubjectiveContour::Dm_x, M
  szM = size(M, /dim)
  Dp_x = M
  Dp_x[1:szM[0]-1,*] -= M[0:szM[0]-2,*]
  Dp_x[0,*] = 0
  return, Dp_x
end


; Centered difference in y
function C_sImageFilter_SubjectiveContour::Dy, M
  szM = size(M, /dim)
  Dy = make_array(szM, /float, value = 0)
  Dy[*,1:szM[1]-2] = M[*,2:szM[1]-1] - M[*,0:szM[1]-3]
  Dy[*,  szM[1]-1] = M[*,  szM[1]-1] - M[*,  szM[1]-2]
  Dy[*,0] = M[*,1] - M[*,0]
  return, Dy / 2.0
end


; Second derivative (finite differences) in y
function C_sImageFilter_SubjectiveContour::Dyy, M
  szM = size(M, /dim)
  Dyy = -2.0 * M
  Dyy[*,1:szM[1]-2] += M[*,2:szM[1]-1] + M[*,0:szM[1]-3]
  Dyy[*,  szM[1]-1] += M[*,  szM[1]-1] + M[*,szM[1]-2]
  Dyy[*,0] += M[*,1] + M[*,0]
  return, Dyy
end


; Forward difference in y
function C_sImageFilter_SubjectiveContour::Dp_y, M
  szM = size(M, /dim)
  Dp_y = -M
  Dp_y[*,0:szM[1]-2] += M[*,1:szM[1]-1]
  Dp_y[*,szM[1]-1] = 0
  return, Dp_y
end


; Backward difference in y
function C_sImageFilter_SubjectiveContour::Dm_y, M
  szM = size(M, /dim)
  Dp_y = M
  Dp_y[*, 1:szM[1]-1] -= M[*, 0:szM[1]-2]
  Dp_y[*, 0] = 0
  return, Dp_y
end


; f = Hg(Matrix, a)
;
; Hg = H.* |grad(M)|
;
; PARAMETERS;
; - H  mean curvature of graph of M
; - a  stretching factor which shifts the subjective surfaces model 
;      from the mean curvature flow of level sets (a = 0)
;      to the mean curvature flow of graph (a = 1).
function C_sImageFilter_SubjectiveContour::Hg, M = M, a = a
  return, (1.0 * self->Dxx(M) * (1.0 * self->Dy(M)^2 + a) - 2. * self->Dy(M) * self->Dx(M) * self->Dx(self->Dy(M)) + self->Dyy(M) * (1.0 * self->Dx(M)^2 + a)) $
        / ((1.0 * self->Dx(M)^2 + 1.0 * self->Dy(M)^2 + a))
end


; Upwind scheme for grad(I). * grad(F)
;
; result = gUp(I, Fx, Fy) 
function C_sImageFilter_SubjectiveContour::gUp, I = I, Fx = Fx, Fy = Fy 
  return, (1.0 * (Fx gt 0) * self->Dm_x(I) + 1.0 * (Fx lt 0) * self->Dp_x(I)) * Fx $
        + (1.0 * (Fy gt 0) * self->Dm_y(I) + 1.0 * (Fy lt 0) * self->Dp_y(I)) * Fy
end


; Subjective Surfaces segmentation: 
; phi_t = mi* g.* H.* |grad(phi)| + ni* grad(phi) .* grad(g)
;
; PARAMETERS:
;
; phi0:           phi initialized by calling the function initialize
; I0:             original image. Required only for visualization purpose
; edge_indicator: edge indicator 
; mi:             weight parabolic term
; ni:             weight advective term
; a_high:         value of a in the curvature for high diffusion
; a_low:          value of a in the curvature for low diffusion
; dt:             time step of segmentation equation
; niter_a_high:   scale steps with a = a_high
; niter_a_low:    scale steps with a = a_low
;
; visualize:      The surface is visualized every visualize scale steps
; windowNumber:   number of IDL window for visualization
function C_sImageFilter_SubjectiveContour::subsurf, phi0 = phi0,$
                                                    I0 = I0,$
                                                    edge_indicator = edge_indicator,$
                                                    m_i = m_i,$
                                                    n_i = n_i,$
                                                    a_high = a_high,$
                                                    a_low = a_low,$
                                                    niter_a_high = niter_a_high,$
                                                    niter_a_low = niter_a_low,$
                                                    d_t = d_t,$
                                                    visualize = visualize,$
                                                    windowNumber = windowNumber
  phi = phi0
  phimin = min(phi)
  szPhi = size(phi, /dim)
  nx = szPhi[0]
  ny = szPhi[1]
  niter = niter_a_high + niter_a_low

  for i = 1L, niter do begin
    a = (niter le niter_a_high) ? a_high : a_low
    sum1 = total(phi)
    phi = phi + d_t * (m_i * 1.0 * edge_indicator * self->Hg(M = phi, a = a) - n_i * self->Gup(I = phi, Fx = -self->Dx(edge_indicator), Fy = -self->Dy(edge_indicator)))
    sum2 = total(phi)
    ; TODO use a better convergence checking
    if (sqrt(abs(sum1^2 - sum2^2)) lt 0.0001) then begin
      print, 'Evolution stopped after ', niter, 'scale steps'
      break
    endif
    if ((i mod visualize) eq 0) then begin
      window, windowNumber, title = 'Surface evolution ' + string(i) + ' scale steps'
      surface, phi
    endif
    ; TODO test IDL's transposed notation
    phi[0:nx-1,0     ] = phimin
    phi[0:nx-1,ny-1  ] = phimin
    phi[0     ,0:ny-1] = phimin
    phi[nx-1  ,0:ny-1] = phimin
  endfor
  ; The published code performed a rescaling of phi for visualization. This has been removed.
  return, phi
end


; Entry point fot the filter application / calculation.
function C_sImageFilter_SubjectiveContour::apply, image = image,$
                                                  selectedStackObject = selectedStackObject,$
                                                  tPos = tPos,$
                                                  chPos = chPos,$
                                                  zPos = zPos,$
                                                  clusPos = clusPos,$
                                                  segPos = segPos,$
                                                  cut_x = cut_x,$
                                                  cut_y = cut_y,$
                                                  stack_tlb = stack_tlb

  if (size(image, /n_dim) ne 2) or (max(image) eq 0) then return, image

  maxI0 = max(image)
  i0 = double(image) ; casting for decimal precision
  i0 /= maxI0 ; rescaling to 0-1 range

  whParam = (where((*(*self.pParamStruct).pNames) eq 'm_i'))[0]
  m_i = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'n_i'))[0]
  n_i = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'a_high'))[0]
  a_high = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'a_low'))[0]
  a_low = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'niter_a_high'))[0]
  niter_a_high = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'niter_a_low'))[0]
  niter_a_low = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'd_t'))[0]
  dt = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'Display_contour_windows'))[0] ; visualize
  visualize = (*(*self.pParamStruct).pValues)[whParam]
  windowNumber = 31

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Filtering_iterations'))[0]
  filterIterations = (*(*self.pParamStruct).pValues)[whParam] ; Zanella default 50
  whParam = (where((*(*self.pParamStruct).pNames) eq 'Filtering_iterations_edgemap'))[0]
  filterIterationsEdgemap = (*(*self.pParamStruct).pValues)[whParam] ; Zanella default 5
  whParam = (where((*(*self.pParamStruct).pNames) eq 'Filtering_beta'))[0]
  filterBeta = (*(*self.pParamStruct).pValues)[whParam] ; Zanella default 0.01
  whParam = (where((*(*self.pParamStruct).pNames) eq 'Filtering_dt'))[0]
  filterDt = (*(*self.pParamStruct).pValues)[whParam] ; Zanella default 0.1
  whParam = (where((*(*self.pParamStruct).pNames) eq 'Filtering_dt_edgemap'))[0]
  filterDtEdgemap = (*(*self.pParamStruct).pValues)[whParam] ; Zanella default 0.1

  ; Filtering
  i = self->geocurv(i0 = i0, d_t = filterDt, niter = filterIterations, dtg = filterDtEdgemap, beta = filterBeta, iter = filterIterationsEdgemap)
  if (visualize eq 1 ) then begin
    window, 2, title = 'Filtered image'
    tvscl, i
  endif

  ;Initialization of the level set function
  ;print, 'Pick a point in the center of membrane to segment'
  phi0 = s_initializeLevelSetFunction(i, INITTYPE = 'image')
  ;window, 3
  ;surface, phi0, I0 

  ; Edge indicator calculation
  dt = 0.1
  iter = 5
  beta = 0.1
  edge_indicator = self->g(image = I, dt = dt, iter = iter, beta = beta)
  if (visualize eq 1 ) then begin
    window, 4, title = 'Edge indicator'
    surface, edge_indicator
  endif

  ; Segmentation (level set function evolution)
  m_i          = 0.1
  n_i          = 10
  d_t          = 0.1
  a_high       = 1
  a_low        = 0.000001
  niter_a_high = 50000
  niter_a_low  = 50000
  visualize    = 2000
  figureNumber = 10 ; window number
  phi = self->subsurf(phi0 = phi0, I0 = I0, edge_indicator = edge_indicator, m_i = m_i, n_i = n_i,$
                      a_high = a_high, a_low = a_low, d_t = d_t, niter_a_high = niter_a_high, niter_a_low = niter_a_low,$
                      visualize = visualize, windowNumber = figureNumber)

  phiMax = max(phi, min = phiMin)
  levelSetValue = (phiMax - phiMin) / 2. ; Choice of level set value
  levelSetValue = 0
  thresh = 2
  contourPoints = where((phi le (-phiMax + thresh)) and (phi ge (-phiMax - thresh)), pointCount)
  if (pointCount gt 0) then begin
    print, 'returning 0-level set'
    res = make_array(size(image, /dim), /byte, value = 0)
    res[contourPoints] = 1
    return, res
  endif
  contour, phi, levels = [levelSetValue], /overplot
  ; TODO JJ finish this!
  phiScaled = phi + min(phi, max = maxPhi)
  phiScaled *= 255.0 / maxPhi
  ot = obj_new('C_sImageFilter_ThresholdOtsu')
  res = ot->apply(image = phiScaled)
  ;res = fltArr(size(image, /dim))
  ;res[contourPoints] = 1
  return, res
end


function C_sImageFilter_SubjectiveContour::init

  ImageFilterStruct = {Name:       'C_SubjectiveContour',$ ; Filter Name.
                       pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Widget Type.
                       pNames:      ptr_new(),$ ; Pointer on Filter Parameter Names.
                       pActive:     ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
                       pMin:        ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
                       pMax:        ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
                       pDeltas:     ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
                       pValues:     ptr_new()}  ; Pointer on Filter Parameter Values.

  ; Parameters of the subjective contours filter.
  FilterParamNames = ['m_i',$
                      'n_i',$
                      'd_t',$
                      'a_low',$
                      'a_high',$
                      'niter_a_low',$
                      'niter_a_high',$
                      'Filtering_beta',$
                      'Filtering_dt',$
                      'Filtering_dt_edgemap',$
                      'Filtering_iterations',$
                      'Filtering_iterations_edgemap',$
                      'Edgemap_beta',$
                      'Edgemap_dt',$
                      'Edgemap_iterations',$
                      'Display_contour_windows']

  FilterParamWidgetType = make_array(n_elements(FilterParamNames), /string, value = 'widget_slider')
  FilterParamValues = [0.1,$      ; m_i
                       10,$       ; n_i
                       0.1,$      ; d_t
                       0.000001,$ ; a_low
                       1,$        ; a_high
                       800000,$     ; niter_a_low
                       800000,$     ; niter_a_high
                       0.01,$     ; filtering_beta
                       0.1,$      ; filtering_dt
                       0.1,$      ; filtering_dt_edgemap
                       50,$       ; filtering_iterations
                       5,$        ; filtering_iterations_edgemap
                       0.1,$      ; edgemap_beta
                       0.1,$      ; edgemap_dt
                       5,$        ; edgemap_iterations
                       1]         ; display_contour_windows

  FilterParamActive = make_array(n_elements(FilterParamNames), /byte, value = 1)

  FilterParamMin = [0.,$
                    0.,$
                    0.,$
                    0.,$
                    0.01,$
                    1,$
                    1,$
                    0.01,$
                    0.,$
                    0.,$
                    0,$
                    0,$
                    0,$
                    0,$
                    0,$
                    0]

  FilterParamMax = [5.,$
                     5.,$
                     2.,$
                     2.,$
                     5.,$
                     1000000,$
                     1000000,$
                     10.,$
                     2,$
                     1,$
                     1,$
                     1,$
                     1,$
                     1,$
                     1,$
                     1]

  ImageFilterStruct.pWidgetType = ptr_new(FilterParamWidgetType, /no_copy)
  ImageFilterStruct.pActive = ptr_new(FilterParamActive, /no_copy)
  ImageFilterStruct.pValues = ptr_new(FilterParamValues, /no_copy)
  ImageFilterStruct.pDeltas = ptr_new(FilterParamDeltas, /no_copy)
  ImageFilterStruct.pNames = ptr_new(FilterParamNames, /no_copy)
  ImageFilterStruct.pMin = ptr_new(FilterParamMin, /no_copy)
  ImageFilterStruct.pMax = ptr_new(FilterParamMax, /no_copy)

  self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_SubjectiveContour__define
  tmp = {C_sImageFilter_SubjectiveContour,$
         pParamStruct:  ptr_new(),$
         pParamApplied: ptr_new(),$
         pSegImage:     ptr_new(),$
         inherits C_sImageFilter}
end
