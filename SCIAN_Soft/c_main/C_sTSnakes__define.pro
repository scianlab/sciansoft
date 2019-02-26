;_____________________________IOISIOI (ESTO ES ARTE)_____________________________
; NAME:            C_sTsnakes
;
; PURPOSE:         Tsnakes Class.
;
; CALLING SEQUENCE:result = obj_new('C_sTsnakes')
;
; METHODS:
;     setParams
;     getParams
;     getCoordsa
;
;_____________________________IOISIOI (ESTO ES ARTE)_____________________________

function C_sTsnakes::edgeMap, SIGMASQ = sigmasq
  compile_opt idl2, hidden

  if sigmasq gt 0 then begin
    ; The smooth edgemap is the squared - module of the gradient of the image convoluted with a Gaussian .
    x = (make_array(21,21,/index) mod 21 - 10)*exp(1)/5
    y = transpose(x)

    ; These are the partial derivatives of the Gaussian
    gaussx = exp(-(x^2+y^2)/(2*sigmasq)) * (-x)/sigmasq
    gaussy = exp(-(x^2+y^2)/(2*sigmasq)) * (-y)/sigmasq

    ; In a convolution the derivatives can be kicked to the kernel
    igx = ptr_new(convol(double(*(self.pImage)), gaussx, 255, /edge_truncate), /no_copy)
    igy = ptr_new(convol(double(*(self.pImage)), gaussy, 255, /edge_truncate), /no_copy)

  endif else begin
    dervx = [[0,0,0],[-1,0,1],[0,0,0]];
    dervy = [[0,-1,0], [0,0,0], [0,1,0]];

    igx = ptr_new(convol(double(*(self.pImage)), dervx, /edge_truncate), /no_copy)
    igy = ptr_new(convol(double(*(self.pImage)), dervy, /edge_truncate), /no_copy)
  endelse

  return, [igx, igy]     
end



pro C_sTsnakes::calcGVF

  eMap = self->edgeMap(sigmaSq = 1.0)
  ix = *(eMap[0])
  iy = *(eMap[1]) 
  
  ; The smoothed edge map is the squared norm of the smoothed intensity gradient normalized to [0,1]
  f = double(ix^2+iy^2)

  dervx = [[0,0,0],[-1,0,1],[0,0,0]];
  dervy = [[0,-1,0],[0,0,0],[0,1,0]];

  myvfx = convol(double(f), dervx, /edge_truncate)
  myvfy = convol(double(f), dervy, /edge_truncate)

  myquo = sqrt(myvfx^2+myvfy^2) + double(myvfx^2+myvfy^2 eq 0)
  
  ; We compute the partial derivatives of the edge map (spatial step = 1)
  fx = convol(f, [-1,0,1], 2, /edge_truncate)
  fy = convol(f, transpose([-1,0,1]), 2, /edge_truncate)

  ; ivector, fx, fy, title='Edgemap gradient', auto_color=2, RGB_TABLE=39, SCALE_ISOTROPIC=1, data_location=0, /no_saveprompt
  ; ivector, p1, p2, title='EPGVF norm', auto_color=2, RGB_TABLE=39, SCALE_ISOTROPIC=1, data_location=0, /no_saveprompt

  ; We get the dimensions of the image, they are useful :D
  sz = size(f)
  nx = fix(sz[1])
  ny = fix(sz[2])
  
  tau = self.gvfNoiseCut
  delta = self.gvfCutRange
  
  ; Here we calculate the weight functions g and h
  ftemp = f - tau
  ftemp = ftemp * double(ftemp ge -delta) + double(ftemp lt -delta) * (-delta)
  ftemp = ftemp * double(ftemp le  delta) + double(ftemp gt  delta) * ( delta)

  h  = -(ftemp)^3/(4.0*delta^3)+ 3.0*ftemp/(4.0*delta) + 0.5
  g = 1-h
  
  u = double(make_array(nx,ny))
  v = double(make_array(nx,ny))
  
  calc_GVF_wrapper, self.gvfMaxIt,     $
    nx, ny,                            $
    self.gvfStep,                      $
    u, v,                              $
    fx, fy,                            $
    g, h,                              $
    self.gvfError   
  
  quo = sqrt(u^2+v^2) + double(u^2+v^2 eq 0)

  ; The EPGVF is the normalized field we construct
  self.pU= ptr_new(u/quo, /no_copy)
  self.pV= ptr_new(v/quo, /no_copy)
  
  ;ivector,  *self.pU, *self.pV, auto_color=2,$
  ;   length_scale=0.1,  SCALE_ISOTROPIC=1, data_location=0, $
  ;   RGB_TABLE=39, /no_saveprompt
end

pro C_sTsnakes::makeContours, AUTOINIT = autoInit, INITSNAKES = initSnakes
  funcname = 'evolve_snakes'
  libpath =  filepath('tsnakeslib.so', subdir=['_dll'], root_dir=s_getPathForProject())
  
  if(autoInit eq 0) then begin 
    snakes  = initSnakes 
    sz = size(snakes)
    nsnakes = sz[2]/2
  endif else begin
    snakes = [0]
    nsnakes = 0
  endelse 
  
  snakes = evolve_snakes(  $
    fix(*self.pImage), $ ;No 0
    nsnakes,           $ ;No 1 ; Parameter ignored if "autoInit" is 1
    snakes,            $ ;No 2 ; Parameter ignored if "autoInit" is 1
    self.defSteps,     $ ;No 3
    self.gvfWeight,    $ ;No 4
    self.inflWeight,   $ ;No 5
    self.int1LftSide,  $ ;No 6
    self.int1RgtSide,  $ ;No 7
    self.int2LftSide,  $ ;No 8
    self.int2RgtSide,  $ ;No 9
    self.elastFactor,  $ ;No 10
    self.rigFactor,    $ ;No 11
    1.0D,              $ ;No 12 ; Gamma its fixed to 1
    self.evolStep,     $ ;No 13
    *self.pU,          $ ;No 14
    *self.pV,          $ ;No 15
    self.evolIts,      $ ;No 16
    autoInit,          $ ;No 17 ; If off ignores seed snakes and nsnakes parameters, if on seedRadius is required
    self.resFactor,    $ ;No 18
    self.seedRadius    $ ;No 19 
  )

  ; Saving the newly calculated snakes
  if size(snakes, /n_dimensions) gt 0 then begin
    sz = size(snakes, /dimensions)
    nsnakes = sz[1]/2
    self.pXCoords = ptr_new(ptrarr(nsnakes, /nozero))
    self.pYCoords = ptr_new(ptrarr(nsnakes, /nozero))
    
    for i=0l, nsnakes-1 do begin
      snakex = [snakes[1:long(snakes[0,2*i]),2*i], snakes[1,2*i]]
      snakey = [snakes[1:long(snakes[0,2*i]),2*i+1], snakes[1,2*i+1]]  
      
      (*self.pXCoords)[i] = ptr_new(snakex, /no_copy)
      (*self.pYCoords)[i] = ptr_new(snakey, /no_copy)
    endfor
  endif  
end


function C_sTsnakes::getSnakes
  return, [self.pXCoords, self.pYCoords]
end
;*****************************************************************************************************
;+
; NAME:   cleanup
;
; PURPOSE:Object class destructor method.
;
; SYNTAX: Called automatically when the object is destroyed.
;-
;*****************************************************************************************************
pro C_sTsnakes::cleanup
   ptr_free, self.pImage
   ptr_free, self.pXCoords
   ptr_free, self.pYCoords
   ptr_free, self.pU
   ptr_free, self.pV
end


;*****************************************************************************************************
;+
; NAME:   init
;
; PURPOSE:Object class creator method.
;
; SYNTAX: Called automatically when the object is created.
;
; ARGUMENTS:
;
; KEYWORDS:
;
;
;
; MODIFICATION HISTORY:
;
;        Original version:ActiveContour.pro Written by David W. Fanning, 1 December 2003.
;        Copyright Fanning Software Consulting, Inc. All rights reserved.
;        This version:C_sActiveContour.pro Written by Jorge Jara and Steffen Härtel, 2005-2006
;        http://scian.cl
;-
;*****************************************************************************************************
function C_sTsnakes::init,                                                         $
         PIMAGE = pImage, PXCOORDS = pXCoords, PYCOORDS = pYCoords, PNPTS = pNpts, $ 
         PU = pU, PV = pV, ELASTFACTOR = elastFactor, RIGFACTOR = rigFactor,       $
         INT1LFTSIDE = int1LftSide, INT1RGTSIDE = int1RgtSide,                     $
         INT2LFTSIDE = int2LftSide, INT2RGTSIDE = int2RgtSide,                     $
         DEFSTEPS = defSteps,                                                      $
         NSNAKES = nSnakes, EVOLSTEP = evolStep, EVOLITS = evolIts,                $ 
         RESFACTOR = resFactor, GVFWEIGHT = gvfWeight, GVFSTEP = gvfStep,          $
         GVFERROR = gvfError, GVFMAXIT = gvfMaxIt, GVFNOISECUT = gvfNoiseCut,      $
         GVFCUTRANGE = gvfCutRange, INFLWEIGHT = inflWeight,                       $
         SEEDRADIUS = seedRadius                                                   
                                                    
   if n_elements(*pImage) eq 0 then begin
      ok = dialog_message('Active Contour Error - No input image')
      return, 0
   endif

   ; Check dimensions.
   if size(*pImage, /n_dimensions) ne 2 then begin
      ok = dialog_message('The image argument must be a 2D array.')
      return, 0
   endif


   self.pImage = pImage
   self.pXCoords = pXCoords 
   self.pYCoords = pYCoords
   self.pNpts = pNpts
   self.pU = pU
   self.pV = pV
   
   
   ; Check keyword parameters.
   if n_elements(elastFactor) eq 0 then self.elastFactor = 20.0D  else self.elastFactor = elastFactor
   if n_elements(rigFactor)   eq 0 then self.rigFactor   = 10.0D  else self.rigFactor   = rigFactor
   if n_elements(int1LftSide) eq 0 then self.int1LftSide  = 160   else self.int1LftSide  = int1LftSide
   if n_elements(int1RgtSide) eq 0 then self.int1RgtSide  = 255   else self.int1RgtSide  = int1RgtSide
   if n_elements(int2LftSide) eq 0 then self.int2LftSide  = 160   else self.int2LftSide  = int2LftSide
   if n_elements(int2RgtSide) eq 0 then self.int2RgtSide  = 255   else self.int2RgtSide  = int2RgtSide
   if n_elements(defSteps)    eq 0 then self.defSteps    = 5      else self.defSteps    = defSteps 
   if n_elements(nSnakes)     eq 0 then self.nSnakes     = 0      else self.nSnakes     = nSnakes
   if n_elements(evolStep)    eq 0 then self.evolStep    = 0.001D else self.evolStep    = evolStep
   if n_elements(evolIts)     eq 0 then self.evolIts     = 100    else self.evolIts     = evolIts
   if n_elements(resFactor)   eq 0 then self.resFactor   = 0.4D   else self.resFactor   = resFactor   
   if n_elements(gvfWeight)   eq 0 then self.gvfWeight   = 50.0D  else self.gvfWeight   = gvfWeight
   if n_elements(gvfStep)     eq 0 then self.gvfStep     = 0.01D  else self.gvfStep     = gvfStep
   if n_elements(gvfError)    eq 0 then self.gvfError    = 0.01D  else self.gvfError    = gvfError
   if n_elements(gvfMaxIt)    eq 0 then self.gvfMaxIt    = 1000l  else self.gvfMaxIt    = gvfMaxIt
   if n_elements(gvfNoiseCut) eq 0 then self.gvfNoiseCut = 0.1D   else self.gvfNoiseCut = gvfNoiseCut
   if n_elements(gvfCutRange) eq 0 then self.gvfCutRange = 0.05D  else self.gvfCutRange = gvfCutRange
   if n_elements(inflWeight)  eq 0 then self.inflWeight  = 49.0D  else self.inflWeight  = inflWeight
   if n_elements(seedRadius)  eq 0 then self.seedRadius  = 3.0D   else self.seedRadius  = seedRadius
   return, 1
end


;*****************************************************************************************************
;
; NAME:   C_sTsnakes class definition.
;
; PURPOSE:C_sTsnakes object's structure class definition code.
;
;*****************************************************************************************************
pro C_sTSnakes__define
   class = { C_sTsnakes,  $
      pImage:ptr_new(),   $; 2D image for which the snake is being calculated.
      pXCoords:ptr_new(), $; X coordinates of the snakes.
      pYCoords:ptr_new(), $; Y coordinates of the snakes.
      pU:ptr_new(),       $; GVF X coordinate.
      pV:ptr_new(),       $; GVF Y coordinate.
      pNpts:ptr_new(),    $; Number of points in each snake
      nSnakes:0,          $; Number of snakes
      elastFactor:0D,     $; Elasticity parameter (snake's tension).
      rigFactor:0D,       $; Rigidity parameter (snake's rigidity).
      int1LftSide:0,      $; Left side of the first interior the zone
      int1RgtSide:0,      $; Right side of the first the interior zone
      int2LftSide:0,      $; Left side of the second interior the zone
      int2RgtSide:0,      $; Right side of the second the interior zone
      defSteps:0,         $; Number of steps between reparametrization
      evolStep:0D,        $; Step for the resolution of the t-snakes method.
      evolIts:0l,         $; Number of iterations for the snake to converge.
      resFactor:0D,       $; Resolution factor for the t-snake method. Increase to get a finer grid.
      gvfWeight:0D,       $; External force weight
      gvfStep:0D,         $; Step to solve the GVF equation
      gvfError:0D,        $; Error tolerance for the GVF field
      gvfMaxIt:0,         $; Maximum of iterations for the GVF field
      gvfNoiseCut:0D,     $; Cut point for the noise, if the image is noisy level it up
      gvfCutRange:0D,     $; Range of the cut curve
      inflWeight:0D,      $; Inflation force weight
      seedRadius:3.0D     $; Radius of the seed snakes     
    }
end