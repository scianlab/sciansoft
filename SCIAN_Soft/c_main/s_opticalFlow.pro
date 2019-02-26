;_____________________________IOISIOI (THIS IS ART)_____________________________
;
; NAME:
; opticalFlowHS3D
;
; 3D Horn-Schunck optical flow computation
;
; AUTHOR:
;      Jorge Jara (2011)
;
; NOTES:
;      This is a method under development, it has not been yet validated and can be subject to bugs. It lacks
;      the full set of options of the 2D version (even when the method signature is almost the same).
;      "float" data type is used to restrain memory usage, ~56x the size of one of the images (~23x for both)
;
;_____________________________IOISIOI (THIS IS ART)_____________________________


;*****************************************************************************************************
;+
; NAME:      opticalFlowHS
;
; PURPOSE:   Horn & Schunck optical flow computation.
;
; ARGUMENTS:
;            alpha      - Global regularization parameter.
;            iterations - Number of iterations used for the optical flow computation.
;
;*****************************************************************************************************
pro opticalFlowHS, image1, image2, u, v, alpha = alpha,$
                                         iterations = iterations,$
                                         verbose = verbose,$
                                         doShift = doShift,$
                                         boundaryConditions = boundaryConditions,$
                                         iterImprovementThreshold = iterImprovementThreshold,$
                                         energy=energy, $
                                         nwarps=nwarps
  ; Check keyword parameters.
  if n_elements(alpha)                    eq 0 then alpha = 20.0d
  if n_elements(iterations)               eq 0 then iterations = 200L
  if n_elements(boundaryConditions)       eq 0 then boundaryConditions = 1b ; consider boundary conditions in Horn1981
  if n_elements(doShift)                  eq 0 then doShift = 0b
  if n_elements(verbose)                  eq 0 then verbose = 1b
  if n_elements(iterImprovementThreshold) eq 0 then iterImprovementThreshold = 1e-6
  if n_elements(nwarps)                   eq 0 then nwarps = 0

  iterImprovementThreshold = 1e-6
  if verbose eq 1b then print, 'HS-OF computation starting...'

  dimI = size(image1, /dim)
  xSize = dimI[0]
  ySize = dimI[1]

  image1 *= 1. ; to ensure float data type
  image2 *= 1.

  imagecorrected=image2
  alpha2 = alpha * alpha
  vf = fltArr(dimI)
  uf = fltArr(dimI)
      
  ; "local average" kernel for laplacian (does not includes the central point)
  kernel = fltArr(3, 3)
  kernel[[1, 3, 5, 7]] = 1 / 6.
  kernel[[0, 2, 6, 8]] = 1 / 12.
        
  startHS = sysTime(1)
      
  for wi=0, nwarps do begin
  
    ; The three partial derivatives (Ex, Ey, Et) are estimated at the center of an 8-pixel cube
    if verbose then startTime = sysTime(1)
  
    getDerivatives, image1, imagecorrected, ex=ex, ey=ey, et=et, doShift=doShift, alternativeFlag=1b
  
    if verbose then begin
      endTime = sysTime(01)
      print, 'Derivative calculation: ' + string(endTime - startTime) + 's'
    endif
  
      d = (alpha2 + ex^2 + ey^2)
      
      iterImprovementRatio=1
      firstIteration = 1
      i = 0L
      v = fltArr(dimI)
      u = fltArr(dimI)
    
      if verbose then startTime = sysTime(1)
      
      while (i le iterations-1) and (iterImprovementRatio gt iterImprovementThreshold) do begin

        uAnt = u
        vAnt = v
    
        ; Gauss-Seidel iteration
        um = convol(u, kernel)
        vm = convol(v, kernel)
        c = (ex * um + ey * vm + et) / d
    
        u = um - ex * c
        v = vm - ey * c

        energy=(ex * u + ey * v + et)*(ex * u + ey * v + et)+alpha*(u*u+v*v)
    
        ; Treatment of boundary conditions (zero normal derivative)
        if boundaryConditions then begin
          u[1:xSize-2, 0]       = u[1:xSize-2, 1]
          v[1:xSize-2, 0]       = v[1:xSize-2, 1]
          u[1:xSize-2, ySize-1] = u[1:xSize-2, ySize-2]
          v[1:xSize-2, ySize-1] = v[1:xSize-2, ySize-2]
          u[0, 1:ySize-2]       = u[1, 1:ySize-2]
          v[0, 1:ySize-2]       = v[1, 1:ySize-2]
          u[xSize-1, 1:ySize-2] = u[xSize-2, 1:ySize-2]
          v[xSize-1, 1:ySize-2] = v[xSize-2, 1:ySize-2]

          ; and the corners
          u[0, 0]             = u[1, 1]
          v[0, 0]             = v[1, 1]
          u[0, ySize-1]       = u[1, ySize-2]
          v[0, ySize-1]       = v[1, ySize-2]
          u[xSize-1, 0]       = u[xSize-2, 1]
          v[xSize-1, 0]       = v[xSize-2, 1]
          u[xSize-1, ySize-1] = u[xSize-2, ySize-2]
          v[xSize-1, ySize-1] = v[xSize-2, ySize-2]
        endif

        if ~firstIteration then begin
          iterImprovementRatio  = total((u - uAnt)^2 + (v - vAnt)^2) / total(uAnt^2 + vAnt^2)
          iterImprovementRatio2 = max(abs(u - uAnt)) / (max(abs(uAnt))+0.1) > max(abs(v - vAnt)) / (max(abs(vAnt)+0.1))
          iterImprovementRatio3 = max(abs([u - uAnt, v - vAnt]))
          iterImprovementRatio4 = max(abs([u - uAnt, v - vAnt])) / max(abs([uAnt, vAnt])) 
          iterImprovementRatio5 = max(abs(u - uAnt)) > max(abs(v - vAnt))
       endif else firstIteration = 0
       i += 1

       if verbose then begin
        print, 'Warping ', wi, ' Number of iterations: ', i
        print, 'Iter. improvement measure: ', iterImprovementRatio
       endif 
       
      endwhile
      
      uf=uf+u
      vf=vf+v
      
      flowError, image2, uf, vf, imagewarped

;      path='C:\Users\Laboratorio\Documents\Mauricio\warp0.tif'
;      WRITE_TIFF, path, imagecorrected
;      path='C:\Users\Laboratorio\Documents\Mauricio\warp1.tif'
;      WRITE_TIFF, path, imagewarped
      imagecorrected=imagewarped  
      
      if verbose then begin
        endTime = sysTime(1)
        print, 'Number of iterations: ', i
        print, 'Avg. Time Gauss-Seidel iteration: ' + string((endTime - startTime) / iterations) + 's'
        print, 'HS optical flow total time: ' + string(endTime - startHS) + 's'
        print, 'Iter. improvement measure: ', iterImprovementRatio5
        opticalFlowPrintInfo, u, v
      endif
  
    endfor ;endfor warping
  
    u=uf
    v=vf
    
    if verbose eq 1b then print, 'HS-OF calculation complete.'
end


;*****************************************************************************************************
;+
; NAME:      calcCLGOF
;
; PURPOSE:   Bruhn et al. optical flow computation. Implemented in an external DLL file.
;
; ARGUMENTS:
;            alpha      - Global regularization parameter.
;            rho        - Local regularization parameter (gaussian smoothing of space and time derivatives).
;                         This value affects a gaussian kernel size according to
;            iterations - Number of iterations used for the optical flow computation.
;-
;*****************************************************************************************************
pro opticalFlowCLG, image1, image2, u, v, alpha = alpha,$
                                          rho = rho,$
                                          sigma = sigma,$
                                          iterations = iterations,$
                                          verbose = verbose,$
                                          nScales = nScales,$
                                          scaleFactor = scaleFactor,$
                                          coupledMode = coupledMode,$
                                          wFactor = wFactor
  ; Check keyword parameters
  if n_elements(alpha)       eq 0 then alpha = 1.0d
  if n_elements(rho)         eq 0 then rho   = 5.0d
  if n_elements(iterations)  eq 0 then iterations = 200L
  if n_elements(sigma)       eq 0 then sigma = 0.85d
  if n_elements(verbose)     eq 0 then verbose = 0b
  ; incorporated into the ipol-clg
  if n_elements(nScales)     eq 0 then nScales = 10.0d ;the dll checks for the max number of scales
  if n_elements(scaleFactor) eq 0 then scaleFactor = 0.65d ;between levels in the pyramid
  if n_elements(coupledMode) eq 0 then coupledMode = 0b ;0=SOR, 1=coupled Gauss-Seidel
  if n_elements(wFactor)     eq 0 then wFactor = 1.9d ;iteration factor for SOR (only)

  
  ;look for the dll in the _dll folder of the project 
  libpath =  filepath('flowcalc.dll', subdir=['_dll'], root_dir=s_getPathForProjectGeneric())
  
  szI = size(image1, /dim)
  u = dblArr(szI)
  v = dblArr(szI)

  if verbose then startTime = sysTime(1)

    ; Runs a function in the DLL
    ; Note: when working with a DLL and call_external, a type error has a confusing message.
    ; IDL tends to hang up. (The error message may be something like "handler of variable corrupted".) 
    
  fRes = call_external(libpath,$
                       'idlgetclgflow',$
                       double(image1),$ ; required double data type
                       double(image2),$
                       u,$
                       v,$
                       fix(szI[0], type = 2),$ ; force integer data type (sometimes needed)
                       fix(szI[1], type = 2),$
                       fix(0, type =2),$ ;old parameter
                       double(alpha), double(rho), double(sigma), double(iterations),$ 
                       double(nScales), double(scaleFactor), fix(coupledMode, type =2), double(wFactor),$
                       fix(verbose, type =2), /unload)
  if verbose then begin
    endTime = sysTime(1)
    print, 'CLG optical flow total time: ' + string(endTime - startTime) + 's'
    opticalFlowPrintInfo, u, v
  endif
  ;print, 'CLG-OF calculation complete.'
end

;*****************************************************************************************************
;+
; NAME:      opticalFlowLK
;
; PURPOSE:   Lucas & Kanade optical flow computation.
;
; ARGUMENTS:
;            tau      - threshold for the eigenvalues/gradient.
;            iterations - Number of iterations used for the optical flow computation.
;-
;*****************************************************************************************************
pro opticalFlowLK, image1, image2, u, v, tau = tau,$
                                         iterations = iterations,$
                                         verbose = verbose,$
                                         iterImprovementRatio = iterImprovementRatio,$
                                         iterImprovementThreshold = iterImprovementThreshold,$
                                         gradient=gradient,$
                                         indComputed=indComputed,$
                                         l1=l1,$
                                         l2=l2
  ; Check keyword parameters.
  if n_elements(tau)                      eq 0 then tau = 0.0d
  if n_elements(iterations)               eq 0 then iterations = 100L
  if n_elements(stopCrit)                 eq 0 then stopCrit = 0b           ; 0: num iterations / 1: no improvement
  if n_elements(doShift)                  eq 0 then doShift = 0b
  if n_elements(verbose)                  eq 0 then verbose = 1b
  if n_elements(iterImprovementThreshold) eq 0 then iterImprovementThreshold = 1e-6

  print, 'LK-OF computation starting...', tau
  epsilon=1e-16

  dimI = size(image1, /dim)
  xSize = dimI[0]
  ySize = dimI[1]

  ex = fltArr(dimI)
  ey = fltArr(dimI)
  et = fltArr(dimI)
  image1 *= 1.0 ; to ensure float (or double) data type
  image2 *= 1.0

  startHS = sysTime(1)
  if verbose then startTime = sysTime(1)
  
  getDerivatives, image1, image2, ex=ex, ey=ey, et=et, doShift=doShift, alternativeFlag=2
  
  if verbose then begin
    endTime = sysTime(01)
    print, 'Derivative calculation: ' + string(endTime - startTime) + 's'
  endif

  i = 0L
  ; "local average" kernel to smooth derivatives
  kernelW = fltArr(5, 5)
  kernelW[[0, 4, 20, 24]] = 1 / 256.
  kernelW[[1, 3, 5, 9, 15, 19, 21, 23]] = 4 / 256.
  kernelW[[2, 10, 14, 22]] = 6 / 256.
  kernelW[[6, 8, 16, 18]] = 16 / 256.
  kernelW[[7, 11, 13, 17]] = 24 / 256.
  kernelW[[12]] = 36 / 256.

  exx=convol(float(ex*ex), kernelW, center=1)
  eyy=convol(float(ey*ey), kernelW, center=1)
  exy=convol(float(ex*ey), kernelW, center=1)
  
  ext=convol(float(ex*et), kernelW, center=1)
  eyt=convol(float(et*et), kernelW, center=1)

  determinant=exx*eyy-exy*exy
  determinant_cofactor=(exx+epsilon)*(eyy+epsilon)-exy*exy
  trace=exx+eyy
  normanx=convol(sqrt(trace)*ex, kernelW, center=1)
  normany=convol(sqrt(trace)*ey, kernelW, center=1)
  
  l1=trace/2+sqrt((trace^2)/4.0-determinant)
  l2=trace/2-sqrt((trace^2)/4.0-determinant)
  
  condition=LKcondition( image1, image2, epsilon)
  tocompute =where( condition gt tau +epsilon )
  
  ;TODO: detect aperture?, ie when l1 gt 0 and l2 eq 0
  ;tocompute1=where( (abs(l1) gt epsilon )) ;and abs(l2) le epsilon) )
  tocompute1 = [ -1 ]
  if verbose then startTime = sysTime(1)
  
  v = fltArr(dimI)
  u = fltArr(dimI)
  u[*,*]=0.0
  iterImprovementRatio=1
  
  
  while (i lt iterations) and (iterImprovementRatio gt iterImprovementThreshold) do begin

    uAnt = u
    vAnt = v
    eta=et
        
    px=convol(float(ex*eta), kernelW, center=1)
    py=convol(float(ey*eta), kernelW, center=1)
    
    ;normal case
    if tocompute[0] ne -1 then begin
      u(tocompute)=uAnt(tocompute)+(-eyy(tocompute)*px(tocompute)+exy(tocompute)*py(tocompute))/determinant(tocompute)
      v(tocompute)=vAnt(tocompute)+( exy(tocompute)*px(tocompute)-exx(tocompute)*py(tocompute))/determinant(tocompute)
      
    endif
      
    ;aperture problem -> cofactor inversion
    if tocompute1[0] ne -1 then begin
       
        u(tocompute1)=uAnt(tocompute1)+(-(eyy(tocompute1)+epsilon)*px(tocompute1)+exy(tocompute1)*py(tocompute1)) /determinant_cofactor(tocompute1)
        v(tocompute1)=vAnt(tocompute1)+(  exy(tocompute1)*px(tocompute1)-(exx(tocompute1)+epsilon)*py(tocompute1))/determinant_cofactor(tocompute1)
        
        print, 'LK normal direction in : ', n_elements(tocompute1)
    endif
    
    ;check for improvement
    if tocompute[0] ne -1 or tocompute1[0] ne -1 then begin
    
      if tocompute[0] ne -1 and tocompute1[0] ne -1 then indexes=[tocompute, tocompute1]
      if tocompute[0] ne -1 and tocompute1[0] eq -1 then indexes=tocompute
      if tocompute[0] eq -1 and tocompute1[0] ne -1 then indexes=tocompute1
    
      flowError, image2, u, v, imagecorrected
      getDerivatives, image1, imagecorrected, et=et, alternativeFlag=2
     
      iterImprovementRatio = total(abs(eta(indexes)))
      test=total(abs(et(indexes)))
      print, 'Iter. improvement measures: ', iterImprovementRatio
      if finite(test) ne 1 then begin
        print, 'Ratio is NaN'
      endif
    
      if iterImprovementRatio lt total(abs(et(indexes))) then begin
        print, 'Iter. getting worst, stop forced: ', total(abs(et(indexes)))
        if i gt 1 then begin
          u(indexes)=uAnt(indexes)
          v(indexes)=vAnt(indexes)
          et=eta
        endif
        break ;stop iterations
        
       endif
    
       print, 'LK iterations: ', i, ' ROI size:', n_elements(indexes)
       print, 'Iter. improvement measures: ', iterImprovementRatio 
       
      endif

    i += 1

  endwhile
  
  if verbose then begin
    endTime = sysTime(1)
    print, 'Number of iterations: ', i
    print, 'Avg. Time iteration: ' + string((endTime - startTime) / iterations) + 's'
    print, 'LK optical flow total time: ' + string(endTime - startHS) + 's'
    print, 'Iter. improvement measures: ', iterImprovementRatio
    opticalFlowPrintInfo, u, v
  endif
  
  if tocompute[0] eq -1 and tocompute1[0] eq -1 then indexes=[-1] 
  
  gradient=ex+ey
  indComputed=indexes
  
end

;*****************************************************************************************************
;+
; NAME:      opticalFlowMultiscale
;
; PURPOSE:   Horn & Schunck or CLG optical flow computation using multiscale.
;
; ARGUMENTS:
;            alpha      - Global regularization parameter.
;            iterations - Number of iterations used for the optical flow computation.
;            scale      - Number of scales > 0
;            method     - 0: HS, 1: CLG, 2: LK
;-
;*****************************************************************************************************
pro opticalFlowMultiscale, image1, image2, scales, method, u, v, alpha = alpha,$
                                         iterations = iterations,$
                                         verbose = verbose,$
                                         doShift = doShift,$
                                         stopCrit = stopCrit,$
                                         boundaryConditions = boundaryConditions,$
                                         iterImprovementThreshold = iterImprovementThreshold,$
                                         energy1=energy1,$
                                         energy2=energy2,$
                                         tau=tau,$
                                         indComputed=indComputed, $
                                         l1=l1, $
                                         l2=l2
                                         
  ; Check keyword parameters.
  if n_elements(alpha)                    eq 0 then alpha = 20.0d
  if n_elements(iterations)               eq 0 then iterations = 200L
  if n_elements(boundaryConditions)       eq 0 then boundaryConditions = 1b ; consider boundary conditions in Horn1981
  if n_elements(stopCrit)                 eq 0 then stopCrit = 0b           ; 0: num iterations / 1: no improvement
  if n_elements(doShift)                  eq 0 then doShift = 0b
  if n_elements(verbose)                  eq 0 then verbose = 1b
  if n_elements(iterImprovementThreshold) eq 0 then iterImprovementThreshold = 1e-6
  
  kernel_pre=gaussianKernel(.8)   ;pre-smoothing
  kernel    =gaussianKernel(1.03) ;anti-aliasing
  
  th=0.0
  imgsize=size(image1, /dim)
  
  smooth1=ptrarr(scales)
  smooth2=ptrarr(scales)
  levelsize=intarr(scales,2)
  u=fltArr(imgsize)
  v=fltArr(imgsize)
  
  levelsize[0,*]=imgsize
  smooth1[0]    =ptr_new(0)
  *smooth1[0]   =convol(float(image1), kernel_pre)
  smooth2[0]    =ptr_new(0)
  *smooth2[0]   =convol(float(image2), kernel_pre)
  
  ;check if smallest scale is bigger than smoothing kernel
  eta=1.0/.8
  eta2=eta*eta
  kernel_pre=gaussianKernel(.6*sqrt(eta2 -1))   ;pre-smoothing
  
  smallestsize=(min(imgsize)+1.0)/(eta^(scales-1.0))
  if smallestsize le max(size(kernel_pre, /dim)) then begin
    scales=floor(alog(max(size(kernel_pre, /dim)))/alog(eta)+1)
    print, 'Scale is too small, only ', string(scales), ' scales were retained'
  end
  
  ;init all scales   
  for i=1, scales-1 do begin
    levelsize[i,*]=floor((levelsize[i-1,*]+1)/eta)
    smooth1[i]=ptr_new(0)
    smooth2[i]=ptr_new(0)
  endfor
    
  ;low-pass and subsampling   
  for i=1, scales-1 do begin
    *smooth1[i]=convol(congrid(*smooth1[i-1], levelsize[i,0], levelsize[i,1], /CUBIC), kernel_pre)
    *smooth2[i]=convol(congrid(*smooth2[i-1], levelsize[i,0], levelsize[i,1], /CUBIC), kernel_pre)
  endfor
  
  i=scales-1
  while i ge 0 do begin
  
    if verbose eq 1b then print, 'Computing scale ', i
    if i eq 0 then begin
    
      ;if more than 1 scale project, else don't
      if scales gt 1 then begin
        flowProjection, *smooth2[i], ulocal, vlocal, imagecorrected, uproj, vproj, eta

      endif else begin
        imagecorrected=*smooth2[i]
        uproj=fltArr(levelsize[i,*])
        vproj=fltArr(levelsize[i,*])
      endelse
      
      utmp=0
      vtmp=0
      if method eq 0 then begin 
        opticalFlowHS, *smooth1[i], imagecorrected, utmp, vtmp, ALPHA = alpha, ITERATIONS = iterations, DOSHIFT = doShift, $
          verbose = verbose, boundaryConditions = boundaryConditions
      endif
      if method eq 1 then begin
        opticalFlowCLG, *smooth1[i], imagecorrected, utmp, vtmp, ALPHA = alpha, ITERATIONS = iterations, RHO = 5.0 , verbose = verbose  
      endif
      if method eq 2 then begin
        opticalFlowLK, *smooth1[i], imagecorrected, utmp, vtmp, tau=tau, ITERATIONS = 2.0, verbose = verbose,$
          gradient=gradient, indComputed=indComputed, l1=l1, l2=l2  
      endif
      
      ulocal=uproj+utmp
      vlocal=vproj+vtmp
      
    endif else begin

      ;if higher level don't projet else do it
      if i eq scales-1 then begin
        imagecorrected=*smooth2[i]
        uproj=fltArr(levelsize[i,*])
        vproj=fltArr(levelsize[i,*])
      endif else begin
        ;project the previous optical flow
        flowProjection, *smooth2[i], ulocal, vlocal, imagecorrected, uproj, vproj, eta
      endelse
      
      if method eq 0 then begin
          opticalFlowHS, *smooth1[i], imagecorrected, utmp, vtmp, ALPHA = alpha, ITERATIONS = iterations, DOSHIFT = doShift, $
                verbose = verbose, boundaryConditions = boundaryConditions
      endif
      if method eq 1 then begin
          opticalFlowCLG, *smooth1[i], imagecorrected, utmp, vtmp, ALPHA = alpha, ITERATIONS = iterations, RHO = 5.0 , verbose = verbose  
      endif
      if method eq 2 then begin
        opticalFlowLK, *smooth1[i], imagecorrected, utmp, vtmp, tau=tau, ITERATIONS = 2.0, verbose = verbose,$
          gradient=gradient, indComputed=indComputed, l1=l1, l2=l2  
      endif
      
      mag=sqrt(utmp*utmp + vtmp*vtmp)
      sanity=where(mag lt th) ;sanity check
      if n_elements(sanity) ne 1 then begin
        utmp(sanity)=0
        vtmp(sanity)=0
      endif
      
      ulocal=utmp+uproj
      vlocal=vtmp+vproj
        
    endelse
    
    i=i-1
  endwhile


  getDerivatives, image1, image2, ex=ex, ey=ey, et=et, alternativeFlag=1
  
  ;final optical flow                                     
  u=ulocal
  v=vlocal
    
  ;energy of the estimation for HS and CLG
  if method eq 0 or method eq 1 then begin
    energy1=(ex * u + ey * v + et)*(ex * u + ey * v + et)
    energy2 =(u*u+v*v)
  endif
  ;energy for LK method
  if method eq 2 then begin
    energy1=ex+ey
    energy2=ex+ey
  endif
  
end

;*****************************************************************************************************
;+
; NAME:      flowProjection
;
; PURPOSE:   given the optical flow u,v a certain image is compensated (serial multiscale)
;
; ARGUMENTS:
;            image      
;-
;*****************************************************************************************************
pro flowProjection, image, u, v, imagecorrected, ucorrected, vcorrected, eta
  eta2=eta*eta
  kernel_pre=gaussianKernel(.6*sqrt(eta2 -1))   ;pre-smoothing
  ;kernel_pre=gaussianKernel(.8)  
  
  imgsize=size(image, /dim)
  ucorrected= fltArr(imgsize)
  vcorrected= fltArr(imgsize)
  imagecorrected= fltArr(imgsize)
  
  ucorrected=median(congrid(eta*u, imgsize[0], imgsize[1], /INTERP), 3)
  vcorrected=median(congrid(eta*v, imgsize[0], imgsize[1], /INTERP), 3)
 
  x=findgen(imgSize[0])
  y=findgen(imgSize[1])
   
  xarr = x # Replicate(1, N_Elements(y))
  yarr = Replicate(1, N_Elements(x)) # y

  Dx=xarr+ucorrected
  Dy=yarr-vcorrected
 
  x0=floor(Dx)
  y0=floor(Dy)

  ax=Dx mod 1
  ay=Dx mod 1

  ;bilinear interpolation
  for i=0,imgsize[0]-1 do begin
    for j=0,imgsize[1]-1 do begin

        if x0[i,j] lt 0 then x0[i,j]=1
        if y0[i,j] lt 0 then y0[i,j]=1
        if x0[i,j] ge imgsize[0]-1 then x0[i,j]=imgsize[0]-2
        if y0[i,j] ge imgsize[1]-1 then y0[i,j]=imgsize[1]-2
        
        imagecorrected[i,j]=(1.0-ax[i,j])*(1.0-ay[i,j])*image[x0[i,j], y0[i,j]] + ax[i,j]*(1.0-ay[i,j])*image[x0[i,j]+1.0, y0[i,j]] + $
            (1.0-ax[i,j])*ay[i,j]*image[x0[i,j],y0[i,j]+1.0]+ax[i,j]*ay[i,j]*image[x0[i,j]+1,y0[i,j]+1]
    endfor
  endfor
    
  imagecorrected=convol(imagecorrected, kernel_pre)
end


;*****************************************************************************************************
;+
; NAME:      flowProjection
;
; PURPOSE:   given the optical flow u,v a certain image is compensated (serial multiscale)
;
; ARGUMENTS:
;            image      
;-
;*****************************************************************************************************
pro flowError, image, u, v, imagecorrected
  
  kernel_pre=gaussianKernel(.8)   ;pre-smoothing
  
  imgsize=size(image, /dim)
  ucorrected= median(u, 3)
  vcorrected= median(v, 3)
  imagecorrected= fltArr(imgsize)
 
  x=findgen(imgSize[0])
  y=findgen(imgSize[1])
   
  xarr = x # Replicate(1, N_Elements(y))
  yarr = Replicate(1, N_Elements(x)) # y

  Dx=xarr+ucorrected
  Dy=yarr-vcorrected
 
  x0=floor(Dx)
  y0=floor(Dy)

  ax=Dx mod 1
  ay=Dx mod 1

  ;bilinear interpolation
  for i=0,imgsize[0]-1 do begin
    for j=0,imgsize[1]-1 do begin

        if x0[i,j] lt 0 then x0[i,j]=1
        if y0[i,j] lt 0 then y0[i,j]=1
        if x0[i,j] ge imgsize[0]-1 then x0[i,j]=imgsize[0]-2
        if y0[i,j] ge imgsize[1]-1 then y0[i,j]=imgsize[1]-2
        
            
        imagecorrected[i,j]=(1.0-ax[i,j])*(1.0-ay[i,j])*image[x0[i,j], y0[i,j]] + ax[i,j]*(1.0-ay[i,j])*image[x0[i,j]+1, y0[i,j]] + $
            (1.0-ax[i,j])*ay[i,j]*image[x0[i,j],y0[i,j]+1.0]+ax[i,j]*ay[i,j]*image[x0[i,j]+1.0,y0[i,j]+1.0]
    endfor
  endfor
  
  imagecorrected=convol(imagecorrected, kernel_pre)
end

pro opticalFlowPrintInfo, u, v
  ofMagnitude = sqrt(u*u + v*v)
  ; Motion quantification info
  print, 'Optical flow Magnitude avg.      ', string(mean(ofMagnitude))
  print, 'Optical flow Magnitude std. dev. ', string(stdDev(ofMagnitude))
  print, 'Maximum magnitude                ', max(ofMagnitude)
end

; Assn: the image size and the vector fields size must be the same
; If onlyInROI is a non-0 value, ROIimage is used as a binary mask and only the vectors in the ROIs are shown.
pro opticalFlowShowOF, u, v, image1 = image1, image2 = image2, onlyInROI = onlyInROI, ROIimage = ROIimage

  dimI = size(image1, /dim)

  uVis = u
  vVis = v
  if keyword_set(onlyInROI) then begin
    uVis *= ROIimage
    vVis *= ROIimage
  endif

    ; Make "mock" vectors to have full range of orientations and constant scale
  maxLength = 1.;12
  uVis[0, 0] = -maxLength
  vVis[0, 0] = 0
  uVis[0, dimI[1]-1] = maxLength
  vVis[0, dimI[1]-1] = 0
  uVis[dimI[0]-1, dimI[1]-1] = -maxLength
  vVis[dimI[0]-1, dimI[1]-1] = -1e-6

  samplStep = 1.0 ; setup as desired for better visualization
  xVec = (make_array(dimI[0] / double(samplStep), /index) + 1) * samplStep
  yVec = (make_array(dimI[1] / double(samplStep), /index) + 1) * samplStep

  sIToolID = ''
  o_sys = _IDLitSys_GetSystem()
  o_win = o_sys->getByIdentifier(sIToolID + '/WINDOW/VIEW_1')
  ls = 1.0;4

  if (n_elements(image2) ne 0) and (n_elements(image1) ne 0) then ofImage = image2 - image1
  if (n_elements(image2) eq 0) and (n_elements(image1) ne 0) then ofImage = image1
  if (n_elements(image1) eq 0) then ofImage = bytArr(size(u, /DIMENSIONS))

  if ((sIToolID eq '') or (o_win eq obj_new())) then begin  ; obj_new() yields a null object reference
    iImage, bytScl(ofImage), title = 'Superposed optical flow', ident = sIToolID, /no_saveprompt
    iVector, congrid(uVis, dimI[0] / samplStep, dimI[1] / samplStep),$
             congrid(vVis, dimI[0] / samplStep, dimI[1] / samplStep),$
             xVec, yVec,$
             xMargin = [0, 0], yMargin = [0, 0],$
             auto_color = 2, rgb_table = 34, data_location = 0, /HEAD_PROPORTIONAL, $;min_value = 1.e-3,$
             overplot = sIToolID, /no_saveprompt, LENGTH_SCALE = ls, SCALE_ISOTROPIC =1

    o_win = o_sys->getByIdentifier(sIToolID + '/WINDOW/VIEW_1')
  endif else begin
      ; Delete all previous image params (this should delete objects out of the current iTool,
      ; then it would work better when starting the project with old iTools open)
    o_img = o_sys->getByIdentifier(sIToolID)
    o_win->setCurrentZoom, 1. * 600 / dimI[0]

      ; Destroy the image param objects
    ids = o_sys->findIdentifiers('/Data Manager/Image Parameters*')
    for c = 0, n_elements(ids)-1 do begin
      oplot = o_sys->getByIdentifier(ids[c])
      obj_destroy, oplot
    endfor
      ; Destroy vector data objects
    ids = o_img->findIdentifiers('*DATA SPACE/VECTOR')
    for c = 0, n_elements(ids)-1 do begin
      oplot = o_sys->getByIdentifier(ids[c])
      obj_destroy, oplot
    endfor
    ids = o_img->findIdentifiers('*DATA SPACE/VECTOR_[0123456789]')  ; Avoiding risks
    for c = 0, n_elements(ids)-1 do begin
      oplot = o_sys->getByIdentifier(ids[c])
      obj_destroy, oplot
    endfor
      ; Destroy image objects
    ids = o_img->findIdentifiers('*/data space/image')
    for c = 0, n_elements(ids)-1 do begin
      oplot = o_sys->getByIdentifier(ids[c])
      obj_destroy, oplot
    endfor
    ids = o_img->findIdentifiers('*image_[0123456789]')
    for c = 0, n_elements(ids)-1 do begin
      oplot = o_sys->getByIdentifier(ids[c])
      obj_destroy, oplot
    endfor

;    if ((*(*self.pParamStruct).pActive)[whShowIm]) then begin
      iImage, bytScl(image_2 - image), overplot = sIToolID, /no_saveprompt
      iVector, congrid(uVis, dimI[0] / samplStep, dimI[1] / samplStep),$
               congrid(vVis, dimI[0] / samplStep, dimI[1] / samplStep),$
               xVec, yVec,$
               xMargin = [0, 0], yMargin = [0, 0],$
               auto_color = 2, rgb_table = 34, data_location = 0, min_value = minMagn,$
               overplot = sIToolID, /no_saveprompt, LENGTH_SCALE = ls
;    endif else begin
;      iVector, congrid(uVis, dimI[0] / samplStep, dimI[1] / samplStep),$
;               congrid(vVis, dimI[0] / samplStep, dimI[1] / samplStep),$
;               xVec, yVec,$
;               xMargin = [0, 0], yMargin = [0, 0],$
;               auto_color = 2, rgb_table = 34, data_location = 0, min_value = minMagn,$; color=[0,0,0], 
;               overplot = sIToolID, /no_saveprompt, LENGTH_SCALE = ls
;    endelse
  endelse
  ofMagnitude = uVis * uVis + vVis * vVis
    ; Displays a color bar (orientation color code)
  tool = itGetCurrent(TOOL = oTool)
    ; When having zero motion, inserting the colorbar will make the program fail
  if total(ofMagnitude) lt 1e-10 then begin
    print, 'No significant motion computed.'
  endif else begin
;    void       = oTool->DoAction('Operations/Insert/Colorbar')
;    oColorbar  = oTool->GetSelectedItems() 
;    idColorbar = oColorbar->GetFullIdentifier() 
;    success    = oTool->DoSetProperty(idColorbar, 'Axis_Title', 'Orientation') 
  endelse
  oTool->RefreshCurrentWindow

end


function gaussianKernel, sigma

  precision=5
  size = uint(precision * sigma) + 1 ;
  ss  = 2.0*sigma*sigma;
  l=2*size-1
  kernel=fltArr(l,l)
  
  if (l mod 2) ne 0 then begin
    for i=0,l-1 do begin
      for j=0,l-1 do begin
        x=-(size-1.0)+i
        y=-(size-1.0)+j
        kernel(i,j)=exp(-(x*x+y*y)/ss)
      endfor
    endfor
    kernel/=(2.0*!pi*sigma*sigma) 
    
  endif else begin
    print, 'Kernel size is not odd!'
  endelse

  return, kernel
  
end

pro getDerivatives, image1, image2, ex=ex, ey=ey, et=et, doShift=doShift, alternativeFlag=alternativeFlag

  if n_elements(doShift) eq 0 then doShift = 0b
  if n_elements(alternativeFlag) eq 0 then alternativeFlag = 0
  
  imgsize=size(image1, /dim)
  xSize = imgsize[0]
  ySize = imgsize[1]
  ex=fltArr(imgsize)
  ey=fltArr(imgsize)
  et=fltArr(imgsize)

  if doShift eq 1b then begin
    ex[0:xSize-2, 0:ySize-2] = (.25 * (shift(image1,-1, 0) - image1 + shift(image1,-1,-1) - shift(image1, 0,-1) + $
                                       shift(imagecorrected,-1, 0) - imagecorrected + shift(imagecorrected,-1,-1) - shift(imagecorrected, 0,-1)))[0:xSize-2, 0:ySize-2]

    ey[0:xSize-2, 0:ySize-2] = (.25 * (shift(image1, 0,-1) - image1 + shift(image1,-1,-1) - shift(image1,-1, 0) + $
                                       shift(imagecorrected, 0,-1) - imagecorrected + shift(imagecorrected,-1,-1) - shift(imagecorrected,-1, 0)))[0:xSize-2, 0:ySize-2]

    et[0:xSize-2, 0:ySize-2] = (.25 * (-image1 - shift(image1,-1, 0) - shift(image1, 0,-1) - shift(image1,-1,-1) + $
                                            imagecorrected + shift(imagecorrected,-1, 0) + shift(imagecorrected, 0,-1) + shift(imagecorrected,-1,-1)))[0:xSize-2, 0:ySize-2]
  endif else begin
  
    ex[0:xSize-2, 0:ySize-2] = .25 * (image1[1:xSize-1, 0:ySize-2] - image1[0:xSize-2, 0:ySize-2] + $
                                    image1[1:xSize-1, 1:ySize-1] - image1[0:xSize-2, 1:ySize-1] + $
                                    image2[1:xSize-1, 0:ySize-2] - image2[0:xSize-2, 0:ySize-2] + $
                                    image2[1:xSize-1, 1:ySize-1] - image2[0:xSize-2, 1:ySize-1])
                                    
    ey[0:xSize-2, 0:ySize-2] = .25 * (image1[0:xSize-2, 1:ySize-1] - image1[0:xSize-2, 0:ySize-2] + $
                                    image1[1:xSize-1, 1:ySize-1] - image1[1:xSize-1, 0:ySize-2] + $
                                    image2[0:xSize-2, 1:ySize-1] - image2[0:xSize-2, 0:ySize-2] + $
                                    image2[1:xSize-1, 1:ySize-1] - image2[1:xSize-1, 0:ySize-2])
                                    
    et[0:xSize-2, 0:ySize-2] = .25 * (-image1[0:xSize-2, 0:ySize-2] - image1[1:xSize-1, 0:ySize-2] - $
                                     image1[0:xSize-2, 1:ySize-1] - image1[1:xSize-1, 1:ySize-1] + $
                                     image2[0:xSize-2, 0:ySize-2] + image2[1:xSize-1, 0:ySize-2] + $
                                     image2[0:xSize-2, 1:ySize-1] + image2[1:xSize-1, 1:ySize-1]) 
  endelse
  
  if alternativeFlag eq 1 then begin
  
     ex=fltArr(imgsize)
     ey=fltArr(imgsize)
     et=fltArr(imgsize)
     
     ex[2:xSize-3, 0:ySize-1] = (8.0/12.0) * (-image2[1:xSize-4, 0:ySize-1] + image2[3:xSize-2, 0:ySize-1]) + $
                                (1.0/12.0) * ( image2[0:xSize-5, 0:ySize-1] - image2[4:xSize-1, 0:ySize-1]) 
                                
     ey[0:xSize-1, 2:ySize-3] = (8.0/12.0) * (-image2[0:xSize-1, 1:ySize-4] + image2[0:xSize-1, 3:ySize-2]) + $
                                (1.0/12.0) * ( image2[0:xSize-1, 0:ySize-5] - image2[0:xSize-1, 4:ySize-1])
                                
     et[0:xSize-1, 0:ySize-1] =(-image1[0:xSize-1, 0:ySize-1] +image2[0:xSize-1, 0:ySize-1] )
     
  endif
  
    if alternativeFlag eq 2 then begin
  
     ex=fltArr(imgsize)
     ey=fltArr(imgsize)
     et=fltArr(imgsize)
     
     ex[2:xSize-3, 0:ySize-1] = (8.0/12.0) * (-image1[1:xSize-4, 0:ySize-1] + image1[3:xSize-2, 0:ySize-1]) + $
                                (1.0/12.0) * ( image1[0:xSize-5, 0:ySize-1] - image1[4:xSize-1, 0:ySize-1]) 
                                
     ey[0:xSize-1, 2:ySize-3] = (8.0/12.0) * (-image1[0:xSize-1, 1:ySize-4] + image1[0:xSize-1, 3:ySize-2]) + $
                                (1.0/12.0) * ( image1[0:xSize-1, 0:ySize-5] - image1[0:xSize-1, 4:ySize-1])
                                
     et[0:xSize-1, 0:ySize-1] =(-image1[0:xSize-1, 0:ySize-1] +image2[0:xSize-1, 0:ySize-1] )
     
  endif
  
end

function LKcondition, image1, image2, epsilon

  getDerivatives, image1, image2, ex=ex, ey=ey, et=et, doShift=doShift, alternativeFlag=2
  
  dimI = size(image1, /dim)
  condition = fltArr(dimI)
  
  ; "local average" kernel to smooth derivatives
  kernelW = fltArr(5, 5)
  kernelW[[0, 4, 20, 24]] = 1 / 256.
  kernelW[[1, 3, 5, 9, 15, 19, 21, 23]] = 4 / 256.
  kernelW[[2, 10, 14, 22]] = 6 / 256.
  kernelW[[6, 8, 16, 18]] = 16 / 256.
  kernelW[[7, 11, 13, 17]] = 24 / 256.
  kernelW[[12]] = 36 / 256.

  exx=convol(float(ex*ex), kernelW, center=1)
  eyy=convol(float(ey*ey), kernelW, center=1)
  exy=convol(float(ex*ey), kernelW, center=1)
  
  ext=convol(float(ex*et), kernelW, center=1)
  eyt=convol(float(et*et), kernelW, center=1)

  determinant=exx*eyy-exy*exy
  trace=exx+eyy
  normanx=convol(sqrt(trace)*ex, kernelW, center=1)
  normany=convol(sqrt(trace)*ey, kernelW, center=1)
  
  l1=trace/2+sqrt((trace^2)/4.0-determinant)
  l2=trace/2-sqrt((trace^2)/4.0-determinant)
  
  zerocheck=where( abs(l1) gt epsilon )
  zero=where( abs(l2) lt epsilon )
  condition(zerocheck)=(l2(zerocheck)/l1(zerocheck))^2
  
  if n_elements(zero) ne 1 and zero[0] ne -1 then begin
    condition(zero)=0.0
  endif
  
  return, condition
  
end
