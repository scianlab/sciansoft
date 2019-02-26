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
pro opticalFlowHS3D, image1, image2, u, v, w, alpha = alpha,$
                                              iterations = iterations,$
                                              verbose = verbose,$
                                              doShift = doShift,$
                                              stopCrit = stopCrit,$
                                              boundaryConditions = boundaryConditions,$
                                              iterImprovementRatio = iterImprovementRatio,$
                                              iterImprovementThreshold = iterImprovementThreshold
    ; Check keyword parameters.
  if n_elements(alpha)                    eq 0 then alpha = 20.0d
  if n_elements(iterations)               eq 0 then iterations = 200L
  if n_elements(boundaryConditions)       eq 0 then boundaryConditions = 1b ; consider boundary conditions in as [Horn & Schunck 1981]
  if n_elements(stopCrit)                 eq 0 then stopCrit = 0b           ; 0: num iterations / 1: no improvement
  if n_elements(doShift)                  eq 0 then doShift = 0b
  if n_elements(verbose)                  eq 0 then verbose = 1b
  if n_elements(iterImprovementRatio)     eq 0 then iterImprovementRatio = 1.
  if n_elements(iterImprovementThreshold) eq 0 then iterImprovementThreshold = 1e-6

  print, '3D HS-OF computation starting...'
  if verbose then startTime = sysTime(1)

  dimI = size(image1, /dim)
  xSize = dimI[0]
  ySize = dimI[1]
  zSize = dimI[2]

  ex = make_array(dimI, /float, value = 0)
  ey = make_array(dimI, /float, value = 0)
  ez = make_array(dimI, /float, value = 0)
  et = make_array(dimI, /float, value = 0)
  image1 *= 1.0 ; ensure float data type
  image2 *= 1.0

  if keyword_set(doShift) then begin
    print, 'nothing with shift yet'
  endif else begin
    ex[0:xSize-2,0:ySize-2,0:zSize-2] = 0.125 * ( image1[1:xSize-1,0:ySize-2,0:zSize-2] - image1[0:xSize-2,0:ySize-2,0:zSize-2] + $
                                                  image1[1:xSize-1,1:ySize-1,1:zSize-1] - image1[0:xSize-2,1:ySize-1,1:zSize-1] + $
                                                  image1[1:xSize-1,1:ySize-1,0:zSize-2] - image1[0:xSize-2,1:ySize-1,0:zSize-2] + $
                                                  image1[1:xSize-1,0:ySize-2,1:zSize-1] - image1[0:xSize-2,0:ySize-2,1:zSize-1] + $

                                                  image2[1:xSize-1,0:ySize-2,0:zSize-2] - image2[0:xSize-2,0:ySize-2,0:zSize-2] + $
                                                  image2[1:xSize-1,1:ySize-1,1:zSize-1] - image2[0:xSize-2,1:ySize-1,1:zSize-1] + $
                                                  image2[1:xSize-1,1:ySize-1,0:zSize-2] - image2[0:xSize-2,1:ySize-1,0:zSize-2] + $
                                                  image2[1:xSize-1,0:ySize-2,1:zSize-1] - image2[0:xSize-2,0:ySize-2,1:zSize-1])

    ey[0:xSize-2,0:ySize-2,0:zSize-2] = 0.125 * ( image1[0:xSize-2,1:ySize-1,0:zSize-2] - image1[0:xSize-2,0:ySize-2,0:zSize-2] + $
                                                  image1[1:xSize-1,1:ySize-1,1:zSize-1] - image1[1:xSize-1,0:ySize-2,1:zSize-1] + $
                                                  image1[1:xSize-1,1:ySize-1,0:zSize-2] - image1[1:xSize-1,0:ySize-2,0:zSize-2] + $
                                                  image1[1:xSize-1,1:ySize-1,1:zSize-1] - image1[1:xSize-1,0:ySize-2,1:zSize-1] + $

                                                  image2[0:xSize-2,1:ySize-1,0:zSize-2] - image2[0:xSize-2,0:ySize-2,0:zSize-2] + $
                                                  image2[1:xSize-1,1:ySize-1,1:zSize-1] - image2[1:xSize-1,0:ySize-2,1:zSize-1] + $
                                                  image2[1:xSize-1,1:ySize-1,0:zSize-2] - image2[1:xSize-1,0:ySize-2,0:zSize-2] + $
                                                  image2[1:xSize-1,1:ySize-1,1:zSize-1] - image2[1:xSize-1,0:ySize-2,1:zSize-1])

    ez[0:xSize-2,0:ySize-2,0:zSize-2] = 0.125 * ( image1[0:xSize-2,0:ySize-2,1:zSize-1] - image1[0:xSize-2,0:ySize-2,0:zSize-2] + $
                                                  image1[1:xSize-1,1:ySize-1,1:zSize-1] - image1[1:xSize-1,1:ySize-1,0:zSize-2] + $
                                                  image1[1:xSize-1,0:ySize-2,1:zSize-1] - image1[1:xSize-1,0:ySize-2,0:zSize-2] + $
                                                  image1[1:xSize-1,1:ySize-1,1:zSize-1] - image1[1:xSize-1,1:ySize-1,0:zSize-2] + $

                                                  image2[0:xSize-2,0:ySize-2,1:zSize-1] - image2[0:xSize-2,0:ySize-2,0:zSize-2] + $
                                                  image2[1:xSize-1,1:ySize-1,1:zSize-1] - image2[1:xSize-1,1:ySize-1,0:zSize-2] + $
                                                  image2[1:xSize-1,0:ySize-2,1:zSize-1] - image2[1:xSize-1,0:ySize-2,0:zSize-2] + $
                                                  image2[0:xSize-2,1:ySize-1,1:zSize-1] - image2[0:xSize-2,1:ySize-1,0:zSize-2])

    et[0:xSize-2,0:ySize-2,0:zSize-2] = 0.125 * ( image2[0:xSize-2, 0:ySize-2, 0:zSize-2] - image1[0:xSize-2, 0:ySize-2, 0:zSize-2] + $
                                                  image2[0:xSize-2, 1:ySize-1, 0:zSize-2] - image1[0:xSize-2, 1:ySize-1, 0:zSize-2] + $
                                                  image2[1:xSize-1, 0:ySize-2, 0:zSize-2] - image1[1:xSize-1, 0:ySize-2, 0:zSize-2] + $
                                                  image2[0:xSize-2, 0:ySize-2, 1:zSize-1] - image1[0:xSize-2, 0:ySize-2, 1:zSize-1] + $
                                                  image2[1:xSize-1, 0:ySize-2, 1:zSize-1] - image1[1:xSize-1, 0:ySize-2, 1:zSize-1] + $
                                                  image2[0:xSize-2, 1:ySize-1, 1:zSize-1] - image1[0:xSize-2, 1:ySize-1, 1:zSize-1] + $
                                                  image2[1:xSize-1, 1:ySize-1, 0:zSize-2] - image1[1:xSize-1, 1:ySize-1, 0:zSize-2] + $
                                                  image2[1:xSize-1, 1:ySize-1, 1:zSize-1] - image1[1:xSize-1, 1:ySize-1, 1:zSize-1])
  endelse

  d = (alpha^2 + ex^2 + ey^2 + ez^2)

    ; "local average" kernel for laplacian (does not includes the central point)
  u = make_array(dimI, /float, value = 0.0)
  v = make_array(dimI, /float, value = 0.0)
  w = make_array(dimI, /float, value = 0.0)
  
  kernel = make_array(3, 3, 3, /float, value = 0.0)
  c = 1.0 / 6
  kernel[1, 1, 0] = c
  kernel[1, 1, 2] = c
  kernel[1, 0, 1] = c
  kernel[1, 2, 1] = c
  kernel[0, 1, 1] = c
  kernel[2, 1, 1] = c
  
  ;TODO JJ 3D HS-OF: iterations as forced stop criterion
  firstIteration = 1
  i = 0L
  stopCrit = 0b
  while ((stopCrit eq 0) && (i le iterations)) $
     || ((stopCrit eq 1) && (iterImprovementRatio gt iterImprovementThreshold)) do begin

    i += 1
    
    um = convol(u, kernel)
    vm = convol(v, kernel)
    wm = convol(w, kernel)
    
    c = (ex * um + ey * vm + ez * wm + et) / d
    
    u = um - ex * c
    v = vm - ey * c
    w = wm - ez * c

    ; Boundary values for zero normal derivative
    if boundaryConditions then begin
    
      u[1:xSize-2, 1:ySize-2, 0]         = u[1:xSize-2, 1:ySize-2, 1]
      u[1:xSize-2, 1:ySize-2, zSize-1]   = u[1:xSize-2, 1:ySize-2, zSize-2]
      u[0,         1:ySize-2, 1:zSize-2] = u[1,         1:ySize-2, 1:zSize-2]
      u[xSize-1,   1:ySize-2, 1:zSize-2] = u[xSize-2,   1:ySize-2, 1:zSize-2]
      u[1:xSize-2, 0,         1:zSize-2] = u[1:xSize-2, 1,         1:zSize-2]
      u[1:xSize-2, ySize-1,   1:zSize-2] = u[1:xSize-2, ySize-2,   1:zSize-2]

      v[1:xSize-2, 1:ySize-2, 0]         = v[1:xSize-2, 1:ySize-2, 1]
      v[1:xSize-2, 1:ySize-2, zSize-1]   = v[1:xSize-2, 1:ySize-2, zSize-2]
      v[0,         1:ySize-2, 1:zSize-2] = v[1,         1:ySize-2, 1:zSize-2]
      v[xSize-1,   1:ySize-2, 1:zSize-2] = v[xSize-2,   1:ySize-2, 1:zSize-2]
      v[1:xSize-2, 0,         1:zSize-2] = v[1:xSize-2, 1,         1:zSize-2]
      v[1:xSize-2, ySize-1,   1:zSize-2] = v[1:xSize-2, ySize-2,   1:zSize-2]

      w[1:xSize-2, 1:ySize-2, 0]         = w[1:xSize-2, 1:ySize-2, 1]
      w[1:xSize-2, 1:ySize-2, zSize-1]   = w[1:xSize-2, 1:ySize-2, zSize-2]
      w[0,         1:ySize-2, 1:zSize-2] = w[1,         1:ySize-2, 1:zSize-2]
      w[xSize-1,   1:ySize-2, 1:zSize-2] = w[xSize-2,   1:ySize-2, 1:zSize-2]
      w[1:xSize-2, 0,         1:zSize-2] = w[1:xSize-2, 1,         1:zSize-2]
      w[1:xSize-2, ySize-1,   1:zSize-2] = w[1:xSize-2, ySize-2,   1:zSize-2]

        ; boundary rows with fixed (x,y), varying z
      u[0, 0, 1:zSize-2] = u[1, 1, 1:zSize-2]
      v[0, 0, 1:zSize-2] = v[1, 1, 1:zSize-2]
      w[0, 0, 1:zSize-2] = w[1, 1, 1:zSize-2]

      u[0, ySize-1, 1:zSize-2] = u[1, ySize-2, 1:zSize-2]
      v[0, ySize-1, 1:zSize-2] = v[1, ySize-2, 1:zSize-2]
      w[0, ySize-1, 1:zSize-2] = w[1, ySize-2, 1:zSize-2]

      u[xSize-1, 0, 1:zSize-2] = u[xSize-2, 1, 1:zSize-2]
      v[xSize-1, 0, 1:zSize-2] = v[xSize-2, 1, 1:zSize-2]
      w[xSize-1, 0, 1:zSize-2] = w[xSize-2, 1, 1:zSize-2]

      u[xSize-1, ySize-1, 1:zSize-2] = u[xSize-2, ySize-2, 1:zSize-2]
      v[xSize-1, ySize-1, 1:zSize-2] = v[xSize-2, ySize-2, 1:zSize-2]
      w[xSize-1, ySize-1, 1:zSize-2] = w[xSize-2, ySize-2, 1:zSize-2]

        ; boundary rows
      u[0,         1:ySize-2, 0]       = u[1,         1:ySize-2, 1]
      u[xSize-1,   1:ySize-2, 0]       = u[xSize-2,   1:ySize-2, 1]
      u[xSize-1,   1:ySize-2, zSize-1] = u[xSize-2,   1:ySize-2, zSize-2]
      u[0,         1:ySize-2, zSize-1] = u[0,         1:ySize-2, zSize-2]
      u[1:xSize-2, 0,         zSize-1] = u[1:xSize-2, 1,         zSize-2]
      u[1:xSize-2, 0,         0]       = u[1:xSize-2, 1,         1]
      u[1:xSize-2, ySize-1,   zSize-1] = u[1:xSize-2, ySize-2,   zSize-2]
      u[1:xSize-2, ySize-1,   0]       = u[1:xSize-2, ySize-2,   1]
      u[0,         1:ySize-2, 0]       = u[1,         1:ySize-2, 1]
      u[0,         1:ySize-2, zSize-1] = u[1,         1:ySize-2, zSize-2]
      u[xSize-1,   1:ySize-2, zSize-1] = u[xSize-2,   1:ySize-2, zSize-2]
      u[xSize-1,   1:ySize-2, 0]       = u[xSize-2,   1:ySize-2, 1]

      v[0,         1:ySize-2, 0]       = v[1,         1:ySize-2, 1]
      v[xSize-1,   1:ySize-2, 0]       = v[xSize-2,   1:ySize-2, 1]
      v[xSize-1,   1:ySize-2, zSize-1] = v[xSize-2,   1:ySize-2, zSize-2]
      v[0,         1:ySize-2, zSize-1] = v[0,         1:ySize-2, zSize-2]
      v[1:xSize-2, 0,         zSize-1] = v[1:xSize-2, 1,         zSize-2]
      v[1:xSize-2, 0,         0]       = v[1:xSize-2, 1,         1]
      v[1:xSize-2, ySize-1,   zSize-1] = v[1:xSize-2, ySize-2,   zSize-2]
      v[1:xSize-2, ySize-1,   0]       = v[1:xSize-2, ySize-2,   1]
      v[0,         1:ySize-2, 0]       = v[1,         1:ySize-2, 1]
      v[0,         1:ySize-2, zSize-1] = v[1,         1:ySize-2, zSize-2]
      v[xSize-1,   1:ySize-2, zSize-1] = v[xSize-2,   1:ySize-2, zSize-2]
      v[xSize-1,   1:ySize-2, 0]       = v[xSize-2,   1:ySize-2, 1]

      w[0,         1:ySize-2, 0]       = w[1,         1:ySize-2, 1]
      w[xSize-1,   1:ySize-2, 0]       = w[xSize-2,   1:ySize-2, 1]
      w[xSize-1,   1:ySize-2, zSize-1] = w[xSize-2,   1:ySize-2, zSize-2]
      w[0,         1:ySize-2, zSize-1] = w[0,         1:ySize-2, zSize-2]
      w[1:xSize-2, 0,         zSize-1] = w[1:xSize-2, 1,         zSize-2]
      w[1:xSize-2, 0,         0]       = w[1:xSize-2, 1,         1]
      w[1:xSize-2, ySize-1,   zSize-1] = w[1:xSize-2, ySize-2,   zSize-2]
      w[1:xSize-2, ySize-1,   0]       = w[1:xSize-2, ySize-2,   1]
      w[0,         1:ySize-2, 0]       = w[1,         1:ySize-2, 1]
      w[0,         1:ySize-2, zSize-1] = w[1,         1:ySize-2, zSize-2]
      w[xSize-1,   1:ySize-2, zSize-1] = w[xSize-2,   1:ySize-2, zSize-2]
      w[xSize-1,   1:ySize-2, 0]       = w[xSize-2,   1:ySize-2, 1]

        ; corners
      u[0,       0,       0]       = u[1,       1,       1]
      u[0,       0,       zSize-1] = u[1,       1,       zSize-1]
      u[0,       ySize-1, 0]       = u[1,       ySize-2, 1]
      u[0,       ySize-1, zSize-1] = u[1,       ySize-2, zSize-2]
      u[xSize-1, 0,       0]       = u[xSize-2, 1,       1]
      u[xSize-1, 0,       zSize-1] = u[xSize-2, 1,       zSize-2]
      u[xSize-1, ySize-1, 0]       = u[xSize-2, ySize-2, 1]
      u[xSize-1, ySize-1, zSize-1] = u[xSize-2, ySize-2, zSize-2]

      v[0,       0,       0]       = v[1,       1,       1]
      v[0,       0,       zSize-1] = v[1,       1,       zSize-1]
      v[0,       ySize-1, 0]       = v[1,       ySize-2, 1]
      v[0,       ySize-1, zSize-1] = v[1,       ySize-2, zSize-2]
      v[xSize-1, 0,       0]       = v[xSize-2, 1,       1]
      v[xSize-1, 0,       zSize-1] = v[xSize-2, 1,       zSize-2]
      v[xSize-1, ySize-1, 0]       = v[xSize-2, ySize-2, 1]
      v[xSize-1, ySize-1, zSize-1] = v[xSize-2, ySize-2, zSize-2]

      w[0,       0,       0]       = w[1,       1,       1]
      w[0,       0,       zSize-1] = w[1,       1,       zSize-1]
      w[0,       ySize-1, 0]       = w[1,       ySize-2, 1]
      w[0,       ySize-1, zSize-1] = w[1,       ySize-2, zSize-2]
      w[xSize-1, 0,       0]       = w[xSize-2, 1,       1]
      w[xSize-1, 0,       zSize-1] = w[xSize-2, 1,       zSize-2]
      w[xSize-1, ySize-1, 0]       = w[xSize-2, ySize-2, 1]
      w[xSize-1, ySize-1, zSize-1] = w[xSize-2, ySize-2, zSize-2]
    endif

  endwhile
  print, '3D HS-OF computation complete.'

end


;mcerda
;taken from show_stream function built-in IDL
pro opticalFlowShowOF3D, u, v, w, oObjectModel, roinumber, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
          ZFACTOR=zfactor, SEEDS = seeds, LINES = lines, TUBES = tubes, VECTOR = vector, ARROWS = arrows

    if(N_ELEMENTS(u) ne 0) then begin ;data supplied, check it
        nudims = SIZE(u, /N_DIMENSIONS)
        udims = SIZE(u, /DIMENSIONS)
        if(nudims eq 4) then begin
            if(udims[0] ne 3) then MESSAGE, 'Input must be 3D array of' + $
                ' 3-vectors or 3 3D arrays of scalars.'
            data = u            
            nx = udims[1] &  ny = udims[2] & nz = udims[3]
        end else if(nudims eq 3) then begin
            if((N_ELEMENTS(v) ne 0) and (N_ELEMENTS(w) ne 0)) then begin   
                nx = udims[0] &  ny = udims[1] & nz = udims[2]
            end else MESSAGE, 'Input must be 3D array of' + $
                ' 3-vectors or 3 3D arrays of scalars.'
            data = FLTARR(3,nx, ny, nz)
            data[0, *, *, *] = u
            data[1, *, *, *] = v
            data[2, *, *, *] = w  
        end else if (nudims eq 2) then begin    
            if((N_ELEMENTS(u) ne 0) and (N_ELEMENTS(v) ne 0)) then begin   
                nx = udims[0]
                 ny = udims[1]
            end else MESSAGE, 'Input must be 2D array of' + $
                ' 3-vectors or 3 2D arrays of scalars.'
            data = FLTARR(3,nx, ny, 2)
            data[0, *, *, 0] = u
            data[1, *, *, 0] = v
            data[2, *, *, 0] = w  
        end
    end else begin ;data not supplied, compute helical flow test data.
        nx = 15 & ny = 15 & nz = 15
        data = FLTARR(3,nx, ny, nz)
        b = .034*7 & c = .14*7
        for i=0,nx-1 do $
            for j=0,ny-1 do $
            for k=0,nz-1 do begin
            x = i-nx/2
            y = j-ny/2
            z = k-nz/2
            data[0,i,j,k] = FLOAT(-b*y)
            data[1,i,j,k] = FLOAT(b*x)
            data[2,i,j,k] = FLOAT(c)
        end
    end

    if(N_ELEMENTS(seeds) eq 0) then begin ;Compute seed points.
        ;Set seed spacing for a rake at the bottom of the grid.
        xstep=LONG(nx/4) & ystep=LONG(ny/4)
        if(not KEYWORD_SET(vector)) then zstep=nz else zstep = 1

        nseeds = 3*LONG((nx*ny*nz)/(xstep*ystep*zstep))
        seeds = FLTARR(nseeds)
        iseed=0L
        for i=0,nx-1 do $
            for j=0,ny-1 do $
            for k=0,nz-1 do begin
            if( ((k mod zstep) eq 0) and ((i mod xstep) eq 0) and $
                ((j mod ystep) eq 0) and (iseed lt (nseeds-2)) ) then begin
                seeds[iseed] = FLOAT(i)
                seeds[iseed+1] = FLOAT(j)
                seeds[iseed+2] = FLOAT(k)
                iseed = iseed+3
            end
        end
    end

    maxIterations=100
    stepSize=.5
    width=.5 ;ribbon thickness

    ;Create streamlines graphic.
    oModel = oObjectModel

    if(not KEYWORD_SET(vector)) then begin ;Streamlines/ribbons/tubes
    
        PARTICLE_TRACE,data,seeds,outverts,outconn,outnormals, $
            MAX_ITERATIONS=maxIterations, MAX_STEPSIZE=stepSize,  $
            INTEGRATION=0,ANISOTROPY=[1,1,1], SEED_NORMAL=[0, 0, 1]

        if (outconn[0] eq -1l) then $
            MESSAGE, 'No particle trace.'

        maxDim = MAX(outverts[0,*]) > $
            MAX(outverts[1,*]) > $
            MAX(outverts[2,*])

        ; If requested, prepare arrow head labels.
        if (KEYWORD_SET(arrows) and not KEYWORD_SET(tubes)) then begin 
            symScale = (KEYWORD_SET(lines) ? 0.03 : 0.07)
            symScale = maxDim*symScale
            oSymbol = opticalFlow3DmakeArrowHeadSymbol(symScale, zfactor)
            nPolys = opticalFlow3DcountPolys(outconn)
            ; Two arrow heads per particle trace path.
            lblPolys = LINDGEN(nPolys*2) / 2
            oLblSymbols = REPLICATE(oSymbol, nPolys*2)
            evens = LINDGEN(nPolys) * 2
            odds = evens+1
            lblOffsets = FLTARR(nPolys*2)
            lblOffsets[evens] = 0.5
            lblOffsets[odds] = 1.0
        endif

        if(KEYWORD_SET(lines)) then begin ;lines
            oStreamlines = OBJ_NEW('IDLgrPolyline',outverts, $
                POLYLINES=outconn, $
                LABEL_OBJECTS=oLblSymbols, $
                LABEL_POLYLINES=lblPolys, $
                LABEL_OFFSETS=lblOffsets, $
                /LABEL_USE_VERTEX_COLOR, $
                /LABEL_NOGAPS, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
                 name = strCompress('2DOpticalFlowModel:'+string(roinumber), /REMOVE_ALL),  uvalue = 'ObjInOriginalPosition')
            oModel->Add, oStreamlines 
            title = 'Particle Trace'
        endif else begin ;ribbons/tubes
            ; If arrows are to be added, keep a copy of the particle trace 
            ; vertex and connectivity.
            if (KEYWORD_SET(arrows) and not KEYWORD_SET(tubes)) then begin 
                averts = outverts
                aconn = outconn
            endif

            if(KEYWORD_SET(tubes)) then $ ;square profile for stream-tubes.
                profile = [[-1,-1],[-1,1],[1,1],[1,-1],[-1,-1]]
            nverts = N_ELEMENTS(outverts)/3
            STREAMLINE, TEMPORARY(outverts),TEMPORARY(outconn), $
                outnormals*width,outverts,outconn, PROFILE=profile

            oStreamlines = OBJ_NEW('IDLgrPolygon',outverts, POLYGONS=outconn, $
                                  SHADING = 1, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                  name = strCompress('3DOpticalFlowModel'+string(roinumber), /REMOVE_ALL),  uvalue = 'ObjInOriginalPosition')
            ;oModel->Add, oStreamlines 

            if (KEYWORD_SET(arrows) and not KEYWORD_SET(tubes)) then begin 
                oArrows = OBJ_NEW('IDLgrPolyline',averts, $
                    POLYLINES=aconn, $
                    LINESTYLE=6, $
                    LABEL_OBJECTS=oLblSymbols, $
                    LABEL_POLYLINES=lblPolys, $
                    LABEL_OFFSETS=lblOffsets, $
                    /LABEL_USE_VERTEX_COLOR, $
                    /LABEL_NOGAPS, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                    name = strCompress('3DOpticalFlowModel:'+string(roinumber), /REMOVE_ALL),  uvalue = 'ObjInOriginalPosition')
                oModel->Add,oArrows
            endif
            title = 'Streamline'
        end
    end else begin ;Hedgehog vector plot
        VECTOR_FIELD,data,outverts,outconn, VERTICES=seeds

        if (KEYWORD_SET(arrows)) then begin 
            maxDim = MAX(outverts[0,*]) > $
                MAX(outverts[1,*]) > $
                MAX(outverts[2,*])
            maxDim=maxDim*0.03
            oSymbol = opticalFlow3DmakeArrowHeadSymbol(maxDim, zfactor)
            nPolys = opticalFlow3DcountPolys(outconn)
            lblPolys = LINDGEN(nPolys)
            oLblSymbols = REPLICATE(oSymbol, nPolys)
            lblOffsets = REPLICATE(1.0, nPolys)
        endif

        myoutverts=make_array(3, 4*n_elements(outconn)/3 )
        myoutconn=make_array(6*n_elements(outconn)/3 )
        
        k=0.0
        
        for i=0L, n_elements(outconn)/3 -1 do begin 
        
          a=outverts[*,i*2]
          b=outverts[*,i*2+1]
          diff=b-a
          lrand=randomu(100, [3,1])*100.
          r=crossp(a-lrand, b-lrand)
          
          if norm(r) gt 0.0 then begin
          
          s=crossp(r, diff)
          
          r=r/norm(r)
          s=s/norm(s)
          
          rad=norm(diff)*0.2
          if rad gt 5. then rad=5.
          
          lambda=.5
          rad=rad*0.1
          theta=randomu(fix(norm(diff)*10.0), [3,1])*!pi
          p1=a+lambda*(b-a)+ rad*cos(theta)*r+ rad*sin(theta)*s
          
          theta=theta+!pi
          p2=a+lambda*(b-a)+ rad*cos(theta)*r+ rad*sin(theta)*s
          
          myoutverts[*,k*4.]=a
          myoutverts[*,k*4.+1]=b
          myoutverts[*,k*4.+2]=p1
          myoutverts[*,k*4.+3]=p2
          
          myoutconn[k*6.]=5
          myoutconn[k*6.+1]=k*4.
          myoutconn[k*6.+2]=k*4.+1
          myoutconn[k*6.+3]=k*4.+2
          myoutconn[k*6.+4]=k*4.+1
          myoutconn[k*6.+5]=k*4.+3
          
          k=k+1
          end
          
        endfor
        ;myoutverts[*, 0:(4*k-1)],POLYLINES=myoutconn[0:(6*k-1)]
        oStreamlines=OBJ_NEW('IDLgrPolyline', myoutverts[*, 0:(4*k-1)],POLYLINES=myoutconn[0:(6*k-1)], $
            COLOR=[0,0,0], VERT_COLOR=[0,0,0], alpha_channel=1.0, $
            LABEL_OBJECTS=oLblSymbols, $
            LABEL_POLYLINES=lblPolys, $
            LABEL_OFFSETS=lblOffsets, $
            /LABEL_USE_VERTEX_COLOR, $
            /LABEL_NOGAPS, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
            name = strCompress('3DOpticalFlowModel:'+string(roinumber), /REMOVE_ALL),  uvalue = 'ObjInOriginalPosition')

        oModel->Add, oStreamlines
    end

    ;Compute velocity magnitude
    magdata = SQRT(data[0,*, *, *]^2 + data[1,*, *, *]^2 + data[2,*, *, *]^2)
    angdata = ATAN(data[1,*, *, *], data[0,*, *, *] )
    
    ;Interpolate velocity magnitude at streamline vertices, and
    ;use values to color streamlines.
    vertX =  REFORM(outverts[0,*],N_ELEMENTS(outverts)/3)
    vertY =  REFORM(outverts[1,*],N_ELEMENTS(outverts)/3)
    vertZ =  REFORM(outverts[2,*],N_ELEMENTS(outverts)/3)
    vertcolors = BYTSCL(INTERPOLATE(angdata,vertX, vertY, vertZ))
    ;oPalette = OBJ_NEW('IDLgrPalette')
    ;oPalette->LOADCT, 60
    ;oStreamlines->SetProperty, PALETTE = oPalette, VERT_COLORS = vertcolors
    
    if (OBJ_VALID(oArrows)) then begin
        oArrows->GetProperty, DATA=averts
        vertX =  REFORM(averts[0,*],N_ELEMENTS(averts)/3)
        vertY =  REFORM(averts[1,*],N_ELEMENTS(averts)/3)
        vertZ =  REFORM(averts[2,*],N_ELEMENTS(averts)/3)
        vertcolors = BYTSCL(INTERPOLATE(magdata,vertX, vertY, vertZ))
        oArrows->SetProperty, PALETTE = oPalette, VERT_COLORS = vertcolors
    endif
    print, 'TEST'

    ; Apply standard initial rotation.
    ;oModel->Rotate, [1,0,0], -90
    ;oModel->Rotate, [0,1,0], 30
    ;oModel->Rotate, [1,0,0], 30

    ;XOBJVIEW, oModel, SCALE=1.0, TITLE=title, /BLOCK

    ;OBJ_DESTROY, [oModel, oPalette]
    ;if (OBJ_VALID(oArrows)) then $
    ;    OBJ_DESTROY, oArrows
    ;if (OBJ_VALID(oSymbol)) then begin
    ;    oSymbol->GetProperty, DATA=oPoly
    ;    OBJ_DESTROY, [oPoly, oSymbol]
    ;endif
end


function opticalFlow3DmakeArrowHeadSymbol, size, zfactor
    revolutionType = 6
    shape = [[-1,-0.5,0],[0,0,0]]
    nFacets = 16
    rotVec = [1,0,0]
    MESH_OBJ, revolutionType, verts, conn, shape, $
        P1=nFacets, P3=rotVec 
    
    verts[2,0:*]=verts[2,0:*]/zfactor
    
    oPoly = OBJ_NEW('IDLgrPolygon', verts, POLY=conn)
    oSymbol = OBJ_NEW('IDLgrSymbol', oPoly, SIZE=size)

    return, oSymbol
end

function opticalFlow3DcountPolys, inconn
    nElements = N_ELEMENTS(inconn)
    nPolys = 0
    i=0
    while (i lt nElements) do begin
        nVerts = inconn[i]
        if (nVerts eq 0) then continue
        if (nVerts eq -1) then break
        i = i + nVerts + 1
        nPolys = nPolys + 1
    endwhile
   
    return, nPolys
end
