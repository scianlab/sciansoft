;*****************************************************************************************************
;
; NAME:     repairSurfaceMesh
;
; PURPOSE:  from a surface mesh computed using IDL (vertices, polygons) 
;           check if it is valid and optionally simplify the mesh. Valid implies:
;              - to be composed only by triangles (IDL meshes may contain polygons of 4 vertices).
;              - to have unique vertices, edges and triangles
;              - to have edges associated to two triangles always (this may required extensive repair)
;          Optionally it could simplify the mesh and/or smooth the mesh. After each operation the method verifies
;          again to return a valid mesh.
;        
; OUTPUT:  repairedVertices, repairedPolygons
;
; SYNTAX:  
;
; NOTES:   The returned number of vertices or polygons may be different from the original structure. 
;
; DEPENDENCIES:
;   MeshFix.dll
;*****************************************************************************************************

pro repairSurfaceMesh, vertices, polygons, repairedVertices = repairedVertices, repairedPolygons = repairedPolygons,$
                                           skipCheckTriangles = skipCheckTriangles, maxNumVertices = maxNumVertices, $
                                           smoothingIterations=smoothingIterations, roiJoinDistance=roiJoinDistance, $
                                           debug = debug, fileName = fileName

   fDebug             = 1b;keyword_set(debug)              ? 1b : 0b
   maxnvertices       = keyword_set(maxNumVertices)     ? maxNumVertices : 0b
   smoothitera        = keyword_set(smoothingIterations)? smoothingIterations : 0b
   ;TODO when using MeshFix.dll version 1.2 this can be send as a parameter. 
   ;The idea is to avoid splitting a given ROI (Vertices, polygons). When 
   ;structures are thin (dendrites) fixing usually delivers several objects.
   roidistance        = keyword_set(roiJoinDistance)    ? roiJoinDistance : 0b
   
   nvertices = size(vertices, /dim)
         
   ;;;;;;;;;;;;;;;;
   ;MANDATORY CALLS
   ;;;;;;;;;;;;;;;;
   
   ; A checking step to ensure that all the polygons are triangles.
   numtri = MESH_DECIMATE(vertices, polygons, trilist, VERTICES = relvertices)
   print, 'Decimated mesh (to have triangles only)' + String(n_elements(polygons) - numtri)
   vertices = relvertices
   polygons = trilist
   
   if fDebug eq 1b then begin        ;write OFF (DEBUG)
    path = 'C:\RSI\'
    name = strCompress('Mesh'+ (keyword_set(fileName) ? fileName : string(1)) + '_original.off', /rem)
    filename_ = strCompress(path + name, /rem)
    s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename_
   endif
   
   ;repair mesh
   callMeshFixDLL, vertices, polygons, repairedVertices, repairedPolygons
   nverticesRep = size(repairedVertices, /dim)
   
   vertices=repairedVertices
   polygons=repairedPolygons  
   
   if fDebug eq 1b then begin        ;write OFF (DEBUG)
    path = 'C:\RSI\'
    name = strCompress('Mesh'+ (keyword_set(fileName) ? fileName : string(1)) + '_repaired.off', /rem)
    filename_ = strCompress(path + name, /rem)
    s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename_
   endif
      
   ;mcerda, MESH_DECIMATE delivers poor results
   path = 'C:\RSI\'
   name = strCompress('Mesh'+ (keyword_set(fileName) ? fileName : string(1)) + '_repaired.off', /rem)
   filename_ = strCompress(path + name, /rem)
   s_getoSkeletonFromMeshRepair_readoff, filename_, finalvertices, finalpolygons

   repairedVertices=finalvertices
   repairedPolygons=finalpolygons
   
   print, 'debug point: mesh is repaired'
   
;   ;;;;;;;;;;;;;;;;
;   ;OPTIONAL CALLS
;   ;;;;;;;;;;;;;;;;
;   
;   ;Decimate mesh to speed-up computation (use with caution!)
;   if (maxnvertices ne 0b and nverticesRep[1] gt maxnvertices) then begin
;     ;decimate uses the repaired mesh
;     vertices=repairedVertices
;     polygons=repairedPolygons
;     percent = 1.0 + fix(100.0 * maxnvertices/nverticesRep[1]) ;1% is the min. number of vertices to keep
;     print, 'Decimating, keeping '+ string(fix(percent)) + ' % of vertices'
;     numtri = MESH_DECIMATE(vertices, polygons, trilist, PERCENT_VERTICES = percent)
;     polygons = trilist
;     numtri = MESH_VALIDATE(vertices, polygons, /PACK_VERTICES)
;     callMeshFixDLL, vertices, polygons, repairedVertices, repairedPolygons
;   endif
;   
;   if fDebug eq 1b then begin        ;write OFF (DEBUG)
;    path = 'C:\RSI\'
;    name = strCompress('Mesh'+string(1) + '_fix.off', /rem)
;    filename = strCompress(path + name, /rem)
;    s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename
;   endif
;   
;   ;Smooth mesh to make surface as regular as posible (use with caution!)
;   if (smoothitera ne 0b ) then begin
;     ;smooth uses the repaired mesh (that may be decimated)
;     vertices=repairedVertices
;     polygons=repairedPolygons
;     print, 'smoothing mesh '+ string(smoothitera) + ' times'
;     smoothvertices = MESH_SMOOTH(vertices, polygons, ITERATIONS=smoothitera) 
;     numtri = MESH_VALIDATE(smoothvertices, polygons, /PACK_VERTICES)
;     callMeshFixDLL, smoothvertices, polygons, repairedVertices, repairedPolygons
;   endif
   
end


pro verifyOnlyTriangles, vertices, polygons, verticesOUT, polygonsOUT

  position = 0L
  nelAdded = 0
  ni = n_elements(polygons)

  while (position lt n_elements(polygons)) do begin     
    numVert = polygons[position]
    if (numVert gt 3) then begin
      nelAdded = nelAdded + 3
      ;print, numVert, " polygons to process, curr. position is ", position
      tmp = [-1]
      numPart = numVert - 2
      lasTri = position + 2
      for kk = 1, numPart do begin
        tmp = [tmp, 3, polygons[position + 1], polygons[lasTri], polygons[lasTri + 1]]
        lasTri = lasTri + 1
      endfor
      polygons = [polygons[0:position-1], tmp[1:*], polygons[position+1+polygons[position]:*]]
      position = position + n_elements(tmp) - 1
      ;print, "added ", n_elements(tmp) - 1, " elements: ", tmp[1:*]
    endif else position = position + 1 + polygons[position]

    if ((position mod 1000) eq 0) then begin
      print, 'Fixing triangles...'+ String(position)
     endif
  endwhile

  verticesOUT = vertices
  polygonsOUT = polygons
  
end


pro callMeshFixDLL, vertices, polygons, verticesOUT, polygonsOUT

  print, 'callMeshFixDLL starting...'
  nvertices = size(vertices, /dim)

  ;look for the dll in the _dll folder of the project
  libpath = filepath('meshfix.dll', subdir=['_dll'], root_dir = s_getPathForProjectGeneric())
  startTime = sysTime(1)
  sizeVertices = 0L
  sizePolygons = 0L
  sVertices    = size(vertices, /dim)
  sPolygons    = size(polygons, /dim)

  print, 'Mesh nVertices=' + string(sVertices[1])
  print, 'Mesh nPolygons=' + string(sPolygons[0]/4)

  fRes = call_external(libpath,$
                 'meshfixwrapper_size',$
                  double(vertices),$ ; required double data type
                  polygons,$
                  sVertices[1],$ ; (IDL LONG -> int in C)
                  sPolygons[0],$ ; (IDL LONG -> int in C)
                  sizeVertices,$ ; OUTPUT
                  sizePolygons,$ ; OUTPUT
                  /unload)
  endTime = sysTime(1)

  print, 'Mesh Repaired, nVertices=' + string(sizeVertices)
  print, 'Mesh Repaired, nPolygons=' + string(sizePolygons)
  print, 'SurfaceMesh Computed (1st call) and repaired: ' + string(endTime - startTime) + 's'

  if (sizeVertices eq 0) or (sizePolygons eq 0) then begin
    verticesOUT = -1
    polygonsOUT = -1
    return
  endif
  
  repairedVer = dblArr(3,sizeVertices)
  repairedPol = lonArr(4*sizePolygons)

  ;After repair polygons order will be inversed
  fRes = call_external(libpath,$
                 'meshfixwrapper',$
                 double(vertices),$ ; required double data type
                 polygons,$
                 sVertices[1],$ ; (IDL LONG -> int in C)
                 sPolygons[0],$ ; (IDL LONG -> int in C)
                 sizeVertices,$ ; (IDL LONG -> int in C)
                 sizePolygons,$ ; (IDL LONG -> int in C)
                 repairedVer,$  ; OUTPUT
                 repairedPol,$  ; OUTPUT
                 /unload)

  endTime = sysTime(1)
  print, 'SurfaceMesh Computed (2st call) and repaired: ' + string(endTime - startTime) + 's'
  print, 'Loop'

  sVertices    = size(vertices, /dim)
  nverticesRep = size(repairedVer, /dim)

  print, 'Mesh Repaired vertices nr, (old, new)=(' + string(nvertices[1]) + ',' + string(nverticesRep[1]) + ')'
  print, 'Mesh Repaired triangles nr, (old, new)=(' + string(n_elements(polygons)) + ',' + string(n_elements(repairedPol)) + ')'
  print, 'MeshRepaired Computed and repaired: ' + string(endTime - startTime) + 's'

  verticesOUT = repairedVer
  polygonsOUT = repairedPol

;       ;;;;;;;;;;;;;;;;;;
;       ;EXE Call (for debug)
;       ;;;;;;;;;;;;;;;;;;
;       ;write OFF
;       path = 'C:\RSI\'
;       name = strCompress('Mesh'+string(i) + '.off', /rem)
;       filename = strCompress(path + name, /rem)
;       s_getoSkeletonFromMeshRepair_writeoff, vertices, polygons, filename
;       
;       startTime = sysTime(1)
;       CMD =  filepath('meshfix.exe', subdir=['_dll'], root_dir=s_getPathForProjectGeneric())
;       
;       SPAWN, CMD + ' '+filename+ ' -a '+string(minarea)
;       
;       ;read OFF
;       path = 'C:\RSI\'
;       name = strCompress('Mesh'+string(i) + '_fixed.off', /rem)
;       filename = strCompress(path + name, /rem)
;       s_getoSkeletonFromMeshRepair_readoff, filename, repVertices, repPolygons
;       endTime = sysTime(1)
;       
;       nvertices   =size(vertices, /dim)
;       nverticesRep=size(repVertices, /dim)
;       
;       print, 'Mesh Repaired vertices nr, (old, new)=(' + string(nvertices[1])+','+string(nverticesRep[1])+')'
;       print, 'Mesh Repaired triangles nr, (old, new)=(' + string(n_elements(polygons))+','+string(n_elements(repPolygons))+')'
;       print, 'MeshRepaired Computed (1st call) and repaired: ' + string(endTime - startTime) + 's'
end
