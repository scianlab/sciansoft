; skeletonizeFromSavFile
;
; Procedure intended to manualy generate skeleton .sav files from saved surface mesh models.
;
; You must select one or more .sav files containing the 'vertices' and 'polygons' variables that define a surface mesh model (one per file).
; Suitable models would be "3d active contour" and "mesh fix" models.
;
; PARAMETERS
;
;   skeletonizationStep  Indicates which type of skeleton will be generated...
;                        0 -> contract mesh
;                        1 -> 1D skeleton without refinement
;                        2 -> refined skeleton
;   fileNamePrefix       A String specifying a filename prefix for the generated skeleton files. Could be an empty string.
;
; DEPENDENCIES
;
;   JAR file containing the "skeletonization by mesh contraction" algorithm (implemented by L Alcayaga 2012, SCIAN-Lab & DCC)
;   The SAV files must contain the 'vertices' and 'polygons' variables that define a 3D surface mesh model for each ROI 
;   (one per file) in IDL format. Remember that the JAR file needs to be registered in the idljavabrc file. 
;
; NOTES
;   Maybe it's a good idea to include the option for using OBJ or OFF files.
;
; HISTORY
;   First version, J. Jara, F. Santibañez (2012).

pro skeletonizeFromSavFile, skeletonizationStep = skeletonizationStep, fileNamePrefix = fileNamePrefix

  skeletonizationStep = keyword_set(skeletonizationStep) ? skeletonizationStep : 3        ; 1->contract, 2->collapse, 3->refinement
  skeletonType        = (skeletonizationStep gt 1)                                        ; 0->polygon mesh, 1->polylines
  fileName            = keyword_set(fileNamePrefix)      ? fileNamePrefix      : '_skeletonModel'

    ; TODO use "names familiy" for skeleton models in SCIAN-Soft (as well as others ;) ...this is a first
  skeletonModelName = '_Skeleton_'

  fileArr = dialog_pickfile(FILTER = '*.sav', GET_PATH = fileFolder, /MULTIPLE_FILES)
  nObj = n_elements(fileArr)
  if (nObj lt 1) or (fileArr[0] eq '') then begin
    print, 'No files were given, procedure cancelled'
    return
  endif

  oIDLClassName = 'IDLJAVAOBJECT$PROJECT_SKELETONITATIONFROMMESHCONTRACTION'
  oJavaClassName = 'project.SkeletonitationFromMeshContraction'
  oMeshContraction = obj_new(oIDLClassName, oJavaClassName)

  if (obj_class(oMeshContraction) ne oIDLClassName) then begin
    r = dialog_message('ERROR creating ' + oJavaClassName + '. oMeshContraction =' + oMeshContraction)
    if obj_valid(oMeshContraction) then obj_destroy, oMeshContraction
    return
  endif

  for i = 0L, nObj-1 do begin

    restore, fileArr[i]
    verticesx = transpose(vertices[0,*])
    verticesy = transpose(vertices[1,*])
    verticesz = transpose(vertices[2,*])

      ; Call Java object
    tsn = systime(1)

    if (skeletonizationStep ge 1) then begin
      ts2 = systime(1)
      oMeshContraction->geometryContraction, verticesx, verticesy, verticesz, polygons
      t2 = systime(1) - ts2
      print, 'Geometry contraction performed in ', t2, ' sec.'
    endif
    if (skeletonizationStep ge 2) then begin
      ts3 = systime(1)
      oMeshContraction->connectivitySurgery
      t3 = systime(1) - ts3
      print, 'Connectivity surgery performed in ', t3, ' sec.'
    endif
    if (skeletonizationStep ge 3) then begin
      ts4 = systime(1)
      oMeshContraction->embeddingRefinement
      t4 = systime(1) - ts4
      print, 'Embedding refinement performed in ', t4, ' sec.'
    endif

    case skeletonizationStep of
    1: begin
       verticesFinales  = oMeshContraction->getMeshVerticesCoordsIDL()
       poligonosFinales = oMeshContraction->getMeshTrianglesForIDL()
       endcase
    2: begin
       verticesFinales = oMeshContraction->getVerticesCoordsIDL()
       aristasFinales  = oMeshContraction->getEdgesForIDL()
       endcase
    3: begin
       verticesFinales = oMeshContraction->getVerticesCoordsIDL()
       aristasFinales  = oMeshContraction->getEdgesForIDL()
       endcase
    else: begin
          print, 'Error: unrecognized skeletonizationStep (', skeletonizationStep, ')'
          endcase
    endcase

    ;a = oMeshContraction->skeletonNodesWithVerticesForIDL()
    print, 'Skeleton Computed'

    objNumber = strmid(fileArr[i], 0, strPos(fileArr[i], '.'))
    roiModelFileName = strCompress(fileFolder + strcompress(fileName + '_Obj' + objNumber + '.sav', /rem))

    case skeletonType of

     ; contracted mesh
    0: begin
       print, 'Lamento esto'
       endcase

     ; 1D skeleton
    1: begin
       ; variable renaming
       vertices = verticesFinales
       edges = aristasFinales
       print, 'saving ', roiModelFileName
       save, vertices, edges, fileName = roiModelFileName
       vertices = -1
       edges = -1
       endcase
    else: print, 'Unrecognized skeleton Type... nothing done... !? (skeletonType is ', skeletonType, ', skeletonizationStep is ', skeletonizationStep, ')'
    endcase
  endfor

  if obj_valid(oMeshContraction) then obj_destroy, oMeshContraction
end
