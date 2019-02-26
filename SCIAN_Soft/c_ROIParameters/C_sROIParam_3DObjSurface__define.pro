;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjSurface
;
; PURPOSE:
;       - Calculation of Object Surface
;
; AUTHOR:
;     Dr. Steffen Härtel (2007)
;     e_mail:shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjSurface' )
;
; METHOHDS:
;_____________________________IOISIOI____________________


pro C_sROIParam_3DObjSurface::apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position
   fSurf = 0
   fModelSelected = 0
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
      widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
        if(OBJ_VALID(stateObjWindow)) then begin
           oObjectModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
           if not(obj_valid(oObjectModel)) then begin
              oObjectModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
              fModelSelected = 3
           endif else fModelSelected = 1; AC model
         endif
      widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
      if obj_valid(oObjectModel) then fSurf = 1b else fSurf = 0b
      if fSurf then begin
         for i = oObjectModel->count()-1,0,-1 do obj_destroy, oObjectModel->get(position=i)
         oObjectModel->remove, /all
         obj_destroy, oObjectModel
         
         if(fModelSelected eq 1) then begin
              oSurfModel = obj_new('IDLgrModel', uValue = '3D Surface AC Model', name = '3D Surface AC Model')
              C_sROI3DGroupObj->getoACModel, oSurfModel, stack_tlb = stack_tlb
         endif
         if(fModelSelected eq 2) then begin      
              print, '3D Surface Model is no longer supported'
         endif
         if(fModelSelected eq 3) then begin      
              oSurfModel = obj_new('IDLgrModel', uValue = '3D Surface Mesh Model', name = '3D Surface Mesh Model')
              C_sROI3DGroupObj->getoMeshModel, oSurfModel
         endif
      endif
   
   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

       ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1:if (position[0] eq -1) then return else  whParamActive[position] = 1
      else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase

      ; check Pointers
   wherePA = where(whParamActive eq 1)
   if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

   nObjects = C_sROI3DGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

        ; set Object Number Vector
      *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()

        ; set Object Parameter Vectors
      for i = 0, n_elements(whParam)-1 do if whParamActive[i] then *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float, value = -1.)

      xyzSizePerPixel = C_sROI3DGroupObj->getxyzSizePerPixel()
      xyzDim = C_sROI3DGroupObj->getxyzDim()

      for i = 0, nObjects-1 do begin
         ; '3D Surface [Voxel]'
         if whParamActive[0] then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = (C_sROI3DGroupObj->get(position = i))->getObjectSurfaceInVoxels()

         ; '3D Surface [x²]'
         if whParamActive[1] then (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[i] = (C_sROI3DGroupObj->get(position = i))->getObjectSurfaceInVoxels(/realSize)
      endfor

         ; '3D Surface Polygons [x²]' or '3D SSP [S³V²]' or '3D Surf Sphericity'
      if (whParamActive[2] or whParamActive[3] or whParamActive[4]) then begin
            ; if not then make one
         if not(obj_valid(oSurfModel)) then begin
            oSurfModel = obj_new('IDLgrModel')
            C_sROI3DGroupObj->getoSurfaceModel, oSurfModel
            fSurf = 1
         endif else fSurf = 0

            ; '3D Surface Polygons [x²]'
         for i = 0, oSurfModel->count()-1 do begin
            oPoly = oSurfModel->get(position = i)
            if obj_isa(oPoly, 'IDLGRPOLYGON') then begin
               oPoly->getProperty, polygons = polygons, data = vertices, name = objName
               (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = (C_sROI3DGroupObj->get(position = i))->getObjectSurfaceFromPolygons(polygons = polygons, vertices = vertices, realSize = 1)
            endif
         endfor

            ;'3D SSP [S³V²]' or '3D Surf Sphericity'
         if whParamActive[3] or whParamActive[4] then begin
            for i = 0, nObjects-1 do begin
               V = n_elements( *((C_sROI3DGroupObj->get(position = i))->getpWherePoints()) ) * (xyzSizePerPixel[0] * xyzSizePerPixel[1] * xyzSizePerPixel[2])
               S = (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i]

                  ;'3D SSP [S³V²]'
               if whParamActive[3] then (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[i] = S^3/V^2

                  ;'3D Surf Sphericity' -> http://en.wikipedia.org/wiki/Sphericity
               if whParamActive[4] then (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[i] = (!pi)^(1./3) * (6.*V)^(2./3) / S
            endfor
         endif
      endif

         ; '3D Surf Curvature Min [k]' or '3D Surf Curvature Mean [k]' or '3D Surf Curvature Max [k]' or '3D Surf Curv div A [k div x²]'or '3D Surf Curv Entropy'or '3D Surf Curv > Thresh'
      if (total(whParamActive[5:10]) gt 0) then begin
         oAC3D = obj_new('C_sActiveContour3D')
         for i = 0, oSurfModel->count()-1 do begin
            oPoly = oSurfModel->get(position = i)
            if obj_isa(oPoly, 'IDLGRPOLYGON') then begin
               oPoly->getProperty, polygons = polygons, data = vertices
               vertices[0,*] *= xyzSizePerPixel[0]
               vertices[1,*] *= xyzSizePerPixel[1]
               vertices[2,*] *= xyzSizePerPixel[2]
               oAC3D->setContour, reform(vertices[0,*]), reform(vertices[1,*]), reform(vertices[2,*]), polygons
               result = oAC3D->getAvgNeighborNodes()
               curvVec = oAC3D->getCurv4()
               (*(*(*self.pValueStruct)[whParam[5]]).pROIParamVect)[i] = min(curvVec, max = maxCurv)
               (*(*(*self.pValueStruct)[whParam[6]]).pROIParamVect)[i] = (moment(curvVec))[0]
               (*(*(*self.pValueStruct)[whParam[7]]).pROIParamVect)[i] = maxCurv
               (*(*(*self.pValueStruct)[whParam[8]]).pROIParamVect)[i] = total(curvVec) / (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i]
               p = curvVec / total(curvVec)
               whereP = where(p gt 0, count)
               if (count gt 0) then (*(*(*self.pValueStruct)[whParam[9]]).pROIParamVect)[i] =  -1. / alog10(n_elements(curvVec)) * total(p[whereP] * alog10(p[whereP])) $
                  else (*(*(*self.pValueStruct)[whParam[9]]).pROIParamVect)[i] = 0
                  ; Curv > Thresh
               thresh = 2.5
               (*(*(*self.pValueStruct)[whParam[10]]).pROIParamVect)[i] = total(curvVec gt thresh)
            endif else (*(*(*self.pValueStruct)[whParam[5:10]]).pROIParamVect)[i] = [-1,-1,-1,-1,-1,-1]
         endfor
         obj_destroy, oAC3D
      endif

         ; '3D Surface Distance Min [x]' '3D Surface Shared < x [%]' '3D Surface Shared < x [N]'
      if ((total(whParamActive[11:13]) gt 0) and nObjects gt 1) then begin
         paramVec1 = fltArr(nObjects)
         paramVec2 = fltArr(nObjects)
         paramVec3 = fltArr(nObjects)
         pBP = ptrArr(nObjects)

            ; collect all object borders
         for i = 0, nObjects-1 do begin
            if obj_valid(oSurfModel) then oPoly = oSurfModel->get(position = i)
            if obj_valid(oPoly) then begin
               oPoly->getProperty, data = xyzPoints
               xyzBP = fltArr(5, (size(xyzPoints, /dim))[1])
               xyzBP[0,*] = xyzPoints[0,*] * xyzSizePerPixel[0]
               xyzBP[1,*] = xyzPoints[1,*] * xyzSizePerPixel[1]
               xyzBP[2,*] = xyzPoints[2,*] * xyzSizePerPixel[2]
            endif else begin
               (C_sROI3DGroupObj->get(position = i))->getProperty, data = xyzPoints
               minX = min(xyzPoints[0,*])
               minY = min(xyzPoints[1,*])
               minZ = min(xyzPoints[2,*])
               xyzObjectBorderObject = (C_sROI3DGroupObj->get(position = i))->makeObjectBorderInVoxel()
               wherePoints = where(xyzObjectBorderObject ne 0)

               szOBO = size(xyzObjectBorderObject, /dim)
               xyzBP = fltArr(5, n_elements(wherePoints))
               xyzBP[0,*] = wherePoints mod szOBO[0]
               xyzBP[1,*] = floor( (wherePoints mod (szOBO[0]*szOBO[1])) / szOBO[0])
               xyzBP[2,*] = floor(wherePoints / (szOBO[0] * szOBO[1]))

               xyzBP[0,*] += (minX - min(xyzBP[0,*]))
               xyzBP[1,*] += (minY - min(xyzBP[1,*]))
               xyzBP[2,*] += (minZ - min(xyzBP[2,*]))

               xyzBP[0,*] *= xyzSizePerPixel[0]
               xyzBP[1,*] *= xyzSizePerPixel[1]
               xyzBP[2,*] *= xyzSizePerPixel[2]
            endelse
            xyzBP[4,*] = sqrt((xyzDim[0]*xyzSizePerPixel[0])^2 + (xyzDim[1]*xyzSizePerPixel[1])^2 + (xyzDim[2]*xyzSizePerPixel[2])^2)
            pBP[i] = ptr_new(xyzBP, /no_copy)
         endfor
      ; calculate distances, surface sh in [x]
      shDist = 1.5
      case 1 of
        total(whParamActive[11:13]) ge 1: begin

          ; Compute bounding boxes in xyz for each object, in order to avoid comparisons between ("coarsely") distant objects
          xyzBoxes = fltArr(6, nObjects) ; [xmin, ymin, zmin, xmax, ymax, zmax] for each object
          for i = 0, nObjects-1 do begin
            xyzBoxes[0, i] = min((*pBP[i])[0,*], max = maxax) - shDist/2.
            xyzBoxes[3, i] = maxax + shDist/2.
            xyzBoxes[1, i] = min((*pBP[i])[1,*], max = maxax) - shDist/2.
            xyzBoxes[4, i] = maxax + shDist/2.
            xyzBoxes[2, i] = min((*pBP[i])[2,*], max = maxax) - shDist/2.
            xyzBoxes[5, i] = maxax + shDist/2.
          endfor

          for i = 0, nObjects-2 do begin
            nPts = (size(*pBP[i], /dim))[1]
            for j = i+1, nObjects-1 do begin
              ; Check against the expanded box to see if it is necessary to do the exhaustive distance check
              fInX = ((xyzBoxes[3, j] ge xyzBoxes[0, i]) and (xyzBoxes[0, j] le xyzBoxes[0, i])) or ((xyzBoxes[0, j] le xyzBoxes[3, i]) and (xyzBoxes[3, j] ge xyzBoxes[0, i]))
              fInY = ((xyzBoxes[4, j] ge xyzBoxes[1, i]) and (xyzBoxes[1, j] le xyzBoxes[1, i])) or ((xyzBoxes[1, j] le xyzBoxes[4, i]) and (xyzBoxes[4, j] ge xyzBoxes[1, i]))
              fInZ = ((xyzBoxes[5, j] ge xyzBoxes[2, i]) and (xyzBoxes[2, j] le xyzBoxes[2, i])) or ((xyzBoxes[2, j] le xyzBoxes[5, i]) and (xyzBoxes[5, j] ge xyzBoxes[2, i]))
              fInXYZ = fInX and fInY and fInZ
              if (fInXYZ eq 1) then begin
                print, "going to check ", i, "-", j
                fJIsNeighborOfI = 0b
                for k = 0l, nPts-1 do begin
                  xyzDist = sqrt(((*pBP[i])[0,k] - (*pBP[j])[0,*])^2 + ((*pBP[i])[1,k] - (*pBP[j])[1,*])^2 + ((*pBP[i])[2,k] - (*pBP[j])[2,*])^2)
                  ;TODO [jj] replaced the formula with '*(j+1)'
                  ;(*pBP[i])[3,k] >= (total(xyzDist le shDist) ge 1) * (j+1)
                  ijMinDist = min(xyzDist, minPosJ)
                  ; TODO whishlist... make min + where in a fast way
                  ; The min. distance can be to more than one object
                  nptsMinDist = where(xyzDist eq ijMinDist, count)
                  if (ijMinDist le shDist) then begin ; TODO faster if...? (total(xyzDist le shDist) ge 1)
                    (*pBP[i])[3,k] = 1
                    (*pBP[j])[3,minPosJ] = 1
                    if (count gt 1) then $
                      for c = 1, count-1 do (*pBP[j])[3,nptsMinDist[c]] = 1
                    fJIsNeighborOfI = 1b
                  endif
                  (*pBP[i])[4,k] <= ijMinDist
                  (*pBP[j])[4,minPosJ] <= ijMinDist
                  if (count gt 1) then $
                    for c = 1, count-1 do (*pBP[j])[4,nptsMinDist[c]] <= ijMinDist
                endfor
                if (fJIsNeighborOfI eq 1) then begin
                  ; increase the neighbor count for objects i and j
                  paramVec3[i]++
                  paramVec3[j]++
                  print, 'object ', i, ' with ', paramVec3[i], ' neighbor objects in threshold distance'
                  print, 'object ', j, ' with ', paramVec3[j], ' neighbor objects in threshold distance'
                endif                     
              endif
            endfor
            ;if max((*pBP[i])[3,*]) gt 0 then paramVec3[i] = total(histogram((*pBP[i])[3,*], min = 1) gt 0) else paramVec3[i] = 0
          endfor
          for i = 0, nObjects-1 do begin
            paramVec1[i] = min((*pBP[i])[4,*])
            nPts = (size(*pBP[i], /dim))[1]
            paramVec2[i] = total((*pBP[i])[3,*] gt 0) / nPts * 100.
          endfor
        endcase
        total(whParamActive[12:13]) gt 1: begin
        endcase
        else:
      endcase

         *(*(*self.pValueStruct)[whParam[11]]).pROIParamVect = paramVec1
         *(*(*self.pValueStruct)[whParam[12]]).pROIParamVect = paramVec2
         *(*(*self.pValueStruct)[whParam[13]]).pROIParamVect = paramVec3

         for i = nObjects-1, 0 do ptr_free, pBP[i]
         ptr_free, pBP
      endif

;      fCalcBack = 0b
;      oObj = oSurfModel->get(position = 0)
;      if obj_valid(oObj) then begin
;         oObj->getProperty, uvalue = uvalue
;         if (uvalue eq 'ObjInGalleryPosition') then begin
;            self->calcObjPos, oSurfModel, sDirection = 'calcOriginalPosition'
;            fCalcBack = 1b
;         endif
;      endif

         ; '3D Surf Center Dist Min [x]' '3D Surf Center Dist Mean [x]' '3D Surf Center Dist Max [x]'
      if ((total(whParamActive[14:16]) gt 0) and nObjects gt 1) then begin

         fOk = C_sROI3DGroupObj->calcEigenSys()
         pMayorEigenSys = C_sROI3DGroupObj->getpEigenSys()
         for i = 0, oSurfModel->count()-1 do begin
            oPoly = oSurfModel->get(position = i)
            if obj_isa(oPoly, 'IDLGRPOLYGON') then begin
               oPoly->getProperty, polygons = polygons, data = vertices

               xyzDist = (C_sROI3DGroupObj->get(position = i))->getxyzDist(xyzPoints = vertices, xyzCenter = (*pMayorEigenSys).centerXYZ)

               if whParamActive[14] then (*(*(*self.pValueStruct)[whParam[14]]).pROIParamVect)[i] = min(xyzDist, max = maxDist)
               if whParamActive[15] then (*(*(*self.pValueStruct)[whParam[15]]).pROIParamVect)[i] = (moment(xyzDist))[0]
               if whParamActive[16] then (*(*(*self.pValueStruct)[whParam[16]]).pROIParamVect)[i] = maxDist
            endif else (*(*(*self.pValueStruct)[whParam[11:13]]).pROIParamVect)[i] = [-1,-1,-1]
         endfor
      endif
   endelse
    if fSurf then begin
       for i = oSurfModel->count()-1,0,-1 do obj_destroy, oSurfModel->get(position=i)
       oSurfModel->remove, /all
       obj_destroy, oSurfModel
    endif   
end


function C_sROIParam_3DObjSurface::init

    ROIParamStruct = {name:'3D Object Surface',$     ;  ROI Name.
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

    nParams = 17
    self.pValueStruct = ptr_new(ptrArr(nParams))
    ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
    ROIParamNames = ['3D Surface Area [Voxel]',$
                     '3D Surface Area Voxel [x²]',$
                     '3D Surface Area Polygons [x²]',$
                     '3D SSP [S³V²]',$
                     '3D Surf Sphericity',$ -> http://en.wikipedia.org/wiki/Sphericity
                     '3D Surf Curvature Min [k]',$
                     '3D Surf Curvature Mean [k]',$
                     '3D Surf Curvature Max [k]',$
                     '3D Surf Curv div A [k div x²]',$
                     '3D Surf Curv Entropy',$
                     '3D Surf Curv GT Thresh',$
                     '3D Surface Distance Min [x]',$
                     '3D Surface Shared LT x [%]',$
                     '3D Surface Shared LT x [N]',$
                     '3D Surf Center Dist Min [x]',$
                     '3D Surf Center Dist Mean [x]',$
                     '3D Surf Center Dist Max [x]']

    ROIParamActive = make_array(nParams, /byte, value = 1b)
    ROIParamActive[11:13] = 0b
    ROIParamMin = make_array(nParams, /float, value = 0.)
    ROIParamMax = make_array(nParams, /float, value = 1.)
    ROIParamValues = make_array(nParams, /float, value = 0.)
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[0],$
                      type:'3D ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$
                      pNames:ptr_new(),$
                      pActive:ptr_new(),$
                      pMin:ptr_new(),$
                      pMax:ptr_new(),$
                      pValues:ptr_new(),$
                      pROIParamVect:ptr_new()}

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [0,0,$
                      0,0,$
                      0,0,$
                      0,0]
    ROIValueMin = [0.,0.,$
                   0.,0.,$
                   0.,0.,$
                   0.,0.]
    ROIValueMax = [1.,1.,$
                   1.,1.,$
                   1.,1.,$
                   1.,1.]
    ROIValueValues =[0.,1.,$
                     0.,1.,$
                     0.,1.,$
                     0.,1.]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    for i = 1, nParams-2 do begin
       ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[i],$
                       type:'3D ROI-Parameter-Method',$
                       pWidgetType:ptr_new(),$
                       pNames:ptr_new(),$
                       pActive:ptr_new(),$
                       pMin:ptr_new(),$
                       pMax:ptr_new(),$
                       pValues:ptr_new(),$
                       pROIParamVect:ptr_new()}

       ROIValueWidgetType = [ROIValueWidgetType]
       ROIValueNames = [ROIValueNames]
       ROIValueActive = [ROIValueActive]
       ROIValueMin = [ROIValueMin]
       ROIValueMax = [ROIValueMax]
       ROIValueValues =[ROIValueValues]

       pROIParamVect = [-1]
       ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
       ROIValueStruct.pNames = ptr_new(ROIValueNames)
       ROIValueStruct.pActive = ptr_new(ROIValueActive)
       ROIValueStruct.pMin = ptr_new(ROIValueMin)
       ROIValueStruct.pMax = ptr_new(ROIValueMax)
       ROIValueStruct.pValues = ptr_new(ROIValueValues)
       ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

       (*self.pValueStruct)[i] = ptr_new(ROIValueStruct, /no_copy)
    endfor

    ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[nParams-1],$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$
                    pNames:ptr_new(),$
                    pActive:ptr_new(),$
                    pMin:ptr_new(),$
                    pMax:ptr_new(),$
                    pValues:ptr_new(),$
                    pROIParamVect:ptr_new()}

    ROIValueWidgetType = [ROIValueWidgetType]
    ROIValueNames = [ROIValueNames]
    ROIValueActive = [ROIValueActive]
    ROIValueMin = [ROIValueMin]
    ROIValueMax = [ROIValueMax]
    ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[nParams-1] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end


pro C_sROIParam_3DObjSurface__define
   tmp = {C_sROIParam_3DObjSurface, pParamStruct:ptr_new(), pValueStruct:ptr_new(), inherits C_sROIParam}
end