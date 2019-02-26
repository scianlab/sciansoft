pro s_getoSurfCentDistI, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties

  oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos

  xyzDim      = oGroupReference->getxyzDim()


   if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
   oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
   if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b

      ; of / off normalize all distances to min/max
;   for i = 0, (oGroupReference->count())-1 do begin
;      maxDistP = 0.
;      minDistP = 0.
;      if fSurf then begin
;         oObj = oSurfModel->get(position = i)
;         if obj_valid(oObj) then begin
;            oObj->getProperty, data = xyzPoints
;            fCOOk = 1b
;         endif else fCOOk = 0b
;      endif else fCOOk = 0b
;
;      if fCOOk then evProj = (oGroupReference->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (oGroupReference->get(position = i))->getEVSProj()
;      if fCOOk then oObj->getProperty, data = xyzPoints else (oGroupReference->get(position = i))->getProperty, data = xyzPoints
;      if fCOOk then xyzDist = (oGroupReference->get(position = i))->getxyzDist(xyzPoints = xyzPoints, xyzCenter = (*self.pEigenSys).centerXYZ) else xyzDist = (oGroupReference->get(position = i))->getxyzDist(xyzCenter = (*self.pEigenSys).centerXYZ)
;
;      maxDist = max(xyzDist, min = minDist)
;      maxDistP >= maxDist
;      minDistP <= minDist
;      print, 'minDist', minDistP, ' maxDist', maxDistP
;   endfor

   fCalcBack = 0b
   if fSurf then begin
      oObj = oSurfModel->get(position = 0)
      if obj_valid(oObj) then begin
         oObj->getProperty, uvalue = uvalue
         if (uvalue eq 'ObjInGalleryPosition') then begin
            oGroupReference->calcObjPos, oSurfModel, sDirection = 'calcOriginalPosition'
            fCalcBack = 1b
         endif
      endif
   endif

   for i = 0, (oGroupReference->count())-1 do begin
      objNumber = (oGroupReference->get(position = i))->getNumber()

      fCOOk = 0b
      if fSurf then begin
         oObj = oSurfModel->get(position = i)
         if obj_valid(oObj) then begin
            oObj->getProperty, data = xyzPoints, poly = polygons
            fCOOk = 1b
         endif
      endif

      if fCOOk then evProj = (oGroupReference->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (oGroupReference->get(position = i))->getEVSProj()
      if fCOOk then oObj->getProperty, data = xyzPoints else (oGroupReference->get(position = i))->getProperty, data = xyzPoints
      if fCOOk then xyzDist = (oGroupReference->get(position = i))->getxyzDist(xyzPoints = xyzPoints, xyzCenter = ((*(oGroupReference->getpEigenSys()))).centerXYZ) else xyzDist = (oGroupReference->get(position = i))->getxyzDist(xyzCenter = ((*(oGroupReference->getpEigenSys()))).centerXYZ)

      maxDist = max(xyzDist, min = minDist)
      whereMin = (where(xyzDist eq minDist))[0]
      whereMax = (where(xyzDist eq maxDist))[0]
      print, 'minDist', minDist, ' maxDist', maxDist
      if (n_elements(minDistP) gt 0) then begin
         xyzDist -= minDistP
         xyzDist /= (maxDistP-minDistP)
      endif else begin
         xyzDist -= minDist
         xyzDist /= (maxDist-minDist)
      endelse
      xyzDist = reform(xyzDist)

      vert_color = make_array(4, (size(xyzPoints, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,*,*])
      opacVect = oGroupReference->getOpacVect('1st Volume Opacity')
      colorIndex = round(xyzDist * 255)
      vert_color[0,*] = (rgb_table0[*,0])[colorIndex]
      vert_color[1,*] = (rgb_table0[*,1])[colorIndex]
      vert_color[2,*] = (rgb_table0[*,2])[colorIndex]
      vert_color[3,*] = opacVect[colorIndex]

      (oGroupReference->get(position = i))->getProperty, all = prop

      if fUpDateROI3DGroupProperties then begin
         oObj = oObjectModel->GetByName (strCompress('3DSurfDist_:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
         oObj = oObjectModel->GetByName (strCompress('3DSurfVec_:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
      endif else begin

;            ; mark min distance on principal axis
;         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [255,0,0])
;         if fCOOk then oObjectModel->add, obj_new('IDLgrROI', data = reform((evProj.evsProj)[0,*,[whereMin]]), symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark min distance on surface
         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [0,255,0])
         oObjectModel->add, obj_new('IDLgrROI', data = xyzPoints[*,whereMin], symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark center with a yellow cross
         oSymbol = obj_new('IDLgrSymbol', data = 7, thick = 3, size = 5, color = [255,255,0])
         oObjectModel->add, obj_new('IDLgrROI', ((*(oGroupReference->getpEigenSys()))).centerXYZ, symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark lines from center to cells
         oObjectModel->add, obj_new('IDLgrPolyline', data = transpose([transpose(((*(oGroupReference->getpEigenSys()))).centerXYZ), transpose(xyzPoints[*,whereMin])]), uvalue = 'ObjInOriginalPosition',$
                                                              color = [255,255,0], thick = 2, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; plot principal axis
         if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[0,*,*]), vert_colors = vert_color[*,*], name = strCompress('3DSurfVec_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                                   xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[1,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
  ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[2,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
  ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
;            ; plot surface
         if fCOOk and (total(vert_color[3,*]) gt 0) then oObjectModel->add, obj_new('IDLgrPolygon', shading = 1, shininess = 128., data = xyzPoints, poly = polygons, vert_colors = vert_color,$
                                                                  shade_range = [0,255], name = strCompress('3DSurfDist_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                                  xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
      endelse
   endfor
   if fCalcBack then oGroupReference->calcObjPos, oSurfModel, sDirection = 'calcGalleryPosition'

    oObjectModel->getproperty, TRANSFORM = matrixModel
    matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
    ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
    oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix
end
    
pro s_getoSurfCentDistI_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties

  oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos

  xyzDim      = oGroupReference->getxyzDim()


   if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
   oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
   if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b

      ; of / off normalize all distances to min/max
;   for i = 0, (oGroupReference->count())-1 do begin
;      maxDistP = 0.
;      minDistP = 0.
;      if fSurf then begin
;         oObj = oSurfModel->get(position = i)
;         if obj_valid(oObj) then begin
;            oObj->getProperty, data = xyzPoints
;            fCOOk = 1b
;         endif else fCOOk = 0b
;      endif else fCOOk = 0b
;
;      if fCOOk then evProj = (oGroupReference->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (oGroupReference->get(position = i))->getEVSProj()
;      if fCOOk then oObj->getProperty, data = xyzPoints else (oGroupReference->get(position = i))->getProperty, data = xyzPoints
;      if fCOOk then xyzDist = (oGroupReference->get(position = i))->getxyzDist(xyzPoints = xyzPoints, xyzCenter = (*self.pEigenSys).centerXYZ) else xyzDist = (oGroupReference->get(position = i))->getxyzDist(xyzCenter = (*self.pEigenSys).centerXYZ)
;
;      maxDist = max(xyzDist, min = minDist)
;      maxDistP >= maxDist
;      minDistP <= minDist
;      print, 'minDist', minDistP, ' maxDist', maxDistP
;   endfor

   fCalcBack = 0b
   if fSurf then begin
      oObj = oSurfModel->get(position = 0)
      if obj_valid(oObj) then begin
         oObj->getProperty, uvalue = uvalue
         if (uvalue eq 'ObjInGalleryPosition') then begin
            oGroupReference->calcObjPos, oSurfModel, sDirection = 'calcOriginalPosition'
            fCalcBack = 1b
         endif
      endif
   endif

    ; Recovery the center for the cluster 0 .... i.e the center for the original gropu considering both sides..
    C_sROI3DGroupObjOriginal = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = tPos, chPos = chPos, clusPos = 0)
    originalCenter = ((*(C_sROI3DGroupObjOriginal->getpEigenSys()))).centerXYZ

   for i = 0, (oGroupReference->count())-1 do begin
      objNumber = (oGroupReference->get(position = i))->getNumber()

      fCOOk = 0b
      if fSurf then begin
         oObj = oSurfModel->get(position = i)
         if obj_valid(oObj) then begin
            oObj->getProperty, data = xyzPoints, poly = polygons
            fCOOk = 1b
         endif
      endif

      if fCOOk then evProj = (oGroupReference->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (oGroupReference->get(position = i))->getEVSProj()
      if fCOOk then oObj->getProperty, data = xyzPoints else (oGroupReference->get(position = i))->getProperty, data = xyzPoints
      if fCOOk then xyzDist = (oGroupReference->get(position = i))->getxyzDist(xyzPoints = xyzPoints, xyzCenter = originalCenter) else xyzDist = (oGroupReference->get(position = i))->getxyzDist(xyzCenter = originalCenter)

      maxDist = max(xyzDist, min = minDist)
      whereMin = (where(xyzDist eq minDist))[0]
      whereMax = (where(xyzDist eq maxDist))[0]
      print, 'minDist', minDist, ' maxDist', maxDist
      if (n_elements(minDistP) gt 0) then begin
         xyzDist -= minDistP
         xyzDist /= (maxDistP-minDistP)
      endif else begin
         xyzDist -= minDist
         xyzDist /= (maxDist-minDist)
      endelse
      xyzDist = reform(xyzDist)

      vert_color = make_array(4, (size(xyzPoints, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,*,*])
      opacVect = oGroupReference->getOpacVect('1st Volume Opacity')
      colorIndex = round(xyzDist * 255)
      vert_color[0,*] = (rgb_table0[*,0])[colorIndex]
      vert_color[1,*] = (rgb_table0[*,1])[colorIndex]
      vert_color[2,*] = (rgb_table0[*,2])[colorIndex]
      vert_color[3,*] = opacVect[colorIndex]

      (oGroupReference->get(position = i))->getProperty, all = prop

      if fUpDateROI3DGroupProperties then begin
         oObj = oObjectModel->GetByName (strCompress('3DSurfDist_:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
         oObj = oObjectModel->GetByName (strCompress('3DSurfVec_:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
      endif else begin

;            ; mark min distance on principal axis
;         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [255,0,0])
;         if fCOOk then oObjectModel->add, obj_new('IDLgrROI', data = reform((evProj.evsProj)[0,*,[whereMin]]), symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark min distance on surface
         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [0,255,0])
         oObjectModel->add, obj_new('IDLgrROI', data = xyzPoints[*,whereMin], symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark center with a yellow cross
         oSymbol = obj_new('IDLgrSymbol', data = 7, thick = 3, size = 5, color = [255,255,0])
         oObjectModel->add, obj_new('IDLgrROI', originalCenter, symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark lines from center to cells
         oObjectModel->add, obj_new('IDLgrPolyline', data = transpose([transpose(originalCenter), transpose(xyzPoints[*,whereMin])]), uvalue = 'ObjInOriginalPosition',$
                                                              color = [255,255,0], thick = 2, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; plot principal axis
         if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[0,*,*]), vert_colors = vert_color[*,*], name = strCompress('3DSurfVec_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                                   xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[1,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
  ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[2,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
  ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
;            ; plot surface
         if fCOOk and (total(vert_color[3,*]) gt 0) then oObjectModel->add, obj_new('IDLgrPolygon', shading = 1, shininess = 128., data = xyzPoints, poly = polygons, vert_colors = vert_color,$
                                                                  shade_range = [0,255], name = strCompress('3DSurfDist_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                                  xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
      endelse
   endfor
   if fCalcBack then oGroupReference->calcObjPos, oSurfModel, sDirection = 'calcGalleryPosition'

    oObjectModel->getproperty, TRANSFORM = matrixModel
    matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
    ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
    oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix

    obj_destroy, C_sROI3DGroupObjOriginal
end