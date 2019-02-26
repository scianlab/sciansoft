pro s_getoSurfCentDistII, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

  oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos

  xyzDim      = oGroupReference->getxyzDim()

       if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
       oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
       if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
       if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
       if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b
    
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
    
             ; split objects into three parts along the first principal axis
          ev1Dist = (oGroupReference->get(position = i))->getxyzDist(xyzPoints = reform((evProj.evsProj)[0,*,*]), xyzCenter = evProj.evminmax[*,0])
          maxev1Dist = max(ev1Dist)
          where1 = where(ev1Dist le (maxev1Dist/3.))
          where3 = where(ev1Dist ge (maxev1Dist/3.*2.))
    
          dist1 = (moment((oGroupReference->get(position = i))->getxyzDist(xyzPoints = xyzPoints[*,where1], xyzCenter = ((*(oGroupReference->getpEigenSys()))).centerXYZ)))[0]
          dist3 = (moment((oGroupReference->get(position = i))->getxyzDist(xyzPoints = xyzPoints[*,where3], xyzCenter = ((*(oGroupReference->getpEigenSys()))).centerXYZ)))[0]
    
          maxDist = max(xyzDist, min = minDist)
          whereMin = (where(xyzDist eq minDist))[0]
          whereMax = (where(xyzDist eq maxDist))[0]
          print, 'minDist', minDist, ' maxDist', maxDist
    
          vert_color = make_array(4, (size(xyzPoints, /dim))[1], /byte)
          rgb_table0 = bytArr(256,3) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,*,*])
          opacVect = oGroupReference->getOpacVect('1st Volume Opacity')
    
             ; paint object with center-value of the color table
          vert_color[0,*] = (rgb_table0[*,0])[127]
          vert_color[1,*] = (rgb_table0[*,1])[127]
          vert_color[2,*] = (rgb_table0[*,2])[127]
          vert_color[3,*] = opacVect[127]
    
          if dist1 le dist3 then colorVec = [42,212] else colorVec = [212,42]
          vert_color[0,where1] = (rgb_table0[*,0])[colorVec[0]]
          vert_color[1,where1] = (rgb_table0[*,1])[colorVec[0]]
          vert_color[2,where1] = (rgb_table0[*,2])[colorVec[0]]
          vert_color[3,where1] = opacVect[colorVec[0]]
          vert_color[0,where3] = (rgb_table0[*,0])[colorVec[1]]
          vert_color[1,where3] = (rgb_table0[*,1])[colorVec[1]]
          vert_color[2,where3] = (rgb_table0[*,2])[colorVec[1]]
          vert_color[3,where3] = opacVect[colorVec[1]]
    
          (oGroupReference->get(position = i))->getProperty, all = prop
          if fUpDateROI3DGroupProperties then begin
    
             oObj = oObjectModel->GetByName (strCompress('3DSurfDistUnDrifted_:'+string(objNumber),/rem))
             if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
             oObj = oObjectModel->GetByName (strCompress('3DSurfVecUnDrifted_:'+string(objNumber),/rem))
             if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
    
          endif else begin
    
    ;            ; mark min distance on surface
    ;         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [0,255,0])
    ;         oObjectModel->add, obj_new('IDLgrROI', data = xyzPoints[*,whereMin], symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
    ;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    ;            ; mark center with a yellow cross
    ;         oSymbol = obj_new('IDLgrSymbol', data = 7, thick = 3, size = 5, color = [255,255,0])
    ;         oObjectModel->add, obj_new('IDLgrROI', (*oGroupReference).centerXYZ, symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
    ;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    ;            ; mark lines from center to cells
    ;         oObjectModel->add, obj_new('IDLgrPolyline', data = transpose([transpose((*oGroupReference).centerXYZ), transpose(xyzPoints[*,whereMin])]), uvalue = 'ObjInOriginalPosition',$
    ;                                                              color = [255,255,0], thick = 2, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    ;            ; plot principal axis
    ;         if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[0,*,*]), vert_colors = vert_color[*,*], name = strCompress('3DSurfVec_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
    ;                                                                   xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
      ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[1,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
      ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
      ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[2,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
      ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    
                ; plot surface
             if fCOOk and (total(vert_color[3,*]) gt 0) then oObjectModel->add, obj_new('IDLgrPolygon', shading = 1, shininess = 128., data = xyzPoints, poly = polygons, vert_colors = vert_color,$
                                                                      bottom = -1, name = strCompress('3DSurfDistUnDrifted_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                                      xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
          endelse
       endfor
       if fCalcBack then oGroupReference->calcObjPos, oSurfModel, sDirection = 'calcGalleryPosition'
    
      oObjectModel->getproperty, TRANSFORM = matrixModel
      matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
      ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
      oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix

end
    
pro s_getoSurfCentDistII_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

  oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos

  xyzDim      = oGroupReference->getxyzDim()

       if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
       oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
       if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
       if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
       if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b
    
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
    
             ; split objects into three parts along the first principal axis
          ev1Dist = (oGroupReference->get(position = i))->getxyzDist(xyzPoints = reform((evProj.evsProj)[0,*,*]), xyzCenter = evProj.evminmax[*,0])
          maxev1Dist = max(ev1Dist)
          where1 = where(ev1Dist le (maxev1Dist/3.))
          where3 = where(ev1Dist ge (maxev1Dist/3.*2.))
    
          dist1 = (moment((oGroupReference->get(position = i))->getxyzDist(xyzPoints = xyzPoints[*,where1], xyzCenter = originalCenter)))[0]
          dist3 = (moment((oGroupReference->get(position = i))->getxyzDist(xyzPoints = xyzPoints[*,where3], xyzCenter = originalCenter)))[0]
    
          maxDist = max(xyzDist, min = minDist)
          whereMin = (where(xyzDist eq minDist))[0]
          whereMax = (where(xyzDist eq maxDist))[0]
          print, 'minDist', minDist, ' maxDist', maxDist
    
          vert_color = make_array(4, (size(xyzPoints, /dim))[1], /byte)
          rgb_table0 = bytArr(256,3) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,*,*])
          opacVect = oGroupReference->getOpacVect('1st Volume Opacity')
    
             ; paint object with center-value of the color table
          vert_color[0,*] = (rgb_table0[*,0])[127]
          vert_color[1,*] = (rgb_table0[*,1])[127]
          vert_color[2,*] = (rgb_table0[*,2])[127]
          vert_color[3,*] = opacVect[127]
    
          if dist1 le dist3 then colorVec = [42,212] else colorVec = [212,42]
          vert_color[0,where1] = (rgb_table0[*,0])[colorVec[0]]
          vert_color[1,where1] = (rgb_table0[*,1])[colorVec[0]]
          vert_color[2,where1] = (rgb_table0[*,2])[colorVec[0]]
          vert_color[3,where1] = opacVect[colorVec[0]]
          vert_color[0,where3] = (rgb_table0[*,0])[colorVec[1]]
          vert_color[1,where3] = (rgb_table0[*,1])[colorVec[1]]
          vert_color[2,where3] = (rgb_table0[*,2])[colorVec[1]]
          vert_color[3,where3] = opacVect[colorVec[1]]
    
          (oGroupReference->get(position = i))->getProperty, all = prop
          if fUpDateROI3DGroupProperties then begin
    
             oObj = oObjectModel->GetByName (strCompress('3DSurfDistUnDriftedRef_:'+string(objNumber),/rem))
             if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
             oObj = oObjectModel->GetByName (strCompress('3DSurfVecUnDriftedRef_:'+string(objNumber),/rem))
             if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
    
          endif else begin
    
    ;            ; mark min distance on surface
    ;         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [0,255,0])
    ;         oObjectModel->add, obj_new('IDLgrROI', data = xyzPoints[*,whereMin], symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
    ;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    ;            ; mark center with a yellow cross
    ;         oSymbol = obj_new('IDLgrSymbol', data = 7, thick = 3, size = 5, color = [255,255,0])
    ;         oObjectModel->add, obj_new('IDLgrROI', (*oGroupReference).centerXYZ, symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
    ;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    ;            ; mark lines from center to cells
    ;         oObjectModel->add, obj_new('IDLgrPolyline', data = transpose([transpose((*oGroupReference).centerXYZ), transpose(xyzPoints[*,whereMin])]), uvalue = 'ObjInOriginalPosition',$
    ;                                                              color = [255,255,0], thick = 2, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    ;            ; plot principal axis
    ;         if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[0,*,*]), vert_colors = vert_color[*,*], name = strCompress('3DSurfVec_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
    ;                                                                   xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
      ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[1,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
      ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
      ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[2,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
      ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    
                ; plot surface
             if fCOOk and (total(vert_color[3,*]) gt 0) then oObjectModel->add, obj_new('IDLgrPolygon', shading = 1, shininess = 128., data = xyzPoints, poly = polygons, vert_colors = vert_color,$
                                                                      bottom = -1, name = strCompress('3DSurfDistUnDriftedRef_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
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