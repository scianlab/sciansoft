; FASL 
pro s_getoPCUnDrifted, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos,clusPos = clusPos
   xyzDim = oGroupReference->getxyzDim()

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)

   colEV = [0,255,0]
   
   colEV2 = [223,165,0]
   colEV3 = [0,128,64]
   
   scaleAxis = 1.0
   thickEV = 3.

   axisHull = fltArr(3,2 * (oGroupReference->count()))
   for i = 0, (oGroupReference->count())-1 do begin
         objNumber = (oGroupReference->get(position = i))->getNumber()
         scaleEV = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals) / max(((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)) * scaleAxis
         scaleEV = [scaleAxis, scaleAxis, scaleAxis]
         axis = fltArr(3,2)
         axisHull[*,(2*i):(2*i+1)] = axis

   ;      axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * (-scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * (scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                    name = strCompress('3D1stPC_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   ;      axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[1,*] * (-scaleEV[1]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[1,*] * (scaleEV[1]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[1,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[1] * (-scaleEV[1]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[1,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[1] * (scaleEV[1]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV2, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                    name = strCompress('3D2ndPC_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   ;      axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[2,*] * (-scaleEV[2]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[2,*] * (scaleEV[2]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[2,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[2] * (-scaleEV[2]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[2,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[2] * (scaleEV[2]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV3, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                    name = strCompress('3D3rdPC_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   endfor

   QHULL, axisHull[0,*], axisHull[1,*], axisHull[2,*], oQHull, CONNECTIVITY  = pols
   
      ;oObjectModel->add, obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = oQHull, poly = pols, bottom = color, color = color, shading = 1, shininess = 128.,$
      ;                            xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DQHullModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   
      oObjectModel->getproperty, TRANSFORM = matrixModel
      matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
      ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
      oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix
   
end

pro s_getoPCUnDriftedGroup, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   axis = fltArr(3,2)
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos,clusPos = clusPos
   ;scaleEV = [1.1, 1.1, 1.1]
   scaleEV = [.3, .5, .3]
   colEV = [200., 50., 50.]
   thickEV = 2.

   a = oGroupReference->calcEigenSys()
   axis[*,0] = ((*(oGroupReference->getpEigenSys())).eigenVect)[0,*] *((*(oGroupReference->getpEigenSys())).eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
   axis[*,1] = ((*(oGroupReference->getpEigenSys())).eigenVect)[0,*] *((*(oGroupReference->getpEigenSys())).eigenVals)[0] * scaleEV[0] / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D1stGroupPC_:'), uvalue = 'ObjInOriginalPosition')
;   axis[*,0] = ((*(oGroupReference->getpEigenSys())).eigenVect)[1,*] * (-scaleEV[1]) / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
;   axis[*,1] = ((*(oGroupReference->getpEigenSys())).eigenVect)[1,*] * scaleEV[1] / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
    axis[*,0] = ((*(oGroupReference->getpEigenSys())).eigenVect)[1,*] *((*(oGroupReference->getpEigenSys())).eigenVals)[1] * (-scaleEV[1]) / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
    axis[*,1] = ((*(oGroupReference->getpEigenSys())).eigenVect)[1,*] *((*(oGroupReference->getpEigenSys())).eigenVals)[1] * scaleEV[1] / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D2ndGroupPC_:'), uvalue = 'ObjInOriginalPosition')
;   axis[*,0] = ((*(oGroupReference->getpEigenSys())).eigenVect)[2,*] * (-scaleEV[2]) / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
;   axis[*,1] = ((*(oGroupReference->getpEigenSys())).eigenVect)[2,*] * scaleEV[2] / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
    axis[*,0] = ((*(oGroupReference->getpEigenSys())).eigenVect)[2,*] *((*(oGroupReference->getpEigenSys())).eigenVals)[2] * (-scaleEV[2]) / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
    axis[*,1] = ((*(oGroupReference->getpEigenSys())).eigenVect)[2,*] *((*(oGroupReference->getpEigenSys())).eigenVals)[2] * scaleEV[2] / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D3rdGroupPC_:'), uvalue = 'ObjInOriginalPosition')


      oObjectModel->getproperty, TRANSFORM = matrixModel
      matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
      ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
      oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix

end

pro s_getoPCUnDriftedGroupEstimated, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos,clusPos = clusPos
   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   axis = fltArr(3,2)
   ;scaleEV = [1.1, 1.1, 1.1]
   scaleEV = [.3, .5, .3]
   colEV = [200., 50., 50.]
   thickEV = 2.

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)

   
   colEV = [0,0,0]
   colEV2 = [223,165,0]
   colEV3 = [0,128,64]
   
   scaleAxis = 1.0
   thickEV = 3.
   colEV = [0,0,255]

   ;axisG = fltArr(3,2*3* ((oGroupReference->count()) ))
   axisG = fltArr(3,2* ((oGroupReference->count()) ))
   for i = 0, (oGroupReference->count())-1 do begin
         objNumber = (oGroupReference->get(position = i))->getNumber()
         scaleEV = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals) / max(((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)) * scaleAxis
         scaleEV = [scaleAxis, scaleAxis, scaleAxis]

   ;      axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * (-scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * (scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axisG[*,2*i+0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
         axisG[*,2*i+1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
        ; axisG[*,2*3*i+0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
       ;  axisG[*,2*3*i+1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[0] * (scaleEV[0]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ

   ;      axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[1,*] * (-scaleEV[1]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[1,*] * (scaleEV[1]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
       ;  axisG[*,2*3*i+2] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[1,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[1] * (-scaleEV[1]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
      ;   axisG[*,2*3*i+3] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[1,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[1] * (scaleEV[1]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,0] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[2,*] * (-scaleEV[2]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[2,*] * (scaleEV[2]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
      ;   axisG[*,2*3*i+4] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[2,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[2] * (-scaleEV[2]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
      ;   axisG[*,2*3*i+5] = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVect)[2,*] * ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)[2] * (scaleEV[2]) / xyzSPPNorm + (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
   endfor

   axis = fltArr(3,2)

   dimSys = size(axisG, /dim)
   if (n_elements(dimSys) eq 1) then dimSys = [dimSys, 1.]
   MYxyz = make_array(dimSys, /double)

       centerXYZ = [total(axisG[0,*]), total(axisG[1,*]), total(axisG[2,*])] / (1.*(size(axisG, /dim))[1])
       MYxyz[0,*] = (axisG[0,*] - centerXYZ[0]) * ((*(oGroupReference->getpEigenSys())).sizePerXYZ)[0]
       MYxyz[1,*] = (axisG[1,*] - centerXYZ[1]) * ((*(oGroupReference->getpEigenSys())).sizePerXYZ)[1]
       MYxyz[2,*] = (axisG[2,*] - centerXYZ[2]) * ((*(oGroupReference->getpEigenSys())).sizePerXYZ)[2]

       inertiaT = make_array(3,3, /double)
       inertiaT[0:2,0] = [ total(MYxyz[0,*]*MYxyz[0,*]), total(MYxyz[0,*]*MYxyz[1,*]), total(MYxyz[0,*]*MYxyz[2,*]) ]
       inertiaT[0:2,1] = [ inertiaT[1,0],                total(MYxyz[1,*]*MYxyz[1,*]), total(MYxyz[1,*]*MYxyz[2,*]) ]
       inertiaT[0:2,2] = [ inertiaT[2,0],                inertiaT[2,1],                total(MYxyz[2,*]*MYxyz[2,*]) ]
       inertiaT /= dimSys[1]

       eigenVals = eigenQL(inertiaT, /double, eigenvectors = eigenVect, residual = var)
       eigenVect = transpose(eigenVect)
       if (determ(eigenVect) eq -1) then eigenVect = [-(eigenVect)[0,*], (eigenVect)[1,*], (eigenVect)[2,*]]

   axis[*,0] = (eigenVect)[0,*] *(eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + centerXYZ
   axis[*,1] = (eigenVect)[0,*] *(eigenVals)[0] * scaleEV[0] / xyzSPPNorm    + centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D1stGroupPC_:'), uvalue = 'ObjInOriginalPosition')
;   axis[*,0] = ((*(oGroupReference->getpEigenSys())).eigenVect)[1,*] * (-scaleEV[1]) / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
;   axis[*,1] = ((*(oGroupReference->getpEigenSys())).eigenVect)[1,*] * scaleEV[1] / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
    axis[*,0] = (eigenVect)[1,*] *(eigenVals)[1] * (-scaleEV[1]) / xyzSPPNorm + centerXYZ
    axis[*,1] = (eigenVect)[1,*] *(eigenVals)[1] * scaleEV[1] / xyzSPPNorm + centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D2ndGroupPC_:'), uvalue = 'ObjInOriginalPosition')
;   axis[*,0] = ((*(oGroupReference->getpEigenSys())).eigenVect)[2,*] * (-scaleEV[2]) / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
;   axis[*,1] = ((*(oGroupReference->getpEigenSys())).eigenVect)[2,*] * scaleEV[2] / xyzSPPNorm + (*(oGroupReference->getpEigenSys())).centerXYZ
    axis[*,0] = (eigenVect)[2,*] *(eigenVals)[2] * (-scaleEV[2]) / xyzSPPNorm + centerXYZ
    axis[*,1] = (eigenVect)[2,*] *(eigenVals)[2] * scaleEV[2] / xyzSPPNorm +   centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D3rdGroupPC_:'), uvalue = 'ObjInOriginalPosition')


      oObjectModel->getproperty, TRANSFORM = matrixModel
      matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
      ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
      oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix

end
