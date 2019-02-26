
pro s_getoSkelTreeFromMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel
   oColorModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oColorModel)) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oColorModel) then fOk = 1b else fOk = 0b

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()
   
   relacionXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 1
   thickSkel = 3.

   ;for i = 0, (oGroupReference->count())-1 do begin
   for i = 0, 0 do begin
     (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha
     objNumber = (oGroupReference->get(position = i))->getNumber()

; Do MAGIC.... FASL XXX TODO
      ;oObjectModel->add, recursiveOctreMaster->DrawSystem(thick = thickSkel, colV = colFillROI, name = strCompress('3DSkelTreeModel:'+string(i),/rem), uvalue = 'ObjInOriginalPosition', double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)

      ;cloudPoints = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
      ;if (cloudPoints[2] eq 1) then cloudPoints = reform(cloudPoints, xyzDim[0], xyzDim[1], xyzDim[2])
      ;cloudPoints[*((oGroupReference->get(position = i))->getpWherePoints())] = *((oGroupReference->get(position = i))->getpPointValues())
      
      ; Is possible to replace 
      cloudPoints = (oGroupReference->get(position = i))->makePixelObjectInVoxel(pixelFrame = 0, /all)
      if (max(cloudPoints.obj) gt 0) then begin
        
        dimensionesCloud = size(cloudPoints.obj,/DIM) 
        numPoints = total(cloudPoints.obj)
        pointsInCloud = where(cloudPoints.obj eq 1)
        xyzCloudPoints = make_array([3, numPoints],/DOUBLE, VALUE = 0.0d)
  
        factorX = dimensionesCloud(0)
        factorXY = factorX*dimensionesCloud(1)

        for indice = 0.0d, double(numPoints)-1.0d do begin
          index = pointsInCloud[indice]
          fK = floor(index/factorXY)
          fJ = floor((index-fK*factorXY)/factorX)
          fI = index - fK*factorXY - fJ*factorX
          xyzCloudPoints(0,indice) = fI + (cloudPoints.minX - cloudPoints.pixelFrame)
          xyzCloudPoints(1,indice) = fJ + (cloudPoints.minY - cloudPoints.pixelFrame)
          xyzCloudPoints(2,indice) = fK + (cloudPoints.minZ - cloudPoints.pixelFrame)
        endfor
  
        ;Obtain Graph Octree representation
        recursiveOctreMaster = obj_new('C_s3DRecursiveOctree')
        numPuntosDivision = 2.0d 
        factorPuntos = numPuntosDivision/numPoints
        recursiveOctreMaster->MasterOctreeCreation,XYZPOINTS=xyzCloudPoints,VFORCEDIV=1,VMINPOINTSOCTREE=numPuntosDivision,VRATEMINDISTANCEOCTREE=0.05d,VRATEMINPOINTSOCTREE=factorPuntos
        ;color(0) = 0
        ;color(1) = 255
        ;color(2) = 0

        ;oObjectModel->add, recursiveOctreMaster->DrawSystem(thick = thickSkel, colV = color, name = strCompress('3DOctreeModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition', double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)

        ;oObjectModel->add, recursiveOctreMaster->DrawEndSystem(thick = 2.0d*thickSkel, colV = color, name = strCompress('3DSOctreeModelEnd:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition', double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
        ;Obtain Skel representation
        skelTreeMaster = obj_new('C_s3DOctreeSkel')
        skelTreeMaster->MasterSkelCreation, MASTEROWNOCTREE = recursiveOctreMaster->GetMasterNode(),XYZPOINTS=xyzCloudPoints
        ;oObjectModel->add, skelTreeMaster->DrawSystem(factorXYZ = relacionXYZ, thick = thickSkel, colV = color, name = strCompress('3DOctreeSkelModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition', double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
        ;oObjectModel->add, skelTreeMaster->DrawLastNodes(factorXYZ = relacionXYZ, thick = thickSkel, colV = color, name = strCompress('3DOctreeSkelModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition', double = double, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)        
        ;obj.obj = obj.obj*FieldDistance
;          shade_volume, cloudPoints.obj, 0, vertices, polygons, /low
;  
;        vertices[0,*] += (cloudPoints.minX - cloudPoints.pixelFrame)
;        vertices[1,*] += (cloudPoints.minY - cloudPoints.pixelFrame)
;        vertices[2,*] += (cloudPoints.minZ - cloudPoints.pixelFrame)
;

        verticesOctreeGraph = skelTreeMaster->GetVertexsOctreeGraph()
        polygonsOctreeGraph = skelTreeMaster->GetEdgesArrayOctreeGraph()
        
        verticesSkelTree = skelTreeMaster->GetVertexsSkelTree()
        polygonsSkelTree = skelTreeMaster->GetArrayMasterEdges()

        color(0) = 255
        color(1) = 0
        color(2) = 0
  
;        oObjectModel->add, obj_new('IDLgrPolyline', data = verticesOctreeGraph, poly = polygonsOctreeGraph, color = color,$
;                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DMeshModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')

        color(0) = 0
        color(1) = 255
        color(2) = 0

        oObjectModel->add, obj_new('IDLgrPolyline', data = verticesSkelTree, poly = polygonsSkelTree, color = color,$
                                    xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DMeshModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')


      
      endif
   endfor
end
