; FASL get Balls Model
pro s_getoSurfaceUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   nObj = oGroupReference->count()
 
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()  
   factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]      
   
   clusPos  = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Cluster Position'))[0]]    
   chPos    = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Channel Position'))[0]]
   tPos     = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Time Position'))[0]] 
   radio    = 10
   
   C_sROI3DGroupObjOrigen = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = 0, chPos = chPos, clusPos = clusPos)  
   C_sROI3DGroupObjOrigen->getProperty, xCoord_conv = xCoord_convO, yCoord_conv = yCoord_convO, zCoord_conv = zCoord_convO     
   for i = 0, nObj-1 do begin
      obj = (oGroupReference->get(position = i))->makePixelObjectInVoxel(/all)
      shade_volume, obj.obj, 0, vertices, polygons, /low

      vertices[0,*] += (obj.minX - obj.pixelFrame)
      vertices[1,*] += (obj.minY - obj.pixelFrame)
      vertices[2,*] += (obj.minZ - obj.pixelFrame)

;         oObjectModel->add, obj_new('IDLgrPolygon', ambient = [0,0,0], data = vertices, poly = polygons, shading = 1,$
;                                     vert_colors = transpose([[rgb_table0],[opacVect*255.]]),$
;                                     xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DSurfaceUnDriftedModel:'+string(i),/rem), uvalue = 'ObjInOriginalPosition')

      (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha
      objNumber = (oGroupReference->get(position = i))->getNumber()

      oObjectModel->add, obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, shading = 1, shininess = 128.,$
                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DSurfaceUnDriftedModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')


;      matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos, pRefActual = pRefActual, pRefOrigen = pRefOrigen)      
;      oObjectModel->add, obj_new('C_sOrb', POS = pRefActual, RADIUS = radio, xyzFact = factorXYZ, color = [0,255,0], thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
;                                           name = strCompress('3D_Sphere_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
                                   
;      oObjectModel->add, obj_new('C_sOrb', POS = pRefOrigen, RADIUS = radio, xyzFact = factorXYZ, color = [0,0,255], thick = thickEV, xCoord_conv = xCoord_convO, yCoord_conv = yCoord_convO, zCoord_conv = zCoord_convO,$
;                                           name = strCompress('3D_Sphere_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   endfor
      obj_destroy,C_sROI3DGroupObjOrigen 
      
      oObjectModel->getproperty, TRANSFORM = matrixModel
      matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
      ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
      oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix

end
