
; FASL, Obtain fill models.. volumetric representation in relation with color of Mesh Model
pro s_getoVolumeWColorModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel, fROIInt = fROIInt

; CREATE AND SET VALUES OF VARIABLES OF CUT PLANE
   planesCut    = -1

   bUsePlane1   = 0b
   centerPlane1 = [0.0,0.0,0.0]
   normalPlane1 = [0.0,0.0,1.0]
   angles1      = [0.0,0.0] ; In general -> normalPlane = Normalize(refPoint - centerPlane)
   dParam1      = -1.0
   planeData1   = -1
   displacement1= 0.0

   bUsePlane2   = 0b
   centerPlane2 = [0.0,0.0,0.0]
   normalPlane2 = [0.0,1.0,0.0]
   angles2      = [0.0,0.0] ; In general -> normalPlane = Normalize(refPoint - centerPlane)
   dParam2      = -1.0
   planeData2   = -1   
   displacement2= 0.0
   
   xyzDim = oGroupReference->getxyzDim()
    ; FIrst Object---- Arbitrary Plane Parameters
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Active_Plane'))[0]
   if (whParam ne -1) then begin
      bUsePlane1 = fix(*(*(oGroupReference->getpParamStruct())).pValues[whParam] < 1)
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = bUsePlane1
   endif
       
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Center_Plane X'))[0]
   if (whParam ne -1) then begin
      centerPlane1[0] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[0]+1)) < (xyzDim[0]-1))
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = centerPlane1[0]
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Center_Plane Y'))[0]
   if (whParam ne -1) then begin
      centerPlane1[1] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[1]+1)) < (xyzDim[1]-1))
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = centerPlane1[1]
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Center_Plane Z'))[0]
   if (whParam ne -1) then begin
      centerPlane1[2] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[2]+1)) < (xyzDim[2]-1))
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = centerPlane1[2]
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Angle_Plane A'))[0]
   if (whParam ne -1) then begin
      angles1[0] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (- 360.0)) < (360.0))
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = angles1[0]
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Angle_Plane B'))[0]
   if (whParam ne -1) then begin
      angles1[1] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (- 360.0)) < (360.0))
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = angles1[1]
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Displacement_Plane'))[0]
   if (whParam ne -1) then begin
      displacement1 = *(*(oGroupReference->getpParamStruct())).pValues[whParam]
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = displacement1
   endif
      ; get 2nd object and object Params

   ;if obj_valid(oROI3DGroup) then begin
       ; Second Object---- Arbitrary Plane Parameters
       whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2st Active_Plane'))[0]
       if (whParam ne -1) then begin
          bUsePlane2 = fix(*(*(oGroupReference->getpParamStruct())).pValues[whParam] < 1)
          *(*(oGroupReference->getpParamStruct())).pValues[whParam] = bUsePlane2
       endif
           
       whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2st Center_Plane X'))[0]
       if (whParam ne -1) then begin
          centerPlane2[0] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[0]+1)) < (xyzDim[0]-1))
          *(*(oGroupReference->getpParamStruct())).pValues[whParam] = centerPlane2[0]
       endif
    
       whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2st Center_Plane Y'))[0]
       if (whParam ne -1) then begin
          centerPlane2[1] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[1]+1)) < (xyzDim[1]-1))
          *(*(oGroupReference->getpParamStruct())).pValues[whParam] = centerPlane2[1]
       endif

       whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2st Center_Plane Z'))[0]
       if (whParam ne -1) then begin
          centerPlane2[2] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[2]+1)) < (xyzDim[2]-1))
          *(*(oGroupReference->getpParamStruct())).pValues[whParam] = centerPlane2[2]
       endif

       whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2st Angle_Plane A'))[0]
       if (whParam ne -1) then begin
          angles2[0] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (- 360.0)) < (360.0))
          *(*(oGroupReference->getpParamStruct())).pValues[whParam] = angles2[0]
       endif
    
       whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2st Angle_Plane B'))[0]
       if (whParam ne -1) then begin
          angles2[1] = ((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (- 360.0)) < (360.0))
          *(*(oGroupReference->getpParamStruct())).pValues[whParam] = angles2[1]
       endif
    
       whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2st Displacement_Plane'))[0]
       if (whParam ne -1) then begin
          displacement2 = *(*(oGroupReference->getpParamStruct())).pValues[whParam]
          *(*(oGroupReference->getpParamStruct())).pValues[whParam] = displacement2
       endif

    ; Change to conv coords
    oGroupReference->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzDim = oGroupReference->getxyzDim()
    xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()
     
    dimensionesBox = make_array(3,2)
    dimensionesBox[0,*] = [0.0,xyzDim[0]*1.0]
    dimensionesBox[1,*] = [0.0,xyzDim[1]*1.0]
    dimensionesBox[2,*] = [0.0,xyzDim[2]*1.0]

    ; First Normalize NormalPlane
    if (bUsePlane1) then begin
      ; Angle1 is the rotation between axis x and z... need scale for correct rotational effect
      ; Obtain normal using angles1
      angles1 = !dtor* angles1
      normalPlane1[0] = sin(angles1[0])*cos(angles1[1])
      normalPlane1[1] = sin(angles1[0])*sin(angles1[1])
      normalPlane1[2] = cos(angles1[0])

      normalPlane1[0] *= xyzSizePerPixel[0]/xyzSizePerPixel[0] 
      normalPlane1[1] *= xyzSizePerPixel[1]/xyzSizePerPixel[0] 
      normalPlane1[2] *= xyzSizePerPixel[2]/xyzSizePerPixel[0]

      modulo1 = sqrt(total(normalPlane1*normalPlane1))
      normalPlane1 = normalPlane1 / modulo1
      
      ; Reposition of plane in Normal_Plane_Direction
      centerPlane1 += displacement1 * normalPlane1
      
      ; Obtain "dParam" for PLane eq for PLane Cut
      dParam1     = - total(normalPlane1 * centerPlane1)
    endif
    if (bUsePlane2) then begin
      angles2 = !dtor* angles2
      normalPlane2[0] = sin(angles2[0])*cos(angles2[1])
      normalPlane2[1] = sin(angles2[0])*sin(angles2[1])
      normalPlane2[2] = cos(angles2[0])


      normalPlane2[0] *= xyzSizePerPixel[0]/xyzSizePerPixel[0] 
      normalPlane2[1] *= xyzSizePerPixel[1]/xyzSizePerPixel[0] 
      normalPlane2[2] *= xyzSizePerPixel[2]/xyzSizePerPixel[0]

      modulo2 = sqrt(total(normalPlane2*normalPlane2))
      normalPlane2 = normalPlane2 / modulo2
      ; Reposition of plane in Normal_Plane_Direction
      centerPlane2 += displacement2 * normalPlane2
            
      ; Obtain "dParam" for PLane eq
      dParam2 = - total(normalPlane2 * centerPlane2)
    endif

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   nObj = oGroupReference->count()

   for i = 0, nObj-1 do begin
      obj = (oGroupReference->get(position = i))->makePixelObjectInVoxel(/all)
      dimensionesWorld = size(obj.obj,/dim);
      FieldDistance = make_array(dimensionesWorld,/double, VALUE = 0.0d)
            
      if (bUsePlane1) then begin
        ;normalPlane1
        ;dParam1
      endif
      if (bUsePlane2) then begin
        ;normalPlane2
        ;dParam2
      endif

      if (bUsePlane1 or bUsePlane2) then begin
;        for fI=0,dimensionesWorld(0)-1 do begin
;          posX = obj.minX + fI
;          for fJ=0,dimensionesWorld(1)-1 do begin
;            posY = obj.minY + fJ
;            for fK=0,dimensionesWorld(2)-1 do begin
;              posZ = obj.minZ + fK
;              if (bUsePlane1 and ((posX*normalPlane1(0)+posY*normalPlane1(1)+posZ*normalPlane1(2)+dParam1) gt 0.0d)) then begin
;                FieldDistance(fI,fJ,fK) = 0
;              endif
;              if (bUsePlane2 and ((posX*normalPlane2(0)+posY*normalPlane2(1)+posZ*normalPlane2(2)+dParam2) gt 0.0d)) then begin
;                FieldDistance(fI,fJ,fK) = 0
;              endif
;            endfor
;          endfor
;        endfor
        MX = make_array(dimensionesWorld,/DOUBLE, VALUE = 0.0d)
        MY = make_array(dimensionesWorld,/DOUBLE, VALUE = 0.0d)
        MZ = make_array(dimensionesWorld,/DOUBLE, VALUE = 0.0d)
        VX = make_array(dimensionesWorld(0),/DOUBLE, /index) + obj.minX
        VY = make_array(dimensionesWorld(1),/DOUBLE, /index) + obj.minY
        VZ = make_array(dimensionesWorld(2),/DOUBLE, /index) + obj.minZ
        
        unit_vector = replicate(1d,dimensionesWorld(1))
        matrixXY = unit_vector##VX

        unit_vector = replicate(1d,dimensionesWorld(0))
        matrixYX = VY##unit_vector
        for fI=0,dimensionesWorld(2)-1 do begin
          MX(*,*,fI) = matrixXY
          MY(*,*,fI) = matrixYX
        endfor
        matrixXY = 0
        VX = 0
        matrixYX = 0
        VY = 0

        matrixZX = VZ##unit_vector
        for fI=0,dimensionesWorld(1)-1 do begin
          MZ(*,fI,*) = matrixZX
        endfor
        matrixZX = 0
        
        if (bUsePlane1) then begin
          FieldDistance = MX*normalPlane1(0)+MY*normalPlane1(1)+MZ*normalPlane1(2)+dParam1
          obj.obj = obj.obj * (FieldDistance le 0) 
        endif
        if (bUsePlane2) then begin
          FieldDistance = MX*normalPlane2(0)+MY*normalPlane2(1)+MZ*normalPlane2(2)+dParam2
          obj.obj = obj.obj * (FieldDistance le 0)
         endif
      endif
      
      ;obj.obj = obj.obj*FieldDistance
      if (max(obj.obj) gt 0) then begin
        shade_volume, obj.obj, 0, vertices, polygons, /low
  
        vertices[0,*] += (obj.minX - obj.pixelFrame)
        vertices[1,*] += (obj.minY - obj.pixelFrame)
        vertices[2,*] += (obj.minZ - obj.pixelFrame)
  
  ;      oObjectModel->add, obj_new('IDLgrPolygon', ambient = [0,0,0], data = vertices, poly = polygons, shading = 1,$
  ;                                  vert_colors = transpose([[rgb_table0],[opacVect*255.]]),$
  ;                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DSurfaceModel:'+string(i),/rem), uvalue = 'ObjInOriginalPosition')
  
        (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = i))->getNumber()
        oObjectModel->add, obj_new('IDLgrPolygon', ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                    xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DMeshModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
       endif
   endfor
end
