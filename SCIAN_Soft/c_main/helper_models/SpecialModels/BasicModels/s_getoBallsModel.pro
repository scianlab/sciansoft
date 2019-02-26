; FASL get Balls Model
pro s_getoBallsModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

   radio = 5
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Radio Balls Model'))[0]
   if (whParam ne -1) then begin
      radio = fix(*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 1)
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = radio
   endif
   
   oColorModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oColorModel)) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oColorModel) then fOk = 1b else fOk = 0b

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()

   factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 2
   thickEV = 2.
   colEV = [218,218,218]
   radio = 5
   
   
   for i = 0, (oGroupReference->count())-1 do begin
      if fOk then begin
         oObj = oColorModel->get(position = i)
         if obj_valid(oObj) then begin
            oObj->getProperty, data = xyzPoints, poly = polygons, color = colEV
            fCOOk = 1b
         endif else fCOOk = 0b
      endif else fCOOk = 0b
    colEV = [218,218,218]; sacar para usar color mesh
      objNumber = (oGroupReference->get(position = i))->getNumber()
      scaleEV = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals) / max(((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)) * scaleAxis
      scaleEV = [scaleAxis, scaleAxis, scaleAxis]

      center = fltArr(3)
      center = (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
 
      oObjectModel->add, obj_new('C_sOrb', POS=center, RADIUS=radio, xyzFact = factorXYZ, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                 name = strCompress('3D_Sphere_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   endfor
end

pro s_getoBallsMinMaxModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

   radio = 5  
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Radio Balls Model'))[0]
   if (whParam ne -1) then begin
      radio = fix(*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 1)
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = radio
   endif
   minmax = make_array(3, 2)
   minimo = make_array(3)
   maximo = make_array(3)
   centro = make_array(3)
   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()  
   factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]
   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 2
   thickEV = 2.
   
   for i = 0, (oGroupReference->count())-1 do begin
      xyzpoint = ((oGroupReference->get(position = i))->getxyzPoints())
      k=size(xyzpoint,/dim)
      centro += [total(xyzpoint[0,*])/k[1],total(xyzpoint[2,*])/k[1],total(xyzpoint[2,*])/k[1]]
   endfor
   xyzCenter = centro/oGroupReference->count()

   for i = 0, (oGroupReference->count())-1 do begin

      evProj = (oGroupReference->get(position = i))->getEVSProj()
      minmax = evProj.evminmax[*, 0:1]
      xyzDist = reform(sqrt( (minmax[0,*] - xyzCenter[0])^2 + (minmax[1,*] - xyzCenter[1])^2 + (minmax[2,*] - xyzCenter[2])^2))
      minimo = minmax[*, where(xyzDist eq min(xyzDist, max=max))]
      maximo = minmax[*, where(xyzDist eq max)]
      objNumber = (oGroupReference->get(position = i))->getNumber()
      scaleEV = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals) / max(((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)) * scaleAxis
      scaleEV = [scaleAxis, scaleAxis, scaleAxis]
      
      oObjectModel->add, obj_new('C_sOrb', POS = minimo, RADIUS = radio, xyzFact = factorXYZ, color = [255,0,0], thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                           name = strCompress('3D_Sphere_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')

      oObjectModel->add, obj_new('C_sOrb', POS = maximo, RADIUS = radio, xyzFact = factorXYZ, color = [255,255,0], thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                           name = strCompress('3D_Sphere_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')

      oObjectModel->add, obj_new('IDLgrPolyline', [minimo[0],maximo[0]], [minimo[1],maximo[1]], [minimo[2],maximo[2]], color = [255,255,255], thick = 2, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_Sphere_:'+string(objNumber),/rem),  uvalue = 'ObjInOriginalPosition')
   endfor
end

; FASL get AAAMA
pro s_getoAAAMA, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 2
   thickEV = 3.
   colEV = [255,255,255]

; Parameters for Arbitrary Axis
 
   bIsAAAMA = 0b
   vPointInitial = [0.0,0.0,0.0]
   vPointFinal = [100.0,0.0,0.0]
   
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Is AA AMA'))[0]
   if (whParam ne -1) then begin
      bIsAAAMA = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = bIsAAAMA
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'X for first Point'))[0]
   if (whParam ne -1) then begin
      vPointInitial[0] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vPointInitial[0]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Y for first Point'))[0]
   if (whParam ne -1) then begin
      vPointInitial[1] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vPointInitial[1]
   endif 
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Z for first Point'))[0]
   if (whParam ne -1) then begin
      vPointInitial[2] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vPointInitial[2]
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'X for second Point'))[0]
   if (whParam ne -1) then begin
      vPointFinal[0] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vPointFinal[0]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Y for second Point'))[0]
   if (whParam ne -1) then begin
      vPointFinal[1] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vPointFinal[1]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Z for second Point'))[0]
   if (whParam ne -1) then begin
      vPointFinal[2]= *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vPointFinal[2]
   endif
          
   ;scaleEV = ((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals) / max(((*((oGroupReference->get(position = i))->getpEigenSys())).eigenVals)) * scaleAxis
   scaleEV = [1.0, 1.0, 1.0]
   axis = fltArr(3,2)

   axis[*,0] = vPointInitial; * xyzSizePerPixel)/xyzSizePerPixel[0] 
   axis[*,1] = vPointFinal; * xyzSizePerPixel)/xyzSizePerPixel[0] 

   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                              name = strCompress('3D_AA_AMA',/rem), uvalue = 'ObjInOriginalPosition')
end

; FASL get ADAAAMA... axis for cell division
pro s_getoADAAAMA, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 1
   thickEV = 2.
   colEV = [0,0,255]

; Parameters for Arbitrary Axis by cells division 
 ; Obtain AD_AA_AMA
   largo = - 1
   bIsADAAAMA = 0b
   originalCellsNumber = 0
   vChilds = fltArr(12)
   vPointInitial = [0.0,0.0,0.0]
   vPointFinal = [100.0,0.0,0.0]

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Length Div Axis'))[0]
   if (whParam ne -1) then begin
      largo = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = largo
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Is Limit Hist'))[0]
   if (whParam ne -1) then begin
      bIsADAAAMA = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = bIsADAAAMA
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Limit Hist'))[0]
   if (whParam ne -1) then begin
      originalCellsNumber = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = originalCellsNumber
   endif

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 1'))[0]
   if (whParam ne -1) then begin
      vChilds[0] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[0]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 2'))[0]
   if (whParam ne -1) then begin
      vChilds[1] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[1]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 3'))[0]
   if (whParam ne -1) then begin
      vChilds[2] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[2]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 4'))[0]
   if (whParam ne -1) then begin
      vChilds[3] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[3]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 5'))[0]
   if (whParam ne -1) then begin
      vChilds[4] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[4]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 6'))[0]
   if (whParam ne -1) then begin
      vChilds[5] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[5]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 7'))[0]
   if (whParam ne -1) then begin
      vChilds[6] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[6]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 8'))[0]
   if (whParam ne -1) then begin
      vChilds[7] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[7]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 9'))[0]
   if (whParam ne -1) then begin
      vChilds[8] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[8]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 10'))[0]
   if (whParam ne -1) then begin
      vChilds[9] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[9]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 11'))[0]
   if (whParam ne -1) then begin
      vChilds[10] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[10]
   endif
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Child 12'))[0]
   if (whParam ne -1) then begin
      vChilds[11] = *(*(oGroupReference->getpParamStruct())).pValues[whParam] 
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = vChilds[11]
   endif

   oColorModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oColorModel)) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oColorModel) then fOk = 1b else fOk = 0b

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()
   
   scaleAxis = 1
   thickEV = 2.
   colEV = [0,0,255]
   axis = fltArr(3,2)
   
   for i = 0, (oGroupReference->count())-1 do begin
      if fOk then begin
         oObj = oColorModel->get(position = i)
         if obj_valid(oObj) then begin
            oObj->getProperty, data = xyzPoints, poly = polygons, color = colEV
            fCOOk = 1b
         endif else fCOOk = 0b
      endif else fCOOk = 0b

      vPointInitial = (*((oGroupReference->get(position = i))->getpEigenSys())).centerXYZ
      axis[*,0] = vPointInitial; * xyzSizePerPixel)/xyzSizePerPixel[0] 
    
       if (originalCellsNumber  gt 1) then begin
          if (i lt originalCellsNumber) then begin
              for f = 0, 11 do begin
                  fatherIndex = vChilds[f] - 1
                  sonIndex    = originalCellsNumber + f
                  if ((fatherIndex eq i) and (sonIndex lt oGroupReference->count())) then begin
                       vPointFinal = (*((oGroupReference->get(position = sonIndex))->getpEigenSys())).centerXYZ
                       axis[*,1] = vPointFinal; * xyzSizePerPixel)/xyzSizePerPixel[0]
                       if ((largo gt 0) and (total(axis[*,1] - axis[*,0]) ne 0.0)) then begin
                          centro = (axis[*,1] + axis[*,0])/2.0
                          direccion = axis[*,0] - centro;
                          modulo = sqrt(total(direccion*direccion))
                          direccion = direccion / modulo 
                          axis[*,0] = centro + (largo/2.0)*direccion
                          axis[*,1] = centro - (largo/2.0)*direccion
                       endif
                       oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_ADAA_AMA_:'+string(i),/rem), uvalue = 'ObjInOriginalPosition')
                  endif
              endfor
          endif
       endif
   endfor
end
