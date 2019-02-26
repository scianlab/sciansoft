pro s_getoTorsionSphere, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel
end
    
pro s_getoTorsionSphere_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel


  oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos, totalTNum = totalTNum 

  xyzDim      = oGroupReference->getxyzDim()
  xyzPixel    = oGroupReference->getxyzSizePerPixel()
  zxRatio     = xyzPixel[2] / xyzPixel[0]

  ; Recovery the centers for the cluster 0 .... i.e the center for the original group considering both sides..
  groupCenters = dblarr(3,totalTNum) * 0.0d
  sideCenters  = dblarr(3,totalTNum) * 0.0d
  tempPoint    = dblarr(4) * 0.0d
  
  cambioRadiosSign   = dblarr(2) * 0.0d
  for t = 0, totalTNum - 1 do begin
    C_sROI3DGroupObjOriginal = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = t, chPos = chPos, clusPos = 0)
    C_sROI3DSideObjOriginal = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = t, chPos = chPos, clusPos = clusPos)
    
        numObjetosOriginal = C_sROI3DGroupObjOriginal->count()
        numObjetosActual   = C_sROI3DSideObjOriginal->count()
        
        for j = 0, numObjetosOriginal-1 do begin
           groupCenters[*,t] = groupCenters[*,t] + s_RecalculateCenters(stack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjOriginal, actualTime = t, actualNumber = j,chPos = chPos, clusPos = 0) 
        endfor 
        for j = 0, numObjetosActual-1 do begin
          sideCenters[*,t] = sideCenters[*,t] + s_RecalculateCenters(stack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DSideObjOriginal, actualTime = t, actualNumber = j,chPos = chPos, clusPos = clusPos) 
        endfor 
        
    sideCenters[*,t]  = sideCenters[*,t]  / numObjetosActual
    groupCenters[*,t] = groupCenters[*,t] / numObjetosOriginal 

    if(t gt 0) then begin
      radioAct   = sideCenters[*,t] - groupCenters[*,t]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,t-1] - groupCenters[*,t-1]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)            
    endif
    obj_destroy, C_sROI3DGroupObjOriginal
    obj_destroy, C_sROI3DSideObjOriginal
   endfor
      
   ; Please kill me... only for selected Yoya variations
      radioAct   = sideCenters[*,2] - groupCenters[*,2]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,0] - groupCenters[*,0]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       

      radioAct   = sideCenters[*,3] - groupCenters[*,3]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,2] - groupCenters[*,2]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       
     
      radioAct   = sideCenters[*,5] - groupCenters[*,5]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,3] - groupCenters[*,3]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       

      radioAct   = sideCenters[*,7] - groupCenters[*,7]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,5] - groupCenters[*,5]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       
     
      radioAct   = sideCenters[*,9] - groupCenters[*,9]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,5] - groupCenters[*,5]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       
   ; killme end   
   
   indiceRef         = 0
   indiceRef         = (tPos-1 ge 0)? (tPos-1):0  ; Comment for specific times comparative
   ;miRadialAnterior  = sideCenters[*,(tPos-1 ge 0)? (tPos-1):0] - groupCenters[*,(tPos-1 ge 0)? (tPos-1):0]
   miRadialActual    = sideCenters[*,tPos] - groupCenters[*,tPos]
   miRadialAnterior  = sideCenters[*,indiceRef] - groupCenters[*,indiceRef]

  numMeridianos = 3
  numParalelos = 1
  radio = 40  
   miEsferaTorsion = obj_new('C_sTorsionSphere', numParalelos = numParalelos, numMeridianos =  numMeridianos , centroXYZ = groupCenters[*,0], upDir = [0.0d,0.0d,1.0d], radious = radio)  
   ;miEsferaTorsion = obj_new('C_sTorsionSphere', numParalelos = 40, numMeridianos =  5 , centroXYZ = groupCenters[*,0], upDir = [0.3d,1.0d,0.0d], radious = 40)
   ; miEsferaTorsion = obj_new('C_sTorsionSphere', numParalelos = 40, numMeridianos =  40 , centroXYZ = groupCenters[*,0], upDir = [1.0d,0.0d,0.0d], radious = 40)
   
  
   ;miEsferaTorsion->applyTorsion, radialActual = miRadialActual, radialAnterior = miRadialAnterior, factDecay = 0.999
   ; Only need to apply torsion over control points...
   ; maybe speheres coloured blue if radious increment and red if was reduced...  
   ; we need intersection of CenterGroup-CenterSide with sphere...

   vertices       = *(miEsferaTorsion->getpVertexs())
   
   centro         = miEsferaTorsion->getCenter()
   vertices[2,*]  = vertices[2,*] - centro[2]
   vertices[2,*]  = vertices[2,*] / zxRatio
   vertices[2,*]  = vertices[2,*] + centro[2]
   poligonos = *(miEsferaTorsion->getPMesh())

   miEsferaTorsion->applyTorsion, radialActual = miRadialActual, radialAnterior = miRadialAnterior, factDecay = 0.999
   verticesM       = *(miEsferaTorsion->getpVertexs())   
   verticesM[2,*]  = verticesM[2,*] - centro[2]
   verticesM[2,*]  = verticesM[2,*] / zxRatio
   verticesM[2,*]  = verticesM[2,*] + centro[2]

   numVertexs = N_ELEMENTS(vertices[0,*])
   inicio = 0 + numMeridianos*2
   for index = inicio, numVertexs -1 - inicio do begin
      pointO = vertices[*,index]
      pointD = verticesM[*,index]
      normalO = pointO-centro
      normalD = pointD-centro
      diff    = pointD - pointO 
      distancia = sqrt(total(diff*diff)) 
      
;      if(distancia gt 3) then begin ;and (distancia lt 0.8*radio)) then begin
;          distancia = distancia 
;          normalO = normalO / sqrt(total(normalO*normalO))
;          normalD = normalD / sqrt(total(normalD*normalD))
;                
;          normalS = CROSSP(normalO, normalD)
;          normalS = normalS / sqrt(total(normalS*normalS))
;          
;          normalC = CROSSP(normalO, normalS)
;          normalC = normalC / sqrt(total(normalC*normalC))
;         
;          pointE = pointO + distancia * normalC
;         ; verticesM[*,index] = pointE
;      endif  

          if(index eq inicio) then begin
            poligonosLines = [2,index,index+numVertexs]
          endif else begin 
            poligonosLines = [poligonosLines, 2,index,index+numVertexs]
          endelse
   endfor
   verticesFull    = [[vertices],[verticesM]]
   
   
   radioACtual   = sideCenters[*,tPos] - groupCenters[*,tPos]
   mRActual      = sqrt(total(miRadialActual*miRadialActual)) 
   ;radioAnterior = sideCenters[*,((tPos-1)lt 0)?0:(tPos-1)] - groupCenters[*,((tPos-1)lt 0)?0:(tPos-1)]
   mRAnterior    = sqrt(total(miRadialAnterior*miRadialAnterior))
   
    switch 1b of
      (mRActual gt mRAnterior): begin
            agregado = ((mRActual - mRAnterior)/cambioRadiosSign[1])
            ;colorEsfera = [177.0d + agregado,agregado,agregado]
            colorEsfera = [255.0d,107.0d*(1.0d - agregado),107.0d*(1.0d - agregado)]
        break
      end
      (mRActual lt mRAnterior): begin
            agregado = ((mRActual - mRAnterior)/cambioRadiosSign[0]) 
            colorEsfera = [107.0d*(1.0d - agregado),107.0d*(1.0d - agregado),255.0d]
            ;colorEsfera = [27.0d*agregado,27.0d*agregado,177.0d + 27.0d*agregado]
            ;colorEsfera = [107,107,255]
        break
      end
      else: colorEsfera = [107,107,107] 
    endswitch     

   for i = 0, (oGroupReference->count())-1 do begin
     objNumber = (oGroupReference->get(position = i))->getNumber()
     oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = poligonos ,$
                                               ;ambient = [0,0,0], bottom = [0,0,0], color = [255,255,255], alpha_channel = 255, shininess = 128., shading = 1,$
                                               color = colorEsfera,$
                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                               name = strCompress('3DTorsionSphere:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition')

;     oObjectModel->add, obj_new('IDLgrPolyline', data = vertices, poly = poligonos ,$
;                                               ;ambient = [0,0,0], bottom = [0,0,0], color = [255,255,255], alpha_channel = 255, shininess = 128., shading = 1,$
;                                               color = [17,17,17],$
;                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
;                                               name = strCompress('3DTorsionSphere:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition')

;     oObjectModel->add, obj_new('IDLgrPolyline', data = verticesFull, poly = poligonosLines ,$
;                                               ;ambient = [0,0,0], bottom = [0,0,0], color = [255,255,255], alpha_channel = 255, shininess = 128., shading = 1,$
;                                               ;color = colorEsfera,$
;                                               color = [255,0,0],$
;                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
;                                               name = strCompress('3DTorsionSphere:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition')

;     oObjectModel->add, obj_new('IDLgrRoi', data = vertices ,$
;                                               ;ambient = [0,0,0], bottom = [0,0,0], color = [255,255,255], alpha_channel = 255, shininess = 128., shading = 1,$
;                                               color = [0,0,255] , STYLE  = 0, $
;                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
;                                               name = strCompress('3DTorsionSphere:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition')
;;
;     oObjectModel->add, obj_new('IDLgrRoi', data = verticesM ,$
;                                               ;ambient = [0,0,0], bottom = [0,0,0], color = [255,255,255], alpha_channel = 255, shininess = 128., shading = 1,$
;                                               color = [0,255,0] , STYLE  = 0, $
;                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
;                                               name = strCompress('3DTorsionSphere:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition')

   endfor
 
 ; oObjectModel->getproperty, TRANSFORM = matrixModel
 ; movimiento = [0,0,0]
 ; movimiento = [xCoord_conv[1],yCoord_conv[1],zCoord_conv[1]]*[xyzDim[0],xyzDim[1],xyzDim[2]]/2.0
 ; matrixModel[3,0:2] = matrixModel[3,0:2] + movimiento
 ; oObjectModel->setproperty, TRANSFORM = matrixModel  
  ;matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
  ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
  ;oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix
end

pro s_getoMaxTorsionCurve_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

  oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos, clusPos = clusPos, totalTNum = totalTNum 

  xyzDim      = oGroupReference->getxyzDim()
  xyzPixel    = oGroupReference->getxyzSizePerPixel()
  zxRatio     = xyzPixel[2] / xyzPixel[0]

  ; Recovery the centers for the cluster 0 .... i.e the center for the original group considering both sides..
  groupCenters = dblarr(3,totalTNum) * 0.0d
  sideCenters  = dblarr(3,totalTNum) * 0.0d
  tempPoint    = dblarr(4) * 0.0d
  
  cambioRadiosSign   = dblarr(2) * 0.0d
  for t = 0, totalTNum - 1 do begin
    C_sROI3DGroupObjOriginal = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = t, chPos = chPos, clusPos = 0)
    C_sROI3DSideObjOriginal = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = t, chPos = chPos, clusPos = clusPos)
    
        numObjetosOriginal = C_sROI3DGroupObjOriginal->count()
        numObjetosActual   = C_sROI3DSideObjOriginal->count()
        
        for j = 0, numObjetosOriginal-1 do begin
           groupCenters[*,t] = groupCenters[*,t] + s_RecalculateCenters(stack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjOriginal, actualTime = t, actualNumber = j,chPos = chPos, clusPos = 0) 
        endfor 
        for j = 0, numObjetosActual-1 do begin
          sideCenters[*,t] = sideCenters[*,t] + s_RecalculateCenters(stack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DSideObjOriginal, actualTime = t, actualNumber = j,chPos = chPos, clusPos = clusPos) 
        endfor 
        
    sideCenters[*,t]  = sideCenters[*,t]  / numObjetosActual
    groupCenters[*,t] = groupCenters[*,t] / numObjetosOriginal 

    if(t gt 0) then begin
      radioAct   = sideCenters[*,tPos] - groupCenters[*,tPos]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,tPos-1] - groupCenters[*,tPos-1]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)            
    endif
    obj_destroy, C_sROI3DGroupObjOriginal
    obj_destroy, C_sROI3DSideObjOriginal
   endfor
      
   ; Please kill me... only for selected Yoya variations
      radioAct   = sideCenters[*,2] - groupCenters[*,2]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,0] - groupCenters[*,0]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       

      radioAct   = sideCenters[*,3] - groupCenters[*,3]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,2] - groupCenters[*,2]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       
     
      radioAct   = sideCenters[*,5] - groupCenters[*,5]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,3] - groupCenters[*,3]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       

      radioAct   = sideCenters[*,7] - groupCenters[*,7]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,5] - groupCenters[*,5]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       
     
      radioAct   = sideCenters[*,9] - groupCenters[*,9]
      mRAct      = sqrt(total(radioAct*radioAct)) 
      radioAnt   = sideCenters[*,5] - groupCenters[*,5]
      mRAnt      = sqrt(total(radioAnt*radioAnt))
      if((mRAct - mRAnt) ge 0.0d) then cambioRadiosSign[1] = cambioRadiosSign[1] > (mRAct - mRAnt)  
      if((mRAct - mRAnt) lt 0.0d) then cambioRadiosSign[0] = cambioRadiosSign[0] < (mRAct - mRAnt)       
   ; killme end   
   
   indiceRef         = 5
   indiceRef         = (tPos-1 ge 0)? (tPos-1):0  ; Comment for specific times comparative

   miRadialActual    = sideCenters[*,tPos] - groupCenters[*,tPos]
   miRadialAnterior  = sideCenters[*,indiceRef] - groupCenters[*,indiceRef]
  
   mRActual      = sqrt(total(miRadialActual*miRadialActual)) 
   mRAnterior    = sqrt(total(miRadialAnterior*miRadialAnterior))
   
    switch 1b of
      (mRActual gt mRAnterior): begin
            agregado = ((mRActual - mRAnterior)/cambioRadiosSign[1])
            ;colorEsfera = [177.0d + agregado,agregado,agregado]
            colorEsfera = [255.0d,107.0d*(1.0d - agregado),107.0d*(1.0d - agregado)]
        break
      end
      (mRActual lt mRAnterior): begin
            agregado = ((mRActual - mRAnterior)/cambioRadiosSign[0]) 
            colorEsfera = [107.0d*(1.0d - agregado),107.0d*(1.0d - agregado),255.0d]
            ;colorEsfera = [27.0d*agregado,27.0d*agregado,177.0d + 27.0d*agregado]
            ;colorEsfera = [107,107,255]
        break
      end
      else: colorEsfera = [107,107,107] 
    endswitch     

   for i = 0, (oGroupReference->count())-1 do begin
     objNumber = (oGroupReference->get(position = i))->getNumber()

     oObjectModel->add, obj_new('IDLgrPolyline', data = vertices, poly = poligonos ,$
                                               ;ambient = [0,0,0], bottom = [0,0,0], color = [255,255,255], alpha_channel = 255, shininess = 128., shading = 1,$
                                               color = [17,17,17],$
                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                               name = strCompress('3DTorsionSphere:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition')

   endfor
end

PRO PlotCurveSet, z, x, y $
                  , FILL_COLORS = fillColors $
                  , VERTICAL_BARS = vertBars $
                  , PSYM = psym

dimZ = SIZE(z, /DIMENSIONS)
nx = dimZ[0]
ny = dimZ[1]

IF N_ELEMENTS(x) EQ 0 THEN x = FINDGEN(nx)
IF N_ELEMENTS(y) EQ 0 THEN y = FINDGEN(ny)
IF N_ELEMENTS(fillColors) EQ 0 THEN fillColors = LONARR(nx)


;;  Set up coordinate axes 

SURFACE, /NODATA, /SAVE, z, x, y


;;  Draw "sheets" from back to front

FOR ix = nx-1, 0, -1 DO BEGIN
    
    polyY = [y, REVERSE(y), y[0]]
    polyX = x[ix] + FLTARR(N_ELEMENTS(polyY))
    polyZ = [REFORM(z[ix,*]), FLTARR(ny), z[ix,0]]

    POLYFILL, polyX, polyY, polyZ, /T3D, COLOR = fillColors[ix]
    PLOTS, polyX, polyY, polyZ, /T3D, PSYM=psym
    
    IF KEYWORD_SET(vertBars) THEN FOR iy = 0, ny-1 DO $
      PLOTS, x[[ix,ix]], y[[iy, iy]], [0, z[ix,iy]], /T3D

ENDFOR

END
