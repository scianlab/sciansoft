
pro C_s3D_CPM_DFCs::Master_CPM_DFC_Creation, stack_tlb = stack_tlb, paramStruct = paramStruct,  oGroupReference = oGroupReference 
  self._myDummy3DObject = obj_new('C_sROI3DObject')

  self._stack_tlb     = stack_tlb
  self._pParamStruct   = ptr_new(paramStruct)
  self._pGroupReference = ptr_new(oGroupReference) 
  self._clusPos       = *(((paramStruct)).pValues)[(where(*(((paramStruct)).pNames) eq 'Cluster Position'))[0]]
  self._chPos         = *(((paramStruct)).pValues)[(where(*(((paramStruct)).pNames) eq 'Channel Position'))[0]]
  self._tPos          = *(((paramStruct)).pValues)[(where(*(((paramStruct)).pNames) eq 'Time Position'))[0]]
  
  self._dimXYZ = (*self._pGroupReference)->getxyzDim()

   widget_control, self._stack_tlb, get_uValue = stackState, /no_copy
     (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
   widget_control, self._stack_tlb, set_uValue = stackState, /no_copy
   self._path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
   self._modelName = 'CPM_DFCs'


  self._mode2D3D    = 0
  self._seedMC   = 10711L;
  self._seedPP   = 11117L;
  self._MCS      = 100000  
  self._maxStep  = 1000L
  
  self._H0          = 0.0d
  self._T           = 10.0d                       
  self._volT        = 1.0d
  self._surfT       = 1.0d
  self._J_DFC_DFC   = -3.0d
  self._J_DFC_EVL   = 1.0d                                
  self._lambdaVol   = 0.01d
  self._lambdaSurf  = 0.25d 
  self._factorUp    = 100.0d
  self._radioCoA    = 10.0d
  self._factorCoA   = 10.0d
  
  self->Create_CPM_WidgetParams ; widget for configuration...
  self->GenerateNodesSystem     
  ; self->GenerateArtificialNodesSystem
  self->RunSim
end

pro C_s3D_CPM_DFCs::GenerateArtificialNodesSystem
   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalTNum = totalTimes
   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalZNum = totalZimes
   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalChNum = totalChNum

   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()
    factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]
    xyzSPPNorm = (*((*self._pGroupReference)->getpEigenSys())).sizePerXYZ / max((*((*self._pGroupReference)->getpEigenSys())).sizePerXYZ)
    self._tiempos =  totalTimes
    self._maximoObjetos = 100

    self._volArea = ptr_new([0.0d],/no_copy)        
    self._SupPer  = ptr_new([0.0d],/no_copy) 
    
    if(self._mode2D3D eq 0) then begin
     dummy = make_array(self._dimXYZ[0],self._dimXYZ[1],/INTEGER, value = 0);
     self._latticeModell = ptr_new(dummy,/no_copy)
    endif else begin
       dummy = make_array(self._dimXYZ,/INTEGER, value = 0);
       self._latticeModell = ptr_new(dummy,/no_copy)
    endelse

   ; Create objects

; We can to initialize rendersystem
    self->InitRenderArea
    self->setVision, vVision = 5

    time = 0
    self->UpdateRenderArea, time = time
end

pro C_s3D_CPM_DFCs::GenerateNodesSystem
   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalTNum = totalTimes
   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalZNum = totalZimes
   s_ISM_getProjectInfo, stack_tlb = self._stack_tlb, totalChNum = totalChNum

   (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()
    factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]
    xyzSPPNorm = (*((*self._pGroupReference)->getpEigenSys())).sizePerXYZ / max((*((*self._pGroupReference)->getpEigenSys())).sizePerXYZ)

   self._tiempos =  totalTimes
   self._maximoObjetos = -1

    self._volArea = ptr_new([0.0d],/no_copy)        
    self._SupPer  = ptr_new([0.0d],/no_copy) 

    if(self._mode2D3D eq 0) then begin
     dummy = make_array(self._dimXYZ[0],self._dimXYZ[1],/INTEGER, value = 0);
    endif else begin
       dummy = make_array(self._dimXYZ,/INTEGER, value = 0);
    endelse

   C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = self._stack_tlb,tPos = self._tPos , chPos = self._chPos , clusPos = self._clusPos)   
   numObjetosActual = C_sROI3DGroupObjActual->count()
   self._numCells = 0
   division = 4.0
   
      massCenterXYO    = [0.0d,0.0d,0.0d]
      numCMXYO        = 0L
      massCenterXYR    = [0.0d,0.0d,0.0d]
      numCMXYR        = 0L
      
   for index = 0L, numObjetosActual-1L do begin
      pPoints = *((C_sROI3DGroupObjActual->get(position = index))->getpWherePoints())   
      if(pPoints[0] ne -1) then begin
          if(self._mode2D3D eq 0) then begin
;             nPoints = n_elements(pPoints)
;             xyPoints = intArr(2, nPoints)
;             xyPoints[0,*] = pPoints mod self._dimXYZ[0]
;             xyPoints[1,*] = floor( (pPoints mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
;
;             ; scale 1/2
;                minX = min(xyPoints[0,*], max = maxX)
;                minY = min(xyPoints[1,*], max = maxY)
;             xyPoints[0,*] = floor(xyPoints[0,*] / division) 
;             xyPoints[1,*] = floor(xyPoints[1,*] / division)
;
;                minXS = min(xyPoints[0,*], max = maxXS)
;                minYS = min(xyPoints[1,*], max = maxYS)
;
;             xyPoints[0,*] = floor(xyPoints[0,*] + 0.5*(maxX + minX) - minXS)
;             xyPoints[1,*] = floor(xyPoints[1,*] + 0.5*(maxY + minY) - minYS)   

             nPoints = n_elements(pPoints)
             xyPoints = intArr(2, nPoints)
             xyPoints[0,*] = pPoints mod self._dimXYZ[0]
             xyPoints[1,*] = floor( (pPoints mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))

                massCenterXYO[0] = TOTAL(xyPoints[0,*]) + massCenterXYO[0]
                massCenterXYO[1] = TOTAL(xyPoints[1,*]) + massCenterXYO[1]
                numCMXYO         = numCMXYO + nPoints

                
             ; scale 1/2
             xyPoints[0,*] = floor(xyPoints[0,*] / division) 
             xyPoints[1,*] = floor(xyPoints[1,*] / division)

                massCenterXYR[0] = TOTAL(xyPoints[0,*]) + massCenterXYR[0]
                massCenterXYR[1] = TOTAL(xyPoints[1,*]) + massCenterXYR[1]
                
             ;xyPoints[0,*] = floor(xyPoints[0,*] + 0.5*(maxX + minX) - minXS)
             ;xyPoints[1,*] = floor(xyPoints[1,*] + 0.5*(maxY + minY) - minYS)   
             
             dummy[xyPoints[0,*], xyPoints[1,*]] = self._numCells + 1
          endif else begin
             dummy[pPoints] = self._numCells + 1
          endelse
          self._numCells = self._numCells + 1          
      endif
   endfor

   if(self._mode2D3D eq 0) then begin
      massCenterXYO    = massCenterXYO / numCMXYO
      massCenterXYR    = massCenterXYR / numCMXYO
      dummy = SHIFT(dummy, massCenterXYO[0] - massCenterXYR[0], massCenterXYO[1] - massCenterXYR[1])
   endif


   ;;dummy = shift(dummy,self._dimXYZ[0]/4,self._dimXYZ[1]/4,self._dimXYZ[2]/4)
   self._latticeModell = ptr_new(dummy,/no_copy)
   
   ; create volume surface data
   volP = 0.0d
   surfP = 0.0d
   actualNumCell = 0
   for index = 0L, self._numCells-1L do begin
       pPoints = where((*self._latticeModell) eq (actualNumCell + 1),/L64)
       if(pPoints[0] ne -1) then begin
           nPoints = n_elements(pPoints)
           
           minPoints = 30*30/division
           if(self._mode2D3D eq 1) then minPoints = 30*30*7/division
           if(nPoints lt minPoints) then begin
              (*self._latticeModell)[pPoints] = 0

               pPoints = where((*self._latticeModell) gt (actualNumCell + 1),/L64)
               if(pPoints[0] ne -1) then begin
                  (*self._latticeModell)[pPoints] = (*self._latticeModell)[pPoints] - 1
               endif 
           endif else begin
                 xyzPoints = intArr(3, nPoints)
                 xyzPoints[0,*] = pPoints mod self._dimXYZ[0]
                 xyzPoints[1,*] = floor( (pPoints mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
                 if(self._mode2D3D eq 0) then begin
                    xyzPoints[2,*] = 0
                 endif else begin
                    xyzPoints[2,*] = floor( pPoints / (1. * self._dimXYZ[0] * self._dimXYZ[1]))
                 endelse
               volumen = self->makeMiniVol(xyzPoints = xyzPoints, borde = 5)
        
               dummySurf = self->getSurface( volumen = volumen)
               self._volArea = ptr_new([*self._volArea,nPoints*1.0d],/no_copy)        
               self._SupPer  = ptr_new([*self._SupPer ,dummySurf*1.0d],/no_copy) ; not useful
               volP  = volP +  nPoints*1.0d
               surfP = surfP + dummySurf*1.0d
               actualNumCell = actualNumCell + 1
           endelse
       endif else begin
           pPoints = where((*self._latticeModell) gt (actualNumCell + 1),/L64)
           if(pPoints[0] ne -1) then begin
              (*self._latticeModell)[pPoints] = (*self._latticeModell)[pPoints] - 1 
           endif
       endelse
   endfor
   self._numCells = actualNumCell
   self._volT =  mean((*self._volArea)[1:*]) ;volP / self._numCells
   self._surfT = mean((*self._SupPer)[1:*]) ;surfP / self._numCells
   
   obj_destroy,C_sROI3DGroupObjActual   


; We can to initialize rendersystem
    self->InitRenderArea
    self->setVision, vVision = 5
    
    time = 0
    self->UpdateRenderArea, time = time
end

function C_s3D_CPM_DFCs::getSurface, volumen = volumen
  if(self._mode2D3D eq 0) then begin
    str_element = MAKE_ARRAY(3,3,/INTEGER,value = 1)  
  endif else begin
    str_element = MAKE_ARRAY(3,3,3,/INTEGER,value = 1)
  endelse
  binaria = CONVOL(volumen, str_element)
  if(self._mode2D3D eq 0) then begin  
    wPoints = where((binaria ne 0) and (binaria ne 9) and (volumen ne 0),/L64)
  endif else begin
    wPoints = where((binaria ne 0) and (binaria ne 27) and (volumen ne 0),/L64)
  endelse
  binaria = 0
  if(wPoints[0] eq -1) then return, 0
  return, N_ELEMENTS(wPoints)
end


function C_s3D_CPM_DFCs::makeMiniVol,$; Returns 2D/3D-Object = 1 in voxel (2*pixelFrame + dim_xyz)
                         xyzPoints = xyzPoints,$; xyzPoints: xyzPoints Pixel Position of Object in Base-Frame
                         borde = borde,$
                         all = all

   minX = min(xyzPoints[0,*], max = maxX)
   minY = min(xyzPoints[1,*], max = maxY)
   minZ = min(xyzPoints[2,*], max = maxZ)

   ;obj = make_array(maxX - minX + 1 + 2*borde, maxY - minY + 1 + 2*borde, maxZ - minZ + 1 + 2*borde, /INTEGER)
   obj = make_array(maxX - minX + 1 + 2*borde, maxY - minY + 1 + 2*borde, /INTEGER)
   ;obj[xyzPoints[0,*] - (minX+borde), xyzPoints[1,*] - (minY+borde), xyzPoints[2,*] - (minZ+borde)] = 1b
   obj[xyzPoints[0,*] - minX + borde, xyzPoints[1,*] - minY + borde] = 1b

   if (n_elements(all) gt 0) then return, {obj:obj,$
                                           borde: borde,$
                                           minX:minX,$
                                           minY:minY,$
                                           minZ:minZ,$
                                           maxX:maxX,$
                                           maxY:maxY,$
                                           maxZ:maxZ} $
      else return, obj
end

pro C_s3D_CPM_DFCs::RunSim
  bRestart = 0
  _actualStep = 0L

  if(bRestart) then begin
    ; initialate all...
    bArtificial = 0
    if(bArtificial) then begin
        self->GenerateArtificialNodesSystem
    endif else begin
        self->GenerateNodesSystem
    endelse
    self->StartDiffusion     
    _actualStep = 0L
    
    seedMC = self._seedMC
    seedPP = self._seedPP
  endif
  
  while(_actualStep lt   self._maxStep) do begin
    ;self->AdvanceStepProfiles
    ;self->AdvanceStepDiffusion
    self->AdvanceStepCPM, seedMC = seedMC, seedPP = seedPP
    
    
    ;self->UpdateRenderArea, time = _actualStep
            self->RenderCurrentState, mode = 1
          
            o1 = self._win
            o2 = self._oClipboard
            o3 = self._oScene
            s_SaveViewforModel,win = o1,oClipboard = o2, oScene = o3, vPath = self._path, time = _actualStep, modelName = self._modelName
            self._win = o1
            self._oClipboard = o2
            self._oScene = o3
            self._model->remove, /all      
    _actualStep++
    ; if(condicionPausa o FIN) then _maxStep = -1L 
  endwhile
end

pro C_s3D_CPM_DFCs::AdvanceStepCPM, seedMC = seedMC, seedPP = seedPP
  
  ; Randomly .. (to verify optimal IDL RAND functions.. for now.. the basic)
  
  ; Apply MCS iterations for modified Boltzman-Metropolos approach
  ; for fast access and control of no valid attemps... extract only valid components
;  dummy = ERODE( *self._latticeModell, str_element, 1,1,1)
;  pPointsToKeepE = where(dummy ne (*self._latticeModell) ,/L64)
;  dummy = 0
;  
;  dummy = DILATE( *self._latticeModell, str_element, 1,1,1)
;  pPointsToKeepD = where(dummy ne (*self._latticeModell) ,/L64)
;  dummy = 0
;
;      if(  (pPointsToKeepE[0] ne -1) and (pPointsToKeepD[0] ne -1)) then begin
;        wNodesOK = [pPointsToKeepD,pPointsToKeepE]
;      endif else begin
;         if(pPointsToKeepE[0] ne -1) then begin
;            wNodesOK = [pPointsToKeepE]
;         endif
;         if(pPointsToKeepD[0] ne -1) then begin
;            wNodesOK = [pPointsToKeepD]
;         endif
;      endelse
;
;      if(wNodesOK[0] eq -1) then return
;  numElements = N_ELEMENTS(wNodesOK)
  wPoints = self->getActiveBordeForAll()
  
  if(wPoints[0] eq -1) then return
  numElements = N_ELEMENTS(wPoints)

  ; update volumen area information
;    self._seedPP = 11117L;
  for iTry = 0L, self._MCS-1L do begin
     indexRelativo  = floor(randomu(seedMC)* (numElements))
     indiceOriginal = wPoints[indexRelativo]
     
     pXYZOr = self->getXYZ_Index( index = indiceOriginal)
     
     x  = floor(randomu(seedMC)* 3) - 1
     y  = floor(randomu(seedMC)* 3) - 1
     z  = floor(randomu(seedMC)* 3) - 1
     
     indiceDestino = self->getIndexByXYZ( xyz = pXYZOr + [x,y,z])     
     
     ID_origen   = (*self._latticeModell)[indiceOriginal]
     ID_consulta = (*self._latticeModell)[indiceDestino]
     
     ; Calculate Hamiltonian and try to change
     if(ID_origen ne ID_consulta) then begin
        self->CPM_Hamiltonian_Change, indexOrigen = indiceOriginal, indexConsulta = indiceDestino, seed = seed
     endif     
  endfor  
  wPoints = 0

   ;dummy = shift(*self._latticeModell,0,10,0)
   ;*self._latticeModell = 0
   ;PTR_FREE, self._latticeModell
   ;self._latticeModell = ptr_new(dummy,/no_copy)
end

function C_s3D_CPM_DFCs::getActiveBordeForAll
  if(self._mode2D3D eq 0) then begin
    str_element = MAKE_ARRAY(3,3,/INTEGER,value = 1)
     binaria = make_array(self._dimXYZ[0],self._dimXYZ[1],/INTEGER, value = 0);      
  endif else begin
    str_element = MAKE_ARRAY(3,3,3,/INTEGER,value = 1)
    binaria = make_array(self._dimXYZ,/INTEGER, value = 0);
  endelse

;  wPointsGlobal = where(*self._latticeModell ne 0,/L64)
;  if(wPointsGlobal[0] eq -1) then return, wPointsGlobal 
;  binaria[wPointsGlobal] = 1
;  
;  binaria = CONVOL(binaria, str_element)
;  if(self._mode2D3D eq 0) then begin
;    wPointsGlobal = where((binaria ne 0) and (binaria ne 9),/L64)
;  endif else begin
;    wPointsGlobal = where((binaria ne 0) and (binaria ne 27),/L64)
;  endelse
;  binaria = 0
;  return, wPointsGlobal 
;;  
  wPointsGlobal = [-1]
   for index = 0L, self._numCells-1L do begin
       pPoints = where((*self._latticeModell) eq (index + 1),/L64)
       nPoints = n_elements(pPoints)
       
       if(nPoints lt 30) then continue
         xyzPoints = intArr(3, nPoints)
         xyzPoints[0,*] = pPoints mod self._dimXYZ[0]
         xyzPoints[1,*] = floor( (pPoints mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
         if(self._mode2D3D eq 0) then begin
            xyzPoints[2,*] = 0
         endif else begin
            xyzPoints[2,*] = floor( pPoints / (1. * self._dimXYZ[0] * self._dimXYZ[1]))
         endelse
         
         
         volumen = self->makeMiniVol(xyzPoints = xyzPoints,borde = 5, all = 1)

         binaria = CONVOL(volumen.obj, str_element)
         
          if(self._mode2D3D eq 0) then begin
            wPoints = where((binaria ne 0) and (binaria ne 9),/L64)
          endif else begin
            wPoints = where((binaria ne 0) and (binaria ne 27),/L64)
          endelse         
         
         if(wPoints[0] ne -1) then begin
             xyzPointsR = intArr(3, N_ELEMENTS(wPoints))
             xyzPointsR[0,*] = wPoints mod N_ELEMENTS(binaria[*,0]) 
             xyzPointsR[1,*] = floor( (wPoints mod (N_ELEMENTS(binaria[*,0]) * N_ELEMENTS(binaria[0,*]))) / (1.*N_ELEMENTS(binaria[*,0])))
             if(self._mode2D3D eq 0) then begin
                xyzPointsR[2,*] = 0
             endif else begin
                xyzPointsR[2,*] = floor( wPoints / (1. * N_ELEMENTS(binaria[*,0]) * N_ELEMENTS(binaria[0,*])))
             endelse
             
             xyzPointsR[0,*] = xyzPointsR[0,*] + volumen.minX - volumen.borde
             xyzPointsR[1,*] = xyzPointsR[1,*] + volumen.minY - volumen.borde
             ;xyzPointsR[2,*] = xyzPointsR[2,*] + volumen.minZ - volumen.borde
             
             dummyy  = xyzPointsR[0,*] + xyzPointsR[1,*]*self._dimXYZ[0] + ((self._mode2D3D eq 0)? 0 : (xyzPointsR[2,*]* self._dimXYZ[0] * self._dimXYZ[1] ))
             wPoints = transpose(dummyy)
         endif

         wPointsGlobal = [wPointsGlobal, wPoints]
         volumen = 0
         binaria = 0
   endfor
   
   wPoints = where(wPointsGlobal ne -1,/L64)
   if(wPoints[0] eq -1) then return, [-1]
  return, wPointsGlobal[wPoints]

end

function  C_s3D_CPM_DFCs::getIndexByXYZ, xyz = xyz
  return, xyz[0] + xyz[1]*self._dimXYZ[0] + ((self._mode2D3D eq 0)? 0 : (xyz[2]* self._dimXYZ[0] * self._dimXYZ[1] ))
end

function  C_s3D_CPM_DFCs::getXYZ_Index, index = index
   xyzPoint = intArr(3)
   xyzPoint[0] = index mod self._dimXYZ[0]
   xyzPoint[1] = floor( (index mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
   if(self._mode2D3D eq 0) then begin
      xyzPoint[2] = 0
   endif else begin
      xyzPoint[2] = floor( index / (1. * self._dimXYZ[0] * self._dimXYZ[1]))
   endelse
   
   return, xyzPoint
end

pro C_s3D_CPM_DFCs::CPM_Hamiltonian_Change, indexOrigen = indexOrigen, indexConsulta = indexConsulta, seed = seed 
; tipo = 0 ... 2D
; tipo = 1 .... 3D
   xyzPointOrigen = intArr(3)
   xyzPointOrigen[0] = indexOrigen mod self._dimXYZ[0]
   xyzPointOrigen[1] = floor( (indexOrigen mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
   if(self._mode2D3D eq 0) then begin
      xyzPointOrigen[2] = 0
   endif else begin
      xyzPointOrigen[2] = floor( indexOrigen / (1. * self._dimXYZ[0] * self._dimXYZ[1]))
   endelse
   xyzPointConsulta = intArr(3)
   xyzPointConsulta[0] = indexConsulta mod self._dimXYZ[0]
   xyzPointConsulta[1] = floor( (indexConsulta mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
   if(self._mode2D3D eq 0) then begin
      xyzPointConsulta[2] = 0
   endif else begin
      xyzPointConsulta[2] = floor( indexConsulta / (1. * self._dimXYZ[0] * self._dimXYZ[1]))
   endelse

  IDOrigen    = (*self._latticeModell)[indexOrigen]
  ID_consulta = (*self._latticeModell)[indexConsulta]
  
  ; calc DFCs Adhesion Efects -- second level neighbors
  minX = ((xyzPointOrigen[0] - 2) ge 0)? (xyzPointOrigen[0] - 2) : 0 
  maxX = ((xyzPointOrigen[0] + 2) lt self._dimXYZ[0])? (xyzPointOrigen[0] + 2) : (self._dimXYZ[0] - 1)
  minY = ((xyzPointOrigen[1] - 2) ge 0)? (xyzPointOrigen[1] - 2) : 0
  maxY = ((xyzPointOrigen[1] + 2) lt self._dimXYZ[1])? (xyzPointOrigen[1] + 2) : (self._dimXYZ[1] - 1)
  minZ = ((xyzPointOrigen[2] - 2) ge 0)? (xyzPointOrigen[2] - 2) : 0
  if(self._mode2D3D eq 0) then begin
    maxZ = 0 
  endif else begin
    maxZ = ((xyzPointOrigen[2] + 2) lt self._dimXYZ[2])? (xyzPointOrigen[2] + 2) : (self._dimXYZ[2] - 1)
  endelse
  
  JconjuntoOrigen = 0.0d
  JconjuntoConsulta = 0.0d
  for i = 0, (maxX - minX) do begin
    for j = 0, (maxY - minY) do begin
      for k = 0, (maxZ - minZ) do begin
        posX = minX + i
        posY = minY + j
        posZ = minZ + k
        if((posX ne xyzPointOrigen[0]) or (posY ne xyzPointOrigen[1]) or (posZ ne xyzPointOrigen[2])) then begin
          idTest = (*self._latticeModell)[posX,posY,posZ]
          jInteraccionActualO = self->getJ(id1 = IDOrigen, id2 = idTest)
          jInteraccionActualC = self->getJ(id1 = ID_consulta, id2 = idTest)
          
          JconjuntoOrigen   = JconjuntoOrigen + jInteraccionActualO
          JconjuntoConsulta = JconjuntoConsulta +  jInteraccionActualC
        endif
      endfor
    endfor
  endfor
  
  ; calc volume preservation factor
  factorVolumenO = (*self._volArea - self._volT)
  factorVolumenO[0] = 0.0d
  factorVolumenO = self._lambdaVol * total(factorVolumenO*factorVolumenO)
  
  factorSurfaceO = (*self._SupPer - self._surfT)
  factorSurfaceO[0] = 0.0d
  factorSurfaceO = self._lambdaSurf * total(factorSurfaceO*factorSurfaceO)


  dummyV = *self._volArea
  if(ID_consulta ne 0) then dummyV[ID_consulta] = dummyV[ID_consulta] + 1
  if(IDOrigen ne 0)  then dummyV[IDOrigen ] = dummyV[IDOrigen ] - 1
  factorVolumenC = (dummyV - self._volT)
  factorVolumenC[0] = 0.0d
  factorVolumenC = self._lambdaVol * total(factorVolumenC*factorVolumenC)
  

  ; temporal attemp
  (*self._latticeModell)[indexOrigen] = ID_consulta
  
  dummyS = *self._SupPer
  if(ID_consulta ne 0) then begin
       pPoints = where((*self._latticeModell) eq (ID_consulta),/L64)
       nPoints = n_elements(pPoints)

         xyzPoints = intArr(3, nPoints)
         xyzPoints[0,*] = pPoints mod self._dimXYZ[0]
         xyzPoints[1,*] = floor( (pPoints mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
         if(self._mode2D3D eq 0) then begin
            xyzPoints[2,*] = 0
         endif else begin
            xyzPoints[2,*] = floor( pPoints / (1. * self._dimXYZ[0] * self._dimXYZ[1]))
         endelse
       volumen = self->makeMiniVol(xyzPoints = xyzPoints, borde = 5)

       dummyS[ID_consulta] = self->getSurface( volumen = volumen)
  endif 
  if(IDOrigen ne 0)  then begin
       pPoints = where((*self._latticeModell) eq (IDOrigen),/L64)
       nPoints = n_elements(pPoints)

         xyzPoints = intArr(3, nPoints)
         xyzPoints[0,*] = pPoints mod self._dimXYZ[0]
         xyzPoints[1,*] = floor( (pPoints mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
         if(self._mode2D3D eq 0) then begin
            xyzPoints[2,*] = 0
         endif else begin
            xyzPoints[2,*] = floor( pPoints / (1. * self._dimXYZ[0] * self._dimXYZ[1]))
         endelse
       volumen = self->makeMiniVol(xyzPoints = xyzPoints, borde = 5)

       dummyS[IDOrigen] = self->getSurface( volumen = volumen)

  endif 
  factorSurfaceC = (dummyS - self._surfT)
  factorSurfaceC[0] = 0.0d
  factorSurfaceC = self._lambdaSurf * total(factorSurfaceC*factorSurfaceC)

  (*self._latticeModell)[indexOrigen] = IDOrigen
  
  ; we have all information
;  hOriginal = JconjuntoOrigen + factorVolumenO + factorSurfaceO
;  hConsulta = JconjuntoConsulta  + factorVolumenC + factorSurfaceC
  hOriginal = factorVolumenO + factorSurfaceO
  hConsulta = factorVolumenC + factorSurfaceC

  deltaH    = hConsulta - hOriginal
  ; add diffusions
  ; :_(
  ; deltaDifDFCs = uDFC(diff[indexActual] - diff[indexConsulta])
  ; deltaDifDBs = uDDB(diff[indexActual] - diff[indexConsulta])
  
  ;deltaHFull = deltaH; + deltaDifDFCs + deltaDifDBs
  ;deltaHFull = deltaH + deltaDifDFCs + deltaDifDBs
  ; effect DBS
  deltaDBS = -self._factorUp * ( xyzPointOrigen[1] - xyzPointConsulta[1])  
  ; effect EVL
  deltaEVL = 0.0d
  ; effect CoA .... radio Rectangular
  deltaCoA= 0.0d
      ; calc DFCs Adhesion Efects -- second level neighbors
      minX = ((xyzPointOrigen[0] - self._radioCoA) ge 0)? (xyzPointOrigen[0] - self._radioCoA) : 0 
      maxX = ((xyzPointOrigen[0] + self._radioCoA) lt self._dimXYZ[0])? (xyzPointOrigen[0] + self._radioCoA) : (self._dimXYZ[0] - 1)
      minY = ((xyzPointOrigen[1] - self._radioCoA) ge 0)? (xyzPointOrigen[1] - self._radioCoA) : 0
      maxY = ((xyzPointOrigen[1] + self._radioCoA) lt self._dimXYZ[1])? (xyzPointOrigen[1] + self._radioCoA) : (self._dimXYZ[1] - 1)
      minZ = ((xyzPointOrigen[2] - self._radioCoA) ge 0)? (xyzPointOrigen[2] - self._radioCoA) : 0
      if(self._mode2D3D eq 0) then begin
        maxZ = 0 
      endif else begin
        maxZ = ((xyzPointOrigen[2] + self._radioCoA) lt self._dimXYZ[2])? (xyzPointOrigen[2] + self._radioCoA) : (self._dimXYZ[2] - 1)
      endelse

      (*self._latticeModell)[indexOrigen]   = IDOrigen
      cajitaOrigen = (*self._latticeModell)[minX:maxX,minY:maxY,minZ:maxZ]
      wCoA         = where(cajitaOrigen ne 0,/L64)
      massCenterXYZ    = [0.0d,0.0d,0.0d]
      massCenterXYZ[0] = TOTAL(cajitaOrigen[0,*])
      massCenterXYZ[1] = TOTAL(cajitaOrigen[1,*])
      massCenterXYZ[2] = TOTAL(cajitaOrigen[2,*])
      massCenterXYZ    = massCenterXYZ / N_ELEMENTS(wCoA)
      massCenterXYZ    = massCenterXYZ + [minX,minY,minZ]
      
      distanciaOriginal = sqrt(total((massCenterXYZ - xyzPointOrigen)*(massCenterXYZ - xyzPointOrigen)))
      distanciaConsulta = sqrt(total((massCenterXYZ - xyzPointConsulta)*(massCenterXYZ - xyzPointConsulta)))
      deltaCoA = self._factorCoA * (distanciaConsulta - distanciaOriginal)  

  ; Full sim hamiltonian
  ;deltaHFull = deltaH + deltaDBS + deltaEVL + deltaCoA
  deltaHFull = deltaH 
  ;deltaHFull = deltaDBS
  ; try to change 
  (*self._latticeModell)[indexOrigen]   = IDOrigen
  bApply = 0b
  if(deltaHFull le (- self._H0)) then begin
      bApply = 1b
   endif else begin
      pDeltaH = EXP(-(deltaHFull - self._H0)/ self._T)
      if(randomu(seed) le pDeltaH) then bApply = 1b 
   endelse
   
   if(bApply eq 1b) then begin
      (*self._latticeModell)[indexOrigen] = ID_consulta
      
      *self._volArea = 0.0d
      *self._SupPer  = 0.0d
      PTR_FREE, self._volArea 
      PTR_FREE, self._SupPer
      
      self._volArea = PTR_NEW( dummyV  , /NO_COPY)
      self._SupPer = PTR_NEW( dummyS  , /NO_COPY)
   endif
end

function C_s3D_CPM_DFCs::getJ, id1 = id1, id2 = id2
  if((id1 eq 0) or (id2 eq 0)) then return, 10.0d      ; interaction with vaccium
  return, self._J_DFC_DFC
end

pro C_s3D_CPM_DFCs::CalcDivision, vIndice = vIndice
end

pro C_s3D_CPM_DFCs::InitRenderArea
  s_WorldViewforModel, xdim = self._dimXYZ[0], ydim = self._dimXYZ[1] ,win =win,oClipboard = oClip, oScene = oSccene, oModel = self._model, oStaticInputModel = self._staticInputModel
  self._win = win
  self._oClipboard = oClip
  self._oScene = oSccene
 ; self._staticInputModel->remove, /all
end

pro C_s3D_CPM_DFCs::UpdateRenderArea, time = time
  self->RenderCurrentState, mode = 1

  o1 = self._win
  o2 = self._oClipboard
  o3 = self._oScene
  s_SaveViewforModel,win = o1,oClipboard = o2, oScene = o3, vPath = self._path, time = time, modelName = self._modelName
  self._win = o1
  self._oClipboard = o2
  self._oScene = o3
  self._model->remove, /all  
end
pro C_s3D_CPM_DFCs::RenderCurrentState,mode = mode
 (*self._pGroupReference)->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  xyzSizePerPixel = (*self._pGroupReference)->getxyzSizePerPixel()
  factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]
  xyzSPPNorm = (*((*self._pGroupReference)->getpEigenSys())).sizePerXYZ / max((*((*self._pGroupReference)->getpEigenSys())).sizePerXYZ)

   nObj = self._numCells
   for i = 1, nObj-1 do begin
             pPoints = where( (*self._latticeModell) eq (i+1),/L64) 
      if(pPoints[0] eq -1) then continue
             nPoints = n_elements(pPoints)
             xyzPoints = intArr(3, nPoints)
             xyzPoints[0,*] = pPoints mod self._dimXYZ[0]
             xyzPoints[1,*] = floor( (pPoints mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))
             if(self._mode2D3D eq 0) then begin
                xyzPoints[2,*] = 0
             endif else begin
                xyzPoints[2,*] = floor( pPoints / (1. * self._dimXYZ[0] * self._dimXYZ[1]))
             endelse
      
      obj = self._myDummy3DObject->makePixelObjectInVoxel(xyzPoints = xyzPoints,/all)
      shade_volume, obj.obj, 0, vertices, polygons, /low

      vertices[0,*] += (obj.minX - obj.pixelFrame)
      vertices[1,*] += (obj.minY - obj.pixelFrame)
      vertices[2,*] += (obj.minZ - obj.pixelFrame)

        col=[0,255,0]
        colorS = s_hist_white(col,nObj) 
        ;colorS = s_hist_black(col,nObj) 
        ;colorS = s_without_degrade(col,nObj)
        color =  colorS[*,i]
      ;color = [0,255,0]
      alpha = 1.0

      objNumber = i -1
      name = '3D_CPM_DFC_Model:'
      name = name+string(objNumber)


      if(mode eq 0) then begin
        self._staticInputModel->add,  obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, shading = 1, shininess = 128.,$
                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress(name,/rem), uvalue = 'ObjInOriginalPosition')
        
      endif else begin
        self._model->add,  obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, shading = 1, shininess = 128.,$
                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress(name,/rem), uvalue = 'ObjInOriginalPosition')
      endelse
      vertices = 0
      polygons = 0
      obj.obj  = 0
   endfor

   pPoints = 0 
   xyzPoints = 0
   
;
;      wPos = self->getActiveBordeForAll()   
;      imageRGB = MAKE_ARRAY(3,self._dimXYZ[0],self._dimXYZ[1],/BYTE,VALUE = 0)
;      ; :_( imageRGB(*,0:xyzDim[0]-1,0:xyzDim[1]-1) = [rOrigen[image(0:xyzDim[0]-1,0:xyzDim[1]-1)],gOrigen[image(0:xyzDim[0]-1,0:xyzDim[1]-1)],bOrigen[image(0:xyzDim[0]-1,0:xyzDim[1]-1)]]
;      ;for vX = 0, self._dimXYZ[0]-1 do begin
;      ;  for vY = 0, self._dimXYZ[1]-1 do begin
;      ;    imageRGB[*,vX,vY] = [255,0,0]      
;      ;  endfor
;      ;endfor
;      nPoints = N_ELEMENTS(wPos)
;        xyPoints = intArr(2, nPoints)
;        xyPoints[0,*] = wPos mod self._dimXYZ[0]
;        xyPoints[1,*] = floor( (wPos mod (self._dimXYZ[0] * self._dimXYZ[1])) / (1.*self._dimXYZ[0]))      
;      imageRGB[0,xyPoints[0,*],xyPoints[1,*]] = 255
;      
;      self._model->add, obj_new('IDLgrImage',imageRGB, GREYSCALE  = 0,xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, TRANSFORM_MODE = 1, LOCATION = [0,0,100])
;   
end

function C_s3D_CPM_DFCs::NewIDLgrPolygon, name = name, vertices = vectices, polygons = polygons, color = color,alpha = alpha 
;   return, obj_new('IDLgrPolygon', data = vertices, poly = polygons, color = color,$
;                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress(name,/rem), uvalue = 'ObjInOriginalPosition')

;
;   return, obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, shading = 1, shininess = 128.,$
;                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress(name,/rem), uvalue = 'ObjInOriginalPosition')
end

pro C_s3D_CPM_DFCs::cleanup
  if(OBJ_VALID(self._staticInputModel)) then self._staticInputModel->remove, /all
  if(OBJ_VALID(self._model)) then self._model->remove, /all

  OBJ_DESTROY, self._staticInputModel
  OBJ_DESTROY, self._model
end

pro C_s3D_CPM_DFCs::setVision, vVision = vVision
  self._vision = vVision
end

function C_s3D_CPM_DFCs::init
    ; Parameters of C_s3D_CPM_DFCs.
    self._staticInputModel = obj_new('IDLgrModel', uValue = 'oPhaseModel')    
    self._model = obj_new('IDLgrModel', uValue = 'oPhaseModel')
    
    return, 1
end

pro C_s3D_CPM_DFCs::Create_CPM_WidgetParams

  filterStruct = {Name: 'C_CPM_Params',$             ; param Name.
                  pWidgetType: ptr_new(),$           ; Pointer on Filter Parameter Names.
                  pNames: ptr_new(),$                ; Pointer on Filter Parameter Names.
                  pImagefilterParamType: ptr_new(),$ ; Pointer on Filter Parameter Type (Bool, Byte, Int, Long, Float, Double).
                  pActive: ptr_new(),$               ; Pointer on Filter Parameter Active Bool.
                  pMin: ptr_new(),$                  ; Pointer on Filter Parameter Min_Values.
                  pMax: ptr_new(),$                  ; Pointer on Filter Parameter Max_Values.
                  pValues: ptr_new()}                ; Pointer on Filter Parameter Values.

  filterParamNames = ['MCS',$        ; step of MonteCarlo iteration .. for metropolis alg
                      'deadTime',$   ; If each sim step is very fast to visualization XD
                      'Save Params',$ ;
                      'Load Params'$ ;
                      ]

  ; Widget for parameters setting. The number of elements must match the size of the following parameter-feature arrays.
  filterParamWidgetType = make_array(N_ELEMENTS(filterParamNames), /string, value = 'widget_slider')

  filterParamType = ['Long',$
                     'Int',$
                     'Int',$
                     'Int']

  filterParamActive = [1,$
                       1,$
                       1,$
                       1$
                       ]

  filterParamMin = [1,$
                    1,$
                    0,$
                    0]

  filterParamMax = [10000L,$
                    10000,$
                    1,$
                    1]

  filterParamValues = [10,$
                       1,$
                       0,$
                       0]

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
  filterStruct.pImagefilterParamType = ptr_new(filterParamType, /no_copy)
  filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

  self._modellpParamStructure = ptr_new(filterStruct, /no_copy)

  a = self._modellpParamStructure
    wTopBase = widget_base(Title = (*a).Name+' ->', /column,$
                           ;Group_Leader = 0,$
                           scr_ysize = ( n_elements((*(*a).pWidgetType))*150 < 300  ),$
                           y_scroll_size = ( n_elements((*(*a).pWidgetType))*150 < 300  )-50,$
                          tlb_size_events = 1,$
                          /frame)

    pwIDParamNumList = intArr(n_elements((*(*a).pWidgetType)), 2)
    for i = 0, n_elements((*(*a).pWidgetType))-1 do begin

       if ((*(*a).pWidgetType)[i] eq 'widget_slider') then begin
          wSubBase = widget_base(wTopBase, /column, /frame, event_pro = 's_CPM_event')
          wBGroup = cw_bgroup(wSubBase, (*(*a).pNames)[i], /nonexclusive)
          if ( (*(*a).pValues)[i] gt (*(*a).pMax)[i]) then (*(*a).pMax)[i] = (*(*a).pValues)[i]
          if ( (*(*a).pValues)[i] lt (*(*a).pMin)[i]) then (*(*a).pMin)[i] = (*(*a).pValues)[i]

          wfMin = (*(*a).pMin)[i]
          wfMax = (*(*a).pMax)[i]
          if (*(*a).pMax)[i] eq (*(*a).pMin)[i] then wfMax = wfMax + 1

          wFSlider = cw_fslider(wSubBase, maximum = wfMax,$
                                     minimum = wfMin,$
                                     value = (*(*a).pValues)[i],$
                                     edit = 1,$
                                     uValue = {paramNum: i, paramType: 'widget_slider'})
          widget_control, wFSlider, sensitive = (*((*a).pActive))[i]
          widget_control, wBGroup, set_value = (*(*a).pActive)[i], set_uvalue = {paramNum: i, paramType: 'boolOnOff', paramSubWidID: wFSlider}
          pwIDParamNumList[i,0] = wFSlider
          pwIDParamNumList[i,1] = wBGroup
       endif

    endfor

   paramTableUValue = {groupLeader:0,$          ; widget Group LeaderID
                           sTopBaseTitle:'Widget CPM Params',$
                           callName:'s_CPM_Param',$  ; this widget Pro Name
                           pwIDParamNumList: ptr_new(),$  ; pointer to corellate created widget ID's and Parameter Numbers
                           wTopBaseID:-1,$
                           ;oContainer:oParamContainer,$ ; Filter Container Object
                           ;Index:selectParamIndex,$      ; Index of Filter Object in Filter Container Object
                           ;subParamIndex:ev.index,$      ; Index of Filter Object in Filter Container Object
                           pClassParams: ptr_new(),$
                           wTableID:-1}

    self._paramTableUValue = ptr_new(paramTableUValue, /no_copy)
    (*self._paramTableUValue).pwIDParamNumList = ptr_new(pwIDParamNumList, /no_copy)
    (*self._paramTableUValue).wTopBaseID = wTopBase
    (*self._paramTableUValue).pClassParams = a
    
    self._topWidgetParamBase = wTopBase    
    widget_control, wTopBase, set_uvalue = *(self._paramTableUValue), /realize
    XManager, 's_ROIM_PW_Resize', wTopBase, /no_block
end

pro s_CPM_event, ev
    widget_control, ev.top, get_uvalue = paramTableUValue
    widget_control, ev.id, get_uvalue = eventUValue

    a = (paramTableUValue).pClassParams
    for i = 0, n_elements((*(*a).pWidgetType))-1 do begin
       if ((*(*a).pWidgetType)[i] eq 'widget_slider') then begin
          ;if ( (*(*a).pValues)[i] gt (*(*a).pMax)[i]) then (*(*a).pMax)[i] = (*(*a).pValues)[i]
          ;if ( (*(*a).pValues)[i] lt (*(*a).pMin)[i]) then (*(*a).pMin)[i] = (*(*a).pValues)[i]

          widget_control, 1l*(*paramTableUValue.pwIDParamNumList)[i,0],$
                                        get_value = value
          widget_control, 1l*(*paramTableUValue.pwIDParamNumList)[i,1],$
                                        get_value = bActivo
           
          if ( value lt (*(*a).pMin)[i]) then value = (*(*a).pMin)[i]
          if ( value gt (*(*a).pMax)[i]) then value = (*(*a).pMax)[i]
       endif
    endfor
    
    for i = 0, n_elements((*(*a).pWidgetType))-1 do begin
       if ((*(*a).pWidgetType)[i] eq 'widget_slider') then begin
          widget_control, 1l*(*paramTableUValue.pwIDParamNumList)[i,0],$
                                        set_slider_max = (*(*a).pMax)[i],$
                                        set_slider_min = (*(*a).pMin)[i],$
                                        set_value = (*(*a).pValues)[i],$
                                        sensitive = (*((*a).pActive))[i]
          widget_control, 1l*(*paramTableUValue.pwIDParamNumList)[i,1],$
                                        set_value = (*((*a).pActive))[i]
       endif
    endfor
    
    widget_control, ev.top, set_uvalue = paramTableUValue, /no_copy 
end

pro C_s3D_CPM_DFCs__define
   temp = { C_s3D_CPM_DFCs,         _latticeModell :  ptr_new(),$
                                    _difusionModell :  ptr_new(),$
                                    
                                    _staticInputModel  : obj_new(),$
                                    _model             : obj_new(),$ 
                                    _myDummy3DObject   : obj_new(),$
                                    
                                    _topWidgetParamBase : -1L,$
                                    _modellpParamStructure      : ptr_new() ,$
                                    _paramTableUValue : ptr_new() ,$

                                    _pParamStruct  : ptr_new(),$ ,$
                                    _stack_tlb     : 0L ,$
                                    _pGroupReference  : ptr_new(),$ ,$                                    
                                    _clusPos       : 0L ,$
                                    _chPos         : 0L ,$
                                    _tPos          : 0L ,$
                                    _tiempos       : 0L, $

                                    _dimXYZ        : [1L,1L,1L],$
                                    _maximoObjetos : 0L, $
                                    _numCells      : 0L, $
                            
                                    _seedMC        : 0L, $ 
                                    _seedPP        : 10L, $
                                    _MCS           : 100L, $
                                    _maxStep       : 10L, $
   
                                    _factorUp      : 1.0d, $
                                    _radioCoA      : 1.0d, $
                                    _factorCoA     : 1.0d, $                               
                                    _J_DFC_DFC     : 0.0d, $
                                    _J_DFC_EVL     : 0.0d, $                                    
                                    _mode2D3D      : 0.0d, $
                                    _lambdaVol     : 0.0d, $
                                    _lambdaSurf    : 0.0d, $
                                    _volT          : 0.0d, $
                                    _surfT         : 0.0d, $
                                    _H0            : 0.0d, $
                                    _T             : 1.0d, $                       
                                    _volArea       : ptr_new(),$ ,$         
                                    _SupPer        : ptr_new(),$ ,$             

                                    
                                    _win           : OBJ_NEW(), $
                                    _oClipboard    : OBJ_NEW(), $
                                    _oScene        : OBJ_NEW(), $
                                    _path          : 'c:\', $
                                    _modelName     : 'DeadSystem', $
                                    _vision        : 5}
end
