pro s_getoTrackUnDriftedByDeformationModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

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
   thickEV = 6.
   colEV = [0,0,255]

   ; INFO POSICION ACTUAL
   clusPos  = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Cluster Position'))[0]]    
   chPos    = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Channel Position'))[0]]
   tPos     = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Time Position'))[0]] 
   tPosNext = tPos + 1   
   ;pParamStruct = selectedStackObject->getpParamStruct()
   ;INFO T Z CH TOTALES
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = totalTimes
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZimes
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalChNum = totalChNum
   maximoObjetos = -1
   for i = 0, totalTimes-1 do begin
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)   
        numObjetosActual = C_sROI3DGroupObjActual->count()
        if(numObjetosActual gt maximoObjetos) then maximoObjetos =  numObjetosActual
   endfor
   center = fltArr(3,totalTimes,maximoObjetos)

; Original center calc procedure...
;   for i = 0, totalTimes-1 do begin
;        ; We need apply a transform for the model at time "i" to correlate positions with last time "i-1"
;        ; Initial Time is the reference ... and using two planes for centred line reference and base cell plane reference 
;        transformActual = 1
;        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
;        numObjetosActual = C_sROI3DGroupObjActual->count()
;        
;        for j = 0, numObjetosActual-1 do begin
;          center(*,i,j) = (*((C_sROI3DGroupObjActual->get(position = j))->getpEigenSys())).centerXYZ
;        endfor 
;        obj_destroy, C_sROI3DGroupObjActual
;   endfor

; New criteria... for original time we use the centroide of 3DRoi... for next time we consider next 3DRoi but only 
; keeping the elements without intersection with the last 3DRoi....
; 3DRoi(t) = 3DRoi(t)[ where(3DRoi(t) ne 3DRoi(t-1)) ] ... not clear¡¡¡???? XD

   for i = 0, totalTimes-2 do begin
        ; We need apply a transform for the model at time "i" to correlate positions with last time "i-1"
        ; Initial Time is the reference ... and using two planes for centred line reference and base cell plane reference 
        transformActual = 1
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
        numObjetosActual = C_sROI3DGroupObjActual->count()
        
        xyzDim = 1l * C_sROI3DGroupObjActual->getxyzDim()
        ; Im not sure about we need volData2.... based on pro C_sROI3DGroupObj::getoVolumeModel, oObjectModel, stack_tlb = stack_tlb, fROIInt = fROIInt
        ; we can see the final volumes are a mix between volData1 and volData2... for initial analysis ... we use only the first one... 
        volData1_Actual = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = 0)
      
        C_sROI3DGroupObjNext = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i + 1, chPos = chPos, clusPos = clusPos)      
        xyzDim = 1l * C_sROI3DGroupObjNext->getxyzDim()
        volData1_Next = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = 0)
        
        for j = 0, numObjetosActual-1 do begin
          if(i eq 0) then begin
            ; First time....
            center(*,i,j) = (*((C_sROI3DGroupObjActual->get(position = j))->getpEigenSys())).centerXYZ
          endif 
          ; SLOW... FASL REFERENCE:::::
            ; recovery actual volume data
             pPoints = *((C_sROI3DGroupObjNext->get(position = j))->getpWherePoints())
            ; Obtain transformation matrix.. for DATA domain ... vDraworData = 1b
            matrixMagix = s_Driftmatrix( vDraworData = 1b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjActual, actualTime = i, chPos = chPos, clusPos = clusPos)
            ; Apply the transformation ... how???? ahhhhhhhhhhhhhhhhhhhh .... XD ....
            ; first option..... usin pPoints ... we need extract x,y,z position using pPOints Indexs...
            
            ; It solution is not really clear... but is funny.... :D ... we obtain x,y,z position using pPOints(i) and mapping in concordance..
            ; then ... we obtain the transformated posicion xt,yt,zt .. and.. mapping it a new index in volumeDara,,,, setting...
            ; pPOints(i) = mapping(xt,yt,zt) ... but... no today .. :_( ... i need it for tomorrow.... so... i will write directly 
            ; in volume data... :_( ... for the next time!!!!....
            ;volData1_Actual[pPoints] = 1
            
             nPoints = n_elements(pPoints)
             xyzPoints = intArr(3, nPoints)
             xyzPoints[0,*] = pPoints mod xyzDim[0]
             xyzPoints[1,*] = floor( (pPoints mod (xyzDim[0] * xyzDim[1])) / (1.*xyzDim[0]))
             xyzPoints[2,*] = floor( pPoints / (1. * xyzDim[0] * xyzDim[1]))

              for indexPoint = 0, nPoints-1 do begin
                 tempPoint = [xyzPoints[0,indexPoint],xyzPoints[1,indexPoint],xyzPoints[2,indexPoint],1] ; for homogeneus corrd.... 
                 tempPoint = tempPoint # matrixMagix
                 ; later.... to change... in order to use the mapping idea... maybe will be faster... by writing voldata outside the for cicle.. 
                 volData1_Actual[tempPoint[0],tempPoint[1],tempPoint[2]] = 1
              endfor
            
            
            ; For next time... take point... delete common elements with last time.. and recalculate the center...
             pPoints = *((C_sROI3DGroupObjNext->get(position = j))->getpWherePoints())
             volData1_Next[pPoints] = 1
             
             ; Obtain transformation matrix.. for DATA domain ... vDraworData = 1b
             matrixMagix = s_Driftmatrix( vDraworData = 1b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjNext, actualTime = i+1, chPos = chPos, clusPos = clusPos)
             
             nPoints = n_elements(pPoints)
             xyzPoints = intArr(3, nPoints)
             xyzPoints[0,*] = pPoints mod xyzDim[0]
             xyzPoints[1,*] = floor( (pPoints mod (xyzDim[0] * xyzDim[1])) / (1.*xyzDim[0]))
             xyzPoints[2,*] = floor( pPoints / (1. * xyzDim[0] * xyzDim[1]))

              for indexPoint = 0, nPoints-1 do begin
                 tempPoint = [xyzPoints[0,indexPoint],xyzPoints[1,indexPoint],xyzPoints[2,indexPoint],1] 
                 tempPoint = tempPoint # matrixMagix
                 ; later.... to change... in order to use the mapping idea... maybe will be faster... by writing voldata outside the for cicle.. 
                 volData1_Next[tempPoint[0],tempPoint[1],tempPoint[2]] = 1
              endfor
            
             
             ; Find equal values
             whereEqual = where((volData1_Next*volData1_Actual) eq 1)
             ; set a volume with same value positions
             volData1_Actual = volData1_Actual * 0
             if(whereEqual[0] ne -1) then volData1_Actual[whereEqual] = 1
             
             ; apply the actual - intersection... in order to find diferential points
             volData1_Next = volData1_Next - volData1_Actual
             
             
             pPoints = where(volData1_Next eq 1)
             nPoints = n_elements(pPoints)
             xyzPoints = intArr(3, nPoints)
             xyzPoints[0,*] = pPoints mod xyzDim[0]
             xyzPoints[1,*] = floor( (pPoints mod (xyzDim[0] * xyzDim[1])) / (1.*xyzDim[0]))
             xyzPoints[2,*] = floor( pPoints / (1. * xyzDim[0] * xyzDim[1]))
          
             if (nPoints eq 0) then begin
                center(*,i+1,j) = center(*,i,j) 
             endif else begin
                if (nPoints eq 1) then begin 
                    center(*,i+1,j) = xyzPoints
                endif else begin
                    center(*,i+1,j) = [total(xyzPoints[0,*]), total(xyzPoints[1,*]), total(xyzPoints[2,*])] / (1.* nPoints)                
                endelse
             endelse 
          
        endfor 
        
        volData1_Actual  = 0
        volData1_Next    = 0
        obj_destroy, C_sROI3DGroupObjActual
        obj_destroy, C_sROI3DGroupObjNext
   endfor

; Render section 
   maximoObjetos = oGroupReference->count()
   for j = 0, maximoObjetos-1 do begin 
        (oGroupReference->get(position = j))->getProperty, color = colEV, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = j))->getNumber()
        ;colEV=[0,100,0]
        ;color = s_hist_white(colEV,totalTimes) 
        ;color = s_hist_black(colEV,totalTimes) 
        color = s_without_degrade(colEV,totalTimes)
     for i = 0, totalTimes-1 do begin
             oObjectModel->add, obj_new('C_sOrb', POS=center[*,i,j], RADIUS=radio, xyzFact = factorXYZ, color = color[*,i], thick = thickEV, xCoord_conv = xCoord_conv, $
                                                  yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_Track_SphereEnd_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
          if i ne 0 then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,i-1,j]],[center[1,i,j],center[1,i-1,j]], [center[2,i,j], center[2,i-1,j]], color = color[*,i], $
             ;oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,0,j]],[center[1,i,j],center[1,0,j]], [center[2,i,j], center[2,0,j]], color = color[*,0], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackLine_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
     endfor
   endfor
 
end

pro s_getoTrackGroupUnDriftedByDeformationModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel

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
   thickEV = 6.
   colEV = [0,0,255]

   ; INFO POSICION ACTUAL
   clusPos  = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Cluster Position'))[0]]    
   chPos    = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Channel Position'))[0]]
   tPos     = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Time Position'))[0]] 
   tPosNext = tPos + 1   
   ;pParamStruct = selectedStackObject->getpParamStruct()
   ;INFO T Z CH TOTALES
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = totalTimes
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZimes
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalChNum = totalChNum
   maximoObjetos = -1
   for i = 0, totalTimes-1 do begin
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)   
        numObjetosActual = C_sROI3DGroupObjActual->count()
        if(numObjetosActual gt maximoObjetos) then maximoObjetos =  numObjetosActual
   endfor
   center = fltArr(3,totalTimes,maximoObjetos)

; Original center calc procedure...
;   for i = 0, totalTimes-1 do begin
;        ; We need apply a transform for the model at time "i" to correlate positions with last time "i-1"
;        ; Initial Time is the reference ... and using two planes for centred line reference and base cell plane reference 
;        transformActual = 1
;        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
;        numObjetosActual = C_sROI3DGroupObjActual->count()
;        
;        for j = 0, numObjetosActual-1 do begin
;          center(*,i,j) = (*((C_sROI3DGroupObjActual->get(position = j))->getpEigenSys())).centerXYZ
;        endfor 
;        obj_destroy, C_sROI3DGroupObjActual
;   endfor

; New criteria... for original time we use the centroide of 3DRoi... for next time we consider next 3DRoi but only 
; keeping the elements without intersection with the last 3DRoi....
; 3DRoi(t) = 3DRoi(t)[ where(3DRoi(t) ne 3DRoi(t-1)) ] ... not clear¡¡¡???? XD

   for i = 0, totalTimes-2 do begin
        ; We need apply a transform for the model at time "i" to correlate positions with last time "i-1"
        ; Initial Time is the reference ... and using two planes for centred line reference and base cell plane reference 
        transformActual = 1
        C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
        numObjetosActual = C_sROI3DGroupObjActual->count()
        
        xyzDim = 1l * C_sROI3DGroupObjActual->getxyzDim()
        ; Im not sure about we need volData2.... based on pro C_sROI3DGroupObj::getoVolumeModel, oObjectModel, stack_tlb = stack_tlb, fROIInt = fROIInt
        ; we can see the final volumes as a mix between volData1 and volData2... for initial analysis ... we use only the first one... 
        volData1_Actual = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = 0)
      
        C_sROI3DGroupObjNext = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i + 1, chPos = chPos, clusPos = clusPos)      
        xyzDim = 1l * C_sROI3DGroupObjNext->getxyzDim()
        volData1_Next = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = 0)
        
        for j = 0, numObjetosActual-1 do begin
          if(i eq 0) then begin
            ; First time....
            center(*,i,j) = (*((C_sROI3DGroupObjActual->get(position = j))->getpEigenSys())).centerXYZ
          endif 
          ; SLOW... FASL REFERENCE:::::
            ; recovery actual volume data
             pPoints = *((C_sROI3DGroupObjNext->get(position = j))->getpWherePoints())
            ; Obtain transformation matrix.. for DATA domain ... vDraworData = 1b
            matrixMagix = s_Driftmatrix( vDraworData = 1b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjActual, actualTime = i, chPos = chPos, clusPos = clusPos)
            ; Apply the transformation ... how???? ahhhhhhhhhhhhhhhhhhhh .... XD ....
            ; first option..... usin pPoints ... we need extract x,y,z position using pPOints Indexs...
            
            ; It solution is not really clear... but is funny.... :D ... we obtain x,y,z position using pPOints(i) and mapping in concordance..
            ; then ... we obtain the transformated posicion xt,yt,zt .. and.. mapping it a new index in volumeDara,,,, setting...
            ; pPOints(i) = mapping(xt,yt,zt) ... but... no today .. :_( ... i need it for tomorrow.... so... i will write directly 
            ; in volume data... :_( ... for the next time!!!!....
            ;volData1_Actual[pPoints] = 1
            
             nPoints = n_elements(pPoints)
             xyzPoints = intArr(3, nPoints)
             xyzPoints[0,*] = pPoints mod xyzDim[0]
             xyzPoints[1,*] = floor( (pPoints mod (xyzDim[0] * xyzDim[1])) / (1.*xyzDim[0]))
             xyzPoints[2,*] = floor( pPoints / (1. * xyzDim[0] * xyzDim[1]))

              for indexPoint = 0, nPoints-1 do begin
                 tempPoint = [xyzPoints[0,indexPoint],xyzPoints[1,indexPoint],xyzPoints[2,indexPoint],1] ; for homogeneus corrd.... 
                 tempPoint = tempPoint # matrixMagix
                 ; later.... to change... in order to use the mapping idea... maybe will be faster... by writing voldata outside the for cicle.. 
                 volData1_Actual[tempPoint[0],tempPoint[1],tempPoint[2]] = 1
              endfor
            
            
            ; For next time... take point... delete common elements with last time.. and recalculate the center...
             pPoints = *((C_sROI3DGroupObjNext->get(position = j))->getpWherePoints())
             volData1_Next[pPoints] = 1
             
             ; Obtain transformation matrix.. for DATA domain ... vDraworData = 1b
             matrixMagix = s_Driftmatrix( vDraworData = 1b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = C_sROI3DGroupObjNext, actualTime = i+1, chPos = chPos, clusPos = clusPos)
             
             nPoints = n_elements(pPoints)
             xyzPoints = intArr(3, nPoints)
             xyzPoints[0,*] = pPoints mod xyzDim[0]
             xyzPoints[1,*] = floor( (pPoints mod (xyzDim[0] * xyzDim[1])) / (1.*xyzDim[0]))
             xyzPoints[2,*] = floor( pPoints / (1. * xyzDim[0] * xyzDim[1]))

              for indexPoint = 0, nPoints-1 do begin
                 tempPoint = [xyzPoints[0,indexPoint],xyzPoints[1,indexPoint],xyzPoints[2,indexPoint],1] 
                 tempPoint = tempPoint # matrixMagix
                 ; later.... to change... in order to use the mapping idea... maybe will be faster... by writing voldata outside the for cicle.. 
                 volData1_Next[tempPoint[0],tempPoint[1],tempPoint[2]] = 1
              endfor
            
             
             ; Find equal values
             whereEqual = where((volData1_Next*volData1_Actual) eq 1)
             ; set a volume with same value positions
             volData1_Actual = volData1_Actual * 0
             if(whereEqual[0] ne -1) then volData1_Actual[whereEqual] = 1
             
             ; apply the actual - intersection... in order to find diferential points
             volData1_Next = volData1_Next - volData1_Actual
             
             
             pPoints = where(volData1_Next eq 1)
             nPoints = n_elements(pPoints)
             xyzPoints = intArr(3, nPoints)
             xyzPoints[0,*] = pPoints mod xyzDim[0]
             xyzPoints[1,*] = floor( (pPoints mod (xyzDim[0] * xyzDim[1])) / (1.*xyzDim[0]))
             xyzPoints[2,*] = floor( pPoints / (1. * xyzDim[0] * xyzDim[1]))
          
             if (nPoints eq 0) then begin
                center(*,i+1,j) = center(*,i,j) 
             endif else begin
                if (nPoints eq 1) then begin 
                    center(*,i+1,j) = xyzPoints
                endif else begin
                    center(*,i+1,j) = [total(xyzPoints[0,*]), total(xyzPoints[1,*]), total(xyzPoints[2,*])] / (1.* nPoints)                
                endelse
             endelse 
          
        endfor 
        
        volData1_Actual  = 0
        volData1_Next    = 0
        obj_destroy, C_sROI3DGroupObjActual
        obj_destroy, C_sROI3DGroupObjNext
   endfor

; Render section 
   maximoObjetos = oGroupReference->count()
   for j = 0, maximoObjetos-1 do begin 
        (oGroupReference->get(position = j))->getProperty, color = colEV, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = j))->getNumber()
        ;colEV=[0,100,0]
        ;color = s_hist_white(colEV,totalTimes) 
        ;color = s_hist_black(colEV,totalTimes) 
        color = s_without_degrade(colEV,totalTimes)
     for i = 0, totalTimes-1 do begin
             oObjectModel->add, obj_new('C_sOrb', POS=center[*,i,j], RADIUS=radio, xyzFact = factorXYZ, color = color[*,i], thick = thickEV, xCoord_conv = xCoord_conv, $
                                                  yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_Track_SphereEnd_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
          if i ne 0 then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,i-1,j]],[center[1,i,j],center[1,i-1,j]], [center[2,i,j], center[2,i-1,j]], color = color[*,i], $
             ;oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,0,j]],[center[1,i,j],center[1,0,j]], [center[2,i,j], center[2,0,j]], color = color[*,0], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackLine_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
     endfor
   endfor


   maximoObjetos = oGroupReference->count()

        (oGroupReference->get(position = 0))->getProperty, color = colEV, alpha_channel = alpha
        objNumber = (oGroupReference->get(position = 0))->getNumber()
        colEV=[0,100,0]
        ;color = s_hist_white(colEV,totalTimes) 
        ;color = s_hist_black(colEV,totalTimes)
        color = s_without_degrade(colEV,totalTimes)    
   radio = 2
   for i = 0, totalTimes-1 do begin

          oObjectModel->add, obj_new('C_sOrb', POS=centerTime[*,i], RADIUS=radio, xyzFact = factorXYZ, color = color[*,i], thick = thickEV, xCoord_conv = xCoord_conv, $
                                                  yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                  name = strCompress('3D_TrackUnDrifted_SphereEnd_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
          if i ne 0 then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [centerTime[0,i],centerTime[0,i-1]],[centerTime[1,i],centerTime[1,i-1]], [centerTime[2,i], centerTime[2,i-1]], color = color[*,i], $
             ;oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,0,j]],[center[1,i,j],center[1,0,j]], [center[2,i,j], center[2,0,j]], color = color[*,0], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackUnDriftedLine_'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
   endfor
 

 
end