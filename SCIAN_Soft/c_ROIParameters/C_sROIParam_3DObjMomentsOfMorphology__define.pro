;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjMomentsOfMorphology
;
; PURPOSE:
;       - Calculation of 3D Object Moments of Morphology
;
; AUTHOR:
;     Dr. Steffen Härtel (2007)
;     e_mail:shartel@med.uchile.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjMomentsOfMorphology' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_3DObjMomentsOfMorphology::apply, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position, stack_tlb = stack_tlb

   axis = fltArr(3,2)
   MYxyz = fltArr(3,2)
   inertiaT = make_array(3,3, /double)
      
   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do  whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

       ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1:if (position[0] eq -1) then return else  whParamActive[position] = 1
       else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase

       ; check Pointers
    wherePA = where(whParamActive eq 1)
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROI3DGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin

         ; set Object Number Vector
       (*self.pParamStruct).pROINumberVect = ptr_new(C_sROI3DGroupObj->getObjectNumberVector(), /no_copy)

         ; set Object Parameter Vectors
         ;if (whParamActive[i]) then 
       for i = 0, n_elements(whParam)-1 do *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float, value = -1.)

         ; set center coordinate vector for each object
       if whParamActive[6] then centerXYZ = make_array(3, nObjects, /float)

         ; set principal axis vectors for each object
       fGetEVs = 0b
       fOk = 0b
       if ((total(whParamActive[7:30]) + total(whParamActive[51:53]))gt 0) then begin
         fGetEVs = 1b
         fOk = C_sROI3DGroupObj->calcEigenSys()
         eigenVals = make_array(3, nObjects, /float)
         eigenVect = make_array(3, 3, nObjects, /float)
       endif

        s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
        widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
          ;if(OBJ_VALID(stateObjWindow)) then begin
            ; First try to extract planes undrifted
            oPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D UnDrifted Plane Model Base')
            if not(obj_valid(oPlaneModel)) then oPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Plane Model Base')
            
            oOrthogonalPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D UnDrifted Plane Model Orthogonal')
            if not(obj_valid(oOrthogonalPlaneModel)) then oOrthogonalPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Plane Model Orthogonal')
            
            if not(obj_valid(oOrthogonalPlaneModel)) then oOrthogonalPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D UnDrifted Plane Model Complementary')
            if not(obj_valid(oOrthogonalPlaneModel)) then oOrthogonalPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Plane Model Complementary')
          ;endif
        widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
       if(whParamActive[56] and obj_valid(oPlaneModel)) then begin
          angulos = s_anglesRespectArbitraryPlane( oObjectModel = oPlaneModel, stack_tlb = stack_tlb, oGroupReference = C_sROI3DGroupObj)
       endif   
        if(whParamActive[56] and obj_valid(oOrthogonalPlaneModel)) then begin
          angulos = s_anglesRespectArbitraryOrthogonalPlane( oObjectModel = oOrthogonalPlaneModel, stack_tlb = stack_tlb, oGroupReference = C_sROI3DGroupObj) 
       endif
       ;llamada al 3D Plane Model para obtener las distancias al plano arbitrario
       if((whParamActive[57] or whParamActive[58]) and obj_valid(oPlaneModel)) then begin
          distancias = s_distanceRespectArbitraryPlane( oObjectModel = oPlaneModel, stack_tlb = stack_tlb, oGroupReference = C_sROI3DGroupObj)

          ; el sistema esta pensado para 2 escalas de colores, si se quiere usar una
          ; (cuando el plano esta fuera de la figura) mejor usar 3D MC Distance with Arbitrary Plane
          ; y elegir una escala de colores de un solo color.
          distanciasReordenadas = distancias
          indexLT0 = where(distancias lt 0,nLT0, COMPLEMENT = indexBE0, NCOMPLEMENT=nBE0)
          ; deben existir AL MENOS UNO de cada clase (negativos y positivos), si no, no tiene sentido
          ; este calculo y mejor usar una escala de un solo color
          if ((nLT0 ne -1) and (nBE0 ne -1)) then begin
             if (nLT0 gt 0) then begin
                limInferior=-100d
                limSuperior=-10d
                ; los menores que 0 deben ocupar escala -100(para el mas negativo) a -10 (para el menos negativo),
                ; se deja un pequeño margen para que no se confundan los "blancos" negativos (valores muy cercanos a cero)
                ; de los "blancos" positivos. 
                minimo = min(distancias[indexLT0], max= maximo)
                denominador = (maximo-minimo)
                ; nuevo valor... mapeo directo 
                if (abs(denominador) gt 0.00000001d) then distanciasReordenadas[indexLT0] = ((limSuperior-limInferior)/denominador)*(distancias[indexLT0]-maximo)+limSuperior $
                   else distanciasReordenadas[indexLT0]=limInferior
             endif
             if (nBE0 gt 0) then begin
                limInferior=10d
                limSuperior=100d             
             ; los mayores que 0 deben ocupar escala 0(para el menos positivo) a 100 (para el mas positivo)
                minimo = min(distancias[indexBE0], max= maximo)
                denominador = (maximo-minimo)
                if (abs(denominador) gt 0.00000001d) then distanciasReordenadas[indexBE0] = ((limSuperior-limInferior)/denominador)*(distancias[indexBE0]-maximo)+limSuperior $
                   else distanciasReordenadas[indexBE0]=limSuperior
             endif              
          endif
       endif   

       for i = 0, nObjects-1 do begin

          pEigenSys = (C_sROI3DGroupObj->get(position = i))->calcEigenSys()
          pEigenSys = (C_sROI3DGroupObj->get(position = i))->getpEigenSys()
          centerXYZ[*,i] = (*pEigenSys).centerXYZ

          if whParamActive[0] then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = (*pEigenSys).eigenVals[0]

          if whParamActive[1] then (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[i] = (*pEigenSys).eigenVals[1]

          if whParamActive[2] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = (*pEigenSys).eigenVals[2]

          if whParamActive[3] then (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[i] = (*pEigenSys).eigenVals[1] / (*pEigenSys).eigenVals[0]

          if whParamActive[4] then (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[i] = (*pEigenSys).eigenVals[2] / (*pEigenSys).eigenVals[0]

          if whParamActive[5] then (*(*(*self.pValueStruct)[whParam[5]]).pROIParamVect)[i] = (*pEigenSys).eigenVals[2] / (*pEigenSys).eigenVals[1]

             ; elongation
          if whParamActive[6] then (*(*(*self.pValueStruct)[whParam[6]]).pROIParamVect)[i] = 1. - (*pEigenSys).eigenVals[1] / (*pEigenSys).eigenVals[0]
             ; relative elongation
          if whParamActive[7] then (*(*(*self.pValueStruct)[whParam[7]]).pROIParamVect)[i] = 1. - (*pEigenSys).eigenVals[2] / (*pEigenSys).eigenVals[0]
             ; flatness
          if whParamActive[8] then (*(*(*self.pValueStruct)[whParam[8]]).pROIParamVect)[i] = 1. - (*pEigenSys).eigenVals[2] / (*pEigenSys).eigenVals[1]

          if whParamActive[9] then begin
             p = (*pEigenSys).eigenVals[*] / total((*pEigenSys).eigenVals[*]) > [.00001,.00001,.00001]
                ; pSphere = [1./3., 1./3., 1./3.] (all states are equally occupied <-> entropy is max <-> highest disorder)
                ; entropy = -1./alog10(3.) * total(pSphere * alog10(pSphere)) = 1.
                ; pLine = [1., 0., 0.] (all information in one state <-> entropy is min <-> highest order)
                ; entropy = -1./alog10(3.) * total(pLine * alog10(pLine)) = 0.0
                ; entropies are scaled between 0 - 1 in contrast to Tassy et al. 2006
             (*(*(*self.pValueStruct)[whParam[9]]).pROIParamVect)[i] =  -1./alog10(3.) * total(p * alog10(p))
          endif

          if (max(wherePA) ge 13) then begin
             (C_sROI3DGroupObj->get(position = i))->getProperty, data = xyzPoints
             xyzPoints = (C_sROI3DGroupObj->get(position = i))->transformEVSintoXYZ(xyzPoints = xyzPoints)
             xyzCenter = (C_sROI3DGroupObj->get(position = i))->getCenterXYZ()
             xyzPoints[0,*] -= xyzCenter[0]
             xyzPoints[1,*] -= xyzCenter[1]
             xyzPoints[2,*] -= xyzCenter[2]

             dimXYZ = size(xyzPoints, /dim)
             if (n_elements(dimXYZ) eq 1) then dimXYZ = [dimXYZ, 1.]

             ;  µpqr =  total(xyzPoints[0,*]^p * xyzPoints[0,*]^q * xyzPoints[0,*]^r) / ( (dimXYZ[1])^((p+q+r)/3. +1.) )
             if whParamActive[31] then begin
                pqr = [3.,0.,0.]
                (*(*(*self.pValueStruct)[whParam[31]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[32] then begin
                pqr = [0.,3.,0.]
                (*(*(*self.pValueStruct)[whParam[32]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[33] then begin
                pqr = [0.,0.,3.]
                (*(*(*self.pValueStruct)[whParam[33]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[34] then begin
                pqr = [3.,3.,0.]
                (*(*(*self.pValueStruct)[whParam[34]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[35] then begin
                pqr = [3.,3.,3.]
                (*(*(*self.pValueStruct)[whParam[35]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[36] then begin
                pqr = [0.,3.,3.]
                (*(*(*self.pValueStruct)[whParam[36]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[37] then begin
                pqr = [3.,0.,3.]
                (*(*(*self.pValueStruct)[whParam[37]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[38] then begin
                pqr = [4.,0.,0.]
                (*(*(*self.pValueStruct)[whParam[38]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[39] then begin
                pqr = [0.,4.,0.]
                (*(*(*self.pValueStruct)[whParam[39]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[40] then begin
                pqr = [0.,0.,4.]
                (*(*(*self.pValueStruct)[whParam[40]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[41] then begin
                pqr = [4.,4.,0.]
                (*(*(*self.pValueStruct)[whParam[41]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[42] then begin
                pqr = [4.,4.,4.]
                (*(*(*self.pValueStruct)[whParam[42]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[43] then begin
                pqr = [0.,4.,4.]
                (*(*(*self.pValueStruct)[whParam[43]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[44] then begin
                pqr = [4.,0.,4.]
                (*(*(*self.pValueStruct)[whParam[44]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[45] then begin
                pqr = [5.,0.,0.]
                (*(*(*self.pValueStruct)[whParam[45]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[46] then begin
                pqr = [0.,5.,0.]
                (*(*(*self.pValueStruct)[whParam[46]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[47] then begin
                pqr = [0.,0.,5.]
                (*(*(*self.pValueStruct)[whParam[47]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[48] then begin
                pqr = [5.,5.,0.]
                (*(*(*self.pValueStruct)[whParam[48]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[49] then begin
                pqr = [5.,5.,5.]
                (*(*(*self.pValueStruct)[whParam[49]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
             if whParamActive[50] then begin
                pqr = [0.,5.,5.]
                (*(*(*self.pValueStruct)[whParam[50]]).pROIParamVect)[i] = total(xyzPoints[0,*]^pqr[0] * xyzPoints[1,*]^pqr[1] * xyzPoints[2,*]^pqr[2]) / ( (dimXYZ[1])^((pqr[0]+pqr[1]+pqr[2])/3. +1.) )
             endif
          endif
          
          if fGetEVs then begin
             eigenVals[*,i] = (*pEigenSys).eigenVals
             eigenVect[*,*,i] = (*pEigenSys).eigenVect
          endif

       endfor

       for i = 0, 50 do begin
          whNAN = where(finite((*(*(*self.pValueStruct)[whParam[i]]).pROIParamVect)[*], /nan), count)
          if (count gt 0) then (*(*(*self.pValueStruct)[whParam[i]]).pROIParamVect)[whNAN] = 0.          
       endfor

       centerXYZ[0,*] *= (*pEigenSys).sizePerXYZ[0]
       centerXYZ[1,*] *= (*pEigenSys).sizePerXYZ[1]
       centerXYZ[2,*] *= (*pEigenSys).sizePerXYZ[2]

         ; mean center distance among objects
       if whParamActive[14] then begin
          distXYZ = make_array(3, nObjects, /float)
          for i = 0, nObjects-1 do begin
             distXYZ[0,*] = (centerXYZ[0,*] - centerXYZ[0,i])^2
             distXYZ[1,*] = (centerXYZ[1,*] - centerXYZ[1,i])^2
             distXYZ[2,*] = (centerXYZ[2,*] - centerXYZ[2,i])^2
             (*(*(*self.pValueStruct)[whParam[14]]).pROIParamVect)[i] = total(sqrt(distXYZ[0,*] + distXYZ[1,*] + distXYZ[2,*])) / nObjects
          endfor
       endif

         ; center distance of each object with the group center
       if whParamActive[15] then begin
          if not(fOk) then fOk = C_sROI3DGroupObj->calcEigenSys()
          pMayorEigenSys = C_sROI3DGroupObj->getpEigenSys()
          (*pMayorEigenSys).centerXYZ *= (*pEigenSys).sizePerXYZ
          distXYZ = make_array(3, nObjects, /float)
          distXYZ[0,*] = (centerXYZ[0,*] - ((*pMayorEigenSys).centerXYZ)[0])^2
          distXYZ[1,*] = (centerXYZ[1,*] - ((*pMayorEigenSys).centerXYZ)[1])^2
          distXYZ[2,*] = (centerXYZ[2,*] - ((*pMayorEigenSys).centerXYZ)[2])^2
          for i = 0, nObjects-1 do (*(*(*self.pValueStruct)[whParam[15]]).pROIParamVect)[i] = sqrt(total(distXYZ[0,i] + distXYZ[1,i] + distXYZ[2,i]))
       endif

         ; 3D x-center position of each object
       if whParamActive[16] then (*(*(*self.pValueStruct)[whParam[16]]).pROIParamVect)[*] = centerXYZ[0,*]

         ; 3D y-center position of each object
       if whParamActive[17] then (*(*(*self.pValueStruct)[whParam[17]]).pROIParamVect)[*] = centerXYZ[1,*]

         ; 3D z-center position of each object
       if whParamActive[18] then (*(*(*self.pValueStruct)[whParam[18]]).pROIParamVect)[*] = centerXYZ[2,*]

         ; 3D z-distance of each object to top
       if whParamActive[19] then (*(*(*self.pValueStruct)[whParam[19]]).pROIParamVect)[*] = max(centerXYZ[2,*]) - centerXYZ[2,*]

         ; calculate principal axis alignment
       if fGetEVs then begin
          fOk = C_sROI3DGroupObj->calcEigenSys()
          pMayorEigenSys = C_sROI3DGroupObj->getpEigenSys()
          
          ; Computin Guia... aqui esta el for por cada objeto... :D asi que lo que es al objeto actual "i" al que le estan sacando o calculando parametros :D
          for i = 0, nObjects-1 do begin
                ; 1st axis alignment
             alpha = acos(eigenVect[0,0,*] * eigenVect[0,0,i] + eigenVect[0,1,*] * eigenVect[0,1,i] + eigenVect[0,2,*] * eigenVect[0,2,i]) * !radeg
             whNAN = where(finite(alpha, /nan), count)
             if (count gt 0) then alpha[whNAN] = 0.
               ; avoid 3rd moment effects
             whereAlpha = where(alpha gt 90, count)
             if (count gt 0) then alpha[whereAlpha] = 180.- alpha[whereAlpha]
               ; avoid circular rotation artefacts by not considering objects with an excentricity variation lower than 10%
             whereAlphaOne = where(eigenVals[0,*] ge (eigenVals[1,*]*1.1), countOne)
             if (countOne gt 0) then (*(*(*self.pValueStruct)[whParam[10]]).pROIParamVect)[i] = total(alpha[whereAlphaOne]) / countOne

                ; 2nd axis alignment
             alpha = acos(eigenVect[1,0,*] * eigenVect[1,0,i] + eigenVect[1,1,*] * eigenVect[1,1,i] + eigenVect[1,2,*] * eigenVect[1,2,i]) * !radeg
             whNAN = where(finite(alpha, /nan), count)
             if (count gt 0) then alpha[whNAN] = 0.
               ; avoid 3rd moment effects
             whereAlpha = where(alpha gt 90, count)
             if (count gt 0) then alpha[whereAlpha] = 180.- alpha[whereAlpha]
               ; avoid circular rotation artefacts by not considering objects with an excentricity variation lower than 10%
             if (countOne gt 0) then begin
               whereAlphaTwo = where((eigenVals[1,*])[whereAlphaOne] ge ((eigenVals[2,*])[whereAlphaOne]*1.1), countTwo)
               if (countTwo gt 0) then (*(*(*self.pValueStruct)[whParam[11]]).pROIParamVect)[i] = total((alpha[[whereAlphaOne]])[whereAlphaTwo]) / countTwo
             endif else begin
               whereAlphaTwo = where(eigenVals[1,*] ge (eigenVals[2,*]*1.1), countTwo)
               if (countTwo gt 0) then (*(*(*self.pValueStruct)[whParam[11]]).pROIParamVect)[i] = total(alpha[whereAlphaTwo]) / countTwo
             endelse

                ; 3rd axis alignment
             alpha = acos(eigenVect[2,0,*] * eigenVect[2,0,i] + eigenVect[2,1,*] * eigenVect[2,1,i] + eigenVect[2,2,*] * eigenVect[2,2,i]) * !radeg
             whNAN = where(finite(alpha, /nan), count)
             if (count gt 0) then alpha[whNAN] = 0.
               ; avoid 3rd moment effects
             whereAlpha = where(alpha gt 90, count)
             if (countOne gt 0) then begin
               if (countTwo gt 0) then (*(*(*self.pValueStruct)[whParam[12]]).pROIParamVect)[i] = total((alpha[[whereAlphaOne]])[whereAlphaTwo]) / countTwo
             endif else if (countTwo gt 0) then (*(*(*self.pValueStruct)[whParam[12]]).pROIParamVect)[i] = total(alpha[whereAlphaTwo]) / countTwo

             ; mayor axis alignment
             alpha = acos((*pMayorEigenSys).eigenVect[0,0] * eigenVect[0,0,i] + (*pMayorEigenSys).eigenVect[0,1] * eigenVect[0,1,i] + (*pMayorEigenSys).eigenVect[0,2,*] * eigenVect[0,2,i]) * !radeg
             whNAN = where(finite(alpha, /nan), count)
             if (count gt 0) then alpha[whNAN] = 0.
             if (alpha gt 90) then (*(*(*self.pValueStruct)[whParam[13]]).pROIParamVect)[i] = 180. - alpha else $
                (*(*(*self.pValueStruct)[whParam[13]]).pROIParamVect)[i] = alpha

              ; Is arbritary vector required?
; COMPUTIN GUIA.. INICIO DE CALCULOS DE EJE ARBITRARIO
                 vPointInitial = [(*(*(*self.pValueStruct)[whParam[51]]).pValues)[8],(*(*(*self.pValueStruct)[whParam[51]]).pValues)[9],(*(*(*self.pValueStruct)[whParam[51]]).pValues)[10]] 
                 vPointFinal   = [(*(*(*self.pValueStruct)[whParam[51]]).pValues)[11],(*(*(*self.pValueStruct)[whParam[51]]).pValues)[12],(*(*(*self.pValueStruct)[whParam[51]]).pValues)[13]]
                 ;FASL XXX verify redimension requirements
                 axis[*,0] = vPointInitial; * xyzSizePerPixel)/xyzSizePerPixel[0] 
                 axis[*,1] = vPointFinal; * xyzSizePerPixel)/xyzSizePerPixel[0] 
                 centerAxis = [total(axis[0,*]),total(axis[1,*]),total(axis[2,*])]/2

                 MYxyz[0,*] = (axis[0,*] - centerAxis[0]) * ((*pMayorEigenSys).sizePerXYZ)[0]
                 MYxyz[1,*] = (axis[1,*] - centerAxis[1]) * ((*pMayorEigenSys).sizePerXYZ)[1]
                 MYxyz[2,*] = (axis[2,*] - centerAxis[2]) * ((*pMayorEigenSys).sizePerXYZ)[2]
        
                 inertiaT[0:2,0] = [ total(MYxyz[0,*]*MYxyz[0,*]),      total(MYxyz[0,*]*MYxyz[1,*]),      total(MYxyz[0,*]*MYxyz[2,*]) ]
                 inertiaT[0:2,1] = [ inertiaT[1,0], total(MYxyz[1,*]*MYxyz[1,*]),      total(MYxyz[1,*]*MYxyz[2,*]) ]
                 inertiaT[0:2,2] = [ inertiaT[2,0], inertiaT[2,1], total(MYxyz[2,*]*MYxyz[2,*]) ]
                 inertiaT /= 2
                 AAeigenVals = eigenQL(inertiaT, /double, eigenvectors = AAeigenVect, residual = var)
                 AAeigenVect = transpose(AAeigenVect)
                 alpha = acos(AAeigenVect[0,0] * eigenVect[0,0,i] + AAeigenVect[0,1] * eigenVect[0,1,i] + AAeigenVect[0,2,*] * eigenVect[0,2,i]) * !radeg
                 
                 whNAN = where(finite(alpha, /nan), count)
                 if (count gt 0) then alpha[whNAN] = 0.
                 if (alpha gt 90) then (*(*(*self.pValueStruct)[whParam[51]]).pROIParamVect)[i] = 180. - alpha else $
                    (*(*(*self.pValueStruct)[whParam[51]]).pROIParamVect)[i] = alpha
                                  
                 ; Angle for Divisions related with AAMA
                 originalCellsNumber = (*(*(*self.pValueStruct)[whParam[52]]).pValues)[8]
                 if(originalCellsNumber  gt 1) then begin
                    if(i lt originalCellsNumber) then begin
                       (C_sROI3DGroupObj->get(position = i))->getProperty, data = tempxyzPoints
                       tempxyzPoints = (C_sROI3DGroupObj->get(position = i))->transformEVSintoXYZ(xyzPoints = tempxyzPoints)
                       axis[*,0] = (C_sROI3DGroupObj->get(position = i))->getCenterXYZ()                                

                        for f = 9, (n_elements(*(*(*self.pValueStruct)[whParam[52]]).pValues)-1) do begin
                            fatherIndex = (*(*(*self.pValueStruct)[whParam[52]]).pValues)[f] - 1
                            sonIndex    = originalCellsNumber + f -9
                            if((fatherIndex eq i) and (sonIndex lt nObjects))then begin
                                 (C_sROI3DGroupObj->get(position = sonIndex))->getProperty, data = tempxyzPoints
                                 tempxyzPoints = (C_sROI3DGroupObj->get(position = sonIndex))->transformEVSintoXYZ(xyzPoints = tempxyzPoints)
                                 axis[*,1] = (C_sROI3DGroupObj->get(position = sonIndex))->getCenterXYZ()    
                                    
                                 MYxyz[0,*] = (axis[0,*] - centerAxis[0]) * ((*pMayorEigenSys).sizePerXYZ)[0]
                                 MYxyz[1,*] = (axis[1,*] - centerAxis[1]) * ((*pMayorEigenSys).sizePerXYZ)[1]
                                 MYxyz[2,*] = (axis[2,*] - centerAxis[2]) * ((*pMayorEigenSys).sizePerXYZ)[2]
                        
                                 inertiaT[0:2,0] = [ total(MYxyz[0,*]*MYxyz[0,*]),      total(MYxyz[0,*]*MYxyz[1,*]),      total(MYxyz[0,*]*MYxyz[2,*]) ]
                                 inertiaT[0:2,1] = [ inertiaT[1,0], total(MYxyz[1,*]*MYxyz[1,*]),      total(MYxyz[1,*]*MYxyz[2,*]) ]
                                 inertiaT[0:2,2] = [ inertiaT[2,0], inertiaT[2,1], total(MYxyz[2,*]*MYxyz[2,*]) ]
                                 inertiaT /= 2
                                 ADAAeigenVals = eigenQL(inertiaT, /double, eigenvectors = ADAAeigenVect, residual = var)
                                 ADAAeigenVect = transpose(ADAAeigenVect)
                                                                 
                                 alpha = acos(AAeigenVect[0,0] * ADAAeigenVect[0,0] + AAeigenVect[0,1] * ADAAeigenVect[0,1] + AAeigenVect[0,2,*] * ADAAeigenVect[0,2]) * !radeg
                                                 
                                 whNAN = where(finite(alpha, /nan), count)
                                 if (count gt 0) then alpha[whNAN] = 0.
                                 if (alpha gt 90) then (*(*(*self.pValueStruct)[whParam[52]]).pROIParamVect)[sonIndex] = 180. - alpha else $
                                    (*(*(*self.pValueStruct)[whParam[52]]).pROIParamVect)[sonIndex] = alpha
                            endif
                        endfor                        
                    endif
                 endif 
; COMPUTIN GUIA.. Fin ejemplo... :D
; Tips.. centro objeto "i" se saca de (C_sROI3DGroupObj->get(position = i))->getCenterXYZ()
; Si crean el parametro XX para el objeto "i" obteniendo un valor ZZ se guarda en
; (*(*(*self.pValueStruct)[whParam[XX]]).pROIParamVect)[i] = ZZ
; el resto es el calculo :D
              
              ; Is migration analysis required? ... all others parameters are not usefull...
              ; need direction or reference of this.... 
              if ((whParamActive[53] or whParamActive[54] ) and (i lt floor(nObjects/2)))then begin
                 ; Initial POint... center of cell i----
                 (C_sROI3DGroupObj->get(position = i))->getProperty, data = tempxyzPoints
                 tempxyzPoints = (C_sROI3DGroupObj->get(position = i))->transformEVSintoXYZ(xyzPoints = tempxyzPoints)
                 axis[*,0] = (C_sROI3DGroupObj->get(position = i))->getCenterXYZ()                   
  
                 ; Final POint... center of cell i + (floor(nObjects/2))----
                 nextIndex = i + floor(nObjects/2)
                 (C_sROI3DGroupObj->get(position = nextIndex))->getProperty, data = tempxyzPoints
                 tempxyzPoints = (C_sROI3DGroupObj->get(position = nextIndex))->transformEVSintoXYZ(xyzPoints = tempxyzPoints)
                 axis[*,1] = (C_sROI3DGroupObj->get(position = nextIndex))->getCenterXYZ()                   

              ; Los siguientes dos parametros realizan un  analisis de migracion
              ; que usan el objeto "i" y el objeto "i + nObjetos/2"... lo que se ha hecho es crear un stack que tiene dibujadas juntas las celulas
              ; de dos tiempos consecuticos y a las que se les ha realizado tracking... asi...
              ; si el tiempo inicial tenia 40 celulas estas se mantienen en los archivos de maskaras, pero ademas se añaden las 40 celulas del tiempo
              ;siguiente (que el bilogo segmentador dibujo de tal manera que la celula "i" del tiempo inicial corresponda con la celula "i" del
              ; tiempo siguiente)... y las maskaras de las celulas del tiempos posterior se renombrar partiendo en nuesto caso desde 41 hasta 
              ; el 53 : 3D MD_AA_AMA solo calcula la distancia recorrida
                ;3D MD_AA_AMA
                   variacion = axis[*,1] - axis[*,0]
                   (*(*(*self.pValueStruct)[whParam[53]]).pROIParamVect)[i] = (sqrt(TOTAL(variacion*variacion))) 
                
                ;3D AM_AA_AMA
                 centerAxis = [total(axis[0,*]),total(axis[1,*]),total(axis[2,*])]/2
                 MYxyz[0,*] = (axis[0,*] - centerAxis[0]) * ((*pMayorEigenSys).sizePerXYZ)[0]
                 MYxyz[1,*] = (axis[1,*] - centerAxis[1]) * ((*pMayorEigenSys).sizePerXYZ)[1]
                 MYxyz[2,*] = (axis[2,*] - centerAxis[2]) * ((*pMayorEigenSys).sizePerXYZ)[2]
        
                 inertiaT[0:2,0] = [ total(MYxyz[0,*]*MYxyz[0,*]),      total(MYxyz[0,*]*MYxyz[1,*]),      total(MYxyz[0,*]*MYxyz[2,*]) ]
                 inertiaT[0:2,1] = [ inertiaT[1,0], total(MYxyz[1,*]*MYxyz[1,*]),      total(MYxyz[1,*]*MYxyz[2,*]) ]
                 inertiaT[0:2,2] = [ inertiaT[2,0], inertiaT[2,1], total(MYxyz[2,*]*MYxyz[2,*]) ]
                 inertiaT /= 2
                 MAeigenVals = eigenQL(inertiaT, /double, eigenvectors = MAeigenVect, residual = var)
                 MAeigenVect = transpose(MAeigenVect)
                                                 
                 alpha = acos(AAeigenVect[0,0] * MAeigenVect[0,0] + AAeigenVect[0,1] * MAeigenVect[0,1] + AAeigenVect[0,2,*] * MAeigenVect[0,2]) * !radeg
                 
                 whNAN = where(finite(alpha, /nan), count)
                 if (count gt 0) then alpha[whNAN] = 0.
                 if (alpha gt 90) then (*(*(*self.pValueStruct)[whParam[54]]).pROIParamVect)[i] = 180. - alpha else $
                    (*(*(*self.pValueStruct)[whParam[54]]).pROIParamVect)[i] = alpha
             endif    
              ; is eccentricity of nuclei required... 
              if (whParamActive[55] and (i lt floor(nObjects/2)))then begin
                 ; Center of cell i----
                 (C_sROI3DGroupObj->get(position = i))->getProperty, data = tempxyzPoints
                 tempxyzPoints = (C_sROI3DGroupObj->get(position = i))->transformEVSintoXYZ(xyzPoints = tempxyzPoints)
                 centerCell = (C_sROI3DGroupObj->get(position = i))->getCenterXYZ()                   
  
                 ; center nucleus
                 nucleiIndex = i + floor(nObjects/2)
                 (C_sROI3DGroupObj->get(position = nucleiIndex))->getProperty, data = tempxyzPoints
                 tempxyzPoints = (C_sROI3DGroupObj->get(position = nucleiIndex))->transformEVSintoXYZ(xyzPoints = tempxyzPoints)
                 centerNucleus = (C_sROI3DGroupObj->get(position = nucleiIndex))->getCenterXYZ()                   

                 restNucleus = centerCell - centerNucleus
                 distanceNucleus = sqrt(TOTAL(restNucleus*restNucleus))
                 
                 evProj = (C_sROI3DGroupObj->get(position = i))->getEVSProj()
                 variacion = (evProj.evMinMax)[*,1] - (evProj.evMinMax)[*,0]
                 mayorSemiAxisofCell = (sqrt(TOTAL(variacion*variacion)))/2.0

                 (*(*(*self.pValueStruct)[whParam[55]]).pROIParamVect)[i] = distanceNucleus/mayorSemiAxisofCell
             endif 
             
             *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()
             ;llamada al 3D Plane Model para obtener angulos
             if(whParamActive[56] and obj_valid(oPlaneModel)) then begin
                (*(*(*self.pValueStruct)[whParam[56]]).pROIParamVect)[i] = angulos[i]
             endif 
             if(whParamActive[56] and obj_valid(oOrthogonalPlaneModel)) then begin
                (*(*(*self.pValueStruct)[whParam[56]]).pROIParamVect)[i] = angulos[i]
             endif  

             ;llamada al 3D Plane Model para obtener las distancias al plano arbitrario
             if(whParamActive[57] and obj_valid(oPlaneModel)) then begin
                (*(*(*self.pValueStruct)[whParam[57]]).pROIParamVect)[i] = distancias[i]
             endif   

			       ;llamada al 3D Plane Model para obtener las distancias al plano arbitrario.. considerando Lateralidad
             if(whParamActive[58] and obj_valid(oPlaneModel)) then begin
                (*(*(*self.pValueStruct)[whParam[58]]).pROIParamVect)[i] = distanciasReordenadas[i]
             endif
         endfor
      endif
      ; Group Moments of Morphology 
      a      = C_sROI3DGroupObj->calcEigenSys()
      eigenV = C_sROI3DGroupObj->getpEigenSys()
      if whParamActive[59] then (*(*(*self.pValueStruct)[whParam[59]]).pROIParamVect)[*] = (*eigenV).eigenVals[0]

      if whParamActive[60] then (*(*(*self.pValueStruct)[whParam[60]]).pROIParamVect)[*] = (*eigenV).eigenVals[1]

      if whParamActive[61] then (*(*(*self.pValueStruct)[whParam[61]]).pROIParamVect)[*] = (*eigenV).eigenVals[2]
         ; elongation
      if whParamActive[62] then (*(*(*self.pValueStruct)[whParam[62]]).pROIParamVect)[*] = 1. - (*eigenV).eigenVals[1] / (*eigenV).eigenVals[0]
         ; relative elongation
      if whParamActive[63] then (*(*(*self.pValueStruct)[whParam[63]]).pROIParamVect)[*] = 1. - (*eigenV).eigenVals[2] / (*eigenV).eigenVals[0]
         ; flatness
      if whParamActive[64] then (*(*(*self.pValueStruct)[whParam[64]]).pROIParamVect)[*] = 1. - (*eigenV).eigenVals[2] / (*eigenV).eigenVals[1]
      
   endelse
end


function C_sROIParam_3DObjMomentsOfMorphology::init

    ROIParamStruct = {name:'3D Object Moments of Morphology',$
                      type:'3D ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$
                      pNames:ptr_new(),$
                      pActive:ptr_new(),$  ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$   ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$   ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$
                      pROINumberVect:ptr_new()}

    ROIParamNames = ['3D 1st Eigenvalue',$ ;0
                    '3D 2nd Eigenvalue',$
                    '3D 3rd Eigenvalue',$
                    '3D 2nd div 1st Eigenvalue',$
                    '3D 3rd div 1st Eigenvalue',$
                    '3D 3rd div 2nd Eigenvalue',$

                    '3D ev Elongation',$
                    '3D ev Relative Elongation',$
                    '3D ev Flatness',$
                    '3D ev Entropy',$

                    '3D 1st Axis Alignment',$;10
                    '3D 2nd Axis Alignment',$
                    '3D 3rd Axis Alignment',$
                    '3D Alignment with Major Axis',$

                    '3D mean object center distances',$
                    '3D center distance',$
                    '3D x-center position',$
                    '3D y-center position',$
                    '3D z-center position',$
                    '3D z-distance to top',$
                    'void',$;20
                    'void',$
                    'void',$
                    'void',$
                    'void',$
                    'void',$
                    'void',$
                    'void',$
                    'void',$
                    'void',$
                    'void',$
                    '3D µ 300',$
                    '3D µ 030',$
                    '3D µ 003',$
                    '3D µ 330',$
                    '3D µ 333',$
                    '3D µ 033',$
                    '3D µ 303',$
                    '3D µ 400',$
                    '3D µ 040',$
                    '3D µ 004',$;40
                    '3D µ 440',$
                    '3D µ 444',$
                    '3D µ 044',$
                    '3D µ 404',$
                    '3D µ 500',$
                    '3D µ 050',$
                    '3D µ 005',$
                    '3D µ 550',$
                    '3D µ 555',$
                    '3D µ 055',$ ;50
                    '3D AA_AMA',$
                    '3D AD_AA_AMA',$
                    '3D MD_AA_AMA',$; Migration Distance ... require join two consecutive masks models... tracked
                    '3D AM_AA_AMA',$; Angle of migration ... require join two consecutive masks models... tracked
                    '3D Eccentricity of Nuclei',$ ; excentricity of nucleus... require join two consecutive masks models... tracked... first the cell model and second model: nucleus
                    '3D Alignment with Plane Model',$ 
                    '3D MC Distance with Plane Model',$
                    '3D MC Distance w-PM and w-L',$; Mass Centre Distance  
                    '3D 1st Eigenvalue Group',$ 
                    '3D 2nd Eigenvalue Group',$ ;60
                    '3D 3rd Eigenvalue Group',$
                    '3D ev Elongation Group',$
                    '3D ev Relative Elongation Group',$
                    '3D ev Flatness Group']

    nParams = n_elements(ROIParamNames)
    self.pValueStruct = ptr_new(ptrArr(nParams))
    ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
; Computin guia... deben agregar el nombre para su nuevo parametro arriba ... :D y recordar su numero.. (ejemplo 56) para llamarlo despues
    ROIParamActive = make_array(nParams, /byte, value = 1b)
    ROIParamMin = make_array(nParams, /float, value = 0.)
    ROIParamMax = make_array(nParams, /float, value = 1.)
    ROIParamValues = make_array(nParams, /float, value = 0.)
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[0],$
                      type:'3D ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$
                      pNames:ptr_new(),$
                      pActive:ptr_new(),$
                      pMin:ptr_new(),$
                      pMax:ptr_new(),$
                      pValues:ptr_new(),$
                      pROIParamVect:ptr_new()}

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [0,0,0,0,0,0,0,0 ]
    ROIValueMin = [0.,0.,0.,0.,0.,0.,0.,0.]
    ROIValueMax = [1.,1.,1.,1.,1.,1.,1.,1. ]
    ROIValueValues =[0.,1.,0.,1.,0.,1.,0.,1.]

;Computin guia... la estructura de abajo es diferenciada para un parametro particular... en este caso ... AAAMA (arbitrary axis... for 1st axis alignment (1 eje principal))
    ROIValueWidgetTypeAAAMA = make_array(14, /string, value = 'widget_slider')
    ROIValueNamesAAAMA = ['Threshold_1a', 'Threshold_1b',$
                          'Threshold_2a', 'Threshold_2b',$
                          'Threshold_3a', 'Threshold_3b',$
                          'Threshold_4a', 'Threshold_4b',$
                          'First point X','First point Y','First point Z',$
                          'Second point X','Second point Y','Second point Z']  
    ROIValueActiveAAAMA = [0,0,0,0,0,0,0,0 ,1,1,1,1,1,1]
    ROIValueMinAAAMA = [0.,0.,0.,0.,0.,0.,0.,0., -1000.0,-1000.0,-1000.0,-1000.0,-1000.0,-1000.0]
    ROIValueMaxAAAMA = [1.,1.,1.,1.,1.,1.,1.,1., 1000.0,1000.0,1000.0,1000.0,1000.0,1000.0]
    ROIValueValuesAAAMA =[0.,1.,0.,1.,0.,1.,0.,1., 0.0,0.0,0.0,500.0,500.0,500.0]
 ;Computin GUia... fin de la estructura

    ROIValueWidgetTypeADAAAMA = make_array(21, /string, value = 'widget_slider')
    ROIValueNamesADAAAMA = ['Threshold_1a', 'Threshold_1b',$
                          'Threshold_2a', 'Threshold_2b',$
                          'Threshold_3a', 'Threshold_3b',$
                          'Threshold_4a', 'Threshold_4b',$
                          'Root Cells number',$
                          'Child 1','Child 2','Child 3','Child 4','Child 5','Child 6',$
                          'Child 7','Child 8','Child 9','Child 10','Child 11','Child 12']  
    ROIValueActiveADAAAMA = [0,0,0,0,0,0,0,0,          1,1,1,1,1,1,1,1,1,1,1,1,1]
    ROIValueMinADAAAMA = [0.,0.,0.,0.,0.,0.,0.,0.,     0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
    ROIValueMaxADAAAMA = [1.,1.,1.,1.,1.,1.,1.,1.,     100,100,100,100,100,100,100,100,100,100,100,100,100]
    ROIValueValuesADAAAMA =[0.,1.,0.,1.,0.,1.,0.,1.,   0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)
; Computin Guia ... para cada parametro diferente (recordar que 3DObjectMoments Of Morphology incluye hasta ahora 55 parametros diferentes :D)
; para cada uno de estos parametros se asigna la estructura de subparamtros :D, la mayoria no requieren informacion adicional asiq solo incluyen
; los thresholds basicos... los nuevos que estamos inventando requiren datos adicionales... ASi que...
    for i = 1, nParams-2 do begin
       ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[i],$
                         type:'3D ROI-Parameter-Method',$
                         pWidgetType:ptr_new(),$
                         pNames:ptr_new(),$
                         pActive:ptr_new(),$
                         pMin:ptr_new(),$
                         pMax:ptr_new(),$
                         pValues:ptr_new(),$
                         pROIParamVect:ptr_new()}
        ;Computin guia... asi que les asignamos la estructura de subparametros correspondiente :D                 
        if((*(*self.pParamStruct).pNames)[i] eq '3D AD_AA_AMA') then begin
           ROIValueWidgetType = [ROIValueWidgetTypeADAAAMA]
           ROIValueNames = [ROIValueNamesADAAAMA]
           ROIValueActive = [ROIValueActiveADAAAMA]
           ROIValueMin = [ROIValueMinADAAAMA]
           ROIValueMax = [ROIValueMaxADAAAMA]
           ROIValueValues =[ROIValueValuesADAAAMA]
        endif else begin
          if((*(*self.pParamStruct).pNames)[i] eq '3D AA_AMA') then begin
             ROIValueWidgetType = [ROIValueWidgetTypeAAAMA]
             ROIValueNames = [ROIValueNamesAAAMA]
             ROIValueActive = [ROIValueActiveAAAMA]
             ROIValueMin = [ROIValueMinAAAMA]
             ROIValueMax = [ROIValueMaxAAAMA]
             ROIValueValues =[ROIValueValuesAAAMA]
          endif else begin
             ROIValueWidgetType = [ROIValueWidgetType]
             ROIValueNames = [ROIValueNames]
             ROIValueActive = [ROIValueActive]
             ROIValueMin = [ROIValueMin]
             ROIValueMax = [ROIValueMax]
             ROIValueValues =[ROIValueValues]
          endelse
        endelse       

       pROIParamVect = [-1]
       ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
       ROIValueStruct.pNames = ptr_new(ROIValueNames)
       ROIValueStruct.pActive = ptr_new(ROIValueActive)
       ROIValueStruct.pMin = ptr_new(ROIValueMin)
       ROIValueStruct.pMax = ptr_new(ROIValueMax)
       ROIValueStruct.pValues = ptr_new(ROIValueValues)
       ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

       (*self.pValueStruct)[i] = ptr_new(ROIValueStruct, /no_copy)
    endfor

    ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[nParams-1],$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$
                    pNames:ptr_new(),$
                    pActive:ptr_new(),$
                    pMin:ptr_new(),$
                    pMax:ptr_new(),$
                    pValues:ptr_new(),$
                    pROIParamVect:ptr_new()}

     ROIValueWidgetType = [ROIValueWidgetType]
     ROIValueNames = [ROIValueNames]
     ROIValueActive = [ROIValueActive]
     ROIValueMin = [ROIValueMin]
     ROIValueMax = [ROIValueMax]
     ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[nParams-1] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end


pro C_sROIParam_3DObjMomentsOfMorphology__define
   tmp = {C_sROIParam_3DObjMomentsOfMorphology, pParamStruct:ptr_new(), pValueStruct:ptr_new(), inherits C_sROIParam}
end