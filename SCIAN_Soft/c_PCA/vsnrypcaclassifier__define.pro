; $Id: VsnryPcaClassifier__define.pro,v 0.90 2004/07/20 19:19:09
;
; Copyright (c) 2004,Rodrigo Rojas Moraleda - Visionary, All rights reserved.
;                    rodrigo.rojas@gmx.net,rrojas@xperts.cl
;+
; CLASS_NAME:
;       VSNRYPCACLASSIFFIER
;
; PURPOSE:
;      Toolkit for data analisys and classification using correlation and P.C.A.
;
; CATEGORY:
;       DATA ANALISYS
;
;-

function VsnryPcaClassifier::Init
    self.pfaClassification = PTR_NEW(PTR_NEW(FLTARR(1,2)))
    RETURN, 1
end

Pro VsnryPcaClassifier::CLEANUP
    PTR_FREE,(*self.pfaDataPop)[*]
    PTR_FREE,self.pfaDataPop

    PTR_FREE,(*self.pfaClassification)[*]
    PTR_FREE,self.pfaClassification


    PTR_FREE,self.psaLabelPop
    PTR_FREE,self.piaInfoPop
    PTR_FREE,self.piaIndexPopControl
    PTR_FREE,self.piaIndexPopTest


    PTR_FREE, (*self.pfaEigenVectors)[*]
    PTR_FREE, self.pfaEigenVectors
    PTR_FREE, (*self.pfaEigenValues)[*]
    PTR_FREE, self.pfaEigenValues
    PTR_FREE, (*self.pfaMeanStdDev)[*]
    PTR_FREE, self.pfaMeanStdDev
    PTR_FREE, (*self.pfaCorrelation)[*]
    PTR_FREE, self.pfaCorrelation
    PTR_FREE, self.piaCompletedProcess
end


FUNCTION VsnryPcaClassifier::prvtd_AddPop, faInputDataMatrix,sLabelDataMatrix

    iaDimData   = SIZE(faInputDataMatrix)

     IF (self.iLoadElements EQ 0) THEN BEGIN
       self.pfaDataPop         = PTR_NEW(PTR_NEW(FLTARR(iaDimData[1],iaDimData[2])))
       self.psaLabelPop        = PTR_NEW(STRARR(1) )
       self.piaInfoPop         = PTR_NEW(INTARR(2,1))
       self.pfaEigenValues      = PTR_NEW(PTR_NEW(FLTARR(iaDimData[1])))
       self.pfaEigenVectors     = PTR_NEW(PTR_NEW(FLTARR(iaDimData[1],iaDimData[1])))
       self.pfaMeanStdDev       = PTR_NEW(PTR_NEW(FLTARR(iaDimData[1],2)))
       self.pfaCorrelation      = PTR_NEW(PTR_NEW(FLTARR(iaDimData[1],iaDimData[1])))
       self.piaCompletedProcess = PTR_NEW(INTARR(1))

       *(*self.pfaDataPop)  = faInputDataMatrix
       (*self.psaLabelPop)[0] = sLabelDataMatrix
       (*self.piaInfoPop)[*,0] = [iaDimData[1],iaDimData[2]]
     ENDIF ELSE BEGIN
       *self.pfaDataPop = [*self.pfaDataPop,[PTR_NEW(FLTARR(iaDimData[1],iaDimData[2]))]]
       *(*self.pfaDataPop)[(size(*self.pfaDataPop))[1]-1]=faInputDataMatrix

       *self.psaLabelPop = [*self.psaLabelPop,sLabelDataMatrix]
       *self.piaInfoPop   = [[*self.piaInfoPop],[iaDimData[1],iaDimData[2]]]

       *self.pfaEigenValues  = [*self.pfaEigenValues ,[PTR_NEW(FLTARR(iaDimData[1]))]]
       *self.pfaEigenVectors = [*self.pfaEigenVectors,[PTR_NEW(FLTARR(iaDimData[1],iaDimData[1]))]]
       *self.pfaMeanStdDev   = [*self.pfaMeanStdDev   ,[PTR_NEW(FLTARR(iaDimData[1],2))]]
       *self.pfaCorrelation  = [*self.pfaCorrelation ,[PTR_NEW(FLTARR(iaDimData[1],iaDimData[1]))]]
       *self.piaCompletedProcess = [*self.piaCompletedProcess,0]
     ENDELSE

     self.iLoadElements = self.iLoadElements + 1

     RETURN, self.iLoadElements
END

FUNCTION VsnryPcaClassifier::New_KnownPop, faInputDataMatrix,sLabelDataMatrix

    iNumPop = self->prvtd_AddPop(faInputDataMatrix,sLabelDataMatrix)
    IF self.iInitControl EQ 0 THEN BEGIN
       self.piaIndexPopControl       = PTR_NEW(INTARR(1))
       (*self.piaIndexPopControl)[0]  = (iNumPop - 1)
       self.iInitControl          = 1
    ENDIF ELSE BEGIN
       *self.piaIndexPopControl     = [*self.piaIndexPopControl,(iNumPop - 1)]
    ENDELSE
    RETURN, iNumPop
END


FUNCTION VsnryPcaClassifier::New_UnknownPop, faInputDataMatrix,sLabelDataMatrix
    iNumPop       = self->prvtd_AddPop (faInputDataMatrix,sLabelDataMatrix)
    IF self.iInitTest EQ 0 THEN BEGIN
       self.piaIndexPopTest      = PTR_NEW(INTARR(1))
       (*self.piaIndexPopTest)[0] = (iNumPop - 1)
       self.iInitTest          = 1
    ENDIF ELSE BEGIN
       *self.piaIndexPopTest   = [*self.piaIndexPopTest,(iNumPop - 1)]
    ENDELSE
    RETURN, iNumPop
END


FUNCTION VsnryPcaClassifier::prvtd_StandarizedTranslation, Matrix_source,Center_factor,eigen_vec,Dim_pop,Pop_1,Pop_2
    n_param=Dim_pop(Pop_1,0)
    n_cell=Dim_pop(Pop_1,1)
    centered_matrix=FLTARR(Dim_pop(Pop_1,0),Dim_pop(Pop_1,1))
    ; calculate of the centered matrix
       FOR i=0,n_param-1 DO BEGIN
         mean_vector=center_factor(Pop_2,i,0)
         std_vector=center_factor(Pop_2,i,1)
         FOR j=0,n_cell-1 DO BEGIN
          centered_matrix(i,j)= float((Matrix_source(Pop_1,i,j)- mean_vector)/std_vector)
         ENDFOR
       ENDFOR
    ; end calculate of the centered matrix
       Matrix=FLTARR(1,1)
       Matrix(*,*)=-1
       Count=FLTARR(1,n_cell)
       FOR j=0, n_param-1 DO BEGIN
         count(*,*)=0
         FOR i=0, n_param-1 DO BEGIN
          count=count+eigen_vec(Pop_2,i,j)*centered_matrix(i,*)
         ENDFOR
         IF Matrix(0,0) EQ -1 THEN BEGIN
          Matrix=count
         ENDIF ELSE BEGIN
          Matrix=[Matrix,count]
         ENDELSE
       ENDFOR
       RETURN, Matrix
END


FUNCTION VsnryPcaClassifier::prvtd_AbsoluteTranslation, faInputDataMatrix,iaInfoPop,faMeanStdDev,faEigenVector
    size_i              = iaInfoPop[0]; parameters
    size_j              = iaInfoPop[1]; samples
    normal_matrix       = FLTARR(size_i,size_j)
    translated_matrix   = FLTARR(size_i,size_j)
    aOutputMeanStdDev      = fltarr(size_i,2)

    IF ((SIZE(faEigenVector))[1] NE size_i) THEN RETURN, -1

    FOR i=0,(size_i-1) DO BEGIN
       normal_matrix   [i,*] = (faInputDataMatrix[i,*] - faMeanStdDev [i,0])/faMeanStdDev [i,1]
    ENDFOR

    translated_matrix   =  normal_matrix ## transpose(faEigenVector)

    RETURN, translated_matrix
END

FUNCTION VsnryPcaClassifier::prvtd_PopTranslation, faInputDataMatrix,iaInfoPop,faEigenVector
    size_i              = iaInfoPop[0]; parameters
    size_j              = iaInfoPop[1]; samples
    translated_matrix   = FLTARR(size_i,size_j)

    IF ((SIZE(faEigenVector))[1] NE size_i) THEN RETURN, -1

    translated_matrix   =  faInputDataMatrix ## transpose(faEigenVector)

    RETURN, translated_matrix
END

FUNCTION VsnryPcaClassifier::prvtd_PopAutoNormalization, faInputDataMatrix,iaInfoPop,aOutputMeanStdDev
    size_i              = iaInfoPop[0]; parameters
    size_j              = iaInfoPop[1]; samples
    normal_matrix       = FLTARR(size_i,size_j)

    FOR i=0,(size_i-1) DO BEGIN
       mean_vector_i     = MEAN(faInputDataMatrix[i,*])
       std_vector_i      = SQRT((TOTAL((faInputDataMatrix[i,*]-mean_vector_i)^2))/size_j)
       normal_matrix   [i,*] = (faInputDataMatrix[i,*] - mean_vector_i)/std_vector_i
    ENDFOR

    RETURN, normal_matrix
END

FUNCTION VsnryPcaClassifier::prvtd_PopExternalNormalization, faInputDataMatrix,iaInfoPop,faMeanStdDev
    size_i              = iaInfoPop[0]; parameters
    size_j              = iaInfoPop[1]; samples
    normal_matrix       = FLTARR(size_i,size_j)

    FOR i=0,(size_i-1) DO BEGIN
       normal_matrix   [i,*] = (faInputDataMatrix[i,*] - faMeanStdDev[i,0])/faMeanStdDev[i,1]
    ENDFOR

    RETURN, normal_matrix
END

FUNCTION VsnryPcaClassifier::Perform_PcaAnalisys
    FOR i=0, self.iLoadElements-1 DO BEGIN
       self->prvtd_LinearPca, *(*self.pfaDataPop)[i],*(*self.pfaEigenValues)[i],*(*self.pfaEigenVectors)[i],aOutputPcaMatrix,*(*self.pfaCorrelation)[i],*(*self.pfaMeanStdDev)[i]
       (*self.piaCompletedProcess)[i]=1
    ENDFOR
    RETURN, i
END


FUNCTION VsnryPcaClassifier::Perform_TotalClasificationAnalisys, iAxisMinimalRepresentation

    iNumPopControl = N_ELEMENTS((*self.piaIndexPopControl))
    iNumPopTest    = N_ELEMENTS((*self.piaIndexPopTest))

    PTR_FREE,(*self.pfaClassification)[*]
    PTR_FREE,self.pfaClassification


;    The dimensions on pfaClassification follow the next structure
;    X: Control Populations
;    Y: Samples on the Test Population
;    Z: Test Populations
;    Example
;    The coord [1,2,3] on  pfaClassification
;    have the distance from the sample 2 in the three Test Population to
;    the center of the first Population of control

    FOR i=0, iNumPopControl-1 DO BEGIN
       iIndexPopControl = (*self.piaIndexPopControl)[i]
       IF (*self.piaCompletedProcess)[iIndexPopControl] NE 1 THEN BEGIN
            self->prvtd_LinearPca, *(*self.pfaDataPop)[iIndexPopControl],$
                             *(*self.pfaEigenValues)[iIndexPopControl],$
                             *(*self.pfaEigenVectors)[iIndexPopControl],$
                             aOutputPcaMatrix,*(*self.pfaCorrelation)[iIndexPopControl],$
                             *(*self.pfaMeanStdDev)[iIndexPopControl]

            (*self.piaCompletedProcess)[iIndexPopControl]=1
       ENDIF
    ENDFOR

    FOR j=0, iNumPopTest-1 DO BEGIN
       iIndexPopTest = (*self.piaIndexPopTest)[j]
       iaInfoPop   = (*self.piaInfoPop)[*,iIndexPopTest]

       IF (j EQ 0) THEN BEGIN
              self.pfaClassification = PTR_NEW(PTR_NEW(FLTARR(iNumPopControl,iaInfoPop[1])))
       ENDIF ELSE BEGIN
              *self.pfaClassification = [*self.pfaClassification,[PTR_NEW(FLTARR(iNumPopControl,iaInfoPop[1]))]]
       ENDELSE

       FOR i=0, iNumPopControl-1 DO BEGIN
         iIndexPopControl = (*self.piaIndexPopControl)[i]

         faTraslated_tmp  = self->prvtd_AbsoluteTranslation (*(*self.pfaDataPop)[iIndexPopTest],(*self.piaInfoPop)[*,iIndexPopTest],*(*self.pfaMeanStdDev)[iIndexPopControl],*(*self.pfaEigenVectors)[iIndexPopControl])
         faEigenTest_tmp  = *(*self.pfaEigenValues)[iIndexPopControl]

        fAcumulate=0.0
        fTotal= TOTAL(*(*self.pfaEigenValues)[iIndexPopControl])

       IF (iAxisMinimalRepresentation LT 1.0) THEN BEGIN
          iAxis = ((*self.piaInfoPop)[*,iIndexPopControl])[0]-1
           FOR n=0,((*self.piaInfoPop)[*,iIndexPopControl])[0]-1 DO BEGIN
             fAcumulate=fAcumulate+((*(*self.pfaEigenValues)[iIndexPopControl])[n])/fTotal

             IF fAcumulate GE iAxisMinimalRepresentation THEN BEGIN
                iAxis=n
                BREAK
             ENDIF
           ENDFOR
           print, iAxis,fAcumulate
           faTraslated = faTraslated_tmp [0:iAxis,*]
           faEigenTest = faEigenTest_tmp[0:iAxis]
        ENDIF ELSE BEGIN
           faTraslated = faTraslated_tmp
           faEigenTest = faEigenTest_tmp
        ENDELSE

         iaIndexZero  = WHERE ((faEigenTest GE -0.0001)AND(faEigenTest LE 0.0001),icount)
         IF icount GT 0 THEN BEGIN
            FOR k=0,icount-1 DO BEGIN
              faTraslated[iaIndexZero[k],*] = 0
              faEigenTest[iaIndexZero[k]]   = 1.0
            ENDFOR
         ENDIF
        faEigenTest = 1.0/faEigenTest

         (*(*self.pfaClassification)[j])[i,*]=(faTraslated^2) ## TRANSPOSE(faEigenTest)
      ENDFOR
    ENDFOR

    RETURN, self.iLoadElements

END

FUNCTION VsnryPcaClassifier::Get_ClassificationOf, iTestPopIndex,faOutStatistics,saOutStatistics

    faMahalanobis       =  (*(*self.pfaClassification)[iTestPopIndex])
    iaDimensions        =  SIZE(faMahalanobis)
    iaClassification    =  INTARR(iaDimensions[2])
    saOutStatistics     =  STRARR(4,iaDimensions[1])
    faOutStatistics     =  STRARR(3,iaDimensions[1])
    iatemp              =  INTARR(iaDimensions[1])
    FOR i=0,iaDimensions[2]-1 DO BEGIN
       minval               = MIN(faMahalanobis[*,i],iIndex)
       iIDControlPop        = (*self.piaIndexPopControl)[iIndex]
       iaClassification[i]  = iIDControlPop
       iatemp[iIndex]       = iatemp[iIndex] + 1
    ENDFOR

    FOR i=0,iaDimensions[1]-1 DO BEGIN
       iIDControlPop     = (*self.piaIndexPopControl)[i]
       sNamePop          = (*self.psaLabelPop)[iIDControlPop]
       iSamples          = iatemp[i]
       fPercent          = (iSamples/(iaDimensions[2]*1.0))
       faOutStatistics[*,i] = [iIDControlPop,iSamples,fPercent]
       saOutStatistics[*,i] = [STRING(iIDControlPop),STRING(iSamples),STRING(fPercent*100),sNamePop]

    ENDFOR


    RETURN, iaClassification
END


FUNCTION VsnryPcaClassifier::Get_NumberOf, iPopIndex
    RETURN, self.iLoadElements
END

FUNCTION VsnryPcaClassifier::Get_Label, iPopIndex
    RETURN, (*self.psaLabelPop)[iPopIndex]
END

FUNCTION VsnryPcaClassifier::Get_KnownList
    RETURN,  (*self.piaIndexPopControl)
END

FUNCTION VsnryPcaClassifier::Get_UnknownList
    RETURN,  (*self.piaIndexPopTest)
END

FUNCTION VsnryPcaClassifier::Get_Dimensions, iPopIndex
    RETURN, REFORM((*self.piaInfoPop)[*,iPopIndex],2)
END

FUNCTION VsnryPcaClassifier::Get_EigenValues, iPopIndex
    RETURN, *(*self.pfaEigenValues)[iPopIndex]
END

FUNCTION VsnryPcaClassifier::Get_RelativeEigenValues, iPopIndex
    fTotalVariance= TOTAL(*(*self.pfaEigenValues)[iPopIndex])
    RETURN, (*(*self.pfaEigenValues)[iPopIndex])/fTotalVariance
END


FUNCTION VsnryPcaClassifier::Get_EigenVectors, iPopIndex
    RETURN, *(*self.pfaEigenVectors)[iPopIndex]
END


FUNCTION VsnryPcaClassifier::Get_RelativeEigenVectors, iPopIndex
    RETURN, (*(*self.pfaEigenVectors)[iPopIndex])^2
END

FUNCTION VsnryPcaClassifier::Get_AcumulateRelativeEigenVectors, iPopIndex,AxisNumber
    RelativeEigenVectors   = (*(*self.pfaEigenVectors)[iPopIndex])^2
    Dimension           = (*self.piaInfoPop)[*,iPopIndex]
    ParameterAcumulative   = FLTARR(Dimension[0])

    IF (AxisNumber GT Dimension[0]) THEN RETURN, -1
    FOR i= 0, Dimension[0]-1 DO BEGIN
       ParameterAcumulative[i] = TOTAL (RelativeEigenVectors[i,0:AxisNumber-1])
    ENDFOR
    RETURN, ParameterAcumulative
END


FUNCTION VsnryPcaClassifier::Get_Correlations, iPopIndex
    RETURN, *(*self.pfaCorrelation)[iPopIndex]
END

FUNCTION VsnryPcaClassifier::Get_MeanStdDev, iPopIndex
    RETURN, *(*self.pfaMeanStdDev)[iPopIndex]
END


FUNCTION VsnryPcaClassifier::Get_VerboseParameterCluster ,iPopIndex,LeftValue,RightValue,saParamList,clustercounter

cluster = self->Get_ParameterCluster (iPopIndex,LeftValue,RightValue,clustercounter)
IF cluster[0] EQ -1 THEN RETURN, ""

saOutCluster = STRARR(2,1)

    FOR k = 0, clustercounter DO BEGIN
       iIndex = WHERE (cluster[0,*] EQ k,count)
       FOR n=0,count-1 DO BEGIN
         IF (k EQ 0) AND (n EQ 0) THEN BEGIN
            saOutCluster=[STRING(k),saParamList[cluster[2,iIndex[n]]]]
         ENDIF ELSE BEGIN
            saOutCluster=[[saOutCluster],[STRING(k),saParamList[cluster[2,iIndex[n]]]]]
         ENDELSE
       ENDFOR
    ENDFOR
    RETURN, saOutCluster
END

FUNCTION VsnryPcaClassifier::Get_ParameterCluster ,iPopIndex,LeftValue,RightValue,iOutClusterCount

;   pasos
;   - Obtencion de la diagonal superior de la matriz de correlacion
;   - Orden de mayor a menor de un vector que cotiene los datos de la diagonal superior

    corr_matrix =  self->Get_Correlations(iPopIndex)
    iSizeDiagSup= ((((self->Get_Dimensions(iPopIndex))[0])^2)-(self->Get_Dimensions(iPopIndex))[0])/2.0
    faDiagSup  =  FLTARR(iSizeDiagSup,3)
    count=0
    FOR k=0,(self->Get_Dimensions(iPopIndex))[0]-2 DO BEGIN
       FOR n=k+1,(self->Get_Dimensions(iPopIndex))[0]-1 DO BEGIN
        faDiagSup[count,*] = [k,n,corr_matrix[k,n]]
        count = count + 1
       ENDFOR
    ENDFOR
    iaSortByCorrelation = sort(faDiagSup[*,2])
    faCorrelationDesc    = faDiagSup[reverse(iaSortByCorrelation),*]

;   pasos
;   - Obtencion del indice de los registros que cumplen con el intervalo de corr. indicado

    iaIndexes   = WHERE ((faCorrelationDesc[*,2] GE LeftValue)AND(faCorrelationDesc[*,2] LE RightValue),icount)

    IF icount EQ 0  THEN BEGIN
        RETURN, -1
    END ELSE BEGIN
        iLeftEdge  =  iaIndexes[icount-1]
        iRightEdge =  iaIndexes[0]
    ENDELSE


;   pasos
;   - Obtencion de los grupos a partir de sucesivas exploraciones y remociones

    faCorrelationDescBK = faCorrelationDesc
    cluster             = intarr(3,2)
    index4              = WHERE (faCorrelationDescBK[iRightEdge:iLeftEdge,0] NE -100, count4)
    j                   = -1
    clustercounter      = -1

    WHILE count4 NE 0 DO BEGIN
       clustercounter = clustercounter + 1
       j = j + 1

       if j eq 0 then begin
            cluster[*,0] = [clustercounter,0,faCorrelationDescBK[iRightEdge+index4[0],0]]
            cluster[*,1] = [clustercounter,0,faCorrelationDescBK[iRightEdge+index4[0],1]]
            faCorrelationDescBK[iRightEdge+index4[0],*]=-100
       endif else begin
         cluster  = [[cluster],[clustercounter,0,faCorrelationDescBK[iRightEdge+index4[0],0]]]
         cluster  = [[cluster],[clustercounter,0,faCorrelationDescBK[iRightEdge+index4[0],1]]]
         faCorrelationDescBK[iRightEdge+index4[0],*]=-100
       endelse


        index0 = WHERE (cluster[0,*] EQ clustercounter,count)
        n      = 0
        WHILE n LT count DO BEGIN
             index1 = WHERE (faCorrelationDescBK[iRightEdge:iLeftEdge,0] EQ cluster[2,index0[n]],count1)
             index2 = WHERE (faCorrelationDescBK[iRightEdge:iLeftEdge,1] EQ cluster[2,index0[n]],count2)

               FOR k=0,count1-1 DO BEGIN
                 parameter = FIX(faCorrelationDescBK[index1[k]+iRightEdge,1]) ; el parametro acompañante
                 index3 = WHERE (cluster[2,*] EQ parameter)
                 IF index3[0] EQ -1 THEN BEGIN
                  cluster = [[cluster],[clustercounter,0,parameter]]
                  index0 = WHERE (cluster[0,*] EQ clustercounter,count)
                 ENDIF
                 faCorrelationDescBK[index1[k]+iRightEdge,*]=-100
               ENDFOR

               FOR k=0,count2-1 DO BEGIN
                 parameter = FIX(faCorrelationDescBK[index2[k]+iRightEdge,0]) ; el parametro acompañante
                 index3 = WHERE (cluster[2,*] EQ parameter)
                 IF index3[0] EQ -1 THEN BEGIN
                  cluster = [[cluster],[clustercounter,0,parameter]]
                  index0 = WHERE (cluster[0,*] EQ clustercounter,count)
                 ENDIF
                 faCorrelationDescBK[index2[k]+iRightEdge,*]=-100
               ENDFOR
               n=n+1
        ENDWHILE
       index4 = WHERE (faCorrelationDescBK[iRightEdge:iLeftEdge,0] NE -100, count4)
    ENDWHILE

;   pasos
;   - aquellos parametros que no se correlacionan con los restantes en los niveles indicados
;   se agregan en forma aparte

    FOR k = 0,(self->Get_Dimensions(iPopIndex))[0]-1 DO BEGIN
       index0 = WHERE(cluster[2,*] EQ k, count)
       IF count EQ 0 THEN BEGIN
          clustercounter = clustercounter + 1
          cluster = [[cluster],[clustercounter,0,k]]
       ENDIF
    ENDFOR
    iOutClusterCount = clustercounter
    return, cluster
END


PRO VsnryPcaClassifier::prvtd_LinearPca, aInputDataMatrix,aOutputEigenValues,aOutputEigenVectors,aOutputPcaMatrix,aOutputCorrelationMatrix,aOutputMeanStdDev
    vector_size         = size (aInputDataMatrix)

    size_i           = vector_size(1); parameters
    size_j           = vector_size(2); samples

    centered_matrix          = fltarr(size_i,size_j)
    aOutputCorrelationMatrix    = fltarr(size_i,size_i)
    aOutputPcaMatrix         = fltarr(size_i,size_j)
    aOutputMeanStdDev      = fltarr(size_i,2)


    ; calculate of the centered matrix
       for i=0,(size_i-1) do begin
         vector_i    = aInputDataMatrix(i,*)
         mean_vector_i  = MEAN(aInputDataMatrix[i,*])
         std_vector_i   = SQRT((TOTAL((aInputDataMatrix[i,*]-mean_vector_i)^2))/size_j)
         aOutputMeanStdDev [i,*] = [mean_vector_i,std_vector_i]
        centered_matrix   [i,*] = (aInputDataMatrix[i,*] - aOutputMeanStdDev [i,0])/aOutputMeanStdDev [i,1]
       endfor
    ; end calculate of the centered matrix

    ; begin calculate of the covariances matrix
       diag_matrix=fltarr(size_j,size_j)
       centered_matrix_trans=transpose(centered_matrix,[1,0])
       for j=0, size_j-1 do begin
         diag_matrix[j,j]=float(1./size_j)
       endfor
       aOutputCorrelationMatrix=(centered_matrix_trans##diag_matrix)##centered_matrix
    ; end of calculate covariance matrix

    ;decimal simetric adjust of aOutputCorrelationMatrix
    Maxim=MAX(aOutputCorrelationMatrix-(transpose(aOutputCorrelationMatrix,[1,0])))
    Minim=MIN(aOutputCorrelationMatrix-(transpose(aOutputCorrelationMatrix,[1,0])))
    WHILE (Maxim NE Minim)OR(Maxim NE 0)OR(Minim NE 0)  DO BEGIN
       substract_matrix=aOutputCorrelationMatrix-(transpose(aOutputCorrelationMatrix,[1,0]))
       m=1
       for j=0,size_i-2 do begin
         for i=m,size_i-1 do begin
          aOutputCorrelationMatrix(i,j)=aOutputCorrelationMatrix(i,j)-(substract_matrix(i,j))
         endfor
         m=m+1
       endfor
       Maxim=MAX(aOutputCorrelationMatrix-(transpose(aOutputCorrelationMatrix,[1,0])))
       Minim=MIN(aOutputCorrelationMatrix-(transpose(aOutputCorrelationMatrix,[1,0])))
    ENDWHILE
    ;end decimal simetric adjuste of aOutputCorrelationMatrix


    ;begin calculate of eigenvectors and eigenvalues
       residual = 1 & aOutputEigenVectors = 1;
       aOutputEigenValues = EIGENQL(aOutputCorrelationMatrix, EIGENVECTORS = aOutputEigenVectors, RESIDUAL = residual,/DOUBLE)
    ; end the calculate of eigenvectors and eigenvalues

     aOutputPcaMatrix = centered_matrix ## transpose(aOutputEigenVectors)

    ;begin calculate the diagonal sum of the correlate matrix
       sum_diag_corr_matrix=0
       for i=0,size_i-1  do begin
         sum_diag_corr_matrix=sum_diag_corr_matrix+aOutputCorrelationMatrix(i,i)
       endfor
    ;end calculate diagonal sum

end ;program

;OBJ_DESTROY, [oVisionaryPca]
;
;pop1 = REFORM(POPULATIONS[0,0:DIMENSION_POP[0,0]-1,0:DIMENSION_POP[0,1]-1],DIMENSION_POP[0,0],DIMENSION_POP[0,1])
;pop2 = REFORM(POPULATIONS[4,0:DIMENSION_POP[4,0]-1,0:DIMENSION_POP[4,1]-1],DIMENSION_POP[4,0],DIMENSION_POP[4,1])
;
;
;FOR i = 0 , 13 DO BEGIN
;    print, 'Iteracion',i
;    IndexDiplo  = WHERE  (salida1 EQ 0,count1)
;    IndexTriplo = WHERE  (salida2 EQ 1,count2)
;
;    oVisionaryPca   = OBJ_NEW('VsnryPcaClassifier')
;
;        dataload= pop1[*,IndexDiplo]
;        pop1=dataload
;        dataload= pop2[*,IndexTriplo]
;        pop2=dataload
;
;        n = oVisionaryPca->New_KnownPop(pop1,'diploides')
;        n = oVisionaryPca->New_KnownPop(pop2,'triploides')
;
;        n = oVisionaryPca->New_UnKnownPop(pop1,'diploides')
;        n = oVisionaryPca->New_UnKnownPop(pop2,'triploides')
;
;        numb= oVisionaryPca->Perform_TotalClasificationAnalisys(1.0)
;
;        salida1 = oVisionaryPca->Get_ClassificationOf (0,faOutStatistics,saOutStatistics)
;        print, saOutStatistics
;        salida2 = oVisionaryPca->Get_ClassificationOf (1,faOutStatistics,saOutStatistics)
;        print, saOutStatistics
;     OBJ_DESTROY, [oVisionaryPca]
;    print,''
;ENDFOR
;
;IndexDiplo  = WHERE  (salida1 EQ 0,count1)
;IndexTriplo = WHERE  (salida2 EQ 1,count2)
;
;dataload = pop1[*,IndexDiplo]
;DIPLOIDS_MORPHOLOGIC = dataload
;
;dataload = pop2[*,IndexTriplo]
;TRIPLOIDS_MORPHOLOGIC= dataload
;
;
;SAVE, FILENAME='Reference_diplo_triplo.sav',DIPLOIDS_MORPHOLOGIC,TRIPLOIDS_MORPHOLOGIC


pro VsnryPcaClassifier__define
    COMPILE_OPT hidden
    struct=    {VsnryPcaClassifier,$
         pfaDataPop         : PTR_NEW (/ALLOCATE_HEAP),$
         psaLabelPop        : PTR_NEW (/ALLOCATE_HEAP),$
         piaInfoPop         : PTR_NEW (/ALLOCATE_HEAP),$
         piaCompletedProcess: PTR_NEW (/ALLOCATE_HEAP),$
         piaIndexPopControl : PTR_NEW (/ALLOCATE_HEAP),$
         piaIndexPopTest    : PTR_NEW (/ALLOCATE_HEAP),$
         pfaClassification  : PTR_NEW (/ALLOCATE_HEAP),$

         pfaEigenVectors    : PTR_NEW (/ALLOCATE_HEAP),$
         pfaEigenValues     : PTR_NEW (/ALLOCATE_HEAP),$
         pfaMeanStdDev      : PTR_NEW (/ALLOCATE_HEAP),$
         pfaCorrelation     : PTR_NEW (/ALLOCATE_HEAP),$

         iLoadElements      :0,$
         iInitTest          :0,$
         iInitControl       :0 $
         }
end