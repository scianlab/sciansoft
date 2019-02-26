Pro example_pcaanalisys

;% RESTORE: Restored variable: DIPLOIDS_MORPHOLOGIC.
;% RESTORE: Restored variable: TRIPLOIDS_MORPHOLOGIC.
RESTORE, 'C:\RSI\Steffen\s_TimeCalc\s_PCA\Reference_diplo_triplo.dat'


oVisionaryPca   = OBJ_NEW('VsnryPcaClassifier')

    ;LOADING KNOWN INFO
    n =  oVisionaryPca->New_KnownPop (DIPLOIDS_MORPHOLOGIC,'DIPLOIDS')
    n =  oVisionaryPca->New_KnownPop (TRIPLOIDS_MORPHOLOGIC,'TRIPLOIDS')

    numb = oVisionaryPca->Perform_PcaAnalisys()

    Print, "DATA SOURCES PROCESSED ",numb
    Print, ""
    Print, "RESULTS"

    PARAM_NAME=['Fläche:(A)','_P²/A_____','_Eccentr._', '_R_Dist/A_', '_R_Dist___', 'Umfang:(P)',$
                '_Curvature', '_Curv./P__', '_D(curv.)_', 'D(curv.)/P', 'XY_Mt.^2__', '_X_Mt.^3__',$
                '_Y_Mt.^3__', 'XY_Mt.^3__', '_X_Mt.^4__', '_Y_Mt.^4__', 'XY_Mt.^4__', '_X_Mt.^5__',$
                '_Y_Mt.^5__', 'XY_Mt.^5__']

For i = 0, oVisionaryPca->Get_NumberOf()-1 DO BEGIN
    Print, "DATA SOURCE NAME: ",  oVisionaryPca->Get_Label(i)
    Print, "================================"
    Print, "PARAMETER LIST"
    Print, PARAM_NAME,FORMAT = '(20(A10, 4X))'
    Print, "SIZE",     oVisionaryPca->Get_Dimensions(i)
    Print, "EIGEN VALUES"
    Print, oVisionaryPca->Get_EigenValues(i),FORMAT = '(20(F8.2))'
    Print, "AXIS PERCENTS"
    Print, oVisionaryPca->Get_RelativeEigenValues(i)*100.0,FORMAT = '(20(F8.2))'
    Print, "AXIS COMPOSITION "
    Print, oVisionaryPca->Get_EigenVectors(i),FORMAT = '(20(F8.2))'
    print, norm((oVisionaryPca->Get_EigenVectors(i))[*,0])
    Print, "AXIS COMPOSITION PERCENTS"
    Print, oVisionaryPca->Get_RelativeEigenVectors(i)*100,FORMAT = '(20(F8.2))'
    Print, "PERCENT ACUMULATE TO THE THIRD AXIS"
    Print, oVisionaryPca->Get_AcumulateRelativeEigenVectors(i,3)*100,FORMAT = '(20(F8.2))'
    Print, "NORMALIZED BY (MEAN/STDDEVIATION)"
    Print, oVisionaryPca->Get_MeanStdDev(i),FORMAT ='(20(A15))
    ;    Print, "Correlacion", oVisionaryPca->Get_Correlations(i)
    Print, "PARAM GROUPS, CORRELATED BETWEEN 0.8 AND 1.0"
    print, "group","parameter",FORMAT ='(2(A15))
    print, oVisionaryPca->Get_VerboseParameterCluster  ( i,0.8,1.0,PARAM_NAME,clustercounter),FORMAT ='(2(A15))

    ; PLEASE USE ONLY COLUMNS 1 and 3
    ; COLUMN 2 IS RESERVED
    ;cluster=oVisionaryPca->Get_ParameterCluster( i,0.8,1.0,clustercounter)


    Print, ""
    Print, ""

ENDFOR

OBJ_DESTROY, [oVisionaryPca]


End
