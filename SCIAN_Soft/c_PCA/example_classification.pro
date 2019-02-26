pro example_classification

	;% RESTORE: Restored variable: PARAM_NAME.
	;% RESTORE: Restored variable: DIMENSION_POP.
	;% RESTORE: Restored variable: CLASS_NAME.
	;% RESTORE: Restored variable: POPULATIONS
	RESTORE, 'C:\RSI\Steffen\s_TimeCalc\s_PCA\eritrocitos_morfologia.dat'


	;% RESTORE: Restored variable: DIPLOIDS_MORPHOLOGIC.
	;% RESTORE: Restored variable: TRIPLOIDS_MORPHOLOGIC.
	RESTORE, 'C:\RSI\Steffen\s_TimeCalc\s_PCA\Reference_diplo_triplo.dat'

	oVisionaryPca   = OBJ_NEW('VsnryPcaClassifier')

	    ;LOADING KNOWN INFO
	    n =  oVisionaryPca->New_KnownPop (DIPLOIDS_MORPHOLOGIC,'DIPLOIDS')
	    n =  oVisionaryPca->New_KnownPop (TRIPLOIDS_MORPHOLOGIC,'TRIPLOIDS')


	    ;LOADING UNKNOWN INFO
	    TESTING_POP = [0,1,2,3,4]
	    FOR i=0, N_ELEMENTS(TESTING_POP)-1 DO BEGIN
	        n = oVisionaryPca->New_UnknownPop(REFORM(POPULATIONS[TESTING_POP[i],0:DIMENSION_POP[TESTING_POP[i],0]-1,0:DIMENSION_POP[TESTING_POP[i],1]-1],DIMENSION_POP[TESTING_POP[i],0],DIMENSION_POP[TESTING_POP[i],1]),CLASS_NAME[TESTING_POP[i]])
	    ENDFOR
	    n =  oVisionaryPca->New_UnknownPop (DIPLOIDS_MORPHOLOGIC,'DIPLOIDS')
	    n =  oVisionaryPca->New_UnknownPop (TRIPLOIDS_MORPHOLOGIC,'TRIPLOIDS')

	    ;PERFORMING ANALISYS
	    numb = oVisionaryPca->Perform_TotalClasificationAnalisys(1.0)

	    ;DISPLAYING RESULT
	    FOR i=0,N_ELEMENTS(oVisionaryPca->Get_UnknownList())-1 DO BEGIN
	        indexpop = (oVisionaryPca->Get_UnknownList())[i]
	        Print, "SAMPLE ", oVisionaryPca->Get_Label(indexpop)
	        faOutMatrix = oVisionaryPca->Get_ClassificationOf (i,faOutStatistics,saOutStatistics)
	        print, saOutStatistics
	        PRINT,''
	    ENDFOR

	OBJ_DESTROY, [oVisionaryPca]
End
