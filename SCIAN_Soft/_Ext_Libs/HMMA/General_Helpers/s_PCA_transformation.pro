;_____________________________IOISIOI____________________
; FUNCTION NAME:
;      s_PCA_transformation
;
; PURPOSE:
;       - Calculation of PCA of a given matrix of data
;
; AUTHOR:
;   Hector Moraga (2012)
;   e_mail: hector.moraga.aros@gmail.com
;
; CALLING SEQUENCE:
;    s_PCA_transformation, elementsMatrix=elementsMatrix, coefficients=coefficients, $ 
;      eigenvalues=eigenvalues, variances=variances, show=show
;
;    elementsMatrix: input data matrix (mxn) (m: number of variables, n: number of obervations)
;    coefficients: matrix of coefficients
;    eigenvalues: eigenvalues vector
;    variances: variance-covariance matrix  
;    show: shows extra information
;
; METHODS:
;_____________________________IOISIOI____________________

pro s_PCA_transformation, elementsMatrix=elementsMatrix, coefficients=coefficients, $ 
      eigenvalues=eigenvalues, variances=variances, show=show
   m = (size(elementsMatrix))[1]     ; number of variables
   n = (size(elementsMatrix))[2]     ; number of observations
  means = TOTAL(elementsMatrix, 2)/n ; mean of the data
  ;mean substraction of the data
  ; the idea is to have data with 0 of mean and 1 of deviation
  elementsMatrix = elementsMatrix - REBIN(means, m, n) 
  
  ;Compute derived variables based upon the principal components. 
  result = PCOMP(elementsMatrix, COEFFICIENTS = coefficients, $ 
      EIGENVALUES=eigenvalues, VARIANCES=variances, /COVARIANCE) 

   IF (show eq 1) THEN BEGIN
    PRINT, 'Result: '
    str='('+string(m)+'(F10.4))' 
    PRINT, result, FORMAT = str 
    PRINT 
    PRINT, 'Coefficients: '
    str='("Mode#",I1,'+string(m)+'(F10.4))'
    FOR mode=0,m-1 DO PRINT, $ 
      mode+1, coefficients[*,mode], $ 
      FORMAT=str
    eigenvectors = coefficients/REBIN(eigenvalues, m, m) 
    PRINT 
    PRINT, 'Eigenvectors: ' 
    FOR mode=0,m-1 DO PRINT, $ 
      mode+1, eigenvectors[*,mode],$ 
      FORMAT=str 
    array_reconstruct = result ## eigenvectors 
    PRINT 
    PRINT, 'Reconstruction error: ', $ 
      TOTAL((array_reconstruct - elementsMatrix)^2) 
    PRINT 
    PRINT, 'Energy conservation: ', TOTAL(elementsMatrix^2), $ 
      TOTAL(eigenvalues)*(n-1) 
    PRINT 
    PRINT, '     Mode   Eigenvalue  PercentVariance' 
    FOR mode=0,m-1 DO PRINT, $ 
      mode+1, eigenvalues[mode], variances[mode]*100 
   ENDIF
end

