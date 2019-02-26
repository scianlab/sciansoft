;_____________________________IOISIOI____________________
; NAME:
;      s_getPSFData  (c:\rsi\Steffen\s_TimeCalc\C_sImageFilter\helper_routines\s_getPSFData.pro)
;
; PURPOSE:
;       returns PSF-Data
;   the PSF-Values have been derived with Huygens Professional Software for Sampling Distances dx/dy/dz = 40/40/100 nm
;
; AUTHOR:
;   Dr. Steffen Härtel (2005)
;   e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;   result = s_getPSFData(sPSF = sPSF)
; METHOHDS:
;_____________________________IOISIOI____________________


function s_getPSFData, sPSF = sPSF

   if (n_elements(sPSF) eq 0) then return, -1
   case sPSF of
     'Use PSF for 63Xw NA1.2 ex488 em520': return, [4.92e-3,$
                                                4.5e-3,$
                                                3.42e-3,$
                                                2.08e-3,$
                                                9.74e-4,$
                                                3.29e-4,$
                                                8.66e-5,$
                                                3.46e-5]
     'Use PSF for 63Xw NA1.2 ex543 em580': return, [3.42e-3,$
                                                3.19e-3,$
                                                2.57e-3,$
                                                1.75e-3,$
                                                9.78e-4,$
                                                4.30e-4,$
                                                1.45e-4,$
                                                4.54e-5]
     'Use PSF for 63Xoil NA1.4 ex488 em520': return, [8.27e-3,$
                                                7.33e-3,$
                                                5.01e-3,$
                                                2.51e-3,$
                                                8.49e-4,$
                                                1.94e-4,$
                                                6.08e-5,$
                                                3.82e-5]
     'Use PSF for 63Xoil NA1.4 ex543 em580': return, [5.69e-3,$
                                                5.18e-3,$
                                                3.87e-3,$
                                                2.28e-3,$
                                                1.01e-3,$
                                                3.2e-4,$
                                                8.59e-5,$
                                                4.24e-5]
     ;mcerda: dx/dy/dz = 41/41/100 nm
     'Use PSF for 160Xoil/water NA1.338 ex543 em580': return, [1.34e-3,$
                                                1.47e-3,$
                                                1.34e-3,$
                                                1.00e-3,$
                                                6.07e-4,$
                                                2.84e-4,$
                                                9.97e-05,$
                                                3.45e-05]
                                      
     else: return, -1
   endCase

   if (n_elements(sPSF) eq 0) then return, -1
   case sPSF of
     'Use PSF for 63Xw NA1.2 ex488 em520': return, [4.82e-3,$
                                                4.41e-3,$
                                                3.35e-3,$
                                                2.05e-3,$
                                                9.55e-4,$
                                                3.24e-4,$
                                                8.5e-5,$
                                                3.39e-5]
     'Use PSF for 63Xw NA1.2 ex543 em580': return, [2.82e-3,$
                                                2.67e-3,$
                                                2.15e-3,$
                                                1.46e-3,$
                                                8.17e-4,$
                                                3.59e-4,$
                                                1.21e-4,$
                                                3.8e-5]
     'Use PSF for 63Xoil NA1.4 ex488 em520': return, [6.31e-3,$
                                                5.59e-3,$
                                                3.83e-3,$
                                                1.91e-3,$
                                                6.49e-4,$
                                                1.48e-4,$
                                                4.63e-5,$
                                                2.9e-5]
     'Use PSF for 63Xoil NA1.4 ex543 em580': return, [4.68e-3,$
                                                4.26e-3,$
                                                3.18e-3,$
                                                1.88e-3,$
                                                8.32e-4,$
                                                2.63e-4,$
                                                7.07e-5,$
                                                3.48e-5]
     ;mcerda: dx/dy/dz = 41/41/100 nm
     'Use PSF for 160Xoil/water NA1.338 ex543 em580': return, [1.34e-3,$
                                                1.47e-3,$
                                                1.34e-3,$
                                                1.00e-3,$
                                                6.07e-4,$
                                                2.84e-4,$
                                                9.97e-05,$
                                                3.45e-05]
                                                
                                                
     else: return, -1
   endCase
end