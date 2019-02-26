;_____________________________IOISIOI____________________
; NAME:
;      s_Gauss  (c:rsi\steffen\s_Time_Calc\C_sImageFilter\helper_routines\s_Uniform.pro)
;
; PURPOSE:
;       - calculate s_Uniform-Probability Density Function (PDF)
;
; AUTHOR:
;     Dr. Steffen Härtel (2005)
;     e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;       result = s_Uniform( xVect = xVect, a = a, b = b )
;     xVect:     Vector for x-coordinated
;     a:      lower boundary
;     b:      upper boundary
;     µ/mean:   (a+b)/2
;     variance:    (b-a)^2/12
;
;     see Gonzales & Woods, 'Digital Image Processing', Chapter 5
; METHOHDS:
;_____________________________IOISIOI____________________

function s_Uniform, xVect = xVect, a = a, b = b

    if (n_elements(xVect) eq 0) then xVect = [0.,1.,2.,3.]
    if (n_elements(a) eq 0) then a = 1.
    if (n_elements(b) eq 0) then b = 2.

    z = 1. * (xVect ge (a < b)) * (xVect le (a > b)) / (((a > b) - (a < b)) > 1.e-5)
    return, z / total(z)
end