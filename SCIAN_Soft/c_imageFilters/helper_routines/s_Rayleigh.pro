;_____________________________IOISIOI____________________
; NAME:
;      s_Gauss  (rsi\steffen\s_Time_Calc\C_sImageFilter\helper_routines\s_Rayleigh.pro)
;
; PURPOSE:
;       - calculate Rayleigh-Probability Density Function (PDF)
;
; AUTHOR:
;		Dr. Steffen Härtel (2005)
;		e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;      	result = s_Rayleigh( xVect = xVect, a = a, b = b )
;		µ/mean:		a + sqrt(pi*b/4)
;		variance:	b(4-pi)/4
;
;		see Gonzales & Woods, 'Digital Image Processing', Chapter 5
; METHOHDS:
;_____________________________IOISIOI____________________

function s_Rayleigh, xVect = xVect, a = a, b = b

	if (n_elements(xVect) eq 0) then xVect = [0.,1.,2.,3.]
	if (n_elements(b) eq 0) then b = 1. else b = b > 1.e-5
	if (n_elements(a) eq 0) then a = 1.

	return, ( (2./b) * (xVect-a) * exp(-(xVect-a)^2 / b ) )  * (xVect ge a)
end