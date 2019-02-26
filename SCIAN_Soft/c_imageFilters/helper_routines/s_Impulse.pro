;_____________________________IOISIOI____________________
; NAME:
;      s_Gauss  (rsi\steffen\s_Time_Calc\C_sImageFilter\helper_routines\s_Impulse.pro)
;
; PURPOSE:
;       - calculate s_Impulse-Probability Density Function (PDF)
;
; AUTHOR:
;		Dr. Steffen Härtel (2005)
;		e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;      	result = s_Impulse( xVect = xVect, a = a, b = b )
;		xVect:	Vector for x-coordinated
;		a:			 µ = mean
;		b:			 variance = sd^2
;
;		see Gonzales & Woods, 'Digital Image Processing', Chapter 5
; METHOHDS:
;_____________________________IOISIOI____________________

function s_Impulse, xVect = xVect, a = a, b = b, pa = pa, bp = pb

	if (n_elements(xVect) eq 0) then xVect = [0.,1.,2.,3.]
	if (n_elements(a) eq 0) then a = 1.
	if (n_elements(b) eq 0) then b = 2.
	if (n_elements(pa) eq 0) then pa = 1./3.
	if (n_elements(pb) eq 0) then pb = 2./3.

	return, ((xVect eq a) * pa) + ((xVect eq b) * pb)
end