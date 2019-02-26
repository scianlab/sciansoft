;_____________________________IOISIOI____________________
; NAME:
;      s_Gauss  (rsi\steffen\s_Time_Calc\C_sImageFilter\helper_routines\s_Exponential.pro)
;
; PURPOSE:
;       - calculate s_Exponential-Probability Density Function (PDF)
;
; AUTHOR:
;		Dr. Steffen Härtel (2005)
;		e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;      	result = s_Exponential( xVect = xVect, a = a, b = b )
;		xVect:	Vector for x-coordinated
;		a:			1 /  mean = 1 / sqrt(variance) = 1 / sd
;		mean:	sqrt(variance) = sd
;
;		see Gonzales & Woods, 'Digital Image Processing', Chapter 5
; METHOHDS:
;_____________________________IOISIOI____________________

function s_Exponential, xVect = xVect, a = a

	if (n_elements(xVect) eq 0) then xVect = [0.,1.,2.,3.]
	if (n_elements(a) eq 0) then a = 1.d else a = double(a)

	z = a * exp(-a * xVect) * (xVect ge 0)
	return, z / total(z)
end