;_____________________________IOISIOI____________________
; NAME:
;      s_Gauss  (rsi\steffen\s_Time_Calc\C_sImageFilter\helper_routines\s_Poisson.pro)
;
; PURPOSE:
;       - calculate Poisson-Probability Density Function (PDF)
;
; AUTHOR:
;		Dr. Steffen Härtel (2005)
;		e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;      	result = s_Poisson( xVect = xVect, a = a)
;		xVect:	Vector for x-coordinated
;		a:			 µ = mean = variance = sd^2
;
;		see Gonzales & Woods, 'Digital Image Processing', Chapter 5
; METHOHDS:
;_____________________________IOISIOI____________________

function s_Poisson, xVect = xVect, a = a

	if (n_elements(xVect) eq 0) then xVect = [0.,1.,2.,3.]
	if (n_elements(a) eq 0) then a = 1.

	z = exp(-a) *double(a)^(xVect) / factorial(xVect)
	
	whZ = where(finite(z) eq 0, count)
	if count gt 0 then z[whZ] = 0.
	return, z / total(z)
end