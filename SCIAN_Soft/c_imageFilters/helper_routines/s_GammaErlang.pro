;_____________________________IOISIOI____________________
; NAME:
;      s_Gauss  (rsi\steffen\s_Time_Calc\C_sImageFilter\helper_routines\s_GammaErlang.pro)
;
; PURPOSE:
;       - calculate GammaErlang-Probability Density Function (PDF)
;
; AUTHOR:
;		Dr. Steffen Härtel (2005)
;		e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;      	result = s_GammaErlang( xVect = xVect, a = a, b = b )
;		xVect:	Vector for x-coordinated
;		a:	mean / variance
;		b:	mean^2 / variance
;		mean = b/a
;		variance = b/a^2
;
;		see Gonzales & Woods, 'Digital Image Processing', Chapter 5
; METHOHDS:
;_____________________________IOISIOI____________________

function s_GammaErlang, xVect = xVect, a = a, b = b

	if (n_elements(xVect) eq 0) then xVect = [0.,1.,2.,3.]
	if (n_elements(b) eq 0) then b = 1.d else b = double( b > 1.e-5)
	if (n_elements(a) eq 0) then a = 1.d else a = double(a)

	return, ( (a^b / factorial(b-1))  * xVect^(b-1) * exp(-(a*xVect)) )  * (xVect ge 0)
end