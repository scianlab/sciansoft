;_____________________________IOISIOI____________________
; NAME:
;      s_scaleArray
;
; PURPOSE:
;

; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________


function s_scaleArray, array, minValue = minValue, maxValue = maxValue, topValue = topValue
	if (n_elements(array) eq 0) then array = -1
	if (n_elements(minValue) eq 0) then minValue = min(array)
	if (n_elements(maxValue) eq 0) then maxValue = min(array)
	if (n_elements(topValue) eq 0) then topValue = maxP
	return, (array - minValue) * (topValue / ((maxValue - minValue)>1.))
end