;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_2DLabelRegion
;
; PURPOSE:
;       Sorts and numbers areas in ascending order (1,2,...n) and returns DataMatrix
;
; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       s_2DLabelRegion, ori
;       	ori: A 2D array of object data | Objects ne to 0 are detected and numbered
;
; REQUIRED INPUTS:
;       ori
;
; OPTIONAL KEYWORD PARAMETERS:
;       _EXTRA:
;_____________________________IOISIOI____________________

function s_2DLabelRegion, ori, no_border = no_border, no_sort = no_sort, offSet = offSet

	if (max(ori) eq min (ori)) then return, ori*0

	dim_ori = size(ori, /dim)
	alpha = bytArr(dim_ori[0]+2, dim_ori[1]+2)
	alpha[1:dim_ori[0], 1:dim_ori[1]] = ori
	alpha = (label_region(alpha gt 0))[1:dim_ori[0], 1:dim_ori[1]]

	if (keyword_set(no_border)) then begin
		alpha = temporary(alpha) - 1
		alpha[where(alpha le 0)] = 0
	endif

	if ((keyword_set(no_sort)) and (max(alpha) gt 1)) then begin
		a = histogram(alpha, min = 1, reverse_indices = r)
		sortlabel  = sort(a)												; sorts Vektor: a=[7,3,4] -> sortlabel=[1,2,0]
		for i = 0, n_elements(a)-1 do alpha([r(r(i):r(i+1)-1)]) = where(sortlabel eq i)+1
	endif

	if (keyword_set(offSet)) then return, (alpha + (offSet-1)) * (alpha ne 0)
	return, alpha
end
