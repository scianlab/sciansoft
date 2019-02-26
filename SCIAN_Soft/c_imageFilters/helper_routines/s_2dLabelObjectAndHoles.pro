;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_2DLabelObjectAndHoles
;
; PURPOSE:
;       Sorts Object (1) and Holes (2...n) in ascending order (1,2,...n) and returns DataMatrix
;
; AUTHOR:
;		Dr. Steffen Härtel (2003)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       s_2DLabelRegion, ori
;       	ori: A 2D array of object data
;
; REQUIRED INPUTS:
;       ori
;_____________________________IOISIOI____________________


function s_2DLabelObjectAndHoles, ori
	dim_ori = size(ori, /dim)
	alpha = bytArr(dim_ori[0]+2, dim_ori[1]+2)
	alpha[1:dim_ori[0], 1:dim_ori[1]] = ori

	alphaHoles = label_region(alpha eq 0)
	whereObjEmbed = where(alphaHoles eq alphaHoles[1,1])
	if (whereObjEmbed[0] ne -1) then alphaHoles[whereObjEmbed] = 0

	return, alphaHoles[1:dim_ori[0], 1:dim_ori[1]] + (ori ne 0)
end
