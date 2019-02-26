;_____________________________IOISIOI____________________
; NAME:
;      s_ObjBorder_4or8
;
; PURPOSE:
;       - Finds and returns borders of numbered areas in ori and sets them = 1 in result.
;		- Borders are detected, following the 4- or 8-neigborhood rule.
;
; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = s_ObjBorder_4or8(ori)
;					  ori: 2D-Matrix with numbered areas
;_____________________________IOISIOI____________________

function s_ObjBorder_4or8, ori, four = four, eight = eight

	dim_ori  = size(ori, /dim)

		; Enlarge picture dimension
	erw_ori = make_array( dim_ori[0]+2, dim_ori[1]+2, type = size(ori, /type))

		; Fill center with original picture
	erw_ori[ 1:dim_ori[0], 1:dim_ori[1]] = ori[*,*]

	if (n_elements(four) ne 0) then return, (ori ne 0) * $
				 (( erw_ori( 0:dim_ori[0]-1, 1:dim_ori[1] ) ne ori) or $
				  ( erw_ori( 2:dim_ori[0]+1, 1:dim_ori[1] ) ne ori) or $
				  ( erw_ori( 1:dim_ori[0] ,   0:dim_ori[1]-1 ) ne ori) or $
				  ( erw_ori( 1:dim_ori[0] ,   2:dim_ori[1]+1 ) ne ori)) else $
		return, (ori ne 0) * $
				   (( erw_ori[ 0:dim_ori[0]-1 ,   1:dim_ori[1]] ne ori) or  $
					( erw_ori[ 2:dim_ori[0]+1 ,   1:dim_ori[1]] ne ori) or $
					( erw_ori[ 1:dim_ori[0] , 		0:dim_ori[1]-1] ne ori) or  $
					( erw_ori[ 1:dim_ori[0] , 		2:dim_ori[1]+1] ne ori) or $
					( erw_ori[ 0:dim_ori[0]-1 ,   0:dim_ori[1]-1] ne ori) or $
					( erw_ori[ 0:dim_ori[0]-1 ,   2:dim_ori[1]+1] ne ori) or $
					( erw_ori[ 2:dim_ori[0]+1 ,   0:dim_ori[1]-1] ne ori) or  $
					( erw_ori[ 2:dim_ori[0]+1 ,   2:dim_ori[1]+1] ne ori))
end