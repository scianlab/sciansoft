FUNCTION s_rand4, ori			; ori:	Bild mit nummerierten Flächen
; s_rand4 ordnet jedem Pixel >0 in ori: 1, wenn es mindestens einen geraden,
; nicht zur Fläche gehörigen Nachbarpunkt hat.
; Jan|98 Steffen
dim_ori  = SIZE(ori)
rand = BYTARR(dim_ori(1),dim_ori(2))			; Ergebnismatrix
erw_ori = INTARR(dim_ori(1)+2,dim_ori(2)+2)		; Erweitern der Bildmatrix
erw_ori(1:dim_ori(1),1:dim_ori(2)) = ori(*,*) 	; Zentrum gleich Originalbildmatrix
rand =( erw_ori( 0:dim_ori(1)-1 , 1:dim_ori(2) ) NE ori) OR $
	  ( erw_ori( 2:dim_ori(1)+1 , 1:dim_ori(2) ) NE ori) OR $
	  ( erw_ori( 1:dim_ori(1) , 0:dim_ori(2)-1 ) NE ori) OR $
	  ( erw_ori( 1:dim_ori(1) , 2:dim_ori(2)+1 ) NE ori)
RETURN, rand * (ori gt 0)
END