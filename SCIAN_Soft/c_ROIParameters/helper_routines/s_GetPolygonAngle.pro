;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_GetPolygonAngle
;
; PURPOSE:
;       Calculates and returns angle alfa = [0,360] between 2 connected Polygons a and b counterclockwise.
;
; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      alfa = s_GetPolygonAngle(a,b)
;
; REQUIRED INPUTS:
;      a, b
;	   for:  a = [1,0]
;			   b = [ [0,0], [-1,-1], [0,-1], [1,-1], [1,0], [1,1], [0,1], [-1,1], [-1,0]]
; returns:      0.000000      45.0000      90.0000      135.000      180.000      225.000      270.000      315.000      360.000
;_____________________________IOISIOI____________________


function s_GetPolygonAngle, a, b

	if ( (Size(a, /Dim))[0] ne (Size(b, /Dim))[0] ) then begin
		print, 'Dimensions of vectors must agree !'
		return, -1.
	endif

		; length_x = sqrt(total(x*x))
	length_ab = sqrt(total(a*a)) * sqrt(total(b*b))

	if (length_ab eq 0) then begin
		print, '0-Vector !!'
		return, -1.
	endif

	if ( (a[0]*b[1] lt a[1]*b[0])) then return,  180. - acos( (total(a*b) / length_ab)<1.>(-1.) ) * !radeg $
		else return,  180. + acos( (total(a*b) / length_ab)<1.>(-1.) ) * !radeg
end


;pro z
;a = [1,0]
;b = [ [0,0], [-1,-1], [0,-1], [1,-1], [1,0], [1,1], [0,1], [-1,1], [-1,0]]
;al = 0
;tot = 0
;kreuz = 0
;for i = 0, 8 do begin
;	bb = bytarr(2)+b[*,i]
;	alfa = s_GetPolygonAngle( a , bb )
;	al = [al,alfa]
;	tot = [tot, total(a*bb)]
;	kreuz = [kreuz, 1.*a[0]*bb[1]-a[1]*bb[0]]
;endfor
;print, al[1:*]
;print, tot[1:*]
;print, kreuz[1:*]
;end
