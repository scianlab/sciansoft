                  ; ori:  Bildmaske mit nummerierten Flächen
                  ; grau: Originalbild mit Grauwerten
                  ; ord:  Maximale Ordnung von x und y
  ; Berechnet von zusammenhängenden 2D-Objekten in ori:
  ; - Gesamtintensität (Summe der Grauwerte)
  ; - Momente erster Ordnung M10 M01 rel. zu [0,0]: Mjk = INT INT x^j * y^k I(x,y) dx dy
  ; - 'Massenschwerpunkte': xs|ys= M10|M01(i) / Gesamtintensität(i)
  ; - Schwerpunktsmomentse der Ordnung (j,k<=ord): MYjk = INT INT (x-xs)^j*(y-ys)^k dx dy
  ; - Teta: Winkel zwischen x,y von ori und x'y', den Mainachsen des Objektes
  ;   : tan(2*teta) = 2*MY11/(MY20-MY02)
  ; - Rot./Translations Invariante /Zentrale) Momente MY' durch Transf. auf Mainachsen:
  ; |x'|= |cos  sin| * |x|
  ; |y'|  |-sin cos|   |y|
  ; - 90° und 180° Ambiquitäten werden durch MY'20 > MY'02 und MY'03>0 Definition behoben.
  ; Es wird übergeben: [Fläche,xs,yx,TETA,MY'20,MY'02,MY'22,MY'30,...]
  ;
  ; Feb'98 Steffen
function s_apop_moments_grau, ori, grau, ord

	dim_ori = size(ori)
	max_ori = max(ori)

  ; Ergebenismatrix:
  ; (#Fl., [Fläche, x_schwpkt, y_schwpkt,. TETA, Intens., MY10, MY01, MY11,..]
	moments = dblArr(max_ori, 5 + ord * 3)
	M10 = lonArr(max_ori)
	M01 = M10

	moments[*,0] = histogram(ori, min = 1, max = max_ori, rev = r) ;Flächen

	for i = 0, max_ori-1 do begin ; Momente 1'ter Ordnung zu [0,0]
		i_indis = r[r[i] : r[i+1]-1]
		xgrau = double(i_indis mod dim_ori[1])
		ygrau = floor(1.0 * i_indis / dim_ori[1])
		moments[i,4] = total(grau[xgrau, ygrau])
		M10[i] = total(xgrau * grau[xgrau, ygrau])
		M01[i] = total(ygrau * grau[xgrau, ygrau])
	endfor

	moments[*,1] = M10 / moments[*,4]	; 'Massenschwerpunkte'
	moments[*,2] = M01 / moments[*,4]
	;print, M10, M01, moments[*,4]

	for i = 0, max_ori-1 do begin
		i_indis = r[r[i] : r[i+1]-1]
		a  = double(i_indis mod dim_ori[1]) - moments[i,1]
		aa = floor(1.0 * i_indis / dim_ori[1]) - moments[i,2]
		moments[i,6] = total( a*aa * grau[xgrau, ygrau]) ; Schwerpunktsmomente MY11
		moments[i,7] = total( a*a  * grau[xgrau, ygrau]) ; Schwerpunktsmomente MY20 und MY02
		moments[i,8] = total(aa*aa * grau[xgrau, ygrau])
	endfor
	  ; Bestimmung von TETA [RAD]
	a = where((moments[*,7]-moments[*,8]) ne 0)	; Verhindert DIV 0 ERROR
	if (a[0] ne -1) then moments[a,3] = 0.5 * atan((2.0*moments[a,6] / (moments[a,7]-moments[a,8])))

	a = where((moments[*,7]-moments[*,8]) eq 0)
	if (a[0] ne -1) then moments[a,3] = ((moments[a,6] ne 0) - 2 * (moments[a,6] lt 0)) * 45.0 * !DTOR

		; Mainachsentransformation mit Bedingung:
		; MY'20<MY'02 um 90° Tetainvarianz aufzulösen
	for i = 0, max_ori-1 do begin	; ys -> Schwerpunktshauptachse
		i_indis = r[r[i] : r[i+1]-1]
		x = double(i_indis mod dim_ori[1]) - moments[i,1]
		y = floor(1.0 * i_indis / dim_ori[1]) - moments[i,2]
		if (moments[i,7] le moments[i,8]) then begin
			xs = x * cos(moments[i,3]) + y * sin(moments[i,3])
			ys = y * cos(moments[i,3]) - x * sin(moments[i,3]) ; ROT(-TETA)
			moments[i,3] = -temporary(moments[i,3])
		endif
		if (moments[i,7] gt moments[i,8]) then begin
			xs = -1.0 * y * cos(moments[i,3]) + x * sin(moments[i,3]); xs= -y|
			ys =  1.0 * x * cos(moments[i,3]) + y * sin(moments[i,3]) ; ys= x |ROT(-TETA)+ROT(90°)
			moments[i,3] = !DPI/2 - moments[i,3]
		endif
		if (total(ys*ys*ys) lt 0) then begin ; für MY03 < 0:  ROT(180°)
			xs = -(xs)
			ys = -(ys)
			moments[i,3] += !DPI
		endif
		for k = 2, ord do begin
			b  = xs^k
			bb = ys^k
			moments[i, 3*k+1] = total(   b * grau[xgrau, ygrau]) ;(Zoom)-/Rot-& Translationsinvariante
			moments[i, 3*k+2] = total(  bb * grau[xgrau, ygrau]) ; Momente 'k'ter Ordnung.
			moments[i, 3*k+3] = total(b*bb * grau[xgrau, ygrau])
		endfor
	endfor
	moments = float(moments)
	moments[*, 4:3+(ord-1)*3] = moments[*, 7:6+(ord-1)*3]
	moments = moments[*,0:3+(ord-1)*3]
	moments[*,3] = moments[*,3] * !RADEG ; TETA: RAD --> DEG
	;moments *= (abs[moments] gt 0.001)
	return, moments
end
