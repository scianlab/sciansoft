function s_objBorder_chainCode, ori		; ori:	Bild mit nummerierten Rändern.
;											  |3  2  1|
; 		ordnet jedem Randpunkt X: |4  X  0| die Richtung seines nächsten Nachbarpunktes zu.
;											  |5  6  7|

;- Anfangspunkt (x(i),y(i)) jedes Randes i ist (Min(x EQ i),Min(y EQ i)).
;- Nur der äußere Rand wird umfahren.
;- Übergibt: Chain-Code Matrix (#, x/y/code...)
; Feb|98 Steffen

	dim_ori  = size(ori, /dim)
	erw_ori = intArr(dim_ori[0]+2 , dim_ori[1]+2)
	erw_ori[1:dim_ori[0], 1:dim_ori[1]]  = ori[*,*]
	borderHist = (histogram(ori))[1:*]				; border-pixel histogram w/o 0-area
	borderWhere = where(borderHist ne 0)
	code = make_array(n_elements(borderWhere), 2*(max(borderHist)+2), /int, value = -1)	; Chain-Code Matrix (#, x/y/code...)

	maxj = 2
	s =[0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5]
	x =[1, 1, 0, -1,-1, -1, 0, 1]
	y =[0, 1, 1,  1, 0, -1,-1,-1]
	r =[6, 6 ,0, 0, 2, 2, 4, 4]

	for i = 0, n_elements(borderWhere)-1 do begin

		a = (where(ori eq (borderWhere[i]+1)))[0]
		code[i,0:1] = [ a MOD dim_ori[0], floor(1.0*a/dim_ori[0])]
		x_run = code[i,0] + 1
		y_run = code[i,1] + 1

		for j=3,0,-1 do begin
			if ( erw_ori[x_run+x[j], y_run+y[j]] eq (borderWhere[i]+1)) then begin
				code[i,2] = j
				richtplus = j
				rechts = r[j]
			endif
		endfor

		if (code[i,2] EQ -1) then richtplus = -1

		j = 2
		while (code[i,0] NE x_run-1) OR $
				(code[i,1] NE y_run-1) OR $
				(code[i,j>3] NE richtplus) do begin
			x_run = temporary(x_run) + x[code[i,j]]
			y_run = temporary(y_run) + y[code[i,j]]
			a = rechts+1
			j = j+1
			while (a lt (rechts+8)) do begin
				if ( erw_ori[x_run + x[s[a]], y_run+y[s[a]]] eq (borderWhere[i]+1)) then begin
					code[i,j] = s[a]
					rechts = r[s[a]]
					a = 20
				endif
			a = a + 1
			endwhile
		endwhile
		code[i,j] = -1
		maxj = j > maxj
	endfor

	return, code[*, 0:maxj]
end