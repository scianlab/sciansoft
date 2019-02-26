; CHECK !!!!!!!!!!!!!!!! DEPRECATED FUNCTION::::  FASL
; Please kill this... 
; Third time .. I tried to set it as deprecated .. but someone recovery status... 
; TOday .. compilation order provided me this "deprecated" function.... several houres lost!!!!!!!!!!!!!!!  

function s_2Dmoments_OLD, image, order = order, fAreaNorm = fAreaNorm

               ; image:  Originalbild mit nummerierten Flächen
               ; order:  Maximale Ordnung von x und y
               ; fAreaNorm: = 1, dann Flächennormierung
  ; Berechnet von zusammenhängenden 2D-Objekten in image:
  ; - Flächen (Pixelzahl=i)
  ; - Momente erster Ordnung m_one rel. zu [0,0]: Mjk = INT INT x^j * y^k dx dy
  ; - Massenschwerpunkte: xs|ys= m_one(i) / Fläche(i)
  ; - Schwerpunktsmomentse der Ordnung (j,k<=order): MYjk = INT INT (x-xs)^j*(y-ys)^k dx dy
  ; - Teta: Winkel zwischen x,y von image und x'y', den Mainachsen des Objektes
  ;   : tan(2*teta) = 2*MY11/(MY20-MY02)
  ; - Rot./Transl. /Zoom- Invariante Momente MY' durch Transf. auf Mainachsen:
  ;  |x'|= |cos  sin| * |x|
  ;  |y'|  |-sin cos|   |y|
  ; - 90° und 180° Ambiquitäten werden durch MY'20 > MY'02 und MY'03>0 Definition behoben.
  ; Es wird übergeben: [Fläche,xs,yx,TETA,MY'20,MY'02,MY'22,MY'30,...]
  ; Feb'98 Steffen

  if (n_elements(fAreaNorm) eq 0) then fAreaNorm = 0
  szOri = size(image)
  maxOri = max(image)

    ; Ergebenismatrix: (#Fl. , ...) [Fläche,x_schwpkt,y_schwpkt,teta,MY10,MY01,MY11,..]
  moments = dblArr(maxOri, 4 + order * 3)

  m_one = lonArr(maxOri,2)     ; if image =[0,0,1,3,3,2,2,2] then r =[5,7,8,11,13,0,1,2,5,6,7,3,4]

  moments[*,0] = histogram(image, min = 1, max = maxOri, rev = r)   ; MY00: 0'th order = Areas

     ; Momente 1'ter Ordnung zu [0,0]
  for i=0,maxOri-1 do m_one(i,0:1) = [ total(  (r( r[1] : r[i+1]-1 )) mod szOri[1]) ,  total( floor(1. *  (r( r[1] : r[i+1]-1 )) / szOri[1]) ) ]

     ; Massenschwerpunkte
  moments[*,1] = 1. * m_one[*,0]/moments[*,0]
  moments[*,2] = 1. * m_one[*,1]/moments[*,0]

  if (order eq 1) then return, float(moments(*,0:2))

  for i=0,maxOri-1 do begin
     a = double((r[ r[1] : r[i+1]-1]) mod szOri[1]) - moments[i,1]
     aa= 1. * floor( 1. *  (r( r[1] : r[i+1]-1)) / szOri[1] ) - moments[i,2]
  ;  moments(i,4) = total(a)      ; Schwerpunktsmomente 1'ter Ordnung
  ;  moments(i,5) = total(aa)
     moments[i,6] = total(a*aa)
     moments[i,7] = total(a*a)    ; Schwerpunktsmomente 2'ter Ordnung
     moments[i,8] = total(aa*aa)
  ;  moments(i,9) = total(a*a*aa*aa)
  endfor
               ; Bestimmung von TETA [RAD]
  a = where( (moments[*,7]-moments[*,8]) ne 0) ; Verhindert DIV 0 ERROR
  if (a[0] ne -1) then moments[a,3] = 0.5 * atan( (2.0*moments[a,6] / (moments[a,7]-moments[a,8])) )
  a = where( (moments[*,7]-moments[*,8]) eq 0)
  if (a[0] ne -1) then moments[a,3] = ( (moments[a,6] ne 0) - 2 * (moments[a,6] lt 0) ) * 45.0 * !DTOR

  ;  Mainachsentransformation mit Bedingung:  MY20<MY02 um 90° Tetainvarianz aufzulösen
  for i=0,maxOri-1 do begin      ; ys -> Schwerpunktshauptachse
     x = double((r( r[1] : r[i+1]-1)) mod szOri[1]) - moments(i,1)
     y = 1. * floor( 1. *  (r( r[1] : r[i+1]-1)) / szOri[1] ) - moments(i,2)
     if (moments[i,7] le moments[i,8]) then begin
        xs = x * cos(moments[i,3]) + y * sin(moments[i,3])
        ys = y * cos(moments[i,3]) - x * sin(moments[i,3])    ; ROT(-TETA)
        moments[i,3] = - moments[i,3]
     endif
     if (moments[i,7] gt moments[i,8]) then begin
        xs = -1. * y * cos(moments[i,3]) + x * sin(moments[i,3]) ; xs= -y|
        ys =  1. * x * cos(moments[i,3]) + y * sin(moments[i,3]) ; ys= x |ROT(-TETA)+ROT(90°)
        moments[i,3] = !DPI/2. - moments[i,3]
     endif
     if (total(ys*ys*ys) lt 0) then begin    ; für MY03 < 0:  ROT(180°)
        xs = -(xs)
        ys = -(ys)
        moments[i,3] = moments[i,3] + !DPI
     endif
     for k=2,order do begin
        b = xs^k
        bb= ys^k
        if fAreaNorm then begin   ; Wenn Zoominvarianz gewünscht,
           b /= moments[i,0]^k     ; dann Flächennormierung.
           bb /= moments[i,0]^k
        endif
        moments[i,3*k+1] = total(b)     ;(Zoom)- /Rot- & Translationsinvariante
        moments[i,3*k+2] = total(bb)        ; momente 'k'ter Ordnung.
        moments[i,3*k+3] = total(b*bb)
     endfor
  endfor

  moments = float(moments)
  moments[*,4:3+(order-1)*3] = moments[*,7:6+(order-1)*3]
  moments = moments[*,0:3+(order-1)*3]
  moments[*,3] = moments[*,3]  * !RADEG   ; TETA: RAD --> DEG
  ;moments = (ABS(moments) gt 0.001) * moments
  return, moments
end