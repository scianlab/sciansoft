function s_3DMoments, iStack, order = order, fAreaNorm = fAreaNorm, pEigenONS = pEigenONS
               ; iStack: 3D-stack with numbered areas
               ; order:  maximum order in x and y
               ; fAreaNorm = 1, normalize with area

  ; calculates for each object:
  ; - moments of 0th order area (voxel): M000
  ; - moments of 1st order relative to [0,0,0]: M100,M010,M001 = INT INT INT x^i * y^j * z^k dx dy dz
  ; - centers of mass: xc|yc|yc = [M100,M010,M001] / area
  ; - center moments MYijk = INT INT INT (x-xc)^i*(y-yc)^j*(z-zc)^k dx dy dz  , i,j,k <= order
  ; - inertia tensor T (or covariance matrix) with MYijk:

  ;  |MY200 MY110 MY101|
  ;  |MY110 MY020 MY011| / M000 = T
  ;  |MY101 MY011 MY002|

  ; - Teta: Winkel zwischen x,y von iStack und x'y', den Hauptachsen des Objektes
  ;   : tan(2*teta) = 2 * MY11 / (MY20-MY02)
  ; - Rot./Transl. /Zoom- Invariante Momente MY' durch Transf. auf Mainachsen:
  ;  |x'|= |cos  sin| * |x|
  ;  |y'|  |-sin cos|   |y|
  ;  |z'|  |-sin cos|   |z|

  ; - 90° und 180° Ambiquitäten werden durch MY'20 > MY'02 und MY'03>0 Definition behoben.
  ; returns moments: [area, xc, yc, zc, TETA,MY'20,MY'02,MY'22,MY'30,...]

  if (n_elements(fAreaNorm) eq 0) then fAreaNorm = 0
  szOri = size(iStack)
  maxOri = max(iStack)

    ; 0th order moments (M000: areas)
    ; if iStack = [0,0,1,3,3,2,2,2] then r = [5,7,8,11,13,0,1,2,5,6,7,3,4]
  moments_0 = histogram(iStack, min = 1, max = maxOri, rev = r)
  whereNoObj = where(moments_0 eq 0, complement = whereObj, ncomplement = nObj)

    ; define return vector [area, xc, yc, zc, teta, M100, M010, M001, ...]
  moments = dblArr(nObj, 4 + order * 3)

    ; set areas
  if (maxOri ne nObj) then moments[*,0] = moments_0[whereObj] else moments[*,0] = moments_0

    ; 1st order moments in relation to [0,0,0] (M100, M010, M001)
  for i = 0,nObj-1 do moments[i,1:3] = [ total( (r[r[whereObj[i]] : r[whereObj[i]+1]-1]) mod szOri[1] ),$
                                         total( floor( ((r[r[whereObj[i]] : r[whereObj[i]+1]-1]) mod (szOri[1]*szOri[2])) / (1.*szOri[1]) ) ),$
                                         total( floor( (r[r[whereObj[i]] : r[whereObj[i]+1]-1]) / (1.*szOri[1]*szOri[2]) ) )]
    ; set centers of mass [xc, yc, zc]
  moments[*,1] /= moments[*,0]
  moments[*,2] /= moments[*,0]
  moments[*,3] /= moments[*,0]

  if (order eq 1) then return, float(moments[*,0:3])

    ; calculate center moments (MYijk) in order to define the inertia tensor T
    ; T can also be understood an the covariance matrix
    ;  |T200 T110 T101|
    ;  |T110 T020 T011| / M000
    ;  |T101 T011 T002|

  T = make_array(3, 3, /double)
  for i = 0,nObj-1 do begin
     MYx = ((r[r[whereObj[i]] : r[whereObj[i]+1]-1]) mod szOri[1]) - moments[i,1]
     MYy = floor( ((r[r[whereObj[i]] : r[whereObj[i]+1]-1]) mod (szOri[1]*szOri[2])) / (1.*szOri[1]) ) - moments[i,2]
     MYz = floor( (r[r[whereObj[i]] : r[whereObj[i]+1]-1]) / (1.*szOri[1]*szOri[2]) ) - moments[i,3]

     T[0,0] = total(MYx*MYx)
     T[1,0] = total(MYx*MYy)
     T[2,0] = total(MYx*MYz)
     T[0,1] = T[1,0]
     T[1,1] = total(MYy*MYy)
     T[2,1] = total(MYy*MYz)
     T[0,2] = T[2,0]
     T[1,2] = T[2,1]
     T[2,2] = total(MYz*MYz)

     T /= moments[i,0]

      ; now look for the diagonal matrix T' = R^-1 T R with R^-1 = tranpose(R)
     eVals = eigenQL(T, /double, eigenvectors = eVects, residual = var)

  endfor

    ; now look for T x = lam x, (eigenvectors: x = [x1,x2,x3], eigenvalues: lam)
    ; solve (T - E lam) x = 0, or the characteristic equation DET|T - E lam| = 0
    ; test with http://www.arndt-bruenner.de/mathe/scripts/eigenwert.htm




;               ; Bestimmung von TETA [RAD]
;  a = where( (moments[*,7]-moments[*,8]) ne 0) ; Verhindert DIV 0 ERROR
;  if (a[0] ne -1) then moments[a,3] = 0.5 * atan( (2.0*moments[a,6] / (moments[a,7]-moments[a,8])) )
;  a = where( (moments[*,7]-moments[*,8]) eq 0)
;  if (a[0] ne -1) then moments[a,3] = ( (moments[a,6] ne 0) - 2 * (moments[a,6] lt 0) ) * 45.0 * !DTOR
;
;  ;  Mainachsentransformation mit Bedingung:  MY20<MY02 um 90° Tetainvarianz aufzulösen
;  for i = 0,nObj-1 do begin      ; yc -> Schwerpunktshauptachse
;     x = double((r( r[whereObj[i]] : r[whereObj[i]+1]-1)) mod szOri[1]) - moments(i,1)
;     y = 1. * floor( 1. *  (r( r[whereObj[i]] : r[whereObj[i]+1]-1)) / szOri[1] ) - moments(i,2)
;     if (moments[i,7] le moments[i,8]) then begin
;        xc = x * cos(moments[i,3]) + y * sin(moments[i,3])
;        yc = y * cos(moments[i,3]) - x * sin(moments[i,3])    ; ROT(-TETA)
;        moments[i,3] = - moments[i,3]
;     endif
;     if (moments[i,7] gt moments[i,8]) then begin
;        xc = -1. * y * cos(moments[i,3]) + x * sin(moments[i,3]) ; xc= -y|
;        yc =  1. * x * cos(moments[i,3]) + y * sin(moments[i,3]) ; yc= x |ROT(-TETA)+ROT(90°)
;        moments[i,3] = !DPI/2. - moments[i,3]
;     endif
;     if (total(yc*yc*yc) lt 0) then begin    ; für MY03 < 0:  ROT(180°)
;        xc = -(xc)
;        yc = -(yc)
;        moments[i,3] = moments[i,3] + !DPI
;     endif
;     for k=2,order do begin
;        b = xc^k
;        bb= yc^k
;        if fAreaNorm then begin   ; Wenn Zoominvarianz gewünscht,
;           b /= moments[i,0]^k    ; dann Flächennormierung.
;           bb /= moments[i,0]^k
;        endif
;        moments[i,3*k+1] = total(b)     ;(Zoom)- /Rot- & Translationsinvariante
;        moments[i,3*k+2] = total(bb)    ; momente 'k'ter Ordnung.
;        moments[i,3*k+3] = total(b*bb)
;     endfor
;  endfor
;
;  moments = float(moments)
;  moments[* , 4 : 3+(order-1)*3] = moments[* , 7 : 6+(order-1)*3]
;  moments = moments[* , 0 : 3+(order-1)*3]
;  moments[*,3] = moments[*,3]  * !RADEG   ; TETA: RAD --> DEG
;  ;moments = (ABS(moments) gt 0.001) * moments
  return, moments
end