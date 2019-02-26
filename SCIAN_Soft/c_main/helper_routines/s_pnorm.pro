;_____________________________IOISIOI____________________
; NAME:
;      s_pnorm
;
; PURPOSE:
;       - Calculation of Norm P in vectors or matrices
;         p=0 => Infinity norm
;         p=2 => Frobenius norm
; AUTHOR:
;   Héctor Moraga (2011)
;   e_mail: hector.moraga.aros@gmail.com
;
; CALLING SEQUENCE:
;       result = s_pnorm(array,p)
;
; 
;_____________________________IOISIOI____________________

function s_pnorm, arreglo, p

   acumulador=0
   ;Norm for vectors
   if (size(arreglo, /n_dim) eq 1) then begin
      return, norm(arreglo, LNORM=p)
   endif
   if (size(arreglo, /n_dim) eq 2) then begin
   ;Norm for Matrices 
      if ((p eq 0) or (p eq 1) or (p eq 2)) then begin
         return, norm(arreglo, /DOUBLE, LNORM=p)
      endif else begin
         return, (total((abs(double(arreglo)))^p))^(1.0/p) 
      endelse
   endif      
   
end   
