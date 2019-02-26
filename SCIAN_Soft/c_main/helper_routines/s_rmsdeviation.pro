;_____________________________IOISIOI____________________
; NAME:
;      s_RMSDeviation
;
; PURPOSE:
;       - Root Mean Square Deviation of 2 numerical matrices (images) or vectors
;
; AUTHOR:
;   Héctor Moraga (2011)
;   e_mail: hector.moraga.aros@gmail.com
;
; CALLING SEQUENCE:
;       result = C_sRMSDev(m1, m2)
;
;_____________________________IOISIOI____________________
function s_RMSDeviation, m1, m2
  if ((size(m1, /n_dim) ne 2) or (size(m2, /n_dim) ne 2)) then begin
    print, 's_RMSDeviation : Error, 2D matrices required'
    return, -1
  endif
  if (((size(m1, /dim))[0]) ne ((size(m2, /dim))[0])) or (((size(m1, /dim))[1]) ne ((size(m2, /dim))[1])) then begin
    print, 's_RMSDeviation : Error, the size in the input matrixes must be the same'
    return, -1
  endif
  return, sqrt(total((m2 - m1)^2) / (((size(m1, /dim))[0]) * ((size(m1, /dim))[1])))
end
