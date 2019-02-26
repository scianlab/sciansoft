;_____________________________IOISIOI____________________
; NAME:
;      s_getRightNumberFromSting
;
; PURPOSE:
;
; AUTHOR:
;   Dr. Steffen Härtel (2002)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________


function s_getRightNumberFromString, strName
  strLenStr = strlen(strName)
  if (strLenStr gt 0) then begin
   n = 0l
   for i = 0, strLenStr-1 do begin
     case strMid(strName, strLenStr-1-i,1) of
     '0':
     '1': n += 1 * (10l^i)
     '2': n += 2 * (10l^i)
     '3': n += 3 * (10l^i)
     '4': n += 4 * (10l^i)
     '5': n += 5 * (10l^i)
     '6': n += 6 * (10l^i)
     '7': n += 7 * (10l^i)
     '8': n += 8 * (10l^i)
     '9': n += 9 * (10l^i)
     else: if (i eq 0) then return, -1 else return, n
     endcase
   endfor
   return, n
  endif
  return, -1
end
