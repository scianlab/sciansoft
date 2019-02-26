;_____________________________IOISIOI____________________
; NAME:
;      s_getLeftNumberFromStringAsString
;
; PURPOSE:
;
; AUTHOR:
;   Dr. Steffen Härtel (2002)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________


function s_getLeftNumberFromStringAsString, strName
   strLenStr = strlen(strName)
   if (strLenStr gt 0) then begin
     str = '0'
     for i = 0, strLenStr-1 do begin
      case strMid(strName, i,1) of
         '0': str += '0'
         '1': str += '1'
         '2': str += '2'
         '3': str += '3'
         '4': str += '4'
         '5': str += '5'
         '6': str += '6'
         '7': str += '7'
         '8': str += '8'
         '9': str += '9'
         else: if (strlen(str) gt 1) then return, strCompress(strMid(str, 1), /rem) else return, -1
      endcase
     endfor
     if (strlen(str) gt 1) then return, strCompress(strMid(str, 1), /rem) else return, -1
   endif
   return, -1
end