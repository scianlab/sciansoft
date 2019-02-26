;_____________________________IOISIOI____________________
; NAME:
;      s_getRightNumberFromSting
;
; PURPOSE:
;       - isolates sequence numbers from right side of a string and returns this number as value (long)
;
; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________


function s_countRightNumberFromString, searchString
	number = 0
	strLenStr = strLen(searchString)
	for i = 0, strLenStr-1 do begin
		case strMid(searchString, strLenStr-1-i,1) of
			'0':number = temporary(number) + 1
			'1': number = temporary(number) + 1
			'2': number = temporary(number) + 1
			'3': number = temporary(number) + 1
			'4': number = temporary(number) + 1
			'5': number = temporary(number) + 1
			'6': number = temporary(number) + 1
			'7': number = temporary(number) + 1
			'8': number = temporary(number) + 1
			'9': number = temporary(number) + 1
			else: return, number
		endcase
	endfor
	return, number
end
