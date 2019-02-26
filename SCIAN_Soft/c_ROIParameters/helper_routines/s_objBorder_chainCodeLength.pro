;_____________________________IOISIOI____________________
; FUNCTION NAME:
;      s_objBorder_chainCodeLength
;
; PURPOSE:
;       - Calculation of chain code length from Vossepoel
;		(Vossepoel, A.M. and A.W.M. Smeulders. 1982. Vector code probabilities and metrication error in the presentation of straight lines of finite lenght. Computer Graphics and Image Processing,  20: 347-364.)
;
; AUTHOR:
;		Dr. Steffen Härtel (2003)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result =  s_objBorder_chainCodeLength(chainCodeMatrix, xySizePerPixel)
;																			 chainCodeMatrix: Chain-Code-Matrix (# of border, x-y-code...)
;
; METHOHDS:
;_____________________________IOISIOI____________________


function s_objBorder_chainCodeLength, chainCodeMatrix, xySizePerPixel = xySizePerPixel

	code = chainCodeMatrix
	dimCode  = size(code, /dim)
	chainCodeLength = fltArr(dimCode[0])

	case (n_elements(xySizePerPixel) le 0) of
		1: begin
					; 1 for straight line
				corner  = (code eq 0) + (code eq 2) + (code eq 4) + (code eq 6)
					; 2 for diagonal line
				corner  = corner + ((code eq 1) + (code eq 3) + (code eq 5) + (code eq 7)) * 2
					; 3 for turning point
				corner = corner[*, 2:*]
				code[*,2:dimCode[1]-2] = abs( code[*, 3:dimCode[1]-1] - code[*,2:dimCode[1]-2] )
				corner  = corner * (code[*,2:dimCode[1]-2] ne 4) + ( (code[*,2:dimCode[1]-2] eq 4) *3)
					;       - Calculation of chain code length from Vossepoel
				for i = 0,dimCode[0]-1 do chainCodeLength(i) = 2 > (0.980 * total(corner[i,*] eq 1) $
																										 + 1.406 * total(corner[i,*] eq 2) $
																										 - 0.091 * total(corner[i,*] eq 3))
			endcase
		else: begin
					; 1 for straight, horizontal  line
				corner  = (code eq 0) + (code eq 4)
					; 2 for straight, vertical  line
				corner  = corner + ((code eq 2) + (code eq 6)) * 2
					; 3 for diagonal line
				corner  = corner + ((code eq 1) + (code eq 3) + (code eq 5) + (code eq 7)) * 3
					; 4 for turning point
				corner = corner[*, 2:*]
				code[*,2:dimCode[1]-2] = abs( code[*, 3:dimCode[1]-1] - code[*,2:dimCode[1]-2] )
				corner  = corner * (code[*,2:dimCode[1]-2] ne 4) + ( (code[*,2:dimCode[1]-2] eq 4) *4)
					;       - Calculation of chain code length from Vossepoel

				minLength = xySizePerPixel[0] + xySizePerPixel[1]
				for i = 0,dimCode[0]-1 do chainCodeLength(i) = minLength > $
																								( 0.980 * xySizePerPixel[0] * total(corner[i,*] eq 1) $
																								+ 0.980 * xySizePerPixel[1] * total(corner[i,*] eq 2) $
																								+ 1.406 * (total(xySizePerPixel)/2.) * total(corner[i,*] eq 3) $
																								- 0.091 * (total(xySizePerPixel)/2.) * total(corner[i,*] eq 4) )
			endcase
	endcase

	return, chainCodeLength
end