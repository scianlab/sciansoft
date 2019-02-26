;_____________________________IOISIOI____________________
; function_NAME:
;       sMeanImage
;
; PURPOSE:
;       Calculates Mean values of 2D-Image Data.
;
; AUTHor:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       	result = sMeanImage( image,  pixRad )
;       	image: 			 A 2D-Array of Image Data.
;      		pixRad:  		Radius ( PixelSize ) for which mean values are calculated.
;
; OPTIONAL KEYWorD PARAMETERS:
;       Expand_Mirror (DEFAULT):    		Set this keyword to  expand 2D-Image Data by mirroring image edges (s_Expand_Mirror.pro).
;       Expand_Congrid_Method:  					 		Set this keyword to  expand 2D-Image Data by congrid routine.
;       Expand_Congrid_Near (DEFAULT):		forces  nearest-neighbor sampling.
;       Expand_Congrid_Interp:						forces linear interpolation when resizing a 1- or 2-dimensional array.
;       Expand_Congrid_Cubic:		 		 		forces cubic convolution interpolation parameter [-1,0] (Default = -0.5)
;
; returnS:
;       Mean values of 2D-Image Data.
;		Mean values have the same data type as 2D-Image Data.
;_____________________________IOISIOI____________________

function sMeanImage,   image,  pixRad, Expand_Mirror = Expand_Mirror,$
														Expand_Congrid_Method = Expand_Congrid_Method,$
														Expand_Congrid_Near =  Expand_Congrid_Near,$
														Expand_Congrid_Interp =  Expand_Congrid_Interp,$
														Expand_Congrid_Cubic = Expand_Congrid_Cubic

	if not(keyword_set(Expand_Mirror)) then Expand_Mirror = 0
	if not(keyword_set(Expand_Congrid_Method)) then Expand_Congrid_Method = 0
	if not(keyword_set(Expand_Congrid_Near)) then Expand_Congrid_Near = 0
	if not(keyword_set(Expand_Congrid_Interp)) then Expand_Congrid_Interp = 0
	if not(keyword_set(Expand_Congrid_Cubic)) then Expand_Congrid_Cubic = 99

	dimI  = size(image)

	case (size(image, /type))  of																					 ; Data Type
		0: begin
				message, 'Undefined Data Type of image in sMeanImage', /continue
				return, image
			 end
		1: outI  = intarr(dimI[1],dimI[2])								;  Byte
		2: outI  = lonarr(dimI[1],dimI[2])								;  Integer
		3: outI  = lon64arr(dimI[1],dimI[2])							;  Longword integer
		4: outI  = dblarr(dimI[1],dimI[2])								; Floating point
		5: outI  = dblarr(dimI[1],dimI[2])								; double-precision floating
		else:  begin
				message, 'Uncoherent Data Type of image in sMeanImage', /continue
				return, image
			 end
	endcase

		; Define expanded image to avoid border problems
	if (not(Expand_Congrid_Method)) or (Expand_Mirror)	then begin			; DEFAULT  expands 2D-Image Data by mirroring image edges.
		expandI = s_Expand_Mirror(image, pixRad )
	endif else begin
		if ( (Expand_Congrid_Near) or ((Expand_Congrid_Cubic eq 99)and(not(Expand_Congrid_Interp)) )  ) then begin 		; DEFAULT  nearest-neighbor sampling.
			expandI = congrid(image, dimI[1]+ pixRad, dimI[2]+ pixRad, /minus_one)
		endif else begin
			if (Expand_Congrid_Interp) then $
				expandI = congrid(image, dimI[1]+ pixRad, dimI[2]+ pixRad, /INTERP, /MINUS_ONE)
			if (Expand_Congrid_Cubic NE 99) then $
				expandI = congrid(image, dimI[1]+ pixRad, dimI[2]+ pixRad, CUBIC = EXPand_CONGRID_CUBIC, /MINUS_ONE)
		endelse
			; Set center of expandI to image
		expandI[pixRad : pixRad + dimI[1], pixRad : pixRad + dimI[2]] = image
	endelse

		; Sum Image Values in pixRad
	count = 0
	for k = -pixRad, pixRad do for l = -pixRad, pixRad do begin
	 	if ( floor(sqrt(k*k+l*l)) le pixRad ) then begin
			count += 1
			outI += expandI[pixRad+k : pixRad+k+dimI[1]-1, pixRad+l : pixRad+l+dimI[2]-1]
		endif
	endfor

	case (size(image, /type))  of													 ; Data Type
			1: return, byte( round( (1./count) * outI)  )					;  Byte
			2: return, fix( round( (1./count) * outI)  )					;  Integer
			3: return, round( (1./count) * outI)							;  Longword integer
			4: return, float( (1./count) * outI) 							; Floating point
			5: return, ( (1.d/count) * outI)								; double-precision floating
	endcase
end
