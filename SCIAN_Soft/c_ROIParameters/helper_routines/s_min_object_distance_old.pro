;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_min_object_distance
;
; PURPOSE:
;       Find minimum distances between all numerbered objects in NumberedObjectImage
;
; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       s_min_object_distance, NumberedObjectImage
;       	NumberedObjectImage: A 2D array of object data | Objects must be numbered consecutively
;
; REQUIRED INPUTS:
;       None. -1 is returned if NumberedObjectImage does not fullfill requirements
;
; OPTIONAL KEYWORD PARAMETERS:
;       _EXTRA:
;_____________________________IOISIOI____________________


Function s_min_object_distance, NumberedObjectImage, pfad = pfad, ImageCounter = ImageCounter		; NumberedObjectImage:	Bild mit nummerierten Flächen


	if (N_Elements(NumberedObjectImage) EQ 0) then begin
		s_apop_shout, 'No image passed'
		return, -1
	endif

	MaxNumberedObjectImage = Max(NumberedObjectImage)

	if (MaxNumberedObjectImage EQ 0) then begin
		a = s_apop_shout( 'No object segmented in image')
		Return, -1
	endif

	if (MaxNumberedObjectImage EQ 1) then begin
		NumberedObjectImage = s_apop_flaeche(NumberedObjectImage)
		MaxNumberedObjectImage = Max(NumberedObjectImage)
		if (MaxNumberedObjectImage EQ 1) then begin
			a = s_apop_shout('Only one object segmented in image')
			Return, 0
		endif else begin
			a = s_apop_shout('Objects have been renumbered 1, 2, ...N')
		endelse
	endif

		; Caluculate distances, only if more then 2 objects exist !!!
	if (MaxNumberedObjectImage GE 2) then begin
			; Define 2D-object borders and number them in respect to object size.
		BorderPicture = (s_rand4(NumberedObjectImage)) * NumberedObjectImage

			; Get x- and y-BorderIndices -> BorderIndices[BorderIndices[i]:BorderIndices[i+1]-1] contain postition
			; of ith border in BorderPicture !
		hist = Histogram( BorderPicture,$
								 Min = 1, Max = MaxNumberedObjectImage,$
								 /Nan,$
								Reverse_Indices = BorderIndices)

			; Calculate x- and y-BorderPositions -> x(y)PixelPosition[BorderIndices[i]:BorderIndices[i+1]-1]
			; contain postition of ith border in BorderPicture !
		DimImage = Size(NumberedObjectImage, /Dimension)


				Window, 10, Xsize = DimImage[0], Ysize = DimImage[1]
				Erase


			; calculate PixelDimension [µm]
		RealPixelSize = [225./DimImage[0], 177./DimImage[1]]
		xPixelPosition = BorderIndices[BorderIndices[0]:*] MOD DimImage[0]
		yPixelPosition = Floor(BorderIndices[BorderIndices[0]:*] * (1./DimImage[0]))
		BorderIndices = BorderIndices[0:N_Elements(hist)] - BorderIndices[0]

			; Define Distance Matrix: [i,j] defines distance between Object i and j (- 1. is default)
		DistanceMatrix = FltArr(MaxNumberedObjectImage, MaxNumberedObjectImage)
		PosDPM = LonArr(2, MaxNumberedObjectImage, MaxNumberedObjectImage)

			; TotalNumBorderPixels is Total Number of BorderPixels
		TotalNumBorderPixels = N_Elements(xPixelPosition)

			; Calculate BorderPixel Distance for each object i
		for i = 0, N_Elements(hist)-2 do begin
				; Define DistancePixelMatrix -> [Number of BorderPixels of object i, Number of BorderPixels of all remaining objects i+1]
			DistancePixelMatrix = FltArr( hist[i], BorderIndices[N_Elements(hist)] - BorderIndices[i+1] )

				; Calculate BorderPixel Distance for each pixel j of object i in respect to all other object pixels
			for j = BorderIndices[i], BorderIndices[i+1]-1 do $
				DistancePixelMatrix[j-BorderIndices[i], *] = $
														Sqrt(  ( RealPixelSize[0] * (xPixelPosition[BorderIndices[i+1] : *] - xPixelPosition[j]))^2 + $
														          ( RealPixelSize[1] * (yPixelPosition[BorderIndices[i+1] : *] - yPixelPosition[j]))^2		)

;________Optional_Plot_Distances for each Object
				; Plot BorderPixel Distance for each pixel j of object i in respect to all other object pixels
;			 Window, 10, Xsize = DimImage[0], Ysize = DimImage[1]
;			 TVScl, (NumberedObjectImage + (BorderPicture GT 0) * 255) < 256
;			 for z  = BorderIndices[i+1], N_Elements(xPixelPosition)-1 do $
;				 for zz = BorderIndices[i], BorderIndices[i+1]-BorderIndices[0]-1 do $
;					 Plots, [xPixelPosition[z], xPixelPosition[zz]],  [yPixelPosition[z], yPixelPosition[zz]], Color = (200-(z MOD 100)), /Device
;________END_Optional_Plot_Distances for each Object

				; Find Min BorderPixel Distance for each pixel of object i in respect to all other objects j
			for j = i+1, N_Elements(hist)-1 do begin
					; Get Pixel Distance Matrix [pixel i, pixel j]
				dummy =  DistancePixelMatrix[*, BorderIndices[j]-BorderIndices[i+1] : BorderIndices[j+1]-1 -BorderIndices[i+1]]
				k = (Where(dummy EQ Min(dummy)))[0]

					; Set Min Object Distance Matrix [i, j]
				DistanceMatrix[i,j] = dummy[k]
					; Get Min Pixel-Positions in Object i & j [PosDPM[0,i,j] & PosDPM[0,i,j]]
				PosDPM[0,i,j] = k MOD (BorderIndices[i+1]-BorderIndices[i]) + BorderIndices[i]
				PosDPM[1,i,j] = Floor(k * (1./(BorderIndices[i+1]-BorderIndices[i]))) + BorderIndices[j]

				Erase
				Plots, [xPixelPosition[PosDPM[0,i,j]], xPixelPosition[PosDPM[1,i,j]]],  [yPixelPosition[PosDPM[0,i,j]], yPixelPosition[PosDPM[1,i,j]]], Color = 255, /Device

			endfor
		endfor

			; Set Min Object Distance Matrix [i, j-1]
;		DistanceMatrix[i,j-1] = DistanceMatrix[i-1,j-1]
			; Get Min Pixel-Positions in Object i & j [PosDPM[0,i,j] & PosDPM[0,i,j]]
;		PosDPM[0,i,j-1] = PosDPM[0,i-1,j-1]
;		PosDPM[1,i,j-1] = PosDPM[1,i-1,j-1]

			; Find Min BorderPixel Distance for each pixel of object i in respect to all other objects j
		DistanceMatrix = (DistanceMatrix) + Transpose(DistanceMatrix)
		k = Where(DistanceMatrix EQ 0)
		DistanceMatrix[k] = Sqrt((DimImage[0] + RealPixelSize[0])^2+(DimImage[0] + RealPixelSize[1])^2)
		PosDPM[0,*,*] = (PosDPM[0,*,*]) + Transpose(PosDPM[0,*,*])
		PosDPM[1,*,*] = (PosDPM[1,*,*]) + Transpose(PosDPM[1,*,*])

;________Optional_Plot_Minimum_Distances for each Object
		Window, 10, Xsize = DimImage[0], Ysize = DimImage[1]
		Erase
		TVScl, ( (NumberedObjectImage MOD 255) > ((BorderPicture GT 0)*255))
		for i = 0, N_Elements(hist)-1 do begin
			k = (Where(DistanceMatrix[i,*] EQ Min(DistanceMatrix[i,*]) ))[0]
			Plots, [xPixelPosition[PosDPM[0,i,k]], xPixelPosition[PosDPM[1,i,k]]],  [yPixelPosition[PosDPM[0,i,k]], yPixelPosition[PosDPM[1,i,k]]], Color = 255, /Device
		endfor
;________END_Optional_Plot_Minimum_Distances for each Object


;________Save Size-Ordered Distances for each Object

		for i = 0, N_Elements(hist)-1 do begin
			num1 = StrCompress(String(Floor(ImageCounter)), /Remove_All)
			num2 = StrCompress(String(Floor(i/100.) MOD 10) + String(Floor(i/10.) MOD 10) + String(i MOD 10), /Remove_All)
			OpenW,2 , pfad+num1+'_'+ num2 + '.dat'
			PrintF, 2, Transpose((DistanceMatrix[i,Sort(DistanceMatrix[i,*])])[0:N_Elements(hist)-2])
			Close, 2
		endfor
;________Save Size-Ordered Distances for each Object

	Return, 1
	endif

end



Function s_min_object_distanceMatrix, NumberedObjectImage,$					; NumberedObjectImage:	Image with numbered Objects
																		DimImageReal = DimImageReal		; DimImageReal:	Real Image Size [µm] for each dimension

	if (N_Elements(NumberedObjectImage) EQ 0) then begin
		s_apop_shout, 'No image passed'
		return, -1
	endif

	MaxNumberedObjectImage = Max(NumberedObjectImage)

	if (MaxNumberedObjectImage EQ 0) then begin
		a = s_apop_shout( 'No object segmented in image')
		return, -1
	endif

	if (MaxNumberedObjectImage EQ 1) then begin
		NumberedObjectImage = s_apop_flaeche(NumberedObjectImage)
		MaxNumberedObjectImage = Max(NumberedObjectImage)
		if (MaxNumberedObjectImage EQ 1) then begin
			a = s_apop_shout('Only one object segmented in image')
			return, 0
		endif else 	a = s_apop_shout('Objects have been renumbered 1, 2, ...N')
	endif

		; Caluculate distances, only if more then 2 objects exist !!!
		; Define 2D-object borders and number them in respect to object size.
	BorderPicture = (s_rand4(NumberedObjectImage)) * NumberedObjectImage

		; Get x- and y-BorderIndices -> BorderIndices[BorderIndices[i]:BorderIndices[i+1]-1] contain postition
		; of ith border in BorderPicture !
	hist = Histogram( BorderPicture, Min = 1, Max = MaxNumberedObjectImage,  /Nan, Reverse_Indices = BorderIndices)

		; Calculate x- and y-BorderPositions -> x(y)PixelPosition[BorderIndices[i]:BorderIndices[i+1]-1]
		; contain postition of ith border in BorderPicture !
	DimImage = Size(NumberedObjectImage, /Dimension)
		; calculate RealPixelSize [µm/Pixel]
	if (n_elements(DimImageReal)) gt 0 then RealPixelSize = [DimImageReal[0]/DimImage[0], DimImageReal[1]/DimImage[1]] else RealPixelSize = [1,1]

	xPixelPosition = BorderIndices[BorderIndices[0]:*] MOD DimImage[0]
	yPixelPosition = Floor(BorderIndices[BorderIndices[0]:*] * (1./DimImage[0]))
	BorderIndices = BorderIndices[0:N_Elements(hist)] - BorderIndices[0]

		; Define Distance Matrix: [i,j] defines distance between Object i and j (- 1. is default)
	DistanceMatrix = FltArr(MaxNumberedObjectImage, MaxNumberedObjectImage)
	PosDPM = LonArr(2, MaxNumberedObjectImage, MaxNumberedObjectImage)

		; TotalNumBorderPixels is Total Number of BorderPixels
	TotalNumBorderPixels = N_Elements(xPixelPosition)

		; Calculate BorderPixel Distance for each object i
	for i = 0, N_Elements(hist)-2 do begin

			; Define DistancePixelMatrix -> [Number of BorderPixels of object i, Number of BorderPixels of all remaining objects i+1]
		DistancePixelMatrix = FltArr( hist[i], BorderIndices[N_Elements(hist)] - BorderIndices[i+1] )

			; Calculate BorderPixel Distance for each pixel j of object i in respect to all other object pixels
		for j = BorderIndices[i], BorderIndices[i+1]-1 do $
			DistancePixelMatrix[j-BorderIndices[i], *] = Sqrt(  ( RealPixelSize[0] * (xPixelPosition[BorderIndices[i+1] : *] - xPixelPosition[j]))^2 + $
													       											 ( RealPixelSize[1] * (yPixelPosition[BorderIndices[i+1] : *] - yPixelPosition[j]))^2		)
;			Plots, ([ transpose(xPixelPosition[BorderIndices[i+1] : *]*0+ xPixelPosition[j]) , transpose(xPixelPosition[BorderIndices[i+1] : *]) ]) ,$
;					  ([ transpose(yPixelPosition[BorderIndices[i+1] : *]*0+ yPixelPosition[j]) , transpose(yPixelPosition[BorderIndices[i+1] : *]) ]), Color = (j*10 mod 255), /Device

			; Find Min BorderPixel Distance for each pixel of object i in respect to all other objects j
		for j = i+1, N_Elements(hist)-1 do begin
				; Get Pixel Distance Matrix [pixel i, pixel j]
			dummy =  DistancePixelMatrix[*, BorderIndices[j]-BorderIndices[i+1] : BorderIndices[j+1]-1 -BorderIndices[i+1]]
				; Set Min Object Distance Matrix [i, j]
			k =  (Where(dummy EQ Min(dummy)))[0]
			DistanceMatrix[i,j] = dummy[k]
				; Get Min Pixel-Positions in Object i & j [PosDPM[0,i,j] & PosDPM[1,i,j]]
			PosObj_i = k MOD (BorderIndices[i+1]-BorderIndices[i]) + BorderIndices[i]
			PosObj_j = Floor(k * (1./(BorderIndices[i+1]-BorderIndices[i]))) + BorderIndices[j]
			PosDPM[0,i,j] = xPixelPosition[PosObj_i]; x-Position of Object i
			PosDPM[0,j,i] = yPixelPosition[PosObj_i]; y-Position of Object i
			PosDPM[1,i,j] = xPixelPosition[PosObj_j]; x-Position of Object j
			PosDPM[1,j,i] = yPixelPosition[PosObj_j]; y-Position of Object j
;			Plots, [PosDPM[0,i,j], PosDPM[1,i,j]], [PosDPM[0,j,i], PosDPM[1,j,i]], Color = 255, /Device
		endfor
	endfor

		; Find Min BorderPixel Distance for each pixel of object i in respect to all other objects j
	DistanceMatrix = (DistanceMatrix) + Transpose(DistanceMatrix)
	DistanceMatrix[ (Where(DistanceMatrix EQ 0)) ] = Sqrt((DimImage[0] + RealPixelSize[0])^2+(DimImage[0] + RealPixelSize[1])^2)

	ObjectBorder = {	ParamName: 'Object_Border_Distances',$
							 	Distance: DistanceMatrix,$
								PositionMatrix: PosDPM $
								}

	Return, ObjectBorder
end

Function s_min_center_distanceMatrix, NumberedObjectImage,$		; NumberedObjectImage:	Image with numbered Objects
														DimImageReal = DimImageReal	; DimImageReal:	Real Image Size [µm] for each dimension

	if (N_Elements(NumberedObjectImage) EQ 0) then begin
		s_apop_shout, 'No image passed'
		return, -1
	endif

	MaxNumberedObjectImage = Max(NumberedObjectImage)

	if (MaxNumberedObjectImage EQ 0) then begin
		a = s_apop_shout( 'No object segmented in image')
		return, -1
	endif

	if (MaxNumberedObjectImage EQ 1) then begin
		NumberedObjectImage = s_apop_flaeche(NumberedObjectImage)
		MaxNumberedObjectImage = Max(NumberedObjectImage)
		if (MaxNumberedObjectImage EQ 1) then begin
			a = s_apop_shout('Only one object segmented in image')
			return, 0
		endif else 	a = s_apop_shout('Objects have been renumbered 1, 2, ...N')
	endif

		; Caluculate distances, only if more then 2 objects exist !!!
		; Define 2D-object borders and number them in respect to object size.
	BorderPicture = (s_rand4(NumberedObjectImage)) * NumberedObjectImage

		; Get x- and y-BorderIndices -> BorderIndices[BorderIndices[i]:BorderIndices[i+1]-1] contain postition
		; of ith border in BorderPicture !
	hist = Histogram( BorderPicture, Min = 1, Max = MaxNumberedObjectImage,  /Nan, Reverse_Indices = BorderIndices)

		; Calculate x- and y-BorderPositions -> x(y)PixelPosition[BorderIndices[i]:BorderIndices[i+1]-1]
		; contain postition of ith border in BorderPicture !
	DimImage = Size(NumberedObjectImage, /Dimension)
		; calculate RealPixelSize [µm/Pixel]
	if (n_elements(DimImageReal)) gt 0 then RealPixelSize = [DimImageReal[0]/DimImage[0], DimImageReal[1]/DimImage[1]] else RealPixelSize = [1,1]

	xPixelPosition = BorderIndices[BorderIndices[0]:*] MOD DimImage[0]
	yPixelPosition = Floor(BorderIndices[BorderIndices[0]:*] * (1./DimImage[0]))
	BorderIndices = BorderIndices[0:N_Elements(hist)] - BorderIndices[0]

		; Define Distance Matrix: [i,j] defines distance between Object i and j (- 1. is default)
	DistanceMatrix = FltArr(MaxNumberedObjectImage, MaxNumberedObjectImage)
	PosDPM = LonArr(2, MaxNumberedObjectImage, MaxNumberedObjectImage)

		; TotalNumBorderPixels is Total Number of BorderPixels
	TotalNumBorderPixels = N_Elements(xPixelPosition)

		; Calculate BorderPixel Distance for each object i
	for i = 0, N_Elements(hist)-2 do begin

			; Define DistancePixelMatrix -> [Number of BorderPixels of object i, Number of BorderPixels of all remaining objects i+1]
		DistancePixelMatrix = FltArr( hist[i], BorderIndices[N_Elements(hist)] - BorderIndices[i+1] )

			; Calculate BorderPixel Distance for each pixel j of object i in respect to all other object pixels
		for j = BorderIndices[i], BorderIndices[i+1]-1 do $
			DistancePixelMatrix[j-BorderIndices[i], *] = Sqrt(  ( RealPixelSize[0] * (xPixelPosition[BorderIndices[i+1] : *] - xPixelPosition[j]))^2 + $
													       											 ( RealPixelSize[1] * (yPixelPosition[BorderIndices[i+1] : *] - yPixelPosition[j]))^2		)
;			Plots, ([ transpose(xPixelPosition[BorderIndices[i+1] : *]*0+ xPixelPosition[j]) , transpose(xPixelPosition[BorderIndices[i+1] : *]) ]) ,$
;					  ([ transpose(yPixelPosition[BorderIndices[i+1] : *]*0+ yPixelPosition[j]) , transpose(yPixelPosition[BorderIndices[i+1] : *]) ]), Color = (j*10 mod 255), /Device

			; Find Min BorderPixel Distance for each pixel of object i in respect to all other objects j
		for j = i+1, N_Elements(hist)-1 do begin
				; Get Pixel Distance Matrix [pixel i, pixel j]
			dummy =  DistancePixelMatrix[*, BorderIndices[j]-BorderIndices[i+1] : BorderIndices[j+1]-1 -BorderIndices[i+1]]
				; Set Min Object Distance Matrix [i, j]
			k =  (Where(dummy EQ Min(dummy)))[0]
			DistanceMatrix[i,j] = dummy[k]
				; Get Min Pixel-Positions in Object i & j [PosDPM[0,i,j] & PosDPM[1,i,j]]
			PosDPM[0,i,j] = k MOD (BorderIndices[i+1]-BorderIndices[i]) + BorderIndices[i]
			PosDPM[1,i,j] = Floor(k * (1./(BorderIndices[i+1]-BorderIndices[i]))) + BorderIndices[j]
;			Plots, [xPixelPosition[PosDPM[0,i,j]], xPixelPosition[PosDPM[1,i,j]]],  [yPixelPosition[PosDPM[0,i,j]], yPixelPosition[PosDPM[1,i,j]]], Color = 255, /Device
		endfor
	endfor

		; Find Min BorderPixel Distance for each pixel of object i in respect to all other objects j
	DistanceMatrix = (DistanceMatrix) + Transpose(DistanceMatrix)
	DistanceMatrix[ (Where(DistanceMatrix EQ 0)) ] = Sqrt((DimImage[0] + RealPixelSize[0])^2+(DimImage[0] + RealPixelSize[1])^2)
	PosDPM[0,*,*] = (PosDPM[0,*,*]) + Transpose(PosDPM[0,*,*])
	PosDPM[1,*,*] = (PosDPM[1,*,*]) + Transpose(PosDPM[1,*,*])

	ObjectBorder = {	ParamName: 'Object_Center_Distances',$
							 	Distance: DistanceMatrix,$
								PositionMatrix: PosDPM $
								}

	Return, ObjectBorder
end