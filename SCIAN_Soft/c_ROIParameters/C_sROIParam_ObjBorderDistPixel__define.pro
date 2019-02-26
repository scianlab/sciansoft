;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjBorderDistPixel
;
; PURPOSE:
;       - Calculation of Object Size in real Dimensions
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = obj_new('C_sROIParam_ObjBorderDistPixel' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sROIParam_ObjBorderDistPixel::getROIParamType
	return, 'Group ROI-Parameter-Method'
end

function C_sROIParam_ObjBorderDistPixel::apply, mask = mask

	hmax = max(mask)
	if (hmax eq 0) then return,{name: 'Object-Border Distances [Pixel]',$
											type: 'Group ROI-Parameter-Method',$
											objNumberVector: -1,$
											distanceMatrix: -1,$
											positionMatrix: -1 }

	hmin = min(mask[where(mask ne 0)])
	if (hmax eq hmin) then return,{name: 'Object-Border Distances [Pixel]',$
												type: 'Group ROI-Parameter-Method',$
												objNumberVector: hmin,$
												distanceMatrix: -1,$
												positionMatrix: -1 }

		; renumber objects -> 1, 2, ...N
	newNumMask = s_apop_flaeche(mask gt 0)
	maxNewNumMask = max(newNumMask)

		; Caluculate distances, only if more then 2 objects exist !!!
		; Define 2D-object borders and number them in respect to object size.
	borderMasks = s_rand4(newNumMask) * newNumMask

		; Get x- and y-borderIndices -> borderIndices[borderIndices[i]:borderIndices[i+1]-1] contain postition
		; of ith border in borderMasks !
	hist = histogram( borderMasks, min = 1, max = maxNewNumMask,  /nan, reverse_indices = borderIndices)

		; Calculate x- and y-BorderPositions -> x(y)PixPos[borderIndices[i]:borderIndices[i+1]-1]
		; contain postition of ith border in borderMasks !
	dimMask = size(newNumMask, /dim)
	xPixPos = borderIndices[BorderIndices[0]:*] mod dimMask[0]
	yPixPos = floor(borderIndices[BorderIndices[0]:*] * (1./dimMask[0]))
	borderIndices = borderIndices[0:n_elements(hist)] - borderIndices[0]

		; Define Distance Matrix: [i,j] defines distance between Object i and j (- 1. is default)
	distanceMatrix = flrArr(maxNewNumMask, maxNewNumMask)
	posDPM = lonArr(2, maxNewNumMask, maxNewNumMask)

		; totalNumBorderPixels is Total Number of BorderPixels
	totalNumBorderPixels = n_elements(xPixPos)

		; Calculate BorderPixel Distance for each object i
	for i = 0, n_elements(hist)-2 do begin

			; Define distancePixelMatrix -> [Number of BorderPixels of object i, Number of BorderPixels of all remaining objects i+1]
		distancePixelMatrix = flrArr( hist[i], borderIndices[n_elements(hist)] - borderIndices[i+1] )

			; Calculate BorderPixel Distance for each pixel j of object i in respect to all other object pixels
		for j = borderIndices[i], borderIndices[i+1]-1 do $
			distancePixelMatrix[j-borderIndices[i], *] = sqrt(  (xPixPos[borderIndices[i+1] : *] - xPixPos[j])^2 + $
													    											(yPixPos[borderIndices[i+1] : *] - yPixPos[j])^2 )

;			Plots, ([ transpose(xPixPos[borderIndices[i+1] : *]*0+ xPixPos[j]) , transpose(xPixPos[borderIndices[i+1] : *]) ]) ,$
;					  ([ transpose(yPixPos[borderIndices[i+1] : *]*0+ yPixPos[j]) , transpose(yPixPos[borderIndices[i+1] : *]) ]), Color = (j*10 mod 255), /Device

			; Find min BorderPixel Distance for each pixel of object i in respect to all other objects j
		for j = i+1, n_elements(hist)-1 do begin
				; Get Pixel Distance Matrix [pixel i, pixel j]
			dummy =  distancePixelMatrix[*, borderIndices[j]-borderIndices[i+1] : borderIndices[j+1]-1 -borderIndices[i+1]]
				; Set min Object Distance Matrix [i, j]
			k =  (where(dummy eq min(dummy)))[0]
			distanceMatrix[i,j] = dummy[k]
				; Get min Pixel-Positions in Object i & j [posDPM[0,i,j] & posDPM[1,i,j]]
			posObj_i = k mod (borderIndices[i+1]-borderIndices[i]) + borderIndices[i]
			posObj_j = floor(k * (1./(borderIndices[i+1]-borderIndices[i]))) + borderIndices[j]
			posDPM[0,i,j] = xPixPos[posObj_i]; x-Position of Object i
			posDPM[0,j,i] = yPixPos[posObj_i]; y-Position of Object i
			posDPM[1,i,j] = xPixPos[posObj_j]; x-Position of Object j
			posDPM[1,j,i] = yPixPos[posObj_j]; y-Position of Object j
;			Plots, [posDPM[0,i,j], posDPM[1,i,j]], [posDPM[0,j,i], posDPM[1,j,i]], Color = 255, /Device
		endfor
	endfor

	distanceMatrix = (distanceMatrix) + transpose(distanceMatrix)
	distanceMatrix[ (where(distanceMatrix eq 0)) ] = -1.

	objNumberVector = intArr(maxNewNumMask)
	for i = 1, maxNewNumMask do objNumberVector[i] = mask[(where(maxNewNumMask eq i))[0]]

	 return,	{name: 'Object-Border Distances [Pixel]',$
					type: 'Group ROI-Parameter-Method',$
					objNumberVector: objNumberVector,$
					distanceMatrix: distanceMatrix,$
					positionMatrix: posDPM }
end

pro C_sROIParam_ObjBorderDistPixel::set, pParamStruct = pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end

pro C_sROIParam_ObjBorderDistPixel::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end

pro C_sROIParam_ObjBorderDistPixel::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
	    case size((*self.pParamStruct).(i), /tname) of
	        'POINTER': ptr_free, (*self.pParamStruct).(i)
	        'OBJREF': obj_destroy, (*self.pParamStruct).(i)
	        else:
	 	 endcase
	 endfor
	ptr_free, self.pParamStruct
end


function C_sROIParam_ObjBorderDistPixel::init

	ROIParamStruct = {  Name:'Object-Border Distances [Pixel]',$		;  ROI Name.
									pWidgetType:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pNames:ptr_new(),$		; Pointer on ROI-Obj Parameter Names.
									pActive:ptr_new(),$		 ; Pointer on ROI-Obj Parameter Active Bool.
									pMin:ptr_new(),$	   ; Pointer on ROI-Obj Parameter Min_Values.
									pMax:ptr_new(),$		  ; Pointer on ROI-Obj Parameter Max_Values.
									pValues:ptr_new()  }			 ; Pointer on ROI-Obj Parameter Values.

	ROIParamWidgetType = ['widget_slider']
	ROIParamNames = ['Void']
	ROIParamActive = [1b]
	ROIParamMin = [0b]
	ROIParamMax = [1b]
	ROIParamValues =[0b]

	ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
	ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
	ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
	ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
	ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
	ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)
    return, 1
end


pro C_sROIParam_ObjBorderDistPixel__define
   tmp = {C_sROIParam_ObjBorderDistPixel, pParamStruct: ptr_new()}
end