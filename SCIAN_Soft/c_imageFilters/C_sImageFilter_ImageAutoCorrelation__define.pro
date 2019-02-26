;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageAutoCorrelation
;
; PURPOSE:
;       - ImageColocalization-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2005)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ImageAutoCorrelation' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageAutoCorrelation::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end


pro C_sImageFilter_ImageAutoCorrelation::getMatrixValues, ch0_vect = ch0_vect, ch1_vect = ch1_vect
	dimM = size(*(*self.pParamStruct).pCorrelVectXAxis, /dim)
	if (n_elements(dimM) lt 1) then return
	ch0_vect = (*(*self.pParamStruct).pCorrelVectXAxis)
	ch1_vect = (*(*self.pParamStruct).pCorrelVectYAxis)
end


function C_sImageFilter_ImageAutoCorrelation::apply, image = image,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

	whereRad = (where((*(*self.pParamStruct).pNames) eq 'Pixel Radius'))[0]
	pixRad = round((*(*self.pParamStruct).pValues)[whereRad])

	count = 0
	for k = - pixRad, pixRad do for l = - pixRad, pixRad do begin
	      	pixDist = sqrt(k*k+l*l)
	     	if (pixDist le pixRad) then count = count + 1
	endfor

		; define Correlation Vectors
	if not(ptr_valid((*self.pParamStruct).pCorrelVectXAxis)) then (*self.pParamStruct).pCorrelVectXAxis = ptr_new(make_array(count, /int), /no_copy) $
		else *(*self.pParamStruct).pCorrelVectXAxis = make_array(count, /int)
	if not(ptr_valid((*self.pParamStruct).pCorrelVectYAxis)) then (*self.pParamStruct).pCorrelVectYAxis = ptr_new(make_array(count, /float), /no_copy) $
		else *(*self.pParamStruct).pCorrelVectYAxis = make_array(count, /float)

	dimI = size(image, /dim)
	meanF = (moment(image))[0]
	meanFSquare = meanF^2
	normI = image - meanF
	expImage  = s_Expand_Mirror(normI, pixRad)
	count = 0
	for k = - pixRad, pixRad do for l = - pixRad, pixRad do begin
	      	pixDist = sqrt(k*k+l*l)
	     	if (pixDist le pixRad) then begin
	     		(*(*self.pParamStruct).pCorrelVectXAxis)[count] = floor(pixDist)
	     		(*(*self.pParamStruct).pCorrelVectYAxis)[count] = (moment( normI * expImage( pixRad + k : pixRad+k+dimI[0]-1, pixRad+l : pixRad+l+dimI[1]-1 ) ))[0] / meanFSquare
	     		count = count + 1
			endif
	endfor

;		; check if C_sImageColocalization is selected whithin the Segmentation-Cluster
;	fC_sImageColocalization = 0b
;    oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
;    if (obj_valid(oImage)) then begin
;		clusterObj = oImage->getSegContainerObj(active = clusPos)
;		if (obj_valid(clusterObj)) then segNum = (clusterObj->count() - 1) else segNum = -1
;		for i = 0, segNum do begin
;			segObj = clusterObj->get(position = i)
;			if (obj_valid(segObj)) then if(obj_isa(segObj, 'C_SIMAGEFILTER_IMAGECOLOCALIZATION')) then begin
;				fC_sImageColocalization = 1b
;				segObjPos = i
;				pParamStruct = segObj->getpParamStruct()
;				dummy = (*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Pixel Radius'))[0]]
;				segObj->set, pParamStruct = pParamStruct
;			endif
;		endfor
;
;		if (fC_sImageColocalization) then begin
;			segObj = clusterObj->get(position = segObjPos)
;			image_2 = image
;			for pixRad = pixRadStart, pixRadEnd do begin
;					; set pixRad
;				pParamStruct = segObj->getpParamStruct()
;				(*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Pixel Radius'))[0]] = pixRad
;				segObj->set, pParamStruct = pParamStruct
;
;				imageRep =  image_2
;		     	image_2 = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject,$
;                                                         tPos = tPos,$
;                                                         chPos = chPos,$
;                                                         zPos = zPos,$
;                                                         clusPos = clusPos,$
;                                                         segPos = segPos-1,$
;                                                         cut_x = cut_x, cut_y = cut_y)
;
;				(*(*self.pParamStruct).pCorrelVectXAxis)[pixRad - pixRadStart] = pixRad
;				(*(*self.pParamStruct).pCorrelVectYAxis)[pixRad - pixRadStart] = total((image_2 - imageRep)^2) / n_elements(image)
;			endfor
;		(*(*self.pParamStruct).pCorrelVectYAxis)[0] = (*(*self.pParamStruct).pCorrelVectYAxis)[1]
;			; reset pixRad
;		pParamStruct = segObj->getpParamStruct()
;		(*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Pixel Radius'))[0]] = dummy
;		segObj->set, pParamStruct = pParamStruct
;		endif else return, image
;	endif else return, image

	ch0_vectDELTAR = *(*self.pParamStruct).pCorrelVectXAxis
	ch1_vectDELTAR = *(*self.pParamStruct).pCorrelVectYAxis

	ch0_vectMean = make_array(pixRad+1, /int, /index)
	ch1_vectMean = make_array(pixRad+1, /float)
	for i = 0, pixRad do begin
		whereI = where(*(*self.pParamStruct).pCorrelVectXAxis eq i)
		if (n_elements(whereI) eq 1) then ch1_vectMean[i] = (*(*self.pParamStruct).pCorrelVectYAxis)[whereI] else $
			ch1_vectMean[i] = (moment((*(*self.pParamStruct).pCorrelVectYAxis)[whereI]))[0]
	endfor

	live_info, error = error, properties = prop, window_in = 's_ImageAutoCorrelation Plot'
	if (error ne '') then begin
		style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type : 3, symbol_size : 0.01},$
										visualization_properties = {color : 'Light Red'},$
										xAxis_properties = {axisTitle :'pixel radius', exact: 1, compute_range : 0},$
										yAxis_properties = {axisTitle :'chi square', exact: 1, compute_range : 0},$
										legend_properties = {hide :1})
		live_plot, ch1_vectDELTAR, independent = ch0_vectDELTAR,$
                                        /scatter, draw_dimension = [300,300], /no_select,$
                                        style = style, title = 's_ImageAutoCorrelation Plot'
		live_oPlot, ch1_vectMean, independent = ch0_vectMean, /no_select,$
										subType = 'LinePlot',$
                                        window_in = 's_ImageAutoCorrelation Plot'
	endif else begin
        live_control, ch0_vectMean, /update, window_in = 's_ImageAutoCorrelation Plot'
        live_control, ch1_vectMean, /update, window_in = 's_ImageAutoCorrelation Plot'
		live_control, ch1_vectDELTAR, /update, window_in = 's_ImageAutoCorrelation Plot'
		live_control, ch0_vectDELTAR, /update, window_in = 's_ImageAutoCorrelation Plot'
	endelse

	live_info, 'CH0_VECTDELTAR Axis', properties = variable, window_in = 's_ImageAutoCorrelation Plot'
	variable.minRange = min(ch0_vectDELTAR) - .05*(max(ch0_vectDELTAR)-min(ch0_vectDELTAR))
	variable.maxRange = max(ch0_vectDELTAR) + .05*(max(ch0_vectDELTAR)-min(ch0_vectDELTAR))
	live_control, 'CH0_VECTDELTAR Axis', properties = variable, window_in = 's_ImageAutoCorrelation Plot'
	live_info, 'Y Axis', properties = variable, window_in = 's_ImageAutoCorrelation Plot'
	variable.minRange = min(ch1_vectDELTAR) - .05*(max(ch1_vectDELTAR)-min(ch1_vectDELTAR))
	variable.maxRange = max(ch1_vectDELTAR) + .05*(max(ch1_vectDELTAR)-min(ch1_vectDELTAR))
	live_control, 'Y Axis', properties = variable, window_in = 's_ImageAutoCorrelation Plot'

	return, image
end


pro C_sImageFilter_ImageAutoCorrelation::set, pParamStruct = pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
pro C_sImageFilter_ImageAutoCorrelation::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end
function C_sImageFilter_ImageAutoCorrelation::getpParamStruct
   return, self.pParamStruct
end
pro C_sImageFilter_ImageAutoCorrelation::cleanup
	for i = 0,n_tags((*self.pParamStruct))-1 do begin
		case size((*self.pParamStruct).(i), /tname) of
			'POINTER' : ptr_free, (*self.pParamStruct).(i)
			'OBJREF' : obj_destroy, (*self.pParamStruct).(i)
			else:
		endcase
	endfor
	ptr_free, self.pParamStruct
end


function C_sImageFilter_ImageAutoCorrelation::init
   filterStruct = {Name: 'C_ImageAutoCorrelation',$   ;  filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on filter Parameter Names.
                           pActive:ptr_new(),$ 	; Pointer on filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on filter Parameter Min_Values.
                           pMax:ptr_new(),$    ; Pointer on filter Parameter Max_Values.
                           pValues:ptr_new(),$    ; Pointer on filter Parameter Values.
                           pCorrelVectXAxis:ptr_new(),$	; Pointer on xAxisVector.
                           pCorrelVectYAxis:ptr_new()}	; Pointer on yAxisVector.

    filterParamWidgetType = make_array(1, /string, value = 'widget_slider')
    filterParamNames = ['Pixel Radius']
    filterParamActive = [1]
    filterParamMin = [1]
    filterParamMax = [100]
    filterParamValues = [1]

	a = [-1]
    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
    filterStruct.pCorrelVectXAxis = ptr_new(a)
    filterStruct.pCorrelVectYAxis = ptr_new(a, /no_copy)
    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageAutoCorrelation__define
   tmp = {C_sImageFilter_ImageAutoCorrelation, pParamStruct: ptr_new()}
end