;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageColocDeltaR
;
; PURPOSE:
;       - ImageColocalization-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2005)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ImageColocDeltaR' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageColocDeltaR::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


pro C_sImageFilter_ImageColocDeltaR::getMatrixValues, ch0_vect = ch0_vect, ch1_vect = ch1_vect
  dimM = size(*(*self.pParamStruct).pCorrelVectXAxis, /dim)
  if (n_elements(dimM) lt 1) then return
  ch0_vect = (*(*self.pParamStruct).pCorrelVectXAxis)
  ch1_vect = (*(*self.pParamStruct).pCorrelVectYAxis)
end


function C_sImageFilter_ImageColocDeltaR::apply, image = image,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

  whereRadStart = (where((*(*self.pParamStruct).pNames) eq 'Pixel Radius Start'))[0]
  whereRadEnd = (where((*(*self.pParamStruct).pNames) eq 'Pixel Radius End'))[0]
  pixRadStart = round((*(*self.pParamStruct).pValues)[whereRadStart]) < round((*(*self.pParamStruct).pValues)[whereRadEnd])
  pixRadEnd = round((*(*self.pParamStruct).pValues)[whereRadStart] + 1) > round((*(*self.pParamStruct).pValues)[whereRadEnd])
  (*(*self.pParamStruct).pValues)[whereRadStart] = pixRadStart
  (*(*self.pParamStruct).pValues)[whereRadEnd] = pixRadEnd

   ; define Correlation Vectors
  if not(ptr_valid((*self.pParamStruct).pCorrelVectXAxis)) then (*self.pParamStruct).pCorrelVectXAxis = ptr_new(make_array(pixRadEnd-pixRadStart, /int), /no_copy) $
   else *(*self.pParamStruct).pCorrelVectXAxis = make_array(pixRadEnd-pixRadStart, /int)
  if not(ptr_valid((*self.pParamStruct).pCorrelVectYAxis)) then (*self.pParamStruct).pCorrelVectYAxis = ptr_new(make_array(pixRadEnd-pixRadStart, /float), /no_copy) $
   else *(*self.pParamStruct).pCorrelVectYAxis = make_array(pixRadEnd-pixRadStart, /float)

   ; check if C_sImageColocalization is selected within the Segmentation-Cluster
  fC_sImageColocalization = 0b
    oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    if (obj_valid(oImage)) then begin
   clusterObj = oImage->getSegContainerObj(active = clusPos)
   if obj_valid(clusterObj) then segNum = (clusterObj->count() - 1) else segNum = -1
   for i = 0, segNum do begin
     segObj = clusterObj->get(position = i)
     if obj_valid(segObj) then if obj_isa(segObj, 'C_SIMAGEFILTER_IMAGECOLOCALIZATION') then begin
      fC_sImageColocalization = 1b
      segObjPos = i
      pParamStruct = segObj->getpParamStruct()
      dummy = (*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Pixel Radius'))[0]]
      segObj->set, pParamStruct = pParamStruct
     endif
   endfor

   if fC_sImageColocalization then begin
     segObj = clusterObj->get(position = segObjPos)
     dimI = size(image, /dim)
     for pixRad = pixRadStart, pixRadEnd do begin
        ; set pixRad
      pParamStruct = segObj->getpParamStruct()
      (*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Pixel Radius'))[0]] = pixRad
      segObj->set, pParamStruct = pParamStruct

      if (pixRad gt pixRadStart) then imageRep = image_2
         image_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                         tPos = tPos,$
                                                         chPos = chPos,$
                                                         zPos = zPos,$
                                                         clusPos = clusPos,$
                                                         segPos = segPos-1,$
                                                         cut_x = cut_x, cut_y = cut_y)
      if (pixRad gt pixRadStart) then begin
        (*(*self.pParamStruct).pCorrelVectXAxis)[pixRad - pixRadStart - 1] = pixRad - 1
        (*(*self.pParamStruct).pCorrelVectYAxis)[pixRad - pixRadStart - 1] = total((image_2 - imageRep)^2) / (dimI[0]*dimI[1]*(2*!pi*pixRad))
      window, 10
      tvscl, image_2
      tvscl, imageRep
      tvscl, abs(image_2-imageRep)
      endif
     endfor
     ; reset pixRad
   pParamStruct = segObj->getpParamStruct()
   (*(*pParamStruct).pValues)[(where((*(*pParamStruct).pNames) eq 'Pixel Radius'))[0]] = dummy
   segObj->set, pParamStruct = pParamStruct
   endif else return, image
  endif else return, image

  ch0_vectDELTAR = *(*self.pParamStruct).pCorrelVectXAxis
  ch1_vectDELTAR = *(*self.pParamStruct).pCorrelVectYAxis

  live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> DeltaR Plot'
  if (error ne '') then begin
   style = live_style('plot', base_style = 'basic plot', graphic_properties = {symbol_type : 3, symbol_size : 0.1},$
   visualization_properties = {color : 'Light Red'},$
   xAxis_properties = {axisTitle :'pixel radius', exact: 1, compute_range : 0},$
   yAxis_properties = {axisTitle :'chi square', exact: 1, compute_range : 0},$
   legend_properties = {hide :1})
                 live_plot, ch1_vectDELTAR, independent = ch0_vectDELTAR,$
                                                      draw_dimension = [300,300], /no_select,$
                                                      style = style, title = 's_ImageColoc |-> DeltaR Plot'
;                live_oPlot, yFitDELTAR, independent = xFitDELTAR, /no_select,$
;                                                     window_in = 's_ImageColoc |-> DeltaR Plot'
  endif else begin
;              live_control, xFitDELTAR, /update, window_in = 's_ImageColoc |-> DeltaR Plot'
;              live_control, yFitDELTAR, /update, window_in = 's_ImageColoc |-> DeltaR Plot'
   live_control, ch1_vectDELTAR, /update, window_in = 's_ImageColoc |-> DeltaR Plot'
   live_control, ch0_vectDELTAR, /update, window_in = 's_ImageColoc |-> DeltaR Plot'
  endelse

  live_info, 'CH0_VECTDELTAR Axis', properties = variable, window_in = 's_ImageColoc |-> DeltaR Plot'
  variable.minRange = min(ch0_vectDELTAR) - .05*(max(ch0_vectDELTAR)-min(ch0_vectDELTAR))
  variable.maxRange = max(ch0_vectDELTAR) + .05*(max(ch0_vectDELTAR)-min(ch0_vectDELTAR))
  live_control, 'CH0_VECTDELTAR Axis', properties = variable, window_in = 's_ImageColoc |-> DeltaR Plot'
  live_info, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> DeltaR Plot'
  variable.minRange = min(ch1_vectDELTAR) - .05*(max(ch1_vectDELTAR)-min(ch1_vectDELTAR))
  variable.maxRange = max(ch1_vectDELTAR) + .05*(max(ch1_vectDELTAR)-min(ch1_vectDELTAR))
  live_control, 'Y Axis', properties = variable, window_in = 's_ImageColoc |-> DeltaR Plot'

  return, ((image_2 - imageRep)^2) / n_elements(image)
end


function C_sImageFilter_ImageColocDeltaR::init
   filterStruct = {Name: 'C_ImageColocDeltaR',$   ;  filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on filter Parameter Names.
                           pNames: ptr_new(),$   ; Pointer on filter Parameter Names.
                           pActive: ptr_new(),$  ; Pointer on filter Parameter Active Bool.
                           pMin: ptr_new(),$    ; Pointer on filter Parameter Min_Values.
                           pMax: ptr_new(),$    ; Pointer on filter Parameter Max_Values.
                           pValues: ptr_new(),$    ; Pointer on filter Parameter Values.
                           pCorrelVectXAxis: ptr_new(),$ ; Pointer on CorrelMatrix_1.
                           pCorrelVectYAxis: ptr_new()}  ; Pointer on CorrelMatrix_2.

    filterParamWidgetType = make_array(2, /string, value = 'widget_slider')
    filterParamNames = ['Pixel Radius Start',$
                        'Pixel Radius End']
    filterParamActive = [1, 1]
    filterParamMin = [1, 1]
    filterParamMax = [100, 101]
    filterParamValues = [1, 2]

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

pro C_sImageFilter_ImageColocDeltaR__define
  tmp = {C_sImageFilter_ImageColocDeltaR, pParamStruct: ptr_new(), inherits C_sImageFilter}
end