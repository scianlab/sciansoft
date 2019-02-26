;_____________________________IOISIOI (THIS IS ART)_____________________________
; NAME:
;     C_sImageFilter_OpticalFlow
;
; PURPOSE:
;     - OpticalFlow-Filter-Class.
;
; AUTHORS:
;     L Oyarzo (2003), J Delpiano (2010), J Jara & J Scheer (2011)
;
; CALLING SEQUENCE:
;     result = obj_new('C_sImageFilter_OpticalFlow')
;
; METHOHDS:
;     function ->getImageFilterType
;     function ->apply, ...                      (image data parameters)
;     pro ->set, pParamStruct = pParamStruct     ;pParamStruct Pointer on filterStruct-Data
;     pro ->get, pParamStruct = pParamStruct     ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI (THIS IS ART)_____________________________

function C_sImageFilter_OpticalFlowC::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_OpticalFlowC::getImageFilterType
  return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_OpticalFlowC::apply, image = image, stack_tlb = stack_tlb, selectedStackObject = selectedStackObject,$
                                      tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, segPos = segPos,$
                                      cut_x = cut_x, cut_y = cut_y

  pParamStruct = selectedStackObject->getpParamStruct()
  totalTimes = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Total Number of Times'))[0]]

  method     = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'HS_(0)_CLG_(1)'))[0]]
  iterations = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Iterations'))[0]]
  alpha      = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Alpha'))[0]]
  fShow      = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Show_Vector_Field'))[0]]
  fShowROI   = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Show_Vector_Field_ROI'))[0]]
  roiClusPos = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'ROI_clusPos'))[0]]
  roiChPos   = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'ROI_chPos'))[0]]
  roiSegPos  = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'ROI_segPos'))[0]]
  clg_rho    =((*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'rho_(CLG)'))[0]]) > 1

  wh2nd = (where((*(*self.pParamStruct).pNames) eq '2nd_Image_Position'))[0]
  if ((*(*self.pParamStruct).pActive)[wh2nd]) $
  then pos2 = ((tPos + (*(*self.pParamStruct).pValues)[wh2nd]) > 0) < (totalTimes-1) $
  else pos2 = (tPos > 0) < (totalTimes-1)

  ; get image object from container
  oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
  image_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                           tPos = pos2,$
                                           chPos = chPos,$
                                           zPos = zPos,$
                                           clusPos = clusPos,$
                                           segPos = segPos-1,$
                                           cut_x = cut_x, cut_y = cut_y)
    ; If no filter has been defined, get the original image
  if ~(keyword_set(image_2)) then $
    image_2 = (selectedStackObject->getSelectedImage(tPos = pos2, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]

    ; Horn & Schunck OF parameters
  verbose = 1b           ; (bool) display info like elapsed time and stats
  doShift = 0b           ; (bool) computing derivatives with IDL shift function... don't know yet if it's faster
  boundaryConditions = 1 ; consider boundary conditions of Horn & Schunck 1981
  stopCrit = 0           ; 0 - num iterations / 1 - no improvement

  case 1 of
    method eq 0: opticalFlowHS, image, image_2, un, vn, ALPHA = alpha, ITERATIONS = iterations, DOSHIFT = doShift, verbose = verbose, boundaryConditions = boundaryConditions
    method eq 1: opticalFlowCLG, image, image_2, un, vn, ALPHA = alpha, ITERATIONS = iterations, RHO = clg_rho, verbose = verbose
    else: begin
      print, 'no match for method (', method, ') was found. computing HS-OF as default'
      opticalFlowHS, image, image_2, un, vn, ALPHA = alpha, ITERATIONS = iterations
    endcase
  endcase

    ; TODO OF visualization options...
  fShowROI = 0
  fShow = 1
  if (fShow eq 1) then begin
    print, "show in roi... not yet"
    ; pos2 zPos chPos clusPos cut_x cut_y
    roiImage = fShowROI ? roiImage : -1 ; TODO get ROI image
    opticalFlowShowOF, un, vn, image1 = image1, onlyInROI = fShowROI, ROIimage = roiImage
    whRoi = where(ROIimage eq 1)
    if (ROIimage ne -1 ) then begin
      print, 'OF average in ROI: ', mean(un[whRoi]), mean(vn[whROI])
    endif else begin
      print, 'OF average in ROI: ', mean(un), mean(vn)
    endelse
      
  endif

  ;mcerda: this call refers to and old definition of oOF
;  un = oOF->get_u()
;  uv = oOF->get_v()
;  obj_destroy, oOF
  ; TODO don't know if destroying oImage...
  ;obj_destroy, oImage

  mean_un=mean(un) ;whROI
  mean_uv=mean(vn) ;whROI
  print, 'OF average: ', mean_un, mean_uv

  ; return OF magnitude
  return, sqrt(un * un + vn * vn)
end


function  C_sImageFilter_OpticalFlowC::init
  filterStruct = {Name: 'C_OpticalFlow',$  ; Filter Name.
                  pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pNames:      ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pActive:     ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
                  pMin:        ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
                  pMax:        ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
                  pValues:     ptr_new()}  ; Pointer on Filter Parameter Values.

  ; Parameters of C_OpticalFlow.
  filterParamNames = ['2nd_Image_Position',$
                      'Iterations',$
                      'Alpha',$
                      'HS_(0)_CLG_(1)',$
                      'rho_(CLG)',$
                      'Show_Vector_Field',$
                      'Show_Vector_Field_ROI',$
                      'ROI_clusPos',$
                      'ROI_chPos',$
                      'ROI_segPos']

  filterParamWidgetType = make_array(n_elements(filterParamNames), /string, value = 'widget_slider')
  filterParamActive = [1,    1,      1,       1, 1,  1, 0,   0,   0,   0]
  filterParamMin    = [1,    0.0,    0.001,   0, 1,  0, 0,  -1,  -1,  -1]
  filterParamMax    = [1000, 5000.0, 10000.0, 1, 20, 1, 1, 100, 100, 100]
  filterParamValues = [1,    200,    25.0,    0, 2,  1, 0,  -1,  -1,  -1]

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames      = ptr_new(filterParamNames,      /no_copy)
  filterStruct.pActive     = ptr_new(filterParamActive,     /no_copy)
  filterStruct.pMin        = ptr_new(filterParamMin,        /no_copy)
  filterStruct.pMax        = ptr_new(filterParamMax,        /no_copy)
  filterStruct.pValues     = ptr_new(filterParamValues,     /no_copy)

  self.pParamStruct = ptr_new(filterStruct, /no_copy)
  return, 1
end

pro C_sImageFilter_OpticalFlowC__define
  tmp = {C_sImageFilter_OpticalFlowC, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
