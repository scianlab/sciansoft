;_____________________________IOISIOI____________________
; NAME:
;   C_sImageFilter_LocalMetaFilterROIs
;
; PURPOSE:
;   - Local Meta Filter on ROIs-Filter-Class.
;   Intended to call another filter upon rectagles that bound ROIs in binary masks, 
;   making this a class of wrapper for local image filtering.
;
; AUTHOR:
;   jjaraw (2012)
;   e_mail: jjaraw@gmail.com 
;
; CALLING SEQUENCE:
;   result = obj_new('C_sImageFilter_LocalMetaFilterROIs')
;
; METHODS:
;   function  ->apply, image = image          ; Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct    ; pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct    ; pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________
function C_sImageFilter_LocalMetaFilterROIs::getImageFilterType
  return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_LocalMetaFilterROIs::apply, image = image,$
                                                    cut_x = cut_x,$
                                                    cut_y = cut_y,$
                                                    tPos    = tPos,$
                                                    zPos    = zPos,$
                                                    chPos   = chPos,$
                                                    clusPos = clusPos,$
                                                    segPos  = segPos,$
                                                    stack_tlb = stack_tlb,$
                                                    selectedStackObject = selectedStackObject

  if (size(image, /n_dim) ne 2) then return, image
  imageSize = size(image, /dim)

  whParam = (where((*(*self.pParamStruct).pNames) eq 'FilterIndex'))[0]
  filterIdx = (*(*self.pParamStruct).pValues)[whParam] > 0
  ; Index for...
  ; C_sImageFilter_ThresholdOtsu : around 90-95
  filterNameList = s_ISegM_Image_getFilterObjectList()
  filterName = filterNameList[filterIdx]
  if (filterName eq 'C_sImageFilter_LocalMetaFilterROIs') then begin
    print, 'This filter cannot be used to call itself (FilterIndex is ', filterIdx, '). Please change the FilterIndex parameter value.'
    return, image
  endif

  whParam = (where((*(*self.pParamStruct).pNames) eq 'ROI_box_width'))[0]
  roiBoxWidth = (*(*self.pParamStruct).pValues)[whParam] > 0

  nParams = n_elements(*(*self.pParamStruct).pNames)
  npCount = 0
  filterParamValues = [-1.0]
  if (nParams gt 0) then for i = 0, nParams-1 do begin
    fMatch = strMatch((*(*self.pParamStruct).pNames)[i], 'FilterParam[0-9]*', /FOLD_CASE)
    if (fMatch eq 1) then begin
      ;print, 'Found ', (*(*self.pParamStruct).pNames)[i], ' with value ', (*(*self.pParamStruct).pValues)[i]
      filterParamValues = [filterParamValues, (*(*self.pParamStruct).pValues)[i]]
      npCount += 1
    endif
  endfor
  if (npCount gt 0) then filterParamValues = filterParamValues[1:*]

  labeledImage = label_region(image, /ALL_NEIGHBORS, /ULONG)
  nRois = max(labeledImage)

  if (nRois eq 0) then begin
    print, 'No ROIs to process. Returning the input image...'
    return, image
  endif

  print, 'Creating filter object instance for ', filterName
  oFilter = obj_new(filterName)
  self->setFilterParams, oFilter, filterParamValues
  intImage = (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
  filteredImage = image * 0

  for i = 1L, nRois do begin
    whRoi = where(labeledImage eq i)
    whRoiX = whRoi mod imageSize[0] 
    whRoiY = whRoi / imageSize[0]
    minX = min(whRoiX, max = maxX)
    minY = min(whRoiY, max = maxY)
    minX -= roiBoxWidth
    maxX += roiBoxWidth
    minY -= roiBoxWidth
    maxY += roiBoxWidth
    minX >= 0
    minY >= 0
    maxX <= imageSize[0]-1
    maxY <= imageSize[1]-1
    subImage = intImage[minX:maxX, minY:maxY]
    filterType = oFilter->getImageFilterType()
    case filterType of
      'Single_Image_Filter_Method'  : filteredSubImage = oFilter->apply(image = subImage)
      'Multiple_Image_Filter_Method': filteredSubImage = oFilter->apply(image = subImage,$
                                                                        selectedStackObject = selectedStackObject,$
                                                                        stack_tlb = stack_tlb,$
                                                                        tPos = tPos,$
                                                                        chPos = chPos,$
                                                                        zPos = zPos,$
                                                                        clusPos = clusPos,$
                                                                        segPos = segPos,$
                                                                        cut_x = cut_x, cut_y = cut_y)
      else: begin
            print, 'Unrecognized image filter type: ', filterType, '. Returning original image...'
            return, image
            endcase
    endcase
    filteredImage[minX:maxX, minY:maxY] = filteredSubImage
  endfor
  if obj_valid(oFilter) then obj_destroy, oFilter
  return, filteredImage
end


; setFilterParams
;
; PURPOSE:
;   Utility method to set the parameters for a given filter.
;
; ARGUMENTS:
;
; oFilter        The filter object instance whose parameters will be set.
; paramValues    An array that specifies the parameter values to be set.
; paramPositions Optional argument consisting of an array that indicates which parameters of the filter will be set, 
;                by giving their positions. Ffor example, paramPositions = [1, 4] would indicate that only the filter
;                parameters at positions 1 and 4 should be set with the values given in the paramValues array.
;                If left empty, paramPositions will be set and used as a list of consecutive elements starting from 0
;                (i.e. trying to set the parameters with all of the elements from paramValues).
;                NOTE: The number of elements in paramValues and paramPositions must be the same.
pro C_sImageFilter_LocalMetaFilterROIs::setFilterParams, oFilter, paramValues, paramPositions

  filterClassName = 'C_sImageFilter'
  nParams = n_elements(paramValues)
  if (n_elements(paramPositions) eq 0) then paramPositions = indGen(nParams)

  if obj_isa(oFilter, filterClassName) and (nParams gt 0) then begin
    filterpParamStruct = oFilter->getpParamStruct()
    nParamsFilter = n_elements(*((*filterpParamStruct).pNames))
    nLimit = nParams < nParamsFilter
    for i = 0, nLimit-1 do begin
      print, 'Setting parameter ', (*((*filterpParamStruct).pNames))[paramPositions[i]], ' with value ', paramValues[i] 
      (*((*filterpParamStruct).pValues))[paramPositions[i]] = paramValues[i]
    endfor
  endif else print, 'ERROR, object ', oFilter, ' is not a ', filterClassName, ' instance'
end


function C_sImageFilter_LocalMetaFilterROIs::init
  filterStruct = {Name        : 'C_LocalFilterOnROIs',$ ; Filter Name.
                  pWidgetType : ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pNames      : ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pActive     : ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
                  pMin        : ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
                  pMax        : ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
                  pValues     : ptr_new()}  ; Pointer on Filter Parameter Values.

  ; Parameters of C_sImageFilter_LocalMetaFilterROIs.
  filterParamNames  = ['FilterIndex', 'ROI_box_width', 'FilterParam1', 'FilterParam2', 'FilterParam3'] ; TODO make room for more FilterParam parameters... for-endfor
  filterParamWidgetType = make_array(n_elements(filterParamNames), /string, value = 'widget_slider')
  filterParamActive = [     1,  1, 1, 1, 1]
  filterParamMin    = [     0,  0, 0, 0, 0]
  filterParamMax    = [1023.0, 100.0, 1.0, 1.0, 1.0]
  initialIdx = (where(s_ISegM_Image_getFilterObjectList() eq 'C_sImageFilter_ThresholdOtsu'))[0] > 0
  filterParamValues = [initialIdx, 0.0, 0.0, 0.0, -1.0]
  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
  filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct = ptr_new(filterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_LocalMetaFilterROIs__define
  tmp = {C_sImageFilter_LocalMetaFilterROIs, pParamStruct:ptr_new(), inherits C_sImageFilter}
end
