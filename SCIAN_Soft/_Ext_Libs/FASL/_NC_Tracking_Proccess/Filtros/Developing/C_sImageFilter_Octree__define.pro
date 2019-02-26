;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Octree
;
; PURPOSE:
;       - Octree-Filter-Class.
;   Obtain an Octree representation by Intensity model
; AUTHOR:
;     Felipe Santibañez Leal(2011)
;     e_mail: cuentasfsantibanez@gmail.com
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Octree' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_Octree::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

pro C_sImageFilter_Octree::getValues, sIToolID = sIToolID
   sIToolID = (*self.pParamStruct).sIToolID
end

function C_sImageFilter_Octree::apply, image = image, stack_tlb = stack_tlb,$
                                     selectedStackObject = selectedStackObject ,$
                                     tPos = tPos ,$
                                     chPos = chPos ,$
                                     zPos = zPos ,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

      ; --- Setup section (parameters not needed frequently) ---------------------------
      verbose = 1             ; (bool) display info like elapsed time and stats
      saveOctree = 1
      ; --- End setup section ----------------------------------------------------------

   whPos = (where((*(*self.pParamStruct).pNames) eq '2nd_Image_Position'))[0]
   pParamStruct = selectedStackObject->getpParamStruct()
   totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]
   if ( (*(*self.pParamStruct).pActive)[whPos]) then $
      whPos = ((tPos + (*(*self.pParamStruct).pValues)[whPos]) >0) < (totalTimes-1) $
      else whPos = (tPos>0) <  (totalTimes-1)

      ; Get Image Object from Container
      
      oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
      image_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject ,$
                                                            tPos = whPos ,$
                                                            chPos = chPos ,$
                                                            zPos = zPos ,$
                                                            clusPos = clusPos,$
                                                            segPos = segPos-1,$
                                                            cut_x = cut_x, cut_y = cut_y)
      ; get dim image
      image *= 1.
      image_2 *= 1.
      dimI = size (image, /dim)
      xSize = dimI[0]
      ySize = dimI[1]
      nPixeles = dimI[0]*dimI[1]
   
      ; Get Parameters for subdivition for Octree Structure
       
      alpha = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Alpha'))[0]]
      iteration = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Iteration'))[0]]

      
      return, ofMagnitude
end


function C_sImageFilter_Octree::init
    filterStruct = {Name: 'C_Octree',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                           sIToolID: '',$    ; String for iTool ID
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of C_Octree.
    filterParamWidgetType = make_array(11, /string, value = 'widget_slider')
    filterParamNames = ['2nd_Image_Position',$       ;     Normalize ImageDifference Values -> [0 'no gradient', 1 'perfect']
                        'Iteration' , 'Alpha' , 'MinMax_Range', 'InterestPoint' ,$
                        'Show In x' , 'Show In y' , 'Show Vector Field', 'Show Image',$
                        'Min Magnitude', 'Sampling Step']
    filterParamActive = [ 1, 1, 1, 1 , 1, 0 ,0 ,1, 1, 1, 1]
    filterParamMin = [-5, 0, 0.001, 0, 1, 0 ,0 ,0, 0, 0, 1]
    filterParamMax = [ 15, 100000, 1000000.0, 10, 200, 1, 1, 1, 1, 1000, 100]
    filterParamValues = [1, 200, 1.0, 10, 120, 1, 0, 0, 1, 0, 1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Octree__define
  tmp = {C_sImageFilter_Octree,$
        pParamStruct: ptr_new(),$
        inherits C_sImageFilter}
end