;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageColocWindow
;
; PURPOSE:
;      ImageColocalization-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;     result = obj_new('C_sImageFilter_ImageColocWindow' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageColocWindow::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_ImageColocWindow::apply, image = image,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

   dimI = size(image, /dim)
   chPos = abs(chPos - 1)

     ; get Image Object from Container
   oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
   if obj_valid(oImage) then begin
      clusterObj = oImage->getSegContainerObj(active = clusPos)
      if obj_valid(clusterObj) then segNum = (clusterObj->count() - 1) else segNum = -1
      for i = 0, segNum do begin
         segObj = clusterObj->get(position = i)
         if obj_valid(segObj) then if(obj_isa(segObj, 'C_SIMAGEFILTER_IMAGEPSFSIGNALCREATOR')) then begin
            pParamStruct = segObj->getpParamStruct()
            dummy = (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]]
            (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]] = 0b
            segObj->set, pParamStruct = pParamStruct
         endif
      endfor
      image_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                                        tPos = tPos,$
                                                        chPos = chPos,$
                                                        zPos = zPos,$
                                                        clusPos = clusPos,$
                                                        segPos = segPos-1,$
                                                        cut_x = cut_x, cut_y = cut_y)
      for i = 0, segNum do begin
         segObj = clusterObj->get(position = i)
         if obj_valid(segObj) then if(obj_isa(segObj, 'C_SIMAGEFILTER_IMAGEPSFSIGNALCREATOR')) then begin
            pParamStruct = segObj->getpParamStruct()
            (*(*pParamStruct).pActive)[(where((*(*pParamStruct).pNames) eq 'Randomize OnOff'))[0]] = dummy
            segObj->set, pParamStruct = pParamStruct
         endif
      endfor
   endif else image_2 = image

      ; open Colocalization Window
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Open Colocalization Window'))[0]
   fOpenColocWin = (*(*self.pParamStruct).pActive)[whParam]

   if fOpenColocWin then begin
      if (n_elements(image_2) gt 0) then begin
        images = make_array(2, dimI[0], dimI[1], type = size(image, /type))
        images[0,*,*] = image_2
        images[1,*,*] = image
      endif else begin
        images = make_array(1, dimI[0], dimI[1], type = size(image, /type))
        images[0,*,*] = image
      endelse
      dimIs = size(images, /dim)
      if widget_info(long((*(*self.pParamStruct).pValues)[whParam]), /valid_id) then begin
        if ( ((*(*self.pParamStruct).pColocWinParam)[0] ne dimIs[0]) or ((*(*self.pParamStruct).pColocWinParam)[1] ne dimIs[1]) $
          or ((*(*self.pParamStruct).pColocWinParam)[2] ne dimIs[2]) ) then begin
          if widget_info(long((*(*self.pParamStruct).pValues)[whParam]), /valid_id) then widget_control, (*(*self.pParamStruct).pValues)[whParam], /destroy
          s_Coloc_Window, images, stack_tlb = stack_tlb, application_tlb = application_tlb
          (*(*self.pParamStruct).pValues)[whParam] = application_tlb
          *(*self.pParamStruct).pColocWinParam = [dimIs[0], dimIs[1], dimIs[2]]
        endif else s_Coloc_update, (*(*self.pParamStruct).pValues)[whParam], newImages = images
      endif else begin
        s_Coloc_Window, images, stack_tlb = stack_tlb, application_tlb = application_tlb
        (*(*self.pParamStruct).pValues)[whParam] = application_tlb
        *(*self.pParamStruct).pColocWinParam = [dimIs[0], dimIs[1], dimIs[2]]
      endelse
   endif else begin
      if widget_info(long((*(*self.pParamStruct).pValues)[whParam]), /valid_id) then widget_control, (*(*self.pParamStruct).pValues)[whParam], /destroy
      (*(*self.pParamStruct).pActive)[whParam] = 0
      (*(*self.pParamStruct).pValues)[whParam] = 0
   endelse

   return, image
end


function C_sImageFilter_ImageColocWindow::init
   filterStruct = {Name: 'C_ImageColocWindow',$   ;  filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on filter Parameter Names.
                           pNames: ptr_new(),$   ; Pointer on filter Parameter Names.
                           pActive: ptr_new(),$   ; Pointer on filter Parameter Active Bool.
                           pMin: ptr_new(),$    ; Pointer on filter Parameter Min_Values.
                           pMax: ptr_new(),$    ; Pointer on filter Parameter Max_Values.
                           pValues: ptr_new(),$
                           pColocWinParam: ptr_new()}; Pointer on Parameter Vector of Colocalization Window.

    filterParamWidgetType = make_array(1, /string, value = 'widget_slider')
    filterParamNames = ['Open Colocalization Window']

    filterParamActive = [1]
    filterParamMin = [0]
    filterParamMax = [100]
    filterParamValues = [-1]

    a = [-1.]
    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)
    filterStruct.pColocWinParam = ptr_new(a, /no_copy)
    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ImageColocWindow__define
  tmp = {C_sImageFilter_ImageColocWindow, pParamStruct: ptr_new(), inherits C_sImageFilter}
end