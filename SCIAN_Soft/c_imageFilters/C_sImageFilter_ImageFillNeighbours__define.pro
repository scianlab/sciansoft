;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageFillNeighbours
;
; PURPOSE:
;       - FillNeighbours-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_FillNeighbours' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData     ;pImageData   Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageFillNeighbours::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ImageFillNeighbours::apply, image = image,$
                        selectedStackObject = selectedStackObject ,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos ,$
                        chPos = chPos ,$
                        zPos = zPos ,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y
                        
   if (size(image, /n_dim) ne 2) then return, image
   image = image ne 0

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Segmentation Position'))[0]
   segPos = round((*(*self.pParamStruct).pValues)[whParam]) < segPos
   (*(*self.pParamStruct).pValues)[whParam] = segPos

      ; get intensity image
   oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
   intImage = 1. * oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                            stack_tlb = stack_tlb,$
                                            tPos = tPos,$
                                            chPos = chPos,$
                                            zPos = zPos,$
                                            clusPos = clusPos,$
                                            segPos = segPos - 1,$
                                            cut_x = cut_x, cut_y = cut_y)

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Iterations'))[0]
   (*(*self.pParamStruct).pValues)[whParam] = (*(*self.pParamStruct).pValues)[whParam] > 1
   iterations = (*(*self.pParamStruct).pValues)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Threshold'))[0]
   ;No redondea umbral
   threshold = (*(*self.pParamStruct).pValues)[whParam] ; Susana trato de poder definir un humbral con decimales
   
   whParam = (where((*(*self.pParamStruct).pNames) eq '0_LT_1_LE_2_LET_3_GT_4_GE_5_GET'))[0]
   operator = round((*(*self.pParamStruct).pValues)[whParam])

   whParam = (where((*(*self.pParamStruct).pNames) eq '0_4Nghbrs_1_8Nghbrs'))[0]
   f8N = (round((*(*self.pParamStruct).pValues)[whParam]) < 1b) > 0b

   alpha = image
   for i = 0, iterations-1 do begin
      image = alpha
      case f8N of
      0:begin
         case operator of
         0: begin
            alpha >= shift( (((shift(intImage, -1, 0) - intImage) le 0) * image), 1, 0)  ; right
            alpha >= shift( (((shift(intImage, 0, -1) - intImage) le 0) * image), 0, 1)  ; up      
            alpha >= shift( (((shift(intImage, 1, 0) - intImage) le 0) * image), -1, 0)  ; left 
            alpha >= shift( (((shift(intImage, 0, 1) - intImage) le 0) * image), 0, -1)  ; down
         endcase
         1: begin
            alpha >= shift( (((shift(intImage, -1, 0) - intImage) lt 0) * image), 1, 0)  ; right
            alpha >= shift( (((shift(intImage, 0, -1) - intImage) lt 0) * image), 0, 1)  ; up      
            alpha >= shift( (((shift(intImage, 1, 0) - intImage) lt 0) * image), -1, 0)  ; left 
            alpha >= shift( (((shift(intImage, 0, 1) - intImage) lt 0) * image), 0, -1)  ; down
         endcase
         2: begin
            alpha >= shift( ((shift(intImage, -1, 0) le threshold) * image), 1, 0)  ; right
            alpha >= shift( ((shift(intImage, 0, -1) le threshold) * image), 0, 1)  ; up      
            alpha >= shift( ((shift(intImage, 1, 0) le threshold) * image), -1, 0)  ; left 
            alpha >= shift( ((shift(intImage, 0, 1) le threshold) * image), 0, -1)  ; down
         endcase
         3: begin
            alpha >= shift( (((shift(intImage, -1, 0) - intImage) ge 0) * image), 1, 0)  ; right
            alpha >= shift( (((shift(intImage, 0, -1) - intImage) ge 0) * image), 0, 1)  ; up      
            alpha >= shift( (((shift(intImage, 1, 0) - intImage) ge 0) * image), -1, 0)  ; left 
            alpha >= shift( (((shift(intImage, 0, 1) - intImage) ge 0) * image), 0, -1)  ; down
         endcase
         4: begin
            alpha >= shift( (((shift(intImage, -1, 0) - intImage) gt 0) * image), 1, 0)  ; right
            alpha >= shift( (((shift(intImage, 0, -1) - intImage) gt 0) * image), 0, 1)  ; up      
            alpha >= shift( (((shift(intImage, 1, 0) - intImage) gt 0) * image), -1, 0)  ; left 
            alpha >= shift( (((shift(intImage, 0, 1) - intImage) gt 0) * image), 0, -1)  ; down
         endcase
         5: begin
            alpha >= shift( ((shift(intImage, -1, 0) ge threshold) * image), 1, 0)  ; right
            alpha >= shift( ((shift(intImage, 0, -1) ge threshold) * image), 0, 1)  ; up      
            alpha >= shift( ((shift(intImage, 1, 0) ge threshold) * image), -1, 0)  ; left 
            alpha >= shift( ((shift(intImage, 0, 1) ge threshold) * image), 0, -1)  ; down
         endcase
         else:
         endcase
      endcase
      1:begin
      case operator of
         0: begin
            alpha >= shift( (((shift(intImage, -1, 0) - intImage) le 0) * image), 1, 0)  ; right
            alpha >= shift( (((shift(intImage, 0, -1) - intImage) le 0) * image), 0, 1)  ; up      
            alpha >= shift( (((shift(intImage, 1, 0) - intImage) le 0) * image), -1, 0)  ; left 
            alpha >= shift( (((shift(intImage, 0, 1) - intImage) le 0) * image), 0, -1)  ; down
            alpha >= shift( (((shift(intImage, -1, -1) - intImage) le 0) * image), 1, 1)  ; right-up
            alpha >= shift( (((shift(intImage, 1, -1) - intImage) le 0) * image), -1, 1)  ; left-up
            alpha >= shift( (((shift(intImage, -1, 1) - intImage) le 0) * image), 1, -1)  ; right-down
            alpha >= shift( (((shift(intImage, 1, 1) - intImage) le 0) * image), -1, -1)  ; left-down
         endcase
         1: begin
            alpha >= shift( (((shift(intImage, -1, 0) - intImage) lt 0) * image), 1, 0)  ; right
            alpha >= shift( (((shift(intImage, 0, -1) - intImage) lt 0) * image), 0, 1)  ; up      
            alpha >= shift( (((shift(intImage, 1, 0) - intImage) lt 0) * image), -1, 0)  ; left 
            alpha >= shift( (((shift(intImage, 0, 1) - intImage) lt 0) * image), 0, -1)  ; down
            alpha >= shift( (((shift(intImage, -1, -1) - intImage) lt 0) * image), 1, 1)  ; right-up
            alpha >= shift( (((shift(intImage, 1, -1) - intImage) lt 0) * image), -1, 1)  ; left-up
            alpha >= shift( (((shift(intImage, -1, 1) - intImage) lt 0) * image), 1, -1)  ; right-down
            alpha >= shift( (((shift(intImage, 1, 1) - intImage) lt 0) * image), -1, -1)  ; left-down
         endcase
         2: begin
            alpha >= shift( ((shift(intImage, -1, 0) le threshold) * image), 1, 0)  ; right
            alpha >= shift( ((shift(intImage, 0, -1) le threshold) * image), 0, 1)  ; up      
            alpha >= shift( ((shift(intImage, 1, 0) le threshold) * image), -1, 0)  ; left 
            alpha >= shift( ((shift(intImage, 0, 1) le threshold) * image), 0, -1)  ; down
            alpha >= shift( ((shift(intImage, -1, -1) le threshold) * image), 1, 1)  ; right-up
            alpha >= shift( ((shift(intImage, 1, -1) le threshold) * image), -1, 1)  ; left-up
            alpha >= shift( ((shift(intImage, -1, 1) le threshold) * image), 1, -1)  ; right-down
            alpha >= shift( ((shift(intImage, 1, 1) le threshold) * image), -1, -1)  ; left-down
         endcase
         3: begin
            alpha >= shift( (((shift(intImage, -1, 0) - intImage) ge 0) * image), 1, 0)  ; right
            alpha >= shift( (((shift(intImage, 0, -1) - intImage) ge 0) * image), 0, 1)  ; up      
            alpha >= shift( (((shift(intImage, 1, 0) - intImage) ge 0) * image), -1, 0)  ; left 
            alpha >= shift( (((shift(intImage, 0, 1) - intImage) ge 0) * image), 0, -1)  ; down
            alpha >= shift( (((shift(intImage, -1, -1) - intImage) ge 0) * image), 1, 1)  ; right-up
            alpha >= shift( (((shift(intImage, 1, -1) - intImage) ge 0) * image), -1, 1)  ; left-up
            alpha >= shift( (((shift(intImage, -1, 1) - intImage) ge 0) * image), 1, -1)  ; right-down
            alpha >= shift( (((shift(intImage, 1, 1) - intImage) ge 0) * image), -1, -1)  ; left-down
         endcase
         4: begin
            alpha >= shift( (((shift(intImage, -1, 0) - intImage) gt 0) * image), 1, 0)  ; right
            alpha >= shift( (((shift(intImage, 0, -1) - intImage) gt 0) * image), 0, 1)  ; up      
            alpha >= shift( (((shift(intImage, 1, 0) - intImage) gt 0) * image), -1, 0)  ; left 
            alpha >= shift( (((shift(intImage, 0, 1) - intImage) gt 0) * image), 0, -1)  ; down
            alpha >= shift( (((shift(intImage, -1, -1) - intImage) gt 0) * image), 1, 1)  ; right-up
            alpha >= shift( (((shift(intImage, 1, -1) - intImage) gt 0) * image), -1, 1)  ; left-up
            alpha >= shift( (((shift(intImage, -1, 1) - intImage) gt 0) * image), 1, -1)  ; right-down
            alpha >= shift( (((shift(intImage, 1, 1) - intImage) gt 0) * image), -1, -1)  ; left-down
         endcase
         5: begin
            alpha >= shift( ((shift(intImage, -1, 0) ge threshold) * image), 1, 0)  ; right
            alpha >= shift( ((shift(intImage, 0, -1) ge threshold) * image), 0, 1)  ; up      
            alpha >= shift( ((shift(intImage, 1, 0) ge threshold) * image), -1, 0)  ; left 
            alpha >= shift( ((shift(intImage, 0, 1) ge threshold) * image), 0, -1)  ; down
            alpha >= shift( ((shift(intImage, -1, -1) ge threshold) * image), 1, 1)  ; right-up
            alpha >= shift( ((shift(intImage, 1, -1) ge threshold) * image), -1, 1)  ; left-up
            alpha >= shift( ((shift(intImage, -1, 1) ge threshold) * image), 1, -1)  ; right-down
            alpha >= shift( ((shift(intImage, 1, 1) ge threshold) * image), -1, -1)  ; left-down
         endcase
         else:
         endcase
      endcase
      else:
      endcase
   endfor
   return, alpha
end


function C_sImageFilter_ImageFillNeighbours::init
   filterStruct = {Name:'C_ImageFillNeighbours',$   ; Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

      ; Parameters of OpenClose.
   filterParamWidgetType = make_array(7, /string, value = 'widget_slider')

   filterParamNames = ['Iterations',$
                      'Segmentation Position',$
                      '0_4Nghbrs_1_8Nghbrs',$
                      '0_LT_1_LE_2_LET_3_GT_4_GE_5_GET',$
                      'Threshold',$
                      'xxx',$
                      'yyy']

   filterParamActive = [1,1,1,1,1,0,0]
   filterParamMin = [0,0,0,0,-4000.,0,0]
   filterParamMax = [100,10,5,5,4000.,0,0]
   filterParamValues = [1,0,1,0,0.,0,0]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_ImageFillNeighbours__define
   tmp = {C_sImageFilter_ImageFillNeighbours, pParamStruct:ptr_new(), inherits C_sImageFilter}
end