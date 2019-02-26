;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageFillNeighboursRange
;
; PURPOSE:
;       - FillNeighbours-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;     Susana Vargas - fill neighbours if is between defined intensity range -
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_FillNeighbours' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData     ;pImageData   Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageFillNeighboursRange::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ImageFillNeighboursRange::apply, image = image,$
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

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Threshold Low'))[0]
   ;threshold = round((*(*self.pParamStruct).pValues)[whParam])
   threshold_Low = (*(*self.pParamStruct).pValues)[whParam] ; Susana trato de poder definir un humbral con decimales
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Threshold High'))[0]
   ;threshold_High = round((*(*self.pParamStruct).pValues)[whParam])
   threshold_High = (*(*self.pParamStruct).pValues)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq '0_4Nghbrs_1_8Nghbrs'))[0]
   f8N = (round((*(*self.pParamStruct).pValues)[whParam]) < 1b) > 0b
   
   ;pendiente evaluar si puedo y como usar Exclusive - Susana ---- INICIO
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Exclusive'))[0]
   if ((*(*self.pParamStruct).pActive)[whParam]) then fExcl = 1b else fExcl = 0b
   ;pendiente evaluar si puedo y como usar Exclusive - Susana ---- FIN
   
   ;FSantibañez para solventar el problema del shift ... como todos los shifts son de desplazamiento unitario basta expandir
   ;un pixel en cada direccion la imagen 
   dummy = image;FSantibañez para solventar el problema del shift
   dimI = size (dummy, /dim);FSantibañez para solventar el problema del shift
   dimIE = dimI + 2;FSantibañez para solventar el problema del shift
   image = MAKE_ARRAY(dimIE,/BYTE, value = 0b);FSantibañez para solventar el problema del shift
   image[1:dimIE[0]-2,1:dimIE[1]-2] = dummy;FSantibañez para solventar el problema del shift
   
   alpha = image
   dummy = intImage;FSantibañez para solventar el problema del shift
   intImage = MAKE_ARRAY(dimIE,/FLOAT, value = 0.);FSantibañez para solventar el problema del shift
   intImage[1:dimIE[0]-2,1:dimIE[1]-2] = dummy;FSantibañez para solventar el problema del shift
   for i = 0, iterations-1 do begin
      image = alpha      
      case f8N of
      0:begin
            alpha >= shift( (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image), 1, 0)  ; right
            alpha >= shift( (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image), 0, 1)  ; up      
            alpha >= shift( (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image), -1, 0)  ; left 
            alpha >= shift( (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image), 0, -1)  ; down
      endcase
      1:begin
            alpha >= shift( (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image), 1, 0)  ; right
            alpha >= shift( (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image), 0, 1)  ; up      
            alpha >= shift( (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image), -1, 0)  ; left 
            alpha >= shift( (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image), 0, -1)  ; down
            alpha >= shift( (( (shift(intImage, -1, -1) le threshold_High) and (shift(intImage, -1, -1) ge threshold_Low) ) * image), 1, 1)  ; right-up
            alpha >= shift( (( (shift(intImage, 1, -1) le threshold_High) and (shift(intImage, 1, -1) ge threshold_Low) ) * image), -1, 1)  ; left-up
            alpha >= shift( (( (shift(intImage, -1, 1) le threshold_High) and (shift(intImage, -1, 1) ge threshold_Low) ) * image), 1, -1)  ; right-down
            alpha >= shift( (( (shift(intImage, 1, 1) le threshold_High) and (shift(intImage, 1, 1) ge threshold_Low) ) * image), -1, -1)  ; left-down
      endcase
      else:
      endcase
      alpha[0,*] = 0b;FSantibañez para solventar el problema del shift
      alpha[dimIE[0]-1,*] = 0b;FSantibañez para solventar el problema del shift 
      alpha[*,0] = 0b;FSantibañez para solventar el problema del shift
      alpha[*,dimIE[1]-1] = 0b;FSantibañez para solventar el problema del shift
   endfor
   
   ;pendiente evaluar si puedo y como usar Exclusive - Susana ---- INICIO
   ;if fExcl then return, (alpha gt 0) * ((alpha ge srMax) * (alpha le srMin)) $
   ;       else return, ( (alpha gt 0) * ((alpha ge srMax) + (alpha le srMin)) ) gt 0
   ;pendiente evaluar si puedo y como usar Exclusive - Susana ---- FIN
   
   ;return, alpha
   ; devolver resultado de dimensiones correctas;FSantibañez para solventar el problema del shift
   return, alpha[1:dimIE[0]-2,1:dimIE[1]-2];FSantibañez para solventar el problema del shift
end


function C_sImageFilter_ImageFillNeighboursRange::init
   filterStruct = {Name:'C_ImageFillNeighboursRange',$   ; Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

      ; Parameters of OpenClose.
   filterParamWidgetType = make_array(8, /string, value = 'widget_slider')

   filterParamNames = ['Iterations',$
                      'Segmentation Position',$
                      '0_4Nghbrs_1_8Nghbrs',$
                      'Threshold High',$
                      'Threshold Low',$
                      'xxx',$
                      'yyy',$
                      'Exclusive']
   filterParamActive = [1,1,1,1,1,0,0,0]
   filterParamMin = [0,0,0,-4000.0,-4000.0,0,0,0]
   filterParamMax = [100,10,5,40000.0,40000.0,0,0,1]
   filterParamValues = [1,0,1,0.,0.,0,0,1]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_ImageFillNeighboursRange__define
   tmp = {C_sImageFilter_ImageFillNeighboursRange, pParamStruct:ptr_new(), inherits C_sImageFilter}
end