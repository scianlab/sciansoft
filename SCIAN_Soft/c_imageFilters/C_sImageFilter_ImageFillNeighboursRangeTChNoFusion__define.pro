;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageFillNeighboursRangeTChNoFusion
;
; PURPOSE:
;       - FillNeighbours-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;     Susana Vargas - fill neighbours if is between defined intensity range -
;     the intensity range (threshold range) could be evaluated in other channel/cluster ...
;     Felipe Santibañez - no fusion of ROIs

; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_FillNeighbours' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData     ;pImageData   Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageFillNeighboursRangeTChNoFusion::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ImageFillNeighboursRangeTChNoFusion::apply, image = image,$
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
   print,'--FillNeighboursRangeTChNoFusion  INICIO--'
   timePos = tPos
   channelPos = chPos
   zslicePos = zPos
   clusterPos = clusPos
   
   pParamStruct = selectedStackObject->getpParamStruct() ; Susana , incluyo esto para habilitar las 3 líneas siguiente ¿poder destructivo?
   totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]-1; ¿porque el "-1" al final? ¿porque resto?
   totalChannels = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]]-1; ¿porque el "-1" al final? ¿porque resto?
   totalzSlices = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]-1; ¿porque el "-1" al final? ¿porque resto?

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Segmentation Position'))[0]
   segPos = round((*(*self.pParamStruct).pValues)[whParam]) < segPos
   (*(*self.pParamStruct).pValues)[whParam] = segPos
   
      whParam = (where((*(*self.pParamStruct).pNames) eq 'Time Position'))[0]
   ;segPos = round((*(*self.pParamStruct).pValues)[whParam]) < segPos ; ¿porque < ?
   if ( (*(*self.pParamStruct).pActive)[whParam]) then timePos = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else timePos = (tPos < totalTimes)
   (*(*self.pParamStruct).pValues)[whParam] = timePos
   
      whParam = (where((*(*self.pParamStruct).pNames) eq 'Channel Position'))[0]
   if ( (*(*self.pParamStruct).pActive)[whParam]) then channelPos = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channelPos = (chPos < totalChannels)
   (*(*self.pParamStruct).pValues)[whParam] = channelPos
   
      whParam = (where((*(*self.pParamStruct).pNames) eq 'ZSlice Position'))[0]
   if ( (*(*self.pParamStruct).pActive)[whParam]) then zslicePos = (round((*(*self.pParamStruct).pValues)[whParam]) < totalzSlices) else zslicePos = (zPos < totalzSlices)
   (*(*self.pParamStruct).pValues)[whParam] = zslicePos

      whParam = (where((*(*self.pParamStruct).pNames) eq 'Cluster Position'))[0]; Felipe ¿Como recupero el último clusterPos posible para el TimeChannelZSlice seleccionado? 
   if ( (*(*self.pParamStruct).pActive)[whParam]) then clusterPos = round((*(*self.pParamStruct).pValues)[whParam]) else clusterPos = clusPos
   (*(*self.pParamStruct).pValues)[whParam] = clusterPos

      ; get intensity image
   oImage = selectedStackObject->getSelectedImageObject(tPos = timePos, chPos = channelPos, zPos = zslicePos)
   intImage = 1. * oImage->applyImageSegmentation(selectedStackObject = selectedStackObject,$
                                            stack_tlb = stack_tlb,$
                                            tPos = timePos,$
                                            chPos = channelPos,$
                                            zPos = zslicePos,$
                                            clusPos = clusterPos,$
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
   dimI = size(dummy, /dim);FSantibañez para solventar el problema del shift
   dimIE = dimI + 2;FSantibañez para solventar el problema del shift
   ;image = MAKE_ARRAY(dimIE,/int, value = 0);FSantibañez para solventar el problema del shift
   image = MAKE_ARRAY(dimIE,/BYTE, value = 0b);FSantibañez para solventar el problema del shift
   ; image expandida;FSantibañez para solventar el problema del shift
   image[1:dimIE[0]-2,1:dimIE[1]-2] = dummy;FSantibañez para solventar el problema del shift
   
   alpha = image
   dummy = intImage;FSantibañez para solventar el problema del shift
   ;intImage = MAKE_ARRAY(dimIE,/int, value = 0);FSantibañez para solventar el problema del shift
   intImage = MAKE_ARRAY(dimIE,/FLOAT, value = 0.);FSantibañez para solventar el problema del shift
   intImage[1:dimIE[0]-2,1:dimIE[1]-2] = dummy;FSantibañez para solventar el problema del shift
   
   labelMask = label_region(image,/ALL_NEIGHBORS);ALL_NEIGHBORS = 8-neighbor   default = 4-neighbor   
   maxLM = max(labelMask)
   for i = 0, iterations-1 do begin
      image = alpha
      case f8N of
      0:begin
            alpha >= shift( (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image), 1, 0)  ; right
            alpha >= shift( (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image), 0, 1)  ; up      
            alpha >= shift( (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image), -1, 0)  ; left 
            alpha >= shift( (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image), 0, -1)  ; down
            
            ;TODO adaptar la versión de 8neighbor a esta de 4
      endcase
      1:begin
          tempMove1 = (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low)  * image   ; right
          tempMove2 = (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low)  * image ;up
          tempMove3 = (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low)    * image ; left 
          tempMove4 = (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low)    * image; down
          tempMove5 = (shift(intImage, -1, -1) le threshold_High) and (shift(intImage, -1, -1) ge threshold_Low)* image; right-up
          tempMove6 = (shift(intImage, 1, -1) le threshold_High) and (shift(intImage, 1, -1) ge threshold_Low)  * image; left-up
          tempMove7 = (shift(intImage, -1, 1) le threshold_High) and (shift(intImage, -1, 1) ge threshold_Low)  * image ; right-down
          tempMove8 = (shift(intImage, 1, 1) le threshold_High) and (shift(intImage, 1, 1) ge threshold_Low)    * image ; left-down

          for m = 1, maxLM do begin
            whereObj = where(labelMask eq m)
            if(whereObj[0] ne -1) then begin
                imagenRef = image * 0
                imagenRef[whereObj] = tempMove1[whereObj]
                temp2 =  shift( imagenRef, 1, 0) ; right
                whereObj2 = where((temp2 ne 0))
                if(whereObj2[0] ne -1) then begin
                   whereValido = where((labelMask[whereObj2] eq 0) or (labelMask[whereObj2] eq m))
                   
                   if(whereValido[0] ne -1) then begin
                      alpha[whereObj2[whereValido]] = temp2[whereObj2[whereValido]]
                      labelMask[whereObj2[whereValido]] = m
                   endif
                endif  
               
                imagenRef = image * 0
                ;imagenRef[whereObj] = tempMove1[whereObj]
                imagenRef[whereObj] = tempMove2[whereObj]
                temp2 =  shift(imagenRef, 0, 1)  ; up 
                whereObj2 = where((temp2 ne 0))
                if(whereObj2[0] ne -1) then begin
                   whereValido = where((labelMask[whereObj2] eq 0) or (labelMask[whereObj2] eq m))
                   
                   if(whereValido[0] ne -1) then begin
                      alpha[whereObj2[whereValido]] = temp2[whereObj2[whereValido]]
                      labelMask[whereObj2[whereValido]] = m
                   endif
                endif  
                
                imagenRef = image * 0
                ;imagenRef[whereObj] = tempMove1[whereObj]
                imagenRef[whereObj] = tempMove3[whereObj]
                temp2 =  shift(imagenRef, -1, 0)  ; left
                whereObj2 = where((temp2 ne 0))
                if(whereObj2[0] ne -1) then begin
                   whereValido = where((labelMask[whereObj2] eq 0) or (labelMask[whereObj2] eq m))
                   
                   if(whereValido[0] ne -1) then begin
                      alpha[whereObj2[whereValido]] = temp2[whereObj2[whereValido]]
                      labelMask[whereObj2[whereValido]] = m

                   endif
                endif  
                
                imagenRef = image * 0
                imagenRef[whereObj] = tempMove4[whereObj]
                temp2 =  shift(imagenRef, 0, -1)  ; down
                whereObj2 = where((temp2 ne 0))
                if(whereObj2[0] ne -1) then begin
                   whereValido = where((labelMask[whereObj2] eq 0) or (labelMask[whereObj2] eq m))
                   
                   if(whereValido[0] ne -1) then begin
                      alpha[whereObj2[whereValido]] = temp2[whereObj2[whereValido]]
                      labelMask[whereObj2[whereValido]] = m

                   endif
                endif  
                
                imagenRef = image * 0
                imagenRef[whereObj] = tempMove5[whereObj]
                temp2 =  shift(imagenRef, 1, 1)  ; right-up
                whereObj2 = where((temp2 ne 0))
                if(whereObj2[0] ne -1) then begin
                   whereValido = where((labelMask[whereObj2] eq 0) or (labelMask[whereObj2] eq m))
                   
                   if(whereValido[0] ne -1) then begin
                      alpha[whereObj2[whereValido]] = temp2[whereObj2[whereValido]]
                      labelMask[whereObj2[whereValido]] = m

                   endif
                endif  
                
                imagenRef = image * 0
                imagenRef[whereObj] = tempMove6[whereObj]
                temp2 =  shift( imagenRef, -1, 1)  ; left-up
                whereObj2 = where((temp2 ne 0))
                if(whereObj2[0] ne -1) then begin
                   whereValido = where((labelMask[whereObj2] eq 0) or (labelMask[whereObj2] eq m))
                   if(whereValido[0] ne -1) then begin
                      alpha[whereObj2[whereValido]] = temp2[whereObj2[whereValido]]
                      labelMask[whereObj2[whereValido]] = m
                   endif
                endif  
                
                imagenRef = image * 0
                imagenRef[whereObj] = tempMove7[whereObj]
                temp2 =  shift( imagenRef, 1, -1)  ; right-down
                whereObj2 = where((temp2 ne 0))
                if(whereObj2[0] ne -1) then begin
                   whereValido = where((labelMask[whereObj2] eq 0) or (labelMask[whereObj2] eq m))
                   if(whereValido[0] ne -1) then begin
                      alpha[whereObj2[whereValido]] = temp2[whereObj2[whereValido]]
                      labelMask[whereObj2[whereValido]] = m
                   endif
                endif  
                
                imagenRef = image * 0
                imagenRef[whereObj] = tempMove8[whereObj]
                temp2 =  shift( imagenRef, -1, -1)  ; left-down
                whereObj2 = where((temp2 ne 0))
                if(whereObj2[0] ne -1) then begin
                   whereValido = where((labelMask[whereObj2] eq 0) or (labelMask[whereObj2] eq m))
                   if(whereValido[0] ne -1) then begin
                      alpha[whereObj2[whereValido]] = temp2[whereObj2[whereValido]]
                      labelMask[whereObj2[whereValido]] = m
                   endif
                endif
            endif
          endfor
      endcase
      else:
      endcase
      alpha[0,*] = 0b;FSantibañez para solventar el problema del shift
      alpha[dimIE[0]-1,*] = 0b;FSantibañez para solventar el problema del shift
      alpha[*,0] = 0b;FSantibañez para solventar el problema del shift
      alpha[*,dimIE[1]-1] = 0b;FSantibañez para solventar el problema del shift
   endfor
   
   case f8N of
      0:begin
        ;aqui adaptar para 4neighbor
      endcase
      1:begin
        ;TODO lo que viene a continuacion está pensado para versión de 8neighbor debo adaptar opción para la de 4neighbor
        vectorVecinos = MAKE_ARRAY(8, /INTEGER, VALUE= 0)
        for i = 0,dimIE[0]-1 do begin 
          for j = 0,dimIE[1]-1 do begin
            vectorVecinos[0] = labelMask[(i-1) > 0,j]
            vectorVecinos[1] = labelMask[(i+1) < (dimIE[0]-1),j]
            vectorVecinos[2] = labelMask[i,(j+1) < (dimIE[1]-1)]
            vectorVecinos[3] = labelMask[i,(j-1) > 0]
            vectorVecinos[4] = labelMask[(i-1) > 0,(j-1)  > 0]
            vectorVecinos[5] = labelMask[(i-1) > 0,(j+1) < (dimIE[1]-1)]
            vectorVecinos[6] = labelMask[(i+1) < (dimIE[0]-1),(j-1) > 0]
            vectorVecinos[7] = labelMask[(i+1) < (dimIE[0]-1),(j+1) < (dimIE[1]-1)]
            
            whereNoZero = where(vectorVecinos ne 0)
            if(whereNoZero[0] ne -1) then begin
                whereDead = where(vectorVecinos[whereNoZero] ne labelMask[i,j])
                if(whereDead[0] ne -1) then begin
                    labelMask[i,j] = 0                  
                endif              
            endif
          endfor
        endfor
      endcase
      else:
      endcase
    
    alpha = alpha * 0
    alpha = labelMask gt 0   
   ;pendiente evaluar si puedo y como usar Exclusive - Susana ---- INICIO
   ;if fExcl then return, (alpha gt 0) * ((alpha ge srMax) * (alpha le srMin)) $
   ;       else return, ( (alpha gt 0) * ((alpha ge srMax) + (alpha le srMin)) ) gt 0
   ;pendiente evaluar si puedo y como usar Exclusive - Susana ---- FIN
   ;return, alpha
   ; devolver resultado de dimensiones correctas;FSantibañez para solventar el problema del shift
   return, alpha[1:dimIE[0]-2,1:dimIE[1]-2];FSantibañez para solventar el problema del shift
end


function C_sImageFilter_ImageFillNeighboursRangeTChNoFusion::init
   filterStruct = {Name:'C_ImageFillNeighboursRangeTChNoFusion',$   ; Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

      ; Parameters of OpenClose.
   filterParamWidgetType = make_array(12, /string, value = 'widget_slider')

   filterParamNames = ['Iterations',$
                      'Segmentation Position',$
                      'Time Position',$
                      'Channel Position',$
                      'ZSlice Position',$
                      'Cluster Position',$
                      '0_4Nghbrs_1_8Nghbrs',$
                      'Threshold High',$
                      'Threshold Low',$
                      'xxx',$
                      'yyy',$
                      'Exclusive']
   filterParamActive = [1,1,1,1,1,1,1,1,1,0,0,0]
   filterParamMin = [0,0,0,0,0,0,0,-10000.0,-10000.0,0,0,0]
   filterParamMax = [100,100,100,100,100,100,5,100000.0,100000.0,0,0,1]
   filterParamValues = [1,0,0,0,0,0,1,0.,0.,0,0,1]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_ImageFillNeighboursRangeTChNoFusion__define
   tmp = {C_sImageFilter_ImageFillNeighboursRangeTChNoFusion, pParamStruct:ptr_new(), inherits C_sImageFilter}
end