;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ImageFillNeighboursRangeTChBorde
;
; PURPOSE:
;       - FillNeighbours-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;     Susana Vargas - fill neighbours if is between defined intensity range -
;     the intensity range (threshold range) could be evaluated in other channel/cluster ...
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_FillNeighbours' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData     ;pImageData   Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_ImageFillNeighboursRangeTChBorde::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ImageFillNeighboursRangeTChBorde::apply, image = image,$
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
   ;alpha = 1.;susana
   vTotal = 0.;susana
   ;alphaResp = image;susana
   alpha2 = image * 0.;susana
   alpha2NOT = image;susana, para borrar permanentemente ROIs
   dummy = intImage;FSantibañez para solventar el problema del shift
   ;intImage = MAKE_ARRAY(dimIE,/int, value = 0);FSantibañez para solventar el problema del shift
   intImage = MAKE_ARRAY(dimIE,/FLOAT, value = 0.);FSantibañez para solventar el problema del shift
   intImage[1:dimIE[0]-2,1:dimIE[1]-2] = dummy;FSantibañez para solventar el problema del shift
   for i = 0, iterations-1 do begin
      image = alpha
      case f8N of
      0:begin
            vright = (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image)  ; right
            vup = (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image)  ; up      
            vleft = (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image)  ; left 
            vdown = (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image)  ; down
            vTotal = vright + vup + vleft + vdown
            if(vTotal ge 3) then begin
              alpha >= shift( (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image), 1, 0)  ; right
              alpha >= shift( (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image), 0, 1)  ; up      
              alpha >= shift( (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image), -1, 0)  ; left 
              alpha >= shift( (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image), 0, -1)  ; down
            endif
      endcase
      1:begin
;            vright = (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image)  ; right
;            vup = (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image)  ; up      
;            vleft = (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image)  ; left 
;            vdown = (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image)  ; down
;            vright_up = (( (shift(intImage, -1, -1) le threshold_High) and (shift(intImage, -1, -1) ge threshold_Low) ) * image)  ; right-up
;            vleft_up = (( (shift(intImage, 1, -1) le threshold_High) and (shift(intImage, 1, -1) ge threshold_Low) ) * image)  ; left-up
;            vright_down = (( (shift(intImage, -1, 1) le threshold_High) and (shift(intImage, -1, 1) ge threshold_Low) ) * image)  ; right-down
;            vleft_down = (( (shift(intImage, 1, 1) le threshold_High) and (shift(intImage, 1, 1) ge threshold_Low) ) * image)  ; left-down
;            
;            vTotal = vright + vup + vleft + vdown + vright_up + vleft_up + vright_down + vleft_down

            vTotal = image * 0.
            vTotalwhereII = (where(   shift( (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image), 1, 0)   )) ; right
            if (vTotalwhereII[0] gt -1) then vTotal[vTotalwhereII] += 1
            ;print,vTotal[vTotalwhereII]
            
            vTotalwhereII = (where(   shift( (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image), 0, 1)   )) ;up
            if (vTotalwhereII[0] gt -1) then vTotal[vTotalwhereII] += 1
            ;print,vTotal[vTotalwhereII]
            
            vTotalwhereII = (where(   shift( (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image), -1, 0)   )) ;left
            if (vTotalwhereII[0] gt -1) then vTotal[vTotalwhereII] += 1
            ;print,vTotal[vTotalwhereII]
            
            vTotalwhereII = (where(   shift( (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image), 0, -1)   ));down
            if (vTotalwhereII[0] gt -1) then vTotal[vTotalwhereII] += 1
            ;print,vTotal[vTotalwhereII]
            
            vTotalwhereII = (where(   shift( (( (shift(intImage, -1, -1) le threshold_High) and (shift(intImage, -1, -1) ge threshold_Low) ) * image), 1, 1)   )); right-up
            if (vTotalwhereII[0] gt -1) then vTotal[vTotalwhereII] += 1
            ;print,vTotal[vTotalwhereII]
            
            vTotalwhereII = (where(   shift( (( (shift(intImage, 1, -1) le threshold_High) and (shift(intImage, 1, -1) ge threshold_Low) ) * image), -1, 1)   )); left-up
            if (vTotalwhereII[0] gt -1) then vTotal[vTotalwhereII] += 1
            ;print,vTotal[vTotalwhereII]
            
            vTotalwhereII = (where(   shift( (( (shift(intImage, -1, 1) le threshold_High) and (shift(intImage, -1, 1) ge threshold_Low) ) * image), 1, -1)   )); right-down
            if (vTotalwhereII[0] gt -1) then vTotal[vTotalwhereII] += 1
            ;print,vTotal[vTotalwhereII]
            
            vTotalwhereII = (where(   shift( (( (shift(intImage, 1, 1) le threshold_High) and (shift(intImage, 1, 1) ge threshold_Low) ) * image), -1, -1)   )); left-down
            if (vTotalwhereII[0] gt -1) then vTotal[vTotalwhereII] += 1
            ;print,vTotal[vTotalwhereII]
            
;            vTotal = 0
;            vTotal += (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image)  ; right
;            vTotal += (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image)  ; up      
;            vTotal += (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image)  ; left 
;            vTotal += (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image)  ; down
;            vTotal += (( (shift(intImage, -1, -1) le threshold_High) and (shift(intImage, -1, -1) ge threshold_Low) ) * image)  ; right-up
;            vTotal += (( (shift(intImage, 1, -1) le threshold_High) and (shift(intImage, 1, -1) ge threshold_Low) ) * image)  ; left-up
;            vTotal += (( (shift(intImage, -1, 1) le threshold_High) and (shift(intImage, -1, 1) ge threshold_Low) ) * image)  ; right-down
;            vTotal += (( (shift(intImage, 1, 1) le threshold_High) and (shift(intImage, 1, 1) ge threshold_Low) ) * image)  ; left-down
            
            vTotalwhere = (where(vTotal ge 6))
            vTotalwhereNOT = (where(vTotal lt 6))
            ;if (vTotalwhereNOT[0] gt -1) then vTotalwhereNOT = (where(alpha[vTotalwhereNOT] gt 0))
            
            if ((iterations eq 1) and (vTotalwhere[0] lt 0)) then return, alpha = image * 0.;return, alpha[1:dimIE[0]-2,1:dimIE[1]-2]
            
            alpha2 = alpha * 0.
            ;alpha2[vTotalwhere] >= alpha[vTotalwhere]
            alpha2[vTotalwhere] = 1.
            
            ;if ((iterations eq 1) and (vTotalwhere[0] ge 0)) then alpha2NOT[vTotalwhereNOT] = 0.;solo la primera iteración si no hay bode  elimino ROI
            if ((iterations eq 1) and (vTotalwhereNOT[0] ge 0)) then alpha2NOT[vTotalwhereNOT] = 0.;solo la primera iteración si no hay bode  elimino ROI
            
            if (vTotalwhere[0] gt -1) then begin
             ;alpha2 = alpha ;image * 0.
             ;alpha2 = image * 0.
             ;alpha2[vTotalwhere] = alpha[vTotalwhere]
             ;vTotalwhereNOT = (where(vTotal lt 6))
             ;vTotalwhereNOT = (where(alpha[vTotalwhereNOT] gt 0))
             ;print,max(vTotal[vTotalwhere])
             alpha <= alpha2NOT
             
             alpha >= shift( (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image), 1, 0)  ; right
             alpha >= shift( (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image), 0, 1)  ; up      
             alpha >= shift( (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image), -1, 0)  ; left 
             alpha >= shift( (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image), 0, -1)  ; down
             alpha >= shift( (( (shift(intImage, -1, -1) le threshold_High) and (shift(intImage, -1, -1) ge threshold_Low) ) * image), 1, 1)  ; right-up
             alpha >= shift( (( (shift(intImage, 1, -1) le threshold_High) and (shift(intImage, 1, -1) ge threshold_Low) ) * image), -1, 1)  ; left-up
             alpha >= shift( (( (shift(intImage, -1, 1) le threshold_High) and (shift(intImage, -1, 1) ge threshold_Low) ) * image), 1, -1)  ; right-down
             alpha >= shift( (( (shift(intImage, 1, 1) le threshold_High) and (shift(intImage, 1, 1) ge threshold_Low) ) * image), -1, -1)  ; left-down
            
             ;alpha2[vTotalwhere] >= alpha[vTotalwhere]
             
             ;;alpha2[vTotalwhere] = max(alpha)
             ;;alpha2[vTotalwhereNOT] = min(alpha); esto peligroso, borra lo que no tiene contorno dentro de rango
             ;;alpha2[vTotalwhereNOT] = 0.; esto peligroso, borra lo que no tiene contorno dentro de rango
             ;alpha[vTotalwhere] = alpha2[vTotalwhere]
             
             ;alpha2 <= alpha2NOT
             ;alpha[vTotalwhere] = 1. ;max(alpha)
             alpha[vTotalwhereNOT] = alpha2[vTotalwhereNOT]
             
             ;alpha[vTotalwhere] = alpha2NOT[vTotalwhere]; borro ROIs que en primera iteración no presentaron borde
             ;alpha <= alpha2NOT; borro ROIs que en primera iteración no presentaron borde
            endif
;            ;alpha2[vTotalwhere] >= alpha[vTotalwhere]
;            if (vTotalwhere[0] gt -1) then alpha2[vTotalwhere] = max(alpha)
;            ;alpha2[vTotalwhereNOT] = min(alpha); esto peligroso, borra lo que no tiene contorno dentro de rango
;            if (vTotalwhereNOT[0] gt -1) then alpha2[vTotalwhereNOT] = 0.; esto peligroso, borra lo que no tiene contorno dentro de rango
;            if (vTotalwhere[0] gt -1) then alpha[vTotalwhere] = alpha2[vTotalwhere]
;            if (vTotalwhereNOT[0] gt -1) then alpha[vTotalwhereNOT] = alpha2[vTotalwhereNOT]
             ;alpha[vTotalwhere] = max(alpha)
            
;            vTotalwhere = (where(vTotal ge 6))
;            if (vTotalwhere[0] lt 0) then begin
;             ;alpha2 = alpha ;image * 0.; borrar esta linea
;             alpha2 = 0.
;             vTotalwhereNOT = (where(vTotal lt 6)); borrar esta linea
;             vTotalwhereNOT = (where(alpha[vTotalwhereNOT] gt 0)); borrar esta linea
;             ;alpha2[vTotalwhere] = max(alpha); borrar esta linea
;             alpha2[vTotalwhereNOT] = min(alpha); esto peligroso, borra lo que no tiene contorno dentro de rango
;             ;alpha[vTotalwhere] = alpha2[vTotalwhere]
;             alpha[vTotalwhereNOT] = alpha2[vTotalwhereNOT]
;            endif
            ;return, alpha[1:dimIE[0]-2,1:dimIE[1]-2]; borrar esta linea
            
            ;if(vTotal ge (0.85 * 8)) then begin
;              alpha += shift( (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image), 1, 0)  ; right
;              alpha += shift( (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image), 0, 1)  ; up      
;              alpha += shift( (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image), -1, 0)  ; left 
;              alpha += shift( (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image), 0, -1)  ; down
;              alpha += shift( (( (shift(intImage, -1, -1) le threshold_High) and (shift(intImage, -1, -1) ge threshold_Low) ) * image), 1, 1)  ; right-up
;              alpha += shift( (( (shift(intImage, 1, -1) le threshold_High) and (shift(intImage, 1, -1) ge threshold_Low) ) * image), -1, 1)  ; left-up
;              alpha += shift( (( (shift(intImage, -1, 1) le threshold_High) and (shift(intImage, -1, 1) ge threshold_Low) ) * image), 1, -1)  ; right-down
;              alpha += shift( (( (shift(intImage, 1, 1) le threshold_High) and (shift(intImage, 1, 1) ge threshold_Low) ) * image), -1, -1)  ; left-down
;            
;              ;alpha = (alpha gt 2)
;              ;alpha /= max(alpha)
            
;              alpha >= shift( (( (shift(intImage, -1, 0) le threshold_High) and (shift(intImage, -1, 0) ge threshold_Low) ) * image), 1, 0)  ; right
;              alpha >= shift( (( (shift(intImage, 0, -1) le threshold_High) and (shift(intImage, 0, -1) ge threshold_Low) ) * image), 0, 1)  ; up      
;              alpha >= shift( (( (shift(intImage, 1, 0) le threshold_High) and (shift(intImage, 1, 0) ge threshold_Low) ) * image), -1, 0)  ; left 
;              alpha >= shift( (( (shift(intImage, 0, 1) le threshold_High) and (shift(intImage, 0, 1) ge threshold_Low) ) * image), 0, -1)  ; down
;              alpha >= shift( (( (shift(intImage, -1, -1) le threshold_High) and (shift(intImage, -1, -1) ge threshold_Low) ) * image), 1, 1)  ; right-up
;              alpha >= shift( (( (shift(intImage, 1, -1) le threshold_High) and (shift(intImage, 1, -1) ge threshold_Low) ) * image), -1, 1)  ; left-up
;              alpha >= shift( (( (shift(intImage, -1, 1) le threshold_High) and (shift(intImage, -1, 1) ge threshold_Low) ) * image), 1, -1)  ; right-down
;              alpha >= shift( (( (shift(intImage, 1, 1) le threshold_High) and (shift(intImage, 1, 1) ge threshold_Low) ) * image), -1, -1)  ; left-down
            ;endif
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


function C_sImageFilter_ImageFillNeighboursRangeTChBorde::init
   filterStruct = {Name:'C_ImageFillNeighboursRangeTChBorde',$   ; Filter Name.
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

pro C_sImageFilter_ImageFillNeighboursRangeTChBorde__define
   tmp = {C_sImageFilter_ImageFillNeighboursRangeTChBorde, pParamStruct:ptr_new(), inherits C_sImageFilter}
end