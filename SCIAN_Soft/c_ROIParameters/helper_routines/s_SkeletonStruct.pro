;;s_SkeletonStruct
;;
;; DESCRIPTION:
;;
;;  This set of routines is used to build a tree data structure
;;  
;;
;;===========================================================================

;; Procedure SkeletonStructTree_New
PRO SkeletonStructTree_New, Tree

   SKELETON_NODE = {SKELETON_NODE, nHijos     : ptr_new(),$
                                   padre      : ptr_new(),$
                                   hijo       : ptr_new(),$
                                   hermanoD   : ptr_new(),$
                                   hermanoI   : ptr_new(),$
                                   nodo       : ptr_new(),$
                                   dis        : ptr_new(),$
                                   nodoPadre  : ptr_new(),$
                                   vertexNodo : ptr_new(),$
                                   vertexPadre: ptr_new()}
   ;; Make a header for the tree, using anonymous structures:
   tree = { cnt:0l, pHead:ptr_new()}
end


;; Create a new tree node, insert the data and return the pointer:
FUNCTION SkeletonStructTree_NewNode, Data

   Tmp = {SKELETON_NODE}
   tmp.nodo = (*Data)[0]
   tmp.dis = (*Data)[1]
   tmp.nodoPadre = (*Data)[2]
   tmp.vertexNodo = (*Data)[3]
   tmp.vertexPadre = (*Data)[4]
   tmp.nhijos = (*Data)[5]

   return, ptr_new(tmp)
END


pro SkeletonStructTree_insert, Tree, Data

   ;; Data existe?
   if(N_Elements(Data) eq 0)then BEGIN
      message, "Data value undefined",/continue
      return
   endif

   ; hay nodos?
   if(Tree.pHead eq ptr_new())then begin
       tree.pHead = SkeletonStructTree_NewNode(Data)
       tree.cnt = tree.cnt+1
   endif else begin
       _SkeletonStructTree_Insert, Tree, Tree.pHead, Data
   endelse
end


PRO _SkeletonStructTree_Insert, Tree, pNode, Data
    
    elementoB = SkeletonStructEncontrarElem(*(*Data)[2], Tree.phead)
    if ~ptr_valid(elementoB) then begin
      PRINT,'ELEMENTO NO ENCONTRADO'
;      ;TODO si no lo encuentra, guardar data para al final volver a buscar su padre.
;      dim=size(noEnco,/dim)
;      if dim eq 1 and not(ptr_valid(noEnco))then begin
;        noEnco[0]=Data
;      endif else begin
;        aux=noEnco
;        noEnco=ptrarr(dim+1)
;        noEnco[0:dim-1]=aux
;        noEnco[dim]=Data
;      endelse
      ;Result = DIALOG_MESSAGE('No se encontro el elemento')
    endif else begin
        ;hijo = 0 && hermano = 1
        if (*(*Data)[6] eq 1) then begin
        
            if(ptr_valid((*pNode).hermanoD))then begin  ; continue traverse
                SkeletonStructTree_new, temp
                temp=(*pNode).hermanoD
                (*pNode).hermanoD = SkeletonStructTree_NewNode(Data)
                (*(*pNode).hermanoD).hermanoI = pNode
                (*(*pNode).hermanoD).hermanoD = temp 
                (*temp).hermanoI = (*pNode).hermanoD
                ptr_free,temp
                tree.cnt = tree.cnt+1;
            endif else begin
                (*pNode).hermanoD = SkeletonStructTree_NewNode(Data)
                ;_SkeletonStructTree_Insert, Tree, Data, (*pNode).hermanoD, tipo
                (*(*pNode).hermanoD).hermanoI = pNode 
                tree.cnt = tree.cnt+1;
            endelse  
            if ptr_valid((*ElementoB).padre) then begin
              (*(*pNode).hermanoD).padre = (*ElementoB).padre
            endif
        
        endif else begin
          
            if(ptr_valid((*ElementoB).hijo))then begin 
               pNode=(*ElementoB).hijo
               while ptr_valid((*pNode).hermanoD) do begin
                  pNode=(*pNode).hermanoD
               endwhile
               (*pNode).hermanoD = SkeletonStructTree_NewNode(Data)
               (*(*pNode).hermanoD).padre = ElementoB 
               (*(*pNode).hermanoD).hermanoI = pNode
               tree.cnt = tree.cnt+1
            endif else begin
                (*ElementoB).hijo = SkeletonStructTree_NewNode(Data)
                (*(*ElementoB).hijo).padre = ElementoB 
                tree.cnt = tree.cnt+1
            endelse
            
        endelse
    
  endelse
     
END


FUNCTION SkeletonStructEncontrarElem, elem, root
    enco = 0
    elementoB = -1
    elementoB = _SkeletonStructFind_Element(root, elem, enco)
    enco = 0
    return, elementoB
END


;TODO transformar a iterativo (pila o estructura similar)... recursivo es muy lento
FUNCTION _SkeletonStructFind_Element, rootAux, elem, enco
    
    if ~ptr_valid(rootAux) then begin 
      return,-1
    endif else begin 
         if (enco eq 0) then begin
          if (elem eq *(*rootAux).nodo) then begin
              elementoB = rootAux
              enco = 1
              return, elementoB
          endif else begin
              elementoB = _SkeletonStructFind_Element((*rootAux).hermanoD, elem, enco)
              if enco eq 0 then elementoB = _SkeletonStructFind_Element((*rootAux).hijo, elem, enco)
              return, elementoB
          endelse
        endif else begin
              return, elementoB
        endelse
    endelse
END
