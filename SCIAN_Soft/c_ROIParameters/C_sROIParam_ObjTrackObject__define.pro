;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjTrackObject
;
; PURPOSE:
;       - Tracking of Objects.
;
; AUTHOR:
;     Steffen Härtel (2009)
;     e_mail: shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjTrackObject')
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sROIParam_ObjTrackObject::getPriorParamStruct, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, whParam = whParam

   priorDataVect = 0
   priorNumberVect = 0
   priorHighestNumber = 0
   fOPriorROI2DParam = 0
   priorStatusCode = 0
   priorXYZ = 0
   priorMask = 0
   nPriorObj = 0
   pPriorValueStruct = 0
   oPriorROI2DGroup = 0
   
   if tPos ge 0 then oPriorROI2DGroup = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos)
   if obj_valid(oPriorROI2DGroup) then begin
      oPrior2DParamContainer = oPriorROI2DGroup->getParamContainer()
      nPriorObj = oPriorROI2DGroup->count()
   endif
   
   if obj_valid(oPrior2DParamContainer) then begin
      priorXYZ=(oPriorROI2DGroup->getGroupCenterXYZ())[1:*,*]
      priorMask=oPriorROI2DGroup->getGroupMaskInExactParamValOrder(selROIParamName = 'Track Objects from Masks')
      oPriorROI2DParam = oPriorROI2DGroup->getSelectedROIObjFromParamName(selROIParamName = 'Track Objects from Masks', position = pos)
   
      if obj_valid(oPriorROI2DParam) then begin
         fOPriorROI2DParam = 1b
         pPriorValueStruct = oPriorROI2DParam->getpValueStruct(position = whParam[0])
         priorDataVect = *(*pPriorValueStruct).pROIParamVect
         priorNumberVect = *(*(oPriorROI2DParam->getpParamStruct())).pROINumberVect
         priorHighestNumber = (*(*pPriorValueStruct).pValues)[(where( *(*pPriorValueStruct).pNames eq 'Highest Object Number'))[0]]
        
         pPriorValueStruct = oPriorROI2DParam->getpValueStruct(position = whParam[1])
         priorStatusCode = *(*pPriorValueStruct).pROIParamVect
      endif 
   endif
   
   return,{ok:1b,$
      priorDataVect:priorDataVect,$
      priorNumberVect:priorNumberVect,$
      priorHighestNumber:priorHighestNumber,$
      fOPriorROI2DParam:fOPriorROI2DParam,$
      priorStatusCode:priorStatusCode,$
      priorXYZ:priorXYZ,$
      priorMask:priorMask,$
      nPriorObj:nPriorObj,$
      pPriorValueStruct:pPriorValueStruct,$
      oPriorROI2DGroup:oPriorROI2DGroup}
end


pro C_sROIParam_ObjTrackObject::findROIs, roiGroup = C_sROI2DGroupObj, newObjIds = newObjIds, maxDist = maxDist, nTimes = nTimes, stack_tlb = stack_tlb, tPos = tPos,$
                                          chPos = chPos, zPos = zPos, clusPos = clusPos, whParam = whParam, statusCode = statusCode, dataVect = dataVect,$
                                          tProj = tProj

   if(tPos lt 2) then return
   normal = self->getStatus('normal')
   lost = self->getStatus('lost')
   s1 = nTimes*5 ;; Set this parameter
   nObj = n_elements(newObjIds)
   distMatrix = fltarr(nObj, s1, /nozero)
   objXYZ = fltarr(2, nObj, /nozero)
   currentRegionRois = ulonarr(nObj)
   tPositionsArr = make_array(nTimes, value=-1, /long)
   priorObjIds = lonarr(s1, /nozero)
   previousRegionRois = ulonarr(s1)
   pos = 0L

      ; get xyz-coordinates of object i
   for i = 0L, nObj - 1 do begin
      objXYZ[*,i] = (C_sROI2DGroupObj->get(position = newObjIds[i]))->getCenterXYZ()
      regionId = max(tProj[*((C_sROI2DGroupObj->get(position = newObjIds[i]))->getpWherePoints())])
      if(regionId gt 0) then currentRegionRois[i] = regionId
   end
   
   priorParamStructs = ptrArr(nTimes)
   
   tOk = -1
   for t1 = 0, nTimes-1 do begin
      priorParamStructs[t1] = ptr_new(self->getPriorParamStruct(stack_tlb = stack_tlb, tPos = tPos-t1-2, chPos = chPos, zPos = zPos, clusPos = clusPos, whParam = whParam), /NO_COPY)
      if not (*priorParamStructs[t1]).fOPriorROI2DParam then break
      lost_arr = where((*priorParamStructs[t1]).priorStatusCode eq lost, count)
      if count eq 0 then continue

      tPositionsArr[t1] = pos ; Start position of objects from this time
      ; Get lost object positions
      tmpArr = fltarr(nObj, count, /nozero) ; distances
      tmpReg = ulonarr(count) ; Temp for regions
      for j = 0L, count-1 do begin
         priorXYZ = ((*priorParamStructs[t1]).priorXYZ)[*,lost_arr[j]]
         regionId = max(tProj[*((((*priorParamStructs[t1]).oPriorROI2DGroup)->get(position = lost_arr[j]))->getpWherePoints())])
         if(regionId gt 0) then tmpReg[j] = regionId 
         tmpArr[*,j] = sqrt((objXYZ[0,*]-priorXYZ[0])^2 + (objXYZ[1,*]-priorXYZ[1])^2)
      endfor
      if(pos+count ge s1) then begin  ; grow as necessary
         n1 = s1 > (pos+count-s1)
         distMatrix = [[distMatrix], [fltarr(nObj, n1, /nozero)]]
         priorObjIds = [priorObjIds, lonarr(n1, /nozero)]
         previousRegionRois = [[previousRegionRois],ulonarr(n1)]
         s1+=n1
      endif
      distMatrix[nObj*pos] = reform(tmpArr, nObj*count)
      priorObjIds[pos] = lost_arr
      previousRegionRois[pos] = tmpReg
      pos+=count
      tOk = t1
   endfor
   
   print, 'tOk: ', tOk
   if(pos ne 0) then begin
      distMatrix = reform(distMatrix[0:nObj*pos-1], nObj, pos)
      tPositionsArr = tPositionsArr[0:tOk]
      print, 'tPosArr: ', tPositionsArr
   
      ; Sort distances (pos is the number of lost objects in all times)
      if(pos eq 1) then begin
         sortDist = make_array(2,nObj)
         sortDist[0,*] = sort(distMatrix)
      endif else sortDist = array_indices(distMatrix,sort(distMatrix))
   
      nReady = 0
      noDist = -1
      for i = 0L, nObj*pos - 1 do begin
         dist = distMatrix[sortDist[0,i],sortDist[1,i]]
         if(nReady ge (pos < nObj)) then break
         if(dist eq noDist) then continue
         if(dist gt maxDist*(nTimes+1)) then break
         ; MOR - 15 Feb 2011 - begin
;         print, 'looking into timeJump definition'
;         print, 'dist is ', dist
;         print, tPositionsArr 
;         print, sortDist[1,i]
;         print, (tPositionsArr le sortDist[1,i])
;         print, max(where(tPositionsArr le sortDist[1,i] and tPositionsArr ne -1))
;         print, max(where(tPositionsArr le sortDist[1,i] and tPositionsArr ne -1))+2
         ; MOR - end
         timeJump = max(where(tPositionsArr le sortDist[1,i] and tPositionsArr ne -1))+2
;         if(dist gt maxDist*(1+alog(timeJump)/alog(20))) then continue ;; Para quantum dots
         if(dist gt maxDist*(1+timeJump)) then continue ;; Para espermios
         if(previousRegionRois[sortDist[1,i]] ne currentRegionRois[sortDist[0,i]]) then continue ;;It has to belong to the same region
                  
         ; I have a matching
         (*priorParamStructs[timeJump-2]).priorStatusCode[priorObjIds[sortDist[1,i]]] = normal ; update prior object
         statusCode[newObjIds[sortDist[0,i]]] = normal ; update current object
         dataVect[newObjIds[sortDist[0,i]]] = (*priorParamStructs[timeJump-2]).priorDataVect[priorObjIds[sortDist[1,i]]] ; assign prior track number
         
         ; Rule out these objects
         distMatrix[*,sortDist[1,i]] = noDist
         distMatrix[sortDist[0,i],*] = noDist
         nReady++
      endfor
   endif

   for t2 = 0, t1<(nTimes-1) do begin
      if (*priorParamStructs[t2]).fOPriorROI2DParam then begin
         *(*((*priorParamStructs[t2]).pPriorValueStruct)).pRoiParamVect = (*priorParamStructs[t2]).priorStatusCode
         s_ISegM_SaveROI2DGroup, oROI2DGroup = (*priorParamStructs[t2]).oPriorROI2DGroup, stack_tlb = stack_tlb, tPos = tPos-t2-2, chPos = chPos, zPos = zPos, clusPos = clusPos
      endif
      ptr_free, priorParamStructs[t2]
   endfor
   ptr_free, priorParamStructs
end

pro C_sROIParam_ObjTrackObject::apply, stack_tlb = stack_tlb, C_sROI2DGroupObj = C_sROI2DGroupObj, position = position
   whParam = [(where(*(*self.pParamStruct).pNames eq 'Track Objects from Masks'))[0],$
              (where(*(*self.pParamStruct).pNames eq 'Track Status Code'))[0],$
              (where(*(*self.pParamStruct).pNames eq 'Track Objects from Masks II'))[0] ]

      ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1: if (position[0] eq -1) then return else whParamActive[position] = 1
      else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase
   wherePA = where(whParamActive eq 1)

      ; check Pointers
   if not(ptr_valid((*self.pParamStruct).pROINumberVect)) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

   nObj = C_sROI2DGroupObj->count()
   if (nObj lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

      if whParamActive[0] then begin

            ; set ROI Number Vector with Object Number Vector
         *(*self.pParamStruct).pROINumberVect = C_sROI2DGroupObj->getObjectNumberVector()

            ; try to get prior ObjTrackObject at t-1 and its parameters
         s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, totalTNum = totalTNum, segPos = segPos
         priorTPos = tPos - 1
         
         priorParamStruct = self->getPriorParamStruct(stack_tlb = stack_tlb, tPos = priorTPos, chPos = chPos, zPos = zPos, clusPos = clusPos, whParam = whParam)
         priorHighestNumber = priorParamStruct.priorHighestNumber 
         
         normal = self->getStatus('normal')
         appearance = self->getStatus('appearance')
         lost = self->getStatus('lost')
         fusion = self->getStatus('fusion')
         fission = self->getStatus('fission')
         statusCode = make_array(nObj, /integer, value = appearance)

         if priorParamStruct.fOPriorROI2DParam then begin

              ; if priorDataVect does not contain objects
            if (priorParamStruct.priorDataVect[0] eq -1) then begin
               *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = make_array(nObj, /index, /long) + (1 + priorHighestNumber)
               (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = nObj + priorHighestNumber
            endif else begin
            
               ; Parameters
               max_dist = 15 ; maximum object movement ControlSCA=9  CEDAI=7 o 6  Ratón=26     CelulasRedondas=10, en raton he observado espermatozoides avanzan 25 pixeles de un frame al siguiente, Susana
               nTimes = 5 ; time jumps allowed        ControlSCA=5   CEDAI=4      Ratón=6
               region = 1 ; 1 for searching in regions, 0 otherwise ; @todo: Get this parameter from user  default==0
               tProjClusPos = 7 ;Cluster with t-projection for tracking  ControlSCA=9  CEDAI=7  Ratón=7   Fragmentacin=0   CelulasRedondas=14 
                  ; set new Data Vector
               dataVect = make_array(nObj, /long)

                  ; get min/max of prior Data Vector
               minPriorDataVect = min(priorParamStruct.priorDataVect, max = maxPriorDataVect)

                  ; get prior mask with ROIS that are filled with track-numbers
               priorMask = priorParamStruct.priorMask

                  ; sort prior data vector between minPriorDataVect and maxPriorDataVect
               sortPDVect = sort(priorParamStruct.priorDataVect)
               sortIndPDVect = priorParamStruct.priorDataVect[sortPDVect] - minPriorDataVect

               priorObjCount = [-1]
               candidates = ptrArr(nObj)
               closenessMatrix = make_array(nObj, priorParamStruct.nPriorObj, /byte)
               n_ready = 0L
             
               ;;; Regions

               previousRegionRois = ulonarr(priorParamStruct.nPriorObj)
               currentRegionRois = ulonarr(nObj)
               if(region) then begin
                  widget_control, stack_tlb, get_uValue = stackState, /no_copy
                     imageStackInfoObject = *stackState.pImageStackInfoObject
                  widget_control, stack_tlb, set_uValue = stackState, /no_copy
   
                  ;tProj = imageStackInfoObject->getSelectedClusterMask(tPos = 0, chPos = chPos, zPos = zPos, clusPos = 1)
                  tProj = imageStackInfoObject->getSelectedClusterMask(tPos = 0, chPos = chPos, zPos = zPos, clusPos = tProjClusPos) ;Clus7=Todos(Moviles e inmoviles) Clus9=SoloMoviles
                     ; Get labeled regions
                  xyDim = (size(priorMask))[1:2]
                  labelObj = bytArr(xyDim+2)
                  labelObj[1:xyDim[0],1:xyDim[1]] = tProj
                  tProj = (label_region(labelObj, /ulong))[1:xyDim[0],1:xyDim[1]]
                  labelObj = 0
                  tMaskCount = max(tProj)
                  tMaskNumbers = make_array(tMaskCount, /int, /index) + 1

                  ;; Find region for each object
                   
                  for i = 0, priorParamStruct.nPriorObj-1 do begin
                     regionId = max(tProj[*((priorParamStruct.oPriorROI2DGroup->get(position = i))->getpWherePoints())])
                     if(regionId gt 0) then previousRegionRois[i] = regionId
                  endfor
                  for i = 0, nObj-1 do begin
                     regionId = max(tProj[*((C_sROI2DGroupObj->get(position = i))->getpWherePoints())])
                     if(regionId gt 0) then currentRegionRois[i] = regionId
                  endfor
               endif
               
               ;;; Overlapping criteria

               self->applyOverlapping, nObj = nObj, nPriorObj = priorParamStruct.nPriorObj, priorMask = priorMask, C_sROI2DGroupObj = C_sROI2DGroupObj,$
                                       minPriorDataVect = minPriorDataVect, maxPriorDataVect = maxPriorDataVect, sortIndPDVect = sortIndPDVect,$
                                       closenessMatrix = closenessMatrix, n_ready = n_ready, statusCode = statusCode, priorObjCount = priorObjCount,$
                                       dataVect = dataVect, priorDataVect = priorParamStruct.priorDataVect, sortPDVect = sortPDVect,$
                                       previousRegionRois = previousRegionRois, currentRegionRois = currentRegionRois

               ;;; Distance criteria
               
               self->applyDistance, nObj = nObj, nPriorObj = priorParamStruct.nPriorObj, priorMask = priorMask, C_sROI2DGroupObj = C_sROI2DGroupObj, max_dist = max_dist,$
                                    priorXYZ = priorParamStruct.priorXYZ, closenessMatrix = closenessMatrix, n_ready = n_ready, statusCode = statusCode,$
                                    priorObjCount = priorObjCount, dataVect = dataVect, priorDataVect = priorParamStruct.priorDataVect, sortPDVect = sortPDVect,$
                                    previousRegionRois = previousRegionRois, currentRegionRois = currentRegionRois

               
               ;;; Backward matching

               wh = where(statusCode ne normal, count)
               
                  ; try to find missing objects in prior times
               ;; @todo: Consider regions in the search
               ;;;;;; Commented for raton motil
               if count ne 0 then begin
                  self->findROIs, roiGroup = C_sROI2DGroupObj, newObjIds = wh, maxDist = max_dist, nTimes = nTimes, stack_tlb = stack_tlb, tPos = tPos,$
                                  chPos = chPos, zPos = zPos, clusPos = clusPos, whParam = whParam, statusCode = statusCode, dataVect = dataVect,$
                                  tProj = tProj
               endif

               ;;; Event classification

                  ; Try to explain new objects
               wh = where(statusCode ne normal, count)
               if count ne 0 then begin                  
                     ; fill missing objects in new data with new numbers
                  new_obj = make_array(count, /index, /long) + (priorParamStruct.priorHighestNumber + 1)
                  for i = 0, count-1 do begin
                     wh2 = where(closenessMatrix[wh[i],*], count2)
                     if(count2 gt 1) then print, 'number of potential candidates :', count2 ; MOR
                     if(count2 ne 0) then begin ; @todo: consider the best possible fission
                        if(previousRegionRois[sortPDVect[wh2[0]]] eq currentRegionRois[wh[i]]) then begin
                        print, 'fission'
                           statusCode[wh[i]] = fission
   ;                        candidates[wh[i]] = ptr_new([new_obj[i],priorParamStruct.priorDataVect[sortPDVect[wh2[0]]]], /no_copy) ; utilizando nro de track
                           candidates[wh[i]] = ptr_new([wh[i],sortPDVect[wh2[0]]], /no_copy) ; utilizando nro interno
                        endif
                     endif
                     dataVect[wh[i]] = new_obj[i]
                  endfor
               endif
               
                  ; try to explain lost objects
               wh = where((histogram(priorObjCount, min = minPriorDataVect, max = maxPriorDataVect))[sortIndPDVect] eq 0, count)
               if(count ne 0) then begin
                  j = 0
                  fusioned_objs = make_array(count, /LONG) ; keep fusioned objects
                  for i = 0, count-1 do begin
                     wh2 = where(closenessMatrix[*,wh[i]], count2)
                     if count2 gt 1 then  print, 'number of potential candidates gt 1 :', count2 ; MOR
                     if(count2 ne 0) then begin; @todo: consider the best possible fusion
                        if(previousRegionRois[sortPDVect[wh[i]]] eq currentRegionRois[wh2[0]]) then begin
                        ; MOR
                        print, 'fusion'
                           statusCode[wh2[0]] = fusion
                           fusioned_objs[j++] = wh2[0]
                           if not ptr_valid(candidates[wh2[0]]) then begin
   ;                           candidates[wh2[0]] = ptr_new([dataVect[wh2[0]]]) ; utilizando nro de track
                              candidates[wh2[0]] = ptr_new([wh2[0]]) ; utilizando nro interno
                           endif
                           ;*candidates[wh2[0]] = [*candidates[wh2[0]], priorParamStruct.priorDataVect[sortPDVect[wh[i]]]] ; utilizando nro de track
                           *candidates[wh2[0]] = [*candidates[wh2[0]], sortPDVect[wh[i]]] ; utilizando nro interno
                        endif
                     endif
                        ; label lost objects at t-1.
                     priorParamStruct.priorStatusCode[sortPDVect[wh[i]]] = lost
                  endfor
                  
                     ;;; Latest change
                     ; Fix assignation when there are spots
                  for i = 0, j-1 do begin
                     prev_id = where(priorParamStruct.priorDataVect eq dataVect[fusioned_objs[i]])
                     if(priorParamStruct.priorStatusCode[prev_id] eq fission or priorParamStruct.priorStatusCode[prev_id] eq appearance) then begin
                     print, 'all candidates ', (*candidates[fusioned_objs[i]])
                      print, 'candidate being used ', (*candidates[fusioned_objs[i]])[1]
                        prev_close_obj_id = (*candidates[fusioned_objs[i]])[1] ; @todo: I'm taking the firt one, it should take the closest one
                        if(priorParamStruct.priorStatusCode[prev_close_obj_id] eq lost) then begin
                           print, "Cambio", dataVect[fusioned_objs[i]], " por", priorParamStruct.priorDataVect[prev_close_obj_id]
                           priorParamStruct.priorStatusCode[prev_close_obj_id] = normal
                           dataVect[fusioned_objs[i]] = priorParamStruct.priorDataVect[prev_close_obj_id]
                           priorParamStruct.priorStatusCode[prev_id] = lost
                        endif
                     endif
                  endfor
                  ; save information
                  if priorParamStruct.fOPriorROI2DParam then begin
                     *(*(priorParamStruct.pPriorValueStruct)).pRoiParamVect = priorParamStruct.priorStatusCode
                     s_ISegM_SaveROI2DGroup, oROI2DGroup = priorParamStruct.oPriorROI2DGroup, stack_tlb = stack_tlb, tPos = priorTPos, chPos = chPos, zPos = zPos, clusPos = clusPos
                  endif
               endif
               
               if ptr_valid((*self.pParamStruct).pROICandidateArray) then begin
                   for i = 0, n_elements(*(*self.pParamStruct).pROICandidateArray)-1 do begin
                     if ptr_valid((*(*self.pParamStruct).pROICandidateArray)[i]) then ptr_free, (*(*self.pParamStruct).pROICandidateArray)[i] 
                  endfor
                  ptr_free, (*self.pParamStruct).pROICandidateArray
               endif
               (*self.pParamStruct).pROICandidateArray = ptr_new(candidates)
             
               *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = dataVect; Susana - ¿aqui se guarda numero de TO?
               (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where ( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = priorParamStruct.priorHighestNumber > max(dataVect)

            endelse
         endif else begin
            *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = make_array(nObj, /index, /long) + 1
            (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = nObj
         endelse

         if whParamActive[1] then *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = statusCode

      endif
   endelse
end

pro C_sROIParam_ObjTrackObject::applyOverlapping, nObj = nObj, nPriorObj = nPriorObj, priorMask = priorMask, C_sROI2DGroupObj = C_sROI2DGroupObj,$
                                                       minPriorDataVect = minPriorDataVect, maxPriorDataVect = maxPriorDataVect, sortIndPDVect = sortIndPDVect,$
                                                       closenessMatrix = closenessMatrix, n_ready = n_ready, statusCode = statusCode, priorObjCount = priorObjCount,$
                                                       dataVect = dataVect, priorDataVect = priorDataVect, sortPDVect = sortPDVect,$
                                                       previousRegionRois = previousRegionRois, currentRegionRois = currentRegionRois
   normal = self->getStatus('normal')

      ; overlapMatrix counts how many pixeles of the prior ROIs overlap with the new ROIs
   overlapMatrix = make_array(nObj, nPriorObj, /long)

   ; fill overlapMatrix with the number of overlapping pixels from prior ROIs
   for i = 0, nObj-1 do begin
     ; print, uniq(priorMask[*((C_sROI2DGroupObj->get(position = i))->getpWherePoints())]) ; MOR
    overlapMatrix[i,*] = (histogram(priorMask[*((C_sROI2DGroupObj->get(position = i))->getpWherePoints())], min = minPriorDataVect, max = maxPriorDataVect))[sortIndPDVect]
    endfor
   
   if ptr_valid((*self.pParamStruct).pROIOverlapMatrix) then ptr_free, (*self.pParamStruct).pROIOverlapMatrix
   (*self.pParamStruct).pROIOverlapMatrix = ptr_new(overlapMatrix) ; create a copy of the overlap matrix

   wh = where(overlapMatrix, count)
   if(count gt 0) then closenessMatrix[wh] = 1b
   if(nPriorObj eq 1) then begin
      sortOverlap = make_array(2,nObj)
      sortOverlap[0,*] = sort(overlapMatrix)
   endif else sortOverlap = array_indices(overlapMatrix, sort(overlapMatrix))
   for i = nObj*nPriorObj - 1, 0, -1 do begin
      if (n_ready ge (nPriorObj < nObj)) then break
      overlap = overlapMatrix[sortOverlap[0,i],sortOverlap[1,i]]
      if(overlap eq 0) then break
      if(overlap eq -1) then continue
      if (previousRegionRois[sortPDVect[sortOverlap[1,i]]] ne currentRegionRois[sortOverlap[0,i]]) then begin
         continue
      endif
      wh = where(overlapMatrix[*,sortOverlap[1,i]] gt 0,count)
      overlapMatrix[wh,sortOverlap[1,i]] = -1
      overlapMatrix[sortOverlap[0,i],sortOverlap[1,i]] = overlap
      wh = where(overlapMatrix[sortOverlap[0,i],*] gt 0,count)
      if(count ne 1) then overlapMatrix[sortOverlap[0,i],wh] = -1
      statusCode[sortOverlap[0,i]] = normal
      priorObjCount = [priorObjCount, priorDataVect[sortPDVect[sortOverlap[1,i]]]]
      dataVect[sortOverlap[0,i]] = priorDataVect[sortPDVect[sortOverlap[1,i]]]
      n_ready++
   endfor
   if (n_elements(priorObjCount) gt 1) then priorObjCount = priorObjCount[1:*]
end

pro C_sROIParam_ObjTrackObject::applyDistance, nObj = nObj, nPriorObj = nPriorObj, priorMask = priorMask, C_sROI2DGroupObj = C_sROI2DGroupObj, max_dist = max_dist,$
                                                    priorXYZ = priorXYZ, closenessMatrix = closenessMatrix, n_ready = n_ready, statusCode = statusCode,$
                                                    priorObjCount = priorObjCount, dataVect = dataVect, priorDataVect = priorDataVect, sortPDVect = sortPDVect,$
                                                    previousRegionRois = previousRegionRois, currentRegionRois = currentRegionRois
   normal = self->getStatus('normal')
   inf_dist = total((size(priorMask))[1:2]) ; infinity distance
   distMatrix = make_array(nObj, nPriorObj, /float)
      ; priorXYZ: xyz-coordinates of all prior objects
   for i = 0L, nObj - 1 do begin
         ; get xyz-coordinates of object i
      objXYZ = (C_sROI2DGroupObj->get(position = i))->getCenterXYZ()
         ; calculated distance-vector of ith object with all other objects
      distMatrix[i,*] = (sqrt((objXYZ[0]-priorXYZ[0,*])^2 + (objXYZ[1]-priorXYZ[1,*])^2))[sortPDVect]
   endfor
   (*self. pParamStruct).pROIDistanceMatrix = ptr_new(distMatrix) ; create a copy of the distance matrix
   closenessMatrix += (distMatrix lt max_dist)
   wh = where(dataVect,count)
   if(count gt 0) then distMatrix[wh,*] = inf_dist ; Exclude assigned prior objects
   wh = where(distMatrix gt max_dist, count)
   if(count gt 0) then distMatrix[wh] = inf_dist
  
      ; sort distance vector
   if(nPriorObj eq 1) then begin
      sortDist = make_array(2,nObj)
      sortDist[0,*] = sort(distMatrix)
   endif else sortDist = array_indices(distMatrix,sort(distMatrix))

   for i = 0L, nObj*nPriorObj - 1 do begin
      dist = distMatrix[sortDist[0,i],sortDist[1,i]]
      if(n_ready ge (nPriorObj < nObj)) then break
      if(dist eq inf_dist or total(priorDataVect[sortPDVect[sortDist[1,i]]] eq priorObjCount) ne 0) then continue
      if (previousRegionRois[sortPDVect[sortDist[1,i]]] ne currentRegionRois[sortDist[0,i]]) then begin
         continue
      endif
      wh = where(distMatrix[*,sortDist[1,i]] lt inf_dist,count)
      distMatrix[wh,sortDist[1,i]] = inf_dist
      wh = where(distMatrix[sortDist[0,i],*] lt inf_dist,count)
      if(count ne 0) then distMatrix[sortDist[0,i],wh] = inf_dist

      statusCode[sortDist[0,i]] = normal
      priorObjCount = [priorObjCount, priorDataVect[sortPDVect[sortDist[1,i]]]]
      dataVect[sortDist[0,i]] = priorDataVect[sortPDVect[sortDist[1,i]]]
      n_ready++
   endfor
end

function C_sROIParam_ObjTrackObject::getStatus, status
   result = 0
   case status of
      'normal': result = 0
      'appearance': result = 1
      'lost': result = 2
      'fusion': result = 3
      'fission': result = 4
   endcase
   return,result
end

function C_sROIParam_ObjTrackObject::init

   ROIParamStruct = {name:'Object Track Objects',$   ;  ROI Name.
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROINumberVect:ptr_new(),$     ; Pointer on ROI-Obj Number Vector
                     pROIOverlapMatrix:ptr_new(),$    ; Pointer on ROI-Obj Overlap Matrix.
                     pROIDistanceMatrix:ptr_new(),$     ; Pointer on ROI-Obj Distance Matrix.
                     pROICandidateArray:ptr_new() }  ; Pointer on ROI-Obj Candidates Matrix.
                                                     ; each row (track#, 1,2,3, ...,N) contains a list of candidates
                    
   self.pValueStruct = ptr_new(ptrArr(3))
   ROIParamWidgetType = make_array(3, /string, value = 'widget_slider')
   ROIParamNames = [ 'Track Objects from Masks',$
                     'Track Status Code',$
                     'Track Objects from Masks II' ]
   ROIParamActive = [1,1,0]
   ROIParamMin = [0,0,0]
   ROIParamMax = [1,1,1]
   ROIParamValues = [0,0,0]
   pROINumberVect = [-1]

   ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
   ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
   ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
   ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
   ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
   ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
   ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

   self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

   ROIValueStruct = {name:'Track Objects from Masks',$
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(9, /string, value = 'widget_slider')
   ROIValueNames = [ 'Highest Object Number',$
                     'Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
   ROIValueActive = [1,0,0,0,0,0,0,0,0]
   ROIValueMin = [1,0.,0.,0.,0.,0.,0.,0.,0.]
   ROIValueMax = [10000,1.,1.,1.,1.,1.,1.,1.,1.]
   ROIValueValues = [0,0.,1.,0.,1.,0.,1.,0.,1.]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
   (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)
   
   ROIValueStruct = {name:'Track Status Code',$
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = [ 'Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
   ROIValueActive = [0,0,0,0,0,0,0,0]
   ROIValueMin = [0.,0.,0.,0.,0.,0.,0.,0.]
   ROIValueMax = [1.,1.,1.,1.,1.,1.,1.,1.]
   ROIValueValues = [0.,1.,0.,1.,0.,1.,0.,1.]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
   (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:'Track Objects from Masks II',$
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect: ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = ['widget_slider','widget_slider', ROIValueWidgetType]
   ROIValueNames = ['x-Size per Pixel','y-Size per Pixel', ROIValueNames]
   ROIValueActive = [1, 1, ROIValueActive]
   ROIValueMin = [0., 0., ROIValueMin]
   ROIValueMax = [1000., 1000., ROIValueMax]
   ROIValueValues = [1., 1., ROIValueValues]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
   ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
   ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
   ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
   ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
   ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

   (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)
   return, 1
end

pro C_sROIParam_ObjTrackObject__define
   tmp = {C_sROIParam_ObjTrackObject, pParamStruct: ptr_new(), pValueStruct: ptr_new(), inherits C_sROIParam}
end