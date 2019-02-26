;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DTrackObject
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DTrackObject' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sROIParam_3DTrackObject::getPriorParamStruct, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, whParam = whParam

           priorDataVect = 0
           priorNumberVect = 0
           priorHighestNumber = 0 
           fOPriorROI3DParam = 0
           priorStatusCode = 0 
           priorXYZ = 0 
           priorMask = 0 
           nPriorObj = 0
           pPriorValueStruct = 0
           oPriorROI3DGroup = 0
            
            if tPos ge 0 then oPriorROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos,clusPos = clusPos)
            if obj_valid(oPriorROI3DGroup) then begin
               oPrior3DParamContainer = oPriorROI3DGroup->getParamContainer()
               nPriorObj = oPriorROI3DGroup->count()
            endif
            
            if obj_valid(oPrior3DParamContainer) then begin
              priorXYZ=(oPriorROI3DGroup->getGroupCenterXYZ())[1:*,*]
              priorMask=oPriorROI3DGroup->getGroupMaskInExactParamValOrder(selROIParamName = 'Track Objects from Masks')
              oPriorROI3DParam = oPriorROI3DGroup->getSelectedROIObjFromParamName(selROIParamName = 'Track Objects from Masks', position = pos)
            
              if obj_valid(oPriorROI3DParam) then begin
                 fOPriorROI3DParam = 1b
                 pPriorValueStruct = oPriorROI3DParam->getpValueStruct(position = whParam[0])
                 priorDataVect = *(*pPriorValueStruct).pROIParamVect
                 priorNumberVect = *(*(oPriorROI3DParam->getpParamStruct())).pROINumberVect
                 priorHighestNumber = (*(*pPriorValueStruct).pValues)[(where( *(*pPriorValueStruct).pNames eq 'Highest Object Number'))[0]]
                 
                 pPriorValueStruct = oPriorROI3DParam->getpValueStruct(position = whParam[1])
                 priorStatusCode = *(*pPriorValueStruct).pROIParamVect
              endif 
            endif
            
           return,{ok:1b,$
           priorDataVect:priorDataVect,$
           priorNumberVect:priorNumberVect,$
           priorHighestNumber:priorHighestNumber,$ 
           fOPriorROI3DParam:fOPriorROI3DParam,$
           priorStatusCode:priorStatusCode,$
           priorXYZ:priorXYZ,$
           priorMask:priorMask,$
           nPriorObj:nPriorObj,$
           pPriorValueStruct:pPriorValueStruct,$
           oPriorROI3DGroup:oPriorROI3DGroup}
end


pro C_sROIParam_3DTrackObject::findROIs, roiGroup = C_sROI3DGroupObj, newObjIds = newObjIds, maxDist = maxDist, nTimes = nTimes, stack_tlb = stack_tlb, tPos = tPos,$
                                          chPos = chPos, zPos = zPos, clusPos = clusPos, whParam = whParam, statusCode = statusCode, dataVect = dataVect

   if(tPos lt 2) then return
   lost = 2
   normal = 0   
   s1 = nTimes*5 ;; Set this parameter 
   nObj = n_elements(newObjIds)
   distMatrix = fltarr(nObj, s1, /NOZERO)
   objXYZ = fltarr(3, nObj, /NOZERO)
   tPositionsArr = make_array(nTimes, VALUE=-1, /INTEGER)
   priorObjIds = lonarr(s1, /NOZERO)
   pos = 0

      ; get xyz-coordinates of object i
   for i = 0, nObj - 1 do objXYZ[*,i] = (C_sROI3DGroupObj->get(position = newObjIds[i]))->getCenterXYZ()
   
   priorParamStructs = ptrArr(nTimes)

   tOk = -1
   for t1 = 0, nTimes-1 do begin
      priorParamStructs[t1] = ptr_new(self->getPriorParamStruct(stack_tlb = stack_tlb, tPos = tPos-t1-2, chPos = chPos, zPos = zPos, clusPos = clusPos, whParam = whParam), /NO_COPY)
      if not (*priorParamStructs[t1]).fOPriorROI3DParam then break
      lost_arr = where((*priorParamStructs[t1]).priorStatusCode eq lost, count)
      if count eq 0 then continue

      tPositionsArr[t1] = pos ; Start position of objects from this time
      ; Get lost object positions
      tmpArr = fltarr(nObj, count, /NOZERO) ; distances
      for j = 0, count-1 do begin
         priorXYZ = ((*priorParamStructs[t1]).priorXYZ)[*,lost_arr[j]]
         tmpArr[*,j] = sqrt((objXYZ[0,*]-priorXYZ[0])^2 + (objXYZ[1,*]-priorXYZ[1])^2 + (objXYZ[2,*]-priorXYZ[2])^2 )
      endfor
      if pos+count ge s1 then begin  ; grow as necessary
         distMatrix = [[distMatrix], [fltarr(nObj, s1, /NOZERO)]]
         priorObjIds = [priorObjIds, lonarr(s1, /NOZERO)]
         s1*=2
      endif
      distMatrix[nObj*pos] = reform(tmpArr, nObj*count)
      priorObjIds[pos] = lost_arr
      pos+=count
      tOk = t1
   endfor
   
   if(pos ne 0) then begin
      distMatrix = reform(distMatrix[0:nObj*pos-1], nObj, pos)
      tPositionsArr = tPositionsArr[0:tOk]
   
      ; Sort distances (pos is the number of lost objects in all times)
      if(pos eq 1) then begin
         sortDist = make_array(2,nObj)
         sortDist[0,*] = sort(distMatrix)
      endif else sortDist = array_indices(distMatrix,sort(distMatrix))
   
      nReady = 0
      noDist = -1
      for i = 0, nObj*pos - 1 do begin
         dist = distMatrix[sortDist[0,i],sortDist[1,i]]
         if(nReady ge (pos < nObj)) then break
         if(dist eq noDist) then continue
         if(dist gt maxDist*(nTimes+1)) then break
         timeJump = max(where(tPositionsArr le sortDist[1,i] and tPositionsArr ne -1))+2
         if(dist gt maxDist*(1+alog(timeJump))) then continue
         ;if(dist gt maxDist*timeJump) then continue
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
      if (*priorParamStructs[t2]).fOPriorROI3DParam then begin
         *(*((*priorParamStructs[t2]).pPriorValueStruct)).pRoiParamVect = (*priorParamStructs[t2]).priorStatusCode
         s_ISegM_SaveROI3DGroup, oROI3DGroup = (*priorParamStructs[t2]).oPriorROI3DGroup, stack_tlb = stack_tlb, tPos = tPos-t2-2, chPos = chPos, zPos = zPos, clusPos = clusPos
      endif
      ptr_free, priorParamStructs[t2]
   endfor
   ptr_free, priorParamStructs
end
pro C_sROIParam_3DTrackObject::apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position
   whParam = [(where(*(*self.pParamStruct).pNames eq '3D Track Objects from Masks'))[0],$
                 (where(*(*self.pParamStruct).pNames eq '3D Track Status Code'))[0],$
                 (where(*(*self.pParamStruct).pNames eq '3D Track Objects from Masks II'))[0] ]

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

   nObjects = C_sROI3DGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

      if whParamActive[0] then begin

            ; set ROI Number Vector with Object Number Vector
         *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()

            ; try to get prior ObjTrackObject at t-1 and its parameters
         s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
         priorTimePosition = tPos - 1
         if (priorTimePosition ge 0) then begin
            oPriorROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = priorTimePosition, chPos = chPos, clusPos = clusPos)
            if obj_valid(oPriorROI3DGroup) then begin
               oPrior3DParamContainer = oPriorROI3DGroup->getParamContainer()
               nPriorObjects = oPriorROI3DGroup->count()
            endif
            if obj_valid(oPrior3DParamContainer) then oPriorROI3DParam = oPriorROI3DGroup->getSelectedROIObjFromParamName(selROIParamName = '3D Track Objects from Masks', position = pos)
            if obj_valid(oPriorROI3DParam) then begin
               pPriorValueStruct = oPriorROI3DParam->getpValueStruct(position = whParam[0])
               priorDataVect = *(*pPriorValueStruct).pROIParamVect
               priorNumberVect = *(*(oPriorROI3DParam->getpParamStruct())).pROINumberVect
               priorHighestNumber = (*(*pPriorValueStruct).pValues)[(where( *(*pPriorValueStruct).pNames eq 'Highest Object Number'))[0]]
            endif
         endif else priorHighestNumber = 0

         NORMAL = 0
         APPEARANCE = 1
         DISAPPEARANCE = 2
         FUSION = 3
         FISSION = 4
         statusCode = make_array(nObjects, /INTEGER, VALUE=APPEARANCE)

         if obj_valid(oPriorROI3DParam) then begin

              ; if priorDataVect does not contain objects
            if (priorDataVect[0] eq -1) then begin
               *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = make_array(nObjects, /index, /long) + (1 + priorHighestNumber)
               (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = nObjects + priorHighestNumber
            endif else begin

                  ; set new Data Vector
               dataVect = make_array(nObjects, /long)

                  ; get min/max of prior Data Vector
               minPriorDataVect = min(priorDataVect, max = maxPriorDataVect)

                  ; get prior mask with ROIS that are filled with track-numbers
               priorMask = oPriorROI3DGroup->getGroupMaskInExactParamValOrder(selROIParamName = '3D Track Objects from Masks')

;               tvscl, (C_sROI3DGroupObj->getGroupMask() < 1) * priorMask

                  ; overlapMatrix counts how many pixles of the prior ROIs overlap with the new ROIs
               overlapMatrix = make_array(nObjects, nPriorObjects, /long)

                  ; where histogram of prior data vector ne 0
               whereHistPDVect = where(histogram(priorDataVect))

                  ; sort prior data vector between minPriorDataVect and maxPriorDataVect
               sortPDVect = sort(priorDataVect)
               sortIndPDVect = priorDataVect[sortPDVect] - minPriorDataVect

                  ; fill overlapMatrix with the number of overlapping pixels from prior ROIs
               for i = 0, nObjects-1 do overlapMatrix[i,*] = (histogram(priorMask[*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())], min = minPriorDataVect, max = maxPriorDataVect))[sortIndPDVect]

               (*self.pParamStruct).pROIOverlapMatrix = ptr_new(overlapMatrix) ; create a copy of the overlap matrix
               
               priorObjCount = [-1]
               candidates = ptrArr(nObjects)

               closenessMatrix = make_array(nObjects, nPriorObjects, /BYTE)
               wh = where(overlapMatrix, count)
               if(count gt 0) then closenessMatrix[wh] = 1b
               if(nPriorObjects eq 1) then begin
                  sortOverlap = make_array(2,nObjects)
                  sortOverlap[0,*] = sort(overlapMatrix)
               endif else sortOverlap = ARRAY_INDICES(overlapMatrix,sort(overlapMatrix))
               n_ready = 0
               for i = nObjects*nPriorObjects - 1, 0, -1 do begin
                  if n_ready ge (nPriorObjects < nObjects) then break
                  overlap = overlapMatrix[sortOverlap[0,i],sortOverlap[1,i]]
                  if(overlap eq 0) then break
                  if(overlap eq -1) then continue
                  wh = where(overlapMatrix[*,sortOverlap[1,i]] gt 0,count)
                  overlapMatrix[wh,sortOverlap[1,i]] = -1
                  overlapMatrix[sortOverlap[0,i],sortOverlap[1,i]] = overlap
                  wh = where(overlapMatrix[sortOverlap[0,i],*] gt 0,count)
                  if(count ne 1) then overlapMatrix[sortOverlap[0,i],wh] = -1
                  statusCode[sortOverlap[0,i]] = NORMAL
                  overlapMatrix[sortOverlap[0,i],sortOverlap[1,i]] = priorDataVect[sortPDVect[sortOverlap[1,i]]]
                  priorObjCount = [priorObjCount, priorDataVect[sortPDVect[sortOverlap[1,i]]]]
                  dataVect[sortOverlap[0,i]] = priorDataVect[sortPDVect[sortOverlap[1,i]]]
                  n_ready++
               endfor
               if (n_elements(priorObjCount) gt 1) then priorObjCount = priorObjCount[1:*]
               inf_dist = total((size(priorMask))[1:2]) ; infinity distance
               max_dist = 22 ; maximum object movement
               distMatrix = make_array(nObjects, nPriorObjects, /float)
                  ; get xyz-coordinates of all prior objects
               ;priorXYZ = (oPriorROI3DGroup->getGroupCenterXYZ())
			   priorXYZ = (oPriorROI3DGroup->getGroupCenterXYZ())[1:*,*]
               for i = 0, nObjects - 1 do begin
                     ; get xyz-coordinates of object i
                  objXYZ = (C_sROI3DGroupObj->get(position = i))->getCenterXYZ()
                     ; calculated distance-vector of ith object with all other objects
                  distMatrix[i,*] = (sqrt((objXYZ[0]-priorXYZ[0,*])^2 + (objXYZ[1]-priorXYZ[1,*])^2 + (objXYZ[2]-priorXYZ[2,*])^2))[sortPDVect]
               endfor
               (*self.pParamStruct).pROIDistanceMatrix = ptr_new(distMatrix) ; create a copy of the distance matrix
               closenessMatrix+= (distMatrix lt max_dist)
               wh = where(dataVect,count)
               if(count gt 0) then distMatrix[wh,*] = inf_dist ; Exclude assigned prior objects
               wh = where(distMatrix gt max_dist, count)
               if(count gt 0) then distMatrix[wh] = inf_dist

                  ; sort distance vector
               if(nPriorObjects eq 1) then begin
                  sortDist = make_array(2,nObjects)
                  sortDist[0,*] = sort(distMatrix)
               endif else sortDist = ARRAY_INDICES(distMatrix,sort(distMatrix))                  
               for i = 0L, nObjects*nPriorObjects - 1 do begin
                  if(n_ready ge (nPriorObjects < nObjects)) then break
                  dist = distMatrix[sortDist[0,i],sortDist[1,i]]
                  if(dist eq inf_dist or total(priorDataVect[sortPDVect[sortDist[1,i]]] eq priorObjCount) ne 0) then continue
                  wh = where(distMatrix[*,sortDist[1,i]] lt inf_dist,count)
                  distMatrix[wh,sortDist[1,i]] = inf_dist
                  wh = where(distMatrix[sortDist[0,i],*] lt inf_dist,count)
                  if(count ne 0) then distMatrix[sortDist[0,i],wh] = inf_dist

                  statusCode[sortDist[0,i]] = NORMAL
                  overlapMatrix[sortDist[0,i],sortDist[1,i]] = priorDataVect[sortPDVect[sortDist[1,i]]]
                  priorObjCount = [priorObjCount, priorDataVect[sortPDVect[sortDist[1,i]]]]
                  dataVect[sortDist[0,i]] = priorDataVect[sortPDVect[sortDist[1,i]]]
                  n_ready++
               endfor

                  ; Try to explain new objects
               wh = where(statusCode ne NORMAL, count)
               
               if count ne 0 then begin
                     ; fill missing objects in new data with new numbers
                  new_obj = make_array(count, /index, /long) + (priorHighestNumber + 1)
                  for i = 0L, count-1 do begin
                     wh2 = where(closenessMatrix[wh[i],*], count2)
                     if(count2 ne 0) then begin ; @todo: consider the best possible fission
                        statusCode[wh[i]] = FISSION
                        candidates[wh[i]] = ptr_new([new_obj[i],priorDataVect[sortPDVect[wh2[0]]]], /no_copy)
                     endif
                     dataVect[wh[i]] = new_obj[i]
                  endfor
               endif
                  ; Try to explain lost objects
               wh = where((histogram(priorObjCount, min = minPriorDataVect, max = maxPriorDataVect))[sortIndPDVect] eq 0, count)
               if(count ne 0) then begin
                  for i = 0, count-1 do begin
                     wh2 = where(closenessMatrix[*,wh[i]], count2)
                     if(count2 ne 0) then begin; @todo: consider the best possible fusion
                        statusCode[wh2[0]] = FUSION
                        if not ptr_valid(candidates[wh2[0]]) then begin
                           candidates[wh2[0]] = ptr_new([dataVect[wh2[0]]])
                        endif
                        *candidates[wh2[0]] = [*candidates[wh2[0]],priorDataVect[sortPDVect[wh[i]]]]
                     endif
                  endfor
               endif
               
               if ptr_valid((*self.pParamStruct).pROICandidateArray) then begin
                  for i = 0, n_elements(*(*self.pParamStruct).pROICandidateArray)-1 do begin
                     if(ptr_valid((*(*self.pParamStruct).pROICandidateArray)[i])) then ptr_free, (*(*self.pParamStruct).pROICandidateArray)[i]
                  endfor
                  ptr_free, (*self.pParamStruct).pROICandidateArray
               endif
               (*self.pParamStruct).pROICandidateArray = ptr_new(candidates)
              
               *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = dataVect
               (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = priorHighestNumber > max(dataVect)

            endelse
         endif else begin
            *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = make_array(nObjects, /index, /long) + 1
            (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = nObjects
         endelse

         if whParamActive[1] then begin
            *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = statusCode      
         endif

      endif
   endelse
end

pro C_sROIParam_3DTrackObject::apply_old, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position

   whParam = [(where( *(*self.pParamStruct).pNames eq '3D Track Objects from Masks'))[0],$
                 (where( *(*self.pParamStruct).pNames eq '3D Track Objects from Masks II'))[0] ]
   
      ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1: if (position[0] eq -1) then return else  whParamActive[position] = 1
      else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase
   wherePA = where(whParamActive eq 1)
   
      ; check Pointers
   if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)
   
   nObjects = C_sROI3DGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin
   
      if whParamActive[0] then begin
         ; set Object  Number Vector
        *(*self.pParamStruct).pROINumberVect = (C_sROI3DGroupObj->getObjectNumberVector())
   
         ; get prior ObjTrackObject and Parameter
        s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
        priorImagePosition = tPos -1
        priorHighestNumber = 0
        if (priorImagePosition ge 0) then begin
         oPriorROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = priorImagePosition, chPos = chPos, clusPos = clusPos)
         if (obj_valid(oPriorROI3DGroup)) then oPrior3DParamContainer = oPriorROI3DGroup->getParamContainer()
         if (obj_valid(oPrior3DParamContainer)) then oPriorROI3DParam = oPriorROI3DGroup->getSelectedROIObjFromParamName(selROIParamName = '3D Track Objects from Masks', position = pos)
         if (obj_valid(oPriorROI3DParam)) then begin
             pPriorValueStruct = oPriorROI3DParam->getpValueStruct(position = whParam[0])
             priorDataVect = *(*pPriorValueStruct).pROIParamVect
             priorNumberVect = *(*(oPriorROI3DParam->getpParamStruct())).pROINumberVect
             priorHighestNumber = (*(*pPriorValueStruct).pValues)[(where( *(*pPriorValueStruct).pNames eq 'Highest Object Number'))[0]]
         endif
        endif
   
        overlapMask = C_sROI3DGroupObj->getGroupMask()
        hmax = max(overlapMask)
        hmin = min(overlapMask[where(overlapMask ne 0)])
        hist = histogram(overlapMask, min = hmin, max = hmax, reverse_indices = revInd )
        whereHist = where(hist ne 0)
   
        if (obj_valid(oPriorROI3DParam)) then begin
             ; prior frame does not contain objects
         if (priorDataVect[0] eq -1) then begin
             *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = (whereHist + priorHighestNumber)
             (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = max( (whereHist + priorHighestNumber) )
         endif else begin
   
             overlapMask = (temporary(overlapMask) gt 0) * (oPriorROI3DGroup->getGroupMaskInExactParamValOrder(selROIParamName = '3D Track Objects from Masks'))
             thisDataVect = lonArr(n_elements(whereHist))
             minDataVect = min(priorDataVect)
             maxDataVect = max(priorDataVect)
             overlapMatrix = lonArr(n_elements(whereHist), maxDataVect - minDataVect + 1)
             for i = 0, n_elements(whereHist)-1 do $
                overlapMatrix[i, *] = histogram(overlapMask[revInd[revInd[whereHist[i]]:revInd[whereHist[i]+1]-1]], min = minDataVect, max = maxDataVect)
             for i = 0, (maxDataVect - minDataVect) do begin
                whereMax = where(overlapMatrix[*,i] eq max(overlapMatrix[*,i]))
                overlapMatrix[*,i] = 0
                overlapMatrix[whereMax[0],i] = minDataVect + i
             endfor
             for i = 0, n_elements(thisDataVect)-1 do begin
                whereMax = (where(overlapMatrix[i,*] ne 0))[0]
                if (whereMax ne -1) then thisDataVect[i] = overlapMatrix[i,whereMax]
             endfor
             whereZero = where(thisDataVect eq 0)
             if (whereZero[0] ne -1) then thisDataVect[whereZero] = (priorHighestNumber +1 ) + make_Array(n_elements(whereZero), /index, /long)
   
             *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = thisDataVect
             (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = priorHighestNumber > max(thisDataVect)
         endelse
        endif else begin
         *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = make_array(n_elements(whereHist), /index, /int) + 1
         (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Highest Object Number'))[0]] = n_elements(whereHist)
        endelse
      endif
      if whParamActive[1] then begin
      endif
   endelse
end


pro C_sROIParam_3DTrackObject::applyThresholdFilter, position = position

    if (position eq -1) then pos = where( *(*self.pParamStruct).pActive eq 1) else pos = position
    whParamActive = where( *(*self.pParamStruct).pActive eq 1)

    if (total(pos) gt -1) then for i = 0, n_elements(pos)-1 do begin
       flagVector = bytArr(n_elements(*(*(*self.pValueStruct)[pos[i]]).pROIParamVect))
       if (n_elements(flagVector) le 0) then return

       if ( (((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]]) + $
          ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]]) + $
          ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]]) + $
          ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]])) le 0 ) then begin
         flagVector[*] = 1
       endif else begin
         if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]]) then begin
               thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]] ) < $
                          ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1b'))[0]] )
               thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]] ) > $
                          ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1b'))[0]] )
               flagVector = temporary(flagVector) > ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
         endif
         if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]]) then begin
               thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]] ) < $
                          ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2b'))[0]] )
               thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]] ) > $
                          ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2b'))[0]] )
               flagVector = temporary(flagVector) > ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
         endif
         if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]]) then begin
               thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]] ) < $
                          ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3b'))[0]] )
               thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]] ) > $
                          ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3b'))[0]] )
               flagVector = temporary(flagVector) > ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
         endif
         if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]]) then begin
               thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]] ) < $
                          ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4b'))[0]] )
               thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]] ) > $
                          ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4b'))[0]] )
               flagVector = temporary(flagVector) > ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
         endif
       endelse
       if (total(flagVector) gt 0) then begin
         whereFlagVector = where(flagVector)
         *(*self.pParamStruct).pROINumberVect = (*(*self.pParamStruct).pROINumberVect)[whereFlagVector]
         for j = 0, n_elements(whParamActive)-1 do $
          *(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect = (*(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect)[whereFlagVector]
       endif else begin
         *(*self.pParamStruct).pROINumberVect = -1
         for j = 0, n_elements(whParamActive)-1 do *(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect = -1
       endelse
    endfor
end


pro C_sROIParam_3DTrackObject::setThresholdFilters, oROIParamReference = oROIParamReference
    transferNameList = ['Threshold_1a', 'Threshold_1b','Threshold_2a','Threshold_2b','Threshold_3a','Threshold_3b','Threshold_4a','Threshold_4b']
    for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do for j = 0, n_elements(transferNameList)-1 do begin $
       whParam = (where((*(*(*self.pValueStruct)[i]).pNames) eq transferNameList[j]))[0]
       (*(*(*self.pValueStruct)[i]).pActive)[whParam] = (*(*(*oROIParamReference.pValueStruct)[i]).pActive)[whParam]
       (*(*(*self.pValueStruct)[i]).pMin)[whParam] = (*(*(*oROIParamReference.pValueStruct)[i]).pMin)[whParam]
       (*(*(*self.pValueStruct)[i]).pMax)[whParam] = (*(*(*oROIParamReference.pValueStruct)[i]).pMax)[whParam]
       (*(*(*self.pValueStruct)[i]).pValues)[whParam] = (*(*(*oROIParamReference.pValueStruct)[i]).pValues)[whParam]
    endfor
end


pro C_sROIParam_3DTrackObject::updateParamValueStruct, objectNumberVector = objectNumberVector, position = position, all = all
    n_objectNumberVector = n_elements(objectNumberVector)
    if (n_objectNumberVector le 0) then return

    flagVector = bytArr(n_elements(*(*self.pParamStruct).pROINumberVect))
    if not((n_objectNumberVector eq 1) and (objectNumberVector[0] eq -1)) then begin
       for i = 0, n_objectNumberVector-1 do begin
         whereNumberVector = (where(  (*(*self.pParamStruct).pROINumberVect) eq objectNumberVector[i]  ))[0]
         if (whereNumberVector ne -1) then flagVector[whereNumberVector] = 1b
       endfor
    endif

    if (total(flagVector) gt 0) then begin
       whereFlagVector = where(flagVector)
       *(*self.pParamStruct).pROINumberVect = (*(*self.pParamStruct).pROINumberVect)[whereFlagVector]
       if (keyWord_set(position)) then *(*(*self.pValueStruct)[position]).pROIParamVect = (*(*(*self.pValueStruct)[position]).pROIParamVect)[whereFlagVector]
       if (keyWord_set(all)) then    for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do $
          if ( (*(*self.pParamStruct).pActive)[i]  ) then *(*(*self.pValueStruct)[i]).pROIParamVect = (*(*(*self.pValueStruct)[i]).pROIParamVect)[whereFlagVector]
    endif else begin
       if (keyWord_set(position)) then $
         if ptr_valid((*(*self.pValueStruct)[position]).pROIParamVect) then *(*(*self.pValueStruct)[position]).pROIParamVect = -1 else (*(*self.pValueStruct)[position]).pROIParamVect = ptr_new( -1, /no_copy)
       if (keyWord_set(all)) then $
         for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do $
          if ptr_valid((*(*self.pValueStruct)[i]).pROIParamVect) then *(*(*self.pValueStruct)[i]).pROIParamVect = -1 else (*(*self.pValueStruct)[i]).pROIParamVect = ptr_new( -1, /no_copy)
    endelse
end


pro C_sROIParam_3DTrackObject::setpValueStruct, pValueStruct
    whereValueStructName = (where( *(*self.pParamStruct).pNames eq *pValueStruct.name))[0]
    if ptr_valid((*self.pValueStruct)[position]) then ptr_free, (*self.pValueStruct)[position]
    (*self.pValueStruct)[position] = ptr_new(*pValueStruct, /no_copy)
end
function C_sROIParam_3DTrackObject::getpValueStruct, position = position
    if (ptr_valid((*self.pValueStruct)[position])) then return, (*self.pValueStruct)[position]  else return, -1
end
pro C_sROIParam_3DTrackObject::setpParamStruct, pParamStruct
    if ptr_valid(self.pParamStruct) then ptr_free, self.pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
function C_sROIParam_3DTrackObject::getpParamStruct
   return, self.pParamStruct
end


pro C_sROIParam_3DTrackObject::cleanup
    for i = 0, n_tags((*self.pParamStruct))-1 do begin
        case size((*self.pParamStruct).(i), /tname) of
            'POINTER': ptr_free, (*self.pParamStruct).(i)
            'OBJREF': obj_destroy, (*self.pParamStruct).(i)
            else:
        endcase
     endfor
    for j = 0, n_elements(*self.pValueStruct)-1 do begin
       for i = 0, n_tags((*(*self.pValueStruct)[j]))-1 do begin
         case size((*(*self.pValueStruct)[j]).(i), /tname) of
               'POINTER': ptr_free, (*(*self.pValueStruct)[j]).(i)
               'OBJREF': obj_destroy, (*(*self.pValueStruct)[j]).(i)
               else:
         endcase
        endfor
        ptr_free, (*self.pValueStruct)[j]
    endfor
    ptr_free, self.pValueStruct
    ptr_free, self.pParamStruct
end


function C_sROIParam_3DTrackObject::init

   ROIParamStruct = {name:'3D Track Objects',$   ;  ROI Name.
                     type:   '3D ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROINumberVect: ptr_new(),$    ; Pointer on ROI-Obj Number Vector
                     pROIOverlapMatrix:ptr_new(),$    ; Pointer on ROI-Obj Overlap Matrix.
                     pROIDistanceMatrix:ptr_new(),$     ; Pointer on ROI-Obj Distance Matrix.
                     pROICandidateArray:ptr_new() }  ; Pointer on ROI-Obj Candidates Matrix.
                                                     ; each row (track#, 1,2,3, ...,N) contains a list of candidates

   self.pValueStruct = ptr_new(ptrArr(3))
   ROIParamWidgetType = make_array(3, /string, value = 'widget_slider')
   ROIParamNames = [ '3D Track Objects from Masks',$
                     '3D Track Status Code',$    
                     '3D Track Objects from Masks II' ]
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

   ROIValueStruct = {name: '3D Track Objects from Masks',$
                     type: '3D ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

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
   
   ROIValueStruct = {name:'3D Track Status Code',$
                     type:'3D ROI-Parameter-Method',$
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

   ROIValueStruct = {name:'3D Track Objects from Masks II',$
                     type:'3D ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

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

pro C_sROIParam_3DTrackObject__define
   tmp = {C_sROIParam_3DTrackObject, pParamStruct: ptr_new(), pValueStruct: ptr_new()}
end