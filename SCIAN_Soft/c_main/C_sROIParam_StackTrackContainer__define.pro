;_____________________________IOISIOI____________________
; NAME:
;     C_sROIParam_StackTrackContainer
;
; PURPOSE:
;       - Container for Track Parameters
;
; AUTHOR:
;     Dr. Steffen Härtel (2009)
;     e_mail: shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_StackTrackContainer' )
;
; METHOHDS:
; MODIFICATIONS:
; 03Feb2011 - MOR: pass real xyz pixel size for track objects
;
;_____________________________IOISIOI____________________

function C_sROIParam_StackTrackContainer::getAndRemoveROIParamObjs
   oROIParams = self.oTrackParamContainer->get(/all)
   self.oTrackParamContainer->remove, /all
   return, oROIParams
end

function C_sROIParam_StackTrackContainer::getfAllParamControl
   count = self.oTrackParamContainer->count()-1L
   if (count lt 0L) then return, -1L
   oROIParam = self.oTrackParamContainer->get(position = 0L)
   if obj_valid(oROIParam) then return, (*(oROIParam->getpValueStruct())).fAllParamControl
end

pro C_sROIParam_StackTrackContainer::setActiveTrackParamNameList
   oParams = self.oTrackParamContainer->get(count = count, /all)
   if (count gt 0L) then begin
      dummy = strArr(count)
      for i = 0L, count-1L do dummy[i] = (*(oParams[i]->getpValueStruct())).name
      if ptr_valid(self.pTrackParamNameList) then *self.pTrackParamNameList = dummy else self.pTrackParamNameList = ptr_new(dummy, /no_copy)
   endif
end

pro C_sROIParam_StackTrackContainer::addoTrackParam, oTrackParam
   if obj_valid(oTrackParam) then if obj_isa(oTrackParam, 'C_sROIParam_StackTrack') then self.oTrackParamContainer->add, oTrackParam
end

function C_sROIParam_StackTrackContainer::getpActiveTrackParamNameList
   return, self.pTrackParamNameList
end

function C_sROIParam_StackTrackContainer::getTotalStackNumber
   return, self.totalTNum
end


function C_sROIParam_StackTrackContainer::getSelectedStackTrackObjFromParamName, paramName = paramName
   if (n_elements(paramName) gt 0) then begin
      if ptr_valid(self.pTrackParamNameList) then whereTrackParamName = (where(*self.pTrackParamNameList eq paramName))[0] else return, -1
      if (whereTrackParamName ne -1) then return, self.oTrackParamContainer->get(position = whereTrackParamName)
   endif
   return, -1
end

function C_sROIParam_StackTrackContainer::getParamFromParamName, paramName = paramName
   oStackTrack = self->getSelectedStackTrackObjFromParamName(paramName = paramName)
   if obj_valid(oStackTrack) then return, oStackTrack->getAllParamVect()
   return, -1
end


function C_sROIParam_StackTrackContainer::getTrackObjects, strID = strID, selROIGroupObj = selROIGroupObj

   case selROIGroupObj of
   'oROI2DGroup':track = self->getParamFromParamName(paramName = strID + 'Track Objects from Masks')
   'oROI3DGroup':track = self->getParamFromParamName(paramName = strID + '3D Track Objects from Masks')
   endcase

      ; sort objects in track
   if (track[0] ne -1) then begin
      whTrack = where(track ne -1, count)
      if (count gt 0) then track = track[whTrack]
      sortTrack = fix(track[uniq(track, sort(track))])
   endif else return, ptr_new({num:-1, t:-1, x:-1, y:-1, z:-1}, /no_copy)

   case selROIGroupObj of
   'oROI2DGroup':begin
      t = self->getParamFromParamName(paramName = strID + 'Object Time [s]')
      x = self->getParamFromParamName(paramName = strID + '2D x-center position')
      y = self->getParamFromParamName(paramName = strID + '2D y-center position')
      z = y * 0
     
      ; MOR - get the real pixel size - BEGIN
      xPixSize = self->getParamFromParamName(paramName = strID + 'Object X Real Size [x]')
      xPixSize = xPixSize[0]  ; all the times should have the same dimensions
      yPixSize = self->getParamFromParamName(paramName = strID + 'Object Y Real Size [y]') ; MOR - THIS IS NOT BEING PROPERLY REASSIGN, IT IS STAYING AS -1!!!
      yPixSize = xPixSize[0] ; use the xPixSize for now since it is the same for the motility calculations
      zPixSize = xPixSize[0] * 0
      ; MOR - get the real pixel size - END 
      
      ; MOR - 11Feb2011 - get count in order to display concentration on track motil - BEGIN
      numObjs = self->getParamFromParamName(paramName = strID + 'Object Number [Total]')
      ; MOR - 11Feb2011 - get count in order to display concentration on track motil - END      
          
   endcase
   'oROI3DGroup':begin
      t = self->getParamFromParamName(paramName = strID + '3D Object Time [s]')
      x = self->getParamFromParamName(paramName = strID + '3D x-center position')
      y = self->getParamFromParamName(paramName = strID + '3D y-center position')
      z = self->getParamFromParamName(paramName = strID + '3D z-center position')
            
      xPixSize = self->getParamFromParamName(paramName = strID + 'Object X Real Size [x]')
      xPixSize = xPixSize[0]  ; all the times should have the same dimensions
      yPixSize = self->getParamFromParamName(paramName = strID + 'Object Y Real Size [y]') ; MOR - THIS IS NOT BEING PROPERLY REASSIGN, IT IS STAYING AS -1!!!
      yPixSize = xPixSize[0] ; use the xPixSize for now since it is the same for the motility calculations
      zPixSize = xPixSize[0] * 0
      
      numObjs = self->getParamFromParamName(paramName = strID + 'Object Number [Total]')
      ; FASL: MOR changes.... make incompatible current 2D track with 3D definitions XD
      ; FASL: i will try to integrate my track system with 3D track proposed....but in first place I need fix MOR mods.
     
   endcase
   endcase

   if (count gt 0) then begin
      t = t[whTrack]
      x = x[whTrack]
      y = y[whTrack]
      z = z[whTrack]
      ; MOR - concentration
      numObjs = numObjs[whTrack]
      numObjs = numObjs(uniq(t))
   endif

   if (sortTrack[0] ne -1) then begin
      pAllParams = ptrArr(n_elements(sortTrack))
      for i = 0L, n_elements(sortTrack)-1L do begin
         whParam = where(track eq sortTrack[i])
         if (whParam[0] eq -1) then begin
            tMin = min(t, max = tMax)
            xMin = min(x, max = xMax)
            yMin = min(y, max = yMax)
            zMin = min(z, max = zMax)
            numObjsMin = min(numObjs, max = numObjsMax) ;MOR - include the number of objects

            pAllParams[i] = ptr_new({num:sortTrack[i],$
                                     t:[tMin, tMax],$
                                     x:[xMin, xMax],$
                                     y:[yMin, yMax],$
                                     z:[zMin, zMax],$
                                     xPixSize:xPixSize,$ ;MOR - include the per pixel size
                                     yPixSize:yPixSize,$
                                     zPixSize:zPixSize,$;}, /no_copy)
                                     numObjs:[numObjsMin, numObjsMax]}, /no_copy) ;MOR - include the number of objects

                                     
         endif else pAllParams[i] = ptr_new({num:sortTrack[i],$
                                             t:t[whParam],$
                                             x:x[whParam],$
                                             y:y[whParam],$
                                             z:z[whParam],$
                                             xPixSize:xPixSize,$
                                             yPixSize:yPixSize,$
                                             zPixSize:zPixSize,$;}, /no_copy)
                                             numObjs:numObjs},/no_copy)
      endfor
   endif

   return, pAllParams
end


pro C_sROIParam_StackTrackContainer::setTrackParameters, poCurrROI2DGroup = poCurrROI2DGroup,$
                                                         poCurrROI3DGroup = poCurrROI3DGroup,$
                                                         tPos = tPos,$
                                                         chPos = chPos,$
                                                         zPos = zPos,$
                                                         clusPos = clusPos,$
                                                         totalTNum = totalTNum,$
                                                         fAllParamControl = fAllParamControl

    self->setActiveTrackParamNameList
    if (self.totalTNum ne totalTNum) then self.totalTNum = totalTNum

    if obj_valid(*poCurrROI2DGroup) then if obj_isa(*poCurrROI2DGroup, 'C_sROIGroupObj') then begin
       strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(zPos>0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem)
       oParamContainer = (*poCurrROI2DGroup)->getParamContainer()
       count = oParamContainer->count()-1L
       for i = 0L, count do begin
         oROIParam = oParamContainer->get(position = i)
         whereROIParamActive = where(*(*(oROIParam->getpParamStruct())).pActive)
         if (whereROIParamActive[0] ne -1) then begin
          for j = 0, n_elements(whereROIParamActive)-1 do begin
              pValueStruct = oROIParam->getpValueStruct(position = whereROIParamActive[j])
              if ptr_valid(self.pTrackParamNameList) then whereTrackParamName = (where(*self.pTrackParamNameList eq (strID+(*pValueStruct).name) ))[0] else whereTrackParamName = -1
              if (whereTrackParamName ne -1) then begin
                 oTrackParam = self.oTrackParamContainer->get(position = whereTrackParamName)
                 if obj_valid(oTrackParam) then oTrackParam->addTrackParam, pValueStruct = pValueStruct,$
                                                                            tPos = tPos,$
                                                                            totalTNum = self.totalTNum,$
                                                                            fAllParamControl = fAllParamControl
              endif else begin
                 oTrackParam = obj_new('C_sROIParam_StackTrack', pValueStruct = oROIParam->getpValueStruct(position = whereROIParamActive[j]),$
                                                                 strID = strID,$
                                                                 tPos = tPos,$
                                                                 totalTNum = self.totalTNum,$
                                                                 fAllParamControl = fAllParamControl)
                 self.oTrackParamContainer->add, oTrackParam
              endelse
          endfor
         endif
       endfor
    endif

    if obj_valid(*poCurrROI3DGroup) then if obj_isa(*poCurrROI3DGroup, 'C_sROI3DGroupObj') then begin
       strID = strCompress('Ch' + string(chPos>0) + '_' + 'Z' + string(0) + '_'  + 'Clus' + string(clusPos>0) + '_', /rem)
       oParamContainer = (*poCurrROI3DGroup)->getParamContainer()
       count = oParamContainer->count()-1L
       for i = 0L, count do begin
         oROIParam = oParamContainer->get(position = i)
         whereROIParamActive = where(*(*(oROIParam->getpParamStruct())).pActive)
         if (whereROIParamActive[0] ne -1) then begin
          for j = 0L, n_elements(whereROIParamActive)-1L do begin
              pValueStruct = oROIParam->getpValueStruct(position = whereROIParamActive[j])
              if ptr_valid(self.pTrackParamNameList) then whereTrackParamName = (where(*self.pTrackParamNameList eq (strID+(*pValueStruct).name) ))[0] else whereTrackParamName = -1
              if (whereTrackParamName ne -1) then begin
                 oTrackParam = self.oTrackParamContainer->get(position = whereTrackParamName)
                 if obj_valid(oTrackParam) then oTrackParam->addTrackParam, pValueStruct = pValueStruct,$
                                                                                           tPos = tPos,$
                                                                                           totalTNum = self.totalTNum,$
                                                                                           fAllParamControl = fAllParamControl
              endif else begin
                 oTrackParam = obj_new('C_sROIParam_StackTrack', pValueStruct = oROIParam->getpValueStruct(position = whereROIParamActive[j]),$
                                                                                strID = strID,$
                                                                                tPos = tPos,$
                                                                                totalTNum = self.totalTNum,$
                                                                                fAllParamControl = fAllParamControl)
                 self.oTrackParamContainer->add, oTrackParam
              endelse
          endfor
         endif
       endfor
    endif

    self->setActiveTrackParamNameList
end


pro C_sROIParam_StackTrackContainer::getStackTrackContainer, stack_tlb = stack_tlb, currentStackTrackFileName = currentStackTrackFileName
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
       oImage = (*stackState.pImageStackInfoObject)->getSelectedImageObject(tPos = 0, chPos = 0, zPos = 0)
    widget_control, stack_tlb, set_uValue = stackState, /no_copy

    if obj_valid(oImage) then begin
       parameterPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Parameter Path'))[0]]
       oImage->get, pParamStruct = pParamStruct
       fileName = strCompress(parameterPath + '_oStackTrackParam.sav')
       if (fileName ne currentStackTrackFileName) then begin
         currentStackTrackFileName = fileName
         openr, 1, currentStackTrackFileName, error = err
         close, /all
         if (err eq 0) then begin
          restore, fileName = fileName, restored_objects = oStackTrackContainer, /relaxed
          for i = 0L, (n_elements(oStackTrackContainer)-1L) do if obj_isa(oStackTrackContainer[i], 'C_sROIParam_StackTrackContainer') then begin
              oROIParams = self.oTrackParamContainer->get(/all)
              self.oTrackParamContainer->remove, /all
              if obj_valid(oROIParams[0]) then for j = 0L,n_elements(oROIParams)-1L do obj_destroy, oROIParams[j]
              oROIParams = (oStackTrackContainer[i])->getAndRemoveROIParamObjs()
              if obj_valid(oROIParams[0]) then for j = 0L, n_elements(oROIParams)-1L do self.oTrackParamContainer->add, oROIParams[j]
              obj_destroy, oStackTrackContainer[i]
              return
          endif
         endif
       endif
    endif
end


pro C_sROIParam_StackTrackContainer::cleanup
   oROIParams = self.oTrackParamContainer->get(/all)
   self.oTrackParamContainer->remove, /all
   obj_destroy, self.oTrackParamContainer
   if obj_valid(oROIParams[0]) then for i = 0L,n_elements(oROIParams)-1L do obj_destroy, oROIParams[i]
   if ptr_valid(self.pTrackParamNameList) then ptr_free, self.pTrackParamNameList
end


function C_sROIParam_StackTrackContainer::init
   self.oTrackParamContainer = obj_new('IDL_Container')
   return, 1
end


pro C_sROIParam_StackTrackContainer__define
   tmp = {C_sROIParam_StackTrackContainer, oTrackParamContainer:obj_new(),$
                                           pTrackParamNameList:ptr_new(),$
                                           totalTNum:-1L}
end