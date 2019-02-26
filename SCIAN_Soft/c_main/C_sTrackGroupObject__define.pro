;_____________________________IOISIOI____________________
; NAME:
;      C_sTrackGroupObj
;
; PURPOSE:
;       - Encapsulation of C_sTrackObjects
;
; AUTHOR:
;    Dr. Steffen Härtel (2009)
;    e_mail: shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sTrackGroupObj')
;
; METHOHDS:
;     function C_sTrackGroupObj::init
;_____________________________IOISIOI____________________


function C_sTrackGroupObj::getParamNameList
   return, ['T Position',$
            'Ch Position',$
            'Z Position',$
            'Clus Position',$
            'T Start',$
            'T End',$
            'T Interval']
end

pro C_sTrackGroupObj::setParamAsStruct, paramStruct
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'T Position'))[0]] = paramStruct.a
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Ch Position'))[0]] = paramStruct.b
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Z Position'))[0]] = paramStruct.c
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Clus Position'))[0]] = paramStruct.d
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'T Start'))[0]] = paramStruct.e
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'T End'))[0]] = paramStruct.f
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'T Interval'))[0]] = paramStruct.g
end

function C_sTrackGroupObj::getParamAsStruct
   return, {a: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'T Position'))[0]],$
            b: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Ch Position'))[0]] ,$
            c: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Z Position'))[0]] ,$
            d: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Clus Position'))[0]] ,$
            e: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'T Start'))[0]] ,$
            f: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'T End'))[0]] ,$
            g: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'T Interval'))[0]]}
end


pro C_sTrackGroupObj::setActiveGroupParameterNameList
   oParams = self.oParamContainer->get(count = count, /all)
   if (count gt 0) then begin
      dummy = strArr(count)
      for i = 0, count-1 do dummy[i] = (*(oParams[i]->getpParamStruct())).Name
      if ptr_valid(self.pActiveGroupParameterNameList) then *self.pActiveGroupParameterNameList = dummy $
         else self.pActiveGroupParameterNameList = ptr_new(dummy, /no_copy)
   endif
end

function C_sTrackGroupObj::getObjectModelParamList, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos
   if (n_elements(ROIObjPos) eq 0) then ROIObjPos = (self->count() -1) else ROIObjPos = ROIObjPos < (self->count() -1)
   obj = self->get(position = ROIObjPos)
   if obj_valid(obj) then begin
      count = obj.oModelContainer->count()
      if (count lt 1) then return, '-NO SELECTION-'
      if (n_elements(ROIObjModelPos) eq 0) then ROIObjModelPos = (count-1) else ROIObjModelPos = ROIObjModelPos < (count-1)
      oModel = obj.oModelContainer->get(position = ROIObjModelPos)
      if obj_valid(oModel) then return,  *(*(oModel->getpParamStruct())).pNames
;         dummy = (*(*(oModel->getpParamStruct())).pNames)
;         return, dummy
;      endif
   endif else return, '-NO SELECTION-'
end

function C_sTrackGroupObj::getObjectModelList, ROIObjPos = ROIObjPos
   if (n_elements(ROIObjPos) eq 0) then ROIObjPos = (self->count() -1) else ROIObjPos = ROIObjPos < (self->count() -1)
   obj = self->get(position = ROIObjPos)
   if obj_valid(obj) then begin
      count = obj.oModelContainer->count()
      if (count lt 1) then return, '-NO SELECTION-'
      dummy = strArr(count)
      for i = 0, count-1 do dummy[i] = (*((obj.oModelContainer->get(position = i))->getpParamStruct())).name
      return, dummy
   endif else return, '-NO SELECTION-'
end

function C_sTrackGroupObj::getoROIObjectModelContainer, ROIObjPos = ROIObjPos
   if (n_elements(ROIObjPos) eq 0) then ROIObjPos = (self->count() -1) else ROIObjPos = ROIObjPos < (self->count() -1)
   obj = self->get(position = ROIObjPos)
   if obj_valid(obj) then return, obj.oModelContainer else return, -1
end

function C_sTrackGroupObj::getoROIObjectModel, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos
   if (n_elements(ROIObjPos) eq 0) then ROIObjPos = (self->count() -1) else ROIObjPos = ROIObjPos < (self->count() -1)
   obj = self->get(position = ROIObjPos)
   if obj_valid(obj) then begin
      if (n_elements(ROIObjModelPos) eq 0) then ROIObjModelPos = (obj.oModelContainer->count()-1) else ROIObjModelPos = ROIObjModelPos < (obj.oModelContainer->count()-1)
      oModel = obj.oModelContainer->get(position = ROIObjModelPos)
       if obj_valid(oModel) then return, oModel
   endif else return, -1
end


pro C_sTrackGroupObj::addObjectToGroup, ROIObjPos = ROIObjPos
   if obj_valid(self) then begin
      if (n_elements(ROIObjPos) eq 0) then ROIObjPos = self->count() else ROIObjPos = ROIObjPos < self->count()
      maxNumber = max(self->getObjectNumberVector())
      self->add, obj_new('C_sTrackObject', number = maxNumber+1), position = ROIObjPos
   endif
end


pro C_sTrackGroupObj::deleteObjectInGroup, ROIObjPos = ROIObjPos
   if obj_valid(self) then begin
      if (n_elements(ROIObjPos) eq 0) then ROIObjPos = (self.oObjContainer->count() -1) else ROIObjPos = ROIObjPos < (self.oObjContainer->count() -1)
      obj = self.oObjContainer->get(position = ROIObjPos)
      self.oObjContainer->remove, position = ROIObjPos
      if obj_valid(obj) then obj_destroy, obj
      ROIObjPos = ROIObjPos < (self.oObjContainer->count() -1)
   endif else ROIObjPos = -1
end


pro C_sTrackGroupObj::setObjParamsFromModels, ROIObjPos = ROIObjPos
   if (n_elements(ROIObjPos) eq 0) then ROIObjPos = (self->count() -1) else ROIObjPos = ROIObjPos < (self->count() -1)
   for i = 0, ROIObjPos do begin
      obj = self->get(position = i)
      if obj_valid(obj) then obj->setParamsFromModels
   endfor
end


function C_sTrackGroupObj::getpParamStruct
   return, self.pParamStruct
end

pro C_sTrackGroupObj::set,pParamStruct = pParamStruct
    *self.pParamStruct = pParamStruct
end

function C_sTrackGroupObj::getpColorState
   return, self.pColorState
end

pro C_sTrackGroupObj::setpColorState, pColorState = pColorState
    *self.pColorState = *pColorState
end


function C_sTrackGroupObj::getObjectNumberVector
   numVec = -1
   for i = 0, ((self.oObjContainer)->count())-1 do numVec = [numVec, ((self.oObjContainer)->get(position = i))->getNumber()]
   if (n_elements(numVec) eq 1) then return, numVec else return, numVec[1:*]
end


function C_sTrackGroupObj::getObjectNumber
   return, (self.oObjContainer)->count()
end


function C_sTrackGroupObj::getOpacVect, objName
   if (n_elements(objName) eq 0) then objName = '1st Volume Opacity'
   if (objName ne '2nd Volume Opacity') then objName = '1st Volume Opacity'
   case (*self.pColorState).opacFlag[0] of
      0:begin
         whParam = (where(*(*self.pParamStruct).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = ((*(*self.pParamStruct).pValues[whParam] > 0) < 255)
            *(*self.pParamStruct).pValues[whParam] = opacVal
            opacVect = make_array(256, /byte, value = opacVal)
            if (((*self.pColorState).bottomValues[0]) gt 0) then opacVect[0: ((*self.pColorState).bottomValues[0])] = 0
            if (((*self.pColorState).topValues[0]) le 254) then opacVect[((*self.pColorState).topValues[0])+1 : 255] = 0
         endif
      endcase
      else: opacVect = bytArr(256) + (*self.pColorState).opacValues[0,*]
   endcase
   return, opacVect
end


;_BEGIN_OBJECT_PARAMETER_FUNCTIONS_____________________________________________________________________
function C_sTrackGroupObj::getParamContainer
   return, self.oParamContainer
end

pro C_sTrackGroupObj::setParamContainer, object = object
   if obj_valid(self.oParamContainer) then begin
      oParams = (self.oParamContainer)->get(/all)
      (self.oParamContainer)->remove, /all
      if obj_valid(oParams[0]) then for i = 0, n_elements(oParams)-1 do obj_destroy, oParams[i]

      if obj_valid(object) then begin
         oParams = object->get(/all)
         object->remove, /all
         obj_destroy, object
         if obj_valid(oParams[0]) then for i = 0, n_elements(oParams)-1 do (self.oParamContainer)->add, oParams[i], position = i
      endif
   endif else if obj_valid(object) then self.oParamContainer = object
end

function C_sTrackGroupObj::getParamContainerObj, active = active
   if (active gt (self.oParamContainer->count()-1)) then active = self.oParamContainer->count()-1
   return, self.oParamContainer->get(position = active)
end

pro C_sTrackGroupObj::addParamObj, object
   if obj_valid(object) then self.oParamContainer->add, object
end

pro C_sTrackGroupObj::deleteParamContainerObj, active = active
   if (active gt (self.oSegContainer->count())) then active = self.oSegContainer->count()-1
   self.oParamContainer->remove, position = active
end


function C_sTrackGroupObj::getSelectedROIObjFromParamName, selROIParamName = selROIParamName
   if (n_elements(selROIParamName) gt 0) then $
      for i = 0, self.oParamContainer->count()-1 do begin
         oROIParam = self.oParamContainer->get(position = i)
         if ((*(oROIParam->getpParamStruct())).name eq selROIParamName) then return, oROIParam
         position = (where(*(*(oROIParam->getpParamStruct())).pNames eq selROIParamName, count))[0]
         if (count gt 0) then return, oROIParam
      endfor
   return, -1
end


function C_sTrackGroupObj::getGroupMaskInExactParamValOrder, selROIParamName = selROIParamName
  oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = position)
  if obj_valid(oROIParam) then begin
     pValueStruct = oROIParam->getpValueStruct(position = position)
     mask = make_array(dimension = dimArray, type = size(*(*pValueStruct).pROIParamVect, /type))
     for i = 0, self->count()-1 do begin
        whereObj = where(*(*(oROIParam->getpParamStruct())).pROINumberVect eq (self->get(position = i))->getNumber())
        if (whereObj[0] ne -1) then mask[*((self->get(position = i))->getpWherePoints())] = (*(*pValueStruct).pROIParamVect)[whereObj]
     endfor
     return, mask
  endif
  return, make_array(dimension = dimArray, /byte)
end


pro C_sTrackGroupObj::setGroupObjColorsInParamValOrder, selROIParamName = selROIParamName,$
                                                      ROIObjParamPos = ROIObjParamPos,$
                                                      ROIObjSubParamPos = ROIObjSubParamPos,$
                                                      pWhereThreshArr = pWhereThreshArr,$
                                                      pColorArr = pColorArr

   rgb_table0 = bytArr(256,3) + transpose((*self.pColorState).rgbValues[0,*,*])
   opacVect = self->getOpacVect('1st Volume Opacity')

   if (n_elements(selROIParamName) gt 0) then $
      oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = ROIObjSubParamPos) else $
      oROIParam = self.oParamContainer->get(position = ROIObjParamPos)

   if obj_valid(oROIParam) then begin
      pParamStruct = *(oROIParam->getpParamStruct())
      pValueStruct = oROIParam->getpValueStruct(position = ROIObjSubParamPos)
      minParam = min(*(*pValueStruct).pROIParamVect, max = maxParam)
      minParam *= 1.
      maxParam *= 1.
      fParam = (minParam eq -1) and (maxParam eq -1)
      if (maxParam eq minParam) then begin
         minParam = 0.
         maxParam >= 1.
      endif

         ; sortNumberVectInParamValOrder
      sortParamVect = sort(*(*pValueStruct).pROIParamVect)
      sortNumberVect = (*(pParamStruct).pROINumberVect)[sortParamVect]
      self->getProperty, uvalue = sortVect
      if (n_elements(sortVect) eq 0) then begin
         sortVect = {pOld:ptr_new(), pNew:ptr_new(), fUpdate:0b, fMovieKeepGalleryPosition:0b}
         sortVect.pNew = ptr_new(sortNumberVect)
         sortVect.pOld = ptr_new(sortNumberVect, /no_copy)
      endif else *sortVect.pNew = sortNumberVect
      self->setProperty, uvalue = sortVect

      if fParam then begin
         for i = 0, self->count()-1 do begin
            objNumber = (self->get(position = i))->getNumber()
            whereObj = (where(*(pParamStruct).pROINumberVect eq objNumber))[0]
            if (whereObj ne -1) then (self->get(position = i))->setProperty, color = [200,200,200]
         endfor
      endif else begin
         if (n_elements(pWhereThreshArr) le 0) then begin
         for i = 0, self->count()-1 do begin
            objNumber = (self->get(position = i))->getNumber()
            whereObj = (where(*(pParamStruct).pROINumberVect eq objNumber))[0]
            if (whereObj ne -1) then $
               (self->get(position = i))->setProperty, color = reform(rgb_table0[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1), *]),$
                                                       alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1)] / 255.
         endfor
         endif else begin
            whObjInThresh_1 = -1
            whObjInThresh_2 = -1
            whObjInThresh_3 = -1
            whObjInThresh_4 = -1
            for i = 0, self->count()-1 do begin
               objNumber = (self->get(position = i))->getNumber()
               whereObj = (where(*(pParamStruct).pROINumberVect eq objNumber))[0]

               if (whereObj ne -1) then begin
                  if ((*(pWhereThreshArr)[0])[0] ne -1) then whObjInThresh_1 = ((where((*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[0]] eq objNumber))[0] ne -1)
                  if ((*(pWhereThreshArr)[1])[0] ne -1) then whObjInThresh_2 = ((where((*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[1]] eq objNumber))[0] ne -1)
                  if ((*(pWhereThreshArr)[2])[0] ne -1) then whObjInThresh_3 = ((where((*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[2]] eq objNumber))[0] ne -1)
                  if ((*(pWhereThreshArr)[3])[0] ne -1) then whObjInThresh_4 = ((where((*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[3]] eq objNumber))[0] ne -1)

;                 print, i, (*(*pValueStruct).pROIParamVect)[whereObj], minParam, maxParam, round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1)
                  case 1 of
                     whObjInThresh_4: (self->get(position = i))->setProperty, color = *(pColorArr)[3], alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1)] / 255.
                     whObjInThresh_3: (self->get(position = i))->setProperty, color = *(pColorArr)[2], alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1)] / 255.
                     whObjInThresh_2: (self->get(position = i))->setProperty, color = *(pColorArr)[1], alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1)] / 255.
                     whObjInThresh_1: (self->get(position = i))->setProperty, color = *(pColorArr)[0], alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1)] / 255.

                     else:(self->get(position = i))->setProperty, color = reform(rgb_table0[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1), *]),$
                                                                  alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / (maxParam-minParam) * 254 + 1)] / 255.
                  endcase
               endif
            endfor
         endelse
      endelse
   endif
end


pro C_sTrackGroupObj::selectObjectsInThresholdIntervals, selROIParamName = selROIParamName
   oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = position)
   oROIParam->applyThresholdFilter, position = position

   pParamStruct = oROIParam->getpParamStruct()
   for i = self->count()-1,0,-1 do begin
      objNumber = (self->get(position = i))->getNumber()
      whereObj = (where (*(*pParamStruct).pROINumberVect eq objNumber))[0]
      if (whereObj eq -1) then obj_destroy, self->get(position = i)
   endfor
end


pro C_sTrackGroupObj::setParamFilters, oParamContainerReference = oParamContainerReference

   self->setActiveGroupParameterNameList
   count = oParamContainerReference->count()-1

   if ptr_valid(self.pActiveGroupParameterNameList) then begin
      if (n_elements(*self.pActiveGroupParameterNameList) lt 1) then *self.pActiveGroupParameterNameList = 'XXX'
   endif else self.pActiveGroupParameterNameList = ptr_new('XXX', /no_copy)

   for i = 0, count do begin
      oROIParamReference = oParamContainerReference->get(position = i)
      if (i eq 0) then referenceNameList = (*(oROIParamReference->getpParamStruct())).Name else referenceNameList = [referenceNameList, (*(oROIParamReference->getpParamStruct())).Name]
      whParam = where(*self.pActiveGroupParameterNameList eq referenceNameList[i])
      if (whParam[0] ne -1) then begin
         if (n_elements(whParam) gt 1) then begin
            if (n_elements(where (referenceNameList[i] eq referenceNameList)) eq 1) then whParam = whParam[0] else whParam = whParam[n_elements(where (referenceNameList[i] eq referenceNameList))-1]
         endif
         (self.oParamContainer->get(position = whParam))->setThresholdFilters, oROIParamReference = oParamContainerReference->get(position = i)
      endif
   endfor
end


pro C_sTrackGroupObj::calcParams, paramName = paramName, stack_tlb = stack_tlb

   count = self.oParamContainer->count()-1
   if (n_elements(selROIParamName) gt 0) then begin
      oROIParam = self->getSelectedROIObjFromParamName(paramName = paramName)
      count = 0
   endif

   for i = 0, count do begin
      if (i eq 0) then begin
         if not(obj_valid(oROIParam)) then oROIParam = self.oParamContainer->get(position = 0)
      endif else oROIParam = self.oParamContainer->get(position = i)

      case (*(oROIParam->getpParamStruct())).name of
         '3D Object Track Path': oROIParam->apply, stack_tlb = stack_tlb
         else:
      endcase

   endfor
end


function C_sTrackGroupObj::getObjNum
   return, self.oObjContainer->count()
end


function C_sTrackGroupObj::getTrackParam, paramName = paramName, objNum = objNum

    if (n_elements(paramName) eq 0) then return, -1
   if (n_elements(objNum) eq 0) then objNum = 0
   oObj = self.oObjContainer->get(position = objNum)
   if not(obj_valid(oObj)) then return, -1
   case paramName of
   'Track Path': return, {ok:1b, pXYZValues:oObj->getpXYZValues(), pTValues:oObj->getpTValues()}   
   ;'Track VCL': return, {ok:1b, XYZVelocity:oObj->getXYZVelocity(),pTValues:oObj->getpTValues(), pXYZValues:oObj->getpXYZValues()}
   'Track VCL': return, {ok:1b, XYZDisplacement:oObj->getXYZDisplacement(),pTValues:oObj->getpTValues(), pXYZValues:oObj->getpXYZValues(),dT:oObj->getdTValues()}
   'Track VSL': return, {ok:1b,VSL:oObj->getVSL(), DPos:oObj->getDPos(), pXYZValues:oObj->getpXYZValues()}
   'Track VAP': return, {ok:1b,VAP:oObj->getVAP(),Spath:oObj->getSpath(), pXYZValues:oObj->getpXYZValues(),pTValues:oObj->getpTValues()}
   ;'Track VAP': return, {ok:1b,VAP:oObj->getVAP(),Spath:oObj->getSpath(), pXYZValues:oObj->getpXYZValues(),pTValues:oObj->getpTValues(), pXYZRotEdges:oObj->getRotatedEdges()}
   'Track ALH': return, {ok:1b,ALH:oObj->getALH(),Spath:oObj->getSpath(),pXYZValues:oObj->getpXYZValues(),DPos:oObj->getDPos()}
   'Track LIN': return, {ok:1b,VSL:oObj->getVSL(),VCL:oObj->getXYZVelocity(), pTValues:oObj->getpTValues()}
   'Track WOB': return, {ok:1b,VCL:oObj->getXYZVelocity(), VAP:oObj->getVAP()}
   'Track STR': return, {ok:1b,VSL:oObj->getVSL(), VAP:oObj->getVAP()}
   'Track Aceleration': return, {ok:1b, XYZAceleration:oObj->getXYZAceleration(),pTValues:oObj->getpTValues(), pXYZValues:oObj->getpXYZValues()}
   ; MOR - 12Feb2011 - get concentration in time 
   'Track Concentration': return,{ok:1b, pTValues:oObj->getpTValues(), pNumObjs:oObj->getNumberConcentration()}
   
   else:
   endcase
   return, 3
   
end


pro C_sTrackGroupObj::initObjects, poStackTrackContainer = poStackTrackContainer, strID = strID, selROIGroupObj = selROIGroupObj

   pAllParams = *poStackTrackContainer->getTrackObjects(strID = strID, selROIGroupObj = selROIGroupObj)

   if ((*pAllParams[0]).num eq -1) then begin
      ptr_free, pAllParams[0]
      for i = 0, self.oObjContainer->count()-1 do self->deleteObjectInGroup
      return
   endif

   for i = 0, n_elements(pAllParams)-1 do begin
;      XYZValues = make_array(n_elements((*pAllParams[i]).t), 3)
;      XYZValues[*, 0] = (*pAllParams[i]).x
;      XYZValues[*, 1] = (*pAllParams[i]).y
;      XYZValues[*, 2] = (*pAllParams[i]).z
;      self.oObjContainer->add, obj_new('C_sTrackObject', number = (*pAllParams[i]).num,$
;                                                         tValues = (*pAllParams[i]).t,$
;                                                         XYZValues = XYZValues)
      ; MOR - Pass in the real size coordinates - BEGIN
      XYZValues = make_array(n_elements((*pAllParams[i]).t), 3)
      
      XYZValues[*, 0] = (*pAllParams[i]).x * ( (*pAllParams[i]).xPixSize )
      XYZValues[*, 1] = (*pAllParams[i]).y * ( (*pAllParams[i]).yPixSize ) 
      XYZValues[*, 2] = (*pAllParams[i]).z * ( (*pAllParams[i]).zPixSize )
      self.oObjContainer->add, obj_new('C_sTrackObject', number = (*pAllParams[i]).num,$
                                                         tValues = (*pAllParams[i]).t,$
                                                         XYZValues = XYZValues,$
                                                         numObjs = (*pAllParams[i]).numObjs);) ; MOR - changed name from XYZValues to XYZRealValues
     ; MOR - Pass in the real size coordinates - END
     
     
   endfor
   if (n_elements(pAllParams) ge 1) then for i = 0, n_elements(pAllParams)-1 do if ptr_valid(pAllParams[i]) then ptr_free, pAllParams[i]
   pAllParams = 0
end


pro C_sTrackGroupObj::cleanUp
   if ptr_valid(self.pActiveGroupParameterNameList) then ptr_free, self.pActiveGroupParameterNameList
   if ptr_valid(self.pColorState) then ptr_free, self.pColorState

   if ptr_valid(self.pParamStruct) then begin
      ptr_free, (*self.pParamStruct).pNames
      for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
      ptr_free, self.pParamStruct
   endif

   if obj_valid(self.oParamContainer) then begin
      oROIParams = self.oParamContainer->get(/all)
      self.oParamContainer->remove, /all
      if obj_valid(oROIParams[0]) then for i = 0, n_elements(oROIParams)-1 do obj_destroy, oROIParams[i]
      obj_destroy, self.oParamContainer
   endif

   if obj_valid(self.oObjContainer) then begin
      oROIParams = self.oObjContainer->get(/all)
      self.oObjContainer->remove, /all
      if obj_valid(oROIParams[0]) then for i = 0, n_elements(oROIParams)-1 do obj_destroy, oROIParams[i]
      obj_destroy, self.oObjContainer
   endif

   if obj_valid(self) then obj_destroy, self
end


function C_sTrackGroupObj::init, tPos = tPos,$
                                 chPos = chPos,$
                                 zPos = zPos,$
                                 clusPos = clusPos,$
                                 tStart = tStart,$
                                 tEnd = tEnd,$
                                 tInt = tInt

   paramNameList = self->getParamNameList()
   paramStruct = {TrackGroupParamStruct,$
                  pValues: ptrArr(n_elements(paramNameList), /allocate),$
                  pNames: ptr_new(paramNameList, /no_copy)}

   if (n_elements(tPos) eq 0) then tPos = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'T Position'))[0]] = tPos
   if (n_elements(chPos) eq 0) then chPos = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Ch Position'))[0]] = chPos
   if (n_elements(zPos) eq 0) then zPos = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Z Position'))[0]] = zPos
   if (n_elements(clusPos) eq 0) then clusPos = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Clus Position'))[0]] = clusPos
   if (n_elements(tStart) eq 0) then tStart = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'T Start'))[0]] = tStart
   if (n_elements(tEnd) eq 0) then tEnd = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'T End'))[0]] = tEnd
   if (n_elements(tInt) eq 0) then tInt = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'T Interval'))[0]] = tInt

   self.pParamStruct = ptr_new(paramStruct, /no_copy)

   colorState = {opacFlag:make_array(2, /byte, value = 0b),$
                 colTbl:make_array(2, /byte),$
                 rgbValues:make_array(2, 3, 256, /byte),$
                 gamma:make_array(2, /float, value = 1.),$
                 opacGamma:make_array(2, /float, value = 1.),$
                 stretchBotTop:make_array(2, 2, /byte),$
                 opacStretchBotTop:make_array(2, 2, /byte),$
                 blendValues:make_array(2, /byte, value = 255b),$
                 opacValues:make_array(2, 256, /byte, /index),$
                 bottomValues:make_array(2, /byte),$
                 topValues:make_array(2, /byte, value = 255b)}

   self.pColorState = ptr_new(colorState, /no_copy)

   self.oObjContainer = obj_new('IDL_Container')
   self.oParamContainer = obj_new('IDL_Container')
   return, 1
end


pro C_sTrackGroupObj__define
   tmp = {C_sTrackGroupObj, pParamStruct:ptr_new(),$
                            pColorState:ptr_new(),$
                            pActiveGroupParameterNameList:ptr_new(),$
                            oObjContainer:obj_new(),$
                            oParamContainer:obj_new()}
end