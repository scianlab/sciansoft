; NAME:
;      C_sROI3DGroupObj
;
; PURPOSE:
;       - Encapsulation of IDLgrROIGroup object  
;       - Creates the following graphic objects:
;       ->  oPhaseModel = obj_new('IDLgrModel', uValue = 'oPhaseModel')                       ;IDLgrModel-Container of all Sub-Models below
;                    add-> oObjectModel = obj_new('IDLgrModel', uValue = 'oObjectModel')       ; contains 1st and 2nd segmented Objects (1/0)
;                       -> oSubPhaseModel = obj_new('IDLgrModel', uValue = 'oSubPhaseModel')   ;   contains Sub-Phase
;                       -> oObjectBorderModel = obj_new('IDLgrModel', uValue = 'oObjectBorderModel')   ; contains Object Border as Polygons
;                       -> oBorderDistanceModel = obj_new('IDLgrModel', uValue = 'oBorderDistanceModel')  ; contains Object Border Distances as Polygons
;                       -> oCenterDistanceModel = obj_new('IDLgrModel', uValue = 'oCenterDistanceModel')  ; contains Object Centrer Distances as Polygons
;                       -> o3DModel = obj_new('IDLgrModel', uValue = 'o3DModel')  ; contains Object Centrer Distances as Polygons
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROI3DGroupObj')
;
; METHOHDS:
;     function C_sROI3DGroupObj::init, Name = Name, Number = Number, ImagePixSize = ImagePixSize, WhereBorderPoints = WhereBorderPoints
;                          Name: Name of Object
;                          Number: Number of Object
;                          ImagePixSize: 2D- or 3D-embedding Object-Space
;_____________________________IOISIOI____________________

;_BEGIN_INTERNAL_FUNCTIONS_____________________________________________________________________

pro C_sROI3DGroupObj::setIDLgrROIGroupXYZCoord
   xyzDim = 1l * self->getxyzDim()
   minFract = min((*self.pEigenSys).sizePerXYZ)
   xyzFract = (*self.pEigenSys).sizePerXYZ / (minFract * max(xyzDim))
   if (xyzDim[0] gt xyzDim[1]) then xyFact = [1., 1. * xyzDim[1] / xyzDim[0]] else xyFact = [1. * xyzDim[0] / xyzDim[1], 1.]
   self->setProperty, xCoord_conv = [-.5*xyFact[0], xyzFract[0]], yCoord_conv = [-.5*xyFact[1], xyzFract[1]], zCoord_conv = [.0, xyzFract[2]]
end

pro C_sROI3DGroupObj::setActiveGroupParameterNameList
   oParams = self.oParamContainer->get(count = count, /all)
   if (count gt 0) then begin
      dummy = strArr(count)
      for i = 0L, count-1 do dummy[i] = (*(oParams[i]->getpParamStruct())).Name
      if ptr_valid(self.pActiveGroupParameterNameList) $
      then *self.pActiveGroupParameterNameList = dummy $
      else self.pActiveGroupParameterNameList = ptr_new(dummy, /no_copy)
   endif
end

function C_sROI3DGroupObj::getObjectModelParamList, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos
   ROIObjPos = (n_elements(ROIObjPos) eq 0) ? (self->count() -1) : (ROIObjPos < (self->count() -1))
   obj = self->get(position = ROIObjPos)
   if obj_valid(obj) then begin
      count = obj.oModelContainer->count()
      if (count lt 1) then return, '-NO SELECTION-'
      ROIObjModelPos = (n_elements(ROIObjModelPos) eq 0) ? (count-1) : (ROIObjModelPos < (count-1))
      oModel = obj.oModelContainer->get(position = ROIObjModelPos)
      if obj_valid(oModel) then return, *(*(oModel->getpParamStruct())).pNames
;         dummy = (*(*(oModel->getpParamStruct())).pNames)
;         return, dummy
;      endif
   endif else return, '-NO SELECTION-'
end

function C_sROI3DGroupObj::getObjectModelList, ROIObjPos = ROIObjPos
   ROIObjPos = (n_elements(ROIObjPos) eq 0) ? (self->count() - 1) : (ROIObjPos < (self->count() - 1))
   obj = self->get(position = ROIObjPos)
   if obj_valid(obj) then begin
      count = obj.oModelContainer->count()
      if (count lt 1) then return, '-NO SELECTION-'
      dummy = strArr(count)
      for i = 0L, count-1 do dummy[i] = (*((obj.oModelContainer->get(position = i))->getpParamStruct())).name
      return, dummy
   endif else return, '-NO SELECTION-'
end

function C_sROI3DGroupObj::getoROIObjectModelContainer, ROIObjPos = ROIObjPos
   ROIObjPos = (n_elements(ROIObjPos) eq 0) ? (self->count() - 1) : (ROIObjPos < (self->count() - 1))
   obj = self->get(position = ROIObjPos)
   if obj_valid(obj) then return, obj.oModelContainer  else return, -1
end

function C_sROI3DGroupObj::getoROIObjectModel, ROIObjPos = ROIObjPos, ROIObjModelPos = ROIObjModelPos
   ROIObjPos = (n_elements(ROIObjPos) eq 0) ? (self->count() - 1) : (ROIObjPos < (self->count() - 1))
   obj = self->get(position = ROIObjPos)
   if obj_valid(obj) then begin
      ROIObjModelPos = (n_elements(ROIObjModelPos) eq 0) ? (obj.oModelContainer->count() - 1) : (ROIObjModelPos < (obj.oModelContainer->count() - 1))
      oModel = obj.oModelContainer->get(position = ROIObjModelPos)
      if obj_valid(oModel) then return, oModel
   endif else return, -1
end

pro C_sROI3DGroupObj::addObjectToGroup, ROIObjPos = ROIObjPos
   if obj_valid(self) then begin
      ROIObjPos = (n_elements(ROIObjPos) eq 0) ? self->count() : (ROIObjPos < self->count())
      maxNumber = max(self->getObjectNumberVector())
      self->add, obj_new('C_sROI3DObject', number = maxNumber+1), position = ROIObjPos
   endif
end

pro C_sROI3DGroupObj::deleteObjectInGroup, ROIObjPos = ROIObjPos
   if obj_valid(self) then begin
      ROIObjPos = (n_elements(ROIObjPos) eq 0) ? (self->count() - 1) : (ROIObjPos < (self->count() - 1))
      obj = self->get(position = ROIObjPos)
      self->remove, position = ROIObjPos
      if obj_valid(obj) then obj_destroy, obj
      ROIObjPos = ROIObjPos < (self->count() - 1)
   endif else ROIObjPos = -1
end

pro C_sROI3DGroupObj::addModelToObject, ROIObjPos = ROIObjPos, selROIObjModel = selROIObjModel, position = position
   if obj_valid(self) then begin
      obj = self->get(position = ROIObjPos)
      if obj_valid(obj) then begin
         position = (n_elements(position) eq 0) ? obj.oModelContainer->count() : (position < obj.oModelContainer->count())
         oROIModel = obj_new(selROIObjModel)
         if obj_valid(oROIModel) then obj.oModelContainer->add, oROIModel
      endif
   endif
end

pro C_sROI3DGroupObj::deleteModelInObject, ROIObjModelPos = ROIObjModelPos, ROIObjPos = ROIObjPos
   if obj_valid(self) then begin
      ROIObjPos = (n_elements(ROIObjPos) eq 0) ? (self->count() - 1) : (ROIObjPos < (self->count() - 1))
      obj = self->get(position = ROIObjPos)
      if obj_valid(obj) then begin
         ROIObjModelPos = (n_elements(ROIObjModelPos) eq 0) ? (obj.oModelContainer->count() - 1) : (ROIObjModelPos < (obj.oModelContainer->count() - 1))
         oModel = obj.oModelContainer->get(position = ROIObjModelPos)
         obj.oModelContainer->remove, position = ROIObjModelPos
         if obj_valid(oModel) then obj_destroy, oModel
         ROIObjModelPos = ROIObjModelPos < (obj.oModelContainer->count() - 1)
      endif
   endif else ROIObjModelPos = -1
end

pro C_sROI3DGroupObj::setObjParamsFromModels, ROIObjPos = ROIObjPos
   ROIObjPos = (n_elements(ROIObjPos) eq 0) ? (self->count() - 1) : (ROIObjPos < (self->count() - 1))
   for i = 0L, ROIObjPos do begin
      obj = self->get(position = i)
      if obj_valid(obj) then obj->setParamsFromModels
   endfor
end

pro C_sROI3DGroupObj::cleanUp
   self->IDLgrROIGroup::cleanUp
   if ptr_valid(self.pActiveGroupParameterNameList) then ptr_free, self.pActiveGroupParameterNameList
   if ptr_valid(self.pVolState) then ptr_free, self.pVolState
   if ptr_valid(self.pEigenSys) then ptr_free, self.pEigenSys

   if ptr_valid(self.pParamStruct) then begin
      ptr_free, (*self.pParamStruct).pNames
      for i = 0L, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
      ptr_free, self.pParamStruct
   endif
   if obj_valid(self.oParamContainer) then begin
      oROIParams = self.oParamContainer->get(/all)
      self.oParamContainer->remove, /all
      if obj_valid(oROIParams[0]) then for i = 0L, n_elements(oROIParams)-1 do obj_destroy, oROIParams[i]
      obj_destroy, self.oParamContainer
   endif
   if obj_valid(self) then begin
      self->getProperty, uvalue = sortVect
      if (n_elements(sortVect) ne 0) then begin
         if ptr_valid(self.pParamStruct) then ptr_free, sortVect.pOld
         if ptr_valid(self.pParamStruct) then ptr_free, sortVect.pNew
      endif
      objs = self->get(/all)
      if obj_valid(objs[0]) then begin
         nObjs = n_elements(objs)
         for i = nObjs-1, 0ul, -1 do obj_destroy, objs[i]
         self->remove, /all
      endif
      obj_destroy, self
   endif
end

;_BEGIN_SET_FUNCTIONS_____________________________________________________________________
pro C_sROI3DGroupObj::set, pParamStruct = pParamStruct
    *self.pParamStruct = pParamStruct
end

pro C_sROI3DGroupObj::setpVolState, pVolState = pVolState
    *self.pVolState = *pVolState
end

pro C_sROI3DGroupObj::setParamAsStruct, paramStruct
   parameterNameList = self->getParameterNameList()
   whParam = (where(parameterNameList[0] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.a
   whParam = (where(parameterNameList[1] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.b
   whParam = (where(parameterNameList[2] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.c
   whParam = (where(parameterNameList[3] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.d
   whParam = (where(parameterNameList[4] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.e
   whParam = (where(parameterNameList[5] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.f
   whParam = (where(parameterNameList[6] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.g
   whParam = (where(parameterNameList[7] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.h
   whParam = (where(parameterNameList[8] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.i
   whParam = (where(parameterNameList[9] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.j
   whParam = (where(parameterNameList[10] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.k
   whParam = (where(parameterNameList[11] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.l
   whParam = (where(parameterNameList[12] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.m
   whParam = (where(parameterNameList[13] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.n
   whParam = (where(parameterNameList[14] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.o
   whParam = (where(parameterNameList[15] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.p
   whParam = (where(parameterNameList[16] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.q
   whParam = (where(parameterNameList[17] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.r
   whParam = (where(parameterNameList[18] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.s
   whParam = (where(parameterNameList[19] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.t
   whParam = (where(parameterNameList[20] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.u
   whParam = (where(parameterNameList[21] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.v
   whParam = (where(parameterNameList[22] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.w
   whParam = (where(parameterNameList[23] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.x
   whParam = (where(parameterNameList[24] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.y
   whParam = (where(parameterNameList[25] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.z
   whParam = (where(parameterNameList[26] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aa
   whParam = (where(parameterNameList[27] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ab
   whParam = (where(parameterNameList[28] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ac
   whParam = (where(parameterNameList[29] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ad
   whParam = (where(parameterNameList[30] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ae
   whParam = (where(parameterNameList[31] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.af
   whParam = (where(parameterNameList[32] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ag
   whParam = (where(parameterNameList[33] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ah
   whParam = (where(parameterNameList[34] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ai
   whParam = (where(parameterNameList[35] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aj
   whParam = (where(parameterNameList[36] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ak
   whParam = (where(parameterNameList[37] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.al
   whParam = (where(parameterNameList[38] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.am
   whParam = (where(parameterNameList[39] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.an
   whParam = (where(parameterNameList[40] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ao
   whParam = (where(parameterNameList[41] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ap
   whParam = (where(parameterNameList[42] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aq
   whParam = (where(parameterNameList[43] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ar
   whParam = (where(parameterNameList[44] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.as
   whParam = (where(parameterNameList[45] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.at
   whParam = (where(parameterNameList[46] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.au
   whParam = (where(parameterNameList[47] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.av   
   whParam = (where(parameterNameList[48] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aw   
   whParam = (where(parameterNameList[49] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ax  
   
   whParam = (where(parameterNameList[50] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ay
   whParam = (where(parameterNameList[51] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.az
   whParam = (where(parameterNameList[52] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ba
   whParam = (where(parameterNameList[53] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bb
   whParam = (where(parameterNameList[54] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bc
   whParam = (where(parameterNameList[55] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bd
   whParam = (where(parameterNameList[56] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.be
   whParam = (where(parameterNameList[57] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bf
   whParam = (where(parameterNameList[58] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bg
   whParam = (where(parameterNameList[59] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bh
   whParam = (where(parameterNameList[60] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bi
   whParam = (where(parameterNameList[61] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bj    
   whParam = (where(parameterNameList[62] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bk    
   whParam = (where(parameterNameList[63] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bl    
   whParam = (where(parameterNameList[64] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bm    
   whParam = (where(parameterNameList[65] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bn    
   whParam = (where(parameterNameList[66] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bo    
   whParam = (where(parameterNameList[67] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bp    
   whParam = (where(parameterNameList[68] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bq    
   whParam = (where(parameterNameList[69] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.br    
   whParam = (where(parameterNameList[70] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bs    
   whParam = (where(parameterNameList[71] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bt    
   whParam = (where(parameterNameList[72] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bu
end
 
pro C_sROI3DGroupObj::keepVisParamStruct, paramStruct
   parameterNameList = self->getParameterNameList()
   whParam = (where(parameterNameList[13] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.n
   whParam = (where(parameterNameList[14] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.o
   whParam = (where(parameterNameList[15] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.p
   whParam = (where(parameterNameList[16] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.q
   whParam = (where(parameterNameList[17] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.r
   whParam = (where(parameterNameList[18] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.s
   whParam = (where(parameterNameList[19] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.t
   whParam = (where(parameterNameList[20] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.u
   whParam = (where(parameterNameList[21] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.v
   whParam = (where(parameterNameList[22] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.w
   whParam = (where(parameterNameList[23] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.x
   whParam = (where(parameterNameList[24] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.y
   whParam = (where(parameterNameList[25] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.z
   whParam = (where(parameterNameList[26] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aa
   whParam = (where(parameterNameList[27] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ab
   whParam = (where(parameterNameList[28] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ac
   whParam = (where(parameterNameList[29] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ad
   whParam = (where(parameterNameList[30] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ae
   whParam = (where(parameterNameList[31] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.af
   whParam = (where(parameterNameList[32] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ag
   whParam = (where(parameterNameList[33] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ah
   whParam = (where(parameterNameList[34] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ai
   whParam = (where(parameterNameList[35] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aj
   whParam = (where(parameterNameList[36] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ak
   whParam = (where(parameterNameList[37] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.al
   whParam = (where(parameterNameList[38] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.am
   whParam = (where(parameterNameList[39] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.an
   whParam = (where(parameterNameList[40] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ao
   whParam = (where(parameterNameList[41] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ap
   whParam = (where(parameterNameList[42] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aq
   whParam = (where(parameterNameList[43] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ar
   whParam = (where(parameterNameList[44] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.as
   whParam = (where(parameterNameList[45] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.at
   whParam = (where(parameterNameList[46] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.au
   whParam = (where(parameterNameList[47] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.av
   whParam = (where(parameterNameList[48] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aw
   whParam = (where(parameterNameList[49] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ax

   whParam = (where(parameterNameList[50] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ay
   whParam = (where(parameterNameList[51] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.az
   whParam = (where(parameterNameList[52] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ba
   whParam = (where(parameterNameList[53] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bb
   whParam = (where(parameterNameList[54] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bc
   whParam = (where(parameterNameList[55] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bd
   whParam = (where(parameterNameList[56] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.be
   whParam = (where(parameterNameList[57] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bf
   whParam = (where(parameterNameList[58] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bg
   whParam = (where(parameterNameList[59] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bh
   whParam = (where(parameterNameList[60] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bi
   whParam = (where(parameterNameList[61] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bj
   whParam = (where(parameterNameList[62] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bk      
   whParam = (where(parameterNameList[63] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bl    
   whParam = (where(parameterNameList[64] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bm    
   whParam = (where(parameterNameList[65] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bn    
   whParam = (where(parameterNameList[66] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bo    
   whParam = (where(parameterNameList[67] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bp    
   whParam = (where(parameterNameList[68] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bq    
   whParam = (where(parameterNameList[69] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.br    
   whParam = (where(parameterNameList[70] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bs    
   whParam = (where(parameterNameList[71] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bt    
   whParam = (where(parameterNameList[72] eq *(*self.pParamStruct).pNames))[0]
   if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.bu  
   
end;_END_SET_FUNCTIONS_____________________________________________________________________

;_BEGIN_GET_FUNCTIONS_____________________________________________________________________
function  C_sROI3DGroupObj::getpParamStruct
   return, self.pParamStruct
end

function C_sROI3DGroupObj::getpVolState
   return, self.pVolState
end

function  C_sROI3DGroupObj::getParamAsStruct
   parameterNameList = self->getParameterNameList()
   pArrValues = ptrArr(n_elements(parameterNameList))
   for i = 0L, n_elements(parameterNameList)-1 do begin
      whParam = (where(parameterNameList[i] eq *(*self.pParamStruct).pNames))[0]
      if (whParam eq -1) then begin
         pArrValues[i] = ptr_new('NOT CONTAINED')
         print, *pArrValues[i]
      endif else pArrValues[i] = ptr_new(*(*self.pParamStruct).pValues[whParam])
   endfor
   paramStruct = {a: *pArrValues[0],$
                  b: *pArrValues[1],$
                  c: *pArrValues[2],$
                  d: *pArrValues[3],$
                  e: *pArrValues[4],$
                  f: *pArrValues[5],$
                  g: *pArrValues[6],$
                  h: *pArrValues[7],$
                  i: *pArrValues[8],$
                  j: *pArrValues[9],$
                  k: *pArrValues[10],$
                  l: *pArrValues[11],$
                  m: *pArrValues[12],$
                  n: *pArrValues[13],$
                  o: *pArrValues[14],$
                  p: *pArrValues[15],$
                  q: *pArrValues[16],$
                  r: *pArrValues[17],$
                  s: *pArrValues[18],$
                  t: *pArrValues[19],$
                  u: *pArrValues[20],$
                  v: *pArrValues[21],$
                  w: *pArrValues[22],$
                  x: *pArrValues[23],$
                  y: *pArrValues[24],$
                  z: *pArrValues[25],$
                  aa: *pArrValues[26],$
                  ab: *pArrValues[27],$
                  ac: *pArrValues[28],$
                  ad: *pArrValues[29],$
                  ae: *pArrValues[30],$
                  af: *pArrValues[31],$
                  ag: *pArrValues[32],$
                  ah: *pArrValues[33],$
                  ai: *pArrValues[34],$
                  aj: *pArrValues[35],$
                  ak: *pArrValues[36],$
                  al: *pArrValues[37],$
                  am: *pArrValues[38],$
                  an: *pArrValues[39],$
                  ao: *pArrValues[40],$
                  ap: *pArrValues[41],$
                  aq: *pArrValues[42],$
                  ar: *pArrValues[43],$
                  as: *pArrValues[44],$
                  at: *pArrValues[45],$
                  au: *pArrValues[46],$
                  av: *pArrValues[47],$
                  aw: *pArrValues[48],$
                  ax: *pArrValues[49],$
                  ay: *pArrValues[50],$
                  az: *pArrValues[51],$
                  ba: *pArrValues[52],$
                  bb: *pArrValues[53],$
                  bc: *pArrValues[54],$
                  bd: *pArrValues[55],$
                  be: *pArrValues[56],$
                  bf: *pArrValues[57],$
                  bg: *pArrValues[58],$
                  bh: *pArrValues[59],$
                  bi: *pArrValues[60],$
                  bj: *pArrValues[61],$
                  bk: *pArrValues[62],$
                  bl: *pArrValues[63],$
                  bm: *pArrValues[64],$
                  bn: *pArrValues[65],$
                  bo: *pArrValues[66],$
                  bp: *pArrValues[67],$
                  bq: *pArrValues[68],$
                  br: *pArrValues[69],$
                  bs: *pArrValues[70],$
                  bt: *pArrValues[71],$
                  bu: *pArrValues[72]}

   for i = 0L, n_elements(parameterNameList)-1 do ptr_free, pArrValues[i]
   return, paramStruct
end

function C_sROI3DGroupObj::getObjectNumberVector
   objNumberVector = -1
   for i = 0L, (self->count())-1 do objNumberVector = [objNumberVector, (self->get(position = i))->getNumber()]
   if (n_elements(objNumberVector) eq 1) then return, objNumberVector else return, objNumberVector[1:*]
end

pro C_sROI3DGroupObj::initObjectSortVect
  ; sortNumberVectInParamValOrder
  objNumVect = self->getObjectNumberVector()
  sortParamVect = sort(objNumVect)
  sortNumVect = objNumVect[sortParamVect]
  self->getProperty, uvalue = sortVect

  if (n_elements(sortVect) eq 0) then begin
    sortVect = {pOld:ptr_new(), pNew:ptr_new(), fUpdate:0b, fMovieKeepGalleryPosition:0b}
    sortVect.pNew = ptr_new(sortNumVect)
    sortVect.pOld = ptr_new(sortNumVect, /no_copy)
  endif else *sortVect.pNew = sortNumVect

  self->setProperty, uvalue = sortVect
end

function C_sROI3DGroupObj::getGroupMask, mask = mask, zPos = zPos
   if (n_elements(mask) eq 0) then mask = intArr(*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                                                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
                                                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]])
   for i = 0L, (self->count())-1 do mask[*((self->get(position = i))->getpWherePoints())] = (self->get(position = i))->getNumber()
   if (n_elements(zPos) gt 0) then begin
      dimMask = size(mask, /dim)
      return, intArr(dimMask[0], dimMask[1]) + mask[*, *, (zPos>0) < (dimMask[2]-1)]
   endif else return, mask
end

function C_sROI3DGroupObj::getGroupMaskIntensity, maskIntensity = maskIntensity, zPos = zPos

   xyzDim = self->getxyzDim()

   if (n_elements(zPos) eq 0) then begin
      if (n_elements(maskIntensity) eq 0) then maskIntensity = intArr(xyzDim)
      for i = 0L, (self->count())-1 do maskIntensity[*((self->get(position = i))->getpWherePoints())] = *((self->get(position = i))->getpPointValues())
   endif else begin
      if (n_elements(maskIntensity) eq 0) then maskIntensity = intArr(xyzDim[0:1])

      for i = 0L, (self->count())-1 do begin
         xyzPoints = ((self->get(position = i))->getxyzPoints())
         whereZ = where(xyzPoints[2,*] eq zPos, count)
         if (count gt 0) then maskIntensity[(xyzPoints[0,*])[whereZ], (xyzPoints[1,*])[whereZ]] = (*((self->get(position = i))->getpPointValues()))[(xyzPoints[0,*])[whereZ] + $
                                                                                                                                                    (xyzPoints[1,*])[whereZ] * xyzDim[0] + $
                                                                                                                                                    (xyzPoints[2,*])[whereZ] * (xyzDim[0] * xyzDim[1])]
      endfor
   endelse
   return, maskIntensity
end

function C_sROI3DGroupObj::getObjectInArray, objNumber = objNumber
   xyzPoints = (self->get(position = objNumber))->getxyzPoints()
   minxyzPoints0 = min(xyzPoints[0,*], max = maxxyzPoints0)
   minxyzPoints1 = min(xyzPoints[1,*], max = maxxyzPoints1)
   minxyzPoints2 = min(xyzPoints[2,*], max = maxxyzPoints2)
   xyzPoints[0,*] -= (minxyzPoints0 - 1)
   xyzPoints[1,*] -= (minxyzPoints1 - 1)
   xyzPoints[2,*] -= (minxyzPoints2 - 1)
   mask = make_array(maxxyzPoints0-minxyzPoints0+2, maxxyzPoints1-minxyzPoints1+2, maxxyzPoints2-minxyzPoints2+2, /byte)
   intValues = mask - 1.
   mask[xyzPoints[0,*], xyzPoints[1,*], xyzPoints[2,*]] = 1
   intValues[where(mask)] = *((self->get(position = objNumber))->getpPointValues())
   return, {mask: mask, intValues: intValues}
end

function C_sROI3DGroupObj::getGroupCenterXYZ
   nROIGroupObjs = self->count()
   ctrCoord = make_array(3, nROIGroupObjs > 1, /float, value = -1.)
   for i = 0L, nROIGroupObjs-1 do ctrCoord[*,i] = [(self->get(position = i))->getCenterXYZ()]
   return, ctrCoord
end

function C_sROI3DGroupObj::getxyzDim
   return, [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]]]
end

function C_sROI3DGroupObj::getxyzRealSize
   return, [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [real]'))[0]]]
end

function C_sROI3DGroupObj::getxyzSizePerPixel
   return, [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]] / *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]] / *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [real]'))[0]] / *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]]]
end

pro C_sROI3DGroupObj::update3DModelWithGrowthFactor, maxGrowthFactor = maxGrowthFactor

   xyzDim = self->getxyzDim()
   if (n_elements(maxGrowthFactor) eq 0) then maxGrowthFactor = 9

      ;getGroupMask
   objNumVec = self->getObjectNumberVector()
   dummy = self->getGroupMask()
   gMask = dummy gt 0

      ;define growthFilter
   oOpenClose = obj_new('C_sImageFilter_OpenClose')
   oOpenClose->get, pParamStruct = pParamStruct
   (*(*pParamStruct).pActive)[(where(*(*pParamStruct).pNames eq 'Erode'))[0]] = 0
   (*(*pParamStruct).pActive)[(where(*(*pParamStruct).pNames eq 'Delate'))[0]] = 1
   (*(*pParamStruct).pValues)[(where(*(*pParamStruct).pNames eq 'Tolerance'))[0]] = 7

   slice = (bytArr(xyzDim[0], xyzDim[1]) + gMask[*,*,xyzDim[2]-1])
   for i = xyzDim[2]-1,1,-1 do begin
      if (max(slice) gt 0) then begin
         for j = 0, maxGrowthFactor-1 do slice = oOpenClose->apply(image = slice)
            ; define Overlapping Regions
         slice = temporary(slice) * gMask[*,*,i-1]
            ; add appearing Objects
         for j = 0L, n_elements(objNumVec)-1 do begin
            whereObj = where(dummy[*,*,i-1] eq objNumVec[j])
            if (whereObj[0] ne -1) then if (max(slice[whereObj]) eq 0) then slice[whereObj] = 1
         endfor
         gMask[*,*,i-1] = slice
         tvscl, slice
      endif else slice = gMask[*,*,i-1]
   endfor
   obj_destroy, oOpenClose

   gMask = temporary(gMask) * dummy
   dummy = self->getGroupMaskIntensity()

   for i = (self->count())-1, 0, -1 do begin
      obj = self->get(position = i)
      self->remove, position = i
      whereObj = where(gMask eq obj->getNumber())
      if (whereObj[0] ne -1) then begin
         obj->setWherePoints, whereObj
         obj->setPointValues, dummy[whereObj]
         self->add, obj, position = i
      endif else obj_destroy, obj
   endfor
end


;_BEGIN_ADD_FUNCTIONS_____________________________________________________________________
pro C_sROI3DGroupObj::initialize3DModels, stack_tlb = stack_tlb, fAcceptAllObjects = fAcceptAllObjects

   xyzDim = self->getxyzDim()
   xyzFrameRealSize = self->getxyzRealSize()

   clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
   tPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]
   chPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]
   zPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Position'))[0]]
   zSliceInitial = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Initial'))[0]]

      ; get imageStackInfoObject
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy

      ; get 3D z-Slice Objects
   for i = 0, xyzDim[2]-1 do begin
      oImage = imageStackInfoObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = i)
      if obj_valid(oImage) then begin
         oImage->get, pParamStruct = pParamStruct

         oROIGroup = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = i, clusPos = clusPos)
         if obj_valid(oROIGroup) then groupMask = oROIGroup->getGroupMask() else groupMask = make_array(xyzDim[0], xyzDim[1], /byte)
         if (n_elements(zSliceObj) eq 0) then begin
            dimGroupMask = size(groupMask, /dim)
            xyzDim = [xyzDim[0]<dimGroupMask[0], xyzDim[1]<dimGroupMask[1], xyzDim[2]]
            zSliceObj = intArr(xyzDim)
            image3D = intArr(xyzDim)
            if (xyzDim[2] eq 1) then begin
               zSliceObj = reform(zSliceObj, xyzDim)
               image3D = reform(image3D, xyzDim)
            endif
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = xyzDim[0]
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = xyzDim[1]
         endif

         zSliceObj[*,*,i] = groupMask
         if obj_valid(oROIGroup) then image3D[*,*,i] = oROIGroup->getGroupMaskIntensity() else image3D[*,*,i] = 0b

         if (i eq zSliceInitial) then begin
            zSliceMaskNumbers = oROIGroup->getObjectNumberVector()
            zSliceMaskCount = oROIGroup->count()
            if fAcceptAllObjects then begin
               zSliceMaskNumbers = 1
               zSliceMaskCount = 1
            endif
         endif
         groupMask = 0
         if obj_valid(oROIGroup) then obj_destroy, oROIGroup
      endif
   endfor

   oImage3D = obj_new('IDLgrVolume', image3D)

   save, oImage3D, filename = s_getPathForSystem()+'oImage3D.tmp'
   image3D = 0

   obj_destroy, oImage3D

   if ~fAcceptAllObjects then begin

      zSliceObjInit = zSliceObj[*,*,zSliceInitial]                   ;take the initial zslice image
      for i = zSliceInitial+1, xyzDim[2]-1 do begin                  ;go through the stack, up from the initial zslice image
         zSliceObjZ = zSliceObj[*,*,i] gt 0                          ;save the next image as a binary mask
         zSliceObjZNum = s_apop_flaeche(zSliceObjZ)                  ;label image regions by size
         zSliceObjZ *= zSliceObjInit                                 ;calculate intersection and add to the image

         for j = 1, max(zSliceObjZNum) do begin                      ;loop through all the current image objects
            whereArea = where(zSliceObjZNum eq j)                    ;select a particular object
            if (max(zSliceObjZ[whereArea]) ne 0) then begin
               areaHist = histogram(zSliceObjZ[whereArea], min = 1)  ;accumulate the different values in the mask (overlay)
               zSliceObjZ[whereArea] = (where(areaHist eq max(areaHist)))[0] + 1  ; get the index of the biggest region in the labeled image
            endif else begin
               zSliceObjZ[whereArea] = 0
            endelse
         endfor
         zSliceObj[*,*,i] = zSliceObjZ                               ;save new image in the stack
         zSliceObjInit = zSliceObjZ                                  ;continue with the next zslice image in the stack
      endfor

      zSliceObjInit = zSliceObj[*,*,zSliceInitial]
      for i = zSliceInitial-1, 0, -1 do begin
        zSliceObjZ = zSliceObj[*,*,i] gt 0                          ;save the next image as a binary mask
         zSliceObjZNum = s_apop_flaeche(zSliceObjZ)                 ;label image regions by size
         zSliceObjZ *= zSliceObjInit                                ;compute intersection and add it to the image

         for j = 1, max(zSliceObjZNum) do begin                      ; loop through all objects in the current image
            whereArea = where(zSliceObjZNum eq j)                    ; select a particular object
            if (max(zSliceObjZ[whereArea]) ne 0) then begin
               areaHist = histogram(zSliceObjZ[whereArea], min = 1)  ; accumulate the distinct values in the mask (overlap)
               zSliceObjZ[whereArea] = (where(areaHist eq max(areaHist)))[0] + 1  ;index of the biggest region in the image (identified by its size in the labeled image)
            endif else begin
               zSliceObjZ[whereArea] = 0
            endelse
         endfor
         zSliceObj[*,*,i] = zSliceObjZ                               ;save in the stack the new image
         zSliceObjInit = zSliceObjZ                                  ;continue with the next zslice image in the stack
      endfor

   endif else begin
     zSliceObj gt= 0
     labelObj = bytArr(xyzDim+2)
     labelObj[1:xyzDim[0],1:xyzDim[1],1:xyzDim[2]] = zSliceObj
     zSliceObj = 0
     zSliceObj = (label_region(labelObj, /ulong))[1:xyzDim[0],1:xyzDim[1],1:xyzDim[2]]
     labelObj  = 0
     zSliceMaskCount   = max(zSliceObj)
     zSliceMaskNumbers = make_array(zSliceMaskCount, /int, /index) + 1
   endelse

   if (zSliceMaskCount gt 0) then begin
     pWhereXYZArr = ptrArr(zSliceMaskCount)
     ;for i = 0L, zSliceMaskCount-1 do pWhereXYZArr[i] = ptr_new(where(zSliceObj eq zSliceMaskNumbers[i]), /no_copy)
     h = histogram(zSliceObj, min = zSliceMaskNumbers[0], reverse_indices = ri)
     for j = 0L, n_elements(h)-1 do if (ri[j+1] gt ri[j]) then pWhereXYZArr[j] = ptr_new([ri[ri[j]:ri[j+1]-1]], /no_copy)
     ri = 0
     h  = 0
   endif
   zSliceObj = 0

   restore, s_getPathForSystem()+'oImage3D.tmp', restored_objects = oImage3D, /relaxed
   oImage3D = oImage3D[0]
   oImage3D->getProperty, data0 = image3D, /no_copy
   obj_destroy, oImage3D
   save, oImage3D, filename = s_getPathForSystem() + 'oImage3D.tmp'

   ;max3DcontainerSize = 0u - 1
   ;if (zSliceMaskCount gt max3DcontainerSize) then stop ; Too many 3D objects for the container to store. Not an issue in 2017 with 64-bit IDL.
   for i = 0L, zSliceMaskCount-1 do $
     if ((*pWhereXYZArr[i])[0] eq -1) then stop else $ ; If stopped here, check why a labeled ROI has no labeled voxels.
     self->add, obj_new('C_sROI3DObject', number = zSliceMaskNumbers[i], tPos = tPos, chPos = chPos, zPos = zPos, $
                                          xyzFramePixSize = xyzDim, xyzFrameRealSize = xyzFrameRealSize, $
                                          pointValues = image3D[*pWhereXYZArr[i]], wherePoints = *pWhereXYZArr[i]), position = i

   for i = zSliceMaskCount-1, 0, -1 do ptr_free, pWhereXYZArr[i]
   if n_elements(pWhereXYZArr) ne 0 then ptr_free, pWhereXYZArr
end

;Yoya Old
;pro C_sROI3DGroupObj::init3DModelsFromPaintedMasks, stack_tlb = stack_tlb, fAcceptAllObjects = fAcceptAllObjects
;
;   xyzDim = self->getxyzDim()
;   xyzFrameRealSize = self->getxyzRealSize()
;
;   clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
;   tPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]
;   chPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]
;
;      ; get imageStackInfoObject
;   widget_control, stack_tlb, get_uValue = stackState, /no_copy
;      imageStackInfoObject = *stackState.pImageStackInfoObject
;   widget_control, stack_tlb, set_uValue = stackState, /no_copy
;   imageStackInfoObject->get, pParamStruct = pParamStruct
;   paintedMaskPath = [*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Mask Path'))[0]], 'X']
;
;      ; get 3D Mask
;   file = dialog_pickfile(/read, path = paintedMaskPath[0], get_path = path, filter = '*.tif')
;   posLastBatch = strPos(path, path_sep(), /reverse_s)
;   path = strMid(path,0, posLastBatch)
;   posNextBatch = strPos(path,path_sep(), /reverse_s)
;   paintedMaskPath = [strMid(path, 0, posNextBatch+1), strMid(path, posNextBatch+1, posLastBatch-posNextBatch)]
;
;   pos_c = strPos(paintedMaskPath[1],'_c', /reverse_s)
;   pos_fill = strPos(paintedMaskPath[1],'_fill', /reverse_s)
;   nCell = strMid(paintedMaskPath[1],pos_c + 2, pos_fill - pos_c - 2)
;   nCellNum = s_getRightNumberFromString(nCell)
;   baseName = strMid(paintedMaskPath[1],0, pos_c + 2) + strCompress(string(nCellNum), /rem) + '_fill'
;   tryName = paintedMaskPath[0] + baseName + path_sep()+ baseName + '_z01.tif'
;   pWhereCell = ptrArr(1)
;   cellNumVect = -1
;
;   mask3D = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
;
;   while query_tiff(tryName) do begin
;
;      for i = 0, xyzDim[2]-1 do begin
;         strZ = strCompress(string(i+1), /rem)
;         while (strLen(strZ) lt 2) do strZ = strCompress('0' + strZ, /rem)
;         tryName = paintedMaskPath[0] + baseName + '\' + baseName + '_z' + strZ + '.tif'
;         if query_tiff(tryName) then begin
;            thisImage = read_tiff(tryName)
;;            thisImage = congrid(read_tiff(tryName),512,512)
;            maxI = max(thisImage, min = minI)
;            if (maxI ne minI) then begin
;               hist = histogram(thisImage, min = minI, max = maxI)
;               if (hist[0] lt hist[maxI]) then mask3D[*,*,i] = maxI - thisImage else mask3D[*,*,i] = thisImage
;            endif else mask3D[*,*,i] = 0
;;            tvscl, thisImage
;         endif else mask3D[*,*,i] = 0
;      endfor
;
;      mask3D = byte(label_region(mask3D, /all))
;      if (max(mask3D) gt 1) then begin
;         hist = histogram(mask3D, min = 1)
;         whereMax = where(hist eq max(hist)) + 1
;      endif else whereMax = 1
;      whereCell = where(mask3D eq whereMax[0])
;      if (whereCell[0] ne -1) then begin
;         if (cellNumVect[0] eq -1) then begin
;            pWhereCell = ptr_new(whereCell, /no_copy)
;            cellNumVect = nCellNum
;         endif else begin
;            pWhereCell = [pWhereCell, ptr_new(whereCell, /no_copy)]
;            cellNumVect = [cellNumVect, nCellNum]
;         endelse
;      endif
;
;      nCellNum += 1
;      baseName = strMid(paintedMaskPath[1],0, pos_c + 2) + strCompress(string(nCellNum), /rem) + '_fill'
;      tryName = paintedMaskPath[0] + baseName + '\' + baseName + '_z01.tif'
;   endwhile
;   mask3D = 0
;
;      ; get 3D Image
;   image3D = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
;   for i = 0, xyzDim[2]-1 do image3D[*,*,i] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i)
;
;   if (cellNumVect[0] ne -1) then for i = 0L, n_elements(cellNumVect)-1 do $
;      self->add, obj_new('C_sROI3DObject', number = cellNumVect[i],$
;                                           tPos = tPos,$
;                                           chPos = chPos,$
;                                           zPos = zPos,$
;                                           xyzFramePixSize = xyzDim,$
;                                           xyzFrameRealSize = xyzFrameRealSize,$
;                                           pointValues = image3D[*pWhereCell[i]],$
;                                           wherePoints = *pWhereCell[i]),$
;                                           position = i
;
;   if (cellNumVect[0] ne -1) then for i = n_elements(cellNumVect)-1, 0, -1 do ptr_free, pWhereCell[i]
;   ptr_free, pWhereCell
;end


; TODO JJ here making 3D mask chaos, pt2
; OPTIONALS ARGUMENTS:
;     curMaskPath  used as a 'memory' parameter to avoid asking the user for mask folders
;     tPos         current time position, auxiliary 'memory' parameter to avoid asking the user for mask folders
;- stackFolder
; - masksFolder
;  - t000
;    - ch00
;      - clus00
;       - fill_c1
;       - ...
;      - clus01
;       - fill_c1
;       - ...
;    - ch01
;      - clus00
;       - fill_c1
;       - ...
;      - clus01
;       - fill_c1
;       - ...
;  - t001
;    - ch00
;      - clus00
;       - fill_c1
;       - ...
; AUTHOR:
;       Jorge Jara W. (2011) jjara@dcc.uchile.cl
;
; PURPOSE:
;       Generation and optional storing of the mask folder structure for a stack with time/channel/cluster mask images.
;
; RETURN VALUE:
;       A string containing the path to the image masks folders of the given time/channel/cluster position in the stack.
;
; ARGUMENTS:
;       tPos        indicates the time position for the output path.
;       chPos       indicates the channel position for the output path.
;       clusPos     indicates the cluster position for the output path.
;       samplePath  path to a mask file that can be used to detect the folder name pattern in order to construct the path structure.
;       pathStruct  a named variable to specify or store the path structure. If this variable is set, the 
;                   content os samplePath is ignored. This variable is intended to be used for repeated 
;                   invocations of the function.
function testAndAssemblePath, tPos = tPos, chPos = chPos, clusPos = clusPos, samplePath = samplePath, pathStruct = pathStruct

  slash = path_sep()

  case 1 of
    n_elements(pathStruct) ne 0: begin
      print, 'reading pathStruct'
      thePath = pathStruct.basePath
      countTdigits = pathStruct.numTdigits
      countChDigits = pathStruct.numChDigits
      countClusDigits = pathStruct.numClusDigits
    end

    n_elements(samplePath) ne 0: begin
      print, 'parsing samplePath'
      basePath = strMid(samplePath, 0, strPos(strMid(samplePath, 0, strLen(samplePath)-1), slash, /reverse_search))

      ; TODO JJ says "please somebody make a function of these ;)"
      regExpChPrefix = '\'+ slash + 'ch'
      regExpCh = regExpChPrefix + '[0|1|2|3|4|5|6|7|8|9]*'
      matchChPos = stRegEx(samplePath, regExpCh)
      strCh = (matchChPos ne -1) ? stRegEx(samplePath, regExpCh, /extract) : ''
      countChDigits = strLen(strCh) - strLen(regExpChPrefix) + 1
      if countChDigits gt 0 then basePath = strMid(basePath, 0, strPos(strMid(basePath, 0, strLen(basePath)-1), slash, /reverse_search))

      regExpClusPrefix = '\' + slash + 'clus'
      regExpClus = regExpClusPrefix + '[0|1|2|3|4|5|6|7|8|9]*'
      matchClusPos = stRegEx(samplePath, regExpClus)
      strClus = (matchClusPos ne -1) ? stRegEx(samplePath, regExpClus, /extract) : ''
      countClusDigits = strLen(strClus) - strLen(regExpClusPrefix) + 1
      if countClusDigits gt 0 then basePath = strMid(basePath, 0, strPos(strMid(basePath, 0, strLen(basePath)-1), slash, /reverse_search))

      regExpTprefix = '\' + slash + 't'
      regExpT = regExpTprefix + '[0|1|2|3|4|5|6|7|8|9]*'
      matchTPos = stRegEx(samplePath, regExpT)
      strT = (matchTPos ne -1) ? stRegEx(samplePath, regExpT, /extract) : ''
      countTdigits = strLen(strT) - strLen(regExpTprefix) + 1
      if countTDigits gt 0 then basePath = strMid(basePath, 0, strPos(strMid(basePath, 0, strLen(basePath)-1), slash, /reverse_search))

      thePath = basePath
    end

    else: begin
      print, 'No useful parameter was supplied to assemble proper path, returning a fish head...'
      return, -1
    end
  endcase

  if (countTDigits gt 0) then begin
  ;if (n_elements(tPos) ne 0) then begin
    tStr = strCompress('t' + string(tPos, format='(I0' + strCompress(countTdigits, /remove_all) + ')'), /remove_all)
    thePath += slash + tStr
  endif

  if (countChDigits gt 0) then begin
  ;if (n_elements(chPos) ne 0) then begin
    chStr = strCompress('ch' + string(chPos, format='(I0' + strCompress(countChdigits, /remove_all) + ')'), /remove_all)
    thePath += slash + chStr
  endif

  if (countClusDigits gt 0) then begin
  ;if (n_elements(clusPos) ne 0) then begin
    clusStr = strCompress('clus' + string(clusPos, format='(I0' + strCompress(countClusDigits, /remove_all) + ')'), /remove_all)
    thePath += slash + clusStr
  endif
  thePath += '\'
  print, 'assembled path is ', thePath
  if arg_present(pathStruct) and (~keyword_set(pathStruct)) $
  then pathStruct = {basePath : basePath,$
                     numTdigits: countTdigits > 0,$
                     numChDigits: countChDigits > 0,$
                     numClusDigits: countClusDigits > 0}
  return, thePath
end


; AUTHOR:
;        Jorge Jara W. (2011)
; NOTES:
;        Method rewritten to manage masks folders for a 3D stack with multiple times, channels and/or clusters.
;        This method depends on the utilitary function testAndAssemblePath for proper folder reading.
;        2012.03.19 TODO Yoya fix. Mask value for ROI must be 255
pro C_sROI3DGroupObj::init3DModelsFromPaintedMasks, stack_tlb = stack_tlb,$
                                                   fAcceptAllObjects = fAcceptAllObjects,$
                                                   maskPathCacheStruct = maskPathCacheStruct,$
                                                   fValueType = fValueType
  fValueType = keyword_set(fValueType) ? fValueType : 1 ; Default: 1 .. data with no zero values

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZNum
  slash = path_sep()
  xyzFrameRealSize = self->getxyzRealSize()
  xyzDim = self->getxyzDim()
  ;xyzDim[2] = totalZNum

  clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
  tPos    = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]
  chPos   = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]
  zPos    = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Position'))[0]]

    ; get imageStackInfoObject
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
    imageStackInfoObject->get, pParamStruct = pParamStruct
  paintedMaskPath = [*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Mask Path'))[0]], 'X']

    ; get 3D Mask; use the maskPathCacheStruct variable if available to avoid repeated pickfile dialog
  fFile = keyword_set(maskPathCacheStruct) ? 1b : 0b
  if (fFile eq 0) then begin
    file = dialog_pickfile(/read, path = paintedMaskPath[0], get_path = path, filter = '*.tif', title = 'Select the first mask image file for reading')
    mPath = testAndAssemblePath(tPos = tPos, chPos = chPos, clusPos = clusPos, samplePath = path, pathStruct = maskPathCacheStruct)
  endif else mPath = testAndAssemblePath(tPos = tPos, chPos = chPos, clusPos = clusPos, pathStruct = maskPathCacheStruct)

  cellNumVect = [-1]
    ; look for the folders with masks in the directory that match the pattern
  pattern = '*fill_c*'
  cellMaskFoldersList = file_search(mPath, pattern, /test_directory, count = nRois)

  if (nRois lt 1) then begin
    print, 'no masks found for t =', tPos, ' ch =', chPos, 'in directory ', mPath
    return
  endif

    ; get the number of the cells whose masks are located as subfolders in the current folder
    ; (not always the full cell set is stored, nor are they found in correlatives)
  for i = 0L, n_elements(cellMaskFoldersList)-1 do begin
    cPos = strPos(cellMaskFoldersList[i], '_c', /reverse_s)
    cNum = s_getRightNumberFromString(strMid(cellMaskFoldersList[i], cPos))
    cellNumVect = [cellNumVect, cNum]
  endfor

  i0 = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = 0)
  xyzDim = self->getxyzDim()
  mask3D = make_array(xyzDim, /byte)
  image3D = make_array(xyzDim, /byte)
  image3D[*,*,0] = temporary(i0)
  if (totalZNum gt 1) then $
    for i = 1, xyzDim[2]-1 do image3D[*,*,i] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i)

    ; for loop, first element is a -1 index so it is ignored
  for i = 1L, nRois do begin
    mask3D = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
    zSlicesList = file_search(cellMaskFoldersList[i-1], '*fill_c*_z*.tif', count = zCount)
    preNumberLen = strLen(strMid(zSlicesList[0], 0, strPos(zSlicesList[0], '.tif')))
    zIndices = make_array(n_elements(zSlicesList), /uint)
    ; TODO JJ sorting file names before loading the files in main memory... perform speed tests without sorting
    for j = 0, zCount-1 do zIndices[j] = s_getRightNumberFromString(strMid(zSlicesList[j], 0, strLen(strMid(zSlicesList[j], 0, strPos(zSlicesList[j], '.tif')))))
    sortedZindices = sort(zIndices)
    zInitial = xyzDim[2] - 1
    for j = 0, xyzDim[2]-1 do begin
      print, 'Reading mask file ', zSlicesList[sortedZindices[j]]
      ; always data values returned as non zero
      mask3d[*,*,j] = getRightMaskFromTiff(name = zSlicesList[sortedZindices[j]], fValueType = fValueType)
      ; TODO JJ I'm not sure how much necessary is to get the zStartCell (zInitial) parameter for the 3D-ROI generation
      if (max(mask3d[*,*,j]) gt 0) then zInitial <= j
    endfor
    whereRoi = where(mask3D ne 0, /L64)
    mask3D = 0
    self->add, obj_new('C_sROI3DObject', number = cellNumVect[i],$
                                         tPos = tPos,$
                                         chPos = chPos,$
                                         zPos = zPos,$ ; TODO JJ ??? Algo not sure about this one... pending
                                         xyzFramePixSize = xyzDim,$
                                         xyzFrameRealSize = xyzFrameRealSize,$
                                         pointValues = image3D[whereRoi],$
                                         wherePoints = whereRoi,$
                                         zSliceInitial = zInitial);zStartCell); position = i-1) ; ??? pending
  endfor
end


pro C_sROI3DGroupObj::init3DModelsFromPaintedMasksBAK, stack_tlb = stack_tlb, fAcceptAllObjects = fAcceptAllObjects,$
                                                    maskPathCacheStruct = maskPathCacheStruct

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZNum
   xyzDim = self->getxyzDim()
   xyzDim[2] = totalZNum

   xyzFrameRealSize = self->getxyzRealSize()

   clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
   tPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]
   chPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]

      ; get imageStackInfoObject
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   imageStackInfoObject->get, pParamStruct = pParamStruct
   paintedMaskPath = [*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Mask Path'))[0]], 'X']

      ; get 3D Mask
   file = dialog_pickfile(/read, path = paintedMaskPath[0], get_path = path, filter = '*.tif')
   posLastBatch = strPos(path,path_sep(), /reverse_s)
   path = strMid(path,0, posLastBatch)
   posNextBatch = strPos(path,path_sep(), /reverse_s)
   paintedMaskPath = [strMid(path, 0, posNextBatch+1), strMid(path, posNextBatch+1, posLastBatch-posNextBatch)]

   pos_c = strPos(paintedMaskPath[1],'_c', /reverse_s)
   if (pos_c eq -1) then print, '_c is not present in mask-name !' $
      else nCellNum = s_getRightNumberFromString(s_getLeftNumberFromStringAsString(strMid(paintedMaskPath[1],pos_c + 2)))

   baseName = strMid(paintedMaskPath[1],0, pos_c + 2) + strCompress(string(nCellNum), /rem)
   strZ = '0'
   for i = 1, 10 do begin
      if query_tiff(paintedMaskPath[0] + baseName + path_sep() + baseName + '_z' + strZ + '.tif') then strZNum = i
      strZ = '0' + strZ
   endfor

   strZ = '0'
   while (strLen(strZ) lt strZNum) do strZ = strCompress('0' + strZ, /rem)
   tryName = paintedMaskPath[0] + baseName + path_sep() + baseName + '_z' + strZ + '.tif'
   pWhereCell = ptrArr(1)
   cellNumVect = -1

   mask3D = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   while query_tiff(tryName) do begin

      for i = 0, xyzDim[2]-1 do begin
         strZ = strCompress(string(i), /rem)
         while (strLen(strZ) lt strZNum) do strZ = strCompress('0' + strZ, /rem)
         tryName = paintedMaskPath[0] + baseName + path_sep() + baseName + '_z' + strZ + '.tif'
         if query_tiff(tryName) then begin
            thisImage = read_tiff(tryName)
;            thisImage = congrid(read_tiff(tryName),512,512)
             mask3D[*,*,i] = (thisImage ne thisImage[0])
;            tvscl, thisImage
         endif else mask3D[*,*,i] = 0
      endfor

      mask3D = byte(label_region(mask3D, /all))
      if (max(mask3D) gt 1) then begin
         hist = histogram(mask3D, min = 1)
         whereMax = where(hist eq max(hist)) + 1
      endif else whereMax = 1
      whereCell = where(mask3D eq whereMax[0])
      if (whereCell[0] ne -1) then begin
         if (cellNumVect[0] eq -1) then begin
            pWhereCell = ptr_new(whereCell, /no_copy)
            cellNumVect = nCellNum
         endif else begin
            pWhereCell = [pWhereCell, ptr_new(whereCell, /no_copy)]
            cellNumVect = [cellNumVect, nCellNum]
         endelse
      endif

      nCellNum += 1
      baseName = strMid(paintedMaskPath[1],0, pos_c + 2) + strCompress(string(nCellNum), /rem)
      strZ = '0'
      while (strLen(strZ) lt strZNum) do strZ = strCompress('0' + strZ, /rem)
      tryName = paintedMaskPath[0] + baseName + path_sep() + baseName + '_z' + strZ + '.tif'
   endwhile
   mask3D = 0

      ; get 3D Image
   image3D = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   for i = 0, xyzDim[2]-1 do image3D[*,*,i] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i)

   if (cellNumVect[0] ne -1) then for i = 0L, n_elements(cellNumVect)-1 do begin
      pos = i
      self->add, obj_new('C_sROI3DObject', number = cellNumVect[i],$
                                           tPos = tPos,$
                                           chPos = chPos,$
                                           zPos = zPos,$
                                           xyzFramePixSize = xyzDim,$
                                           xyzFrameRealSize = xyzFrameRealSize,$
                                           pointValues = image3D[*pWhereCell[i]],$
                                           wherePoints = *pWhereCell[i]), position = pos
   endfor

   if (cellNumVect[0] ne -1) then for i = n_elements(cellNumVect)-1, 0, -1 do ptr_free, pWhereCell[i]
   ptr_free, pWhereCell
end

pro C_sROI3DGroupObj::init3DModelsFromNumberedMasks, stack_tlb = stack_tlb, fAcceptAllObjects = fAcceptAllObjects

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZNum
   xyzDim = self->getxyzDim()
   xyzDim[2] = totalZNum

   xyzFrameRealSize = self->getxyzRealSize()

   clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
   tPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]
   chPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]

      ; get imageStackInfoObject
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   imageStackInfoObject->get, pParamStruct = pParamStruct

   mask3D = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)

   for i = 0, xyzDim[2]-1 do begin
      thisImage = imageStackInfoObject->getSelectedClusterMask(tPos = tPos, chPos = chPos, zPos = i, clusPos = clusPos)
      if (thisImage[0] ne -1) then mask3D[*,*,i] = thisImage else mask3D[*,*,i] = 0
   endfor

   cellNumVect = [-1]
   if (max(mask3D) gt 1) then begin
      hist = histogram(mask3D, min = 1)
      whereCellNum = where(hist gt 0, count)
      if (count gt 0) then for i = 0L, count-1 do begin
         whereCell = where(mask3D eq whereCellNum[i]+1)
         if (cellNumVect[0] eq -1) then begin
            pWhereCell = ptr_new(whereCell, /no_copy)
            cellNumVect = whereCellNum[i]+1
         endif else begin
            pWhereCell = [pWhereCell, ptr_new(whereCell, /no_copy)]
            cellNumVect = [cellNumVect, whereCellNum[i]+1]
         endelse
      endfor
   endif
   mask3D = 0

      ; get 3D Image
   image3D = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   for i = 0, xyzDim[2]-1 do image3D[*,*,i] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i)

   if (cellNumVect[0] ne -1) then for i = 0L, n_elements(cellNumVect)-1 do begin
      pos = i
      self->add, obj_new('C_sROI3DObject', number = cellNumVect[i],$
                                           tPos = tPos,$
                                           chPos = chPos,$
                                           zPos = zPos,$
                                           xyzFramePixSize = xyzDim,$
                                           xyzFrameRealSize = xyzFrameRealSize,$
                                           pointValues = image3D[*pWhereCell[i]],$
                                           wherePoints = *pWhereCell[i]), position = pos
   endfor

   if (cellNumVect[0] ne -1) then for i = n_elements(cellNumVect)-1, 0, -1 do ptr_free, pWhereCell[i]
   ptr_free, pWhereCell
end

; C_sROI3DGroupObj::checkAndAddModels
;
; If you miss something like this...
;
; ...
; if (bUsePlane1) then $
;   oObjectModel->add, obj_new('IDLgrPolygon', principalIntersections1, uValue = '3D Plane Cut 1', name = '3D Plane Cut 1', STYLE=2, COLOR=[177,177,177],$
;                                              xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
; if (bUsePlane2) then $
;    oObjectModel->add, obj_new('IDLgrPolygon', principalIntersections2, uValue = '3D Plane Cut 2', name = '3D Plane Cut 2', STYLE=2, COLOR=[77,77,77],$
;                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
; ...
;
; Now there is a method intended to handle these (and possibly other?) similar cases :)
; It is assumed that the models will be added according to the array order.
;
; PARAMETERS:
;     oObjectModel     the root node (model).
;     modelNames       a n-element array containing the name (and 'uValue') of each model to be checked/added. 
;     modelTypes       a n-element array containing the object type of each model to be checked/added (e.g. 'IDLgrPolygon')
;     pModelData       a n-element pointer array for the data corresponding to each model to be checked/added.
;     flags            a byte array indicating if the corresponding model will be generated and added (1b) or not (0b).
;     colors           a '3 x n' array of RGB color values for each model.
;     styles           a n-element bye vector indicating the style property of the graphic model to add.
;     x|y|zCoord_conv  conversion factors for x|y|z.
;
; NOTES
;     Initial version, J Jara (2012), intended for cut-plane models
pro C_sROI3DGroupObj::checkAndAddModels, oObjectModel, modelNames = modelNames, modelTypes = modelTypes, pModelData = pModelData,$
                                                       flags = flags, styles = styles, colors = colors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  nFlags = n_elements(flags)
  for i = 0, nFlags-1 do $
    if (flags[i] eq 1) then begin
      oObjectModel->add, obj_new(string(modelTypes[i]), *pModelData[i], uValue = modelNames[i], name = modelNames[i],$
                                 color = colors[*,i], style = styles[i], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
      print, 'added ', modelNames[i]
    endif
end


; XXX Clean pro... FASL
pro C_sROI3DGroupObj::manageCurrentPhaseModel, includeModelList = includeModelList, stack_tlb = stack_tlb

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
   widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
      fMovieKeepObjectPosition = stateObj.fMovieKeepObjectPosition
      fMovieKeepGalleryPosition = stateObj.fMovieKeepGalleryPosition
      fUpDateROI3DGroup = stateObj.fUpDateROI3DGroup
      fUpDateROI3DGroupProperties = stateObj.fUpDateROI3DGroupProperties
      poCurrROIGraphicModel = stateObj.poCurrROIGraphicModel
   widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy

   if ~obj_valid(*poCurrROIGraphicModel) then *poCurrROIGraphicModel = obj_new('IDLgrModel', uValue = 'oPhaseModel')
   
   widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
      oPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Plane Model Base')
   widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy

;   widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
;      oPlaneModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Plane Model 2Planes')
;   widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy


   ; CREATE AND SET VALUES OF VARIABLES OF CUT PLANE
   planesCut        = -1

   bUsePlane1       = 0b
   centerPlane1     = [0.0,0.0,0.0]
   normalPlane1     = [0.0,0.0,1.0]
   angles1          = [0.0,0.0] ; In general -> normalPlane = Normalize(refPoint - centerPlane)
   dParam1          = -1.0
   planeData1       = -1
   displacement1    = 0.0
   distRender1      = 0.0

   bUsePlane2       = 0b
   centerPlane2     = [0.0,0.0,0.0]
   normalPlane2     = [0.0,1.0,0.0]
   angles2          = [0.0,0.0] ; In general -> normalPlane = Normalize(refPoint - centerPlane)
   dParam2          = -1.0
   planeData2       = -1   
   displacement2    = 0.0
   distRender2      = 0.0
   fColorLaterality = 0b

   xyzDim = self->getxyzDim()
    ; FIrst Object---- Arbitrary Plane Parameters
   whParam = (where(*(*self.pParamStruct).pNames eq '1st Active_Plane'))[0]
   if (whParam ne -1) then begin
      bUsePlane1 = fix(*(*self.pParamStruct).pValues[whParam] < 1)
      *(*self.pParamStruct).pValues[whParam] = bUsePlane1
   endif

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Center_Plane X'))[0]
   if (whParam ne -1) then begin
      centerPlane1[0] = ((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[0]+1)) < (xyzDim[0]-1))
      *(*self.pParamStruct).pValues[whParam] = centerPlane1[0]
   endif

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Center_Plane Y'))[0]
   if (whParam ne -1) then begin
      centerPlane1[1] = ((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[1]+1)) < (xyzDim[1]-1))
      *(*self.pParamStruct).pValues[whParam] = centerPlane1[1]
   endif

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Center_Plane Z'))[0]
   if (whParam ne -1) then begin
      centerPlane1[2] = ((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[2]+1)) < (xyzDim[2]-1))
      *(*self.pParamStruct).pValues[whParam] = centerPlane1[2]
   endif

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Angle_Plane A'))[0]
   if (whParam ne -1) then begin
      angles1[0] = ((*(*self.pParamStruct).pValues[whParam] > (- 360.0)) < (360.0))
      *(*self.pParamStruct).pValues[whParam] = angles1[0]
   endif

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Angle_Plane B'))[0]
   if (whParam ne -1) then begin
      angles1[1] = ((*(*self.pParamStruct).pValues[whParam] > (- 360.0)) < (360.0))
      *(*self.pParamStruct).pValues[whParam] = angles1[1]
   endif

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Displacement_Plane'))[0]
   if (whParam ne -1) then begin
      displacement1 = *(*self.pParamStruct).pValues[whParam]
      *(*self.pParamStruct).pValues[whParam] = displacement1
   endif

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Distance_Render'))[0]
   if (whParam ne -1) then begin
      distRender1 = *(*self.pParamStruct).pValues[whParam]
      *(*self.pParamStruct).pValues[whParam] = distRender1
   endif
      ; get 2nd object and object Params

   ;if obj_valid(oROI3DGroup) then begin
       ; Second Object---- Arbitrary Plane Parameters
       whParam = (where(*(*self.pParamStruct).pNames eq '2st Active_Plane'))[0]
       if (whParam ne -1) then begin
          bUsePlane2 = fix(*(*self.pParamStruct).pValues[whParam] < 1)
          *(*self.pParamStruct).pValues[whParam] = bUsePlane2
       endif

       whParam = (where(*(*self.pParamStruct).pNames eq '2st Center_Plane X'))[0]
       if (whParam ne -1) then begin
          centerPlane2[0] = ((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[0]+1)) < (xyzDim[0]-1))
          *(*self.pParamStruct).pValues[whParam] = centerPlane2[0]
       endif

       whParam = (where(*(*self.pParamStruct).pNames eq '2st Center_Plane Y'))[0]
       if (whParam ne -1) then begin
          centerPlane2[1] = ((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[1]+1)) < (xyzDim[1]-1))
          *(*self.pParamStruct).pValues[whParam] = centerPlane2[1]
       endif

       whParam = (where(*(*self.pParamStruct).pNames eq '2st Center_Plane Z'))[0]
       if (whParam ne -1) then begin
          centerPlane2[2] = ((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[2]+1)) < (xyzDim[2]-1))
          *(*self.pParamStruct).pValues[whParam] = centerPlane2[2]
       endif

       whParam = (where(*(*self.pParamStruct).pNames eq '2st Angle_Plane A'))[0]
       if (whParam ne -1) then begin
          angles2[0] = ((*(*self.pParamStruct).pValues[whParam] > (- 360.0)) < (360.0))
          *(*self.pParamStruct).pValues[whParam] = angles2[0]
       endif

       whParam = (where(*(*self.pParamStruct).pNames eq '2st Angle_Plane B'))[0]
       if (whParam ne -1) then begin
          angles2[1] = ((*(*self.pParamStruct).pValues[whParam] > (- 360.0)) < (360.0))
          *(*self.pParamStruct).pValues[whParam] = angles2[1]
       endif

       whParam = (where(*(*self.pParamStruct).pNames eq '2st Displacement_Plane'))[0]
       if (whParam ne -1) then begin
          displacement2 = *(*self.pParamStruct).pValues[whParam]
          *(*self.pParamStruct).pValues[whParam] = displacement2
       endif

       whParam = (where(*(*self.pParamStruct).pNames eq '2st Distance_Render'))[0]
       if (whParam ne -1) then begin
          distRender2 = *(*self.pParamStruct).pValues[whParam]
          *(*self.pParamStruct).pValues[whParam] = distRender2
       endif 
   ;endif

       whParam = (where(*(*self.pParamStruct).pNames eq 'Colouring by Laterality'))[0]
       if (whParam ne -1) then begin
          fColorLaterality = fix(*(*self.pParamStruct).pValues[whParam] < 1)
          *(*self.pParamStruct).pValues[whParam] = fColorLaterality
       endif

    ; Change to conv coords
    self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzDim = self->getxyzDim()
    xyzSizePerPixel = self->getxyzSizePerPixel()

    ; Parameters of arbitrary planes
    ; If Point = {x,y,z} is over the plane then volData1[x,y,z] = 0
    ; Over is Distance Point-Plane > 0
    ; Plane eq: PointInPlane * NormalPlane + d = 0
    ; Obtain "d" value -> d = - PointInPlane * NormalPlane
    ; if we use a point in plane Plan eq = 0, 
    ; else... if we use a arbitrary point  PointArbitrary * NormalPlane + d = Distance

    dimensionesBox = make_array(3,2)
;    dimensionesBox[0,*] = [xCoord_conv[0],-xCoord_conv[0]]
;    dimensionesBox[1,*] = [yCoord_conv[0],-yCoord_conv[0]]
;    dimensionesBox[2,*] = [zCoord_conv[0]*xyzDim[2],zCoord_conv[1]*xyzDim[2]]
    dimensionesBox[0,*] = [0.0,xyzDim[0]*1.0]
    dimensionesBox[1,*] = [0.0,xyzDim[1]*1.0]
    dimensionesBox[2,*] = [0.0,xyzDim[2]*1.0]

    ; First Normalize NormalPlane
    if (bUsePlane1) then begin
      ; Angle1 is the rotation between axis x and z... need scale for correct rotational effect
      ; Obtain normal using angles1
      angles1 = !dtor* angles1
      normalPlane1[0] = sin(angles1[0])*cos(angles1[1])
      normalPlane1[1] = sin(angles1[0])*sin(angles1[1])
      normalPlane1[2] = cos(angles1[0])

      normalPlane1[0] *= xyzSizePerPixel[0]/xyzSizePerPixel[0] 
      normalPlane1[1] *= xyzSizePerPixel[1]/xyzSizePerPixel[0] 
      normalPlane1[2] *= xyzSizePerPixel[2]/xyzSizePerPixel[0]

      modulo1 = sqrt(total(normalPlane1*normalPlane1))
      normalPlane1 = normalPlane1 / modulo1

      ; Reposition of plane in Normal_Plane_Direction
      centerPlane1 += displacement1 * normalPlane1

      ; Obtain "dParam" for PLane eq for PLane Cut
      dParam1     = - total(normalPlane1 * centerPlane1)

      planeData1  = [normalPlane1[0],normalPlane1[1],normalPlane1[2],dParam1]
      ; Plane To Render
      centerPlane1 += distRender1 * normalPlane1
      ; Calc Points for polygon
;      centerPlane1[0] = dimensionesBox[0,0] + centerPlane1[0]*(dimensionesBox[0,1] - dimensionesBox[0,0])/(xyzDim[0])
;      centerPlane1[1] = dimensionesBox[1,0] + centerPlane1[1]*(dimensionesBox[1,1] - dimensionesBox[1,0])/(xyzDim[1])
;      centerPlane1[2] = dimensionesBox[2,0] + centerPlane1[2]*(dimensionesBox[2,1] - dimensionesBox[2,0])/(xyzDim[2])
;
;      origen1 = [0.0,0.0,0.0]
; 
;      origen1[0] = dimensionesBox[0,0] + origen1[0]*(dimensionesBox[0,1] - dimensionesBox[0,0])/(xyzDim[0])
;      origen1[1] = dimensionesBox[1,0] + origen1[1]*(dimensionesBox[1,1] - dimensionesBox[1,0])/(xyzDim[1])
;      origen1[2] = dimensionesBox[2,0] + origen1[2]*(dimensionesBox[2,1] - dimensionesBox[2,0])/(xyzDim[2])
; 
;      normalPlane1[0] = dimensionesBox[0,0] + normalPlane1[0]*(dimensionesBox[0,1] - dimensionesBox[0,0])/(xyzDim[0])
;      normalPlane1[1] = dimensionesBox[1,0] + normalPlane1[1]*(dimensionesBox[1,1] - dimensionesBox[1,0])/(xyzDim[1])
;      normalPlane1[2] = dimensionesBox[2,0] + normalPlane1[2]*(dimensionesBox[2,1] - dimensionesBox[2,0])/(xyzDim[2])

      ;normalPlane1 = normalPlane1 - origen1
;      modulo1 = sqrt(total(normalPlane1*normalPlane1))
;      normalPlane1 = normalPlane1 / modulo1

      dParam1     = - total(normalPlane1 * centerPlane1)
    endif
    if (bUsePlane2) then begin
      angles2 = !dtor* angles2
      normalPlane2[0] = sin(angles2[0])*cos(angles2[1])
      normalPlane2[1] = sin(angles2[0])*sin(angles2[1])
      normalPlane2[2] = cos(angles2[0])

      normalPlane2[0] *= xyzSizePerPixel[0]/xyzSizePerPixel[0] 
      normalPlane2[1] *= xyzSizePerPixel[1]/xyzSizePerPixel[0] 
      normalPlane2[2] *= xyzSizePerPixel[2]/xyzSizePerPixel[0]

      modulo2 = sqrt(total(normalPlane2*normalPlane2))
      normalPlane2 = normalPlane2 / modulo2
      ; Reposition of plane in Normal_Plane_Direction
      centerPlane2 += displacement2 * normalPlane2

      ; Obtain "dParam" for PLane eq
      dParam2 = - total(normalPlane2 * centerPlane2)
      planeData2 = [normalPlane2[0],normalPlane2[1],normalPlane2[2],dParam2]
      ; Plane To Render
      centerPlane2 += distRender2 * normalPlane2
      ; Calc Points for polygon
;      centerPlane2[0] = xCoord_conv[0] + centerPlane2[0]*(xCoord_conv[1] - xCoord_conv[0])/(xyzDim[0])
;      centerPlane2[1] = yCoord_conv[0] + centerPlane2[1]*(yCoord_conv[1] - yCoord_conv[0])/(xyzDim[1])
;      centerPlane2[2] = zCoord_conv[0] + centerPlane2[2]*(zCoord_conv[1] - zCoord_conv[0])/(xyzDim[2])
      dParam2 = - total(normalPlane2 * centerPlane2)
    endif

   case 1 of
     (bUsePlane1 and bUsePlane2): planesCut = [planeData1,planeData2]
     bUsePlane1: planesCut = planeData1
     bUsePlane2: planesCut = planeData2
     else: planesCut = planesCut
   endcase

    intersections = s_getPointsIntersectionPlanes(dimensionesBox,bUsePlane1,normalPlane1,dParam1,bUsePlane2,oROI3DGroup,normalPlane2,dParam2)
    principalIntersections1 = make_array(3,4)
    principalIntersections2 = make_array(3,4)
    principalIntersections1[*,*] = intersections[0,*,*]
    principalIntersections2[*,*] = intersections[1,*,*]

    principalIntersections1 = s_getOrderPlane(principalIntersections1,4)
    principalIntersections2 = s_getOrderPlane(principalIntersections2,4)

;    principalIntersections1[0,*] = [xCoord_conv[0],xCoord_conv[0],-xCoord_conv[0],-xCoord_conv[0]]
;    principalIntersections1[1,*] = [yCoord_conv[0],-yCoord_conv[0],-yCoord_conv[0],yCoord_conv[0]]
;    principalIntersections1[2,*] = [xyzDim[2]*zCoord_conv[1],xyzDim[2]*zCoord_conv[1],xyzDim[2]*zCoord_conv[1],xyzDim[2]*zCoord_conv[1]]

   cutPlanesNames  = ['3D Plane Cut 1', '3D Plane Cut 2']
   cutPlanesStyles = [2               , 2]
   cutPlanesColors = [[177,177,177]   , [77,77,77]]
   cutPlanesFlags  = [bUsePlane1      , bUsePlane2]
   cutPlanesTypes  = make_array(n_elements(cutPlanesNames), /string, value = 'IDLgrPolygon')
   pCutPlanesData   = ptrArr(n_elements(cutPlanesNames))
     pCutPlanesData[0] = ptr_new(principalIntersections1)
     pCutPlanesData[1] = ptr_new(principalIntersections2)

   self->getProperty, uvalue = sortVect
   if (n_elements(sortVect) eq 0) then begin
      self->initObjectSortVect
      self->getProperty, uvalue = sortVect
   endif
   sortVect.fMovieKeepGalleryPosition = fMovieKeepGalleryPosition
   self->setProperty, uvalue = sortVect

       ; check state of CurrentROIPhaseModel
    for i = ((*poCurrROIGraphicModel)->count()-1), 0, -1 do begin
       oObjectModel = (*poCurrROIGraphicModel)->get(position = i)
       if (obj_valid(oObjectModel) and (obj_class(oObjectModel) ne 'IDLEXMODELMANIP')) then begin
         oObjectModel->getProperty, uValue = modelName
         whModel = (where(modelName eq includeModelList))[0]
          ;cleanup, remove and desytroy oObjectModel
         if (whModel eq -1) then begin
          case modelName of
              '3D Point Model': begin
                    oObjectModel->remove, /all
                    if ~fMovieKeepObjectPosition then begin
                       (*poCurrROIGraphicModel)->remove, oObjectModel
                       obj_destroy, oObjectModel
                    endif
                 endcase
              else: begin
                   if (strPos(modelName,'3D') ne -1) then begin
                    oSubModels = oObjectModel->get(/all)
                    oObjectModel->remove, /all
                    if obj_valid(oSubModels[0]) then for j = 0L, n_elements(oSubModels)-1 do obj_destroy, oSubModels[j]
                    if ~fMovieKeepObjectPosition then begin
                       (*poCurrROIGraphicModel)->remove, oObjectModel
                       obj_destroy, oObjectModel
                    endif
                   endif
              endcase
          endcase
         endif
       endif
    endfor

    if (n_elements(includeModelList) ne 0) then if (includeModelList[0] ne '') then begin
       for i = 0L, n_elements(includeModelList)-1 do begin
         case includeModelList[i] of
          '3D Point Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Point Model', name = '3D Point Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                    for i = 0L, self->count()-1 do (self->get(position = i))->setProperty, style = 0b
                    oObjectModel->add, self
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else $
                            (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif else begin
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else $
                            (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endelse
              endcase
          '3D Point Model Gallery': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Point Model Gallery', name = '3D Point Model Gallery')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   for j = 0L, self->count()-1 do begin
                       (self->get(position = j))->getProperty, all = prop, data = xyzPoints
                       oObjectModel->add, obj_new('IDLgrROI', xyzPoints, style = 0b,$
                                                                         name = prop.name,$
                                                                         color = prop.color,$
                                                                         thick = prop.thick,$
                                                                         double = prop.double,$
                                                                         xCoord_conv = prop.xCoord_conv,$
                                                                         yCoord_conv = prop.yCoord_conv,$
                                                                         zCoord_conv = prop.zCoord_conv,$
                                                                         CLIP_PLANES = planesCut $
                                                                         )
                    endfor
                 endif
              endcase
          '3D Border Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Border Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Border Model', name = '3D Border Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoBorderModel, oObjectModel
                   for modelIndex = 0L, oObjectModel->count()-1 do begin
                        valor = -1
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                   endfor
                 endif
              endcase
          '3D Plane Model Base': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Plane Model Base')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Plane Model Base', name = '3D Plane Model Base')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPlaneModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPlaneModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
           '3D Plane Model 2Planes': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Plane Model 2Planes')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Plane Model 2Planes', name = '3D Plane Model 2Planes')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                    s_getoPlaneParModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPlaneParModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
           '3D Plane Model Orthogonal': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Plane Model Orthogonal')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Plane Model Orthogonal', name = '3D Plane Model Orthogonal')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPlaneOrthogonalModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPlaneOrthogonalModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
          '3D Plane Model Complementary': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Plane Model Complementary')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Plane Model Complementary', name = '3D Plane Model Complementary')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPlaneComplementaryModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPlaneComplementaryModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
       ; UnDrifted Planes Section--
           '3D UnDrifted TorsionSphere Ref 0': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted TorsionSphere Ref 0')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted TorsionSphere Ref 0', name = '3D UnDrifted TorsionSphere Ref 0')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTorsionSphere_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTorsionSphere_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if fUpDateROI3DGroupProperties then $
                   s_getoTorsionSphere_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase



          '3D UnDrifted Plane Model Base': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Plane Model Base')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted Plane Model Base', name = '3D UnDrifted Plane Model Base')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoUnDriftedPlaneModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoUnDriftedPlaneModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
           '3D UnDrifted Plane Model 2Planes': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Plane Model 2Planes')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted Plane Model 2Planes', name = '3D UnDrifted Plane Model 2Planes')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                    s_getoUnDriftedPlaneParModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoUnDriftedPlaneParModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
           '3D UnDrifted Plane Model Orthogonal': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Plane Model Orthogonal')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted Plane Model Orthogonal', name = '3D UnDrifted Plane Model Orthogonal')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoUnDriftedPlaneOrthogonalModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoUnDriftedPlaneOrthogonalModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
          '3D UnDrifted Plane Model Complementary': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Plane Model Complementary')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted Plane Model Complementary', name = '3D UnDrifted Plane Model Complementary')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoUnDriftedPlaneComplementaryModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoUnDriftedPlaneComplementaryModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
       
          '3D Surface AC UnDrifted Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface AC UnDrifted Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface AC UnDrifted Model', name = '3D Surface AC UnDrifted Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                  s_getoACSurfaceUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoACSurfaceUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                       if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                       if (valor eq '3D Plane Cut 2') then begin
                          if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                       endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
              endcase

       
       ; END___ UnDrifted PLanes section
       '3D Track Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Track Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Track Model', name = '3D Track Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
           '3D Tracking ImageJ Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Tracking ImageJ Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Tracking ImageJ Model', name = '3D Tracking ImageJ Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoTrackingImageJModel, oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoTrackingImageJModel, oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
       '3D Track By Deformation Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Track By Deformation Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Track By Deformation Model', name = '3D Track By Deformation Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackByDeformationModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackByDeformationModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
       '3D Track UnDrifted By Deformation Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Track UnDrifted By Deformation Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Track UnDrifted By Deformation Model', name = '3D Track UnDrifted By Deformation Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackUnDriftedByDeformationModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackUnDriftedByDeformationModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase

       '3D Track UnDrifted Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Track UnDrifted Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Track UnDrifted Model', name = '3D Track UnDrifted Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                     s_getoTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
       '3D Track Smooth UnDrifted Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Track Smooth UnDrifted Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Track Smooth UnDrifted Model', name = '3D Track Smooth UnDrifted Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSmoothTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                     s_getoSmoothTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
       '3D Track Group UnDrifted Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Track Group UnDrifted Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Track Group UnDrifted Model', name = '3D Track Group UnDrifted Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoGroupTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                     s_getoGroupTrackUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                      if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
           endcase
           
           '3D UnDrifted Surface Center Distance I': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance I')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted Surface Center Distance I', name = '3D UnDrifted Surface Center Distance I')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfCentDistI, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfCentDistI, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                 endif
                 if fUpDateROI3DGroupProperties then $
                   s_getoSurfCentDistI, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase

           '3D UnDrifted Surface Center Distance I Ref 0': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance I Ref 0')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted Surface Center Distance I Ref 0', name = '3D UnDrifted Surface Center Distance I Ref 0')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfCentDistI_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfCentDistI_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                 endif
                 if fUpDateROI3DGroupProperties then $
                   s_getoSurfCentDistI_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase

           '3D UnDrifted Surface Center Distance II': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance II')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted Surface Center Distance II', name = '3D UnDrifted Surface Center Distance II')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfCentDistII, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfCentDistII, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if fUpDateROI3DGroupProperties then begin
                   s_getoSurfCentDistII, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                    ;self->updateModel, oObjectModel
                 endif
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase

           '3D UnDrifted Surface Center Distance II Ref 0': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance II Ref 0')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D UnDrifted Surface Center Distance II Ref 0', name = '3D UnDrifted Surface Center Distance II Ref 0')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfCentDistII_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfCentDistII_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if fUpDateROI3DGroupProperties then begin
                   s_getoSurfCentDistII_RefOrigCenter, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase

          '3D Frame Model': begin
             oName = '3D Frame Model'
             oObjectModel = *poCurrROIGraphicModel->getByName(oName)
             if ~obj_valid(oObjectModel) then begin
                oObjectModel = obj_new('IDLgrModel', uValue = oName, name = oName)
                *poCurrROIGraphicModel->add, oObjectModel
             endif

             if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                oObjectModel->remove, /all
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                 self->getoFrameModel, oObjectModel
              endif

              if (oObjectModel->count() eq 0) then begin
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                self->getoFrameModel, oObjectModel 
              endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

              for modelIndex = 0, oObjectModel->count()-1 do begin
                (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                if (valor eq '3D Plane Cut 1') then begin
                  if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                endif else begin
                  if (valor eq '3D Plane Cut 2') then begin
                    if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                  endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                endelse
              endfor
            endcase


          '3D Surface Compute-Mesh Model': begin
             oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Compute-Mesh Model')
             if ~obj_valid(oObjectModel) then begin
                oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Compute-Mesh Model', name = '3D Surface Compute-Mesh Model')
                *poCurrROIGraphicModel->add, oObjectModel
             endif

             if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                oObjectModel->remove, /all
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                 self->getoComputeMeshModel, oObjectModel
             endif

             if (oObjectModel->count() eq 0) then begin
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                self->getoComputeMeshModel, oObjectModel
             endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

             for modelIndex = 0, oObjectModel->count()-1 do begin
                (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                if (valor eq '3D Plane Cut 1') then begin
                  if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                endif else begin
                  if (valor eq '3D Plane Cut 2') then begin
                    if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                  endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                endelse
             endfor
           endcase

          '3D Surface Mesh Model': begin
             oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
             if ~obj_valid(oObjectModel) then begin
                oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Mesh Model', name = '3D Surface Mesh Model')
                *poCurrROIGraphicModel->add, oObjectModel
             endif

             if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                oObjectModel->remove, /all
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                 self->getoMeshModel, oObjectModel
              endif

              if (oObjectModel->count() eq 0) then begin
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                self->getoMeshModel, oObjectModel 
              endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

              for modelIndex = 0, oObjectModel->count()-1 do begin
                (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                if (valor eq '3D Plane Cut 1') then begin
                  if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                endif else begin
                  if (valor eq '3D Plane Cut 2') then begin
                    if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                  endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                endelse
              endfor
            endcase

          '3D Surface Mesh-Fix Model': begin
             oModelName = '3D Surface Mesh-Fix Model'
             oObjectModel = *poCurrROIGraphicModel->getByName(oModelName)
             if ~obj_valid(oObjectModel) then begin
                oObjectModel = obj_new('IDLgrModel', uValue = oModelName, name = oModelName)
                *poCurrROIGraphicModel->add, oObjectModel
             endif

             if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                oObjectModel->remove, /all
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                 self->getoMeshFixModel, oObjectModel, stack_tlb = stack_tlb
              endif

              if (oObjectModel->count() eq 0) then begin
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                self->getoMeshFixModel, oObjectModel, stack_tlb = stack_tlb
              endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

              for modelIndex = 0, oObjectModel->count()-1 do begin
                (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                if (valor eq '3D Plane Cut 1') then begin
                  if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                endif else begin
                  if (valor eq '3D Plane Cut 2') then begin
                    if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                  endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                endelse
              endfor
            endcase

          '3D Surface Mesh Model OvrC': begin
             oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model OvrC')
             if ~obj_valid(oObjectModel) then begin
                oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Mesh Model OvrC', name = '3D Surface Mesh Model OvrC')
                *poCurrROIGraphicModel->add, oObjectModel
             endif

             if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                oObjectModel->remove, /all
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                 self->getoMeshModelOvrC, oObjectModel
              endif

              if (oObjectModel->count() eq 0) then begin
                self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                       flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                self->getoMeshModelOvrC, oObjectModel 
              endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

              for modelIndex = 0, oObjectModel->count()-1 do begin
                (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                if (valor eq '3D Plane Cut 1') then begin
                  if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                endif else begin
                  if (valor eq '3D Plane Cut 2') then begin
                    if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                  endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                endelse
              endfor
            endcase

            '3D Load OBJ_OFF Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Load OBJ_OFF Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Load OBJ_OFF Model', name = '3D Load OBJ_OFF Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                    s_getoObjOrOffMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoObjOrOffMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self 
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase

            '3D Skeleton From Mesh Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Skeleton From Mesh Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Skeleton From Mesh Model', name = '3D Skeleton From Mesh Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSkeletonFromMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, o3DroiGroup = self, fSaveSkeletonInSavFile = 0b, fUseSavedSkeletons = 1b
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSkeletonFromMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, o3DroiGroup = self, fSaveSkeletonInSavFile = 0b, fUseSavedSkeletons = 1b
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
            '3D Graph Skeleton Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Graph Skeleton Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Graph Skeleton Model', name = '3D Graph Skeleton Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoReducedSkeletonFromSavedSkel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, fAddGraphicModel = 1
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoReducedSkeletonFromSavedSkel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, fAddGraphicModel = 1
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase

          '3D SkelTree From Mesh Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D SkelTree From Mesh Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D SkelTree From Mesh Model', name = '3D SkelTree From Mesh Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                    s_getoSkelTreeFromMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSkelTreeFromMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel 
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                 endfor
              endcase
          
          '3D Skeleton From Mesh File - Test': begin
              print, 'Start Skeleton'
              oObjectModel = *poCurrROIGraphicModel->getByName('3D Skeleton From Mesh File - Test')
              if ~(obj_valid(oObjectModel)) then begin
                oObjectModel = obj_new('IDLgrModel', uValue = '3D Skeleton From Mesh File - Test', name = '3D Skeleton From Mesh File - Test')
                *poCurrROIGraphicModel->add, oObjectModel
              endif
               s_getoSkeletonFromMeshRepair, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self
              print, 'End Skeleton'
              endcase
; First attemp to integrate modelling modell... for now... inpendient windows will be used for rendering... 
; in the future.. maybe #D manipulation and interaction integration will be considered
      '3D Modelling CPM_DFCs Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Modelling CPM_DFCs Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel',uValue = '3D Modelling CPM_DFCs Model', name = '3D Modelling CPM_DFCs Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoCPM_DFCs, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoCPM_DFCs, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase     

              
          '3D Surface Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Model', name = '3D Surface Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfaceModel, oObjectModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfaceModel, oObjectModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                       if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
              endcase
 
           '3D Surface UnDrifted Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface UnDrifted Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface UnDrifted Model', name = '3D Surface UnDrifted Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfaceUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoSurfaceUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                       if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                      if (valor eq '3D Plane Cut 2') then begin
                        if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                      endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
              endcase       
      '3D Tracking Estimated Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Tracking Estimated Model')
                 creado=0b
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel',uValue = '3D Tracking Estimated Model', name = '3D Tracking Estimated Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackingEstimatedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                   creado = 1b
                 endif
                 if (oObjectModel->count() eq 0 and creado eq 0b) then begin
                    ;TODO Tracking se calcula 2 veces porque no genera un objetos en el modelo
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoTrackingEstimatedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase     
              
         '3D Tracking Estimator Trajectory Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Tracking Estimator Trajectory Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel',uValue = '3D Tracking Estimator Trajectory Model', name = '3D Tracking Estimator Trajectory Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoTrackingEstimatorTrajectoryModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoTrackingEstimatorTrajectoryModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 
                 if fUpDateROI3DGroupProperties then $
                    self->updateModel, oObjectModel
                 
                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                      (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                      if (valor eq '3D Plane Cut 1') then begin
                        if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                      endif else begin
                        if (valor eq '3D Plane Cut 2') then begin
                          if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                      endelse
                 endfor
              endcase 
                 
         '3D Optical Flow Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Optical Flow Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel',uValue = '3D Optical Flow Model', name = '3D Optical Flow Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getOpticalFlowModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getOpticalFlowModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                      (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                      if (valor eq '3D Plane Cut 1') then begin
                        if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                      endif else begin
                        if (valor eq '3D Plane Cut 2') then begin
                          if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                      endelse
                 endfor
              endcase    
                 
          '3D Surface AC Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface AC Model', name = '3D Surface AC Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                  self->getoACModel, oObjectModel, stack_tlb = stack_tlb
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoACModel, oObjectModel, stack_tlb = stack_tlb
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                       if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                       if (valor eq '3D Plane Cut 2') then begin
                          if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                       endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
              endcase

          '3D Surface AC Model With OF': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model With OF')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface AC Model With OF', name = '3D Surface AC Model With OF')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoACOFsurfacePathModel, oObjectModel, stack_tlb = stack_tlb
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoACOFsurfacePathModel, oObjectModel, stack_tlb = stack_tlb
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                       if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                       if (valor eq '3D Plane Cut 2') then begin
                          if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                       endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
              endcase

          '3D Surface Path Tracking Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Path Tracking Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Path Tracking Model', name = '3D Surface Path Tracking Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfacePathTrackingModel, oObjectModel, stack_tlb = stack_tlb
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfacePathTrackingModel, oObjectModel, stack_tlb = stack_tlb
                 endif else if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel

                 for modelIndex = 0L, oObjectModel->count()-1 do begin
                    (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                    if (valor eq '3D Plane Cut 1') then begin
                       if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                    endif else begin
                       if (valor eq '3D Plane Cut 2') then begin
                          if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                       endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                    endelse
                 endfor
              endcase

           '3D Surface Intensity Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Intensity Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Intensity Model', name = '3D Surface Intensity Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                    oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfIntModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfIntModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                  endif
;                 if fUpDateROI3DGroupProperties then self->getoSurfIntModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                           if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
           '3D Surface Intensity Model (Binary)': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Intensity Model (Binary)')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel',uValue = '3D Surface Intensity Model (Binary)', name = '3D Surface Intensity Model (Binary)')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoBinSurfIntModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoBinSurfIntModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                  endif
;                 if fUpDateROI3DGroupProperties then self->getoBinSurfIntModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
           '3D Surface Curvature Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Curvature Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Curvature Model', name = '3D Surface Curvature Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfCurvModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                    self->getoSurfCurvModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
;                 if fUpDateROI3DGroupProperties then self->getoSurfCurvModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
           '3D PA Box Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D PA Box Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D PA Box Model', name = '3D PA Box Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoPABoxModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoPABoxModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if fUpDateROI3DGroupProperties then $
                    self->updateModel, oObjectModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
           '3D Surface Distance Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Distance Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Distance Model', name = '3D Surface Distance Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfDistanceModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                    self->getoSurfDistanceModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                  endif
;                 if fUpDateROI3DGroupProperties then self->getoSurfDistanceModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
           '3D Surface Center Distance I': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Center Distance I')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Center Distance I', name = '3D Surface Center Distance I')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfCentDistI, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfCentDistI, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if fUpDateROI3DGroupProperties then $
                    self->getoSurfCentDistI, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
           '3D Surface Center Distance II': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Center Distance II')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Surface Center Distance II', name = '3D Surface Center Distance II')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfCentDistII, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSurfCentDistII, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if fUpDateROI3DGroupProperties then begin
                    self->getoSurfCentDistII, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties
                    ;self->updateModel, oObjectModel
                 endif
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
          '3D Sphere Projection' : begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Sphere Projection')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Sphere Projection', name = '3D Sphere Projection')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSphereProjectionModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoSphereProjectionModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel 
                 endif else begin
                    if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel
                 endelse
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
          '3D Principal Axis (Obj)': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal Axis (Obj)')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Principal Axis (Obj)', name = '3D Principal Axis (Obj)')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoPC, oObjectModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoPC, oObjectModel 
                   oObjectModel->SetProperty, CLIP_PLANES = planesCut
                 endif else begin
                    if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel
                 endelse
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
           '3D Principal Axis (Group)': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal Axis (Group)')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Principal Axis (Group)', name = '3D Principal Axis (Group)')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   self->getoPCGroup, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase
          '3D Principal UnDrifted Axis (Obj)': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal UnDrifted Axis (Obj)')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Principal UnDrifted Axis (Obj)', name = '3D Principal UnDrifted Axis (Obj)')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPCUnDrifted, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif

                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPCUnDrifted, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel 
                   oObjectModel->SetProperty, CLIP_PLANES = planesCut
                 endif else begin
                    if fUpDateROI3DGroupProperties then self->updateModel, oObjectModel
                 endelse
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
           '3D Principal UnDrifted Axis (Group)': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal UnDrifted Axis (Group)')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Principal UnDrifted Axis (Group)', name = '3D Principal UnDrifted Axis (Group)')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPCUnDriftedGroup, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase
           '3D Principal UnDrifted Axis (Estimated Group)': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal UnDrifted Axis (Estimated Group)')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Principal UnDrifted Axis (Estimated Group)', name = '3D Principal UnDrifted Axis (Estimated Group)')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoPCUnDriftedGroupEstimated, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase              
           '3D Fill Volume Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Fill Volume Model')
                 if ~obj_valid(oObjectModel) then begin
                     oObjectModel = obj_new('IDLgrModel', uValue = '3D Fill Volume Model', name = '3D Fill Volume Model')
                     *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                    oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                    s_getoVolumeWColorModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel, /fROIInt

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase
           '3D ROI-Intensity Model by Object': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D ROI-Intensity Model by Object')
                 if ~obj_valid(oObjectModel) then begin
                     oObjectModel = obj_new('IDLgrModel', uValue = '3D ROI-Intensity Model by Object', name = '3D ROI-Intensity Model by Object')
                     *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                    s_getoROIIMWColorModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel, /fROIInt

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase

              ; XXX this cases uses getoVOlume
           '3D ROI-Intensity Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D ROI-Intensity Model')
                 if ~obj_valid(oObjectModel) then begin
                     oObjectModel = obj_new('IDLgrModel', uValue = '3D ROI-Intensity Model', name = '3D ROI-Intensity Model')
                     *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                   self->getoVolumeModel, oObjectModel, stack_tlb = stack_tlb, /fROIInt
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty,CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase
          '3D Intensity Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Intensity Model')
                 if ~obj_valid(oObjectModel) then begin
                     oObjectModel = obj_new('IDLgrModel', uValue = '3D Intensity Model', name = '3D Intensity Model')
                     *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                   self->getoVolumeModel, oObjectModel, stack_tlb = stack_tlb
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase
              ; FASL--- adds... :(
          '3D Intensity UnDrifted Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Intensity UnDrifted Model')
                 if ~obj_valid(oObjectModel) then begin
                     oObjectModel = obj_new('IDLgrModel', uValue = '3D Intensity UnDrifted Model', name = '3D Intensity UnDrifted Model')
                     *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                   s_getoIntensityUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase

           '3D Arbritary Axis AMA Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Arbritary Axis AMA Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Arbritary Axis AMA Model', name = '3D Arbritary Axis AMA Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                   s_getoAAAMA, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase
              
          '3D Balls Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Balls Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel',uValue = '3D Balls Model', name = '3D Balls Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                   s_getoBallsModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoBallsModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if fUpDateROI3DGroupProperties then $
                    self->updateModel, oObjectModel
                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
          '3D Balls Min-Max Model': begin
                 oObjectModel = *poCurrROIGraphicModel->getByName('3D Balls Min-Max Model')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel',uValue = '3D Balls Min-Max Model', name = '3D Balls Min-Max Model')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif

                 if (fUpDateROI3DGroup and ~fUpDateROI3DGroupProperties) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                   s_getoBallsMinMaxModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if (oObjectModel->count() eq 0) then begin
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
                   s_getoBallsMinMaxModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel
                 endif
                 if fUpDateROI3DGroupProperties then $
                    self->updateModel, oObjectModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
              endcase
          '3D Cells Division': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Cells Division')
                 if ~obj_valid(oObjectModel) then begin
                    oObjectModel = obj_new('IDLgrModel', uValue = '3D Cells Division', name = '3D Cells Division')
                    *poCurrROIGraphicModel->add, oObjectModel
                 endif
                 if ~(oObjectModel->isContained(self)) then begin
                   oObjectModel->remove, /all
                   self->checkAndAddModels, oObjectModel, modelNames = cutPlanesNames, modelTypes = cutPlanesTypes, pModelData = pCutPlanesData,$
                                                          flags = cutPlanesFlags, styles = cutPlanesStyles, colors = cutPlanesColors,$
                                                          xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

                   s_getoADAAAMA, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = self, poCurrROIGraphicModel = poCurrROIGraphicModel

                     for modelIndex = 0L, oObjectModel->count()-1 do begin
                        (oObjectModel->get(position = modelIndex))->getProperty, name = valor
                        if (valor eq '3D Plane Cut 1') then begin
                          if (bUsePlane1) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections1
                        endif else begin
                          if (valor eq '3D Plane Cut 2') then begin
                            if (bUsePlane2) then (oObjectModel->get(position = modelIndex))->setProperty, DATA = principalIntersections2
                          endif else (oObjectModel->get(position = modelIndex))->SetProperty, CLIP_PLANES = planesCut
                        endelse
                     endfor
                 endif
              endcase
          '3D Project Model Gallery': begin
                 for j = 0L, n_elements(includeModelList)-1 do begin
                    case includeModelList[j] of
                       '3D Surface Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Surface UnDrifted Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface UnDrifted Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase                       
                       '3D Surface Mesh Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Load OBJ_OFF Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Load OBJ_OFF Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Skeleton From Mesh Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Skeleton From Mesh Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Graph Skeleton Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Graph Skeleton Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D SkeleTree From Mesh Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D SkelTree From Mesh Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Skeleton From Mesh File - Test': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Skeleton From Mesh File - Test')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Surface AC Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Surface Curvature Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Curvature Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D PA Box Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D PA Box Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Arbritary Axis AMA Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Arbritary Axis AMA Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Modelling CPM_DFCs Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Modelling CPM_DFCs Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Tracking Estimated Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Tracking Estimated Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Tracking Estimator Trajectory Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Tracking Estimator Trajectory Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Optical Flow Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Optical Flow Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Balls Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Balls Model')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Cells Division': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Cells Division')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                    
                       '3D Surface Center Distance I': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Center Distance I')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D UnDrifted Surface Center Distance I': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance I')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Surface Center Distance II': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Center Distance II')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D UnDrifted Surface Center Distance II': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance II')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D UnDrifted Surface Center Distance I Ref 0': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance I Ref 0')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D UnDrifted TorsionSphere Ref 0': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted TorsionSphere Ref 0')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D UnDrifted Surface Center Distance II Ref 0': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance II Ref 0')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase

                       '3D Principal Axis (Obj)': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal Axis (Obj)')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Principal Axis (Group)': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal Axis (Group)')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Principal UnDrifted Axis (Obj)': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal UnDrifted Axis (Obj)')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Principal UnDrifted Axis (Group)': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal UnDrifted Axis (Group)')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Point Model Gallery': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Track Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Track By Deformation Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Track UnDrifted Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Track Smooth UnDrifted Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Track Group UnDrifted Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase
                       '3D Track UnDrifted By Deformation Model': begin
                          oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                          if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcGalleryPosition'
                       endcase               

                       else:
                    endcase
                 endfor
              endcase
              else:
         endcase
       endfor
       dummy = where(includeModelList eq '3D Project Model Gallery', count)

       case count of
       0: begin
          for j = 0L, n_elements(includeModelList)-1 do begin
            case includeModelList[j] of
               '3D Surface Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Surface UnDrifted Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface UnDrifted Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase               
               '3D Surface Mesh Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Load OBJ_OFF Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Load OBJ_OFF Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Skeleton From Mesh Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Skeleton From Mesh Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Graph Skeleton Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Graph Skeleton Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D SkelTree From Mesh Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D SkelTree From Mesh Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Skeleton From Mesh File - Test': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Skeleton From Mesh File - Test')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Surface AC Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Surface Curvature Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Curvature Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
                   endcase
               '3D PA Box Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D PA Box Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Arbritary Axis AMA Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Arbritary Axis AMA Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               
               '3D Modelling CPM_DFCs Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Modelling CPM_DFCs Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase 
               '3D Tracking Estimated Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Tracking Estimated Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase 
               '3D Tracking Estimator Trajectory Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Tracking Estimator Trajectory Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase 
               '3D Optical Flow Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Optical Flow Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase 
               '3D Balls Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Balls Model')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Cells Division': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Cells Division')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Surface Center Distance I': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Center Distance I')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D UnDrifted Surface Center Distance I': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance I')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Surface Center Distance II': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Surface Center Distance II')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D UnDrifted Surface Center Distance II': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance II')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D UnDrifted Surface Center Distance I Ref 0': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance I Ref 0')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D UnDrifted TorsionSphere Ref 0': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted TorsionSphere Ref 0')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D UnDrifted Surface Center Distance II Ref 0': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D UnDrifted Surface Center Distance II Ref 0')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase

               '3D Principal Axis (Obj)': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal Axis (Obj)')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Principal Axis (Group)': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal Axis (Group)')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Principal UnDrifted Axis (Obj)': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal UnDrifted Axis (Obj)')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Principal UnDrifted Axis (Group)': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Principal UnDrifted Axis (Group)')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Point Model Gallery': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase

               '3D Track Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Track By Deformation Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Track UnDrifted Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Track Group UnDrifted Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Track Smooth UnDrifted Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase
               '3D Track UnDrifted By Deformation Model': begin
                  oObjectModel = *poCurrROIGraphicModel->getByName('3D Point Model Gallery')
                  if obj_valid(oObjectModel) then self->calcObjPos, oObjectModel, sDirection = 'calcOriginalPosition'
               endcase               

               else:
            endcase
          endfor
       endcase
       else:
       endcase
   endif
   self->getProperty, uvalue = sortVect
   if sortVect.fUpdate then *sortVect.pOld = *sortVect.pNew
   sortVect.fUpdate = 0b
   self->setProperty, uvalue = sortVect

   widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
      stateObj.fUpDateROI3DGroupProperties = 0b
   widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy
end

pro C_sROI3DGroupObj::getoVolumeModel, oObjectModel, stack_tlb = stack_tlb, fROIInt = fROIInt

   if (n_elements(fROIInt) gt 0) then fROIInt = 1b else fROIInt = 0b
   oObjectModel->getProperty, name = modelName
   modelName1 = strCompress(modelName + '1', /rem)
   modelName2 = strCompress(modelName + '2', /rem)

   xyzDim = self->getxyzDim()

      ; get 1st  object and object parameters
   volData1 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   if (xyzDim[2] eq 1) then volData1 = reform(volData1, xyzDim[0], xyzDim[1], xyzDim[2])
   if ~fROIInt then begin
      s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos
      widget_control, stack_tlb, get_uValue = stackState, /no_copy
         imageStackInfoObject = *stackState.pImageStackInfoObject
      widget_control, stack_tlb, set_uValue = stackState, /no_copy
      for z = 0, xyzDim[2]-1 do begin
         image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
         szI = size(image, /dim)
         if ((szI[0] eq xyzDim[0]) and (szI[1] eq xyzDim[1])) then volData1[*,*,z] = image; bytScl(image, min = 0, top = 255)
      endFor
   endif else for i = 0L, (self->count()-1) do volData1[*((self->get(position = i))->getpWherePoints())] = *((self->get(position = i))->getpPointValues())

   rgb_table0 = bytArr(256,3) + transpose((*self.pvolState).rgbValues[0,*,*])
   whereNo = where(volData1 lt ((*self.pVolState).bottomValues[0]))
   if (whereNo[0] ne -1) then volData1[whereNo] = 0
   whereNo = where(volData1 gt ((*self.pVolState).topValues[0]))
   if (whereNo[0] ne -1) then volData1[whereNo] = 0

   case (*self.pVolState).opacFlag[0] of
      0: begin
         whParam = (where(*(*self.pParamStruct).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*self.pParamStruct).pValues[whParam] > 0) < 255
            *(*self.pParamStruct).pValues[whParam] = opacVal
            opacVect_0 = make_array(256, /byte, value = opacVal)
            opacVect_0[0 : ((*self.pVolState).bottomValues[0])] = 0
            if (((*self.pVolState).topValues[0]) le 254) then opacVect_0[((*self.pVolState).topValues[0])+1 : 255] = 0
         endif
      endcase
      else: opacVect_0 = bytArr(256) + (*self.pVolState).opacValues[0,*]
   endcase

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Cutting Plane x'))[0]
   if (whParam ne -1) then begin
      x1 = fix((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[0]+1)) < (xyzDim[0]-1))
      *(*self.pParamStruct).pValues[whParam] = x1
      if (x1 le 0) then factorX1 = 1 else factorX1 = -1
   endif else x1 = 0

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Cutting Plane y'))[0]
   if (whParam ne -1) then begin
      y1 = fix((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[1]+1)) < (xyzDim[1]-1))
      *(*self.pParamStruct).pValues[whParam] = y1
      if (y1 le 0) then factorY1 = 1 else factorY1 = -1
   endif else y1 = 0

   whParam = (where(*(*self.pParamStruct).pNames eq '1st Cutting Plane z'))[0]
   if (whParam ne -1) then begin
      z1 = fix((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[2]+1)) < (xyzDim[2]-1))
      *(*self.pParamStruct).pValues[whParam] = z1
      if (z1 le 0) then factorZ1 = 1 else factorZ1 = -1
   endif else z1 = 0

      ; get 2nd object and object Params
   whereName = (where(*(*self.pParamStruct).pNames eq '2nd Volume Object'))[0]
   if (whereName ne -1) then begin
      strSelect = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq '2nd Volume Object'))[0]]
      case 1 of
      ((strPos(strSelect, 'Channel') ne -1) or (strPos(strSelect, 'channel') ne -1)): begin
         strNum = s_getRightNumberFromString(strSelect)
         clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
         tPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]
         oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = strNum, clusPos = clusPos, fileName = fileName)
      endcase

      ((strPos(strSelect, 'Time') ne -1) or (strPos(strSelect, 'time') ne -1)): begin
         strNum = s_getRightNumberFromString(strSelect)
         clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
         chPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]
         oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = strNum, chPos = chPos, clusPos = clusPos, fileName = fileName)
      endcase

      ((strPos(strSelect, 'Cluster') ne -1) or (strPos(strSelect, 'cluster') ne -1)): begin
         strNum = s_getRightNumberFromString(strSelect)
         clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
         tPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]
         chPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]
         chPos = 1
         oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, clusPos = strNum, fileName = fileName)
      endcase
      else:
      endcase
   endif

   if obj_valid(oROI3DGroup) then begin
      volData2 = bytArr(xyzDim[0], xyzDim[1], xyzDim[2])
      for i = 0L, (oROI3DGroup->count()-1) do volData2[*((oROI3DGroup->get(position = i))->getpWherePoints())] = *((oROI3DGroup->get(position = i))->getpPointValues())
      rgb_table1 = (bytArr(256,3) + transpose((*self.pvolState).rgbValues[1,*,*]))

      whereNo = where(volData2 lt ((*self.pVolState).bottomValues[1]))
      if (whereNo[0] ne -1) then volData2[whereNo] = 0
      whereNo = where(volData2 gt ((*self.pVolState).topValues[1]))
      if (whereNo[0] ne -1) then volData2[whereNo] = 0

      case (*self.pVolState).opacFlag[1] of
      0: begin
         whParam = (where(*(*self.pParamStruct).pNames eq '2nd Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*self.pParamStruct).pValues[whParam] > 0) < 255
            *(*self.pParamStruct).pValues[whParam] = opacVal
            opacVect_1 = make_array(256, /byte, value = opacVal)
            opacVect_1[0 : ((*self.pVolState).bottomValues[1])] = 0
            if (((*self.pVolState).topValues[1]) le 254) then opacVect_1[((*self.pVolState).topValues[1])+1 : 255] = 0
         endif
      endcase
      else: opacVect_1 = bytArr(256) + (*self.pVolState).opacValues[1,*]
      endcase

      whParam = (where(*(*self.pParamStruct).pNames eq '2nd Cutting Plane x'))[0]
      if (whParam ne -1) then begin
         x2 = fix((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[0]+1)) < (xyzDim[0]-1))
         *(*self.pParamStruct).pValues[whParam] = x2
         if (x2 le 0) then factorX2 = 1 else factorX2 = -1
      endif else x2 = 0

      whParam = (where(*(*self.pParamStruct).pNames eq '2nd Cutting Plane y'))[0]
      if (whParam ne -1) then begin
         y2 = fix((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[1]+1)) < (xyzDim[1]-1))
         *(*self.pParamStruct).pValues[whParam] = y2
         if (y2 le 0) then factorY2 = 1 else factorY2 = -1
      endif else y2 = 0

      whParam = (where(*(*self.pParamStruct).pNames eq '2nd Cutting Plane z'))[0]
      if (whParam ne -1) then begin
         z2 = fix((*(*self.pParamStruct).pValues[whParam] > (-xyzDim[2]+1)) < (xyzDim[2]-1))
         *(*self.pParamStruct).pValues[whParam] = z2
         if (z2 le 0) then factorZ2 = 1 else factorZ2 = -1
      endif else z2 = 0
    endif

   self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   case (*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Merge Volumes'))[0]]) of
   'off': begin
         
      oObjectModel->add, obj_new('IDLgrVolume', data0 = volData1,$
                                                opacity_table0 = opacVect_0,$
                                                rgb_table0 = rgb_table0,$
                                                /interpolate, uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                cutting_plane = [[factorX1,0,0, x1], [0,factorY1,0, y1], [0,0,factorZ1, z1]],$
                                                ambient = [255, 255, 255])

      if obj_valid(oROI3DGroup) then $
         oObjectModel->add, obj_new('IDLgrVolume', data0 = volData2,$
                                                   opacity_table0 = opacVect_1,$
                                                   rgb_table0 = rgb_table1,$
                                                   /interpolate, uValue = modelName2, /zero,$
                                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                   cutting_plane = [[factorX2,0,0, x2], [0,factorY2,0, y2], [0,0,factorZ2, z2]],$
                                                   ambient = [255, 255, 255],lighting_model = 1)
   endcase
             
   'on': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0
      
      volData_0 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,0,*]))[volData1]
      volData_1 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,1,*]))[volData1]
      volData_2 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,2,*]))[volData1]

      case ((*self.pVolState).opacFlag[0]) of
      0: begin
         whParam = (where(*(*self.pParamStruct).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*self.pParamStruct).pValues[whParam] > 0) < 255
            *(*self.pParamStruct).pValues[whParam] = opacVal
            volData_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
            whereI = where(volData1 le (*self.pVolState).bottomValues[0])
            if (whereI[0] ne -1) then volData_3[whereI] = 0
            if (((*self.pVolState).topValues[0]) le 254) then begin
               whereI = where(volData1 ge (*self.pVolState).topValues[0])
               if (whereI[0] ne -1) then volData_3[whereI] = 0
            endif
         endif
      endcase

      else: volData_3 = (bytArr(256) + transpose((*self.pvolState).opacValues[0,*]))[volData1]

      endcase

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData2[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData2[x2:*,*,*] = 0
         if (y2 lt 0) then volData2[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData2[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData_0 = byte((fix(temporary(volData_0) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,0,*]))[volData2])) < 255)
         volData_1 = byte((fix(temporary(volData_1) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,1,*]))[volData2])) < 255)
         volData_2 = byte((fix(temporary(volData_2) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,2,*]))[volData2])) < 255)

         case (*self.pVolState).opacFlag[1] of

         0: begin
            whParam = (where(*(*self.pParamStruct).pNames eq '2nd Volume Opacity'))[0]
            if (whParam ne -1) then begin
                opacVal = (*(*self.pParamStruct).pValues[whParam] > 0) < 255
                *(*self.pParamStruct).pValues[whParam] = opacVal
                volData2_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
                whereI = where(volData2 le (*self.pVolState).bottomValues[1])
                if (whereI[0] ne -1) then volData2_3[whereI] = 0
                if (((*self.pVolState).topValues[1]) le 254) then begin
                   whereI = where(volData2 ge (*self.pVolState).topValues[1])
                   if (whereI[0] ne -1) then volData2_3[whereI] = 0
                endif
             endif
          endcase

          else: volData2_3 = (bytArr(256) + transpose((*self.pvolState).opacValues[1,*]))[volData2]

          endcase

         volData_3 >= volData2_3
      endif

      oObjectModel->add, obj_new('IDLgrVolume', volume_select = 2,$
                                                data0 = volData_0,$
                                                data1 = volData_1,$
                                                data2 = volData_2,$
                                                data3 = volData_3,$
                                                /interpolate, uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                ambient = [255, 255, 255], lighting_model = 1)
   endcase

   'on_1': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0

      volData_0 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,0,*]))[volData1]
      volData_1 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,1,*]))[volData1]
      volData_2 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,2,*]))[volData1]

      case ((*self.pVolState).opacFlag[0]) of
      0: begin
         whParam = (where(*(*self.pParamStruct).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*self.pParamStruct).pValues[whParam] > 0) < 255
            *(*self.pParamStruct).pValues[whParam] = opacVal
            volData_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
            whereI = where(volData1 le (*self.pVolState).bottomValues[0])
            if (whereI[0] ne -1) then volData_3[whereI] = 0
            if (((*self.pVolState).topValues[0]) le 254) then begin
               whereI = where(volData1 ge (*self.pVolState).topValues[0])
               if (whereI[0] ne -1) then volData_3[whereI] = 0
            endif
         endif
      endcase

      else: volData_3 = (bytArr(256) + transpose((*self.pvolState).opacValues[0,*]))[volData1]
      endcase

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData1[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData1[x2:*,*,*] = 0
         if (y2 lt 0) then volData1[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData1[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData_0 = (fix(temporary(volData_0) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,0,*]))[volData2])) < 255
         volData_1 = (fix(temporary(volData_1) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,1,*]))[volData2])) < 255
         volData_2 = (fix(temporary(volData_2) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,2,*]))[volData2])) < 255

         case ((*self.pVolState).opacFlag[1]) of
         0: begin
            whParam = (where(*(*self.pParamStruct).pNames eq '2nd Volume Opacity'))[0]
            if (whParam ne -1) then begin
               opacVal = (*(*self.pParamStruct).pValues[whParam] > 0) < 255
               *(*self.pParamStruct).pValues[whParam] = opacVal
               volData2_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
               whereI = where(volData2 le (*self.pVolState).bottomValues[1])
               if (whereI[0] ne -1) then volData2_3[whereI] = 0
               if (((*self.pVolState).topValues[1]) le 254) then begin
                  whereI = where(volData2 ge (*self.pVolState).topValues[1])
                  if (whereI[0] ne -1) then volData2_3[whereI] = 0
               endif
            endif
         endcase

         else: volData2_3 = (bytArr(256) + transpose((*self.pvolState).opacValues[1,*]))[volData2]
         endcase

         rgb_table0 >= rgb_table1
      endif

      oObjectModel->add, obj_new('IDLgrVolume', volume_select = 2,$
                                                data0 = volData_0,$
                                                data1 = volData_1,$
                                                data2 = volData_2,$
                                                data3 = volData_3,$
                                                opacity_table0 = make_array(256, /byte, /index),$
                                                rgb_table0 = rgb_table0,$
                                                /interpolate, uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                ambient = [255, 255, 255], lighting_model = 1)
   endcase

   'on_2': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0

      volData_0 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,0,*]))[volData1]
      volData_1 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,1,*]))[volData1]
      volData_2 = (bytArr(256) + transpose((*self.pvolState).rgbValues[0,2,*]))[volData1]

      case (*self.pVolState).opacFlag[0] of
      0: begin
         whParam = (where(*(*self.pParamStruct).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*self.pParamStruct).pValues[whParam] > 0) < 255
            *(*self.pParamStruct).pValues[whParam] = opacVal
            volData_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
            whereI = where(volData1 le (*self.pVolState).bottomValues[0])
            if (whereI[0] ne -1) then volData_3[whereI] = 0
            if (((*self.pVolState).topValues[0]) le 254) then begin
               whereI = where(volData1 ge (*self.pVolState).topValues[0])
               if (whereI[0] ne -1) then volData_3[whereI] = 0
            endif
         endif
      endcase

      else: volData_3 = (bytArr(256) + transpose((*self.pvolState).opacValues[0,*]))[volData1]
      endcase

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData1[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData1[x2:*,*,*] = 0
         if (y2 lt 0) then volData1[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData1[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData_0 = (fix(temporary(volData_0) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,0,*]))[volData2])) < 255
         volData_1 = (fix(temporary(volData_1) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,1,*]))[volData2])) < 255
         volData_2 = (fix(temporary(volData_2) + (bytArr(256) + transpose((*self.pvolState).rgbValues[1,2,*]))[volData2])) < 255

         case (*self.pVolState).opacFlag[1] of

         0: begin
            whParam = (where(*(*self.pParamStruct).pNames eq '2nd Volume Opacity'))[0]
            if (whParam ne -1) then begin
               opacVal = (*(*self.pParamStruct).pValues[whParam] > 0) < 255
               *(*self.pParamStruct).pValues[whParam] = opacVal
               volData2_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
               whereI = where(volData2 le (*self.pVolState).bottomValues[1])
               if (whereI[0] ne -1) then volData2_3[whereI] = 0
               if (((*self.pVolState).topValues[1]) le 254) then begin
                  whereI = where(volData2 ge (*self.pVolState).topValues[1])
                  if (whereI[0] ne -1) then volData2_3[whereI] = 0
               endif
            endif
         endcase

         else: volData2_3 = (bytArr(256) + transpose((*self.pvolState).opacValues[1,*]))[volData2]
         endcase

         volData_3 = volData2_3
         rgb_table0 >= rgb_table1
      endif

      oObjectModel->add, obj_new('IDLgrVolume', volume_select = 2,$
                                                data0 = volData_0,$
                                                data1 = volData_1,$
                                                data2 = volData_2,$
                                                data3 = volData_3,$
                                                opacity_table0 = make_array(256, /byte, /index),$
                                                rgb_table0 = rgb_table0,$
                                                /interpolate, uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                cutting_plane = [[factorX2,0,0, x2], [0,factorY2,0, y2], [0,0,factorZ2, z2]],$
                                                ambient = [255, 255, 255], lighting_model = 1)
   endcase

   'one_in_two': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0

      volData1 /= 2
      rgb_table0 = byte(congrid(rgb_table0, 128, 3))
      opacVect_0 = byte(congrid(opacVect_0, 128))

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData2[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData2[x2:*,*,*] = 0
         if (y2 lt 0) then volData2[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData2[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData2 = temporary(volData2) / 2 + 128b
         rgb_table0 = [rgb_table0, byte(congrid(rgb_table1, 128, 3))]
         opacVect_0 = [opacVect_0, byte(congrid(opacVect_1, 128))]

         whereObj = where(volData1 ne 0)
         if (whereObj[0] ne -1) then volData2[whereObj] = volData1[whereObj]
         oObjectModel->add, obj_new('IDLgrVolume', data0 = volData2,$
                                                   opacity_table0 = opacVect_0,$
                                                   rgb_table0 = rgb_table0,$
                                                   uValue = modelName1, /zero,$
                                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                   ambient = [255, 255, 255], lighting_model = 1)
      endif else begin
         rgb_table0 = [rgb_table0, bytArr(128,3)]
         opacVect_0 = [opacVect_0, bytArr(128)]
         oObjectModel->add, obj_new('IDLgrVolume', data0 = volData1,$
                                                   opacity_table0 = opacVect_0,$
                                                   rgb_table0 = rgb_table0,$
                                                   uValue = modelName1, /zero,$
                                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                   ambient = [255, 255, 255], lighting_model = 1)
      endelse
   endcase

   'two_in_one': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0

      volData1 /= 2
      rgb_table0 = byte(congrid(rgb_table0, 128, 3))
      opacVect_0 = byte(congrid(opacVect_0, 128))

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData2[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData2[x2:*,*,*] = 0
         if (y2 lt 0) then volData2[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData2[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData2 /= 2
         rgb_table0 = [rgb_table0, byte(congrid(rgb_table1, 128, 3))]
         opacVect_0 = [opacVect_0, byte(congrid(opacVect_1, 128))]

         whereObj = where(volData2 gt 0)
         if (whereObj[0] ne -1) then volData1[whereObj] = (volData2[whereObj] + 128b)

      endif else begin
         rgb_table0 = [rgb_table0, bytArr(128,3)]
         opacVect_0 = [opacVect_0, bytArr(128)]
      endelse

      oObjectModel->add, obj_new('IDLgrVolume', data0 = volData1,$
                                                opacity_table0 = opacVect_0,$
                                                rgb_table0 = rgb_table0,$
                                                uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
   endcase
   else:
   endcase
end

pro C_sROI3DGroupObj::getoBorderModel, oObjectModel
  if ~keyWord_set(color) then color = [0,255,0]

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]
  if (whParam ne -1) then $
     color = [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]],$
              *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_g'))[0]],$
              *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_b'))[0]]] > [0,0,0] < [255,255,255]

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border Style'))[0]
  if (whParam ne -1) then begin
     modelStyle = (*(*self.pParamStruct).pValues[whParam] > 0) < 2
     *(*self.pParamStruct).pValues[whParam] = modelStyle
  endif

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]
  if (whParam ne -1) then begin
     borderThick = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] > 1. < 10.
                   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] = borderThick
   endif else borderThick = 1.

   whParam = (where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]
   if (whParam ne -1) then begin
      borderLineStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] > 0 < 6
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] = borderLineStyle
   endif else borderLineStyle = 0
   for i = 0L, (self->count())-1 do (self->get(position = i))->makeZSliceObjectBorderPolygon, color = color, oModel = oObjectModel, modelStyle = modelStyle,borderThick = borderThick, borderLineStyle = borderLineStyle
end

function  C_sROI3DGroupObj::getOpacVect, objName
   if (n_elements(objName) eq 0) then objName = '1st Volume Opacity'
   if (objName ne '2nd Volume Opacity') then objName = '1st Volume Opacity'
   case (*self.pVolState).opacFlag[0] of
      0: begin
         whParam = (where(*(*self.pParamStruct).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = ((*(*self.pParamStruct).pValues[whParam] > 0) < 255)
            *(*self.pParamStruct).pValues[whParam] = opacVal
            opacVect = make_array(256, /byte, value = opacVal)
            if (((*self.pVolState).bottomValues[0]) gt 0) then opacVect[0: ((*self.pVolState).bottomValues[0])] = 0
            if (((*self.pVolState).topValues[0]) le 254) then opacVect[((*self.pVolState).topValues[0])+1 : 255] = 0
         endif
      endcase
      else: opacVect = bytArr(256) + (*self.pVolState).opacValues[0,*]
   endcase
   return, opacVect
end

pro C_sROI3DGroupObj::updateModel, oObjectModel
   selfNumberVector = self->getObjectNumberVector()
   for i = (oObjectModel->count())-1, 0, -1 do begin
      oObj = oObjectModel->get(position = i)
      oObj->getProperty, name = objName
      case objName of
      '3DOrbModel': begin
;         (self->get(position = objPos))->getProperty, color = color, alpha_channel = alpha
;         oObj->setProperty, alpha_channel = alpha, bottom = color, color = color
      endcase
      else: begin
         objNum = s_getRightNumberFromString(objName)
         objPos = where(selfNumberVector eq objNum, count)
         case count of
         0: begin
               oObjectModel->remove, position = i
               obj_destroy, oObj
         endcase
         else: begin
               (self->get(position = objPos))->getProperty, color = color, alpha_channel = alpha
               ;FASL REFERENCE ... This proc... still the original values of color for planes based models...
               vkeepColor = 0b
               vkeepColor = vkeepColor or (objName eq strcompress('3DPlaneModel_Base:' + string(objNum), /rem))
               vkeepColor = vkeepColor or (objName eq strcompress('3DPlaneModel_Orthogonal:' + string(objNum), /rem))
               vkeepColor = vkeepColor or (objName eq strcompress('3DPlaneModel_Complementary:' + string(objNum), /rem))
               vkeepColor = vkeepColor or (objName eq strcompress('3DPlaneModel_Base:' + string(objNum), /rem))
               vkeepColor = vkeepColor or (objName eq strcompress('3DPlaneModel_Orthogonal:' + string(objNum), /rem))
               vkeepColor = vkeepColor or (objName eq strcompress('3DPlaneModel_Complementary:' + string(objNum), /rem))

               vChangeColor = 0b 
               vChangeColor = vChangeColor or (objName eq strcompress('3DTrackingEstimatorTrajectoryModel:' + string(objNum),/rem))
               ;vChangeColor = vChangeColor or (objName eq strcompress('3DOpticalFlowModel:' + string(objNum),/rem))

               case 1 of
                 obj_isa(oObj, 'IDLgrPolygon'): if (vkeepColor eq 0b) then $
                         oObj->setProperty, alpha_channel = alpha, bottom = color, color = color
                 obj_isa(oObj, 'IDLgrPolyline'): if (vChangeColor eq 1b) then $
                         oObj->setProperty, alpha_channel = alpha, color = color
               else:
               endcase
         endcase
         endcase
      endcase
      endcase
   endfor
end


; Makes a 3D wireframe box model with the size of the 3D ROI group.
pro C_sROI3DGroupObj::getoFrameModel, oObjectModel

  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  verts = [[0,0,0], [1,0,0], [1,1,0], [0,1,0], [0,0,0], [0,0,1], [1,0,1], [1,1,1], [0,1,1], [0,0,1], [1,0,1], [1,0,0], [1,0,1], [1,1,1], [1,1,0], [1,1,1], [0,1,1], [0,1,0]]
  verts[0,*] *= *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
  verts[1,*] *= *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
  verts[2,*] *= *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]]
  frameColor = [255, 255, 255]
  oObjectModel->add, obj_new('IDLgrPolyline', verts, color = frameColor, linestyle = 0, thick = 1.0, $
                             xcoord_conv = xcoord_conv, ycoord_conv = ycoord_conv, zcoord_conv = zcoord_conv)
end


; Repair and ensure triangles-only meshes
pro C_sROI3DGroupObj::getoMeshFixModel, oObjectModel, stack_tlb = stack_tlb

  ;numVerticesFactor    = 3.0
  fMaxNumVertices      = 100000 ; 0 indicates no resizing (num. of vertices)
  fSmoothingIterations = 10     ; 0 indicates no smoothing
  fRoiJoinDistance     = 10     ; 0 indicates no join/re-join of possibly disconnected parts of the mesh

  fSaveMeshes = 1b
  
  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos
  
  ; TODO save these names please... they are looking for a common home
  modelName = '_MeshFix_'
  nObj = self->count()
  if (nObj lt 1) then return

  self->restoreSavedSurfaceModels, stack_tlb = stack_tlb, modelName = modelName, pVertices = pVertices, pPolygons = pPolygons, fFile = fFile
  if (total(fFile) eq nObj) then begin
    for i = 0L, nObj-1 do begin
     (self->get(position = i))->getProperty, color = color, alpha_channel = alpha
     objNumber = (self->get(position = i))->getNumber()
     oObjectModel->add, obj_new('IDLgrPolygon', data = *pVertices[i], poly = *pPolygons[i],$
                                                ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                name = strCompress('3DMeshFixModel:' + string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
    endfor
    return 
  endif

  if (fSaveMeshes eq 1) then begin
    widget_control, stateObj_tlb, get_uValue = state, /no_copy
      currROI3DGroupFileName = state.currROI3DGroupFileName
    widget_control, stateObj_tlb, set_uValue = state, /no_copy

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
      (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = stackState, /no_copy

    slash = path_sep()
    posLastSlash = strPos(currROI3DGroupFileName, slash, /reverse_search)
    currROI3DGroupFileName = strMid(currROI3DGroupFileName, strPos(currROI3DGroupFileName, slash, /reverse_search) + 1)
    currROI3DGroupFileName = strMid(currROI3DGroupFileName, 0, strPos(currROI3DGroupFileName, '.sav'))
    modelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]] + strcompress(modelName + slash, /rem))
    if (file_test(strCompress(modelPath),/DIRECTORY) eq 0b) then file_mkDir, strCompress(modelPath)
  endif

  for i = 0L, nObj-1 do begin
     obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
     shade_volume, obj.obj, 0, vertices_, polygons_, /low

     vertices_[0,*] += (obj.minX - obj.pixelFrame)
     vertices_[1,*] += (obj.minY - obj.pixelFrame)
     vertices_[2,*] += (obj.minZ - obj.pixelFrame)

     roiMeshRepairedFileName = strCompress(modelPath + strcompress(currROI3DGroupFileName + modelName + string(objNumber)))
     repairSurfaceMesh, vertices_, polygons_, repairedVertices    = vertices,$
                                              repairedPolygons    = polygons,$ 
                                              smoothingIterations = fSmoothingIterations,$
                                              maxNumVertices      = fMaxNumVertices,$;n_elements(vertices_[0,*]) * numVerticesFactor,$;fMaxNumVertices,$
                                              roiJoinDistance     = fRoiJoinDistance,$
                                              fileName            = roiMeshRepairedFileName
     objNumber = (self->get(position = i))->getNumber()

     if (fSaveMeshes eq 1) then begin
       roiMeshFileName = strCompress(modelPath + strcompress(currROI3DGroupFileName + modelName + string(objNumber) + '.sav', /rem))
       save, vertices, polygons, fileName = roiMeshFileName  
     endif

     (self->get(position = i))->getProperty, color = color, alpha_channel = alpha
     oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = polygons,$
                                                ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                name = strCompress('3DMeshFixModel:' + string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
  endfor
end


pro C_sROI3DGroupObj::getoComputeMeshModel, oObjectModel, stack_tlb = stack_tlb

   nObj = self->count()
   if (nObj lt 1) then return

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzFramePixSize = [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]]]
   ; no need to get these, actually ;)
   xyzSizePerPixel = [1, 1, 1]
   pixelsPerSlice = xyzFramePixSize[0] * xyzFramePixSize[1]

   for i = 0L, nObj-1 do begin
      oRoiGroup = obj_new('IDLgrROIGroup')
      points = *((self->get(position = i))->getpWherePoints())
      ; get the 3d indices for the points list... somewhat inefficient but not so slow :)
      arrPoints = array_indices(xyzFramePixSize, points, /DIMENSIONS)
      curZstart = 0
      zCoords = uniq(arrPoints[2,*,*])
      if (n_elements(zCoords) eq 1) then zCoords = [zCoords]
      for j = 0, n_elements(zCoords)-1 do begin
        curZend = zCoords[j]
        pointsFor2Droi = arrPoints[*, curZstart:curZend, *]
        pointsFor2Droi = points[curZstart : curZend]
        ;zPos = floor(pointsFor2Droi[0] / pixelsPerSlice) ; or...
        zPos = (arrPoints[*,curZstart,*])[2]
        pointsFor2Droi -= pixelsPerSlice * zPos
      ; temporal object for the getting 2D-ROI-polygon arrays
        oRoiObj = obj_new('C_sROIObject', number = i, framePixSize = xyzFramePixSize, xyzSizePerPixel = xyzSizePerPixel,$
                                          pointValues = 1, wherePoints = pointsFor2Droi)
        pBorderPolygon = oRoiObj->getpObjectBorderPolygonList()
        obj_destroy, oRoiObj

; old stuff... backup
;        i2D = bytArr(xyzFramePixSize[0], xyzFramePixSize[1])
;        if (n_elements(num) eq 1) then begin
;          x = [pointsFor2Droi3D[0] - 0.5, pointsFor2Droi3D[0] + 0.5, pointsFor2Droi3D[0] + 0.5, pointsFor2Droi3D[0] - 0.5]
;          y = [pointsFor2Droi3D[1] - 0.5, pointsFor2Droi3D[1] - 0.5, pointsFor2Droi3D[1] + 0.5, pointsFor2Droi3D[1] + 0.5]
;        endif else begin
;          pointsFor2Droi2D = [extrac(pointsFor2Droi3D, 0, 0, 1, num[1]-1),$
;                              extrac(pointsFor2Droi3D, 1, 0, 1, num[1]-1)]
;          i2D[pointsFor2Droi2D] = 1
;          oRoiObj = obj_new('C_sROIObject', number = i, framePixSize = xyzFramePixSize, xyzSizePerPixel = xyzSizePerPixel,$
;                                            pointValues = 255, wherePoints = pointsFor2Droi3D)
;          ccode = oRoiObj->makeObjectBorderChainCode(voxelObject = i2D)
;          ppoly = oRoiObj->makePolygonFromChainCodeVector(ccode)
;          obj_destroy, oRoiObj
;          x = transpose((*pBorderPolygon[0])[0,*])
;          y = transpose((*pBorderPolygon[0])[1,*])
;        endelse
;        print, 'x', transpose((*pBorderPolygon[0])[0,*])
;        print, 'y', transpose((*pBorderPolygon[0])[0,*])
;        print, 'z', (arrPoints[*,curZindex,*])[2]
        oRoiGroup->add, obj_new('IDLgrROI', transpose((*pBorderPolygon[0])[0,*]),$
                                            transpose((*pBorderPolygon[0])[1,*]),$
                                            replicate(zPos,n_elements((*pBorderPolygon[0])[0,*])))
        curZstart = curZend + 1
      endfor
      numTriangles = oRoiGroup->computeMesh(vertices, polygons, capped = 3, surface_area = surfArea)
      print, 'computeMesh for ROI', i, '... triangles : ', numTriangles, ', surfaceArea : ', surfArea
      for j = oRoiGroup->count()-1, 0, -1 do obj_destroy, oRoiGroup->get(position = j)
      oRoiGroup->remove, /all
      obj_destroy, oRoiGroup

      if (numTriangles ne 0) then begin
        oPoly = obj_new('IDLgrPolygon', data = vertices, poly = polygons,$
                                        bottom = [255, 0, 0], color = [255,0, 0], shading = 1,$
                                        xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                        name = strCompress('3DComputeMeshModel:' + string(i), /rem), uvalue = 'ObjInOriginalPosition')
        oObjectModel->add, oPoly
      endif
   endfor
end


; An old unused alternative version to getoMeshModel. Keep for future reference :)
pro C_sROI3DGroupObj::getoQhullMeshModel, oObjectModel, stack_tlb = stack_tlb

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   nObj = self->count()
   for i = 0L, nObj-1 do begin
      (self->get(position = i))->getProperty, data = xyzPoints, color = color, alpha_channel = alpha
      stop
      qhull, xyzPoints, tri, bounds = bounds, vvertices = vertices, connectivity = list, /delaunay
      xyzPoints = xyzPoints[*,bounds]
      qhull, xyzPoints, tri, bounds = bounds, vvertices = vertices, connectivity = list, /delaunay
      stop
      objNumber = (self->get(position = i))->getNumber()
      oObjectModel->add, obj_new('IDLgrROI', style = 0, data = xyzPoints, thick = 2, color = [255,255,0],$
                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DMeshModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')

      ;XOBJVIEW, oObjectModel, renderer = 1

      for k = 0L, (size(tri, /dim))[1]-1 do begin
         oObjectModel->add, obj_new('IDLgrPolygon', data = [xyzPoints[0, tri[[0,1,2,3,0], k]], xyzPoints[1, tri[[0,1,2,3,0], k]], xyzPoints[2, tri[[0,1,2,3,0], k]]],$
                                  bottom = color, color = [0,255,0], alpha_channel = alpha, shininess = 128., shading = 1,$
                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DMeshModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
      endfor
;      oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = polygons, bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
;                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DMeshModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   endfor
end


pro C_sROI3DGroupObj::getoMeshModel, oObjectModel, stack_tlb = stack_tlb

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   nObj = self->count()
   for i = 0L, nObj-1 do begin
      obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
      shade_volume, obj.obj, 0, vertices, polygons, /low

      vertices[0,*] += (obj.minX - obj.pixelFrame)
      vertices[1,*] += (obj.minY - obj.pixelFrame)
      vertices[2,*] += (obj.minZ - obj.pixelFrame)
      print, n_elements(vertices)/3, ' vertices - ', n_elements(polygons)/4, ' edges'

;      oObjectModel->add, obj_new('IDLgrPolygon', ambient = [0,0,0], data = vertices, poly = polygons, shading = 1,$
;                                  vert_colors = transpose([[rgb_table0],[opacVect*255.]]),$
;                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DSurfaceModel:'+string(i),/rem), uvalue = 'ObjInOriginalPosition')

      (self->get(position = i))->getProperty, color = color, alpha_channel = alpha
      objNumber = (self->get(position = i))->getNumber()
      oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = polygons,$
                                                 ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                 xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                 name = strCompress('3DMeshModel:' + string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   endfor
end

; TODO test method for overlap corrected 3D ROIs
pro C_sROI3DGroupObj::getoMeshModelOvrC, oObjectModel
  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  nObj = self->count()
  if (nObj lt 1) then return

  ; if there are more than two objects, check for overlaps and...
  ; optional: flag for compute and use of axis aligned bounding boxes
  pBP = ptrArr(nObj)
  pBPoly = ptrArr(nObj)
  pZcoords = ptrArr(nObj)
  for i = 0L, nObj-1 do begin
    obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
    shade_volume, obj.obj, 0, vertices, polygons, /low
    vertices[0,*] += (obj.minX - obj.pixelFrame)
    vertices[1,*] += (obj.minY - obj.pixelFrame)
    vertices[2,*] += (obj.minZ - obj.pixelFrame)
    ;zSlicesCoords = (vertices[2,*])[uniq(vertices[2,*], sort(vertices[2,*]))] 
    ;pzCoords[i] = ptr_new(zSlicesCoords, /no_copy)
    pBP[i] = ptr_new(vertices, /no_copy)
    pBPoly[i] = ptr_new(polygons, /no_copy)
  endfor

  ; Compute bounding boxes in xyz for each object, in order to avoid comparisons between "coarsely" distant objects
  fUseBBoxes = nObj gt 1

  if (fUseBBoxes eq 1) then begin
    xyzBoxes = fltArr(6, nObj) ; [xmin, ymin, zmin, xmax, ymax, zmax] for each object
    for i = 0L, nObj-1 do begin
      roiI = (self->get(position = i))
      roiI->getProperty, ROI_XRANGE = xr, ROI_YRANGE = yr, ROI_ZRANGE = zr
      xyzBoxes[0, i] = xr[0] - 1.0 ; min and max for x coords... surface models cover 1 voxel more for each
      xyzBoxes[3, i] = xr[1] + 1.0
      xyzBoxes[1, i] = yr[0] - 1.0 ; min and max for y coords... surface models cover 1 voxel more for each
      xyzBoxes[4, i] = yr[1] + 1.0
      xyzBoxes[2, i] = zr[0] - 1.0 ; min and max for z coords... surface models cover 1 voxel more for each
      xyzBoxes[5, i] = zr[1] + 1.0
    endfor
  endif

  ; Now check and "correct" overlaps
  if (nObj gt 1) then $
  for i = 0L, nObj-2 do begin
    nPtsI = (size(*pBP[i], /dim))[1]
    for j = i+1, nObj-1 do begin
      fInXYZ = 1
      ; Check with bounding boxes to discard some exhaustive distance checkings
      fInX = ((xyzBoxes[3, j] ge xyzBoxes[0, i]) and (xyzBoxes[0, j] le xyzBoxes[0, i])) $
          or ((xyzBoxes[0, j] le xyzBoxes[3, i]) and (xyzBoxes[3, j] ge xyzBoxes[0, i]))
      fInY = ((xyzBoxes[4, j] ge xyzBoxes[1, i]) and (xyzBoxes[1, j] le xyzBoxes[1, i])) $
          or ((xyzBoxes[1, j] le xyzBoxes[4, i]) and (xyzBoxes[4, j] ge xyzBoxes[1, i]))
      fInZ = ((xyzBoxes[5, j] ge xyzBoxes[2, i]) and (xyzBoxes[2, j] le xyzBoxes[2, i])) $
          or ((xyzBoxes[2, j] le xyzBoxes[5, i]) and (xyzBoxes[5, j] ge xyzBoxes[2, i]))
      fInXYZ = fInX and fInY and fInZ
      if (fInXYZ eq 1) then begin
        print, 'cheking points of possibly overlapping ROIs ', i, '-', j
        numReps = 0
        repsLimit = 3
        threshDist = 0.2
        ; costly and slow iterative loop for testing overlap against the point model and the surface model
chkOvr: points = (self->get(position = j))->containsPoints(*pBP[i])
        ; comparison returns 0 for points in the exterior, 1 in the interior and 2 in the edge
        pointsIinJ = where(points ne 0, pointCount)
        ; check also for distance between surface models
        points2 = [-1]
        np2 = 0
        for jj = 0L, n_elements((*pBP[j])[0,*])-1 do begin
          curDist = sqrt(((*pBP[i])[0,*] - (*pBP[j])[0,jj])^2 + ((*pBP[i])[1,*] - (*pBP[j])[1,jj])^2 + ((*pBP[i])[2,*] - (*pBP[j])[2,jj])^2)
          ; search for points in ROI j whose distance to point ii in ROI i is <= thresDist
          whNear = where(curDist le threshDist, countNear)
          if (countNear gt 0) then begin
            points2 = [points2, whNear]
            points2 = points2[uniq(points2, sort(points2))]
          endif
        end
        if (n_elements(points2) gt 1) then begin
          points2 = points2[1:*]
          np2 = n_elements(points2)
        endif
        ; combine overlapped points with nearby points
        if (np2 gt 0) then begin
          pointsIinJ = pointCount eq 0 ? points2 : [pointsIinJ, points2]
          pointsIinJ = pointsIinJ[uniq(pointsIinJ, sort(pointsIinJ))]
          pointCount = n_elements(pointsIinJ)
        endif
        ; ROI I is neighbour of ROI J, either for overlap with the point model or for proximity between surface models
        if (pointCount gt 0) then begin            
          normals = compute_mesh_normals(*pBP[i], *pBPoly[i])
          ; for each point found as intersecting object J, "retract" it along its normal direction
          print, 'going to correct ', pointCount, ' points'
          for p = 0L, pointCount-1 do begin
            currentPointI = pointsIinJ[p]
            ; displace vertices along their normal direction
            scaleFactor = 0.5
            (*pBP[i])[0, currentPointI] -= normals[0, currentPointI] * scaleFactor
            (*pBP[i])[1, currentPointI] -= normals[1, currentPointI] * scaleFactor
            (*pBP[i])[2, currentPointI] -= normals[2, currentPointI] * scaleFactor
          endfor
          normals = [-1, -1, -1]
          numReps += 1
          ; after correcting the model, re-check overlapping using a nice GOTO statement
          if (numReps lt repsLimit) then goto, chkOvr
        endif else print, 'done with ', numReps, ' iterations of correction'
        ; TODO ...it remains to be seen if this works for typical cases (until the arrival of AABM ;)
      endif
    endfor
  endfor
  ; free memory of the bboxes by assigning a scalar value to the variable
  if (fUseBBoxes eq 1) then xyzBoxes = -1

  ; Finally add the corrected models
  for i = 0L, nObj-1 do begin
    (self->get(position = i))->getProperty, color = color, alpha_channel = alpha
    objNumber = (self->get(position = i))->getNumber()
    oObjectModel->add, obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = *pBP[i], poly = *pBPoly[i], bottom = color, color = color, shading = 1, shininess = 128.,$
                                               xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DMeshModelOvrC:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
  endfor
  print, 'getoSurfaceMovelOvrC, done'
end


pro C_sROI3DGroupObj::getoSurfaceModel, oObjectModel

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   nObj = self->count()
   for i = 0L, nObj-1 do begin
      obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
      shade_volume, obj.obj, 0, vertices, polygons, /low

      vertices[0,*] += (obj.minX - obj.pixelFrame)
      vertices[1,*] += (obj.minY - obj.pixelFrame)
      vertices[2,*] += (obj.minZ - obj.pixelFrame)

;         oObjectModel->add, obj_new('IDLgrPolygon', ambient = [0,0,0], data = vertices, poly = polygons, shading = 1,$
;                                     vert_colors = transpose([[rgb_table0],[opacVect*255.]]),$
;                                     xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DSurfaceModel:'+string(i),/rem), uvalue = 'ObjInOriginalPosition')

      (self->get(position = i))->getProperty, color = color, alpha_channel = alpha
      objNumber = (self->get(position = i))->getNumber()
      oObjectModel->add, obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, shading = 1, shininess = 128.,$
                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DSurfaceModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
      print, strCompress('3DSurfaceModel:'+string(objNumber),/rem)
   endfor
end


pro C_sROI3DGroupObj::getoTrackingImageJModel, oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel
   
   ;; View Tracking from origin => set view_trackOrigin=1 -- View normal coordinates => set view_trackOrigin=0
   ;; Same Color = 1 -> different color = 0
   view_trackOrigin = 1 
   view_plane = 1
   view_sameColor = 1
   ;; Color RGB 0-255
   r=0
   g=255
   b=0
   
   radio = 5
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Radio Balls Model'))[0]
   if (whParam ne -1) then begin
      radio = fix(*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 1)
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = radio
   endif
   
   oColorModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if ~obj_valid(oColorModel) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oColorModel) then fOk = 1b else fOk = 0b

   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()

   factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 2
   thickEV = 2.
   colEV = [0,0,255]

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = totalTimes
   totalTimes=230
   ;center
   file = dialog_pickfile( /read, get_path = path, filter = '*.dat', /multiple_files)
   dim= size(file,/DIM) 
   center = fltArr(3,dim,totalTimes)
   center_origin = fltArr(3,dim,totalTimes)
   line = make_Array(3,/double)
      
   for i=0, dim[0]-1 do begin
      GET_LUN, inunit
      if file_test(file[i]) eq 0 then return
      openr, inunit, file[i]
      j=0
      while (~ EOF(inunit)) do begin
         READF, inunit, line
         center[*,i,j] = line
         j++ 
      endwhile
      FREE_LUN, inunit
   endfor

   ;Angulo entre plano eje Z, y punto final de trayectoria
   verts = [[0,0,0],[100,0,0],[100,0,5],[0,0,5]]
   conn  = [4,3,2,1,0]
   ;Ortogonal plane
   verts2 = [[0,0,0],[0,0,5],[0,100,5],[0,100,0]]
   conn2 = [4,3,2,1,0]

   ;Tracking from Origin
   if view_trackOrigin eq 1 then begin
       for i = 0, dim[0]-1 do begin
          for j = 0L, totalTimes-1 do begin
              center_origin[*,i,j] = center[*,i,j] - center[*,i,0]
              ;center_origin[2,i,j] = 0
              if (center_origin[0,i,j] eq (center[0,i,0]*[-1])) and $
                 (center_origin[1,i,j] eq (center[1,i,0]*[-1])) then center_origin[*,i,j] = [1000000, 0, 0]
          endfor
       endfor
   endif

   if (view_sameColor eq 1) $
   then colorRand = transpose([[make_array(dim[0], Value=1)*r],  [make_array(dim[0], Value=1)*g],   [make_array(dim[0], Value=1)*b]]) $
   else colorRand = transpose([[round(0+255*RANDOMU(1, dim[0]))],[round(0+255*RANDOMU(2, dim[0]))], [round(0+255*RANDOMU(3, dim[0]))]])

   maximoObjetos = dim[0]
   for i = 0L, maximoObjetos-1 do begin 
     for j = 0L, totalTimes-1 do begin
        if (view_trackOrigin eq 0) then begin
          ;;normal coord
          if (j eq 0) then begin 
                       oObjectModel->add, obj_new('C_sOrb', POS=center[*,i,j], RADIUS=radio, xyzFact = factorXYZ, color = [0,255,0], thick = thickEV, xCoord_conv = xCoord_conv, $
                                                            yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                            name = strCompress('3D_Track_SphereEnd_'+string(i),/rem), uvalue = 'ObjInOriginalPosition')
;                      ;Planes 
;                      if view_plane eq 1 then oObjectModel->Add, obj_new('IDLgrPolygon',data = verts, poly = conn, color = [200,0,0],$
;                                                            xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
;                                                            name = strCompress('3DPlaneModel_Base:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
;                      if view_plane eq 1 then oObjectModel->Add, obj_new('IDLgrPolygon',data = verts2, poly = conn2, color = [200,200,0],$
;                                                            xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
;                                                            name = strCompress('3DPlaneModel_Base:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
          
          endif
          if (j ne 0) and (center[0,i,j] ne 0.0) then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [center[0,i,j],center[0,i,j-1]],[center[1,i,j],center[1,i,j-1]], [center[2,i,j], center[2,i,j-1]], color = colorRand[*,i], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackLine_'+string(i),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
        endif else begin
          ;;Origin coord
          if (j eq 0) then begin 
                       oObjectModel->add, obj_new('C_sOrb', POS=center_origin[*,i,j], RADIUS=radio, xyzFact = factorXYZ, color = [0,255,0], thick = thickEV, $
                                                            xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                            name = strCompress('3D_Track_SphereEnd_'+string(i),/rem), uvalue = 'ObjInOriginalPosition')
                       ;Planes
                       if view_plane eq 1 then oObjectModel->Add, obj_new('IDLgrPolygon',data = verts, poly = conn, color = [200,0,0],$
                                                            xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
                                                            name = strCompress('3DPlaneModel_Base:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
                       if view_plane eq 1 then oObjectModel->Add, obj_new('IDLgrPolygon',data = verts2, poly = conn2, color = [200,200,0],$
                                                            xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
                                                            name = strCompress('3DPlaneModel_Base:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
          endif
          if (j ne 0) and (center_origin[0,i,j] ne 1000000) then begin
             oObjectModel->add, obj_new('IDLgrPolyline', [center_origin[0,i,j],center_origin[0,i,j-1]],[center_origin[1,i,j],center_origin[1,i,j-1]], [center_origin[2,i,j], center_origin[2,i,j-1]], color = colorRand[*,i], $
                                                          thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                          name = strCompress('3D_TrackLine_'+string(i),/rem), uvalue = 'ObjInOriginalPosition') 
          endif
        endelse
     endfor
   endfor
 
end


; Show 3D Trajectory
pro C_sROI3DGroupObj::getoTrackingEstimatorTrajectoryModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy

  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  xyzDim = self->getxyzDim()
  xyzSizePerPixel = self->getxyzSizePerPixel()
  zxFactor = xyzSizePerPixel[2]/xyzSizePerPixel[0]

  pParamStruct = self->getpParamStruct()

  clusPos = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Cluster Position'))[0]]
  chPos   = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Channel Position'))[0]]
  tPos    = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Time Position'))[0]]

  print, 'computing trajectory'
  radio = 5
  colEV = [0.,0.,255.]
  limitDistance = 40
  thickEV = 2.

   ;other parameters
  vAngleCorreccion  = 0.0
  vCentroCorreccion = [0,0,0]

  masterTracking = obj_new('C_s3DTrackingEstimator')
  masterTracking->MasterTrackingCreation, vSelected = seleccionados,stack_tlb = stack_tlb,$
                                          pParamStruct = *(self->getpParamStruct()), limitDistance = limitDistance,$
                                          factorXYZ = zxFactor, vAngleCorreccion = vAngleCorreccion, vCentroCorreccion = vCentroCorreccion, vRadio = radio
  vColor = fltArr(3)
  loadct, masterTracking.indexColorTable
  tvlct, r, g, b, /get

  ;tPos: current time
  ;roiIndex=1
  for indexRoot = 1L, n_elements(*masterTracking.indexOfRoots)-1 do begin

    dummy = where((*((*masterTracking.rootByTimes)[tPos]))[*] eq (*masterTracking.indexOfRoots)[indexRoot])
    if(dummy[0] ne -1) then begin

      for k = 0L, n_elements(dummy)-1 do begin

        localIndex = (*((*masterTracking.indexByTimes)[tPos]))[dummy[k]]
        masterTracking->getStaticLevels, vIndice = localIndex, vRadio = vRadio, colEV = colEV, thickEV = thickEV,$
                                         xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, vRecursive = 1b,$
                                         vDrawNodes = vDrawNodes, vDrawLines = vDrawLines, vColor = vColor, rLeft = rLeft, rRight = rRight

        roiIndex = ((*masterTracking.masterTracking))[localIndex]->getOwnNumber()+1

        if n_elements(rLeft) ne 0 and n_elements(rLeft)/3 gt 1 then begin

          print, strCompress('Tracking computed, drawing ROI...' + string(roiIndex), /REMOVE_ALL)
          nSegments = n_elements(rLeft)/3
          data = make_array(3, nSegments-1, /DOUBLE)

          for i = 0L, nSegments-2 do $
            data[*,i] = [rLeft[i*3], rLeft[i*3+1], rLeft[i*3+2]]
            oObjectModel->add, obj_new('IDLgrPolyline', data, color = [0,255,0], thick = 2,$
                                                        xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                        name = strCompress('3DTrackingEstimatorTrajectoryModel:' + string(roiIndex), /REMOVE_ALL),$
                                                        uvalue = 'ObjInOriginalPosition')
          print, 'Drawing computed'
        endif
      endfor
    endif
  endfor
end


; Show 3D Optical Flow
pro C_sROI3DGroupObj::getOpticalFlowModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

   widget_control, stack_tlb, get_uValue = stackState, /no_copy
     imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = self->getxyzDim()
   xyzSizePerPixel = self->getxyzSizePerPixel()
   zxFactor = xyzSizePerPixel[2]/xyzSizePerPixel[0]

   pParamStruct = self->getpParamStruct()

   clusPos = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Cluster Position'))[0]]
   chPos   = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Channel Position'))[0]]
   tPos    = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Time Position'))[0]]

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = totalTimes
   if (tPos ge (totalTimes-1)) then begin
     print, 'unable to compute 3DOF for the last time frame, returning...'
     return
   endif

   volData1 = make_array(xyzDim, /byte)
   for z = 0, xyzDim[2]-1 do $
     volData1[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
  
   volData2 = make_array(xyzDim, /byte)
   for z = 0, xyzDim[2]-1 do $
     volData2[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos+1, chPos = chPos, zPos = z)

   ofAlpha      = 80
   ofIterations = 200
   print, 'OF 3D starting...'
   opticalFlowHS3D, volData1, volData2, u, v, w, alpha = ofAlpha, iterations = ofIterations
   print, 'OF 3D computed'

   ;remove low values for visualization
   th  = 0.01
   mag = sqrt(u*u+v*v+w*w)
   indLow = where(mag lt th)
   ;u[indLow] = 0.0
   ;v[indLow] = 0.0
   ;w[indLow] = 0.0

   npoints = 10000 ;nr points where to draw the OF
   seed1   = 0
   seed2   = 100

   for i = 0L, (self->count())-1 do begin

      center = (self->get(position = i))->getCenterXYZ()

      myseeds=make_array(3,npoints)
      myseeds[0, *]=fix(center[0]+(randomu(seed1, [1,npoints])-.5)*xyzDim[0]*0.3)
      myseeds[1, *]=fix(center[1]+(randomu(seed2, [1,npoints])-.5)*xyzDim[1]*0.3)
      myseeds[2, *]=center[2];center[2]; fix(center[2]+(randomu(seed2, [1,npoints])-.5)*xyzDim[2]*0.3);

      objNumber = (self->get(position = i))->getNumber()

      opticalFlowShowOF3D, u, v, w, oObjectModel, objNumber, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, ZFACTOR=zxFactor, $
            /VECTOR, SEEDS = myseeds

      print, 'OF 3D using show_stream like function'
      
   endfor
   
end


pro C_sROI3DGroupObj::getoACOFsurfacePathModel, oObjectModel, stack_tlb = stack_tlb

  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  self->getProperty, color = color, alpha_channel = alpha
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = nTimes
  xyzDim = self->getxyzDim()

  clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
  chPos   = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]
  tPos    = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]

    ; hard coded time range
  startTime = tPos
  deltaTime = 3
  endTime = (tPos + deltaTime) < (nTimes - 2) ; OF is used so we compare with "nTimes-2" instead of "nTimes-1"

  ;nObj = self->count()
    ; using roi numbers as filter
  roiNumVec = [1, 2, 3]
  nObj = n_elements(roiNumVec)

  tVertArray = ptrArr(nObj, deltaTime+1)
  tPolyArray = ptrArr(nObj, deltaTime+1)

  roiIndexes = make_array(n_elements(nObj), value = -1, /integer)
  roiCount = self->count()
  for i = 0L, nObj-1 do $
    for j = 0L, roiCount-1 do begin
      if (self->get(position = j))->getNumber() eq roiNumVec[i] then roiIndexes[i] = j
    endfor

  for i = 0L, nObj-1 do begin
      ; get surface mesh model for the ROI
    obj = (self->get(position = roiNumVec[i]))->makePixelObjectInVoxel(/all)
    shade_volume, obj.obj, 0, vertices, polygons, /low

    vertices[0,*] += (obj.minX - obj.pixelFrame)
    vertices[1,*] += (obj.minY - obj.pixelFrame)
    vertices[2,*] += (obj.minZ - obj.pixelFrame)

    tVertArray[i,0] = ptr_new(vertices, /no_copy)
    tPolyArray[i,0] = ptr_new(polygons, /no_copy)
  endfor

  acAlpha = 1.0
  acBeta = 1.0
  acGamma = 1.0
  acKappa = 1.0
  acIterations = 10
  acVfIterations = 200
    ; 0->GVF, 1->GGVF, 2->EPGGVF
  acVfMethod = 2

  ofAlpha = 80
  ofIterations = 200

    ; 1st time 3D active contour segmentation
  imageData1 = make_array(xyzDim, /byte)
  for z = 0, xyzDim[2]-1 do $
    imageData1[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = t, chPos = chPos, zPos = z)

  oAC3D = obj_new('C_sActiveContour3D', imageData1, alpha = acAlpha, beta = acBeta, gamma = acGamma, kappa = acKappa, iterations = acIterations, GVF_iterations = acVfIterations)
  case acVfMethod of
    0: oAC3D->calcGVF
    1: oAC3D->calcGGVF
    2: oAC3D->calcEPGGVF
    else: oAC3D->calcGVF
  endcase

  for i = 0L, nObj-1 do begin
    oAC3D->setContour, *tVertArray[i,0]
    fRes = oAC3D->adjustContour()
    *tVertArray[i,0] = oAC3D->getCoords()
    *tPolyArray[i,0] = oAC3D->getPolygonList()
  endfor
  obj_destroy, oAC3D

  for t = startTime, endTime do begin

      ; get 3d images
    imageData1 = make_array(xyzDim, /byte)
    for z = 0, xyzDim[2]-1 do $
      imageData1[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = t, chPos = chPos, zPos = z)

    imageData2 = make_array(xyzDim, /byte)
    for z = 0, xyzDim[2]-1 do $
      imageData2[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = t+1, chPos = chPos, zPos = z)

      ; 3D OF calculations
    opticalFlowHS3D, imageData1, imageData2, u, v, w, alpha = ofAlpha, iterations = ofIterations

    oAC3D = obj_new('C_sActiveContour3D', imageData2, alpha = acAlpha, beta = acBeta, gamma = acGamma, kappa = acKappa, iterations = acIterations, GVF_iterations = acVfIterations)
    case acVfMethod of
      0: oAC3D->calcGVF
      1: oAC3D->calcGGVF
      2: oAC3D->calcEPGGVF
      else: oAC3D->calcGVF
    endcase

    for i = 0L, nObj-1 do begin

        ; project segmentation with the corresponding 3D-OF and adjustment with 3D active contour for the next frame
      xCoords = (*tVertArray[i,t])[0,*]
      yCoords = (*tVertArray[i,t])[1,*]
      zCoords = (*tVertArray[i,t])[2,*]
      xCoords += OF_u
      yCoords += OF_v
      zCoords += OF_w

        ; 3D active contour optimization
      oAC3D->setContour, xCoords, yCoords, zCoords, *tPolyArray[i,t]
      fSeg = oAC3D->adjustContour()

      tVertArray[i,t+1] = ptr_new(oAC3D->getCoords(), /no_copy)
      tPolyArray[i,t+1] = ptr_new(oAC3D->getPolygonList(), /no_copy)
    endfor
    obj_destroy, oAC3D
  endfor
    ; post processing using convex hull
end


; It is assumed that only the objects in the current instance of the ROI group will be considered
; for display
pro C_sROI3DGroupObj::getoSurfacePathTrackingModel, oObjectModel, stack_tlb = stack_tlb

  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  self->getProperty, color = color, alpha_channel = alpha

  xyzDim = self->getxyzDim()
  xyzSizePerPixel = self->getxyzSizePerPixel()
  zxSizeRatio = xyzSizePerPixel[2] / xyzSizePerPixel[0]

  roiNumVec = [12, 13, 21]

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = nTimes
  clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
  chPos   = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]
  tPos    = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]

  print, 'Computing TrackingEstimator trajectories'
  radio = 5
  limitDistance = 40
  vAngleCorreccion = 0.0
  vCentroCorreccion = [0,0,0]

  masterTracking = obj_new('C_s3DTrackingEstimator')
  masterTracking->MasterTrackingCreation, stack_tlb = stack_tlb, vSelected = selected,$
                                          pParamStruct = *(self->getpParamStruct()), limitDistance = limitDistance,$
                                          factorXYZ = zxSizeRatio, vAngleCorreccion = vAngleCorreccion, vCentroCorreccion = vCentroCorreccion, vRadio = radio
  trackMatrix = masterTracking->getTrackingMatrix()
  obj_destroy, masterTracking

; alt 1
;  trackMatrix = make_array(masterTracking._maximoObjetos, masterTracking._tiempos, /integer, value = -1)
;  for t = 0L, masterTracking._tiempos-1 do begin
;    ; gwt the roi indexes from the tracking object (that point to the rootByTimes array)
;    pROIindexByTimes = (*(masterTracking.indexByTimes))[t]
;    nRois = n_elements(*pROIindexByTimes)
;    if (t eq 0) then begin
;      for i = 0L, nRois-1 do begin
;        currRoiIndex  = (*pROIindexByTimes)[i]
;        currRoiNumber = ((*masterTracking.masterTracking)[currRoiIndex])->getOwnNumber()
;        trackMatrix[currRoiNumber,t] = currRoiNumber
;      endfor
;    endif else begin
;      for i = 0L, nRois-1 do begin
;        currRoiIndex   = (*pROIindexByTimes)[i]
;        parentRoiIndex = ((*masterTracking.masterTracking)[currRoiIndex])->getOwnParent()
;        roiNumber = (parentRoiIndex eq -1)$
;                     ? ((*masterTracking.masterTracking)[currRoiIndex])->getOwnNumber()
;                     : ((*masterTracking.masterTracking)[parentRoiIndex])->getOwnNumber()
;        trackMatrix[roiNumber,t] = currRoiNumber
;      endfor
;    endelse
;  endfor

  trackMatrixSize = size(trackMatrix, /dim)
  nTrajectories = trackMatrixSize[0]
  nTimes = trackMatrixSize[1]
    ; hard coded time range
  startTime = tPos
  deltaTime = 80
  endTime = (tPos + deltaTime) < (nTimes - 1)

  nObj = self->count()
  roiNums = make_array(nObj, value = 0, /INTEGER)
  for i = 0L, nObj-1 do $
    roiNums[i] = (self->get(position = i))->getNumber()

  roiNumbersInTrajectory = [-1]
  roiTrajectoryIndexes   = [-1]

    ; adaptive for-loop to avoid nObj x nTrajectories comparison
    ; assuming that only one roi number will appear in the tracking matrix
    ; and that the roi numbers are sorted
  roiNums = roiNums[sort(roiNums)]
  roiNumIndex = 0L
  for k = 0L, nTrajectories-1 do begin
    for i = roiNumIndex, nObj-1 do $
        ; XXX using the roiNumber -1 because... FASL?
      if (trackMatrix[k,startTime]+1 eq roiNums[i]) then begin
         roiNumbersInTrajectory = [roiNumbersInTrajectory, roiNums[i]]
         roiTrajectoryIndexes   = [roiTrajectoryIndexes,   k]
         roiNumIndex += 1
      endif
    if (roiNumIndex eq nObj) then break
  endfor

  nObjInTrajectories = n_elements(roiNumbersInTrajectory)-1
  if (nObjInTrajectories lt 1) then begin
    print, 'trajectories for ROIs with number ', roiNums,' not found, returning...'
    return
  end

  tVertArray = ptrArr(nObjInTrajectories, deltaTime+1)
  tPolyArray = ptrArr(nObjInTrajectories, deltaTime+1)

  for i = 1L, nObjInTrajectories do begin

    trajectoryPos    = roiTrajectoryIndexes[i]
    currentRoiNumber = roiNumbersInTrajectory[i]

    for t = startTime, endTime do begin

      oROIGroupT = (tPos eq t) ? self : s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = t, chPos = chPos, clusPos = clusPos)
      nObjT = oROIGroupT->count()

      for j = 0L, nObjT-1 do begin

        jRoi = (oROIGroupT->get(position = j))
          ;XXX again... using roi number +1
        if (jRoi->getNumber() eq (trackMatrix[trajectoryPos,t]+1)) then begin
            ; get the points for the ROI... by now we can assume that (pos ne -1)
          ;xyzPoints = jRoi->getXyzPoints()
          ;qhull, xyzPoints, tri, bounds = bounds, vvertices = vertices, connectivity = list, /delaunay
          ;xyzPoints = xyzPoints[*,bounds]
          ;tVertArray[i-1,t] = ptr_new(jRoi->getXyzPoints())
          tVertArray[i-1,t] = ptr_new(*(jRoi->getpWherePoints()))
          ;tVertArray[i,t] = ptr_new(vertices, /no_copy)
;          tPolyArray[i,t] = ptr_new(polygons, /no_copy)
        endif
      endfor

    endfor
  endfor

    ; free space of tracking matrix... not too big but who knows...
  trackMatrix = -1

  trajectoriesToRender = make_array(nObjInTrajectories, /integer, value = -1)
  nTrajectoriesToRender = 0
  minOccurrencesToRender = 2
  for i = 0L, nObjInTrajectories-1 do begin

      ; alt 1, use ptr array
    roiCount = 0
    for t = 0L, deltaTime-1 do $
      if ptr_valid(tVertArray[i,t]) then roiCount += 1

    trajectoriesToRender[i] = roiCount gt 0
    if (roiCount gt 0) then nTrajectoriesToRender += 1
  endfor

  if (nTrajectoriesToRender eq 0) then begin
    print, 'No ROI meeting the occurrence criterion for trajectory display has been found. Returning...'
    return
  endif

  xySamplingTimeStepFactor = 1 ; TODO not immplemented yet... leave as 1
  for i = 0L, nTrajectoriesToRender-1 do begin
    if (trajectoriesToRender[i] eq 1) then begin
      roiNumber = roiNumbersInTrajectory[i]
      xyzPoints = 1
      fPoints = 0b
      for t = 0L, deltaTime do begin
        if ptr_valid(tVertArray[i,t]) then $
          if (~fPoints and (n_elements(xyzPoints) eq 1)) then begin
            xyzPoints = *tVertArray[i,t]
            fPoints = 1b
          endif else begin
            if (xySamplingTimeStepFactor ne 1) then begin
              tmp = *tVertArray[i,t]
              tmp[0:1,*] *= xySamplingTimeStepFactor
            ;endif else xyzPoints = [[xyzPoints], [*tVertArray[i,t]]]
            endif else xyzPoints = [xyzPoints, *tVertArray[i,t]]
          endelse
      endfor
      if (where(roiNumVec eq roiNumber) ne -1) then begin
        print, 'drawing roi number ', roiNumber
        volData = make_array(xyzDim, /byte, value = 0b)
        volData[xyzPoints] = 1
        shade_volume, volData, 0, vertices, polygons, /low
;        oObjectModel->add, obj_new('IDLgrROI', style = 0, data = xyzPoints, thick = 2, color = [255,255,0],$
;                                    xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
;                                    name = strCompress('3DSurfacePathTrackingModel:'+string(roiNumber),/rem), uvalue = 'ObjInOriginalPosition')
        oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = polygons,$
                                                   ambient = [0,0,0], bottom = color, color = color, alpha_channel = 0.2, shininess = 128., shading = 1,$
                                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                   name = strCompress('3DSurfacePathTrackingModel:' + string(roiNumber),/rem), uvalue = 'ObjInOriginalPosition')
      endif
;      qhull, xyzPoints, tri, bounds = bounds, vvertices = vertices, connectivity = list, /delaunay
;      xyzPoints = xyzPoints[*,bounds]
;      qhull, xyzPoints, tri, bounds = bounds, vvertices = vertices, connectivity = list, /delaunay
;
;      for k = 0L, (size(tri, /dim))[1]-1 do begin
;        oObjectModel->add, obj_new('IDLgrPolygon', data = [xyzPoints[0, tri[[0,1,2,3,0], k]], xyzPoints[1, tri[[0,1,2,3,0], k]], xyzPoints[2, tri[[0,1,2,3,0], k]]],$
;                                    bottom = color, color = [0,255,0], alpha_channel = 0.20, shading = 1,$
;                                    xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DTrackingMeshModel:' + string(roiNumber),/rem), uvalue = 'ObjInOriginalPosition')
;      endfor
    endif
  endfor

    ; pointers cleanup
  for i = 0L, nObjInTrajectories-1 do $
    for j = 0L, deltaTime do begin
      ptr_free, tVertArray[i,j]
      ptr_free, tPolyArray[i,j]
    endfor
  ptr_free, tVertArray
  ptr_free, tPolyArray
end


pro C_sROI3DGroupObj::getoACModel, oObjectModel, stack_tlb = stack_tlb

  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos

  xyzDim      = self->getxyzDim()
  nObj        = self->count()
  acModelName = '_AC3D_Obj_'

    ; Option to load saved 3D active contour models (if they exist). Be careful with saved models computed with who-knows? parameter values :)
  fUseSavedModels = 1b
  fSaveACModels   = 1b

  fFile = make_array(nObj, /byte, value = 0b)
  if (fUseSavedModels eq 1) then begin
    self->restoreSavedSurfaceModels, stack_tlb = stack_tlb, modelName = acModelName, pPolygons = pPolygons, pVertices = pVertices, fFile = fFile
    if (total(fFile) eq nObj) then begin
      fSaveACModels = 0b ; avoid accidental overwriting of saved models
    endif else begin
      fUseSavedModels = 0b
      fSaveACModels = 1b
    endelse
  endif else begin
    pVertices = ptrArr(nObj)
    pPolygons = ptrArr(nObj)
  endelse

  if (fsaveACModels eq 1) then begin
    widget_control, stateObj_tlb, get_uValue = state, /no_copy
      currROI3DGroupFileName = state.currROI3DGroupFileName
    widget_control, stateObj_tlb, set_uValue = state, /no_copy

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
      (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = stackState, /no_copy

    slash = path_sep()
    posLastSlash = strPos(currROI3DGroupFileName, slash, /reverse_search)
    currROI3DGroupFileName = strMid(currROI3DGroupFileName, strPos(currROI3DGroupFileName, slash, /reverse_search)+1)
    currROI3DGroupFileName = strMid(currROI3DGroupFileName, 0, strPos(currROI3DGroupFileName, '.sav'))
    acModelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]] + strcompress(acModelName + slash, /rem))
    if (file_test(strCompress(acModelPath),/DIRECTORY) eq 0b) then file_mkDir, strCompress(acModelPath)
  endif

    ; Check if a C_sActiveContour3D instance is required... the use of || skips unnecessary evaluations
  if (~fUseSavedModels || (max(fFile) eq 0)) then begin
    print, 'Creating C_sActiveContour3D instance'
    acIterations       = 5
    acAlpha            = 0.05
    acBeta             = 0.2
    acKappa            = 0.5
    fForceVFcalc       = 0b ; if non-0, forces the vector field computation even when kappa = 0
    acGamma            = 1.0
    acVFiterations     = 5
      ; Vector field parameters (GVF/GGVF/EPGGVF)
    acGVFmu            = 0.05
    acEPGGVFstep       = 1
    acEPGGVFerror      = 1
    acEPGGVFnoiseCut   = 1
    acEPGGVFnoiseRange = 1

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
    widget_control, stack_tlb, set_uValue = stackState, /no_copy

    volData = make_array(xyzDim, /byte, value = 0)

      ; Option to consider only the image intensities at the voxels of the initial ROI models, instead of the entire 3D image.
    fUseROIintOnly = 0b
    case fUseROIintOnly of
      1:    for i = 0L, nObj-1     do volData[*((self->get(position = i))->getpWherePoints())] = *((self->get(position = i))->getpPointValues())
      else: for z = 0, xyzDim[2]-1 do volData[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
    endcase

    oAC3D   = obj_new('C_sActiveContour3D', volData, iterations = acIterations, alpha = acAlpha, beta = acBeta, kappa = acKappa, gamma = acGamma, gvf_iterations = acVFiterations, mu = acGVFmu)
    volData = -1
      ; Set one of the multiple choices for vector field computation
    fVFmethod = 0
    if (acKappa gt 0) or (fForceVFcalc) then $
    case fVFmethod of
     0: oAC3D->calcGVF;, mu = acGVFmu
     1: oAC3D->calcGGVF, mu = acGVFmu
     2: oAC3D->calcEPGGVF, noiseCut = acEPGGVFnoiseCut, noiseRange = acEPGGVFnoiseRange, error = acEPGGVFerror, step = acEPGGVFstep
     else: oAC3D->calcGVF, mu = acGVFmu
    end
  endif else print, '3DAC models loaded. No C_sActiveContour3D computation to be performed this time'

  fColorByCurvature   = 0b
  fColorByZpos        = 0b and ~fColorByCurvature; Toggle this value to switch between flat color and z-position color values.
    fCustomColorTable = 1b  ; To be used when coloring by z-pos.
    colorTableIndex   = 25

  for i = 0L, nObj-1 do begin

     objNumber = (self->get(position = i))->getNumber()

    if (fUseSavedModels and fFile[i]) then begin

      pRoiVertices = pVertices[i]
      pRoiPolygons = pPolygons[i]
      ; TODO include vert_color, opacVect

    endif else begin

      s_getSavedModels, stack_tlb, self, ['_MeshFix_'], pPolygons = pPolygonsLoad, pVertices = pVerticesLoad, whereObj = [i], fOK = loadOK, /fSilent
      if (loadOK eq 1) then begin
        vertices = *pVerticesLoad[0]
        polygons = *pPolygonsLoad[0]
        oAC3D->setContour, transpose((*pVerticesLoad[0])[0,*]), transpose((*pVerticesLoad[0])[1,*]), transpose((*pVerticesLoad[0])[2,*]), *pPolygonsLoad[0]
      endif else begin
        obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
        shade_volume, obj.obj, 0, vertices, polygons, /low
        vertices[0,*] += (obj.minX - obj.pixelFrame)
        vertices[1,*] += (obj.minY - obj.pixelFrame)
        vertices[2,*] += (obj.minZ - obj.pixelFrame)
        oAC3D->setContour, transpose(vertices[0,*]), transpose(vertices[1,*]), transpose(vertices[2,*]), polygons
      endelse

      fAdjustOK = oAC3D->adjustContour()

      if (fAdjustOK eq 1) then begin
        pRoiVertices  = ptr_new(oAC3D->getCoords())
        pRoiPolygons  = ptr_new(oAC3D->getPolygonList())
        colorVect     = oAC3D->getCurv4()

        if fColorByCurvature then begin
          maxCV = max(colorVect, min = minCV)
          ;hist = histogram(colorVect, max = maxCV, min = minCV, nbins = 100, locations = curvValues)
          ;xhist = ((make_array(n_elements(hist), /float, /index)) / 100.) * (maxCV-minCV) + minCV
          ;live_plot, hist, independent = xhist
          ;print, 'radius r - calc r - sd (calc r) - curvature k - calc k - sd(calc k) - surface s - calc s'
          print, tPos + 1,      (moment(1./colorVect))[0], sqrt(((1./colorVect))[1]),$
                 1./(tPos + 1), (moment(colorVect))[0],    sqrt((moment(colorVect))[1]),$
                 4. * !pi * (tPos + 1)^2, (self->get(position = i))->getObjectSurfaceFromPolygons(polygons = polygons, vertices = vertices)

          if (maxCV gt minCV) then colorVect -= minCV

          vert_color = make_array(4, (size(vertices, /dim))[1], /byte)
          rgb_table0 = bytArr(256,3) + transpose((*self.pVolState).rgbValues[0,*,*])
          opacVect   = self->getOpacVect('1st Volume Opacity')

          vert_color[0,*] = (rgb_table0[*,0])[colorVect]
          vert_color[1,*] = (rgb_table0[*,1])[colorVect]
          vert_color[2,*] = (rgb_table0[*,2])[colorVect]
          vert_color[3,*] = opacVect[colorVect]
        endif

      endif else print, 'Error adjusting contour for object number ', objNumber

      if (fSaveACModels eq 1) and (fAdjustOK eq 1) then begin
        roiACfileName = strCompress(acModelPath + strcompress(currROI3DGroupFileName + acModelName + string(objNumber) + '.sav', /rem))
        vertices = *pRoiVertices
        polygons = *pRoiPolygons
        save, vertices, polygons, vert_color, fileName = roiACfileName
        ;save, vertices, polygons, opacVect, vert_color, fileName = roiACfileName
      endif
    endelse

    if fColorByZpos then begin
      vert_color  = bytArr(3, n_elements((*pRoiVertices)[2,*])) + 255
      if keyword_set(fCustomColorTable) then loadCT, colorTableIndex, rgb_table = zColorTable
      minZpos     = min((*pRoiVertices)[2,*], max = maxZpos)
      zRange      = maxZpos - minZpos
      deltaColorZ = zRange / 257.0
      zPos = minZpos
      for zColorInd = 0, 255 do begin
        zColor = keyword_set(fCustomColorTable) ? transpose(zColorTable[zColorInd, *]) : [zColorInd, zColorInd, zColorInd]
        whZ = where((*pRoiVertices)[2,*] ge zPos, countWhZpos, /L64)
        if (countWhZpos gt 0) then $
          for whInd = 0u, countWhZpos-1 do $
            vert_color[*, whZ[whInd]] = zColor
        ;print, 'Setting color ', zColor, ' for ', countWhZpos, ' vertices at z-pos. ' , zPos, ' z-color ind. ', zColorInd
        zPos += deltaColorZ
      endfor
      print, 'Z-colors done (table index ', colorTableIndex, '), object ', i
    endif

    (self->get(position = i))->getProperty, alpha_channel = alpha, color = color
    if n_elements(vert_color) gt 0 $
    then oObjectModel->add, obj_new('IDLgrPolygon', data = *pRoiVertices, poly = *pRoiPolygons,$
                                                    ambient = [0,0,0], bottom = color, vert_colors = vert_color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                    xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                    name = strCompress('3DACModel:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition') $
    else oObjectModel->add, obj_new('IDLgrPolygon', data = *pRoiVertices, poly = *pRoiPolygons,$
                                                    ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                    xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                    name = strCompress('3DACModel:' + string(objNumber), /rem), uvalue = 'ObjInOriginalPosition')
  endfor
  if obj_valid(oAC3D) then obj_destroy, oAC3D
end


pro C_sROI3DGroupObj::getoACModelWTF, oObjectModel, stack_tlb = stack_tlb

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos

   widget_control, stateObj_tlb, get_uValue = state, /no_copy
      currROI3DGroupFileName = state.currROI3DGroupFileName
   widget_control, stateObj_tlb, set_uValue = state, /no_copy
   currROI3DGroupFileName = strMid(currROI3DGroupFileName,0, strPos(currROI3DGroupFileName, '.sav'))

   xyzDim = self->getxyzDim()

   widget_control, stack_tlb, get_uValue = state, /no_copy
      (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
   widget_control, stack_tlb, set_uValue = state, /no_copy

      ; test if AC have been saved before
   fileName = currROI3DGroupFileName + '_AC3D_Obj_'
   nObj = self->count()
   fileNameArr = make_array(nObj, /string)
   fFile = make_array(nObj, /byte)
   for i = 0L, nObj-1 do begin
      fileNameArr[i] = strCompress(fileName+strcompress(string((self->get(position = i))->getNumber()),/rem))
      fileStack = file_info(fileNameArr[i]+'.sav')
      fFile[i] = fileStack[0].exists
   endfor

   fROIint = 1
      ; get 1st  object and object parameters
   volData1 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   if ~fROIInt then begin
      widget_control, stack_tlb, get_uValue = stackState, /no_copy
         imageStackInfoObject = *stackState.pImageStackInfoObject
      widget_control, stack_tlb, set_uValue = stackState, /no_copy
      for z = 0, xyzDim[2]-1 do begin
         image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
         szI = size(image, /dim)
         if ((szI[0] eq xyzDim[0]) and (szI[1] eq xyzDim[1])) then volData1[*,*,z] = image
      endfor
   endif else for i = 0L, (self->count()-1) do volData1[*((self->get(position = i))->getpWherePoints())] = *((self->get(position = i))->getpPointValues())

      ; define initial surfaces
   nObj = self->count()
   pVertices = ptrArr(nObj)
   pPolygons = ptrArr(nObj)
   for i = 0L, nObj-1 do begin
      case fFile[i] of
         0: begin
            obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
            shade_volume, obj.obj, 0, vertices, polygons, /low
            vertices[0,*] += (obj.minX - obj.pixelFrame)
            vertices[1,*] += (obj.minY - obj.pixelFrame)
            vertices[2,*] += (obj.minZ - obj.pixelFrame)
         endcase
         else: begin
;             openR,1, fileNameArr[i]+'.dat'
;                verticesDim = make_array(2, /long)
;                readF, 1, verticesDim, polygonsDim
;                vertices = make_array(verticesDim, /float)
;                polygons = make_array(polygonsDim, /float)
;                readF, 1, vertices, polygons
;             close, 1

             restore, fileNameArr[i]+'.sav', /relaxed
             oDummy->getProperty, data = vertices, poly = polygons
             obj_destroy, oDummy
         endcase
      endcase
      pVertices[i] = ptr_new(vertices, /no_copy)
      pPolygons[i] = ptr_new(polygons, /no_copy)
   endfor

   iterations = 5
   surfAreas = fltArr(iterations+1)

   oAC3D = obj_new('C_sActiveContour3D', volData1, iterations = iterations, alpha = .05, beta = .2, kappa = .5, gamma = 1., gvf_iterations = 5)
   ;oAC3D->calcGGVF

   for i = 0L, nObj-1 do begin

      oAC3D->setContour, transpose((*pVertices[i])[0,*]), transpose((*pVertices[i])[1,*]), transpose((*pVertices[i])[2,*]), *(pPolygons[i])

         ; calculate AC3D
      ;if (fFile[i] eq 0) then result = oAC3D->adjustContour() else result = oAC3D->getAvgNeighborNodes()
      result = oAC3D->adjustContour()
      vertices = oAC3D->getCoords()
      polygons = oAC3D->getPolygonList()
      colorVect = oAC3D->getCurv4()

      maxCV = max(colorVect, min = minCV)
      ;hist = histogram(colorVect, max = maxCV, min = minCV, nbins = 100, locations = curvValues)
      ;xhist = ((make_array(n_elements(hist), /float, /index)) / 100.) * (maxCV-minCV) + minCV
      ;live_plot, hist, independent = xhist
      ;print, 'radius r - calc r - sd (calc r) - curvature k - calc k - sd(calc k) - surface s - calc s'
      print, tPos + 1, (moment(1./colorVect))[0], sqrt(((1./colorVect))[1]),$
             1./(tPos + 1), (moment(colorVect))[0], sqrt((moment(colorVect))[1]),$
             4. * !pi * (tPos + 1)^2, (self->get(position = i))->getObjectSurfaceFromPolygons(polygons = polygons, vertices = vertices)

      if (maxCV gt minCV) then colorVect -= minCV
      vert_color = make_array(4, (size(vertices, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*self.pVolState).rgbValues[0,*,*])
      opacVect = self->getOpacVect('1st Volume Opacity')

      vert_color[0,*] = (rgb_table0[*,0])[colorVect]
      vert_color[1,*] = (rgb_table0[*,1])[colorVect]
      vert_color[2,*] = (rgb_table0[*,2])[colorVect]
      vert_color[3,*] = opacVect[colorVect]

      (self->get(position = i))->getProperty, color = color, alpha_channel = alpha
      objNumber = (self->get(position = i))->getNumber()
      oObjectModel->add, obj_new('IDLgrPolygon', ambient = [0,0,0], data = vertices, poly = polygons, bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                 xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                 name = strCompress('3DACModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
;      if (fFile[i] eq 0) then begin
;         openW, 1, fileNameArr[i]+'.dat'
;         printF, 1, size(vertices,/dim), size(polygons,/dim)
;         printF, 1, vertices, polygons
;         close, 1
;      endif

      if (fFile[i] eq 0) then begin
         oDummy = obj_new('IDLgrPolygon', data = vertices, poly = polygons)
         save, oDummy, filename = fileNameArr[i]+'.sav'
         obj_destroy, oDummy
      endif

      if (n_elements(realSize) gt 0) then begin
         vertices[0,*]  *= ((*self.pEigenSys).sizePerXYZ)[0]
         vertices[1,*]  *= ((*self.pEigenSys).sizePerXYZ)[1]
         vertices[2,*]  *= ((*self.pEigenSys).sizePerXYZ)[2]
      endif
   endfor

   obj_destroy, oAC3D
   for i = nObj-1, 0, -1 do begin
      ptr_free, pVertices[i]
      ptr_free, pPolygons[i]
   endfor
   ptr_free, pVertices
   ptr_free, pPolygons
end

pro C_sROI3DGroupObj::getoSurfIntModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

   xyzDim = self->getxyzDim()
      ; get 1st  object and object parameters
   volData1 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   for z = 0, xyzDim[2]-1 do begin
      image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
      szI = size(image, /dim)
      if ((szI[0] eq xyzDim[0]) and (szI[1] eq xyzDim[1])) then volData1[*,*,z] = image
   endFor
   image = 0

   if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzSizePerPixel = self->getxyzSizePerPixel()
   oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b

   fCalib = 0b
   minInt = 0
   maxInt = 0
   for i = 0L, (self->count())-1 do begin

      if fSurf then begin
         oPoly = oSurfModel->get(position = i)
         if obj_isa(oPoly, 'IDLGRPOLYGON') then oPoly->getProperty, polygons = polygons, data = vertices
      endif

      if (n_elements(vertices) le 0) then vertices = -1
      if (vertices[0] eq -1) then begin
         obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
         shade_volume, obj.obj, 0, vertices, polygons, /low
;         normals = COMPUTE_MESH_NORMALS(vertices, polygons)*rebin([1,1,1],3,(size(vertices))[2])
;         vertices = round(temporary(vertices) - normals)
         vertices[0,*] += (obj.minX - obj.pixelFrame)
         vertices[1,*] += (obj.minY - obj.pixelFrame)
         vertices[2,*] += (obj.minZ - obj.pixelFrame)
      endif

      intVec = surf_intensity_r(vertices = vertices, volume = volData1, xyz_size_per_pixel = xyzSizePerPixel, r_pixels = 1, pts_factor = 2)

      vertices[0,*] *= xyzSizePerPixel[0]
      vertices[1,*] *= xyzSizePerPixel[1]
      vertices[2,*] *= xyzSizePerPixel[2]

;      maxInt = 50
;      minInt = 0
;      intVec -= minInt
;      intVec /= ((maxInt - minInt) / 255.)
      
      vert_color = make_array(4, (size(vertices, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*self.pVolState).rgbValues[0,*,*])
      opacVect = self->getOpacVect('1st Volume Opacity')

      vert_color[0,*] = (rgb_table0[*,0])[intVec]
      vert_color[1,*] = (rgb_table0[*,1])[intVec]
      vert_color[2,*] = (rgb_table0[*,2])[intVec]
      vert_color[3,*] = 255 ;opacVect[intVec]

      objNumber = (self->get(position = i))->getNumber()
      if fUpDateROI3DGroupProperties then begin
         oObj = oObjectModel->GetByName (strCompress('3DSurfIntModel:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
      endif else begin

         vertices[0,*] /= xyzSizePerPixel[0]
         vertices[1,*] /= xyzSizePerPixel[1]
         vertices[2,*] /= xyzSizePerPixel[2]

        oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = polygons, vert_colors = vert_color, shininess = 128., shading = 1,$
                                                 xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                 name = strCompress('3DSurfIntModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
      endelse
      vertices = -1
   endfor
end

pro C_sROI3DGroupObj::getoBinSurfIntModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel
   xyzDim = self->getxyzDim()
      ; get 1st  object and object parameters
   volData1 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   for z = 0, xyzDim[2]-1 do begin
      image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
      szI = size(image, /dim)
      if ((szI[0] eq xyzDim[0]) and (szI[1] eq xyzDim[1])) then volData1[*,*,z] = image
   endFor
   
   image = 0

   if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzSizePerPixel = self->getxyzSizePerPixel()
   oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b

   fCalib = 0b
   minInt = 0
   maxInt = 0
   for i = 0L, (self->count())-1 do begin

      if fSurf then begin
         oPoly = oSurfModel->get(position = i)
         if obj_isa(oPoly, 'IDLGRPOLYGON') then oPoly->getProperty, polygons = polygons, data = vertices
      endif

      if (n_elements(vertices) le 0) then vertices = -1
      if (vertices[0] eq -1) then begin
         obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
         shade_volume, obj.obj, 0, vertices, polygons, /low
         vertices[0,*] += (obj.minX - obj.pixelFrame)
         vertices[1,*] += (obj.minY - obj.pixelFrame)
         vertices[2,*] += (obj.minZ - obj.pixelFrame)
      endif

      intVec = surf_intensity_r(vertices = vertices, volume = volData1, xyz_size_per_pixel = xyzSizePerPixel, r_pixels = 2, pts_factor = 2)

      vertices[0,*] *= xyzSizePerPixel[0]
      vertices[1,*] *= xyzSizePerPixel[1]
      vertices[2,*] *= xyzSizePerPixel[2]

      maxInt = max(intVec, min = minInt)
      middleInt = (minInt+maxInt)/2
      middleInt = 24.4962 ;;;;;;;;;;; Control
;      middleInt = 15.8219 ;;;;;;;;;;; Mutante
      minZ = min(vertices[2,*], max = maxZ)
      wh_low = where(intVec le middleInt, n_low, COMPLEMENT= wh_high, NCOMPLEMENT=n_high)
      print, "N° se puntos: " + string(n_high+n_low)
      print, "Baja intensidad: " +  string(round(100.0*n_low/(n_low+n_high))) + "%"
      print, "Alta intensidad: " +  string(round(100.0*n_high/(n_low+n_high))) + "%"
      print, "Intensidad promedio: " +  string(1.0*total(intVec)/N_ELEMENTS(intVec))

      vert_color = make_array(4, (size(vertices, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*self.pVolState).rgbValues[0,*,*])
      opacVect = self->getOpacVect('1st Volume Opacity')

      vert_color[0,*] = (rgb_table0[*,0])[intVec]
      vert_color[1,*] = (rgb_table0[*,1])[intVec]
      vert_color[2,*] = (rgb_table0[*,2])[intVec]
      
      if (n_low gt 0 and n_high gt 0) then begin
         vert_color[0,wh_low]  = (rgb_table0[*,0])[35]
         vert_color[0,wh_high] = (rgb_table0[*,0])[106]
         vert_color[1,wh_low]  = (rgb_table0[*,1])[35]
         vert_color[1,wh_high] = (rgb_table0[*,1])[106]
         vert_color[2,wh_low]  = (rgb_table0[*,2])[35]
         vert_color[2,wh_high] = (rgb_table0[*,2])[106] 
;         Pintar las tapas de otro color
;         vert_color[0,where(vertices[2,*] eq minZ)] = (rgb_table0[*,0])[130]
;         vert_color[1,where(vertices[2,*] eq minZ)] = (rgb_table0[*,1])[130]
;         vert_color[2,where(vertices[2,*] eq minZ)] = (rgb_table0[*,2])[130]
;         vert_color[0,where(vertices[2,*] eq maxZ)] = (rgb_table0[*,0])[130]
;         vert_color[1,where(vertices[2,*] eq maxZ)] = (rgb_table0[*,1])[130]
;         vert_color[2,where(vertices[2,*] eq maxZ)] = (rgb_table0[*,2])[130]
      endif

      vert_color[3,*] = 255;opacVect[intVec]

      objNumber = (self->get(position = i))->getNumber()
      if fUpDateROI3DGroupProperties then begin
         oObj = oObjectModel->GetByName (strCompress('3DSurfIntModel:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
      endif else begin

         vertices[0,*] /= xyzSizePerPixel[0]
         vertices[1,*] /= xyzSizePerPixel[1]
         vertices[2,*] /= xyzSizePerPixel[2]

        oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = polygons, vert_colors = vert_color, shininess = 128., shading = 1,$
                                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                   name = strCompress('3DSurfIntModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
      endelse
      vertices = -1
   endfor
end

pro C_sROI3DGroupObj::getoSurfCurvModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

   if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzSizePerPixel = self->getxyzSizePerPixel()
   oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b


   oAC3D = obj_new('C_sActiveContour3D')
   fCalib = 0b
;   for i = 0L, (self->count())-1 do begin
;      if fSurf then begin
;         oPoly = oSurfModel->get(position = i)
;         if obj_isa(oPoly, 'IDLGRPOLYGON') then oPoly->getProperty, polygons = polygons, data = vertices
;      endif
;
;      if (n_elements(vertices) le 0) then begin
;         obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
;         shade_volume, obj.obj, 0, vertices, polygons, /low
;         vertices[0,*] += (obj.minX - obj.pixelFrame)
;         vertices[1,*] += (obj.minY - obj.pixelFrame)
;         vertices[2,*] += (obj.minZ - obj.pixelFrame)
;      endif
;
;      vertices[0,*] *= xyzSizePerPixel[0]
;      vertices[1,*] *= xyzSizePerPixel[1]
;      vertices[2,*] *= xyzSizePerPixel[2]
;
;      oAC3D->setContour, reform(vertices[0,*]), reform(vertices[1,*]), reform(vertices[2,*]), polygons
;      result = oAC3D->getAvgNeighborNodes()
;      curvVec = alog(oAC3D->getCurv4() + 1)
;
;      if (i eq 0) then maxCurv = max(curvVec, min = minCurv)
;      maxCurv >= max(curvVec, min = minCur)
;      minCurv <= minCur
;      fCalib = 1b
;   endfor

   for i = 0L, (self->count())-1 do begin

      if fSurf then begin
         oPoly = oSurfModel->get(position = i)
         if obj_isa(oPoly, 'IDLGRPOLYGON') then oPoly->getProperty, polygons = polygons, data = vertices
      endif

      if (n_elements(vertices) le 0) then begin
         obj = (self->get(position = i))->makePixelObjectInVoxel(/all)
         shade_volume, obj.obj, 0, vertices, polygons, /low
         vertices[0,*] += (obj.minX - obj.pixelFrame)
         vertices[1,*] += (obj.minY - obj.pixelFrame)
         vertices[2,*] += (obj.minZ - obj.pixelFrame)
      endif

      vertices[0,*] *= xyzSizePerPixel[0]
      vertices[1,*] *= xyzSizePerPixel[1]
      vertices[2,*] *= xyzSizePerPixel[2]

      oAC3D->setContour, reform(vertices[0,*]), reform(vertices[1,*]), reform(vertices[2,*]), polygons
      result = oAC3D->getAvgNeighborNodes()
      curvVec = alog(oAC3D->getCurv4() + 1)

      if ~fCalib then maxCurv = max(curvVec, min = minCurv)
      if (maxCurv gt minCurv) then begin
         curvVec -= minCurv
         curvVec /= ((maxCurv - minCurv) / 255.)
      endif

      vert_color = make_array(4, (size(vertices, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*self.pVolState).rgbValues[0,*,*])
      opacVect = self->getOpacVect('1st Volume Opacity')

      vert_color[0,*] = (rgb_table0[*,0])[curvVec]
      vert_color[1,*] = (rgb_table0[*,1])[curvVec]
      vert_color[2,*] = (rgb_table0[*,2])[curvVec]
      vert_color[3,*] = opacVect[curvVec]

      objNumber = (self->get(position = i))->getNumber()
      if fUpDateROI3DGroupProperties then begin
         oObj = oObjectModel->GetByName (strCompress('3DCurvSurfModel:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
      endif else begin

         result = compute_mesh_normals(vertices, polygons)
         result[0,*] /= xyzSizePerPixel[0]
         result[1,*] /= xyzSizePerPixel[1]
         result[2,*] /= xyzSizePerPixel[2]

         vertices[0,*] /= xyzSizePerPixel[0]
         vertices[1,*] /= xyzSizePerPixel[1]
         vertices[2,*] /= xyzSizePerPixel[2]

         ; set this value to adjust the interval for normal vector "sampling" in the 3D model
         stepVec = 1
         for k = 0L, (size(result, /dim))[1]-1, stepVec do $
           oObjectModel->add, obj_new('IDLgrPolyline', transpose([transpose(vertices[*,k]),transpose(vertices[*,k] + result[*,k])]), color = [255,255,255],$
                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                       name = strCompress('3DCurvSurfPoly:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')

         oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = polygons, vert_colors = vert_color, shininess = 128., shading = 1,$
                                                    xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                    name = strCompress('3DCurvSurfModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
      endelse
   endfor
   obj_destroy, oAC3D
end

pro C_sROI3DGroupObj::getoSphereProjectionModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
   widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
      fMovieKeepSpherePosition = stateObj.fMovieKeepSpherePosition
      xyzrSpherePos = stateObj.xyzrSpherePos
   widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy

   xyzDim = self->getxyzDim()
   xyzFrameRealSize = self->getxyzRealSize()
   xyzPixSize = self->getxyzSizePerPixel()

   xyzFact = xyzPixSize[0] / xyzPixSize[2]

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   nObj = self->count()

   oSourceModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if ~obj_valid(oSourceModel) then oSourceModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if ~obj_valid(oSourceModel) then oSourceModel = *poCurrROIGraphicModel->getByName('3D Surface Model')

   if ~fMovieKeepSpherePosition then begin
      randomTime0 = sysTime(1)

         ; choose 4 equidistant objects
      objStep = nObj / 4.1
      objPos = 0
      objVect = [objPos]
      for i = 1, 3 do begin
         objPos += objStep
         objVect = [objVect, floor(objPos)]
      endfor

         ; create C_sActiveContour3D
      oAC3D = obj_new('C_sActiveContour3D', bytArr(3,3,3))

         ; number of random samples
      nSamples = 400
      xCenter = fltArr(nSamples)
      yCenter = fltArr(nSamples)
      zCenter = fltArr(nSamples)
      radius = fltArr(nSamples)

         ; define 4 random  xyz-positions
      xyzPos = fltArr(3,4)

      for j = 0L, nSamples-1 do begin

        for i = 0, 3 do begin
           if ~obj_valid(oSourceModel) then xyzPoints = (self->get(position = objVect[i]))->getxyzPoints() $
              else (self->get(position = objVect[i]))->getProperty, data = xyzPoints

           nDim = size(xyzPoints, /dim)
           if (n_elements(nDim) gt 1) then begin
              randomPos = round(randomu(sysTime(1) - randomTime0 + memory(/current) + j * i, /uni) * ((nDim[1]-1)>0))
              xyzPos[*,i] = xyzPoints[*,randomPos]
           endif else xyzPos[*,i] = xyzPoints
        endfor

       ; plot selected ramdom points
;       oObjectModel->add, obj_new('IDLgrPolygon', alpha_channel = .3, ambient = [0,0,0], data = xyzPos, poly = [4,0,1,2,3], bottom = color, color = color, shading = 1,$
;                                                  xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, name = strCompress('3DSphereModel:'+string(i),/rem))

        objVect = (objVect + 1) mod (nObj-1)

        oAC3D->computeCurv4, xyzPos[0,*] * xyzPixSize[0], xyzPos[1,*] * xyzPixSize[1], xyzPos[2,*] * xyzPixSize[2], xC, yC, zC, r

        xC /= xyzPixSize[0]
        yC /= xyzPixSize[1]
        zC /= xyzPixSize[2]
        r /= xyzPixSize[0]

;        oOrb = obj_new('C_sOrb', pos = [xC, yC, zC], color = [255,0,0], radius = r, shading = 1, select_target = 1, xyzFact = xyzFact, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
;        oObjectModel->add, oOrb

        xCenter[j] = [xC]
        yCenter[j] = [yC]
        zCenter[j] = [zC]
        radius[j] = [r]
      endfor
      obj_destroy, oAC3D

      sortRad = sort(radius)
      cutRad = floor(nSamples * .4)
      radius = (radius[sortRad])[cutRad : nSamples - cutRad]
      xCenter = (xCenter[sortRad])[cutRad : nSamples - cutRad]
      yCenter = (yCenter[sortRad])[cutRad : nSamples - cutRad]
      zCenter = (zCenter[sortRad])[cutRad : nSamples - cutRad]

;     for i = 0L, n_elements(xCenter)-1 do begin
;         oOrb = obj_new('C_sOrb', pos = [xCenter[i], yCenter[i], zCenter[i]], color = [255,0,0], alpha_channel = .2,$
;         radius = radius[i], shading = 1, select_target = 1, xyzFact = xyzFact, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
;         oObjectModel->add, oOrb
;     endfor

      xyzrSpherePos = [(moment(xCenter))[0], (moment(yCenter))[0], (moment(zCenter))[0], (moment(radius))[0],$
                       (moment(xCenter))[1], (moment(yCenter))[1], (moment(zCenter))[1], (moment(radius))[1]]

      widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
         stateObj.xyzrSpherePos = xyzrSpherePos
      widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy

      openW,2, strCompress(s_getPathForSystem()+strcompress('_sphereData_XYZCenter_Rad_andSDs.dat', /rem))
      printF, 2, transpose(xyzrSpherePos)
      close, 2
   endif else begin
      xyzrSpherePos = make_array(8, /float)
      openR,2, strCompress(s_getPathForSystem()+strcompress('_sphereData_XYZCenter_Rad_andSDs.dat', /rem))
      readF, 2, xyzrSpherePos
      close, 2
   endelse   ; fMovieKeepSpherePosition

   radius = xyzrSpherePos[3]
   print, ' Fitted Sphere :'
   print, ' x-center :', xyzrSpherePos[0], xyzrSpherePos[4]
   print, ' y-center :', xyzrSpherePos[1], xyzrSpherePos[5]
   print, ' z-center :', xyzrSpherePos[2], xyzrSpherePos[6]
   print, ' radius :', xyzrSpherePos[3], xyzrSpherePos[7]
   print, ' Total sphere surface area:', 4. * !pi  * radius * radius * xyzPixSize[0] * xyzPixSize[0]

   oObjectModel->add, obj_new('C_sOrb', pos = [xyzrSpherePos[0], xyzrSpherePos[1], xyzrSpherePos[2]], color = [100, 100, 100], radius = radius, shading = 1, alpha_channel = .25,$
                                        name = strCompress('3DOrbModel',/rem),$
                                        select_target = 1, xyzFact = xyzFact, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)

   if obj_valid(oSourceModel) then begin

      nSelf = self->count()
      nObj = oSourceModel->count()
         ; create C_sActiveContour3D to surface area
      oAC3D = obj_new('C_sActiveContour3D', bytArr(3,3,3))
      totalSurfArea = 0.
      for i = 0L, nObj-1 do begin
         obj = oSourceModel ->get(position = i)
         if obj_valid(obj) then begin

            (oSourceModel->get(position = i<(nSelf-1)))->getProperty, data = xyzPoints, polygons = polygons

                  ; project points on sphere
            for j = 0L, (size(xyzPoints, /dim))[1] - 1 do begin
               xyzVect = xyzPoints[*, j] - xyzrSpherePos[0:2]
               xyzPoints[*, j] = xyzrSpherePos[0:2] + xyzVect * (radius / sqrt(total((xyzVect / [1.,1.,xyzFact])^2)))

;               if ((j mod 90) eq 1) then oObjectModel->add, obj_new('IDLgrPolyline', alpha_channel = 1., transpose([transpose(xyzrSpherePos[0:2]), transpose(xyzrSpherePos[0:2] + xyzVect)]),$
;                                                     color = [255,0,0], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
            endfor

            (oSourceModel->get(position = i<(nSelf-1)))->setProperty, data = xyzPoints, poly = polygons, alpha_channel = 1., bottom = color, color = color

            opacVal = 255
;            print, 'Red polygon = original polygon: '
;            oObjectModel->add, obj_new('IDLgrPolygon', xyzPoints, polygons = polygons, alpha_channel = opacVal/255., style = 2,$
;                                        color = [255,0,0], bottom = [0,0,0], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)

            xyzPoints[0,*] *= xyzPixSize[0]
            xyzPoints[1,*] *= xyzPixSize[1]
            xyzPoints[2,*] *= xyzPixSize[2]

            oAC3D->SetContour, xyzPoints[0,*], xyzPoints[1,*], xyzPoints[2,*], polygons
            surfArea = oAC3D->getContourArea()
            print, 'Surface-Area with oAC3D->SetContour:', surfArea/2.
            totalSurfArea += (surfArea / 2.)
            print, '__________________________________________'

;            xyzPoints[0,*] /= xyzPixSize[0]
;            xyzPoints[1,*] /= xyzPixSize[1]
;            xyzPoints[2,*] /= xyzPixSize[2]
;
;;                   project points into xyzFrame in order to clean redundant vertices and shruggs
;            xyzFrame = make_array(xyzDim, /byte)
;            xyzFrame[round(xyzPoints[0,*]), round(xyzPoints[1,*]), round(xyzPoints[2,*])] = 1b
;
;                  ; make new vertices and polygons
;            shade_volume, xyzFrame, 0, xyzPoints, polygons, /low
;            xyzFrame = 0
;
;                  ; project points on sphere
;            for j = 0L, (size(xyzPoints, /dim))[1] - 1 do begin
;               xyzVect = xyzPoints[*, j] - xyzrSpherePos[0:2]
;               xyzPoints[*, j] = xyzrSpherePos[0:2] + xyzVect * (radius / sqrt(total((xyzVect / [1.,1.,xyzFact])^2)))
;            endfor
;
;            z = where(xyzPoints[2,*] ge 30)
;            zz = where(xyzPoints[2,z] le 31)
;            iplot, xyzPoints[0,z[zz]], xyzPoints[1,z[zz]], /scatter, linestyle = 0, color = [0,255,0]
;            print, 'Green polygon = round projected polygon: '
;            xyzPoints[0,*] += 150
;            oObjectModel->add, obj_new('IDLgrPolygon', xyzPoints, polygons = polygons, alpha_channel = opacVal/255., style = 2,$
;                                        color = [0,255,0], bottom = [0,0,0], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
;            xyzPoints[0,*] -= 150
;
;            xyzPoints[0,*] *= xyzPixSize[0]
;            xyzPoints[1,*] *= xyzPixSize[1]
;            xyzPoints[2,*] *= xyzPixSize[2]
;
;            oAC3D->SetContour, xyzPoints[0,*], xyzPoints[1,*], xyzPoints[2,*], polygons
;            surfArea = oAC3D->getContourArea()
;            print, 'Surface-Area with oAC3D->SetContour:', surfArea/2.
;            totalSurfArea += (surfArea / 2.)
;            print, '__________________________________________'
;
;            xyzPoints[0,*] /= xyzPixSize[0]
;            xyzPoints[1,*] /= xyzPixSize[1]
;            xyzPoints[2,*] /= xyzPixSize[2]
;
;;                   project points into xyzFrame in order to clean redundant vertices and shruggs
;            xyzFrame = make_array(xyzDim, /byte)
;            xyzFrame[(xyzPoints[0,*]), (xyzPoints[1,*]), (xyzPoints[2,*])] = 1b
;
;                  ; make new vertices and polygons
;            shade_volume, xyzFrame, 0, xyzPoints, polygons, /low
;            xyzFrame = 0
;
;                  ; project points on sphere
;            for j = 0L, (size(xyzPoints, /dim))[1] - 1 do begin
;               xyzVect = xyzPoints[*, j] - xyzrSpherePos[0:2]
;               xyzPoints[*, j] = xyzrSpherePos[0:2] + xyzVect * (radius / sqrt(total((xyzVect / [1.,1.,xyzFact])^2)))
;            endfor
;
;            z = where(xyzPoints[2,*] ge 30)
;            zz = where(xyzPoints[2,z] le 31)
;            iplot, xyzPoints[0,z[zz]], xyzPoints[1,z[zz]], /scatter, linestyle = 0, color = [0,0,255]
;            print, 'Blue polygon = projected polygon: '
;            xyzPoints[0,*] += 300
;            oObjectModel->add, obj_new('IDLgrPolygon', xyzPoints, polygons = polygons, alpha_channel = opacVal/255., style = 2,$
;                                        color = [0,0,255], bottom = [0,0,0], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
;            xyzPoints[0,*] -= 300
;
;
;            xyzPoints[0,*] *= xyzPixSize[0]
;            xyzPoints[1,*] *= xyzPixSize[1]
;            xyzPoints[2,*] *= xyzPixSize[2]
;
;            oAC3D->SetContour, xyzPoints[0,*], xyzPoints[1,*], xyzPoints[2,*], polygons
;            surfArea = oAC3D->getContourArea()
;            print, 'Surface-Area with oAC3D->SetContour:', surfArea/2.
;            totalSurfArea += (surfArea / 2.)
;            print, '__________________________________________'

            xyzPoints[0,*] /= xyzPixSize[0]
            xyzPoints[1,*] /= xyzPixSize[1]
            xyzPoints[2,*] /= xyzPixSize[2]

            result = compute_mesh_normals(xyzPoints, polygons)
            result[0,*] /= xyzPixSize[0]
            result[1,*] /= xyzPixSize[1]
            result[2,*] /= xyzPixSize[2]

               ; normalize again
            normRes = sqrt((result[0,*] * result[0,*] + result[1,*] * result[1,*] + result[2,*] * result[2,*]))

            result[0,*] /= normRes
            result[1,*] /= normRes
            result[2,*] /= normRes
            result[2,*] *= xyzFact

            xyzPoints[0,*] /= xyzPixSize[0]
            xyzPoints[1,*] /= xyzPixSize[1]
            xyzPoints[2,*] /= xyzPixSize[2]

            result *= 10

            ; detect tangents on shpere
;            xyzProj = xyzPoints * 0.
;            xyzDist = fltArr((size(xyzPoints, /dim))[1])
;            for j = 0L, (size(xyzPoints, /dim))[1] - 1 do begin
;               xyzVect = xyzPoints[*, j] + result[*, j] - xyzrSpherePos[0:2]
;               if ((j mod 90) eq 1) then oObjectModel->add, obj_new('IDLgrPolyline', alpha_channel = 1., transpose([transpose(xyzPoints[*,j]), transpose(xyzPoints[*,j]+result[*,j])]),$
;                                                     color = [255,0,255], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
;               xyzProj[*, j] = xyzrSpherePos[0:2] + xyzVect *(radius /(sqrt(total((xyzVect / [1., 1., xyzFact])^2))))
;               if ((j mod 90) eq 1) then oObjectModel->add, obj_new('IDLgrPolyline', alpha_channel = 1., transpose([transpose(xyzrSpherePos[0:2]), transpose(xyzProj[*, j])]),$
;                                                     color = [255,255,0], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
;               xyzDist[j] = sqrt(total((xyzPoints[*, j] - xyzProj[*, j])^2))
;            endfor
;
;            (self->get(position = i))->getProperty, color = color, alpha_channel = alpha
;            objNumber = (self->get(position = i))->getNumber()
;             oObjectModel->add, obj_new('IDLgrPolygon', alpha_channel = alpha, ambient = [0,0,0], data = xyzPoints, poly =polygons,$
;                                                        bottom = color, color = color, shading = 1,$
;                                                        xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
;                                                        name = strCompress('3DSphereModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')

;             opacVal = 255
;             oObjectModel->add, obj_new('IDLgrPolygon', alpha_channel = opacVal/255., ambient = [0,0,0], data = xyzPoints, poly =polygons, bottom = color, color = color, shading = 1,$
;                                                     xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)

;             whereBig = where(xyzDist gt 5, count)
;             if count gt 0 then $
;                 for j = 0L, count-1 do oObjectModel->add, obj_new('IDLgrPolyline', alpha_channel = 1., transpose([transpose(xyzPoints[*, whereBig[j]]), transpose(xyzPoints[*,whereBig[j]]+result[*,whereBig[j]])]),$
;                                                     color = [255,255,255], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)

;             oObjectModel->add, obj_new('IDLgrROI', alpha_channel = 1., xyzPoints[*,whereBig],$
;                                                     color = [255,255,255], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)


;             for k = 0, n_elements(whereBig)-2 do oObjectModel->add, obj_new('IDLgrPolyline', alpha_channel = 1., transpose([transpose(xyzPoints[*,whereBig[k]]), transpose(xyzPoints[*,whereBig[k+1]])]),$
;                                                     color = [0,255,255], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)

;            radius = (moment(radius))[0]
;            xyzrSpherePos[0:2] = [(moment(xCenter))[0], (moment(yCenter))[0], (moment(zCenter))[0]]
;            for k = 0L, (size(result, /dim))[1]-1, 10 do oObjectModel->add, obj_new('IDLgrPolyline', alpha_channel = 1., xyzPoints[*,k], xyzPoints[*,k]+result[*,k],$
;                                                     color = [0,0,255], xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
         endif
      endfor
      print, 'Total Surface-Area =', totalSurfArea
      obj_destroy, oAC3D
   endif
end

pro C_sROI3DGroupObj::getoPC, oObjectModel, stack_tlb = stack_tlb

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = self->getxyzDim()

   xyzSPPNorm = (*self.pEigenSys).sizePerXYZ / max((*self.pEigenSys).sizePerXYZ)

   
   colEV = [0,0,0]
   colEV2 = [223,165,0]
   colEV3 = [0,128,64]
   
   scaleAxis = 0.3
   thickEV = 3.
   ;colEV = [1,1,1]

   for i = 0L, (self->count())-1 do begin
         objNumber = (self->get(position = i))->getNumber()
         scaleEV = ((*((self->get(position = i))->getpEigenSys())).eigenVals) / max(((*((self->get(position = i))->getpEigenSys())).eigenVals)) * scaleAxis
         scaleEV = [scaleAxis, scaleAxis, scaleAxis]
         axis = fltArr(3,2)

   ;      axis[*,0] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[0,*] * (-scaleEV[0]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[0,*] * (scaleEV[0]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,0] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((self->get(position = i))->getpEigenSys())).eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,1] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[0,*] * ((*((self->get(position = i))->getpEigenSys())).eigenVals)[0] * (scaleEV[0]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                    name = strCompress('3D1stPC_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   ;      axis[*,0] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[1,*] * (-scaleEV[1]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[1,*] * (scaleEV[1]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,0] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[1,*] * ((*((self->get(position = i))->getpEigenSys())).eigenVals)[1] * (-scaleEV[1]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,1] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[1,*] * ((*((self->get(position = i))->getpEigenSys())).eigenVals)[1] * (scaleEV[1]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV2, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                    name = strCompress('3D2ndPC_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   ;      axis[*,0] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[2,*] * (-scaleEV[2]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
   ;      axis[*,1] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[2,*] * (scaleEV[2]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,0] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[2,*] * ((*((self->get(position = i))->getpEigenSys())).eigenVals)[2] * (-scaleEV[2]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         axis[*,1] = ((*((self->get(position = i))->getpEigenSys())).eigenVect)[2,*] * ((*((self->get(position = i))->getpEigenSys())).eigenVals)[2] * (scaleEV[2]) / xyzSPPNorm + (*((self->get(position = i))->getpEigenSys())).centerXYZ
         oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV3, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                    name = strCompress('3D3rdPC_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
   endfor
end

pro C_sROI3DGroupObj::getoPABoxModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

   nObjects = self->count()
   if (nObjects lt 1) then return

   oColorModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if ~obj_valid(oColorModel) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
   if ~obj_valid(oColorModel) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oColorModel) then fOk = 1b else fOk = 0b

   thickEV = 3.
   colEV = [0,0,255]
   for i = 0L, nObjects-1 do begin
      objNumber = (self->get(position = i))->getNumber()

      if fOk then begin
         oObj = oColorModel->get(position = i)
         if obj_valid(oObj) then begin
            oObj->getProperty, data = xyzPoints, poly = polygons, color = colEV
            fCOOk = 1b
         endif else fCOOk = 0b
      endif else fCOOk = 0b

      if fCOOk then evProj = (self->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (self->get(position = i))->getEVSProj()

      (self->get(position = i))->getProperty, all = prop
      oObjectModel->add, obj_new('IDLgrPolyline', reform((evProj.evMinMax)[*,0:1]), thick = thickEV, color = colEV, name = strCompress('3D1EVBox_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                             double = prop.double, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)

      oObjectModel->add, obj_new('IDLgrPolyline', reform((evProj.evMinMax)[*,2:3]), thick = thickEV, color = colEV, name = strCompress('3D1EVBox_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                             double = prop.double, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)

      oObjectModel->add, obj_new('IDLgrPolyline', reform((evProj.evMinMax)[*,4:5]), thick = thickEV, color = colEV, name = strCompress('3D1EVBox_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                             double = prop.double, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)

         ;verts = [[-0.1,-0.1,-0.1],[0.1,-0.1,-0.1],[0.1,0.1,-0.1], [-0.1,0.1,-0.1],[-0.1,-0.1, 0.1],[0.1,-0.1, 0.1],[0.1,0.1, 0.1], [-0.1,0.1, 0.1]]
      conn = [[4,3,2,1,0],[4,4,5,6,7],[4,0,1,5,4],[4,1,2,6,5], [4,2,3,7,6],[4,3,0,4,7]]
      oObjectModel->add, obj_new('IDLgrPolygon', evProj.sqProj, poly = conn, shading = 1, shininess = 128., color = colEV, alpha_channel = 0.5, name = strCompress('3DEVSBox_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                 double = prop.double, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
   endfor
end


pro C_sROI3DGroupObj::getoSurfDistanceModel, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

  nObjects = self->count()
  if (nObjects lt 1) then return

  oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
  if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
  if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
  if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b
  xyzSizePerPixel = self->getxyzSizePerPixel()
  xyzDim = self->getxyzDim()

  fCalcBack = 0b
  if fSurf then begin
    oObj = oSurfModel->get(position = 0)
    if obj_valid(oObj) then begin
      oObj->getProperty, uvalue = uvalue
      if (uvalue eq 'ObjInGalleryPosition') then begin
        self->calcObjPos, oSurfModel, sDirection = 'calcOriginalPosition'
        fCalcBack = 1b
      endif
    endif
  endif

  pBP = ptrArr(nObjects)
    ; collect all object borders

  ; TODO JJ insert AABB indexing code for faster computation.
  fAABBs = 0 ; Set to 1 to enable Axis-Aligned Bounding Boxes to speed-up computation for many large objects.
  if fAABBs then objAABBlist = [-1.0]

  for i = 0L, nObjects-1 do begin
    if obj_valid(oSurfModel) then oPoly = oSurfModel->get(position = i)
    if obj_valid(oPoly) then begin
      oPoly->getProperty, data = xyzPoints
      xyzBP = fltArr(6, (size(xyzPoints, /dim))[1])
      xyzBP[0,*] = xyzPoints[0,*] * xyzSizePerPixel[0]
      xyzBP[1,*] = xyzPoints[1,*] * xyzSizePerPixel[1]
      xyzBP[2,*] = xyzPoints[2,*] * xyzSizePerPixel[2]
    endif else begin
      (self->get(position = i))->getProperty, data = xyzPoints
      minX = min(xyzPoints[0,*])
      minY = min(xyzPoints[1,*])
      minZ = min(xyzPoints[2,*])
      xyzObjectBorderObject = (self->get(position = i))->makeObjectBorderInVoxel()
      wherePoints = where(xyzObjectBorderObject ne 0)
      szOBO = size(xyzObjectBorderObject, /dim)
      xyzBP = fltArr(6, n_elements(wherePoints)) ; 2018.11.27 JJ fix: the old version made fltArr(5, n_elements(wherePoints))... why?!
      xyzBP[0,*] = wherePoints mod szOBO[0]
      xyzBP[1,*] = floor((wherePoints mod (szOBO[0] * szOBO[1])) / szOBO[0])
      xyzBP[2,*] = floor(wherePoints / (szOBO[0] * szOBO[1]))

      xyzBP[0,*] += (minX - min(xyzBP[0,*]))
      xyzBP[1,*] += (minY - min(xyzBP[1,*]))
      xyzBP[2,*] += (minZ - min(xyzBP[2,*]))

      xyzBP[0,*] *= xyzSizePerPixel[0]
      xyzBP[1,*] *= xyzSizePerPixel[1]
      xyzBP[2,*] *= xyzSizePerPixel[2]
    endelse
    xyzBP[3,*] = sqrt((xyzDim[0]*xyzSizePerPixel[0])^2 + (xyzDim[1]*xyzSizePerPixel[1])^2 + (xyzDim[2]*xyzSizePerPixel[2])^2)

    if fAABBs then begin
      xMin = min(xyzBP[0,*], max = xMax)
      yMin = min(xyzBP[0,*], max = yMax)
      zMin = min(xyzBP[2,*], max = zMax)
      objAABBlist = [objAABBlist, xMin, xMax, yMin, yMax, zMin, zMax]
    endif

    pBP[i] = ptr_new(xyzBP, /no_copy)
  endfor
  if fAABBs then objAABBlist = objAABBlist[1:*]
    ; calculate distances
  limit = 1.5
  for i = 0L, nObjects-1 do begin

    objNumber = (self->get(position = i))->getNumber()
    strOnum = string(objNumber)
    uValObj = 'ObjInOriginalPosition'

    (self->get(position = i))->getProperty, all = prop
    nPts = (size(*pBP[i], /dim))[1]
    minAllDist = sqrt((xyzDim[0]*xyzSizePerPixel[0])^2 + (xyzDim[1]*xyzSizePerPixel[1])^2 + (xyzDim[2]*xyzSizePerPixel[2])^2)

    for j = 0L, nObjects-1 do begin
      if (i ne j) then begin
        xyzLimit = [-1,-1,-1]

        for k = 0L, nPts-1 do begin
          xyzDist = sqrt(((*pBP[i])[0,k] - (*pBP[j])[0,*])^2 + ((*pBP[i])[1,k] - (*pBP[j])[1,*])^2 + ((*pBP[i])[2,k] - (*pBP[j])[2,*])^2)
          minDist = min(xyzDist, whereMin)
          if (minDist lt minAllDist) then begin
            (*pBP[i])[3:5,k] = [minDist, j, whereMin]
            minAllDist <= minDist
          endif
          if (minDist le limit) then xyzLimit = transpose([transpose(xyzLimit), transpose((*pBP[i])[0:2,k] / xyzSizePerPixel)])
        endfor

          ; plot cell borders with distance < limit
        if (n_elements(xyzLimit) gt 3) then begin
          oObjectModel->add, obj_new('IDLgrROI', data = xyzLimit[*,1:*], name = strCompress('3DTouchSurf_:' + strOnum, /rem), uvalue = uValObj, style = 0,$
                                     thick = 1, color = [255,255,0], xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
          xyzLimit = [-1,-1,-1]
        endif
      endif
    endfor

      ; plot smallest distance between cells
    a = min((*pBP[i])[3,*], whereMin)
    xyz1 = (*pBP[i])[0:2,whereMin] / xyzSizePerPixel
    xyz2 = (*pBP[(*pBP[i])[4,whereMin]])[0:2,(*pBP[i])[5,whereMin]] / xyzSizePerPixel
    lColor = [0,255,0]
    oObjectModel->add, obj_new('IDLgrROI', data = xyz1, symbol = oSymbol, name = strCompress('3DConnectTouchSurfI_:' + strOnum,/ rem), uvalue = uValObj,$
                                           thick = 3, color = lColor, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    oObjectModel->add, obj_new('IDLgrROI', data = xyz2, symbol = oSymbol, name = strCompress('3DConnectTouchSurfII_:' + strOnum, /rem), uvalue = uValObj,$
                                           thick = 3, color = lColor, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
    oObjectModel->add, obj_new('IDLgrPolyline', data = transpose([transpose(xyz1), transpose(xyz2)]), name = strCompress('3DConnectTouchSurfIII_:' + strOnum, /rem), uvalue = uValObj,$
                                                thick = 1, color = lColor, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  endfor
  for i = nObjects-1, 0, -1 do ptr_free, pBP[i]
  if fCalcBack then self->calcObjPos, oSurfModel, sDirection = 'calcGalleryPosition'
end


pro C_sROI3DGroupObj::getoSurfCentDistI, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties

   if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
   oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b

      ; of / off normalize all distances to min/max
;   for i = 0L, (self->count())-1 do begin
;      maxDistP = 0.
;      minDistP = 0.
;      if fSurf then begin
;         oObj = oSurfModel->get(position = i)
;         if obj_valid(oObj) then begin
;            oObj->getProperty, data = xyzPoints
;            fCOOk = 1b
;         endif else fCOOk = 0b
;      endif else fCOOk = 0b
;
;      if fCOOk then evProj = (self->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (self->get(position = i))->getEVSProj()
;      if fCOOk then oObj->getProperty, data = xyzPoints else (self->get(position = i))->getProperty, data = xyzPoints
;      if fCOOk then xyzDist = (self->get(position = i))->getxyzDist(xyzPoints = xyzPoints, xyzCenter = (*self.pEigenSys).centerXYZ) else xyzDist = (self->get(position = i))->getxyzDist(xyzCenter = (*self.pEigenSys).centerXYZ)
;
;      maxDist = max(xyzDist, min = minDist)
;      maxDistP >= maxDist
;      minDistP <= minDist
;      print, 'minDist', minDistP, ' maxDist', maxDistP
;   endfor

   fCalcBack = 0b
   if fSurf then begin
      oObj = oSurfModel->get(position = 0)
      if obj_valid(oObj) then begin
         oObj->getProperty, uvalue = uvalue
         if (uvalue eq 'ObjInGalleryPosition') then begin
            self->calcObjPos, oSurfModel, sDirection = 'calcOriginalPosition'
            fCalcBack = 1b
         endif
      endif
   endif

   for i = 0L, (self->count())-1 do begin
      objNumber = (self->get(position = i))->getNumber()

      fCOOk = 0b
      if fSurf then begin
         oObj = oSurfModel->get(position = i)
         if obj_valid(oObj) then begin
            oObj->getProperty, data = xyzPoints, poly = polygons
            fCOOk = 1b
         endif
      endif

      if fCOOk then evProj = (self->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (self->get(position = i))->getEVSProj()
      if fCOOk then oObj->getProperty, data = xyzPoints else (self->get(position = i))->getProperty, data = xyzPoints
      if fCOOk then xyzDist = (self->get(position = i))->getxyzDist(xyzPoints = xyzPoints, xyzCenter = (*self.pEigenSys).centerXYZ) else xyzDist = (self->get(position = i))->getxyzDist(xyzCenter = (*self.pEigenSys).centerXYZ)

      maxDist = max(xyzDist, min = minDist)
      whereMin = (where(xyzDist eq minDist))[0]
      whereMax = (where(xyzDist eq maxDist))[0]
      print, 'minDist', minDist, ' maxDist', maxDist
      if (n_elements(minDistP) gt 0) then begin
         xyzDist -= minDistP
         xyzDist /= (maxDistP-minDistP)
      endif else begin
         xyzDist -= minDist
         xyzDist /= (maxDist-minDist)
      endelse
      xyzDist = reform(xyzDist)

      vert_color = make_array(4, (size(xyzPoints, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*self.pVolState).rgbValues[0,*,*])
      opacVect = self->getOpacVect('1st Volume Opacity')
      colorIndex = round(xyzDist * 255)
      vert_color[0,*] = (rgb_table0[*,0])[colorIndex]
      vert_color[1,*] = (rgb_table0[*,1])[colorIndex]
      vert_color[2,*] = (rgb_table0[*,2])[colorIndex]
      vert_color[3,*] = opacVect[colorIndex]

      (self->get(position = i))->getProperty, all = prop

      if fUpDateROI3DGroupProperties then begin
         oObj = oObjectModel->GetByName (strCompress('3DSurfDist_:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
         oObj = oObjectModel->GetByName (strCompress('3DSurfVec_:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
      endif else begin

;            ; mark min distance on principal axis
;         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [255,0,0])
;         if fCOOk then oObjectModel->add, obj_new('IDLgrROI', data = reform((evProj.evsProj)[0,*,[whereMin]]), symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark min distance on surface
         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [0,255,0])
         oObjectModel->add, obj_new('IDLgrROI', data = xyzPoints[*,whereMin], symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark center with a yellow cross
         oSymbol = obj_new('IDLgrSymbol', data = 7, thick = 3, size = 5, color = [255,255,0])
         oObjectModel->add, obj_new('IDLgrROI', (*self.pEigenSys).centerXYZ, symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; mark lines from center to cells
         oObjectModel->add, obj_new('IDLgrPolyline', data = transpose([transpose((*self.pEigenSys).centerXYZ), transpose(xyzPoints[*,whereMin])]), uvalue = 'ObjInOriginalPosition',$
                                                              color = [255,255,0], thick = 2, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
            ; plot principal axis
         if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[0,*,*]), vert_colors = vert_color[*,*], name = strCompress('3DSurfVec_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                                   xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[1,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
  ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[2,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
  ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
;            ; plot surface
         if fCOOk and (total(vert_color[3,*]) gt 0) then oObjectModel->add, obj_new('IDLgrPolygon', shading = 1, shininess = 128., data = xyzPoints, poly = polygons, vert_colors = vert_color,$
                                                                  shade_range = [0,255], name = strCompress('3DSurfDist_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                                  xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
      endelse
   endfor
   if fCalcBack then self->calcObjPos, oSurfModel, sDirection = 'calcGalleryPosition'
end


pro C_sROI3DGroupObj::getoSurfCentDistII, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel, fUpDateROI3DGroupProperties = fUpDateROI3DGroupProperties

   if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
   oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Model')
   if ~obj_valid(oSurfModel) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b

   fCalcBack = 0b
   if fSurf then begin
      oObj = oSurfModel->get(position = 0)
      if obj_valid(oObj) then begin
         oObj->getProperty, uvalue = uvalue
         if (uvalue eq 'ObjInGalleryPosition') then begin
            self->calcObjPos, oSurfModel, sDirection = 'calcOriginalPosition'
            fCalcBack = 1b
         endif
      endif
   endif

   for i = 0L, (self->count())-1 do begin
      objNumber = (self->get(position = i))->getNumber()

      fCOOk = 0b
      if fSurf then begin
         oObj = oSurfModel->get(position = i)
         if obj_valid(oObj) then begin
            oObj->getProperty, data = xyzPoints, poly = polygons
            fCOOk = 1b
         endif
      endif

      if fCOOk then evProj = (self->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (self->get(position = i))->getEVSProj()
      if fCOOk then oObj->getProperty, data = xyzPoints else (self->get(position = i))->getProperty, data = xyzPoints
      if fCOOk then xyzDist = (self->get(position = i))->getxyzDist(xyzPoints = xyzPoints, xyzCenter = (*self.pEigenSys).centerXYZ) else xyzDist = (self->get(position = i))->getxyzDist(xyzCenter = (*self.pEigenSys).centerXYZ)

         ; split objects into three parts along the first principal axis
      ev1Dist = (self->get(position = i))->getxyzDist(xyzPoints = reform((evProj.evsProj)[0,*,*]), xyzCenter = evProj.evminmax[*,0])
      maxev1Dist = max(ev1Dist)
      where1 = where(ev1Dist le (maxev1Dist/3.))
      where3 = where(ev1Dist ge (maxev1Dist/3.*2.))

      dist1 = (moment((self->get(position = i))->getxyzDist(xyzPoints = xyzPoints[*,where1], xyzCenter = (*self.pEigenSys).centerXYZ)))[0]
      dist3 = (moment((self->get(position = i))->getxyzDist(xyzPoints = xyzPoints[*,where3], xyzCenter = (*self.pEigenSys).centerXYZ)))[0]

      maxDist = max(xyzDist, min = minDist)
      whereMin = (where(xyzDist eq minDist))[0]
      whereMax = (where(xyzDist eq maxDist))[0]
      print, 'minDist', minDist, ' maxDist', maxDist

      vert_color = make_array(4, (size(xyzPoints, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*self.pVolState).rgbValues[0,*,*])
      opacVect = self->getOpacVect('1st Volume Opacity')

         ; paint object with center-value of the color table
      vert_color[0,*] = (rgb_table0[*,0])[127]
      vert_color[1,*] = (rgb_table0[*,1])[127]
      vert_color[2,*] = (rgb_table0[*,2])[127]
      vert_color[3,*] = opacVect[127]

      if dist1 le dist3 then colorVec = [42,212] else colorVec = [212,42]
      vert_color[0,where1] = (rgb_table0[*,0])[colorVec[0]]
      vert_color[1,where1] = (rgb_table0[*,1])[colorVec[0]]
      vert_color[2,where1] = (rgb_table0[*,2])[colorVec[0]]
      vert_color[3,where1] = opacVect[colorVec[0]]
      vert_color[0,where3] = (rgb_table0[*,0])[colorVec[1]]
      vert_color[1,where3] = (rgb_table0[*,1])[colorVec[1]]
      vert_color[2,where3] = (rgb_table0[*,2])[colorVec[1]]
      vert_color[3,where3] = opacVect[colorVec[1]]

      (self->get(position = i))->getProperty, all = prop
      if fUpDateROI3DGroupProperties then begin

         oObj = oObjectModel->GetByName (strCompress('3DSurfDist_:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
         oObj = oObjectModel->GetByName (strCompress('3DSurfVec_:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color

      endif else begin

;            ; mark min distance on surface
;         oSymbol = obj_new('IDLgrSymbol', data = 3, thick = 3, size = 15, color = [0,255,0])
;         oObjectModel->add, obj_new('IDLgrROI', data = xyzPoints[*,whereMin], symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
;            ; mark center with a yellow cross
;         oSymbol = obj_new('IDLgrSymbol', data = 7, thick = 3, size = 5, color = [255,255,0])
;         oObjectModel->add, obj_new('IDLgrROI', (*self.pEigenSys).centerXYZ, symbol = oSymbol, uvalue = 'ObjInOriginalPosition',$
;                                                              xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
;            ; mark lines from center to cells
;         oObjectModel->add, obj_new('IDLgrPolyline', data = transpose([transpose((*self.pEigenSys).centerXYZ), transpose(xyzPoints[*,whereMin])]), uvalue = 'ObjInOriginalPosition',$
;                                                              color = [255,255,0], thick = 2, xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
;            ; plot principal axis
;         if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[0,*,*]), vert_colors = vert_color[*,*], name = strCompress('3DSurfVec_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
;                                                                   xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[1,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
  ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
  ;      if fCOOk then oObjectModel->add, obj_new('IDLgrPolyline', data = reform((evProj.evsProj)[2,*,*]), vert_colors = vert_color[*,*], uvalue = 'ObjInOriginalPosition',$
  ;                                                                xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)

            ; plot surface
         if fCOOk and (total(vert_color[3,*]) gt 0) then oObjectModel->add, obj_new('IDLgrPolygon', shading = 1, shininess = 128., data = xyzPoints, poly = polygons, vert_colors = vert_color,$
                                                                  bottom = -1, name = strCompress('3DSurfDist_:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition',$
                                                                  xCoord_conv = prop.xCoord_conv, yCoord_conv = prop.yCoord_conv, zCoord_conv = prop.zCoord_conv)
      endelse
   endfor
   if fCalcBack then self->calcObjPos, oSurfModel, sDirection = 'calcGalleryPosition'
end


pro C_sROI3DGroupObj::getoPCGroup, oObjectModel, stack_tlb = stack_tlb, poCurrROIGraphicModel = poCurrROIGraphicModel

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzSPPNorm = (*self.pEigenSys).sizePerXYZ / max((*self.pEigenSys).sizePerXYZ)
   axis = fltArr(3,2)
   ;scaleEV = [1.1, 1.1, 1.1]
   scaleEV = [.3, .5, .3]
   colEV = [200., 50., 50.]
   thickEV = 2.

   a = self->calcEigenSys()
   axis[*,0] = ((*self.pEigenSys).eigenVect)[0,*] *((*self.pEigenSys).eigenVals)[0] * (-scaleEV[0]) / xyzSPPNorm + (*self.pEigenSys).centerXYZ
   axis[*,1] = ((*self.pEigenSys).eigenVect)[0,*] *((*self.pEigenSys).eigenVals)[0] * scaleEV[0] / xyzSPPNorm + (*self.pEigenSys).centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D1stGroupPC_:'), uvalue = 'ObjInOriginalPosition')
;   axis[*,0] = ((*self.pEigenSys).eigenVect)[1,*] * (-scaleEV[1]) / xyzSPPNorm + (*self.pEigenSys).centerXYZ
;   axis[*,1] = ((*self.pEigenSys).eigenVect)[1,*] * scaleEV[1] / xyzSPPNorm + (*self.pEigenSys).centerXYZ
    axis[*,0] = ((*self.pEigenSys).eigenVect)[1,*] *((*self.pEigenSys).eigenVals)[1] * (-scaleEV[1]) / xyzSPPNorm + (*self.pEigenSys).centerXYZ
    axis[*,1] = ((*self.pEigenSys).eigenVect)[1,*] *((*self.pEigenSys).eigenVals)[1] * scaleEV[1] / xyzSPPNorm + (*self.pEigenSys).centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D2ndGroupPC_:'), uvalue = 'ObjInOriginalPosition')
;   axis[*,0] = ((*self.pEigenSys).eigenVect)[2,*] * (-scaleEV[2]) / xyzSPPNorm + (*self.pEigenSys).centerXYZ
;   axis[*,1] = ((*self.pEigenSys).eigenVect)[2,*] * scaleEV[2] / xyzSPPNorm + (*self.pEigenSys).centerXYZ
    axis[*,0] = ((*self.pEigenSys).eigenVect)[2,*] *((*self.pEigenSys).eigenVals)[2] * (-scaleEV[2]) / xyzSPPNorm + (*self.pEigenSys).centerXYZ
    axis[*,1] = ((*self.pEigenSys).eigenVect)[2,*] *((*self.pEigenSys).eigenVals)[2] * scaleEV[2] / xyzSPPNorm + (*self.pEigenSys).centerXYZ
   oObjectModel->add, obj_new('IDLgrPolyline', axis, color = colEV, thick = thickEV, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                              name = strCompress('3D3rdGroupPC_:'), uvalue = 'ObjInOriginalPosition')
end


pro C_sROI3DGroupObj::getoPaperModel, oObjectModel, stack_tlb = stack_tlb

   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   xyzDim = self->getxyzDim()

   xyzFrameRealSize = self->getxyzRealSize()


   smoothFakt = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq '2nd Cutting Plane x'))[0]] > 5
   zFakt = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq '2nd Cutting Plane y'))[0]] > 1 < 10


      ; get 1st  object and object parameters
   volData1 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   xyFract = 1. * xyzDim[1] / xyzDim[0]
   surface = make_array(xyzDim[0], xyzDim[1], /float)

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy

   nObj = self->count()
   for i = 0L, nObj-1 do begin
      xyzPoints = (self->get(position = i))->getxyzPoints()
      for j = 0L, (size(xyzPoints, /dim))[1]-1 do volData1[xyzPoints[0,j], xyzPoints[1,j], xyzPoints[2,j]] = xyzPoints[2,j]
   endfor

   volData1 *= (xyzFrameRealSize[2] / (xyzDim[2]))

   for i = 0, xyzDim[0] - 1 do begin
      for j = 0, xyzDim[1] - 1 do begin
         surface[i,j] = max(volData1[i,j,*])
;         surface[i,j] = (moment(volData1[i,j,*]))[0]
      endfor
   endfor

   surface = smooth(surface, smoothFakt, /edge, /nan)
   write_tiff, s_getPathForSystem()+'surface.tif', surface, /float

   dimI = size(surface, /dim)
   img = bytArr(4,dimI[0], dimI[1])

   ctab = bytArr(3,256) + reform((*self.pvolState).rgbValues[0,*,*])
   opacVect_0 = bytArr(256) + (*self.pvolState).opacValues[0,*]

   surface = bytScl(surface, min = 0, top = 255)
   img[0,*,*] = ctab[0,surface]
   img[1,*,*] = ctab[1,surface]
   img[2,*,*] = ctab[2,surface]
   img[3,*,*] = opacVect_0[surface]

   self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   oTextureImage = obj_new('IDLgrImage', img, loc = [0.0,0.0], hide = 1, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
   oObjectModel->add, oTextureImage

   xdata = (findgen(256)-128.0)/256.0
   ydata = (findgen(256)-128.0)/256.0
   zCoord_conv = [-1,50. * zFakt]
   oObjectModel->add, obj_new('IDLgrSurface', (surface * .00001),$
                                 xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                 shading = 1,$
                                 style = 2,$
                                 datax = xdata,$
                                 datay = ydata,$
                                 color = [255,255,255],$
                                 /texture_inter,$
                                 texture_map = oTextureImage)
end


pro C_sROI3DGroupObj::calcObjPos, oObjectModel, sDirection = sDirection

   fEVSintoXYZ = -1
   if (n_elements(sDirection) eq 0) then sDirection = 'calcGalleryPosition'
   xyzDim = self->getxyzDim()

   xyzFrameCenter = round(xyzDim/2.)
   xPos = round(xyzDim[0] / 5. * [-2, -1, 0, 1, 2])
   yStep = round(xyzDim[1] / 5.)

   selfNumberVector = self->getObjectNumberVector()
   self->getProperty, uvalue = sortVect
   for i = 0L, (oObjectModel->count()-1) do begin
      oObj = oObjectModel->get(position = i)
      case 1 of
      obj_isa(oObj,'IDLgrPolygon') or obj_isa(oObj,'IDLgrPolyline'): begin
         oObj->getProperty, uvalue = fEVSintoXYZ
         if ((sDirection eq 'calcGalleryPosition') and (fEVSintoXYZ eq 'ObjInOriginalPosition')) or $
            ((sDirection eq 'calcOriginalPosition') and (fEVSintoXYZ eq 'ObjInGalleryPosition')) or $
            ((sDirection eq 'calcGalleryPosition') and (fEVSintoXYZ eq 'ObjInGalleryPosition') and ~(sortVect.fMovieKeepGalleryPosition)) then begin
              oObj->getProperty, name = objName, data = vertices
              objNum = s_getRightNumberFromString(objName)
              objPos = where(selfNumberVector eq objNum, count)
              case count of
              1: begin
                  xyzCenter = (self->get(position = objPos))->getCenterXYZ()
                  case 1 of
                  ((sDirection eq 'calcGalleryPosition') and (fEVSintoXYZ eq 'ObjInGalleryPosition')): begin
                     o_SortPos = where(*sortVect.pOld eq objNum)
                     vertices[0,*] -= (xPos[o_SortPos[0] mod 4] - xyzCenter[0])
                     vertices[1,*] -= (floor(o_SortPos[0]/4.) * yStep - xyzCenter[1])
                     vertices[2,*] -= (xyzFrameCenter[2] - xyzCenter[2])
                     o_SortPos = where(*sortVect.pNew eq objNum)
                     vertices[0,*] += (xPos[o_SortPos[0] mod 4] - xyzCenter[0])
                     vertices[1,*] += (floor(o_SortPos[0]/4.) * yStep - xyzCenter[1])
                     vertices[2,*] += (xyzFrameCenter[2] - xyzCenter[2])
                     oObj->setProperty, data = vertices
                     sortVect.fUpdate = 1b
                  endcase
                  sDirection eq 'calcGalleryPosition': begin
                     o_SortPos = where(*sortVect.pNew eq objNum)
                     vertices = (self->get(position = objPos))->transformEVSintoXYZ(xyzPoints = vertices, sDirection = sDirection)
                     vertices[0,*] += (xPos[o_SortPos[0] mod 4] - xyzCenter[0])
                     vertices[1,*] += (floor(o_SortPos[0]/4.) * yStep - xyzCenter[1])
                     vertices[2,*] += (xyzFrameCenter[2] - xyzCenter[2])
                     oObj->setProperty, data = vertices, uvalue = 'ObjInGalleryPosition'
                     sortVect.fUpdate = 1b
                  endcase
                  sDirection eq 'calcOriginalPosition': begin
                     o_SortPos = where(*sortVect.pOld eq objNum)
                     vertices[0,*] -= (xPos[o_SortPos[0] mod 4] - xyzCenter[0])
                     vertices[1,*] -= (floor(o_SortPos[0]/4.) * yStep - xyzCenter[1])
                     vertices[2,*] -= (xyzFrameCenter[2] - xyzCenter[2])
                     vertices = (self->get(position = objPos))->transformEVSintoXYZ(xyzPoints = vertices, sDirection = sDirection)
                     oObj->setProperty, data = vertices, uvalue = 'ObjInOriginalPosition'
                  endcase
               endcase
            endcase
            else:
            endcase
         endif
      endcase
      obj_isa(oObj,'IDLGRROI'): begin
;         (oObjectModel->get(position = i))->getProperty, data = xyzPoints
;         xyzPoints = (self->get(position = i))->transformEVSintoXYZ(xyzPoints = xyzPoints)
;         xyzCenter = (self->get(position = i))->getCenterXYZ()
;         xyzPoints[0,*] += (xPos[i mod 4] - xyzCenter[0])
;         xyzPoints[1,*] += (floor(i/4.)* yStep - xyzCenter[1])
;         xyzPoints[2,*] += (xyzFrameCenter[2] - xyzCenter[2])
;         (oObjectModel->get(position = i))->setProperty, data = xyzPoints
      endcase
      else:
      endcase
   endfor
   self->setProperty, uvalue = sortVect
end


function  C_sROI3DGroupObj::getoBorderDistanceModel, color = color, nNearestNeighbour = nNearestNeighbour
   if ~keyWord_set(color) then color = [255,255,0]
   oPolygonModel = obj_new('IDLgrModel', uValue = 'oBorderDistanceModel')
   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
      ; Pass BaseObject & Holes
   oParams = self.oParamContainer->get (count = count, /all)
   k = where(*self.pActiveGroupParameterNameList eq 'Inter-Object Border Distance [pixel]')
   nObj = size((*(oParams[k[0]]->getpParamStruct())).Distance, /Dim)
   if keyWord_set(nNearestNeighbour) then begin
      for i = 0L, nObj[0]-1 do begin
         pPosition = ptr_new(sort(((*(oParams[k[0]]->getpParamStruct())).Distance)[i,*]), /no_copy)
         for j = 1, (nNearestNeighbour < n_elements(*pPosition)) do begin
         if ((*pPosition)[j] gt i) then $
            oPolygonModel->add, obj_new('IDLgrPolygon', [((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[0,i,(*pPosition)[j]],$
                                                         ((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[1,i,(*pPosition)[j]]],$
                                                        [((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[0,(*pPosition)[j],i],$
                                                         ((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[1,(*pPosition)[j],i]],$
                                                        style = 1, color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv) $
         else $
            oPolygonModel->add, obj_new('IDLgrPolygon', [((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[0,(*pPosition)[j],i],$
                                                         ((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[1,(*pPosition)[j],i]],$
                                                        [((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[0,i,(*pPosition)[j]],$
                                                         ((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[1,i,(*pPosition)[j]]],$
                                                        style = 1, color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
         endfor
         ptr_free, pPosition
       endfor
    endif else $
       for i = 0L, nObj[0]-1 do for j = i+1, nObj[0]-1 do $
          oPolygonModel->add, obj_new('IDLgrPolygon', [((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[0,i,j],$
                                                       ((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[1,i,j]],$
                                                      [((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[0,j,i],$
                                                       ((*(oParams[k[0]]->getpParamStruct())).PositionMatrix)[1,j,i]],$
                                                      style = 1, color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
   return, oPolygonModel
end


function  C_sROI3DGroupObj::getoCenterDistanceModel, color = color
   if ~keyWord_set(color) then color = [0,255,0]
   oPolygonModel = obj_new('IDLgrModel', uValue = 'o')
   self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = 'Inter-Object Center Distance')
   if obj_valid(oROIParam) then begin
      pParamStruct = oROIParam->getpParamStruct()
      pValueStruct = oROIParam->getpValueStruct(position = (where((*(*pParamStruct).pNames) eq 'Minimal Distance'))[0])
      nNearestNeighbour = ((*(*pValueStruct).pValues)[(where((*(*pValueStruct).pNames) eq 'Number of Nearest Neighbors to Consider'))[0]]) < ((size((*(*pParamStruct).pROIConnectMatrix), /dim))[1])

      for i = 0L, (size((*(*pParamStruct).pROIConnectMatrix), /dim))[0] -1 do begin
         for j = 0L, nNearestNeighbour -1 do begin
            oPolygonModel->add, obj_new('IDLgrPolygon',$
                                       [(*(*pParamStruct).pROICoordinateMatrix)[0,i],$
                                        (*(*pParamStruct).pROICoordinateMatrix)[0,(*(*pParamStruct).pROIConnectMatrix)[i,j]]],$
                                       [(*(*pParamStruct).pROICoordinateMatrix)[1,i],$
                                        (*(*pParamStruct).pROICoordinateMatrix)[1,(*(*pParamStruct).pROIConnectMatrix)[i,j]]],$
                                       style = 1, color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
         endfor
      endfor
   endif
   return, oPolygonModel
end
;_END_GET_MODEL_OBJECT_FUNCTIONS_____________________________________________________________________


;_BEGIN_OBJECT_PARAMETER_FUNCTIONS_____________________________________________________________________
function  C_sROI3DGroupObj::getParamContainer
   return, self.oParamContainer
end


pro C_sROI3DGroupObj::setParamContainer, object = object
   if obj_valid(self.oParamContainer) then begin
      oParams = (self.oParamContainer)->get(/all)
      (self.oParamContainer)->remove, /all
      if obj_valid(oParams[0]) then for i = 0L, n_elements(oParams)-1 do obj_destroy, oParams[i]

      if obj_valid(object) then begin
         oParams = object->get(/all)
         object->remove, /all
         obj_destroy, object
         if obj_valid(oParams[0]) then for i = 0L, n_elements(oParams)-1 do (self.oParamContainer)->add, oParams[i], position = i
      endif
   endif else if obj_valid(object) then self.oParamContainer = object
end

function  C_sROI3DGroupObj::getParamContainerObj, active = active
   if (active gt (self.oParamContainer->count()-1)) then active = self.oParamContainer->count()-1
   return, self.oParamContainer->get(position = active)
end

pro C_sROI3DGroupObj::addParamContainerObj, object = object
   if keyWord_set(object) then self.oParamContainer->add, object
end

pro C_sROI3DGroupObj::deleteParamContainerObj, active = active
   if (active gt (self.oSegContainer->count())) then active = self.oSegContainer->count()-1
   self.oParamContainer->remove, position = active
end


function C_sROI3DGroupObj::getSelectedROIObjFromParamName, selROIParamName = selROIParamName, position = position
   if (n_elements(selROIParamName) gt 0) then $
      for i = 0L, self.oParamContainer->count()-1 do begin
         oROIParam = self.oParamContainer->get(position = i)
         if ((*(oROIParam->getpParamStruct())).name eq selROIParamName) then return, oROIParam
         position = (where(*(*(oROIParam->getpParamStruct())).pNames eq selROIParamName, count))[0]
         if (count gt 0) then return, oROIParam
      endfor
   return, -1
end


function C_sROI3DGroupObj::getGroupMaskInExactParamValOrder, selROIParamName = selROIParamName
  dimArray = [self->getxyzDim()]
  oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = position)
  if obj_valid(oROIParam) then begin
     pValueStruct = oROIParam->getpValueStruct(position = position)
     mask = make_array(dimension = dimArray, type = size(*(*pValueStruct).pROIParamVect, /type))
     for i = 0L, self->count()-1 do begin
        whereObj = where(*(*(oROIParam->getpParamStruct())).pROINumberVect eq (self->get(position = i))->getNumber())
        if (whereObj[0] ne -1) then mask[*((self->get(position = i))->getpWherePoints())] = (*(*pValueStruct).pROIParamVect)[whereObj]
     endfor
     return, mask
  endif
  return, make_array(dimension = dimArray, /byte)
end


pro C_sROI3DGroupObj::setGroupObjColorsInParamValOrder, selROIParamName   = selROIParamName,$
                                                        ROIObjParamPos    = ROIObjParamPos,$
                                                        ROIObjSubParamPos = ROIObjSubParamPos,$
                                                        pWhereThreshArr   = pWhereThreshArr,$
                                                        pColorArr = pColorArr,$
                                                        stack_tlb = stack_tlb
  minParam = -1.
  maxParam = -1.
  fParam   = 0b
  rgb_table0 = bytArr(256,3) + transpose((*self.pVolState).rgbValues[0,*,*])
  opacVect = self->getOpacVect('1st Volume Opacity')

  dummyTemp = ROIObjSubParamPos
  if (n_elements(selROIParamName) gt 0) $
  then oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = dummyTemp > 0) $
  else oROIParam = self.oParamContainer->get(position = ROIObjParamPos)

  if obj_valid(oROIParam) then begin
    pParamStruct = *(oROIParam->getpParamStruct())
    pValueStruct = oROIParam->getpValueStruct(position = ROIObjSubParamPos)
    minParam = min(*(*pValueStruct).pROIParamVect, max = maxParam)
    minParam *= 1.
    maxParam *= 1.
    fParam = (minParam eq -1) and (maxParam eq -1)
    if(fParam) then begin
      minParam = 0.
      maxParam = 1.
    endif
    if (minParam eq maxParam) then begin
      maxParam = minParam + 1.
    endif
    deltaParam = maxParam - minParam

    clusPos = *(((*self.pParamStruct)).pValues)[(where(*(((*self.pParamStruct)).pNames) eq 'Cluster Position'))[0]]

    widget_control, stack_tlb, get_uValue = stackState, /no_copy
      (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStructStack
    widget_control, stack_tlb, set_uValue = stackState, /no_copy

    path = *(*pParamStructStack).pValues[(where( *(*pParamStructStack).pNames eq 'Stack Path'))[0]]
    fileName = strCompress(path + '_FullColors'+ path_sep() + (*(oROIParam->getpParamStruct())).name +  path_sep() + $
                           'MinMAXParam_' + (*(oROIParam->getpParamStruct())).name + $
                           '_cluster_' + string(clusPos) + '_pos_' + strCompress(string(ROIObjSubParamPos > 0)) + '.tmp')

    if ((file_test(fileName)) eq 1) then begin
      restore, fileName, restored_objects = minmaxParam, /relaxed
      minParam = minmaxParam[0]
      maxParam = minmaxParam[1]
      deltaParam = maxParam - minParam
    endif

      ; sortNumberVectInParamValOrder
    sortParamVect = sort(*(*pValueStruct).pROIParamVect)
    sortNumberVect = (*(pParamStruct).pROINumberVect)[sortParamVect]
    self->getProperty, uvalue = sortVect

    if (n_elements(sortVect) eq 0) then begin
      sortVect = {pOld: ptr_new(),$
                  pNew: ptr_new(),$
                  fUpdate: 0b,$
                  fMovieKeepGalleryPosition: 0b}
      sortVect.pNew = ptr_new(sortNumberVect)
      sortVect.pOld = ptr_new(sortNumberVect, /no_copy)
    endif else *sortVect.pNew = sortNumberVect

    self->setProperty, uvalue = sortVect

    if fParam then begin

      for i = 0L, self->count()-1 do begin
        objNumber = (self->get(position = i))->getNumber()
        whereObj = (where(*(pParamStruct).pROINumberVect eq objNumber))[0]
        if (whereObj ne -1) then (self->get(position = i))->setProperty, color = [200, 200, 200]
      endfor

    endif else begin
      if (n_elements(pWhereThreshArr) le 0) then begin
        for i = 0L, self->count()-1 do begin
          objNumber = (self->get(position = i))->getNumber()
          whereObj  = (where(*(pParamStruct).pROINumberVect eq objNumber))[0]
          if (whereObj ne -1) then begin
            currValue = ((*(*pValueStruct).pROIParamVect)[whereObj] < maxParam) > minParam
            pdq = deltaParam eq 0 ? 1 : (currValue - minParam) / deltaParam
            print, 'Object ', i, ':', (*(*pValueStruct).pROIParamVect)[whereObj], minParam, maxParam, round(pdq * 254 + 1)
            (self->get(position = i))->setProperty, color = reform(rgb_table0[round(pdq * 254 + 1), *]), alpha_channel = opacVect[round(pdq * 254 + 1)] / 255.
          endif
        endfor

      endif else begin

        whObjInThresh1 = -1
        whObjInThresh2 = -1
        whObjInThresh3 = -1
        whObjInThresh4 = -1

        for i = 0L, self->count()-1 do begin

          objNumber = (self->get(position = i))->getNumber()
          whereObj  = (where(*(pParamStruct).pROINumberVect eq objNumber))[0]

          if (whereObj ne -1) then begin

            currValue = ((*(*pValueStruct).pROIParamVect)[whereObj] < maxParam) > minParam
            if ((*(pWhereThreshArr)[0])[0] ne -1) then whObjInThresh1 = ((where((*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[0]] eq objNumber))[0] ne -1)
            if ((*(pWhereThreshArr)[1])[0] ne -1) then whObjInThresh2 = ((where((*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[1]] eq objNumber))[0] ne -1)
            if ((*(pWhereThreshArr)[2])[0] ne -1) then whObjInThresh3 = ((where((*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[2]] eq objNumber))[0] ne -1)
            if ((*(pWhereThreshArr)[3])[0] ne -1) then whObjInThresh4 = ((where((*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[3]] eq objNumber))[0] ne -1)

            case 1 of

              whObjInThresh4: (self->get(position = i))->setProperty, color = *(pColorArr)[3], alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / deltaParam * 254 + 1)] / 255.
              whObjInThresh3: (self->get(position = i))->setProperty, color = *(pColorArr)[2], alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / deltaParam * 254 + 1)] / 255.
              whObjInThresh2: (self->get(position = i))->setProperty, color = *(pColorArr)[1], alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / deltaParam * 254 + 1)] / 255.
              whObjInThresh1: (self->get(position = i))->setProperty, color = *(pColorArr)[0], alpha_channel = opacVect[round(((*(*pValueStruct).pROIParamVect)[whereObj] - minParam) / deltaParam * 254 + 1)] / 255.
              else: begin
                pdq = deltaParam eq 0 ? 1 : (currValue - minParam) / deltaParam
                print, 'Object ', i, ':', (*(*pValueStruct).pROIParamVect)[whereObj], minParam, maxParam, round(pdq * 254 + 1)
                (self->get(position = i))->setProperty, color = reform(rgb_table0[round(pdq*254 + 1), *]), alpha_channel = opacVect[round(pdq*254 + 1)] / 255.
              endcase
            endcase
          endif
        endfor
      endelse
    endelse
  endif
end


pro C_sROI3DGroupObj::selectObjectsInThresholdIntervals, selROIParamName = selROIParamName
   oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = position)
   oROIParam->applyThresholdFilter, position = position

   pParamStruct = oROIParam->getpParamStruct()
   for i = self->count()-1,0,-1 do begin
      objNumber = (self->get(position = i))->getNumber()
      whereObj = (where (*(*pParamStruct).pROINumberVect eq objNumber))[0]
      if (whereObj eq -1) then obj_destroy, self->get(position = i)
   endfor
end

; REFERENCIAFASL
pro C_sROI3DGroupObj::complementGroupParameters, oParamContainerReference = oParamContainerReference

   self->setActiveGroupParameterNameList
   count = oParamContainerReference->count()-1

   if ptr_valid(self.pActiveGroupParameterNameList) then begin
      if (n_elements(*self.pActiveGroupParameterNameList) lt 1) then *self.pActiveGroupParameterNameList = 'XXX'
   endif else self.pActiveGroupParameterNameList = ptr_new('XXX', /no_copy)

   for i = 0L, count do begin
      oROIParamReference = oParamContainerReference->get(position = i)
      whParam = (where(*self.pActiveGroupParameterNameList eq ((*(oROIParamReference->getpParamStruct())).Name)))[0]
      if (whParam eq -1) then begin
         save, oROIParamReference, filename = s_getPathForSystem()+'obj.tmp'
         restore, s_getPathForSystem()+'obj.tmp', restored_objects = oROIParam, /relaxed
         oROIParam = oROIParam[0]
         self.oParamContainer->add, oROIParam
         self->setActiveGroupParameterNameList
         self->calculateGroupParameters, selROIParamName = ((*(oROIParam->getpParamStruct())).Name)
      endif else begin
         oROIParam = self.oParamContainer->get(position = whParam)
         if (total((*(*(oROIParam->getpParamStruct())).pActive) ne (*(*(oROIParamReference->getpParamStruct())).pActive)) gt 0) then begin
            *(*(oROIParam->getpParamStruct())).pActive = (*(*(oROIParam->getpParamStruct())).pActive) > (*(*(oROIParamReference->getpParamStruct())).pActive)
            self->calculateGroupParameters, selROIParamName = ((*(oROIParam->getpParamStruct())).Name)
         endif
      endelse
   endfor
end

pro C_sROI3DGroupObj::setParamFilters, oParamContainerReference = oParamContainerReference

   self->setActiveGroupParameterNameList
   count = oParamContainerReference->count()-1

   if ptr_valid(self.pActiveGroupParameterNameList) then begin
      if (n_elements(*self.pActiveGroupParameterNameList) lt 1) then *self.pActiveGroupParameterNameList = 'XXX'
   endif else self.pActiveGroupParameterNameList = ptr_new('XXX', /no_copy)

   for i = 0L, count do begin
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


function C_sROI3DGroupObj::getpEigenSys
   return, self.pEigenSys
end


function C_sROI3DGroupObj::calcEigenSys
   if (self->count() gt 0) then begin
      xyzPoints = (self->get(position = 0))->getxyzPoints()
      for i = 1L, self->count()-1 do xyzPoints = transpose([transpose(xyzPoints), transpose((self->get(position = i))->getxyzPoints())])
   endif
   (*self.pEigenSys).centerXYZ = [total(xyzPoints[0,*]), total(xyzPoints[1,*]), total(xyzPoints[2,*])] / (1.*(size(xyzPoints, /dim))[1])
   eigenState = (self->get(position = 0))->calcEigenSys(xyzPoints = xyzPoints, centerXYZ = (*self.pEigenSys).centerXYZ)
   (*self.pEigenSys).eigenVect = eigenState.eigenVect
   (*self.pEigenSys).eigenVals = eigenState.eigenVals
   return, 1b
end

; This change is for calculate params for other times, clusters or in general for other 3DgroupObject and only for an specific param
pro C_sROI3DGroupObj::calculateGroupParameters, mask = mask, selROIParamName = selROIParamName, position = position, stack_tlb = stack_tlb, otherC_sROI3DGroupObj = otherC_sROI3DGroupObj  
    dummyUsePos = -1
    selectedROI3DGroup = self;
    if(obj_valid(otherC_sROI3DGroupObj)) then selectedROI3DGroup = otherC_sROI3DGroupObj

    count = self.oParamContainer->count()-1
    if (n_elements(selROIParamName) gt 0) then begin
      oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName)
      count = 0
    endif

    dummyPos = 0
    fAllPositions = 0b
    if(N_ELEMENTS(position) gt 0) then begin
        dummyPos = position[0]
    endif else fAllPositions = 1b

   for i = 0L, count do begin
        if (i eq 0) then begin
         if ~obj_valid(oROIParam) then oROIParam = self.oParamContainer->get(position = 0)
       endif else oROIParam = self.oParamContainer->get(position = i)

        fProcesar = 1b
        if((N_ELEMENTS(Position) eq 1)) then begin
          if(Position[0] ne -1) then begin
            if(whParamActive[Position[0]] ne 1) then begin
                fProcesar = 0b
            endif
          endif
        endif     
        
        if(fProcesar eq 1b) then begin 
          ; obtain MINMAX param on demand XD only for miss Yoya,,,,
                  dummyCalcMinMax = 0b
                  if(dummyCalcMinMax eq 1b) then begin
                      ; Obtain params for all times
                      if (N_ELEMENTS(stack_tlb) gt 0) then begin
                        clusPos  = *(((*self.pParamStruct)).pValues)[(where(*(((*self.pParamStruct)).pNames) eq 'Cluster Position'))[0]]
                        widget_control, stack_tlb, get_uValue = stackState, /no_copy
                         (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
                        widget_control, stack_tlb, set_uValue = stackState, /no_copy
                        path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
                        if(FILE_TEST(strCompress(path+'_FullColors' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(path+'_FullColors' + path_sep())
                        if(FILE_TEST(strCompress(path+'_FullColors'+ path_sep()+(*(oROIParam->getpParamStruct())).name + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(path+'_FullColors'+ path_sep()+(*(oROIParam->getpParamStruct())).name+ path_sep())
                
                        nombreFile = strCompress(path+'_FullColors'+ path_sep()+(*(oROIParam->getpParamStruct())).name+ path_sep()+'MinMAXParam_'+(*(oROIParam->getpParamStruct())).name+ '_cluster_'+ string(clusPos)+'_pos_'+strCompress(string(dummyPos > 0))+'.tmp')
                        ;nombreFile = strCompress(path+'_FullColors' + path_sep()+(*(oROIParam->getpParamStruct())).name + path_sep()+'MinMAXParam_'+(*(oROIParam->getpParamStruct())).name+'_pos_'+string(dummyPos > 0)+'.tmp')      
                        if((FILE_TEST(nombreFile)) eq 0) then begin
                            ; Obtain min max for all times :D for current cluster information...
                            ; Valid for multitime models :D
                            ; INFO POSICION ACTUAL
                             clusPos  = *(((*self.pParamStruct)).pValues)[(where(*(((*self.pParamStruct)).pNames) eq 'Cluster Position'))[0]]
                             chPos    = *(((*self.pParamStruct)).pValues)[(where(*(((*self.pParamStruct)).pNames) eq 'Channel Position'))[0]]
                             tPos     = *(((*self.pParamStruct)).pValues)[(where(*(((*self.pParamStruct)).pNames) eq 'Time Position'))[0]]
                         
                             ;pParamStruct = selectedStackObject->getpParamStruct()
                             ;INFO T Z CH TOTALES
                             s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum = totalTimes
                             s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum = totalZimes
                             s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalChNum = totalChNum
                      
                             nParams =  n_elements(*(*(oROIParam->getpParamStruct())).pValues)
                             minParam = MAKE_ARRAY(nParams, /FLOAT, VALUE = 1000000.0)
                             maxParam = MAKE_ARRAY(nParams, /FLOAT, VALUE = -1000000.0)
                             for i = 0L, totalTimes-1 do begin
                                  C_sROI3DGroupObjActual = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = i, chPos = chPos, clusPos = clusPos)
                
                                      case (*(oROIParam->getpParamStruct())).name of
                                         '3D Object Time': oROIParam->apply, time = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time After Start [s]'))[0]], C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position
                                         '3D Track Objects': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position
                                         '3D Object Surface': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position
                                         '3D Object Tracking': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position
                                         '3D Object Box': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position
                                         '3D Object Moments of Morphology': oROIParam->apply, C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position, stack_tlb = stack_tlb
                                         ; TODO JJ development method
                                         '3D Object Surface AC test': oROIParam->apply, C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position, stack_tlb = stack_tlb
                                         '3D Object Overlap': oROIParam->apply, C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position, stack_tlb = stack_tlb
                                         'Group Object Number': oROIParam->apply, number = C_sROI3DGroupObjActual->count()-1
                                         else: oROIParam->apply, C_sROI3DGroupObj = C_sROI3DGroupObjActual, position = position
                                      endcase
                
                                      for j = 0L, nParams-1 do begin
                                        pValueStruct = oROIParam->getpValueStruct(position = j)
                                        minParamActual = min(*(*pValueStruct).pROIParamVect, max = maxParamActual)
                                        minParamActual *= 1.
                                        maxParamActual *= 1.
                                        minParam[j] = minParam[j] < minParamActual
                                        maxParam[j] = maxParam[j] > maxParamActual
                                      endfor
                                   OBJ_DESTROY, C_sROI3DGroupObjActual
                             endfor
                             for j = 0L, nParams-1 do begin
                                if(N_ELEMENTS(position) gt 0) then begin
                                  dummyUsePos = where(position[*] eq j)
                                endif
                                if(fAllPositions OR (dummyUsePos[0] ne -1)) then begin
                                  ;nombreFile = strCompress(path+'_FullColors' + path_sep()+'MinMAXParam_'+(*(oROIParam->getpParamStruct())).name+'_pos_'+string(j)+'.tmp')
                                  nombreFile = strCompress(path+'_FullColors'+ path_sep()+(*(oROIParam->getpParamStruct())).name+ path_sep()+'MinMAXParam_'+(*(oROIParam->getpParamStruct())).name+ '_cluster_'+ string(clusPos)+'_pos_'+strCompress(string(j))+'.tmp')
                                  fParam = (minParam[j] eq 1000000.0) and (maxParam[j] eq  -1000000.0)
                                  if((fParam) eq 0) then begin
                                      if(maxParam[j] le minParam[j]) then maxParam[j] = minParam[j] + 1.0  
                                      minmaxParam = [minParam[j], maxParam[j]]
                                      save, minmaxParam, filename = nombreFile
                                  endif
                                endif
                             endfor
                        endif
                      endif
                endif
      
          ; obtain current param
              case (*(oROIParam->getpParamStruct())).name of
                '3D Object Time': oROIParam->apply, time = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time After Start [s]'))[0]], C_sROI3DGroupObj = selectedROI3DGroup, position = position
                '3D Track Objects': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = selectedROI3DGroup, position = position
                '3D Object Surface': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = selectedROI3DGroup, position = position
                '3D Object Tracking': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = selectedROI3DGroup, position = position
                '3D Object Box': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = selectedROI3DGroup, position = position
                '3D Object Moments of Morphology': oROIParam->apply, C_sROI3DGroupObj = selectedROI3DGroup, position = position, stack_tlb = stack_tlb
                ; TODO JJ development method
                '3D Object Surface AC test': oROIParam->apply, C_sROI3DGroupObj = selectedROI3DGroup, position = position, stack_tlb = stack_tlb
                '3D Object Overlap': oROIParam->apply, C_sROI3DGroupObj = selectedROI3DGroup, position = position, stack_tlb = stack_tlb
                '3D Object Optical Flow': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = selectedROI3DGroup, position = position
                'Group Object Number': oROIParam->apply, number = selectedROI3DGroup->count()-1
                '3D Skeleton': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = selectedROI3DGroup, position = position
                '3D Graph Skeleton': oROIParam->apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = selectedROI3DGroup, position = position
                else: oROIParam->apply, C_sROI3DGroupObj = selectedROI3DGroup, position = position
              endcase
        endif
     endfor
end

pro C_sROI3DGroupObj::saveSkeletonModels, stack_tlb = stack_tlb, modelName = modelName, pVertices = pVertices, pPolygons = pPolygons, fFile = fFile

  if ~obj_valid(self) then return

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos

  widget_control, stateObj_tlb, get_uValue = state, /no_copy
    currROI3DGroupFileName = state.currROI3DGroupFileName
  widget_control, stateObj_tlb, set_uValue = state, /no_copy

  slash = path_sep()
  posLastSlash = strPos(currROI3DGroupFileName, slash, /reverse_search)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, strPos(currROI3DGroupFileName, slash, /reverse_search)+1)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, 0, strPos(currROI3DGroupFileName, '.sav'))

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  modelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]] + strcompress(modelName + slash, /rem))
  if (file_test(strCompress(modelPath), /DIRECTORY) eq 0b) then begin
    f = dialog_message('No model folder was found (' + modelPath + ')', title = 'No saved models.')
    return
  endif
  for i = 0L, 1 do begin
    print, 'Ouch!'
  endfor
  save, vertices, polygons, fileName = modelName
end


pro C_sROI3DGroupObj::restoreSavedSkeletonModels, stack_tlb = stack_tlb, modelName = modelName, pVertices = pVertices, pEdges = pEdges, $
                                                  fSkeletonTypes = fSkeletonTypes, fFile = fFile, loadFromObjFile = loadFromObjFile, silent = silent,$
                                                  pSkeletonMeshAssociation = pSkeletonMeshAssociation

  if ~obj_valid(self) then return
  nObj  = self->count()
  if (nObj lt 1) then return
  loadFromObjFile = keyword_set(loadFromObjFile)
  defaultSkeletonType = 1b

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos

  widget_control, stateObj_tlb, get_uValue = state, /no_copy
    currROI3DGroupFileName = state.currROI3DGroupFileName
  widget_control, stateObj_tlb, set_uValue = state, /no_copy

  slash = path_sep()
  posLastSlash = strPos(currROI3DGroupFileName, slash, /reverse_search)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, strPos(currROI3DGroupFileName, slash, /reverse_search)+1)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, 0, strPos(currROI3DGroupFileName, '.sav'))
  fFile = make_array(nObj, /byte)
  fSkeletonTypes = make_array(nObj, /byte, value = defaultSkeletonType)

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  modelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]] + strcompress(modelName + slash, /rem))
  if (file_test(strCompress(modelPath), /DIRECTORY) eq 0b) then begin
    if ~keyword_set(silent) then f = dialog_message('No model folder was found (' + modelPath + ')', title = 'No saved models')
    return
  endif

    ; test if the requested model has been saved before
  filePathAndNamePrefix = strCompress(modelPath + strCompress(currROI3DGroupFileName + modelName,/rem))
  fileNameArr = make_array(nObj, /string)
  fileExt = (loadFromObjFile eq 1) ? '.obj' : '.sav'
  for i = 0L, nObj-1 do begin
    fileNameArr[i] = strCompress(filePathAndNamePrefix + strCompress(string((self->get(position = i))->getNumber()) + '.sav',/rem))
    fileStack = file_info(fileNameArr[i])
    fFile[i] = fileStack[0].exists
  endfor

  pVertices      = ptrArr(nObj)
  pEdges         = ptrArr(nObj)
  pSkeletonMeshAssociation = ptrArr(nObj)

    ; load from .obj
  if (loadFromObjFile eq 1) then begin
    for i = 0L, nObj-1 do begin
      if (fFile[i] eq 1) then begin
          ; instances of 'vertices' and 'polygons' restored from file
        print, 'loading from ', fileNameArr[i]
        s_getoSkeletonFromMeshRepair_readSkeletonFromObj, fileNameArr[i], vertices, edges
        pVertices[i] = ptr_new(vertices, /no_copy)
        pEdges[i]    = ptr_new(edges, /no_copy)
        fSkeletonTypes[i] = defaultSkeletonType
      endif
    endfor
    return
  endif

    ; load from .sav
  for i = 0L, nObj-1 do begin
    if (fFile[i] eq 1) then begin
      ; instances of 'vertices' and 'polygons' restored from file
      print, 'restoring ', fileNameArr[i]
      restore, fileName = fileNameArr[i], /relaxed
      pVertices[i] = ptr_new(vertices, /no_copy)
      pEdges[i] = ptr_new(edges, /no_copy)

      if keyword_set(savedLastSkelStep) then begin
        fSkeletonTypes[i] = savedLastSkelStep
        p = ptr_new(savedLastSkelStep, /no_copy)
      endif
      if keyword_set(skeletonMeshAssociation) $
      then pSkeletonMeshAssociation[i] = ptr_new(skeletonMeshAssociation, /no_copy) $  
      else pSkeletonMeshAssociation[i] = ptr_new()
    endif
  endfor
end


; restoreSavedSurfaceModel
;
; PURPOSE
;   Load a previously saved surface model (for a given single ROI), returning its array of vertices and polygons.
;
; ARGUMENTS
;   modelName  the name or type of the model to restore (like '_AC3D_Obj_', '_Tracking' or '_Skeleton').
;   roiNumber  the ROI number.
;   pVertices  output variable, pointer to the restored vertices array of the ROI.
;   pPolygons  output variable, pointer to the restored polygons array of the ROI.
;
; RETURN VALUE
;   Two named variables must be defined (pVertices and pPolygons) in order to store the vertices (of size 3 x n) and polygons (of size m) lists for the ROI.
pro C_sROI3DGroupObj::restoreSavedSurfaceModel, stack_tlb = stack_tlb, modelName = modelName, roiNumber = roiNumber, pVertices = pVertices, pPolygons = pPolygons, fSilent = fSilent
  if ~obj_valid(self) then return

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos

  widget_control, stateObj_tlb, get_uValue = state, /no_copy
    currROI3DGroupFileName = state.currROI3DGroupFileName
  widget_control, stateObj_tlb, set_uValue = state, /no_copy

  slash = path_sep()
  posLastSlash = strPos(currROI3DGroupFileName, slash, /reverse_search)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, strPos(currROI3DGroupFileName, slash, /reverse_search)+1)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, 0, strPos(currROI3DGroupFileName, '.sav'))

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  modelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]] + strCompress(modelName + slash, /rem))
  if (file_test(strCompress(modelPath), /DIRECTORY) eq 0b) then begin
    if ~keyword_set(fSilent) then f = dialog_message('No model folder was found (' + modelPath + ')', title = 'No saved models')
    return
  endif

    ; test if the requested model has been saved before
  filePathAndNamePrefix = strCompress(modelPath + strCompress(currROI3DGroupFileName + modelName,/rem))

  fileName = strCompress(filePathAndNamePrefix + strCompress(string(roiNumber) + '.sav',/rem))
  fileStack = file_info(fileName)

  if (fileStack.exists eq 1) then begin
      ; instances of 'vertices' and 'polygons' restored from file
    if ~keyword_set(fSilent) then print, 'Restoring ', fileName
    restore, fileName = fileName, /relaxed
    pVertices = ptr_new(vertices, /no_copy)
    pPolygons = ptr_new(polygons, /no_copy)
  endif else print, 'File not found: ', fileName
end


; restoreSavedSurfaceModels
;
; PURPOSE
;   Load previously saved surface models as arrays of vertices and polygons.
;
; ARGUMENTS
;
;   stack_tlb  stack_tlb
;   modelName  the name of the model to restore (like '_AC3D_Obj_', '_Tracking' or '_Skeleton').
;   fFile      output variable in the form of a byte array, containing the value 1 if a given object was found, and 0 if not.
;   pVertices  output variable, n-element pointer array to the restored ROI vertices.
;   pPolygons  output variable, n-element pointer array to the restored ROI polygons.
;   fNoDialog if set, avoids dialog window to alert users in case of models being not found.
;
; RETURN VALUE
;   Two named variables store the output, in the form of two pointer arrays (one element for each ROI)
;   containing the vertices (size 3 x n_i) and polygons (size m_i) lists for each ROI (i)
;
; HISTORY
;   First version for vertices+polygons restoring. J Jara (2012)
pro C_sROI3DGroupObj::restoreSavedSurfaceModels, stack_tlb = stack_tlb, modelName = modelName, pVertices = pVertices, pPolygons = pPolygons, fFile = fFile, fNoDialog = fNoDialog

  if ~obj_valid(self) then return
    ; Output arrays. They are intialized on purpose, even if no saved models are found, in order to ease the use by calling methods.
  nObj  = self->count()
  fFile = make_array(nObj, /byte)

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb, tPos = tPos, chPos = chPos

  widget_control, stateObj_tlb, get_uValue = state, /no_copy
    currROI3DGroupFileName = state.currROI3DGroupFileName
  widget_control, stateObj_tlb, set_uValue = state, /no_copy

  slash = path_sep()
  posLastSlash = strPos(currROI3DGroupFileName, slash, /reverse_search)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, strPos(currROI3DGroupFileName, slash, /reverse_search)+1)
  currROI3DGroupFileName = strMid(currROI3DGroupFileName, 0, strPos(currROI3DGroupFileName, '.sav'))

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  modelPath = strCompress(*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Stack Path'))[0]] + modelName + slash)
  if (file_test(modelPath, /DIRECTORY) eq 0b) then begin
    msgTitle = 'No saved models'
    msgText  = 'No model folder was found (' + modelPath + ')'
    if keyword_set(fNoDialog) then print, msgTitle + ': ' + msgText else f = dialog_message(msgText, title = msgTitle)
    return
  endif

    ; test if the requested model has been saved before
  filePathAndNamePrefix = modelPath + currROI3DGroupFileName + modelName
  fileNameArr = make_array(nObj, /string)
  pVertices   = ptrArr(nObj)
  pPolygons   = ptrArr(nObj)

  for i = 0L, nObj-1 do begin
    fileNameArr[i] = strCompress(filePathAndNamePrefix) + strCompress(string((self->get(position = i))->getNumber()), /remove_all) + '.sav'
    fileStack = file_info(fileNameArr[i])
    fFile[i] = fileStack[0].exists
  endfor

  for i = 0L, nObj-1 do begin
    if (fFile[i] eq 1) then begin
        ; instances of 'vertices' and 'polygons' restored from file
      print, 'Restoring ', fileNameArr[i]
      restore, fileName = fileNameArr[i], /relaxed
      if keyword_set(oDummy) and obj_valid(oDummy) then begin
        oDummy->getProperty, data = vertices, poly = polygons
        obj_destroy, oDummy
      endif
      pVertices[i] = ptr_new(vertices, /no_copy)
      pPolygons[i] = ptr_new(polygons, /no_copy)
    endif
  endfor
end


function C_sROI3DGroupObj::getParameterNameList
   return, ['Cluster Position',$
            'Time Position',$
            'Channel Position',$
            'z-Slice Position',$
            'z-Slice Initial',$
            'Time After Start [s]',$
            'x-Size [real]',$
            'y-Size [real]',$
            'z-Size [real]',$
            'x-Size [pixel]',$
            'y-Size [pixel]',$
            'z-Size [pixel]',$
            'Comments',$
            'Hide On/Off',$
            'Border Object',$
            'Border Style',$
            'Border Color_r',$
            'Border Color_g',$
            'Border Color_b',$
            'Border Thick',$
            'Border LineStyle',$
            '1st Volume Object',$
            '1st Volume Color',$
            '1st Volume Opacity',$
            '1st Cutting Plane x',$
            '1st Cutting Plane y',$
            '1st Cutting Plane z',$
            '2nd Volume Object',$
            '2nd Volume Color',$
            '2nd Volume Opacity',$
            '2nd Cutting Plane x',$
            '2nd Cutting Plane y',$
            '2nd Cutting Plane z',$
            'Merge Volumes',$
            '1st Active_Plane',$
            '1st Center_Plane X',$
            '1st Center_Plane Y',$
            '1st Center_Plane Z',$
            '1st Angle_Plane A',$
            '1st Angle_Plane B',$
            '1st Displacement_Plane',$   
            '1st Distance_Render',$             
            '2st Active_Plane',$
            '2st Center_Plane X',$
            '2st Center_Plane Y',$
            '2st Center_Plane Z',$
            '2st Angle_Plane A',$
            '2st Angle_Plane B',$
            '2st Displacement_Plane',$   
            '2st Distance_Render',$
            
             'Is AA AMA',$
             'X for first Point',$
             'Y for first Point',$
             'Z for first Point',$
             'X for second Point',$
             'Y for second Point',$
             'Z for second Point',$
               
            'Radio Balls Model',$ 
            'Length Div Axis',$           
            'Is Limit Hist',$   
            'Limit Hist',$ 
            'Child 1',$
            'Child 2',$
            'Child 3',$
            'Child 4',$
            'Child 5',$
            'Child 6',$
            'Child 7',$
            'Child 8',$
            'Child 9',$
            'Child 10',$
            'Child 11',$
            'Child 12']
end

function C_sROI3DGroupObj::init, clusPos = clusPos,$
                                 tPos = tPos,$
                                 chPos = chPos,$
                                 zPos = zPos,$
                                 zSliceInitial = zSliceInitial,$
                                 timeAfterStart = timeAfterStart,$
                                 xyzFramePixSize = xyzFramePixSize,$
                                 xyzFrameRealSize = xyzFrameRealSize,$
                                 borderColor = borderColor,$
                                 borderThick = borderThick,$
                                 borderLineStyle = borderLineStyle,$
                                 volumeColor = volumeColor,$
                                 hideOnOff = hideOnOff

   parameterNameList = self->getParameterNameList()
   paramStruct = {pValues: ptrArr(n_elements(parameterNameList), /allocate),$ ; Pointer on Parameter Values.
                  pNames: ptr_new(parameterNameList, /no_copy)}               ; Pointer on Parameter Names.

   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Cluster Position'))[0]] = clusPos
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Time Position'))[0]] = tPos
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Channel Position'))[0]] = chPos
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Slice Position'))[0]] = zPos
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Slice Initial'))[0]] = zSliceInitial

   if (n_elements(timeAfterStart) eq 0) then timeAfterStart = tPos
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Time After Start [s]'))[0]] = timeAfterStart
   if (n_elements(xyzFrameRealSize) eq 0) then xyzFrameRealSize = [-1,-1,-1]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'x-Size [real]'))[0]] = xyzFrameRealSize[0]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'y-Size [real]'))[0]] = xyzFrameRealSize[1]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Size [real]'))[0]] = xyzFrameRealSize[2]
   if (n_elements(xyzFramePixSize) eq 0) then xyzFramePixSize = [-1,-1,-1]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'x-Size [pixel]'))[0]] = xyzFramePixSize[0]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'y-Size [pixel]'))[0]] = xyzFramePixSize[1]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Size [pixel]'))[0]] = xyzFramePixSize[2]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Comments'))[0]] = '007'
   if (n_elements(hideOnOff) eq 0) then hideOnOff = 0b
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Hide On/Off'))[0]] = hideOnOff
      ; Border Parameters
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Object'))[0]] = 'CONTAINED'
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Style'))[0]] = 1
   if (n_elements(borderColor) eq 0) then borderColor = [255,0,0]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_r'))[0]] = borderColor[0]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_g'))[0]] = borderColor[1]
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_b'))[0]] = borderColor[2]
   if (n_elements(borderThick) eq 0) then borderThick = 1.
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Thick'))[0]] = borderThick
   if (n_elements(borderLineStyle) eq 0) then borderLineStyle = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border LineStyle'))[0]] = borderLineStyle

      ; Volume Parameters
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Volume Object'))[0]] = 'CONTAINED'
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Volume Color'))[0]] = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Volume Opacity'))[0]] = 255
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2nd Volume Object'))[0]] = 'NO SELECTION'
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2nd Volume Color'))[0]] = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2nd Volume Opacity'))[0]] = 20
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Cutting Plane x'))[0]] = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Cutting Plane y'))[0]] = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Cutting Plane z'))[0]] = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2nd Cutting Plane x'))[0]] = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2nd Cutting Plane y'))[0]] = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2nd Cutting Plane z'))[0]] = 0
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Merge Volumes'))[0]] = 'off'
   
   ; CutPlanes Params
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Active_Plane'))[0]] = 0                           ; Values:: 0=> No PlaneCut Used, 1 => PlaneCut in use but non visible, 2 => PlaneCut rendered
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Center_Plane X'))[0]] = xyzFramePixSize[0] /2.0   ; coords for center plane position
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Center_Plane Y'))[0]] = xyzFramePixSize[1] /2.0   
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Center_Plane Z'))[0]] = xyzFramePixSize[2] /2.0   
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Angle_Plane A'))[0]] = 0.0                      ; spheric representation for angles of plane 
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Angle_Plane B'))[0]] = 0.0   
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Displacement_Plane'))[0]] = 0.0                   ; Desplacement of plane in Normal direction... for navigation
   *paramStruct.pValues[(where(*paramStruct.pNames eq '1st Distance_Render'))[0]] = 0.0                      ; set distance for render the plane---   
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2st Active_Plane'))[0]] = 0                           ; Values:: 0=> No PlaneCut Used, 1 => PlaneCut in use but non visible, 2 => PlaneCut rendered
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2st Center_Plane X'))[0]] = xyzFramePixSize[0] /2.0   ; coords for center plane position
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2st Center_Plane Y'))[0]] = xyzFramePixSize[1] /2.0   
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2st Center_Plane Z'))[0]] = xyzFramePixSize[2] /2.0   
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2st Angle_Plane A'))[0]] = 0.0                      ; spheric representation for angles of plane 
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2st Angle_Plane B'))[0]] = 0.0   
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2st Displacement_Plane'))[0]] = 0.0                   ; Desplacement of plane in Normal direction... for navigation
   *paramStruct.pValues[(where(*paramStruct.pNames eq '2st Distance_Render'))[0]] = 0.0                      ; set distance for render the plane---   

   ; Arbitray Axis
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Is AA AMA'))[0]] = 0                      ; Is used AAAMA??   
   
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'X for first Point'))[0]] = 0                      ; Is used AAAMA??
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Y for first Point'))[0]] = 0                      ; Is used AAAMA??
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Z for first Point'))[0]] = 0                      ; Is used AAAMA??
   
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'X for second Point'))[0]] = 0                      ; Is used AAAMA??
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Y for second Point'))[0]] = 0                      ; Is used AAAMA??
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Z for second Point'))[0]] = 0                      ; Is used AAAMA??

   ; Sphere Model
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Radio Balls Model'))[0]] = 5                      ; Value of radius if balls model   
   
   ; Hist modified by sons
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Length Div Axis'))[0]] = -1                      ; lenght for draw axis
   
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Is Limit Hist'))[0]] = 0                      ; 0: normal system. 1: use limited array   
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Limit Hist'))[0]] = 0                     ; Value of new Limit Hist .. < num objects 
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 1'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 2'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 3'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 4'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 5'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 6'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 7'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 8'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 9'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 10'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 11'))[0]] = -1                      ; Value of replic... info of root parent
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Child 12'))[0]] = -1                      ; Value of replic... info of root parent   
   
   self.pParamStruct = ptr_new(paramStruct, /no_copy)

   eigenState = {fOK:0b,$
                 sizePerXYZ:xyzFrameRealSize/xyzFramePixSize,$
                 centerXYZ:make_array(3, /double, value = -1.),$
                 inertiaT:make_array(3,3, /double, value = -1.),$
                 eigenVect:make_array(3,3, /double, value = -1.),$
                 eigenVals:make_array(3, /double, value = -1.)}

   self.pEigenSys = ptr_new(eigenState, /no_copy)

   volState = {opacFlag:make_array(2, /byte, value = 0b),$
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

   self.pVolState = ptr_new(volState, /no_copy)

   self->setIDLgrROIGroupXYZCoord
   self.oParamContainer = obj_new('IDL_Container')
   return, 1
end


pro C_sROI3DGroupObj__define
  tmp = {C_sROI3DGroupObj, pParamStruct:ptr_new(),$
                           pVolState:ptr_new(),$
                           pEigenSys:ptr_new(),$
                           pActiveGroupParameterNameList:ptr_new(),$
                           oParamContainer:obj_new(),$
                           inherits IDLgrROIGroup}
end
