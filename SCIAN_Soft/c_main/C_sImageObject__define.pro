;_____________________________IOISIOI____________________
; NAME:
;      C_sImageObject
;
; PURPOSE:
;       - Creates C_sImage object
;
; AUTHOR:
;   Dr. Steffen Härtel (2002)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;        result = obj_new('C_sImageObject' )
;
; METHOHDS:
;   function C_sImageObject::init
;_____________________________IOISIOI____________________


pro C_sImageObject::set, pParamStruct = pParamStruct
   *self.pParamStruct = *pParamStruct
end

pro C_sImageObject::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end
function C_sImageObject::getpParamStruct
   return, self.pParamStruct
end

pro C_sImageObject::setParamAsStruct, paramStruct
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
end



function C_sImageObject::getParamAsStruct
   parameterNameList = self->getParameterNameList()
   pArrValues = ptrArr(n_elements(parameterNameList))
   for i = 0, n_elements(parameterNameList)-1 do begin
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
         k:*pArrValues[10],$
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
         w: *pArrValues[22] }
   for i = 0, n_elements(parameterNameList)-1 do ptr_free, pArrValues[i]
   return, paramStruct
end


function C_sImageObject::getSegClusterNumber
   if (obj_valid(self.oSegContainer)) then return, self.oSegContainer->count() else return, -1
end


function C_sImageObject::getSegContainer
   return, self.oSegContainer
end


pro C_sImageObject::setSegContainer, object = object
   if (obj_valid(self.oSegContainer)) then begin
     oSegs = self.oSegContainer->get(/all)
     self.oSegContainer->remove, /all
     if (obj_valid(oSegs[0])) then for i = 0, n_elements(oSegs)-1 do obj_destroy, oSegs[i]
     obj_destroy, self.oSegContainer
   endif
   self.oSegContainer = object
end


function C_sImageObject::getSegContainerObj, active = active
   if (active gt (self.oSegContainer->count()-1)) then active = self.oSegContainer->count()-1
    return, self.oSegContainer->get(position = active)
end


pro C_sImageObject::addSegContainerObj, position = position, object = object
   if not(obj_Valid(self.oSegContainer)) then self.oSegContainer = obj_new('IDL_Container')
   if (n_elements(position) gt 0) then position = (position > 0) < (self.oSegContainer)->count() $
      else position = 0 > (self.oSegContainer)->count()
   if (keyWord_set(object)) then self.oSegContainer->add, object, position = position $
     else self.oSegContainer->add, obj_new('IDL_Container'), position = position
end


pro C_sImageObject::deleteSegContainerObj, position = position
   position = position < ((self.oSegContainer->count())-1)
   oCont = self.oSegContainer->get(position = position)
   if (obj_valid(oCont)) then begin
     if (obj_isa(oCont, 'IDL_CONTAINER')) then begin
      oSegs = oCont->get(/all)
      oCont->remove, /all
      if (obj_valid(oSegs[0])) then for i = 0, n_elements(oSegs)-1 do obj_destroy, oSegs[i]
     endif
   endif
   self.oSegContainer->remove, position = position
   obj_destroy, oCont
end


function C_sImageObject::getSelectedImage, position = position, path = path, format = format
   filename = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Name'))[0]]
   format = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Format'))[0]]
   (self->IDL_Container::get(position = position))->get, pParamStruct = pParamStruct
   filename = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
   dummy = query_tiff(pathname+filename+format, info)
;   if (n_elements(info) gt 0) then return, congrid(read_tiff(pathname+filename+format), 512, 512) else return, bytArr(3,3)
   if (n_elements(info) gt 0) then return, read_tiff(pathname+filename+format) else return, bytArr(3,3)
end


function C_sImageObject::getSelectedSegObj, clusPos = clusPos, segPos = segPos
     ; Get Cluster Object
   if (n_elements(clusPos) le 0) then clusPos = 0
   clusterObj = self->getSegContainerObj(active = clusPos)
     ; Get Segmentation Object
   if (obj_valid(clusterObj)) then return, clusterObj->get(position = segPos) else return, -1
end


function C_sImageObject::applyImageSegmentation, stack_tlb = stack_tlb, selectedStackObject = selectedStackObject,$
                                 tPos = tPos,$
                                 chPos = chPos,$
                                 zPos = zPos,$
                                 clusPos = clusPos,$
                                 segPos = segPos,$
                                 cut_x = cut_x, cut_y = cut_y

   image = selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
   dimIm = size(image, /dim)
   if (n_elements(cut_x) le 0) then cut_x = [0, dimIm[0]-1] else cut_x = [(0>cut_x[0]) < (dimIm[0]-2), (dimIm[0]-1) < cut_x[1]]
   if (n_elements(cut_y) le 0) then cut_y = [0, dimIm[1]-1] else cut_y = [(0>cut_y[0])  < (dimIm[1]-2), (dimIm[1]-1) < cut_y[1]]
   if (min(cut_x) lt 0) then cut_x = [0, dimIm[0]-1] else cut_x = [(0>cut_x[0]) < (dimIm[0]-2), (dimIm[0]-1) < cut_x[1]]
   if (min(cut_y) lt 0) then cut_y = [0, dimIm[1]-1] else cut_y = [(0>cut_y[0]) < (dimIm[1]-2), (dimIm[1]-1) < cut_y[1]]
   cut_x = [cut_x[0]<cut_x[1],cut_x[0]>cut_x[1] ]
   cut_y = [cut_y[0]<cut_y[1],cut_y[0]>cut_y[1] ]

   if (cut_x[1] eq -1) then if ((dimIm[0] eq 3) and (dimIm[1] eq 3)) then return, image * 0
   image = image[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
     ; Get Cluster Object
   if (n_elements(clusPos) le 0) then clusPos = 0
   clusterObj = self->getSegContainerObj(active = clusPos)
     ; Get Segmentation Object
   if (n_elements(segPos) le 0) then segPos = (clusterObj->count() - 1) else segPos = segPos < (clusterObj->count() - 1)
   for i = 0, segPos do begin
      ; Get Segmentation Method
     j = i
     segObj = clusterObj->get(position = j)
     if obj_valid(segObj) then begin
        case segObj->getImageFilterType() of
          'Single_Image_Filter_Method': image = segObj->apply(image = image)
          'Multiple_Image_Filter_Method': image = segObj->apply(image = image,$
                                   selectedStackObject = selectedStackObject,$
                                   stack_tlb = stack_tlb,$
                                   tPos = tPos,$
                                   chPos = chPos,$
                                   zPos = zPos,$
                                   clusPos = clusPos,$
                                   segPos = j,$
                                   cut_x = cut_x, cut_y = cut_y)
        endcase
     endif
   endfor
  ;  image[0,0]=0 ; delete me
   return, image
end


pro C_sImageObject::cleanup
   if ptr_valid(self.pParamStruct) then begin
      if ptr_valid((*self.pParamStruct).pNames) then ptr_free, (*self.pParamStruct).pNames
      for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
      ptr_free, self.pParamStruct
   endif
   if obj_valid(self.oSegContainer) then begin
      oConts = (self.oSegContainer)->get(/all)
      (self.oSegContainer)->remove, /all
      if obj_valid(oConts[0]) then for i = 0, n_elements(oConts)-1 do begin
        oSegs = oConts[i]->get(/all)
        oConts[i]->remove, /all
        if obj_valid(oSegs[0]) then for j = 0, n_elements(oSegs)-1 do obj_destroy, oSegs[j]
        obj_destroy, oConts[i]
      endfor
      obj_destroy, self.oSegContainer
   endif
   if obj_valid(self.oIDLgrImage) then obj_destroy, self.oIDLgrImage
end


function C_sImageObject::getParameterNameList
    return, ['Name',$
          'Time Number',$
          'Channel Number',$
          'z-Slice Number',$
          'z-Position [real]',$
          'Time after start [s]',$
          'x-Size [pixel]',$
          'y-Size [pixel]',$
          'x-Size [real]',$
          'y-Size [real]',$
          'xSegmentBox [x0]',$
          'xSegmentBox [x1]',$
          'ySegmentBox [y0]',$
          'ySegmentBox [y1]',$
          'zSegmentBox [z0]',$
          'zSegmentBox [z1]',$
          'xShowBox [x0]',$
          'xShowBox [x1]',$
          'yShowBox [y0]',$
          'yShowBox [y1]',$
          'zShowBox [z0]',$
          'zShowBox [z1]',$
          'Hide On/Off']
end


function C_sImageObject::init
   parameterNameList = self->getParameterNameList()
   paramStruct = {ImageParamStruct,$
                  pValues : ptrArr(n_elements(parameterNameList), /allocate),$       ; Pointer on Filter Parameter Values.
                  pNames : ptr_new(parameterNameList, /no_copy)  }     ; Pointer on Filter Parameter Names.

   *paramStruct.pValues[(where( *ParamStruct.pNames eq 'Name'))[0]] = '-NO NAME-'
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'Time Number'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'Channel Number'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'z-Slice Number'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'z-Position [real]'))[0]] = 0.
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'Time after start [s]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'x-Size [pixel]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'y-Size [pixel]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'x-Size [real]'))[0]] = -1.
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'y-Size [real]'))[0]] = -1.
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'xSegmentBox [x0]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'xSegmentBox [x1]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'ySegmentBox [y0]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'ySegmentBox [y1]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'zSegmentBox [z0]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'zSegmentBox [z1]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'xShowBox [x0]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'xShowBox [x1]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'yShowBox [y0]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'yShowBox [y1]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'zShowBox [z0]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'zShowBox [z1]'))[0]] = -1
   *ParamStruct.pValues[(where( *ParamStruct.pNames eq 'Hide On/Off'))[0]] = 0b
   self.pParamStruct = ptr_new(ParamStruct, /no_copy)
   self.oSegContainer = obj_new('IDL_Container')
   self.oSegContainer->add, obj_new('IDL_Container')
   return, 1
end

pro C_sImageObject__define
   tmp = {C_sImageObject, pParamStruct: ptr_new(),$
                         oSegContainer: obj_new(),$
                         oIDLgrImage: obj_new() }
end