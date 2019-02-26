;_____________________________IOISIOI____________________
; NAME:
;      C_sImageStackObject
;
; PURPOSE:
;       - Encapsulation of C_sImage objects
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageStackObject' )
;
; METHOHDS:
;     function C_sImageStackObject::init
;_____________________________IOISIOI____________________


pro C_sImageStackObject::set, pParamStruct = pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
    ptr_free, pParamStruct
end

pro C_sImageStackObject::get, pParamStruct = pParamStruct
    pParamStruct = self.pParamStruct
end
function C_sImageStackObject::getpParamStruct
    return, self.pParamStruct
end

pro C_sImageStackObject::setParamAsStruct, ParamStruct
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
end

function C_sImageStackObject::getParamAsStruct
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
          s: *pArrValues[18] }

    for i = 0, n_elements(parameterNameList)-1 do ptr_free, pArrValues[i]
    return, paramStruct
end


function C_sImageStackObject::getpSelectedImageParams, tPos = tPos, chPos = chPos, zPos = zPos
    oChannelContainerObj = self.oTimeContainer->get(position = tPos > 0)
    if obj_valid(oChannelContainerObj) then begin
       oZSliceContainerObj = oChannelContainerObj->get(position = chPos > 0)
       if obj_valid(oZSliceContainerObj) then begin
         oImage = oZSliceContainerObj->get(position = zPos > 0)
         if obj_isa(oImage, 'C_sImageObject') then return, oImage->getpParamStruct() else return, -1
       endif else return, -1
    endif else return, -1
end


function C_sImageStackObject::getSelectedImageParamNameList, tPos = tPos, chPos = chPos, zPos = zPos
    oChannelContainerObj = self.oTimeContainer->get(position = tPos > 0)
    if obj_valid(oChannelContainerObj) then begin
       oZSliceContainerObj = oChannelContainerObj->get(position = chPos > 0)
       if obj_valid(oZSliceContainerObj) then begin
         oImage = oZSliceContainerObj->get(position = zPos > 0)
          if obj_valid(oImage) then if obj_isa(oImage, 'C_sImageObject') then return, oImage->getParameterNameList()
       endif
    endif
    return, -1
end


function C_sImageStackObject::getSelectedImageParamAsStruct, tPos = tPos, chPos = chPos, zPos = zPos
    oChannelContainerObj = self.oTimeContainer->get(position = tPos > 0)
    if obj_valid(oChannelContainerObj) then begin
       oZSliceContainerObj = oChannelContainerObj->get(position = chPos > 0)
       if obj_valid(oZSliceContainerObj) then begin
         oImage = oZSliceContainerObj->get(position = zPos > 0)
         if obj_valid(oImage) then if (obj_isa(oImage, 'C_sImageObject')) then return, oImage->getParamAsStruct()
       endif
    endif
    return, -1
end


pro C_sImageStackObject::setSelectedImageParamAsStruct, paramStruct, tPos = tPos, chPos = chPos, zPos = zPos
    oChannelContainerObj = self.oTimeContainer->get(position = tPos > 0)
    if obj_valid(oChannelContainerObj) then begin
       oZSliceContainerObj = oChannelContainerObj->get(position = chPos > 0)
       if obj_valid(oZSliceContainerObj) then begin
         oImage = oZSliceContainerObj->get(position = zPos > 0)
         if obj_isa(oImage, 'C_sImageObject') then oImage->setParamAsStruct, paramStruct
       endif
    endif
end


function C_sImageStackObject::getSelectedImageObject, tPos = tPos, chPos = chPos, zPos = zPos
    pathname = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Image Path'))[0]]
    format = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Format'))[0]]

    if (tPos lt self.oTimeContainer->count()) then oChannelContainerObj = self.oTimeContainer->get(position = tPos > 0) else return, -1
    if obj_valid(oChannelContainerObj) then begin
       if (chPos lt oChannelContainerObj->count()) then oZSliceContainerObj = oChannelContainerObj->get(position = chPos > 0) else return, -1
       if obj_valid(oZSliceContainerObj) then begin
         if (zPos lt oZSliceContainerObj->count()) then begin
            oImage = oZSliceContainerObj->get(position = zPos > 0)
         endif else return, -1
         if obj_valid(oImage) then if obj_isa(oImage, 'C_sImageObject') then return, oImage else return, -1
       endif else return, -1
    endif else return, -1
end


function C_sImageStackObject::getSelectedImage, tPos = tPos, chPos = chPos, zPos = zPos
    pathname = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Image Path'))[0]]
    format = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Format'))[0]]
    oChannelContainerObj = self.oTimeContainer->get(position = tPos > 0)
    if obj_valid(oChannelContainerObj) then begin
       oZSliceContainerObj = oChannelContainerObj->get(position = ((chPos > 0) < (oChannelContainerObj->count()-1)) )
       if obj_valid(oZSliceContainerObj) then begin
         oImage = oZSliceContainerObj->get(position = ((zPos > 0) < (oZSliceContainerObj->count()-1)) )
         if obj_valid(oImage) then begin
          if obj_isa(oImage, 'C_sImageObject') then begin
              oImage->get, pParamStruct = pParamStruct
              filename = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
              dummy = query_tiff(pathname + filename + format, info)
              print, pathname+filename+format
;              if (n_elements(info) gt 0) then return, congrid(read_tiff(pathname + filename + format),512,512) else return, bytArr(3,3)
              if (n_elements(info) gt 0) then return, read_tiff(pathname + filename + format) else return, bytArr(3,3)
          endif else return, bytArr(3,3)
         endif else return, bytArr(3,3)
       endif else return, bytArr(3,3)
    endif else return, bytArr(3,3)
end


pro C_sImageStackObject::saveSelectedImage, image, tPos = tPos, chPos = chPos, zPos = zPos
   if (n_elements(image) eq 0) then return
   pathname = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Image Path'))[0]]
   format = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Format'))[0]]
   oChannelContainerObj = self.oTimeContainer->get(position = tPos > 0)
   if obj_valid(oChannelContainerObj) then begin
      oZSliceContainerObj = oChannelContainerObj->get(position = ((chPos > 0) < (oChannelContainerObj->count()-1)) )
      if obj_valid(oZSliceContainerObj) then begin
         oImage = oZSliceContainerObj->get(position = ((zPos > 0) < (oZSliceContainerObj->count()-1)) )
         if obj_valid(oImage) then begin
            if obj_isa(oImage, 'C_sImageObject') then begin
               oImage->get, pParamStruct = pParamStruct
               filename = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
               write_tiff, pathname + filename + format, image
            endif
         endif
      endif
   endif
end


function C_sImageStackObject::getSelectedSegObj, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                 clusPos = clusPos, segPos = segPos

    oImage = self->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    if obj_valid(oImage) then return, oImage->getSelectedSegObj(clusPos = clusPos, segPos = segPos) else return, -1
end


function C_sImageStackObject::applyImageSegmentation, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                      clusPos = clusPos, segPos = segPos, cut_x = cut_x, cut_y = cut_y

    oImage = self->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
    if obj_valid(oImage) then return, oImage->applyImageSegmentation(stack_tlb = stack_tlb, selectedStackObject = self,$
                                                      tPos = tPos,$
                                                      chPos = chPos,$
                                                      zPos = zPos,$
                                                      clusPos = clusPos,$
                                                      segPos = segPos,$
                                                      cut_x = cut_x, cut_y = cut_y) $
    else return, -1
end


function C_sImageStackObject::removeSelectedObject, tPos = tPos, chPos = chPos, zPos = zPos, moveActive = moveActive

    if ((moveActive eq 0) and obj_valid(self.oTimeContainer)) then begin
       oChannelContainerObj = self.oTimeContainer->get(position = tPos)
       if obj_valid(oChannelContainerObj) then begin
         for channelPos = (oChannelContainerObj->count()-1), 0, -1 do begin
          oZSliceContainerObj = oChannelContainerObj->get(position = channelPos)
          if obj_valid(oZSliceContainerObj) then begin
              for zSlicePos = (oZSliceContainerObj->count()-1), 0, -1 do begin
                 oImage = oZSliceContainerObj->get(position = zSlicePos)
                 oZSliceContainerObj->remove, position = zSlicePos
                 if obj_valid(oImage) then obj_destroy, oImage
              endfor; zSlice
          endif
          oChannelContainerObj->remove, position = channelPos
          if obj_valid(oZSliceContainerObj) then obj_destroy, oZSliceContainerObj
         endfor; channel
         self.oTimeContainer->remove, position = tPos
         if obj_valid(oChannelContainerObj) then obj_destroy, oChannelContainerObj
         return, 1
       endif
    endif

    oChannelContainerObj = self.oTimeContainer->get(position = tPos)
    if ((moveActive eq 1) and obj_valid(oChannelContainerObj)) then begin
       oZSliceContainerObj = oChannelContainerObj->get(position = chPos)
       if obj_valid(oZSliceContainerObj) then begin
         for zSlicePos = (oZSliceContainerObj->count()-1), 0, -1 do begin
          oImage = oZSliceContainerObj->get(position = zSlicePos)
          oZSliceContainerObj->remove, position = zSlicePos
          if obj_valid(oImage) then obj_destroy, oImage
         endfor; zSlice
       endif
       oChannelContainerObj->remove, position = chPos
       if obj_valid(oZSliceContainerObj) then obj_destroy, oZSliceContainerObj
       return, 1
    endif

    if obj_valid(oChannelContainerObj) then oZSliceContainerObj = oChannelContainerObj->get(position = chPos)
    if ((moveActive eq 2) and obj_valid(oZSliceContainerObj) and zPos ge 0) then begin
       oImage = oZSliceContainerObj->get(position = zPos)
       oZSliceContainerObj->remove, position = zPos
       if obj_valid(oImage) then obj_destroy, oImage
       return, 1
    endif
    return, -1
end


function C_sImageStackObject::moveSelectedObject, tPos = tPos, chPos = chPos, zPos = zPos,$
                                                       moveTo = moveTo, moveActive = moveActive

    if ((moveActive eq 0) and (obj_valid(self.oTimeContainer))) then begin
       if ( ((tPos ge 1) and (moveTo lt tPos)) or ((tPos lt (self.oTimeContainer->count()-1)) and (moveTo gt tPos)) ) then begin
         self.oTimeContainer->move, tPos, moveTo
         return, 1
       endif else return, -1
    endif
    oChannelContainerObj = self.oTimeContainer->get(position = tPos > 0)
    if ((moveActive eq 1) and (obj_valid(oChannelContainerObj)))  then begin
       if ( ((chPos ge 1) and (moveTo lt chPos)) or ((chPos lt (oChannelContainerObj->count()-1)) and (moveTo gt chPos)) ) then begin
         oChannelContainerObj->move, chPos, moveTo
         return, 1
       endif else return, -1
    endif
    oZSliceContainerObj = oChannelContainerObj->get(position = chPos > 0)
    if ((moveActive eq 2) and (obj_valid(oZSliceContainerObj))) then begin
       if ( ((zPos ge 1) and (moveTo lt zPos)) or ((zPos lt (oZSliceContainerObj->count()-1)) and (moveTo gt zPos)) ) then begin
         oZSliceContainerObj->move, zPos, moveTo
         return, 1
       endif else return, -1
    endif
    return, -1
end


pro C_sImageStackObject::getTimeChannelZSliceNameLists, timeList = timeList, channelList = channelList, zSliceList = zSliceList,$
                                            tPos = tPos, chPos = chPos, zPos = zPos, moveActive = moveActive

    totalTNum = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of Times'))[0]] > 1
    totalTimeDigits = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number of Time Digits'))[0]] > strLen(strCompress(totalTNum, /rem))
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number of Time Digits'))[0]] = totalTimeDigits
    totalChNum = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of Channels'))[0]] > 1
    totalZNum = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] > 1

    case moveActive of
       0: begin
              ; actualize array-size in TimeContainerObjArray
          nContainer = (self.oTimeContainer->count()-1) > 0
          if (tPos gt nContainer) then tPos = nContainer
          oChannelContainerObj = self.oTimeContainer->get(position = tPos)
          listNumbers = strCompress('00000' + make_array(nContainer+1, /string, /index), /rem)
          timeList = strCompress('Time_' +  strMid(listNumbers, totalTimeDigits-1, /rev), /rem)
              ; actualize array-size in ChannelContainerObjArray
          if (obj_valid(oChannelContainerObj)) then begin
              nContainer = (oChannelContainerObj->count()-1) > 0
              if (chPos gt nContainer) then chPos = nContainer
              oZSliceContainerObj = oChannelContainerObj->get(position = chPos)
              listNumbers = strCompress('00000' + make_array(nContainer+1, /string, /index), /rem)
              channelList = strCompress('Channel_' +  strMid(listNumbers, 2, /rev), /rem)
          endif else begin
              chPos = 0
              channelList = strCompress('-NoChannel-', /rem)
          endelse
              ; actualize array-size in ZSliceContainerObjArray
          if (obj_valid(oZSliceContainerObj)) then begin
              nContainer = (oZSliceContainerObj->count()-1)
              if (nContainer eq -1) then begin
                 zPos = 0
                 zSliceList = strCompress('-NoZSlice-', /rem)
              endif else begin
                 nContainer = nContainer > 0
                 if (zPos gt nContainer) then zPos = nContainer
                 listNumbers = strCompress('00000' + make_array(nContainer+1, /string, /index), /rem)
                 zSliceList = strCompress('ZSlice_' +  strMid(listNumbers, 2, /rev), /rem)
              endelse
          endif else begin
              zPos = 0
              zSliceList = strCompress('-NoZSliceObject-', /rem)
          endelse
         endcase
       1: begin
              ; actualize array-size in TimeContainerObjArray
          nContainer = -1
          for i = 0, (self.oTimeContainer->count()-1) > 0 do begin
              oChannelContainerObj = self.oTimeContainer->get(position = i)
              if ((oChannelContainerObj->count()-1) ge chPos) then nContainer = [nContainer, i]
          endfor
          if (n_elements(nContainer) gt 1) then nContainer = nContainer[1:*]
          listNumbers = strCompress('00000' + string(nContainer), /rem)
          timeList = strCompress('Time_' +  strMid(listNumbers, totalTimeDigits-1, /rev), /rem)

          oChannelContainerObj = self.oTimeContainer->get(position = tPos)
              ; actualize array-size in ChannelContainerObjArray
          if (obj_valid(oChannelContainerObj)) then begin
              nContainer = (oChannelContainerObj->count()-1) > 0
              if (chPos gt nContainer) then chPos = nContainer
              oZSliceContainerObj = oChannelContainerObj->get(position = chPos)
              listNumbers = strCompress('00000' + make_array(nContainer+1, /string, /index), /rem)
              channelList = strCompress('Channel_' +  strMid(listNumbers, 2, /rev), /rem)
          endif else begin
              chPos = 0
              channelList = strCompress('-NoChannel-', /rem)
          endelse

              ; actualize array-size in ZSliceContainerObjArray
          if (obj_valid(oZSliceContainerObj)) then begin
              nContainer = (oZSliceContainerObj->count()-1)
              if (nContainer eq -1) then begin
                 zPos = 0
                 zSliceList = strCompress('-NoZSlice-', /rem)
              endif else begin
                 nContainer = nContainer > 0
                 if (zPos gt nContainer) then zPos = nContainer
                 listNumbers = strCompress('00000' + make_array(nContainer+1, /string, /index), /rem)
                 zSliceList = strCompress('ZSlice_' +  strMid(listNumbers, 2, /rev), /rem)
              endelse
          endif else begin
              zPos = 0
              zSliceList = strCompress('-NoZSliceObject-', /rem)
          endelse
         endcase
       2: begin
              ; actualize array-size in TimeContainerObjArray
          nContainer = (self.oTimeContainer->count()-1) > 0
          if (tPos gt nContainer) then tPos = nContainer
          oChannelContainerObj = self.oTimeContainer->get(position = tPos)
          listNumbers = strCompress('00000' + make_array(nContainer+1, /string, /index), /rem)
          timeList = strCompress('Time_' +  strMid(listNumbers, totalTimeDigits-1, /rev), /rem)
              ; actualize array-size in ChannelContainerObjArray
          if (obj_valid(oChannelContainerObj)) then begin
              nContainer = (oChannelContainerObj->count()-1) > 0
              if (chPos gt nContainer) then chPos = nContainer
              oZSliceContainerObj = oChannelContainerObj->get(position = chPos)
              listNumbers = strCompress('00000' + make_array(nContainer+1, /string, /index), /rem)
              channelList = strCompress('Channel_' +  strMid(listNumbers, 2, /rev), /rem)
          endif else begin
              chPos = 0
              channelList = strCompress('-NoChannel-', /rem)
          endelse
              ; actualize array-size in ZSliceContainerObjArray
          if (obj_valid(oZSliceContainerObj)) then begin
              nContainer = (oZSliceContainerObj->count()-1)
              if (nContainer eq -1) then begin
                 zPos = 0
                 zSliceList = strCompress('-NoZSlice-', /rem)
              endif else begin
                 nContainer = nContainer > 0
                 if (zPos gt nContainer) then zPos = nContainer
                 listNumbers = strCompress('00000' + make_array(nContainer+1, /string, /index), /rem)
                 zSliceList = strCompress('ZSlice_' +  strMid(listNumbers, 2, /rev), /rem)
              endelse
          endif else begin
              zPos = 0
              zSliceList = strCompress('-NoZSliceObject-', /rem)
          endelse
         endcase
    endcase
end


pro C_sImageStackObject::addSelectedImageObj, oImage, tPos = tPos, chPos = chPos, zPos = zPos

    if obj_valid(oImage) then begin  ; affirm oImage as C_sImageObject
       if not(obj_isa(oImage, 'C_sImageObject')) then begin
         obj_destroy, oImage
         oImage = obj_new('C_sImageObject')
       endif
    endif else oImage = obj_new('C_sImageObject')
print, tPos, chPos, zPos

       ; actualize array-size in TimeContainerObjArray
    for i = (self.oTimeContainer->count())>0, tPos do self.oTimeContainer->add, obj_new('IDL_Container'), position = i
       ; get TimeContainerObj
    oChannelContainerObj = self.oTimeContainer->get(position = tPos)

       ; actualize array-size in ChannelContainerObjArray
    for i = (oChannelContainerObj->count())>0, chPos do oChannelContainerObj->add, obj_new('IDL_Container'), position = i
       ; get ChannelContainerObj
    oZSliceContainerObj = oChannelContainerObj->get(position = chPos)

       ; actualize array-size in ZSliceContainerObjArray
    for i = (oZSliceContainerObj->count())>0, zPos do oZSliceContainerObj->add, obj_new('C_sImageObject'), position = i
    remObj = oZSliceContainerObj->get(position = zPos)
       ; get ZSliceContainerObj
    oZSliceContainerObj->remove, position = zPos
    if (obj_valid(remObj)) then obj_destroy, remObj
    oZSliceContainerObj->add, oImage, position = zPos
end


pro C_sImageStackObject::updateTimeChannelZStackDimensions, tPos = tPos, chPos = chPos, zPos = zPos

    nTimes = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of Times'))[0]]
    nChannels = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of Channels'))[0]]
    nZSlices = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]

       ; actualize array-size in TimeContainerObjArray
    nCount = self.oTimeContainer->count()
    if (nTimes gt nCount) then $
       for i = (nCount>0), (nTimes-1) do self.oTimeContainer->add, obj_new('IDL_Container'), position = i
    if (nTimes lt nCount) then $
       for i = (nCount-1), (nTimes>0), -1 do dummy = self->removeSelectedObject(tPos = i, chPos = chPos, zPos = zPos, moveActive = 0b)
    tPos = (tPos < (nTimes-1)) > 0
       ; get TimeContainerObj
    oChannelContainerObj = self.oTimeContainer->get(position = tPos)

       ; actualize array-size in ChannelContainerObjArray
    nCount = oChannelContainerObj->count()
    if (nChannels gt nCount) then $
       for i = (nCount>0), (nChannels-1) do oChannelContainerObj->add, obj_new('IDL_Container'), position = i
    if (nChannels lt nCount) then $
       for i = (nCount-1), (nChannels>0), -1 do dummy = self->removeSelectedObject(tPos = tPos, chPos = i, zPos = zPos, moveActive = 1b)
    chPos = (chPos < (nChannels-1)) > 0
       ; get ChannelContainerObj
    oZSliceContainerObj = oChannelContainerObj->get(position = chPos)

    nCount = oZSliceContainerObj->count()
    if (nZSlices gt nCount) then $
       for i = nCount>0, (nZSlices-1) do oZSliceContainerObj->add, obj_new('C_sImageObject'), position = i
    if (nZSlices lt nCount) then $
       for i = nCount, ((nZSlices+1)>0), -1 do dummy = self->removeSelectedObject(tPos = tPos, chPos = chPos, zPos = i-1, moveActive = 2b)
    zPos = (zPos < (nZSlices-1)) > 0
end


pro C_sImageStackObject::getSelectedClusterMask, mask = mask_1, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    pathname = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Mask Path'))[0]]
    format = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Format'))[0]]
    if (n_elements(clusPos) eq 0) then clusterName = strCompress('_Clus' + string(0)+'_', /rem) else clusterName = strCompress('_Clus' + string(clusPos)+'_', /rem)
    (self->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos))->get, pParamStruct = pParamStruct
    filename = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
    dummy = query_tiff(pathname+clusterName+filename+format, info)
    if (n_elements(info) gt 0) then mask_1 = read_tiff(pathname+clusterName+filename+format) else mask_1 = -1
end


function C_sImageStackObject::getSelectedClusterMask, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
    pathname = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Mask Path'))[0]]
    format = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Format'))[0]]
    if (n_elements(clusPos) eq 0) then clusterName = strCompress('_Clus' + string(0)+'_', /rem) else clusterName = strCompress('_Clus' + string(clusPos)+'_', /rem)
    (self->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos))->get, pParamStruct = pParamStruct
    filename = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
    dummy = query_tiff(pathname+clusterName+filename+format, info)
    if (n_elements(info) gt 0) then return, read_tiff(pathname+clusterName+filename+format) else return, -1
end


pro C_sImageStackObject::saveSelectedClusterMask, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, mask = mask
    pathname = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Mask Path'))[0]]
    format = *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'Format'))[0]]
    (self->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos))->get, pParamStruct = pParamStruct
    filename = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Name'))[0]]
    write_tiff, pathname+ strCompress('_Clus' + string(clusPos)+'_', /rem) + filename + format, mask
end


pro C_sImageStackObject::cleanup
    ptr_free, (*self.pParamStruct).pNames
    for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
    ptr_free, self.pParamStruct

    if obj_valid(self.oTimeContainer) then begin
       for tPos = self.oTimeContainer->count()-1, 0, -1 do begin
         oChannelContainerObj = self.oTimeContainer->get(position = tPos)
         if obj_valid(oChannelContainerObj) then begin
          for chPos = (oChannelContainerObj->count()-1), 0, -1 do begin
              oZSliceContainerObj = oChannelContainerObj->get(position = chPos)
              if obj_valid(oZSliceContainerObj) then begin
                 for zPos = (oZSliceContainerObj->count()-1), 0, -1 do begin
                   oImage = oZSliceContainerObj->get(position = zPos)
                   oZSliceContainerObj->remove, position = zPos
                   obj_destroy, oImage
                 endfor; zSlice
              endif
              oChannelContainerObj->remove, position = chPos
              obj_destroy, oZSliceContainerObj
          endfor; channel
         endif
         self.oTimeContainer->remove, position = tPos
         obj_destroy, oChannelContainerObj
       endfor;time
    obj_destroy, self.oTimeContainer
    endif
end


function C_sImageStackObject::getParameterNameList
    return, ['Stack Path',$
             'Mask Path',$
             'Parameter Path',$
             'Image Path',$
             'Name',$
             'Format',$
             'Image Bit-Rate',$
             'Number of Time Digits',$
             'Total Number of Times',$
             'Total Number of Channels',$
             'Total Number of z-Slices',$
             'Total Time [s]',$
             'Time Interval [s]',$
             'x-Size [pixel]',$
             'y-Size [pixel]',$
             'x-Size [real]',$
             'y-Size [real]',$
             'z-Interval [real]',$
             'Comments']
end


function C_sImageStackObject::init

    StackParameterNameList = self->getParameterNameList()
    nNames = n_elements(StackParameterNameList)
    paramStruct = {ImageStackParamStruct,$
                   pValues:ptrArr(nNames, /allocate),$         ; Pointer on Filter Parameter Values.
                   pNames:ptr_new(StackParameterNameList, /no_copy)}       ; Pointer on Filter Parameter Names.

    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Stack Path'))[0]] = s_getPathForSystem()
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Mask Path'))[0]] = s_getPathForSystem()
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Parameter Path'))[0]] = s_getPathForSystem()
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Image Path'))[0]] = s_getPathForSystem()
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Name'))[0]] = '-NO NAME-'
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Format'))[0]] = '.tif'
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Image Bit-Rate'))[0]] = 8
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Number of Time Digits'))[0]] = 3
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Total Number of Times'))[0]] = 1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Total Number of Channels'))[0]] = 1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Total Number of z-Slices'))[0]] = 1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Total Time [s]'))[0]] = -1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Time Interval [s]'))[0]] = -1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'x-Size [pixel]'))[0]] = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'y-Size [pixel]'))[0]] = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'x-Size [real]'))[0]] = -1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'y-Size [real]'))[0]] = -1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Interval [real]'))[0]] = -1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Comments'))[0]] = 'No Comments'

    self.pParamStruct = ptr_new(paramStruct, /no_copy)

    oZSliceContainerObj = obj_new('IDL_Container')
    oZSliceContainerObj->add, obj_new('C_sImageObject')
    oChannelContainer = obj_new('IDL_Container')
    oChannelContainer->add, oZSliceContainerObj
    self.oTimeContainer = obj_new('IDL_Container')
    self.oTimeContainer->add, oChannelContainer
    return, 1
end

pro C_sImageStackObject__define
   tmp = {C_sImageStackObject, pParamStruct:ptr_new(),$
                               oTimeContainer:obj_new() }
end
