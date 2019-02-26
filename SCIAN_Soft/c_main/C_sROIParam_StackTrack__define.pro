;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_StackTrack
;
; PURPOSE:
;       - A container of object variables
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_StackTrack' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_StackTrack::setpValueStruct, pValueStruct
    if ptr_valid(self.pValueStruct) then ptr_free, self.pValueStruct
    self.pValueStruct = ptr_new(*pValueStruct, /no_copy)
end

function C_sROIParam_StackTrack::getpValueStruct
   return, self.pValueStruct
end


function C_sROIParam_StackTrack::getAllParamVect
    case (*self.pValueStruct).type of
       'Group ROI-Parameter-Method': return, (*(*self.pValueStruct).pROIParamVect)[*]
       'Single ROI-Parameter-Method': begin
          for i = 0, n_elements((*self.pValueStruct).pROIAllParamVect)-1 do begin
             if ptr_valid((*self.pValueStruct).pROIAllParamVect[i]) then begin
                if (n_elements(allParamVect) eq 0) then allParamVect = *((*self.pValueStruct).pROIAllParamVect[i]) else allParamVect = [allParamVect, *((*self.pValueStruct).pROIAllParamVect[i])]
             endif
          endfor
          if (n_elements(allParamVect) gt 0) then return, allParamVect else return, -1
       endcase
       '3D ROI-Parameter-Method': begin
          for i = 0, n_elements((*self.pValueStruct).pROIAllParamVect)-1 do begin
             if ptr_valid((*self.pValueStruct).pROIAllParamVect[i]) then begin
               if (n_elements(allParamVect) eq 0) then allParamVect = *((*self.pValueStruct).pROIAllParamVect[i]) else allParamVect = [allParamVect, *((*self.pValueStruct).pROIAllParamVect[i])]
             endif
          endfor
          if (n_elements(allParamVect) gt 0) then return, allParamVect else return, -1
       endcase
       'Inter ROI-Parameter-Method': begin
              for i = 0, n_elements((*self.pValueStruct).pROIAllParamVect)-1 do begin
                 if (ptr_valid((*self.pValueStruct).pROIAllParamVect[i])) then $
                   if (n_elements(allParamVect) eq 0) then allParamVect = reform(*((*self.pValueStruct).pROIAllParamVect[i])) else allParamVect = [allParamVect, reform(*((*self.pValueStruct).pROIAllParamVect[i]))]
              endfor
          return, allParamVect
         endcase
    endcase
end


pro C_sROIParam_StackTrack::addTrackParam, pValueStruct = pValueStruct,$
                                           tPos = tPos,$
                                           totalTNum = totalTNum,$
                                           fAllParamControl = fAllParamControl

    if (n_elements(fAllParamControl) gt 0) then (*self.pValueStruct).fAllParamControl = fAllParamControl

    if (n_elements((*(*self.pValueStruct).pROIParamVect)) ne totalTNum) then begin
       if (n_elements((*(*self.pValueStruct).pROIParamVect)) lt totalTNum) then begin
         addParams = totalTNum - n_elements((*(*self.pValueStruct).pROIParamVect))
         paramType = size((*(*self.pValueStruct).pROIParamVect), /type)
         (*(*self.pValueStruct).pROIParamVect) = [(*(*self.pValueStruct).pROIParamVect), make_array(addParams, type = paramType)]
         paramType = size((*(*self.pValueStruct).pROIErrorVect), /type)
         (*(*self.pValueStruct).pROIErrorVect) = [(*(*self.pValueStruct).pROIErrorVect), make_array(addParams, type = paramType)]
       endif else begin
         (*(*self.pValueStruct).pROIParamVect) = (*(*self.pValueStruct).pROIParamVect)[0:(totalTNum-1)>0]
         (*(*self.pValueStruct).pROIErrorVect) = (*(*self.pValueStruct).pROIErrorVect)[0:(totalTNum-1)>0]
       endelse
    endif

    case (*self.pValueStruct).type of
       'Group ROI-Parameter-Method': begin
              (*(*self.pValueStruct).pROIParamVect)[tPos] = *(*pValueStruct).pROIParamVect
              case (*self.pValueStruct).name of
              'Group Object Number': (*(*self.pValueStruct).pROIErrorVect)[tPos] = sqrt(abs(*(*pValueStruct).pROIParamVect))
              else: (*(*self.pValueStruct).pROIErrorVect)[tPos] = 0.
              endcase
       endcase
       'Single ROI-Parameter-Method': begin
              if (n_elements(*(*pValueStruct).pROIParamVect) lt 2) then momentValue = [(*(*pValueStruct).pROIParamVect)[0], 0] else $
                 momentValue = moment(*(*pValueStruct).pROIParamVect)
              (*(*self.pValueStruct).pROIParamVect)[tPos] = momentValue[0]   ; meanValue
              case (*self.pValueStruct).name of
                 '':
                 else: (*(*self.pValueStruct).pROIErrorVect)[tPos] = sqrt(momentValue[1]) ; standard deviation = sqrt(variance)
              endcase

              if (*self.pValueStruct).fAllParamControl then begin
                 if (ptr_valid(  (*self.pValueStruct).pROIAllParamVect[tPos] )) then *((*self.pValueStruct).pROIAllParamVect[tPos]) = *(*pValueStruct).pROIParamVect $
                   else (*self.pValueStruct).pROIAllParamVect[tPos] = ptr_new(*(*pValueStruct).pROIParamVect)
              endif
       endcase
       '3D ROI-Parameter-Method': begin
              if (n_elements(*(*pValueStruct).pROIParamVect) lt 2) then momentValue = [(*(*pValueStruct).pROIParamVect)[0], 0] else $
                 momentValue = moment(*(*pValueStruct).pROIParamVect)
              (*(*self.pValueStruct).pROIParamVect)[tPos] = momentValue[0]   ; meanValue
              case (*self.pValueStruct).name of
                 '':
                 else:(*(*self.pValueStruct).pROIErrorVect)[tPos] = sqrt(momentValue[1]) ; standard deviation = sqrt(variance)
              endcase

              if ((*self.pValueStruct).fAllParamControl) then begin
                 if ptr_valid((*self.pValueStruct).pROIAllParamVect[tPos]) then *((*self.pValueStruct).pROIAllParamVect[tPos]) = *(*pValueStruct).pROIParamVect $
                   else (*self.pValueStruct).pROIAllParamVect[tPos] = ptr_new(*(*pValueStruct).pROIParamVect)
              endif
       endcase
       'Inter ROI-Parameter-Method': begin
              if (n_elements(*(*pValueStruct).pROIParamVect) lt 2) then momentValue = [(*(*pValueStruct).pROIParamVect)[0], 0] else $
                 momentValue = moment(*(*pValueStruct).pROIParamVect)
              (*(*self.pValueStruct).pROIParamVect)[tPos] = momentValue[0]   ; meanValue
              case (*self.pValueStruct).name of
                 '':
                 else:(*(*self.pValueStruct).pROIErrorVect)[tPos] = sqrt(momentValue[1]) ; standard deviation = sqrt(variance)
              endcase

              if (*self.pValueStruct).fAllParamControl then begin
                 if ptr_valid(  (*self.pValueStruct).pROIAllParamVect[tPos]) then *((*self.pValueStruct).pROIAllParamVect[tPos]) = *(*pValueStruct).pROIParamVect $
                   else (*self.pValueStruct).pROIAllParamVect[tPos] = ptr_new(*(*pValueStruct).pROIParamVect)
              endif
          endcase
    endcase
end


pro C_sROIParam_StackTrack::cleanUp
   for i = 0, n_elements((*self.pValueStruct).pROIAllParamVect)-1 do $
      if ptr_valid((*self.pValueStruct).pROIAllParamVect[i]) then ptr_free, (*self.pValueStruct).pROIAllParamVect[i]

   for i = 0, n_tags((*self.pValueStruct))-1 do begin
      case size((*self.pValueStruct).(i), /tname) of
      'POINTER':ptr_free, (*self.pValueStruct).(i)
      'OBJREF':obj_destroy, (*self.pValueStruct).(i)
      else:
      endcase
   endfor
   ptr_free, self.pValueStruct
end


function C_sROIParam_StackTrack::init, pValueStruct = pValueStruct,$
                                        strID = strID,$
                                        tPos = tPos,$
                                        totalTNum = totalTNum,$
                                        fAllParamControl = fAllParamControl

    StackTrackValueStruct = {name:strID +(*pValueStruct).name,$
                             type:(*pValueStruct).type,$
                             pWidgetType:ptr_new(*(*pValueStruct).pWidgetType),$
                             pNames:ptr_new(*(*pValueStruct).pNames),$
                             pActive:ptr_new(*(*pValueStruct).pActive),$
                             pMin:ptr_new(*(*pValueStruct).pMin),$
                             pMax:ptr_new(*(*pValueStruct).pMax),$
                             pValues:ptr_new(*(*pValueStruct).pValues),$
                             fAllParamControl:fAllParamControl,$
                             pROIAllParamVect:ptrArr(totalTNum),$                  ; PointerArr on All StackTrack Parameter Vector.
                             pROIErrorVect:ptr_new(fltArr(totalTNum)),$          ; Pointer on StackTrack Error Vector.
                             pROIParamVect:ptr_new(fltArr(totalTNum))}          ; Pointer on StackTrack Parameter Vector.

    self.pValueStruct = ptr_new(StackTrackValueStruct, /no_copy)

    self->addTrackParam, pValueStruct = pValueStruct,$
                         tPos = tPos,$
                         totalTNum = totalTNum,$
                         fAllParamControl = fAllParamControl
    return, 1
end

pro C_sROIParam_StackTrack__define
   tmp = {C_sROIParam_StackTrack, pValueStruct:ptr_new()}
end