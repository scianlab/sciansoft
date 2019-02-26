;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam
;
; PURPOSE:
;       - Superclass for sROIParams
;
; AUTHOR:
;   Dr. Steffen Härtel (2001)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;        result = obj_new('C_sROIParam' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam::applyThresholdFilter, position = position

   if (position eq -1) then pos = where( *(*self.pParamStruct).pActive eq 1) else pos = position
   whParamActive = where( *(*self.pParamStruct).pActive eq 1)

   if (total(pos) gt -1) then for i = 0, n_elements(pos)-1 do begin
     fVect = bytArr(n_elements(*(*(*self.pValueStruct)[pos[i]]).pROIParamVect))
     if (n_elements(fVect) le 0) then return

     if ((((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]]) + $
          ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]]) + $
          ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]]) + $
          ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]])) le 0 ) then begin
        fVect[*] = 1
     endif else begin
        if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]]) then begin
           thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]] ) < $
                       ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1b'))[0]] )
           thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1a'))[0]] ) > $
                       ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_1b'))[0]] )
           fVect >= ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
        endif
        if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]]) then begin
           thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]] ) < $
                       ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2b'))[0]] )
           thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2a'))[0]] ) > $
                       ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_2b'))[0]] )
           fVect >= ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
        endif
        if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]]) then begin
           thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]] ) < $
                       ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3b'))[0]] )
           thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3a'))[0]] ) > $
                       ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_3b'))[0]] )
           fVect >= ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
        endif
        if ((*(*(*self.pValueStruct)[pos[i]]).pActive)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]]) then begin
           thres_min = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]] ) < $
                       ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4b'))[0]] )
           thres_max = ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4a'))[0]] ) > $
                       ((*(*(*self.pValueStruct)[pos[i]]).pValues)[(where((*(*(*self.pValueStruct)[pos[i]]).pNames) eq 'Threshold_4b'))[0]] )
           fVect >= ( ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) ge thres_min) * ((*(*(*self.pValueStruct)[pos[i]]).pROIParamVect) le thres_max) )
        endif
     endelse
     if (total(fVect) gt 0) then begin
        whfVect = where(fVect)
        *(*self.pParamStruct).pROINumberVect = (*(*self.pParamStruct).pROINumberVect)[whfVect]
        for j = 0, n_elements(whParamActive)-1 do $
           *(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect = (*(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect)[whfVect]
     endif else begin
        *(*self.pParamStruct).pROINumberVect = -1
        for j = 0, n_elements(whParamActive)-1 do *(*(*self.pValueStruct)[whParamActive[j]]).pROIParamVect = -1
     endelse
   endfor
end


pro C_sROIParam::setThresholdFilters, oROIParamReference = oROIParamReference
   transferNameList = ['Threshold_1a', 'Threshold_1b','Threshold_2a','Threshold_2b','Threshold_3a','Threshold_3b','Threshold_4a','Threshold_4b']
   for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do for j = 0, n_elements(transferNameList)-1 do begin $
     whParam = (where((*(*(*self.pValueStruct)[i]).pNames) eq transferNameList[j]))[0]
     (*(*(*self.pValueStruct)[i]).pActive)[whParam] = (*(*(*oROIParamReference.pValueStruct)[i]).pActive)[whParam]
     (*(*(*self.pValueStruct)[i]).pMin)[whParam] = (*(*(*oROIParamReference.pValueStruct)[i]).pMin)[whParam]
     (*(*(*self.pValueStruct)[i]).pMax)[whParam] = (*(*(*oROIParamReference.pValueStruct)[i]).pMax)[whParam]
     (*(*(*self.pValueStruct)[i]).pValues)[whParam] = (*(*(*oROIParamReference.pValueStruct)[i]).pValues)[whParam]
   endfor
end


pro C_sROIParam::updateParamValueStruct, objectNumberVector = objectNumberVector, position = position, all = all
   n_objectNumberVector = n_elements(objectNumberVector)
   if (n_objectNumberVector le 0) then return

   fVect = bytArr(n_elements(*(*self.pParamStruct).pROINumberVect))
   if not((n_objectNumberVector eq 1) and (objectNumberVector[0] eq -1)) then begin
     for i = 0, n_objectNumberVector-1 do begin
      whNumberVector = (where(  (*(*self.pParamStruct).pROINumberVect) eq objectNumberVector[i]  ))[0]
      if (whNumberVector ne -1) then fVect[whNumberVector] = 1b
     endfor
   endif

   if (total(fVect) gt 0) then begin
     whfVect = where(fVect)
     *(*self.pParamStruct).pROINumberVect = (*(*self.pParamStruct).pROINumberVect)[whfVect]
     if (keyWord_set(position)) then *(*(*self.pValueStruct)[position]).pROIParamVect =  (*(*(*self.pValueStruct)[position]).pROIParamVect)[whfVect]
     if (keyWord_set(all)) then $
      for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do begin
         if ( (*(*self.pParamStruct).pActive)[i]  ) then $
           *(*(*self.pValueStruct)[i]).pROIParamVect =  (*(*(*self.pValueStruct)[i]).pROIParamVect)[whfVect]
      endfor
   endif else begin
     if (keyWord_set(position)) then begin
      if ptr_valid((*(*self.pValueStruct)[position]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[position]).pROIParamVect
      (*(*self.pValueStruct)[position]).pROIParamVect = ptr_new( -1, /no_copy)
     endif
     if (keyWord_set(all)) then $
      for i = 0, n_elements(*(*self.pParamStruct).pNames)-1 do begin
         if ptr_valid((*(*self.pValueStruct)[i]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[i]).pROIParamVect
         (*(*self.pValueStruct)[i]).pROIParamVect = ptr_new( -1, /no_copy)
      endfor
   endelse
end


pro C_sROIParam::setpValueStruct, pValueStruct
   whereValueStructName = (where( *(*self.pParamStruct).pNames eq *pValueStruct.name))[0]
   if ptr_valid((*self.pValueStruct)[position]) then ptr_free, (*self.pValueStruct)[position]
    (*self.pValueStruct)[position] = ptr_new(*pValueStruct, /no_copy)
end
function C_sROIParam::getpValueStruct, position = position
   if (ptr_valid((*self.pValueStruct)[position])) then return, (*self.pValueStruct)[position]  else return, -1
end
pro C_sROIParam::setpParamStruct, pParamStruct
   if ptr_valid(self.pParamStruct) then ptr_free, self.pParamStruct
   self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
function C_sROIParam::getpParamStruct
   return, self.pParamStruct
end

pro C_sROIParam::cleanup
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

function C_sROIParam::init
   return, 1
end

pro C_sROIParam__define
   tmp = {C_sROIParam, a:1b}
end