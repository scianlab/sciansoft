;_____________________________IOISIOI____________________
; NAME:
;  only for help pro...
;
; PURPOSE:
;
;
; AUTHOR:
;     FASL 2012
;     e_mail: fsantibanez@med.uchile.cl
; CALLING SEQUENCE:
;
;
; METHOHDS:
;_____________________________IOISIOI____________________
function sGetSavedSelectedParam, stack_tlb = stack_tlb
  param = -1
 
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  fUpdate = 0b

  vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
  slash = path_sep()
  if (FILE_TEST(strCompress(vPath + '_Skeleton_' + slash),/DIRECTORY) eq 0b) then FILE_MKDIR, strCompress(vPath + '_Skeleton_' + slash)
  if (FILE_TEST(strCompress(vPath + '_Skeleton_' + slash + '_Params' + slash), /DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath + '_Skeleton_' + slash + '_Params' + slash)

  filename = strCompress(vPath + strcompress('_Skeleton_' + slash + '_Params' + slash + 'ColoredParams_' + '.sav', /rem))
  defValue = 0 ; use color... or return default param value
  line     = make_Array(2,/double)
  if (FILE_TEST(filename) eq 1) then begin
    GET_LUN, inunit
    openr, inunit, filename
      READF, inunit, line 
    FREE_LUN, inunit    
    return, line[1]
   endif
  return, defValue
 end
 