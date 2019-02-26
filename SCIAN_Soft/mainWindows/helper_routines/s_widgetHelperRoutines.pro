;_____________________________IOISIOI____________________
; NAME:
;       s_toggleButtonOnOffState
;
; PURPOSE:
;       toggles button between On & Off state
;
; AUTHOR:
;   Dr. Steffen Härtel (2002)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      s_toggleButtonOnOffState, widget_id
;
; REQUIRED INPUTS:
;       widget_id    ;widget_id of the button to toggle
;_____________________________IOISIOI____________________

function s_ToggleButtonOnOffState, widget_id
   widget_control, widget_id, get_value = name, /no_copy
     s = strPos(name,'(off)')
     if (s ne -1) then begin
         strput, name,'(on )', s
         ret = 1
     endif else begin
         s = strPos(name,'(on )')
         strput, name,'(off)', s
         ret = 0
     endelse
   widget_control, widget_id, set_value = name, /no_copy
   return, ret
end

function s_ToggleStringOnOffState, stringOnOff
   s = strPos(stringOnOff,'(off)')
   if (s ne -1) then begin
       strput, stringOnOff,'(on )', s
       return, 1
   endif else begin
       s = strPos(stringOnOff,'(on )')
       strput, stringOnOff,'(off)', s
       return, 0
   endelse
end


; NAME:
;       paramNameListSet
;
; PURPOSE:
;       toggles list captions between On & Off states
;
; CALLING SEQUENCE:
;       result = paramNameListSet(paramNameList, paramActiveList)
;
function paramNameListSet, paramNameList, paramActiveList
   setParamNameList = paramNameList
   for i = 0, n_elements(setParamNameList)-1 do $
     if (paramActiveList[i]) then setParamNameList[i] = '(on ) ' + setParamNameList[i] else setParamNameList[i] = '(off) ' + setParamNameList[i]
   return, setParamNameList
end


function What_Button_Pressed, ev
  return, (['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT'])[ev.press]
end


function What_Button_Released, ev
  return, (['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT'])[ev.release]
end
