;_____________________________IOISIOI____________________
; NAME:
;      s_HistMod
;
; PURPOSE:
;  for use variation of color in track
; AUTHOR:
;   LB (2011)
;   e_mail: 
;
; CALLING SEQUENCE:
;       
;
; 
;_____________________________IOISIOI____________________


;Lleva cada color en degrade hasta blanco 
function s_hist_white, colEV, totalTimes
  color=make_array(3,totalTimes)
  delta=round(abs(colEv-255)/totalTimes)
  color[*,0]=colEV
  for i=1, totalTimes-1 do begin
      color[*,i]=colEV+delta*i
  end
return, color
end

;Lleva cada color en degrade hasta blanco 
function s_hist_black, colEV, totalTimes
  color=make_array(3,totalTimes)
  delta=round(colEv/totalTimes)
  color[*,0]=colEV
  for i=1, totalTimes-1 do begin
      color[*,i]=colEV-delta*i
  end
return, color
end

function s_without_degrade, colEV, totalTimes
  color=make_array(3,totalTimes)
  delta=round(colEv/totalTimes)
  color[*,0]=colEV
  for i=1, totalTimes-1 do begin
      color[*,i]=colEV
  end
return, color
end

