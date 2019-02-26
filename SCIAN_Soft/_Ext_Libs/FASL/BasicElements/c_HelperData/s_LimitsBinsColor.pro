
function s_LimitsBinsColor, minValue = minValue, maxValue = maxValue, freeBinsLow = freeBinsLow, freeBinsHigh = freeBinsHigh
  deltaLimits = [0.0,0.0]

  freeBinsLow      = abs(freeBinsLow)
  freeBinsHigh     = abs(freeBinsHigh)
  if( (freeBinsLow gt 0) and (freeBinsLow gt 0)) then begin
    f0             = (255.0/freeBinsLow) - 1.0
    f1             = (255.0/freeBinsHigh) - 1.0
    d              = maxValue - minValue
    deltaLimits[1] = (d*(1.0 + f0))/((f1*f0)-1.0)
    deltaLimits[0] = (d + deltaLimits[1])/(f0)
  endif else begin
    if(freeBinsLow gt 0) then begin
      deltaLimits[0] = (d)/(f0)      
    endif else begin
      if(freeBinsLow gt 0) then begin
        deltaLimits[1] = (d)/(f1)
      endif      
    endelse  
  endelse
  
  return, deltaLimits 
end
