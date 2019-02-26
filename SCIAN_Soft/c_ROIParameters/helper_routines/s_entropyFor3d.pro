pro s_entropyFor3d

   x = make_array(100, /float, /ind) / 100.
   y = make_array(100, /float, /ind) / 100.
   z = make_array(100, /float, /ind) / 100.

   tx = [0]
   ty = [0]
   tz = [0]
   te = [0]

   for i = 0, 99 do begin
     whereY = where(y le (1-x[i]), count)
     if count gt 0 then begin
        for j = 0, count - 1 do begin
           tx = [tx,x[i]]
           ty = [ty,y[j]]
           tz = [tz,1.-x[i]-y[j]]
           if (x[i] gt 0) and (y[j] gt 0) and ((1.-x[i]-y[j]) gt 0) then begin
              p = [x[i], y[j], (1.-x[i]-y[j])]
              te = [te, -1. * total(p * alog10(p)) / alog10(3.)]
           endif else te = [te,0.]
        endfor
     endif else begin
        tx = [tx,x[i]]
        ty = [ty,0.]
        tz = [tz,0.]
        te = [e,0.]
     endelse
   endfor
   tx = tx[1:*]
   ty = ty[1:*]
   tz = tz[1:*]
   te = te[1:*]

   iPlot, tx, ty, te, /scatter

end