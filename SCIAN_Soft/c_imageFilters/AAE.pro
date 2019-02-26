pro AAE

ugt = [1.,0.]
uof = [1.,0.]
uoff = [1.,0.]

aae = make_array(180, /float)
aee = make_array(180, /float)
for i = 0, 179 do begin

   uof[0] = sqrt(uoff[0]^2+uoff[1]^2)*cos(!pi/180. * i)
   uof[1] = sqrt(uoff[0]^2+uoff[1]^2)*sin(!pi/180. * i)
   print, "uof", uof, i

   up = (ugt[0]*uof[0]+ugt[1]*uof[1]+1)
   down = (sqrt(ugt[0]^2+ugt[1]^2+1)*sqrt(uof[0]^2+uof[1]^2+1))
   
   aae[i] = 180./!PI * acos( up / down)
   aee[i] =  sqrt( (uof[0]-ugt[0])^2 + (uof[1]-ugt[1])^2)
;   aae[i] = 180./!PI * acos( (ugt[0]*uof[0]+ugt[1]*uof[1]+1) / (sqrt(ugt[0]^2+ugt[1]^2+1)*sqrt(uof[0]^2+uof[1]^2+1)) )
   

endfor

iplot, aae
iplot, aee
print, aae

end

