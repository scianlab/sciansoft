pro s_HuygensScript_ReLister

   t_begin = 2
   t_end = 176
   t_name = 'LUIS1605_T0000_488nm_Z001'

   t_pos = strPos(t_name, '_T')
   t_left = strMid(t_name, 0, t_pos)
   t_right = strMid(t_name, t_pos+6)

   tVec = '0'
   for i = t_begin, t_end do begin

      strImageTime = strCompress(string(i), /rem)
      while(strLen(strImageTime) lt 4) do strImageTime = strCompress('0' + strImageTime, /rem)

      strImageTime = t_left + '_T' + strImageTime + t_right

      tVec = [tVec,strImageTime]
   endfor

   tVec = tVec[1:*]

   openf

   print, tVec
end