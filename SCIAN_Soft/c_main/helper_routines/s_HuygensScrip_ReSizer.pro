pro s_HuygensScrip_ReSizer


   strImageName = '\\TERASVR4\Antiguo\paulina\pinchado2_export\serie5\PIN_t000_ch00_z000\'

   path = '\\TERASVR4\Antiguo\paulina\pinchado2_export\serie5\'

   iNameOld = 'PIN'

   iNameNew = 'pinchic'

   sizeFactor = .5

   s_getTimeChannelzSliceInfoFromString, strImageName, timeStr = timeStr, channelStr = channelStr, zSliceStr = zSliceStr, sInfo = sInfo
   if (timeStr ne -1) then tPos = s_getRightNumberFromString(timeStr) + sInfo.timeAdd else tPos = tPos
   if (channelStr ne -1) then chPos = s_getRightNumberFromString(channelStr) + sInfo.channelAdd else chPos = chPos
   if (zSliceStr ne -1) then zPos = s_getRightNumberFromString(zSliceStr) + sInfo.zSliceAdd else zPos = zPos

   tStr = strCompress(tPos, /rem)
   chStr = strCompress(channelStr, /rem)
   zStr = strCompress(zPos, /rem)

   tLen = 3
   chLen = 2
   zLen = 3

      ; increase output time with tPLus
   tPlus = 0

   repeat begin

      while(strLen(tStr) lt tLen) do tStr = strCompress('0' + tStr, /rem)
      while(strLen(chStr) lt chLen) do chStr = strCompress('0' + chStr, /rem)
      while(strLen(zStr) lt zLen) do zStr = strCompress('0' + zStr, /rem)

      strName = strCompress(path + iNameOld + '_t' + tStr + '_ch' + chStr + '_z' + zStr + '.tif', /rem)
      stackInfo = file_info(strName)
      if (stackInfo.exists) then begin

         repeat begin
            while(strLen(chStr) lt chLen) do chStr = strCompress('0' + chStr, /rem)
            while(strLen(zStr) lt zLen) do zStr = strCompress('0' + zStr, /rem)

            strName = strCompress(path + iNameOld + '_t' + tStr + '_ch' + chStr + '_z' + zStr + '.tif', /rem)
            stackInfo = file_info(strName)
            if (stackInfo.exists) then begin

               repeat begin
                  while(strLen(zStr) lt zLen) do zStr = strCompress('0' + zStr, /rem)

                  strName = strCompress(path + iNameOld + '_t' + tStr + '_ch' + chStr + '_z' + zStr + '.tif', /rem)
                  print, strName
                  stackInfo = file_info(strName)
                  if (stackInfo.exists) then begin
                     image = read_tiff(strName)
                     tvscl, image
                     image = congrid(image, (round(size(image, /dim)*sizeFactor))[0], (round(size(image, /dim)*sizeFactor))[1], /interp, /center)
                     tvscl, image

                     if (tPlus gt 0) then begin
                        tOld = tStr
                        tStr = strCompress(tPos+tPlus, /rem)
                        while(strLen(tStr) lt tLen) do tStr = strCompress('0' + tStr, /rem)
                        writeName = strCompress(path + iNameNew + '_t' + tStr + '_ch' + chStr + '_z' + zStr + '.tif', /rem)
                        write_tiff, writeName, image
                        tStr = tOld
                     endif else begin
                        writeName = strCompress(path + iNameNew + '_t' + tStr + '_ch' + chStr + '_z' + zStr + '.tif', /rem)
                        write_tiff, writeName, image
                     endelse

                     zPos += 1
                     zStr = strCompress(zPos, /rem)
                  endif else zPos = -1
               endRep until (zPos eq -1)

               zPos = 0
               chPos += 1
               chStr = strCompress(chPos, /rem)
               zStr = strCompress(zPos, /rem)
            endif else chPos = -1
         endRep until (chPos eq -1)

         zPos = 0
         chPos = 0
         tPos += 1
         tStr = strCompress(tPos, /rem)
         chStr = strCompress(chPos, /rem)
         zStr = strCompress(zPos, /rem)
      endif else tPos = -1
   endRep until (tPos eq -1)

end