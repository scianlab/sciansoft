;_____________________________IOISIOI____________________
; NAME:
;      s_getTimeChannelZSlizeInfoFromString
;
; PURPOSE:
;
; AUTHOR:
;   Dr. Steffen Härtel (2003)
;   e_mail: shaertel@physik.uni-bremen.de
;   Modified by FASL 2011... include 647nm channel
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________

pro s_getTimeChannelZSliceInfoFromString, strImageName, timeStr = timeStr, channelStr = channelStr, zSliceStr = zSliceStr, sInfo = sInfo

   timeStr = -1
   channelStr = -1
   zSliceStr = -1
   sInfo = {DataType:"Huygens", timeAdd:0, channelAdd:0, zSliceAdd:0}

   zSliceNumber = 0
   if (strlen(strImageName) gt 0) then begin
     result = strPos(strImageName, '_t', /reverse_search)
     if (result eq -1) then result = strPos(strImageName, '_T', /reverse_search)
     add = 2
     if (result eq -1) then begin
        result = strPos(strImageName, 'T', /reverse_search)
        add = 1
     endif
     if (result ne -1) then timeStr = s_getLeftNumberFromStringAsString(strMid(strImageName, result+add))

     result = strPos(strImageName, '_z', /reverse_search)
     if (result eq -1) then result = strPos(strImageName, '_Z', /reverse_search)
     add = 2
     if (result eq -1) then begin
        result = strPos(strImageName, '_co', /reverse_search)
        add = 3
     endif
      if (result eq -1) then begin
        result = strPos(strImageName, 'Z', /reverse_search)
        add = 1
        sInfo.zSliceAdd = -1
        sInfo.timeAdd = -1
        sInfo.channelAdd = -1
     endif    
     if (result ne -1) then zSliceStr      = s_getLeftNumberFromStringAsString(strMid(strImageName, result+add))

     result = strPos(strImageName, '_ch', /reverse_search)
     add = 3
     if (result eq -1) then result = strPos(strImageName, '_CH', /reverse_search)
     if (result eq -1) then result = strPos(strImageName, '_Ch', /reverse_search)
     if (result eq -1) then begin
        result = strPos(strImageName, '_C', /reverse_search)
        if (result ne -1) then begin
           sInfo.DataType = 'Olympus'
           sInfo.channelAdd = -1
           sInfo.zSliceAdd = -1
           sInfo.timeAdd = -1
           channelStr = s_getLeftNumberFromStringAsString(strMid(strImageName, result+2))
           zSliceStr = s_getLeftNumberFromStringAsString(strMid(strImageName, result+6))
           timeStr = s_getLeftNumberFromStringAsString(strMid(strImageName, result+10))
           result = -1
           add = 2
        endif
         if (result eq -1) then begin
            result = strPos(strImageName, 'C', /reverse_search)
            add = 1
            if (result ne -1) then begin
              channelStr = s_getLeftNumberFromStringAsString(strMid(strImageName, result+add))
            endif
         endif          
     endif
     if (result eq -1) then begin
        result = strPos(strImageName, '488nm', /reverse_search)
        if (result ne -1) then channelStr = '000'
        if (result ne -1) then sInfo.zSliceAdd = -1
        result = strPos(strImageName, '568nm', /reverse_search)
        if (result ne -1) then channelStr = '001'
        if (result ne -1) then sInfo.zSliceAdd = -1
        result = strPos(strImageName, '647nm', /reverse_search)
        if (result ne -1) then channelStr = '002'
        if (result ne -1) then sInfo.zSliceAdd = -1
        result = -1
     endif
     ; TODO wtf? if (result ne -1) then channelStr = s_getLeftNumberFromStringAsString(strMid(strImageName, result+3))
     if (result ne -1) then channelStr = s_getLeftNumberFromStringAsString(strMid(strImageName, result+add))
   endif
end
