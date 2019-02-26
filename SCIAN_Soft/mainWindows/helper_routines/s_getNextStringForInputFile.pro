;_____________________________IOISIOI____________________
; NAME:
;      s_getRightNumberFromSting
;
; PURPOSE:
;      return the string the next .tif file with correct format
;      if the file with this string not exist... then return -1
; AUTHOR:
;   FASL (2011)
;   e_mail: cuentasfsantibanez@gmail.com
;
; CALLING SEQUENCE:
;_____________________________IOISIOI____________________

function s_getNextStringForInputFile, strName,levelActual,offsetZero, offsetZeroChannel
; levelAction define the level to increse in one... the others level (at right) change to "0", the left levels keep his values
; levelAction usually is 1 or 2 (1: channel, 2:time) and "0" is keep for functionality
   actualString = strName
   largoVariable = 3
   strLenStr = strlen(actualString)
   if ((strLenStr gt 0) and (levelActual ge 0) and (levelActual le 2)) then begin
      ; Modify zSilce value
      add = 2
      result = strPos(strName, '_Z', /reverse_search)
      if (result eq -1) then result = strPos(actualString, '_z', /reverse_search) $
      else begin
         largoVariable = 4
         ;offsetZero = 1
      endelse
      if (result eq -1) then begin
        result = strPos(actualString, '_co', /reverse_search)
        add = 3
      endif
       if (result eq -1) then begin
        result = strPos(actualString, 'Z', /reverse_search)
        add = 1
        largoVariable = 3
      endif     
      if (result ne -1) then begin
         ; Obtain string and data 
         baseNameFile = strMid(actualString,0,result+add)
         zSlideActual = s_getRightNumberFromString(strMid(actualString, result+add,largoVariable))
         endNameFile  = strMid(actualString,result+add+largoVariable)
         if(levelActual eq 0) then begin ; only increase zslice value
            ; increase value of level data and obtain string
            zSlideActual++
         endif else begin
            ;Level to change is greater and need set zSlide to "0"
            zSlideActual = offsetZero
         endelse 
         actualLevelString = strCompress(STRING(zSlideActual), /rem)
         if(strlen(actualLevelString) eq 1) then actualLevelString = '00' + actualLevelString
         if(strlen(actualLevelString) eq 2) then actualLevelString = '0' + actualLevelString
         if(largoVariable eq 4) then actualLevelString = '0' + actualLevelString           
         ; recreate the string
         actualString = baseNameFile + actualLevelString + endNameFile
         
         if(levelActual eq 0) then begin
            ;...And verify if file with string name exists.
            if(FILE_TEST(actualString)) then return, actualString  $
            else return, -1        
         endif          
      endif else return, "-1"

      ; Modify channel value
      add = 3
      largoVariable = 2
      result = strPos(actualString, '_CH', /reverse_search)
      if (result eq -1) then result = strPos(actualString, '_ch', /reverse_search)
      if (result eq -1) then result = strPos(actualString, '_Ch', /reverse_search)
      if (result eq -1) then begin
         result = strPos(actualString, '_C', /reverse_search)
         if (result ne -1) then begin
            add = 2
        endif
        if (result eq -1) then begin
          result = strPos(actualString, 'C', /reverse_search)
          add = 1
          largoVariable = 2
        endif
      endif
      if (result eq -1) then begin
         largoVariable = 5
         ; if(levelActual eq 1) only increase channel value
         result = strPos(actualString, '488nm', /reverse_search)
         if (result ne -1) then actualLevelString = '568nm' $
         else begin
            result = strPos(actualString, '568nm', /reverse_search)
            if (result ne -1) then actualLevelString = '647nm' $
            else begin
               result = strPos(actualString, '647nm', /reverse_search)
               if (result ne -1) then actualLevelString = '777nm' else return, -1
            endelse
         endelse
         if(levelActual eq 2) then actualLevelString = '488nm' ; set channel value to "0"
         ; The channel have string structure
         baseNameFile    = strMid(actualString,0,result)
         endNameFile     = strMid(actualString,result+largoVariable)
         ; recreate the string
         actualString = baseNameFile + actualLevelString + endNameFile
         if(levelActual eq 1) then begin
            ;...And verify if file with string name exists.
            if(FILE_TEST(actualString)) then return, actualString  $
            else return, -1          
         endif             
      endif else begin
         ; The channel have decimal numeric structure
         baseNameFile    = strMid(actualString,0,result+add)
         channelActual   = s_getRightNumberFromString(strMid(actualString, result+add,largoVariable))
         endNameFile     = strMid(actualString,result+add+largoVariable)
         if(levelActual eq 1) then begin ; only increase channel value
            ; increase value of level data and obtain string
            channelActual++
         endif else begin
            ;Level to change is greater and need set channel to "0"
            channelActual = offsetZeroChannel
         endelse          
         actualLevelString = strCompress(STRING(channelActual), /rem)
         if(strlen(actualLevelString) eq 1) then actualLevelString = '0' + actualLevelString
         ; recreate the string
         actualString = baseNameFile + actualLevelString + endNameFile
         if(levelActual eq 1) then begin
            ;...And verify if file with string name exists.
            if(FILE_TEST(actualString)) then return, actualString  $
            else return, -1          
         endif  
      endelse
      
      ; Modify time value
      add = 2
      largoVariable = 3
      result = strPos(actualString, '_T', /reverse_search)
      if (result eq -1) then result = strPos(actualString, '_t', /reverse_search) $
      else begin
         largoVariable = 4
      endelse
      
      if (result eq -1) then begin
        result = strPos(actualString, 'T', /reverse_search)
        largoVariable = 5
        add = 1
      endif
      if (result ne -1) then begin
         ; Obtain string and data 
         baseNameFile = strMid(actualString,0,result+add)
         timeActual = s_getRightNumberFromString(strMid(actualString, result+add,largoVariable))
         endNameFile  = strMid(actualString,result+add+largoVariable)
         ; levelActual is 2...only increase time value
            ; increase value of level data and obtain string
            timeActual++
            
         actualLevelString = strCompress(STRING(timeActual), /rem)
         if(largoVariable eq 5) then begin
           if(strlen(actualLevelString) eq 1) then actualLevelString = '0000' + actualLevelString
           if(strlen(actualLevelString) eq 2) then actualLevelString = '000' + actualLevelString 
           if(strlen(actualLevelString) eq 3) then actualLevelString = '00' + actualLevelString
           if(strlen(actualLevelString) eq 4) then actualLevelString = '0' + actualLevelString                     
         endif else begin
           if(strlen(actualLevelString) eq 1) then actualLevelString = '00' + actualLevelString
           if(strlen(actualLevelString) eq 2) then actualLevelString = '0' + actualLevelString 
           if(largoVariable eq 4) then actualLevelString = '0' + actualLevelString
         endelse 
         ; recreate the string
         actualString = baseNameFile + actualLevelString + endNameFile
         ;...And verify if file with string name exists.
         if(FILE_TEST(actualString)) then return, actualString          
      endif else return,"-1"
   endif
   return, "-1"
end

