function s_obtainBasePath, tipo = tipo

   slash = path_sep()

   case strlowcase(tipo) of
      'channel': begin
         preffix = 'ch'
      end
      'cluster': begin
         preffix = 'clus'
      end
      'time': begin
         preffix = 't'
      end
   endcase

   regExpCh = '\' + slash + preffix + '[0-9]+'
   matchChPos = stRegEx(samplePath, regExpCh)
   strCh = (matchChPos ne -1) ? stRegEx(samplePath, regExpCh, /extract) : ''
   countChDigits = strLen(strCh) - strLen(regExpChPrefix) + 1
   if countChDigits gt 0 then basePath = strMid(basePath, 0, strPos(strMid(basePath, 0, strLen(basePath)-1), slash, /reverse_search))

   return, basePath
end
