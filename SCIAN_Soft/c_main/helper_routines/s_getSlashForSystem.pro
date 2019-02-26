function s_getSlashForSystem

    case !VERSION.OS_FAMILY of
       'Windows': return, '\'
       'UNIX': return, '/'
       'unix': return, '/'
       else: return, '\'
    endcase

end