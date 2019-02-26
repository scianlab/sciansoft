function s_getPathForSystem

    project=s_getPathForProjectGeneric()
    
    case !VERSION.OS_FAMILY of
       'Windows': return, 'c:\rsi\'
       'UNIX': return, project+'../../RSI/'
       'unix': return, project+'../../RSI/'
       else: return, 'c:\rsi\'
    endcase

end

