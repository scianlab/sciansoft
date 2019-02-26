function s_getPathForProject
  
  case !VERSION.OS_FAMILY of
    'Windows' : return, s_getPathForProjectGeneric()
    'unix'    : return, s_getPathForProjectGeneric()
  endcase
end