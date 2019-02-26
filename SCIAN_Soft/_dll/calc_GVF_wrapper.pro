pro calc_GVF_wrapper, it, nx, ny, dt, u, v, fx, fy, g, h, err_tol  

  ; First, get filename extension of the system to build the path
  case !VERSION.OS_FAMILY of
    'Windows': ext = 'dll'
    'unix'   : ext = 'so'
  endcase

  calc_GVF_lib = filepath('gvflib.' + ext, root_dir = s_getPathForProject(), subdir=['_dll']) 
  print, 'Calling External Library ' + calc_GVF_lib

  ; Cross the fingers and call the external library
  ret = call_external( $
    calc_GVF_lib, $
    'gvf', $
    it, $
    nx, ny, $
    dt,  $
    u, v,   $
    fx, fy, $
    g, h,   $
    err_tol,$
    /CDECL, /UNLOAD)

   if (ret eq 0) then message, 'Dynamic link library load failed: ' + fd_iterations_lib 

end
