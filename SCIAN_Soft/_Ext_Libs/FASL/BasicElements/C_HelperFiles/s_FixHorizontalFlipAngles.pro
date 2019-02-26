;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_FixHorizontalFlipAngles.pro
;
; PURPOSE:
;       recalculate angles ... flipped horizontally
;
; AUTHOR:
;   Felipe Santibañez (2012)
;   e_mail: fsantibanez@med.uchile.cl
;
; CALLING SEQUENCE:
;      pro s_FixHorizontalFlipAngles
;
; REQUIRED INPUTS:
;      NONE
;_____________________________IOISIOI____________________

pro s_FixHorizontalFlipAngles, vFolder = vFolder
; Lee archivos con un dato por fila... cada dato es considerado un angulo
; Se reemplaza cada valor con el resultado de flipear horizontalmente el valor de entrada
                            
    ;vPath = strCompress(vPath+'_Tracking' + path_sep()+'transformaciones' + path_sep()+ 'TransformacionImagen_', /rem)
    ;filename = dialog_pickfile( /read, path = 'c:\\', get_path = path, filter = '*.sav')
    
    if( N_ELEMENTS(vFolder) gt 0) then begin
      sFolder = vFolder
    endif else begin
      sFolder = DIALOG_PICKFILE(PATH='c:\', /DIRECTORY, $ 
                TITLE="Choose Directory containing Flipped Angles.")
    endelse
    
    if(strLen(sFolder) lt 1) then begin
      print, 'No se ha seleccionado directorio'
      return
    endif
    
    Result = FILE_SEARCH(sFolder,'*Angle*')
    
    for index = 0, N_ELEMENTS(Result)-1 do begin
        filenameIN = Result[index]
        flag = FILE_TEST(filenameIn)
        line = make_Array(1,/double)
        i=0
        
        bFlipped = strPos(filenameIN, 'Flipped', /reverse_search)
        if (bFlipped ne -1) then flag = 0b        
        
        if flag then begin
            filenameOut = strMid(filenameIN, 0,strLen(filenameIN)-4) + 'Flipped.sav'
            if(FILE_TEST(filenameOut)) then FILE_DELETE, filenameOut
              
            GET_LUN, inunit
            GET_LUN, outunit
            openu, inunit, filenameIn
            openw, outunit, filenameOut
            while ~ EOF(inunit) do begin
               READF, inunit, line 
               angulo = line[0]
               angulo = 180.0 - angulo
               if(angulo lt 0.0) then angulo = 360.0 + angulo
               printf, outunit, angulo
               i++ 
            endwhile    
            FREE_LUN, inunit
            FREE_LUN, outunit
            
            FILE_DELETE, filenameIN 
       endif
    endfor
    
    print, 'End Flip Angle proccess'
end