; FASL 2012

function s_getPalette, PaletteNumber
   filename = filepath('colors1.tbl', subdir = ['resource', 'colors'])
   openr,lun,filename, /block, /get_lun
      ntables = 0b
      readu, lun, ntables
      if (paletteNumber lt 0) then PaletteNumber = 0
      if (paletteNumber ge ntables) then PaletteNumber = ntables-1
      arr = bytArr(256)
      ctab = bytArr(3,256)
      point_lun, lun, PaletteNumber*768L + 1L
      readu, lun, arr
      ctab[0,*] = arr
      readu, lun, arr
      ctab[1,*] = arr
      readu, lun, arr
      ctab[2,*] = arr
   close,lun
   free_lun,lun
   return,ctab

end

