  
  pro make_hist,vect,name,tempFileName
  
     index=where(finite(vect),count)
     vect=vect(index)
     ;print,transpose(vect)
   
     ;file=strCompress(s_getPathForSystem() + name + '.dat', /remove)
     file=strCompress(tempFileName + name + '.dat', /remove) ;SUSANA - Guardo en misma carpeta de datos
     
     GET_LUN, Unit 
     openW,Unit, file
          printF,Unit, name
          printF, Unit, transpose(vect)
          close, Unit
    
     bin_hist=round(sqrt(n_elements(vect)))
     hist=histogram(vect,nbins=bin_hist,max=max(vect),min=min(vect))
     xaxis=make_array(n_elements(hist),/index,/float)
     xaxis=xaxis*(max(vect)-min(vect))/max(xaxis)+min(vect)
   
     if n_elements(hist)*10 le 255 then n=n_elements(hist)*10 else n=255
     histo_new=fltarr(1,n)
     xaxis_new=fltarr(n)
     if size(n*(1./bin_hist),/Type) eq 2 then div=n*(1./bin_hist) else div=ceil(n*(1./bin_hist)) 
             
     for i=0,bin_hist-1 do begin
     histo_new[i*div:div*(i+1)-1]=hist[i]
     xaxis_new[i*div:div*(i+1)-1]=xaxis[i]
     endfor
           
           
     ;Color Table
     mesa = READ_IMAGE(s_getPathForSystem() + 'SCIAN_Code\SCIAN_Soft\imageDat\Rainbow_Table.tif')
   
     ind_mesa=ceil(sindgen(n)*(size(mesa,/dimensions))[1]*1./n)
     
     !Y.RANGE = [0, MAX(hist)] 
     ncols = N_ELEMENTS(histo_new[0,*])
     nrows = N_ELEMENTS(histo_new[0,*]) 
         
     ;Plot
     iplot,xaxis_new[0:4],histo_new[0:4],/histogram,thick=1,fill_color=[255,255,255],yrange=[min(hist),max(hist)],/no_saveprompt,ident=id,/fill_background,FILL_LEVEL=0,$;user_interface='none' 
     ;xtitle=name,ytitle="Frecuencia"
     xtitle=name,ytitle="Frequency" ; SUSANA - ingles
     FOR I = 0,nrows-2 DO BEGIN
        iplot,xaxis_new[i:i+1],histo_new[i:i+1],/histogram,thick=1,fill_color=mesa[*,ind_mesa[I]],yrange=[min(hist),max(hist)],/no_saveprompt,overplot=id,/fill_background,FILL_LEVEL=0,$;, user_interface='none'
        ;xtitle=name,ytitle="Frecuencia"
        xtitle=name,ytitle="Frequency" ; SUSANA - ingles
     ENDFOR

end