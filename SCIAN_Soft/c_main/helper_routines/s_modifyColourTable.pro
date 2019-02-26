function sPath
    path=!path
    case !VERSION .OS_FAMILY of
       'UNIX': return,strmid(path,0,strpos(path,'/lib'))
       'unix': return,strmid(path,0,strpos(path,'/lib'))
       'Windows': return,strmid(path,0,strpos(path,'\lib'))
       '': return,strmid(path,0,strpos(path,'\lib'))
       else: return,strmid(path,0,strpos(path,'\lib'))
    endcase
end


pro s_modifyColourTable

    slash = path_sep()
    if File_test(!DIR + '\resource\colors\colors1.tbl') then begin
      ctblPath = !DIR + '\resource\colors\colors1.tbl'
    endif else begin
      if File_Test('C:\Program Files\ITT\IDL70\resource\colors\colors1.tbl') $
      then ctblPath = 'C:\Program Files\ITT\IDL70\resource\colors\colors1.tbl' $
      else ctblPath = 'C:\Archivos de programa\ITT\IDL70\resource\colors\colors1.tbl'
    endelse

    openR, 1, ctblPath, error = err
    if (err ne 0) then ctblPath = sPath() + slash + 'resource' + slash + 'colors' + slash + 'colors1.tbl'
    close, 1

    ; Para la alegria de los usuarios actuales amantes del rojo :_(
    loadct, 3
    tvlct, rOrigen, gOrigen, bOrigen, /get

    r = make_array(256, /byte, /index)
    g = make_array(256, /byte, /index) * 0
    b = make_array(256, /byte, /index) * 0
    modifyCt, 41, 'Black-Red', r, g, b , file = ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    r = make_array(256, /byte, /index) * 0
    g = make_array(256, /byte, /index)
    b = make_array(256, /byte, /index) * 0
    modifyCt, 42, 'Black-Green', r, g, b , file = ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    r = make_array(256, /byte, /index) * 0
    g = make_array(256, /byte, /index) * 0
    b = make_array(256, /byte, /index)
    modifyCt, 43, 'Black-Blue', r, g, b , file = ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    r = make_array(256, /byte, /index)
    g = make_array(256, /byte, /index)
    b = make_array(256, /byte, /index) * 0
    modifyCt, 44, 'Black-Yellow', r, g, b , file = ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    r = make_array(256, /byte, /index)
    g = 255 - make_array(256, /byte, /index)
    r[0 : 127] = g[0 : 127]
    b = 255 - make_array(256, /byte, /index)
    modifyCt, 45, 'White-Grey-Red', r, g, b , file = ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    loadct, 3
    tvlct, r3, g3, b3, /get
    g3 = 255 - g3
    b3 = 255 - b3
    modifyCt, 46, 'White-Red', r, g3, b3 , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    r = [make_array(64, /byte, value = 0),make_array(64, /byte, value = 255),make_array(64, /byte, value = 0),make_array(64, /byte, value = 255)]
    g = [make_array(64, /byte, value = 0),make_array(64, /byte, value = 0),make_array(64, /byte, value = 255),make_array(64, /byte, value = 255)]
    b = [make_array(64, /byte, value = 0),make_array(64, /byte, value = 0),make_array(64, /byte, value = 0),make_array(64, /byte, value = 0)]
    modifyCt, 47, 'Black-Red-Green-Yellow', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    r = [make_array(64, /byte, value = 0),make_array(64, /byte, value = 0),make_array(64, /byte, value = 255),make_array(64, /byte, value = 255)]
    g = [make_array(64, /byte, value = 0),make_array(64, /byte, value = 255),make_array(64, /byte, value = 0),make_array(64, /byte, value = 255)]
    b = [make_array(64, /byte, value = 0),make_array(64, /byte, value = 0),make_array(64, /byte, value = 0),make_array(64, /byte, value = 0)]
    modifyCt, 48, 'Black-Green-Red-Yellow', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'
       
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index) * 0
    b = make_array(256, /byte, /index)
    r[*] = 255
    g[*] = 255
    b = 255 - b
    modifyCt, 49, 'White-Yellow', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    loadct, 3
    tvlct, r3, g3, b3, /get

    ; modified by FASL 2010
    inicio = 55
    tempR = make_array(256 - inicio, /byte, /index)
    tempG = make_array(256 - inicio, /byte, /index)
    tempB = make_array(256 - inicio, /byte, /index) * 0

    tempR = r3[inicio:*]
    tempG = g3[inicio:*]
    tempB = b3[inicio:*]

    for i = 0,255 do begin
      index = floor(i / (255.0/(255.0 - inicio)))
      r3[i] = tempR[index] 
      g3[i] = tempG[index]
      b3[i] = tempB[index]
    endfor
    modifyCt, 50, 'Red-Temperature without Black', r3, g3, b3 , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'

    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index) * 0
    b = make_array(256, /byte, /index)*0

    gris = make_array(128, /byte, /index)
    gris += 77
    
    for i = 0,127 do begin
      index = 2*i
      r[i] = r3[index] 
      g[i] = g3[index]
      b[i] = b3[index]
    endfor
    ; lo anterior podria ser r []
    r[128:*] = gris
    g[128:*] = gris
    b[128:*] = gris
    modifyCt, 51, 'RT w/o Black: Gray', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl' 
     
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index) * 0
    b = make_array(256, /byte, /index)*0

    baseRojoR = 185
    baseRojoG = 0
    baseRojoB = 0    

    baseAzulR = 162
    baseAzulG = 162
    baseAzulB = 255    

    for i = 0,127 do begin
      if(baseRojoR le 255)then begin
        r[i]     = baseRojoR
        baseRojoR++
      endif else begin
        r[i]     = 255        
        baseRojoG++
        baseRojoB++
        g[i]     = baseRojoG
        b[i]     = baseRojoB 
      endelse        

      if(baseAzulR ge 0)then begin
        r[i+127] = baseAzulR
        g[i+127] = baseAzulG
        b[i+127] = baseAzulB
        baseAzulR = baseAzulR - 2
        baseAzulG = baseAzulG - 2
      endif else begin
        baseAzulB = baseAzulB - 2
        r[i+127] = 0
        g[i+127] = 0
        b[i+127] = baseAzulB
      endelse        
    endfor
    
    ;first blue 162,162,255
    ; last blue 147
    modifyCt, 52, 'Red w/ Blue', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'  
     
    ; Added by Hector Moraga
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index) * 0
    b = make_array(256, /byte, /index)*0

    baseAmarilloR = 255  ; 255
    baseAmarilloG = 128  ; 255
    baseAmarilloB = 1    ; 255

    baseVerdeR = 255     ; 162
    baseVerdeG = 255     ; 228
    baseVerdeB = 255     ;  38

    for i = 0,127 do begin
       r[i] = baseAmarilloR
       g[i] = baseAmarilloG+i
       b[i] = baseAmarilloB+2*i
    endfor
    
    for i = 128,255 do begin
       r[i] = baseVerdeR-0.73*(i-128)
       g[i] = baseVerdeG-0.21*(i-128)
       b[i] = baseVerdeB-1.69*(i-128)
    endfor
    
    modifyCt, 53, 'Yellow w/White then Green', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'
    
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index) * 0
    b = make_array(256, /byte, /index)*0
    
    baseRojoR = 200
    baseRojoG = 0  
    baseRojoB = 0
        
    for i=0,55 do begin
       r[i] = baseRojoR+i 
       g[i] = baseRojoG+i
       b[i] = baseRojoB+i
    endfor
       
    for i=56,117 do begin
       r[i] = 255
       g[i] = floor(200*(i-55)/62)+55
       b[i] = floor(200*(i-55)/62)+55  
    endfor
    
    for i = 118,138 do begin
       r[i] = 255 
       g[i] = 255
       b[i] = 255
    endfor
    
    for i = 139,255 do begin
       r[i] = floor(255*(255-i)/117)
       g[i] = floor(255*(255-i)/117)
       b[i] = 255
    endfor
    
    ;first red 200,0,0
    ;last blue 0, 0, 255
    modifyCt, 54, 'Red w/White then Blue', r, g, b , file= ctblPath
    
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index)* 0
    b = make_array(256, /byte, /index)* 0
    
    baseRojoR = 200
    baseRojoG = 0  
    baseRojoB = 0
        
    for i=0,55 do begin
       r[i] = baseRojoR+i 
       g[i] = baseRojoG+i
       b[i] = baseRojoB+i
    endfor
    
    for i=56,117 do begin
       r[i] = 255
       g[i] = floor(200*(i-55)/62)+55
       b[i] = floor(200*(i-55)/62)+55  
    endfor
    
    for i = 118,138 do begin
       r[i] = 255 
       g[i] = 255
       b[i] = 255
    endfor
    
    for i = 139,255 do begin
       r[i] = 255 
       g[i] = 255
       b[i] = 255-floor(255*(i-139)/116)
    endfor
    
    ;first red   135,22,22 (Pure)
    ;last Yellow 255,255,1 (Pure)
    modifyCt, 55, 'Red w/White then Yellow', r, g, b , file= ctblPath
    
    r = make_array(256, /int, /index)
    g = make_array(256, /int, /index)
    b = make_array(256, /int, /index)
    r1 = make_array(256, /int, /index)
    g1 = make_array(256, /int, /index)
    b1 = make_array(256, /int, /index)

    for i = 0,255 do begin
      ;index = floor(i / (255.0/(255.0 - inicio)))
      r1[i] = ((r[i]-0.0)/255.0)*(200.0-30.0)+30 
      g1[i] = ((g[i]-0.0)/255.0)*(230.0-50.0)+50
      b1[i] = ((b[i]-0.0)/255.0)*(180.0-20.0)+20
    endfor
    
    for i = 0,255 do begin
      ;index = floor(i / (255.0/(255.0 - inicio)))
      r[i] = r1[255-i] 
      g[i] = g1[255-i]
      b[i] = b1[255-i]
    endfor
    modifyCt, 56, 'Green Reduced', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'  
    
   loadct, 8
   tvlct, r1, g1, b1, /get
    for i = 0,255 do begin
      ;index = floor(i / (255.0/(255.0 - inicio)))
      r[i] = r1[255-i] 
      g[i] = g1[255-i]
      b[i] = b1[255-i]
    endfor   
   modifyCt, 57, 'Green 8 inverse', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl'  
   
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index) * 0
    b = make_array(256, /byte, /index)*0
    
    for i = 0,127 do begin
      index = 2*i
      r[i] = r3[index] 
      g[i] = g3[index]
      b[i] = b3[index]
    endfor
   
    baseVerdeR = 255     ; 162
    baseVerdeG = 255     ; 228
    baseVerdeB = 255     ;  38
    
    for i = 128,255 do begin
       r[i] = baseVerdeR-1.26*(i-128)
       g[i] = baseVerdeG-0.82*(i-128)
       b[i] = baseVerdeB-1.84*(i-128)
    endfor
    modifyCt, 58, 'RT w/white then Green', r, g, b , file= ctblPath ;sPath()+'\resource\colors\colors1.tbl' 
    
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index)* 0
    b = make_array(256, /byte, /index)* 0
    
    baseRojoR = 200
    baseRojoG = 0  
    baseRojoB = 0
        
    for i=0,55 do begin
       r[i] = baseRojoR+i 
       g[i] = baseRojoG+i
       b[i] = baseRojoB+i
    endfor
    
    for i=56,117 do begin
       r[i] = 255
       g[i] = floor(200*(i-55)/62)+55
       b[i] = floor(200*(i-55)/62)+55  
    endfor
    
    for i = 118,138 do begin
       r[i] = 255 
       g[i] = 255
       b[i] = 255
    endfor
    j=0
    for i = 139,255 do begin
       r[i] = 255 
       g[i] = floor(255-j/2.5)
       b[i] = 255-floor(255*(i-139)/116)
       j++
    endfor
    
    ;first red   135,22,22 (Pure)
    ;last Yellow 255,255,1 (Pure)
    modifyCt, 59, 'Red w/White then Orange', r, g, b , file= ctblPath
         
    colorInicio = [0,0,0]
    colorMedio = [0,255,0]
    colorFin = [255,0,0]
    Tabla3Colores, nombre = 'Invento', archivo = ctblPath, numTabla = 60, colorInicio = colorInicio, colorMedio = colorMedio, colorFin = colorFin
    
    colorInicio = [255,255,255]
    colorMedio = [190,190,190]
    colorFin = [255,0,0]
    Tabla3Colores, nombre = '45 low grey', archivo = ctblPath, numTabla = 61, colorInicio = colorInicio, colorMedio = colorMedio, colorFin = colorFin
  
    colorInicio = [221,221,221]
    colorFin = [255,0,0]
    Tabla2Colores, nombre = 'Incresing-white', archivo = ctblPath, numTabla = 62, colorInicio = colorInicio, colorFin = colorFin

    colorInicio = [7,7,254]
    colorFin = [241,241,241]
    Tabla2Colores, nombre = 'Decresing-white', archivo = ctblPath, numTabla = 63, colorInicio = colorInicio, colorFin = colorFin
    
    tvlct, rOrigen, gOrigen, bOrigen
end


pro Tabla2Colores, nombre = nombre, archivo = archivo, numTabla = numTabla, colorInicio = colorInicio, colorFin = colorFin
    ; base vectores vacios
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index)* 0
    b = make_array(256, /byte, /index)* 0

    for i = 0,255 do begin
      ;Interpolacion para la primera mitad    
      r[i] =  colorInicio[0]*float(255-i)/255.0 + colorFin[0]*float(i)/255.0 
      g[i] =  colorInicio[1]*float(255-i)/255.0 + colorFin[1]*float(i)/255.0 
      b[i] =  colorInicio[2]*float(255-i)/255.0 + colorFin[2]*float(i)/255.0 

    endfor
   
    modifyCt, numTabla, nombre, r, g, b , file = archivo
end


pro Tabla3Colores, nombre = nombre, archivo = archivo, numTabla = numTabla, colorInicio = colorInicio, colorMedio = colorMedio, colorFin = colorFin
    ; base vectores vacios
    r = make_array(256, /byte, /index)* 0
    g = make_array(256, /byte, /index)* 0
    b = make_array(256, /byte, /index)* 0

    for i = 0,127 do begin
      ;Interpolacion para la primera mitad    

      r[i] =  colorInicio[0]*float(127-i)/127.0 + colormedio[0]*float(i)/127.0 
      g[i] =  colorInicio[1]*float(127-i)/127.0 + colormedio[1]*float(i)/127.0 
      b[i] =  colorInicio[2]*float(127-i)/127.0 + colormedio[2]*float(i)/127.0 

      r[i] =  colorInicio[0]*float(127.0-i)/127.0 + colormedio[0]*float(i)/127.0 
      g[i] =  colorInicio[1]*float(127.0-i)/127.0 + colormedio[1]*float(i)/127.0 
      b[i] =  colorInicio[2]*float(127.0-i)/127.0 + colormedio[2]*float(i)/127.0 

      
      ;Interpolacion para la ultima mitad      

      r[i+128] =  colormedio[0]*float(127-i)/127.0 + colorFin[0]*float(i)/127.0 
      g[i+128] =  colormedio[1]*float(127-i)/127.0 + colorFin[1]*float(i)/127.0 
      b[i+128] =  colormedio[2]*float(127-i)/127.0 + colorFin[2]*float(i)/127.0 

      r[i+128] =  colormedio[0]*float(127.0-i)/127.0 + colorFin[0]*float(i)/127.0 
      g[i+128] =  colormedio[1]*float(127.0-i)/127.0 + colorFin[1]*float(i)/127.0 
      b[i+128] =  colormedio[2]*float(127.0-i)/127.0 + colorFin[2]*float(i)/127.0 

    endfor
   
    modifyCt, numTabla, nombre, r, g, b , file = archivo
end
