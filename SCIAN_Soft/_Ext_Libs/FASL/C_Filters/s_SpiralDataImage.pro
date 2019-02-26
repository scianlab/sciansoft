function s_SpiralDataImage, image = image,numPixeles =numPixeles 

  dimI = size(image, /dim)
  typeI = size(image, /type)

  spiralPixels = MAKE_ARRAY(numPixeles,/DOUBLE, VALUE= 0)

  posX = floor(dimI[0]/2);
  posY = floor(dimI[1]/2);

  ;possible directions
  ;1 : down
  ;2 : right
  ;3 : up
  ;4 : left
  direccion        = 4;
  persistencia     = 1;
  stepsByDirection = 1;
  counterChangePer = 0;

  spiralPixels[0] = image[posX,posY];
  indexSpiral = 1;
  while(indexSpiral lt NUMPIXELES) do begin
    counterChangePer++
    
    for idxFill = 0, persistencia-1 do begin
    
      ;Bajando 
      if(direccion eq 1) then posX--;
      ;a la derecha
      if(direccion eq 2) then posY++;
      ;subiendo
      if(direccion eq 3) then posX++;
      ;izquierdo
      if(direccion eq 4) then posY--;

      if((posX lt 0) or (posX gt (dimI[0]-1)) or (posY lt 0) or (posY gt (dimI[0]-1))) then begin
        return, spiralPixels;
      endif else spiralPixels[indexSpiral] = image[posX,posY];
      indexSpiral++; 
      if(indexSpiral ge NUMPIXELES) then return, spiralPixels; 
    endfor
    ; Actualizar a siguiente direccion
    direccion++;
    if(direccion > 4) then direccion = 1;
    
    if(counterChangePer eq 2) then begin
      counterChangePer = 0;
      persistencia++;
    endif
  endwhile
  
  return, spiralPixels;
  
end


pro testSpiralData
  image = MAKE_ARRAY(5,5,/INT, /INDEX)  + 1
  print, image
  print, s_SpiralDataImage( image = image,numPixeles = 23 )
  
  
end
