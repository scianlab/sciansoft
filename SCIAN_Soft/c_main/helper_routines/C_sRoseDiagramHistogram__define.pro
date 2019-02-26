;_____________________________IOISIOI____________________
; NAME:
;      C_sRoseDiagramHistogram
;
; PURPOSE:
;       - Rose Diagram Histogram
;
; AUTHOR:
;     Hector Moraga A. (2011)
; 
;
; CALLING SEQUENCE:
;        result = Obj_New('C_sRoseDiagramHistogram' )
;
; METHODS:
;   based on C_sCircle class
;  PRO               ->draw, pData = pData                    ;pData  Pointer on Data 2 columns Array
;  PRO               ->SetProperty, data = data, diagType = DiagType, numClass = numClass,  color = color,$
;                                    titulo = titulo
;  PRO               ->GetProperty, data = data, diagType = DiagType, numClass = numClass,  color = color,$
;                                    titulo = titulo
;_____________________________IOISIOI____________________

;    Existen 2 tipos de diagramas: de 180° y de 360°, los rangos para los diagramas de 180° pueden ser [0,-180[ o [-90, 90[,
; el rango para el diagrama de 360° es [0, 360[
;    Se puede definir el numero de clases a graficar, donde el angulo que tendra
; cada clase es 180°/numClass o 360°/numClass.
; los datos son 2 columnas de n filas, donde la primera columna es el identificador del
; dato y  la segunda columna es la frecuencia absoluta.

;Para probar usar el siguiente programa
;seed=6L
;a=make_array(2, 181, /DOUBLE, VALUE=0)
;a[0, *]=FINDGEN(181)
;a[1, *]=transpose(floor(100*RANDOMU(seed, 181)))
;b=ptr_new(a, /no_copy)
;result=Obj_New('C_sRoseDiagramHistogram')
;result->SetProperty, diagType=0, numClass=18, subDivs=10, triangleWidth=0.9, width=800, height=600, color=[200,0,0], axisColor=[255,255,255], bgColor=[0,0,0], titulo='Ejemplo Aleatorio'
;result->draw, pData = b
;
pro C_sRoseDiagramHistogram::SetProperty, diagType = diagType, numClass = numClass, subDivs = subDivs, triangleWidth = triangleWidth, $
                                        width = width, height = height, foreColor = foreColor, axisColor = axisColor, bgColor = bgColor, $
                                        apPlot = apPlot, titulo = titulo

   if (n_elements(diagType) ne 0) then self.diagType = diagType
   if (n_elements(numClass) ne 0) then self.numClass = numClass
   if (n_elements(subDivs) ne 0) then self.subDivs = subDivs
   if (n_elements(triangleWidth) ne 0) then self.triangleWidth = triangleWidth
   if (n_elements(width) ne 0) then self.width = width
   if (n_elements(height) ne 0) then self.height = height
   if (n_elements(foreColor) ne 0) then self.foreColor = foreColor
   if (n_elements(axisColor) ne 0) then self.axisColor = axisColor
   if (n_elements(bgColor) ne 0) then self.bgColor = bgColor
   if (n_elements(apPlot) ne 0) then self.apPlot = apPlot
   if (n_elements(titulo) ne 0) then self.titulo = titulo
end

pro C_sRoseDiagramHistogram::GetProperty, diagType = diagType, numClass = numClass, subDivs = subDivs, triangleWidth = triangleWidth, $
                                        width = width, height = height, foreColor = foreColor, axisColor = axisColor, bgColor = bgColor, $
                                        apPlot = apPlot, titulo = titulo

   diagType=self.diagType
   numClass=self.numClass
   subDivs=self.subDivs
   triangleWidth=self.triangleWidth
   width=self.width
   height=self.height
   foreColor=self.foreColor
   axisColor=self.axisColor
   bgColor=self.bgColor
   apPlot=self.apPlot
   titulo=self.titulo
end

function C_sRoseDiagramHistogram::init, diagType = diagType, numClass = numClass, subDivs = subDivs, triangleWidth = triangleWidth, $
                                        width = width, height = height, foreColor = foreColor, axisColor = axisColor, bgColor = bgColor, $
                                        apPlot = apPlot, titulo = titulo

    if keyword_set(diagType) then self.diagType = diagType
    if keyword_set(numClass) then self.numClass = numClass
    if keyword_set(subDivs) then self.subDivs = subDivs
    if keyword_set(triangleWidth) then self.triangleWidth = triangleWidth
    if keyword_set(width) then self.width = width
    if keyword_set(height) then self.height = height    
    if keyword_set(foreColor) then self.foreColor = foreColor
    if keyword_set(axisColor) then self.axisColor = axisColor       
    if keyword_set(bgColor) then self.bgColor = bgColor
    if keyword_set(apPlot) then self.apPlot = apPlot 
    if keyword_set(titulo) then self.titulo = titulo
   
   return, 1
end

function C_sRoseDiagramHistogram::createXAxis, roseDiag = roseDiag
   ncircles = (1+(indgen(self.subDivs)))*ceil(max(roseDiag[1,*])/self.subDivs)
   range = [-max(ncircles), max(ncircles)]  
   rango = [reverse(s_dynamicRangeArray(min = 0, step = max(ncircles)/self.subDivs, max = max(ncircles), dType=2)), $
             (s_dynamicRangeArray(min = 0, step = max(ncircles)/self.subDivs, max = max(ncircles), dType=2))[1:*]]
   oXaxis=OBJ_NEW('IDLgrAxis', 0, COLOR=self.axisColor, RANGE=range, $
                  LOCATION=[1000, 0, 0])
   oXaxis->SetProperty, MAJOR = 2*self.subDivs+1
   xtl = 0.02 * (range[1] - range[0])
   oXaxis->SetProperty, TICKLEN = xtl
   oXaxis->SetProperty, MINOR = 0     ; ningun TICK pequeno
   oXaxis->SetProperty, EXACT = 1
   oXaxis->SetProperty, TICKUNITS = 'NUMERIC'
   oXaxis->SetProperty, TICKFORMAT = 'ABSFMT'
   oXaxis->SetProperty, TICKVALUES = rango
   return, oXaxis
end

function ABSFMT, axis, index, value, level
   return, string(abs(value),FORMAT='(I)')
end
   
function C_sRoseDiagramHistogram::createYAxis, maximum = maximum
   if ((self.diagType ne 2) and (self.apPlot eq 1)) then begin     
      oYaxis=OBJ_NEW('IDLgrAxis', COLOR=self.axisColor, /NOTEXT)
      oYaxis->SetProperty, MAJOR = self.subDivs     ; cantidad de TICK mayores
      oYaxis->SetProperty, MINOR = 0     ; ningun TICK pequeno
      oYaxis->SetProperty, DIRECTION = 1 ;eje Y
      oYaxis->SetProperty, THICK = 2
      oYaxis->SetProperty, RANGE=[0, self.subDivs*round(maximum/self.subDivs)] ;rango
      oYaxis->SetProperty, EXACT=1
      return, oYaxis
   endif
end

pro C_sRoseDiagramHistogram::createBackCircles, oModel = oModel, roseDiag = roseDiag
   
   ncircles = (1+(indgen(self.subDivs)))*ceil(max(roseDiag[1,*])/self.subDivs)

   case self.diagType of
     0: angles = indgen(181)
     1: angles = indgen(181)
     2: angles = indgen(361)
     else: angles = indgen(181)
   endcase

   for i=0, n_elements(ncircles)-1 do begin
      backgndCircle = OBJ_NEW('IDLgrPlot', make_array(size(angles, /DIMENSIONS), VALUE=ncircles[i], /FLOAT),$
                                           angles*!dtor, COLOR=self.axisColor, /POLAR)
      backgndCircle->SetProperty, ZVALUE=10                                        
      oModel->Add, backgndCircle
      if (self.diagType ne 2) then begin
         linea = OBJ_NEW('IDLgrPlot', ncircles[i]*[1,1], [min(angles),max(angles)]*!dtor, COLOR = self.axisColor, /POLAR)
         oModel->Add, linea
      endif
   endfor
   if (self.apPlot eq 1) then begin
      linea2 = OBJ_NEW('IDLgrPolyLine', [0,0], [0,max(ncircles)], COLOR = self.axisColor)
      oModel->Add, linea2
      textos = OBJ_NEW('IDLgrText', ['a','p'], COLOR = self.axisColor)
      textos->SetProperty, LOCATION=[[-5,max(ncircles)+20,1],[-5,-60,1]]
      oModel->Add, textos      
   endif 
end

pro C_sRoseDiagramHistogram::calcSortedData, pData = pData, maxData = maxData, classId = classId, $
                                             roseDiag = roseDiag
   ;input: pData =  pointer to Data
   ;output: maxData = maximum value of Data
   ;        sortedData = array of 2 dimensions, first the classId (in angles)
   ;                     represented by a histogram class 

   ;tamaño del arreglo de datos
   szdata=size(*pData, /dimensions)

   ;se ordenan los datos en base al identificador del dato
   sortedData=(*pData)[*,sort((*pData)[0,*])]
   ;classid
   case 1 of
     (self.diagType eq 2): classId=360./self.numClass
     ((self.diagType eq 0) or (self.diagType eq 1)): classId=180./self.numClass
     else: message, 'Wrong Option (diagType ={360, -90a90, 0a180})'
   endcase
   
   ;maximo identificador de los datos
   maxId=max((*pData)[0,*],min = minId)
   
   ;arreglo de datos transformado
   roseDiag=make_array(2, self.numClass, /double, value=0.0) 
   
   for i=1,self.numClass do begin
      classData=floor(classId*i)
      
      ;busco los datos cuyos valores sean:
      ; minId+(i-1)*(maxId-minId)/numClass < datos < minId+i*(maxId-minId)/numClass
      if (i ne self.numClass) then $
         idx=where((sortedData[0,*] lt (minId+i*(maxId-minId)/self.numClass)) and (sortedData[0,*] ge (minId+(i-1)*(maxId-minId)/self.numClass))) $
      else $
         idx=where((sortedData[0,*] le maxId) and (sortedData[0,*] ge (minId+(i-1)*(maxId-minId)/self.numClass)))
      roseDiag[0,i-1]=classData
      if (n_elements(idx) eq 0) then roseDiag[1,i-1]=0
      if (n_elements(idx) ge 1) then roseDiag[1,i-1]=total(sortedData[1,idx]) 
   endfor
   
   ;busco el maximo del valor de los datos, con eso dimensiono el grafico de rosa respecto a las dimensiones de la ventana
   maxData=max(roseDiag[1,*])
end

pro C_sRoseDiagramHistogram::createTriangles, classId = classId, roseDiag = roseDiag, oModel = oModel 

   ;obtengo los parametros de centro de acuerdo al oModel
   ;genero los triangulos
   for i=0,self.numClass-1 do begin
     ; classId me da el angulo que deben tener los triangulos
     ; los triangulos siempre tendran vertices en (0,0), (0,largo), (largo*cos(classId),largo*sin(classId))
     ; solo se rotaran de acuerdo al valor de "i" que tienen que va de 0 a numClass-1, es decir,
     rotation=i*classId
     ; matriz de rotacion
     b = [[cos(!dtor*rotation), -sin(!dtor*rotation), 1],[sin(!dtor*rotation), cos(!dtor*rotation), 1]]
     
     ; genera el triangulo, lo centramos en la mitad de la ventana
     point1=floor(transpose(b##[0, 0, -10]))
     point2=floor(transpose(b##[roseDiag[1,i], 0, -10]))
     point3=floor(transpose(b##[roseDiag[1,i]*cos(!dtor*classId*self.triangleWidth), $
                                roseDiag[1,i]*sin(!dtor*classId*self.triangleWidth), -10]))
     ;agrego el triangulo de acuerdo a la rotacion en la ventana
     oPolygon = OBJ_NEW('IDLgrPolygon', COLOR=self.foreColor, ALPHA_CHANNEL=0.8, DATA=[[point1],[point2],[point3]])
     oModel->Add, oPolygon
  endfor
end

pro C_sRoseDiagramHistogram::Indicator, roseDiag = roseDiag, oModel = oModel

   ncircles = (1+(indgen(self.subDivs)))*ceil(max(roseDiag[1,*])/self.subDivs)

   case self.diagType of
      0: texto=[string(180,FORMAT='(I)'), string(0,FORMAT='(I)')]
      1: texto=[string(90,FORMAT='(I)'), string(-90,FORMAT='(I)')]
   endcase
   indicator = OBJ_NEW('IDLgrText', texto, COLOR = self.axisColor, $
                                    LOCATION = [[-max(roseDiag[1,*])-80, -60,1],[max(roseDiag[1,*])-40, -60,1]])
   oModel->Add, indicator
end

pro C_sRoseDiagramHistogram::draw, pData = pData

   ; dimensiono el grafico, coloco color de fondo y color del grafico
   
   oView=OBJ_NEW('IDLgrView', COLOR=self.bgColor, VIEWPLANE_RECT=[-100,-100,200,200]) 
   oWindow=OBJ_NEW('IDLgrWindow', TITLE=self.titulo, DIMENSIONS=[self.width, self.height], RETAIN=2)
   
   ;creo el modelo, cargo los ejes y creo el plot de los circulos de referencia 
   oModel = OBJ_NEW('IDLgrModel')
   oView->Add, oModel
   self->calcSortedData, pData = pData, maxData = maxData, classId = classId, roseDiag = roseDiag
   self->createTriangles, classId = classId, roseDiag = roseDiag, oModel = oModel
   self->createBackCircles, oModel = oModel, roseDiag = roseDiag
   oXAxis = self->createXAxis(roseDiag = roseDiag)
   oModel->Add, oXAxis
   if ((self.diagType ne 2) and (self.apPlot eq 1)) then begin
      oYAxis = self->createYAxis(maximum = maxData)
      oModel->Add, oYAxis
   endif
   if (self.diagType ne 2) then self->Indicator, roseDiag = roseDiag, oModel = oModel
   oModel->Scale, 0.9, 0.9, 1
   if (self.diagType ne 2) then $ 
       oView->SetProperty, VIEWPLANE_RECT=[-max(roseDiag), -0.2*max(roseDiag), 2*max(roseDiag), 1.2*max(roseDiag)] $
   else oView->SetProperty, VIEWPLANE_RECT=[-max(roseDiag), -max(roseDiag), 2*max(roseDiag), 2*max(roseDiag)]
   oWindow->Draw, oView
 end

pro C_sRoseDiagramHistogram::cleanup
end

pro C_sRoseDiagramHistogram__define
   tmp = {C_sRoseDiagramHistogram, $
          diagType:0, $
          numClass:12, $
          subDivs:4, $
          triangleWidth:0.9, $
          width:800, $
          height:600, $
          foreColor:[200,0,0], $
          axisColor:[255,255,255], $
          bgColor:[0,0,0], $
          apPlot:1, $
          titulo:'Grafico de Ejemplo'}
end