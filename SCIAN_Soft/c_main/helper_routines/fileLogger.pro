pro fileLogger, msg, fileName

  if (file_test(fileName) eq 1)$
  then openw, fileUnit, fileName, /APPEND, /GET_LUN $
  else openw, fileUnit, fileName, /GET_LUN

  printf,   fileUnit, msg
  close,    fileUnit
  free_lun, fileUnit
end

pro fileLoggerTest
  fileName = 'c:\rsi\deleteMe.txt'
  fileLogger, 'udqsae', fileName
end
