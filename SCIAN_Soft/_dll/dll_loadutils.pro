function getExtLibextension
  case !VERSION.OS_FAMILY of
    'Windows' : return, '.dll'
    'unix'    : return, '.so'
    else      : return, '.unknown'
  endcase
end


function getDLLfromBaseDir, libName
  lib_dir = filePath('gvflib' + getExtLibextension(), root_dir = s_getPathForProject(), subdir=['_dll'])
  ;return, lib_dir
  return, 'not ready, yet... use getDLLfilename instead'
end


function getDLLfilename, libName, dirName = dirName, getFileNameAndPath = getFileNameAndPath

  ext = getExtLibextension()
  pathSep = path_sep()
  case libName of

    'AABBTreeCollision2D': begin
      dirName = 'C:\RSI\'
      dllName = 'AABBTree2D'
    endcase

    'SweepAndPrune2D': begin
      dirName = 'C:\RSI\workspaceVS\SweepAndPrune\SweepAndPrune\x64\Release\'
      ;dirName = s_getPathForProject() + pathSep + '_dll' + pathSep + 'x64' + pathSep
      dllName = 'SweepAndPrune2D'
    endcase
  
    'SweepAndPrune3D': begin
      ;dirName = 'D:\RSI\jjara\wsVisualStudio2008\collision_detection_IDL\x64\Release\'
      dirName = 'C:\RSI\workspaceVS\SweepAndPrune\SweepAndPrune\x64\Release\'
      dllName = 'SweepAndPrune3D'
    endcase

    'polygon_intersection': begin
      ;dirName = 'D:\RSI\jjara\wsVisualStudio2008\polygon_utils_IDL\x64\Release\'
      ;dirName = 'C:\Users\JetGo\workspaceVS\polygon_intersection\x64\Release\'
      dirName = 'C:\RSI\workspaceVS\polygon_utils_IDL\x64\Release\'
      ;dirName  = s_getPathForProject() + pathSep + '_dll' + pathSep + 'x64' + pathSep
      dllName = 'polygon_utils'
    endcase

    'polygon_intersection_alt': begin
      dirName = 'C:\RSI\workspaceVS\polygon_utils_IDL\x64\Release\'
      dllName = 'polygon_clipping_IDL'
    endcase

    'svm_lib': begin
      dirName = 'C:\RSI\workspaceVS\SVM_Light_IDL\SVM_Light_IDL\x64\Release\'
      dllName = 'LibSVM_IDL'
    endcase

    'svm_light': begin
      ;dirName = 'C:\RSI\workspaceVS\SVM_Light_IDL\SVM_Light_IDL\x64\Release\'
      dirName = 'C:\RSI\workspaceVS\SVM_Light_IDL\SVM_Light_IDL\x64\Release\'
      dllName = 'SVM_Light_IDL'
    endcase

    'ipol_CLGOF': begin
      dirName  = s_getPathForProject() + pathSep + '_dll' + pathSep + 'x64' + pathSep
      dllName = 'ipolCLG-OF_IDL'
    endcase

    'idl_lsd_full': begin
      dirName = s_getPathForProject() + pathSep + '_dll' + pathSep + 'x64' + pathSep
      dllName = 'idl_lsd'
    endcase

    'idl_lsd': begin
      dirName = s_getPathForProject() + pathSep + '_dll' + pathSep + 'x64' + pathSep
      dirName = 'C:\RSI\workspaceVS\IDL_ipolLSD\x64\Release\'
      dllName = 'IDL_ipolLSD'
    endcase

    else: begin
          print, 'Unrecognized DLL name: ', libName
          dirName = ''
          dllName = ''
          return, ''
          endcase
  endcase

  return, keyword_set(getFileNameAndPath) ? (dirName + dllName + ext) : (dllName + ext)
end
