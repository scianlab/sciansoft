pro create_dll, LIBNAME=libname, FUNCNAME=funcname

  slash  = path_sep()
  indir  = s_getPathForProject() + slash + '_dll' + slash + 'src'
  outdir = s_getPathForProject() + slash + '_dll'

  case !VERSION.OS of

    'darwin': begin
      include = '-I"/Applications/itt/idl70/external/include"'
      MAKE_DLL, libname, funcname, /VERBOSE, /NOCLEANUP, /SHOW_ALL_OUTPUT,$
        EXTRA_CFLAGS='-Wall -fopenmp -rdynamic -O2 -c -fPIC',$
        EXTRA_LFLAGS='-fopenmp -rdynamic -bundle -flat_namespace -undefined suppress', $
        CC='gcc-4.2  %X '+ include +' %C -o %O',$
        LD='gcc-4.2  %X -o %L %O',$
        INPUT_DIRECTORY=indir, OUTPUT_DIRECTORY=outdir
    end

    'Win32': begin
      include = '/I"C:\Program Files\ITT\IDL71\external\include"'
      path = getenv('PATH')
      oldpath = path
      vsinstalldir = 'C:\Program Files (x86)\Microsoft Visual Studio 10.0'
      vcinstalldir = 'C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC'
      frameworkdir = 'C:\Windows\Microsoft.NET\Framework64'
      frameworkversion = 'v2.0.50727'
      framework35version = 'v3.5'
      windowssdkdir = 'C:\Program Files\Microsoft SDKs\Windows\v6.0A'

      path = vcinstalldir+'\BIN\amd64;'+$
      frameworkdir+'\'+framework35version+';'+$
      frameworkdir+'\'+framework35version+'\Microsoft .NET Framework 3.5 (Pre-Release Version);'+$
      frameworkdir+'\'+frameworkversion+';'+$
      vcinstalldir+'\VCPackages;'+$
      vsinstalldir+'\Common7\IDE;'+$
      vsinstalldir+'\Common7\Tools;'+$
      vsinstalldir+'\Common7\Tools\bin;'+$
      windowssdkdir+'\bin\x64;'+$
      windowssdkdir+'\bin\win64\x64;'+$
      windowssdkdir+'\bin;'+$
      path

      setenv, 'PATH='+path
      path = getenv('PATH')

      include = getenv('INCLUDE')
      include = vcinstalldir+'\ATLMFC\INCLUDE;'+$
      vcinstalldir+'\INCLUDE;'+$
      windowssdkdir+'\include;'+$
      include
      setenv, 'INCLUDE='+include

      lib = getenv('LIB')
      lib = vcinstalldir+'\ATLMFC\LIB\amd64;'+$
      vcinstalldir+'\LIB\amd64;'+$
      windowssdkdir+'\lib\x64;'+$
      lib
      setenv, 'LIB='+lib

      libpath = getenv('LIBPATH')
      libpath = frameworkdir+'\'+framework35version+';'+$
      frameworkdir+'\'+frameworkversion+';'+$
      frameworkdir+'\'+framework35version+';'+$
      frameworkdir+'\'+frameworkversion+';'+$
      vcinstalldir+'\ATLMFC\LIB\amd64;'+$
      vcinstalldir+'\LIB\amd64;'+$
      libpath
      setenv, 'LIBPATH='+libpath

      MAKE_DLL, libname, funcname,$
        /VERBOSE, /NOCLEANUP, /SHOW_ALL_OUTPUT,$
        EXTRA_CFLAGS=' /openmp /O2 /favor:INTEL64 ',$
        EXTRA_LFLAGS=' vcomp.lib ',$
        INPUT_DIRECTORY=INDIR, OUTPUT_DIRECTORY=OUTDIR

      setenv, 'PATH='+oldpath
    end

  endcase

end
