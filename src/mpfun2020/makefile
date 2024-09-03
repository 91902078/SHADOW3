SRCS = mpmodule.f90 mpfuna.f90 mpfunf.f90 mpfungq1.f90 mpfunhq1.f90  second.f90 mpmath.f90 mpinterface.c
OBJECTS = mpmodule.obj mpfuna.obj mpfunf.obj mpfungq1.obj mpfunhq1.obj mpinterface.obj second.obj mpmath.obj
LIBS = mpfr.lib mpir.lib

All: mpfun2020.lib

mpfunhq1.obj: mpfungq1.obj mpfunhq1.f90
	ifort /c /O3  /fpp /threads /names:lowercase /I. /I.. /Fo$@ mpfunhq1.f90
    
mpfungq1.obj: mpfuna.obj mpfunf.obj mpfungq1.f90
#	ifort /c /O3  /threads /assume:nounderscore /names:lowercase /I. /I.. /Fo$@ mpfungq1.f90
	ifort /c /O3  /fpp /threads /names:lowercase /I. /I.. /Fo$@ mpfungq1.f90

mpmodule.obj: mpfuna.obj mpfunf.obj mpfungq1.obj mpfunhq1.obj mpmath.obj
#	ifort /c /O3  /threads /assume:nounderscore /I. /I.. /Fo$@ mpmodule.f90
	ifort /c /O3  /threads /names:lowercase /I. /I.. /Fo$@ mpmodule.f90

mpfun2020.lib: $(OBJECTS)
	lib /SUBSYSTEM:CONSOLE /OUT:$@  $^  $(LIBS) /LIBPATH:"C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\lib\amd64"

#ifort /O3 $(LIBS) $^  /exe:$@ /link /LIBPATH:"C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\\lib"\amd64\

%.obj: %.c
	cl /c /O2 /std:c++latest /EHsc /I. /I.. /I../../mpfr-4.1.0/src /Fo:$@ $<

%.obj: %.f90
#	ifort /c /O3  /threads /assume:nounderscore /names:lowercase /I. /I.. /Fo$@ $<
	ifort /c /O3  /fpp /threads /names:lowercase /I. /I.. /Fo$@ $<

clean:
	del /F *.obj

