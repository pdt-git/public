!include rules.mk
DLLS=stream_decorator.dll
all:	$(DLLS)

stream_decorator.dll:	stream_decorator.obj
	$(LD) /dll /out:$@ $(LDFLAGS) stream_decorator.obj $(TERMLIB) $(PLLIB) $(LIBS)

clean::
	if exist *.obj del *.obj
	if exist *~ del *~

distclean: clean
	-del *.dll *.lib *.exp *.pdb 2>nul

