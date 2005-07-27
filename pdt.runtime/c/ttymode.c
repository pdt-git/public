#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
foreign_t
pl_ttymode(term_t stream_term, term_t mode)
{ 
  IOSTREAM * stream=0;
  char *s;
  const char * no_tty="no_tty";
  const char * raw_tty="raw_tty";
  const char * cooked_tty="cooked_tty";
  const char * r = 0;
  int ttymode;
  
  int rval;

  if ( !PL_get_stream_handle(stream_term, &stream) ){
    return PL_warning("ttymode/2: problem obtaining stream handle");
  }
  ttymode = PL_ttymode(stream);  
  switch(ttymode){
     case PL_NOTTY: r=no_tty; break;
     case PL_RAWTTY:r=raw_tty; break;
     case PL_COOKEDTTY:r=cooked_tty; break;    
  }  
  s= malloc(strlen(r)+1);
  if(s==0){
    return PL_warning("ttymode/2: problem allocating memory");
  }
  strcpy(s,r);  
  rval = PL_unify_atom_chars(mode, s);
  free(s);
  return rval;
}

install_t
install()
{ PL_register_foreign("ttymode", 2, pl_ttymode, 0);
}
