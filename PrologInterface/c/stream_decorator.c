#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>


static IOFUNCTIONS Sdelegate_functions =
{ Xterm_read,
  Xterm_write,
  NULL,
  Xterm_close,
  Xterm_control
};




foreign_t pl_stream_decorator_create(
	term_t orig_term, 
	term_t module_term, 
	term_t args_term, 
	term_t decorator_stream_term)
	
{ 
	IOSTREAM * orig=0;
  if ( !PL_get_stream_handle(orig_term, &orig) ){
    return PL_warning("stream_decorator_create/4: problem obtaining backend stream handle.");
  }
  
	
	HANDLE_T * handle = (HANDLE_T *)malloc(sizeof(HANDLE_T));
	if(! handle){
		return PL_warning("stream_decorator_create/4: could not allocate decorator handle.");
	}
	handle->orig_stream=orig;
	handle->orig_stream_term=orig_term;
	
	IOSTREAM * decorator_stream = Snew(handle,orig->flags|SIO_ISATTY,&Sdelegate_functions);
	if(! decorator_stream){
		return PL_warning("stream_decorator_create/4: could not create decorator stream.");
	}
	handle->decorator_stream=decorator_stream;
	handle->decorator_stream_term=decorator_stream_term;
	module_t module=0;
	if(! PL_get_module(module_term, &module)){
		return PL_warning("stream_decorator_create/4: module lookup failed.");
	}
	handle->module=module;
	handle->args_term=args_term;
	return PL_unify_stream(decorator_stream_term,decorator_stream);
}





install_t
install()
{ PL_register_foreign("stream_decorator_create", 4, pl_stream_decorator_create, 0);
}



