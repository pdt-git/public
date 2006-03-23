
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stddef.h>

#include "custom_io.h"


static int delegate_read(void *_handle, char *buf, int bufsize)
{
	fid_t frame = PL_open_foreign_frame();	
	HANDLE_T * handle = (HANDLE_T*)_handle;
	term_t stream_term= PL_new_term_refs(5);
	term_t args_term = stream_term+1;
	term_t size_term = args_term+1;
	term_t ttymode_term = size_term+1;
	term_t chars_term = ttymode_term+1;
	int ttymode=0;  
	unsigned int read_len=0;
	int rval=0;
	qid_t qid=0;
	char * read_data=0;
    
	
	PL_recorded(handle->stream_record,stream_term);
	PL_recorded(handle->args_record,args_term);
	PL_put_integer(size_term,bufsize);
	
	ttymode = PL_ttymode(handle->stream);  
	switch(ttymode)
	{
	   case PL_NOTTY:PL_put_atom_chars(ttymode_term,"no_tty"); break;
	   case PL_RAWTTY:PL_put_atom_chars(ttymode_term,"raw_tty"); break;
	   case PL_COOKEDTTY:PL_put_atom_chars(ttymode_term,"cooked_tty"); break;    
	}  
	PL_put_variable(chars_term);

	
	
	qid = PL_open_query(handle->module,PL_Q_NORMAL|PL_Q_CATCH_EXCEPTION|PL_Q_NODEBUG,handle->read_hook,stream_term);
	rval = PL_next_solution(qid);
	
	if(rval){ 

		if(PL_get_list_nchars(chars_term,&read_len,&read_data,0))
		{
			memcpy(buf,read_data,read_len*sizeof(char));			
		}
		else
		{
			Sputs("hmm... PL_get_atom_nchars did not work.\n");
		}
		
		
	}
	else{
		Sputs("read hook failed!\n");
	}

	PL_close_query(qid); 
	PL_discard_foreign_frame(frame);

	return read_len;
}



static int delegate_write(void *_handle, char*buf, int bufsize)
{
	int write_len=0;
	fid_t frame = PL_open_foreign_frame();	
	HANDLE_T * handle = (HANDLE_T*)_handle;
	term_t stream_term = PL_new_term_refs(4);
	term_t args_term = stream_term+1;
	term_t chars_term = stream_term+2;
	term_t tail_term = stream_term+3;
	int rval=0;
	unsigned int tail_len=0;
	char * tail;
	
	
	PL_recorded(handle->stream_record,stream_term);
	PL_recorded(handle->args_record,args_term);	
	PL_put_list_nchars(chars_term,bufsize,buf);
	PL_put_variable(tail_term);
	rval=PL_call_predicate(NULL,PL_Q_NORMAL|PL_Q_CATCH_EXCEPTION|PL_Q_NODEBUG,handle->write_hook,stream_term);
	if(rval)
	{
		
		if(PL_get_list_nchars(tail_term,&tail_len,&tail,0))
		{
			write_len=bufsize-tail_len;
		}
	}
	else{
		Sputs("write hook failed!\n");
	}
	
	PL_close_foreign_frame(frame);
	return write_len;
}


static long delegate_seek(void *_handle, long offset, int whence)
{
	int seek_len=0;
	//HANDLE_T * handle=0;
	
	//TODO
	
	return seek_len;
	
}

static int delegate_close(void *_handle)
{
	fid_t frame = PL_open_foreign_frame();	
	HANDLE_T * handle = (HANDLE_T*)_handle;
	term_t stream_term = PL_new_term_refs(2);
	term_t args_term = stream_term+1;
	int rval=0;
	
	
	PL_recorded(handle->stream_record,stream_term);
	PL_recorded(handle->args_record,args_term);	
	rval=PL_call_predicate(NULL,PL_Q_NORMAL|PL_Q_CATCH_EXCEPTION|PL_Q_NODEBUG,handle->close_hook,stream_term);
	if(!rval){
		Sputs("close hook failed\n");
	}	
	PL_close_foreign_frame(frame);	
	PL_erase(handle->args_record);
	PL_erase(handle->stream_record);
	PL_free(handle);
	

	return rval;
	
}


static IOFUNCTIONS Sdelegate_functions =
{ delegate_read,
  delegate_write,
  delegate_seek,
  delegate_close/*,
  delegate_control*/
};




foreign_t pl_cio_create(term_t module_term, term_t args_term, term_t mode_term, term_t stream_term){

/* The value t is never actualy used. 
 * But please keep this assignment anyway!
 * If it is removed, the mycrashoft compiler
 * chokes and mumbles nonsense.  Any suggestions why this happens?
 */
	int	t=PL_set_feature("tty_control", PL_BOOL, TRUE); //FIXME: is there no way to do this?
	IOSTREAM * stream=0;
	HANDLE_T * handle=0;
	char * module_name =0;
	char * mode_chars = 0;
	int flags = (
		SIO_TEXT|
		SIO_NOCLOSE|
		SIO_ISATTY|
		SIO_NOFEOF|
		SIO_RECORDPOS
	);
	
	handle = (HANDLE_T *)PL_malloc(sizeof(HANDLE_T));

	if(! handle)
	{
		return PL_warning("problems allocating handle");
	}
	handle->magic=CIO_MAGIC;
	
		
	/*
	 * lookup delegate predicates and initialize handle
	 */
	 if(! PL_get_atom_chars(module_term,&module_name))
	 {
	 	return PL_warning("second term does not look like an atom??");
	 }
	 if(! PL_get_module(module_term,&(handle->module)))
	 {
	 	return PL_warning("problems finding module.");
	 }

	handle->read_hook=PL_predicate("cio_read",5,module_name);
	handle->write_hook=PL_predicate("cio_write",4,module_name);
	handle->seek_hook=PL_predicate("cio_seek",4,module_name);
	handle->close_hook=PL_predicate("cio_close",2,module_name);		
	handle->control_hook=PL_predicate("cio_control",4,module_name);	

	
	/*
	 * determin read/write mode
	 */

	if(!PL_get_atom_chars(mode_term,&mode_chars)){
		return PL_warning("could not get chars from mode term");	
	}
	if(strcmp("read",mode_chars)==0){
		flags |= SIO_INPUT;
	}
	else if(strcmp("write",mode_chars)==0){
		flags |= SIO_OUTPUT;
	}
	else{
		return PL_warning("mode must be either 'read' or 'write'");
	}
	
	/*
	 * create the stream and connect it to the handle.
	 */
	stream  = Snew(handle,  flags, &Sdelegate_functions);	

	handle->stream=stream;
	
	/*
	 * unify prolog-side stream handle 
	 */
	if(!PL_unify_stream(stream_term,stream)){
		return PL_warning("could not unify stream handle");
	}
	 
	/*
	 * remember prolog-side stream handle and argument term 
	 */
	handle->stream_record=PL_record(stream_term);
	handle->args_record=PL_record(args_term);
	PL_succeed;
}


#define CIO_READ_NC_LIMIT 256 /*FIXME: what is a sensible value?*/

foreign_t pl_cio_read_n_chars(term_t stream_term, term_t limit_term, term_t chars_term){
	IOSTREAM * stream =0;	
	int limit = -1;
	char buf[CIO_READ_NC_LIMIT];
	int r=0;
	if ( !PL_get_stream_handle(stream_term, &stream) )
	{
		return PL_warning("problem obtaining backend stream handle.");
	}  	
	if(! PL_get_integer(limit_term,&limit)){
		return PL_warning("problems getting limit integer");
	}
	if(limit>CIO_READ_NC_LIMIT){
		limit=CIO_READ_NC_LIMIT;
	}
	//Sprintf("pl_cio_read_n_chars: request for %d chars.\n",limit);
	r=Sread_pending(stream,buf,limit,SIO_RP_BLOCK);
	//Sprintf("pl_cio_read_n_chars: got %d chars.\n",r);
	if(r<0){
		PL_fail;
	}
	
	return PL_unify_list_nchars(chars_term,r,buf);
}


install_t install()
{ 	
	PL_register_foreign("cio_create", 4, pl_cio_create, 0);
	PL_register_foreign("cio_read_n_chars", 3, pl_cio_read_n_chars, 0);
}



