
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stddef.h>

#include "stream_decorator.h"

static int display_term(term_t t)
{ 
	int arity, n;
	unsigned int len;
	char * s;
	atom_t name;
	term_t a;
	switch( PL_term_type(t) )
	{ 
		case PL_VARIABLE:
	    case PL_ATOM:

    	case PL_INTEGER:
	    case PL_FLOAT:
    	  	PL_get_chars(t, &s, CVT_ALL);
	      	Sprintf("%s", s);
	      	break;
	    case PL_STRING:
    	  	PL_get_string_chars(t, &s, &len);
	      	Sprintf("\"%s\"", s);	
	      	break;
	    case PL_TERM:
	      	a = PL_new_term_ref();
	      	PL_get_name_arity(t, &name, &arity);
	      	Sprintf("%s(", PL_atom_chars(name));
	      	for(n=1; n<=arity; n++)
					{
		      	PL_get_arg(n, t, a);
        		if ( n > 1 )
						{
          			Sprintf(", ");
        		}
        		display_term(a);
      		}
      		Sprintf(")");
      		break;
    	default:
      		PL_fail;
  	}
  	PL_succeed;
}


static int delegate_read(void *_handle, char *buf, int bufsize)
{
	fid_t frame = PL_open_foreign_frame();	
	HANDLE_T * handle = (HANDLE_T*)_handle;
	term_t t0 = PL_new_term_refs(5);
	term_t t1 = t0+1;
	term_t t2 = t1+1;
	term_t t3 = t2+1;
	term_t t4 = t3+1;
	int ttymode=0;  
	unsigned int read_len=0;
	int rval=0;
	qid_t qid=0;
	char * read_data=0;
	term_t exception=0;
	
	PL_recorded(handle->stream_record,t0);
	PL_recorded(handle->args_record,t1);
	PL_put_integer(t2,bufsize);
	
	ttymode = PL_ttymode(handle->stream);  
	switch(ttymode)
	{
	   case PL_NOTTY:PL_put_atom_chars(t3,"no_tty"); break;
	   case PL_RAWTTY:PL_put_atom_chars(t3,"raw_tty"); break;
	   case PL_COOKEDTTY:PL_put_atom_chars(t3,"cooked_tty"); break;    
	}  
	PL_put_variable(t4);
	display_term(t0);Sprintf("\n");
	display_term(t1);Sprintf("\n");
	display_term(t2);Sprintf("\n");
	display_term(t3);Sprintf("\n");
	
	
	
	qid = PL_open_query(handle->module,PL_Q_NORMAL|PL_Q_CATCH_EXCEPTION|PL_Q_NODEBUG,handle->read_hook,t0);
	rval = PL_next_solution(qid);
	
	if(rval)
	{
		Sputs("read hook defined!\n");
	

		if(PL_get_list_nchars(t4,&read_len,&read_data,0))
		{
			Sprintf("got %d chars: %s\n",read_len,read_data);
			memcpy(buf,read_data,read_len*sizeof(char));			
		}
		else
		{
			Sputs("hmm... PL_get_atom_nchars did not work.\n");
		}
		PL_close_query(qid); 
		PL_discard_foreign_frame(frame);
		
	}
	else
	{
		exception= PL_exception(qid) ;
		if(exception )
		{

			if(!display_term(exception))
			{
				Sputs("display failed\n");
			}
		}
		else
		{
			Sputs("read hook failed!\n");
			read_len=(*handle->orig_io_functions->read)(handle->orig_handle,buf,bufsize);
			Sprintf("relayed %d chars.\n",read_len);
		}
		PL_cut_query(qid); 
		PL_close_foreign_frame(frame);
	}

			Sputs("done.\n");
	return read_len;
}



static int delegate_write(void *_handle, char*buf, int bufsize)
{
	int write_len=0;
	fid_t frame = PL_open_foreign_frame();	
	HANDLE_T * handle = (HANDLE_T*)_handle;
	term_t t0 = PL_new_term_refs(4);
	term_t t1 = t0+1;
	term_t t2 = t0+2;
	term_t t3 = t0+3;
	int rval=0;
	unsigned int tail_len=0;
	char * tail;
	
	
	PL_recorded(handle->stream_record,t0);
	PL_recorded(handle->args_record,t1);	
	PL_put_list_nchars(t2,bufsize,buf);
	PL_put_variable(t3);
	rval=PL_call_predicate(NULL,PL_Q_NORMAL|PL_Q_CATCH_EXCEPTION|PL_Q_NODEBUG,handle->write_hook,t0);
	if(rval)
	{
		if(PL_get_list_nchars(t3,&tail_len,&tail,0))
		{
			write_len=bufsize-tail_len;
		}
	}
	else
	{
		handle = (HANDLE_T*)_handle;
		write_len=(*handle->orig_io_functions->write)(handle->orig_handle,buf,bufsize);
	}
	PL_close_foreign_frame(frame);
	return write_len;
}


static long delegate_seek(void *_handle, long offset, int whence)
{
	int seek_len=0;
	HANDLE_T * handle=0;
	if(FALSE)
	{
	}
	else
	{
		handle = (HANDLE_T*)_handle;
		seek_len=(*handle->orig_io_functions->seek)(handle->orig_handle,offset,whence);
	}
	return seek_len;
	
}

static int delegate_close(void *_handle)
{
	HANDLE_T * handle = 0;
	int ret_val=0;
	if(FALSE)
	{
	}
	else
	{
		handle = (HANDLE_T*)_handle;
		ret_val=(*handle->orig_io_functions->close)(handle->orig_handle);
	}
	return ret_val;
	
}

static int delegate_control(void *_handle, int action, void *arg)
{
	int ret_val=0;
	HANDLE_T * handle = (HANDLE_T*)_handle;
	if(FALSE)
	{
				Sputs("kaum zu glauben\n");
	}
	else if (action==SIO_GETFILENO&&
			(handle->orig_flags&SIO_FILE
			||handle->orig_flags&SIO_PIPE)
			)
	{
		*(int*)arg=	handle->orig_fileno;
		
		ret_val=0;
	}
	else
	{
		
		
		ret_val=(*handle->orig_io_functions->control)(handle->orig_handle,action,arg);
		
	}
	return ret_val;
	
}



static IOFUNCTIONS Sdelegate_functions =
{ delegate_read,
  delegate_write,
  delegate_seek,
  delegate_close,
  delegate_control
};

foreign_t pl_orig_read(term_t stream_term, term_t bufsize_term, term_t chars_term)
{
	HANDLE_T * handle;
	IOSTREAM * stream=0;
	int bufsize=0;
	int read_len=0;
	char * buf =0;
	int rval=0;
	if ( !PL_get_stream_handle(stream_term, &stream) )
	{
    	return PL_warning("orig_read: problem obtaining backend stream handle.");  
	}  	
	if(stream->functions!=&Sdelegate_functions)
	{
		return PL_warning("orig_read: does not look like a hijacked stream.");
	}
	
	
	handle= (HANDLE_T *) stream->handle;
	
	if(! PL_get_integer(bufsize_term,&bufsize))
	{
		return PL_warning("orig_read: second arg does not look like an integer");		
	}

	buf = (char*)PL_malloc(sizeof(char)*bufsize);
	
	read_len=(*handle->orig_io_functions->read)(handle->orig_handle,buf,bufsize);
	rval=PL_unify_list_nchars(chars_term,read_len,buf);
	PL_free(buf);
	return rval;
}

foreign_t pl_orig_write(term_t stream_term, term_t chars_term, term_t tail_term){
	IOSTREAM * stream=0;
	HANDLE_T * handle =0;
	unsigned int len=0;
	char * buf=0;
	int write_len=0;
	char * tail =0;
	
	if ( !PL_get_stream_handle(stream_term, &stream) )
	{
    	return PL_warning("problem obtaining backend stream handle.");
  }  	
	if(stream->functions!=&Sdelegate_functions)
	{
		return PL_warning("does not look like a hijacked stream.");
	}
	handle = (HANDLE_T *) stream->handle;
	if(! PL_get_list_nchars(chars_term,&len,&buf,0))
	{
		return PL_warning("problems reading chars from second arg");
	}

	write_len=(*handle->orig_io_functions->write)(handle->orig_handle,buf,len);
	if(write_len<len)
	{
		tail = &(buf[write_len]);
		return PL_unify_list_chars(tail_term,tail);
	}	
	return PL_unify_nil(tail_term);
}


static void unhijack(IOSTREAM * stream)
{
	HANDLE_T * handle =0;
	if( stream->functions!=&Sdelegate_functions)
	{
		return;
	}
	handle = (HANDLE_T*)stream->handle;
  stream->handle=handle->orig_handle;
	stream->functions=handle->orig_io_functions;
	stream->flags=handle->orig_flags;
	PL_erase(handle->args_record);
	PL_erase(handle->stream_record);
	PL_free(handle);
	return;
}


foreign_t pl_hijack_stream(term_t stream_term, term_t module_term, term_t args_term)	
{ 
	/* A hack, i admit it.
	 On windows it seems to be allowed to modify this flag in prolog.
	 on linux, at least with my version, it is not allowed.
	 Pfff...
	*/
	
	
	
	IOSTREAM * stream=0;
	HANDLE_T * handle=0;
	int wasssolldas =  PL_set_feature("tty_control", PL_BOOL, TRUE);
	char * module_name =0;
	
	/*
	 * construct handle
	 */
	
	if ( !PL_get_stream_handle(stream_term, &stream) )
	{
		return PL_warning("stream_decorator_create/4: problem obtaining backend stream handle.");
	}  	
	handle = (HANDLE_T *)PL_malloc(sizeof(HANDLE_T));
	if(! handle)
	{
		return PL_warning("stream_decorator_create/4: could not allocate decorator handle.");
	}
	
	
	 

	handle->stream_record=PL_record(stream_term);
	handle->args_record=PL_record(args_term);
	handle->stream=stream;
	handle->orig_handle=stream->handle;
	handle->orig_io_functions=stream->functions;
	/*
	 * lookup delegate predicates
	 */
	 display_term(module_term);
	 if(! PL_get_atom_chars(module_term,&module_name))
	 {
	 	return PL_warning("stream_decorator_create/4: second term does not look like an atom??");
	 }
	 if(! PL_get_module(module_term,&(handle->module)))
	 {
	 	return PL_warning("stream_decorator_create/4: problems finding module.");
	 }
	handle->read_hook=PL_predicate("stream_decorator_read_hook",5,module_name);
	handle->write_hook=PL_predicate("stream_decorator_write_hook",4,module_name);
	handle->seek_hook=PL_predicate("stream_decorator_seek_hook",4,module_name);
	handle->close_hook=PL_predicate("stream_decorator_close_hook",2,module_name);		
	handle->control_hook=PL_predicate("stream_decorator_control_hook",4,module_name);	

	

	
	 /* a somewhat subtile detail:
	  we are not a file/pipe stream, even if the orig stream was one.
	  why? 
	  A couple of swi's io features rely on doing things to the underlying file handle
	  (for instance if it is a tty).Afaics this is the only occasion where the SIO_FILE
	  flag is queried.
	  For file streams, the implementation of Sfileno does (correctly!) assume,
	  that the file handle is simply the stream handle. In particular, it does not call the
	  control io function pointer. So if we did not unset
	  the SIO_FILE flag, weird things could happen. The following aproach 
	  seems sound to me: The flag will be unset, as will be the SIO_PIPE flag.
	  (we will remember to set them on unhijack) 
	  In our new handle we store the original return value of Sfile_no and give it 
	  as an extra paramter to the control hook. This way, an implementer may 
	  decide what to do with it. If not overridden, the default implemetation
	  should probably return the original fileno.	  
	  
	 */
	
	 /*remember fileno and flags*/
	 handle->orig_fileno=Sfileno(stream);
	 handle->orig_flags=stream->flags;
	 
	 /*unset file and pipe flags*/
	 if(stream->flags & SIO_FILE)
	 {
		 stream->flags-=SIO_FILE;
	 }
	 if(stream->flags & SIO_PIPE)
	 {
		 stream->flags-=SIO_PIPE;
	 }
	/*
	 * everybody keep cool this is a robbery...
	 */
	Slock(stream);
	stream->functions=&Sdelegate_functions;
	stream->handle=handle;
	Sunlock(stream);
	
	/*
	 * when the stream is closed, we must unhijack it - we do not know 
	 * when it will be destroyed, but it seems reasonable to assume that this will be
	 * shortly after it is closed.
	 */
	Sclosehook(unhijack);
	 
	PL_succeed;
}


foreign_t pl_unhijack_stream(term_t stream_term)
{
	IOSTREAM * stream=0;
	if ( !PL_get_stream_handle(stream_term, &stream) )
	{
    	return PL_warning("unhijack_stream/1: problem obtaining backend stream handle.");
  }  	
	
	
	unhijack(stream);
	PL_succeed;
	
	
}	


foreign_t pl_is_hijacked_stream(term_t stream_term)
{
	IOSTREAM * stream=0;
	if ( !PL_get_stream_handle(stream_term, &stream) )
	{
    	return PL_warning("is_hijacked_stream/1: problem obtaining backend stream handle.");
  }  	
	
	if( stream->functions==&Sdelegate_functions)
	{
		PL_succeed;
	}
	else
	{
		PL_fail;
	}
}	



install_t install()
{ 
	
	PL_register_foreign("hijack_stream", 3, pl_hijack_stream, 0);
	PL_register_foreign("unhijack_stream", 1, pl_unhijack_stream, 0);
	PL_register_foreign("is_hijacked_stream", 1, pl_is_hijacked_stream, 0);
	PL_register_foreign("orig_read", 3, pl_orig_read, 0);
	PL_register_foreign("orig_write", 3, pl_orig_write, 0);
}



