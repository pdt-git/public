#ifndef STREAM_DECODER_H
#define STREAM_DECODER_H

#include <SWI-Prolog.h>
#include <SWI-Stream.h>


/* decorator handle */

typedef struct streamhandle{
	IOSTREAM * orig_stream; /*the backend stream*/
	IOSTREAM * decorator_stream; /*the decorator stream*/
	term_t orig_stream_term; /*ref to the original handle term*/ 
	term_t decorator_stream_term;		/*ref to the decorator handle term*/
	term_t args_term; /*argument term*/
	module_t module;
} HANDLE_T;

/* delegating callbacks */
int delegate_read(void *_handle, char *buf, int bufsize);
int delegate_write(void *_handle, char*buf, int bufsize);
long delegate_seek(void *_handle, long offset, int whence);
int delegate_close(void *_handle);
int delegate_control(void *_handle, int action, void *arg);
#endif
