#ifndef STREAM_DECODER_H
#define STREAM_DECODER_H

#include <SWI-Prolog.h>
#include <SWI-Stream.h>


/* decorator handle */

typedef struct streamhandle{
	IOFUNCTIONS * orig_io_functions;
	void * orig_handle;
	int orig_fileno;
	int orig_flags;
	IOSTREAM * stream; 
	record_t stream_record; 
	record_t args_record; 
	module_t module;
	predicate_t read_hook;
	predicate_t write_hook;
	predicate_t seek_hook;
	predicate_t close_hook;
	predicate_t control_hook;
} HANDLE_T;

/* delegating callbacks */
static int delegate_read(void *_handle, char *buf, int bufsize);
static int delegate_write(void *_handle, char*buf, int bufsize);
static long delegate_seek(void *_handle, long offset, int whence);
static int delegate_close(void *_handle);
static int delegate_control(void *_handle, int action, void *arg);
#endif
