#ifndef STREAM_DECODER_H
#define STREAM_DECODER_H



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


/* foreign predicates */
foreign_t pl_hijack_stream(term_t stream_term, term_t module_term, term_t args_term);
foreign_t pl_unhijack_stream(term_t stream_term);
foreign_t pl_orig_write(term_t stream_term, term_t chars_term, term_t tail_term);
foreign_t pl_orig_read(term_t stream_term, term_t bufsize_term, term_t chars_term);
#endif
