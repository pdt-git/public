#ifndef JTRANSFORMER_H
#define JTRANSFORMER_H

#include <jni.h>
#include <SWI-Stream.h>

typedef struct {
	JavaVM *jvm;
	jobject consoleSession;

	IOSTREAM * in;
	IOSTREAM * out;
	IOSTREAM * err;

	IOFUNCTIONS in_callbacks;
	IOFUNCTIONS out_callbacks;
	IOFUNCTIONS err_callbacks;
} console_io_t;

#endif
