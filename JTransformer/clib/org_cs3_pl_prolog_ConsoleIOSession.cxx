#include <sys/types.h>

#include <jni.h>

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include "jtransformer.h"
#include "org_cs3_pl_prolog_ConsoleIOSession.h"

void handler(int signum) {
	fprintf(stderr, "Signal Händler\n");
}

extern "C" {
	static int write_function(void *, char *, int, int);
	static int out_write_function(void *, char *, int);
	static int err_write_function(void *, char *, int);
	static int read_function(void *, char*, int);
}

static int out_write_function(void *handle, char *buf, int bufsize){
	return write_function(handle, buf, bufsize, 2);
}

static int err_write_function(void *handle, char *buf, int bufsize){
	return write_function(handle, buf, bufsize, 3);
}

static int read_function(void *handle, char *buf, int bufsize){
	console_io_t *console = (console_io_t *) handle;
	int rv;
	
	jobject obj = console->consoleSession;
	jclass cls;
	jfieldID field;
	jmethodID method;
	jbyteArray array;
	jbyte *barray;
	jthrowable exc;
	
	void *kenv;
	JNIEnv *env;
	
	jint ok = console->jvm->GetEnv(&kenv, JNI_VERSION_1_2);

	fprintf(stderr, "read_function called\n");

	if (ok != JNI_OK){
		fprintf(stderr, "COULD NOT FETCH JAVA ENVIRONMENT\n");
		exit(EXIT_FAILURE);
	}

	env = (JNIEnv*) kenv;

	cls = env->GetObjectClass(obj);
	method = env->GetMethodID(cls, "readHelper", "(I)I");
	field = env->GetFieldID(cls, "helperArray", "[B");
	array = (jbyteArray) env->GetObjectField(obj, field);

	env->MonitorEnter(obj);

	rv = env->CallIntMethod(obj, method, bufsize, 0);

	exc = env->ExceptionOccurred();

	if (exc != NULL){
		env->ExceptionDescribe();
		env->ExceptionClear();
		return -1;
	}

	barray = env->GetByteArrayElements(array, NULL);

	memcpy(buf, barray, rv);

	env->ReleaseByteArrayElements(array, barray, 0);

	env->MonitorExit(obj);

	return rv;
}

static int write_function(void *handle, char *buf, int bbufsize, int stream){
	console_io_t *console = (console_io_t*) handle;
	int rv;
	jsize bufsize=bbufsize;
	jobject obj = console->consoleSession;
	jclass cls;
	jfieldID field;
	jmethodID method;
	jbyte *barray = (jbyte*) 0xdeadbeef;
	jbyteArray array;
	jthrowable exc;
	
	void *kenv;
	JNIEnv *env;
	
	jint ok = console->jvm->GetEnv(&kenv, JNI_VERSION_1_2);

	fprintf(stderr, "write_function called on (%p)\n", obj);

	if (ok != JNI_OK){
		fprintf(stderr, "COULD NOT FETCH JAVA ENVIRONMENT\n");
		exit(EXIT_FAILURE);
	}

	env = (JNIEnv *) kenv;

	cls = env->GetObjectClass(obj);
	method = env->GetMethodID(cls, "writeHelper", "(I)I");
	field = env->GetStaticFieldID(cls, "helperArray", "[B");

	fprintf(stderr, "got various java descriptors (%p,%p,%p)\n", cls,
			method, field);

	rv = env->MonitorEnter(obj);

	fprintf(stderr, "Entered Monitor (%d)\n", rv);
	
	array = (jbyteArray) env->NewByteArray(bufsize);

	fprintf(stderr, "Created array %p\n", array);
	
	env->SetStaticObjectField(cls, field, array);

	fprintf(stderr, "Set array\n");

	exc = env->ExceptionOccurred();

	if (exc != NULL){
		env->ExceptionDescribe();
		env->ExceptionClear();
		return -1;
	}

	barray = env->GetByteArrayElements(array, NULL);

	fprintf(stderr, "Got unrolled array (%p)\n", barray);

	memcpy(barray, buf, bufsize);

	fprintf(stderr, "copied content to array <%s>\n", barray);
	
	env->ReleaseByteArrayElements(array, barray, 0);

	fprintf(stderr, "rolled array back together\n");
	
	rv = env->CallIntMethod(obj, method, stream);

	fprintf(stderr, "Method returned %d", rv);

	exc = env->ExceptionOccurred();

	if (exc != NULL){
		env->ExceptionDescribe();
		env->ExceptionClear();
		return -1;
	}
	

	rv = env->MonitorExit(obj);

	fprintf(stderr, "Exited Monitor\n");

	return rv;
}

static void *jxmalloc(size_t size, JNIEnv *env){
	void *rv = malloc(size);

	if (rv == NULL){
		jclass oom = 
			env->FindClass("java/lang/OutOfMemoryError");
		if (oom == NULL)
			/* can't even throw the exception :-( */
			exit(EXIT_FAILURE);
		env->ThrowNew(oom, "Out Of Memory in C code");
	}

	return rv;
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    createEngine
 * Signature: ()J
 *
JNIEXPORT jlong JNICALL
Java_org_cs3_pl_prolog_ConsoleIOSession_createEngine
(JNIEnv *env, jobject obj)
{
	jlong engine;
	static char * x[] = {"jtransformer", NULL};

	fprintf(stderr, "Initializing Prolog if needed\n");
	if (PL_is_initialised(NULL, NULL) == FALSE) {
		fprintf(stderr, "PROLOG NOT INITIALISED!\n");
		PL_initialise(1, x);
	}
	fprintf(stderr, "Creating engine\n");
	engine = (jlong) (jint) PL_create_engine(NULL);
	fprintf(stderr, "Created engine @ %ld\n", engine);

	return engine;
}
*/
/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    createConsoleStruct
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_createConsoleStruct
(JNIEnv *env, jobject obj)
{
	console_io_t *consoleIO = (console_io_t *)
		jxmalloc(sizeof(consoleIO), env);

	fprintf(stderr, "Creating console structure\n");
	fflush(NULL);

	env->GetJavaVM(&(consoleIO->jvm));
	consoleIO->consoleSession = env->NewGlobalRef(obj);

	consoleIO->in_callbacks.read = NULL;
	consoleIO->in_callbacks.write = NULL;
	consoleIO->in_callbacks.seek = NULL; /* we are not seekable */
	consoleIO->in_callbacks.close = NULL; /* neither are we closeable */
	consoleIO->in_callbacks.control = NULL;

	consoleIO->out_callbacks = consoleIO->in_callbacks;
	consoleIO->err_callbacks = consoleIO->in_callbacks;

	fprintf(stderr, "Setting callback functions\n");
	fflush(NULL);

	consoleIO->out_callbacks.write = out_write_function;
	consoleIO->err_callbacks.write = err_write_function;
	consoleIO->in_callbacks.read = read_function;

	fprintf(stderr, "Callback functions set\n");
	fflush(NULL);

	signal(5, handler);

	Java_org_cs3_pl_prolog_ConsoleIOSession_testCallbacks(env, obj, (jlong) (int) consoleIO);
	return (jlong) (int) consoleIO;
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    createStreamInC
 * Signature: (JI)J
 *
JNIEXPORT jlong JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_createStreamInC
(JNIEnv *env, jobject obj, jlong con, jint stream)
{
	fprintf(stderr, "createStreamInC\n");
	console_io_t *console = (console_io_t *) (int) con;
	long rv;
	int flags = SIO_NBUF;
	flags |= SIO_ISATTY;
	flags |= SIO_NOCLOSE;
	flags |= SIO_TEXT;
	flags |= SIO_NOFEOF;

	fprintf(stderr, "Creating Stream #%d\n", stream);
	fflush(NULL);

	switch (stream) {
		case 1: flags |= SIO_INPUT; 
			fprintf(stderr, "Creating input stream\n");
			fflush(NULL);
			console->in = 
				Snew(console, flags, &console->in_callbacks);
			rv = (jlong) (int) console->in;	
			break;
		case 2: flags |= SIO_OUTPUT;
			fprintf(stderr, "Creating output stream\n");
			fflush(NULL);
			console->out =
				Snew(console, flags, &console->out_callbacks);
			rv = (jlong) (int) console->out;
			break;
		case 3: flags |= SIO_OUTPUT;
			fprintf(stderr, "Creating error stream\n");
			fflush(NULL);
			console->err = 
				Snew(console, flags, &console->err_callbacks);
			rv = (jlong) (int) console->err;
	}

	fprintf(stderr, "Created at %p\n", (void*)(int)rv);
	fflush(NULL);

	return rv;
}
*/
/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    unifyWithTerm
 * Signature: (J)L/java/lang/String
 *
JNIEXPORT void JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_unifyWithTerm
(JNIEnv *env, jobject obj, jlong handle, jint streamnr)
{
	int rv;
	char *target;
	char *target2;
	term_t term = PL_new_term_refs(2);
	term_t targetTerm = term + 1;
	term_t term2 = PL_new_term_refs(2);
	term_t targetTerm2 = term2 + 1;

	fprintf(stderr, "Before call to PL_predicate\n");
	fflush(NULL);
	
	predicate_t p = PL_predicate("set_stream", 2, "system");

	rv = PL_unify_stream(term, (IOSTREAM *) (int) handle);
	fprintf(stderr,"unified %d (%d) \n", term, rv);

	rv = PL_unify_stream(term2, (IOSTREAM *) (int) handle);
	fprintf(stderr,"unified %d (%d) \n", term2, rv);
	fflush(NULL);

	switch (streamnr) {
		case 1:
			target="alias(user_input)";
			target2="alias(current_input)";
			break;
		case 2: target="alias(user_output)";
			target2="alias(current_output)";
			break;
		case 3: target="alias(user_error)";
			target2="alias(current_error)";
	}
	
	rv = PL_chars_to_term(target, targetTerm);
	fprintf(stderr, "bound term %d to %s (%d)\n", targetTerm, target, rv);
	rv = PL_chars_to_term(target2, targetTerm2);
	fprintf(stderr, "bound term %d to %s (%d)\n", targetTerm2, target2, rv);
	fflush(NULL);
	
	rv = PL_call_predicate(NULL, PL_Q_NODEBUG, p, term);

	if (rv)
		fprintf(stderr, "Binding succeeded\n");
	else
		fprintf(stderr, "Binding failed\n");

	rv = PL_call_predicate(NULL, PL_Q_NODEBUG, p, term2);

	if (rv)
		fprintf(stderr, "Binding succeeded\n");
	else
		fprintf(stderr, "Binding failed\n");

	fflush(NULL);
}
*/
/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    deleteConsoleStruct
 * Signature: (J)V
 */
JNIEXPORT void JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_deleteConsoleStruct
(JNIEnv *env, jobject obj, jlong ptr)
{
	console_io_t *consoleIO = (console_io_t*) (int) ptr;
	env->DeleteGlobalRef(consoleIO->consoleSession);

	free(consoleIO);
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    deleteEngine
 * Signature: (J)V
 */
JNIEXPORT void JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_deleteEngine
(JNIEnv *env, jobject obj, jlong ptr)
{
//	PL_destroy_engine((void *) (int) ptr);
}

JNIEXPORT void JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_testCallbacks
(JNIEnv *env, jobject obj, jlong ptr) {
	char buffer[256];

	fprintf(stderr, "in testCallbacks\n");
	fflush(NULL);
	
	console_io_t * console = (console_io_t *) (int) ptr;
	strcpy(buffer, "Hallo Welt (Out)\n");
	console->out_callbacks.write(console, buffer, strlen(buffer));
	strcpy(buffer, "Tschuess Welt (Err)\n");
	console->err_callbacks.write(console, buffer, strlen(buffer));
	console->in_callbacks.read(console, buffer, 255);
	console->out_callbacks.write(console, buffer, strlen(buffer));

}
