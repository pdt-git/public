#include <sys/types.h>

#include <jni.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include "jtransformer.h"

static int in_write_function(void *handle, char *buf, int bufsize){
	write_function(handle, buf, bufsize, 1);
}

static int out_write_function(void *handle, char *buf, int bufsize){
	write_function(handle, buf, bufsize, 2);
}

static int err_write_function(void *handle, char *buf, int bufsize){
	write_function(handle, buf, bufsize, 3);
}

static int read_function(void *handle, char *buf, int bufsize){
	console_io_t *console = handle;
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
	
	jint ok = (*console->jvm)->GetEnv(console->jvm, &kenv, JNI_VERSION_1_2);

	if (ok != JNI_OK){
		fprintf(stderr, "COULD NOT FETCH JAVA ENVIRONMENT\n");
		exit(EXIT_FAILURE);
	}

	env = kenv;

	cls = (*env)->GetObjectClass(env, obj);
	method = (*env)->GetMethodID(env, cls, "readHelper", "(I)I");
	field = (*env)->GetFieldID(env, cls, "helperArray", "[B");
	array = (*env)->GetObjectField(env, obj, field);

	(*env)->MonitorEnter(env, obj);

	rv = (*env)->CallIntMethod(env, obj, method, bufsize, 0);

	exc = (*env)->ExceptionOccurred(env);

	if (exc != NULL){
		(*env)->ExceptionDescribe(env);
		(*env)->ExceptionClear(env);
		return 0;
	}

	barray = (*env)->GetByteArrayElements(env, array, NULL);

	memcpy(buf, barray, rv);

	(*env)->ReleaseByteArrayElements(env, array, barray, 0);

	(*env)->MonitorExit(env, obj);

	return rv;
}

static int write_function(void *handle, char *buf, int bufsize, int stream){
	console_io_t *console = handle;
	int rv;
	
	jobject obj = console->consoleSession;
	jclass cls;
	jfieldID field;
	jmethodID method;
	jbyte *barray;
	jbyteArray array;
	jthrowable exc;
	
	void *kenv;
	JNIEnv *env;
	
	jint ok = (*console->jvm)->GetEnv(console->jvm, &kenv, JNI_VERSION_1_2);

	if (ok != JNI_OK){
		fprintf(stderr, "COULD NOT FETCH JAVA ENVIRONMENT\n");
		exit(EXIT_FAILURE);
	}

	env = kenv;

	cls = (*env)->GetObjectClass(env, obj);
	method = (*env)->GetMethodID(env, cls, "writeHelper", "(I)I");
	field = (*env)->GetFieldID(env, cls, "helperArray", "[B");

	(*env)->MonitorEnter(env, obj);

	array = (*env)->NewByteArray(env, bufsize);
	(*env)->SetObjectField(env, obj, field, array);
	barray = (*env)->GetByteArrayElements(env, array, 0);

	memcpy(barray, buf, bufsize);
	rv = (*env)->CallIntMethod(env, obj, method, stream);

	exc = (*env)->ExceptionOccurred(env);

	if (exc != NULL){
		(*env)->ExceptionDescribe(env);
		(*env)->ExceptionClear(env);
		return 0;
	}
	
	(*env)->ReleaseByteArrayElements(env, array, barray, 0);

	(*env)->MonitorExit(env, obj);

	return rv;
}

static int control_function(void *handle, int action, void *arg){
	// FIXME: WHAT NOW?! 
	return 0;
}

static void *jxmalloc(size_t size, JNIEnv *env){
	void *rv = malloc(size);

	if (rv == NULL){
		jclass oom = 
			(*env)->FindClass(env, "java/lang/OutOfMemoryError");
		if (oom == NULL)
			/* can't even throw the exception :-( */
			exit(EXIT_FAILURE);
		(*env)->ThrowNew(env, oom, "Out Of Memory in C code");
	}

	return rv;
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    createEngine
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_org_cs3_pl_prolog_ConsoleIOSession_createEngine
(JNIEnv *env, jobject obj)
{
	return (jlong) (jint) PL_create_engine(NULL);
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    createConsoleStruct
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_createConsoleStruct
(JNIEnv *env, jobject obj)
{
	console_io_t *consoleIO = jxmalloc(sizeof(consoleIO), env);

	(*env)->GetJavaVM(env, &(consoleIO->jvm));
	consoleIO->consoleSession = (*env)->NewGlobalRef(env, obj);

	consoleIO->in_callbacks.read = read_function;
	consoleIO->in_callbacks.write = in_write_function;
	consoleIO->in_callbacks.seek = NULL; /* we are not seekable */
	consoleIO->in_callbacks.close = NULL; /* neither are we closeable */
	consoleIO->in_callbacks.control = control_function;

	consoleIO->out_callbacks = consoleIO->in_callbacks;
	consoleIO->err_callbacks = consoleIO->in_callbacks;

	colsoleIO->out_callback.write = out_write_function;
	colsoleIO->err_callback.write = err_write_function;

	return (jlong) (int) consoleIO;
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    createStreamInC
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_createStreamInC
(JNIEnv *env, jobject obj, jlong con, jint stream)
{
	console_io_t *console = (void*)con;
	console = NULL;

	return NULL;
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    unifyWithTerm
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_unifyWithTerm
(JNIEnv *env, jobject obj, jlong handle)
{
	return NULL;
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    setStream
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT void JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_setStream
(JNIEnv *env, jobject obj, jlong stream, jstring term)
{
}

/*
 * Class:     org_cs3_pl_prolog_ConsoleIOSession
 * Method:    deleteConsoleStruct
 * Signature: (J)V
 */
JNIEXPORT void JNICALL 
Java_org_cs3_pl_prolog_ConsoleIOSession_deleteConsoleStruct
(JNIEnv *env, jobject obj, jlong ptr)
{
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
	PL_destroy_engine((void *) (int) ptr);
}
