/*
 * This file is a replacement for Sun's jni_md.h.  It is here to make
 * cross-compilation for Windows work in case $(JAVA_HOME)/include/win32
 * is not installed.  It's a simple file that will hopefully not change
 * between Java versions.
 */

#ifndef _JNI_MD_H_
#define _JNI_MD_H_

#define JNIEXPORT __declspec(dllexport)
#define JNIIMPORT __declspec(dllimport)
#define JNICALL __stdcall

typedef long jint;
typedef __int64 jlong;
typedef signed char jbyte;

#endif
