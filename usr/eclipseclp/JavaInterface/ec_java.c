/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 *
 * Contributor(s): 
 *
 * END LICENSE BLOCK */

/*
 * Eclipse Java interface
 *
 * $Id: ec_java.c,v 1.1 2006/09/23 01:54:08 snovello Exp $
 *
 */

#include <stdlib.h>
#include <eclipse.h>
#include "com_parctechnologies_eclipse_NativeEclipse.h"


#define CHECK_JAVA_EXCEPTION \
	if((*JavaEnv)->ExceptionOccurred(JavaEnv)) {\
		(*JavaEnv)->ExceptionDescribe(JavaEnv);\
		(*JavaEnv)->ExceptionClear(JavaEnv);\
		return -1;\
	}

static JNIEnv * JavaEnv;

#if 0
 * Unused: reference type to store refs to java objects in eclipse

void       JavaRefFree(t_ext_ptr obj);
t_ext_ptr  JavaRefCopy(t_ext_ptr obj);
t_ext_type JavaRefType =
{
	JavaRefFree,
	JavaRefCopy,
	NULL, /* mark_dids */
	NULL, /* string_size */
	NULL, /* to_string */
	NULL, /* equal */
	NULL, /* remote_copy  */
	NULL, /* get  */
	NULL  /* set  */
};


void
JavaRefFree(t_ext_ptr obj)
{
	(*JavaEnv)->DeleteGlobalRef(JavaEnv,(jobject) obj);
}

t_ext_ptr  JavaRefCopy(t_ext_ptr obj)
{
	jobject NewObj;

	NewObj = (*JavaEnv)->NewGlobalRef(JavaEnv,(jobject) obj);
	return (t_ext_ptr) NewObj;
}
#endif
	
/*
 * Class:     eclipse_NativeEclipse
 * Method:    init
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_init
  (JNIEnv * env, jclass jc)
{
	JavaEnv = env;
	return ec_init();
}


/*
 * Class:     eclipse_NativeEclipse
 * Method:    resume
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_resume
  (JNIEnv * e, jclass jc)
{
    return ec_resume();
}



/*
 * Class:     eclipse_NativeEclipse
 * Method:    setOption
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_setOption__II
  (JNIEnv * e, jclass jc, jint jiOpt, jint jiVal)
{
    return (jint) ec_set_option_int((int) jiOpt, (int) jiVal);
}

/*
 * Class:     eclipse_NativeEclipse
 * Method:    setOption
 * Signature: (ILjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_setOption__ILjava_lang_String_2
  (JNIEnv * e, jclass js, jint jiOpt, jstring jsVal)
{
    const char * s;
    jint res;
    char *buf;

    s = (*e)->GetStringUTFChars(e,jsVal,NULL);
    /* make a permanent copy */
    buf = (char *) malloc(strlen(s)+1);
    strcpy(buf,s);
    res = (jint) ec_set_option_ptr((int) jiOpt, buf);
    (*e)->ReleaseStringUTFChars(e,jsVal,s);
    return res;
}

/*
 * Class:     com_parctechnologies_eclipse_NativeEclipse
 * Method:    setOption
 * Signature: (I[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_setOption__I_3Ljava_lang_String_2
  (JNIEnv * e, jclass jc , jint jiOpt, jobjectArray jsArray)
{
  jsize arrayLen = (*e)->GetArrayLength(e, jsArray);
  jstring currentString;
  const char * s;
  jint res;
  static char ** bufs;
  int i;

  bufs = (char **) malloc(arrayLen * sizeof(char *));
  for(i = 0; i < arrayLen; i++)
    {
      currentString = 
	(jstring) (*e)->GetObjectArrayElement(e, jsArray, (jsize) i);
      bufs[i] = (char *) malloc(1024 * sizeof(char));
      s = (*e)->GetStringUTFChars(e,currentString,NULL);
      strcpy(bufs[i],s);
      (*e)->ReleaseStringUTFChars(e,currentString,s);     

    }

  res = (jint) ec_set_option_ptr((int) jiOpt,(char **) bufs);

  return(res);

}


/*
 * Class:     eclipse_NativeEclipse
 * Method:    QueueRead
 * Signature: (III[B)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_QueueRead
  (JNIEnv * e, jclass jc,
  jint jiQueue, jint jiPos, jint jiLen, jbyteArray jbBuf)
{
    jint jiBytesRead;
    jsize n;
    jbyte * buf;
    n = (*e)->GetArrayLength(e,jbBuf) - jiPos;
    if (jiLen < n)
    	n = jiLen;
    buf = (*e)->GetByteArrayElements(e,jbBuf,NULL);
    jiBytesRead = (jint) ec_queue_read((int) jiQueue,buf+jiPos,n);

    (*e)->ReleaseByteArrayElements(e,jbBuf,buf,0);
    return jiBytesRead;
}

/*
 * Class:     com_parctechnologies_eclipse_NativeEclipse
 * Method:    QueueReadByte
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_QueueReadByte
  (JNIEnv * e, jclass jc, jint jiQueue)
{
  jbyte byte_read;
  int ec_queue_read_return_val =
    ec_queue_read((int) jiQueue, &byte_read, 1);

  /* return -1 if zero bytes were read */
  if(ec_queue_read_return_val == 0)
  {
    return((jint) -1);
  }

  /* the other return values are -192 and -193, so the -1 doesn't clash */
  /* with these.  */
  if(ec_queue_read_return_val < 0)
  {
    return((jint) ec_queue_read_return_val);
  }

    return((jint) (0xff & byte_read));
  
}


/*
 * Class:     com_parctechnologies_eclipse_NativeEclipse
 * Method:    QueueWriteByte
 * Signature: (IB)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_QueueWriteByte
  (JNIEnv * e, jclass jc, jint jiQueue, jbyte write_byte)
{
  char write_char = (char) write_byte;
  jint res;
  res = (jint) ec_queue_write((int) jiQueue,&write_char,1);
  return(res);
}


/*
 * Class:     com_parctechnologies_eclipse_NativeEclipse
 * Method:    QueueAvailable
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_QueueAvailable
  (JNIEnv *e, jclass jc, jint stream)
{
  return ec_queue_avail((int) stream);

}


/*
 * Class:     com_parctechnologies_eclipse_NativeEclipse
 * Method:    QueueWrite
 * Signature: (I[BII)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_QueueWrite
  (JNIEnv * e, jclass jc, jint jiQueue, jbyteArray jbBuf, jint jiPos, jint jiLen)
{
	jsize n;
    jbyte * buf;
	jint res;

	n = (*e)->GetArrayLength(e,jbBuf) - jiPos;
    if (jiLen < n)
    	n = jiLen;
	buf = (*e)->GetByteArrayElements(e,jbBuf,NULL);
    res = (jint) ec_queue_write((int) jiQueue,(char *) buf + jiPos,(int) n);
    (*e)->ReleaseByteArrayElements(e,jbBuf,buf,0);
    return res;
}

/*
 * Class:     eclipse_NativeEclipse
 * Method:    StreamNumber
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_StreamNumber
  (JNIEnv * e, jclass jc, jstring jsStreamName)
{
    const char * s;
    jint res;

    s = (*e)->GetStringUTFChars(e,jsStreamName,NULL);
    res = (jint) ec_stream_nr((char *) s);
    (*e)->ReleaseStringUTFChars(e,jsStreamName,s);
    return res;
}

/*
 * Class:     eclipse_NativeEclipse
 * Method:    HandleEvents
 * Signature: (Ljava/lang/Integer;)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_HandleEvents
  (JNIEnv * e, jclass jc, jobject StreamId)
{
	jint res;
	long stream_id;
	jclass StreamIdClass;
	jfieldID fid;

	StreamIdClass = (*e)->GetObjectClass(e,StreamId);
	fid = (*e)->GetFieldID( e, StreamIdClass, "value","I");
	res = ec_handle_events(&stream_id);
	(*e)->SetIntField(e,StreamId,fid,(jint) stream_id);
	return res;
}
/*
 * Class:     eclipse_NativeEclipse
 * Method:    resumeLong
 * Signature: (Ljava/lang/Integer;)I
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_resumeLong
  (JNIEnv * e, jclass jc, jobject StreamId)
{
	jint res;
	long stream_id;
	jclass StreamIdClass;
	jfieldID fid;

	StreamIdClass = (*e)->GetObjectClass(e,StreamId);
	fid = (*e)->GetFieldID( e, StreamIdClass, "value","I");
	res = (jint) ec_resume_long(&stream_id);
	(*e)->SetIntField(e,StreamId,fid,(jint) stream_id);
	return res;
}


/*
 * Class:     com_parctechnologies_eclipse_NativeEclipse
 * Method:    Cleanup
 * Signature: ()V
 *
 * Calls ec_cleanup, which frees all resources used by eclipse
 */
JNIEXPORT void JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_Cleanup
  (JNIEnv *je, jclass jc)
{
  ec_cleanup();
}


/*
 * javaConstructor(ClassNameString,SignatureString,ArgsStructure,Instance)
 * Call = javaCallConstructor(Class,MethodStruct,JSig,ESig,NewInstance),
 */


/*
int
p_javaCallConstructor()
{
    int res;
    char * class_name;
    char * java_signature;
    char * ec_signature;
    int arity;
    int argn;
    pword pargs,arg;
    jclass ThisClass;
    jmethodID ThisConstructor;
    jobject ThisNewObject;
    jvalue *jArgs;

    res = 0;
    res += ec_get_string(ec_arg(1), &class_name);
    res -= PSUCCEED;
    res += ec_get_string(ec_arg(3), &java_signature);
    res -= PSUCCEED;
    res += ec_get_string(ec_arg(4), &ec_signature);
    res -= PSUCCEED;
    ThisClass = (*JavaEnv)->FindClass(JavaEnv,class_name);
    CHECK_JAVA_EXCEPTION
    ThisConstructor = (*JavaEnv)->GetMethodID(JavaEnv,ThisClass,"<init>",java_signature);
    CHECK_JAVA_EXCEPTION
    pargs = ec_arg(2);
    arity = ec_arity(pargs);

    jArgs = (jvalue *) malloc(arity * sizeof(jvalue));
    for (argn = 1 ; argn <= arity ; argn++)
    {
	ec_get_arg(argn,pargs,&arg);
	ec_to_java(arg,&(jArgs[argn-1]),&ec_signature);
    }
    ThisNewObject = (*JavaEnv)->NewObjectA(JavaEnv,ThisClass,ThisConstructor,jArgs);
    free(jArgs);
    CHECK_JAVA_EXCEPTION

    return ec_unify_arg(3, ec_handle(&JavaRefType,JavaRefCopy((t_ext_ptr) ThisNewObject)));
}

int ec_to_java(pword pw, jvalue * jv, char **signature)
{
    char * s;
    long i,ar;
    pword pwarg;

    switch(**signature++)
    {
 	case 'I':
	    ec_get_long(pw,&(jv->i));
	    break;   
 	case 'S':
	    ec_get_string_length(pw,&s,&i);
	    jv->l = (*JavaEnv)->NewString(JavaEnv,(jchar *) s,i);
	    break;   
 	case 'D':
	    ec_get_double(pw,&jv->d);
	    break;   
 	case 'O':
	    ec_get_handle(pw,&JavaRefType,(t_ext_ptr) &jv->l);
	    break;   
 	case '[':
	    ar = ec_arity(pw);
	    switch(**signature++)
	    {
	    case 'I':
		{
		    jintArray ja;
		    jint * ji;

		    ja = (*JavaEnv)->NewIntArray(JavaEnv,ar);
		    ji = (*JavaEnv)->GetIntArrayElements(JavaEnv,ja,NULL);
		    for(i = 0 ;  i < ar ; i++)
		    {
			ec_get_arg(i+1,pw,&pwarg);
			ec_get_long(pwarg,ji+i);
		    }
		    (*JavaEnv)->ReleaseIntArrayElements(JavaEnv,ja,ji,0);
		    break;   
	    	}
	    case 'D':
		{
		    jdoubleArray ja;
		    jdouble * ji;

		    ja = (*JavaEnv)->NewDoubleArray(JavaEnv,ar);
		    ji = (*JavaEnv)->GetDoubleArrayElements(JavaEnv,ja,NULL);
		    for(i = 0 ;  i < ar ; i++)
		    {
			ec_get_arg(i+1,pw,&pwarg);
			ec_get_double(pwarg,ji+i);
		    }
		    (*JavaEnv)->ReleaseDoubleArrayElements(JavaEnv,ja,ji,0);
		    break;   
	    	}
		(*JavaEnv)->NewDoubleArray(JavaEnv,ar);
		break;   
	    }
	    break;   
    	default:
	    return RANGE_ERROR;
    }
    CHECK_JAVA_EXCEPTION
    return PSUCCEED;
}
*/

/*************
 * Type Signature Java Type
 * 
 * Z boolean
 * 
 * B byte
 * 
 * C char
 * 
 * S short
 * 
 * I int
 * 
 * J long
 * 
 * F float
 * 
 * D double
 * 
 * L fully-qualified-class ;
 * 
 * fully-qualified-class
 * 
 * [ type type[]
 * 
 * ( arg-types ) ret-type method type
 */ 
