/*--------------------------------------------------------------------------*
 | Copyright (C) 2001 Christopher Kohlhaas                                  |
 |                                                                          |
 | This program is free software; you can redistribute it and/or modify     |
 | it under the terms of the GNU General Public License as published by the |
 | Free Software Foundation. A copy of the license has been included with   |
 | these distribution in the COPYING file, if not go to www.fsf.org         |
 |                                                                          |
 | As a special exception, you are granted the permissions to link this     |
 | program with every library, which license fulfills the Open Source       |
 | Definition as published by the Open Source Initiative (OSI).             |
 *--------------------------------------------------------------------------*/
package org.rapla.components.rpc;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;

public class RPCCallEvent implements java.io.Serializable {
    private static final long serialVersionUID = 1;
    String role;
    String methodName;
    Object[] arguments;

    private static HashMap methodMap = new HashMap();

    public RPCCallEvent(String role,String methodName,Object[] arguments) {
    	this.methodName = methodName;
    	this.arguments = arguments;
    	this.role = role;
    }

    public String getMethodName() {
        return methodName;
    }

    public String getRole() {
        return role;
    }

    private boolean checkMethod(Method method) {
    	Class[] types = method.getParameterTypes();
    	if (types.length == arguments.length) {
    	    int param;
    	    for (param=0;param<types.length;param++) {
    		if (!types[param].isInstance(arguments[param]))
    		    continue;
    	    }
    	    if (param == types.length) {
    		return true;
    	    }
    	}
    	return false;
    }

    public Method findMethod(Class givenClass) throws NoSuchMethodException {
    	String key = givenClass.getName() + methodName;
    	Method cachedMethod = (Method)  methodMap.get(key);
    	if (cachedMethod != null && checkMethod(cachedMethod)) {
    	    return cachedMethod;
    	}
    	Method[] methods = givenClass.getMethods();
    	for (int i=0;i<methods.length;i++) {
    	    if (methods[i].getName().equals(methodName)) {
        		if (checkMethod(methods[i])) {
        		    methodMap.put(key,methods[i]);
        		    return methods[i];
        		}
    	    }
    	}
    	throw new NoSuchMethodException(methodName  + " not found. Maybe its a private or protected method!");
    }
    
    public Object dispatchEventOn(Object object) throws InvocationTargetException,NoSuchMethodException,IllegalAccessException,IllegalArgumentException  {
    	Object result = findMethod(object.getClass()).invoke(object,arguments);
    	return result;
    }

    public Object[] getArguments() {
        return arguments;
    }

    public String toString() {
    	StringBuffer buf = new StringBuffer();
    	buf.append(getMethodName());
    	buf.append("(");
    	for (int i=0;i<arguments.length;i++) {
    	    if (i>0)
    		buf.append(", ");
    	    buf.append(arguments[i]);
    	}
    	buf.append(" )");
    	return buf.toString();
    }
}
