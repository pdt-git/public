/*
 * Created on 13.07.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.prolog;

/**
 * @author schulzs1
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class SessionException extends Exception {
	
	public SessionException(){
		
	}
	
	public SessionException(String msg){
		super(msg);
	}
	
	public SessionException(String msg, Throwable cause){
		super(msg, cause);
	}
	
	public SessionException(Throwable cause){
		super(cause);
	}
}
