package org.cs3.pl.prolog;

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
