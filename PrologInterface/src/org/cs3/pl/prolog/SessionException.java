package org.cs3.pl.prolog;

public class SessionException extends Exception {
	
	/**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 7988782404441586015L;

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
