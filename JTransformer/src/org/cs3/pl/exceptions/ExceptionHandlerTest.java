package org.cs3.pl.exceptions;

import junit.framework.TestCase;


public class ExceptionHandlerTest extends TestCase {
	/**
	 *
	 */
	public ExceptionHandlerTest() {
		super();
	}
	
	public void testExceptionSwallowing(){
		try {
			throw new Exception("A Thrown Exception");
		} catch (Exception e){
			ExceptionHandler.handle(e);
		}
		ExceptionHandler.handle(new Error("An Error"));
		ExceptionHandler.handle(new Exception("An Exception"));
		
	}
}
