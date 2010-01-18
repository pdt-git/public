package org.cs3.pl.prolog.tests;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

public class XpceTest extends TestCase {
	public void testXpce() throws  PrologInterfaceException {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		PrologInterface plInterface = AbstractPrologInterface.newInstance();
		plInterface.start();
		try{			
			plInterface.getSession().queryOnce("help");
		}catch(Exception pissnelke){
			Debug.report(pissnelke);
			fail();
		}

	}
	
	public void _testDifferent() throws Throwable{
		String home=System.getProperty("user.home");
		Runtime.getRuntime().exec(new String[]{"/usr/X11R6/bin/xterm"},new String[]{"DISPLAY=:0.0","HOME="+home});
		System.out.println("success");
		
	}
}
