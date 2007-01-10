package org.cs3.pl.tuprolog.internal;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;

public class XpceTest extends TestCase {
	public void testXpce() throws  PrologInterfaceException {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		PrologInterface arsch = PrologInterfaceFactory.newInstance(TuProlog.FQN).create();
		arsch.start();
		try{			
			arsch.getSession().queryOnce("help");
		}catch(Exception pissnelke){
			Debug.report(pissnelke);
			fail();
		}

	}
	
	public void _testDifferent() throws Throwable{
		String home=System.getProperty("user.home");
		Runtime.getRuntime().exec(new String[]{"/usr/X11R6/bin/xterm"/*, "-e", "/opt/local/bin/xpce", "-g", "['/private/tmp/socketPif47748.tmp']"*/},new String[]{"DISPLAY=:0.0","HOME="+home});
		//Runtime.getRuntime().exec(new String[]{"/usr/X11R6/bin/xterm", "-e", "/opt/local/bin/xpce"/*, "-g", "['/private/tmp/socketPif47748.tmp']"*/},new String[]{"DISPLAY=:0.0"});
		System.out.println("success");
		
	}
}
