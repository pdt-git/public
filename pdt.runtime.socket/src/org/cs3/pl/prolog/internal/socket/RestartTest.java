package org.cs3.pl.prolog.internal.socket;


import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;

public class RestartTest extends TestCase {
	public void testRecover() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		PrologInterface pif = Factory.newInstance().create();
		//String kill=pif.getOption(SocketPrologInterface.PREF_KILLCOMMAND);
		SocketPrologInterface spif = (SocketPrologInterface) pif;
		String kill= spif.getKillcommand();
		
		pif.start();

		
		SocketSession session = (SocketSession) pif.getSession();
		long pid = session.getClient().getServerPid();
		Runtime.getRuntime().exec(new String[]{kill,""+pid});
		try{
			pif.stop();
		}
		catch(Throwable t){
			;
		}
		pif.start();
		session = (SocketSession) pif.getSession();
		assertTrue(pid!=session.getClient().getServerPid());
		assertNotNull(session.queryOnce("true"));
		

	}
	public void testRecover_lazy() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		PrologInterface pif = Factory.newInstance().create();
		//String kill=pif.getOption(SocketPrologInterface.KILLCOMMAND);
		SocketPrologInterface spif = (SocketPrologInterface) pif;
		String kill= spif.getKillcommand();
		
		
		//pif.start();

		
		SocketSession session = (SocketSession) pif.getSession();
		long pid = session.getClient().getServerPid();
		Runtime.getRuntime().exec(new String[]{kill,""+pid});
		try{
			pif.stop();
		}
		catch(Throwable t){
			;
		}
		//pif.start();
		session = (SocketSession) pif.getSession();
		assertTrue(pid!=session.getClient().getServerPid());
		assertNotNull(session.queryOnce("true"));
		

	}
}
