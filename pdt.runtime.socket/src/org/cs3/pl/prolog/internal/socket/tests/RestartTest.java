package org.cs3.pl.prolog.internal.socket.tests;


import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.cs3.pl.prolog.internal.socket.JackTheProcessRipper;
import org.cs3.pl.prolog.internal.socket.SocketSession;

public class RestartTest extends TestCase {
	public void testRecover() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		
		PrologInterface pif = AbstractPrologInterface.newInstance();
		
		pif.start();

		
		SocketSession session = (SocketSession) pif.getSession(PrologInterface.LEGACY);
		long pid = session.getClient().getServerPid();
		JackTheProcessRipper.getInstance().markForDeletion(pid);
		try{
			pif.stop();
		}
		catch(Throwable t){
			;
		}
		pif.start();
		session = (SocketSession) pif.getSession(PrologInterface.LEGACY);
		assertTrue(pid!=session.getClient().getServerPid());
		assertNotNull(session.queryOnce("true"));
	}
	
	
	public void testRecover_lazy() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		PrologInterface pif = AbstractPrologInterface.newInstance();
		
		SocketSession session = (SocketSession) pif.getSession(PrologInterface.LEGACY);
		long pid = session.getClient().getServerPid();
		JackTheProcessRipper.getInstance().markForDeletion(pid);
		try{
			pif.stop();
		}
		catch(Throwable t){
			;
		}
		session = (SocketSession) pif.getSession(PrologInterface.LEGACY);
		assertTrue(pid!=session.getClient().getServerPid());
		assertNotNull(session.queryOnce("true"));
	}
}
