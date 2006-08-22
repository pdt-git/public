package org.cs3.jtransformer.tests;

import java.io.IOException;

import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;

public class SourceLocationArgumentTest extends PseudoRoundTripTest {


	public SourceLocationArgumentTest() {
		super("testIt", "test0002");
	}
	
	
    public synchronized void testIt_impl() throws CoreException, IOException,
    BadLocationException, InterruptedException, PrologException, PrologInterfaceException 
    {
		super.testIt_impl();
		PrologInterface pif = getTestJTransformerProject().getPrologInterface();
		PrologSession session = null;
		try {
			session = pif.getSession();
			//session.queryAll(sl_argT)
		} finally {
			if(session != null) {
				session.dispose();
			}
		}
	}
}
