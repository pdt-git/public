/*
 * Created on 02.09.2004
 *
 */
package org.cs3.pl.prolog;

import java.io.IOException;

import junit.framework.TestCase;

/**
 * @author rho
 *
 */
public class PrologServerTest extends TestCase {
    
    /**
     * @throws IOException
     * 
     */
    public void testSystemExit() throws IOException {
        PrologManager manager = PrologManager.getInstance();
        IPrologClient client = manager.getClient();
        assertTrue("killed server process",manager.killServerProcess());  
    }

}
