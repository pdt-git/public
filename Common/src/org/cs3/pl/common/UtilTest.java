package org.cs3.pl.common;

import java.io.IOException;

import junit.framework.TestCase;

public class UtilTest extends TestCase {
    public void testExec() throws IOException, InterruptedException {
    	String[] r = org.cs3.pl.common.Util.exec("plwin -t source_file(A),concat(X,'/plwin.rc',A),write(X),nl,flush_output. -g halt.");
		System.out.println("'"+r[0]+"'\n'"+r[1]+"'" );
    	assertEquals("hallo",Util.exec("echo hallo")[0].trim());
    }
}
