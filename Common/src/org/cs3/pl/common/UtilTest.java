package org.cs3.pl.common;

import java.io.IOException;

import junit.framework.TestCase;

public class UtilTest extends TestCase {
    public void testExec() throws IOException, InterruptedException {
        assertEquals("hallo",Util.exec("echo hallo")[0].trim());
    }
}
