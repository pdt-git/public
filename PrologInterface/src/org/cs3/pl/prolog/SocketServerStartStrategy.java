/*
 */
package org.cs3.pl.prolog;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;

/**
 */
public class SocketServerStartStrategy implements ServerStartStrategy {
    private String executable;
    private String engineDir;

    public class _InputStreamPump extends InputStreamPump {

        private Writer log;
        public _InputStreamPump(InputStream s,Writer writer) {
            super(s);        
            this.log=writer;
        }
        protected void dataAvailable(char[] buffer, int length) {
            try {
                log.write(buffer,0,length);
                log.flush();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

    }
    /**
     * 
     */
    public SocketServerStartStrategy(String engineDir,String executable) {
        this.engineDir=engineDir;
        this.executable=executable;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.ServerStartStrategy#startServer(int)
     */
    public Process startServer(int port) {
        String sep=System.getProperty("file.separator");
         String cmdline = executable+ " -p library="+engineDir+" -s "+engineDir+sep+"main.pl -g consult_server("+port+")";//['"+engineDir+sep+"consult_server.pl'],consult_server("+port+")";
        Debug.debug("Starting server with " + cmdline);

        try {

            Process serverProcess = Runtime.getRuntime().exec(cmdline);
            while (!Util.probePort(port)) {
                try {
                    Thread.sleep(50);
                } catch (InterruptedException e1) {
                    Debug.report(e1);
                }
            }
            File logFile = Util.getLogFile("org.cs3.pdt.server.log");
            BufferedWriter writer = new BufferedWriter(new FileWriter(logFile));            
            new _InputStreamPump(serverProcess.getErrorStream(),writer).start();
            new _InputStreamPump(serverProcess.getInputStream(),writer).start();
            return serverProcess;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

}
