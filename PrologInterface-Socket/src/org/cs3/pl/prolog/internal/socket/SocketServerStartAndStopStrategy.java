/*
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;

/**
 */
public class SocketServerStartAndStopStrategy implements
        ServerStartAndStopStrategy {

    public class _InputStreamPump extends InputStreamPump {

        private Writer log;

        public _InputStreamPump(InputStream s, Writer writer) {
            super(s);
            this.log = writer;
        }

        protected void dataAvailable(char[] buffer, int length) {
            try {
                log.write(buffer, 0, length);
                log.flush();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.ServerStartAndStopStrategy#startServer(org.cs3.pl.prolog.IPrologInterface)
     */
    public Process startServer(PrologInterface pif) {
        int port = Integer.parseInt(pif.getOption(SocketPrologInterface.PORT));
        String executable = pif.getOption(SocketPrologInterface.EXECUTABLE);
        String engineDir = pif.getOption(SocketPrologInterface.ENGINE_DIR);
        String sep = System.getProperty("file.separator");
        //        String cmdline = executable + " -p library=" + engineDir + " -s "
        //                + engineDir + sep + "main_socket.pl -g consult_server(" + port +
        // ")";//['"+engineDir+sep+"consult_server.pl'],consult_server("+port+")";
        StringBuffer sb = new StringBuffer();
        sb.append(executable);
        sb.append(" -p library=");
        sb.append(engineDir);
        sb.append(" -g [");
        List bootstrapLIbraries = pif.getBootstrapLibraries();
        for (Iterator it = bootstrapLIbraries.iterator(); it.hasNext();) {
            String s = (String) it.next();
            sb.append("'");
            sb.append(s);
            sb.append("'");
            if (it.hasNext()) {
                sb.append(",");
            }
        }
        sb.append("],consult_server(");
        sb.append(port);
        sb.append(")");
        String cmdline = sb.toString();
        Debug.debug("Starting server with " + cmdline);

        try {

            Process serverProcess = Runtime.getRuntime().exec(cmdline);
            File logFile = Util.getLogFile("org.cs3.pdt.server.log");
            BufferedWriter writer = new BufferedWriter(new FileWriter(logFile));
            new _InputStreamPump(serverProcess.getErrorStream(), writer)
                    .start();
            new _InputStreamPump(serverProcess.getInputStream(), writer)
                    .start();
            while (!Util.probePort(port)) {
                try {
                    Thread.sleep(50);
                } catch (InterruptedException e1) {
                    Debug.report(e1);
                }
            }
            

            return serverProcess;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.ServerStartAndStopStrategy#stopServer(org.cs3.pl.prolog.IPrologInterface,
     *           boolean)
     */
    public void stopServer(PrologInterface pif, boolean now) {
        try {
            int port = Integer.parseInt(pif
                    .getOption(SocketPrologInterface.PORT));
            if (!Util.probePort(port)) {
                Debug
                        .info("There is no server running, afaics. So i wont stop anything.");
                return;
            }
            SocketClient c = new SocketClient("localhost", port);
            try {
                c.readUntil(SocketClient.GIVE_COMMAND);
                c.writeln(SocketClient.SHUTDOWN);
                c.readUntil(SocketClient.BYE);
                c.close();
            } catch (Exception e) {
                Debug.warning("There was a problem during server shutdown.");
                Debug.report(e);
                if (!now) {
                    throw new RuntimeException(e);
                }
            }

            while ((Util.probePort(port))) {
                try {
                    Thread.sleep(50);
                } catch (InterruptedException e1) {
                    Debug.report(e1);
                }
            }
        } catch (Throwable e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.ServerStartAndStopStrategy#isRunning(org.cs3.pl.prolog.IPrologInterface)
     */
    public boolean isRunning(PrologInterface pif) {
        int port = Integer.parseInt(pif.getOption(SocketPrologInterface.PORT));
        return Util.probePort(port);
    }
}
