/*
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;

import sun.rmi.runtime.GetThreadPoolAction;

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
    public Process startServer(PrologInterface pipi) {
    		SocketPrologInterface pif = (SocketPrologInterface) pipi;
    		pif.setLockFile(Util.getLockFile());
        if(Boolean.valueOf (pif.getOption(SocketPrologInterface.STANDALONE)).booleanValue()){
            Debug.warning("Will not start server; the option "+SocketPrologInterface.STANDALONE+" is set.");
            return null;
        }
        int port;
		try {
			port = Util.findFreePort();
			pif.setPort(port);
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
        String executable = pif.getOption(SocketPrologInterface.EXECUTABLE);
        String engineDir = Util.prologFileName(pif.getFactory().getResourceLocator().resolve("/"));
        
        File tmpFile=null;
        try {			
			tmpFile = File.createTempFile("socketPif",null);
			PrintWriter p = new PrintWriter(new BufferedOutputStream(new FileOutputStream(tmpFile)));
			if(pif.isHidePlwin()){
				p.println(":- (  (current_prolog_flag(executable,_A),atom_concat(_,'plwin.exe',_A))" +
						"->win_window_pos([show(false)])" +
						";true).");
			}
			List bootstrapLIbraries = pif.getBootstrapLibraries();
	        for (Iterator it = bootstrapLIbraries.iterator(); it.hasNext();) {
	            String s = (String) it.next();
	            p.println(":- ['"+s+"'].");	            
	        }
	        p.println("file_search_path(library,'"+engineDir+"').");
	        p.println(":-consult_server("+port+",'"+Util.prologFileName(pif.getLockFile())+"').");
	        p.close();
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
        
        String[] command=executable.split(" ");
		String[] args= new String[]{
				"-g",
				"['"+Util.prologFileName(tmpFile)+"']",
				
		};
		String[] commandArray = new String[command.length+args.length];
		System.arraycopy(command,0,commandArray,0,command.length);
		System.arraycopy(args,0,commandArray,command.length,args.length);
        StringBuffer sb = new StringBuffer();
		sb.append(executable);
		sb.append(" -g ['");

        sb.append(Util.prologFileName(tmpFile));
        sb.append("']");
        String cmdline = sb.toString();
        Debug.info("Starting server with " + Util.prettyPrint(commandArray));
        
        

        try {

            Process serverProcess = Runtime.getRuntime().exec(commandArray);
            
            File logFile = Util.getLogFile("org.cs3.pdt.server.log");
            BufferedWriter writer = new BufferedWriter(new FileWriter(logFile,true));
            writer.write("\n---8<-----------------------8<---\n");
            writer.write(new Date().toString()+"\n");
            writer.write("---8<-----------------------8<---\n\n");
            new _InputStreamPump(serverProcess.getErrorStream(), writer)
                    .start();
            new _InputStreamPump(serverProcess.getInputStream(), writer)
                    .start();
            
            long timeout=pif.getTimeout();
            long startTime = System.currentTimeMillis();
            while (!pif.getLockFile().exists()) {
                try {
                	long now = System.currentTimeMillis();
                	if(now-startTime>timeout){
                		throw new RuntimeException("Timeout exceeded while waiting for peer to come up.");
                	}
                    Thread.sleep(50);
                } catch (InterruptedException e1) {
                    Debug.report(e1);
                }
                try{
                	if(serverProcess.exitValue()!=0){
                		throw new RuntimeException("Failed to start server. Process exited with err code "+serverProcess.exitValue());
                	}
                }catch (IllegalThreadStateException e) {
                	; //nothing. the process is still running.
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
    public void stopServer(PrologInterface pipi, boolean now) {
    	SocketPrologInterface pif = (SocketPrologInterface) pipi;
        try {
            if(Boolean.valueOf (pif.getOption(SocketPrologInterface.STANDALONE)).booleanValue()){
                Debug.warning("Will not stop server; the option "+SocketPrologInterface.STANDALONE+" is set.");
                return;
            }
            int port = pif.getPort();
            if (!pif.getLockFile().exists()) {
                Debug
                        .info("There is no server running, afaics. So i wont stop anything.");
                return;
            }
            SocketClient c = new SocketClient((String)null, port);
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

           pif.getLockFile().delete();
           
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
        File lockFile = ((SocketPrologInterface)pif).getLockFile();
		return lockFile!=null&&lockFile.exists();
    }
}
