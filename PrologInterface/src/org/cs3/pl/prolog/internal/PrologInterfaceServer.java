/*
 * Created on 21.01.2004
 *
 */
package org.cs3.pl.prolog.internal;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.text.DateFormat;
import java.util.Date;
import java.util.EventObject;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;

import jpl.Atom;
import jpl.JPLException;
import jpl.Query;
import jpl.fli.Prolog;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.rapla.components.rpc.ClientConnection;
import org.rapla.components.rpc.ConnectionListener;
import org.rapla.components.rpc.Logger;
import org.rapla.components.rpc.MessagingServerLoggingWrapper;
import org.rapla.components.rpc.RPCCallEvent;
import org.rapla.components.rpc.RemoteService;

/**
 * @author windeln
 *  
 */
public class PrologInterfaceServer extends MessagingServerLoggingWrapper
        implements RemoteService, ConnectionListener {

    private static boolean WINDOWSPLATTFORM = System.getProperty("os.name")
            .indexOf("Windows") > -1;

    private Hashtable queries = new Hashtable();

    private PrologInterfaceServer() {
        //Query query = new
        // Query("forall(member(A,[int,ill,fpe,segv,abrt,term]),on_signal(A,_,throw))");
        //query.allSolutions();
    }

    public Hashtable[] queryAll(ClientConnection c, String text) {
        try {
            Query query;

            query = (Query) queries.get(c);

            if (query != null)
                query.close();
            //query = new jpl.Query("catch((" + text +
            // "),error(Formal,Context),fail)"
            //		+ ",flush_output");
            query = new jpl.Query(text);
            queries.put(c, query);

            Debug.debug("parsed query: " + text);
            Hashtable[] allSolutions = query.allSolutions();
            convertToStringValues(allSolutions);
            return allSolutions;
        } catch (Throwable t) {
            Debug.report(t);
            //throw new RuntimeException(t);
            throw new RuntimeException("hidden exception. see server log.");
        }

    }

    public Hashtable query(ClientConnection c, String text) {
        try {
            Query query;

            query = (Query) queries.get(c);

            if (query != null)
                query.close();
            query = new jpl.Query("catch((" + text
                    + "),A,(format(\"~a ~n\",[A]),fail))" + ",flush_output");
            //query = new jpl.Query( text );
            queries.put(c, query);

            Debug.debug("parsed query: " + text);
            return next(c);
        } catch (Throwable jpl) {
            Debug.report(jpl);
            throw new RuntimeException("hidden  exception. see server log.");
        }

    }

    public Hashtable next(ClientConnection c) {
        try {
            Query query;

            query = (Query) queries.get(c);

            if (query == null)
                return null;

            Debug.debug("\texecute query: hasMoreSolutions() ...");

            if (query.hasMoreSolutions()) {
                Debug.debug("\t... true, retrieve solution ....");
                Hashtable solution = null;

                solution = query.nextSolution();

                Debug.debug("\tsolution returned: "
                        + (solution == null ? "null" : "size: "
                                + solution.size()));

                if (solution == null)
                    return null;

                return convertToStringValues(solution);
            }

            Debug.debug("next: no more Solutions");
            query.close();
            query = null;
            return null;
        } catch (Throwable t) {
            Debug.report(t);
            //throw new RuntimeException(t);
            throw new RuntimeException("hidden  exception. see server log.");
        }
    }

    /**
     * @param solution
     * @return
     */
    private Hashtable convertToStringValues(Hashtable solution) {
        Hashtable strSolution = new Hashtable();
        for (Iterator iter = solution.keySet().iterator(); iter.hasNext();) {
            String key = iter.next().toString();
            Object obj = solution.get(key);
            String val;
            if (obj instanceof Atom)
                val = ((Atom) obj).name();
            else
                val = obj.toString();
            strSolution.put(key, val);
        }
        return strSolution;
    }

    private void convertToStringValues(Hashtable solutions[]) {
        for (int i = 0; i < solutions.length; i++) {
            solutions[i] = convertToStringValues(solutions[i]);
        }
    }

    Set clients = new HashSet();

    private static MessagingServerLoggingWrapper server;

    private static PrologInterfaceServer instance;

	private boolean shutdown;

    public Object dispatch(ClientConnection client, RPCCallEvent call)
            throws Exception {
        // add the client to list of registered services
        Debug.info(" client registered " + client.getClientName());
        Debug.info("class: " + this.getClass().toString() + ", "
                + call.toString());
        Object[] oldArgs = call.getArguments();
        Object[] args = new Object[oldArgs.length + 1];
        int j = 1;
        args[0] = client;
        for (int i = 0; i < oldArgs.length; i++)
            args[i + 1] = oldArgs[i];

        RPCCallEvent event = new RPCCallEvent(call.getRole(), call
                .getMethodName(), args);
        Object result = event.dispatchEventOn(this);
        return result;
    }

    public void connectionClosed(EventObject obj) {
        ClientConnection client = (ClientConnection) obj.getSource();
        clients.remove(client);
        Debug.info(" client unregistered " + client.getClientName());
        if(shutdown&&getClientCount()==0){
        	try {
				stop();
			} catch (IOException e) {
				Debug.report(e);
			}
        	System.exit(0);
        }
    }

    public static void main(String[] args) throws IOException {
        //ld: this line was recommended by Paul to stop the server from
        // entering interactive debugging whenever it encounters an error.
        //		they work fine, but i disabled them for now so i can actualy see the
        // exceptions in the server log.
        String executable = Prolog.get_default_init_args()[0];
        Prolog.set_default_init_args(new String[] { executable, "-f", "none",
                "-g", "set_prolog_flag(debug_on_error,false)", "-q" });

        File logFile = Util.getLogFile("org.cs3.pdt.server.log");
       
        System.out.println("The server debug output is safed in "
                + logFile.getAbsolutePath());
        BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
                new FileOutputStream(logFile));
        Debug.setOutputStream(new PrintStream(bufferedOutputStream));

        Debug.setDebugLevel(Debug.LEVEL_DEBUG);
        server = new MessagingServerLoggingWrapper();
        Logger logger = new Logger("server");
        logger.setDebug(true);
        server.enableLogging(logger);
        server.configure(25, java.lang.Integer.parseInt(args[0]), false);
        try {
            logger.debug("Server started @"
                    + DateFormat.getDateInstance(DateFormat.FULL).format(
                            new Date()));
            server.start();

        } catch (IOException e) {
            Debug.report(e);
        }
        instance = new PrologInterfaceServer();
        //manager.enableLogging(new Logger("manager"));
        //manager.getLogger().setDebug(true);
        server.setRemoteService("RemotePrologSession", instance);

    }

    public void shutdownServer(ClientConnection c, Boolean now) {
    	Debug.debug("I will die as soon as all connections have been closed.");
        shutdown=true;
        if(now.booleanValue()){
        	System.exit(-1);
        }
    }
}