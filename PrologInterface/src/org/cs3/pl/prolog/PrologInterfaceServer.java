/*
 * Created on 21.01.2004
 *
 */
package org.cs3.pl.prolog;

import java.io.IOException;
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

import org.cs3.pl.common.Debug;
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

	public Hashtable query(ClientConnection c, String text) {
		try {
			Query query;

			query = (Query) queries.get(c);

			if (query != null)
				query.close();
			query = new jpl.Query("catch((" + text + "),MyException,fail)"
					+ ",flush_output");

			queries.put(c, query);

			Debug.debug("parsed query: " + text);
			return next(c);
		} catch (JPLException jpl) {
			Debug.report(jpl);
		}

		return null;
	}

	public Hashtable next(ClientConnection c) {
		Query query;

		query = (Query) queries.get(c);

		if (query == null)
			return null;

		Debug.debug("\texecute query: hasMoreSolutions() ...");

		if (query.hasMoreSolutions()) {
			Debug.debug("\t... true, retrieve solution ....");
			Hashtable solution = null;
			try {
				solution = query.nextSolution();
			} catch (Throwable t) {
				Debug.report(t);
				throw new RuntimeException(t);
			}

			Debug.debug("\tsolution returned: "
					+ (solution == null ? "null" : "size: " + solution.size()));

			if (solution == null)
				return null;

			return convertToStringValues(solution);
		}

		Debug.debug("next: no more Solutions");
		query.close();
		query = null;
		return null;
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

	Set clients = new HashSet();

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
	}

	public static void main(String[] args) {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		MessagingServerLoggingWrapper server = new MessagingServerLoggingWrapper();
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
		PrologInterfaceServer manager = new PrologInterfaceServer();
		//manager.enableLogging(new Logger("manager"));
		//manager.getLogger().setDebug(true);
		server.setRemoteService("RemotePrologSession", manager);

	}

	public void shutdownServer(ClientConnection c) {
		Debug.debug("Sepuko requested. AAAaaaAAAaAAaa!.");
		System.exit(0);
	}
}