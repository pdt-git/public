/*
 * Created on 21.01.2004
 *
 */
package org.cs3.pl.prolog;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import java.text.DateFormat;

import java.util.Date;
import java.util.Enumeration;
import java.util.EventObject;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;

import jpl.Atom;
import jpl.Compound;
import jpl.JPL;
import jpl.Query;
import jpl.Term;

import org.cs3.pl.Debug;
import org.cs3.pl.SystemProperties;

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
public class PrologInterfaceServer extends MessagingServerLoggingWrapper implements RemoteService, ConnectionListener, IPrologServer{
	
	private static boolean WINDOWSPLATTFORM;
	
	private SystemProperties systemProperties;
	
	private Hashtable predicates = new Hashtable();
	
	private jpl.Query query = null;

	private Hashtable queries = new Hashtable();
	
	private PrologInterfaceServer() {
		systemProperties = new SystemProperties();
		WINDOWSPLATTFORM = systemProperties.isWindowsPlattform();
	}
	
	
	public void init() {
		JPL.init();
		JPL.setDTMMode(false);
	}
	
	
	public Boolean consultLibrary(ClientConnection c,String name) {
		return consultLibrary(name);
	}
	
	/**
	 * @param string
	 */
	public Boolean consultLibrary(String module) {
		getLogger().log("consultLibrary");
		Term arg[] = {new Atom( makeFilenameSWIConform(module))};
		Term library[] = { new Compound( "library" , arg )};
		
		Query consult_query = 
			new Query( "consult",  library );
		
		return new Boolean(consult_query.hasSolution());
	}
	
	
	public static void copyFile(String oldname, String newname) {
		String newLine = System.getProperty("line.separator");
		FileWriter fw = null;
		FileReader fr = null;
		BufferedReader br = null;
		BufferedWriter bw = null;
		File source = null;
		
		try {
			fr = new FileReader(oldname);
			fw = new FileWriter(newname);
			br = new BufferedReader(fr);
			bw = new BufferedWriter(fw);
			
			/* Determine the size of the buffer to allocate */
			source = new File(oldname);
			
			int fileLength = (int) source.length();
			
			char charBuff[] = new char[fileLength];
			
			while (br.read(charBuff, 0, fileLength) != -1)
				bw.write(charBuff, 0, fileLength);
		} catch (FileNotFoundException fnfe) {
			Debug.error(oldname + " does not exist!");
		} catch (IOException ioe) {
			Debug.error("Error reading/writing files!");
		} finally {
			try {
				if (br != null)
					br.close();
				
				if (bw != null)
					bw.close();
			} catch (IOException ioe) {
			}
		}
	}
	
	
	/**
	 * 
	 */
	public void restart() {
		halt();
		init();
	}
	
	/**
	 * 
	 */
	public void halt() {
		JPL.halt();
	}

	public void consult(ClientConnection c,String filename) {
		getLogger().debug("consultcc");
		consult(filename);
	}
	public void consult(String filename) { //TODO: broken?
		Term arg[] = { new Atom( makeFilenameSWIConform(filename) ) };
//		Compound swi = new Compound( new Atom( "swi" ), arg );
		
		Query consult_query = 
			new Query( "consult",  arg );
		
		consult_query.hasSolution();
	}
		
	
//	public PrologElementData[] retrievePrologElements(String file) {
//		throw new RuntimeException("This point should never been reached");
//	}	
//	public PrologElementData[] retrievePrologElements(ClientConnection c,String file) {
//		
//		Hashtable result = query(c,"bagof([Pos_,Len_],"+PrologClient.METADATA+"('"+file+"',Module,Name,Arity,Public,Pos_,Len_, Dyn,Mul),[[Pos,Len]|_])");
//		List list = new ArrayList();
//		while(result != null){
//			//debug(result.get("Name").toString()+" - PUBLIC- "+Boolean.valueOf(result.get("Public").toString()).booleanValue());
//			PrologElementData data = new PrologElementData(
//					result.get("Name").toString(), 
//					java.lang.Integer.parseInt(result.get("Arity").toString()),
//					Boolean.valueOf(result.get("Public").toString()).booleanValue(),
//					java.lang.Integer.parseInt(result.get("Pos").toString()),
//					java.lang.Integer.parseInt(result.get("Len").toString()),
//					result.get("Dyn").toString().equals("1"),
//					result.get("Mul").toString().equals("1")
//					);
//			list.add(data);
//			result = next(c);
//		}
//		return (PrologElementData[]) list.toArray(new PrologElementData[0]); 
//	}
	
	/**
	 * @param file
	 * @return
	 */
	private String makeFilenameSWIConform(String file) {
		if(systemProperties.isWindowsPlattform()){
			return file.toLowerCase().replace('\\', '/');	
		}
		return file;
		
	}
	
	
	public Hashtable query( String text) {
		throw new RuntimeException("This point should never been reached");
	}	
	/**
	 * @param text
	 */
	public Hashtable query(ClientConnection c, String text) {
		Query query = (Query)queries.get(c);
		if (query != null)
			query.close();
		query = new jpl.Query(text+",flush_output");
		queries.put(c,query);
		getLogger().debug("parsed query: " + text);
		return next(c);
	}
	
	
	
	public boolean assertFact(ClientConnection client,String text) {
		return assertFact(text);
	}
	
	public boolean assertFact(String text) {
/*		if (query != null)
			query.close();
*/		Query query = new jpl.Query(text);
		query = new jpl.Query("assert",new Term[] {query.goal()});
		try {
			boolean ret = query.hasSolution();			
			getLogger().debug("asserted: " + ret);
			return ret;
		} catch(Exception ex){
			return false;
		}
	}
	
	public Hashtable next() {
		throw new RuntimeException("This point should never been reached");
	}
	public Hashtable next(ClientConnection c) {
		Query query = (Query)queries.get(c);

		if (query == null)
			return null;
		getLogger().debug("\texecute query: hasMoreSolutions() ...");
		if(query.hasMoreSolutions()){
			getLogger().debug("\t... true, retrieve solution ....");
			Hashtable solution = query.nextSolution();
			getLogger().debug("\tsolution returned: " + (solution == null ? "null" : "size: "+solution.size()));
			if (solution == null) return null;
			if (getLogger().isDebugEnabled()) {
				Enumeration keys =solution.keys();
//				for (;keys.hasMoreElements();) {
//					String varname = (String) keys.nextElement();
//					String value = solution.get(varname).toString();
//					getLogger().debug("\n " +varname + " = " + value + " " );
//				}
			}
			return convertToStringValues(solution);
			//return solution;
		}
		getLogger().debug("next: no more Solutions");
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
				val = ((Atom)obj).name();
			else
				val = obj.toString();
			strSolution.put(key, val);
		}
		return strSolution;
	}


	public void rewind() {
		if (query != null) 
			query.rewind();
	}
	
	Set clients = new HashSet();
	
	public Object dispatch(ClientConnection client,RPCCallEvent call) throws Exception {
		// add the client to list of registered services
		getLogger().log(" client registered " + client.getClientName());
		getLogger().log("class: " +this.getClass().toString() + ", " +call.toString());
		Object[] oldArgs = call.getArguments();
		Object[] args = new Object[oldArgs.length +1];
		int j = 1;
		args[0] = client;
		for (int i = 0; i < oldArgs.length; i++) 
			args[i+1] = oldArgs[i];
		
		RPCCallEvent event = new RPCCallEvent(call.getRole(),call.getMethodName(),args);
		Object result = event.dispatchEventOn(this);
		return result;
	}
	
	public void connectionClosed(EventObject obj) {
		ClientConnection client = (ClientConnection) obj.getSource();
		clients.remove(client);
		Debug.info(" client unregistered " + client.getClientName());
	}
	public static void main(String[] args) {
		MessagingServerLoggingWrapper server = new MessagingServerLoggingWrapper();
		Logger logger =new Logger("server");
		logger.setDebug(true);
		server.enableLogging(logger);
		server.configure(25,java.lang.Integer.parseInt(args[0]),false);
		try {
			logger.debug("Server started @" + DateFormat.getDateInstance(DateFormat.FULL).format(new Date()));
			server.start();
			
		} catch (IOException e) {
			Debug.report(e);
		}
		PrologInterfaceServer manager =new PrologInterfaceServer();
		manager.enableLogging(new Logger("manager"));
		manager.getLogger().setDebug(true);
		server.setRemoteService("PrologSession", manager);
	}
		
	
	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.IPrologManager#getPrologWriter()
	 */
	public BufferedWriter getPrologWriter() {
		// TODO Auto-generated method stub
		return null;
	}
}
