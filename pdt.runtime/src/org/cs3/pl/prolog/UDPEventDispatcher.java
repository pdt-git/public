package org.cs3.pl.prolog;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;

public class UDPEventDispatcher implements IPrologEventDispatcher{

	private final class _Dispatcher extends Thread {
		
		private boolean shouldBeRunning;
		private DatagramSocket socket;

		private DatagramPacket packet;
		private int port;

		private _Dispatcher(int port) {
			
			super("UDP Event Dispatchher" + port);
			
			this.port=port;
			try {
				socket = new DatagramSocket(port);
				byte[] buf = new byte[255];
				packet = new DatagramPacket(buf, 255);
				socket.setSoTimeout(1000);
				if(socket.getLocalPort()==-1){
					Debug.debug("debug");
				}
			} catch (SocketException e) {
				Debug.rethrow(e);
			}
		}

		public void run() {

			try {

				while (shouldBeRunning) {
					try {
						socket.receive(packet);
						dispatch(packet);
					} catch (SocketTimeoutException e) {
						; // this is anticipated.
					}
					
				}
				socket.close();				
			} catch (IOException e) {
				Debug.rethrow(e);
			}

		}

		

		public int getPort() {
			
			return port;
		}
	}
	private void dispatch(DatagramPacket p) {
		String data = new String(p.getData(),p.getOffset(),p.getLength());
		CCompound term = (CCompound) PLUtil.createCTerm(data);
		String ticket = PLUtil.renderTerm(term.getArgument(0));
		String subject = PLUtil.renderTerm(term.getArgument(1));
		String event = PLUtil.renderTerm(term.getArgument(2));
		if(event.equals("invalid")){
			Debug.debug("debug");
		}
		fireUpdate(ticket,subject, event);
	}
	
	private void fireUpdate(String ticket,  String subject, String event) {
		Vector<PrologInterfaceListener> listeners = listenerLists.get(ticket);
		if (listeners == null) {
			return;
		}
		PrologInterfaceEvent e = new PrologInterfaceEvent(this, subject, ticket,
				event);
		

		Vector<PrologInterfaceListener> cloned = null;
		synchronized (listeners) {
			cloned = (Vector<PrologInterfaceListener>) listeners.clone();
		}
		for (Iterator<PrologInterfaceListener> it = cloned.iterator(); it.hasNext();) {
			PrologInterfaceListener l = it.next();
			try{
				l.update(e);
			}catch(Throwable t){
				Debug.report(t);
			}
		}
	}

	/** maps tickets to lists of listeners. needed for dispatching */
	private HashMap<String, Vector<PrologInterfaceListener>> listenerLists = new HashMap<String, Vector<PrologInterfaceListener>>();
 
	/** maps subjects to tickets. needed for subscribing/unsubscribing */
	private HashMap<String,String> tickets = new HashMap<String, String>();
	
	private PrologInterface pif;

	private _Dispatcher dispatcher;

	public UDPEventDispatcher(PrologInterface pif) {
		this.pif = pif;
		// make sure that we do not hang the pif on shutdown.
		LifeCycleHook hook = new LifeCycleHook2() {

			public void onInit(PrologInterface pif, PrologSession initSession)
					throws PrologException, PrologInterfaceException {

				PLUtil.configureFileSearchPath(pif.getFactory()
						.getLibraryManager(), initSession,
						new String[] { "pdt.runtime.library.pif" });
				initSession.queryOnce("use_module(library(pif_observe2))");

			}

				public void afterInit(PrologInterface pif)
						throws PrologInterfaceException {
					Set<String> subjects = new HashSet<String>(tickets.keySet());
					for (Iterator<String> it = subjects.iterator(); it.hasNext();) {
						String subject = it.next();
						String ticket = tickets.remove(subject);
						Vector listeners = listenerLists.remove(ticket);
						ticket = enableSubject(subject);
						tickets.put(subject, ticket);
						listenerLists.put(ticket, listeners);
					}
	
				}

			public void beforeShutdown(PrologInterface pif,
					PrologSession session) throws PrologException,
					PrologInterfaceException {

			}

			public void onError(PrologInterface pif) {

			}

			
			public void setData(Object data) {
				// TODO Auto-generated method stub
				
			}

		};
		pif.addLifeCycleHook(hook, null, null);
		if (pif.isUp()) {
			PrologSession s = null;
			try {
				s = pif.getSession();
				hook.onInit(pif, s);

			} catch (PrologInterfaceException e) {
				Debug.rethrow(e);
			} finally {
				if (s != null) {
					s.dispose();
				}
			}

		}
	}

	static String canonical(String in){
		return PLUtil.renderTerm(PLUtil.createCTerm(in));
	}
	
	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.PrologInterfaceEventDispatcher#addPrologInterfaceListener(java.lang.String, org.cs3.pl.prolog.PrologInterfaceListener)
	 */
	public void addPrologInterfaceListener(String subject,
			PrologInterfaceListener l) throws PrologInterfaceException {
		String csubject = canonical(subject);
		synchronized (listenerLists) {
			String ticket = enableSubject(csubject);
			Vector<PrologInterfaceListener> list = listenerLists.get(ticket);
			if (list == null) {
				list = new Vector<PrologInterfaceListener>();
				listenerLists.put(ticket, list);
				
			}
			if (!list.contains(l)) {
				list.add(l);
			}
		}

	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.PrologInterfaceEventDispatcher#removePrologInterfaceListener(java.lang.String, org.cs3.pl.prolog.PrologInterfaceListener)
	 */
	public void removePrologInterfaceListener(String subject,
			PrologInterfaceListener l) throws PrologInterfaceException {
		String csubject = canonical(subject);
		synchronized (listenerLists) {
			String ticket = tickets.get(csubject);
			Vector list = listenerLists.get(ticket);
			if (list == null) {
				return;
			}
			if (list.contains(l)) {
				list.remove(l);
			}
			if (list.isEmpty()) {				
				listenerLists.remove(ticket);
				disableSubject(csubject);
			}
		}

	}

	private void disableSubject(String subject) {
		PrologSession s = null;
		
		String ticket = tickets.remove(subject);
		if(ticket==null){
			return;
		}
		
		if (this.dispatcher == null) {
			return;
		}
		try {
			s = pif.getSession();
			String query = "pif_unsubscribe("+ticket+ ")";
			s.queryOnce(query);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
		if(listenerLists.isEmpty()){
			this.dispatcher.shouldBeRunning=false;
			try {
				this.dispatcher.join();
				this.dispatcher=null;
			} catch (InterruptedException e) {
				Debug.rethrow(e);
			}
		}
				

	}

	private String enableSubject(String subject) {
		PrologSession s = null;
		String ticket = tickets.get(subject);
		if(ticket!=null){
			return ticket;
		}
		
		try {
			if (this.dispatcher == null) {
				int port = Util.findFreePort();
				if(port==-1){
					Debug.debug("debug");
				}
				this.dispatcher = new _Dispatcher(port);
				
				
				this.dispatcher.shouldBeRunning=true;
				this.dispatcher.start();
			}
		} catch (IOException e) {
			Debug.rethrow(e);
		}
		try {
			s = pif.getSession();
			int port = dispatcher.getPort();
			if(port==-1){
				Debug.debug("debug");
			}
			String query = "pif_subscribe(localhost:" + port
					+ "," + subject + ",Ticket)";
			ticket= (String) s.queryOnce(query).get("Ticket");
			tickets.put(subject, ticket);
			
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
		return ticket;

	}

}
