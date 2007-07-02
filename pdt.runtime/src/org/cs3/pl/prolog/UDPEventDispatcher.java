package org.cs3.pl.prolog;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;

public class UDPEventDispatcher implements IPrologEventDispatcher {

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
		String subject = PLUtil.renderTerm(term.getArgument(0));
		String event = PLUtil.renderTerm(term.getArgument(1));
		fireUpdate(subject, event);
	}
	
	private void fireUpdate(String subject,  String event) {
		Vector listeners = (Vector) listenerLists.get(subject);
		if (listeners == null) {
			return;
		}
		PrologInterfaceEvent e = new PrologInterfaceEvent(this, subject, subject,
				event);

		Vector cloned = null;
		synchronized (listeners) {
			cloned = (Vector) listeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
			PrologInterfaceListener l = (PrologInterfaceListener) it.next();
			l.update(e);
		}
	}
	
	private HashMap<String, Vector<PrologInterfaceListener>> listenerLists = new HashMap<String, Vector<PrologInterfaceListener>>();

	private PrologInterface2 pif;

	private _Dispatcher dispatcher;

	public UDPEventDispatcher(PrologInterface2 pif) {
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
				Set subjects = listenerLists.keySet();
				for (Iterator it = subjects.iterator(); it.hasNext();) {
					String subject = (String) it.next();
					enableSubject(subject);
				}

			}

			public void beforeShutdown(PrologInterface pif,
					PrologSession session) throws PrologException,
					PrologInterfaceException {

			}

			public void onError(PrologInterface pif) {

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
	
	public void addPrologInterfaceListener(String subject,
			PrologInterfaceListener l) throws PrologInterfaceException {
		String csubject = canonical(subject);
		synchronized (listenerLists) {
			Vector<PrologInterfaceListener> list = listenerLists.get(csubject);
			if (list == null) {
				list = new Vector<PrologInterfaceListener>();
				listenerLists.put(csubject, list);
				enableSubject(csubject);
			}
			if (!list.contains(l)) {
				list.add(l);
			}
		}

	}

	public void removePrologInterfaceListener(String subject,
			PrologInterfaceListener l) throws PrologInterfaceException {
		String csubject = canonical(subject);
		synchronized (listenerLists) {
			Vector list = listenerLists.get(csubject);
			if (list == null) {
				return;
			}
			if (list.contains(l)) {
				list.remove(l);
			}
			if (list.isEmpty()) {				
				listenerLists.remove(csubject);
				disableSubject(csubject);
			}
		}

	}

	private void disableSubject(String subject) {
		PrologSession s = null;

		if (this.dispatcher == null) {
			return;
		}
		try {
			s = pif.getSession();
			String query = "pif_unsubscribe(localhost:" + dispatcher.getPort()
					+ "," + Util.quoteAtom(subject) + ")";
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

	private void enableSubject(String subject) {
		PrologSession s = null;

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
					+ "," + subject + ")";
			s.queryOnce(query);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

}
