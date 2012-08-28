/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.lifecycle;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.load.PrologLibraryManager;
import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceEvent;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.PrologInterfaceListener;
import org.cs3.prolog.session.AsyncPrologSession;
import org.cs3.prolog.session.AsyncPrologSessionEvent;
import org.cs3.prolog.session.DefaultAsyncPrologSessionListener;
import org.cs3.prolog.session.PrologSession;

public class PrologEventDispatcher extends DefaultAsyncPrologSessionListener implements IPrologEventDispatcher {

	private HashMap<String, Vector<PrologInterfaceListener>> listenerLists = new HashMap<String, Vector<PrologInterfaceListener>>();

	/*
	 * XXX i don't like the idea of keeping a reference to this session on the
	 * heap. This has proven a bad practice in the past. Is there any other way
	 * to solve this?
	 */
	private AsyncPrologSession session;

	Object observerTicket = new Object();

	Object eventTicket = new Object();

	private PrologInterface pif;

	protected PrologLibraryManager libraryManager;
	
	private HashSet<String> subjects = new HashSet<String>();

	public PrologEventDispatcher(PrologInterface pif,PrologLibraryManager libManager){
		this.pif = pif;
		this.libraryManager = libManager;
		//make sure that we do not hang the pif on shutdown.
		LifeCycleHook hook = new LifeCycleHook(){

			@Override
			public void onInit(PrologInterface pif, PrologSession initSession) throws PrologException, PrologInterfaceException {				
//				FileSearchPathConfigurator.configureFileSearchPath(libraryManager,initSession,new String[]{"pdt.runtime.library.pif"});
				initSession.queryOnce("use_module(library(pif_observe))");				
			}

			@Override
			public void afterInit(PrologInterface pif) throws PrologInterfaceException {
				synchronized (listenerLists) {
					Set<String> subjects = listenerLists.keySet();
					for (Iterator<String> it = subjects.iterator(); it.hasNext();) {
						String subject = it.next();
						enableSubject(subject);
					}				
				}
			}

			@Override
			public void beforeShutdown(PrologInterface pif, PrologSession session) throws PrologException, PrologInterfaceException {
				synchronized (subjects) {
					subjects.clear();
				}
				stop(session);
			}

			@Override
			public void onError(PrologInterface pif) {
				synchronized (subjects) {
					subjects.clear();
				}
				session=null;
			}

			@Override
			public void setData(Object data) {
				;
			}
			
			@Override
			public void lateInit(PrologInterface pif) {
				;
			}

		};

		pif.addLifeCycleHook(hook, null,null);
		if(pif.isUp()){
			PrologSession s =null;
			try{
				s= pif.getSession(PrologInterface.NONE);
				hook.onInit(pif,s);

			} catch (PrologInterfaceException e) {
				Debug.rethrow(e);
			}
			finally{
				if(s!=null){
					s.dispose();
				}
			}

		}
	}

	@Override
	protected void finalize() throws Throwable {
		if (session != null) {
			stop();

		}
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.IPrologEventDispatcher#addPrologInterfaceListener(java.lang.String, org.cs3.pl.prolog.PrologInterfaceListener)
	 */
	@Override
	public void addPrologInterfaceListener(String subject,
			PrologInterfaceListener l) throws PrologInterfaceException {
		synchronized (listenerLists) {
			Vector<PrologInterfaceListener> list = listenerLists.get(subject);
			if (list == null) {
				list = new Vector<PrologInterfaceListener>();
				listenerLists.put(subject, list);
				enableSubject(subject);
			}
			if (!list.contains(l)) {
				list.add(l);
			}
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.IPrologInterface#removePrologInterfaceListener(java.lang.String,
	 *      org.cs3.pl.prolog.PrologInterfaceListener)
	 */
	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.IPrologEventDispatcher#removePrologInterfaceListener(java.lang.String, org.cs3.pl.prolog.PrologInterfaceListener)
	 */
	@Override
	public void removePrologInterfaceListener(String subject,
			PrologInterfaceListener l) throws PrologInterfaceException {
		synchronized (listenerLists) {
			Vector<PrologInterfaceListener> list = listenerLists.get(subject);
			if (list == null) {
				return;
			}
			if (list.contains(l)) {
				list.remove(l);
			}
			if (list.isEmpty()) {
				disableSubject(subject);
				listenerLists.remove(subject);
			}
		}

	}

	private synchronized void enableSubject(String subject) throws PrologInterfaceException {
		synchronized (subjects) {
			if (subjects.contains(subject)) {
				Debug.warning("Aborted enableSubject: " + subject + " is already active");
				return;
			}
		}
		if (session == null) {
			session = pif.getAsyncSession(PrologInterface.NONE);
			session.addBatchListener(this);
		} else {
			abort();
		}
		PrologSession s = pif.getSession(PrologInterface.NONE);
		try {
			String query = "pif_observe('" + session.getProcessorThreadAlias() + "',"
			+ subject + ","+Util.quoteAtom(subject) +")";
			s.queryOnce(query);
			synchronized (subjects) {
				subjects.add(subject);
			}
		} finally {
			s.dispose();
		}
		// session.queryOnce(observerTicket,"thread_self(_Me),observe(_Me,"+subject+",'"+subject+"')");
		dispatch();
	}

	private void disableSubject(String subject) throws PrologInterfaceException {
		if (session == null) {
			return;
		}

		abort();
		session.queryOnce(observerTicket, "thread_self(_Me),pif_unobserve(_Me,"
				+ subject + ")");
		if (!listenerLists.isEmpty()) {
			dispatch();
		}
		synchronized (subjects) {
			subjects.remove(subject);
		}
	}

	private void dispatch() throws PrologInterfaceException {
		session.queryAll(eventTicket, "pif_dispatch(Subject,Key,Event)");
	}

	private void abort() throws PrologInterfaceException {
		PrologSession s = pif.getSession(PrologInterface.NONE);
		try {
			abort(s);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

	private void abort(PrologSession s) throws PrologException, PrologInterfaceException {
		s.queryOnce("thread_send_message('"
				+ session.getProcessorThreadAlias()
				+ "',notify('$abort',_))");
	}


	public void stop() throws PrologInterfaceException {
		if (session == null) {
			return;
		}
		abort();
		session.dispose();
		session = null;
	}

	public void stop(PrologSession s) throws PrologException, PrologInterfaceException {
		if (session == null) {
			return;
		}
		abort(s);
		session.dispose();
		session = null;
	}

	/**
	 * @param subject2
	 * @param string
	 */
	private void fireUpdate(String subject, String key, String event) {
		Vector<PrologInterfaceListener> listeners = listenerLists.get(key);
		if (listeners == null) {
			return;
		}
		PrologInterfaceEvent e = new PrologInterfaceEvent(this, subject, event);
		Vector<PrologInterfaceListener> cloned = getAListenersClone(listeners);
		for (Iterator<PrologInterfaceListener> it = cloned.iterator(); it.hasNext();) {
			PrologInterfaceListener l = it.next();
			l.update(e);
		}
	}

	@SuppressWarnings("unchecked")
	private Vector<PrologInterfaceListener> getAListenersClone(
			Vector<PrologInterfaceListener> listeners) {
		Vector<PrologInterfaceListener> cloned = null;
		synchronized (listeners) {
			cloned = (Vector<PrologInterfaceListener>) listeners.clone();
		}
		return cloned;
	}


	@Override
	public void goalHasSolution(AsyncPrologSessionEvent e) {
		String subject = (String) e.getBindings().get("Subject");
		if ("$abort".equals(subject)) {
			return;
		}
		String key = (String) e.getBindings().get("Key");
		String event = (String) e.getBindings().get("Event");
		fireUpdate(subject, key, event);
	}
	
	public Set<String> getSubjects(){
		Set<String> res = new HashSet<String>();
		synchronized (subjects) {
			res.addAll(subjects);
		}
		return res;
	}

}


