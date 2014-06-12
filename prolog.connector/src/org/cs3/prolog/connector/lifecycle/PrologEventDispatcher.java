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

package org.cs3.prolog.connector.lifecycle;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.common.logging.Debug;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologEvent;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.process.PrologEventListener;
import org.cs3.prolog.connector.session.AsyncPrologSession;
import org.cs3.prolog.connector.session.AsyncPrologSessionEvent;
import org.cs3.prolog.connector.session.DefaultAsyncPrologSessionListener;
import org.cs3.prolog.connector.session.PrologSession;

public class PrologEventDispatcher extends DefaultAsyncPrologSessionListener implements IPrologEventDispatcher {

	private HashMap<String, Vector<PrologEventListener>> listenerLists = new HashMap<String, Vector<PrologEventListener>>();

	/*
	 * XXX i don't like the idea of keeping a reference to this session on the
	 * heap. This has proven a bad practice in the past. Is there any other way
	 * to solve this?
	 */
	private AsyncPrologSession session;

	Object observerTicket = new Object();

	Object eventTicket = new Object();

	private PrologProcess process;
	
	private HashSet<String> subjects = new HashSet<String>();

	public PrologEventDispatcher(PrologProcess process) {
		this.process = process;
		//make sure that we do not hang the process on shutdown.
		LifeCycleHook hook = new LifeCycleHook(){

			@Override
			public void onInit(PrologProcess process, PrologSession initSession) throws PrologException, PrologProcessException {				
				try {
					String query = QueryUtils.bT("use_module", QueryUtils.prologFileNameQuoted(getObserveFile()));
					initSession.queryOnce(query);
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			}

			@Override
			public void afterInit(PrologProcess process) throws PrologProcessException {
				synchronized (listenerLists) {
					Set<String> subjects = listenerLists.keySet();
					for (Iterator<String> it = subjects.iterator(); it.hasNext();) {
						String subject = it.next();
						enableSubject(subject);
					}				
				}
			}

			@Override
			public void beforeShutdown(PrologProcess process, PrologSession session) throws PrologException, PrologProcessException {
				synchronized (subjects) {
					subjects.clear();
				}
				stop(session);
			}

			@Override
			public void onError(PrologProcess process) {
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
			public void lateInit(PrologProcess process) {
				;
			}

		};

		process.addLifeCycleHook(hook, null,null);
		if(process.isUp()){
			PrologSession s =null;
			try{
				s= process.getSession(PrologProcess.NONE);
				hook.onInit(process,s);

			} catch (PrologProcessException e) {
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
	public void addPrologEventListener(String subject,
			PrologEventListener l) throws PrologProcessException {
		synchronized (listenerLists) {
			Vector<PrologEventListener> list = listenerLists.get(subject);
			if (list == null) {
				list = new Vector<PrologEventListener>();
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
	public void removePrologEventListener(String subject,
			PrologEventListener l) throws PrologProcessException {
		synchronized (listenerLists) {
			Vector<PrologEventListener> list = listenerLists.get(subject);
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

	private synchronized void enableSubject(String subject) throws PrologProcessException {
		synchronized (subjects) {
			if (subjects.contains(subject)) {
				Debug.info("Aborted enableSubject: " + subject + " is already active");
				return;
			}
		}
		if (session == null) {
			session = process.getAsyncSession(PrologProcess.NONE);
			session.addBatchListener(this);
		} else {
			abort();
		}
		PrologSession s = process.getSession(PrologProcess.NONE);
		try {
			String query = "process_observe('" + session.getProcessorThreadAlias() + "',"
			+ subject + ","+QueryUtils.quoteAtom(subject) +")";
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

	private void disableSubject(String subject) throws PrologProcessException {
		if (session == null) {
			return;
		}

		abort();
		session.queryOnce(observerTicket, "thread_self(_Me),process_unobserve(_Me,"
				+ subject + ")");
		if (!listenerLists.isEmpty()) {
			dispatch();
		}
		synchronized (subjects) {
			subjects.remove(subject);
		}
	}

	private void dispatch() throws PrologProcessException {
		session.queryAll(eventTicket, "process_dispatch(Subject,Key,Event)");
	}

	private void abort() throws PrologProcessException {
		PrologSession s = process.getSession(PrologProcess.NONE);
		try {
			abort(s);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

	private void abort(PrologSession s) throws PrologException, PrologProcessException {
		s.queryOnce("thread_send_message('"
				+ session.getProcessorThreadAlias()
				+ "',notify('$abort',_))");
	}


	public void stop() throws PrologProcessException {
		if (session == null) {
			return;
		}
		abort();
		session.dispose();
		session = null;
	}

	public void stop(PrologSession s) throws PrologException, PrologProcessException {
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
		Vector<PrologEventListener> listeners = listenerLists.get(key);
		if (listeners == null) {
			return;
		}
		PrologEvent e = new PrologEvent(this, subject, event);
		Vector<PrologEventListener> cloned = getAListenersClone(listeners);
		for (Iterator<PrologEventListener> it = cloned.iterator(); it.hasNext();) {
			PrologEventListener l = it.next();
			l.update(e);
		}
	}

	@SuppressWarnings("unchecked")
	private Vector<PrologEventListener> getAListenersClone(
			Vector<PrologEventListener> listeners) {
		Vector<PrologEventListener> cloned = null;
		synchronized (listeners) {
			cloned = (Vector<PrologEventListener>) listeners.clone();
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

	private static File observeFile = null;

	public static File getObserveFile() throws IOException {
		if (observeFile == null) {
			String tempDir = System.getProperty("java.io.tmpdir");
			InputStream resourceAsStream;
			resourceAsStream = PrologEventDispatcher.class.getResourceAsStream("process_observe.pl");
			if (resourceAsStream == null) {
				throw new RuntimeException("Cannot find process_observe.pl!");
			}
			observeFile = new File(tempDir, "process_observe.pl");
			if (observeFile.exists()) {
				observeFile.delete();
			}
			Util.copy(resourceAsStream, new FileOutputStream(observeFile));
		}
		return observeFile;
	}

}


