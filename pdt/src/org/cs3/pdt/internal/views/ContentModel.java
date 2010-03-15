package org.cs3.pdt.internal.views;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pdt.runtime.ui.PrologRuntimeUI;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.AsyncPrologSessionProxy;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.FileSearchPathConfigurator;
import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.part.FileEditorInput;

public abstract class ContentModel extends DefaultAsyncPrologSessionListener
		implements PrologFileContentModel, LifeCycleHook,
		PrologInterfaceListener {

	private static final String HOOK_ID = "PrologFileContentModelHook";
	protected Object hookData;
	private Object input;
	private PrologInterface pif;
	private HashMap<Object, Vector<Object>> cache = new HashMap<Object, Vector<Object>>();
	private String oneMomentPlease = "one moment please...";
	private Vector<PrologFileContentModelListener> listeners = new Vector<PrologFileContentModelListener>();
	private HashMap<Object, Vector<PrologFileContentModelListener>> specificListeners = new HashMap<Object, Vector<PrologFileContentModelListener>>();
	private long timestamp = Long.MIN_VALUE;
	private IPrologEventDispatcher dispatcher;
	private String subject;
	private AsyncPrologSession session;

	public void setData(Object data) {
		this.hookData=data;
	}

	public void update(PrologInterfaceEvent e) {
		String subject = e.getSubject();
		String event = e.getEvent();

		Debug.debug("ContentModel received event: subject=" + subject
				+ ", event=" + event);
		if ("invalid".equals(event)) {
			try {
				reset();
			} catch (PrologInterfaceException e1) {
				Debug.rethrow(e1);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#hasChildren(java.lang.Object)
	 */
	public boolean hasChildren(Object parentElement) {
		if (parentElement instanceof PEFNode) {
			String type = ((PEFNode) parentElement).getType();
			return "pef_file".equals(type) || "pef_predicate".equals(type);
		}
		return parentElement == input;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#getChildren(java.lang.Object)
	 */
	public Object[] getChildren(Object parentElement)
			throws PrologInterfaceException {
		Debug.debug("outline getChildren for " + parentElement);
		Vector<Object> children = null;
		synchronized (cache) {
			children = getCachedChildren(parentElement);
			if (children.isEmpty()) {
				Debug.debug("outline no children cached for " + parentElement);
				if (pif != null && pif.isUp()) {
					Debug.debug("outline adding oneMomentPlease to "
							+ parentElement);
					children.add(oneMomentPlease);
					fetchChildren(parentElement);
				}
			} else {
				Debug.debug("outline found cached children "
						+ Util.prettyPrint(children));
			}
		}
		return children.toArray();
	}

	private void fetchChildren(Object parentElement)
			throws PrologInterfaceException {

		if (parentElement instanceof PEFNode) {
			fetchChildren((PEFNode) parentElement);

		} else if (parentElement == input) {
			fetchChildren(getFile());
		}
	}

	private void fetchChildren(File file) throws PrologInterfaceException {
		synchronized (sessionLock) {
			final AsyncPrologSession session = getSession();
			String query = "pdt_outline(" +
					"'" +Util.prologFileName(file)+"'," +
							"ChildT," +
							"Child," +
							"Label," +
							"Tags," +
							"Start," +
							"End)";
			session.queryAll(input, query);
		}
	}

	private Object sessionLock = new Object();

	private void fetchChildren(PEFNode parent) throws PrologInterfaceException {
		synchronized (sessionLock) {
			AsyncPrologSession session = getSession();
			String query = "pdt_outline(" +
			"'" +Util.prologFileName(getFile())+"'," +
					parent.getType()+"," +
					parent.getId()+"," +
					"ChildT," +
					"Child," +
					"Label," +
					"Tags," +
					"Start," +
					"End)";
	
			session.queryAll(parent, query);
		}
	}

	private class _PEFNode implements PEFNode, IAdaptable {

		private int endPosition;
		private File file;
		private String id;
		private String label;
		private int startPosition;
		private Set<String> tags;
		private String type;

		public _PEFNode(AsyncPrologSessionEvent event, File file) {
			Map<String,Object>bindings = event.getBindings();
			type = (String) bindings.get("ChildT");
			id = (String) bindings.get("Child");
			startPosition = Integer.parseInt((String) bindings.get("Start"));
			endPosition = Integer.parseInt((String) bindings.get("End"));
			this.file = file;
			label = Util.unquoteAtom((String) bindings.get("Label"));
			tags = new HashSet<String>();
			tags.addAll((Collection<String>) bindings.get("Tags"));
		}

		public Object getAdapter(Class adapter) {
			return Platform.getAdapterManager().getAdapter(this, adapter);
		}

		@Override
		public String toString() {
			return label;
		}

		public int getEndPosition() {
			return endPosition;
		}

		public File getFile() {
			return this.file;
		}

		public String getId() {

			return this.id;
		}

		public String getLabel() {
			return this.label;
		}

		public int getStartPosition() {
			return this.startPosition;
		}

		public Set<String> getTags() {
			return this.tags;
		}

		public String getType() {
			return this.type;
		}

		public PrologInterface getPrologInterface() {
			return getPif();
		}

	}

	
	
	public void goalHasSolution(AsyncPrologSessionEvent e) {
		Object parent = e.ticket;
		PEFNode p = new _PEFNode(e, getFile());
		addChild(parent, p);
	}

	private AsyncPrologSession getSession() throws PrologInterfaceException {
		synchronized (sessionLock) {
			if (session == null && pif != null) {
				//session = ((PrologInterface2) pif).getAsyncSession();
				session=new AsyncPrologSessionProxy((PrologInterface) pif,PrologInterface.PROCESS_LISTS);
				// session.setPreferenceValue("socketsession.canonical",
				// "true");
				session.addBatchListener(this);

			}
			return session;
		}

	}

	private void disposeSession() throws PrologInterfaceException {
		synchronized (sessionLock) {
			if (session == null) {
				return;
			}
			session.removeBatchListener(this);
			session.join();
			session.dispose();
			session = null;
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#setPif(org.cs3.pl.prolog.PrologInterface)
	 */
	public void setPif(PrologInterface pif, IPrologEventDispatcher d)
			throws PrologInterfaceException {

		disposeSession();
		
		if (this.dispatcher != null && this.subject != null) {
			this.dispatcher.removePrologInterfaceListener(this.subject, this);
		}
		if (this.pif != null) {
			((PrologInterface) this.pif).removeLifeCycleHook(this, HOOK_ID);
		}
		this.pif = pif;
		this.dispatcher = d;
		if (this.pif != null) {
			this.pif.addLifeCycleHook(this, HOOK_ID, new String[0]);
			if (pif.isUp()) {
				afterInit(pif);
			}
		}
		if (this.dispatcher != null && this.subject != null) {
			this.dispatcher.addPrologInterfaceListener(this.subject, this);
		}
		reset();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#getPif()
	 */
	public PrologInterface getPif() {
		return pif;
	}

	public Object getInput() {

		return input;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#reset()
	 */
	public void reset() throws PrologInterfaceException {
		synchronized (this) {
			timestamp = System.currentTimeMillis();
		}
		disposeSession();
		synchronized (cache) {
			cache.clear();

		}

		fireContentModelChanged();
	}

	private void addChildren(Object parent, Collection<Object> collection) {
		Vector<Object> children = null;
		Object[] removed = null;
		synchronized (cache) {
			children = getCachedChildren(parent);
			if (children.contains(oneMomentPlease)) {
				removed = children.toArray();
				children.clear();

			}
			children.addAll(collection);
		}
		fireChildrenAdded(parent, collection.toArray());
		if (removed != null) {
			fireChildrenRemoved(parent, removed);
		}

	}

	private void addChild(Object parent, Object child) {
		Vector<Object> children = new Vector<Object>();
		children.add(child);
		addChildren(parent, children);
	}

	private Vector<Object> getCachedChildren(Object parent) {
		synchronized (cache) {
			Vector<Object> children = cache.get(parent);
			if (children == null) {
				children = new Vector<Object>();

				cache.put(parent, children);
			}
			return children;
		}

	}

	private void fireChildrenAdded(Object parent, Object[] children) {
		PrologFileContentModelEvent e = new PrologFileContentModelEvent(this,
				parent, children, getLastResetTime());
		HashSet<PrologFileContentModelListener> clone = new HashSet<PrologFileContentModelListener>();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		synchronized (specificListeners) {
			clone.addAll(getListenersForParent(parent));
		}
		for (Iterator<PrologFileContentModelListener> iter = clone.iterator(); iter.hasNext();) {
			PrologFileContentModelListener l = iter
					.next();
			l.childrenAdded(e);
		}

	}

	private void fireChildrenRemoved(Object parent, Object[] children) {
		PrologFileContentModelEvent e = new PrologFileContentModelEvent(this,
				parent, children, getLastResetTime());
		HashSet<PrologFileContentModelListener> clone = new HashSet<PrologFileContentModelListener>();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		synchronized (specificListeners) {
			clone.addAll(getListenersForParent(parent));
		}
		for (Iterator<PrologFileContentModelListener> iter = clone.iterator(); iter.hasNext();) {
			PrologFileContentModelListener l = iter
					.next();
			l.childrenRemoved(e);
		}

	}

	private void fireContentModelChanged() {

		PrologFileContentModelEvent e = new PrologFileContentModelEvent(this,
				getLastResetTime());
		HashSet<PrologFileContentModelListener> clone = new HashSet<PrologFileContentModelListener>();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		synchronized (specificListeners) {
			for (Iterator<Vector<PrologFileContentModelListener>> iter = specificListeners.values().iterator(); iter
					.hasNext();) {
				Collection<PrologFileContentModelListener> c = iter.next();
				clone.addAll(c);
			}
		}
		for (Iterator<PrologFileContentModelListener> iter = clone.iterator(); iter.hasNext();) {
			PrologFileContentModelListener l = iter
					.next();
			l.contentModelChanged(e);
		}
	}

	public void addPrologFileContentModelListener(
			PrologFileContentModelListener l) {
		if (!listeners.contains(l)) {
			listeners.add(l);
		}

	}

	public void removePrologFileContentModelListener(
			PrologFileContentModelListener l) {
		if (listeners.contains(l)) {
			listeners.remove(l);
		}

	}

	public void setInput(Object input) {
		this.input = input;
		try {
			if (this.dispatcher != null && this.subject != null) {

				this.dispatcher.removePrologInterfaceListener(this.subject,
						this);

			}
			if(getFile() == null) {
				String fileSpecificTest = "";
				if(input != null) {
					fileSpecificTest =  " for file '"+((FileEditorInput)input).getFile().getFullPath()+"'";
				}
				Debug.warning("Stopped setting the PDT outline content"+fileSpecificTest+". " +
					"The ContentModel has not been initialized. Probably the associated project does not have the PDT nature.");
				
				return;
			}
			
			this.subject = "builder(interprete(file('" + Util.prologFileName(getFile())
					+ "')))";
			if (this.dispatcher != null && this.subject != null) {

				this.dispatcher.addPrologInterfaceListener(this.subject, this);

			}

		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
	}

	public Vector<PrologFileContentModelListener> getListenersForParent(Object parent) {
		synchronized (specificListeners) {
			Vector<PrologFileContentModelListener> listeners = specificListeners.get(parent);
			if (listeners == null) {
				listeners = new Vector<PrologFileContentModelListener>();
				specificListeners.put(parent, listeners);
			}
			return listeners;
		}
	}

	public void addPrologFileContentModelListener(Object parent,
			PrologFileContentModelListener l) {
		synchronized (specificListeners) {
			Vector<PrologFileContentModelListener> listeners = getListenersForParent(parent);
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	public void removePrologFileContentModelListener(Object parent,
			PrologFileContentModelListener l) {
		synchronized (specificListeners) {
			Vector<PrologFileContentModelListener> listeners = getListenersForParent(parent);
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}
	}

	public void afterInit(PrologInterface pif) throws PrologInterfaceException {
		PrologLibraryManager mgr = PrologRuntimeUIPlugin.getDefault()
				.getLibraryManager();
		PrologSession s = pif.getSession(PrologInterface.NONE);
		try {
			FileSearchPathConfigurator.configureFileSearchPath(mgr, s,
					new String[] { PrologRuntimeUI.LIB_PDT });
			s.queryOnce("use_module(library('facade/pdt_outline'))");
			s.queryOnce("use_module(library('pef/pef_api'))");
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
		reset();
	}

	public void beforeShutdown(PrologInterface pif, PrologSession s)
			throws PrologInterfaceException {
		reset();
	}

	public void onError(PrologInterface pif) {
		try {
			reset();
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
	}
	
	public void lateInit(PrologInterface pif) {
		;
	}

	public void onInit(PrologInterface pif, PrologSession initSession)
			throws PrologInterfaceException {
		;
	}

	public void dispose() {
		try {
			setPif(null, null);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
		cache.clear();
		listeners.clear();

		specificListeners.clear();
	}

	public synchronized long getLastResetTime() {
		return timestamp;
	}

}
