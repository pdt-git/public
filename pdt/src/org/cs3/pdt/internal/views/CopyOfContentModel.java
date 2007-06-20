package org.cs3.pdt.internal.views;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

public class CopyOfContentModel extends DefaultAsyncPrologSessionListener implements
		PrologFileContentModel, LifeCycleHook2 {

	private static final String HOOK_ID = "PrologFileContentModelHook";

	private File file;

	private PrologInterface pif;

	private HashMap cache = new HashMap();

	private String oneMomentPlease = "one moment please...";

	private Object directiveTicket = "directiveTicket";

	private Object fileAnnosTicket = "fileAnnosTicket";

	private Object root = "root";

	private AsyncPrologSession session;

	private Vector listeners = new Vector();

	private HashMap specificListeners = new HashMap();

	private long timestamp = Long.MIN_VALUE;

	public CopyOfContentModel() {
		Debug.debug("debug");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#hasChildren(java.lang.Object)
	 */
	public boolean hasChildren(Object parentElement) {
		return (parentElement instanceof CTermNode && ((CTermNode) parentElement).term instanceof CCompound)
				|| parentElement instanceof Predicate
				|| parentElement instanceof ClauseNode
				&& ((ClauseNode) parentElement).hasBody()
				|| parentElement instanceof DirectiveNode;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#getChildren(java.lang.Object)
	 */
	public Object[] getChildren(Object parentElement)
			throws PrologInterfaceException {
		Debug.debug("outline getChildren for " + parentElement);
		Vector children = null;
		synchronized (cache) {
			children = getCachedChildren(parentElement);
			if (children.isEmpty()) {
				Debug.debug("outline no children cached for " + parentElement);
				if (parentElement instanceof CTermNode) {
					// FIXME: handle partial fetched terms (not implemented yet)
					CTerm term = ((CTermNode) parentElement).term;
					CCompound c = null;
					if (term instanceof CCompound) {
						c = (CCompound) term;
						for (int i = 0; i < c.getArity(); i++) {
							children.add(new CTermNode(c.getArgument(i)));
						}
					}
				} else if (pif != null && pif.isUp()) {
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
		Debug.debug("outline fetch children of " + parentElement);
		if (pif == null) {
			return;
		}
		if (getSession().isPending(parentElement)) {
			Debug.debug("outline fetch request already pending for "
					+ parentElement);
			return;
		}

		if (root == parentElement) {
			Debug.debug("outline: parent is root");
			if (getSession().isPending(directiveTicket)
					|| getSession().isPending(fileAnnosTicket)) {
				Debug
						.debug("outline: request for root (fileAnnos or directive ticket) is already pending");
				return;
			} else {
				fetchPredicates();
				fetchDirectives();
			}
		} else if (parentElement instanceof CTermNode) {
			fetchArguments((CTermNode) parentElement);

		} else if (parentElement instanceof PredicateNode) {
			PredicateNode p = (PredicateNode) parentElement;
			fetchClauses(p);
		} else if (parentElement instanceof ClauseNode) {
			ClauseNode c = (ClauseNode) parentElement;
			if (c.hasBody()) {
				fetchBody(c);
			}
		} else if (parentElement instanceof DirectiveNode) {
			DirectiveNode d = (DirectiveNode) parentElement;
			fetchBody(d);
		}
	}

	private void fetchDirectives() throws PrologInterfaceException {
		AsyncPrologSession session = getSession();
		String query = "pdt_file_directive('" + Util.prologFileName(file)
				+ "',Properties)";
		session.queryAll(directiveTicket, query);
	}

	private void fetchPredicates() throws PrologInterfaceException {
		AsyncPrologSession session = getSession();
		String query = "pdt_file_annotation('" + Util.prologFileName(file)
				+ "',FileAnnos)";
		session.queryOnce(fileAnnosTicket, query);

	}

	private void fetchBody(DirectiveNode d) throws PrologInterfaceException {
		String fileref = d.getProperty("file_ref");
		String n = d.getProperty("n");
		AsyncPrologSession session = getSession();
		String query = "pdt_lookup_aterm(file_ref(" + fileref + "), " + n
				+ ", Term)";
		session.queryOnce(d, query);
	}

	private void fetchBody(ClauseNode c) throws PrologInterfaceException {
		String fileref = c.getProperty("file_ref");
		String n = c.getProperty("n");
		AsyncPrologSession session = getSession();
		String query = "pdt_lookup_aterm(file_ref(" + fileref + "), " + n
				+ ", Term)";
		session.queryOnce(c, query);

	}

	private void fetchClauses(PredicateNode p) throws PrologInterfaceException {
		String file = "'" + getPlFile() + "'";
		String signature = "'"+p.getModule() + "': (" + p.getName() + ")	/"
				+ p.getArity();
		AsyncPrologSession session = getSession();
		String query = "pdt_predicate_clause(" + file + ", " + signature
				+ ", Properties)";
		session.queryAll(p, query);
	}

	private void fetchArguments(CTermNode node) {
		// TODO

	}

	public void goalHasSolution(AsyncPrologSessionEvent e) {
		Debug.debug("goal has solution: ");
		Debug.debug("  ticket: " + e.ticket);
		Debug.debug("  	query: " + e.query);
		if (e.ticket == directiveTicket) {
			addDirective(e);
		} else if (e.ticket == fileAnnosTicket) {
			addPredicates(e);
		} else if (e.ticket instanceof PredicateNode) {
			addClause(e);
		} else if (e.ticket instanceof DirectiveNode) {
			addDirectiveBody(e);
		} else if (e.ticket instanceof ClauseNode) {
			addClauseBody(e);
		}
	}

	private void addClauseBody(AsyncPrologSessionEvent e) {
		ClauseNode clause = (ClauseNode) e.ticket;
		CCompound term = (CCompound) e.bindings.get("Term");
		CTerm body = term.getArgument(1);
		clause.term = term;
		addChild(e.ticket, new CTermNode(body));

	}

	private void addDirectiveBody(AsyncPrologSessionEvent e) {
		DirectiveNode directive = (DirectiveNode) e.ticket;
		CCompound term = (CCompound) e.bindings.get("Term");
		directive.term = term;
		CTerm body = term.getArgument(0);
		addChild(e.ticket, new CTermNode(body));

	}

	private void addClause(AsyncPrologSessionEvent e) {
		PredicateNode predicate = (PredicateNode) e.ticket;
		Map properties = PLUtil.listAsMap((CTerm) e.bindings.get("Properties"));
		try {
			ClauseNode clause = new ClauseNode(properties, file);
			addChild(predicate, clause);
		} catch (IOException ex) {
			// UIUtils.logError(PDTPlugin.getDefault().getErrorMessageProvider(),
			// PDT.ERR_FILENAME_CONVERSION_PROBLEM, PDT.CX_OUTLINE, ex);
			// the actual problem happend earlier and should have already raised
			// an exception.
			// At the current position, there is not much we can do.
			Debug.rethrow(ex);
		}
	}

	private void addPredicates(AsyncPrologSessionEvent e) {

		String module = null;

		CTerm annosTerm = (CTerm) e.bindings.get("FileAnnos");

		Map fileAnnos = PLUtil.listAsMap(annosTerm);

		CTerm moduleTerm = (CTerm) fileAnnos.get("defines_module");
		module = moduleTerm == null ? "user" : moduleTerm.getFunctorValue();

		CTerm sigterm = (CTerm) fileAnnos.get("defines2");
		HashMap defines = new HashMap();
		if (sigterm != null) {
			CTerm[] assocs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < assocs.length; i++) {
				CTerm sig = ((CCompound) assocs[i]).getArgument(0);
				CTerm[] refTerms = PLUtil.listAsArray(((CCompound) assocs[i])
						.getArgument(1));
				String refString = Util.splice(refTerms, ",");

				PredicateNode predicateNode = new PredicateNode(
						(CCompound) sig, module);
				predicateNode.setPredicateProperty("clause_refs", refString);
				defines.put(PLUtil.renderSignature(sig, module), predicateNode);
			}

		}

		setPredicateProperty(defines, fileAnnos, module, "exports",
				Predicate.EXPORTED);
		setPredicateProperty(defines, fileAnnos, module, "defines_dynamic",
				Predicate.DYNAMIC);
		setPredicateProperty(defines, fileAnnos, module, "defines_multifile",
				Predicate.MULTIFILE);
		setPredicateProperty(defines, fileAnnos, module,
				"defines_module_transparent", Predicate.MODULE_TRANSPARENT);

		addChildren(root, defines.values());
	}

	private void addDirective(AsyncPrologSessionEvent e) {

		Map properties = PLUtil.listAsMap((CTerm) e.bindings.get("Properties"));
		try {
			DirectiveNode directive = new DirectiveNode(properties, file);

			addChild(root, directive);
		} catch (IOException ex) {
			// UIUtils.logError(PDTPlugin.getDefault().getErrorMessageProvider(),
			// PDT.ERR_FILENAME_CONVERSION_PROBLEM, PDT.CX_OUTLINE, ex);
			// the actual problem happend earlier and should have already raised
			// an exception.
			// At the current position, there is not much we can do.
			Debug.rethrow(ex);
		}

	}

	private void setPredicateProperty(Map defines, Map fileAnnos,
			String module, String key, String property) {
		CTerm sigterm = (CTerm) fileAnnos.get(key);
		if (sigterm != null) {
			CTerm[] sigs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < sigs.length; i++) {
				PredicateNode predicate = ((PredicateNode) defines.get(PLUtil
						.renderSignature(sigs[i], module)));
				if (predicate != null) {
					predicate.setPredicateProperty(property, "true");
				}

			}
		}
	}

	private AsyncPrologSession getSession() throws PrologInterfaceException {
		if (session == null && pif != null) {
			session = ((PrologInterface2) pif).getAsyncSession();
			session.setPreferenceValue("socketsession.canonical", "true");
			session.addBatchListener(this);
		}
		return session;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#getFile()
	 */
	public File getFile() {
		return file;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#setFile(java.io.File)
	 */
	public void setFile(File file) throws IOException, PrologInterfaceException {
		if (file != null) {
			// check if the file name can be resolved.
			// if there is a problem, throw now.
			// We want to avoid exceptions during lazy update() calls.
			file.getCanonicalPath();
		}

		this.file = file;

		reset();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#setPif(org.cs3.pl.prolog.PrologInterface)
	 */
	public void setPif(PrologInterface pif) throws PrologInterfaceException {
		if (this.session != null) {
			session.removeBatchListener(this);
			session.dispose();
			session = null;
		}
		if (this.pif != null) {
			((PrologInterface2) this.pif).removeLifeCycleHook(this, HOOK_ID);
		}
		this.pif = pif;
		if (this.pif != null) {
			this.pif.addLifeCycleHook(this, HOOK_ID, new String[0]);
		}
		reset();
	}

	private String getPlFile() {
		return file == null ? null : Util.prologFileName(file);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.internal.views.PrologFileContentModel#getPif()
	 */
	public PrologInterface getPif() {
		return pif;
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
		if (session != null) {
			session.abort();

		}
		Vector children = getCachedChildren(root);
		synchronized (cache) {
			cache.clear();

		}

		fireContentModelChanged();
	}

	private void addChildren(Object parent, Collection collection) {
		Vector children = null;
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
		Vector children = new Vector();
		children.add(child);
		addChildren(parent, children);
	}

	private Vector getCachedChildren(Object parent) {
		synchronized (cache) {
			Vector children = (Vector) cache.get(parent);
			if (children == null) {
				children = new Vector();

				cache.put(parent, children);
			}
			return children;
		}

	}

	private void fireChildrenAdded(Object parent, Object[] children) {
		PrologFileContentModelEvent e = new PrologFileContentModelEvent(this,
				parent, children, getLastResetTime());
		HashSet clone = new HashSet();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		synchronized (specificListeners) {
			clone.addAll(getListenersForParent(parent));
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologFileContentModelListener l = (PrologFileContentModelListener) iter
					.next();
			l.childrenAdded(e);
		}

	}

	private void fireChildrenRemoved(Object parent, Object[] children) {
		PrologFileContentModelEvent e = new PrologFileContentModelEvent(this,
				parent, children, getLastResetTime());
		HashSet clone = new HashSet();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		synchronized (specificListeners) {
			clone.addAll(getListenersForParent(parent));
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologFileContentModelListener l = (PrologFileContentModelListener) iter
					.next();
			l.childrenRemoved(e);
		}

	}

	private void fireContentModelChanged() {

		PrologFileContentModelEvent e = new PrologFileContentModelEvent(this,
				getLastResetTime());
		HashSet clone = new HashSet();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		synchronized (specificListeners) {
			for (Iterator iter = specificListeners.values().iterator(); iter
					.hasNext();) {
				Collection c = (Collection) iter.next();
				clone.addAll(c);
			}
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologFileContentModelListener l = (PrologFileContentModelListener) iter
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
		this.root = input;

	}

	public Vector getListenersForParent(Object parent) {
		synchronized (specificListeners) {
			Vector listeners = (Vector) specificListeners.get(parent);
			if (listeners == null) {
				listeners = new Vector();
				specificListeners.put(parent, listeners);
			}
			return listeners;
		}
	}

	public void addPrologFileContentModelListener(Object parent,
			PrologFileContentModelListener l) {
		synchronized (specificListeners) {
			Vector listeners = getListenersForParent(parent);
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	public void removePrologFileContentModelListener(Object parent,
			PrologFileContentModelListener l) {
		synchronized (specificListeners) {
			Vector listeners = getListenersForParent(parent);
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}
	}

	public void afterInit(PrologInterface pif) throws PrologInterfaceException {
		;

	}

	public void beforeShutdown(PrologInterface pif, PrologSession s)
			throws PrologInterfaceException {
		if (this.session != null) {
			session.removeBatchListener(this);
			session.dispose();
			session = null;
		}
		reset();

	}

	public void onError(PrologInterface pif) {
		if (this.session != null) {
			session.removeBatchListener(this);
			session = null;
		}
		try {
			reset();
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
	}

	public void onInit(PrologInterface pif, PrologSession initSession)
			throws PrologInterfaceException {
		;

	}

	public void dispose() {

		try {
			setPif(null);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
		cache.clear();
		cache = null;
		listeners.clear();
		listeners = null;
		specificListeners.clear();
		specificListeners = null;
		file = null;
		root = null;

	}

	public synchronized long getLastResetTime() {
		return timestamp;
	}

	public Object getInput() {

		return root;
	}

}
