package org.cs3.pdt.internal.views;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.internal.editors.PLPartitionScanner;
import org.cs3.pdt.runtime.PLUtil;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.ClauseData;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.ConsultServiceEvent;
import org.cs3.pl.prolog.ConsultServiceListener;
import org.cs3.pl.prolog.PrologSession2;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IFileEditorInput;

public class CTermContentProvider implements ITreeContentProvider,
		ConsultServiceListener {

	private ConsultService service;

	private Object[] data;

	private IFile file;

	private IPrologProject plProject;

	private HashMap clauses;

	private Viewer viewer;

	public CTermContentProvider(Viewer outline) {
		viewer = outline;

	}

	private void setConsultService(ConsultService nservice) {
		if (this.service != null) {
			this.service.removeConsultServiceListener(this);
		}
		this.service = nservice;
		if (this.service != null) {
			this.service.addConsultServiceListener(this);
		}
	}

	public Object[] getChildren(Object parentElement) {

		if (parentElement instanceof CCompound) {
			CCompound compound = (CCompound) parentElement;
			Object[] children = new Object[compound.getArity()];
			for (int i = 0; i < children.length; i++) {
				children[i] = compound.getArgument(i);
			}
			return children;
		} else if (parentElement instanceof Predicate) {
			Predicate p = (Predicate) parentElement;
			return getClauses(p);
		} else if (parentElement instanceof ClauseNode) {
			ClauseNode c = (ClauseNode) parentElement;
			return new CTerm[] { c.term };
		} else if (parentElement instanceof DirectiveNode) {
			DirectiveNode d = (DirectiveNode) parentElement;
			return new CTerm[] { d.term };
		}
		return getData();
	}

	public Object getParent(Object element) {
		return null;
	}

	public boolean hasChildren(Object parentElement) {
		return parentElement instanceof CCompound
				|| parentElement instanceof Predicate
				|| parentElement instanceof ClauseNode
				|| parentElement instanceof DirectiveNode;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
	 */
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	public void dispose() {
		;
	}

	/**
	 * 
	 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(Viewer,
	 *      Object, Object)
	 */
	public void inputChanged(final Viewer viewer, Object oldInput, Object input) {

		try {
			IFileEditorInput editorInput = null;
			IFile file = null;
			IProject project = null;
			plProject = null;
			if (input instanceof IFileEditorInput) {
				editorInput = (IFileEditorInput) input;

			}
			if (editorInput != null) {
				file = editorInput.getFile();
				project = file.getProject();
			}
			if (project != null && project.hasNature(PDTCore.NATURE_ID)) {
				plProject = (IPrologProject) project
						.getNature(PDTCore.NATURE_ID);
			}
			if (plProject == null) {
				setConsultService(null);
				return;
			}
			setConsultService(plProject.getMetadataPrologInterface()
					.getConsultService(PDTCore.CS_METADATA));
			setFile(file);

		} catch (Exception e) {
			Debug.report(e);
		}
	}

	private void setFile(IFile file) {
		this.file = file;
		this.data = null;

	}

	private IFile getFile() {
		return this.file;
	}

	private IPrologProject getPrologProject() {
		return this.plProject;

	}

	public void consultDataChanged(final ConsultServiceEvent e) {
		if (viewer == null || viewer.getControl().isDisposed()) {

			ConsultService service = (ConsultService) e.getSource();
			service.removeConsultServiceListener(this);
			return;
		}
		Display display = viewer.getControl().getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					consultDataChanged(e);
				}
			});
			return;
		}
		data = null;
		clauses = null;
		viewer.refresh();

	}

	private Object[] getData() {
		if (data == null) {
			update();
		}
		return data;
	}

	private Clause[] getClauses(Predicate p) {
		if (clauses == null) {
			update();
		}
		List l = (List) clauses.get(p);
		return (Clause[]) l.toArray(new Clause[l.size()]);
	}

	private void update() {
		clauses = new HashMap();
		IFile wsfile = getFile();
		String plFile = Util.prologFileName(wsfile.getLocation().toFile());
		plProject = getPrologProject();
		PrologSession2 s = (PrologSession2) plProject
				.getMetadataPrologInterface().getSession();
		CTerm[] members = null;
		Map fileAnnos = null;
		String module = null;

		HashSet exported = new HashSet();
		HashSet dynamic = new HashSet();
		HashSet multifile = new HashSet();
		HashSet module_transparent = new HashSet();
		try {
			s.setPreferenceValue("socketsession.canonical", "true");
			String query = "current_file_annotation('" + plFile
					+ "',Annos,Members)";
			Map map = s.queryOnce(query);
			if (map == null) {
				data = new Object[0];
				Debug.warning("no annotation found for file "+plFile+".\n" +
						"(failed query: \""+query+"\"");
				return;
			}
			CTerm membersTerm = (CTerm) map.get("Members");
			CTerm annosTerm = (CTerm) map.get("Annos");
			members = PLUtil.listAsArray(membersTerm);
			fileAnnos = PLUtil.listAsMap(annosTerm);
		}catch(Throwable t){				
			Debug.report(t);
			data = new Object[0];
			return;
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
		CTerm moduleTerm = (CTerm) fileAnnos.get("defines_module");
		module = moduleTerm == null ? "user" : moduleTerm.getFunctorValue();

		CTerm[] sigs = null;

		CTerm sigterm = (CTerm) fileAnnos.get("exports");
		if (sigterm != null) {
			sigs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < sigs.length; i++) {
				exported.add(new PredicateNode(sigs[i], module));
			}
		}
		sigterm = (CTerm) fileAnnos.get("dynamic");
		if (sigterm != null) {
			sigs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < sigs.length; i++) {
				dynamic.add(new PredicateNode(sigs[i], module));
			}
		}
		sigterm = (CTerm) fileAnnos.get("multifile");
		if (sigterm != null) {
			sigs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < sigs.length; i++) {
				multifile.add(new PredicateNode(sigs[i], module));
			}
		}
		sigterm = (CTerm) fileAnnos.get("module_transparent");
		if (sigterm != null) {
			sigs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < sigs.length; i++) {
				module_transparent.add(new PredicateNode(sigs[i], module));
			}
		}
		Vector tempData = new Vector();

		for (int i = 0; i < members.length; i++) {
			CTerm term = members[i];
			if (term.getAnotation("clause_of") == null) {
				tempData.add(new DirectiveNode(wsfile, module, term));
			} else {
				Clause clause = new ClauseNode(term, wsfile);
				Predicate predicate = clause.getPredicate();
				List l = (List) clauses.get(predicate);
				if (l == null) {
					l = new Vector();
					clauses.put(predicate, l);
					tempData.add(predicate);
					if (exported.contains(predicate)) {
						predicate.setPredicateProperty(Predicate.EXPORTED,
								"true");
					}
					if (dynamic.contains(predicate)) {
						predicate.setPredicateProperty(Predicate.DYNAMIC,
								"true");
					}
					if (multifile.contains(predicate)) {
						predicate.setPredicateProperty(Predicate.MULTIFILE,
								"true");
					}
					if (module_transparent.contains(predicate)) {
						predicate.setPredicateProperty(
								Predicate.MODULE_TRANSPARENT, "true");
					}
				}
				l.add(clause);

			}
		}
		data = tempData.toArray();
	}

}
