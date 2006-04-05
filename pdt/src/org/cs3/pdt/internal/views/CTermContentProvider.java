/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.internal.views;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.PLUtil;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession2;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IFileEditorInput;

public class CTermContentProvider implements ITreeContentProvider,
		 PrologInterfaceListener {


	private Object[] data;

	private IFile file;

	private IPrologProject plProject;

	private HashMap clauses;

	private Viewer viewer;

	public CTermContentProvider(Viewer outline) {
		viewer = outline;

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
				setFile(null);
				return;
			}
			
			setFile(file);
			viewer.refresh();

		} catch (Exception e) {
			Debug.report(e);
		}
	}

	private void setFile(IFile file) {
		if(file!=null){
			String plFile = Util.prologFileName(file.getLocation().toFile());
			getPrologProject().getMetaDataEventDispatcher().removePrologInterfaceListener("file_annotation('"+plFile+"')", this);
			
		}
		this.file = file;
		if(file!=null){
			String plFile = Util.prologFileName(file.getLocation().toFile());
			getPrologProject().getMetaDataEventDispatcher().addPrologInterfaceListener("file_annotation('"+plFile+"')", this);
		}
		this.data = null;

	}

	private IFile getFile() {
		return this.file;
	}

	private IPrologProject getPrologProject() {
		return this.plProject;

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



	public void update(final PrologInterfaceEvent e) {
		if (viewer == null || viewer.getControl().isDisposed()) {
			return;
		}
		Display display = viewer.getControl().getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					update(e);
				}
			});
			return;
		}
		data = null;
		clauses = null;
		viewer.refresh();
		
	}



	

}
