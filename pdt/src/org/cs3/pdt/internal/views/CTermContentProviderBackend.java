package org.cs3.pdt.internal.views;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologEventDispatcher;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession2;

public class CTermContentProviderBackend  {

	private File file;

	private HashMap clauses;

	private Object[] data;

	private PrologInterface pif;

	

	public Object[] getData() throws PrologInterfaceException {
		if (data == null) {
			try {
				update();
			} catch (IOException e) {
				Debug.error("this error should have been catched earlier!!");
				Debug.rethrow(e);
				
			}
		}
		return data;
	}

	private Clause[] getClauses(Predicate p) throws PrologInterfaceException {
		if (clauses == null) {

			try {
				update();
			} catch (IOException e) {
				Debug.error("this error should have been catched earlier!!");
				Debug.rethrow(e);
				
			}

		}
		List l = (List) clauses.get(p);
		return (Clause[]) l.toArray(new Clause[l.size()]);
	}
	public boolean hasChildren(Object parentElement) {
		return (parentElement instanceof CTermNode && ((CTermNode)parentElement).term instanceof CCompound)
				|| parentElement instanceof Predicate
				|| parentElement instanceof ClauseNode
				|| parentElement instanceof DirectiveNode;

	}
	public Object[] getChildren(Object parentElement)
			throws PrologInterfaceException {

		if (parentElement instanceof CTermNode) {
			CTerm term = ((CTermNode)parentElement).term;
			if(term instanceof CCompound){
				CCompound compound = (CCompound) term;
				Object[] children = new Object[compound.getArity()];
				for (int i = 0; i < children.length; i++) {
					children[i] = new CTermNode(compound.getArgument(i));
				}
				return children;
			}
			return null;
		} else if (parentElement instanceof Predicate) {
			Predicate p = (Predicate) parentElement;
			return getClauses(p);
		} else if (parentElement instanceof ClauseNode) {
			ClauseNode c = (ClauseNode) parentElement;
			return new CTermNode[] { new CTermNode(c.term) };
		} else if (parentElement instanceof DirectiveNode) {
			DirectiveNode d = (DirectiveNode) parentElement;
			return new CTermNode[] { new CTermNode(d.term) };
		}
		return getData();
	}

	private void update() throws PrologInterfaceException, IOException {

		clauses = new HashMap();
		if (file == null) {
			Debug
					.warning("CTermContentProvider.update was called, but file is null");
			data = new Object[0];
			return;
		}
		if (pif == null) {
			Debug
					.warning("CTermContentProvider.update was called, but pif is null");
			data = new Object[0];
			return;
		}
		CTerm[] members = null;
		Map fileAnnos = null;
		String module = null;
		String plFile = Util.prologFileName(file);
		HashSet exported = new HashSet();
		HashSet dynamic = new HashSet();
		HashSet multifile = new HashSet();
		HashSet module_transparent = new HashSet();
		PrologSession2 s = (PrologSession2) pif.getSession();
		try {
			
			s.setPreferenceValue("socketsession.canonical", "true");
			//s.queryOnce("guitracer");
			String query = "current_file_annotation('" + plFile
					+ "',Annos,Members)";
			
			Map map = s.queryOnce(query);
			if (map == null) {
				Debug.warning("no annotation found for file " + plFile + ".\n"
						+ "(failed query: \"" + query + "\"");
				data = new Object[0];
				return;
			}
			CTerm membersTerm = (CTerm) map.get("Members");
			CTerm annosTerm = (CTerm) map.get("Annos");
			members = PLUtil.listAsArray(membersTerm);
			fileAnnos = PLUtil.listAsMap(annosTerm);
		} catch (Throwable t) {
			Debug.report(t);
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
				exported.add(new PredicateNode((CCompound) sigs[i], module));
			}
		}
		sigterm = (CTerm) fileAnnos.get("dynamic");
		if (sigterm != null) {
			sigs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < sigs.length; i++) {
				dynamic.add(new PredicateNode((CCompound) sigs[i], module));
			}
		}
		sigterm = (CTerm) fileAnnos.get("multifile");
		if (sigterm != null) {
			sigs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < sigs.length; i++) {
				multifile.add(new PredicateNode((CCompound) sigs[i], module));
			}
		}
		sigterm = (CTerm) fileAnnos.get("transparent");
		if (sigterm != null) {
			sigs = PLUtil.listAsArray(sigterm);
			for (int i = 0; i < sigs.length; i++) {
				module_transparent.add(new PredicateNode((CCompound) sigs[i],
						module));
			}
		}
		Vector tempData = new Vector();

		for (int i = 0; i < members.length; i++) {
			CTerm term = members[i];
			if (term.getAnotation("clause_of") == null) {
				tempData.add(new DirectiveNode(file, module, term));
			} else {
				Clause clause = new ClauseNode(term, file);
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

	public File getFile() {
		return file;
	}

	public void setFile(File file) throws PrologInterfaceException, IOException {
		if(file!=null){
			//check if the file name can be resolved.
			//if there is a problem, throw now.
			//We want to avoid exceptions during lazy update() calls.
			file.getCanonicalPath();
		}
		
		this.file = file;
		
		reset();

	}

	public void setPif(PrologInterface pif) {
		this.pif = pif;
		reset();
	}
	
	
	
	private String getPlFile() {
		return file==null?null:Util.prologFileName(file);
	}
	
	public PrologInterface getPif() {
		return pif;
	}

	public void reset() {
		this.data = null;
		this.clauses= null;
		
	}

		
}
