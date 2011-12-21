package org.cs3.pdt.internal.queries;

import java.io.IOException;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.structureElements.PDTMatch;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;

public class ReferencesSearchQuery extends PDTSearchQuery {
	private static final String LINE_VAR = "Line";
	private static final String ARITY_VAR = "Arity";
	private static final String FUNCTOR_VAR = "Name";
	private static final String MODULE_VAR = "RefModule";
	private static final String FILE_VAR = "File";
	
	public ReferencesSearchQuery(PrologInterface pif, Goal goal) {
		super(pif, goal);
		setSearchType("References to");
	}

	@Override
	// pdt_search:find_reference_to(Functor,Arity,DefFile, DefModule,RefModule,RefHead,RefFile,RefLine,Nth,Kind)
	protected String buildSearchQuery(Goal goal, String module) {
	 // String query = "get_references('" +goal.getFile()+ "','" +goal.getName()+ "'/" +goal.getArity()+ ",'" + goal.getModule() + "'," +
		String query = "get_references('" +goal.getFile()+ "','" +goal.getFunctor()+ "'/" +goal.getArity()+ ", " + module           + " ," +
						FILE_VAR + "," +
						LINE_VAR + "," +
						MODULE_VAR + "," +
						FUNCTOR_VAR + "," +
						ARITY_VAR + ")";
		return query;
	}

	
	@Override
	protected PDTMatch constructPrologMatchForAResult(Map<String, Object> m)
	throws IOException {
		String module = (String)m.get(MODULE_VAR);
		String name = (String)m.get(FUNCTOR_VAR);
		int arity = Integer.parseInt((String)m.get(ARITY_VAR));

		IFile file = PDTCoreUtils.getFileForLocationIndependentOfWorkspace((String)m.get(FILE_VAR));
		int line = Integer.parseInt((String) m.get(LINE_VAR));

		PDTMatch match = createUniqueMatch(module, name, arity, file, line, new Vector<String>(), null, "definition");
		return match;
	}
	
	public boolean isCategorized(){
		return false;
	}
	
}
