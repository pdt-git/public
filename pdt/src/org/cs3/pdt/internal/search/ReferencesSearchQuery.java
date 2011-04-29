package org.cs3.pdt.internal.search;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;

public class ReferencesSearchQuery extends PrologSearchQuery {
	private static final String LINE_VAR = "Line";
	private static final String ARITY_VAR = "Arity";
	private static final String FUNCTOR_VAR = "Name";
	private static final String MODULE_VAR = "RefModule";
	private static final String FILE_VAR = "File";
	
	public ReferencesSearchQuery(PrologInterface pif, GoalData goal) {
		super(pif, goal);
	}

	@Override
	protected String buildSearchQuery(GoalData goal, String module) {
	 // String query = "get_references('" +goal.getFile()+ "','" +goal.getName()+ "'/" +goal.getArity()+ ",'" + goal.getModule() + "'," +
		String query = "get_references('" +goal.getFile()+ "','" +goal.getName()+ "'/" +goal.getArity()+ ", " + module           + " ," +
						FILE_VAR + "," +
						LINE_VAR + "," +
						MODULE_VAR + "," +
						FUNCTOR_VAR + "," +
						ARITY_VAR + ")";
		return query;
	}



//	protected List<Map<String, Object>> getResultForQuery(PrologSession session,
//			String module, String query, GoalData goal) throws PrologInterfaceException {
//		Debug.info(query);
//		
//		List<Map<String, Object>> clauses = session.queryAll(query);
//		
//		if(clauses.size()>0 && goal.getModule()==null){
//			goal.setModule((String)clauses.get(0).get("Module"));
//		}
//		return clauses;
//	}

	
	
	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
	throws IOException {
		String module = (String)m.get(MODULE_VAR);
		String name = (String)m.get(FUNCTOR_VAR);
		int arity = Integer.parseInt((String)m.get(ARITY_VAR));

		IFile file = PDTCoreUtils.getFileForLocationIndependentOfWorkspace((String)m.get(FILE_VAR));
		int line = Integer.parseInt((String) m.get(LINE_VAR));

		PrologMatch match = createMatch(module, name, arity, file, line);
		return match;
	}
	
	public boolean isCategorized(){
		return false;
	}
	
}
