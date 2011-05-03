package org.cs3.pdt.internal.search;

import java.io.IOException;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;

public class DefinitionsSearchQuery extends PrologSearchQuery {
	private static final String LINE_VAR = "Line";
	private static final String ARITY_VAR = "Arity";
	private static final String FUNCTOR_VAR = "Name";
	private static final String FILE_VAR = "File";


	public DefinitionsSearchQuery(PrologInterface pif, Goal goal) {
		super(pif, goal);
		setSearchType("Definitions of");
	}
	
	@Override
	protected String buildSearchQuery(Goal goal, String module) {

		String query = "find_definition_visible_in('" 
//			            +goal.getFile()+ "','" +goal.getName()+ "'," +goal.getArity()+ ",'" +goal.getModule()+ "'," +
			            +goal.getFile()+ "','" +goal.getName()+ "'," +goal.getArity()+ ", " +module          + " ," +
						FILE_VAR + "," + 
						LINE_VAR + ")";
		return query;
	}


	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
			throws IOException {

		        Goal goal = getGoal();
		        
				String module = goal.getModule();
				String name =  goal.getName();
				int arity =   goal.getArity();
				IFile file = PDTCoreUtils.getFileForLocationIndependentOfWorkspace((String)m.get(FILE_VAR));
				int line = Integer.parseInt((String) m.get(LINE_VAR));
				
				PrologMatch match = createMatch(module, name, arity, file, line);
				return match;
	}

	public boolean isCategorized(){
		return false;
	}

}
