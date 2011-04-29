package org.cs3.pdt.internal.search;

import java.io.IOException;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;

public class CategorizedDefinitionsSearchQuery extends PrologSearchQuery {
	private static final String LINE_VAR = "Line";
//	private static final String ARITY_VAR = "Arity";
//	private static final String FUNCTOR_VAR = "Name";
	private static final String FILE_VAR = "File";


	public CategorizedDefinitionsSearchQuery(PrologInterface pif, GoalData goal) {
		super(pif, goal);
	}
	
	@Override
	protected String buildSearchQuery(GoalData goal, String module) {

		String query = "find_definition_visible_in('"
//			            +goal.getFile()+ "','" +goal.getName()+ "'," +goal.getArity()+ ",'" +goal.getModule()+ "'," +
			            +goal.getFile()+ "','" +goal.getName()+ "'," +goal.getArity()+ ", " +module          + " ," +
						FILE_VAR + "," + 
						LINE_VAR + ")" ;
		return query;
	}

	
	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
			throws IOException {
		
                GoalData goal = getGoal();
        
		        String module = goal.getModule();
		        String name =  goal.getName();
		        int arity =   goal.getArity();
				IFile file = PDTCoreUtils.getFileForLocationIndependentOfWorkspace((String)m.get(FILE_VAR));
				int line = Integer.parseInt((String) m.get(LINE_VAR));
				
				PrologMatch match = createMatch(module, name, arity, file, line);
				addCategoryEntry(match, "Visible definitions of "+name+"/"+arity+" in '"+module+"'");
//				addCategoryEntry(match, "Supermodule definitions imported into '"+module+"'");
//				addCategoryEntry(match, "Local definitions in '"+module+"'");
//				addCategoryEntry(match, "Submodule definitions meta-callable in '"+module+"'");
//				addCategoryEntry(match, "Definitions needing explicit module prefix in '"+module+"'");
				return match;
			}
	
}
