package org.cs3.pdt.internal.search;

import java.io.IOException;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;

public class CategorizedDefinitionsSearchQuery extends PrologSearchQuery {


	public CategorizedDefinitionsSearchQuery(PrologInterface pif, Goal goal) {
		super(pif, goal);
		setSearchType("Definitions and declarations of");
	}

	protected String getCategoryDescription(Goal goal, String category)  {
		return category + " of "+goal.getName()+"/"+goal.getArity()+" in '"+goal.getModule()+"'";
	}

//	@Override
//	protected String buildSearchQuery(GoalData goal, String module) {
//
//		String query = "find_definition_visible_in('"
////			            +goal.getFile()+ "','" +goal.getName()+ "'," +goal.getArity()+ ",'" +goal.getModule()+ "'," +
//			            +goal.getFile()+ "','" +goal.getName()+ "'," +goal.getArity()+ ", " +module          + " ," +
//						FILE_VAR + "," + 
//						LINE_VAR + ")" ;
//		return query;
//	}


	@Override
	protected String buildSearchQuery(Goal goal, String module) {		
		String arity = Integer.toString(goal.getArity());
		if (goal.getArity() < 0) 
			arity = "Arity";
		
		String file = "'"+goal.getFile()+"'";
		if (goal.getFile().equals(""))
			file = "OrigFile";
		
		String name = "'"+goal.getName()+"'";
		if (goal.getName().equals(""))
			name = "Predicate";
		
		String module2 = module;
		if (module.equals("''"))
			module2 = "Module";
		
		String query = "find_definitions_categorized(" 
//			            +goal.getFile()+ "','" +goal.getName()+ "'," +goal.getArity()+ ",'" +goal.getModule()+ "'," +
			            + file + "," + name+ ", " + arity+ ", "+ module2 + 
			            ",Visibility,DefiningModule,File,Line)";
		return query;
	}



	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
			throws IOException {
		
                Goal goal = getGoal();
        
		        String definingModule = (String)m.get("DefiningModule");
//		        String contextModule = goal.getModule(); 
		        String name =  goal.getName();
		        int arity =   goal.getArity();
				IFile file = PDTCoreUtils.getFileForLocationIndependentOfWorkspace((String)m.get("File"));
				int line = Integer.parseInt((String) m.get("Line"));
				
				PrologMatch match = createMatch(definingModule, name, arity, file, line);
				
				String visibility = (String)m.get("Visibility");
				addCategoryEntry(match, getCategoryDescription(goal, visibility));			

//				addCategoryEntry(match, getCategoryDescription(goal, "Visible "));			
//				addCategoryEntry(match, getCategoryDescription(goal, "Supermodule"));			
//				addCategoryEntry(match, getCategoryDescription(goal, "Local"));
//				addCategoryEntry(match, getCategoryDescription(goal, "Meta-callable submodule"));
//				addCategoryEntry(match, getCategoryDescription(goal, "Locally invisible"));
				return match;
			}
	
	
}
