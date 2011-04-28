package org.cs3.pdt.internal.search;

import java.io.IOException;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.GoalData;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;

public class DefinitionsSearchQuery extends PrologSearchQuery {
	private static final String LINE_VAR = "Line";
	private static final String ARITY_VAR = "Arity";
	private static final String FUNCTOR_VAR = "Name";
	private static final String FILE_VAR = "File";
	private String module;

	public DefinitionsSearchQuery(PrologInterface pif, GoalData goal) {
		super(pif, goal);
	}
	
	@Override
	protected String buildSearchQuery(String module, String enclFile, GoalData goal) {
		this.module = module;
		String query = "find_definition_visible_in('"+enclFile+"','" + goal.getName()+"'," + goal.getArity()+ "," + module  + "," +
						FILE_VAR + "," + 
						LINE_VAR + "," +
						FUNCTOR_VAR + "," +
						ARITY_VAR + ")";
		return query;
	}


	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
			throws IOException {
/*
				String module = //(String)m.get(MODULE_VAR);
					            goal.getModule();
				String name =  // (String)m.get(FUNCTOR_VAR);
					           goal.getName();
				int arity =   // Integer.parseInt((String)m.get(ARITY_VAR));
				              goal.getArity();
				IFile file = getFileForString((String)m.get(FILE_VAR));
				int line = Integer.parseInt((String) m.get(LINE_VAR))-1;
				
				PrologMatch match = createMatch(module, name, arity, file, line);
				return match;
			}
*/
		String resultModule = module;
		if (Util.isVariable(module))
			resultModule = (String)m.get(module);

		String name = (String)m.get(FUNCTOR_VAR);
		int arity = Integer.parseInt((String)m.get(ARITY_VAR));

		IFile file = PDTCoreUtils.getFileForLocationIndependentOfWorkspace((String)m.get(FILE_VAR));
		int line = Integer.parseInt((String) m.get(LINE_VAR))-1;

		PrologMatch match = createMatch(resultModule, name, arity, file, line);
		return match;
	}

	public boolean isCategorized(){
		return false;
	}

}
