package org.cs3.pdt.internal.queries;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.internal.structureElements.PrologMatch;
import org.cs3.pdt.metadata.Goal;
import org.cs3.prolog.common.FileUtils;
import org.eclipse.core.resources.IFile;

public class DefinitionsSearchQuery extends PDTSearchQuery {
	public DefinitionsSearchQuery(Goal goal) {
		super(goal);
		setSearchType("Definitions and declarations of");
	}

	@Override
	protected String buildSearchQuery(Goal goal, String module) {		
		String file = "'"+goal.getFile()+"'";
		if (goal.getFile().equals(""))
			file = "OrigFile";

		String module2 = module;
		if (module.equals("''"))
			module2 = "Module";
		
		String term = goal.getTermString();
		//String term = Util.quoteAtom(origTerm);
		
		
		String query = "pdt_search:find_definitions_categorized(" 
			            + file + "," + goal.getLine() + "," + term + ", Functor, Arity, "+ module2 + 
			            ", DeclOrDef, DefiningModule, File, Line, PropertyList, Visibility)";
		return query;
	}



	@SuppressWarnings("unchecked")
	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
	throws IOException {
		String definingModule = m.get("DefiningModule").toString();
		String functor = m.get("Functor").toString();
		int arity = Integer.parseInt(m.get("Arity").toString());
		IFile file = FileUtils.findFileForLocation(m.get("File").toString());
		int line = Integer.parseInt(m.get("Line").toString());

		Object prop = m.get("PropertyList");
		List<String> properties = null;
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}	
		String declOrDef = m.get("DeclOrDef").toString();
		String visibility = m.get("Visibility").toString();

		PrologMatch match = createUniqueMatch(definingModule, functor, arity,
				file, line, properties, visibility, declOrDef);
		
		return match;
	}
	
}
