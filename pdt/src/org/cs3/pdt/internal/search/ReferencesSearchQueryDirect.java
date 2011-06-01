/**
 * 
 */
package org.cs3.pdt.internal.search;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;

/**
 * @author gk
 *
 */
public class ReferencesSearchQueryDirect extends PrologSearchQuery {

	
	public ReferencesSearchQueryDirect(PrologInterface pif, Goal goal) {
		super(pif, goal);
		setSearchType("References to");
	}


	@Override
	protected String buildSearchQuery(Goal goal, String module) {
		String arity = Integer.toString(goal.getArity());
		if (goal.getArity() < 0) 
			arity = "Arity";
		
		String file = "'"+goal.getFile()+"'";
		if (goal.getFile().equals(""))
			file = "File";
		
		String name = "'"+goal.getName()+"'";
		if (goal.getName().equals(""))
			name = "Predicate";
		
		String module2 = module;
		if (module.equals("''"))
			module2 = "Module";
		
		String query = "find_reference_to(" 
			             +name+  ", " 
			             +arity+ ", " 
			             +file+  ", " 
			             +module2
		                 +",RefModule,RefName,RefArity,RefFile,RefLine,Nth,Kind,PropertyList)";
		return query;
	}

	
	@SuppressWarnings("unchecked")
	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
	throws IOException {

		String module = (String)m.get("RefModule");
		String name = (String)m.get("RefName");
		int arity = Integer.parseInt((String)m.get("RefArity"));
		
		List<String> properties = null;
		Object prop = m.get("PropertyList");
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}
		IFile file = PDTCoreUtils.getFileForLocationIndependentOfWorkspace((String)m.get("RefFile"));
		int line = Integer.parseInt((String) m.get("RefLine"));

		PrologMatch match = createMatch(module, name, arity, file, line, properties );
		return match;
	}
	
	public boolean isCategorized(){
		return false;
	}
	
}

