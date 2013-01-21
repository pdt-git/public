package org.cs3.pdt.common.queries;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.metadata.Goal;
import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class MetaPredicatesSearchQuery extends PDTSearchQuery {
	
	public MetaPredicatesSearchQuery() {
		super(new Goal("", "", "", -1, ""));
		setSearchType("Undeclared or wrongly declared meta predicates");
	}
	
	@Override
	protected String buildSearchQuery(Goal goal, String module) {
		return bT("find_undeclared_meta_predicate", "Module", "Name", "Arity", "MetaSpec", "File", "Line", "PropertyList");
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {
		String definingModule = m.get("Module").toString();
		String functor = m.get("Name").toString();
		int arity=-1;
		try {
			arity = Integer.parseInt(m.get("Arity").toString());
		} catch (NumberFormatException e) {}
		
		IFile file = findFile(m.get("File").toString());
		int line = Integer.parseInt(m.get("Line").toString());

		Object prop = m.get("PropertyList");
		List<String> properties = null;
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}	
		Match match = createUniqueMatch(definingModule, functor, arity, file, line, properties, "", "definition");
		
		return match;
	}
	
}
