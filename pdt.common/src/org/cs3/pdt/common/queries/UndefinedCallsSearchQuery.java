package org.cs3.pdt.common.queries;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class UndefinedCallsSearchQuery extends PDTSearchQuery {
	
	public UndefinedCallsSearchQuery() {
		super(new Goal("", "", "", -1, ""));
	}

	@Override
	protected String buildSearchQuery(Goal goal, String module) {
		return bT(PDTCommonPredicates.FIND_UNDEFINED_CALL, "Module", "Name", "Arity", "File", "Start", "End", "PropertyList");
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {
		String module = m.get("Module").toString();
		String name = m.get("Name").toString();
		int arity = Integer.parseInt(m.get("Arity").toString());
		
		List<String> properties = null;
		Object prop = m.get("PropertyList");
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}
		IFile file = findFile(m.get("File").toString());
		int offset = Integer.parseInt(m.get("Start").toString());
		int length = Integer.parseInt(m.get("End").toString()) - offset;
		PrologMatch match = createUniqueMatch(module, name, arity, file, offset, length, properties, null, "definition");
		return match;
	}
	
}
