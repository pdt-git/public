package org.cs3.pdt.common.queries;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.metadata.Goal;
import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class DeadPredicatesSearchQuery extends PDTSearchQuery {

	public DeadPredicatesSearchQuery() {
		super(new Goal("", "", "", -1, ""));
	}

	@Override
	protected String buildSearchQuery(Goal goal, String module) {
		return bT(PDTCommonPredicates.FIND_DEAD_PREDICATE, "Module", "Name", "Arity", "File", "Location", "PropertyList");
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
		String offsetOrLine = m.get("Location").toString();

		Object prop = m.get("PropertyList");
		List<String> properties = null;
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}	
		Match match = null;
		if (offsetOrLine.indexOf("-") >= 0) {
			String[] positions = offsetOrLine.split("-");
			int offset = Integer.parseInt(positions[0]);
			int length = Integer.parseInt(positions[1]) - offset;
			match = createUniqueMatch(definingModule, functor, arity, file, offset, length, properties, "", "definition");
		} else {
			int line = Integer.parseInt(offsetOrLine);
			match = createUniqueMatch(definingModule, functor, arity, file, line, properties, "", "definition");
		}
		
		return match;	}

}
