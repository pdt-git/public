package org.cs3.pdt.common.queries;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.search.ui.text.Match;

public class UndefinedCallsSearchQuery extends MarkerCreatingSearchQuery {
	
	private static final String ATTRIBUTE = "pdt.undefined.call";

	public UndefinedCallsSearchQuery(boolean createMarkers) {
		super(new Goal("", "", "", -1, ""), createMarkers, ATTRIBUTE, ATTRIBUTE);
		setSearchType("Undefined calls");
	}

	@Override
	protected String buildSearchQuery(Goal goal, String module) {
		return bT(PDTCommonPredicates.FIND_UNDEFINED_CALL, "Module", "Name", "Arity", "File", "Start", "End", "UndefName", "UndefArity", "PropertyList");
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
		int end = Integer.parseInt(m.get("End").toString());
		PrologMatch match = createUniqueMatch(module, name, arity, file, offset, end - offset, properties, null, "definition");
		if (createMarkers && match != null) {
			try {
				IDocument document = UIUtils.getDocument(file);
				offset = UIUtils.logicalToPhysicalOffset(document, offset);
				end = UIUtils.logicalToPhysicalOffset(document, end);
				createMarker(file, "Undefined call: " + m.get("UndefName") + "/" + m.get("UndefArity") + " is not defined", offset, end);
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
		return match;
	}
	
}
