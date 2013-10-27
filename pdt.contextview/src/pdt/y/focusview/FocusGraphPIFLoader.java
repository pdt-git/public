package pdt.y.focusview;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.prolog.common.Util;

import pdt.y.main.PDTGraphView;

public class FocusGraphPIFLoader extends GraphPIFLoaderBase {

	private static final String NAME_OF_FOCUS_HELPING_FILE = "pdt-focus-help.graphml";

	private String focusFile;
	private List<String> dependencies = new ArrayList<String>();

	public FocusGraphPIFLoader(PDTGraphView view) {
		super(view, NAME_OF_FOCUS_HELPING_FILE);
	}

	@Override
	public String getCurrentPath() {
		return focusFile;
	}

	@Override
	public void setCurrentPath(String currentPath) {
		this.focusFile = currentPath;
	}

	public List<String> getDependencies() {
		return dependencies;
	}

	protected String generateQuery(File helpFile) {
		String query;
		query = bT("write_focus_to_graphML", Util.quoteAtom(focusFile), Util.quoteAtom(Util.prologFileName(helpFile)), "Dependencies");
		return query;
	}

	@Override
	public Map<String, Object> loadGraph() {

		Map<String, Object> output = super.loadGraph();

		dependencies.clear();

		if (output != null && output.containsKey("Dependencies")) {
			@SuppressWarnings("unchecked")
			Vector<String> deps = (Vector<String>) output.get("Dependencies");
			dependencies.addAll(deps);
		}
		return output;
	}
}
