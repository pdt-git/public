package pdt.y.focusview;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.prolog.common.ResourceFileLocator;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;

import pdt.y.main.PDTGraphView;

public class FocusGraphPIFLoader extends GraphPIFLoaderBase {

	private static final String NAME_OF_HELPING_FILE = "pdt-focus-help.graphml";

	private String focusFile;
	private List<String> dependencies = new ArrayList<String>();

	public FocusGraphPIFLoader(PDTGraphView view) {
		super(view);

		PrologRuntimeUIPlugin plugin = PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(NAME_OF_HELPING_FILE);
	}

	public String getFocusFile() {
		return focusFile;
	}

	public void setFocusFile(String focusFile) {
		this.focusFile = focusFile;
	}

	public List<String> getDependencies() {
		return dependencies;
	}

	protected String generateQuery(File helpFile) {
		String query;
		query = "ensure_generated_factbase_for_source_file('" + focusFile + "'), " 
				+ "write_focus_to_graphML('" + focusFile + "', " + "'"
				+ Util.prologFileName(helpFile) + "', " + "Dependencies).";
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
