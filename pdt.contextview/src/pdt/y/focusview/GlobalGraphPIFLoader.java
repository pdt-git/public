package pdt.y.focusview;

import java.io.File;
import java.util.List;

import org.cs3.prolog.common.ResourceFileLocator;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;

import pdt.y.main.PDTGraphView;

public class GlobalGraphPIFLoader extends GraphPIFLoaderBase {
	
	private static final String NAME_OF_GLOBAL_HELPING_FILE = "pdt-global-help.graphml";
	
	private List<String> paths;
	private String currentPath;

	public GlobalGraphPIFLoader(PDTGraphView view) {
		super(view);
		
		PrologRuntimeUIPlugin plugin = PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(NAME_OF_GLOBAL_HELPING_FILE);
	}
	
	public String getCurrentPath() {
		return currentPath;
	}
	
	public void setCurrentPath(String currentPath) {
		this.currentPath = currentPath;
	}
	
	public List<String> getPaths() {
		return paths;
	}
	
	public void setPaths(List<String> paths) {
		this.paths = paths;
	}
	
	@Override
	protected String generateQuery(File helpFile) {
		String query;
		query = "ensure_generated_factbase_for_source_file('" + currentPath + "'), "
				+ "write_global_to_graphML(" + paths.toString() + ",'" + Util.prologFileName(helpFile) + "').";
		return query;
	}

}
