package pdt.y.focusview;

import java.io.File;
import java.util.List;

import org.cs3.prolog.common.Util;

import pdt.y.main.PDTGraphView;

public class GlobalGraphPIFLoader extends GraphPIFLoaderBase {
	
	private static final String NAME_OF_GLOBAL_HELPING_FILE = "pdt-global-help.graphml";
	
	protected List<String> paths;
	protected String currentPath;

	public GlobalGraphPIFLoader(PDTGraphView view) {
		super(view, NAME_OF_GLOBAL_HELPING_FILE);
	}
	
	protected GlobalGraphPIFLoader(PDTGraphView view, String helpFileName) {
		super(view, helpFileName);
	}
	
	@Override
	public String getCurrentPath() {
		return currentPath;
	}
	
	@Override
	public void setCurrentPath(String currentPath) {
		this.currentPath = currentPath;
	}
	
	@Override
	public List<String> getPaths() {
		return paths;
	}
	
	@Override
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
