package pdt.y.focusview;

import java.io.File;

import org.cs3.prolog.common.Util;

import pdt.y.main.PDTGraphView;

public class DependenciesGraphPIFLoader extends GlobalGraphPIFLoader {
	
	private static final String NAME_OF_DEPENDENCIES_HELPING_FILE = "pdt-dependencies-help.graphml";
	
	public DependenciesGraphPIFLoader(PDTGraphView view) {
		super(view, NAME_OF_DEPENDENCIES_HELPING_FILE);
	}
	
	@Override
	protected String generateQuery(File helpFile) {
		String query;
		query = "ensure_generated_factbase_for_source_file('" + currentPath + "'), "
				+ "write_dependencies_to_graphML(" + paths.toString() + ",'" + Util.prologFileName(helpFile) + "').";
		return query;
	}
}
