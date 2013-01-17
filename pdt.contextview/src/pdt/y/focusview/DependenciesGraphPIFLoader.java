package pdt.y.focusview;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;

import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.Util;
import org.eclipse.core.resources.IProject;

import pdt.y.graphml.GraphMLReader;
import pdt.y.main.PDTGraphView;
import pdt.y.model.GraphModel;
import pdt.y.model.realizer.nodes.SimpleFileNodeRealizer;

public class DependenciesGraphPIFLoader extends GlobalGraphPIFLoader {
	
	private static final String NAME_OF_DEPENDENCIES_HELPING_FILE = "pdt-dependencies-help.graphml";
	
	public DependenciesGraphPIFLoader(PDTGraphView view) {
		super(view, NAME_OF_DEPENDENCIES_HELPING_FILE);
	}
	
	@Override
	protected void doLoadFile() throws MalformedURLException {
		GraphModel graphModel = new GraphMLReader().readFile(helpFile.toURI().toURL());
		graphModel.setDefaultNodeRealizer(new SimpleFileNodeRealizer(graphModel));
		graphModel.categorizeData();
		graphModel.assignPortsToEdges();
		
		view.loadGraph(graphModel);
	}
	
	@Override
	protected String generateQuery(File helpFile) {
		try {
			loadPaths(currentPath);

			IProject project = FileUtils.findFileForLocation(currentPath).getProject();
			String projectPath = Util.normalizeOnWindows(project.getLocation().toString());
			
			String query;
			query = "ensure_generated_factbase_for_source_file('" + currentPath + "'), "
					+ "write_dependencies_to_graphML(" + paths.toString() + ", '" + projectPath + "', '" + Util.prologFileName(helpFile) + "').";
			return query;
			
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
}
