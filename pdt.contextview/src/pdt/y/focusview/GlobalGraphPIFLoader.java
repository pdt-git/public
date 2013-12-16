package pdt.y.focusview;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.ui.util.ExternalPrologFilesProjectUtils;
import org.cs3.prolog.ui.util.FileUtils;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

import pdt.y.PDTGraphPredicates;
import pdt.y.main.PDTGraphView;

public class GlobalGraphPIFLoader extends GraphPIFLoaderBase {
	
	private static final String NAME_OF_GLOBAL_HELPING_FILE = "pdt-global-help.graphml";
	
	private static final ArrayList<String> extensions = new ArrayList<String>();
	static {
		extensions.add("pl");
		extensions.add("prolog");
		extensions.add("lgt");
		extensions.add("logtalk");
	}
	
	protected List<String> paths = new ArrayList<String>();
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
	protected String generateQuery(File helpFile) {
		loadPaths(currentPath);
		
		String query;
		query = bT(PDTGraphPredicates.WRITE_GLOBAL_TO_GRAPHML, paths.toString(), Util.quoteAtom(Util.prologFileName(helpFile)));
		return query;
	}

	protected void loadPaths(String path) {
		paths = getFilePaths(path);
	}

	public List<String> getFilePaths(String path) {
		final List<String> paths = new ArrayList<String>();
		try {			
			IProject project = FileUtils.findFileForLocation(path).getProject();
			
			if (ignoreExternalPrologFilesProject() && ExternalPrologFilesProjectUtils.getExternalPrologFilesProject().equals(project)) {
				return paths;
			}
			
			project.accept(new IResourceVisitor() {

				@Override
				public boolean visit(IResource resource) throws CoreException {
					if (!(resource instanceof IFile)) 
						return true;
					IFile file = (IFile)resource;
					if (file.getFileExtension() != null && extensions.contains(file.getFileExtension())) {
						try {
							paths.add(Util.quoteAtom(UIUtils.prologFileName(file)));
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
					return false;
				}
				
			});
		} catch (Exception e) {
			e.printStackTrace();
		}
		return paths;
	}
	
	public boolean containsFilePath(String path) {
		return paths.contains(path);
	}
	
	protected boolean ignoreExternalPrologFilesProject() {
		return true;
	}
	
}
