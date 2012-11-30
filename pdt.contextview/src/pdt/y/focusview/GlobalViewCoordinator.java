package pdt.y.focusview;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

import pdt.y.main.PDTGraphView;

public class GlobalViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	public GlobalViewCoordinator(ViewBase focusView)
	{
		super(focusView);
		
		PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService()
			.registerConsultListener(this);
	}
	
		
	public void swichFocusView(String path) {
		
		IProject project = getContainingProject(path);
		List<String> paths = getFilePaths(project);
		
		currentFocusView = views.get(project.getName());
		
		if (currentFocusView == null) {
			PDTGraphView pdtGraphView = new PDTGraphView(focusView);
			GraphPIFLoaderBase loader = focusView.createGraphPIFLoader(pdtGraphView);
			loader.setCurrentPath(path);
			loader.setPaths(paths);
			
			currentFocusView = focusView.new FocusViewControl(pdtGraphView, loader);

			refreshCurrentView();
			
			views.put(project.getName(), currentFocusView);
		}

		currentFocusView.recalculateMode();
		focusView.setCurrentFocusView(currentFocusView);
	}


	private IProject getContainingProject(String path) {
		IFile file;
		try {
			file = FileUtils.findFileForLocation(path);
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}		
		return file.getProject();
	}


	private List<String> getFilePaths(IProject project) {
		final List<String> paths = new LinkedList<String>();
		try {			
			project.accept(new IResourceVisitor() {

				@Override
				public boolean visit(IResource resource) throws CoreException {
					if (!(resource instanceof IFile)) 
						return true;
					IFile file = (IFile)resource;
					if (file.getFileExtension().equals("pl")) {
						try {
							paths.add(Util.quoteAtom(Util.prologFileName(file)));
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
					return false;
				}
				
			});
		} catch (CoreException e) {
			e.printStackTrace();
		}
		return paths;
	}


	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return currentFocusView != null 
				&& ((GraphPIFLoaderBase)currentFocusView.getPifLoader()).getPaths().contains(path);
	}
}