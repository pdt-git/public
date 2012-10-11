package pdt.y.view;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.FileUtils;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

import pdt.y.main.PDTGraphView;
import pdt.y.main.PluginActivator;

public class GlobalGraphPIFLoader extends GraphPIFLoader {
	
	private GlobalView parent;
	private static final String NAME_OF_GLOBAL_HELPING_FILE = "pdt-global-help.graphml";

	public GlobalGraphPIFLoader(PDTGraphView view, GlobalView parent) {
		super(view);
		PrologRuntimeUIPlugin plugin = PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(NAME_OF_GLOBAL_HELPING_FILE);
		this.parent = parent;
	}
	
	@Override
	protected String generateQuery(String focusFileForParsing, File helpFile) {
		final List<String> paths = new LinkedList<String>();
		
		IFile file;
		try {
			file = FileUtils.findFileForLocation(focusFileForParsing);
			file.getProject().accept(new IResourceVisitor() {

				@Override
				public boolean visit(IResource resource) throws CoreException {
					if (!(resource instanceof IFile)) 
						return true;
					IFile file = (IFile)resource;
					if (file.getFileExtension().equals("pl")) {
						paths.add("'" + file.getLocation().makeAbsolute().toString() + "'");
					}
					return false;
				}
				
			});
		} catch (CoreException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		
		String query;
		query = "write_global_to_graphML(" + paths.toString() + ",'" + Util.prologFileName(helpFile) + "').";
		return query;
	}

}
