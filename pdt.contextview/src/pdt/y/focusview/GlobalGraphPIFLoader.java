package pdt.y.focusview;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.ResourceFileLocator;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

import pdt.y.main.PDTGraphView;

public class GlobalGraphPIFLoader extends GraphPIFLoader {
	
	private static final String NAME_OF_GLOBAL_HELPING_FILE = "pdt-global-help.graphml";

	public GlobalGraphPIFLoader(PDTGraphView view, GlobalView parent) {
		super(view);
		PrologRuntimeUIPlugin plugin = PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(NAME_OF_GLOBAL_HELPING_FILE);
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
		} catch (IOException e) {
			e.printStackTrace();
		}

		
		String query;
		query = "write_global_to_graphML(" + paths.toString() + ",'" + Util.prologFileName(helpFile) + "').";
		return query;
	}

}
