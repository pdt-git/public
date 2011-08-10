package pdt.y.view.swt.commands;

import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import pdt.y.main.PDTGraphSwingStandalone;
import pdt.y.main.PluginActivator;

public class GraphPIFAction  extends Action {
	private static final String FILE_TO_CONSULT = "pl_ast_to_graphML";
	private static final String PATH_ALIAS = "pdt_runtime_builder_graphml_creator";
	private static final String NAME_OF_HELPING_FILE = "pdt-yworks-help.graphml";

	static {
		image = ImageDescriptor.createFromImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
	}

	private File helpFile;
	private static ImageDescriptor image;
	private PDTGraphSwingStandalone view;


	public GraphPIFAction(PDTGraphSwingStandalone view) {
		super("Directory Selection",image);
		this.view = view;
		PrologRuntimeUIPlugin plugin=PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(NAME_OF_HELPING_FILE);
		System.out.println("Location of file .graphml file: "+helpFile.toString());
	}

	//	private void getWorkspaceLocation() {
	//		IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
	//		workspaceLocation = wsRoot.getLocation().toString();
	//		System.out.println(workspaceLocation);
	//	}
	//

	@Override
	public void run() {
		PrologInterface pif=PluginActivator.getDefault().getPrologInterface();
		String prologNameOfFileToConsult = PATH_ALIAS+"("+FILE_TO_CONSULT+")";

		String folderToParse = selectFolderToParse();
		try {
			PrologSession session = pif.getSession(PrologInterface.LEGACY);

			session.queryOnce("consult("+prologNameOfFileToConsult+").");

			List<String> folderList = collectPrologFilesInWorkspace();
			//folderToParse = convertJavaListToPrologListString(folderList);

			String query = "pl_test_graph(['"+folderToParse+"'],'"+Util.prologFileName(helpFile)+"').";
			session.queryOnce(query);

			view.loadGraph(helpFile.toURI().toURL());
		} catch (PrologException e1) {
			e1.printStackTrace();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
	}

	private String selectFolderToParse() {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		// File standard dialog
		DirectoryDialog folderDialog = new DirectoryDialog(shell);
		folderDialog.setText("Select Folder");
		// Open Dialog and save result of selection
		String selectedPath = folderDialog.open();
		File file = new File(selectedPath);
		String prologNameOfSelectedPath = Util.prologFileName(file);

		return prologNameOfSelectedPath;
	}

	private List<String> collectPrologFilesInWorkspace() {
		final List<String> fileList = new ArrayList<String>();
		try {
			ResourcesPlugin.getWorkspace().getRoot().accept(new IResourceVisitor(){

				@Override
				public boolean visit(IResource resource) throws CoreException {
					if ((resource instanceof IProject) && resource.isAccessible()) {
						IPath path = resource.getLocation();
						File file = path.toFile();

						fileList.add(Util.prologFileName(file));
						System.out.println("Folder: "+file.toString());
						return false;
					}
					return true;
				}
			});
		} catch (CoreException e) {
		};
		System.out.println("fertig mit sammeln");
		return fileList;
	}

	private String convertJavaListToPrologListString(List<String> stringList) {
		StringBuffer buffer = new StringBuffer();
		for (String elem : stringList) {
			buffer.append(elem);
			buffer.append("','");
		}
		buffer.delete(buffer.length()-3, buffer.length());
		System.out.println("String to write:"+buffer.toString());
		return buffer.toString();
	}
}
