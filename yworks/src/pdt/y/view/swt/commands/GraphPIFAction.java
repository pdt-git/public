package pdt.y.view.swt.commands;

import java.io.File;
import java.net.MalformedURLException;

import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import pdt.y.main.PluginActivator;
import pdt.y.main.PDTGraphSwingStandalone;

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
		super("PIF",image);
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
			session.queryOnce("pl_test(['"+folderToParse+"'],'"+Util.prologFileName(helpFile)+"').");
			
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
}
