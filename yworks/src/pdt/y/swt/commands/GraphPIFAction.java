package pdt.y.swt.commands;

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

import pdt.y.main.Activator;
import pdt.y.main.PDTGraphSwing;

public class GraphPIFAction  extends Action {
	private static final String NAME_OF_HELPING_FILE = "pdt-yworks-help.graphml";

	static {
		image = ImageDescriptor.createFromImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER/*.IMG_OBJ_PROJECT*/));
	}
	  
//	private static File helpFile = new File("z:/pdt.git/yworks/help.graphml");
	private File helpFile;
    private static ImageDescriptor image;
	private PDTGraphSwing view;
//	private String workspaceLocation;


	public GraphPIFAction(PDTGraphSwing view) {
		super("PIF",image);
		this.view = view;
		PrologRuntimeUIPlugin plugin=PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(NAME_OF_HELPING_FILE);
		System.out.println(helpFile.toString());
//		
//		getWorkspaceLocation();
//		File ahelpFile = new File(workspaceLocation+"/"+NAME_OF_HELPING_FILE);
//		System.out.println(ahelpFile);
//		System.out.println(ahelpFile.toString());
		
	}


//	private void getWorkspaceLocation() {
//		IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
//		workspaceLocation = wsRoot.getLocation().toString();
//		System.out.println(workspaceLocation);
//	}
//	
	
	@Override
	public void run() {
		PrologInterface pif=Activator.getDefault().getPrologInterface();
//		File file = getFileToConsult();
//		String prologNameOfFileToConsult = Util.prologFileName(file);
		String prologNameOfFileToConsult = "pdt_runtime_builder_graphml_creator(pl_ast_to_graphML)";

		String folderToParse = selectFolderToParse();
		try {
//			if (pif.isDown()) {
//				pif.start();
//			}	else{
//			}
			PrologSession session = pif.getSession(PrologInterface.LEGACY);
			//session.queryOnce("win_window_pos([show(true)])");
			
			session.queryOnce("consult("+prologNameOfFileToConsult+").");
			session.queryOnce("pl_test(['"+folderToParse+"'],'"+Util.prologFileName(helpFile)+"').");
			view.loadGraph(helpFile.toURI().toURL());
			//pif.stop();
		} catch (PrologException e1) {
			e1.printStackTrace();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
	}


//	private File getFileToConsult() {
//		PrologRuntimeUIPlugin plugin=PrologRuntimeUIPlugin.getDefault();
//		ResourceFileLocator locator = plugin.getResourceLocator();
////		Class.getResource
////		File file=new File("z:/pdt.git/pdt.runtime.ui/library/pdt/xref/pl_ast_to_graphML.pl");
//		return file;
//	}
	
	
//	private void consultNeededFiles(PrologInterface prologInterface) {
//		String contributionKey = "yworks.contribution.key";
//		List<BootstrapPrologContribution> libraryList = PrologRuntimePlugin.getDefault().getBootstrapList(contributionKey);
//		for (BootstrapPrologContribution library : libraryList) {
//			if (!prologInterface.getBootstrapLibraries().contains(library)) {
//					prologInterface.getBootstrapLibraries().add(library);
//					if (prologInterface.isUp()) {
//						PrologSession session = null;
//						try {
//							session = prologInterface.getSession(PrologInterface.LEGACY);
//							
//							String consult = library.getPrologInitStatement();
//							Debug.debug("consult " + consult + ", from " + library);
//							session.queryOnce(consult);
//						} catch (PrologInterfaceException e) {
//							Debug.report(e);
//							if (session != null)
//								session.dispose();
//						}
//					}
//				}
//			}
//		
//	}

	
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
