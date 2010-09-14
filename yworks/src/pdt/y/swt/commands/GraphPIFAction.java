package pdt.y.swt.commands;

import java.io.File;
import java.net.MalformedURLException;

import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import pdt.y.main.Activator;
import pdt.y.main.PDTGraphSwing;

public class GraphPIFAction  extends Action {
	static {
		image = ImageDescriptor.createFromImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_PROJECT));
	}
	  
	private static File helpFile = new File("z:/pdt.git/graphml/help.graphml");
    private static ImageDescriptor image;
	private PDTGraphSwing view;

	public GraphPIFAction(PDTGraphSwing view) {
		super("PIF",image);
		this.view = view;
	}
	
	
	@Override
	public void run() {
		PrologInterface pif=Activator.getDefault().getRegistry().getPrologInterface("GraphML");
		System.out.println("PIF:"+pif);
		File file = getFileToConsult();
		String folderToParse = selectFolderToParse();
		System.out.println("Folder to parse:"+folderToParse);
		try {
			System.out.println("Help file:"+helpFile.toString());
			if (pif.isDown()) {
				System.out.println("starting PIF");
				pif.start();
			}	else{
				System.out.println("Pif is up");
			}
			System.out.println("Help file:"+helpFile.toString());
			PrologSession session = pif.getSession();
			System.out.println("Help file:"+helpFile.toString());
			session.queryOnce("consult("+file.toURI().toString()+").");
			session.queryOnce("pl_test(["+folderToParse+"],"+helpFile.toString()+").");
			view.loadGraph(helpFile.toURI().toURL());
			pif.stop();
		} catch (PrologException e1) {
			e1.printStackTrace();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}


	}


	private File getFileToConsult() {
//		PrologRuntimeUIPlugin plugin=PrologRuntimeUIPlugin.getDefault();
//		ResourceFileLocator locator = plugin.getResourceLocator();
//		Class.getResource
		File file=new File("z:/pdt.git/pdt.runtime.ui/library/pdt/xref/pl_ast_to_graphML.pl");
		return file;
	}
	
	private String selectFolderToParse() {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		// File standard dialog
		DirectoryDialog folderDialog = new DirectoryDialog(shell);
		folderDialog.setText("Select Folder");
		// Open Dialog and save result of selection
		String selectedPath = folderDialog.open();
		
		return selectedPath;
	}
}
