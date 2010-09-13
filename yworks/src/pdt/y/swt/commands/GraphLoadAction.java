package pdt.y.swt.commands;

import java.io.File;
import java.net.MalformedURLException;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import pdt.y.main.PDTGraphSwing;

public class GraphLoadAction  extends Action {
	static {
		image = ImageDescriptor.createFromImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_SAVE_EDIT));
	}
	  
    private static ImageDescriptor image;
	private PDTGraphSwing view;

	public GraphLoadAction(PDTGraphSwing view) {
		super("Load",image);
		this.view = view;
	}
	
	@Override
	public void run() {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		// File standard dialog
		FileDialog fileDialog = new FileDialog(shell);
		fileDialog.setText("Select File");
		fileDialog.setFilterExtensions(new String[] { "*.graphml" });
		fileDialog.setFilterNames(new String[] { "GraphML Format(*.graphml)" });
		// Open Dialog and save result of selection
		String selectedFile = fileDialog.open();
		File file = new File(selectedFile);
		
		if(selectedFile == null) 
			return;
		
		
		try {
			view.loadGraph(file.toURI().toURL());
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}

		
		
		
	}
}
