package pdt.y.swt.commands;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

public class LoadCommandHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		Shell shell = HandlerUtil.getActiveWorkbenchWindow(event).getShell();
		// File standard dialog
		FileDialog fileDialog = new FileDialog(shell);
		fileDialog.setText("Select File");
		fileDialog.setFilterExtensions(new String[] { "*.graphml" });
		fileDialog.setFilterNames(new String[] { "GraphML Format(*.graphml)" });
		// Open Dialog and save result of selection
		String selected = fileDialog.open();
		System.out.println(selected);
		return null;
	}

}