package pdt.pefgraph.internal.views;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import pdt.pefgraph.internal.Activator;

public class SetVisibilityDialog extends Dialog {

	private PrologInterface pif;
	private Label idLabel;
	private Text idText;
	

	protected SetVisibilityDialog(Shell parentShell, PrologInterface pif) {
		super(parentShell);
		this.pif = pif;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		idLabel = new Label(composite, SWT.NONE);
		idLabel.setText("PEF Identifier: ");
		idText = new Text(composite, SWT.BORDER);
				
		return composite;

	}
	
	@Override
	protected void okPressed() {
		PrologSession s=null;
		try {
			s=pif.getSession();
			String query = "pef_graph_set_visible("+idText.getText()+",true),pef_graph_refresh";
			s.queryOnce(query);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if(s!=null){
				s.dispose();
			}
			this.close();
		}
	}
}
