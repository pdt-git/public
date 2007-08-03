package pdt.pefgraph.internal.views;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Shell;

public abstract class SetVisibilityAction extends Action{
	
	

	protected abstract PrologInterface getPrologInterface();
	protected abstract Shell getShell();
	
	@Override
	public void run() {
		Shell shell = getShell();
		PrologInterface pif = getPrologInterface();
		if(pif==null||shell==null||shell.isDisposed()){
			return;
		}
		new SetVisibilityDialog(shell,pif).open();	
	}
	
	
}
