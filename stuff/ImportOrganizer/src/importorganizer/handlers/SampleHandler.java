package importorganizer.handlers;

import java.util.List;
import java.util.Map;

import org.cs3.prolog.connector.DefaultSubscription;
import org.cs3.prolog.connector.Subscription;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class SampleHandler extends AbstractHandler {
	/**
	 * The constructor.
	 */
	public SampleHandler() {
	}
	
	private static Subscription s = new DefaultSubscription("Ne Id", "My Yap Process", "wow", "wow2");

	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {
		if (MessageDialog.openQuestion(HandlerUtil.getActiveShell(event), "X", "?")) {
			InputDialog d = new InputDialog(HandlerUtil.getActiveShell(event), "", "", "true", null);
			d.open();
			PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(s, "YAP");
			List<Map<String, Object>> queryAll;
			try {
				queryAll = pif.queryAll(d.getValue());
				MessageDialog.openInformation(HandlerUtil.getActiveShell(event), "Result", queryAll.toString());
			} catch (PrologInterfaceException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(s, "YAP");
			PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().setActivePrologInterface(pif);
		}
		return null;
	}
}
