package importorganizer.views;


import java.io.IOException;
import java.util.Map;

import org.cs3.prolog.common.QueryUtils;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.part.ViewPart;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 */

public class SampleView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "importorganizer.views.SampleView";

	private Text text;

	/*
	 * The content provider class is responsible for
	 * providing objects to the view. It can wrap
	 * existing objects in adapters or simply return
	 * objects as-is. These objects may be sensitive
	 * to the current input of the view, or ignore
	 * it and always show the same content 
	 * (like Task List, for example).
	 */
	 

	/**
	 * The constructor.
	 */
	public SampleView() {
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent) {
		text = new Text(parent, SWT.MULTI | SWT.V_SCROLL);
		text.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.keyCode == SWT.F9) {
					PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();
					IEditorInput editorInput = getSite().getPage().getActiveEditor().getEditorInput();
					if (!(editorInput instanceof IFileEditorInput)) {
						return;
					}
					String prologFileName;
					try {
						prologFileName = UIUtils.prologFileName(((IFileEditorInput)editorInput).getFile());
						Map<String, Object> result = pif.queryOnce(QueryUtils.bT("import_organizer:imports_for_file", Util.quoteAtom(prologFileName), "Imports"));
						text.setText(result.get("Imports").toString());
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
				}
			}
		});
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		text.setFocus();
	}
}