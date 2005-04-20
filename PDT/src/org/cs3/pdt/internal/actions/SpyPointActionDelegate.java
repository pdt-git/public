package org.cs3.pdt.internal.actions;

import java.util.Hashtable;
import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.UIUtils;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class SpyPointActionDelegate extends TextEditorAction {
    /**
     *  
     */
    public SpyPointActionDelegate(ITextEditor editor) {
        super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),SpyPointActionDelegate.class.getName(),editor); //$NON-NLS-1$
    }

    /**
     * @see IWorkbenchWindowActionDelegate#run
     */

    Hashtable spypred = new Hashtable();

    public void run() {
        PDTPlugin plugin = PDTPlugin.getDefault();
        UIUtils.getDisplay().asyncExec(new Runnable() {
            public void run() {
                PDTPlugin plugin = PDTPlugin.getDefault();

                PLEditor editor = (PLEditor) UIUtils.getActiveEditor();
                PrologSession session = null;
                session = plugin.getPrologInterface().getSession();
                String pred;

                PrologElementData data;
                try {
                    data = editor.getSelectedPrologElement();
                } catch (BadLocationException e2) {
                    Debug.report(e2);
                    return;
                }
                if (data == null) {
                    MessageDialog
                            .openInformation(editor.getEditorSite().getShell(),
                                    "PDT Plugin", 
                                    "Cannot locate a predicate at the specified location."); 
                    return;

                }
                if (data.isModule()) {
                    MessageDialog.openInformation(editor.getEditorSite()
                            .getShell(), "PDT Plugin", 
                            "Cannot spy on a module " +": "+ data.getSignature()  //$NON-NLS-2$
                                    + "."); //$NON-NLS-1$
                    return;

                }
                pred = data.getSignature();

                if (spypred.get(pred) != null) {
                    try {
                        session.query("nospy(" + pred + ")"); //$NON-NLS-1$ //$NON-NLS-2$
                    } catch (PrologException e1) {
                        Debug.report(e1);
                        return;
                    }
                    spypred.remove(pred);
                } else {
                    try {
                        session.query("spy(" + pred + ")"); //$NON-NLS-1$ //$NON-NLS-2$
                    } catch (PrologException e1) {
                        Debug.report(e1);
                        return;
                    }
                    spypred.put(pred, pred);
                }

            }
        });
    }

    



    /**
     * @see IWorkbenchWindowActionDelegate#dispose
     */
    public void dispose() {
    }

  
}