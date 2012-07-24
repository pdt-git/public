/* $LICENSE_MSG$(ld) */

/*
 */
package org.cs3.pdt.internal.actions;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Set;

import org.cs3.pdt.PDTUtils;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.PrologInterfaceRegistry;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.session.PrologSession;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class AbortTestAction implements IObjectActionDelegate {
	
	private int port;

	@Override
	public void run(IAction action) {
		// get input from active editor
		IEditorInput input = UIUtils.getActiveEditor().getEditorInput();
		if (input == null) {
			Debug.warning("Consult action triggered, but active editor input is null.");
			return;
		}
		PrologSession s = null;
		try {
			PrologInterfaceRegistry registry = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
			Set<String> keys = registry.getAllKeys();
			for (String key : keys) {
				PrologInterface pif = registry.getPrologInterface(key);
				String threadId = readThreadIdFromFile();
				int pifPort = (Integer)pif.getClass().getMethod("getPort").invoke(pif);
				if(pifPort == port){
					s=pif.getSession();
					if(threadId != null){
						s.queryOnce("catch(thread_signal('"+threadId +"', abort),_,fail)");
					}
					return;
				}
			}

			Debug.warning("No PrologInterface with port "+ port + " found.");
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if(s!=null)
				s.dispose();
		}

	}

	private String readThreadIdFromFile() throws IOException {
		File file = new File(System.getProperty("java.io.tmpdir")+File.separator + "pdt_current_unit_test_thread.txt");
		if(!file.exists()){
			return null;
		}
		BufferedReader reader = new BufferedReader(new FileReader(file));
		port = Integer.parseInt(reader.readLine());
		String tid = reader.readLine();
		reader.close();
		return tid;
	}
	
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		File file = new File(System.getProperty("java.io.tmpdir")+File.separator + "pdt_current_unit_test_thread.txt");
		action.setEnabled(file.exists());
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		
	}
}

