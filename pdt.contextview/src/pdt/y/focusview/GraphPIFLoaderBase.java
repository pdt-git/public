/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package pdt.y.focusview;

import java.io.File;
import java.net.MalformedURLException;
import java.util.Map;

import org.cs3.prolog.common.ResourceFileLocator;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;

import pdt.y.main.PDTGraphView;

public abstract class GraphPIFLoaderBase {

	protected File helpFile;
	protected PDTGraphView view;
	private PrologInterface pif;
	//private ExecutorService executor = Executors.newCachedThreadPool();

	public GraphPIFLoaderBase(PDTGraphView view, String helpFileName) {
		this.view = view;
		
		PrologRuntimeUIPlugin plugin = PrologRuntimeUIPlugin.getDefault();
		ResourceFileLocator locator = plugin.getResourceLocator();
		helpFile = locator.resolve(helpFileName);
	}

	protected abstract String generateQuery(File helpFile);
	
	public Map<String, Object> loadGraph() {

		try {
			helpFile.delete();
			pif = getActivePif();
			if (pif != null) {
				String query = generateQuery(helpFile);
				Map<String, Object> output = sendQueryToCurrentPiF(query);
					
				// query =
				// "collect_ids_for_focus_file(FocusId,Files,CalledPredicates,Calls)";
				// Map<String, Object> result = sendQueryToCurrentPiF(query);
				// result.get("FocusId");

				new UIJob("Layouting") {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						try {
							doLoadFile();
						} catch (MalformedURLException e) {
							Debug.rethrow(e);
						}
						return Status.OK_STATUS;
					}
				}.schedule();
				
				return output;
			}
		} catch (PrologException e1) {
			e1.printStackTrace();
		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
		return null;
	}

	protected void doLoadFile() throws MalformedURLException {
		view.loadGraph(helpFile.toURI().toURL());
	};
	
	public Map<String, Object> sendQueryToCurrentPiF(String query)
		throws PrologInterfaceException {
	
		PrologSession session = pif.getSession(PrologInterface.LEGACY);
		Map<String, Object> result = session.queryOnce(query);
		return result;
	}

	public PrologInterface getActivePif() {
		PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();
		return pif;
	}

	public abstract void setCurrentPath(String currentPath);

	public abstract String getCurrentPath();
}
