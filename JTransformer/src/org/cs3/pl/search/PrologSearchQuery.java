/*
 * Created on 23.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.search;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Hashtable;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologElementData;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.internal.filebuffers.FileBuffersPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.FindReplaceDocumentAdapter;
import org.eclipse.jface.text.IRegion;
import org.eclipse.search.internal.ui.text.FileMatch;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;


public class PrologSearchQuery implements ISearchQuery {

	
	
	private PrologElementData data;
	private PrologSearchResult result;
	private HashMap fSearchViewStates = new HashMap();

	public PrologSearchQuery(PrologElementData data) {
		this.data = data;
		result= new PrologSearchResult(this,data);
		// FIXME: Internal FileSearchPage, must implement AbstractTextSearchViewPage
	//(new PrologSearchViewPage()).setInput(result,null);
		
		
	}
	
	public IStatus run(IProgressMonitor monitor) {

		try {
			String title = data.getSignature();
			if(data.isModule())
				title += "  Search for modules not supported yet!";
			if(!data.isModule()) {
				IPrologClient client = PrologManager.getInstance().getHiddenClient();
//				IPrologManager manager = PDTPlugin.getDefault().getPrologManager();
				
				Hashtable solution = client.query(PDTPlugin.MODULEPREFIX
						+ "get_references(" + data.getSignature()
						+ ",FileName,Line,Name,Arity)");
				int pos = 10;
				while (solution != null) {
					HashMap attributes = new HashMap();
					String fileName = solution.get("FileName").toString();
					java.io.File ioFile;
					ioFile = (new File(fileName)).getCanonicalFile();
					Path path = new Path(ioFile.getAbsolutePath());
					IFile iFile = PDTPlugin.getDefault()
							.getFileSystemManager().fileForLocation(path);
					FileBuffersPlugin.getDefault().getFileBufferManager().connect(iFile.getFullPath(),null);

					int line = Integer.parseInt(solution.get("Line").toString());

					ITextFileBuffer buffer = FileBuffersPlugin.getDefault().getFileBufferManager().getTextFileBuffer(iFile.getFullPath());
					IRegion region = buffer.getDocument().getLineInformation(line);
					FindReplaceDocumentAdapter findAdapter = new FindReplaceDocumentAdapter(buffer.getDocument());
					
					//TODO: provide correct RegEx and find ALL occurances in the current predicate (by now it is just the first)
					// and add all rule heads to the result, too - modify get_references/5.
					IRegion resultRegion = findAdapter.find(region.getOffset(),data.getLabel(),true,true,true,false);
					
					if (iFile != null && resultRegion != null) {
						
						//query.addMatch(new FileMatch(iFile, 0, Integer.parseInt(line)));
						
						result.addMatch(new FileMatch(iFile, resultRegion.getOffset(), resultRegion.getLength()));
					} else {
						String msg = "Cannot find the file'" + path
								+ "' in the workspace.";
						System.out.println(msg);
						PDTPlugin.getDefault().setStatusErrorMessage(msg);
					}

					solution = client.next();
				}
			}
			//manager.stop();
			//fView.searchFinished();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			Debug.report(e);
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return Status.OK_STATUS;
	}

	public String getLabel() {
		return "Prolog Query: " + data.getSignature();
	}

	public boolean canRerun() {
		return true;
	}

	public boolean canRunInBackground() {
		return false;
	}

	public ISearchResult getSearchResult() {	
		return result;
	}
	
}