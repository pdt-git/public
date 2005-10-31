
package org.cs3.pdt.internal.search;

import java.io.IOException;
import java.util.HashMap;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;

public class PrologSearchQuery implements ISearchQuery {

	private Predicate data;

	private PrologSearchResult result;

	private HashMap fSearchViewStates = new HashMap();

	private IMetaInfoProvider metaInfoProvider;

	public PrologSearchQuery(IMetaInfoProvider provider, Predicate data) {
		this.data = data;
		this.metaInfoProvider=provider;
		result = new PrologSearchResult(this, data);
		

	}

	public IStatus run(IProgressMonitor monitor) {
		try {
			return run_impl(monitor);
		} catch (Throwable t) {
			Debug.report(t);
			return new Status(Status.ERROR,PDT.PLUGIN_ID,42,"Exception caught during search.",t);
		}
	}

	private IStatus run_impl(IProgressMonitor monitor) throws CoreException,
			BadLocationException, IOException {
		if(data==null){
			Debug.error("Data is null!");
			throw new NullPointerException();
		}
		String title = data==null?"oops, data is null?!" : data.getSignature();
		if (false/*FIXME was: data.isModule()*/){
			title += "  Search for modules not supported yet!";
		}
		else{
			PrologSession session;
			
			SourceLocation[] locations = metaInfoProvider.findReferences(data);
			if(locations==null){
				//FIXME: is it realy ok? --lu
				return Status.OK_STATUS;
			}
			for (int i = 0; i < locations.length; i++) {
				SourceLocation location = locations[i];
				if (location.isRowBased){
					String msg = "Sorry, i currently can not handle row-based locations.";
					Debug.warning(msg);
					UIUtils.setStatusErrorMessage(msg);
					continue;
				}
				IRegion resultRegion = new Region(location.offset,location.endOffset-location.offset);
				IFile file = null;
				if(location.isWorkspacePath){
					ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(location.file));
				}
				else{
					file = PDTUtils.findFileForLocation(location.file);
				}
				if(file==null||! file.isAccessible()){
					String msg = "Not found in workspace: "+location.file;
					Debug.warning(msg);
					UIUtils.setStatusErrorMessage(msg);
					continue;
				}
				
				result.addMatch(new Match(file, resultRegion
						.getOffset(), resultRegion.getLength()));
				Debug.debug("Found reference: " + file + ", offset: "
						+ resultRegion.getOffset() + ", length: "
						+ resultRegion.getLength());				
			}
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