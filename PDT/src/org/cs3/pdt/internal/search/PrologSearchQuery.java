/*
 * Created on 23.08.2004
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pdt.internal.search;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.FindReplaceDocumentAdapter;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;

public class PrologSearchQuery implements ISearchQuery {

    private PrologElementData data;

    private PrologSearchResult result;

    private HashMap fSearchViewStates = new HashMap();

    public PrologSearchQuery(PrologElementData data) {
        this.data = data;
        result = new PrologSearchResult(this, data);
        // FIXME: Internal FileSearchPage, must implement
        // AbstractTextSearchViewPage
        //(new PrologSearchViewPage()).setInput(result,null);

    }
    public IStatus run(IProgressMonitor monitor) {
        try{
            return run_impl(monitor);
        }
        catch(Throwable t){
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }
    private IStatus run_impl(IProgressMonitor monitor) throws CoreException, BadLocationException, IOException {

        
            String title = data.getSignature();
            if (data.isModule())
                title += "  Search for modules not supported yet!";
            if (!data.isModule()) {
                PrologSession client;
                client = PDTPlugin.getDefault()
                        .getPrologInterface().getSession();
                //				
                List solutions = client.queryAll(PDTPlugin.MODULEPREFIX
                        + "get_references(" + data.getSignature()
                        + ",FileName,Line,Name,Arity)");
                int pos = 10;
                for (Iterator iter = solutions.iterator(); iter.hasNext();) {
                    Map solution = (Map) iter.next();
                    HashMap attributes = new HashMap();
                    String fileName = solution.get("FileName").toString();
                    if(fileName.startsWith("'")){
                        fileName=fileName.substring(1,fileName.length()-1);
                    }
                    java.io.File ioFile;
                    ioFile = (new File(fileName)).getCanonicalFile();
                    Path path = new Path(ioFile.getAbsolutePath());

                    IWorkspace workspace = ResourcesPlugin.getWorkspace();
                    IWorkspaceRoot root = workspace.getRoot();

                    IFile[] files = root.findFilesForLocation(path);
                    if (files == null || files.length == 0) {
                        Debug.warning("Not in Workspace: " + path);
                        continue;
                    }
                    if (files.length > 1) {
                        Debug.warning("Mapping into workspace is ambiguose:"
                                + path);
                        Debug.warning("i will use the first match found: "
                                + files[0]);
                    }
                    IFile iFile = files[0];
                    if (!iFile.isAccessible()) {
                        Debug.warning("The specified file \"" + iFile
                                + "\" is not accessible.");
                        continue;
                    }

                    int line = Integer
                            .parseInt(solution.get("Line").toString());
                    
                    String originalContents = Util.toString(iFile.getContents());
                    IDocument document = new Document(originalContents);
                    IRegion region = document.getLineInformation(
                            line);
                    FindReplaceDocumentAdapter findAdapter = new FindReplaceDocumentAdapter(
                            document);

                    //TODO: provide correct RegEx and find ALL occurances in
                    // the current predicate (by now it is just the first)
                    // and add all rule heads to the result, too - modify
                    // get_references/5.
                    IRegion resultRegion = findAdapter.find(region.getOffset(),
                            data.getLabel(), true, true, true, false);

                    if (iFile != null && resultRegion != null) {

                        //query.addMatch(new FileMatch(iFile, 0,
                        // Integer.parseInt(line)));

                        result.addMatch(new Match(iFile, resultRegion
                                .getOffset(), resultRegion.getLength()));
                    } else {
                        String msg = "Cannot find the file'" + path
                                + "' in the workspace.";
                        System.out.println(msg);
                        PDTPlugin.getDefault().setStatusErrorMessage(msg);
                    }

                    //solution = client.next();
                }
            }
            //manager.stop();
            //fView.searchFinished();
        
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