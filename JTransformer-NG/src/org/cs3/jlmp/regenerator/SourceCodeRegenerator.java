/*
 * Created on 10.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jlmp.regenerator;

import java.io.IOException;
import java.io.StringBufferInputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;

/**
 * @author Stefan Schulz
 * 
 * This class (re-)generates Sourcecode for classes in Prolog factbase.
 * 
 * Use generateDirtyClasses() to replace/generate the files for alle dirty
 * classes.
 */
public class SourceCodeRegenerator implements ISourceRegenerator {

    PrologInterface pif;

    public SourceCodeRegenerator(PrologInterface pif) throws IOException {
        this.pif = pif;
    }

    /**
     * Returns the Java source code of the file which defines the class fqn.
     * 
     * @param fqn
     *                full qualified names (without quotes)
     * @return
     */
    public String generateClassString(String fqn) {
        PrologSession s = pif.getSession();
        try {
            Map table = s
                    .queryOnce("fullQualifiedName(CID,'"
                            + fqn
                            + "'),classDefT(CID, _,_,_), getToplevel(CID,TID),gen_tree(TID, Text)");
            return table.get("Text").toString();
        } finally {
            s.dispose();
        }

    }

    public void generateClass(int id) throws CoreException {
        ResourcesPlugin rp = ResourcesPlugin.getPlugin();
        PDTPlugin pdt = PDTPlugin.getDefault();
        IWorkspaceRoot iwr = ResourcesPlugin.getWorkspace().getRoot();

        String name;
        //IProject project = pdt.getProject();//getCurrentProject();
        PrologSession s = pif.getSession();
        Map ht = null;
        try {
            ht = s
                    .queryOnce("classDefT("
                            + id
                            + ", PID,_,_), toplevelT(TID, PID, SOURCEPATH, MLIST), member("
                            + id + ", MLIST), gen_tree(TID, TEXT)");
        } finally {
            s.dispose();
        }
        if (ht == null) {
            Debug.error("Error occured while (re-)generating type with ID "
                    + id + ". Probably the toplevel fact was not be found.");
            return;
        }
        IPath path = new Path(ht.get("SOURCEPATH").toString());
        String input = ht.get("TEXT").toString();

        IFile file = iwr.getFile(path);

        writeWorkspaceFile(iwr, file, input);

    }

    /**
     * @param path
     * @throws CoreException
     */
    private void ensureFolderExistance(IWorkspaceRoot iwr, IPath path)
            throws CoreException {
        if (!iwr.exists(path) && path.segmentCount() > 0) {
            ensureFolderExistance(iwr, path.removeLastSegments(1));
            IFolder folder = iwr.getFolder(path);
            folder.create(true, false, null);
            folder.refreshLocal(IFile.DEPTH_INFINITE, null);
        }
    }

    //	public void generateClass(String fqn) throws CoreException{
    //		ResourcesPlugin rp = ResourcesPlugin.getPlugin();
    //		PDTPlugin pdt = PDTPlugin.getDefault();
    //			
    //		IWorkspaceRoot iwr = ResourcesPlugin.getWorkspace().getRoot();
    //				
    //		String name;
    //		String _package;
    //		
    //		if (fqn.lastIndexOf(".") < 0){
    //			name = fqn;
    //			_package = "null";
    //		} else {
    //			name = fqn.substring(fqn.lastIndexOf(".") + 1);
    //			_package = fqn.substring(0, fqn.lastIndexOf("."));
    //		}
    //		
    //		//IProject project = pdt.getProject();//getCurrentProject();
    //		
    //		Hashtable ht = pif.query("classDefT(CID, PID, '" + name + "', _),
    // packageT(PID, '" + _package + "'), toplevelT(TID, PID, SOURCEPATH,
    // MLIST), member(CID, MLIST), gen_tree(TID, TEXT)");
    //		
    //
    //		writeWorkspaceFile(iwr, ht, fqn);
    //	}

    /**
     * Iterates through the dirty classes, and (re-)generates a new Java source
     * file for each class / interface that has been changed. If all files have
     * been successfully replaced/created it removes all
     * 
     * <pre>
     * dirty_tree / 1
     * </pre>
     * 
     * facts from the fact base.
     *  
     */

    public void generateDirtyClasses() {
        //ld: lost while porting to jt ng. what was this line supposed to do?
        //PDTPlugin.getDefault().getPrologConsole().setFocus();
        PrologSession s = pif.getSession();
        try {
            for (Iterator i = getDirtyClasses().iterator(); i.hasNext();) {
                generateClass(((Integer) i.next()).intValue());
            }
            s.queryOnce("retractall(dirty_tree(_))");
        } catch (CoreException e) {
            Debug.report(e);
            throw new RuntimeException (e);
        } finally {
            s.dispose();
        }

    }

    /**
     * gets a list of the IDs of dirty classes from Prolog.
     * 
     * @return a list of dirty classes
     */

    private Set getDirtyClasses() {
        Set set = new HashSet();
        List l = null;
        PrologSession s = pif.getSession();        
        try{
            l= s.queryAll("dirty_tree(X), enclClass(X, CLASS), classDefT(CLASS, _,_,_)");    
        }
        finally{
            s.dispose();
        }
        for (Iterator it = l.iterator(); it.hasNext();) {
            Map m = (Map) it.next();
            Integer inter = new Integer(Integer.parseInt(m.get("CLASS")
                    .toString()));            
                set.add(inter);            
        }
        return set;
    }

    /**
     * @param iwr
     * @param target
     * @param input
     * @param file
     * @throws CoreException
     */
    private void writeWorkspaceFile(IWorkspaceRoot iwr, IFile file, String input)
            throws CoreException {

        if (!iwr.exists(file.getFullPath())) {
            ensureFolderExistance(iwr, file.getFullPath().removeLastSegments(1));

            file.create(new StringBufferInputStream(""), true, null);
        }

        file.setContents(new StringBufferInputStream(input), true, true,
                new NullProgressMonitor());
        file.refreshLocal(IFile.DEPTH_INFINITE, new NullProgressMonitor());
    }

    /*
     * Will also remove all meta information tracked by the prolog system like
     * removed files or added files.
     * 
     * @see org.cs3.jlmp.regenerator.ISourceRegenerator#getAffectedTypes()
     */
    public IAffectedFile[] getAffectedFiles() {

        List l = new ArrayList();

        l.addAll(retrieveDeletedFiles());

        //List dirtyClasses = getDirtyClasses();

        changedFiles(l, "created_file", IAffectedFile.CREATED);

        changedFiles(l, "modified_file", IAffectedFile.CHANGED);

        //		pif.query("retract_api_meta_data");

        return (IAffectedFile[]) l.toArray(new IAffectedFile[0]);
    }

    /**
     * @param l
     */
    private void changedFiles(List l, String pred, int status) {
        PrologSession s = pif.getSession();
        List results=null;
        try{
            results=s.queryAll(pred + "(Filename,Src)");
        }
        finally{
            s.dispose();
        }
        for (Iterator it = results.iterator(); it.hasNext();) {
            Map m = (Map) it.next();
            String filename = (String) m.get("Filename");
            String src = (String) m.get("Src");
            l.add(new AffectedFile(filename, status,
                    new ITextChange[] { new TextChange(src) }));
        }        
    }

    /*
     * Will undo all changes made by CTs since the last rollback, or the last
     * modification of Java code in the Java Editor.
     */

    public void rollback() {
        PrologSession s = pif.getSession();        
        try{
            s.queryOnce("rollback, retract_api_meta_data");
        }
        finally{
            s.dispose();
        }
    }

    /*
     * Commits all changes made by CTs. No rollback possible afterwards.
     */

    public void commit() {
        PrologSession s = pif.getSession();        
        try{
            s.queryOnce("retract_api_meta_data");
        }
        finally{
            s.dispose();
        }
    }

    /**
     * @param l
     */
    private Set retrieveDeletedFiles() {
        Set result=new HashSet();
        List l=null;
        PrologSession s = pif.getSession();        
        try{        
            l = s.queryAll("deleted_file(Filename)");
        }finally{
            s.dispose();            
        }
        for (Iterator it = l.iterator(); it.hasNext();) {
            Map m = (Map) it.next();
            result.add(new AffectedFile((String) m.get("Filename"),
                    IAffectedFile.REMOVED));            
        }
        //pif.query("retractall(deleted_file(_))");
        return result;
    }
}
