/*
 */
package org.cs3.pdt;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.cs3.pdt.editors.PLEditor;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.DefaultMetaInfoProvider;
import org.cs3.pl.metadata.PrologElementData;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;

/**
 */
public class PDTPrologHelper  implements
        IPrologHelper {
    private static final boolean windowsPlattform = System
    .getProperty("os.name").indexOf("Windows") > -1;
    private String pdtModulePrefix="";
    private IPrologInterface prologInterface;
  
public PDTPrologHelper(IPrologInterface prologInterface, String pdtModulePrefix) {
    this.prologInterface = prologInterface;
    this.pdtModulePrefix=pdtModulePrefix;
}
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pdt.IPrologHelper#consult(org.eclipse.core.resources.IFile)
     */
    public void consult(IFile file) throws SessionException {
             consult(file.getLocation().toString());
    }

    public static String makeFilenameSWIConform(String file) {
        if (windowsPlattform){
            return file.toLowerCase().replace('\\', '/');
        }
        return file;
    }

    public void consult( String filename) throws SessionException{
	    PrologSession session = prologInterface.getSession();	 
        session.query("consult('" + makeFilenameSWIConform(filename) + "')");
       session.dispose();
    }
	

   
   

    public  boolean assertFact( String text) throws SessionException{
        PrologSession session = prologInterface.getSession();
        Hashtable r = session.query("assert("+text+")");
        session.dispose();
        return r!=null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pdt.IPrologHelper#showSourceLocation(org.cs3.pl.metadata.SourceLocation)
     */
    public void showSourceLocation(SourceLocation loc)  {
        IFile file = null;
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        IWorkspaceRoot root = workspace.getRoot();
        IPath fpath = new Path(loc.file);
        IFile[] files = root.findFilesForLocation(fpath);
        if (files == null || files.length == 0) {
            Debug.warning("Not in Workspace: " + fpath);
            return;
        }
        if (files.length > 1) {
            Debug.warning("Mapping into workspace is ambiguose:" + fpath);
            Debug.warning("i will use the first match found: " + files[0]);
        }
        file = files[0];
        if (!file.isAccessible()) {
            Debug.warning("The specified file \"" + file
                    + "\" is not accessible.");
            return;
        }
        IWorkbenchPage page = PDTPlugin.getDefault().getActivePage();
        IEditorPart part;
        try {
            part = IDE.openEditor(page, file);
        } catch (PartInitException e) {
            Debug.report(e);
            return;
        }
        if (part instanceof PLEditor) {
            PLEditor editor = (PLEditor) part;
            editor.gotoLine(loc.line);
        }
    }
    /**
     * Retrieves the location or a predicate in the running prolog prozess.
     * 
     * @param functor
     *                    The functor of the predicate.
     * @param arity
     *                    The arity of the predicate.
     * 
     * @return null, if no location can be found.
     * @throws SessionException
     * @see SourceLocation
     */
    public SourceLocation getLocation( String functor, int arity, String filename) throws SessionException {
        //		return (SourceLocation) catchedCall(ROLE, "getLocation", new
        // Object[]{
        //				functor, new Integer(arity), filename});
        //		if (!isCompleted())
        //		abort();
        PrologSession session = prologInterface.getSession();
        Hashtable solution = session.query(pdtModulePrefix + "get_file_pos('"
                + filename + "', " + functor + ", " + arity + ",File,Pos,_,_)");
        if (solution == null){
            session.dispose();
            return null;
        }
        SourceLocation location = new SourceLocation();
        location.file = /* removeQuotes( */solution.get("File").toString()/* ) */;
        location.line = Integer.parseInt(solution.get("Pos").toString());
        Debug
                .debug(
                        "getLocation solution: " + location.file + ", "
                                + location.line);
        session.dispose();
        return location;
    
    }

    /**
     * @param file
     * @return
     * @throws SessionException
     * @throws NumberFormatException
     */
    public PrologElementData[] getPredicatesWithPrefix(String module,
            String prefix) throws NumberFormatException, SessionException {
        return getPredicatesWithPrefix(module, prefix, null);
    }

    /**
     * //TODO: add pos, len(?), dyn, multi.. Retrieves Predicates with prefix
     * <i>prefix </i>. There to ways to restrict the returned elements: module
     * and filename
     * 
     * 
     * @param prefix
     * @param module
     *                    can be null -> no restriction on the module
     * @param filename
     *                    can be null -> no restriction on the declaring file
     * @module Module name or null, if module is not defined.
     * @return
     * @throws SessionException
     * @throws NumberFormatException
     */
    public PrologElementData[] getPredicatesWithPrefix(String module,
            String prefix, String filename) throws NumberFormatException, SessionException {
        //return
        // (PrologElementData[])predicates.get(makeFilenameSWIConform(filename));
    
        if (module == null)
            module = "_";
        if (filename == null)
            filename = "_";
        PrologSession session = prologInterface.getSession();
        Hashtable result = session.query(pdtModulePrefix+ "find_pred('"
                + filename + "','" + prefix + "', " + module
                + ",Name,Arity,Public)");
        List list = new ArrayList();
        while (result != null) {
            boolean pub = Boolean.valueOf(result.get("Public").toString())
                    .booleanValue();
            PrologElementData data = new PrologElementData(result.get("Name")
                    .toString(), Integer.parseInt(result.get("Arity")
                    .toString()), pub, 0, 0, false, false);
            list.add(data);
            result =session.next();
        }
        session.dispose();
        return (PrologElementData[]) list.toArray(new PrologElementData[0]);
    }

    public PrologElementData[] retrievePrologElements(String file) throws SessionException {
        PrologSession session = prologInterface.getSession();
        Hashtable result = session.query("bagof([Pos_,Len_],"
                + "meta_data"
                + "('"
                + file
                + "',Module,Name,Arity,Public,Pos_,Len_, Dyn,Mul),[[Pos,Len]|_])");
        List list = new ArrayList();
        while (result != null) {
            //debug(result.get("Name").toString()+" - PUBLIC-
            // "+Boolean.valueOf(result.get("Public").toString()).booleanValue());
            PrologElementData data = new PrologElementData(result.get("Name")
                    .toString(), java.lang.Integer.parseInt(result.get("Arity")
                    .toString()), Boolean.valueOf(
                    result.get("Public").toString()).booleanValue(),
                    java.lang.Integer.parseInt(result.get("Pos").toString()),
                    java.lang.Integer.parseInt(result.get("Len").toString()),
                    result.get("Dyn").toString().equals("1"), result.get("Mul")
                            .toString().equals("1"));
            list.add(data);
            result = session.next();
        }
        session.dispose();
        return (PrologElementData[]) list.toArray(new PrologElementData[0]);
    }


}
