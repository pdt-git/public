/*
 * Created on 10.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.regenerator;

import java.io.IOException;
import java.io.StringBufferInputStream;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;


/**
 * @author Stefan Schulz
 *
 * This class (re-)generates Sourcecode for classes in Prolog factbase.
 * 
 * Use generateDirtyClasses() to replace/generate the files for alle dirty classes. 
 */
public class SourceCodeRegenerator implements ISourceRegenerator{
	
	IPrologClient manager;
	
	public SourceCodeRegenerator() throws IOException{
		manager = PrologManager.getInstance().getHiddenClient();
	}
	
	
	/**
	 * Returns the Java source code of the file which defines the class fqn. 
	 * 
	 * @param fqn full qualified names (without quotes)
	 * @return
	 */
	public String generateClassString(String fqn){
		
		Hashtable table = manager.query("fullQualifiedName(CID,'"+fqn+"'),classDefT(CID, _,_,_), getToplevel(CID,TID),gen_tree(TID, Text)");

		return table.get("Text").toString();
	}

	
	public void generateClass(int id) throws CoreException {
		ResourcesPlugin rp = ResourcesPlugin.getPlugin();
		PDTPlugin pdt = PDTPlugin.getDefault();
		IWorkspaceRoot iwr = ResourcesPlugin.getWorkspace().getRoot();
				
		String name;		
		//IProject project = pdt.getProject();//getCurrentProject();
		
		Hashtable ht = manager.query("classDefT(" + id +", PID,_,_), toplevelT(TID, PID, SOURCEPATH, MLIST), member(" + id +", MLIST), gen_tree(TID, TEXT)");
		if (ht == null) {
			Debug.error("Error occured while (re-)generating type with ID " + id +". Probably the toplevel fact was not be found.");
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
	private void ensureFolderExistance(IWorkspaceRoot iwr, IPath path) throws CoreException {
		if(!iwr.exists(path) && path.segmentCount() > 0) {
			ensureFolderExistance(iwr,path.removeLastSegments(1));
			IFolder folder = iwr.getFolder(path);
			folder.create(true, false, null);
			folder.refreshLocal(IFile.DEPTH_INFINITE,null);
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
//		Hashtable ht = manager.query("classDefT(CID, PID, '" + name + "', _), packageT(PID, '" + _package + "'), toplevelT(TID, PID, SOURCEPATH, MLIST), member(CID, MLIST), gen_tree(TID, TEXT)");
//		
//
//		writeWorkspaceFile(iwr, ht, fqn);
//	}
	
	/**
	 * Iterates through the dirty classes, and (re-)generates a new Java source file
	 * for each class / interface that has been changed. If all files have been
	 * successfully replaced/created it removes
	 * all <pre>dirty_tree/1</pre> facts from the fact base.
	 * 
	 */
	
	
	public void generateDirtyClasses() {
		PDTPlugin.getDefault().getPrologConsole().setFocus();
		
		try {
			for (Iterator i = getDirtyClasses().iterator(); i.hasNext(); ){
				generateClass(((Integer)i.next()).intValue());
			}
			manager.query("retractall(dirty_tree(_))");
		} catch (CoreException e) {
			Debug.report(e);
		}
	}

	/**
	 * gets a list of the IDs of dirty classes from Prolog.
	 * @return a list of dirty classes
	 */
	
	private List getDirtyClasses(){
		List l = new ArrayList();
		Hashtable ht = manager.query("dirty_tree(X), enclClass(X, CLASS), classDefT(CLASS, _,_,_)");
		
		while (ht != null){
			Integer inter = new Integer(Integer.parseInt(ht.get("CLASS").toString()));
		
			if (!l.contains(inter))
				l.add(inter);
			
			ht = manager.next();
		}
		return l;
	}


	/**
		 * @param iwr
		 * @param target
		 * @param input
		 * @param file
		 * @throws CoreException
		 */
		private void writeWorkspaceFile(IWorkspaceRoot iwr, IFile file, String input) throws CoreException {
			
			if (!iwr.exists(file.getFullPath())) {
				ensureFolderExistance(iwr,file.getFullPath().removeLastSegments(1));
	
				file.create(new StringBufferInputStream(""), true, null);
			}
				
			
			file.setContents(new StringBufferInputStream(input),true, true, new NullProgressMonitor());
			file.refreshLocal(IFile.DEPTH_INFINITE, new NullProgressMonitor());
		}


	/* 
	 * Will also remove all meta information tracked by the prolog system
	 * like removed files or added files.
	 * 
	 * @see org.cs3.pl.regenerator.ISourceRegenerator#getAffectedTypes()
	 */
	public IAffectedFile[] getAffectedFiles() {
		
		List l = new ArrayList();
		
		l.addAll(retrieveDeletedFiles());
		
		//List dirtyClasses = getDirtyClasses();
		
		changedFiles(l,"created_file",IAffectedFile.CREATED);

		changedFiles(l,"modified_file",IAffectedFile.CHANGED);
	
//		manager.query("retract_api_meta_data");
		
		return (IAffectedFile[])l.toArray(new IAffectedFile[0]);
	}


	/**
	 * @param l
	 */
	private void changedFiles(List l, String pred, int status) {
		Hashtable ht = manager.query(pred+"(Filename,Src)");
		
		while (ht != null){
			String filename = (String)ht.get("Filename");
			String src = (String)ht.get("Src");
			l.add(new AffectedFile(filename,status,
					new ITextChange[] {new TextChange(src)}));
			ht = manager.next();
		}
	}

	/*
	 * Will undo all changes made by CTs since the last rollback, 
	 * or the last modification of Java code in the Java Editor.
	 */
	
	public void rollback() {
		manager.query("rollback, retract_api_meta_data");
	}

	/*
	 * Commits all changes made by CTs. No rollback possible afterwards.
	 */
	
	public void commit() {
		manager.query("retract_api_meta_data");
	}

	
	/**
	 * @param l
	 */
	private List retrieveDeletedFiles() {
		List l = new ArrayList();
		Hashtable ht = manager.query("deleted_file(Filename)");
		while (ht != null){
			l.add(new AffectedFile((String)ht.get("Filename"),IAffectedFile.REMOVED));
			ht = manager.next();
		}
		//manager.query("retractall(deleted_file(_))");
		return l;
	}
}
