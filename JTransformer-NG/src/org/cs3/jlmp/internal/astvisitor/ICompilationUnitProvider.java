/*
 * Created on 02.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.jlmp.internal.astvisitor;

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 * Implementations of this interface provide the application with easy 
 * access to the CompilationUnit objects of a Project.
 * 
 * <p>
 * Implementations of this Interface must provide a Constructor
 * with an IProject as sole argument</p>
 * 
 * <p>
 * This will cause the CUProvider to search for java sourcefiles in
 * the given Project. getSourceFiles() and getIterator will throw
 * an IllegalStateException when called on an uninitialized CUProvider.
 * 
 */

public interface ICompilationUnitProvider {
	
	
	/**
	 * Get all found Java Source files.
	 * 
	 * <p>
	 * This will return a list of all java source files found during 
	 * initialization. Note that any changes made to the workspace after
	 * the initialization might not be reflected yet. If in doubt, call 
	 * init again.  
	 * 
	 * @return A list of objects implementing IFile
	 */
	//public List getSourceFiles();
	
	/**
	 * Get an iterator over all CUs.
	 * <p>
	 * This returns an iterator that should be used to iterate over the
	 * CUs that correspond to the java source files found during initialization.
	 * Note that any changes made to the workspace after
	 * the initialization might not be reflected yet. If in doubt, call 
	 * init again.
	 * 
	 * <p>
	 * The order of iteration is guarantied to be the same as the corresponding
	 * iteration order in the list of files returned by the getSourceFilesMethod.
	 * 
	 * @return an Iterator over all CompilationUnits
	 * @deprecated use ICompilationUnitProvider#getICUIterator() instead;
	 */
	//public Iterator getCUIterator();
	
	/**
	 * Get an iterator over all ICompilationUnits.
	 * 
	 * <p>
	 * This returns an iterator that should be used to iterate over the
	 * ICUs that correspond to the java source files found during initialization.
	 * Note that any changes made to the workspace after
	 * the initialization might not be reflected yet. If in doubt, call 
	 * init again.
	 * 
	 * <p>
	 * Note: We do not iterate over CompilationUnit (the DOM /AST node) 
	 * instances any more, since we need the ICompilationUnit in the FactGenerator
	 * to access the source code of a node (e.g. literals). The step of
	 * retrieving a WorkingCopy from an ICompilationUnit aswell as
	 * parsing an ASTNode (the "old" CompilationUnit) from  it is
	 * trivial enough to happen elsewhere (and under better control of the
	 * client code).
	 * <p>
	 * 
	 * @return an instance of ICompilationUnit. <b>No guaranties are made on wether
	 *  this instance is in WorkingCopy mode!</b>
	 */
	//public Iterator getICUIterator();
	/**
	 * create an ICompilationUnit from an IFile.
	 * <p>
	 * If the file belongs to a project that does not have
	 * a Java ProjectNature, this method will add that nature to the 
	 * project, and assure some basic build properties (jdk classes in 
	 * CLASSPATH etc.)
	 * <p>
	 * <b>NOTE however that the returned ICompilationUnit is not neccessarily in  
	 * WorkingCopy-mode yet!</b>
	 * @param a file.
	 * @return a ICompilationUnit (potentialy not in WorkingCopy mode yet.)
	 */
	public ICompilationUnit createICompilationUnit(IFile file);
}
