/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.core;

import java.util.Set;

import org.cs3.pdt.runtime.Subscription;
import org.cs3.pl.common.OptionProvider;
import org.cs3.pl.common.OptionProviderExtension;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

/**
 * not much yet ;-)
 */
public interface IPrologProject extends OptionProvider,OptionProviderExtension{
    

    /**
     * parses the current value of the sourcePath property and returns
     * a Set containing IContainer objects that represent the
     * source folders that actually exist within the project.
     * @return
     * @throws CoreException
     */
    public Set<IContainer> getExistingSourcePathEntries() throws CoreException;
    
    /**
     * parses the current value of the entry points property and returns
     * a Set containing IFile objects that represent the
     * entry points that actually exit
     * @return
     * @throws CoreException
     */
    public Set<IFile> getExistingEntryPoints() throws CoreException;
    
    /**
     * Checks if the given resource is part of the prolog  source.
     * <p>
     * A folder is regarded as part of the source tree if it exists and is either
     * a source path entry itself or its parent is part of the source tree..
     * <p>
     * a file is regarded as part of the source tree if it exists, is the child of a 
     * folder that is part of the source tree and has the ".pl" file extension.
     * @param resource
     * @return
     * @throws CoreException
     */
    public boolean isPrologSource(IResource resource) throws CoreException;
    
    public boolean isAutoConsulted(IFile file) throws CoreException;
    public void setAutoConsulted(IFile file,boolean val) throws CoreException;

	public PrologInterface getMetadataPrologInterface();
	public Subscription getMetadataSubscription() ;
	public IMetaInfoProvider getMetaInfoProvider() ;
	public OptionProvider getAnnotatorsOptionProvider() throws PrologInterfaceException ;
	public IProject getProject();

	public PrologInterface getRuntimePrologInterface();
	public Subscription getRuntimeSubscription();

	/**
	 * returns the library keys that represent 
	 * the existing source folders 
	 * in this project.
	 * 
	 * @return a set of PrologLibrary instances.
	 * @throws CoreException 
	 */
	public String[] getPrologLibraryKeys() throws CoreException;

	public void updateMarkers() throws CoreException;
	public void updateBuildPath(PrologSession s) throws CoreException, PrologInterfaceException;
    
    
}
