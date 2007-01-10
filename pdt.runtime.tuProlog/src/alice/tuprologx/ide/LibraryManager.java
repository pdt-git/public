/*
 * tuProlog - Copyright (C) 2001-2004  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprologx.ide;

import java.util.ArrayList;

import alice.tuprolog.InvalidLibraryException;
import alice.tuprolog.Prolog;

/**
 * A dynamic manager for tuProlog libraries.
 *
 * @author	<a href="mailto:giulio.piancastelli@studio.unibo.it">Giulio Piancastelli</a>
 * @version	1.1 - 27-may-05
 */

public final class LibraryManager {

    /** The Prolog engine referenced by the Library Manager. */
    private Prolog engine;
    /** Stores classnames for managed libraries. */
    private ArrayList libraries;

    public LibraryManager() {
    		libraries = new ArrayList();
    }

    /**
     * Set the engine to be referenced by the library manager.
     *
     * @param engine The engine to be referenced by the library manager.
     */
    public void setEngine(Prolog engine) {
        this.engine = engine;
        initialize();
        //engine.addLibraryListener(this);
    }
    
    /**
     * Initialize the repository for managed libraries using the
	 * standard libraries which come loaded with the tuProlog engine.
     */
    void initialize() {
    		String[] loadedLibraries = engine.getCurrentLibraries();
    		for (int i = loadedLibraries.length - 1; i >= 0; i--)
			libraries.add(loadedLibraries[i]);
	}

    /**
     * Get the engine referenced by the library manager.
     *
     * @return the engine referenced by the library manager.
     */
    public Prolog getEngine() {
        return engine;
    }

    /**
     * Check if a library is loaded into the Prolog engine.
     *
     * @param libraryClassname The complete name of the library class to check.
     * @return true if the library is loaded into the engine, false otherwise.
     */
    public boolean isLibraryLoaded(String libraryClassname) {
        return (engine.getLibrary(libraryClassname) != null);
    }

    /**
     * Add a library to the manager.
     *
     * @param libraryClassname The name of the .class of the library to be added.
     * @throws ClassNotFoundException if the library class cannot be found.
     * @throws InvalidLibraryException if the library is not a valid tuProlog library.
     */
    public void addLibrary(String libraryClassname) throws ClassNotFoundException, InvalidLibraryException {
        // Class library = Class.forName(libraryClassname);
    		Class library = getClass().getClassLoader().loadClass(libraryClassname);
         if (library.getSuperclass().equals(alice.tuprolog.Library.class))
         	libraries.add(libraryClassname);
         else
         	throw new InvalidLibraryException(libraryClassname,-1,-1);
    }

    /**
     * Get the libraries managed by the library manager.
     *
     * @return The libraries managed by the library manager as an array of
     * <code>Object</code>s.
     */
    public Object[] getLibraries() {
        return libraries.toArray();
    }

    /**
     * Load a library from the Library Manager into the engine.
     *
     * @param library The library to be loaded into the engine.
     * @throws InvalidLibraryException
     */
    public void loadLibrary(String library) throws InvalidLibraryException {
        engine.loadLibrary(library);
    }

    /**
     * Unload a library from the Library Manager out of the engine.
     *
     * @param library The library to be unloaded out of the engine.
     * @throws InvalidLibraryException
     * @throws EngineRunningException
     */
    public void unloadLibrary(String library) throws InvalidLibraryException {
        engine.unloadLibrary(library);
    }
    
    /**
     * Check if a library is contained in the manager.
     * 
     * @param library The name of the library we want to check the load status on.
     * @since 1.3.0
     */
    public boolean contains(String library) {
    		return libraries.contains(library);
    }

} // end LibraryManager class