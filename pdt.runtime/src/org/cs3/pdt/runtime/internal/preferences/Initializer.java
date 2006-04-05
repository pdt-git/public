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

/*
 */
package org.cs3.pdt.runtime.internal.preferences;

import java.io.IOException;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.osgi.service.prefs.BackingStoreException;

/**
 * this class tries to guess sensible defaults for most property values.
 */
public class Initializer extends AbstractPreferenceInitializer {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
     */
    public void initializeDefaultPreferences() {
        try {
        	
        	
        		initializeDefaultPreferences_impl();
        	
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t.getMessage(), t);
        }
    }

    /**
     * @throws BackingStoreException
     * @throws IOException
     * @throws InterruptedException
     *  
     */
    private void initializeDefaultPreferences_impl()
            throws BackingStoreException, IOException, InterruptedException {
        PrologRuntimePlugin plugin = PrologRuntimePlugin.getDefault();

        String qualifier = plugin.getBundle().getSymbolicName();

        IScopeContext context = new DefaultScope();

        IEclipsePreferences node = context.getNode(qualifier);
        if (node == null) {
            Debug.error("Häh?!");
        } else {
            Option[] options = plugin.getOptions();
            for (int i = 0; i < options.length; i++) {
                String id = options[i].getId();
                String def = System.getProperty(id, options[i].getDefault());
                node.put(id, def);
            }
            String pifImpl=plugin.getPreferenceValue(PrologRuntime.PREF_PIF_IMPLEMENTATION,null);
            PrologInterfaceFactory factory = PrologInterfaceFactory
                    .newInstance(pifImpl);            
             options = factory.getOptions();
            for (int i = 0; i < options.length; i++) {
                String id = options[i].getId();
                String def = System.getProperty(id, options[i].getDefault());
                if(id==null||def==null){
                		Debug.warning("during preferences default initialization: id="+id+", def="+def);
                }
                node.put(id, def);
            }
            String[] strings = node.keys();
            for (int i = 0; i < strings.length; i++) {
                Debug.info(strings[i] + " --> " + node.get(strings[i], "n.a."));
            }
        }
        
    }
}