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
 * Created on 05.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal;

import java.net.URL;
import java.util.HashMap;

import org.cs3.pdt.PDTPlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

/**
 * @author xproot
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
public class ImageRepository {

	public final static String GUITRACER = "guitracer.gif";

	public final static String CLEAR = "clear.gif";

	public final static String NOGUITRACER = "noguitracer.gif";
	
	public final static String FILTER = "filter.gif";
	
	public final static String SORT = "alphab_sort_co.gif";
	
    public final static String PE_PUBLIC = "public_co.gif";

    public final static String PE_BUILT_IN = "built_in.gif";

    public final static String VERIFIED_MATCH = "public_co.gif";

    public final static String PE_HIDDEN = "protected_co.gif";
    public final static String POTENTIAL_MATCH = "protected_co.gif";

    public final static String PE_MULTIFILE = "multifile.gif";

    public final static String PE_VARIABLE = "variable.gif";

    public final static String PE_MODULE = "module_2.gif";

    public final static String PE_CLAUSE = "clause.gif";

    public final static String PE_TERM = "term.gif";
    
    public final static String FILE = "prolog_file.gif";
    
    public final static String MATCH = "sample.gif";
    
    public final static String PACKAGE = "package_obj.gif";

    private static HashMap<String, Image> cache = new HashMap<String, Image>();

	

    public static final String PE_ATOM = "atom.gif";

	public static final String BREAK = "break.gif";

	public static final String RESTART = "restart.gif";

	public static final String UNRESOLVED_PRED_MATCH = "private_co.gif";

    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PDTPlugin.getDefault().getBundle().getEntry("/icons/" + icon);
        return ImageDescriptor.createFromURL(url);
    }

    public static final Image getImage(String icon) {
        Image image = cache.get(icon);
        if (image == null) {
            image = getImageDescriptor(icon).createImage();
            cache.put(icon, image);
        }
        return image;
    }
}
