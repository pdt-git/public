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

	public final static String NOGUITRACER = "noguitracer.gif";
	
    public final static String PE_PUBLIC = "public_co.gif";

    public final static String PE_HIDDEN = "protected_co.gif";

    public final static String PE_MULTIFILE = "multifile.gif";

    public final static String PE_VARIABLE = "variable.gif";

    public final static String PE_MODULE = "module_2.gif";

    public final static String PE_CLAUSE = "clause.gif";

    public final static String PE_TERM = "term.gif";

    private static HashMap cache = new HashMap();

    public static final String PE_ATOM = "atom.gif";

	public static final String BREAK = "break.gif";

	public static final String RESTART = "restart.gif";

    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PDTPlugin.getDefault().getBundle().getEntry("/icons/" + icon);
        return ImageDescriptor.createFromURL(url);
    }

    public static final Image getImage(String icon) {
        Image image = (Image) cache.get(icon);
        if (image == null) {
            image = getImageDescriptor(icon).createImage();
            cache.put(icon, image);
        }
        return image;
    }
}
