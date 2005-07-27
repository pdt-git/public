/*
 * Created on 05.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.console.internal;

import java.net.URL;
import java.util.HashMap;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

public class ImageRepository {

	public final static String GUITRACER = "guitracer.gif";

	public final static String CLEAR = "clear.gif";

	public final static String NOGUITRACER = "noguitracer.gif";
	
    
    private static HashMap cache = new HashMap();

    
	public static final String BREAK = "break.gif";

	public static final String RESTART = "restart.gif";

    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PrologConsolePlugin.getDefault().getBundle().getEntry("/icons/" + icon);
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
