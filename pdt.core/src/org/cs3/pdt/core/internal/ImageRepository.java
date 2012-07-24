/* $LICENSE_MSG$(ld) */

/*
 * Created on 05.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.core.internal;

import java.net.URL;
import java.util.HashMap;

import org.cs3.pdt.core.PDTCorePlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

/**
 * @author xproot
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
public class ImageRepository {

	private static HashMap<String, Image> cache = new HashMap<String, Image>();
	
	public final static String SOURCEPATH_DECORATION = "pdt_container_decorator.png";

	public static final String ENTRYPOINT_DECORATION = "pdt_file_decorator.png";

	
    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PDTCorePlugin.getDefault().getBundle().getEntry("/icons/" + icon);
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

