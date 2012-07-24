/* $LICENSE_MSG$ */

package pdt.y.internal;

import java.net.URL;
import java.util.HashMap;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

import pdt.y.main.PluginActivator;

public class ImageRepository {

	private static HashMap<String, Image> cache = new HashMap<String, Image>();
	
	public static final String HIERARCHY = "hierarchy.ico";
	public static final String ORGANIC = "organic.ico";
	
	public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PluginActivator.getDefault().getBundle().getEntry("/icons/" + icon);
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

