/*
 * Created on 05.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal;

import org.eclipse.jface.resource.ImageDescriptor;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ImageRepository {
	
	public final static int PE_PUBLIC = 0;
	public final static int PE_HIDDEN = 1;
	//public final static int PE_MULTIFILE =2;
	public final static int PE_VARIABLE =3;
	
	private final static ImageDescriptor[] images = {
			ImageDescriptor.createFromFile(PrologElement.class, "icons/public_co.gif"),
			ImageDescriptor.createFromFile(PrologElement.class, "icons/protected_co.gif"),
			ImageDescriptor.createFromFile(PrologElement.class, "icons/private_co.gif"),
			ImageDescriptor.createFromFile(PrologElement.class, "icons/variable.gif")};

	public static final ImageDescriptor getImage(int id){
		return images[id];
	}
}
