package org.cs3.pdt;

import org.cs3.pl.metadata.PrologElementData;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.model.IWorkbenchAdapter;


public class PrologElement extends PrologElementData implements IAdaptable {

	
	protected int imageDescriptorId;

	
	public static PrologElement[] fromData(PrologElementData[] data){
		PrologElement[] elems =new PrologElement[data.length];
		for (int i = 0; i < data.length; i++) {
			elems[i] = new PrologElement(data[i]);
		}
		return elems;
	}

	public PrologElement(PrologElementData data) {
		this(data.getLabel(), data.getArity(), data.isPublic(), data.getPosition(),data.getLength(),data.isDynamic(), data.isMultifile());
	}
	
	/**
	 * @param name
	 * @param arity
	 */
	public PrologElement(String label, int arity, boolean isPublic, int pos, int length,boolean dynamic, boolean multifile) {
		super(label, arity, isPublic, pos, length, dynamic,multifile);
		if (!isPublic)
			imageDescriptorId = ImageRepository.PE_HIDDEN;
	}


	public Object getAdapter(Class adapter) {
		if (adapter.equals(IWorkbenchAdapter.class))
		return new IWorkbenchAdapter() {

			public Object[] getChildren(Object o) {
				return new Object[0];
			}

			public ImageDescriptor getImageDescriptor(Object object) {
				return ImageRepository.getImage(imageDescriptorId); //$NON-NLS-1$ 
			}

			public String getLabel(Object o) {
				return getSignature();
			}

			public Object getParent(Object o) {
				return null;
			}
			
		};
		return null;
	}


	/**
	 * @return
	 */
	public String getSignature() {
		return label + "/" + arity;
	}

	public String toString() {
		return getSignature()+ " ( pos " + pos + ")";
	}


	public void setPosition(int pos) {
		this.pos = pos;
	}


	public int getPosition() {
		return pos;
	}


	public void setArity(int arity) {
		this.arity = arity;
	}


	public int getArity() {
		return arity;
	}

	public void setDynamic(boolean dynamic) {
		this.dynamic = dynamic;
	}

	public boolean isDynamic() {
		return dynamic;
	}

	public void setMultifile(boolean multifile) {
		this.multifile = multifile;
	}

	public boolean isMultifile() {
		return multifile;
	}
		
}
