package org.cs3.pdt.internal.views;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

public class PrologElementLabelProvider implements ILabelProvider {

	public Object[] getChildren(Object o) {
		/*
		 * FIXME right now there is no sufficiently efficient way to implement
		 * this direction on a per-element basis. So tree-structured views need
		 * to use their own content provider. See PrologElementContentProvier
		 * --lu
		 */
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
	 */
	public ImageDescriptor getImageDescriptor(Object object) {

		if (object instanceof String) {// FIXME: need a module type --lu
			return ImageRepository
					.getImageDescriptor(ImageRepository.PE_MODULE);
		} else if (object instanceof Predicate) {
			Predicate p = (Predicate) object;
			boolean exported = p.isPublic();
			return ImageRepository
					.getImageDescriptor(exported ? ImageRepository.PE_PUBLIC
							: ImageRepository.PE_HIDDEN);
		}
		if (object instanceof Clause) {
			return ImageRepository
					.getImageDescriptor(ImageRepository.PE_CLAUSE);
		}

		String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
		return PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(
				imageKey);

	}

	public String getLabel(Object o) {
		if (o instanceof Predicate) {
			Predicate p = (Predicate) o;
			return p.getLabel() + "/" + p.getArity();
		} else if (o instanceof Clause) {
			Clause c = (Clause) o;
			return c.getLabel() + "/" + c.getArity();
		}
		return o.toString();
	}

	public Object getParent(Object o) {
		if (o instanceof Predicate) {
			Predicate p = (Predicate) o;
			return p.getModule();
		} else if (o instanceof Clause) {
			Clause c = (Clause) o;
			return c.getPredicate();
		}
		return null;
	}

	public Image getImage(Object object) {

		if (object instanceof String) {// FIXME: need a module type --lu
			return ImageRepository.getImage(ImageRepository.PE_MODULE);
		} else if (object instanceof Predicate) {
			Predicate p = (Predicate) object;
			boolean exported = p.isPublic();
			return ImageRepository
					.getImage(exported ? ImageRepository.PE_PUBLIC
							: ImageRepository.PE_HIDDEN);
		}
		if (object instanceof Clause) {
			return ImageRepository.getImage(ImageRepository.PE_CLAUSE);
		}

		String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
		return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);

	}

	public String getText(Object element) {
		return getLabel(element);
	}

	public void addListener(ILabelProviderListener listener) {

	}

	public void dispose() {

	}

	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {

	}

}
