package org.cs3.pdt.internal;


import org.cs3.pdt.internal.views.lightweightOutline.PDTTreeElement;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;

public class PrologTreeElementLabelDecorator extends LabelProvider implements ILightweightLabelDecorator{
	

	@Override
	public void decorate(Object element, IDecoration decoration) {
		if(!(element instanceof PDTTreeElement)){
			return;
		}
		if (element instanceof Predicate) {
			Predicate predicate = (Predicate)element;
			if (predicate.isMultifile())
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PE_MULTIFILE));
		}
	}
	
	
	@Override
	public void addListener(ILabelProviderListener listener) {
		}

	@Override
	public void dispose() {
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {

	}

}
