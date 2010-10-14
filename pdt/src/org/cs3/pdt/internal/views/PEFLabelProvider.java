package org.cs3.pdt.internal.views;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

public class PEFLabelProvider implements ILabelProvider {

	@Override
	public Image getImage(Object element) {
		if(element instanceof PEFNode){
			return getImage(((PEFNode)element));
		}
		if(element instanceof IAdaptable){
			PEFNode node = (PEFNode) ((IAdaptable)element).getAdapter(PEFNode.class);
			if(node!=null){
				return getImage(node);
			}
		}
		return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT);
	}

	Image getImage(PEFNode node){
		if("pef_toplevel".equals(node.getType())){
			return ImageRepository.getImage(ImageRepository.PE_CLAUSE);
		}
		else if("pef_predicate".equals(node.getType())){
			if(node.getTags().contains("public")){
				return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
			}
			
			return ImageRepository.getImage(ImageRepository.PE_HIDDEN);
		}
		return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT);
	}
	
	@Override
	public String getText(Object element) {
		if(element instanceof PEFNode){
			return ((PEFNode)element).getLabel();
		}
		if(element instanceof IAdaptable){
			PEFNode node = (PEFNode) ((IAdaptable)element).getAdapter(PEFNode.class);
			if(node!=null){
				return node.getLabel();
			}
		}
		return element.toString();
	}

	@Override
	public void addListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

}
