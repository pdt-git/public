package pdt.pefgraph.internal.views;

import org.cs3.pl.common.Util;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

public class PEFGraphLabelProvider implements ILabelProvider{

	public Image getImage(Object element) {		
		return null;
	}

	public String getText(Object element) {
		if(element instanceof PEFNode){
			PEFNode node = (PEFNode) element;
			if(node.getLabels().length>0){
				return node.getId()+":"+node.getType() +"\n"+ Util.splice(node.getLabels(),"\n");
			}
			return node.getId()+":"+node.getType();
		}
		if(element instanceof PEFEdge){
			PEFEdge edge = (PEFEdge) element;
			return edge.label;
		}
		return null;
	}

	public void addListener(ILabelProviderListener listener) {
		;
		
	}

	public void dispose() {
		;
		
	}

	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
		
	}

}
