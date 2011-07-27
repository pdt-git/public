package org.cs3.pdt.core.internal.decorators;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.internal.ImageRepository;
import org.cs3.pl.common.OptionProviderEvent;
import org.cs3.pl.common.OptionProviderListener;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;

public class SourcePathDecorator implements ILightweightLabelDecorator, OptionProviderListener{

	private Vector<ILabelProviderListener> listeners = new Vector<ILabelProviderListener>();

	public SourcePathDecorator() {
	}

	@Override
	public void decorate(Object element, IDecoration decoration) {
		if(!(element instanceof IResource)){
			return;
		}
		IResource resource = (IResource)element;
		IProject project = resource.getProject();
		if(!project.isOpen()){
			return;
		}
		try {
			if(!project.hasNature(PDTCore.NATURE_ID)){
				return;
			}
			IPrologProject prologProject = (IPrologProject)project.getNature(PDTCore.NATURE_ID);
			
			//TODO: this is a bit ugly, or isn't it`?
			prologProject.addOptionProviderListener(this);
			
			if(prologProject.getExistingSourcePathEntries().contains(resource)){
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.SOURCEPATH_DECORATION));				
			}
		} catch (CoreException e) {
//			UIUtils.logAndDisplayError(PDTCorePlugin.getDefault().getErrorMessageProvider(), UIUtils.getDisplay().getActiveShell(), 
//					PDTCore.ERR_UNKNOWN, PDTCore.CX_UNKNOWN, e);
		}
		
		
	}

	@Override
	public void addListener(ILabelProviderListener l) {
		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}
		}
		
	}

	@Override
	public void dispose() {
		
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener l) {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
			}
		}
		
	}

	@Override
	public void valuesChanged(OptionProviderEvent e) {
		String[] ids= e.ids;
		for (int i = 0; i < ids.length; i++) {
			if(PDTCore.PROP_SOURCE_PATH.equals(ids[i])){
				fireLabelProviderChanged();
			}	
		}
		
		
	}

	private void fireLabelProviderChanged() {
		LabelProviderChangedEvent e = new LabelProviderChangedEvent(this);
		Vector<ILabelProviderListener> clone=new Vector<ILabelProviderListener>();
		synchronized(listeners){
			clone.addAll(listeners);
		}
		for (Iterator<ILabelProviderListener> it = clone.iterator(); it.hasNext();) {
			ILabelProviderListener l = it.next();
			l.labelProviderChanged(e);
		}
		
	}

}
