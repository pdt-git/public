package org.cs3.pdt.core.internal.decorators;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.internal.ImageRepository;
import org.cs3.pdt.ui.util.UIUtils;
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

	private Vector listeners = new Vector();

	public void decorate(Object element, IDecoration decoration) {
		if(!(element instanceof IResource)){
			return;
		}
		IResource resource = (IResource)element;
		IProject project = resource.getProject();
		try {
			if(!project.hasNature(PDTCore.NATURE_ID)){
				return;
			}
			IPrologProject prologProject = (IPrologProject)project.getNature(PDTCore.NATURE_ID);
			prologProject.addOptionProviderListener(this);
			if(prologProject.getExistingSourcePathEntries().contains(resource)){
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.SOURCEPATH_DECORATION));				
			}
		} catch (CoreException e) {
			UIUtils.logAndDisplayError(PDTCorePlugin.getDefault().getErrorMessageProvider(), UIUtils.getDisplay().getActiveShell(), 
					PDTCore.ERR_UNKNOWN, PDTCore.CX_UNKNOWN, e);
		}
		
		
	}

	public void addListener(ILabelProviderListener l) {
		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}
		}
		
	}

	public void dispose() {
		
	}

	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	public void removeListener(ILabelProviderListener l) {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
			}
		}
		
	}

	public void valueChanged(OptionProviderEvent e) {
		if(PDTCore.PROP_SOURCE_PATH.equals(e.id)){
			fireLabelProviderChanged();
		}
		
	}

	private void fireLabelProviderChanged() {
		LabelProviderChangedEvent e = new LabelProviderChangedEvent(this);
		Vector clone=new Vector();
		synchronized(listeners){
			clone.addAll(listeners);
		}
		for (Iterator it = clone.iterator(); it.hasNext();) {
			ILabelProviderListener l = (ILabelProviderListener) it.next();
			l.labelProviderChanged(e);
		}
		
	}

}
