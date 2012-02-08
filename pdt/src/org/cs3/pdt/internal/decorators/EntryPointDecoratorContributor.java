package org.cs3.pdt.internal.decorators;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.actions.ToggleEntryPointAction;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.OptionProviderEvent;
import org.cs3.pl.common.OptionProviderListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;

public class EntryPointDecoratorContributor implements ILightweightLabelDecorator, OptionProviderListener {

	private Vector<ILabelProviderListener> listeners = new Vector<ILabelProviderListener>();

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
	public void decorate(Object element, IDecoration decoration) {
		if(!(element instanceof IFile)){
			return;
		}
		
		PDTPlugin.getDefault().addDecorator(this);
		
		IFile file = (IFile) element;
		try {
			String isEntryPoint = file.getPersistentProperty(ToggleEntryPointAction.KEY);
			
			if (isEntryPoint != null && isEntryPoint.equalsIgnoreCase("true")) {
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PROLOG_ENTRY_POINT));
				decoration.addSuffix(" [entry point]");
			}
			
		} catch (CoreException e) {
			e.printStackTrace();
		}
		
		
	}
	
	@Override
	public void valuesChanged(OptionProviderEvent e) {
		fireLabelProviderChanged();
	}

	private void fireLabelProviderChanged() {
		final LabelProviderChangedEvent e = new LabelProviderChangedEvent(this);
		Vector<ILabelProviderListener> clone=new Vector<ILabelProviderListener>();
		synchronized(listeners){
			clone.addAll(listeners);
		}
		for (Iterator<ILabelProviderListener> it = clone.iterator(); it.hasNext();) {
			final ILabelProviderListener l = (ILabelProviderListener) it.next();
			UIUtils.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					l.labelProviderChanged(e);
				}
			});
		}
	}

}
