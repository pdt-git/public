package org.cs3.jtransformer.internal.decorators;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerImageRepository;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.util.JTUtils;
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

public class JTransformerProjectDecoratorContributor implements ILightweightLabelDecorator, OptionProviderListener{

	private Vector listeners = new Vector();


	
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
			JTransformerPlugin.getDefault().addOptionProviderListener(project,this);

			if(!project.hasNature(JTransformer.NATURE_ID)){
				return;
			}
			
//			String projectInterfaceKey = project.getPersistentProperty(new QualifiedName("", JTransformer.PROLOG_RUNTIME_KEY));
//
//			JTransformerSubscription pifSubscription = new JTransformerSubscription(project,
//					 projectInterfaceKey + "_tempory_project_key",
//					 projectInterfaceKey,
//					// getProject().getName(),
//					// getProject().getName(),
//					"JTransformer factbase of project "
//							+ project.getName(), "JTransformer-tbb");
//			PrologRuntimePlugin.getDefault().getPrologInterface(
//					pifSubscription);
//			
//			JTransformerPlugin.getDefault().setTemporaryProjectSubscription(project,pifSubscription);
			
			//  TODO: this is a bit ugly, or isn't it`?
			String state = JTransformerPlugin.getDefault().getNonPersistantPreferenceValue(project, JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_ACTIVATED);
			
			if(state.equals(JTransformer.FACTBASE_STATE_ACTIVATED)) {
				decoration.addOverlay(JTransformerImageRepository.getImageDescriptor(JTransformerImageRepository.JTRANSFORMER_PROJECT_DECORATION_GREY));
				//JTransformerPlugin.getDefault().setNonPersistantPreferenceValue(project, JTransformer.FACTBASE_STATE_KEY, JTransformer.FACTBASE_STATE_ACTIVATED);
			}else if(state.equals(JTransformer.FACTBASE_STATE_IN_PROCESS)) {
				decoration.addOverlay(JTransformerImageRepository.getImageDescriptor(JTransformerImageRepository.JTRANSFORMER_PROJECT_DECORATION_GREEN));
			}else if(state.equals(JTransformer.FACTBASE_STATE_READY)){
				decoration.addOverlay(JTransformerImageRepository.getImageDescriptor(JTransformerImageRepository.JTRANSFORMER_PROJECT_DECORATION));
			}
		} catch (CoreException e) {
			JTUtils.logAndDisplayUnknownError(e);
		}

	}

	public void addListener(ILabelProviderListener l) {
		//JTransformerPlugin.getDefault().add

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
	
	public void valuesChanged(OptionProviderEvent e) {
		fireLabelProviderChanged();
		
		
	}

	private void fireLabelProviderChanged() {
		final LabelProviderChangedEvent e = new LabelProviderChangedEvent(this);
		Vector clone=new Vector();
		synchronized(listeners){
			clone.addAll(listeners);
		}
		for (Iterator it = clone.iterator(); it.hasNext();) {
			final ILabelProviderListener l = (ILabelProviderListener) it.next();
			UIUtils.getDisplay().asyncExec(new Runnable() {
				public void run() {
					l.labelProviderChanged(e);
				}
			});
		}
	}

}


