package org.cs3.pdt.internal.decorators;

import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.OptionProviderEvent;
import org.cs3.pl.common.OptionProviderListener;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.DecorationContext;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.IDecorationContext;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;

public class PDTConsultDecoratorContributor implements ILightweightLabelDecorator, OptionProviderListener {

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
		
		PDTPlugin.getDefault().setConsultDecorator(this);
		
		IFile file = (IFile) element;
		
		// get active pif from console
		PrologInterface currentPif = PDTUtils.getActiveConsolePif();
		
		if (currentPif == null) {
			return;
		}
		
		// check if file is consulted
		try {
			Map<String, Object> result = currentPif.queryOnce("source_file(" + getPrologFileName(file) + ")");
			if (result != null) {
				decoration.addSuffix(" [consulted]");
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PROLOG_FILE_CONSULTED));
			}

		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
	}
	
	
	private String getPrologFileName(IFile file) {
		String enclFile = file.getRawLocation().toPortableString();
		if (Util.isWindows()) {
			enclFile = enclFile.toLowerCase();
		}

		IPath filepath = new Path(enclFile);
		return "'" + Util.prologFileName(filepath.toFile()) + "'";
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
