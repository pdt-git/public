package org.cs3.pdt.core.internal.properties;

import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.ui.util.OptionProviderPropertyPage;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.OptionProvider;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;

public class PrologProjectPropertyPage extends OptionProviderPropertyPage {

	protected OptionProvider getOptionProvider(IAdaptable element) {
		if(element==null){
			return null;
		}
		if (element instanceof IProject) {
			IProject project = (IProject) element;
			try{
			if(project.hasNature(PDTCore.NATURE_ID)){
				return (OptionProvider) project.getNature(PDTCore.NATURE_ID);
			}
			}
			catch (CoreException e){
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}
		return null;
	}


}