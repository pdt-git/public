/*
 */
package org.cs3.jtransformer.internal.properties;

import org.cs3.jtransformer.JTDebug;
import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.pdt.ui.util.OptionProviderPropertyPage;
import org.cs3.pl.common.OptionProvider;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;

/**
 */
public class JTransformerProjectPropertyPage extends OptionProviderPropertyPage {

	protected OptionProvider getOptionProvider(IAdaptable element) {
		if(element==null){
			return null;
		}
		if (element instanceof IProject) {
			IProject project = (IProject) element;
			try{
			if(project.hasNature(JTransformer.NATURE_ID)){
				return JTransformerPlugin.getNature( project);
			}
			}
			catch (CoreException e){
				JTDebug.report(e);
				throw new RuntimeException(e);
			}
		}
		return null;
	}
    
}
