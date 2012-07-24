/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.core.internal.properties;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.ui.util.OptionProviderPropertyPage;
import org.cs3.pl.common.OptionProvider;
import org.cs3.pl.common.logging.Debug;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;

public class AnnotatorsPropertyPage extends OptionProviderPropertyPage {

	@Override
	protected OptionProvider getOptionProvider(IAdaptable element) {
		if (element == null) {
			return null;
		}
		if (element instanceof IProject) {
			IProject project = (IProject) element;
			try {
				if (project.hasNature(PDTCore.NATURE_ID)) {
					return ((IPrologProject) project
							.getNature(PDTCore.NATURE_ID))
							.getAnnotatorsOptionProvider();
				}
			} catch (CoreException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}
		return null;
	}

}

