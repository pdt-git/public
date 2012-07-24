/* $LICENSE_MSG$(ld) */


package org.cs3.prolog.ui.util;

import org.eclipse.core.runtime.Plugin;

public interface ErrorMessageProvider {
	public String getErrorMessage(int errCode);
	public String getContextMessage(int cxCode);
	public String getId();
	public Plugin getPlugin();
}

