package org.cs3.prolog.common;

import java.util.EventListener;

public interface OptionProviderListener extends EventListener {
	public void valuesChanged(OptionProviderEvent e);
	
}
