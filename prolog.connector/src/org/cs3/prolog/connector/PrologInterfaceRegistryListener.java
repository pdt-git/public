/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.connector;

import java.util.EventListener;

public interface PrologInterfaceRegistryListener extends EventListener{
	public void prologInterfaceAdded(PrologInterfaceRegistryEvent e);
	public void prologInterfaceRemoved(PrologInterfaceRegistryEvent e);
	public void subscriptionAdded(PrologInterfaceRegistryEvent e);
	public void subscriptionRemoved(PrologInterfaceRegistryEvent e);
}

