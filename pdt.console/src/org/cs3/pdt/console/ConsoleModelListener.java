/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.console;
import java.util.EventListener;

public interface ConsoleModelListener extends EventListener {
	
	abstract public void onOutput(ConsoleModelEvent e);
	abstract public void onEditBufferChanged(ConsoleModelEvent e);
	abstract public void onCommit(ConsoleModelEvent e);
	abstract public void onModeChange(ConsoleModelEvent e);
	abstract public void afterConnect(ConsoleModelEvent e);
	abstract public void beforeDisconnect(ConsoleModelEvent e);
}

