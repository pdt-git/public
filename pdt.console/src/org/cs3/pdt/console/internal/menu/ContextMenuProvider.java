package org.cs3.pdt.console.internal.menu;

import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cs3.pl.common.Debug;
import org.cs3.pl.console.ConsoleView;
import org.cs3.pl.console.SelectionContextAction;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.Bundle;

public class ContextMenuProvider {

	List contextActions = new ArrayList();

	public ContextMenuProvider() {
		collectContextAction();
	}

	private void collectContextAction() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(
				"org.cs3.pdt.console",
				ConsoleView.CONTEXT_ACTION_EXTENSION_POINT_ID);
		if (point == null) {
			Debug.error("could not find the extension point "
					+ ConsoleView.CONTEXT_ACTION_EXTENSION_POINT_ID);
			return;
		}
		IExtension[] extensions = point.getExtensions();
		try {
			for (int i = 0; i < extensions.length; i++) {
				IConfigurationElement[] celem = extensions[i]
						.getConfigurationElements();
				for (int j = 0; j < celem.length; j++) {

					if (!celem[j].getName().equals("SelectionContextAction")) {
						Debug
								.warning("hmmm... asumed a SelectionContextAction, but got a "
										+ celem[j].getName());
					} else {
						SelectionContextAction contextAction = (SelectionContextAction) celem[j]
								.createExecutableExtension("class");
						contextAction.setText(celem[j].getAttribute("name"));
						String sharedIcon = celem[j].getAttribute("sharedIcon");
						if (sharedIcon != null) {
							ISharedImages sharedImages = PlatformUI
									.getWorkbench().getSharedImages();
							contextAction.setImageDescriptor(sharedImages
									.getImageDescriptor(sharedIcon));
						}

						String icon = celem[j].getAttribute("icon");
						if (icon != null && sharedIcon == null) {
							Bundle bundle = Platform.getBundle(extensions[i]
									.getNamespace());
							URL url = bundle.getEntry(icon);
							//DeclaringPluginDescriptor().getPlugin().getBundle().getEntry(icon);
							contextAction.setImageDescriptor(ImageDescriptor
									.createFromURL(url));
						}
						contextActions.add(contextAction);

					}
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}

	public void addMenu(StyledText styledText) {
        styledText.addMouseListener(new ConsoleMouseListener(styledText));
	}

	
	/**
	 * @author trho
	 */
	class ConsoleMouseListener implements MouseListener {
		
		private StyledText text;

		public ConsoleMouseListener(StyledText text) {
			this.text = text;
		}
		public void mouseDoubleClick(MouseEvent e) {
		}

		private void fillContextMenu(IMenuManager mgr) {
			for (Iterator iter = contextActions.iterator(); iter.hasNext();) {
				SelectionContextAction element = (SelectionContextAction) iter
						.next();
				element.init(text);
				mgr.add(element);
				if (element.validate()) {
					element.setEnabled(true);
				} else
					element.setEnabled(false);
			}
		}

		// mgr.add(factory.create(PlatformUI.getWorkbench().getActiveWorkbenchWindow()));

		public void mouseDown(MouseEvent e) {
			if (e.button == 3) {
				// output.getDisplay().getActiveShell().
				MenuManager mgr = new MenuManager();
				mgr.setRemoveAllWhenShown(true);
				mgr.addMenuListener(new IMenuListener() {
					public void menuAboutToShow(IMenuManager mgr) {
						fillContextMenu(mgr);
					}
				});
				text.setMenu(mgr.createContextMenu(text));

			}
		}

		public void mouseUp(MouseEvent e) {
		}
	}
}
