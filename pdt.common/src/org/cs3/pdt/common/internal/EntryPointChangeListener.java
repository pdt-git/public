package org.cs3.pdt.common.internal;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.Path;

public class EntryPointChangeListener implements IResourceChangeListener {

	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		int kind = event.getDelta().getKind();
		if ((kind == IResourceDelta.ADDED || kind == IResourceDelta.CHANGED)) {
			for (IResourceDelta deltaChild : event.getDelta().getAffectedChildren()) {
				if (deltaChild.getResource() instanceof IProject && deltaChild.findMember(new Path(".pdtproperties")) != null) {
					PDTProperties.getPDTProperties((IProject) deltaChild.getResource()).loadPropertyFile();
					PDTCommonPlugin.getDefault().notifyDecorators();
				};
			}
		}
	}

}
