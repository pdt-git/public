package org.cs3.pdt.common.queries;

import org.cs3.pdt.common.metadata.Goal;
import org.cs3.prolog.common.logging.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;

public abstract class MarkerCreatingSearchQuery extends PDTSearchQuery {
	
	protected boolean createMarkers;
	private String attribute;
	private String value;

	public MarkerCreatingSearchQuery(Goal goal, boolean createMarkers, String attribute, String value) {
		super(goal);
		this.createMarkers = createMarkers;
		this.attribute = attribute;
		this.value = value;
	}
	
	@Override
	public IStatus run(IProgressMonitor monitor) {
		clearMarkers();
		return super.run(monitor);
	}

	private void clearMarkers() {
		try {
			IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(IMarker.PROBLEM, false, IResource.DEPTH_INFINITE);
			for (IMarker marker : markers) {
				if (marker.getAttribute(attribute) != null) {
					marker.delete();
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
	
	protected IMarker createMarker(IFile file, String message, int line) throws CoreException {
		IMarker marker = getMarker(file, message);

		marker.setAttribute(IMarker.LINE_NUMBER, line);
		
		return marker;
	}
	
	protected IMarker createMarker(IFile file, String message, int start, int end) throws CoreException {
		IMarker marker = getMarker(file, message);
		
		marker.setAttribute(IMarker.CHAR_START, start);
		marker.setAttribute(IMarker.CHAR_END, end);
		
		return marker;
	}
	
	private IMarker getMarker(IFile file, String message) throws CoreException {
		IMarker marker = file.createMarker(IMarker.PROBLEM);
		marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
		marker.setAttribute(attribute, value);
		marker.setAttribute(IMarker.MESSAGE, message);
		return marker;
	}
	
	
}
