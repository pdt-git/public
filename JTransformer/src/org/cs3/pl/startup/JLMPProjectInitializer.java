package org.cs3.pl.startup;
import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.natures.JLMPProjectNature;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.core.internal.events.ResourceChangeEvent;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IStartup;
/**
 * takes care of initial build/consult of JLMP Projects.
 * <p>
 * This is - to say the best - an unelegent and gross way to solve a simple
 * problem, but i can't think of a better one right now.
 * 
 * <p>
 * After Workbench start up, this class will subscribe itself as a listener to
 * the workspace. It will solely react on the opening if existing projects that
 * have the JLMPProjectNature.
 * <p>
 * If such an event occurs, it will be made sure, that the facts for this
 * project are up to date and asserted by the prolog session.
 */
public class JLMPProjectInitializer
		implements
			IStartup,
			IResourceChangeListener {
	/**
	 */
	public JLMPProjectInitializer() {
		//endlich Ferien! :-)
	}
	/**
	 * called by eclipse <b>after </b> initialization of the workbench.
	 */
	public void earlyStartup() {
		PrologManager.getInstance().getClient();
		//we are interested in workspace changes.
		//we want to be notified when a project is opened
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this,
				ResourceChangeEvent.POST_CHANGE);
		//for all projects that are already open, we synthesize this notification
		
		//PDTPlugin.getDefault().initializeOpenProjects();
	}
	
	
	public void resourceChanged(IResourceChangeEvent event) {
		try {
			event.getDelta().accept(new IResourceDeltaVisitor() {
				public boolean visit(IResourceDelta delta) throws CoreException {
					IResource r = delta.getResource();
					//System.err.println("DEBUG: Initializer: visiting: " + r);
					switch (r.getType()) {
						case IResource.ROOT :
							return true;
						case IResource.PROJECT :
							IProject p = (IProject) r;
							if (delta.getKind() == IResourceDelta.CHANGED
									&& (delta.getFlags() & IResourceDelta.OPEN) != 0) {
								if (p.isOpen() && p.hasNature(JLMPProjectNature.NATURE_ID)) {
									PDTPlugin.getDefault().buildJLMPProject(p);
								}
								else{
									projectClosed(p);
								}
							}
							break;
						default :
							return false;
					}
					return false;
				}

				
			});
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			Debug.report(e);
		}
	}
	
	
	/**
	 * called when a JLMP project has been closed.
	 * <p>
	 * 
	 * @param p
	 */
	private void projectClosed(IProject p) {
		
		//we can use this later to clean up all facts associated with this project
	}
}