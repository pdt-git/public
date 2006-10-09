package org.cs3.jtransformer.internal.builders;
import java.util.Map;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.internal.astvisitor.Names;
import org.cs3.jtransformer.internal.natures.JTransformerProjectNature;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
/**
 * keeps the fact base up-to-date.
 * 
 * since this class is currently subject to very substantial changes, i do not
 * dare write an overview of its inner workings yet. Please feel free to
 * contact me in case of questions: AFBLukas_AT_gmx_DOT_de
 * 
 * @see IncrementalProjectBuilder
 */
public class JTransformerProjectBuilder extends IncrementalProjectBuilder{
	//please keep this ensync with the id assigned to this builder in the
	// plugin.xml file.
    /**
     * @deprecated use JTransformer.BUILDER_ID instead.
     */
	public static final String BUILDER_ID = "org.cs3.jtransformer.JTransformerProjectBuilder";
	//whether we are doing a FULL, INCREMENTAL or AUTO build. set in
	// build(int,Map,IProgressMonitor)
	private int buildKind;
	//the arguments to the current build. set in
	// build(int,Map,IProgressMonitor)
	private Map args;
	
	//true, if the user hit cancel on the monitor, or if anything particulary
	// nasty happens
	private boolean buildCanceled;
	//hm. a prolog manager, what else. stupid.
	
	
	/**
	 * Default constructor. The builder is typicaly created by eclipse. There
	 * should be no need to create your own instance.
	 */
	public JTransformerProjectBuilder() {
		/*
		 * whatever you do: don't assume to much about WHEN this constructor is
		 * called! the build() method is propably a better place to initialize
		 * variables, etc.
		 */
	}
	/**
	 * main entry point, called by eclipse framework.
	 * 
	 * @see IncrementalProjectBuilder#build
	 */
	protected IProject[] build(int kind, Map args, IProgressMonitor monitor)
			throws CoreException {
		Debug.debug("JTransformerPRojectBuilder triggered.");
		
		try {
			
			buildCanceled = false;
			if (getProject() == null) {
				Debug.warning("Project is NULL!!");
				return null;
			}
			
			this.buildKind = kind;
			this.args = args;
			
			if(! getProject().hasNature(JavaCore.NATURE_ID)){
				Debug.warning("Project does not have a Java nature!");
				return null;
			}
			IJavaProject javaProject = (IJavaProject) getProject().getNature(
					JavaCore.NATURE_ID);
			
			if(javaProject==null){
				Debug.warning("Java Project is NULL!!");
				return null;
			}
			
			if(! getProject().hasNature(JTransformer.NATURE_ID)){
				Debug.warning("Project does not have a JTransformer nature!");
				return null;
			}
			JTransformerProjectNature nature = (JTransformerProjectNature) getProject().getNature(JTransformer.NATURE_ID);
			if(nature==null){
				Debug.warning("JTransformer Nature is NULL!!");
				return null;
			}
			
			
			int flags = FactBaseBuilder.IS_ECLIPSE_BUILD;
			switch (kind) {
				case FULL_BUILD :
					//updateProjectLocationInFactbase(nature);
					nature.getFactBaseBuilder().build(null, flags, monitor);
					break;
				case INCREMENTAL_BUILD :
				case AUTO_BUILD :
					//ld: i currently don't see any reason to
					//differantiate here.					
					//builder.incrementalBuild(getDelta(getProject()),monitor);
				    nature.getFactBaseBuilder().build(getDelta(getProject()),flags,monitor);
					break;
			}
			monitor.done();
		}
		catch(OperationCanceledException e){
		    throw e;
		}
		catch (Throwable e) {
			Debug.report(e);		
			throw new CoreException(new Status(IStatus.ERROR,JTransformer.PLUGIN_ID,IResourceStatus.BUILD_FAILED,"Problems during build",e));
		}
		return null;
	}
//	/**
//	 * updates the projectT/4 fact
//	 * @param nature
//	 */
//	private void updateProjectLocationInFactbase(JTransformerProjectNature nature) {
//		PrologSession session = null;
//		try {
//			session = nature.getPrologInterface().getSession();
//			String projectName = quote(getProject().getName());
//			String query = 
//				    "retractall("+Names.PROJECT_T + "(" + projectName + ",_,_,_ ))" +
//					"," +
//					"assert("    +Names.PROJECT_T + "(" + projectName + ", " +
//						quote(getProject().getLocation().toPortableString()) + "," +
//						quote(JTUtils.getOutputProjectName(getProject())) + ", " +
//						quote(JTUtils.getOutputProjectPath(getProject())) + "))";
//			session.queryOnce(query);
//					
//		} catch(Exception ex){
//			if(session != null) {
//				session.dispose();
//			}
//		}
//	}

	/* (non-Javadoc)
     * @see org.eclipse.core.resources.IncrementalProjectBuilder#clean(org.eclipse.core.runtime.IProgressMonitor)
     */
    protected void clean(IProgressMonitor monitor) throws CoreException {
        try {
            JTransformerProjectNature nature = (JTransformerProjectNature) getProject().getNature(JTransformer.NATURE_ID);
            nature.getFactBaseBuilder().clean(monitor);
        }
            catch (Throwable e) {
                Debug.report(e);
    			throw new CoreException(new Status(IStatus.ERROR,JTransformer.PLUGIN_ID,IResourceStatus.BUILD_FAILED,"Problems during clean",e));
            }
		}
    }
   


