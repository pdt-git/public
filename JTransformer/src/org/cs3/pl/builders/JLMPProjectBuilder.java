package org.cs3.pl.builders;
import java.util.Map;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.natures.JLMPProjectNature;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
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
public class JLMPProjectBuilder extends IncrementalProjectBuilder{
	//please keep this ensync with the id assigned to this builder in the
	// plugin.xml file.
	public static final String BUILDER_ID = "org.cs3.pl.JTransformer.JLMPProjectBuilder";
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
	
	private boolean clearProjectFacts;
	private boolean clearExternalFacts;
	private PDTPlugin plugin;
	/**
	 * Default constructor. The builder is typicaly created by eclipse. There
	 * should be no need to create your own instance.
	 */
	public JLMPProjectBuilder() {
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
		Debug.debug("JLMPPRojectBuilder triggered.");
		plugin = PDTPlugin.getDefault();
//		ld: TODO: remove this try/catch, its only used for debuging.
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
			
			if(! getProject().hasNature(JLMPProjectNature.NATURE_ID)){
				Debug.warning("Project does not have a JLMP nature!");
				return null;
			}
			JLMPProjectNature nature = (JLMPProjectNature) getProject().getNature(JLMPProjectNature.NATURE_ID);
			if(nature==null){
				Debug.warning("JLMP Nature is NULL!!");
				return null;
			}
			
			FactBaseBuilder builder = nature.getBuilder();
			
			if (builder==null){
				Debug.warning("JLMP Builder is NULL!!");
				return null;
			}
			//monitor.beginTask("Generating Facts:", PROGRESS_SIZE);
			//parse args, set flags
			setUpBuildFlags();
			
			int flags = FactBaseBuilder.IS_ECLIPSE_BUILD;
			//ld: better to have the eclipse worker thread wait for our build
			//    to complete.

			if(clearExternalFacts) flags |= FactBaseBuilder.CLEAR_EXT_FACTS; 
			if(clearProjectFacts) flags |= FactBaseBuilder.CLEAR_PRJ_FACTS;
			switch (kind) {
				case FULL_BUILD :
					builder.build(null, flags, monitor);
					break;
				case INCREMENTAL_BUILD :
				case AUTO_BUILD :
					//ld: i currently don't see any reason to
					//differantiate here.					
					//builder.incrementalBuild(getDelta(getProject()),monitor);
					builder.build(getDelta(getProject()),flags,monitor);
					break;
			}
			monitor.done();
		}
		//		ld: see above, this is only used for debuging.
		//			 since eclipse tends to handle exceptions a bit _too_ quietly :-(
		catch (Exception e) {
			Debug.report(e);
		}
		return null;
	}
	/**
	 * 
	 */
	private void setUpBuildFlags() {
		String fvalue = null;
		fvalue=args==null ? null : (String)args.get("clearProjectFacts");
		clearProjectFacts=(fvalue!=null)&&fvalue.equals("true");
		fvalue=args==null ? null :(String)args.get("clearExternalFacts");
		clearExternalFacts=(fvalue!=null)&&fvalue.equals("true");				
	}
}

