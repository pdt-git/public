package org.cs3.jtransformer.internal.natures;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.JTransformerProjcetEvent;
import org.cs3.jtransformer.JTransformerProject;
import org.cs3.jtransformer.JTransformerProjectListener;
import org.cs3.jtransformer.internal.builders.FactBaseBuilder;
import org.cs3.jtransformer.regenerator.ISourceRegenerator;
import org.cs3.jtransformer.regenerator.SourceCodeRegenerator;
import org.cs3.pdt.runtime.DefaultSubscription;
import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 * @see IProjectNature
 */
public class JTransformerProjectNature implements IProjectNature, JTransformerProject,
		JTransformerProjectListener {

	
	// The project for which this nature was requested,
	// see IProject.getNature(String)
	private IProject project;

	private Option[] options;

	private FactBaseBuilder builder;

	private Vector listeners = new Vector();

	/**
	 * 
	 */
	public JTransformerProjectNature() {

	}

	/**
	 * @see IProjectNature#configure
	 */
	public void configure() throws CoreException {
		try
		{
			getFactBaseBuilder().clean(null);
		} catch (PrologInterfaceException e1)
		{
			JTransformerPlugin.getDefault().createPrologInterfaceExceptionCoreExceptionWrapper(e1);
			
		}
		Debug.debug("configure was called");
		IProjectDescription descr = project.getDescription();
		ICommand sheepBuilder = descr.newCommand();
		sheepBuilder.setBuilderName(JTransformer.BUILDER_ID);
		ICommand builders[] = descr.getBuildSpec();
		for (int i = 0; i < builders.length; i++) {
			if (builders[i].getBuilderName().equals(JTransformer.BUILDER_ID)) {
				return;
			}
		}
		ICommand newBuilders[] = new ICommand[builders.length + 1];
		System.arraycopy(builders, 0, newBuilders, 0, builders.length);
		newBuilders[builders.length] = sheepBuilder;
		descr.setBuildSpec(newBuilders);

		project.setDescription(descr, null);

		// important: touch the ConsultServices NOW if the pif is already
		// running.
		// otherwise recorded facts might be reloaded to late! (i.e. by the
		// builder AFTER it has
		// "found" unresolved types
		PrologInterface pif = getPrologInterface();
		if (pif.isUp()) {
			Job j = new Job("building PEFs for project " + project.getName()) {
				protected IStatus run(IProgressMonitor monitor) {
					try {
						IWorkspaceDescription wd = ResourcesPlugin
								.getWorkspace().getDescription();
						if (wd.isAutoBuilding()) {
							project.build(IncrementalProjectBuilder.FULL_BUILD,
									monitor);
						}
					} catch (OperationCanceledException opc) {
						return Status.CANCEL_STATUS;
					} catch (CoreException e) {
						return new Status(Status.ERROR, JTransformer.PLUGIN_ID, -1,
								"exception caught during build", e);
					}
					return Status.OK_STATUS;
				}

				public boolean belongsTo(Object family) {
					return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
				}

			};
			j.setRule(ResourcesPlugin.getWorkspace().getRoot());
			j.schedule();
		} else {
			// if the pif is not up yet, this is no problem at all: the reload
			// hook will
			// take care of the due build in its afterInit method.
			;
		}

	}

	/**
	 * 
	 */
	private void initOptions() {
		options = new Option[] {
				new SimpleOption(
						JTransformer.PROP_OUTPUT_PROJECT,
						"Output Project",
						"The project to which generated sourcecode is written."
								+ "This option is ignored if JTransformer is operating in place.",
						Option.STRING, JTransformerPlugin.getDefault()
								.getPreferenceValue(
										JTransformer.PREF_DEFAULT_OUTPUT_PROJECT, null)),
				new SimpleOption(
						JTransformer.PROP_OUTPUT_FOLDER,
						"Output source folder",
						"The source folder to which generated sourcecode is written."
								+ "This should be a relative path - it is eather appended to the"
								+ "path of the output project, or - if JTransformer operates inplace -"
								+ " to that of the original project",
						Option.STRING, JTransformerPlugin.getDefault()
								.getPreferenceValue(
										JTransformer.PREF_DEFAULT_OUTPUT_FOLDER, null)),
				new SimpleOption(
						JTransformer.PROP_INPLACE,
						"Try inplace operation",
						"NOT IMPLEMENTED YET! \nIf this option is set, changes "
								+ "to existing source files will be performed in-place."
								+ "Source files for which the respective PEFs where "
								+ "deleted, are removed. \n"
								+ "Newly created source files however "
								+ "will still be created in the specified output folder.",
						Option.FLAG, "false"),
				new SimpleOption(
						JTransformer.PROP_PEF_STORE_FILE,
						"PEF store file",
						"The file where PEFs for this project should be stored",
						Option.FILE, JTransformerPlugin.getDefault()
								.getPreferenceValue(
										JTransformer.PREF_DEFAULT_PEF_STORE_FILE, null)),

		};

	}

	/**
	 * @see IProjectNature#deconfigure
	 */
	public void deconfigure() throws CoreException {
		IProjectDescription descr = project.getProject().getDescription();
		Debug.debug("deconfigure was called");
		ICommand builders[] = descr.getBuildSpec();
		int index = -1;
		for (int i = 0; i < builders.length; i++) {
			if (builders[i].getBuilderName().equals(JTransformer.BUILDER_ID)) {
				index = i;
				break;
			}
		}
		if (index != -1) {
			ICommand newBuilders[] = new ICommand[builders.length - 1];
			System.arraycopy(builders, 0, newBuilders, 0, index);
			System.arraycopy(builders, index + 1, newBuilders, index,
					builders.length - index - 1);
			descr.setBuildSpec(newBuilders);
		}
		PrologInterface pif = getPrologInterface();
		PrologSession s = null;

		try {
			if (pif.isUp()) {
					s = pif.getSession();
					String projectName = getProject().getName();
					String sourceFolder = Util.prologFileName(new File(
							getPreferenceValue(JTransformer.PROP_OUTPUT_FOLDER, null)));
					s.queryOnce("retractall(project_option('" + projectName
							+ "', _))");
			}
			getFactBaseBuilder().clean(null);
		} catch (PrologInterfaceException e)
		{
			JTransformerPlugin.getDefault().createPrologInterfaceExceptionCoreExceptionWrapper(e);
		} finally {
			if(s != null)
				s.dispose();
		}

	}

	/**
	 * @see IProjectNature#getProject
	 */
	public IProject getProject() {
		return project;
	}

	/**
	 * @see IProjectNature#setProject
	 */
	public void setProject(IProject project){
		this.project = project;
		PrologInterface pif = getPrologInterface();
		if (pif.isUp()) {
			reconfigure();
		}

	}
	
	PrologInterface pif = null;
	Object pifMonitor = new Object();
	
	class JTransformerSubscription extends DefaultSubscription implements LifeCycleHook {
		
		
		public JTransformerSubscription(String id,
				   String pifID,  
				   String descritpion, 
				   String name){
			super(id, pifID, descritpion, name);
		}

		public void configure(PrologInterface pif) {
			pif.addLifeCycleHook(this, getId(), new String[0]);
			if (pif.isUp()) {
				try {
					afterInit(pif);
				} catch (PrologInterfaceException e) {
					Debug.rethrow(e);
				}
			}
		}

		
	    /*
	     * (non-Javadoc)
	     * 
	     * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
	     */
	    public void onInit(PrologInterface pif, final PrologSession initSession) {
	        try {
	            JTransformerProject[] jtransformerProjects = JTransformer.getJTransformerProjects(pif);
	            for (int i = 0; i < jtransformerProjects.length; i++) {
	                // XXX: ld: i don't like that cast. Any idee?
	                JTransformerProjectNature jtransformerProject = (JTransformerProjectNature) jtransformerProjects[i];

	                jtransformerProject.reconfigure(initSession);

	            }
	            JTransformerPlugin plugin = JTransformerPlugin.getDefault();
	            String v = plugin.getPreferenceValue(JTransformer.PREF_USE_PEF_STORE,
	                    "false");
	            if (Boolean.valueOf(v).booleanValue()) {
	                plugin.reload(initSession);
	            }
			
					PLUtil.configureFileSearchPath(PrologRuntimePlugin.getDefault().getLibraryManager(),
						initSession, new String[] {JTransformer.LIB_ENGINE});
					
					initSession.queryOnce(
							"[library('jt_engine.pl')],"+
							"[library('org/cs3/pdt/test/main.pl')],"+
							"[library('org/cs3/pdt/util/main.pl')]," +
					        "[library('org/cs3/pdt/compatibility/main.pl')]");
	                initSession.queryOnce("update_java_lang");


				
	            
	        } catch (Throwable e) {
	            Debug.report(e);
	            throw new RuntimeException(e);
	        }
	    }

	    /*
	     * (non-Javadoc)
	     * 
	     * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	     */
	    public void afterInit(PrologInterface pif) throws PrologInterfaceException{

	    	
	        try {
	            IWorkspace workspace = ResourcesPlugin.getWorkspace();
				IWorkspaceDescription wd = workspace
	                    .getDescription();
	            if (!wd.isAutoBuilding()) {
	                return;
	            }
	            IProgressMonitor monitor = new NullProgressMonitor();
	            IProject[] projects = workspace.getRoot()
	                    .getProjects();
	            final JTransformerProject[] jtransformerProjects = JTransformer.getJTransformerProjects(pif);
	            Job j = new Job("Building workspace") {
	                public IStatus run(IProgressMonitor monitor) {
	                    try {
	                        for (int i = 0; i < jtransformerProjects.length; i++) {

	                            JTransformerProject jtransformerProject = jtransformerProjects[i];
	                            IProject project = jtransformerProject.getProject();
	                            project.build(IncrementalProjectBuilder.FULL_BUILD,
	                                    monitor);

	                        }
	                    } catch (OperationCanceledException opc) {
	                        return Status.CANCEL_STATUS;
	                    } catch (Exception e) {
	                        return new Status(IStatus.ERROR, JTransformer.PLUGIN_ID, -1,
	                                "Problems during build", e);
	                    }
	                    return Status.OK_STATUS;
	                }
				      public boolean belongsTo(Object family) {
					         return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
				      }
					
	            };

				//j.setRule(JTransformer.JTransformer_BUILDER_SCHEDULING_RULE);
				j.setRule(workspace.getRoot());
				j.schedule();
				
	        } catch (Throwable e) {
	            Debug.report(e);
	            throw new RuntimeException(e);
	        }
	    }

	    /*
	     * (non-Javadoc)
	     * 
	     * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	     */
	    public void beforeShutdown(PrologInterface pif, PrologSession session) throws PrologException, PrologInterfaceException {
	        JTransformerPlugin plugin = JTransformerPlugin.getDefault();
	        String v = plugin.getPreferenceValue(JTransformer.PREF_USE_PEF_STORE, "false");
	        if (Boolean.valueOf(v).booleanValue()) {
	            JTransformerPlugin.getDefault().save(session);
	        }
	    }
	}
	
	public PrologInterface getPrologInterface() {
		synchronized (pifMonitor) {
			if(pif != null) {
				return pif;
			}
			pif = PrologRuntimePlugin.getDefault().getPrologInterface(
					new JTransformerSubscription(
							getProject().getName(),
					        getProject().getName(),
					        "JTransformer factbase of project " + getProject().getName(),
					        "JTransformer")
					);
	
			return pif;
		}

//		if (!pif.isUp()) {
//			try {
//				pif.start();
//			} catch (IOException e) {
//				Debug.report(e);
//				throw new RuntimeException(e);
//			}
//		}
		// List libraries = pif.getBootstrapLibraries();
		// ResourceFileLocator l =
		// JTransformerPlugin.getDefault().getResourceLocator(JTransformer.ENGINE);
		// String name = Util.prologFileName(l.resolve(JTransformer.MAIN_PL));
		// if(!libraries.contains(name)){
		// libraries.add(name);
		// if(pif.isUp());
		// PrologSession s = pif.getSession();
		// s.queryOnce("['"+name+"']");
		// }
	}

	/**
	 * @return Returns the builder.
	 */
	public FactBaseBuilder getFactBaseBuilder() {
		if (builder == null) {
			builder = new FactBaseBuilder(this);
			builder.addJTransformerProjectListener(this);
		}
		return builder;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.JTransformerProject#addJTransformerProjectListener(org.cs3.jtransformer.JTransformerProjectListener)
	 */
	public void addJTransformerProjectListener(JTransformerProjectListener l) {
		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.JTransformerProject#removeJTransformerProjectListener(org.cs3.jtransformer.JTransformerProjectListener)
	 */
	public void removeJTransformerProjectListener(JTransformerProjectListener l) {
		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}
	}

	protected void fireFactBaseUpdated() {
		final JTransformerProjectNature currentProject = this;

		Runnable r = new Runnable() {
			public void run() {
				JTransformerProjcetEvent e = new JTransformerProjcetEvent(currentProject);

				JTransformerPlugin.getDefault().fireFactBaseUpdated(e);

				Vector cloned = null;
				synchronized (listeners) {
					cloned = (Vector) listeners.clone();
				}
				for (Iterator it = cloned.iterator(); it.hasNext();) {
					JTransformerProjectListener l = (JTransformerProjectListener) it.next();
					l.factBaseUpdated(e);
				}

			}
		};
		Thread t = new Thread(r, "JTransformerProject listener notification");
		t.setDaemon(true);
		t.start();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.JTransformerProject#generateFacts(org.eclipse.jdt.core.ICompilationUnit,
	 *      java.io.PrintStream)
	 */
	public void generateFacts(AsyncPrologSession session, ICompilationUnit cu)
			throws IOException, CoreException, PrologInterfaceException {
		getFactBaseBuilder().writeFacts(session, cu);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.JTransformerProjectListener#factBaseUpdated(org.cs3.jtransformer.JTransformerProjcetEvent)
	 */
	public void factBaseUpdated(JTransformerProjcetEvent e) {
		fireFactBaseUpdated();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.JTransformerProject#getOptions()
	 */
	public Option[] getOptions() {
		if (options == null) {
			initOptions();
		}
		return options;
	}

	/**
	 * look up a preference value.
	 * <p>
	 * tries the following values in the given order and returns the first
	 * non-null result. If everything returns null, the given defaultValue is
	 * returned.
	 * <ul>
	 * <li>System.getProperty(key)</li>
	 * <li>getProject().getPersistentProperty(key)</li>
	 * <li>if an option with the given id exists in the array returned by
	 * getOptions(), take its default value</li>
	 * <li>the given default value
	 * </ul>
	 * 
	 * @param key
	 * @return the value or specified default if no such key exists..
	 * @throws CoreException
	 */
	public String getPreferenceValue(String key, String defaultValue)
			{
		String value = System.getProperty(key);
		if (value != null) {
			return value;
		}
		try {
			value = getProject().getPersistentProperty(new QualifiedName("", key));
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		if (value != null) {
			return value;
		}
		Option[] o = getOptions();
		for (int i = 0; i < o.length; i++) {
			if (o[i].getId().equals(key)) {
				return o[i].getDefault();
			}
		}
		return defaultValue;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.JTransformerProject#reconfigure()
	 */
	public void reconfigure(PrologSession s) throws PrologException, PrologInterfaceException {
		String projectName = getProject().getName();
		String outputFolder = Util.prologFileName(new File(
				getPreferenceValue(JTransformer.PROP_OUTPUT_FOLDER, null)));
		String outputProject = Util.prologFileName(new File(
				getPreferenceValue(JTransformer.PROP_OUTPUT_FOLDER, null)));
		boolean inplace = Boolean.valueOf(
				Util.prologFileName(new File(getPreferenceValue(
						JTransformer.PROP_INPLACE, "false")))).booleanValue();

		s.queryOnce("retractall(project_option('" + projectName + "'))");
		s.queryOnce("assert(project_option('" + projectName
				+ "', output_project('" + outputProject + "')))");
		s.queryOnce("assert(project_option('" + projectName
				+ "', output_folder('" + outputFolder + "')))");
		if (inplace) {
			s.queryOnce("assert(project_option('" + projectName
					+ "',inplace))");
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.JTransformerProject#reconfigure()
	 */
	public void reconfigure(){
		PrologSession s = null;
		try {
			s = getPrologInterface().getSession();
			reconfigure(s);
		} catch (PrologException e) {
			Debug.report(e);
			throw e;
		} catch (PrologInterfaceException e)
		{
			UIUtils.logAndDisplayError(
					JTransformerPlugin.getDefault().getErrorMessageProvider(),
					UIUtils.getDisplay().getActiveShell(),
					JTransformer.ERR_PROLOG_INTERFACE_EXCEPTION,
					JTransformer.ERR_CONTEXT_NATURE_INIT,
					e
					);
		} finally {
			if(s != null)
				s.dispose();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.jtransformer.JTransformerProject#getSourceRegenerator()
	 */
	public ISourceRegenerator getSourceRegenerator() {
		try {
			return new SourceCodeRegenerator(getPrologInterface());
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}

	public void setPreferenceValue(String id, String value) {
		try {
			getProject().setPersistentProperty(new QualifiedName("", id), value);
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		
	}

}