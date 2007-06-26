/**
 * 
 */
package org.cs3.jtransformer.internal.natures;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.jtransformer.JTDebug;
import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.internal.actions.TopoSortProjects;
import org.cs3.jtransformer.internal.astvisitor.Names;
import org.cs3.jtransformer.internal.builders.FactBaseBuilder;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.runtime.DefaultSubscription;
import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

public class JTransformerSubscription extends DefaultSubscription implements
			LifeCycleHook {

		//private IProject project;

		/**
		 * still needed?
		 */
		Object configureMonitor = new Object();

		private PrologInterface pif;

//		private JTransformerNature nature;
		
		static Set toBeBuilt = new HashSet();

		static Object toBeBuiltMonitor = new Object();

		public JTransformerSubscription() {
			super();
		}

		
		public JTransformerSubscription(IProject project, String id, String pifID,
				String descritpion, String name) {
			super(id, pifID, descritpion, name, JTransformer.PLUGIN_ID, true);
//			this.project = project;
		}

		public void restoreState(Map params) {
			super.restoreState(params);
//			project = ResourcesPlugin.getWorkspace().
//				getRoot().getProject((String) params.get("project"));
			
		}

		public Map saveState() {
			Map map = super.saveState();
//			map.put("project", project.getName());
			return map;
		}
		
		public void configure(PrologInterface pif) {
	    	JTDebug.debug("JT: JTransformerSubscription.configure: for project '" + getId() + "' (begin)");

			pif.addLifeCycleHook(this, getId(), new String[0]);
			if (pif.isUp()) {
				PrologSession session = null;
				try {
					session = pif.getSession();
					onInit(pif, session);

					afterInit(pif);

				} catch (PrologInterfaceException e) {
					JTDebug.rethrow(e);
				} finally {
					if (session != null) {
						session.dispose();
					}
				}
			}
	    	JTDebug.debug("JT: JTransformerSubscription.configure: for project '" + getId() + "' (end)");
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
		 */
		public void onInit(PrologInterface pif, final PrologSession initSession) {
	    	JTDebug.debug("JT: JTransformerSubscription.onInit: for project '" + getId() + "' (begin)");
	    	this.pif = pif;
			try {
//				JTransformerProject[] jtransformerProjects = JTransformer
//						.getJTransformerProjects(pif);
//				for (int i = 0; i < jtransformerProjects.length; i++) {
//					// XXX: ld: i don't like that cast. Any idee?
//					JTransformerNature jtransformerProject = (JTransformerNature) jtransformerProjects[i];
//					jtransformerProject.reconfigure(initSession);
//
//				}
//				try {
//					if(	JTransformerPlugin.getDefault().getPreferenceValue(project, 
//							JTransformer.PROLOG_RUNTIME_KEY, null) == null) {
//						JTransformerPlugin.getDefault().setPreferenceValue(project, 
//						JTransformer.PROLOG_RUNTIME_KEY, getPifKey());
//					}
//				} catch (CoreException e) {
//					JTUtils.logAndDisplayUnknownError(e);			
//				}

	    		//updateProjectLocationInFactbase(initSession);

				JTransformerPlugin plugin = JTransformerPlugin.getDefault();
				String v = plugin.getPreferenceValue(
						JTransformer.PREF_USE_PEF_STORE, "false");
				if (Boolean.valueOf(v).booleanValue()) {
					plugin.reload(initSession);
				}

				PLUtil.configureFileSearchPath(PrologRuntimePlugin.getDefault()
						.getLibraryManager(), initSession,
						new String[] { JTransformer.LIB_ENGINE });

				PLUtil.configureFileSearchPath(PrologRuntimePlugin.getDefault()
						.getLibraryManager(), initSession,
						new String[] { PrologRuntime.LIB_ATTIC });
//				initSession
//						.queryOnce("use_module(library('org/cs3/pdt/metadata/pdtplugin'))");

				initSession.queryOnce("[library('jt_engine.pl')],"
						+ "[library('org/cs3/pdt/test/main.pl')],"
						+ "[library('org/cs3/pdt/util/main.pl')],"
						+ "[library('org/cs3/pdt/compatibility/main.pl')]");
				
				if(JTransformerPlugin.getDefault().useReverseIndex()) {
					initSession.queryOnce(Names.PRED_ACTIVATE_REVERSE_INDEXES);
				} 
				initSession.queryOnce("update_java_lang");

			} catch (Throwable e) {
				JTDebug.report(e);
				throw new RuntimeException(e);
			}
	    	JTDebug.debug("JT: JTransformerSubscription.onInit: for project '" + getId() + "' (end)");

		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
		 */
		public void afterInit(PrologInterface pif)
				throws PrologInterfaceException {
			
	    	JTDebug.debug("JT: JTransformerSubscription.afterInit ("+hashCode()+"): for project '" + getId() + "' (begin)");

			buildAllProjectsOfPif();
	    	JTDebug.debug("JT: JTransformerSubscription.afterInit ("+hashCode()+"): for project '" + getId() + "' (end)");
		}

		private void buildAllProjectsOfPif() {
			try {
				synchronized (toBeBuiltMonitor) {
					JTDebug.info("JT: Deactivating auto-building");
					 ResourcesPlugin.getPlugin().getWorkspace().getDescription().setAutoBuilding(false);

				}

				JTDebug.info("JT: JTransformerSubscription.buildAllProjectsOfPif: start");

				List tmpProjects = null;
				synchronized (toBeBuiltMonitor) {
					tmpProjects = getSortedListOfNotYetBuildJTProjects();

					if(tmpProjects == null) {
						return;
					}
					
					JTDebug.info("JT: JTransformerSubscription.buildAllProjectsOfPif: toBeBuilt.addAll");
					toBeBuilt.addAll(tmpProjects);
				}
				IWorkspace workspace = ResourcesPlugin.getWorkspace();
				IWorkspaceDescription wd = workspace.getDescription();
				if (!wd.isAutoBuilding()) {
					JTDebug.warning("JT: auto building is deactivated. PEFs will not be updated!" );
//					wd.setAutoBuilding(true);
				}
				JTDebug.info("JT: JTransformerSubscription.buildAllProjectsOfPif: finalizeProjectBuilding: before");

				final List projects = tmpProjects;
				try {
					for (Iterator iter = projects.iterator(); iter.hasNext();) {
						buildProject((IProject) iter.next());
					}
				} finally {
					finalizeProjectBuilding(projects);
				}

			} catch (Throwable e) {
				JTDebug.report(e);
				throw new RuntimeException(e);
			}
		}

		private List getSortedListOfNotYetBuildJTProjects() throws CoreException, Exception {
			
			List unsortedProjectsList = getAssociatedProjects();
			if(unsortedProjectsList == null) {
				return null;
			}
			if(unsortedProjectsList.size() == 0) {
				JTDebug.error("JTransformerSubscription.getSortedListOfNotYetBuildJTProjects(): project list ist empty!");
				PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry().removeSubscription(getId());
				String msg = "Factbase " + getPifKey() + " referenced by " + getId() + " cannot be configured correctly. " +
					"Please remove the JTransformer nature from all sharing projects and reassign it.";
				UIUtils.displayMessageDialog(JTUtils.getShell(true), "JTransformer", msg);
				throw new RuntimeException(msg);
				//unsortedProjectsList.add(project);
			} else {
				JTDebug.info("JT:getSortedListOfNotYetBuildJTProjects: first project name: " + ((IProject)unsortedProjectsList.get(0)).getName());
			}
			//unsortedProjectsList = filterBuildProjects(unsortedProjectsList);
			TopoSortProjects topoSorter = new TopoSortProjects();
			//JTUtils.getProjectsWithPifKey(key);
			return topoSorter.sort(false, unsortedProjectsList);
		}


//		private List filterBuildProjects(List unsortedProjectsList) throws CoreException {
//			List filteredUnsortedProjectsList = new ArrayList();
//			for (Iterator iter = unsortedProjectsList.iterator(); iter
//					.hasNext();) {
//				IProject project = (IProject) iter.next();
//				if(!JTransformerPlugin.getNature(project).isBuild())
//				{
//					filteredUnsortedProjectsList.add(project);
//				}
//			}
//			return filteredUnsortedProjectsList;
//		}

		private void finalizeProjectBuilding(final List projects) {
			Job j = new Job("Finalize building of projects") {
				public IStatus run(IProgressMonitor monitor) {
					synchronized (toBeBuiltMonitor) {
						toBeBuilt.removeAll(projects);
						JTDebug.info("JT: Re-activating auto-building");
 					    ResourcesPlugin.getPlugin().getWorkspace().getDescription().setAutoBuilding(true);
					}
					return Status.OK_STATUS;
				}

				public boolean belongsTo(Object family) {
					return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
				}

			};
			JTDebug.info("finalizeProjectBuilding: before scheduling job");
			synchronized (configureMonitor) {
//							if (!buildTriggered) {
//								buildTriggered = true;
					j.setRule(ResourcesPlugin.getWorkspace().getRoot());
					j.schedule();
					pefInitializationDone();
//
//					notifyJTNaturesAboutEndOfInitialization(projects);

//							}
//							buildTriggered = false;
			}
			if(projects.size() == 0) {
				JTDebug.error("finalizeProjectBuilding: projects size equals 0");
			} else {
				JTDebug.info("finalizeProjectBuilding: after scheduling job - " + ((IProject)projects.get(0)).getName());
			}
			
		}
		
//		private void notifyJTNaturesAboutEndOfInitialization(final List projects) {
//			pefInitializationDone();
//
////			for (Iterator iter = projects.iterator(); iter
////					.hasNext();) {
////				IProject project = (IProject) iter.next();
////				try {
////					if(project.getName().equals(this.project.getName())) {
////						if(nature == null) { // only happens on 
////							nature = JTransformerPlugin.getNature(project);
////						} 
////						nature.pefInitializationDone();
////						//nature = JTransformerPlugin.getNature(project);
////					} else {
////						JTUtils.getNature(project).pefInitializationDone();
////					}
////				} catch (CoreException e) {
////					JTUtils.logAndDisplayUnknownError(e);
////				}
////			}
//		}

		private Object pefInitializationMonitor = new Object();

		private boolean pefInitializationDone = false;


		public void pefInitializationDone() {
			JTDebug.debug("JT: Nature.pefInitializationDone before monitor: " + getId());
			synchronized (pefInitializationMonitor) {
				pefInitializationDone = true;
				pefInitializationMonitor.notifyAll();
			}
			JTDebug.debug("JT: Nature.pefInitializationDone after monitor: " + getId());

		}
		
		public void ensurePefInitializationFinished() throws InterruptedException {
			
			synchronized (pefInitializationMonitor) {
				if(pefInitializationDone) {
					return;
				} else {
					pefInitializationMonitor.wait();
				}
			}
			
		}
				
		
		private void buildProject(final IProject project) {
			Job j = new Job("Building JTransformer PEFs for project " + project.getName()) {
				public IStatus run(IProgressMonitor monitor) {
					try {
						if(!project.isSynchronized(IResource.DEPTH_INFINITE))
						{
							JTUtils.clearPersistantFacts(getPifKey());
							JTDebug.debug("JTransformerSubscription.buildProject: project " + project.getName() +
									" is not up-to-date. Refreshing project.");
							project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
//							project.build(
//									IncrementalProjectBuilder.FULL_BUILD, 
//									JTransformer.BUILDER_ID,
//									new HashMap(),
//									monitor);
						} 
//						FactBaseBuilder builder = new FactBaseBuilder(project);
//						builder.build(null, 0, monitor);
						project.build(IncrementalProjectBuilder.FULL_BUILD, 
								JTransformer.BUILDER_ID,
								new HashMap(),
								monitor);
						
					} catch (OperationCanceledException opc) {
						return Status.CANCEL_STATUS;
					} catch (Exception e) {
						return new Status(IStatus.ERROR,
								JTransformer.PLUGIN_ID, -1,
								"Problems during build", e);
					}
					return Status.OK_STATUS;
				}
				public boolean belongsTo(Object family) {
					return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
				}

			};

			synchronized (configureMonitor) {
//					if (!buildTriggered) {
//						buildTriggered = true;
					j.setRule(ResourcesPlugin.getWorkspace().getRoot());
					j.schedule();
//					}
//					buildTriggered = false;
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
		 */
		public void beforeShutdown(PrologInterface pif, PrologSession session)
				throws PrologException, PrologInterfaceException {
			JTransformerPlugin plugin = JTransformerPlugin.getDefault();
			String v = plugin.getPreferenceValue(
					JTransformer.PREF_USE_PEF_STORE, "false");
			if (Boolean.valueOf(v).booleanValue()) {
				JTransformerPlugin.getDefault().save(session);
			}
		}
		
//		/**
//		 * updates the projectT/4 fact
//		 * 
//		 * @throws PrologInterfaceException
//		 * @throws PrologException
//		 */
//		void updateProjectLocationInFactbase(PrologSession session)
//				throws PrologException, PrologInterfaceException {
//			String projectName = quote(project.getName());
//			String query = "retractall(" + Names.PROJECT_T + "(" + projectName
//					+ ",_,_,_ ))" + "," + "assert(" + Names.PROJECT_T + "("
//					+ projectName + ", "
//					+ quote(project.getLocation().toPortableString()) + ","
//					+ quote(JTUtils.getOutputProjectName(project)) + ", "
//					+ quote(JTUtils.getOutputProjectPath(project)) + "))";
//			session.queryOnce(query);
//
//		}
		
		static private String quote(String str) {
			return "'" + str + "'";
		}

		public List getAssociatedProjects() throws CoreException {
			final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
			
			List sharingProjects = new ArrayList();
			for (int i = 0; i < projects.length; i++) {
				if(projects[i].isOpen()){
					String runtimeKey = JTransformerPlugin.getDefault().getPreferenceValue(projects[i], 
							JTransformer.PROLOG_RUNTIME_KEY, null);
//					if(projects[i].getName().equals("Test4"))
//						System.err.println("DEBUG");
					if(runtimeKey != null && runtimeKey.equals(getPifKey()) &&
					   projects[i].hasNature(JTransformer.NATURE_ID) ){
						sharingProjects.add(projects[i]);
					}
				}
			}
			return sharingProjects;
		}


		public boolean isPrologInterfaceUp() {
			if(pif == null) {
				return false;
			}
			return pif.isUp();
		}



	}