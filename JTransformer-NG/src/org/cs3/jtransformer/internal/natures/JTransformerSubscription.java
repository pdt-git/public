/**
 * 
 */
package org.cs3.jtransformer.internal.natures;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.internal.actions.TopoSortProjects;
import org.cs3.jtransformer.internal.astvisitor.Names;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.runtime.DefaultSubscription;
import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
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

		private IProject project;

		/**
		 * still needed?
		 */
		Object configureMonitor = new Object();

		private JTransformerProjectNature nature;
		
		static Set toBeBuilt = new HashSet();

		static Object toBeBuiltMonitor = new Object();

		
		public JTransformerSubscription(JTransformerProjectNature nature, IProject project, String id, String pifID,
				String descritpion, String name) {
			super(id, pifID, descritpion, name, JTransformer.PLUGIN_ID, true);
			this.project = project;
			this.nature = nature;
		}

		public void configure(PrologInterface pif) {
			pif.addLifeCycleHook(this, getId(), new String[0]);
			if (pif.isUp()) {
				PrologSession session = null;
				try {
					session = pif.getSession();
					onInit(pif, session);

					afterInit(pif);

				} catch (PrologInterfaceException e) {
					Debug.rethrow(e);
				} finally {
					if (session != null) {
						session.dispose();
					}
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
//				JTransformerProject[] jtransformerProjects = JTransformer
//						.getJTransformerProjects(pif);
//				for (int i = 0; i < jtransformerProjects.length; i++) {
//					// XXX: ld: i don't like that cast. Any idee?
//					JTransformerProjectNature jtransformerProject = (JTransformerProjectNature) jtransformerProjects[i];
//					jtransformerProject.reconfigure(initSession);
//
//				}
	    		updateProjectLocationInFactbase(initSession);

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
				initSession
						.queryOnce("use_module(library('org/cs3/pdt/metadata/pdtplugin'))");

				initSession.queryOnce("[library('jt_engine.pl')],"
						+ "[library('org/cs3/pdt/test/main.pl')],"
						+ "[library('org/cs3/pdt/util/main.pl')],"
						+ "[library('org/cs3/pdt/compatibility/main.pl')]");
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
		public void afterInit(PrologInterface pif)
				throws PrologInterfaceException {

			buildAllProjectsOfPif();
		}

		private void buildAllProjectsOfPif() {
			try {
				Debug.info("JT: buildAllProjectsOfPif: start");

				List tmpProjects = null;
				synchronized (toBeBuiltMonitor) {

					if (toBeBuilt.contains(project)) {
						// built triggered by other project
						Debug.info("JT: buildAllProjectsOfPif: toBeBuilt.contains(project)");

						return;
					}
					tmpProjects = getSortedListOfNotYetBuildJTProjects();
					
					Debug.info("JT: buildAllProjectsOfPif: toBeBuilt.addAll");
					toBeBuilt.addAll(tmpProjects);
				}
				IWorkspace workspace = ResourcesPlugin.getWorkspace();
				IWorkspaceDescription wd = workspace.getDescription();
				if (!wd.isAutoBuilding()) {
					return;
				}
				Debug.info("JT: finalizeProjectBuilding: before");

				final List projects = tmpProjects;
				try {
					for (Iterator iter = projects.iterator(); iter.hasNext();) {
						buildProject((IProject) iter.next());
					}
				} finally {
					finalizeProjectBuilding(projects);
				}

			} catch (Throwable e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}

		private List getSortedListOfNotYetBuildJTProjects() throws CoreException, Exception {
			String key = getPifKey();
			List unsortedProjectsList = JTUtils.getProjectsWithPifKey(key);
			if(unsortedProjectsList.size() == 0) {
				Debug.error("JTransformerSubscription.getSortedListOfNotYetBuildJTProjects(): project list ist empty!");
				unsortedProjectsList.add(project);
			} else {
				Debug.info("JT:getSortedListOfNotYetBuildJTProjects: first project name: " + ((IProject)unsortedProjectsList.get(0)).getName());
			}
			TopoSortProjects topoSorter = new TopoSortProjects();
			return topoSorter.sort(false, unsortedProjectsList);
		}

		private void finalizeProjectBuilding(final List projects) {
			Job j = new Job("Finalize building of projects") {
				public IStatus run(IProgressMonitor monitor) {
					synchronized (toBeBuiltMonitor) {
						toBeBuilt.removeAll(projects);
					}
					return Status.OK_STATUS;
				}

				public boolean belongsTo(Object family) {
					return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
				}

			};
			Debug.info("finalizeProjectBuilding: before scheduling job");
			synchronized (configureMonitor) {
//							if (!buildTriggered) {
//								buildTriggered = true;
					j.setRule(ResourcesPlugin.getWorkspace().getRoot());
					j.schedule();
					notifyJTNaturesAboutEndOfInitialization(projects);

//							}
//							buildTriggered = false;
			}
			if(projects.size() == 0) {
				Debug.error("finalizeProjectBuilding: projects size equals 0");
			} else {
				Debug.info("finalizeProjectBuilding: after scheduling job - " + ((IProject)projects.get(0)).getName());
			}
			
		}
		
		private void notifyJTNaturesAboutEndOfInitialization(final List projects) {
			for (Iterator iter = projects.iterator(); iter
					.hasNext();) {
				IProject project = (IProject) iter.next();
				try {
					if(project == this.project) {
						nature.pefInitializationDone();
					} else {
						JTUtils.getNature(project).pefInitializationDone();
					}
				} catch (CoreException e) {
					JTUtils.logAndDisplayUnknownError(e);
				}
			}
		}

		private void buildProject(final IProject project) {
			Job j = new Job("Building JTransformer PEFs for project " + project.getName()) {
				public IStatus run(IProgressMonitor monitor) {
					try {
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
		
		/**
		 * updates the projectT/4 fact
		 * 
		 * @param nature
		 * @throws PrologInterfaceException
		 * @throws PrologException
		 */
		void updateProjectLocationInFactbase(PrologSession session)
				throws PrologException, PrologInterfaceException {
			String projectName = quote(project.getName());
			String query = "retractall(" + Names.PROJECT_T + "(" + projectName
					+ ",_,_,_ ))" + "," + "assert(" + Names.PROJECT_T + "("
					+ projectName + ", "
					+ quote(project.getLocation().toPortableString()) + ","
					+ quote(JTUtils.getOutputProjectName(project)) + ", "
					+ quote(JTUtils.getOutputProjectPath(project)) + "))";
			session.queryOnce(query);

		}
		
		static private String quote(String str) {
			return "'" + str + "'";
		}

		
	}