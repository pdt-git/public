/*
 */
package org.cs3.pdt.internal.natures;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.cs3.pdt.IPrologProject;
import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 */
public class PrologProjectNature implements IProjectNature, IPrologProject {

	private IProject project;

	private Option[] options;

	/**
	 * @see IProjectNature#configure
	 */
	public void configure() throws CoreException {
		try {
			Debug.debug("configure was called");
			IProjectDescription descr = project.getDescription();
			ICommand builder = descr.newCommand();
			builder.setBuilderName(PDT.BUILDER_ID);
			ICommand builders[] = descr.getBuildSpec();
			for (int i = 0; i < builders.length; i++) {
				if (builders[i].getBuilderName().equals(PDT.BUILDER_ID)) {
					return;
				}
			}
			ICommand newBuilders[] = new ICommand[builders.length + 1];
			System.arraycopy(builders, 0, newBuilders, 0, builders.length);
			newBuilders[builders.length] = builder;
			descr.setBuildSpec(newBuilders);
			project.setDescription(descr, null);

			PrologInterface pif = getPrologInterface();
			if (pif.isUp()) {
				Job j = new Job("building Prolog Metadata for project "
						+ project.getName()) {
					protected IStatus run(IProgressMonitor monitor) {
						try {
							IWorkspaceDescription wd = ResourcesPlugin
									.getWorkspace().getDescription();
							if (wd.isAutoBuilding()) {
								project.build(
										IncrementalProjectBuilder.FULL_BUILD,
										PDT.BUILDER_ID, null, monitor);
							}
						} catch (OperationCanceledException opc) {
							return Status.CANCEL_STATUS;
						} catch (CoreException e) {
							return new Status(Status.ERROR, PDT.PLUGIN_ID, -1,
									"exception caught during build", e);
						}
						return Status.OK_STATUS;
					}

					public boolean belongsTo(Object family) {
						return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
					}

				};
				j.schedule();
			} else {
				// if the pif is not up yet, this is no problem at all: the
				// reload
				// hook will
				// take care of the due build in its afterInit method.
				;
			}

		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	/**
	 * @see IProjectNature#deconfigure
	 */
	public void deconfigure() throws CoreException {
		try {
			IProjectDescription descr = project.getProject().getDescription();
			org.cs3.pl.common.Debug.debug("deconfigure was called");
			ICommand builders[] = descr.getBuildSpec();
			int index = -1;
			for (int i = 0; i < builders.length; i++) {
				if (builders[i].getBuilderName().equals(PDT.BUILDER_ID)) {
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
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#getProject()
	 */
	public IProject getProject() {
		return this.project;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
	 */
	public void setProject(IProject project) {
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.IPrologProject#getExistingSourcePathEntries()
	 */
	public Set getExistingSourcePathEntries() throws CoreException {
		Set r = new HashSet();
		String[] elms = getPreferenceValue(PDT.PROP_SOURCE_PATH, "/").split(
				System.getProperty("path.separator"));
		for (int i = 0; i < elms.length; i++) {
			IProject p = getProject();

			if (elms[i] == null || "".equals(elms[i])
					|| File.separator.equals(elms[i])
					|| "/".equals(elms[i].trim())) {
				r.add(p);
			} else {
				IFolder folder = p.getFolder(elms[i]);
				if (folder.exists()) {
					r.add(folder);
				}
			}
		}
		return r;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.IPrologProject#isPrologSource(org.eclipse.core.resources.IResource)
	 */
	public boolean isPrologSource(IResource resource) throws CoreException {
		Set sourcePathEntries = getExistingSourcePathEntries();

		if (!resource.exists()) {
			return false;
		}
		if (sourcePathEntries.contains(resource)) {
			return true;
		}
		if (resource.getType() == IResource.FILE) {
			String incl = getPreferenceValue(PDT.PROP_SOURCE_INCLUSION_PATTERN,
					"");
			String excl = getPreferenceValue(PDT.PROP_SOURCE_EXCLUSION_PATTERN,
					"");
			String path = resource.getFullPath().toString();
			return isPrologSource(resource.getParent()) && path.matches(incl)
					&& !path.matches(excl);
		} else if (resource.getType() == IResource.FOLDER) {
			return isPrologSource(resource.getParent());
		}
		return false;
	}

	public void setAutoConsulted(IFile file, boolean val) throws CoreException {
		file.setPersistentProperty(
		// TODO: toggled functionality - to test
				new QualifiedName("", PDT.PROP_NO_AUTO_CONSULT), val ? "false"
						: "true");
	}

	public boolean isAutoConsulted(IFile file) throws CoreException {
		// if it is no source file, there is no need to consult it.
		if (!isPrologSource(file)) {
			return false;
		}

		// if the "master switch" says "no auto-consult", then there is no
		// auto-consult.
		String val = PDTPlugin.getDefault().getPreferenceValue(
				PDT.PREF_AUTO_CONSULT, "false");
		if ("false".equalsIgnoreCase(val)) {
			return false;
		}

		// finally, if auto-consult is enabled, and the file is not explicitly
		// excluded, then auto-consult it.
		val = file.getPersistentProperty(new QualifiedName("",
				PDT.PROP_NO_AUTO_CONSULT));
		// TODO: toggled functionality - to test
		boolean autoConsult = !(val != null && val.equalsIgnoreCase("true"));
		return autoConsult;
	}

	public PrologInterface getPrologInterface() {
		PDTPlugin r = PDTPlugin.getDefault();
		PrologInterface pif = PrologRuntimePlugin.getDefault().getPrologInterface(getProject().getName());

		if (!pif.isUp()) {
			try {
				pif.start();
			} catch (IOException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}

		return pif;
	}

	public Option[] getOptions() {
		if (options == null) {
			options = new Option[] {
					new SimpleOption(
							PDT.PROP_SOURCE_PATH,
							"Source Path",
							"List of folders in which the PDT looks for Prolog code.",
							Option.DIRS, "/") {
						public String getDefault() {
							return PDTPlugin.getDefault().getPreferenceValue(
									PDT.PREF_SOURCE_PATH_DEFAULT, "/");
						}
					},
					new SimpleOption(
							PDT.PROP_SOURCE_INCLUSION_PATTERN,
							"Inclusion Pattern",
							"Regular expression - only matched files are considered prolog source code.",
							Option.STRING, ".*\\.pl"),
					new SimpleOption(
							PDT.PROP_SOURCE_EXCLUSION_PATTERN,
							"Exclusion Pattern",
							"Regular expression - matched files are excluded even if they match the inclusion pattern.",
							Option.STRING, ""), };
		}
		return options;
	}

	public void reconfigure() {
		Job j = new Job("Building Prolog Metadata") {
			public IStatus run(IProgressMonitor monitor) {
				try {

					;
					IProject project = getProject();
					Debug.debug("PDTReloadHook.afterInit: lets build project "
							+ project);
					
					project.build(IncrementalProjectBuilder.CLEAN_BUILD,
							PDT.BUILDER_ID, null, monitor);
					
					Debug.debug("PDTReloadHook.afterInit: done: " + project);

				} catch (OperationCanceledException opc) {
					return Status.CANCEL_STATUS;
				} catch (Exception e) {
					return new Status(IStatus.ERROR, PDT.PLUGIN_ID, -1,
							"Problems during build", e);
				}
				return Status.OK_STATUS;
			}

			public boolean belongsTo(Object family) {
				return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
			}

		};

		j.setRule(ResourcesPlugin.getWorkspace().getRoot());
		j.schedule();

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
	public String getPreferenceValue(String key, String defaultValue) {
		String value = System.getProperty(key);
		if (value != null) {
			return value;
		}
		try {
			value = getProject().getPersistentProperty(
					new QualifiedName("", key));
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

	public void setPreferenceValue(String id, String value) {
		try {
			getProject()
					.setPersistentProperty(new QualifiedName("", id), value);
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}

}
