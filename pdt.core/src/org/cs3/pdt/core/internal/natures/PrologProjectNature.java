/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pdt.core.internal.natures;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.internal.builder.UpdateMarkersJob;
import org.cs3.pdt.core.internal.properties.AnnotatorsOptionProvider;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.OptionProvider;
import org.cs3.pl.common.OptionProviderEvent;
import org.cs3.pl.common.OptionProviderListener;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.MetaInfoProviderFactory;
import org.cs3.pl.prolog.DefaultPrologLibrary;
import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibrary;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;

/**
 */
public class PrologProjectNature implements IProjectNature, IPrologProject {

	private IProject project;

	private Option[] options;

	private PrologInterface metadataPif;

	private PrologInterface runtimePif;

	private Subscription runtimePifSubscription;

	private Subscription metadataPifSubscription;

	private HashMap<String, DefaultPrologLibrary> libraries;

	private Vector<OptionProviderListener> listeners = new Vector<OptionProviderListener>();

	private AnnotatorsOptionProvider annotatorsOptionProvider;

	

	/**
	 * @see IProjectNature#configure
	 */
	@Override
	public void configure() throws CoreException {

		Debug.debug("configure was called");

		addBuilder(PDTCore.PROLOG_BUILDER_ID);

		registerLibraries();

	}

	private void addBuilder(String builderId) throws CoreException {
		IProjectDescription descr = project.getDescription();
		ICommand builder = descr.newCommand();
		builder.setBuilderName(builderId);
		ICommand builders[] = descr.getBuildSpec();
		for (int i = 0; i < builders.length; i++) {
			if (builders[i].getBuilderName().equals(builderId)) {
				return;
			}
		}
		ICommand newBuilders[] = new ICommand[builders.length + 1];
		System.arraycopy(builders, 0, newBuilders, 0, builders.length);
		newBuilders[builders.length] = builder;
		descr.setBuildSpec(newBuilders);
		project.setDescription(descr, null);
	}

	private void registerLibraries() throws CoreException {
		HashMap<String, DefaultPrologLibrary> libs = getPrologLibraries();
		PrologLibraryManager mgr = PrologRuntimeUIPlugin.getDefault()
				.getLibraryManager();
		for (Iterator<DefaultPrologLibrary> it = libs.values().iterator(); it.hasNext();) {
			PrologLibrary lib = it.next();
			mgr.addLibrary(lib);
		}

	}

	private void unregisterLibraries() throws CoreException {
		HashMap<String, DefaultPrologLibrary> libs = getPrologLibraries();
		PrologLibraryManager mgr = PrologRuntimeUIPlugin.getDefault()
				.getLibraryManager();
		for (Iterator<DefaultPrologLibrary> it = libs.values().iterator(); it.hasNext();) {
			PrologLibrary lib = it.next();
			mgr.removeLibrary(lib);
		}

	}

	/**
	 * @see IProjectNature#deconfigure
	 */
	@Override
	public void deconfigure() throws CoreException {
		try {
			unregisterSubscriptions();
			unregisterLibraries();

			removeBuilder(PDTCore.PROLOG_BUILDER_ID);
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	private void removeBuilder(String builderId) throws CoreException {
		IProjectDescription descr = project.getProject().getDescription();
		org.cs3.pl.common.Debug.debug("deconfigure was called");
		ICommand builders[] = descr.getBuildSpec();
		int index = -1;
		for (int i = 0; i < builders.length; i++) {
			if (builders[i].getBuilderName().equals(builderId)) {
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
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#getProject()
	 */
	@Override
	public IProject getProject() {
		return this.project;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
	 */
	@Override
	public void setProject(IProject project) {
		if (this.project != null) {

		}
		this.project = project;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.IPrologProject#getExistingSourcePathEntries()
	 */
	@Override
	public Set<IContainer> getExistingSourcePathEntries() throws CoreException {
		Set<IContainer> r = new HashSet<IContainer>();
		String[] elms = getPreferenceValue(PDTCore.PROP_SOURCE_PATH, "/")
				.split(System.getProperty("path.separator"));
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
	 * @see org.cs3.pdt.IPrologProject#getExistingSourcePathEntries()
	 */
	@Override
	public Set<IFile> getExistingEntryPoints() throws CoreException {
		Set<IFile> r = new HashSet<IFile>();
		String[] elms = getPreferenceValue(PDTCore.PROP_ENTRY_POINTS, "")
				.split(System.getProperty("path.separator"));
		for (int i = 0; i < elms.length; i++) {
			if(elms[i]==null||elms[i].length() == 0){
				continue;
			}
			IProject p = getProject();

			IFile folder = p.getFile(elms[i]);
			if (folder.exists()) {
				r.add(folder);
			}

		}
		return r;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pdt.IPrologProject#isPrologSource(org.eclipse.core.resources.IResource)
	 */
	@Override
	public boolean isPrologSource(IResource resource) throws CoreException {
		Set<IContainer> sourcePathEntries = getExistingSourcePathEntries();

//		if (!resource.exists()) {
//			return false;
//		}
		if (sourcePathEntries.contains(resource)) {
			return true;
		}
		if (resource.getType() == IResource.FILE) {
			String incl = getPreferenceValue(
					PDTCore.PROP_SOURCE_INCLUSION_PATTERN, "");
			String excl = getPreferenceValue(
					PDTCore.PROP_SOURCE_EXCLUSION_PATTERN, "");
			String path = resource.getFullPath().toString();
			return isPrologSource(resource.getParent()) && path.matches(incl)
					&& !path.matches(excl);
		} else if (resource.getType() == IResource.FOLDER) {
			return isPrologSource(resource.getParent());
		}
		return false;
	}

	@Override
	public void setAutoConsulted(IFile file, boolean val) throws CoreException {
		file.setPersistentProperty(
				// TODO: toggled functionality - to test
				new QualifiedName("", PDTCore.PROP_NO_AUTO_CONSULT),
				val ? "false" : "true");
	}

	@Override
	public boolean isAutoConsulted(IFile file) throws CoreException {
		// if it is no source file, there is no need to consult it.
		if (!isPrologSource(file)) {
			return false;
		}

		// if the "master switch" says "no auto-consult", then there is no
		// auto-consult.
		String val = PDTCorePlugin.getDefault().getPreferenceValue(
				PDTCore.PREF_AUTO_CONSULT, "false");
		if ("false".equalsIgnoreCase(val)) {
			return false;
		}

		// finally, if auto-consult is enabled, and the file is not explicitly
		// excluded, then auto-consult it.
		val = file.getPersistentProperty(new QualifiedName("",
				PDTCore.PROP_NO_AUTO_CONSULT));
		// TODO: toggled functionality - to test
		boolean autoConsult = !(val != null && val.equalsIgnoreCase("true"));
		return autoConsult;
	}

	@Override
	public PrologInterface getMetadataPrologInterface() {
		if (metadataPif == null) {
			metadataPif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(
					getMetadataSubscription());

		}
		return metadataPif;
	}

	@Override
	public PrologInterface getRuntimePrologInterface() {
		if (runtimePif == null) {
			runtimePif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(
					getRuntimeSubscription());

		}
		return runtimePif;
	}

	protected void registerSubscriptions() {
		// this should do the trick:
		getMetadataSubscription();
		getRuntimeSubscription();
	}

	protected void unregisterSubscriptions() throws PrologInterfaceException {
		// this should do the trick:
		PrologInterfaceRegistry r = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		r.removeSubscription(getMetadataSubscriptionKey());
		r.removeSubscription(getRuntimeSubscriptionKey());
	}

	@Override
	public Subscription getMetadataSubscription() {
		String id = getMetadataSubscriptionKey();
		String pifKey = getMetadataPrologInterfaceKey();
		PrologInterfaceRegistry r = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		if (metadataPifSubscription == null) {
			metadataPifSubscription = r.getSubscription(id);
		}
		if (metadataPifSubscription == null) {
			metadataPifSubscription = new MetadataSubscription(getProject()
					.getName(), id, pifKey);
			r.addSubscription(metadataPifSubscription);
		}

		return metadataPifSubscription;
	}

	@Override
	public Subscription getRuntimeSubscription() {
		String id = getRuntimeSubscriptionKey();
		PrologInterfaceRegistry r = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		if (runtimePifSubscription == null) {
			runtimePifSubscription = r.getSubscription(id);
		}
		if (runtimePifSubscription == null) {
			String pifID = getRuntimePrologInterfaceKey();
			String projectName = getProject().getName();
			runtimePifSubscription = new RuntimeSubscription(projectName, id,
					pifID);
			r.addSubscription(runtimePifSubscription);
		}

		return runtimePifSubscription;
	}

	private String getRuntimeSubscriptionKey() {
		return getProject().getName() + ".runtime_subscription";
	}

	private String getMetadataSubscriptionKey() {
		return getProject().getName() + ".metadata_subscription";
	}

	private String getRuntimePrologInterfaceKey() {
		String value = getPreferenceValue(PDTCore.PROP_RUNTIME_PIF_KEY,
				"%project%");
		value = value.replaceAll("%project%", getProject().getName());
		return value;
	}

	private String getMetadataPrologInterfaceKey() {
		String value = getPreferenceValue(PDTCore.PROP_METADATA_PIF_KEY,
				"%project%");
		value = value.replaceAll("%project%", getProject().getName());
		return value;
	}

	@Override
	public Option[] getOptions() {
		if (options == null) {
			options = new Option[] {
					new SimpleOption(
							PDTCore.PROP_SOURCE_PATH,
							"Source Path",
							"List of folders in which the PDT looks for Prolog code.",
							Option.DIRS, "/") {
						@Override
						public String getDefault() {
							return PDTCorePlugin.getDefault()
									.getPreferenceValue(
											PDTCore.PREF_SOURCE_PATH_DEFAULT,
											"/");
						}

						@Override
						public String getHint(String key) {
							if (UIUtils.IS_WORKSPACE_RESOURCE.equals(key)) {
								return "true";
							}
							if (UIUtils.ROOT_CONTAINER.equals(key)) {
								return getProject().getFullPath()
										.toPortableString();
							}
							if (UIUtils.RELATIVE.equals(key)) {
								return "true";
							}
							return null;
						}
					},
					new SimpleOption(
							PDTCore.PROP_SOURCE_INCLUSION_PATTERN,
							"Inclusion Pattern",
							"Regular expression - only matched files are considered prolog source code.",
							Option.STRING, ".*\\.pl"),
					new SimpleOption(
							PDTCore.PROP_SOURCE_EXCLUSION_PATTERN,
							"Exclusion Pattern",
							"Regular expression - matching files are NOT considered prolog source code, even if \n"
									+ "they match the inclusion pattern above.",
							Option.STRING, ""),
					new SimpleOption(
							PDTCore.PROP_ADDITIONAL_LIBRARIES,
							"Additional Libraries",
							"A list of directories that should be included in the file search path.",
							Option.DIRS, ""),
					new SimpleOption(PDTCore.PROP_ENTRY_POINTS,
							"Program Entry Points", "List of 'main' files",
							Option.FILES, "") {

						@Override
						public String getHint(String key) {
							if (UIUtils.IS_WORKSPACE_RESOURCE.equals(key)) {
								return "true";
							}
							if (UIUtils.ROOT_CONTAINER.equals(key)) {
								return getProject().getFullPath()
										.toPortableString();
							}
							if (UIUtils.RELATIVE.equals(key)) {
								return "true";
							}
							return null;
						}
					},
					new SimpleOption(
							PDTCore.PROP_PARSE_COMMENTS,
							"Parse Comments",
							"If true, the pdt core will parse and process comments in prolog source files.",
							Option.FLAG, "true"),
					new SimpleOption(
							PDTCore.PROP_DEFAULT_ENCODING,
							"Default Encoding",
							"The Encoding to use by default for all prolog source files.",
							Option.ENUM, "utf8", new String[][] {
									{ "octet", "octet" }, { "ascii", "ascii" },
									{ "iso_latin_1", "iso_latin_1" },
									{ "text", "text" }, { "utf8", "utf8" },
									{ "unicode_be", "unicode_be" },
									{ "unicode_le", "unicode_le" } }),

					new SimpleOption(
							PDTCore.PROP_METADATA_PIF_KEY,
							"Metadata PrologInterface",
							"The key identifying the PrologInterface instance used by the pdt core to store"
									+ " meta information on the project. Any occurance of the string %project% will be"
									+ " replaced with the project name.",
							Option.STRING, "") {
						@Override
						public String getDefault() {
							return PDTCorePlugin
									.getDefault()
									.getPreferenceValue(
											PDTCore.PREF_METADATA_PIF_KEY_DEFAULT,
											"%project%-meta");
						}

					},
					new SimpleOption(
							PDTCore.PROP_RUNTIME_PIF_KEY,
							"Runtime PrologInterface",
							"The key identifying the PrologInterface instance that"
									+ "will be used by default to consult prolog files into it. Any occurance of the string %project% will be"
									+ " replaced with the project name.",
							Option.STRING, "") {
						@Override
						public String getDefault() {
							return PDTCorePlugin
									.getDefault()
									.getPreferenceValue(
											PDTCore.PREF_RUNTIME_PIF_KEY_DEFAULT,
											"%project%-runtime");
						}
					}, };
		}
		return options;
	}

	@Override
	public void updateBuildPath(PrologSession initSession)
			throws CoreException, PrologInterfaceException {
		String include = getPreferenceValue(
				PDTCore.PROP_SOURCE_INCLUSION_PATTERN, ".*\\.pl");
		String exclude = getPreferenceValue(
				PDTCore.PROP_SOURCE_EXCLUSION_PATTERN, "");
		String projectName = getProject().getName();

		Set<IContainer> sourcePathEntries = getExistingSourcePathEntries();
		for (IContainer entry : sourcePathEntries) {
			File osPath = entry.getLocation().toFile();
			String plPath = Util.prologFileName(osPath);
			initSession.queryOnce("pdt_add_source_path('" + projectName + "','"
					+ plPath + "','" + include + "','" + exclude + "')");
		}

		Set<IFile> entryPoints = getExistingEntryPoints();
		for (IFile entry : entryPoints) {
			File osPath = entry.getLocation().toFile();
			String plPath = Util.prologFileName(osPath);
			initSession.queryOnce("pdt_add_entry_point('" + projectName + "','"
					+ plPath + "')");
		}
	}

	@Override
	public void reconfigure() {
		Job j = new Job("Building Prolog Metadata") {
			@Override
			public IStatus run(IProgressMonitor monitor) {
				try {

					PrologInterface pif = getMetadataPrologInterface();
					PrologSession s = null;
					try {
						s = pif.getSession(PrologInterface.NONE);
						updateBuildPath(s);
					} finally {
						if (s != null) {
							s.dispose();
						}
					}
					IProject project = getProject();
					Debug.debug("PDTReloadHook.afterInit: lets build project "
							+ project);

					project.build(IncrementalProjectBuilder.CLEAN_BUILD,
							PDTCore.PROLOG_BUILDER_ID, null, monitor);

					Debug.debug("PDTReloadHook.afterInit: done: " + project);

				} catch (OperationCanceledException opc) {
					return Status.CANCEL_STATUS;
				} catch (Exception e) {
					return new Status(IStatus.ERROR, PDTCore.PLUGIN_ID, -1,
							"Problems during build", e);
				}
				return Status.OK_STATUS;
			}

			@Override
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
	@Override
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

	@Override
	public void setPreferenceValue(String id, String value) {

		try {

			String oldValue = getPreferenceValue(id, "");
			if (!oldValue.equals(value)) {
				getProject().setPersistentProperty(new QualifiedName("", id),
						value);

				if (PDTCore.PROP_RUNTIME_PIF_KEY.equals(id)
						|| PDTCore.PROP_METADATA_PIF_KEY.equals(id)) {
					pifKeysChanged();

				}
				fireValueChanged(id);
			}

		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}

	@Override
	public void setPreferenceValues(String[] ids, String[] values) {
		Vector<String> changed = new Vector<String>();
		try {
			for (int i = 0; i < values.length; i++) {
				String value = values[i];
				String id = ids[i];
				String oldValue = getPreferenceValue(id, "");
				if (!oldValue.equals(value)) {
					getProject().setPersistentProperty(
							new QualifiedName("", id), value);
					if (PDTCore.PROP_RUNTIME_PIF_KEY.equals(id)
							|| PDTCore.PROP_METADATA_PIF_KEY.equals(id)) {
						pifKeysChanged();
					}
					changed.add(id);
				}

			}
			if (!changed.isEmpty()) {
				fireValuesChanged(changed.toArray(new String[changed
						.size()]));
			}

		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}

	}

	@Override
	public OptionProvider getAnnotatorsOptionProvider()
			throws PrologInterfaceException {
		if (annotatorsOptionProvider == null) {
			annotatorsOptionProvider = new AnnotatorsOptionProvider(this);
			IPrologEventDispatcher eventDispatcher = PrologRuntimeUIPlugin.getDefault().getPrologEventDispatcher(
					getMetadataPrologInterface());
			eventDispatcher.addPrologInterfaceListener(
					AnnotatorsOptionProvider.SUBJECT, annotatorsOptionProvider);
		}
		return annotatorsOptionProvider;
	}

	private void pifKeysChanged() {
		PrologInterfaceRegistry reg = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		if (runtimePif != null) {
			reg.removeSubscription(runtimePifSubscription);
			runtimePifSubscription = null;
			runtimePif = null;

		}
		if (metadataPif != null) {
			reg.removeSubscription(metadataPifSubscription);
			metadataPifSubscription = null;
			metadataPif = null;
		}

	}

	@Override
	public IMetaInfoProvider getMetaInfoProvider() {
		return MetaInfoProviderFactory.newInstance().create(
				getMetadataPrologInterface());
	}

	@Override
	public String[] getPrologLibraryKeys() throws CoreException {

		HashMap<String, DefaultPrologLibrary> libs = getPrologLibraries();

		return libs.keySet().toArray(new String[libs.size()]);
	}

	public HashMap<String, DefaultPrologLibrary> getPrologLibraries() throws CoreException {
		if (libraries == null) {
			createLibraries();
			registerLibraries();
		}
		return libraries;
	}

	private void createLibraries() throws CoreException {

		Set<IContainer> s = getExistingSourcePathEntries();
		libraries = new HashMap<String, DefaultPrologLibrary>();
		for (Iterator<IContainer> it = s.iterator(); it.hasNext();) {
			IContainer c = it.next();
			File f = c.getLocation().toFile();
			String key = c.getFullPath().toString();
			if (f != null) {
				libraries.put(key, new DefaultPrologLibrary(key, new String[0], // TODO
						// let
						// user
						// define
						// dependencies
						"library", Util.prologFileName(f)));
			}
		}
		String string = getPreferenceValue(PDTCore.PROP_ADDITIONAL_LIBRARIES,
				"");
		String[] strings = Util.split(string, System
				.getProperty("path.separator"));
		for (int i = 0; i < strings.length; i++) {
			File f = new Path(strings[i]).toFile();
			String key = null;
			try {
				key = f.getCanonicalPath();
			} catch (IOException e) {

				e.printStackTrace();
			}
			libraries.put(key, new DefaultPrologLibrary(key, new String[0],
					"library", Util.prologFileName(f)));
		}

	}

	@Override
	public void addOptionProviderListener(OptionProviderListener l) {
		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}
	}

	@Override
	public void removeOptionProviderListener(OptionProviderListener l) {
		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}

	}

	protected void fireValueChanged(String id) {
		OptionProviderEvent e = new OptionProviderEvent(this, id);
		Vector<OptionProviderListener> clone = new Vector<OptionProviderListener>();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		for (Iterator<OptionProviderListener> it = clone.iterator(); it.hasNext();) {
			OptionProviderListener l = it.next();
			l.valuesChanged(e);
		}
	}

	private void fireValuesChanged(String[] ids) {
		OptionProviderEvent e = new OptionProviderEvent(this, ids);
		Vector<OptionProviderListener> clone = new Vector<OptionProviderListener>();
		synchronized (listeners) {
			clone.addAll(listeners);
		}
		for (Iterator<OptionProviderListener> it = clone.iterator(); it.hasNext();) {
			OptionProviderListener l = it.next();
			l.valuesChanged(e);
		}

	}

	private static class Mutex implements ISchedulingRule {
		@Override
		public boolean isConflicting(ISchedulingRule rule) {
			return rule == this;
		}

		@Override
		public boolean contains(ISchedulingRule rule) {
			return rule == this;
		}
	}

	private static Mutex mux = new Mutex();

	@Override
	public void updateMarkers() throws CoreException {
		UpdateMarkersJob job = new UpdateMarkersJob(this,"cheap");
		job.setPriority(Job.INTERACTIVE);
		job.setRule(mux);
		job.schedule();
	}

}
