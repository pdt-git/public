package org.cs3.pdt.transform;

import java.io.File;
import java.net.URL;
import java.util.HashMap;

import org.cs3.pl.common.Debug;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class PDTTransformationsPlugin extends AbstractUIPlugin implements IStartup{

	// The plug-in ID
	public static final String PLUGIN_ID = "org.cs3.pdt.transform";

	// The shared instance
	private static PDTTransformationsPlugin plugin;

	private HashMap<String, PrologRefactoringDescriptor> refactoringDescriptors;

	/**
	 * The constructor
	 */
	public PDTTransformationsPlugin() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static PDTTransformationsPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in
	 * relative path
	 * 
	 * @param path
	 *            the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	private void initializeRefactoringDescriptors() throws CoreException {
		refactoringDescriptors = new HashMap<String, PrologRefactoringDescriptor>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(
				PDTTransform.PLUGIN_ID, PDTTransform.EP_REFACTORINGS);
		if (point == null) {
			Debug.error("could not find the extension point "
					+ PDTTransform.EP_REFACTORINGS);
			throw new RuntimeException("could not find the extension point "
					+ PDTTransform.EP_REFACTORINGS);
		}

		IExtension[] extensions = point.getExtensions();

		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] celms = extension
					.getConfigurationElements();
			for (int j = 0; j < celms.length; j++) {
				IConfigurationElement celm = celms[j];

				PrologRefactoringDescriptor desc = createDescriptor(celm,
						extension);
				if (desc != null) {
					refactoringDescriptors.put(desc.getId(), desc);
				}
			}
		}

	}

	public PrologRefactoringDescriptor getPrologRefactoringDescriptor(String id)
			throws CoreException {
		if (refactoringDescriptors == null) {
			initializeRefactoringDescriptors();
		}
		return refactoringDescriptors.get(id);
	}

	private PrologRefactoringDescriptor createDescriptor(
			IConfigurationElement celm, IExtension extension)
			throws CoreException {
		PrologRefactoringDescriptor desc = (PrologRefactoringDescriptor) celm
				.createExecutableExtension("descriptor");
		desc.setAdaptable(Boolean.parseBoolean(celm.getAttribute("adaptable")));
		desc.setDescription(celm.getAttribute("description"));
		desc.setId(celm.getAttribute("id"));
		desc.setLabel(celm.getAttribute("label"));
		if (celm.getAttribute("objectClass") != null) {
			try {
				desc.setObjectClass(Class.forName(celm
						.getAttribute("objectClass")));
			} catch (ClassNotFoundException e) {
				throw new CoreException(
						new Status(
								Status.ERROR,
								PDTTransform.PLUGIN_ID,
								"failed to create descriptor, objectClass could not be resolved",
								e));
			}
		}
		IConfigurationElement[] children = celm.getChildren("definition");
		File[] definitions = new File[children.length];
		for (int i = 0; i < children.length; i++) {
			String resName = children[i].getAttribute("path");
			Debug.debug("got this resname: " + resName);
			String namespace = extension.getNamespaceIdentifier();
			Debug.debug("got this namespace: " + namespace);
			URL url = FileLocator.find(Platform.getBundle(namespace), new Path(
					resName), null);
			try {
				Debug.debug("trying to resolve this url: " + url);
				url = FileLocator.toFileURL(url);
			} catch (Exception e) {
				Debug.rethrow("Problem resolving url: " + url.toString(), e);
			}
			definitions[i] = new File(url.getFile());

		}
		desc.setDefinitions(definitions);

		children = celm.getChildren("dependency");
		String[] dependencies = new String[children.length];
		for (int i = 0; i < dependencies.length; i++) {

			dependencies[i] = children[i].getAttribute("libraryId");
		}
		desc.setDependencies(dependencies);
		return desc;
	}

	@Override
	public void earlyStartup() {
	}
}
