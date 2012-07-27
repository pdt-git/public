/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class PDTCommonPlugin implements BundleActivator {

	public static final QualifiedName ENTRY_POINT_KEY = new QualifiedName("pdt", "entry.point");
	
	private static BundleContext context;

	private static PDTCommonPlugin plugin;
	
	public PDTCommonPlugin() {
		super();
		plugin = this;
	}
	
	static BundleContext getContext() {
		return context;
	}
	
	/**
	 * Returns the shared instance.
	 */
	public static PDTCommonPlugin getDefault() {
		return plugin;
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		PDTCommonPlugin.context = null;
	}
	
	
	@Override
	public void start(BundleContext bundleContext) throws Exception{
		PDTCommonPlugin.context = bundleContext;
		ResourcesPlugin.getWorkspace().getRoot().accept(new IResourceVisitor() {
			@Override
			public boolean visit(IResource resource) throws CoreException {
				if (resource instanceof IFile) {
					IFile file = (IFile) resource;
					if ("true".equalsIgnoreCase(file.getPersistentProperty(ENTRY_POINT_KEY))) {
						addEntryPoint(file);
					}
				}
				return true;
			}
			
		});

	}
	
	/* 
	 * entry point handling
	 */
	private Set<IFile> entryPoints = new HashSet<IFile>();

	public void addEntryPoint(IFile f) {
		entryPoints.add(f);
	}

	public void removeEntryPoint(IFile f) {
		entryPoints.remove(f);
	}

	public Set<IFile> getEntryPoints() {
		return entryPoints;
	}

}


