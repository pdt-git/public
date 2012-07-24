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

package org.cs3.pdt.internal.decorators;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTPredicates;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.prolog.common.OptionProviderEvent;
import org.cs3.prolog.common.OptionProviderListener;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;

public class PDTConsultDecoratorContributor implements ILightweightLabelDecorator, OptionProviderListener {

	private Vector<ILabelProviderListener> listeners = new Vector<ILabelProviderListener>();

	@Override
	public void addListener(ILabelProviderListener l) {

		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}
		}
		
	}

	@Override
	public void dispose() {
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener l) {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
			}
		}
		
	}
	
	HashMap<Object, Long> elementDecorated = new HashMap<Object, Long>();

	@Override
	public void decorate(Object element, IDecoration decoration) {
		if(!(element instanceof IFile) && !(element instanceof IFolder)){
			return;
		}
		
		Long lastElementUpdate = elementDecorated.get(element);
		
		if (lastElementUpdate != null && lastElementUpdate > lastUpdate) {
			fireLabelProviderChanged();
		}
		elementDecorated.put(element, System.currentTimeMillis());

		PDTPlugin.getDefault().addDecorator(this);

		// get active pif from console
		PrologInterface currentPif = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();

		if (currentPif == null) {
			if (element instanceof IFile) {
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PROLOG_FILE_UNCONSULTED), IDecoration.UNDERLAY);
			}
			return;
		}
		
		if (element instanceof IFile) {
			IFile file = (IFile) element;
			
//			final DecorationContext decorationContext = (DecorationContext) decoration.getDecorationContext();
//			decorationContext.putProperty(IDecoration.ENABLE_REPLACE, Boolean.TRUE);

			// check if file is in consulted files list (important for qlf files)
			String prologFileName = getPrologFileName(file);
			
			// XXX: don't mark qlf file if only the pl file is consulted 
			if(prologFileName.endsWith(".qlf")){
				prologFileName = prologFileName.substring(0, prologFileName.length()-3)+"pl";
			}
			// check if file is source_file
			if (sourceFiles.contains(prologFileName)) {
				decoration.addSuffix(" [consulted]");
				if (file.getFileExtension().equalsIgnoreCase("QLF")) {
					decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.QLF_FILE_CONSULTED), IDecoration.BOTTOM_LEFT);
				} else {
					decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PROLOG_FILE_CONSULTED), IDecoration.UNDERLAY);
				}
			} else if (modifiedSourceFiles.contains(prologFileName)) {
				decoration.addSuffix(" [consulted]");
				if (file.getFileExtension().equalsIgnoreCase("QLF")) {
					decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.QLF_FILE_CONSULTED_OLD), IDecoration.BOTTOM_LEFT);
				} else {
					decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PROLOG_FILE_CONSULTED_OLD), IDecoration.UNDERLAY);
				}
			} else {
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PROLOG_FILE_UNCONSULTED), IDecoration.UNDERLAY);
			}
		} else {
			IFolder folder = (IFolder) element;
			String dirName = Util.prologFileName(folder.getRawLocation().toFile());
			
			if (dirs.contains(dirName)) {
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PROLOG_FOLDER_CONSULTED), IDecoration.TOP_LEFT);
			}
		}
	}
	
	private String getPrologFileName(IFile file) {
		String enclFile = file.getRawLocation().toPortableString();
		if (Util.isWindows()) {
			enclFile = enclFile.toLowerCase();
		}

		IPath filepath = new Path(enclFile);
		return Util.prologFileName(filepath.toFile());
	}

	@Override
	public void valuesChanged(OptionProviderEvent e) {
		fireLabelProviderChanged();
	}

	Set<String> sourceFiles = new HashSet<String>();
	Set<String> modifiedSourceFiles = new HashSet<String>();
	Set<String> dirs= new HashSet<String>();
	private long lastUpdate = 0;

	private void fireLabelProviderChanged() {
		

		sourceFiles.clear();
		modifiedSourceFiles.clear();
		dirs.clear();
		
		PrologInterface currentPif = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();
		if (currentPif == null) {
			return;
		}
		try {
			List<Map<String, Object>> results = currentPif.queryAll(bT(PDTPredicates.PDT_SOURCE_FILE, "File", "State"));
			lastUpdate = System.currentTimeMillis();
			for (Map<String, Object> result: results) {
				String fileName = result.get("File").toString();
				String fileState = result.get("State").toString();
				if ("current".equals(fileState)) {
					sourceFiles.add(fileName);
				} else {
					modifiedSourceFiles.add(fileName);
				}
				String parentDir = fileName.substring(0, fileName.lastIndexOf('/'));
				while (dirs.add(parentDir)) {
					int parentDirEndPos = parentDir.lastIndexOf('/');
					if (parentDirEndPos < 0) {
						break;
					}
					parentDir = parentDir.substring(0, parentDirEndPos);
				}
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		}

		final LabelProviderChangedEvent e = new LabelProviderChangedEvent(this);
		Vector<ILabelProviderListener> clone=new Vector<ILabelProviderListener>();
		synchronized(listeners){
			clone.addAll(listeners);
		}
		for (Iterator<ILabelProviderListener> it = clone.iterator(); it.hasNext();) {
			final ILabelProviderListener l = (ILabelProviderListener) it.next();
			UIUtils.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					l.labelProviderChanged(e);
				}
			});
		}
	}

}


