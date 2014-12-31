/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.common.callhierachy;

import static org.cs3.prolog.connector.common.QueryUtils.bT;
import static org.cs3.prolog.connector.common.QueryUtils.quoteAtom;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Display;

public class LocationProvider {

	private HashMap<PredicateEdge, TreeSet<Location>> cachedLocations = new HashMap<>();

	private int mode;

	private TableViewer locationTableViewer;

	public LocationProvider(TableViewer locationTableViewer) {
		this.locationTableViewer = locationTableViewer;
	}
	
	public void fillLocations(final PredicateEdge edge) {
		if (edge == null) {
			locationTableViewer.setInput(new ArrayList<Location>());
		} else {
			synchronized (cachedLocations) {
				TreeSet<Location> locations = cachedLocations.get(edge);
				if (locations != null) {
					locationTableViewer.setInput(locations);
				} else {
					final TreeSet<Location> newLocations = new TreeSet<>();
					Job j = getLocationFillJob(edge, newLocations);
					j.addJobChangeListener(new JobChangeAdapter() {
						@Override
						public void done(IJobChangeEvent event) {
							Display.getDefault().asyncExec(new Runnable() {
								@Override
								public void run() {
									locationTableViewer.setInput(newLocations);
								}
							});
						}
					});
					j.schedule();
				}
			}
		}
	}

	public void selectFirstLocationInEditor(final PredicateEdge edge) {
		if (edge == null) {
			return;
		} 
		synchronized (cachedLocations) {
			TreeSet<Location> locations = cachedLocations.get(edge);
			if (locations != null) {
				if (locations.size() > 0) {
					CallHierarchyUtil.selectLocationInEditor(locations.first());
				}
				locationTableViewer.setInput(locations);
			} else {
				final TreeSet<Location> newLocations = new TreeSet<>();
				Job j = getLocationFillJob(edge, newLocations);
				j.addJobChangeListener(new JobChangeAdapter() {
					@Override
					public void done(IJobChangeEvent event) {
						Display.getDefault().asyncExec(new Runnable() {
							@Override
							public void run() {
								if (newLocations.size() > 0) {
									CallHierarchyUtil.selectLocationInEditor(newLocations.first());
								}
							}
						});
					}
				});
				j.schedule();
			}
		}
	}
	
	private Job getLocationFillJob(final PredicateEdge edge, final TreeSet<Location> newLocations) {
		Job j = new Job("Search call locations") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				synchronized (cachedLocations) {
					PrologProcess process = PDTCommonUtil.getActivePrologProcess();
					try {
						Predicate from;
						Predicate to;
						if (mode == CallHierarchyView.CALLER_MODE) {
							from = edge.getTarget();
							to = edge.getSource();
						} else {
							from = edge.getSource();
							to = edge.getTarget();
						}
						List<Map<String, Object>> results = process.queryAll(bT("pdt_call_hierarchy:find_call_location",
								quoteAtom(from.getModule()),
								quoteAtom(from.getName()),
								from.getArity(),
								quoteAtom(to.getModule()),
								quoteAtom(to.getName()),
								to.getArity(),
								"_",
								"File",
								"Position"));
						for (Map<String,Object> result : results) {
							try {
								IFile file = FileUtils.findFileForLocation((String) result.get("File"));
								String position = (String) result.get("Position");
								int line;
								int start;
								int end;
								String text;
								IDocument doc = UIUtils.getDocument(file);
								if (position.indexOf("-") >= 0) {
									String[] positions = position.split("-");
									start = Integer.parseInt(positions[0]);
									end = Integer.parseInt(positions[1]);
									start = UIUtils.logicalToPhysicalOffset(doc, start);
									end = UIUtils.logicalToPhysicalOffset(doc, end);
									line = doc.getLineOfOffset(start);
									text = doc.get(start, end - start).replaceAll("\n|\r", "");
								} else {
									line = Integer.parseInt(position) - 1;
									start = doc.getLineOffset(line);
									end = start;
									text = edge.getTarget().getLabel();
								}
								Location location = new Location(text, file, start, end, line);
								newLocations.add(location);
							} catch (Exception e) {
								Debug.report(e);
							}
						}
						cachedLocations.put(edge, newLocations);
					} catch (Exception e) {
						Debug.report(e);
					}
				}
				return Status.OK_STATUS;
			}
		};
		j.setRule(ResourcesPlugin.getWorkspace().getRoot());
		return j;
	}

	public void clearCache() {
		synchronized (cachedLocations) {
			cachedLocations.clear();
		}
	}

	public void setMode(int mode) {
		this.mode = mode;
	}

}
