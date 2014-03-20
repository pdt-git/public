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

package pdt.y.view.modes;

import java.awt.event.MouseEvent;

import org.cs3.pdt.common.PDTCommonUtil;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PartInitException;

import pdt.y.focusview.GraphPIFLoaderBase;
import pdt.y.main.PDTGraphView;
import pdt.y.model.GraphDataHolder;
import y.base.Edge;
import y.base.Node;
import y.view.EdgeLabel;
import y.view.ViewMode;

public class OpenInEditorViewMode extends ViewMode {

	private PDTGraphView view;
	private GraphPIFLoaderBase pifLoader;

	public OpenInEditorViewMode(PDTGraphView view, GraphPIFLoaderBase pifLoader) {
		this.view = view;
		this.pifLoader = pifLoader;
	}

	@Override
	public void mouseClicked(MouseEvent event) {
		if(event.getClickCount() >= 2) {

			// Retrieve the node that has been hit at the location.
			Node node = getHitInfo(event).getHitNode();

			if (node != null) {
				selectNode(node);
				return;
			}
			
			Edge edge = getHitInfo(event).getHitEdge();
			if (edge != null) {
				selectEdge(edge);
				return;
			}
			
			EdgeLabel label = getHitInfo(event).getHitEdgeLabel();
			if (label != null) {
				selectEdge(label.getEdge());
				return;
			}
			
//			String idInt = dataHolder.getNodeText(node);
//
//			String query = "parse_util:predicateT("+idInt+",FileId,_,_,_),parse_util:fileT(FileId,FileName,_),parse_util:filePosT("+idInt+",Pos,Len).";
//			Map<String,Object> result = null;
//			try {
//				result = pifLoader.sendQueryToCurrentPiF(query);
//			} catch (PrologInterfaceException e1) {
//				e1.printStackTrace();
//			}
//
//			if(result==null)
//				return;
//
//			final String filename = result.get("FileName").toString();
//			final int start = Integer.parseInt(result.get("Pos").toString());
//			final int length = Integer.parseInt(result.get("Len").toString());
//
//			//			ExecutorService executor = Executors.newCachedThreadPool();
//			//			FutureTask<String> futureParser = new FutureTask<String>(new Runnable() {
//			//				@Override
//			//				public void run() {
//			//					try {
//			//						//Display.getDefault().
//			//						PDTCoreUtils.selectInEditor(start, length, filename);
//			//					} catch (Exception e) {
//			//
//			//					}
//			//				}
//			//			},null);
//			//
//			//
//			//			executor.execute(futureParser);
//			//
//			Display.getDefault().asyncExec(new Runnable() {
//				@Override
//				public void run() {
//					try {
//						PDTCommonUtil.selectInEditor(start, length, filename, true);
//					} catch (PartInitException e) {
//						e.printStackTrace();
//					}
//				}
//			});
		}
	}

	private void selectNode(Node node) {
		GraphDataHolder dataHolder = view.getDataHolder();
		if (dataHolder.isFile(node) || dataHolder.isModule(node)) {
			try {
				final String fileName = dataHolder.getFileName(node);
				Display.getDefault().asyncExec(new Runnable() {
					@Override
					public void run() {
						try {
							PDTCommonUtil.selectInEditor(1, fileName, true);
						} catch (PartInitException e) {
							e.printStackTrace();
						}
					}
				});
			} catch (NullPointerException e) {}
		} else if (dataHolder.isPredicate(node)) {
			try {
				final String fileName = dataHolder.getFileName(node);
				int line = dataHolder.getLineNumber(node);
				final int lineToSelect = (line >= 1 ? line : 1);
				Display.getDefault().asyncExec(new Runnable() {
					@Override
					public void run() {
						try {
							PDTCommonUtil.selectInEditor(lineToSelect, fileName, true);
						} catch (PartInitException e) {
							e.printStackTrace();
						}
					}
				});
			} catch (NullPointerException e) {}
		}
	}

	private void selectEdge(Edge edge) {
		GraphDataHolder dataHolder = view.getDataHolder();
		try {
			final String fileName = dataHolder.getFileName(edge);
			String offset = dataHolder.getOffset(edge);
			if (offset == null)
				return;
			String[] parts = offset.split("-");
			final int start = Integer.parseInt(parts[0]);
			final int length = Integer.parseInt(parts[1]) - start;
				
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					try {
						PDTCommonUtil.selectInEditor(start, length, fileName, true);
					} catch (PartInitException e) {
						e.printStackTrace();
					}
				}
			});
		} catch (NullPointerException e) {}
	}
}
