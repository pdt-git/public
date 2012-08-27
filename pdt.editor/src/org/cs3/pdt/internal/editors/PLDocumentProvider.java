/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.internal.editors;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;

public class PLDocumentProvider extends TextFileDocumentProvider {

//	@Override
//	protected IDocument createDocument(Object element) throws CoreException {
//		IDocument document = super.createDocument(element);
//		if (document != null) {
//			IDocumentPartitioner partitioner =
//				new FastPartitioner(
//					new PLPartitionScanner(),
//					new String[] {
//						PLPartitionScanner.PL_MULTI_COMMENT,
//						PLPartitionScanner.PL_COMMENT,
//						PLPartitionScanner.PL_SINGLE_QUOTED_STRING,
//						PLPartitionScanner.PL_DOUBLE_QUOTED_STRING});
//			partitioner.connect(document);
//			document.setDocumentPartitioner(partitioner);
//		}
//		return document;
//	}
	
	private IDocumentPartitioner partitioner;

	@Override
	public IDocument getDocument(Object element) {
		IDocument document = super.getDocument(element);
		IDocumentPartitioner partitioner2 = getPartitioner();
		if (document.getDocumentPartitioner() != partitioner2) {
			partitioner2.connect(document);
			document.setDocumentPartitioner(partitioner2);
		}
		return document;
	}
	
	private IDocumentPartitioner getPartitioner() {
		if (partitioner == null) {
			partitioner = new FastPartitioner(
					new PLPartitionScanner(),
					new String[] {
						PLPartitionScanner.PL_MULTI_COMMENT,
						PLPartitionScanner.PL_COMMENT,
						PLPartitionScanner.PL_SINGLE_QUOTED_STRING,
						PLPartitionScanner.PL_DOUBLE_QUOTED_STRING});
		}
		return partitioner;
	}
}


