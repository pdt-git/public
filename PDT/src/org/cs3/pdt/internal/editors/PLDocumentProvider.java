package org.cs3.pdt.internal.editors;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.DefaultPartitioner;
import org.eclipse.ui.editors.text.FileDocumentProvider;

public class PLDocumentProvider extends FileDocumentProvider {

	protected IDocument createDocument(Object element) throws CoreException {
		IDocument document = super.createDocument(element);
		if (document != null) {
			IDocumentPartitioner partitioner =
				new DefaultPartitioner(
					new PLPartitionScanner(),
					new String[] {
						PLPartitionScanner.PL_MULTI_COMMENT,
						PLPartitionScanner.PL_COMMENT,
						PLPartitionScanner.PL_SINGLE_QUOTED_STRING,
						PLPartitionScanner.PL_DOUBLE_QUOTED_STRING});
			partitioner.connect(document);
			document.setDocumentPartitioner(partitioner);
		}
		return document;
	}
}