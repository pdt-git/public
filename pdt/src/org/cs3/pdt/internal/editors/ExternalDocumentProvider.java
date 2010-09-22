package org.cs3.pdt.internal.editors;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;

public class ExternalDocumentProvider extends TextFileDocumentProvider{
	
	@Override
	protected FileInfo createFileInfo(Object element) throws CoreException {
        FileInfo info = super.createFileInfo(element);
        if(info==null){
                info = createEmptyFileInfo();
        }
        IDocument document = info.fTextFileBuffer.getDocument();
        if (document != null) {
        	IDocumentPartitioner partitioner =
				new FastPartitioner(
					new PLPartitionScanner(),
					new String[] {
						PLPartitionScanner.PL_MULTI_COMMENT,
						PLPartitionScanner.PL_COMMENT,
						PLPartitionScanner.PL_SINGLE_QUOTED_STRING,
						PLPartitionScanner.PL_DOUBLE_QUOTED_STRING});
			partitioner.connect(document);
			document.setDocumentPartitioner(partitioner);
        }
        return info;
    }

}
