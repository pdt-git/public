package pdt.y.view;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;

import pdt.y.main.PDTGraphView;

public class GlobalView extends FocusView {
	
	@Override
	protected GraphPIFLoader createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new GlobalGraphPIFLoader(pdtGraphView, this);
	}
}
