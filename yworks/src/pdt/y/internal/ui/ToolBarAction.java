package pdt.y.internal.ui;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.progress.UIJob;

public abstract class ToolBarAction extends Action {
	private String name;
	private String toolTipText;
	private ImageDescriptor image;
	
	public ToolBarAction(String name, ImageDescriptor image) {
		this(name, name, image);
	}
	
	public ToolBarAction(String name, String toolTipText, ImageDescriptor image) {
		this.name = name;
		this.toolTipText = toolTipText;
		this.image = image;
	}
	
	@Override
	public void run(){
			new UIJob("Hierarchical layout")
			{
				@Override
				public IStatus runInUIThread(IProgressMonitor monitor) {
					performAction();
					return Status.OK_STATUS;
			}
		}.schedule();
	}
	
	public abstract void performAction();
	
	@Override
	public ImageDescriptor getImageDescriptor() {
		return image;
	}
	
	@Override
	public String getText() {
		return name;
	}
	
	@Override
	public String getToolTipText() {
		return toolTipText;
	}
}
