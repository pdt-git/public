package pdt.y.focusview;

import java.util.List;

import javax.swing.JComponent;

import org.eclipse.albireo.core.SwingControl;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.ui.progress.UIJob;

import pdt.y.main.PDTGraphView;
import pdt.y.model.GraphDataHolder;
import pdt.y.preferences.PreferencePage;
import y.base.Node;
import y.view.HitInfo;
import y.view.NodeLabel;
import y.view.ViewMode;

public class FocusView extends SwingControl {

	private final String FOCUS_VIEW_IS_OUTDATED = "[FocusView is outdated]";
	
	private final FocusViewPlugin focusViewPlugin;
	private final Composite viewContainer;
	private final PDTGraphView pdtGraphView;
	private final GraphPIFLoader pifLoader;
	private final String filePath;
	
	private boolean isDirty = false;
	
	public FocusView(final FocusViewPlugin plugin, final Composite viewContainer, final String filePath) {
		super(viewContainer, SWT.NONE); 
		
		this.focusViewPlugin = plugin;
		this.viewContainer = viewContainer;
		this.filePath = filePath;
		
		this.pdtGraphView = new PDTGraphView();
		
		this.pifLoader = new GraphPIFLoader(pdtGraphView);
		
		pdtGraphView.addViewMode(new OpenInEditorViewMode(pdtGraphView, pifLoader));
		pdtGraphView.addViewMode(new HoverTrigger(getShell()));
	}	
	
	public String getFilePath() {
		return filePath;
	}
	
	public boolean isEmpty() {
		return pdtGraphView.isEmpty();
	}
	
	public void setDirty() {
		focusViewPlugin.setStatusText(FOCUS_VIEW_IS_OUTDATED);
		isDirty = true;
	}
	
	public boolean isDirty() {
		return isDirty;
	}
	
	public List<String> getDependencies() {
		return pifLoader.getDependencies();
	}

	public void reload() {
		pifLoader.queryPrologForGraphFacts(filePath);
		focusViewPlugin.setStatusText("");
		
		isDirty = false;
	}
	
	public void updateLayout() {
		pdtGraphView.updateLayout();
	}

	@Override
	protected JComponent createSwingComponent() {
		return pdtGraphView;
	}

	@Override
	public Composite getLayoutAncestor() {
		return viewContainer;
	}

	private final class HoverTrigger extends ViewMode {

		private final ToolTip t;
		
		public HoverTrigger(Shell parent) {
			t = new ToolTip(parent, SWT.NONE);
			t.setVisible(false);
		}
		
		public void mouseMoved(final double x, final double y) {
			super.mouseMoved(x, y);
			
			new UIJob("Updating status") {
				
				@Override
				public IStatus runInUIThread(IProgressMonitor monitor) {
					updateStatus(x, y);
					return Status.OK_STATUS;
				}
			
			}.schedule();
		}

		protected void updateStatus(double x, double y) {
			HitInfo hitInfo = getHitInfo(x, y);
			
			if (hitInfo.hasHitNodes()) {
				Node node = hitInfo.getHitNode();
				NodeLabel label = pdtGraphView.getGraph2D().getRealizer(node).getLabel();
				
				StringBuilder sb = new StringBuilder(); 

				GraphDataHolder data = pdtGraphView.getDataHolder();

				if (data.isModule(node)) {
					sb.append("Module: ");
				} else if (data.isFile(node)) {
					sb.append("File: ");
				} else if (data.isPredicate(node)) {
					sb.append("Predicate: ");
				}

				sb.append(label.getText());
				
				if (data.isExported(node)) {
					sb.append(" [Exported]");
				}

				if (data.isDynamicNode(node)) {
					sb.append(" [Dynamic]");
				}

				if (data.isUnusedLocal(node)) {
					sb.append(" [Unused]");
				}
				
				String text = sb.toString();

				focusViewPlugin.setInfoText(text);
				
				if (PreferencePage.isShowToolTip() && text.startsWith("Predicate")) {
					Point location = Display.getCurrent().getCursorLocation();
					location.x += 10;
					location.y += 10;
					t.setLocation(location);
					t.setMessage(text.substring(11));
					t.setVisible(true);
				}
				else {
					t.setVisible(false);
				}
			} else {
				focusViewPlugin.setInfoText("");
				t.setVisible(false);
			}
		}
	}
}
