package pdt.y.focusview;

import java.util.List;

import javax.swing.JComponent;

import org.eclipse.albireo.core.SwingControl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import pdt.y.main.PDTGraphView;
import pdt.y.model.GraphDataHolder;
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
		pdtGraphView.addViewMode(new HoverTrigger());
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

		public void mouseMoved(double x, double y) {
			super.mouseMoved(x, y);
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

				focusViewPlugin.setInfoText(sb.toString());
			} else {
				focusViewPlugin.setInfoText("");
			}
		}
	}
}
