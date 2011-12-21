package pdt.y.main;

import java.util.List;

import javax.swing.JComponent;

import org.eclipse.albireo.core.SwingControl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

import pdt.y.model.GraphDataHolder;
import y.base.Node;
import y.view.HitInfo;
import y.view.NodeLabel;
import y.view.ViewMode;

public class FocusView extends SwingControl {

	final FocusViewPlugin focusViewPlugin;
	final Composite viewContainer;
	final PDTGraphView pdtGraphView;
	final GraphPIFCoordinator pifCoordinator;
	final String filePath;
	
	public FocusView(final FocusViewPlugin plugin, final Composite viewContainer, final String filePath) {
		super(viewContainer, SWT.NONE);
		
		this.focusViewPlugin = plugin;
		this.viewContainer = viewContainer;
		this.filePath = filePath;
		
		pdtGraphView = new PDTGraphView();
		
		pifCoordinator = new GraphPIFCoordinator(pdtGraphView);
		
		pdtGraphView.addViewMode(new OpenInEditorViewMode(pdtGraphView, pifCoordinator));

		HoverTrigger hoverTrigger = new HoverTrigger();
		pdtGraphView.addViewMode(hoverTrigger);
	}	
	
	public String getFilePath() {
		return filePath;
	}
	
	public boolean isEmpty() {
		return pdtGraphView.isEmpty();
	}
	
	public void setDirty() {
		focusViewPlugin.setInfoStatus("[FocusView is outdated]");
		if (focusViewPlugin.isAutomaticUpdate()) {
			reload();
		}
	}
	
	public List<String> getDependencies() {
		return pifCoordinator.getDependencies();
	}

	public void reload() {
		pifCoordinator.queryPrologForGraphFacts(filePath);
		focusViewPlugin.setInfoStatus("");
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

				focusViewPlugin.setInfo(sb.toString());
			} else {
				focusViewPlugin.setInfo("");
			}
		}
	}
}
