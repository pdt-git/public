package pdt.y.main;

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

	Composite viewContainer;
	FocusViewPlugin focusViewPlugin;
	PDTGraphSwingStandalone pdtGraphView;
	GraphPIFCoordinator pifCoordinator;
	String filePath;
	
	public FocusView(FocusViewPlugin plugin, Composite viewContainer, String filePath) {
		super(viewContainer, SWT.NONE);
		this.viewContainer = viewContainer;
		this.focusViewPlugin = plugin;
		this.filePath = filePath;
	}
	
	public String getFilePath() {
		return filePath;
	}

	@Override
	protected JComponent createSwingComponent() {
		pdtGraphView = new PDTGraphSwingStandalone();
		pifCoordinator = new GraphPIFCoordinator(pdtGraphView);

		pdtGraphView.addViewMode(new OpenInEditorViewMode(pdtGraphView, pifCoordinator));

		HoverTrigger hoverTrigger = new HoverTrigger();
		pdtGraphView.addViewMode(hoverTrigger);

		return pdtGraphView;
	}
	
	public boolean isEmpty() {
		return pdtGraphView.isEmpty();
	}

	public void reload() {
		pifCoordinator.queryPrologForGraphFacts(filePath);
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
