package pdt.y.main;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.net.URL;

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;

import pdt.y.graphml.GraphMLReader;
import pdt.y.model.GraphDataHolder;
import pdt.y.model.GraphLayout;
import pdt.y.model.GraphModel;
import pdt.y.view.modes.HierarchicPopupMode;
import pdt.y.view.modes.MoveSelectedSelectionMode;
import pdt.y.view.modes.ToggleOpenClosedStateViewMode;
import pdt.y.view.swing.actions.ExitAction;
import pdt.y.view.swing.actions.LoadAction;
import pdt.y.view.swing.actions.ResetLayout;
import y.base.Node;
import y.layout.router.OrthogonalEdgeRouter;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.Graph2DViewMouseWheelZoomListener;
import y.view.NavigationMode;
import y.view.ViewMode;

public class PDTGraphView extends  JPanel {
	final Graph2DView view;
	GraphModel model;
	Graph2D graph;
	GraphMLReader reader;

	GraphLayout layoutModel;
	
	EditMode editMode;
	NavigationMode navigationMode;

	private static final long serialVersionUID = -611433500513523511L;

	public PDTGraphView()
	{
		setLayout(new BorderLayout());
		
		layoutModel = new  GraphLayout();

		reader = new GraphMLReader();
		view = new Graph2DView();

		initEditMode();
		
		initNavigationMode();
		
		initMouseZoomSupport();

		initKeyListener();
		
		add(view);
	}

	private void  initEditMode() {
		editMode = new EditMode();
		editMode.allowNodeCreation(false);
		editMode.allowEdgeCreation(false);
		editMode.setPopupMode(new HierarchicPopupMode());
		editMode.setMoveSelectionMode(new MoveSelectedSelectionMode(new OrthogonalEdgeRouter()));
		
		view.addViewMode(editMode);
		view.addViewMode(new ToggleOpenClosedStateViewMode());
	}

	protected void initNavigationMode() {
		navigationMode = new NavigationMode();
		navigationMode.setDefaultCursor(new Cursor(Cursor.MOVE_CURSOR));
		navigationMode.setNavigationCursor(new Cursor(Cursor.MOVE_CURSOR));
	}

	private void initMouseZoomSupport() {
		// why do we need two mouse wheel listeners???
		//view.addMouseWheelListener(new WheelScroller(view));
		
		Graph2DViewMouseWheelZoomListener wheelZoomListener = new Graph2DViewMouseWheelZoomListener();
		wheelZoomListener.setCenterZooming(false);
		view.getCanvasComponent().addMouseWheelListener(wheelZoomListener);
	}
	
	private void initKeyListener() {
		view.getCanvasComponent().addKeyListener(new KeyListener() {
			
			Boolean navigation = false;
			
			@Override
			public void keyPressed(KeyEvent e) {
				if (navigation)
					return;
				
				if (e.getKeyCode() == KeyEvent.VK_CONTROL) {
					navigationMode();
					navigation = true;
				}
			}

			@Override
			public void keyReleased(KeyEvent e) {
				if (!navigation)
					return;
				
				if (e.getKeyCode() == KeyEvent.VK_CONTROL) {
					editMode();
					navigation = false;
				}
			}

			@Override
			public void keyTyped(KeyEvent arg0) { }
		});
	}
	
	public void navigationMode() {
		view.addViewMode(navigationMode);
		view.removeViewMode(editMode);
	}
	public void editMode() {
		view.addViewMode(editMode);
		view.removeViewMode(navigationMode);
	}

	public GraphDataHolder getDataHolder() {
		return model.getDataHolder();
	}
	public Graph2D getGraph2D() {
		return graph;
	}

	public void addViewMode(ViewMode viewMode){
		view.addViewMode(viewMode);
	}

	public void setModel(GraphModel model){
		this.model = model;
	}

	public void loadGraph(URL resource) {
		model = reader.readFile(resource);
		model.categorizeData();
		model.assignPortsToEdges();
		graph = model.getGraph();
		view.setGraph2D(graph);

		updateView();
	}

	private void updateView() {
		createFirstLabel();
		calcLayout();
	}

	public boolean isEmpty() {
		return graph == null 
			|| graph.getNodeArray().length == 0;
	}

	private void createFirstLabel() {
		String labelText;
		for (Node node: graph.getNodeArray()) {
			labelText = model.getLabelTextForNode(node);
			graph.setLabelText(node,labelText);
		}
	}

	public void calcLayout() {
		view.applyLayout(layoutModel.getLayouter());
		
		//		layoutModel.getLayouter().doLayout(graph);
		//		graph.updateViews();
		
		view.fitContent();
		view.updateView();
	}


	/**
	 * Create a menu bar for this demo.
	 */
	protected JMenuBar createMenuBar() {
		JMenuBar menuBar = new JMenuBar();
		JMenu menu = new JMenu("File");

		menu.add(new LoadAction(this));
		menu.addSeparator();
		menu.add(new ResetLayout(this));
		menu.add(new ExitAction());
		menuBar.add(menu);
		return menuBar;
	}
}
