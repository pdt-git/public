package pdt.y.main;

import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.net.URL;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JRootPane;

import pdt.y.graphml.GraphMLReader;
import pdt.y.model.GraphModel;
import pdt.y.view.actions.ExitAction;
import pdt.y.view.actions.LoadAction;
import pdt.y.view.actions.ResetLayout;
import pdt.y.view.modes.HierarchicPopupMode;
import pdt.y.view.modes.MyMoveSelectionMode;
import pdt.y.view.modes.ToggleOpenClosedStateViewMode;
import pdt.y.view.modes.WheelScroller;
import y.base.Node;
import y.layout.BufferedLayouter;
import y.layout.CanonicMultiStageLayouter;
import y.layout.CompositeLayoutStage;
import y.layout.LayoutOrientation;
import y.layout.OrientationLayouter;
import y.layout.grouping.FixedGroupLayoutStage;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.router.OrthogonalEdgeRouter;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.Graph2DViewMouseWheelZoomListener;

public class GraphPDTDemo extends  JPanel {
	private Graph2DView view;
	private GraphModel model;
	private Graph2D graph;
	private GraphMLReader reader;
	
	private OrthogonalEdgeRouter router;
	
	private static final long serialVersionUID = -611433500513523511L;
	private CanonicMultiStageLayouter layouter;
	private CompositeLayoutStage stage;

	public GraphPDTDemo() 
	{
		setLayout(new BorderLayout());
		reader = new GraphMLReader();
		view = new Graph2DView();
		router = new OrthogonalEdgeRouter();
		view.addMouseWheelListener(new WheelScroller(view));
		createLayout();

		EditMode editMode = new EditMode();
		editMode.allowNodeCreation(false);
		editMode.allowEdgeCreation(false);
		editMode.setPopupMode(new HierarchicPopupMode());
		editMode.setMoveSelectionMode(new MyMoveSelectionMode(router));
		view.addViewMode(editMode);
		view.addViewMode(new ToggleOpenClosedStateViewMode());
		
		add(view);

		Graph2DViewMouseWheelZoomListener wheelZoomListener = new Graph2DViewMouseWheelZoomListener();
		//zoom in/out at mouse pointer location 
		wheelZoomListener.setCenterZooming(false);    
		view.getCanvasComponent().addMouseWheelListener(wheelZoomListener);
		//updateView();
	}

	private void createLayout() {
		layouter = null;

		IncrementalHierarchicLayouter layout = new IncrementalHierarchicLayouter();
		
		//set some options
		layout.getNodeLayoutDescriptor().setMinimumLayerHeight(60);
		layout.getNodeLayoutDescriptor().setMinimumDistance(20);

		//use left-to-right layout orientation
		OrientationLayouter ol = new OrientationLayouter();
		ol.setOrientation(LayoutOrientation.BOTTOM_TO_TOP);
		layout.setOrientationLayouter(ol);
		layout.setBackloopRoutingEnabled(true);
		layout.setFromScratchLayeringStrategy(IncrementalHierarchicLayouter.LAYERING_STRATEGY_HIERARCHICAL_TOPMOST);
	
		layouter = layout;
		
	}

	public void loadGraph(URL resource) {
		model = reader.readFile(resource);
		graph = model.getGraph();
		view.setGraph2D(graph);
		this.updateView();
	}
	
	private void updateView() {
		for (Node node : graph.getNodeArray()) {
			graph.setLabelText(node, model.getIdForNode(node));
		}


		this.calcLayout();
	}

	public void calcLayout() {
		view.applyLayout(layouter);
//		stage.doLayout(graph);
//		view.applyLayout(stage);
		view.fitContent();
		view.updateView();
	}


	public void start()
	{
		JFrame frame = new JFrame(getClass().getName());
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		addContentTo(frame.getRootPane());
		frame.pack();
		frame.setLocationRelativeTo(null);
		frame.setVisible(true);
	}

	public final void addContentTo( final JRootPane rootPane )
	{
		rootPane.setContentPane(this);
		rootPane.setJMenuBar(createMenuBar());
	}

	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				(new GraphPDTDemo()).start();
			}
		});
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
