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
import pdt.y.model.GraphLayout;
import pdt.y.model.GraphModel;
import pdt.y.model.realizer.ModuleGroupNodeRealizer;
import pdt.y.model.realizer.FileGroupNodeRealizer;
import pdt.y.view.actions.ExitAction;
import pdt.y.view.actions.LoadAction;
import pdt.y.view.actions.ResetLayout;
import pdt.y.view.modes.HierarchicPopupMode;
import pdt.y.view.modes.MyMoveSelectionMode;
import pdt.y.view.modes.ToggleOpenClosedStateViewMode;
import pdt.y.view.modes.WheelScroller;
import y.base.Node;
import y.layout.router.OrthogonalEdgeRouter;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.Graph2DViewMouseWheelZoomListener;
import y.view.hierarchy.GroupNodeRealizer;

public class GraphPDTDemo extends  JPanel {
	private Graph2DView view;
	public GraphModel model;
	private Graph2D graph;
	private GraphMLReader reader;
	
	private GraphLayout layoutModel;
	
	private static final long serialVersionUID = -611433500513523511L;

	public GraphPDTDemo() 
	{
		this.setLayout(new BorderLayout());
		
		layoutModel = new  GraphLayout();
		
		reader = new GraphMLReader();
		view = new Graph2DView();
		view.addMouseWheelListener(new WheelScroller(view));

		EditMode editMode = initEditMode();
		
		view.addViewMode(editMode);
		view.addViewMode(new ToggleOpenClosedStateViewMode());
		
		add(view);

		addMouseZoomSupport();
	}



	private EditMode initEditMode() {
		EditMode editMode = new EditMode();
		editMode.allowNodeCreation(false);
		editMode.allowEdgeCreation(false);
		editMode.setPopupMode(new HierarchicPopupMode());
		editMode.setMoveSelectionMode(new MyMoveSelectionMode(new OrthogonalEdgeRouter()));
		return editMode;
	}



	public void addMouseZoomSupport() {
		Graph2DViewMouseWheelZoomListener wheelZoomListener = new Graph2DViewMouseWheelZoomListener();
		//zoom in/out at mouse pointer location 
		wheelZoomListener.setCenterZooming(false);    
		view.getCanvasComponent().addMouseWheelListener(wheelZoomListener);
	}



	public void loadGraph(URL resource) {
		model = reader.readFile(resource);
		graph = model.getGraph();
		view.setGraph2D(graph);
		categorizeData();
		this.updateView();
	}
	
	private void categorizeData() {
		categorizeNodes();		
	}
	
	private void updateView() {
		
		createFirstLabel();
		this.calcLayout();
	}

	private void categorizeNodes() {
		GroupNodeRealizer fileRealizer = model.getFilegroupNodeRealizer();
		GroupNodeRealizer moduleRealizer = model.getModuleGroupNodeRealizer();
		for (Node node: graph.getNodeArray()) {
			if (model.isModule(node)) {
				graph.setRealizer(node, new ModuleGroupNodeRealizer(moduleRealizer));
			} else if (model.isFile(node)) {
				graph.setRealizer(node, new FileGroupNodeRealizer(fileRealizer));
			} else {
				// no realizer to set because it is already bound to default realizer
			}
		}
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
