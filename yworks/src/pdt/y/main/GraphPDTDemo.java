package pdt.y.main;

import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.filechooser.FileFilter;

import pdt.y.graphml.GraphMLReader;
import pdt.y.model.GraphModel;
import pdt.y.view.modes.HierarchicPopupMode;
import pdt.y.view.modes.MyMoveSelectionMode;
import pdt.y.view.modes.ToggleOpenClosedStateViewMode;
import pdt.y.view.modes.WheelScroller;
import y.base.Node;
import y.layout.LayoutOrientation;
import y.layout.OrientationLayouter;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.router.OrthogonalEdgeRouter;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DView;

public class GraphPDTDemo extends  JPanel {
	private Graph2DView view;
	private GraphModel model;
	private Graph2D graph;
	private GraphMLReader reader;
	private IncrementalHierarchicLayouter layout;
	private OrthogonalEdgeRouter router;
	
	private static final long serialVersionUID = -611433500513523511L;
	private ExitAction data = new ExitAction();

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

		//updateView();
	}

	private void createLayout() {
		layout = new IncrementalHierarchicLayouter();
		
		//set some options
		layout.getNodeLayoutDescriptor().setMinimumLayerHeight(60);
		layout.getNodeLayoutDescriptor().setMinimumDistance(20);

		//use left-to-right layout orientation
		OrientationLayouter ol = new OrientationLayouter();
		ol.setOrientation(LayoutOrientation.LEFT_TO_RIGHT);
		layout.setOrientationLayouter(ol);
	}

	protected void loadGraph(URL resource) {
		model = reader.readFile(resource);
		graph = model.getGraph();
		this.updateView();
	}
	
	private void updateView() {
		view.setGraph2D(graph);

		for (Node node : graph.getNodeArray()) {
			graph.setLabelText(node, model.getIdForNode(node));
		}

		view.applyLayout(layout);
		view.updateWorldRect();
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
		Action action;
		action = createLoadAction();
		if (action != null) {
			menu.add(action);
		}

		menu.addSeparator();
		menu.add(new ExitAction());
		menuBar.add(menu);
		return menuBar;
	}

	protected Action createLoadAction() {
		return new LoadAction();
	}

	/**
	 * Action that loads the current graph from a file in GraphML format.
	 */
	protected class LoadAction extends AbstractAction {
		JFileChooser chooser;

		public LoadAction() {
			super("Load...");
			chooser = null;
		}

		public void actionPerformed(ActionEvent e) {
			if (chooser == null) {
				chooser = new JFileChooser();
				chooser.setAcceptAllFileFilterUsed(false);
				chooser.addChoosableFileFilter(new FileFilter() {
					public boolean accept(File f) {
						return f.isDirectory() || f.getName().endsWith(".graphml");
					}

					public String getDescription() {
						return "GraphML Format (.graphml)";
					}
				});
			}
			if (chooser.showOpenDialog(GraphPDTDemo.this) == JFileChooser.APPROVE_OPTION) {
				URL resource = null;
				try {
					resource = chooser.getSelectedFile().toURI().toURL();
				} catch (MalformedURLException urlex) {
					urlex.printStackTrace();
				}
				loadGraph(resource);
			}
		}
	}
	
	
	/**
	 * Action that terminates the application
	 */
	protected  class ExitAction extends AbstractAction {
		/**
		 * 
		 */
		private static final long serialVersionUID = 96864293273482994L;

		public ExitAction() {
			super("Exit");
		}

		public void actionPerformed(ActionEvent e) {
			System.exit(0);
		}
	}

	

	
}
