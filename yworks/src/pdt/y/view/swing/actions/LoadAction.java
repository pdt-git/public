package pdt.y.view.swing.actions;

import java.awt.event.ActionEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import pdt.y.main.PDTGraphView;

/**
	 * Action that loads the current graph from a file in GraphML format.
	 */
	public class LoadAction extends AbstractAction {
		/**
		 * 
		 */
		private static final long serialVersionUID = 65359810633139537L;
		JFileChooser chooser;
		private PDTGraphView frame;

		public LoadAction(PDTGraphView pdtGraphSwing) {
			super("Load...");
			chooser = null;
			this.frame = pdtGraphSwing;
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			if (chooser == null) {
				chooser = new JFileChooser();
				chooser.setAcceptAllFileFilterUsed(false);
				chooser.addChoosableFileFilter(new FileFilter() {
					@Override
					public boolean accept(File f) {
						return f.isDirectory() || f.getName().endsWith(".graphml");
					}

					@Override
					public String getDescription() {
						return "GraphML Format (.graphml)";
					}
				});
			}
			chooser.showOpenDialog(null);
			URL resource = null;
			if (chooser.getSelectedFile() == null )
				return;
			
			try {
				resource = chooser.getSelectedFile().toURI().toURL();
			} catch (MalformedURLException urlex) {
				urlex.printStackTrace();
			}
			frame.loadGraph(resource);
		}
	}
	