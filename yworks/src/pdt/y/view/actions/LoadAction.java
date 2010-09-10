package pdt.y.view.actions;

import java.awt.event.ActionEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import pdt.y.main.PDTGraphSwing;

/**
	 * Action that loads the current graph from a file in GraphML format.
	 */
	public class LoadAction extends AbstractAction {
		/**
		 * 
		 */
		private static final long serialVersionUID = 65359810633139537L;
		JFileChooser chooser;
		private PDTGraphSwing frame;

		public LoadAction(PDTGraphSwing pdtGraphSwing) {
			super("Load...");
			chooser = null;
			this.frame = pdtGraphSwing;
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
			chooser.showOpenDialog(frame);
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
	