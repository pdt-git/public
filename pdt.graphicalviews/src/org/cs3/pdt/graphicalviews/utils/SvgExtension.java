package org.cs3.pdt.graphicalviews.utils;

import java.io.IOException;

import y.io.IOHandler;
import y.view.Graph2DView;
import yext.svg.io.SVGIOHandler;

public class SvgExtension  {

	public void Export(Graph2DView view, String path) throws IOException {
	    IOHandler ioh = new SVGIOHandler();  
	    double tmpPDT = view.getPaintDetailThreshold();  
	    view.setPaintDetailThreshold(0.0);  
	    ioh.write(view.getGraph2D(), path);  
	    view.setPaintDetailThreshold(tmpPDT);
	}
}
