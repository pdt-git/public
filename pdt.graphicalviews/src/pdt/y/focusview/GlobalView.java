/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Ilshat Aliev
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package pdt.y.focusview;

import pdt.y.main.PDTGraphView;


public class GlobalView extends ViewBase {
	
	@Override
	protected ViewCoordinatorBase createViewCoordinator() {
		return new GlobalViewCoordinator(this);
	}
	
	@Override
	protected GraphPIFLoaderBase createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new GlobalGraphPIFLoader(pdtGraphView);
	}
}
