/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pl.parser;


/**
 * Collects problem during compilation of prolog streams.
 */
public interface ProblemCollector {

    /**
     * Called by the ClassicPrologCompiler to report a Problem.
     * 
     * @param token
     *                    the token causing the problem
     * @param msg
     *                    a message describing the problem.
     * @param severity
     *                    one of ClassicPrologCompiler.INFO,ClassicPrologCompiler.WARNING or
     *                    ClassicPrologCompiler.ERROR
     */
    public void reportProblem(Problem p);
    
    /**
     * called by the prolog compiler before parsing of the file.
     * Implementations may use this opertunity to clear all error state there may exist for
     * what ever is compiled... e.g. problem markers for a given resource (hint, hint) :-)
     */
    public void reset();
    
    /**
     * Called by the prolog compiler when the completion finished.
     * Implementations may use this opertunity to do bulk reports
     * of collected problems.
     */
    public void done();

	

}


