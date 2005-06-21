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