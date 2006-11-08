package alice.tuprologx.runtime.tcp;

import  java.io.*;
import  java.util.*;

import alice.tuprolog.*;

public interface Prolog {

    public void clearTheory() throws Exception;
    public Theory getTheory() throws Exception;
    void setTheory(Theory theory) throws Exception;
    void addTheory(Theory theory) throws Exception;

    public SolveInfo   solve(String g) throws Exception;
    public SolveInfo   solve(Term th) throws Exception;
    public SolveInfo   solveNext() throws Exception;
    public boolean     hasOpenAlternatives() throws Exception;
    public void solveHalt() throws Exception;
    public void solveEnd() throws Exception;

    public void loadLibrary(String className) throws Exception;
    public void unloadLibrary(String className) throws Exception;
}
