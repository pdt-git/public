package alice.tuprologx.runtime.rmi;
import alice.tuprolog.InvalidLibraryException;
import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoMoreSolutionException;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Term;
import alice.tuprolog.Theory;

public interface Prolog extends java.rmi.Remote {


    public void clearTheory() throws java.rmi.RemoteException;

    public Theory getTheory() throws java.rmi.RemoteException;

    public void setTheory(Theory theory) throws InvalidTheoryException, java.rmi.RemoteException;

    public void addTheory(Theory theory) throws InvalidTheoryException, java.rmi.RemoteException;


    public SolveInfo   solve(Term g) throws java.rmi.RemoteException;

    public SolveInfo   solve(String g) throws MalformedGoalException, java.rmi.RemoteException;

    public boolean   hasOpenAlternatives() throws java.rmi.RemoteException;

    public SolveInfo   solveNext() throws NoMoreSolutionException, java.rmi.RemoteException;

    public void solveHalt() throws java.rmi.RemoteException;

    public void solveEnd() throws java.rmi.RemoteException;


    public void loadLibrary(String className) throws InvalidLibraryException, java.rmi.RemoteException;

    public void unloadLibrary(String className) throws InvalidLibraryException, java.rmi.RemoteException;

}
