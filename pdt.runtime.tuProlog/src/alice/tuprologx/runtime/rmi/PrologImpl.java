package alice.tuprologx.runtime.rmi;
import  alice.tuprolog.*;
import  java.io.*;

import java.rmi.*;
import java.rmi.server.UnicastRemoteObject;

public class PrologImpl extends UnicastRemoteObject
    implements alice.tuprologx.runtime.rmi.Prolog, Serializable {

    private alice.tuprolog.Prolog imp;

    public PrologImpl() throws RemoteException {
        try {
            imp=new alice.tuprolog.Prolog();
        } catch (Exception ex){
            ex.printStackTrace();
        }
    }

    public void clearTheory() throws RemoteException {
        imp.clearTheory();
    }

    public Theory getTheory() throws RemoteException{
        return imp.getTheory();
    }

    public void setTheory(Theory theory) throws InvalidTheoryException, RemoteException {
        imp.setTheory(theory);
    }

    public void addTheory(Theory theory) throws InvalidTheoryException, RemoteException {
        imp.addTheory(theory);
    }


    public SolveInfo   solve(Term g) throws RemoteException {
        return imp.solve(g);
    }

    public SolveInfo   solve(String g) throws MalformedGoalException, RemoteException{
        return imp.solve(g);
    }

    public boolean hasOpenAlternatives() throws java.rmi.RemoteException {
        return imp.hasOpenAlternatives();
    }

    public SolveInfo   solveNext() throws NoMoreSolutionException, RemoteException {
        return imp.solveNext();
    }

    public void solveHalt() throws RemoteException {
        imp.solveHalt();
    }

    public void solveEnd() throws RemoteException{
        imp.solveEnd();
    }


    public void loadLibrary(String className) throws InvalidLibraryException, RemoteException {
        imp.loadLibrary(className);
    }

    public void unloadLibrary(String className) throws InvalidLibraryException, RemoteException {
        imp.unloadLibrary(className);
    }

}
