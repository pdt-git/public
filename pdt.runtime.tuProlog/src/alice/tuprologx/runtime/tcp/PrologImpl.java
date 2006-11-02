package alice.tuprologx.runtime.tcp;
import alice.tuprolog.*;
import  java.io.*;

public class PrologImpl implements Serializable {

    alice.tuprolog.Prolog core;
    //Vector solutionListeners;

    public PrologImpl(alice.tuprolog.Prolog core_){
        core=core_;
        //utionListeners=new Vector();
    }

    public void clearTheory(ObjectInputStream in,ObjectOutputStream out){
        core.clearTheory();
    }

    public void getTheory(ObjectInputStream in,ObjectOutputStream out) throws Exception {
        Theory th=core.getTheory();
        out.writeObject(new Boolean(false));
        out.writeObject(th);
    }

    public void setTheory(ObjectInputStream in,ObjectOutputStream out) throws Exception {
        try {
            Theory th=(Theory)in.readObject();
            core.setTheory(th);
            out.writeObject(new Boolean(true));
        } catch (InvalidTheoryException ex){
            out.writeObject(new Boolean(false));
        }
    }

    public void addTheory(ObjectInputStream in,ObjectOutputStream out)  throws Exception {
        try {
            Theory th=(Theory)in.readObject();
            core.addTheory(th);
            out.writeObject(new Boolean(true));
        } catch (InvalidTheoryException ex){
            out.writeObject(new Boolean(false));
        }
    }

    public void solveString(ObjectInputStream in,ObjectOutputStream out)  throws Exception {
        try {
            String st=(String)in.readObject();
            SolveInfo info=core.solve(st);
            out.writeObject(new Boolean(true));
            out.writeObject(info);
        } catch (MalformedGoalException ex){
            out.writeObject(new Boolean(false));
        }
    }

    public void hasOpenAlternatives(ObjectInputStream in,ObjectOutputStream out)  throws Exception {
        out.writeObject(new Boolean(core.hasOpenAlternatives()));
    }

    public void solveTerm(ObjectInputStream in,ObjectOutputStream out)  throws Exception {
        Term th=(Term)in.readObject();
        SolveInfo info=core.solve(th);
        out.writeObject(new Boolean(true));
        out.writeObject(info);
    }

    public void solveNext(ObjectInputStream in,ObjectOutputStream out) throws Exception {
        try {
            SolveInfo info=core.solveNext();
            out.writeObject(new Boolean(true));
            out.writeObject(info);
        } catch (NoMoreSolutionException ex){
            out.writeObject(new Boolean(false));
        }
    }

    public void solveHalt(ObjectInputStream in,ObjectOutputStream out){
        core.solveHalt();
    }

    public void solveEnd(ObjectInputStream in,ObjectOutputStream out){
        core.solveEnd();
    }


    public void loadLibrary(ObjectInputStream in,ObjectOutputStream out) throws Exception{
        try {
            String st=(String)in.readObject();
            core.loadLibrary(st);
            out.writeObject(new Boolean(true));
        } catch (InvalidLibraryException ex){
            out.writeObject(new Boolean(false));
        }
    }

    public void unloadLibrary(ObjectInputStream in,ObjectOutputStream out) throws Exception {
        try {
            String st=(String)in.readObject();
            core.unloadLibrary(st);
            out.writeObject(new Boolean(true));
        } catch (InvalidLibraryException ex){
            out.writeObject(new Boolean(false));
        }
    }
}
