package alice.tuprologx.runtime.corba;

import org.omg.CosNaming.*;
import org.omg.CosNaming.NamingContextPackage.*;
import org.omg.CORBA.*;

import alice.tuprolog.*;

public class Test{
    public static void main(String args[]){
        try{
            if (args.length<1){
                System.err.println("args: <goal>");
                System.exit(-1);
            }
            // Create and initialize the ORB
            ORB orb = ORB.init(args, null);
            // Get the root naming context
            org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
            NamingContext ncRef = NamingContextHelper.narrow(objRef);
            // Resolve the object reference in naming
            NameComponent nc = new NameComponent("Prolog", " ");
            NameComponent path[] = {nc};
            Prolog engine = PrologHelper.narrow(ncRef.resolve(path));
            SolveInfo info=engine.solve(args[0]);
            if (info.success)
                System.out.println("yes: "+info.solution);
            else
                System.out.println("no.");
        } catch(Exception e) {
            System.err.println("ERROR: " + e);
            e.printStackTrace(System.out);
        }
    }
}




































