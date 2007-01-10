package alice.tuprologx.runtime.corba;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;


public class Daemon{
    public static void main(String args[]){
        try{
            // Create and initialize the ORB
            ORB orb = ORB.init(args, null);
            // Create the servant and register it with the ORB
            PrologImpl prologRef = new PrologImpl();
            orb.connect(prologRef);
            // Get the root naming context
            org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
            NamingContext ncRef = NamingContextHelper.narrow(objRef);
            // Bind the object reference in naming
            NameComponent nc = new NameComponent("Prolog", " ");
            NameComponent path[] = {nc};
            ncRef.rebind(path, prologRef);
            System.out.println("prolog CORBA daemon waiting requests.");
            // Wait for invocations from clients
            java.lang.Object sync = new java.lang.Object();
            synchronized(sync){
                sync.wait();
            }
        } catch(Exception e) {
            System.err.println("ERROR: " + e);
            e.printStackTrace(System.out);
        }
    }
}




































