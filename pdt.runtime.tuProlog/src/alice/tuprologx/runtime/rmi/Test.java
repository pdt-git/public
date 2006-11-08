package alice.tuprologx.runtime.rmi;

import alice.tuprolog.*;

import java.io.*;
import java.rmi.*;
import java.rmi.registry.*;
import java.rmi.server.*;

public class Test
{
    public static void main(String args[])
    {
        if (args.length<2){
            System.err.println("args:  <host> <goal>");
            System.exit(-1);
        }
        try{
            System.setSecurityManager(new RMISecurityManager());
            try {
                LocateRegistry.createRegistry(1099);
            } catch (Exception ex){
            }
            String rmiName="rmi://"+args[0]+"/prolog";
            alice.tuprologx.runtime.rmi.Prolog engine =
                (alice.tuprologx.runtime.rmi.Prolog)Naming.lookup(rmiName);

            SolveInfo info=engine.solve(args[1]);
            if (info.isSuccess())
                System.out.println("yes: "+info.getSolution());
            else
                System.out.println("no.");
        } catch(Exception e) {
            System.err.println("ERROR: " + e);
            e.printStackTrace(System.out);
        }
    }
}




































