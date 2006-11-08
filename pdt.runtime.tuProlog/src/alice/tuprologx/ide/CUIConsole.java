package alice.tuprologx.ide;

import alice.util.*;
import alice.tuprolog.*;
import alice.tuprolog.event.OutputEvent;
import alice.tuprolog.event.OutputListener;
import alice.tuprolog.event.SpyEvent;
import alice.tuprolog.event.SpyListener;
import alice.tuprolog.event.WarningEvent;
import alice.tuprolog.event.WarningListener;

import java.io.*;

public class CUIConsole extends Automaton implements Serializable, OutputListener, SpyListener, WarningListener {

    BufferedReader  stdin;
    Prolog          engine;
    SolveInfo       info;

    static final String incipit=
        "\ntuProlog "+Prolog.getVersion()+" - DEIS,  Universita' di Bologna a Cesena \n"+
        new java.util.Date()+"\n";
       
    public CUIConsole(String[] args){

        if (args.length>1){
            System.err.println("args: { theory file }");
            System.exit(-1);
        }
        /*
        try {
            engine=new Prolog(new String[]{});
        } catch (Exception ex){
        }*/
        engine=new Prolog();
        stdin=new BufferedReader(new InputStreamReader(System.in));
        engine.addWarningListener(this);
        engine.addOutputListener(this);
        engine.addSpyListener(this);
        if (args.length>0) {
            try {
                engine.setTheory(new Theory(new FileInputStream(args[0])));
            } catch (InvalidTheoryException ex){
                System.err.println("invalid theory - line: "+ex.line);
                System.exit(-1);
            } catch (Exception ex){
                System.err.println("invalid theory.");
                System.exit(-1);
            }
        }
    }

    public void boot(){
        System.out.println(incipit);
        become("goalRequest");
    }

    public void goalRequest(){
        String goal="";
        try {
            while (goal.equals("")){
                System.out.print("\n?- ");
                goal=stdin.readLine();
            }
            solveGoal(goal);
        } catch (Exception ex){
        };
    }

    void solveGoal(String goal){
        try {
            info=engine.solve(goal);
            if (engine.isHalted())
                System.exit(0);
            else  if (!info.isSuccess()){
                System.out.println("no.");
                become("goalRequest");
            } else  if (!engine.hasOpenAlternatives()){
                String binds=info.toString();
                if (binds.equals("")){
                    System.out.println("yes.");
                } else {
                    System.out.println("yes ( "+binds+" ).");
                }
                System.out.println("solution: "+engine.toString(info.getSolution()));
                become("goalRequest");
            } else {
                System.out.print("substitution: "+info+"  ?  ");
                become("getChoice");
            }
        } catch (MalformedGoalException ex){
            System.out.println("syntax error in "+goal);
            become("goalRequest");
        } catch (NoSolutionException ex){
            System.out.println("syntax error in "+goal);
            become("goalRequest");
        }
    }

    public void getChoice(){
        String choice="";
        try {
            while (true){
                choice=stdin.readLine();
                if (!choice.equals(";") && !choice.equals(""))
                    System.out.println("\nAction ( ';' for more choices, otherwise <return> ) ");
                else
                    break;
            }
        } catch (IOException ex){
        }
        try {
            if (!choice.equals(";")){
                String binds=info.toString();
                if (binds.equals(""))
                    System.out.println("yes.");
                else
                    System.out.println("yes ( "+binds+" ).");
                System.out.print("solution: "+engine.toString(info.getSolution()));
                engine.solveEnd();
                become("goalRequest");
            } else {
                try {
                    info=engine.solveNext();
                    if (!info.isSuccess()){
                        System.out.println("no.");
                        become("goalRequest");
                    } else {
                        System.out.print("substitution: "+info+"  ?  ");
                        become("getChoice");
                    }
                } catch (Exception ex){
                    System.out.println("no.");
                    become("goalRequest");
                }
            }
        } catch (NoSolutionException ex){
            System.out.println("no.");
            become("goalRequest");
        }
    }

    public void onOutput(OutputEvent e) {
        System.out.print(e.getMsg());
    }
    public void onSpy(SpyEvent e) {
        System.out.println(e.getMsg());
    }
    public void onWarning(WarningEvent e) {
        System.out.println(e.getMsg());
    }

    public static void main(String[] args){
        new Thread(new CUIConsole(args)).start();
    }
}

