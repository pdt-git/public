package org.cs3.pl.interaction;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;

import org.cs3.pl.Debug;

public class ConsoleInteraction implements UserInteractionStrategy {

    private PrintStream out;

    private InputStream in;

    public ConsoleInteraction(InputStream in, PrintStream out) {
        this.in = in;
        this.out = out;
    }

    public void tell(String message, String title) {
        out.println(title + ": " + message);
        out.println("(press enter to continue.)");
        try {
            in.read();
        } catch (IOException e) {
            Debug.report(e);
        }
    }

    public int ask(String message, String title, String[] options, int defVal) {
        out.println(title + ": " + message);
        out.println("Your options:");
        for (int i = 0; i < options.length; i++) {
            if (i == defVal) {
                out.println("\t(" + i + ") -- " + options[i] + "(default)");
            } else {
                out.println("\t " + i + "  -- " + options[i]);
            }
        }
        while (true) {
            out.print("your choice (<enter> to accept default)? :");
            out.flush();
            String line = null;
            try {
                line = new BufferedReader(new InputStreamReader(in)).readLine();
            } catch (IOException e) {
                Debug.report(e);
            }
            if (line == null) {
                out.println("rats!");
                throw new NullPointerException("did not expect null here!");
            }
            if (line.trim().length() == 0) {//user wants default
                                                        // value
                out.println("ok");
                return defVal;
            }
            try {
                int choice = Integer.parseInt(line);
                out.println("ok");
                return choice;
            } catch (NumberFormatException nme) {
                out.println("?????");
            }
        }
    }

    public String ask(String message, String title, String prompt, String defVal) {
        out.println(title+":"+message);
        out.print(prompt+" (default:"+defVal+")");
        out.flush();
        String line=null;
        try {
            line= new BufferedReader(new InputStreamReader(in)).readLine();
        } catch (IOException e) {
            Debug.report(e);
        }
        if (line == null) {
            out.println("rats!");
            throw new NullPointerException("did not expect null here!");
        }
        if (line.trim().length() == 0) {//user wants default
                                                    // value
            out.println("ok");
            return defVal;
        }
        return line;
    }

    public boolean askForConfirmation(String message, String title) {
        return ask(message,title,"ok or not ok?","ok").trim().equals("ok");
    }

}