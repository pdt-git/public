package org.cs3.pl.interaction;

import org.cs3.pl.Debug;

public class NoInteraction implements UserInteractionStrategy {

    public void tell(String message, String title) {
        Debug.info("INTERACTION:"+title+": "+message);
    }

    public int ask(String message, String title, String[] options, int defVal) {
        Debug.info("INTERACTION:"+title+": "+message+" [assuming default: "+defVal+"=="+options[defVal]+"]");
        return defVal;
    }

    public String ask(String message, String title, String prompt, String defVal) {
        Debug.info("INTERACTION:"+title+": "+message+" [assuming default: "+defVal+"]");
        return defVal;
    }

    public boolean askForConfirmation(String message, String title) {
        Debug.info("INTERACTION:"+title+": "+message+" [confiming]");
        return true;
    }

}
