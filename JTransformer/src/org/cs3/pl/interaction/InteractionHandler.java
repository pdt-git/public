/*
 * Created on 03.07.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.interaction;


/**
 * @author lukas
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class InteractionHandler{
    private static UserInteractionStrategy uiStrategy=new NoInteraction();

    
    public static UserInteractionStrategy getUserInteractionStrategy() {
        return uiStrategy;
    }
    
    public static void setUserInteractionStrategy(UserInteractionStrategy uiStrategy) {
        InteractionHandler.uiStrategy = uiStrategy;
    }
    
    public static void tell(String message, String title) {
        uiStrategy.tell(message,title);
    }
    public static int ask(String message, String title, String[] options, int defVal) {
        return uiStrategy.ask(message,title,options,defVal);
    }

    public static String ask(String message, String title, String prompt, String defVal) {
       return uiStrategy.ask(message,title,prompt,defVal);
    }
    public static boolean askForConfirmation(String message, String title) {
        return uiStrategy.askForConfirmation(message,title);
    }

}
