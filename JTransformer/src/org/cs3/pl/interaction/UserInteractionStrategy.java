package org.cs3.pl.interaction;

/**
 * defines a strategy for addressing the user.
 * 
 * all methods should block the calling thread until the user reacts in
 * some way. This is different from Debug or similar classes.
 * Explicit interaction by the user should be required. 
 * (unless this is a test run) 
 */
public interface UserInteractionStrategy {
    public void tell(String message, String title);
    public int ask(String message, String title, String[] options,int defVal);
    public String ask(String message, String title, String prompt, String defVal);
    public boolean askForConfirmation(String message, String title);
    
}
