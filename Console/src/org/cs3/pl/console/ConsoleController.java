package org.cs3.pl.console;

/*
 */
/**
 */
public interface ConsoleController {
    public boolean keyStrokeIntercepted(int keyCode, char keyChar);   
    public void keyPressed(int keyCode, char keyChar);
    public boolean inputModificationIntercepted(String text, int from, int to, String string);
    public void inputModified(String newInput);
    public void setUI(ConsoleUI consoleUI);
    public void setModel(ConsoleModel consoleModel);
    
}
