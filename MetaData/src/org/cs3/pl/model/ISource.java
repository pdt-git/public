package org.cs3.pl.model;

public interface ISource extends IPrologElement{
    public ISourceFile getSourceFile();
    public int getOffset();
    public int getLength();
    public int getRow();
    public int getColumn();    
}
