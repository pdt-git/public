package model;

public interface ISource {
    public ISourceFile getSourceFile();
    public int getOffset();
    public int getLength();
    public int getRow();
    public int getColumn();
    public String getData();
}
