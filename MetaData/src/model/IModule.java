/*
 */
package model;

/**
 */
public interface IModule extends IPrologElement{
    public ISourceFile getSourceFile();
    public boolean isDefault();
    public IPredicate[] getPredicates();
}
