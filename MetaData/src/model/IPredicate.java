/*
 */
package model;

/**
 */
public interface IPredicate  extends ICompound{
    public IClause[] getClauses();
    public IModule getModule();
    public String getName();
    
    public boolean isMultifile();
    public boolean isDynamic();
    public boolean isPublic();
}
