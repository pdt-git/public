/*
 */
package model;

/**
 */
public interface ICompound extends ITerm {    
    public final static int INFIX=3;
    public final static int PREFIX=4;
    public final static int POSTFIX=5;
    public int getCompoundType();
    public String getFunctor();
    public String getOperator();
    public ITerm[] getOperants();
    public int getArity();
    
}
