/*
 */
package org.cs3.pl.model;

/**
 */
public interface ICompound extends ITerm {    
    public String getFunctor();
    public String getOperatorName();
    public ITerm[] getOperands();
    public IOperator getOperator();
    public int getArity();
    
}
