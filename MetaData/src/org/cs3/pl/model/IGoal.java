/*
 */
package org.cs3.pl.model;

/**
 */
public interface IGoal extends ICompound{
    public IClause getClause();
    public IModule getContext();
   
}
