/*
 */
package org.cs3.pl.model;

/**
 */
public interface IClause extends IPrologElement {
    public final static int DIRECTIVE=0;
    public final static int RULE=1;
    public final static int FACT=2;
    public int getClauseType();
    public String getClauseReference();
    public IPredicate getPredicate();
    public ITerm getHead();
    public ITerm getBody();        
}
