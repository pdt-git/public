/*
 */
package model;

/**
 */
public interface IClause extends IPrologElement {
    public final static int DIRECTIVE=0;
    public final static int RULE=1;
    public final static int FACT=2;
    public int getClauseType();
    public IPredicate getPredicate();
    public ITerm getHead();
    public IGoal[] getBody();
    public boolean isFact();
    public boolean isRule();
    public boolean isDirective();
}
