/*
 */
package org.cs3.pl.model;

/**
 */
public interface IPredicate extends IPrologElement {
    public IClause[] getClauses();
    public IModule getModule();
    public String getName();
    public String getFunctor();
    public int getArity();
    public boolean isMultifile();
    public boolean isDynamic();
    public boolean isExported();
}
