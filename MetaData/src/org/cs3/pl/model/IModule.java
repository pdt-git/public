/*
 */
package org.cs3.pl.model;

/**
 */
public interface IModule extends IPrologElement{
    public IPredicate[] getPredicates();
    public ISourceFile getSourceFile();
}
