/*
 */
package org.cs3.pl.model;

/**
 */
public interface IList extends ITerm{
    ITerm[] getHead();
    IList getTail();
}
