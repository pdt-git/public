/*
 */
package model;

/**
 */
public interface IList extends ITerm{
    ITerm[] getHead();
    IList getTail();
}
