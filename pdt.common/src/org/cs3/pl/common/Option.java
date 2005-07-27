/*
 */
package org.cs3.pl.common;

/**
 */
public interface Option {
    public final static int FLAG=0;
    public final static int NUMBER=1;
    public final static int STRING=2;
    public final static int FILE=3;
    public final static int DIR=4;
    public final static int FILES=5;
    public final static int DIRS=6;
    public final static int PATH=7;
    public static final int ENUM = 8;
    public  String getDefault();
    public  String getDescription();
    public String getId();
    public  String getLabel();
    public String[][] getEnumValues();
    public  int getType ();
    /**
     * @param value
     * @return
     */
    public String validate(String value);

}
