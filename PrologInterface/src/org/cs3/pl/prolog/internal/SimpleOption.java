/*
 */
package org.cs3.pl.prolog.internal;

import org.cs3.pl.prolog.Option;

/**
 */
public class SimpleOption implements Option {
    
    private String id;
    private String label;
    private String description;
    private int type;
    private String defaultValue;
    
    
    /**
     * @param id
     * @param label
     * @param description
     * @param type
     * @param defaultValue
     */
    public SimpleOption(String id, String label, String description, int type,
            String defaultValue) {
        super();
        this.id = id;
        this.label = label;
        this.description = description;
        this.type = type;
        this.defaultValue = defaultValue;
    }
    public String getId(){
        return id;
    }
    public  String getLabel(){
        return label;
    }
    public  String getDescription(){
        return description;
    }
    public  int getType (){
        return type;
    }
    public  String getDefault(){
        return defaultValue;
    }
}
