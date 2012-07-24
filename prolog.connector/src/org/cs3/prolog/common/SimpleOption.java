/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 */
package org.cs3.prolog.common;


/**
 */
public class SimpleOption implements Option {
    
    private String id;
    private String label;
    private String description;
    private int type;
    private String defaultValue;
    private String[][] enumValues;
    
    
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
        this.enumValues=null;
    }
    
    public SimpleOption(String id, String label, String description, int type,
            String defaultValue,String[][] enumValues) {
        super();
        this.id = id;
        this.label = label;
        this.description = description;
        this.type = type;
        this.defaultValue = defaultValue;
        this.enumValues=enumValues;
    }
    @Override
	public String getId(){
        return id;
    }
    @Override
	public  String getLabel(){
        return label;
    }
    @Override
	public  String getDescription(){
        return description;
    }
    @Override
	public  int getType (){
        return type;
    }
    @Override
	public  String getDefault(){
        return defaultValue;
    }
    @Override
	public String[][] getEnumValues() {       
        return enumValues;
    }
    @Override
	public String validate(String value) {       
        return null;
    }

    /**
     * this implementation always returns true.
     * override to change.
     */
    @Override
	public boolean isVisible() {
		return true;
	}
    
	@Override
	public String getHint(String key) {
		return null;
	}
	
	@Override
	public boolean isEditable() {	
		return true;
	}
}


