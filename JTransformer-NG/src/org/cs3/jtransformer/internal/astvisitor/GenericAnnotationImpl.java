package org.cs3.jtransformer.internal.astvisitor;

import java.util.Iterator;
import java.util.List;

public class GenericAnnotationImpl implements GenericAnnotation {

	private String name;
	private List args;
	boolean inline;
	
	public GenericAnnotationImpl(String name, List args){
		this.name = name;
		this.args = args;
	}
	
	public String getName() {
		return name;
	}

	public List getArgs() {
		return args;
	}

	public String getPredicate() {
		String annList = "";
		for (Iterator iter = args.iterator(); iter.hasNext();) {
			String element =  (String)iter.next();
			if(annList.length()>0)
				annList+=",";
			annList+=element;
		}
		if(annList.length() > 0)
			annList = "("+annList+")";
		return name +annList;
	}

	public void setInline(boolean inline) {
		this.inline = inline;
		
	}

	public boolean isInline() {
		return inline;
	}

}
