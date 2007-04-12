package org.cs3.jtransformer.util;

/**
 * Helper class which wrapps the CT properties. 
 *  
 * @author schmatz, rho		
 *
 */
public class CtProperties {
	private String ctNamesAndFiles;
	private String adviceKinds; 
	private boolean dynamic;
	
	
	
	public CtProperties(String ctNamesAndFiles, String adviceKinds, boolean dynamic) {
		this.ctNamesAndFiles = ctNamesAndFiles;
		this.adviceKinds = adviceKinds;
		this.dynamic = dynamic;
	}
	
	public String getAdviceKinds() {
		return adviceKinds;
	}
	public void setAdviceKinds(String adviceKinds) {
		this.adviceKinds = adviceKinds;
	}
	public String getCtNamesAndFiles() {
		return ctNamesAndFiles;
	}
	public void setCtNamesAndFiles(String ctNamesAndFiles) {
		this.ctNamesAndFiles = ctNamesAndFiles;
	}
	public boolean isDynamic() {
		return dynamic;
	}
	public void setDynamic(boolean dynamic) {
		this.dynamic = dynamic;
	}
	
}
