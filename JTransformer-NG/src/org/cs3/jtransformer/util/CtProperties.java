package org.cs3.jtransformer.util;

/**
 * Helper class which wrapps the CT properties. 
 *  
 * @author schmatz, rho		
 *
 */
public class CtProperties {
	private String name;
	private String file;
	private String adviceKind; 
	private boolean dynamic;
	
	
	
	public CtProperties(String ctName, String file, String adviceKinds, boolean dynamic) {
		this.name = ctName;
		this.file = file;
		this.adviceKind = adviceKinds;
		this.dynamic = dynamic;
	}
	
	public String getAdviceKind() {
		return adviceKind;
	}
	public void setAdviceKind(String adviceKind) {
		this.adviceKind = adviceKind;
	}
	public String getName() {
		return name;
	}
	public void setName(String ctNamesAndFiles) {
		this.name = ctNamesAndFiles;
	}
	public boolean isDynamic() {
		return dynamic;
	}
	public void setDynamic(boolean dynamic) {
		this.dynamic = dynamic;
	}

	public String getFileName() {
		return file;
	}

	public void setFile(String file) {
		this.file = file;
	}
	
}
