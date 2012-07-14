package org.cs3.pdt.internal.structureElements;

import java.util.List;

public class PrologClause {
	
	private String file;
	private String clauseFile;
	private boolean isFromOtherFile;
	private String entity;
	private int entityLine;
	private String kindOfEntity;
	private String functor;
	private int arity;
	private int line;
	private String type;
	private List<String> properties;

	public PrologClause(String file, String entity, int entityLine, String kindOfEntity, String functor, int arity, int line, String type, List<String> properties) {
		this.file = file;
		this.entity = entity;
		this.entityLine = entityLine;
		this.kindOfEntity = kindOfEntity;
		this.functor = functor;
		this.arity = arity;
		this.line = line;
		this.type = type;
		this.properties = properties;
		calculateOccurranceFile();
	}

	public String getFile() {
		return file;
	}

	public String getEntity() {
		return entity;
	}

	public int getEntityLine() {
		return entityLine;
	}

	public String getKindOfEntity() {
		return kindOfEntity;
	}

	public String getFunctor() {
		return functor;
	}

	public int getArity() {
		return arity;
	}

	public int getLine() {
		return line;
	}

	public String getType() {
		return type;
	}

	public List<String> getProperties() {
		return properties;
	}

	private void calculateOccurranceFile() {
		String selectedFile = null;
		if (properties.contains("multifile")) {
			for (String property : properties) {
				if (property.startsWith("defining_file(")) {
					selectedFile = property.substring(15, property.length()-2);
				}
			}
		}
		if (selectedFile == null) {
			clauseFile = file;
			isFromOtherFile = false;
		} else {
			isFromOtherFile = !selectedFile.equals(file);
			clauseFile = selectedFile;
		}
	}
	
	public String getOccuranceFile() {
		return clauseFile;
	}
	
	public boolean isFromOtherFile() {
		return isFromOtherFile;
	}
}
