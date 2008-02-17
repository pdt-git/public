package org.cs3.pdt.transform;

import java.io.File;

import org.cs3.pl.prolog.PrologLibrary;

public abstract class PrologRefactoringDescriptor {
	private PrologLibrary[] dependencies;
	private File[] definitions;
	private String id;
	private String label;
	private Class objectClass;
	private boolean adaptable;
	private String description;
	
	public final PrologLibrary[] getDependencies() {
		return dependencies;
	}
	public final void setDependencies(PrologLibrary[] dependencies) {
		this.dependencies = dependencies;
	}
	public final File[] getDefinitions() {
		return definitions;
	}
	public final void setDefinitions(File[] definitions) {
		this.definitions = definitions;
	}
	public final String getId() {
		return id;
	}
	public final void setId(String id) {
		this.id = id;
	}
	public final String getLabel() {
		return label;
	}
	public final void setLabel(String label) {
		this.label = label;
	}
	public final Class getObjectClass() {
		return objectClass;
	}
	public final void setObjectClass(Class objectClass) {
		this.objectClass = objectClass;
	}
	public final boolean isAdaptable() {
		return adaptable;
	}
	public final void setAdaptable(boolean adaptable) {
		this.adaptable = adaptable;
	}
	public final String getDescription() {
		return description;
	}
	public final void setDescription(String description) {
		this.description = description;
	}
	
}
