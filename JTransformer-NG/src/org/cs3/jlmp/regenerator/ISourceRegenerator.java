package org.cs3.jlmp.regenerator;


public interface ISourceRegenerator {

    public void generateDirtyClasses();
    public IAffectedFile[] getAffectedFiles();

} 