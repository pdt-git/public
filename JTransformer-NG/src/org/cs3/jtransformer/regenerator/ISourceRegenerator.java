package org.cs3.jtransformer.regenerator;


public interface ISourceRegenerator {

    public void generateDirtyClasses();
    public IAffectedFile[] getAffectedFiles();

} 