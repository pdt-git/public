/*
 */
package model;

import java.io.InputStream;

/**
 */
public interface ISourceFile {
    InputStream getContents();
    String getContents(int offset, int length);
    IModule getModule();
    IClause[] getClauses();
    ISourceFolder getParent();
}
