/*
 */
package org.cs3.pl.prolog.internal.xml;

import java.io.InputStream;

/**
 */
public interface IFileProvider {

    /**
     * @param subject
     * @param file
     * @return
     */
    InputStream getContent(String subject, String file);

}
