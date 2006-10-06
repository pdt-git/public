package org.cs3.jtransformer.tests;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;

public class SourceLocationArgumentTest extends PseudoRoundTripTest {


	public SourceLocationArgumentTest(String name) {
        super(name);
        this.packageName = name;
    }

    /**
     * @param string
     * @param string2
     */
    public SourceLocationArgumentTest(String name, String packageName) {
        super(name,packageName);
    }
	

    protected void checkTreeValidity()  {
		PrologSession session = null;
		try {
			PrologInterface pif = getTestJTransformerProject()
					.getPrologInterface();
			session = pif.getSession();
			List results = session
					.queryAll("source_location_identifier('Java', AST, Identifier, File, Start, Length)");
			for (Iterator iter = results.iterator(); iter.hasNext();) {
				Map result = (Map) iter.next();
				String ident = (String) result.get("Identifier");
				String filename = (String) result.get("File");
				int start = Integer.parseInt((String) result.get("Start"));
				int length = Integer.parseInt((String) result.get("Length"));

				String content = getFileContent(filename);
				assertEquals(ident, content.substring(start, start + length));

			}
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null) {
				session.dispose();
			}
		}
	}
    
    Hashtable table = new Hashtable();


	private String getFileContent(String filename) throws CoreException, IOException {
		if(table.get(filename) != null)
			return (String)table.get(filename);
		IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(filename));
		StringBuffer buf = new StringBuffer();
		BufferedReader reader = null; 
		try {
			reader = new BufferedReader(new InputStreamReader(file.getContents()));
			while(reader.ready()) {
				buf.append((char)reader.read());
			}
		} finally {
			if(reader != null)
			   reader.close();
		}
		String content = buf.toString();
		table.put(filename, content);
		return content;
	}
	
    public static Test suite() {
    	clazz = SourceLocationArgumentTest.class;
    	TestSuite suite = (TestSuite)PseudoRoundTripTest.suite();
    	suite.setName("SourceLocationArgumentTest");
    	return suite;
    }
}
