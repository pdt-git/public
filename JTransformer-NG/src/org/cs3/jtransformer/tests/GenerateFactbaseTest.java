/*
 */
package org.cs3.jtransformer.tests;

import java.io.File;
import java.util.List;

import org.cs3.jtransformer.JTPrologFacade;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;

public class GenerateFactbaseTest extends FactGenerationTest {
    public GenerateFactbaseTest(String name) {
        super(name);

    }

    /**
     * At first, have a look at the existing projects
     */
    public void setUp() throws Exception {

        super.setUp();
        ResourceFileLocator l = JTransformerPlugin.getDefault().getResourceLocator("");
        
        // This next two lines of code are optional:
        // Specify a zip file containing your source code in the JTransformer project:
        File r = l.resolve("testdata-facts.zip");
        Util.unzip(r);

        // Specify the directoy (relative to the JTransformer directory) containing your projects directories : 
        
        setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator(
                "testdata-facts"));
        setAutoBuilding(false);
        PrologInterface pif = getTestJTransformerProject().getPrologInterface();
        try {
        	
            // Specify which of the projects should be installed. 
        	// Every jar file contained in this directory will be automatically added to the class path.
            install("jt136");
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public void testIt() throws CoreException, PrologInterfaceException {
        clean();
        build();
        PrologSession s = getTestJTransformerProject().getPrologInterface()
                .getSession();

        
        // Write to the file: factbase.pl
        ResourceFileLocator locator = JTransformerPlugin.getDefault().getResourceLocator("");
        File file = locator.resolve("factbase.pl");
        
        
        String filename = file.getAbsolutePath();
        System.err.println("write factbase to " + filename);
        filename = filename.replace("\\", "/");
        List l = s.queryAll(JTPrologFacade.WRITE_TREE_FACTS + "('" + filename+ "')");
        assertEquals(1, l.size());
        s.dispose();
        assertEquals(1, l.size());
    }

}
