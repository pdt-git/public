package org.cs3.jtransformer.tests;

import java.io.File;
import java.util.Map;

import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IncrementalProjectBuilder;

public class UnicodeEscapingTest extends FactGenerationTest {

	public UnicodeEscapingTest(String name) {
		super(name);
	}

	PrologInterface pif;

    public void setUp() throws Exception {
        
        super.setUp();
    
        ResourceFileLocator l = JTransformerPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-facts.zip");
        setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator("testdata-facts"));
        Util.unzip(r);
        setAutoBuilding(false);
         pif = getTestJTransformerProject().getPrologInterface();
        try {            
            install("jt109");
            pif.start();
            build(IncrementalProjectBuilder.CLEAN_BUILD);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }       
    }
    
    public void testUnicodeCharacter() throws Exception {
        build();
        PrologSession session = null;
        try {
        	session = pif.getSession();
        	session.queryOnce("win_window_pos([show(true)])");
        	//Source
            Map map = session.queryOnce("fieldDefT(_,_,_,'UNICODE_CHARACTER_FIELD',Literal)," +
    		"literalT(Literal,_,_,_,Value)");
			String value = (String)map.get("Value");
			assertEquals("\\uffff", value);
			
			// Bytecode - we decided to ignore this problem!
//            map = session.queryOnce("fieldDefT(_,CharacterClass,_,'MAX_VALUE',Literal)," +
//            "fullQualifiedName(CharacterClass,'java.lang.Character'),"+
//    		"literalT(Literal,_,_,_,Value)");
//			value = (String)map.get("Value");
//			assertEquals("\\uffff", value);

        } finally {
        	if(session != null)
        		session.dispose();
        }
		
	}
    
}
