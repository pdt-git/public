/*
 */
package org.cs3.pl.parser;

import java.io.File;

import junit.framework.TestCase;

/**
 */
public class StringLineBreakInfoProviderTest extends TestCase {
    public void test1()throws Throwable{
        String input = "Elephants forgot, force-fed on stale chalk,\n" +
        		"Ate the floors of their cages.\n" +
        		"Strongmen lost their hair, paybox collapsed and \n" +
        		"Lions sharpen their teeth.";
        //File.createTempFile("StringLineBreakInfoProviderTest","txt");
        int[] linebreaks=new int[]{0,43,74};
        int[] t = new int[]{25,43,56,74,100,123,130,149};
        int[] y = new int[]{0,0,1,1,2,2,3,3};
        
        StringLineBreakInfoProvider provider = new StringLineBreakInfoProvider (input,"\n");
        for(int i=0;i<linebreaks.length;i++){
            int offsetAtLine = provider.getOffsetAtLine(i);
            assertEquals(i==0? 0 : linebreaks[i]+1,offsetAtLine);
        }
    }
}
