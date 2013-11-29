package org.cs3.pdt.internal.contentassistant.templates;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.contentassistant.DefaultCompletion;
import org.cs3.prolog.common.logging.Debug;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

public class TemplateReader {
	
	public static List<DefaultCompletion> readDefaultCompletions() {
		ArrayList<DefaultCompletion> completions = new ArrayList<DefaultCompletion>();
		readLogtalkTemplates(completions);
		return completions;
	}
	
	private static void readLogtalkTemplates(ArrayList<DefaultCompletion> completions) {
		readLogtalkTextMateTemplates(completions);
	}
	
	private static void readLogtalkTextMateTemplates(ArrayList<DefaultCompletion> completions) {
		XMLReader xmlReader;
		try {
			xmlReader = XMLReaderFactory.createXMLReader();
			xmlReader.setContentHandler(new TextMateTemplateParser(completions));
			ArrayList<URL> urls = getLogtalkTextMateTemplateURLs();
			for (URL url : urls) {
				InputSource inputSource = new InputSource(url.openStream());
				xmlReader.parse(inputSource);
			} 
		} catch (SAXException e) {
			Debug.report(e);
		} catch (IOException e) {
			Debug.report(e);
		}
	}
	
	private static ArrayList<URL> getLogtalkTextMateTemplateURLs() {
		ArrayList<URL> urls = new ArrayList<URL>();
		Enumeration<String> entryPaths = PDTPlugin.getDefault().getBundle().getEntryPaths("/templates/logtalk/textmate/");
		while (entryPaths.hasMoreElements()) {
			String value= entryPaths.nextElement();
			URL url = PDTPlugin.getDefault().getBundle().getEntry(value);
			if (url != null) {
				urls.add(url);
			}
		}
		return urls;
	}


}
