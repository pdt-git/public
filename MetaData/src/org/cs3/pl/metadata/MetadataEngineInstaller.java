package org.cs3.pl.metadata;

import java.util.List;

import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;

/*
 * XXX: a ugly and hopefuly temporary solution.
 * the only (static) method of this class installs the prolog source in the directory pointed
 * to by the pif's factory's locator. it will then add the installed source files to the
 * pif's bootstrap library path.
 */
public class MetadataEngineInstaller {
    public static void install(PrologInterface pif) {
        PrologInterfaceFactory factory = pif.getFactory();
        factory.ensureInstalled("pdtplugin.pl", MetadataEngineInstaller.class);
        //factory.ensureInstalled("plparser.pl", MetadataEngineInstaller.class);
        //factory.ensureInstalled("runtime.pl", MetadataEngineInstaller.class);
        //factory.ensureInstalled("model.pl", MetadataEngineInstaller.class);
        //factory.ensureInstalled("metamodel.pl", MetadataEngineInstaller.class);
        factory.ensureInstalled("abba_graph_generator.pl", MetadataEngineInstaller.class);
        ResourceFileLocator locator = factory.getResourceLocator();
        List l = pif.getBootstrapLibraries();
        l.add(Util.prologFileName(locator.resolve("pdtplugin.pl")));
        //l.add(Util.prologFileName(locator.resolve("plparser.pl")));
        //l.add(Util.prologFileName(locator.resolve("runtime.pl")));
        //l.add(Util.prologFileName(locator.resolve("model.pl")));
        //l.add(Util.prologFileName(locator.resolve("metamodel.pl")));
        l.add(Util.prologFileName(locator.resolve("abba_graph_generator.pl")));
    }
}
