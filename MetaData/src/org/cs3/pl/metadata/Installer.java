package org.cs3.pl.metadata;

import java.util.List;

import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;

/**
 * XXX: a ugly and hopefuly temporary solution.
 */
public class Installer {
    public static void install(PrologInterface pif) {
        PrologInterfaceFactory factory = pif.getFactory();
        factory.ensureInstalled("pdtplugin.pl", Installer.class);
        factory.ensureInstalled("plparser.pl", Installer.class);
        factory.ensureInstalled("runtime.pl", Installer.class);
        factory.ensureInstalled("model.pl", Installer.class);
        ResourceFileLocator locator = factory.getResourceLocator();
        List l = pif.getBootstrapLibraries();
        l.add(Util.prologFileName(locator.resolve("pdtplugin.pl")));
        l.add(Util.prologFileName(locator.resolve("plparser.pl")));
        l.add(Util.prologFileName(locator.resolve("runtime.pl")));
        l.add(Util.prologFileName(locator.resolve("model.pl")));
    }
}
