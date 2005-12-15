package org.cs3.pl.metadata.internal.classic;

import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.MetaInfoProviderFactory;
import org.cs3.pl.prolog.PrologInterface;


public class Factory extends MetaInfoProviderFactory {

	public IMetaInfoProvider create(PrologInterface pif) {
		return new DefaultMetaInfoProvider(pif);
	}

}
