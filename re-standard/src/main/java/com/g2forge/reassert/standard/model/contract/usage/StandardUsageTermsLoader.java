package com.g2forge.reassert.standard.model.contract.usage;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.CSVTermsLoader;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;

@ReassertLegalOpinion
public class StandardUsageTermsLoader extends CSVTermsLoader<StandardUsageTerm> implements ISingleton {
	protected static final StandardUsageTermsLoader INSTANCE = new StandardUsageTermsLoader();

	public static StandardUsageTermsLoader create() {
		return INSTANCE;
	}

	protected StandardUsageTermsLoader() {
		super(StandardUsageTermsLoader.class, StandardUsageTerm.class);
	}
}
