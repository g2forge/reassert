package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.CSVTermsLoader;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;

@ReassertLegalOpinion
public class StandardLicenseTermsLoader extends CSVTermsLoader<StandardLicenseTerm> implements ISingleton {
	protected static final StandardLicenseTermsLoader INSTANCE = new StandardLicenseTermsLoader();

	public static StandardLicenseTermsLoader create() {
		return INSTANCE;
	}

	protected StandardLicenseTermsLoader() {
		super(StandardLicenseTermsLoader.class, StandardLicenseTerm.class);
	}
}
