package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.TermsLoader;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamilyEnum;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardLicense implements ILicenseFamilyEnum, ILicenseSpecific {
	Owner(null, null, null, false),
	/**
	 * Note that this isn't a real license. This is usually an attempt by the author to put the work into the public domain. The
	 * <a href="https://creativecommons.org/share-your-work/public-domain/cc0/">creative commons CC0</a> description does a good job of outlining why "public
	 * domain" isn't a generally considered a license.
	 */
	PublicDomain(null, null, null, false),
	WTFPL("WTFPL", null, null, false),
	ZLIB("Zlib", null, null, false),
	ISC("ISC", null, null, false),
	MIT("MIT", StandardLicenseFamily.Permissive, null, false),
	PostgreSQL("PostgreSQL", null, null, false),
	Beerware("Beerware", null, null, false),
	FTL("FTL", null, null, false),
	Perl5(null, null, null, false);

	static {
		ILicenseFamilyEnum.validate(StandardLicense.class);
	}

	@ReassertLegalOpinion
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final TermsLoader<String, ILicenseTerm> loader = new TermsLoader<>(StandardLicense.class, String.class, StandardLicenseTerm.class);

	public static StandardLicense valueOfSPDX(String text) {
		return HEnum.valueOf(StandardLicense.class, ILicenseSpecific::getSPDXShortID, true, IFunction1.identity(), text);
	}

	protected final String SPDXShortID;

	protected final ILicenseFamily family;

	protected final LicenseVersion version;

	protected final boolean orLater;

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = getLoader().getTerms(getShortID());
}
