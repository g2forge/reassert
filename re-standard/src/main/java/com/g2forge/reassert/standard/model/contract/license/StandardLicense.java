package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.TermsLoader;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecificEnum;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardLicense implements ILicenseSpecificEnum {
	Apache2("Apache-2.0"),
	BSD2("BSD-2-Clause"),
	BSD3("BSD-3-Clause"),
	GPL2Only("GPL-2.0-only"),
	GPL2OrLater("GPL-2.0-or-later"),
	GPL3Only("GPL-3.0-only"),
	GPL3OrLater("GPL-3.0-or-later"),
	LGPL2Only("LGPL-2.0-only"),
	LGPL2OrLater("LGPL-2.0-or-later"),
	LGPL21Only("LGPL-2.1-only"),
	LGPL21OrLater("LGPL-2.1-or-later"),
	LGPL3Only("LGPL-3.0-only"),
	LGPL3OrLater("LGPL-3.0-or-later"),
	Owner(null),
	/**
	 * Note that this isn't a real license. This is usually an attempt by the author to put the work into the public domain. The
	 * <a href="https://creativecommons.org/share-your-work/public-domain/cc0/">creative commons CC0</a> description does a good job of outlining why "public
	 * domain" isn't a generally considered a license.
	 */
	PublicDomain(null),
	WTFPL("WTFPL"),
	ZLIB("Zlib"),
	AFL21("AFL-2.1"),
	AGPL3Only("AGPL-3.0-only"),
	AGPL3OrLater("AGPL-3.0-or-later"),
	Apache11("Apache-1.1"),
	Artistic1("Artistic-1.0"),
	Artistic2("Artistic-2.0"),
	BSD4("BSD-4-Clause"),
	BSL1("BSL-1.0"),
	CC01("CC0-1.0"),
	CCBY3("CC-BY-3.0"),
	CCBYSA2("CC-BY-SA-2.0"),
	CCBYSA3("CC-BY-SA-3.0"),
	EPL1("EPL-1.0"),
	EPL2("EPL-2.0"),
	GPL1Only("GPL-1.0-only"),
	GPL1OrLater("GPL-1.0-or-later"),
	GPL21Only("GPL-2.1-only"),
	GPL21OrLater("GPL-2.1-or-later"),
	ISC("ISC"),
	MIT("MIT"),
	MPL11("MPL-1.1"),
	MPL2("MPL-2.0"),
	OFL11("OFL-1.1"),
	PostgreSQL("PostgreSQL"),
	Beerware("Beerware"),
	BSD1("BSD-1-Clause"),
	BSD5(null),
	CDDL1("CDDL-1.0"),
	CDDL11("CDDL-1.1"),
	EDL1(null),
	FTL("FTL"),
	GFDL11Only("GFDL-1.1-only"),
	GFDL11OrLater("GFDL-1.1-or-later"),
	GFDL12Only("GFDL-1.2-only"),
	GFDL12OrLater("GFDL-1.2-or-later"),
	GFDL13Only("GFDL-1.3-only"),
	GFDL13OrLater("GFDL-1.3-or-later"),
	Perl5(null),
	PSF2("PSF-2.0"),
	Python2("Python-2.0"),
	IndianaExtreme111(null),
	CPL1("CPL-1.0");

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final TermsLoader<StandardLicense, ILicenseTerm> loader = new TermsLoader<>(StandardLicense.class, StandardLicenseTerm.class);

	public static StandardLicense valueOfSPDX(String text) {
		return HEnum.valueOf(StandardLicense.class, ILicenseSpecific::getSPDXShortID, true, IFunction1.identity(), text);
	}

	protected final String SPDXShortID;

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = getLoader().getTerms(this);

	@Override
	public ILicenseFamily getFamily() {
		return null;
	}

	@Override
	public LicenseVersion getVersion() {
		return null;
	}

	@Override
	public boolean isChild(ILicenseFamily license) {
		return false;
	}

	@Override
	public boolean isOrLater() {
		return false;
	}
}
