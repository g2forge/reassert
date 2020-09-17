package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
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
	Apache2("Apache-2.0", StandardLicenseFamily.Apache, new LicenseVersion(2, 0), false),
	BSD2("BSD-2-Clause", StandardLicenseFamily.BSD, null, false),
	BSD3("BSD-3-Clause", StandardLicenseFamily.BSD, null, false),
	GPL2Only("GPL-2.0-only", StandardLicenseFamily.GPL, new LicenseVersion(2, 0), false),
	GPL2OrLater("GPL-2.0-or-later", StandardLicenseFamily.GPL, new LicenseVersion(2, 0), true),
	GPL3Only("GPL-3.0-only", StandardLicenseFamily.GPL, new LicenseVersion(3, 0), false),
	GPL3OrLater("GPL-3.0-or-later", StandardLicenseFamily.GPL, new LicenseVersion(3, 0), true),
	LGPL2Only("LGPL-2.0-only", StandardLicenseFamily.LGPL, new LicenseVersion(2, 0), false),
	LGPL2OrLater("LGPL-2.0-or-later", StandardLicenseFamily.LGPL, new LicenseVersion(2, 0), true),
	LGPL21Only("LGPL-2.1-only", StandardLicenseFamily.LGPL, new LicenseVersion(2, 1), false),
	LGPL21OrLater("LGPL-2.1-or-later", StandardLicenseFamily.LGPL, new LicenseVersion(2, 1), true),
	LGPL3Only("LGPL-3.0-only", StandardLicenseFamily.LGPL, new LicenseVersion(3, 0), false),
	LGPL3OrLater("LGPL-3.0-or-later", StandardLicenseFamily.LGPL, new LicenseVersion(3, 0), true),
	Owner(null, null, null, false),
	/**
	 * Note that this isn't a real license. This is usually an attempt by the author to put the work into the public domain. The
	 * <a href="https://creativecommons.org/share-your-work/public-domain/cc0/">creative commons CC0</a> description does a good job of outlining why "public
	 * domain" isn't a generally considered a license.
	 */
	PublicDomain(null, null, null, false),
	WTFPL("WTFPL", null, null, false),
	ZLIB("Zlib", null, null, false),
	AFL21("AFL-2.1", null, new LicenseVersion(2, 1), false),
	AGPL3Only("AGPL-3.0-only", null, new LicenseVersion(3, 0), false),
	AGPL3OrLater("AGPL-3.0-or-later", null, new LicenseVersion(3, 0), true),
	Apache11("Apache-1.1", StandardLicenseFamily.Apache, new LicenseVersion(1, 1), false),
	Artistic1("Artistic-1.0", null, new LicenseVersion(1, 0), false),
	Artistic2("Artistic-2.0", null, new LicenseVersion(2, 0), false),
	BSD4("BSD-4-Clause", StandardLicenseFamily.BSD, null, false),
	BSL1("BSL-1.0", null, new LicenseVersion(1, 0), false),
	CC01("CC0-1.0", null, new LicenseVersion(1, 0), false),
	CCBY3("CC-BY-3.0", null, new LicenseVersion(3, 0), false),
	CCBYSA2("CC-BY-SA-2.0", null, new LicenseVersion(2, 0), false),
	CCBYSA3("CC-BY-SA-3.0", null, new LicenseVersion(3, 0), false),
	EPL1("EPL-1.0", null, new LicenseVersion(1, 0), false),
	EPL2("EPL-2.0", null, new LicenseVersion(2, 0), false),
	GPL1Only("GPL-1.0-only", StandardLicenseFamily.GPL, new LicenseVersion(1, 0), false),
	GPL1OrLater("GPL-1.0-or-later", StandardLicenseFamily.GPL, new LicenseVersion(1, 0), true),
	GPL21Only("GPL-2.1-only", StandardLicenseFamily.GPL, new LicenseVersion(2, 1), false),
	GPL21OrLater("GPL-2.1-or-later", StandardLicenseFamily.GPL, new LicenseVersion(2, 1), true),
	ISC("ISC", null, null, false),
	MIT("MIT", null, null, false),
	MPL11("MPL-1.1", null, new LicenseVersion(1, 1), false),
	MPL2("MPL-2.0", null, new LicenseVersion(2, 0), false),
	OFL11("OFL-1.1", null, new LicenseVersion(1, 1), false),
	PostgreSQL("PostgreSQL", null, null, false),
	Beerware("Beerware", null, null, false),
	BSD1("BSD-1-Clause", StandardLicenseFamily.BSD, null, false),
	BSD5(null, StandardLicenseFamily.BSD, null, false),
	CDDL1("CDDL-1.0", null, new LicenseVersion(1, 0), false),
	CDDL11("CDDL-1.1", null, new LicenseVersion(1, 1), false),
	EDL1(null, null, new LicenseVersion(1, 0), false),
	FTL("FTL", null, null, false),
	GFDL11Only("GFDL-1.1-only", StandardLicenseFamily.GFDL, new LicenseVersion(1, 1), false),
	GFDL11OrLater("GFDL-1.1-or-later", StandardLicenseFamily.GFDL, new LicenseVersion(1, 1), true),
	GFDL12Only("GFDL-1.2-only", StandardLicenseFamily.GFDL, new LicenseVersion(1, 2), false),
	GFDL12OrLater("GFDL-1.2-or-later", StandardLicenseFamily.GFDL, new LicenseVersion(1, 2), true),
	GFDL13Only("GFDL-1.3-only", StandardLicenseFamily.GFDL, new LicenseVersion(1, 3), false),
	GFDL13OrLater("GFDL-1.3-or-later", StandardLicenseFamily.GFDL, new LicenseVersion(1, 3), true),
	Perl5(null, null, null, false),
	PSF2("PSF-2.0", null, null, false),
	Python2("Python-2.0", null, null, false),
	IndianaExtreme111(null, null, new LicenseVersion(1, 1, 1), false),
	CPL1("CPL-1.0", null, new LicenseVersion(1, 0), false);

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

	@Note(type = NoteType.TODO, value = "Implement license families", issue = "G2-928")
	@Override
	public boolean isChild(ILicenseFamily license) {
		return false;
	}
}
