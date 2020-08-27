package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.TermsLoader;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardLicense implements ILicense {
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
	ZLIB("Zlib");

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final TermsLoader<StandardLicense, ILicenseTerm> loader = new TermsLoader<>(StandardLicense.class, StandardLicenseTerm.class);

	public static StandardLicense valueOfSPDX(String text) {
		return HEnum.valueOf(StandardLicense.class, ILicense::getSPDX, true, IFunction1.identity(), text);
	}

	protected final String SPDX;

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = getLoader().getTerms(this);

	@Override
	public String getName() {
		final String string = (getSPDX() == null) ? name() : getSPDX().replace('-', ' ');
		return string + " license";
	}
}
