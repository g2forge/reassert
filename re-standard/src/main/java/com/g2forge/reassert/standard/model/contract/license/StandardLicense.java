package com.g2forge.reassert.standard.model.contract.license;

import java.util.Map;

import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.reassert.contract.TermsMapper;
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
	ZLIB("Zlib");

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final Map<StandardLicense, ITerms<ILicenseTerm>> allTerms = computeAllTerms();

	protected static Map<StandardLicense, ITerms<ILicenseTerm>> computeAllTerms() {
		final Class<StandardLicense> klass = StandardLicense.class;
		return new TermsMapper().<StandardLicense, ILicenseTerm>read(klass, StandardLicenseTerm.class, new ResourceDataSource(new Resource(klass, klass.getSimpleName().toLowerCase() + ".csv")));
	}

	public static StandardLicense valueOfSPDX(String text) {
		return HEnum.valueOf(StandardLicense.class, ILicense::getSPDX, IFunction1.identity(), text);
	}

	protected final String SPDX;

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = computeTerms();

	protected ITerms<ILicenseTerm> computeTerms() {
		return getAllTerms().get(this);
	}

	@Override
	public String getName() {
		final String string = (getSPDX() == null) ? name() : getSPDX().replace('-', ' ');
		return string + " license";
	}
}
