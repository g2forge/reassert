package com.g2forge.reassert.license;

import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.Terms;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.term.StandardLicenseTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardLicense implements ILicense {
	// @formatter:off
	Apache2("Apache-2.0", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Notice, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Trademark, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.PatentNonGrant).build()),
	BSD3("BSD-3-Clause", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Notice, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	GPL2Only("GPL-2.0-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	GPL2OrLater("GPL-2.0-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	GPL3Only("GPL-3.0-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	GPL3OrLater("GPL-3.0-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	LGPL21Only("LGPL-2.1-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	LGPL21OrLater("LGPL-2.1-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	LGPL3Only("LGPL-3.0-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	LGPL3OrLater("LGPL-3.0-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	Owner(null, Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse).exclude(StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Liability, StandardLicenseTerm.Trademark, StandardLicenseTerm.Warranty).build());
	// @formatter:on

	protected final String SPDX;

	protected final ITerms<ILicenseTerm> terms;

	@Override
	public String getName() {
		final String string = (getSPDX() == null) ? name() : getSPDX().replace('-', ' ');
		return string + " license";
	}
}
