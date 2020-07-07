package com.g2forge.reassert.license;

import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.Terms;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.term.StandardLicenseTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum StandardLicense implements ILicense {
	BSD3("BSD-3-Clause", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Notice, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build()),
	Apache2("Apache-2.0", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Notice, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Trademark, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.PatentNonGrant).build()),
	GPL3OrLater("GPL-3.0-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark).build());

	protected final String SPDX;

	protected final ITerms<ILicenseTerm> terms;

	@Override
	public String getName() {
		return name() + " License";
	}
}
