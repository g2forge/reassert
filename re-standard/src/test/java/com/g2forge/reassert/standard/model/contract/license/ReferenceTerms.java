package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamilyEnum;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum ReferenceTerms implements ILicenseFamilyEnum, ILicenseSpecific {
	// @formatter:off
	Apache2("Apache-2.0", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Notice, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Trademark, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.NoRedistribution).build()),
	BSD2("BSD-2-Clause", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Notice, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	BSD3("BSD-3-Clause", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Notice, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	GPL2Only("GPL-2.0-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	GPL2OrLater("GPL-2.0-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	GPL3Only("GPL-3.0-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	GPL3OrLater("GPL-3.0-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	LGPL2Only("LGPL-2.0-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	LGPL2OrLater("LGPL-2.0-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	LGPL21Only("LGPL-2.1-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	LGPL21OrLater("LGPL-2.1-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	LGPL3Only("LGPL-3.0-only", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	LGPL3OrLater("LGPL-3.0-or-later", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build()),
	Owner(null, Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PatentGrant, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.NoRedistribution).exclude(StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.Notice, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.StateChanges, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Liability, StandardLicenseTerm.Trademark, StandardLicenseTerm.Warranty).build()),
	ZLIB("Zlib", Terms.<ILicenseTerm>builder().include(StandardLicenseTerm.CommercialUse, StandardLicenseTerm.Distribution, StandardLicenseTerm.Modification, StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Notice, StandardLicenseTerm.StateChanges, StandardLicenseTerm.Liability, StandardLicenseTerm.Warranty).exclude(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.DisclosureSource, StandardLicenseTerm.SaaSIsDistribution, StandardLicenseTerm.SameLicense, StandardLicenseTerm.PatentNonGrant, StandardLicenseTerm.Trademark, StandardLicenseTerm.NoRedistribution).build());
	// @formatter:on

	static {
		ILicenseFamilyEnum.validate(ReferenceTerms.class);
	}

	public static ReferenceTerms valueOfShortID(String text) {
		return HEnum.valueOf(ReferenceTerms.class, ILicenseSpecific::getShortID, true, IFunction1.identity(), text);
	}

	protected final String SPDXShortID;

	protected final ITerms<ILicenseTerm> terms;

	@Override
	public ILicenseFamily getFamily() {
		return null;
	}

	@Override
	public LicenseVersion getVersion() {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean isOrLater() {
		throw new UnsupportedOperationException();
	}
}
