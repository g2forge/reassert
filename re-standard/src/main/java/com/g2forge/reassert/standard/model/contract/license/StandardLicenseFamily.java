package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.java.validate.CompositeValidation;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.parse.IMatch;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.license.ChildLicenseFamilyValidation;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamilyEnum;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.license.LicenseVersioning;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardLicenseFamily implements ILicenseFamilyEnum {
	GNU("GNU Licenses", LicenseVersioning.VariableAllowed, LicenseVersion.Field.MINOR, null),
	GPL("GNU General Public License", LicenseVersioning.VariableAllowed, LicenseVersion.Field.MINOR, GNU),
	LGPL("GNU Lesser General Public License", LicenseVersioning.VariableAllowed, LicenseVersion.Field.MINOR, GNU),
	GFDL("GNU Free Documentation License", LicenseVersioning.VariableAllowed, LicenseVersion.Field.MINOR, GNU),
	AGPL("GNU Affero General Public License", LicenseVersioning.VariableAllowed, LicenseVersion.Field.MINOR, GNU),
	Permissive("Permissive Licenses", LicenseVersioning.Unversioned, null, null),
	BSD("Berkeley Software Distribution License", LicenseVersioning.Unversioned, null, Permissive),
	CC("Creative Commons Licenses", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	AFL("Academic Free License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	Apache("Apache Software License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, Permissive),
	Artistic("The Artistic License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	BSL("Boost Software License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	CDDL("Common Development and Distribution License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	CPL("Common Public License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	EDL("Eclipse Distribution License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	EPL("Eclipse Public License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	IndianaExtreme("Indiana University Extreme! Lab Software License", LicenseVersioning.FixedOnly, LicenseVersion.Field.PATCH, null),
	MPL("Mozilla Public License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	OFL("SIL Open Font License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	PSF("Python Software Foundation License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null),
	Python("Python License", LicenseVersioning.FixedOnly, LicenseVersion.Field.MINOR, null);

	static {
		ILicenseFamilyEnum.validate(StandardLicenseFamily.class);
	}

	protected final String name;

	protected final LicenseVersioning versioning;

	protected final LicenseVersion.Field maxVersionField;

	protected final ILicenseFamily family;

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = ILicenseFamily.getTerms(this);

	public ILicenseFamily create(IMatch<FamilyVersionLicense> match) {
		if (LicenseVersioning.Unversioned.equals(getVersioning())) throw new UnsupportedOperationException(String.format("Cannot version licenses in the %1$s family", getName()));
		final LicenseVersion version = match.getAsObject(FamilyVersionLicense::getVersion);

		final boolean orLater;
		if (LicenseVersioning.VariableAllowed.equals(getVersioning())) {
			final Boolean parsed = match.getAsObject(FamilyVersionLicense::isOrLater);
			orLater = (parsed != null) ? parsed : false;
		} else orLater = false;

		return create(version, orLater);
	}

	public ILicenseFamily create(final LicenseVersion version, final boolean orLater) {
		final boolean unversioned = LicenseVersioning.Unversioned.equals(getVersioning());
		if (unversioned && (version != null)) throw new UnsupportedOperationException(String.format("Cannot version licenses in the %1$s family", getName()));
		if ((version == null) && !orLater) return this;
		return new FamilyVersionLicense(this, version, orLater);
	}

	@Override
	public String getSPDXShortID() {
		return null;
	}

	@Override
	public IValidation validate(ILicenseFamily child) {
		final IValidation childValidation = new ChildLicenseFamilyValidation(child.isChild(this));
		final IValidation versioningValidation;
		if (child instanceof ILicenseSpecific) {
			final ILicenseSpecific cast = (ILicenseSpecific) child;
			versioningValidation = getVersioning().validate(cast, getMaxVersionField());
		} else versioningValidation = null;
		return CompositeValidation.create(childValidation, versioningValidation);
	}
}
