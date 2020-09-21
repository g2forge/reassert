package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.parse.IMatch;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamilyEnum;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardLicenseFamily implements ILicenseFamilyEnum {
	GNU("GNU Licenses", Versioning.VariableAllowed, null),
	GPL("GNU General Public License", Versioning.VariableAllowed, GNU),
	LGPL("GNU Lesser General Public License", Versioning.VariableAllowed, GNU),
	GFDL("GNU Free Documentation License", Versioning.VariableAllowed, GNU),
	AGPL("GNU Affero General Public License", Versioning.VariableAllowed, GNU),
	Permissive("Permissive Licenses", Versioning.Unversioned, null),
	BSD("Berkeley Software Distribution License", Versioning.Unversioned, Permissive),
	CC("Creative Commons Licenses", Versioning.FixedOnly, null),
	AFL("Academic Free License", Versioning.FixedOnly, null),
	Apache("Apache Software License", Versioning.FixedOnly, Permissive),
	Artistic("The Artistic License", Versioning.FixedOnly, null),
	BSL("Boost Software License", Versioning.FixedOnly, null),
	CDDL("Common Development and Distribution License", Versioning.FixedOnly, null),
	CPL("Common Public License", Versioning.FixedOnly, null),
	EDL("Eclipse Distribution License", Versioning.FixedOnly, null),
	EPL("Eclipse Public License", Versioning.FixedOnly, null),
	IndianaExtreme("Indiana University Extreme! Lab Software License", Versioning.FixedOnly, null),
	MPL("Mozilla Public License", Versioning.FixedOnly, null),
	OFL("SIL Open Font License", Versioning.FixedOnly, null),
	PSF("Python Software Foundation License", Versioning.FixedOnly, null),
	Python("Python License", Versioning.FixedOnly, null);

	public enum Versioning {
		Unversioned,
		FixedOnly,
		VariableAllowed;
	}

	protected final String name;

	protected final Versioning versioning;

	protected final ILicenseFamily family;

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = StandardLicense.getLoader().getTerms(getShortID());

	public ILicenseFamily create(IMatch<FamilyVersionLicense> match) {
		if (Versioning.Unversioned.equals(getVersioning())) throw new UnsupportedOperationException(String.format("Cannot version licenses in the %1$s family", getName()));
		final LicenseVersion version = match.getAsObject(FamilyVersionLicense::getVersion);

		final boolean orLater;
		if (Versioning.VariableAllowed.equals(getVersioning())) {
			final Boolean parsed = match.getAsObject(FamilyVersionLicense::isOrLater);
			orLater = (parsed != null) ? parsed : false;
		} else orLater = false;

		return create(version, orLater);
	}

	public ILicenseFamily create(final LicenseVersion version, final boolean orLater) {
		final boolean unversioned = Versioning.Unversioned.equals(getVersioning());
		if (unversioned && (version != null)) throw new UnsupportedOperationException(String.format("Cannot version licenses in the %1$s family", getName()));
		if ((version == null) && !orLater) return this;
		return new FamilyVersionLicense(this, version, orLater);
	}

	@Override
	public String getSPDXShortID() {
		return null;
	}
}
