package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseFamily.Versioning;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class FamilyVersionLicense implements ILicenseSpecific {
	protected final StandardLicenseFamily family;

	protected final LicenseVersion version;

	protected final boolean orLater;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final String SPDXShortID = computeSPDXShortID();

	protected String computeSPDXShortID() {
		final StringBuilder retVal = new StringBuilder();

		final StandardLicenseFamily family = getFamily();
		retVal.append(family.name());

		final Versioning versioning = family.getVersioning();
		if (StandardLicenseFamily.Versioning.Unversioned.compareTo(versioning) < 0) {
			retVal.append('-').append(getVersion());
			if (StandardLicenseFamily.Versioning.VariableAllowed.compareTo(versioning) <= 0) retVal.append(isOrLater() ? "-or-later" : "-only");
		}

		return retVal.toString();
	}

	@Override
	public String getName() {
		return getShortID().replace('-', ' ') + " license";
	}

	@Note(type = NoteType.TODO, value = "Implement license term loading", issue = "G2-931")
	@Override
	public ITerms<ILicenseTerm> getTerms() {
		// TODO Auto-generated method stub
		return null;
	}

	@Note(type = NoteType.TODO, value = "Implement license families", issue = "G2-928")
	@Override
	public boolean isChild(ILicenseFamily license) {
		return false;
	}
}
