package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class BSDLicense implements ILicenseSpecific {
	protected final int clauses;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final String SPDXShortID = getFamily().name() + '-' + getClauses() + "-Clause";

	@Override
	public StandardLicenseFamily getFamily() {
		return StandardLicenseFamily.BSD;
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

	@Override
	public LicenseVersion getVersion() {
		return null;
	}

	@Note(type = NoteType.TODO, value = "Implement license families", issue = "G2-928")
	@Override
	public boolean isChild(ILicenseFamily license) {
		return false;
	}

	@Override
	public boolean isOrLater() {
		return false;
	}
}
