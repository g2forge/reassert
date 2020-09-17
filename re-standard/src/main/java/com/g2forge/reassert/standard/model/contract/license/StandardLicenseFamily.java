package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamilyEnum;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum StandardLicenseFamily implements ILicenseFamilyEnum {
	GPL("GNU General Public License", Versioning.VariableAllowed),
	LGPL("GNU Lesser General Public License", Versioning.VariableAllowed),
	GFDL("GNU Free Documentation License", Versioning.VariableAllowed),
	BSD("Berkeley Software Distribution License", Versioning.Unversioned),
	Apache("Apache Software License", Versioning.FixedOnly);

	public enum Versioning {
		Unversioned,
		FixedOnly,
		VariableAllowed;
	}

	protected final String name;

	protected final Versioning versioning;

	@Override
	public ILicenseFamily getFamily() {
		return null;
	}

	@Override
	public String getSPDXShortID() {
		return null;
	}

	@Note(type = NoteType.TODO, value = "Implement license families", issue = "G2-928")
	@Override
	public ITerms<ILicenseTerm> getTerms() {
		// TODO Auto-generated method stub
		return null;
	}

	@Note(type = NoteType.TODO, value = "Implement license families & ischild test", issue = "G2-928")
	@Override
	public boolean isChild(ILicenseFamily license) {
		// TODO Auto-generated method stub
		return false;
	}
}
