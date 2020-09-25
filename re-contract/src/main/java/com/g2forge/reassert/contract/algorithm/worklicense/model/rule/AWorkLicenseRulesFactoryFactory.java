package com.g2forge.reassert.contract.algorithm.worklicense.model.rule;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class AWorkLicenseRulesFactoryFactory implements IWorkLicenseRulesFactoryFactory {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<ILicenseApplied, IFunction1<ILicenseFamily, IWorkLicenseRulesFactory>> function = computeFunction();

	@Note(type = NoteType.TODO, value = "Implement license operations", issue = "G2-919")
	@Override
	public IWorkLicenseRulesFactory apply(ILicenseApplied license) {
		final IFunction1<ILicenseFamily, IWorkLicenseRulesFactory> constructor = getFunction().apply(license);
		if (constructor == null) return null;
		return constructor.apply((ILicenseFamily) license);
	}

	protected abstract IFunction1<ILicenseApplied, IFunction1<ILicenseFamily, IWorkLicenseRulesFactory>> computeFunction();
}
