package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.reassert.core.model.contract.IContractTerms;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

public interface IUsage extends IUsageApplied, IContractTerms {
	@Note(type = NoteType.TODO, value = "Implement license operations", issue = "G2-919")
	public static boolean isEqualTerms(IUsageApplied ua0, IUsageApplied ua1) {
		final IUsage u0 = (IUsage) ua0;
		final IUsage u1 = (IUsage) ua1;
		return u0.getTerms().equals(u1.getTerms());
	}

	@Override
	public ITerms<IUsageTerm> getTerms();
}
