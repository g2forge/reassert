package com.g2forge.reassert.core.model.contract.usage;

import java.util.Set;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Singular;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class MergedUsage implements IUsage {
	@Singular
	protected final Set<IUsageApplied> usages;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final ITerms<IUsageTerm> terms = computeTerms();

	@Note(type = NoteType.TODO, value = "Implement license operations", issue = "G2-919")
	protected ITerms<IUsageTerm> computeTerms() {
		final Terms.TermsBuilder<IUsageTerm> builder = Terms.<IUsageTerm>builder();
		final Set<IUsageApplied> usages = getUsages();
		for (IUsageApplied applied : usages) {
			final IUsage usage = (IUsage) applied;
			final ITerms<IUsageTerm> terms = usage.getTerms();
			for (IUsageTerm term : terms.getTerms(true)) {
				if (!terms.isIncluded(term)) builder.exclude(term);
			}
		}
		for (IUsageApplied applied : usages) {
			final IUsage usage = (IUsage) applied;
			final ITerms<IUsageTerm> terms = usage.getTerms();
			for (IUsageTerm term : terms.getTerms(true)) {
				if (terms.isIncluded(term)) builder.include(term);
			}
		}
		return builder.build();
	}
}
