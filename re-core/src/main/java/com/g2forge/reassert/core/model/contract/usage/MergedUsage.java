package com.g2forge.reassert.core.model.contract.usage;

import java.util.Set;

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
	protected final Set<IUsage> usages;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final ITerms<IUsageTerm> terms = computeTerms();

	protected ITerms<IUsageTerm> computeTerms() {
		final Terms.TermsBuilder<IUsageTerm> builder = Terms.<IUsageTerm>builder();
		final Set<IUsage> usages = getUsages();
		for (IUsage usage : usages) {
			final ITerms<IUsageTerm> terms = usage.getTerms();
			for (IUsageTerm term : terms.getTerms(true)) {
				if (!terms.isIncluded(term)) builder.exclude(term);
			}
		}
		for (IUsage usage : usages) {
			final ITerms<IUsageTerm> terms = usage.getTerms();
			for (IUsageTerm term : terms.getTerms(true)) {
				if (terms.isIncluded(term)) builder.include(term);
			}
		}
		return builder.build();
	}
}
