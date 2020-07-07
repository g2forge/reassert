package com.g2forge.reassert.core.model.contract;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Terms<T> implements ITerms<T> {
	public static class TermsBuilder<T> implements ITermsBuilder<T> {
		public TermsBuilder<T> exclude(@SuppressWarnings("unchecked") T... terms) {
			for (T term : terms) {
				term(term, TermRelation.Excluded);
			}
			return this;
		}

		public TermsBuilder<T> include(@SuppressWarnings("unchecked") T... terms) {
			for (T term : terms) {
				term(term, TermRelation.Included);
			}
			return this;
		}
	}

	protected static ITerms<?> NONE = Terms.builder().build();

	@SuppressWarnings("unchecked")
	public static <T> ITerms<T> createNone() {
		return (ITerms<T>) NONE;
	}

	@Singular
	@Getter(AccessLevel.PROTECTED)
	protected final Map<T, TermRelation> terms;

	@Override
	public TermRelation getRelation(T term) {
		final TermRelation retVal = getTerms().get(term);
		if (retVal == null) return TermRelation.Unspecified;
		return retVal;
	}

	@Override
	public Set<T> getSpecifiedTerms() {
		return getTerms().keySet().stream().filter(this::isSpecified).collect(Collectors.toCollection(LinkedHashSet::new));
	}

	@Override
	public boolean isIncluded(T term) {
		if (!isSpecified(term)) throw new IllegalArgumentException();
		return TermRelation.Included == getTerms().get(term);
	}

	@Override
	public boolean isSpecified(T term) {
		final TermRelation retVal = getTerms().get(term);
		return (retVal != null) && (retVal != TermRelation.Unspecified);
	}
}
