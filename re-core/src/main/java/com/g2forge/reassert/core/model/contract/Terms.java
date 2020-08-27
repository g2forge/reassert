package com.g2forge.reassert.core.model.contract;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollector;

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

		public TermsBuilder<T> unspecified(@SuppressWarnings("unchecked") T... terms) {
			for (T term : terms) {
				term(term, TermRelation.Unspecified);
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
	public boolean equals(Object other) {
		if (this == other) return true;
		if (other == null) return false;
		if (getClass() != other.getClass()) return false;

		final Terms<?> that = (Terms<?>) other;
		return Objects.equals(getRelations(true), that.getRelations(true));
	}

	@Override
	public TermRelation getRelation(T term) {
		final TermRelation retVal = getTerms().get(term);
		if (retVal == null) return TermRelation.Unspecified;
		return retVal;
	}

	@Override
	public Map<T, TermRelation> getRelations(boolean specified) {
		final Map<T, TermRelation> terms = getTerms();
		if (terms == null) return null;
		if (specified) return terms.entrySet().stream().filter(e -> !TermRelation.Unspecified.equals(e.getValue())).collect(HCollector.toMapEntries());
		else return Collections.unmodifiableMap(terms);
	}

	@Override
	public Set<T> getTerms(boolean specified) {
		if (specified) return getTerms().keySet().stream().filter(this::isSpecified).collect(Collectors.toCollection(LinkedHashSet::new));
		else return Collections.unmodifiableSet(getTerms().keySet());
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(getRelations(true));
	}

	@Override
	public boolean isIncluded(T term) {
		if (!isSpecified(term)) throw new IllegalArgumentException();
		return TermRelation.Included.equals(getTerms().get(term));
	}

	@Override
	public boolean isSpecified(T term) {
		final TermRelation retVal = getTerms().get(term);
		return (retVal != null) && !TermRelation.Unspecified.equals(retVal);
	}
}
