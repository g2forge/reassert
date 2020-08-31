package com.g2forge.reassert.core.model.contract.terms;

import java.util.Map;
import java.util.Set;

import com.g2forge.alexandria.java.function.builder.IBuilder;

public interface ITerms<T> {
	public static interface ITermsBuilder<T> extends IBuilder<ITerms<T>> {
		public ITermsBuilder<T> exclude(@SuppressWarnings("unchecked") T... terms);

		public ITermsBuilder<T> include(@SuppressWarnings("unchecked") T... terms);
	}

	public TermRelation getRelation(T term);

	public Map<T, TermRelation> getRelations(boolean specified);

	public Set<T> getTerms(boolean specified);

	/**
	 * Test whether the term is included in these terms.
	 * 
	 * @param term The term to test.
	 * @return {@code true} if this term is specified in this set of terms.
	 * @throws IllegalArgumentException if {@link #isSpecified(Object)} returns {@code false} for this {@code term}.
	 */
	public boolean isIncluded(T term);

	/**
	 * Test whether the term is specified in this set of terms. An unspecified term means that we know nothing about it. It may be included or not.
	 * 
	 * @param term The term to test.
	 * @return {@code true} if the term is specified in this set of terms.
	 */
	public boolean isSpecified(T term);

	public ITermsBuilder<T> toBuilder();
}
