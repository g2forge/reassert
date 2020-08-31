package com.g2forge.reassert.contract;

import java.util.Map;

import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class TermsLoader<C, T> {
	protected final Class<C> contractClass;

	protected final Class<? extends T> termClass;

	protected final Terms<T> EMPTY_TERMS = Terms.<T>builder().build();

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Map<C, ITerms<T>> allTerms = computeAllTerms();

	protected Map<C, ITerms<T>> computeAllTerms() {
		final Class<C> klass = getContractClass();
		return new TermsMapper().<C, T>read(klass, getTermClass(), new ResourceDataSource(new Resource(klass, klass.getSimpleName().toLowerCase() + ".csv")));
	}

	public ITerms<T> getTerms(C contract) {
		final Map<C, ITerms<T>> allTerms = getAllTerms();
		return allTerms.getOrDefault(contract, EMPTY_TERMS);
	}
}
