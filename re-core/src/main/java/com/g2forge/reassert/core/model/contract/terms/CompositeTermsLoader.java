package com.g2forge.reassert.core.model.contract.terms;

import java.util.Collection;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.model.contract.IContractIdentified;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class CompositeTermsLoader implements ITermsLoader {
	protected final Collection<ITermsLoader> loaders;

	@Override
	public <T> ITerms<T> getTerms(IContractIdentified contract, Class<? extends T> termClass) {
		final Set<ITerms<T>> retVal = getLoaders().stream().map(loader -> loader.<T>getTerms(contract, termClass)).filter(Objects::nonNull).collect(Collectors.toSet());
		if (retVal.size() != 1) throw new IllegalStateException(String.format("Unable to load terms for %1$s, possible terms: %2$s", contract.getShortID(), retVal));
		return HCollection.getOne(retVal);
	}
}
