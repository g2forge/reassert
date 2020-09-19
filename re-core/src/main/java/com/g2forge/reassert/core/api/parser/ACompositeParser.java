package com.g2forge.reassert.core.api.parser;

import java.util.Collection;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public abstract class ACompositeParser<T> implements IParser<T> {
	protected final Collection<? extends IParser<? extends T>> parsers;

	@SafeVarargs
	public ACompositeParser(IParser<? extends T>... parsers) {
		this(HCollection.asList(parsers));
	}

	@Override
	public T parse(String text) {
		final Set<T> retVals = getParsers().stream().map(parser -> parser.parse(text)).filter(Objects::nonNull).filter(license -> !(license instanceof UnknownLicense)).collect(Collectors.toSet());
		if (retVals.size() != 1) return unknown(text);
		return HCollection.getOne(retVals);
	}

	protected abstract T unknown(String text);
}
