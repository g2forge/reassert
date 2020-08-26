package com.g2forge.reassert.contract.license.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import com.g2forge.alexandria.java.function.IPredicate1;
import com.g2forge.alexandria.java.function.builder.IBuilder;

public class PatternMapBuilder<L> implements IBuilder<Map<L, List<IPredicate1<String>>>> {
	protected final Map<L, List<IPredicate1<String>>> retVal = new HashMap<>();

	@Override
	public Map<L, List<IPredicate1<String>>> build() {
		return retVal;
	}

	protected List<IPredicate1<String>> getList(L license) {
		return retVal.computeIfAbsent(license, l -> new ArrayList<>());
	}

	public APatternBuilder<PatternMapBuilder<L>> license(L license) {
		return new APatternBuilder<PatternMapBuilder<L>>(new StringBuilder(), true) {
			@Override
			public PatternMapBuilder<L> build() {
				final Pattern pattern = Pattern.compile(getBuilder().toString(), Pattern.CASE_INSENSITIVE);
				getList(license).add(string -> pattern.matcher(string.trim()).matches());
				return PatternMapBuilder.this;
			}
		};
	}

	public PatternMapBuilder<L> license(L license, IPredicate1<String> predicate) {
		getList(license).add(predicate);
		return this;
	}
}