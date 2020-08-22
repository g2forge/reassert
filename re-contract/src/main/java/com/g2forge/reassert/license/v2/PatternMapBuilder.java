package com.g2forge.reassert.license.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.regex.Pattern;

import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.license.StandardLicense;

public class PatternMapBuilder implements IBuilder<Map<StandardLicense, List<Pattern>>> {
	protected final Map<StandardLicense, List<Pattern>> retVal = new HashMap<>();

	@Override
	public Map<StandardLicense, List<Pattern>> build() {
		return retVal;
	}

	public APatternBuilder<PatternMapBuilder> license(StandardLicense license) {
		return new APatternBuilder<PatternMapBuilder>(new StringBuilder(), true) {
			@Override
			public PatternMapBuilder build() {
				retVal.computeIfAbsent(license, (Function<? super StandardLicense, ? extends List<Pattern>>) l -> new ArrayList<>()).add(Pattern.compile(getBuilder().toString(), Pattern.CASE_INSENSITIVE));
				return PatternMapBuilder.this;
			}
		};
	}
}