package com.g2forge.reassert.license.v2;

import java.util.regex.Pattern;

public class PatternBuilder extends APatternBuilder<Pattern> {
	public PatternBuilder() {
		super(new StringBuilder());
	}

	public Pattern build() {
		return Pattern.compile(builder.toString(), Pattern.CASE_INSENSITIVE);
	}
}