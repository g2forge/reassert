package com.g2forge.reassert.license.v2;

import java.util.regex.Pattern;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
public abstract class APatternBuilder<T> implements IPatternBuilder<T> {
	protected final StringBuilder builder;

	protected boolean usable = true;

	@Override
	public IPatternBuilder<IPatternBuilder<T>> optional() {
		if (!isUsable()) throw new IllegalStateException();

		final StringBuilder builder = getBuilder();
		builder.append('(');
		usable = false;
		final APatternBuilder<T> retVal = this;
		return new APatternBuilder<IPatternBuilder<T>>(builder) {
			@Override
			public IPatternBuilder<T> build() {
				retVal.usable = true;
				builder.append(")?");
				return retVal;
			}
		};
	}

	@Override
	public IPatternBuilder<T> separator(boolean required) {
		if (!isUsable()) throw new IllegalStateException();
		getBuilder().append("[^\\p{Alnum}]").append(required ? '+' : '*');
		return this;
	}

	@Override
	public IPatternBuilder<T> text(String text) {
		if (!isUsable()) throw new IllegalStateException();
		getBuilder().append(Pattern.quote(text));
		return this;
	}

	@Override
	public IPatternBuilder<T> version(int major, int minor) {
		if (!isUsable()) throw new IllegalStateException();
		final StringBuilder builder = getBuilder();
		builder.append(major);
		if (minor == 0) builder.append("(\\.0)?");
		else builder.append("\\.").append(minor);
		return this;
	}
}