package com.g2forge.reassert.license.v2;

import java.util.regex.Pattern;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
public abstract class APatternBuilder<T> implements IPatternBuilder<T> {
	protected final StringBuilder builder;

	protected boolean current = true;

	protected void assertCurrent() {
		if (!isCurrent()) throw new IllegalStateException();
	}

	@Override
	public IPatternBuilder<IPatternBuilder<T>> child(boolean required) {
		assertCurrent();

		final StringBuilder builder = gap();
		builder.append('(');
		current = false;
		final APatternBuilder<T> retVal = this;
		return new APatternBuilder<IPatternBuilder<T>>(builder) {
			@Override
			public IPatternBuilder<T> build() {
				retVal.current = true;
				builder.append(')');
				if (!required) builder.append('?');
				return retVal;
			}
		};
	}

	protected StringBuilder gap() {
		final StringBuilder builder = getBuilder();
		if (!isEmpty()) builder.append("[-_\\p{Space}]*");
		return builder;
	}

	protected boolean isEmpty() {
		return builder.length() <= 0;
	}

	@Override
	public IPatternBuilder<T> text(String text) {
		assertCurrent();

		gap().append(Pattern.quote(text));
		return this;
	}

	@Override
	public IPatternBuilder<T> version(int major, int minor) {
		assertCurrent();

		final StringBuilder builder = gap();
		builder.append("v?");
		builder.append(major);
		if (minor == 0) builder.append("(\\.0)?");
		else builder.append("\\.").append(minor);
		return this;
	}
}