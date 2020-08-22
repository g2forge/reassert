package com.g2forge.reassert.license.v2;

import java.util.regex.Pattern;

import com.g2forge.alexandria.java.function.IConsumer1;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
public abstract class APatternBuilder<T> implements IPatternBuilder<T> {
	protected final StringBuilder builder;

	protected final boolean gap;

	protected boolean current = true;

	@Override
	public IPatternBuilder<T> alt(@SuppressWarnings("unchecked") IConsumer1<? super IPatternBuilder<?>>... alternatives) {
		final StringBuilder builder = getBuilder();
		builder.append('(');
		boolean first = true;
		for (IConsumer1<? super IPatternBuilder<?>> alternative : alternatives) {
			if (first) first = false;
			else builder.append('|');
			builder.append('(');
			alternative.accept(this);
			builder.append(')');
		}
		builder.append(')');
		return this;
	}

	protected void assertCurrent() {
		if (!isCurrent()) throw new IllegalStateException();
	}

	@Override
	public IPatternBuilder<IPatternBuilder<T>> child(boolean required, boolean gap) {
		assertCurrent();

		final StringBuilder builder = getBuilder();
		builder.append('(');
		current = false;
		final APatternBuilder<T> retVal = this;
		return new APatternBuilder<IPatternBuilder<T>>(builder, gap) {
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
		if (isGap() && !isEmpty()) builder.append("[-_\\p{Space}]*");
		return builder;
	}

	protected boolean isEmpty() {
		return builder.length() <= 0;
	}

	@Override
	public IPatternBuilder<IPatternBuilder<T>> optional() {
		return child(false, isGap());
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
		final String separator = "[-.]";
		if (minor == 0) builder.append('(').append(separator).append("0)?");
		else builder.append(separator).append(minor);
		builder.append("\\.?");
		return this;
	}
}