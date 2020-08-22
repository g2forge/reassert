package com.g2forge.reassert.license.v2;

import com.g2forge.alexandria.java.function.builder.IBuilder;

public interface IPatternBuilder<T> extends IBuilder<T> {
	public IPatternBuilder<IPatternBuilder<T>> optional();

	public IPatternBuilder<T> separator(boolean required);

	public IPatternBuilder<T> text(String text);

	public IPatternBuilder<T> version(int major, int minor);
}