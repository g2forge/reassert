package com.g2forge.reassert.license.v2;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.alexandria.java.function.builder.IBuilder;

public interface IPatternBuilder<T> extends IBuilder<T> {
	public IPatternBuilder<IPatternBuilder<T>> child(boolean required, boolean gap);

	public IPatternBuilder<IPatternBuilder<T>> optional();

	public IPatternBuilder<T> text(String text);

	public IPatternBuilder<T> version(int major, int minor);

	public default IPatternBuilder<T> with(IConsumer1<? super IPatternBuilder<?>> wither) {
		wither.accept(this);
		return this;
	}
}