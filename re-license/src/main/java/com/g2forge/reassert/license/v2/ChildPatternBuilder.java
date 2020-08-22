package com.g2forge.reassert.license.v2;

public class ChildPatternBuilder<T> extends APatternBuilder<T> {
	protected final T parent;

	public ChildPatternBuilder(StringBuilder builder, T parent) {
		super(builder);
		this.parent = parent;
	}

	public T build() {
		return parent;
	}
}