package com.g2forge.reassert.term.eee.express;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Literal<T> implements IConstant<T> {
	protected final String name;

	protected final T value;

	public Literal(T value) {
		this(null, value);
	}

	@Override
	public T get() {
		return getValue();
	}
}
