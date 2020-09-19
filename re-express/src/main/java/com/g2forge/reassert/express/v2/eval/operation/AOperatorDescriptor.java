package com.g2forge.reassert.express.v2.eval.operation;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NonNullOptional;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public abstract class AOperatorDescriptor<Value> implements IOperatorDescriptor<Value> {
	protected final IOptional<? extends Value> zero;

	protected final IOptional<? extends Value> identity;

	public AOperatorDescriptor(Value zero, Value identity) {
		this.zero = NonNullOptional.ofNullable(zero);
		this.identity = NonNullOptional.ofNullable(identity);
	}
}
