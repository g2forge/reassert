package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NonNullOptional;
import com.g2forge.alexandria.java.function.IFunction1;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public abstract class AOperatorDescriptor<Value> implements IOperatorDescriptor<Value> {
	protected final IOptional<? extends Value> zero;

	protected final IOptional<? extends Value> identity;

	protected final IFunction1<? super Value, ? extends Value> summarizer;

	public AOperatorDescriptor(Value zero, Value identity, IFunction1<? super Value, ? extends Value> summarizer) {
		this.zero = NonNullOptional.ofNullable(zero);
		this.identity = NonNullOptional.ofNullable(identity);
		this.summarizer = summarizer;
	}
}
