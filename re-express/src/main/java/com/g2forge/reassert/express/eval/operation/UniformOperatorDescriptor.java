package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.function.IFunction1;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class UniformOperatorDescriptor<Value> implements IOperatorDescriptor<Value> {
	protected final IArgumentDescriptor<Value> argument;

	protected final IFunction1<? super Value, ? extends Value> summarizer;

	public UniformOperatorDescriptor(Value zero, Value identity, IFunction1<? super Value, ? extends Value> summarizer) {
		this(new ArgumentDescriptor<>(zero, identity), summarizer);
	}

	@Override
	public Value combine(Value left, Value right) {
		throw new UnsupportedOperationException();
	}

	@Override
	public IArgumentDescriptor<Value> getArgument(int index) {
		return getArgument();
	}
}
