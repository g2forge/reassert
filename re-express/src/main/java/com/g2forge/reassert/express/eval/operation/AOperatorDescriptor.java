package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.function.IFunction1;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public abstract class AOperatorDescriptor<Value> implements IOperatorDescriptor<Value> {
	protected final IArgumentDescriptor<Value> argument;
	
	protected final IFunction1<? super Value, ? extends Value> summarizer;
	
	@Override
	public IArgumentDescriptor<Value> getArgument(int index) {
		return getArgument();
	}
}
