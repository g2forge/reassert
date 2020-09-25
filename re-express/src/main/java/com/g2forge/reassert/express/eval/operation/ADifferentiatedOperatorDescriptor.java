package com.g2forge.reassert.express.eval.operation;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction1;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
public abstract class ADifferentiatedOperatorDescriptor<Value> implements IOperatorDescriptor<Value> {
	protected final List<IArgumentDescriptor<Value>> arguments;

	@SafeVarargs
	public ADifferentiatedOperatorDescriptor(IArgumentDescriptor<Value>... arguments) {
		this(HCollection.asList(arguments));
	}

	@Override
	public IArgumentDescriptor<Value> getArgument(int index) {
		return getArguments().get(index);
	}

	@Override
	public IFunction1<? super Value, ? extends Value> getSummarizer() {
		return null;
	}
}
