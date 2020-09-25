package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NonNullOptional;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ArgumentDescriptor<Value> implements IArgumentDescriptor<Value> {
	protected final IOptional<? extends Value> zeroInput;

	protected final IOptional<? extends Value> zeroOutput;

	protected final IOptional<? extends Value> identity;

	public ArgumentDescriptor(Value zero, Value identity) {
		this.zeroInput = NonNullOptional.ofNullable(zero);
		this.zeroOutput = this.zeroInput;
		this.identity = NonNullOptional.ofNullable(identity);
	}

	public ArgumentDescriptor(Value zeroInput, Value zeroOutput, Value identity) {
		this(NonNullOptional.ofNullable(zeroInput), NonNullOptional.ofNullable(zeroOutput), NonNullOptional.ofNullable(identity));
	}
}
