package com.g2forge.reassert.express.v2.model.constant;

import com.g2forge.reassert.express.v2.model.IExpression;

public class NoValueConstant<Name, Value> implements IConstant<Name, Value> {
	@Override
	public boolean isSame(IExpression<?, ?> that) {
		throw new UnsupportedOperationException();
	}
}