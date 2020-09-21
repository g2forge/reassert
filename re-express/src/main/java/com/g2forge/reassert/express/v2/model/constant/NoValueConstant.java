package com.g2forge.reassert.express.v2.model.constant;

import com.g2forge.alexandria.java.validate.IValidation;

public class NoValueConstant<Name, Value> implements IConstant<Name, Value> {
	protected static class NoValueConstantValidation implements IValidation {
		@Override
		public boolean isValid() {
			return false;
		}
	}

	@Override
	public IValidation validate() {
		return new NoValueConstantValidation();
	}
}