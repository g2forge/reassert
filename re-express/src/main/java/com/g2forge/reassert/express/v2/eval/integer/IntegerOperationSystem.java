package com.g2forge.reassert.express.v2.eval.integer;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.v2.eval.operation.AOperatorDescriptor;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.v2.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation.IOperator;

public class IntegerOperationSystem implements IOperationSystem<Integer>, ISingleton {
	protected static class ArithmeticOperatorDescriptor extends AOperatorDescriptor<Integer> {
		public ArithmeticOperatorDescriptor(Integer zero, Integer identity, IFunction1<? super Integer, ? extends Integer> summarizer) {
			super(zero, identity, summarizer);
		}

		@Override
		public Integer combine(Integer left, Integer right) {
			throw new UnsupportedOperationException();
		}

		@Override
		public IValidation validate(IOperation<?, Integer> operation) {
			return operation.getOperator().validate(operation.getArguments());
		}
	}

	private static final IntegerOperationSystem INSTANCE = new IntegerOperationSystem();

	public static IntegerOperationSystem create() {
		return INSTANCE;
	}

	protected IntegerOperationSystem() {}

	@Override
	public IOperatorDescriptor<Integer> getDescriptor(IOperator operator) {
		if (!(operator instanceof ArithmeticOperation.Operator)) throw new UnsupportedOperationException("Arithmetic system only supports arithmetic operations!");

		final ArithmeticOperation.Operator cast = (ArithmeticOperation.Operator) operator;
		switch (cast) {
			case Add:
				return new ArithmeticOperatorDescriptor(null, 0, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left + right;
					}
				};
			case Subtract:
				return new ArithmeticOperatorDescriptor(null, null, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left - right;
					}
				};
			case Multiply:
				return new ArithmeticOperatorDescriptor(0, 1, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left * right;
					}
				};
			case Divide:
				return new ArithmeticOperatorDescriptor(null, null, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left / right;
					}
				};
			default:
				throw new EnumException(ArithmeticOperation.Operator.class, cast);
		}
	}
}
