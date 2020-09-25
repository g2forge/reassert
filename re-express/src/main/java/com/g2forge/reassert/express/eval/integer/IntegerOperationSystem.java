package com.g2forge.reassert.express.eval.integer;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.eval.operation.AOperatorDescriptor;
import com.g2forge.reassert.express.eval.operation.ArgumentDescriptor;
import com.g2forge.reassert.express.eval.operation.EnumOperatorRendering;
import com.g2forge.reassert.express.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.eval.operation.IOperatorRendering;
import com.g2forge.reassert.express.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.operation.IOperation.IOperator;

public class IntegerOperationSystem implements IOperationSystem<Integer>, ISingleton {
	protected static class ArithmeticOperatorDescriptor extends AOperatorDescriptor<Integer> {
		public ArithmeticOperatorDescriptor(Integer zero, Integer identity, IFunction1<? super Integer, ? extends Integer> summarizer) {
			super(new ArgumentDescriptor<>(zero, identity), summarizer);
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
			case ADD:
				return new ArithmeticOperatorDescriptor(null, 0, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left + right;
					}
				};
			case SUBTRACT:
				return new ArithmeticOperatorDescriptor(null, null, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left - right;
					}
				};
			case MULTIPLY:
				return new ArithmeticOperatorDescriptor(0, 1, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left * right;
					}
				};
			case DIVIDE:
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

	@Override
	public IOperatorRendering getRendering(IOperator operator) {
		if (!(operator instanceof ArithmeticOperation.Operator)) throw new UnsupportedOperationException("Arithmetic system only supports arithmetic operations!");

		final ArithmeticOperation.Operator cast = (ArithmeticOperation.Operator) operator;
		switch (cast) {
			case ADD:
				return new EnumOperatorRendering<>(cast, "added to");
			case SUBTRACT:
				return new EnumOperatorRendering<>(cast, "subtracted from");
			case MULTIPLY:
				return new EnumOperatorRendering<>(cast, "multiplied by");
			case DIVIDE:
				return new EnumOperatorRendering<>(cast, "divided by");
			default:
				throw new EnumException(ArithmeticOperation.Operator.class, cast);
		}
	}
}
