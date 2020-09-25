package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.operation.IOperation.IOperator;

public class BooleanOperationSystem implements IOperationSystem<Boolean>, ISingleton {
	protected static class BooleanOperatorDescriptor extends AOperatorDescriptor<Boolean> {
		public BooleanOperatorDescriptor(Boolean zero, Boolean identity, IFunction1<? super Boolean, ? extends Boolean> summarizer) {
			super(new ArgumentDescriptor<>(zero, identity), summarizer);
		}

		@Override
		public Boolean combine(Boolean left, Boolean right) {
			throw new UnsupportedOperationException();
		}

		@Override
		public IValidation validate(IOperation<?, Boolean> operation) {
			return operation.getOperator().validate(operation.getArguments());
		}
	}

	private static final BooleanOperationSystem INSTANCE = new BooleanOperationSystem();

	public static BooleanOperationSystem create() {
		return INSTANCE;
	}

	protected BooleanOperationSystem() {}

	@Override
	public IOperatorDescriptor<Boolean> getDescriptor(IOperator operator) {
		if (!(operator instanceof BooleanOperation.Operator)) throw new UnsupportedOperationException("Boolean system only supports boolean operations!");

		final BooleanOperation.Operator cast = (BooleanOperation.Operator) operator;
		switch (cast) {
			case NOT:
				return new BooleanOperatorDescriptor(null, null, v -> !v);
			case OR:
				return new BooleanOperatorDescriptor(true, false, null) {
					@Override
					public Boolean combine(Boolean left, Boolean right) {
						return left || right;
					}
				};
			case AND:
				return new BooleanOperatorDescriptor(false, true, null) {
					@Override
					public Boolean combine(Boolean left, Boolean right) {
						return left || right;
					}
				};
			case XOR:
				return new BooleanOperatorDescriptor(null, false, null) {
					@Override
					public Boolean combine(Boolean left, Boolean right) {
						return left ^ right;
					}
				};
			case IMPLIES:
				return new IOperatorDescriptor<Boolean>() {
					@Override
					public Boolean combine(Boolean left, Boolean right) {
						return (!left) || right;
					}

					@Override
					public IArgumentDescriptor<Boolean> getArgument(int index) {
						switch (index) {
							case 0:
								return new ArgumentDescriptor<>(false, true, true);
							case 1:
								return new ArgumentDescriptor<>(true, null);
							default:
								throw new IllegalArgumentException(String.format("Implies supports exactly 2 arguments, cannot get descriptor for argument %1$d", index));
						}
					}

					@Override
					public IFunction1<? super Boolean, ? extends Boolean> getSummarizer() {
						return null;
					}

					@Override
					public IValidation validate(IOperation<?, Boolean> operation) {
						return operation.getOperator().validate(operation.getArguments());
					}
				};
			default:
				throw new EnumException(BooleanOperation.Operator.class, cast);
		}
	}

	@Override
	public IOperatorRendering getRendering(IOperator operator) {
		if (!(operator instanceof BooleanOperation.Operator)) throw new UnsupportedOperationException("Boolean system only supports boolean operations!");

		final BooleanOperation.Operator cast = (BooleanOperation.Operator) operator;
		switch (cast) {
			case IMPLIES:
				return new EnumOperatorRendering<>(cast, "implied by");
			default:
				return new EnumOperatorRendering<>(cast, cast.name().toLowerCase() + "-ed with");
		}
	}
}
