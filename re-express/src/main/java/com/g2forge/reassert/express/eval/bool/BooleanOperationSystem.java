package com.g2forge.reassert.express.eval.bool;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.eval.operation.AOperatorDescriptor;
import com.g2forge.reassert.express.eval.operation.EnumOperatorRendering;
import com.g2forge.reassert.express.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.eval.operation.IOperatorRendering;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.operation.IOperation.IOperator;

public class BooleanOperationSystem implements IOperationSystem<Boolean>, ISingleton {
	protected static class BooleanOperatorDescriptor extends AOperatorDescriptor<Boolean> {
		public BooleanOperatorDescriptor(Boolean zero, Boolean identity, IFunction1<? super Boolean, ? extends Boolean> summarizer) {
			super(zero, identity, summarizer);
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
			default:
				throw new EnumException(BooleanOperation.Operator.class, cast);
		}
	}

	@Override
	public IOperatorRendering getRendering(IOperator operator) {
		if (!(operator instanceof BooleanOperation.Operator)) throw new UnsupportedOperationException("Boolean system only supports boolean operations!");

		final BooleanOperation.Operator cast = (BooleanOperation.Operator) operator;
		return new EnumOperatorRendering<>(cast, cast.name().toLowerCase() + "-ed with");
	}
}