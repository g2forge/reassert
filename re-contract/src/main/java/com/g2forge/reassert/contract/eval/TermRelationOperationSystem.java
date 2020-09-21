package com.g2forge.reassert.contract.eval;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.eval.operation.AOperatorDescriptor;
import com.g2forge.reassert.express.v2.eval.operation.EnumOperatorRendering;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorRendering;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation.IOperator;

public class TermRelationOperationSystem implements IOperationSystem<TermRelation>, ISingleton {
	protected static class TermRelationOperatorDescriptor extends AOperatorDescriptor<TermRelation> {
		public TermRelationOperatorDescriptor(TermRelation zero, TermRelation identity, IFunction1<? super TermRelation, ? extends TermRelation> summarizer) {
			super(zero, identity, summarizer);
		}

		@Override
		public TermRelation combine(TermRelation left, TermRelation right) {
			throw new UnsupportedOperationException();
		}

		@Override
		public IValidation validate(IOperation<?, TermRelation> operation) {
			return operation.getOperator().validate(operation.getArguments());
		}
	}

	protected static final TermRelationOperationSystem INSTANCE = new TermRelationOperationSystem();

	public static TermRelationOperationSystem create() {
		return INSTANCE;
	}

	protected TermRelationOperationSystem() {}

	@Override
	public IOperatorDescriptor<TermRelation> getDescriptor(IOperator operator) {
		if (!(operator instanceof BooleanOperation.Operator)) throw new UnsupportedOperationException("Term relation operation system only supports boolean operations!");

		final BooleanOperation.Operator cast = (BooleanOperation.Operator) operator;
		switch (cast) {
			case NOT:
				return new TermRelationOperatorDescriptor(null, null, v -> {
					if (v == TermRelation.Unspecified) return TermRelation.Unspecified;
					return v == TermRelation.Included ? TermRelation.Excluded : TermRelation.Included;
				});
			case OR:
				return new TermRelationOperatorDescriptor(TermRelation.Included, TermRelation.Excluded, null) {
					@Override
					public TermRelation combine(TermRelation left, TermRelation right) {
						if ((left == TermRelation.Included) || (right == TermRelation.Included)) return TermRelation.Included;
						return ((left == TermRelation.Excluded) && (right == TermRelation.Excluded)) ? TermRelation.Excluded : TermRelation.Unspecified;
					}
				};
			case AND:
				return new TermRelationOperatorDescriptor(TermRelation.Excluded, TermRelation.Included, null) {
					@Override
					public TermRelation combine(TermRelation left, TermRelation right) {
						if ((left == TermRelation.Excluded) || (right == TermRelation.Excluded)) return TermRelation.Excluded;
						return ((left == TermRelation.Included) && (right == TermRelation.Included)) ? TermRelation.Included : TermRelation.Unspecified;
					}
				};
			case XOR:
				return new TermRelationOperatorDescriptor(null, TermRelation.Excluded, null) {
					@Override
					public TermRelation combine(TermRelation left, TermRelation right) {
						if ((left == TermRelation.Unspecified) || (right == TermRelation.Unspecified)) return TermRelation.Unspecified;
						return ((left == TermRelation.Included) ^ (right == TermRelation.Included)) ? TermRelation.Included : TermRelation.Excluded;
					}
				};
			default:
				throw new EnumException(BooleanOperation.Operator.class, cast);
		}
	}

	@Override
	public IOperatorRendering getRendering(IOperator operator) {
		if (!(operator instanceof BooleanOperation.Operator)) throw new UnsupportedOperationException("Term relation operation system only supports boolean operations!");

		final BooleanOperation.Operator cast = (BooleanOperation.Operator) operator;
		return new EnumOperatorRendering<>(cast, cast.name().toLowerCase() + "-ed with");
	}
}
