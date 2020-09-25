package com.g2forge.reassert.contract.eval;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.eval.operation.ABooleanOperationSystem;
import com.g2forge.reassert.express.eval.operation.ADifferentiatedOperatorDescriptor;
import com.g2forge.reassert.express.eval.operation.ArgumentDescriptor;
import com.g2forge.reassert.express.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.eval.operation.UniformOperatorDescriptor;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.BooleanOperation.Operator;

public class TermRelationOperationSystem extends ABooleanOperationSystem<TermRelation> implements ISingleton {
	protected static final TermRelationOperationSystem INSTANCE = new TermRelationOperationSystem();

	public static TermRelationOperationSystem create() {
		return INSTANCE;
	}

	protected TermRelationOperationSystem() {}

	@Override
	protected IOperatorDescriptor<TermRelation> computeDescriptor(Operator operator) {
		switch (operator) {
			case NOT:
				return new UniformOperatorDescriptor<TermRelation>(null, null, v -> {
					if (v == TermRelation.Unspecified) return TermRelation.Unspecified;
					return v == TermRelation.Included ? TermRelation.Excluded : TermRelation.Included;
				});
			case OR:
				return new UniformOperatorDescriptor<TermRelation>(TermRelation.Included, TermRelation.Excluded, null) {
					@Override
					public TermRelation combine(TermRelation left, TermRelation right) {
						if ((left == TermRelation.Included) || (right == TermRelation.Included)) return TermRelation.Included;
						return ((left == TermRelation.Excluded) && (right == TermRelation.Excluded)) ? TermRelation.Excluded : TermRelation.Unspecified;
					}
				};
			case AND:
				return new UniformOperatorDescriptor<TermRelation>(TermRelation.Excluded, TermRelation.Included, null) {
					@Override
					public TermRelation combine(TermRelation left, TermRelation right) {
						if ((left == TermRelation.Excluded) || (right == TermRelation.Excluded)) return TermRelation.Excluded;
						return ((left == TermRelation.Included) && (right == TermRelation.Included)) ? TermRelation.Included : TermRelation.Unspecified;
					}
				};
			case XOR:
				return new UniformOperatorDescriptor<TermRelation>(null, TermRelation.Excluded, null) {
					@Override
					public TermRelation combine(TermRelation left, TermRelation right) {
						if ((left == TermRelation.Unspecified) || (right == TermRelation.Unspecified)) return TermRelation.Unspecified;
						return ((left == TermRelation.Included) ^ (right == TermRelation.Included)) ? TermRelation.Included : TermRelation.Excluded;
					}
				};
			case IMPLIES:
				return new ADifferentiatedOperatorDescriptor<TermRelation>(new ArgumentDescriptor<>(TermRelation.Excluded, TermRelation.Included, TermRelation.Included), new ArgumentDescriptor<>(TermRelation.Included, null)) {
					@Override
					public TermRelation combine(TermRelation left, TermRelation right) {
						if ((left == TermRelation.Excluded) || (right == TermRelation.Included)) return TermRelation.Included;
						return ((left == TermRelation.Included) && (right == TermRelation.Excluded)) ? TermRelation.Excluded : TermRelation.Unspecified;
					}
				};
			default:
				throw new EnumException(BooleanOperation.Operator.class, operator);
		}
	}
}
