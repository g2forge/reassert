package com.g2forge.reassert.contract.analyze;

import java.util.function.BinaryOperator;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.eee.evaluate.bool.BooleanOperator;
import com.g2forge.reassert.contract.eee.evaluate.bool.IBooleanOperatorDescriptor;
import com.g2forge.reassert.contract.eee.evaluate.bool.IBooleanSystem;
import com.g2forge.reassert.core.model.contract.TermRelation;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

public class TermRelationBooleanSystem implements IBooleanSystem<TermRelation>, ISingleton {
	@Getter
	@RequiredArgsConstructor
	protected static enum BooleanOperatorDescriptor implements IBooleanOperatorDescriptor<TermRelation> {
		AND("&&", (tr0, tr1) -> {
			if ((tr0 == TermRelation.Excluded) || (tr1 == TermRelation.Excluded)) return TermRelation.Excluded;
			return ((tr0 == TermRelation.Included) && (tr1 == TermRelation.Included)) ? TermRelation.Included : TermRelation.Unspecified;
		}, IFunction1.identity(), TermRelation.Included, TermRelation.Excluded),
		OR("||", (tr0, tr1) -> {
			if ((tr0 == TermRelation.Included) || (tr1 == TermRelation.Included)) return TermRelation.Included;
			return ((tr0 == TermRelation.Excluded) && (tr1 == TermRelation.Excluded)) ? TermRelation.Excluded : TermRelation.Unspecified;
		}, IFunction1.identity(), TermRelation.Excluded, TermRelation.Included),
		NOT("!", null, tr -> {
			if (tr == TermRelation.Unspecified) return TermRelation.Unspecified;
			return tr == TermRelation.Included ? TermRelation.Excluded : TermRelation.Included;
		}, null, null) {
			@Override
			public boolean isValid(int nArguments) {
				return nArguments == 1;
			}
		};

		protected final String name;

		protected final BinaryOperator<TermRelation> combiner;

		protected final IFunction1<TermRelation, TermRelation> summarizer;

		protected final TermRelation identity;

		protected final TermRelation zero;

		public boolean isValid(int nArguments) {
			return nArguments >= 1;
		}
	}

	protected static final TermRelationBooleanSystem INSTANCE = new TermRelationBooleanSystem();

	public static TermRelationBooleanSystem create() {
		return INSTANCE;
	}

	protected TermRelationBooleanSystem() {}

	@Override
	public IBooleanOperatorDescriptor<TermRelation> getDescriptor(BooleanOperator operator) {
		return BooleanOperatorDescriptor.valueOf(operator.name());
	}

	@Override
	public boolean isValid(TermRelation value) {
		return value != null;
	}
}
