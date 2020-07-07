package com.g2forge.reassert.term.eee.evaluate.bool;

import java.util.function.BinaryOperator;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

public class BooleanBooleanSystem implements IBooleanSystem<Boolean>, ISingleton {
	@Getter
	@RequiredArgsConstructor
	protected static enum BooleanOperatorDescriptor implements IBooleanOperatorDescriptor<Boolean> {
		AND("&&", Boolean::logicalAnd, IFunction1.identity(), true, false),
		OR("||", Boolean::logicalOr, IFunction1.identity(), false, true),
		NOT("!", null, b -> !b, null, null) {
			@Override
			public boolean isValid(int nArguments) {
				return nArguments == 1;
			}
		};

		protected final String name;

		protected final BinaryOperator<Boolean> combiner;

		protected final IFunction1<Boolean, Boolean> summarizer;

		protected final Boolean identity;

		protected final Boolean zero;

		public boolean isValid(int nArguments) {
			return nArguments >= 1;
		}
	}

	protected static final BooleanBooleanSystem INSTANCE = new BooleanBooleanSystem();

	public static BooleanBooleanSystem create() {
		return INSTANCE;
	}

	protected BooleanBooleanSystem() {}

	@Override
	public IBooleanOperatorDescriptor<Boolean> getDescriptor(BooleanOperator operator) {
		return BooleanOperatorDescriptor.valueOf(operator.name());
	}

	@Override
	public boolean isValid(Boolean value) {
		return value != null;
	}
}
