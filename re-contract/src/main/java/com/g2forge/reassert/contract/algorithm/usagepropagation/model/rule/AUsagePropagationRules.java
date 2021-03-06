package com.g2forge.reassert.contract.algorithm.usagepropagation.model.rule;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class AUsagePropagationRules<Term extends IUsageTerm> implements IUsagePropagationRules {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction2<IEdge, IUsage, IUsage> function = computeFunction();

	@Override
	public IUsage apply(IEdge edge, IUsage usage) {
		final IFunction2<IEdge, IUsage, IUsage> function = getFunction();
		final IUsage result = function.apply(edge, usage);
		if (result.equals(usage)) return usage;
		return result;
	}

	protected abstract IFunction2<IEdge, IUsage, IUsage> computeFunction();

	protected <Edge extends IEdge> IFunction2<Edge, IUsage, IUsage> rule(final IConsumer1<? super IUsagePropagationRuleBuilder<Term, Edge>> consumer) {
		return IUsagePropagationRules.rule(consumer);
	}
}
