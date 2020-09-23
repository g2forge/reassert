package com.g2forge.reassert.contract.algorithm.usagepropagation.model.rule;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

public interface IUsagePropagationRules extends IFunction2<IEdge, IUsage, IUsage> {
	public static <Term extends IUsageTerm, Edge extends IEdge> IFunction2<Edge, IUsage, IUsage> with(final IConsumer1<? super UsagePropagationRuleBuilder<Term, Edge>> consumer) {
		final UsagePropagationRuleBuilder<Term, Edge> builder = new UsagePropagationRuleBuilder<Term, Edge>();
		consumer.accept(builder);
		return builder.build();
	}

	@Override
	public IUsage apply(IEdge edge, IUsage usage);
}
