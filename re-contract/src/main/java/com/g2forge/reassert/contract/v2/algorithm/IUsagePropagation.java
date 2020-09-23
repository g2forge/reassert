package com.g2forge.reassert.contract.v2.algorithm;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.contract.v2.model.usagepropagation.UsagePropagationBuilder;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

public interface IUsagePropagation extends IFunction2<IEdge, IUsage, IUsage> {
	public static <Term extends IUsageTerm, Edge extends IEdge> IFunction2<Edge, IUsage, IUsage> with(final IConsumer1<? super UsagePropagationBuilder<Term, Edge>> consumer) {
		return new UsagePropagationBuilder<Term, Edge>().with(consumer).build();
	}

	@Override
	public IUsage apply(IEdge edge, IUsage usage);
}
