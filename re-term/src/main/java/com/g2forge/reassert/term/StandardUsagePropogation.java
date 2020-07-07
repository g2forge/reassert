package com.g2forge.reassert.term;

import static com.g2forge.reassert.term.analyze.model.logic.HTermLogic.and;
import static com.g2forge.reassert.term.analyze.model.logic.HTermLogic.context;
import static com.g2forge.reassert.term.analyze.model.logic.HTermLogic.or;

import java.util.Map;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.artifact.Depends;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.contract.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.Usage;
import com.g2forge.reassert.core.model.file.Contains;
import com.g2forge.reassert.term.propogate.IUsagePropogation;
import com.g2forge.reassert.term.propogate.UsageTermMapBuilder;

public class StandardUsagePropogation implements IUsagePropogation, ISingleton {
	private static final StandardUsagePropogation INSTANCE = new StandardUsagePropogation();

	public static StandardUsagePropogation create() {
		return INSTANCE;
	}

	protected StandardUsagePropogation() {}

	@Override
	public IUsage apply(IEdge edge, IUsage usage) {
		final Map<StandardUsageTerm, IFunction2<Depends, IUsage, TermRelation>> dependsUsageTermMap;
		{
			final UsageTermMapBuilder builder = new UsageTermMapBuilder();
			builder.copy(StandardUsageTerm.Commercial);

			builder.compute(StandardUsageTerm.DistributionPublic, and(StandardUsageTerm.DistributionPublic, context(Depends::isTransitive, TermRelation::valueOf), context(Depends::isRuntime, TermRelation::valueOf)));
			builder.compute(StandardUsageTerm.DistributionPrivate, or(and(StandardUsageTerm.DistributionPrivate, context(Depends::isTransitive, TermRelation::valueOf), context(Depends::isRuntime, TermRelation::valueOf)), and(or(StandardUsageTerm.DistributionPublic, StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionService), or(context(Depends::isCompiletime, TermRelation::valueOf), context(Depends::isTesttime, TermRelation::valueOf)))));
			builder.compute(StandardUsageTerm.DistributionService, and(StandardUsageTerm.DistributionService, context(Depends::isTransitive, TermRelation::valueOf), context(Depends::isRuntime, TermRelation::valueOf)));

			builder.include(StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy).exclude(StandardUsageTerm.UseModified);
			builder.copy(StandardUsageTerm.DistributingBinary).copy(StandardUsageTerm.DistributingSource);

			dependsUsageTermMap = builder.build();
		}

		final TypeSwitch2.FunctionBuilder<IEdge, IUsage, IUsage> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(Inherits.class, IUsage.class, (e, u) -> null);
		builder.add(Contains.class, IUsage.class, (e, u) -> u);
		builder.add(Depends.class, IUsage.class, createFunction(dependsUsageTermMap));
		return builder.build().apply(edge, usage);
	}

	protected <E extends IEdge> IFunction2<E, IUsage, IUsage> createFunction(Map<StandardUsageTerm, IFunction2<E, IUsage, TermRelation>> usageTermMap) {
		return (e, u) -> {
			final Terms.TermsBuilder<IUsageTerm> tBuilder = Terms.builder();
			for (StandardUsageTerm term : StandardUsageTerm.values()) {
				final IFunction2<E, IUsage, TermRelation> function = usageTermMap.get(term);
				final TermRelation relation = function.apply(e, u);
				tBuilder.term(term, relation);
			}

			final Terms<IUsageTerm> terms = tBuilder.build();
			if (u.getTerms().equals(terms)) return u;
			return new Usage(e.toString() + " " + u.getName(), terms);
		};
	}
}
