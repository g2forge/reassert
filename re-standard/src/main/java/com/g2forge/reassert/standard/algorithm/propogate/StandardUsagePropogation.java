package com.g2forge.reassert.standard.algorithm.propogate;

import static com.g2forge.reassert.contract.model.logic.HTermLogic.and;
import static com.g2forge.reassert.contract.model.logic.HTermLogic.context;
import static com.g2forge.reassert.contract.model.logic.HTermLogic.or;

import java.util.Map;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.artifact.Depends;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.PropagatedUsage;
import com.g2forge.reassert.core.model.file.Contains;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

import lombok.AccessLevel;
import lombok.Getter;

@ReassertLegalOpinion
public class StandardUsagePropogation implements IUsagePropogation, ISingleton {
	private static final StandardUsagePropogation INSTANCE = new StandardUsagePropogation();

	public static StandardUsagePropogation create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction2<IEdge, IUsage, IUsage> function = computeFunction();

	protected StandardUsagePropogation() {}

	@Override
	public IUsage apply(IEdge edge, IUsage usage) {
		final IFunction2<IEdge, IUsage, IUsage> function = getFunction();
		final IUsage result = function.apply(edge, usage);
		if (result.getTerms().equals(usage.getTerms())) return usage;
		return result;
	}

	protected <E extends IEdge> IUsage apply(Map<StandardUsageTerm, IFunction2<E, IUsage, TermRelation>> usageTermMap, E edge, IUsage usage) {
		final Terms.TermsBuilder<IUsageTerm> termsBuilder = Terms.builder();
		for (StandardUsageTerm term : StandardUsageTerm.values()) {
			final IFunction2<E, IUsage, TermRelation> function = usageTermMap.get(term);
			final TermRelation relation = function.apply(edge, usage);
			termsBuilder.term(term, relation);
		}

		final Terms<IUsageTerm> terms = termsBuilder.build();
		if (usage.getTerms().equals(terms)) return usage;
		return new PropagatedUsage(edge, usage, terms);
	}

	protected IFunction2<IEdge, IUsage, IUsage> computeFunction() {
		final Map<StandardUsageTerm, IFunction2<Inherits, IUsage, TermRelation>> inheritsUsageTermMap;
		{
			final UsageTermMapBuilder<Inherits> builder = new UsageTermMapBuilder<>();
			builder.copy(StandardUsageTerm.Commercial).copy(StandardUsageTerm.DistributionPublic).copy(StandardUsageTerm.DistributionPrivate).copy(StandardUsageTerm.DistributionService);
			builder.include(StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy).exclude(StandardUsageTerm.UseModified);
			builder.copy(StandardUsageTerm.DistributingBinary).copy(StandardUsageTerm.DistributingSource);
			inheritsUsageTermMap = builder.build();
		}

		final Map<StandardUsageTerm, IFunction2<Depends, IUsage, TermRelation>> dependsUsageTermMap;
		{
			final UsageTermMapBuilder<Depends> builder = new UsageTermMapBuilder<>();
			builder.copy(StandardUsageTerm.Commercial);

			builder.compute(StandardUsageTerm.DistributionPublic, and(StandardUsageTerm.DistributionPublic, context(Depends::isTransitive, TermRelation::valueOf), context(Depends::isRuntime, TermRelation::valueOf)));
			builder.compute(StandardUsageTerm.DistributionPrivate, or(and(StandardUsageTerm.DistributionPrivate, context(Depends::isTransitive, TermRelation::valueOf), context(Depends::isRuntime, TermRelation::valueOf)), and(or(StandardUsageTerm.DistributionPublic, StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionService), or(context(Depends::isCompiletime, TermRelation::valueOf), context(Depends::isTesttime, TermRelation::valueOf)))));
			builder.compute(StandardUsageTerm.DistributionService, and(StandardUsageTerm.DistributionService, context(Depends::isTransitive, TermRelation::valueOf), context(Depends::isRuntime, TermRelation::valueOf)));

			builder.include(StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy).exclude(StandardUsageTerm.UseModified);
			builder.copy(StandardUsageTerm.DistributingBinary).copy(StandardUsageTerm.DistributingSource);

			dependsUsageTermMap = builder.build();
		}

		final TypeSwitch2.FunctionBuilder<IEdge, IUsage, IUsage> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(Inherits.class, IUsage.class, (e, u) -> apply(inheritsUsageTermMap, e, u));
		builder.add(Contains.class, IUsage.class, (e, u) -> u);
		builder.add(Depends.class, IUsage.class, (e, u) -> apply(dependsUsageTermMap, e, u));
		return builder.build();
	}
}
