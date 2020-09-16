package com.g2forge.reassert.standard.algorithm.propagate;

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
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.PropagatedUsage;
import com.g2forge.reassert.core.model.file.Contains;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

import lombok.AccessLevel;
import lombok.Getter;

@ReassertLegalOpinion
public class StandardUsagePropagation implements IUsagePropagation, ISingleton {
	private static final StandardUsagePropagation INSTANCE = new StandardUsagePropagation();

	public static StandardUsagePropagation create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction2<IEdge, IUsageApplied, IUsageApplied> function = computeFunction();

	protected StandardUsagePropagation() {}

	@Override
	public IUsageApplied apply(IEdge edge, IUsageApplied usage) {
		final IFunction2<IEdge, IUsageApplied, IUsageApplied> function = getFunction();
		final IUsageApplied result = function.apply(edge, usage);
		if (result.equals(usage)) return usage;
		return result;
	}

	protected <E extends IEdge> IUsageApplied apply(Map<StandardUsageTerm, IFunction2<E, IUsageApplied, TermRelation>> usageTermMap, E edge, IUsageApplied usage) {
		final Terms.TermsBuilder<IUsageTerm> termsBuilder = Terms.builder();
		for (StandardUsageTerm term : StandardUsageTerm.values()) {
			final IFunction2<E, IUsageApplied, TermRelation> function = usageTermMap.get(term);
			final TermRelation relation = function.apply(edge, usage);
			termsBuilder.term(term, relation);
		}

		final PropagatedUsage retVal = new PropagatedUsage(edge, usage, termsBuilder.build());
		if (IUsage.isEqualTerms(usage, retVal)) return usage;
		return retVal;
	}

	protected IFunction2<IEdge, IUsageApplied, IUsageApplied> computeFunction() {
		final Map<StandardUsageTerm, IFunction2<Inherits, IUsageApplied, TermRelation>> inheritsUsageTermMap;
		{
			final UsageTermMapBuilder<Inherits> builder = new UsageTermMapBuilder<>();
			builder.copy(StandardUsageTerm.Commercial).copy(StandardUsageTerm.DistributionPublic).copy(StandardUsageTerm.DistributionPrivate).copy(StandardUsageTerm.DistributionService);
			builder.include(StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy).exclude(StandardUsageTerm.UseModified);
			builder.copy(StandardUsageTerm.DistributingBinary).copy(StandardUsageTerm.DistributingSource);
			inheritsUsageTermMap = builder.build();
		}

		final Map<StandardUsageTerm, IFunction2<Depends, IUsageApplied, TermRelation>> dependsUsageTermMap;
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

		final TypeSwitch2.FunctionBuilder<IEdge, IUsageApplied, IUsageApplied> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(Inherits.class, IUsage.class, (e, u) -> apply(inheritsUsageTermMap, e, u));
		builder.add(Contains.class, IUsage.class, (e, u) -> u);
		builder.add(Depends.class, IUsage.class, (e, u) -> apply(dependsUsageTermMap, e, u));
		return builder.build();
	}
}
