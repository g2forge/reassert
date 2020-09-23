package com.g2forge.reassert.standard.algorithm;

import static com.g2forge.reassert.express.model.operation.BooleanOperation.and;
import static com.g2forge.reassert.express.model.operation.BooleanOperation.or;

import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.contract.algorithm.usagepropagation.AUsagePropagation;
import com.g2forge.reassert.contract.algorithm.usagepropagation.model.name.IUsagePropagationName;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.artifact.Depends;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.file.Contains;
import com.g2forge.reassert.express.model.variable.IVariable;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

@ReassertLegalOpinion
public class StandardUsagePropagation extends AUsagePropagation<StandardUsageTerm> {
	private static final StandardUsagePropagation INSTANCE = new StandardUsagePropagation();

	public static StandardUsagePropagation create() {
		return INSTANCE;
	}

	protected IFunction2<IEdge, IUsage, IUsage> computeFunction() {
		final TypeSwitch2.FunctionBuilder<IEdge, IUsage, IUsage> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(Inherits.class, IUsage.class, with(b -> {
			b.copy(StandardUsageTerm.Commercial).copy(StandardUsageTerm.DistributionPublic).copy(StandardUsageTerm.DistributionPrivate).copy(StandardUsageTerm.DistributionService);
			b.include(StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy).exclude(StandardUsageTerm.UseModified);
			b.copy(StandardUsageTerm.DistributingBinary).copy(StandardUsageTerm.DistributingSource);
		}));
		builder.add(Contains.class, IUsage.class, (e, u) -> u);
		builder.add(Depends.class, IUsage.class, with(b -> {
			b.copy(StandardUsageTerm.Commercial);

			final IVariable<IUsagePropagationName<StandardUsageTerm, Depends>, TermRelation> transitive = b.of(Depends::isTransitive, TermRelation::valueOf);
			final IVariable<IUsagePropagationName<StandardUsageTerm, Depends>, TermRelation> runtime = b.of(Depends::isRuntime, TermRelation::valueOf);
			final IVariable<IUsagePropagationName<StandardUsageTerm, Depends>, TermRelation> testtime = b.of(Depends::isTesttime, TermRelation::valueOf);
			final IVariable<IUsagePropagationName<StandardUsageTerm, Depends>, TermRelation> compiletime = b.of(Depends::isCompiletime, TermRelation::valueOf);

			b.compute(StandardUsageTerm.DistributionPublic, and(b.of(StandardUsageTerm.DistributionPublic), transitive, runtime));
			b.compute(StandardUsageTerm.DistributionPrivate, or(and(b.of(StandardUsageTerm.DistributionPrivate), transitive, runtime), and(or(b.of(StandardUsageTerm.DistributionPublic), b.of(StandardUsageTerm.DistributionPrivate), b.of(StandardUsageTerm.DistributionService)), or(compiletime, testtime))));
			b.compute(StandardUsageTerm.DistributionService, and(b.of(StandardUsageTerm.DistributionService), transitive, runtime));

			b.include(StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy).exclude(StandardUsageTerm.UseModified);
			b.copy(StandardUsageTerm.DistributingBinary).copy(StandardUsageTerm.DistributingSource);
		}));
		return builder.build();
	}
}
