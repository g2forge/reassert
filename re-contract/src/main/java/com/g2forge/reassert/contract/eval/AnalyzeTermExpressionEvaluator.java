package com.g2forge.reassert.contract.eval;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IPredicate1;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.ILicenseUsageName;
import com.g2forge.reassert.contract.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.eval.AnalyzeInputsEvaluator;
import com.g2forge.reassert.express.eval.IEvaluator;
import com.g2forge.reassert.express.eval.ValueEvaluator;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.environment.Environment;
import com.g2forge.reassert.express.model.environment.IEnvironment;
import com.g2forge.reassert.express.model.variable.Closure;
import com.g2forge.reassert.express.model.variable.IVariable;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class AnalyzeTermExpressionEvaluator implements IEvaluator<ILicenseUsageName, TermRelation, ExpressionContextFinding> {
	protected static final IEvaluator<ILicenseUsageName, TermRelation, Set<IVariable<ILicenseUsageName, TermRelation>>> analyzeInputs = new AnalyzeInputsEvaluator<>();

	protected static final IEvaluator<ILicenseUsageName, TermRelation, TermRelation> valueEvaluator = new ValueEvaluator<>(TermRelationValueSystem.create(), TermRelationOperationSystem.create());

	protected static Set<ITerm> toTerms(final Set<IVariable<ILicenseUsageName, TermRelation>> variables) {
		final Set<ITerm> set = variables.stream().map(IVariable::getName).map(ILicenseUsageName::getTerm).collect(Collectors.toCollection(LinkedHashSet::new));
		return Collections.unmodifiableSet(set);
	}

	@Getter(AccessLevel.PROTECTED)
	protected final IPredicate1<TermRelation> test;

	public AnalyzeTermExpressionEvaluator(TermRelation target) {
		this(t -> t.equals(target));
	}

	@Override
	public ExpressionContextFinding eval(IExpression<ILicenseUsageName, TermRelation> expression) {
		final Set<IVariable<ILicenseUsageName, TermRelation>> inputs = analyzeInputs.eval(expression), outputs = new LinkedHashSet<>();

		final Literal<ILicenseUsageName, TermRelation> unspecified = new Literal<>(TermRelation.Unspecified);
		final IEnvironment<ILicenseUsageName, TermRelation> environment = new Environment<>(inputs.stream().collect(Collectors.toMap(IFunction1.identity(), IFunction1.create(unspecified))));
		for (IVariable<ILicenseUsageName, TermRelation> input : inputs) {
			final IEnvironment<ILicenseUsageName, TermRelation> override = environment.override(Environment.<ILicenseUsageName, TermRelation>builder().bind$(input, TermRelation.Excluded).build());
			final TermRelation result = valueEvaluator.eval(new Closure<>(override, expression));
			if (getTest().test(result)) outputs.add(input);
		}

		return new ExpressionContextFinding(expression, toTerms(inputs), toTerms(outputs), null);
	}
}
