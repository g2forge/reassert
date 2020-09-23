package com.g2forge.reassert.contract.v2.algorithm;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IPredicate1;
import com.g2forge.reassert.contract.v2.eval.TermRelationOperationSystem;
import com.g2forge.reassert.contract.v2.eval.TermRelationValueSystem;
import com.g2forge.reassert.contract.v2.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.contract.v2.model.licenseusage.ICTName;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.eval.AnalyzeInputsEvaluator;
import com.g2forge.reassert.express.v2.eval.IEvaluator;
import com.g2forge.reassert.express.v2.eval.ValueEvaluator;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.environment.Environment;
import com.g2forge.reassert.express.v2.model.environment.IEnvironment;
import com.g2forge.reassert.express.v2.model.variable.Closure;
import com.g2forge.reassert.express.v2.model.variable.IVariable;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class AnalyzeTermExpressionEvaluator implements IEvaluator<ICTName, TermRelation, ExpressionContextFinding> {
	protected static final IEvaluator<ICTName, TermRelation, Set<IVariable<ICTName, TermRelation>>> analyzeInputs = new AnalyzeInputsEvaluator<>();

	protected static final IEvaluator<ICTName, TermRelation, TermRelation> valueEvaluator = new ValueEvaluator<>(TermRelationValueSystem.create(), TermRelationOperationSystem.create());

	protected static Set<ITerm> toTerms(final Set<IVariable<ICTName, TermRelation>> variables) {
		return Collections.unmodifiableSet(variables.stream().map(IVariable::getName).map(ICTName::getTerm).collect(Collectors.toSet()));
	}

	public AnalyzeTermExpressionEvaluator(TermRelation target) {
		this(t -> t.equals(target));
	}

	@Getter(AccessLevel.PROTECTED)
	protected final IPredicate1<TermRelation> test;

	@Override
	public ExpressionContextFinding eval(IExpression<ICTName, TermRelation> expression) {
		final Set<IVariable<ICTName, TermRelation>> inputs = analyzeInputs.eval(expression), outputs = new LinkedHashSet<>();

		final Literal<ICTName, TermRelation> unspecified = new Literal<>(TermRelation.Unspecified);
		final IEnvironment<ICTName, TermRelation> environment = new Environment<>(inputs.stream().collect(Collectors.toMap(IFunction1.identity(), IFunction1.create(unspecified))));
		for (IVariable<ICTName, TermRelation> input : inputs) {
			final IEnvironment<ICTName, TermRelation> override = environment.override(Environment.<ICTName, TermRelation>builder().bind$(input, TermRelation.Excluded).build());
			final TermRelation result = valueEvaluator.eval(new Closure<>(override, expression));
			if (getTest().test(result)) outputs.add(input);
		}

		return new ExpressionContextFinding(expression, toTerms(inputs), toTerms(outputs), null);
	}
}
