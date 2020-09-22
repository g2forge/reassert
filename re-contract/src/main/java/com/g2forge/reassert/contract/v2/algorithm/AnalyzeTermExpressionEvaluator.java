package com.g2forge.reassert.contract.v2.algorithm;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.v2.algorithm.AnalyzeTermExpressionEvaluator.Analyzed;
import com.g2forge.reassert.contract.v2.eval.TermRelationOperationSystem;
import com.g2forge.reassert.contract.v2.eval.TermRelationValueSystem;
import com.g2forge.reassert.contract.v2.model.ICTName;
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
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class AnalyzeTermExpressionEvaluator implements IEvaluator<ICTName, TermRelation, Analyzed> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class Analyzed {
		protected final IExpression<ICTName, TermRelation> expression;

		protected final Set<ITerm> inputs;

		protected final Set<ITerm> outputs;
	}

	protected static final IEvaluator<ICTName, TermRelation, Set<IVariable<ICTName, TermRelation>>> analyzeInputs = new AnalyzeInputsEvaluator<>();

	protected static final IEvaluator<ICTName, TermRelation, TermRelation> valueEvaluator = new ValueEvaluator<>(TermRelationValueSystem.create(), TermRelationOperationSystem.create());

	protected static Set<ITerm> toTerms(final Set<IVariable<ICTName, TermRelation>> variables) {
		return Collections.unmodifiableSet(variables.stream().map(IVariable::getName).map(ICTName::getTerm).collect(Collectors.toSet()));
	}

	@Getter(AccessLevel.PROTECTED)
	protected final TermRelation target;

	@Override
	public Analyzed eval(IExpression<ICTName, TermRelation> expression) {
		final Set<IVariable<ICTName, TermRelation>> inputs = analyzeInputs.eval(expression), outputs = new LinkedHashSet<>();

		final Literal<ICTName, TermRelation> unspecified = new Literal<>(TermRelation.Unspecified);
		final IEnvironment<ICTName, TermRelation> environment = new Environment<>(inputs.stream().collect(Collectors.toMap(IFunction1.identity(), IFunction1.create(unspecified))));
		for (IVariable<ICTName, TermRelation> input : inputs) {
			final IEnvironment<ICTName, TermRelation> override = environment.override(Environment.<ICTName, TermRelation>builder().bind$(input, TermRelation.Excluded).build());
			final TermRelation result = valueEvaluator.eval(new Closure<>(override, expression));
			if (getTarget().equals(result)) outputs.add(input);
		}

		return new Analyzed(expression, toTerms(inputs), toTerms(outputs));
	}
}
