package com.g2forge.reassert.express.evaluate;

import com.g2forge.reassert.expression.evaluate.bool.ExplainedBooleanEvaluator;
import com.g2forge.reassert.expression.evaluate.bool.IBooleanSystem;
import com.g2forge.reassert.expression.explain.convert.ExplanationMode;
import com.g2forge.reassert.expression.explain.convert.ExplanationRenderer;
import com.g2forge.reassert.expression.explain.model.IExplained;
import com.g2forge.reassert.expression.express.IExpression;

import lombok.Getter;

public abstract class ATestExplainedBooleanEvaluator<T> {
	@Getter(lazy = true)
	private final IBooleanSystem<T> system = createSystem();

	@Getter(lazy = true)
	private final ExplainedBooleanEvaluator<T> evaluator = new ExplainedBooleanEvaluator<>(getSystem());

	protected abstract IBooleanSystem<T> createSystem();

	protected IExplained<T> evaluate(IExpression<T> expression) {
		return getEvaluator().eval(expression);
	}

	protected String explain(ExplanationMode mode, IExpression<T> expression) {
		final IExplained<T> explanation = evaluate(expression);
		return new ExplanationRenderer(mode).render(explanation);
	}

	protected String explain(IExpression<T> expression) {
		return explain(ExplanationMode.Trace, expression);
	}
}
