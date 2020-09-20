package com.g2forge.reassert.express.v2.eval;

import org.junit.Test;

import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.bool.BooleanOperationSystem;
import com.g2forge.reassert.express.v2.eval.bool.BooleanValueSystem;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.environment.Environment;
import com.g2forge.reassert.express.v2.model.variable.Closure;
import com.g2forge.reassert.express.v2.model.variable.ExplainedClosure;
import com.g2forge.reassert.express.v2.model.variable.ExplainedVariable;
import com.g2forge.reassert.express.v2.model.variable.Variable;

import lombok.Getter;

public class TestExplainingEvaluator {
	@Getter(lazy = true)
	private static final IEvaluator<String, Boolean, IExplained<Boolean>> evaluator = new ExplainingEvaluator<>(BooleanValueSystem.create(), BooleanOperationSystem.create());

	@Test
	public void closure() {
		final Variable<String, Boolean> x = new Variable<>("x");

		final NullableOptional<IExplained<Boolean>> value = NullableOptional.of(new Literal<>(false));
		final IExplained<Boolean> expected = ExplainedClosure.<String, Boolean>builder().expression(new ExplainedVariable<>(x, value)).binding$(IExplained.Relevance.Evaluated, x, value).build();

		final IExplained<Boolean> actual = getEvaluator().eval(new Closure<>(Environment.<String, Boolean>builder().bind$(x, false).build(), x));

		HAssert.assertEquals(expected, actual);
	}

	@Test
	public void literal() {
		final Literal<String, Boolean> x = new Literal<>("x", false);
		HAssert.assertEquals(x, getEvaluator().eval(x));
	}

	@Test
	public void variable() {
		final Variable<String, Boolean> x = new Variable<>("x");
		HAssert.assertEquals(new ExplainedVariable<>(x, NullableOptional.empty()), getEvaluator().eval(x));
	}
}
