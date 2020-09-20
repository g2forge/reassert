package com.g2forge.reassert.express.v2.eval;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.ReductionRewriter.Reduction;
import com.g2forge.reassert.express.v2.eval.bool.BooleanOperationSystem;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.environment.Environment;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.variable.Closure;
import com.g2forge.reassert.express.v2.model.variable.Variable;

public class TestReductionRewriter {
	@Test
	public void applyDisabled() {
		final Variable<String, Boolean> x = new Variable<>("x");
		final Literal<String, Boolean> literal = new Literal<>(false);
		final Closure<String, Boolean> closure = new Closure<>(Environment.<String, Boolean>builder().bind(x, literal).build(), x);
		HAssert.assertEquals(closure, new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), new Reduction[0]).eval(closure));
	}

	@Test
	public void applyEnabled() {
		final Variable<String, Boolean> x = new Variable<>("x");
		final Literal<String, Boolean> literal = new Literal<>(false);
		final Closure<String, Boolean> closure = new Closure<>(Environment.<String, Boolean>builder().bind(x, literal).build(), x);
		HAssert.assertEquals(literal, new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), Reduction.ApplyClosures).eval(closure));
	}

	@Test
	public void applyUseless() {
		final Variable<String, Boolean> x = new Variable<>("x");
		final Closure<String, Boolean> closure = new Closure<>(Environment.<String, Boolean>builder().build(), x);
		HAssert.assertEquals(x, new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), Reduction.ApplyClosures).eval(closure));
	}

	@Test
	public void trivial0() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().build();
		HAssert.assertEquals(new Literal<>(true), new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), Reduction.TrivialOperations).eval(expression));
	}

	@Test
	public void trivial1() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), Reduction.TrivialOperations).eval(expression));
	}

	@Test
	public void trivialDisabled() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$(false).build();
		HAssert.assertEquals(expression, new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), new Reduction[0]).eval(expression));
	}
}
