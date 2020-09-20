package com.g2forge.reassert.express.v2.eval;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.ReductionRewriter.Reduction;
import com.g2forge.reassert.express.v2.eval.bool.BooleanOperationSystem;
import com.g2forge.reassert.express.v2.eval.bool.BooleanValueSystem;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.constant.NoValueConstant;
import com.g2forge.reassert.express.v2.model.environment.Environment;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.variable.Closure;
import com.g2forge.reassert.express.v2.model.variable.Variable;

import lombok.Getter;

public class TestReductionRewriter {
	@Getter(lazy = true)
	private static final ValueEvaluator<String, Boolean> valueEvaluator = new ValueEvaluator<>(BooleanValueSystem.create(), BooleanOperationSystem.create());

	@Test
	public void applyDisabled() {
		final Variable<String, Boolean> x = new Variable<>("x");
		final Literal<String, Boolean> literal = new Literal<>(false);
		final Closure<String, Boolean> closure = new Closure<>(Environment.<String, Boolean>builder().bind(x, literal).build(), x);
		HAssert.assertEquals(closure, new ReductionRewriter<String, Boolean>(getValueEvaluator(), new Reduction[0]).eval(closure));
	}

	@Test
	public void applyEnabled() {
		final Variable<String, Boolean> x = new Variable<>("x");
		final Literal<String, Boolean> literal = new Literal<>(false);
		final Closure<String, Boolean> closure = new Closure<>(Environment.<String, Boolean>builder().bind(x, literal).build(), x);
		HAssert.assertEquals(literal, new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ApplyClosures).eval(closure));
	}

	@Test
	public void applyUseless() {
		final Variable<String, Boolean> x = new Variable<>("x");
		final Closure<String, Boolean> closure = new Closure<>(Environment.<String, Boolean>builder().build(), x);
		HAssert.assertEquals(x, new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ApplyClosures).eval(closure));
	}

	@Test
	public void constantNoChange() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument(new NoValueConstant<>()).argument$(true).build();
		HAssert.assertEquals(expression, new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ConstantFolding).eval(expression));
	}

	@Test
	public void constantNoValue() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument(new NoValueConstant<>()).argument$(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ConstantFolding).eval(expression));
	}
	
	@Test
	public void constantSimple() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$(true).argument$(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ConstantFolding).eval(expression));
	}
	
	@Test
	public void constantUnbound() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument(new Variable<>("x")).argument$(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ConstantFolding).eval(expression));
	}

	@Test
	public void trivial0() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().build();
		HAssert.assertEquals(new Literal<>(true), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.TrivialOperations).eval(expression));
	}

	@Test
	public void trivial1() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.TrivialOperations).eval(expression));
	}

	@Test
	public void trivialDisabled() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$(false).build();
		HAssert.assertEquals(expression, new ReductionRewriter<String, Boolean>(getValueEvaluator(), new Reduction[0]).eval(expression));
	}
}
