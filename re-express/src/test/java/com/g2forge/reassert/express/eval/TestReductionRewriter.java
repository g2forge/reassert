package com.g2forge.reassert.express.eval;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.eval.ReductionRewriter.Reduction;
import com.g2forge.reassert.express.eval.operation.BooleanOperationSystem;
import com.g2forge.reassert.express.eval.value.ObjectValueSystem;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.constant.NoValueConstant;
import com.g2forge.reassert.express.model.environment.Environment;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.variable.Closure;
import com.g2forge.reassert.express.model.variable.Variable;

import lombok.Getter;

public class TestReductionRewriter {
	@Getter(lazy = true)
	private static final ValueEvaluator<String, Boolean> valueEvaluator = computeValueEvaluator();

	protected static ValueEvaluator<String, Boolean> computeValueEvaluator() {
		return new ValueEvaluator<>(ObjectValueSystem.create(), BooleanOperationSystem.create());
	}

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
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument(new NoValueConstant<>()).argument$L(true).build();
		HAssert.assertEquals(expression, new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ConstantFolding).eval(expression));
	}

	@Test
	public void constantNoValue() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument(new NoValueConstant<>()).argument$L(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ConstantFolding).eval(expression));
	}

	@Test
	public void constantSimple() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$L(true).argument$L(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ConstantFolding).eval(expression));
	}

	@Test
	public void constantUnbound() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument(new Variable<>("x")).argument$L(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.ConstantFolding).eval(expression));
	}

	@Test
	public void trivial0() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().build();
		HAssert.assertEquals(new Literal<>(true), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.TrivialOperations).eval(expression));
	}

	@Test
	public void trivial1() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$L(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(getValueEvaluator(), Reduction.TrivialOperations).eval(expression));
	}

	@Test
	public void trivialDisabled() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$L(false).build();
		HAssert.assertEquals(expression, new ReductionRewriter<String, Boolean>(getValueEvaluator(), new Reduction[0]).eval(expression));
	}
}
