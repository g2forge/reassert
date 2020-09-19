package com.g2forge.reassert.express.v2.eval;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.ReductionRewriter.Reduction;
import com.g2forge.reassert.express.v2.eval.bool.BooleanOperationSystem;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;

public class TestReductionRewriter {
	@Test
	public void trivial0() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.And.<String, Boolean>builder().build();
		HAssert.assertEquals(new Literal<>(true), new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), Reduction.TrivialOperations).eval(expression));
	}

	@Test
	public void trivial1() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.And.<String, Boolean>builder().argument$(false).build();
		HAssert.assertEquals(new Literal<>(false), new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), Reduction.TrivialOperations).eval(expression));
	}

	@Test
	public void trivialDisabled() {
		final IOperation<String, Boolean> expression = BooleanOperation.Operator.And.<String, Boolean>builder().argument$(false).build();
		HAssert.assertEquals(expression, new ReductionRewriter<String, Boolean>(BooleanOperationSystem.create(), new Reduction[0]).eval(expression));
	}
}
