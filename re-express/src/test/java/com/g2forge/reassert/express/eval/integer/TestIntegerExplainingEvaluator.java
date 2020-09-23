package com.g2forge.reassert.express.eval.integer;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.eval.ATestExplainingEvaluator;
import com.g2forge.reassert.express.eval.ExplainingEvaluator;
import com.g2forge.reassert.express.eval.IEvaluator;
import com.g2forge.reassert.express.eval.integer.IntegerOperationSystem;
import com.g2forge.reassert.express.eval.integer.IntegerValueSystem;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.IExplained.Relevance;
import com.g2forge.reassert.express.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.model.operation.ExplainedOperation;
import com.g2forge.reassert.express.model.operation.IOperation.IOperator;
import com.g2forge.reassert.express.model.operation.ZeroExplainedOperation;

import lombok.Getter;

public class TestIntegerExplainingEvaluator extends ATestExplainingEvaluator<Integer> {
	@Getter(lazy = true)
	private final IEvaluator<String, Integer, IExplained<Integer>> evaluator = new ExplainingEvaluator<>(IntegerValueSystem.create(), IntegerOperationSystem.create());

	@Override
	public IOperator getMultiply() {
		return ArithmeticOperation.Operator.MULTIPLY;
	}

	@Override
	public Integer getOne() {
		return 1;
	}

	@Override
	public Integer getZero() {
		return 0;
	}

	@Test
	public void operationCombined() {
		final IExplained<Integer> expected = ExplainedOperation.<Integer>builder().operator(getMultiply()).value(6).identity$(getOne()).argument$(Relevance.Identity, getOne()).argument$(Relevance.Combined, 2).argument$(Relevance.Combined, 3).build();
		final IExplained<Integer> actual = getEvaluator().eval(getMultiply().<String, Integer>builder().argument$L(getOne()).argument$L(2).argument$L(3).build());
		HAssert.assertEquals(expected, actual);
	}

	@Test
	public void operationZero3() {
		final IExplained<Integer> expected = ZeroExplainedOperation.<Integer>builder().operator(getMultiply()).zero(getZero()).argument$(Relevance.Combined, 3).argument$(Relevance.Dominant, getZero()).argument$(Relevance.Unevaluated, 2).build();
		final IExplained<Integer> actual = getEvaluator().eval(getMultiply().<String, Integer>builder().argument$L(3).argument$L(getZero()).argument$L(2).build());
		HAssert.assertEquals(expected, actual);
	}
}
