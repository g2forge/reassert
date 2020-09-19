package com.g2forge.reassert.express.v2.model;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;

public class TestArithmeticOperation {
	@Test
	public void construct() {
		final IOperation<String, Integer> operation = ArithmeticOperation.Operator.Add.<String, Integer>builder().argument$("A", 0).valid();
		HAssert.assertEquals(ArithmeticOperation.Operator.Add, operation.getOperator());
		HAssert.assertEquals(HCollection.asList(new Literal<>("A", 0)), operation.getArguments());
	}

	@Test
	public void notSameArguments() {
		final IOperation<String, Integer> left = ArithmeticOperation.Operator.Add.<String, Integer>builder().argument$("A", 0).argument$("B", 0).valid();
		final ArithmeticOperation<String, Integer> right = new ArithmeticOperation<>(ArithmeticOperation.Operator.Add, new Literal<>("A", 0), new Literal<>("B", 1));
		HAssert.assertFalse(left.isSame(right));
	}

	@Test
	public void notSameArgumentsSize() {
		final IOperation<String, Integer> left = ArithmeticOperation.Operator.Add.<String, Integer>builder().argument$("A", 0).valid();
		final ArithmeticOperation<String, Integer> right = new ArithmeticOperation<>(ArithmeticOperation.Operator.Add, new Literal<>("A", 0), new Literal<>("B", 1));
		HAssert.assertFalse(left.isSame(right));
	}

	@Test
	public void notSameOperator() {
		HAssert.assertFalse(new ArithmeticOperation<>(ArithmeticOperation.Operator.Add).isSame(ArithmeticOperation.Operator.Subtract.builder().build()));
	}

	@Test
	public void same() {
		final IOperation<String, Integer> left = ArithmeticOperation.Operator.Subtract.<String, Integer>builder().argument$("A", 0).argument$("B", 1).valid();
		final ArithmeticOperation<String, Integer> right = new ArithmeticOperation<>(ArithmeticOperation.Operator.Subtract, new Literal<>("A", 0), new Literal<>("B", 1));
		HAssert.assertTrue(left.isSame(right));
	}
}
