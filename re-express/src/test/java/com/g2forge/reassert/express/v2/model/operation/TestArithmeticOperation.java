package com.g2forge.reassert.express.v2.model.operation;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;

public class TestArithmeticOperation {
	@Test
	public void construct() {
		final IOperation<String, Integer> operation = ArithmeticOperation.Operator.ADD.<String, Integer>builder().argument$("A", 0).valid();
		HAssert.assertEquals(ArithmeticOperation.Operator.ADD, operation.getOperator());
		HAssert.assertEquals(HCollection.asList(new Literal<>("A", 0)), operation.getArguments());
	}

	@Test
	public void equals() {
		final IOperation<String, Integer> left = ArithmeticOperation.Operator.SUBTRACT.<String, Integer>builder().argument$("A", 0).argument$("B", 1).valid();
		final ArithmeticOperation<String, Integer> right = new ArithmeticOperation<>(ArithmeticOperation.Operator.SUBTRACT, new Literal<>("A", 0), new Literal<>("B", 1));
		HAssert.assertTrue(left.equals(right));
	}

	@Test
	public void notEqualsArguments() {
		final IOperation<String, Integer> left = ArithmeticOperation.Operator.ADD.<String, Integer>builder().argument$("A", 0).argument$("B", 0).valid();
		final ArithmeticOperation<String, Integer> right = new ArithmeticOperation<>(ArithmeticOperation.Operator.ADD, new Literal<>("A", 0), new Literal<>("B", 1));
		HAssert.assertFalse(left.equals(right));
	}

	@Test
	public void notEqualsArgumentsSize() {
		final IOperation<String, Integer> left = ArithmeticOperation.Operator.ADD.<String, Integer>builder().argument$("A", 0).valid();
		final ArithmeticOperation<String, Integer> right = new ArithmeticOperation<>(ArithmeticOperation.Operator.ADD, new Literal<>("A", 0), new Literal<>("B", 1));
		HAssert.assertFalse(left.equals(right));
	}

	@Test
	public void notEqualsOperator() {
		HAssert.assertFalse(new ArithmeticOperation<>(ArithmeticOperation.Operator.ADD).equals(ArithmeticOperation.Operator.SUBTRACT.builder().build()));
	}
}
