package com.g2forge.reassert.express.v2.model.operation;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.validate.ValidationFailureException;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;

public class TestBooleanOperation {
	@Test
	public void construct() {
		final IOperation<String, Boolean> operation = BooleanOperation.Operator.NOT.<String, Boolean>builder().argument$("A", false).valid();
		HAssert.assertEquals(BooleanOperation.Operator.NOT, operation.getOperator());
		HAssert.assertEquals(HCollection.asList(new Literal<>("A", false)), operation.getArguments());
	}

	@Test(expected = ValidationFailureException.class)
	public void invalid() {
		BooleanOperation.Operator.NOT.<String, Boolean>builder().argument$("A", false).argument$(true).valid();
	}

	@Test
	public void notSameArguments() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$("A", false).argument$("B", false).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.AND, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertFalse(left.isSame(right));
	}

	@Test
	public void notSameArgumentsSize() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.XOR.<String, Boolean>builder().argument$("A", false).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.XOR, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertFalse(left.isSame(right));
	}

	@Test
	public void notSameOperator() {
		HAssert.assertFalse(new BooleanOperation<>(BooleanOperation.Operator.XOR).isSame(BooleanOperation.Operator.AND.builder().build()));
	}

	@Test
	public void same() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.OR.<String, Boolean>builder().argument$("A", false).argument$("B", true).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.OR, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertTrue(left.isSame(right));
	}
}
