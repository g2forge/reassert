package com.g2forge.reassert.express.model.operation;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.validate.ValidationFailureException;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.model.constant.Literal;

public class TestBooleanOperation {
	@Test
	public void construct() {
		final IOperation<String, Boolean> operation = BooleanOperation.Operator.NOT.<String, Boolean>builder().argument$L("A", false).valid();
		HAssert.assertEquals(BooleanOperation.Operator.NOT, operation.getOperator());
		HAssert.assertEquals(HCollection.asList(new Literal<>("A", false)), operation.getArguments());
	}

	@Test
	public void equals() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.OR.<String, Boolean>builder().argument$L("A", false).argument$L("B", true).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.OR, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertTrue(left.equals(right));
	}

	@Test(expected = ValidationFailureException.class)
	public void invalid() {
		BooleanOperation.Operator.NOT.<String, Boolean>builder().argument$L("A", false).argument$L(true).valid();
	}

	@Test
	public void notEqualsArguments() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.AND.<String, Boolean>builder().argument$L("A", false).argument$L("B", false).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.AND, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertFalse(left.equals(right));
	}

	@Test
	public void notEqualsArgumentsSize() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.XOR.<String, Boolean>builder().argument$L("A", false).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.XOR, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertFalse(left.equals(right));
	}

	@Test
	public void notEqualsOperator() {
		HAssert.assertFalse(new BooleanOperation<>(BooleanOperation.Operator.XOR).equals(BooleanOperation.Operator.AND.builder().build()));
	}
}
