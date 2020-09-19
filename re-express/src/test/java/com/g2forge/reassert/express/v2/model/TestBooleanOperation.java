package com.g2forge.reassert.express.v2.model;

import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.test.HAssert;

public class TestBooleanOperation {
	@Test
	public void construct() {
		final IOperation<String, Boolean> operation = BooleanOperation.Operator.Not.<String, Boolean>builder().argument$("A", false).valid();
		HAssert.assertEquals(BooleanOperation.Operator.Not, operation.getOperator());
		HAssert.assertEquals(HCollection.asList(new Literal<>("A", false)), operation.getArguments());
	}

	@Test(expected = IllegalArgumentException.class)
	public void invalid() {
		BooleanOperation.Operator.Not.<String, Boolean>builder().argument$("A", false).argument$(true).valid();
	}

	@Test
	public void notSameArguments() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.And.<String, Boolean>builder().argument$("A", false).argument$("B", false).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.And, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertFalse(left.isSame(right));
	}

	@Test
	public void notSameArgumentsSize() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.Xor.<String, Boolean>builder().argument$("A", false).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.Xor, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertFalse(left.isSame(right));
	}

	@Test
	public void notSameOperator() {
		HAssert.assertFalse(new BooleanOperation<>(BooleanOperation.Operator.Xor).isSame(BooleanOperation.Operator.And.builder().build()));
	}

	@Test
	public void same() {
		final IOperation<String, Boolean> left = BooleanOperation.Operator.Or.<String, Boolean>builder().argument$("A", false).argument$("B", true).valid();
		final BooleanOperation<String, Boolean> right = new BooleanOperation<>(BooleanOperation.Operator.Or, new Literal<>("A", false), new Literal<>("B", true));
		HAssert.assertTrue(left.isSame(right));
	}
}
