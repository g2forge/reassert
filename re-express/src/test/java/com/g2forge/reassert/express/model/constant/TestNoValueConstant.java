package com.g2forge.reassert.express.model.constant;

import org.junit.Test;

import com.g2forge.alexandria.java.validate.ValidationFailureException;
import com.g2forge.reassert.express.model.constant.NoValueConstant;

public class TestNoValueConstant {
	@Test(expected = ValidationFailureException.class)
	public void test() {
		new NoValueConstant<>().validate().throwIfInvalid();
	}
}
