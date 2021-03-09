package com.g2forge.reassert.express.model.constant;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;

public class TestLiteral {
	@Test
	public void access() {
		final ILiteral<String, Integer> literal = new Literal<>("A", 0);
		HAssert.assertEquals("A", literal.getName());
		HAssert.assertEquals(Integer.valueOf(0), literal.get());
	}

	@Test
	public void equals() {
		HAssert.assertTrue(Literal.builder().name("A").value(0).build().equals(new Literal<>("A", 0)));
	}

	@Test
	public void notEqualsName() {
		HAssert.assertFalse(Literal.builder().name("B").value(0).build().equals(new Literal<>("A", 0)));
	}

	@Test
	public void notEqualsValue() {
		HAssert.assertFalse(Literal.builder().name("A").value(1).build().equals(new Literal<>("A", 0)));
	}
}
