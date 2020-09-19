package com.g2forge.reassert.express.v2.model;

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
	public void notSameName() {
		HAssert.assertFalse(Literal.builder().name("B").value(0).build().isSame(new Literal<>("A", 0)));
	}

	@Test
	public void notSameValue() {
		HAssert.assertFalse(Literal.builder().name("A").value(1).build().isSame(new Literal<>("A", 0)));
	}

	@Test
	public void same() {
		HAssert.assertTrue(Literal.builder().name("A").value(0).build().isSame(new Literal<>("A", 0)));
	}
}
