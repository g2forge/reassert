package com.g2forge.reassert.express.v2.model.variable;

import org.junit.Test;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.Literal;

public class TestEnvironment {
	@Test
	public void lookupEmpty() {
		final Environment<String, Integer> env = Environment.<String, Integer>builder().build();

		HAssert.assertTrue(env.lookup(new Variable<>("x")).isEmpty());
	}

	@Test
	public void lookupFound() {
		final Literal<String, Integer> literal = new Literal<>(0);
		final Environment<String, Integer> env = Environment.<String, Integer>builder().bind(new Variable<>("x"), literal).build();

		final IOptional<? extends IExpression<String, Integer>> lookup = env.lookup(new Variable<>("x"));
		HAssert.assertFalse(lookup.isEmpty());
		HAssert.assertEquals(literal, lookup.get());
	}

	@Test
	public void same() {
		final Environment<String, Integer> env0 = Environment.<String, Integer>builder().bind(new Variable<>("x"), new Literal<>(0)).build();
		final Environment<String, Integer> env1 = Environment.<String, Integer>builder().bind(new Variable<>("x"), new Literal<>(0)).build();
		HAssert.assertTrue(env0.isSame(env1));
	}

	@Test
	public void notSsame() {
		final Environment<String, Integer> env0 = Environment.<String, Integer>builder().bind(new Variable<>("x"), new Literal<>(0)).build();
		final Environment<String, Integer> env1 = Environment.<String, Integer>builder().bind(new Variable<>("y"), new Literal<>(1)).build();
		HAssert.assertFalse(env0.isSame(env1));
	}
}
