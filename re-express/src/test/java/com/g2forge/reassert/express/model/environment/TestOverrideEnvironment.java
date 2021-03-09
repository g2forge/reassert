package com.g2forge.reassert.express.model.environment;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.variable.Variable;

public class TestOverrideEnvironment {
	@Test
	public void one() {
		final Literal<String, Integer> one = new Literal<>(1);
		final Variable<String, Integer> x = new Variable<>("x");
		final Environment<String, Integer> env0 = Environment.<String, Integer>builder().build();
		final Environment<String, Integer> env1 = Environment.<String, Integer>builder().bind(x, one).build();
		HAssert.assertEquals(one, env0.override(env1).lookup(x).get());
	}

	@Test
	public void override() {
		final Literal<String, Integer> zero = new Literal<>(0), one = new Literal<>(1);
		final Variable<String, Integer> x = new Variable<>("x");
		final Environment<String, Integer> env0 = Environment.<String, Integer>builder().bind(x, zero).build();
		final Environment<String, Integer> env1 = Environment.<String, Integer>builder().bind(x, one).build();
		HAssert.assertEquals(one, env0.override(env1).lookup(x).get());
	}

	@Test
	public void zero() {
		final Literal<String, Integer> zero = new Literal<>(0);
		final Variable<String, Integer> x = new Variable<>("x");
		final Environment<String, Integer> env0 = Environment.<String, Integer>builder().bind(x, zero).build();
		final Environment<String, Integer> env1 = Environment.<String, Integer>builder().build();
		HAssert.assertEquals(zero, env0.override(env1).lookup(x).get());
	}
}
